open Location
open Tools

module T = Michelson
module M = Model
module A = ParseTree

type env = {
  name: string;
}

let mk_env ?(name="") _ : env = { name }

let parse_michelson (filename, ic) : T.michelson * env =
  let name =
    match filename with
    | "<stdin>" -> "noname"
    | _ -> filename |> Filename.basename |> Filename.chop_extension
  in
  let env = mk_env ~name:name () in
  let tokens = Lexing.from_channel ic in
  let res = Michelson_parser.main Michelson_lexer.token tokens, env in
  match !Error.errors with
  | [] -> res
  | _ -> raise (Error.ParseError !Error.errors)

type ir_env = {
  cpt_alpha: int;
  deep:      int;
  fail:      bool;
  scopes:    (T.dinstruction list) list;
}

let mk_ir_env ?(cpt_alpha=0) ?(deep=0) ?(fail=false) ?(scopes=[]) _ : ir_env =
  { cpt_alpha; deep; fail; scopes }

let inc_deep (env : ir_env) : ir_env = { env with deep = env.deep + 1 }
let dec_deep (env : ir_env) : ir_env = { env with deep = env.deep - 1 }

let to_dir (michelson, env : T.michelson * env) =
  let tstorage   = michelson.storage in
  let tparameter = michelson.parameter in

  let storage_data =
    match tstorage.node with
    | T.Tunit   -> T.Dunit
    | T.Tnat
    | T.Tint    -> T.Dint Big_int.zero_big_int
    | T.Tstring -> T.Dstring ""
    | _ -> T.Dunit (* FIXME*)
  in

  let inc_cpt_alpha env = { env with cpt_alpha = env.cpt_alpha + 1 } in

  let pp_stack fmt (st : T.dexpr list) =
    List.iteri (fun i (c : T.dexpr) ->
        Format.fprintf fmt "%i. %a@\n" i Printer_michelson.pp_dexpr c) st
  in

  let pp_trace fmt (instr, stack, sys : T.code option * (T.dexpr) list * T.sysofequations) =
    Format.fprintf fmt "@\nsys:@\n%a@\n@\n" Printer_michelson.pp_sysofequations sys;
    Format.fprintf fmt "@\nstack:@\n%a" pp_stack stack;
    (Printer_tools.pp_option (fun fmt -> Format.fprintf fmt "@\ninstr: %a" Printer_michelson.pp_code)) fmt instr;
    Format.fprintf fmt "@."
  in

  let trace (env : ir_env) (instrs : T.code list) (stack : (T.dexpr) list)  (sys : T.sysofequations) =
    let print_indent fmt n =
      for _i = 1 to n do
        Format.fprintf fmt "  "
      done
    in
    match !Options.opt_trace, instrs with
    | true, i::_ -> Format.eprintf "%a@[%a@]@." print_indent env.deep pp_trace (Some i, stack, sys)
    | true, [] -> Format.eprintf "%a@[%a@]@." print_indent env.deep pp_trace (None, stack, sys)
    | _ -> ()
  in

  let add_instruction (_env : ir_env) (sys : T.sysofequations) (i : T.dinstruction) =
    let assigns =
      let rec aux accu (a, b : T.dexpr * T.dexpr) =
        match a, b with
        | _ when T.cmp_dexpr a b -> accu
        | T.Dbop (Bpair, a1, b1), _ -> begin
            let f op =
              match b, op with
              | T.Dbop (Bpair, x, _), T.Ucar -> x
              | T.Dbop (Bpair, _, y), T.Ucdr -> y
              | _ -> T.Duop (op, b)
            in
            let car = f Ucar in
            let cdr = f Ucdr in
            aux (aux accu (a1, car)) (b1, cdr)
          end
        | _ -> (a, b)::accu
      in
      match i with
      | T.Dassign (a, b) -> aux [] (a, b)
      | _                -> []
    in

    match assigns with
    | [] -> i::sys
    | _ -> begin
        List.fold_right (
          fun (a, b) (accu : T.dinstruction list) ->

            let _cpt =
              let rec f accu (e : T.dexpr) = if T.cmp_dexpr a e then accu + 1 else T.fold_dexpr f accu e in
              let fi accu (i : T.dinstruction) =
                match i with
                | T.Dassign (_, x) -> f accu x
                | _ -> T.fold_dinstruction_dexpr f accu i
              in
              List.fold_left fi 0 (accu @ (List.flatten _env.scopes))
            in

            let is_outside =
              let rec f accu (e : T.dexpr) = if T.cmp_dexpr a e then true else T.fold_dexpr f accu e in
              let fi accu (i : T.dinstruction) = T.fold_dinstruction_dexpr f accu i in
              List.fold_left fi false (List.flatten _env.scopes)
            in

            let _cost =
              match b with
              | T.Dalpha      _ -> 0
              | T.Dvar        _ -> 0
              | T.Dstorage    _ -> 0
              | T.Doperations   -> 0
              | T.Ddata       _ -> 0
              | T.Dzop        _ -> 1
              | T.Duop        _ -> 1
              | T.Dbop        _ -> 1
              | T.Dtop        _ -> 1
            in

            let is_assigned_a =
              let rec aux accu x =
                match x with
                | T.Dassign (z, _) when T.cmp_dexpr a z -> true
                | _ -> T.fold_dinstruction aux accu x
              in

              List.fold_left aux false (accu @ (List.flatten _env.scopes))
            in

            match _cpt, a, is_assigned_a, is_outside with
            | _, Dalpha _, false, false -> begin
                let replace instrs = List.map (
                    fun x ->
                      let rec fe (x : T.dexpr) : T.dexpr =
                        if T.cmp_dexpr a x then b else T.map_dexpr fe x
                      in
                      let rec f (x : T.dinstruction) : T.dinstruction = T.map_dinstruction_gen fe f x in
                      f x) instrs in
                let opt_expr sys =
                  List.map (fun instr -> begin
                        let rec fe (expr : T.dexpr) =
                          match expr with
                          | T.Duop (T.Ucar, T.Dbop (Bpair, x, _)) -> fe x
                          | T.Duop (T.Ucdr, T.Dbop (Bpair, _, y)) -> fe y
                          | _ -> T.map_dexpr fe expr
                        in
                        let rec f (x : T.dinstruction) : T.dinstruction = T.map_dinstruction_gen fe f x in
                        f instr
                      end) sys
                in
                let opt_instr (sys  : T.dinstruction list) =
                  let rec aux (instrs : T.dinstruction list) =
                    List.fold_right (fun instr accu ->
                        match instr with
                        | T.Dassign (a, b) when T.cmp_dexpr a b -> accu
                        | T.Dif     (c, t, e)  -> (T.Dif (c, aux t, aux e))::accu
                        | _ -> instr::accu) instrs []
                  in
                  aux sys
                in
                accu |> replace |> opt_expr |> opt_instr
              end
            | _ -> (T.Dassign (a, b))::accu

          (* match _cpt, a, _env.scopes with
             (* | 0, _, _ -> accu *)
             | 1, Dalpha _, []
             | _, Dalpha _, [] when _cost = 0 -> begin
              let replace instrs = List.map (
                  fun x ->
                    let rec fe (x : T.dexpr) : T.dexpr =
                      if T.cmp_dexpr a x then b else T.map_dexpr fe x
                    in
                    let rec f (x : T.dinstruction) : T.dinstruction = T.map_dinstruction_gen fe f x in
                    f x) instrs in
              replace accu
             end
             | _ -> (T.Dassign (a, b))::accu *)

        ) assigns sys
      end

  in

  (* let add_instruction _env (sys : T.sysofequations) (i : T.dinstruction) = i::sys in *)

  let rec interp (env : ir_env) (sys : T.sysofequations) (instrs : T.code list) (stack : (T.dexpr) list) =
    let f = interp in

    let emit_error _ =
      match instrs with
      | i::_ -> Format.eprintf "error:@\n%a@." pp_trace (Some i, stack, sys); assert false
      | _ -> assert false
    in

    let interp_zop env op it st =
      match st with
      | a::st -> f env (add_instruction env sys (T.Dassign (a, Dzop op))) it st
      | _ -> emit_error ()
    in

    let interp_uop env op it st =
      match st with
      | a::st -> begin
          let x = T.dalpha env.cpt_alpha in
          let env = inc_cpt_alpha env in
          f env (add_instruction env sys (T.Dassign (a, Duop (op, x)))) it (x::st)
        end
      | _ -> emit_error ()
    in

    let interp_bop env op it st =
      match st with
      | a::st -> begin
          let x = T.dalpha env.cpt_alpha in
          let env = inc_cpt_alpha env in
          let y = T.dalpha env.cpt_alpha in
          let env = inc_cpt_alpha env in
          f env (add_instruction env sys (T.Dassign (a, Dbop (op, x, y)))) it (x::y::st)
        end
      | _ -> emit_error ()
    in

    let interp_top env op it st =
      match st with
      | a::st -> begin
          let x = T.dalpha env.cpt_alpha in
          let env = inc_cpt_alpha env in
          let y = T.dalpha env.cpt_alpha in
          let env = inc_cpt_alpha env in
          let z = T.dalpha env.cpt_alpha in
          let env = inc_cpt_alpha env in
          f env (add_instruction env sys (T.Dassign (a, Dtop (op, x, y, z)))) it (x::y::z::st)
        end
      | _ -> emit_error ()
    in

    trace env instrs stack sys;

    match instrs with

    (* Control structures *)

    | SEQ l::it       -> begin
        f env sys (it @ List.rev l) stack
      end
    | APPLY::_       -> assert false
    | EXEC::_        -> assert false
    | FAILWITH::it   -> begin
        let env = {env with fail = true} in
        let x = T.dalpha env.cpt_alpha in
        let env = inc_cpt_alpha env in
        f env (add_instruction env sys (T.Dfail x)) it (x::stack)
      end
    | IF (th, el)::it -> begin
        let env = inc_deep env in
        let scopes = env.scopes in
        let env = { env with scopes = sys::scopes } in
        let sys_then, stack_then, env_then = interp env [] (List.rev th) stack in
        let sys_else, stack_else, env_else = interp { env with cpt_alpha = env_then.cpt_alpha } [] (List.rev el) stack in
        let env = { env with scopes = scopes; cpt_alpha = env_else.cpt_alpha } in

        (* Format.printf "stack:@\n%a@\n@." pp_stack stack; *)
        (* Format.printf "_stack_then:@\n%a@\n@." pp_stack stack_then; *)
        (* Format.printf "_stack_else:@\n%a@\n@." pp_stack stack_else; *)

        let stack, env, decls, sys_then, sys_else =
          let stack_rev_ref  = List.rev stack in
          let stack_rev_then = List.rev stack_then in
          let stack_rev_else = List.rev stack_else in

          let _stack_in =
            match env_then.fail, env_else.fail with
            | false, false -> stack_then
            | true,  false -> stack_else
            | false, true  -> stack_then
            | true,  true  -> assert false
          in

          let g lref lbranch =
            let size = min (List.length lref) (List.length lbranch) in
            let l1 = List.sub 0 size lref in
            let l2 = List.sub 0 size lbranch in
            let rec aux accu x y =
              match x, y with
              | [], [] -> accu
              | a::t1, b::t2 -> aux (add_instruction env accu (Dassign (b, a))) t1 t2
              | _ -> assert false
            in
            aux [] l1 l2
          in

          let sys_then = sys_then @ (g stack_rev_ref stack_rev_then) in
          let sys_else = sys_else @ (g stack_rev_ref stack_rev_else) in

          let stack, decls =
            let l1 = (List.length _stack_in) in
            let l2 = (List.length stack) in
            if l1 = l2
            then stack, []
            else begin
              if l1 < l2
              then begin
                let ast, bst = stack |> List.rev |> List.cut (l2 - 1) in
                let sys = List.fold_left (fun accu (x : T.dexpr) ->
                    match x with
                    | T.Dalpha id -> [T.Ddecl id]
                    | _ -> accu) sys bst in
                List.rev ast, sys
              end
              else assert false
            end
          in

          stack, env, decls, sys_then, sys_else
        in

        let x = T.dalpha env.cpt_alpha in
        let env = inc_cpt_alpha env in

        let sys = add_instruction env sys (T.Dif (x, sys_then, sys_else)) in
        let sys = List.fold_left (fun accu x -> add_instruction env accu x) sys decls in

        f env sys it (x::stack)
      end
    | IF_CONS _::_   -> assert false
    | IF_LEFT _::_   -> assert false
    | IF_NONE _::_   -> assert false
    | ITER _::_      -> assert false
    | LAMBDA _::_    -> assert false
    | LOOP _::_      -> assert false
    | LOOP_LEFT _::_ -> assert false


    (* Stack manipulation *)

    | DIG n::it -> begin
        match stack with
        | a::st -> begin
            let rec insert idx e l =
              match l with
              | _ when idx = 0 -> e::l
              | a::t -> a::(insert (idx - 1) e t)
              | _ -> emit_error ()
            in

            let stack = insert n a st in
            f env sys it stack
          end
        | _ -> emit_error ()
      end

    | DIP (n, instrs)::it -> begin
        let ast, bst =
          let rec aux idx accu l =
            match l with
            | _ when idx = 0 -> accu, l
            | i::t -> aux (idx - 1) (accu @ [i]) t
            | _ -> emit_error ()
          in
          aux n [] stack
        in
        let env = inc_deep env in
        let accu, stack, env = interp env sys (List.rev instrs) bst in
        trace env [] stack accu;
        let env = dec_deep env in
        f env accu it (ast @ stack)
      end

    | DROP n::it -> begin
        let stack, env = foldi
            (fun (st, env) ->
               let d = T.dalpha env.cpt_alpha in
               let env = inc_cpt_alpha env in
               (d::st), env
            ) (stack, env) n
        in
        f env sys it stack
      end

    | DUG n::it -> begin
        let rec aux accu idx l =
          match l with
          | e::t when idx = 0 -> (e::accu @ t)
          | e::t -> aux (accu @ [e]) (idx - 1) t
          | [] -> emit_error ()
        in

        let stack = aux [] n stack in
        f env sys it stack
      end

    | DUP::it -> begin
        match stack with
        | a::b::st -> f env (add_instruction env sys (T.Dassign (a, b))) it (b::st)
        | _ -> emit_error ()
      end

    | PUSH (_t, d)::it -> begin
        match stack with
        | a::t -> begin
            let data = d in
            let accu = add_instruction env sys (T.Dassign (a, T.Ddata data)) in
            f env accu it t
          end
        | _ -> emit_error ()
      end

    | SWAP::it -> begin
        match stack with
        | a::b::st -> f env sys it (b::a::st)
        | _ -> emit_error ()
      end


    (* Arthmetic operations *)

    | ABS::it      -> interp_uop env Uabs     it stack
    | ADD::it      -> interp_bop env Badd     it stack
    | COMPARE::it  -> interp_bop env Bcompare it stack
    | EDIV::it     -> interp_bop env Bediv    it stack
    | EQ::it       -> interp_uop env Ueq      it stack
    | GE::it       -> interp_uop env Uge      it stack
    | GT::it       -> interp_uop env Ugt      it stack
    | INT::it      -> interp_uop env Uint     it stack
    | ISNAT::it    -> interp_uop env Uisnat   it stack
    | LE::it       -> interp_uop env Ule      it stack
    | LSL::it      -> interp_bop env Blsl     it stack
    | LSR::it      -> interp_bop env Blsr     it stack
    | LT::it       -> interp_uop env Ult      it stack
    | MUL::it      -> interp_bop env Bmul     it stack
    | NEG::it      -> interp_uop env Uneg     it stack
    | NEQ::it      -> interp_uop env Une      it stack
    | SUB::it      -> interp_bop env Bsub     it stack


    (* Boolean operations *)

    | AND::it     -> interp_bop env Band it stack
    | NOT::it     -> interp_uop env Unot it stack
    | OR::it      -> interp_bop env Bor  it stack
    | XOR::it     -> interp_bop env Bxor it stack


    (* Cryptographic operations *)

    | BLAKE2B::it          -> interp_uop env Ublake2b         it stack
    | CHECK_SIGNATURE::it  -> interp_top env Tcheck_signature it stack
    | HASH_KEY::it         -> interp_uop env Uhash_key        it stack
    | SHA256::it           -> interp_uop env Usha256          it stack
    | SHA512::it           -> interp_uop env Usha512          it stack


    (* Blockchain operations *)

    | ADDRESS::it            -> interp_zop env Zaddress it stack
    | AMOUNT::it             -> interp_zop env Zamount it stack
    | BALANCE::it            -> interp_zop env Zbalance it stack
    | CHAIN_ID::it           -> interp_zop env Zchain_id it stack
    | CONTRACT (t, a)::it    -> interp_uop env (Ucontract (t, a)) it stack
    | CREATE_CONTRACT _::_   -> assert false
    | IMPLICIT_ACCOUNT::it   -> interp_uop env (Uimplicitaccount) it stack
    | NOW::it                -> interp_zop env Znow it stack
    | SELF::it               -> interp_zop env (Zself None) it stack
    | SENDER::it             -> interp_zop env Zsender it stack
    | SET_DELEGATE::it       -> interp_uop env Usetdelegate it stack
    | SOURCE::it             -> interp_zop env Zsource it stack
    | TRANSFER_TOKENS::it    -> interp_top env Ttransfer_tokens it stack


    (* Operations on data structures *)

    | CAR::it                  -> interp_uop env Ucar it stack
    | CDR::it                  -> interp_uop env Ucdr it stack
    | CONCAT::it               -> interp_bop env Bconcat it stack
    | CONS::it                 -> interp_bop env Bcons it stack
    | EMPTY_BIG_MAP (k, v)::it -> interp_zop env (Zemptybigmap (k, v)) it stack
    | EMPTY_MAP (k, v)::it     -> interp_zop env (Zemptymap (k, v)) it stack
    | EMPTY_SET t::it          -> interp_zop env (Zemptyset t) it stack
    | GET::it                  -> interp_bop env Bget it stack
    | LEFT t::it               -> interp_uop env (Uleft t) it stack
    | MAP _::_                 -> assert false
    | MEM::it                  -> interp_bop env Bmem it stack
    | NIL t::it                -> interp_zop env (Znil t) it stack
    | NONE t::it               -> interp_zop env (Znone t)it stack
    | PACK::it                 -> interp_uop env Upack it stack
    | PAIR::it                 -> interp_bop env Bpair it stack
    | RIGHT t::it              -> interp_uop env (Uright t) it stack
    | SIZE::it                 -> interp_uop env Usize it stack
    | SLICE::it                -> interp_top env Tslice it stack
    | SOME::it                 -> interp_uop env (Usome) it stack
    | UNIT::it                 -> interp_zop env (Zunit) it stack
    | UNPACK t::it             -> interp_uop env (Uunpack t) it stack
    | UPDATE::it               -> interp_top env Tupdate it stack


    (* Other *)

    | UNPAIR::it  -> begin
        match stack with
        | x::y::st -> f env sys it (T.Dbop (Bpair, x, y)::st)
        | _ -> emit_error ()
      end
    | SELF_ADDRESS::it   -> interp_zop env (Zself_address) it stack

    | CAST::_                     -> assert false
    | CREATE_ACCOUNT::_           -> assert false
    | RENAME::_                   -> assert false
    | STEPS_TO_QUOTA::_           -> assert false
    | LEVEL::_                    -> assert false
    | SAPLING_EMPTY_STATE::_      -> assert false
    | SAPLING_VERIFY_UPDATE::_    -> assert false
    | NEVER::_                    -> assert false
    | VOTING_POWER::_             -> assert false
    | TOTAL_VOTING_POWER::_       -> assert false
    | KECCAK::_                   -> assert false
    | SHA3::_                     -> assert false
    | PAIRING_CHECK::_            -> assert false
    | SUBMIT_PROPOSALS::_         -> assert false
    | SUBMIT_BALLOT::_            -> assert false
    | SET_BAKER_ACTIVE::_         -> assert false
    | TOGGLE_BAKER_DELEGATIONS::_ -> assert false
    | SET_BAKER_CONSENSUS_KEY::_  -> assert false
    | SET_BAKER_PVSS_KEY::_       -> assert false

    | [] -> sys, stack, env
  in

  let name = env.name in
  let irenv = mk_ir_env () in
  let type_to_dexpr (a : string) (x : T.type_) =
    let rec aux (x : T.type_) =
      match x.node with
      | _ when Option.is_some x.annotation -> Some (T.Dvar x)
      | T.Tpair (a, b) -> begin
          match aux a, aux b with
          | Some a, Some b -> Some (T.Dbop (Bpair, a, b))
          | _ -> None
        end
      | _ -> None
    in
    let r = aux x in
    match r with
    | Some v -> v
    | None -> T.Dvar { x with annotation = Some a }
  in

  let init_stack : (T.dexpr) list = T.[Dbop (Bpair, Doperations, type_to_dexpr "storage" tstorage)] in
  let sys, stack, irenv = interp irenv [] [michelson.code] init_stack in
  trace irenv [] stack sys;
  let sys =
    match stack with
    | x::_ -> begin
        add_instruction irenv sys (T.Dassign (x, Dbop (Bpair, type_to_dexpr "parameter" tparameter, type_to_dexpr "storage" tstorage)))
      end
    | _ -> Format.eprintf "error: stack not empty@."; assert false
  in
  (T.mk_dprogram tstorage tparameter storage_data name sys), env

let to_ir (dir, env : T.dprogram * env) : T.ir * env =
  let tstorage     = dir.storage in
  let tparameter   = dir.parameter in
  let storage_data = dir.storage_data in

  let for_expr (e : T.dexpr) : M.mterm =
    (* let f = for_expr in *)
    match e with
    | Dalpha _i              -> assert false
    | Dvar _t                -> assert false
    | Duop (_op, _a)         -> assert false
    | Dstorage _t            -> assert false
    | Doperations            -> assert false
    | Ddata _d               -> assert false
    | Dzop _op               -> assert false
    | Dbop (_op, _a, _b)     -> assert false
    | Dtop (_op, _a, _b, _c) -> assert false
  in

  let rec _for_instr (i : T.dinstruction) : M.mterm =
    let f = _for_instr in
    let seq x = M.seq (List.map f x) in
    match i with
    | Dassign (_a, _b) -> assert false
    | Dif (c, t, e)    -> M.mk_mterm (M.Mif (for_expr c, seq t, Some (seq e))) M.tunit
    | Dfail e          -> M.failg (for_expr e)
    | Ddecl _id        -> assert false
  in

  T.mk_ir tstorage storage_data [] tparameter [] [], env

let to_model (ir, env : T.ir * env) : M.model * env =

  let rec for_type (t : T.type_) : M.type_ =
    let f = for_type in
    match t.node with
    | Tkey                  -> M.tkey
    | Tunit                 -> M.tunit
    | Tsignature            -> M.tsignature
    | Toption    t          -> M.toption (f t)
    | Tlist      t          -> M.tlist   (f t)
    | Tset       t          -> M.tset    (f t)
    | Toperation            -> M.toperation
    | Tcontract  t          -> M.tcontract (f t)
    | Tpair      (lt, rt)   -> M.ttuple [f lt; f rt]
    | Tor        (_lt, _rt) -> assert false
    | Tlambda    (at, rt)   -> M.tlambda (f at) (f rt)
    | Tmap       (kt, vt)   -> M.tmap (f kt) (f vt)
    | Tbig_map   (kt, vt)   -> M.tbig_map (f kt) (f vt)
    | Tchain_id             -> M.tchainid
    | Tint                  -> M.tint
    | Tnat                  -> M.tnat
    | Tstring               -> M.tstring
    | Tbytes                -> M.tbytes
    | Tmutez                -> M.ttez
    | Tbool                 -> M.tbool
    | Tkey_hash             -> M.tkeyhash
    | Ttimestamp            -> M.ttimestamp
    | Taddress              -> M.taddress
    | Tsapling_transaction  -> assert false
    | Tsapling_state        -> assert false
    | Tnever                -> assert false
    | Tbls12_381_g1         -> assert false
    | Tbls12_381_g2         -> assert false
    | Tbls12_381_fr         -> assert false
    | Tbaker_hash           -> assert false
    | Tbaker_operation      -> assert false
    | Tpvss_key             -> assert false
  in

  let for_data ?t (d : T.data) : M.mterm =
    let is_nat = Option.map_dfl (fun (t : T.type_) -> match t.node with | T.Tnat -> true | _ -> false) false in
    match d with
    | Dint v when is_nat t -> M.mk_bnat v
    | Dint    v        -> M.mk_bint v
    | Dstring v        -> M.mk_string v
    | Dbytes  v        -> M.mk_bytes v
    | Dunit            -> M.unit
    | Dtrue            -> M.mtrue
    | Dfalse           -> M.mfalse
    | Dpair  (_ld, _rd)-> assert false
    | Dleft   _d       -> assert false
    | Dright  _d       -> assert false
    | Dsome   _d       -> assert false
    | Dnone            -> assert false
    | Dlist  _l        -> assert false
    | Delt _           -> assert false
  in

  let rec for_instr (i : T.instruction) : M.mterm =
    let f = for_instr in
    match i with
    | Iseq []                      -> assert false
    | Iseq _l                      -> assert false
    | IletIn (_id, _v, _b, _)      -> assert false
    | Ivar _id                     -> assert false
    | Icall (_id, _args, _)        -> assert false
    | Iassign (id, v)              -> M.mk_mterm (M.Massign (ValueAssign, M.tunit, Avarstore (dumloc id), f v)) M.tunit
    | IassignRec (_id, _s, _n, _v) -> assert false
    | Iif (_c, _t, _e, _)          -> assert false
    | Iifnone (_v, _t, _id, _s)    -> assert false
    | Iifcons (_v, _t, _e)         -> assert false
    | Iwhile (_c, _b)              -> assert false
    | Iiter (_ids, _c, _b)         -> assert false
    | Izop op -> begin
        match op with
        | Znow                  -> assert false
        | Zamount               -> assert false
        | Zbalance              -> assert false
        | Zsource               -> assert false
        | Zsender               -> assert false
        | Zaddress              -> assert false
        | Zchain_id             -> assert false
        | Zself _               -> assert false
        | Zself_address         -> assert false
        | Znone _t              -> assert false
        | Zunit                 -> assert false
        | Znil _t               -> assert false
        | Zemptyset _t          -> assert false
        | Zemptymap (_k, _v)    -> assert false
        | Zemptybigmap (_k, _v) -> assert false
      end
    | Iunop (op, _e) -> begin
        match op with
        | Ucar               -> assert false
        | Ucdr               -> assert false
        | Uleft  _t          -> assert false
        | Uright _t          -> assert false
        | Uneg               -> assert false
        | Uint               -> assert false
        | Unot               -> assert false
        | Uabs               -> assert false
        | Uisnat             -> assert false
        | Usome              -> assert false
        | Usize              -> assert false
        | Upack              -> assert false
        | Uunpack _t         -> assert false
        | Ublake2b           -> assert false
        | Usha256            -> assert false
        | Usha512            -> assert false
        | Uhash_key          -> assert false
        | Ufail              -> assert false
        | Ucontract (_t, _a) -> assert false
        | Usetdelegate       -> assert false
        | Uimplicitaccount   -> assert false
        | Ueq                -> assert false
        | Une                -> assert false
        | Ugt                -> assert false
        | Uge                -> assert false
        | Ult                -> assert false
        | Ule                -> assert false
      end
    | Ibinop (op, _lhs, _rhs) -> begin
        match op with
        | Badd       -> assert false
        | Bsub       -> assert false
        | Bmul       -> assert false
        | Bediv      -> assert false
        | Blsl       -> assert false
        | Blsr       -> assert false
        | Bor        -> assert false
        | Band       -> assert false
        | Bxor       -> assert false
        | Bcompare   -> assert false
        | Bget       -> assert false
        | Bmem       -> assert false
        | Bconcat    -> assert false
        | Bcons      -> assert false
        | Bpair      -> assert false
      end
    | Iterop (op, _a1, _a2, _a3) -> begin
        match op with
        | Tcheck_signature -> assert false
        | Tslice           -> assert false
        | Tupdate          -> assert false
        | Ttransfer_tokens -> assert false
      end
    | Icompare (op, _lhs, _rhs) -> begin
        match op with
        | Ceq        -> assert false
        | Cne        -> assert false
        | Clt        -> assert false
        | Cgt        -> assert false
        | Cle        -> assert false
        | Cge        -> assert false
      end
    | Iconst (t, d)                     -> for_data ~t:t d
    | Iset (_t, _l)                     -> assert false
    | Ilist (_t, _l)                    -> assert false
    | Imap (_k, _v, _l)                 -> assert false
    | Irecord _l                        -> assert false
    | Irecupdate (_x, _s, _l)           -> assert false
    | Ifold (_ix, _iy, _ia, _c, _a, _b) -> assert false
    | Imichelson (_a, _c, _v)           -> assert false
  in

  let storage =
    let si = M.mk_storage_item (dumloc "storage") MTvar (for_type ir.storage_type) (for_data ~t:ir.storage_type ir.storage_data) in
    [si]
  in
  let for_entry (e : T.entry) : M.function__ =
    let name = dumloc e.name in
    let args = [] in
    let body = for_instr e.body in
    let fn : M.function_struct = M.mk_function_struct name body ~args:args in
    let node : M.function_node = M.Entry fn in
    M.mk_function node
  in
  let functions = List.map for_entry ir.entries in
  M.mk_model (dumloc env.name) ~functions:functions ~storage:storage, env

let to_archetype (model, _env : M.model * env) : A.archetype =
  let rec for_type (t : M.type_) : A.type_t =
    let f = for_type in
    match t with
    | Tasset id           -> A.tref (unloc id)
    | Tenum id            -> A.tref (unloc id)
    | Tstate              -> assert false
    | Tbuiltin Bunit      -> A.tunit
    | Tbuiltin Bbool      -> A.tbool
    | Tbuiltin Bint       -> A.tint
    | Tbuiltin Brational  -> A.trational
    | Tbuiltin Bdate      -> A.tdate
    | Tbuiltin Bduration  -> A.tduration
    | Tbuiltin Btimestamp -> assert false
    | Tbuiltin Bstring    -> A.tstring
    | Tbuiltin Baddress   -> A.taddress
    | Tbuiltin Brole      -> A.trole
    | Tbuiltin Bcurrency  -> A.ttez
    | Tbuiltin Bsignature -> A.tsignature
    | Tbuiltin Bkey       -> A.tkey
    | Tbuiltin Bkeyhash   -> A.tkey_hash
    | Tbuiltin Bbytes     -> A.tbytes
    | Tbuiltin Bnat       -> A.tnat
    | Tbuiltin Bchainid   -> A.tchain_id
    | Tcontainer (t, c)   -> A.mk_tcontainer (f t) (match c with | Collection -> assert false | Aggregate -> A.Aggregate | Partition -> A.Partition | View -> A.View)
    | Tlist t             -> A.mk_tlist (f t)
    | Toption t           -> A.mk_toption (f t)
    | Ttuple tl           -> A.mk_ttuple (List.map f tl)
    | Tset t              -> A.mk_tset (f t)
    | Tmap (_, kt, vt)    -> A.mk_tmap (f kt) (f vt)
    | Trecord id          -> A.tref (unloc id)
    | Tlambda _           -> assert false
    | Tunit               -> A.tunit
    | Tstorage            -> assert false
    | Toperation          -> A.toperation
    | Tcontract t         -> A.mk_tcontract (f t)
    | Tprog _             -> assert false
    | Tvset _             -> assert false
    | Ttrace _            -> assert false
  in

  let for_op = function
    | M.ValueAssign -> A.ValueAssign
    | M.PlusAssign  -> A.PlusAssign
    | M.MinusAssign -> A.MinusAssign
    | M.MultAssign  -> A.MultAssign
    | M.DivAssign   -> A.DivAssign
    | M.AndAssign   -> A.AndAssign
    | M.OrAssign    -> A.OrAssign
  in

  let for_temp = function
    | M.Tbefore -> Some (A.VLBefore)
    | M.Tat lbl -> Some (A.VLIdent (dumloc lbl))
    | M.Tnone   -> None
  in

  let for_delta = function
    | M.Dadded   -> Some (A.VSAdded)
    | M.Dremoved -> Some (A.VSRemoved)
    | M.Dunmoved -> Some (A.VSUnmoved)
    | M.Dnone    -> None
  in

  let rec for_expr (mt : M.mterm) : A.expr =
    let f = for_expr in
    match mt.node with
    (* lambda *)

    | Mletin (_ids, _a, _t, _b, _o) -> assert false
    | Mdeclvar (_ids, _t, _v)       -> assert false
    | Mapp (_e, _args)              -> assert false


    (* assign *)

    | Massign (op, _, Avar id, v)                  -> A.eassign (for_op op) (A.eterm id) (f v)
    | Massign (op, _, Avarstore id, v)             -> A.eassign (for_op op) (A.eterm id) (f v)
    | Massign (_op, _, Aasset (_an, _fn, _k), _v)  -> assert false
    | Massign (_op, _, Arecord (_rn, _fn, _r), _v) -> assert false
    | Massign (_op, _, Astate, _x)                 -> assert false
    | Massign (_op, _, Aassetstate (_an, _k), _v)  -> assert false
    | Massign (_op, _, Aoperations, _v)            -> assert false


    (* control *)

    | Mif (_c, _t, _e)           -> assert false
    | Mmatchwith (_e, _l)        -> assert false
    | Mfor (_i, _c, _b, _l)      -> assert false
    | Miter (_i, _a, _b, _c, _l) -> assert false
    | Mwhile (_c, _b, _l)        -> assert false
    | Mseq _is                   -> assert false
    | Mreturn _x                 -> assert false
    | Mlabel _i                  -> assert false
    | Mmark (_i, _x)             -> assert false


    (* effect *)

    | Mfail _ft          -> assert false
    | Mtransfer (_v, _k) -> assert false


    (* entrypoint *)

    | Mentrypoint (_t, _a, _s) -> assert false
    | Mself _id                -> assert false


    (* operation *)

    | Moperations               -> assert false
    | Mmkoperation (_v, _d, _a) -> assert false


    (* literals *)

    | Mint v             -> A.ebint v
    | Mnat v             -> A.ebnat v
    | Mbool true         -> A.etrue
    | Mbool false        -> A.efalse
    | Menum v            -> A.eterm (dumloc v)
    | Mrational (_n, _d) -> assert false
    | Mstring v          -> A.estring v
    | Mcurrency (_v, _c) -> assert false
    | Maddress v         -> A.eaddress v
    | Mdate _v           -> assert false
    | Mduration _v       -> assert false
    | Mtimestamp _v      -> assert false
    | Mbytes v           -> A.ebytes v
    | Munit              -> assert false


    (* control expression *)

    | Mexprif (_c, _t, _e)        -> assert false
    | Mexprmatchwith (_e, _l)     -> assert false
    | Mmatchsome (_e, _n, _i, _s) -> assert false


    (* composite type constructors *)

    | Mnone         -> assert false
    | Msome _v      -> assert false
    | Mtuple _l     -> assert false
    | Masset _l     -> assert false
    | Massets _l    -> assert false
    | Mlitset _l    -> assert false
    | Mlitlist _l   -> assert false
    | Mlitmap _l    -> assert false
    | Mlitrecord _l -> assert false

    (* access *)

    | Mdot (_e, _i)                 -> assert false
    | Mdotassetfield (_an, _k, _fn) -> assert false


    (* comparison operators *)

    | Mequal (_t, _l, _r)  -> assert false
    | Mnequal (_t, _l, _r) -> assert false
    | Mgt (_l, _r)         -> assert false
    | Mge (_l, _r)         -> assert false
    | Mlt (_l, _r)         -> assert false
    | Mle (_l, _r)         -> assert false
    | Mmulticomp (_e, _l)  -> assert false


    (* arithmetic operators *)

    | Mand (_l, _r)    -> assert false
    | Mor (_l, _r)     -> assert false
    | Mxor (_l, _r)    -> assert false
    | Mnot _e          -> assert false
    | Mplus (_l, _r)   -> assert false
    | Mminus (_l, _r)  -> assert false
    | Mmult (_l, _r)   -> assert false
    | Mdivrat (_l, _r) -> assert false
    | Mdiveuc (_l, _r) -> assert false
    | Mmodulo (_l, _r) -> assert false
    | Muminus _e       -> assert false


    (* asset api effect *)

    | Maddasset    (_an, _i)               -> assert false
    | Maddfield    (_an, _fn, _c, _i)      -> assert false
    | Mremoveasset (_an, _i)               -> assert false
    | Mremovefield (_an, _fn, _c, _i)      -> assert false
    | Mremoveall   (_an, _fn, _a)          -> assert false
    | Mremoveif    (_an, _c, _la, _lb, _a) -> assert false
    | Mclear       (_an, _v)               -> assert false
    | Mset         (_c,  _l, _k, _v)       -> assert false
    | Mupdate      (_an, _k, _l)           -> assert false
    | Maddupdate   (_an, _c, _k, _l)       -> assert false
    | Maddforce    (_an, _v)               -> assert false


    (* asset api expression *)

    | Mget      (_an, _c, _k)           -> assert false
    | Mselect   (_an, _c, _la, _lb, _a) -> assert false
    | Msort     (_an, _c, _l)           -> assert false
    | Mcontains (_an, _c, _i)           -> assert false
    | Mnth      (_an, _c, _i)           -> assert false
    | Mcount    (_an, _c)               -> assert false
    | Msum      (_an, _c, _p)           -> assert false
    | Mhead     (_an, _c, _i)           -> assert false
    | Mtail     (_an, _c, _i)           -> assert false


    (* utils *)

    | Mcast (_src, _dst, _v) -> assert false
    | Mtupleaccess (_x, _k)  -> assert false
    | Mrecupdate (_x, _l)    -> assert false


    (* set api expression *)

    | Msetadd (_t, _c, _a)                -> assert false
    | Msetremove (_t, _c, _a)             -> assert false
    | Msetcontains (_t, _c, _a)           -> assert false
    | Msetlength (_t, _c)                 -> assert false
    | Msetfold (_t, _ix, _ia, _c, _a, _b) -> assert false


    (* list api expression *)

    | Mlistprepend (_, _c, _a)             -> assert false
    | Mlistheadtail (_, _c)                -> assert false
    | Mlistlength (_, _c)                  -> assert false
    | Mlistcontains (_, _c, _a)            -> assert false
    | Mlistnth (_, _c, _a)                 -> assert false
    | Mlistreverse (_, _l)                 -> assert false
    | Mlistfold (_t, _ix, _ia, _c, _a, _b) -> assert false


    (* map api expression *)

    | Mmapput (_, _, _c, _k, _v)               -> assert false
    | Mmapremove (_, _, _c, _k)                -> assert false
    | Mmapget (_, _, _c, _k)                   -> assert false
    | Mmapgetopt (_, _, _c, _k)                -> assert false
    | Mmapcontains (_, _, _c, _k)              -> assert false
    | Mmaplength (_, _, _c)                    -> assert false
    | Mmapfold (_t, _ik, _iv, _ia, _c, _a, _b) -> assert false


    (* builtin functions *)

    | Mmax (_l, _r)       -> assert false
    | Mmin (_l, _r)       -> assert false
    | Mabs _a             -> assert false
    | Mconcat (_x, _y)    -> assert false
    | Mslice (_x, _s, _e) -> assert false
    | Mlength _x          -> assert false
    | Misnone _x          -> assert false
    | Missome _x          -> assert false
    | Moptget _x          -> assert false
    | Mfloor  _x          -> assert false
    | Mceil   _x          -> assert false
    | Mtostring (_, _x)   -> assert false
    | Mpack _x            -> assert false
    | Munpack (_t, _x)    -> assert false


    (* crypto functions *)

    | Mblake2b _x                  -> assert false
    | Msha256  _x                  -> assert false
    | Msha512  _x                  -> assert false
    | Mhashkey _x                  -> assert false
    | Mchecksignature (_k, _s, _x) -> assert false


    (* constants *)

    | Mnow           -> assert false
    | Mtransferred   -> assert false
    | Mcaller        -> assert false
    | Mbalance       -> assert false
    | Msource        -> assert false
    | Mselfaddress   -> assert false
    | Mchainid       -> assert false
    | Mmetadata      -> assert false


    (* variable *)

    | Mvar (_an, Vassetstate _k, _t, _d) -> assert false
    | Mvar(v, Vstorevar, t, d)           -> A.eterm v ?temp:(for_temp t) ?delta:(for_delta d)
    | Mvar(v, Vstorecol, t, d)           -> A.eterm v ?temp:(for_temp t) ?delta:(for_delta d)
    | Mvar(_v, Venumval, _t, _d)         -> assert false
    | Mvar(_v, Vdefinition, _t, _d)      -> assert false
    | Mvar(_v, Vlocal, _t, _d)           -> assert false
    | Mvar(_v, Vparam, _t, _d)           -> assert false
    | Mvar(_v, Vfield, _t, _d)           -> assert false
    | Mvar(_, Vthe, _t, _d)              -> assert false
    | Mvar(_, Vstate, _t, _d)            -> assert false


    (* rational *)

    | Mrateq (_l, _r)         -> assert false
    | Mratcmp (_op, _l, _r)   -> assert false
    | Mratarith (_op, _l, _r) -> assert false
    | Mratuminus _v           -> assert false
    | Mrattez (_c, _t)        -> assert false
    | Mnattoint _e            -> assert false
    | Mnattorat _e            -> assert false
    | Minttorat _e            -> assert false
    | Mratdur (_c, _t)        -> assert false


    (* quantifiers *)

    | Mforall (_i, _t, None, _e)    -> assert false
    | Mforall (_i, _t, Some _s, _e) -> assert false
    | Mexists (_i, _t, None, _e)    -> assert false
    | Mexists (_i, _t, Some _s, _e) -> assert false


    (* formula operators *)

    | Mimply (_l, _r)  -> assert false
    | Mequiv (_l, _r)  -> assert false


    (* formula asset collection *)

    | Msetiterated  _e -> assert false
    | Msettoiterate _e -> assert false


    (* formula asset collection methods *)

    | Mempty _an              -> assert false
    | Msingleton (_an, _k)    -> assert false
    | Msubsetof (_an, _c, _i) -> assert false
    | Misempty  (_l, _r)      -> assert false
    | Munion (_an, _l, _r)    -> assert false
    | Minter (_an, _l, _r)    -> assert false
    | Mdiff (_an, _l, _r)     -> assert false

  in

  let for_storage_item (si : M.storage_item) : A.declaration =
    let id = si.id in
    let t  = for_type si.typ in
    let dv = for_expr si.default in
    match si.model_type with
    | MTvar -> A.mk_variable (A.mk_variable_decl ~dv:dv id t VKvariable)
    | _ -> assert false
  in

  let for_fun (f : M.function__) : A.declaration =
    let exts = None in
    match f.node with
    | Function (_fs, _t)
    | Getter (_fs, _t) -> assert false
    | Entry fs -> begin
        let id = fs.name in
        let body = for_expr fs.body in

        let ep = A.mk_entry_properties () in
        let ed = A.mk_entry_decl id ep ~body:(body, exts) in

        A.mk_entry ed
      end
  in

  let decls =
    List.map for_storage_item model.storage
    @ List.map for_fun model.functions
  in

  A.mk_archetype () ~decls:((A.mk_darchetype model.name)::decls)
