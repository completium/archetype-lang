open Location
open Tools
open UF

module T = Michelson
module M = Model
module A = ParseTree

type env = {
  name: string;
}

let mk_env ?(name="") _ : env = { name }

let parse_micheline ?ijson (filename, ic) : T.obj_micheline * env =
  let name =
    match filename with
    | "<stdin>" -> "noname"
    | _ -> filename |> Filename.basename |> Filename.chop_extension
  in
  let env = mk_env ~name:name () in

  let input =
    if match ijson with | Some v -> v | _ -> !Options.opt_json  then
      let open Yojson.Safe in

      let is_tag s l = List.exists (fun x -> String.equal s (fst x)) l in

      let rec aux (i : t) : T.obj_micheline =
        match i with
        | `Assoc l when is_tag "prim" l -> begin
            let extract l =
              List.fold_left (
                fun (prim, args, annots) (id, json) ->
                  match id, json with
                  | "prim", `String s -> (s, args, annots)
                  | "args", `List l ->  (prim, List.map aux l, annots)
                  | "annots", `List l ->  (prim, args, List.map (function | `String s -> s | _ -> assert false) l)
                  | _ -> (prim, args, annots)
              ) ("", [], []) l
            in
            let prim, args, annots = extract l in
            let prim : T.prim = { prim = prim; args = args; annots = annots } in
            T.Oprim prim
          end
        | `Assoc l when is_tag "int" l -> begin
            let json = List.find (fun x -> String.equal "int" (fst x)) l in
            let s = match snd json with | `String s -> s | _ -> assert false in
            T.Oint s
          end
        | `Assoc l when is_tag "string" l -> begin
            let json = List.find (fun x -> String.equal "string" (fst x)) l in
            let s = match snd json with | `String s -> s | _ -> assert false in
            T.Ostring s
          end
        | `Assoc l when is_tag "bytes" l -> begin
            let json = List.find (fun x -> String.equal "bytes" (fst x)) l in
            let s = match snd json with | `String s -> s | _ -> assert false in
            T.Obytes s
          end
        | `List l -> T.Oarray (List.map aux l)
        | _ -> Format.printf "%s@." (to_string i); assert false
      in
      let open Util in
      let json = from_channel ic in
      let code = json |> member "code" |> to_list in
      T.Oarray (List.map aux code)
    else
      let tokens = Lexing.from_channel ic in
      Michelson_parser.main Michelson_lexer.token tokens
  in
  input, env

let to_michelson (input, env : T.obj_micheline * env) : T.michelson * env =
  let ff (input : T.obj_micheline) : T.michelson =
    let fa l = match l with | a::_ -> Some a | [] -> None in

    let to_type = T.to_type in
    let to_data = T.to_data in

    let to_int = function | T.Oint x -> int_of_string x | o -> Format.eprintf "to_int unknown %a@." T.pp_obj_micheline o; assert false in

    let rec to_code (o : T.obj_micheline) : T.code =
      let f = to_code in
      let seq = function | T.Oarray l -> List.map f l | _ -> assert false in
      match o with
      (* Control structures *)
      | Oarray l                                             -> T.SEQ (List.map f l)
      | Oprim ({prim = "APPLY"; _})                          -> T.APPLY
      | Oprim ({prim = "EXEC"; _})                           -> T.EXEC
      | Oprim ({prim = "FAILWITH"; _})                       -> T.FAILWITH
      | Oprim ({prim = "IF"; args = t::e::_; _})             -> T.IF        (seq t, seq e)
      | Oprim ({prim = "IF_CONS"; args = t::e::_; _})        -> T.IF_CONS   (seq t, seq e)
      | Oprim ({prim = "IF_LEFT"; args = t::e::_; _})        -> T.IF_LEFT   (seq t, seq e)
      | Oprim ({prim = "IF_NONE"; args = t::e::_; _})        -> T.IF_NONE   (seq t, seq e)
      | Oprim ({prim = "ITER"; args = l::_; _})              -> T.ITER      (seq l)
      | Oprim ({prim = "LAMBDA"; args = a::r::b; _})         -> T.LAMBDA    (to_type a, to_type r, List.map f b)
      | Oprim ({prim = "LOOP"; args = l::_; _})              -> T.LOOP      (seq l)
      | Oprim ({prim = "LOOP_LEFT"; args = l::_; _})         -> T.LOOP_LEFT (seq l)
      (* Stack manipulation *)
      | Oprim ({prim = "DIG"; args = n::_})                  -> T.DIG (to_int n)
      | Oprim ({prim = "DIG"; _})                            -> T.DIG 1
      | Oprim ({prim = "DIP"; args = n::l::_})               -> T.DIP (to_int n, seq l)
      | Oprim ({prim = "DIP"; args = l::_})                  -> T.DIP (1, seq l)
      | Oprim ({prim = "DROP"; args = n::_})                 -> T.DROP (to_int n)
      | Oprim ({prim = "DROP"; _})                           -> T.DROP 1
      | Oprim ({prim = "DUG"; args = n::_})                  -> T.DUG (to_int n)
      | Oprim ({prim = "DUG"; _})                            -> T.DUG 1
      | Oprim ({prim = "DUP"; _})                            -> T.DUP
      | Oprim ({prim = "PUSH"; args = t::v::_})              -> T.PUSH (to_type t, to_data v)
      | Oprim ({prim = "SWAP"; _})                           -> T.SWAP
      (* Arthmetic operations *)
      | Oprim ({prim = "ABS"; _})                            -> T.ABS
      | Oprim ({prim = "ADD"; _})                            -> T.ADD
      | Oprim ({prim = "COMPARE"; _})                        -> T.COMPARE
      | Oprim ({prim = "EDIV"; _})                           -> T.EDIV
      | Oprim ({prim = "EQ"; _})                             -> T.EQ
      | Oprim ({prim = "GE"; _})                             -> T.GE
      | Oprim ({prim = "GT"; _})                             -> T.GT
      | Oprim ({prim = "INT"; _})                            -> T.INT
      | Oprim ({prim = "ISNAT"; _})                          -> T.ISNAT
      | Oprim ({prim = "LE"; _})                             -> T.LE
      | Oprim ({prim = "LSL"; _})                            -> T.LSL
      | Oprim ({prim = "LSR"; _})                            -> T.LSR
      | Oprim ({prim = "LT"; _})                             -> T.LT
      | Oprim ({prim = "MUL"; _})                            -> T.MUL
      | Oprim ({prim = "NEG"; _})                            -> T.NEG
      | Oprim ({prim = "NEQ"; _})                            -> T.NEQ
      | Oprim ({prim = "SUB"; _})                            -> T.SUB
      (* Boolean operations *)
      | Oprim ({prim = "AND"; _})                            -> T.AND
      | Oprim ({prim = "NOT"; _})                            -> T.NOT
      | Oprim ({prim = "OR"; _})                             -> T.OR
      | Oprim ({prim = "XOR"; _})                            -> T.XOR
      (* Cryptographic operations *)
      | Oprim ({prim = "BLAKE2B"; _})                        -> T.BLAKE2B
      | Oprim ({prim = "CHECK_SIGNATURE"; _})                -> T.CHECK_SIGNATURE
      | Oprim ({prim = "HASH_KEY"; _})                       -> T.HASH_KEY
      | Oprim ({prim = "SHA256"; _})                         -> T.SHA256
      | Oprim ({prim = "SHA512"; _})                         -> T.SHA512
      (* Blockchain operations *)
      | Oprim ({prim = "ADDRESS"; _})                        -> T.ADDRESS
      | Oprim ({prim = "AMOUNT"; _})                         -> T.AMOUNT
      | Oprim ({prim = "BALANCE"; _})                        -> T.BALANCE
      | Oprim ({prim = "CHAIN_ID"; _})                       -> T.CHAIN_ID
      | Oprim ({prim = "CONTRACT"; args = t::_; annots = a}) -> T.CONTRACT (to_type t, fa a)
      | Oprim ({prim = "CREATE_CONTRACT"; args = a::_; _})   -> begin
          let seek tag a =
            let rec aux tag accu (a : T.obj_micheline) =
              match a with
              | Oprim {prim=a; args=arg::_; _} when String.equal tag a -> Some arg
              | Oarray l -> List.fold_left (fun accu x -> match accu with | Some _ -> accu | None -> aux tag accu x) accu l
              | _ -> None
            in
            match aux tag None a with
            | Some a -> a
            | None -> assert false
          in
          let p = seek "parameter" a in
          let s = seek "storage" a in
          let c = seek "code" a in
          T.CREATE_CONTRACT (to_type p, to_type s, f c)
        end
      | Oprim ({prim = "IMPLICIT_ACCOUNT"; _})               -> T.IMPLICIT_ACCOUNT
      | Oprim ({prim = "NOW"; _})                            -> T.NOW
      | Oprim ({prim = "SELF"; annots = a; _})               -> T.SELF (fa a)
      | Oprim ({prim = "SENDER"; _})                         -> T.SENDER
      | Oprim ({prim = "SET_DELEGATE"; _})                   -> T.SET_DELEGATE
      | Oprim ({prim = "SOURCE"; _})                         -> T.SOURCE
      | Oprim ({prim = "TRANSFER_TOKENS"; _})                -> T.TRANSFER_TOKENS
      (* Operations on data structures *)
      | Oprim ({prim = "CAR"; _})                            -> T.CAR
      | Oprim ({prim = "CDR"; _})                            -> T.CDR
      | Oprim ({prim = "CONCAT"; _})                         -> T.CONCAT
      | Oprim ({prim = "CONS"; _})                           -> T.CONS
      | Oprim ({prim = "EMPTY_BIG_MAP" ; args = k::v::_})    -> T.EMPTY_BIG_MAP (to_type k, to_type v)
      | Oprim ({prim = "EMPTY_MAP" ; args = k::v::_})        -> T.EMPTY_MAP (to_type k, to_type v)
      | Oprim ({prim = "EMPTY_SET" ; args = t::_})           -> T.EMPTY_SET (to_type t)
      | Oprim ({prim = "GET"; _})                            -> T.GET
      | Oprim ({prim = "LEFT" ; args = t::_})                -> T.LEFT (to_type t)
      | Oprim ({prim = "MAP"; args = s::_})                  -> T.MAP (seq s)
      | Oprim ({prim = "MEM"; _})                            -> T.MEM
      | Oprim ({prim = "NIL" ; args = t::_})                 -> T.NIL (to_type t)
      | Oprim ({prim = "NONE" ; args = t::_})                -> T.NONE (to_type t)
      | Oprim ({prim = "PACK"; _})                           -> T.PACK
      | Oprim ({prim = "PAIR"; _})                           -> T.PAIR
      | Oprim ({prim = "RIGHT" ; args = t::_})               -> T.RIGHT (to_type t)
      | Oprim ({prim = "SIZE"; _})                           -> T.SIZE
      | Oprim ({prim = "SLICE"; _})                          -> T.SLICE
      | Oprim ({prim = "SOME"; _})                           -> T.SOME
      | Oprim ({prim = "UNIT"; _})                           -> T.UNIT
      | Oprim ({prim = "UNPACK" ; args = t::_})              -> T.UNPACK (to_type t)
      | Oprim ({prim = "UPDATE"; _})                         -> T.UPDATE
      (* Other *)
      | Oprim ({prim = "UNPAIR"; _})                         -> T.UNPAIR
      | Oprim ({prim = "SELF_ADDRESS"; _})                   -> T.SELF_ADDRESS
      | Oprim ({prim = "CAST"; args = t::_})                 -> T.CAST (to_type t)
      | Oprim ({prim = "CREATE_ACCOUNT"; _})                 -> T.CREATE_ACCOUNT
      | Oprim ({prim = "RENAME"; _})                         -> T.RENAME
      | Oprim ({prim = "STEPS_TO_QUOTA"; _})                 -> T.STEPS_TO_QUOTA
      | Oprim ({prim = "LEVEL"; _})                          -> T.LEVEL
      | Oprim ({prim = "SAPLING_EMPTY_STATE"; args = (Oint n)::_}) -> T.SAPLING_EMPTY_STATE (int_of_string n)
      | Oprim ({prim = "SAPLING_VERIFY_UPDATE"; _})          -> T.SAPLING_VERIFY_UPDATE
      | Oprim ({prim = "NEVER"; _})                          -> T.NEVER
      | Oprim ({prim = "VOTING_POWER"; _})                   -> T.VOTING_POWER
      | Oprim ({prim = "TOTAL_VOTING_POWER"; _})             -> T.TOTAL_VOTING_POWER
      | Oprim ({prim = "KECCAK"; _})                         -> T.KECCAK
      | Oprim ({prim = "SHA3"; _})                           -> T.SHA3
      | Oprim ({prim = "PAIRING_CHECK"; _})                  -> T.PAIRING_CHECK
      | Oprim ({prim = "SUBMIT_PROPOSALS"; _})               -> T.SUBMIT_PROPOSALS
      | Oprim ({prim = "SUBMIT_BALLOT"; _})                  -> T.SUBMIT_BALLOT
      | Oprim ({prim = "SET_BAKER_ACTIVE"; _})               -> T.SET_BAKER_ACTIVE
      | Oprim ({prim = "TOGGLE_BAKER_DELEGATIONS"; _})       -> T.TOGGLE_BAKER_DELEGATIONS
      | Oprim ({prim = "SET_BAKER_CONSENSUS_KEY"; _})        -> T.SET_BAKER_CONSENSUS_KEY
      | Oprim ({prim = "SET_BAKER_PVSS_KEY"; _})             -> T.SET_BAKER_PVSS_KEY
      (* Macro *)
      | Oprim ({prim = "IFCMPEQ"; args = [l; r]})            -> T.SEQ [COMPARE; EQ; T.IF (seq l, seq r)]
      | Oprim ({prim = "IFCMPNEQ"; args = [l; r]})           -> T.SEQ [COMPARE; NEQ; T.IF (seq l, seq r)]
      | Oprim ({prim = "IFCMPLT"; args = [l; r]})            -> T.SEQ [COMPARE; LT; T.IF (seq l, seq r)]
      | Oprim ({prim = "IFCMPGT"; args = [l; r]})            -> T.SEQ [COMPARE; GT; T.IF (seq l, seq r)]
      | Oprim ({prim = "IFCMPLE"; args = [l; r]})            -> T.SEQ [COMPARE; LE; T.IF (seq l, seq r)]
      | Oprim ({prim = "IFCMPGE"; args = [l; r]})            -> T.SEQ [COMPARE; GE; T.IF (seq l, seq r)]

      | Oprim ({prim = "IFEQ"; args = [l; r]})               -> T.SEQ [EQ; T.IF (seq l, seq r)]
      | Oprim ({prim = "IFNEQ"; args = [l; r]})              -> T.SEQ [NEQ; T.IF (seq l, seq r)]
      | Oprim ({prim = "IFLT"; args = [l; r]})               -> T.SEQ [LT; T.IF (seq l, seq r)]
      | Oprim ({prim = "IFGT"; args = [l; r]})               -> T.SEQ [GT; T.IF (seq l, seq r)]
      | Oprim ({prim = "IFLE"; args = [l; r]})               -> T.SEQ [LE; T.IF (seq l, seq r)]
      | Oprim ({prim = "IFGE"; args = [l; r]})               -> T.SEQ [GE; T.IF (seq l, seq r)]

      | Oprim ({prim = "CMPEQ"; _})                          -> T.SEQ [COMPARE; EQ]
      | Oprim ({prim = "CMPNEQ"; _})                         -> T.SEQ [COMPARE; NEQ]
      | Oprim ({prim = "CMPLT"; _})                          -> T.SEQ [COMPARE; LT]
      | Oprim ({prim = "CMPGT"; _})                          -> T.SEQ [COMPARE; GT]
      | Oprim ({prim = "CMPLE"; _})                          -> T.SEQ [COMPARE; LE]
      | Oprim ({prim = "CMPGE"; _})                          -> T.SEQ [COMPARE; GE]

      | Oprim ({prim = "ASSERT"; _})                         -> T.IF ([], [UNIT; FAILWITH])
      | Oprim ({prim = "ASSERT_NONE"; _})                    -> T.IF_NONE ([], [UNIT; FAILWITH])
      | Oprim ({prim = "ASSERT_SOME"; _})                    -> T.IF_NONE ([UNIT; FAILWITH], [])
      | Oprim ({prim = "ASSERT_LEFT"; _})                    -> T.IF_LEFT ([], [UNIT; FAILWITH])
      | Oprim ({prim = "ASSERT_RIGHT"; _})                   -> T.IF_LEFT ([UNIT; FAILWITH], [])

      | Oprim ({prim = "ASSERT_CMPEQ"; _})                   -> T.SEQ [COMPARE; EQ; IF ([], [UNIT; FAILWITH])]
      | Oprim ({prim = "ASSERT_CMPNEQ"; _})                  -> T.SEQ [COMPARE; NEQ; IF ([], [UNIT; FAILWITH])]
      | Oprim ({prim = "ASSERT_CMPLT"; _})                   -> T.SEQ [COMPARE; LT; IF ([], [UNIT; FAILWITH])]
      | Oprim ({prim = "ASSERT_CMPGT"; _})                   -> T.SEQ [COMPARE; GT; IF ([], [UNIT; FAILWITH])]
      | Oprim ({prim = "ASSERT_CMPLE"; _})                   -> T.SEQ [COMPARE; LE; IF ([], [UNIT; FAILWITH])]
      | Oprim ({prim = "ASSERT_CMPGE"; _})                   -> T.SEQ [COMPARE; GE; IF ([], [UNIT; FAILWITH])]

      | Oprim ({prim = "ASSERT_EQ"; _})                      -> T.SEQ [EQ; IF ([], [UNIT; FAILWITH])]
      | Oprim ({prim = "ASSERT_NEQ"; _})                     -> T.SEQ [NEQ; IF ([], [UNIT; FAILWITH])]
      | Oprim ({prim = "ASSERT_LT"; _})                      -> T.SEQ [LT; IF ([], [UNIT; FAILWITH])]
      | Oprim ({prim = "ASSERT_GT"; _})                      -> T.SEQ [GT; IF ([], [UNIT; FAILWITH])]
      | Oprim ({prim = "ASSERT_LE"; _})                      -> T.SEQ [LE; IF ([], [UNIT; FAILWITH])]
      | Oprim ({prim = "ASSERT_GE"; _})                      -> T.SEQ [GE; IF ([], [UNIT; FAILWITH])]

      | _ -> Format.eprintf "code unknown: %a@." T.pp_obj_micheline o; assert false
    in
    let seek i l : T.obj_micheline = List.find T.(function | Oprim ({prim = p; _}) -> String.equal i p | _ -> false) l in
    let get_arg = function | T.Oprim ({args=x::_; _}) -> x | _ -> assert false in

    let l = input |> (function | Oarray l -> l | _ -> assert false) in
    let storage   = l |> seek "storage"   |> get_arg |> to_type in
    let parameter = l |> seek "parameter" |> get_arg |> to_type in
    let code      = l |> seek "code"      |> get_arg |> to_code in
    T.mk_michelson storage parameter (Michelson.Utils.flat code)
  in
  ff input, env

(* -------------------------------------------------------------------- *)

(*
let mk_ir_env ?(cpt_alpha=0) ?(deep=0) ?(fail=false) _ : ir_env =
  { cpt_alpha; deep; fail }

let inc_deep (env : ir_env) : ir_env = { env with deep = env.deep + 1 }
let dec_deep (env : ir_env) : ir_env = { env with deep = env.deep - 1 }

let _to_dir (michelson, env : T.michelson * env) =

  let rec interp (env : ir_env) (sys : T.dinstruction list) (instrs : T.code list) (stack : (T.dexpr) list) =
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

    let interp_if k (th, el) it =

      let env = inc_deep env in
      let scopes = env.scopes in
      let env = { env with scopes = sys::scopes } in
      let sys_then, stack_then, env_then = interp env [] (List.rev th) stack in
      let sys_else, stack_else, env_else = interp { env with cpt_alpha = env_then.cpt_alpha } [] (List.rev el) stack in


      let env = { env with scopes = scopes; cpt_alpha = env_else.cpt_alpha } in

      (* Format.printf "stack:@\n%a@\n@." pp_stack stack; *)
      (* Format.printf "_stack_then:@\n%a@\n@." pp_stack stack_then; *)
      (* Format.printf "_stack_else:@\n%a@\n@." pp_stack stack_else; *)

      let replace_alpha src dst (sys : T.dinstruction list) : T.dinstruction list =
        let fai x = if (x = src) then dst else x in
        let rec fe x = T.map_dexpr        fe fi fai x
        and fi i = T.map_dexpr_dinstr fi fe fai i
        in
        List.map fi sys
      in

      let stack_in, sys_then, stack_then, sys_else, stack_else =
        match env_then.fail, env_else.fail with
        | false, false -> begin
            let doit sys_then stack_then sys_else stack_else =
              let lt = List.length stack_then in
              let le = List.length stack_else in
              if lt <> le
              then assert false; (* TODO *)
              let sys_else, stack_else = List.fold_left2 (fun (sys_else, stack_else) t e ->
                  match t, e with
                  | _ when T.cmp_dexpr t e -> (sys_else, e::stack_else)
                  | T.Dalpha i1, T.Dalpha i2 -> (replace_alpha i2 i1 sys_else, t::stack_else)
                  | _ -> (sys_else, t::stack_else)
                ) (sys_else, []) stack_then stack_else in
              sys_then, stack_then, sys_else, stack_else
            in

            let syt, stt, sye, ste =
              match k, stack_then, stack_else  with
              | `Base, stt, ste -> begin
                  let syt, stt, sye, ste = doit sys_then stt sys_else ste in
                  syt, stt, sye, ste
                end
              | `Cons, a::b::stt, ste -> begin
                  let syt, stt, sye, ste = doit sys_then stt sys_else ste in
                  syt, a::b::stt, sye, ste
                end
              | `Left, a::stt, b::ste -> begin
                  let syt, stt, sye, ste = doit sys_then stt sys_else ste in
                  syt, a::stt, sye, b::ste
                end
              | `None, stt, a::ste -> begin
                  let syt, stt, sye, ste = doit sys_then stt sys_else ste in
                  syt, stt, sye, a::ste
                end
              | _ -> assert false
            in
            stt, syt, stt, sye, ste
          end
        | true,  false -> stack_else, sys_then, stack_then, sys_else, stack_else
        | false, true  -> stack_then, sys_then, stack_then, sys_else, stack_else
        | true,  true  -> assert false
      in

      let get_id_first_stack st =
        match st with
        | (T.Dalpha n)::_ -> n
        | _ -> assert false
      in

      let gst _ = get_id_first_stack stack_then in
      let gse _ = get_id_first_stack stack_else in

      let fif =
        match k with
        | `Base -> fun (x, y, z) -> T.Dif     (x, y, z)
        | `Cons -> fun (x, y, z) -> T.Difcons (x, gst (), gse (), y, z)
        | `Left -> fun (x, y, z) -> T.Difleft (x, gst (), y, gse (), z)
        | `None -> fun (x, y, z) -> T.Difnone (x, y, gse (), z)
      in

      let x = T.dalpha env.cpt_alpha in
      let env = inc_cpt_alpha env in

      let sys = add_instruction env sys (fif (x, sys_then, sys_else)) in

      f env sys it (x::stack_in)
    in

    trace env instrs stack sys;

    match instrs with

    (* Control structures *)

    | SEQ l::it -> begin
        f env sys (it @ List.rev l) stack
      end
    | APPLY::it -> begin
        match stack with
        | a::st ->
          let x = T.dalpha env.cpt_alpha in
          let env = inc_cpt_alpha env in
          let y = T.dalpha env.cpt_alpha in
          let env = inc_cpt_alpha env in
          f env (add_instruction env sys (T.Dassign (a, Dapply (y, x)))) it (x::y::st)
        | _ -> emit_error ()
      end
    | EXEC::it -> begin
        match stack with
        | a::st ->
          let x = T.dalpha env.cpt_alpha in
          let env = inc_cpt_alpha env in
          let y = T.dalpha env.cpt_alpha in
          let env = inc_cpt_alpha env in
          f env (add_instruction env sys (T.Dassign (a, Dexec (y, x)))) it (x::y::st)
        | _ -> emit_error ()
      end
    | FAILWITH::it -> begin
        let env = {env with fail = true} in
        let x = T.dalpha env.cpt_alpha in
        let env = inc_cpt_alpha env in
        f env (add_instruction env sys (T.Dfail x)) it (x::stack)
      end
    | IF      (th, el)::it -> interp_if `Base (th, el) it
    | IF_CONS (th, el)::it -> interp_if `Cons (th, el) it
    | IF_LEFT (th, el)::it -> interp_if `Left (th, el) it
    | IF_NONE (th, el)::it -> interp_if `None (th, el) it
    | ITER cs::it -> begin
        let c = T.dalpha env.cpt_alpha in
        let env = inc_cpt_alpha env in
        let stack = (c::stack) in
        let body =
          let instrs, _stack, _env = interp env [] (List.rev cs) stack in
          instrs
        in
        f env (add_instruction env sys (T.Diter (c , body))) it stack
      end
    | LAMBDA (rt, at, instrs)::it -> begin
        match stack with
        | a::t -> begin
            let instrs =
              let env = mk_ir_env () in
              let stack = [T.Dlbdresult] in
              let body, stack, env = interp env [] (List.rev instrs) stack in
              let body =
                match stack with
                | [x] -> add_instruction env body (T.Dassign (x, T.Dlbdparam))
                | _ -> emit_error ()
              in
              trace env instrs stack sys;
              body
            in
            let accu = add_instruction env sys (T.Dassign (a, T.Dlambda (at, rt, instrs))) in
            f env accu it t
          end
        | _ -> emit_error ()
      end
    | LOOP cs::it -> begin
        let c = T.dalpha env.cpt_alpha in
        let env = inc_cpt_alpha env in
        let stack = (c::stack) in
        let body =
          let instrs, _stack, _env = interp env [] (List.rev cs) stack in
          instrs
        in
        f env (add_instruction env sys (T.Dloop (c , body))) it stack
      end
    | LOOP_LEFT cs::it -> begin
        match stack with
        | a::st -> begin
            let x = T.dalpha env.cpt_alpha in
            let env = inc_cpt_alpha env in
            let stack = (x::st) in
            let body =
              let instrs, _stack, _env = interp env [] (List.rev cs) stack in
              instrs
            in
            f env (add_instruction env sys (T.Dassign (a, (T.Dloopleft (x , body))))) it stack
          end
        | _ -> emit_error ()
      end


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
    | SELF a::it             -> interp_zop env (Zself a) it stack
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
    | MAP cs::it               ->  begin
        match stack with
        | a::st -> begin
            let x = T.dalpha env.cpt_alpha in
            let env = inc_cpt_alpha env in
            let stack = (x::st) in
            let body =
              let instrs, _stack, _env = interp env [] (List.rev cs) stack in
              instrs
            in
            f env (add_instruction env sys (T.Dassign (a, (T.Dmap (x , body))))) it stack
          end
        | _ -> emit_error ()
      end
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


    (* Operations on tickets *)

    | JOIN_TICKETS::it         -> interp_uop env Ujointickets  it stack
    | READ_TICKET::it          -> interp_uop env Ureadticket   it stack
    | SPLIT_TICKET::it         -> interp_bop env Bsplitticket  it stack
    | TICKET::it               -> interp_bop env Bcreateticket it stack


    (* Other *)

    | UNPAIR::it  -> begin
        match stack with
        | x::y::st -> f env sys it (T.Dbop (Bpair, x, y)::st)
        | _ -> emit_error ()
      end
    | SELF_ADDRESS::it   -> interp_zop env (Zself_address) it stack

    | CAST _::_                   -> assert false
    | CREATE_ACCOUNT::_           -> assert false
    | RENAME::_                   -> assert false
    | STEPS_TO_QUOTA::_           -> assert false
    | LEVEL::_                    -> assert false
    | SAPLING_EMPTY_STATE _::_    -> assert false
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
  in *)
(*
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
  (T.mk_dprogram tstorage tparameter storage_data name sys), env *)

(* -------------------------------------------------------------------- *)

module Decomp_dir : sig
  val decompile : T.michelson -> T.dcode
end = struct
  open Michelson

  (* ------------------------------------------------------------------ *)
  let gen () = Oo.id (object end)

  type dvar = [
  | `VLocal  of int
  | `VGlobal of string
  ]

  let vlocal  () : dvar = `VLocal (gen ())
  let vglobal x  : dvar = `VGlobal x

  let as_vlocal (x : dvar) =
    match x with `VLocal i -> i | _ -> assert false

  let rec pp_rstack1 fmt (x : rstack1) =
    match x with
    | `VLocal  i -> Format.fprintf fmt "#%d" i
    | `VGlobal n -> Format.fprintf fmt "%s" n

    | `Paired(x, y) ->
        Format.fprintf fmt "(%a, %a)" pp_rstack1 x pp_rstack1 y

  let pp_rstack fmt (x : rstack) =
    Format.fprintf fmt "[%a]" (Printer_tools.pp_list ", " pp_rstack1) x

  let as_dvar (x : rstack1) : dvar =
    match x with
    | #dvar as x -> x
    | _ -> assert false

  let rec write_var (e : dexpr) (x : rstack1) =
    match x, e with
    | #dvar as x, e -> [DIAssign (x, e)]
    | `Paired (x1, x2), Depair (e1, e2) ->
        let a = vlocal () in
          write_var e1 (a :> rstack1)
        @ write_var e2 x2
        @ write_var (Dvar a) x1
    | _ -> assert false

  let rec dexpr_of_rstack1 (x : rstack1) =
    match x with
    | #dvar as x -> Dvar x
    | `Paired (x, y) -> Depair (dexpr_of_rstack1 x, dexpr_of_rstack1 y)

  let rec merge_rstack (s1 : rstack) (s2 : rstack) =
    assert (List.length s1 = List.length s2);

    match s1, s2 with
    | (#dvar as x) :: s1, (#dvar as y) :: s2 ->
        let (is1, is2), s = merge_rstack s1 s2 in
        let a = vlocal () in
        (([DIAssign (a, Dvar x)] @ is1), is2), y :: s

    | `Paired (x1, y1) :: s1, `Paired (x2, y2) :: s2 ->
        merge_rstack (x1 :: y1 :: s1) (x2 :: y2 :: s2)

    | `Paired (x1, y1) :: s1, (#dvar as xy2) :: s2 ->
        let i1 = write_var (dexpr_of_rstack1 (`Paired (x1, y1))) xy2 in
        let (is1, is2), s = merge_rstack s1 s2 in
        ((i1 @ is1), is2), `Paired (x1, y1) :: s

    | #dvar :: _, `Paired _ :: _ ->
        let (is2, is1), s = merge_rstack s2 s1 in
        (is1, is2), s

    | [], [] ->
        ([], []), []

    | _, _ -> assert false

  let merge_rstack (s1 : rstack) (s2 : rstack) =
    merge_rstack s1 s2

  let rec dptn_of_rstack1 (r : rstack1) =
    match r with
    | `Paired (r1, r2) ->
      let p1, c1 = dptn_of_rstack1 r1 in
      let p2, c2 = dptn_of_rstack1 r2 in
      (DPair (p1, p2), c1 @ c2)

    | `VLocal x ->
      (DVar x, [])
 
    | (`VGlobal _) as n ->
      let x = gen () in (DVar x, [DIAssign (n, Dvar (`VLocal x))])

  exception UnificationFailure

  let unify_dvar (uf : UF.uf) (x : dvar) (y : dvar) =
    match x, y with
    | `VGlobal m, `VGlobal n ->
        if m <> n then raise UnificationFailure

    | `VLocal i, `VLocal j ->
        ignore (UF.union uf i j : int)

    | _, _ ->
        raise UnificationFailure

  let rec unify_dexpr (uf : UF.uf) (e1 : dexpr) (e2 : dexpr) =
    match e1, e2 with
    | Dvar x, Dvar y ->
        unify_dvar uf x y

    | Depair (e1, e2), Depair (f1, f2) ->
        unify_dexpr uf e1 f1;
        unify_dexpr uf e2 f2

    | Ddata d1, Ddata d2 ->
        if d1 <> d2 then
          raise UnificationFailure

    | Dfun (o1, es1), Dfun (o2, es2) ->
        if o1 <> o2 || List.length es1 <> List.length es2 then
          raise UnificationFailure;
        List.iter2 (unify_dexpr uf) es1 es2

    | _, _ ->
        raise UnificationFailure

  let unify_dinstr (uf : UF.uf) (i1 : dinstr) (i2 : dinstr) =
    match i1, i2 with
    | DIAssign (x1, e1), DIAssign (x2, e2) ->
        unify_dvar  uf x1 x2;
        unify_dexpr uf e1 e2

    | _, _ -> raise UnificationFailure

  let unify_dcode (uf : UF.uf) (is1 : dcode) (is2 : dcode) =
    if List.length is1 <> List.length is2 then
      raise UnificationFailure;
    List.iter2 (unify_dinstr uf) is1 is2

  let dvar_apply_uf (uf : UF.uf) (x : dvar) : dvar =
    match x with
    | `VGlobal _ -> x
    | `VLocal  i -> `VLocal (UF.find uf i)

  let rec dexpr_apply_uf (uf : UF.uf) (e : dexpr) : dexpr =
    match e with
    | Dvar x ->
        Dvar (dvar_apply_uf uf x)

    | Depair (e1, e2) ->
        let e1 = dexpr_apply_uf uf e1 in
        let e2 = dexpr_apply_uf uf e2 in
        Depair (e1, e2)

    | Ddata _ ->
        e

    | Dfun (o, es) ->
        Dfun (o, List.map (dexpr_apply_uf uf) es)

  let dpattern_apply_uf (_uf : UF.uf) (p : dpattern) : dpattern =
    p

  let rec dinstr_apply_uf (uf : UF.uf) (is : dinstr) : dinstr =
    match is with
    | DIAssign (x, e) ->
        DIAssign (dvar_apply_uf uf x, dexpr_apply_uf uf e)

    | DIIf (e, (c1, c2)) ->
        let e  = dexpr_apply_uf uf e  in
        let c1 = dcode_apply_uf uf c1 in
        let c2 = dcode_apply_uf uf c2 in
        DIIf (e, (c1, c2))

    | DIMatch (e, bs) ->
        let for_branch (x, ps, c) =
          let ps = List.map (dpattern_apply_uf uf) ps in
          let c  = dcode_apply_uf uf c in
          (x, ps, c) in

        let e  = dexpr_apply_uf uf e in
        let bs = List.map for_branch bs in
        DIMatch (e, bs)

    | DIFailwith e ->
        DIFailwith (dexpr_apply_uf uf e)

    | DIWhile (e, c) ->
        let e = dexpr_apply_uf uf e in
        let c = dcode_apply_uf uf c in
        DIWhile (e, c)

    | DIIter (x, e, c) ->
        let x = dvar_apply_uf  uf x in
        let e = dexpr_apply_uf uf e in
        let c = dcode_apply_uf uf c in
        DIIter (x, e, c)

  and dcode_apply_uf (uf : UF.uf) (is : dcode) : dcode =
    List.map (dinstr_apply_uf uf) is

  let rec rstack1_apply_uf (uf : UF.uf) (r : rstack1) : rstack1 =
    match r with
    | `Paired (r1, r2) ->
        let r1 = rstack1_apply_uf uf r1 in
        let r2 = rstack1_apply_uf uf r2 in
        `Paired (r1, r2)

    | #dvar as r ->
        (dvar_apply_uf uf r :> rstack1)

  let rstack_apply_uf (uf : UF.uf) (r : rstack) : rstack =
    List.map (rstack1_apply_uf uf) r

  (* ------------------------------------------------------------------ *)
  type decomp = {
    stack   : rstack;
    code    : dcode;
    failure : bool;
  }

  let mkdecomp ?(failure = false) stack code =
    { code; stack; failure; }

  let rec decompile_i (s : rstack) (i : code) : decomp =
    match i with

    (* Control structures *)

    | SEQ l -> decompile_s s l

    | IF (c1, c2) -> begin
        let { failure = f1; stack = s1; code = b1; } = decompile_s s c1 in
        let { failure = f2; stack = s2; code = b2; } = decompile_s s c2 in

        let (pr1, pr2), s =
          match f1, f2 with
          | false, false -> merge_rstack s1 s2
          | true , false -> ([], []), s2
          | false, true  -> ([], []), s1
          | true , true  -> assert false in
        let x = vlocal () in
        mkdecomp ((x :> rstack1) :: s) [DIIf (Dvar x, (pr1 @ b1, pr2 @ b2))]
      end

    (* Stack manipulation *)

    | DIG n ->
      assert (List.length s >= n + 1);
      let x, s1 = List.hd s, List.tl s in
      let s1, s2 = List.cut n s1 in
      mkdecomp (s1 @ (x :: s2)) []

    | DIP (n, c) ->
      assert (List.length s >= n);
      let s1, s2 = List.cut n s in
      let { failure; stack = s2; code = ops; } = decompile_s s2 c in
      mkdecomp ~failure (s1 @ s2) ops

    | DROP n ->
      let pre = List.init n (fun _ -> (vlocal () :> rstack1)) in
      mkdecomp (pre @ s) []

    | DUG n ->
      assert (List.length s >= n + 1);
      let s1, s2 = List.cut n s in
      let x, s2 = List.hd s2, List.tl s2 in
      mkdecomp (x :: (s1 @ s2)) []

    | DUP ->
      let x, s = List.pop s in
      let y, s = List.pop s in
      let a = vlocal () in
      let wri1 = write_var (Dvar a) x in
      let wri2 = write_var (Dvar a) y in
      mkdecomp ((a :> rstack1) :: s) (wri1 @ wri2)

    | PUSH (_t, d) ->
        let x, s = List.pop s in
        let wri = write_var (Ddata d) x in
        mkdecomp s wri

    | SWAP ->
        let x, s = List.pop s in
        let y, s = List.pop s in
        mkdecomp (y :: x :: s) []

    (* Arthmetic operations *)

    | ABS      -> decompile_op s (`Uop Uabs     )
    | ADD      -> decompile_op s (`Bop Badd     )
    | COMPARE  -> decompile_op s (`Bop Bcompare )
    | EDIV     -> decompile_op s (`Bop Bediv    )
    | EQ       -> decompile_op s (`Uop Ueq      )
    | GE       -> decompile_op s (`Uop Uge      )
    | GT       -> decompile_op s (`Uop Ugt      )
    | INT      -> decompile_op s (`Uop Uint     )
    | ISNAT    -> decompile_op s (`Uop Uisnat   )
    | LE       -> decompile_op s (`Uop Ule      )
    | LSL      -> decompile_op s (`Bop Blsl     )
    | LSR      -> decompile_op s (`Bop Blsr     )
    | LT       -> decompile_op s (`Uop Ult      )
    | MUL      -> decompile_op s (`Bop Bmul     )
    | NEG      -> decompile_op s (`Uop Uneg     )
    | NEQ      -> decompile_op s (`Uop Une      )
    | SUB      -> decompile_op s (`Bop Bsub     )


    (* Boolean operations *)

    | AND     -> decompile_op s (`Bop Band )
    | NOT     -> decompile_op s (`Uop Unot )
    | OR      -> decompile_op s (`Bop Bor  )
    | XOR     -> decompile_op s (`Bop Bxor )


    (* Cryptographic operations *)

    | BLAKE2B          -> decompile_op s (`Uop Ublake2b         )
    | CHECK_SIGNATURE  -> decompile_op s (`Top Tcheck_signature )
    | HASH_KEY         -> decompile_op s (`Uop Uhash_key        )
    | SHA256           -> decompile_op s (`Uop Usha256          )
    | SHA512           -> decompile_op s (`Uop Usha512          )


    (* Blockchain operations *)

    | ADDRESS            -> decompile_op s (`Zop  Zaddress          )
    | AMOUNT             -> decompile_op s (`Zop  Zamount           )
    | BALANCE            -> decompile_op s (`Zop  Zbalance          )
    | CHAIN_ID           -> decompile_op s (`Zop  Zchain_id         )
    | CONTRACT (t, a)    -> decompile_op s (`Uop (Ucontract (t, a)) )
    | CREATE_CONTRACT _  -> assert false
    | IMPLICIT_ACCOUNT   -> decompile_op s (`Uop (Uimplicitaccount) )
    | NOW                -> decompile_op s (`Zop Znow               )
    | SELF a             -> decompile_op s (`Zop (Zself a)          )
    | SENDER             -> decompile_op s (`Zop Zsender            )
    | SET_DELEGATE       -> decompile_op s (`Uop Usetdelegate       )
    | SOURCE             -> decompile_op s (`Zop Zsource            )
    | TRANSFER_TOKENS    -> decompile_op s (`Top Ttransfer_tokens   )


    (* Operations on data structures *)

    | CAR ->
      let x, s = List.pop s in
      mkdecomp (`Paired (x, (vlocal () :> rstack1)) :: s) []
    | CDR ->
      let y, s = List.pop s in
      mkdecomp (`Paired ((vlocal () :> rstack1), y) :: s) []
    | CONCAT               -> decompile_op s (`Bop Bconcat               )
    | CONS                 -> decompile_op s (`Bop Bcons                 )
    | EMPTY_BIG_MAP (k, v) -> decompile_op s (`Zop (Zemptybigmap (k, v)) )
    | EMPTY_MAP (k, v)     -> decompile_op s (`Zop (Zemptymap (k, v))    )
    | EMPTY_SET t          -> decompile_op s (`Zop (Zemptyset t)         )
    | GET                  -> decompile_op s (`Bop Bget                  )
    | LEFT t               -> decompile_op s (`Uop (Uleft t)             )
    | MAP _cs              -> assert false
    | MEM                  -> decompile_op s (`Bop Bmem                  )
    | NIL t                -> decompile_op s (`Zop (Znil t)              )
    | NONE t               -> decompile_op s (`Zop (Znone t)             )
    | PACK                 -> decompile_op s (`Uop Upack                 )
    | PAIR                 ->  begin
        let x, s = List.pop s in

        match x with
        | `Paired (x1, x2) ->
          mkdecomp (x1 :: x2 :: s) []

        | #dvar as v ->
          let x1 = vlocal () in
          let x2 = vlocal () in
          let op = DIAssign (v, Dfun (`Bop Bpair, [Dvar x1; Dvar x2])) in
          mkdecomp ((x1 :> rstack1) :: (x2 :> rstack1) :: s) [op]
      end
    | RIGHT t              -> decompile_op s (`Uop (Uright t)  )
    | SIZE                 -> decompile_op s (`Uop Usize       )
    | SLICE                -> decompile_op s (`Top Tslice      )
    | SOME                 -> decompile_op s (`Uop (Usome)     )
    | UNIT                 -> decompile_op s (`Zop (Zunit)     )
    | UNPACK t             -> decompile_op s (`Uop (Uunpack t) )
    | UPDATE               -> decompile_op s (`Top Tupdate     )


    (* Other *)

    | UNPAIR ->
      let x, s = List.pop s in
      let y, s = List.pop s in
      mkdecomp (`Paired (x, y) :: s) []

    | SELF_ADDRESS -> decompile_op s (`Zop Zself_address )

    | ITER cs ->
        let { failure; stack = s; code = bd1 } = decompile_s s cs in

        if failure then
          assert false;

        (* FIXME: iterate this ad nauseum: we need to find a fixpoint *)

        let x1, s1  = List.pop s in
        let bd2 = (decompile_s s1 cs).code in

        let uf = UF.create () in

        unify_dcode uf bd1 bd2;

        let bd   = dcode_apply_uf uf bd1 in
        let s    = rstack_apply_uf uf (x1 :: s1) in
        let x, s = List.pop s in 

        let xs = vlocal () in 
        mkdecomp
          ((xs :> rstack1) :: s)
          [DIIter (as_dvar x, Dvar xs, bd)]

    | IF_CONS (c1, c2) ->
      compile_match s [("cons", 1), c1; ("nil", 0), c2]

    | IF_LEFT (c1, c2) ->
      compile_match s [("left", 1), c1; ("right", 1), c2]

    | IF_NONE (c1, c2) ->
      compile_match s [("none", 0), c1; ("some", 1), c2]

    | FAILWITH ->
      let s    = List.map (fun _ -> (vlocal () :> rstack1)) s in
      let x, _ = List.pop s in
      mkdecomp ~failure:true s [DIFailwith (dexpr_of_rstack1 x)]

    | _ -> (Format.eprintf "%a@\n" pp_code i; assert false)

  (***************************************************************************************** *)

  and decompile_op (s : rstack) (op : g_operator) =
    let n = match op with | `Zop _ -> 0 | `Uop _ -> 1 | `Bop _ -> 2 | `Top _ -> 3 in
    let x, s = List.pop s in
    let args = List.init n (fun _ -> vlocal ()) in
    mkdecomp
      ((args :> rstack) @ s)
      (write_var (Dfun (op, List.map (fun v -> Dvar v) args)) x)

  and compile_match (s : rstack) (bs : ((string * int) * code list) list) =
    let sc, subs = List.split (List.map (fun ((name, n), b) ->
        (* FIXME: check for failures *)
        let { stack = sc; code = bc } = decompile_s s b in
        assert (List.length sc >= n);
        let p, sc = List.cut n sc in
        let p, dp = List.split (List.map dptn_of_rstack1 p) in
        (sc, (name, p, List.flatten dp @ bc))) bs) in

    let x  = vlocal () in
    let sc = List.fold_left (fun x y -> snd (merge_rstack x y)) (List.hd sc) (List.tl sc) in

    mkdecomp ((x :> rstack1) :: sc) [DIMatch (Dvar x, subs)]

  and decompile_s (s : rstack) (c : code list) : decomp =
    let (failure, stack), code = List.fold_left_map (fun (oldfail, stack) code ->
      let { failure; stack; code; } = decompile_i stack code in
      (oldfail || failure, stack), code) (false, s) (List.rev c) in

    { failure; stack; code = List.flatten (List.rev code); }

  (* -------------------------------------------------------------------- *)
  let rec expr_fv (e : dexpr) =
    match e with
    | Dvar (`VLocal x) ->
        Sint.singleton x

    | Dvar _ ->
        Sint.empty

    | Depair (e1, e2) ->
        Sint.union (expr_fv e1) (expr_fv e2)

    | Ddata _ ->
        Sint.empty

    | Dfun (_, es) ->
        List.fold_left Sint.union Sint.empty (List.map expr_fv es)

  (* -------------------------------------------------------------------- *)
  let rec pattern_fv (p : dpattern) =
    match p with
    | DVar i ->
        Sint.singleton i

    | DPair (p1, p2) ->
        Sint.union (pattern_fv p1) (pattern_fv p2)

  (* -------------------------------------------------------------------- *)
  let rec instr_wr (i : dinstr) =
    match i with
    | DIAssign (`VLocal i, Dvar (`VLocal j)) when i = j ->
        Sint.empty

    | DIAssign (`VLocal i, _) ->
        Sint.singleton i

    | DIAssign _ ->
        Sint.empty

    | DIIf (_, (c1, c2)) ->
        Sint.union (code_wr c1) (code_wr c2)

    | DIWhile (_, c) ->
        code_wr c

    | DIIter (x, _, c) -> begin
        let wr = code_wr c in

        match x with
        | `VLocal i -> Sint.remove i wr
        | _ -> wr
      end

    | DIMatch (_, bs) ->
        let for1 (_, ps, c) =
          Sint.diff (code_wr c)
            (List.fold_left
               Sint.union Sint.empty
               (List.map pattern_fv ps)) in

        List.fold_left Sint.union Sint.empty (List.map for1 bs)

    | DIFailwith _ ->
        Sint.empty

  and code_wr (c : dcode) =
    List.fold_left Sint.union Sint.empty (List.map instr_wr c)

  (* -------------------------------------------------------------------- *)
  let rec expr_cttprop (env : dexpr Mint.t) (e : dexpr) =
    match e with
    | Dvar (`VLocal x) ->
        Option.get_dfl e (Mint.find_opt x env)

    | Dvar _ ->
        e

    | Depair (e1, e2) ->
        let e1 = expr_cttprop env e1 in
        let e2 = expr_cttprop env e2 in
        Depair (e1, e2)

    | Ddata _ ->
        e

    | Dfun (op, es) ->
        let es = List.map (expr_cttprop env) es in
        Dfun (op, es)

  type cttenv = dexpr Mint.t

  let rec instr_cttprop (env0 : cttenv) (code : dinstr) =
    match code with
    | DIAssign ((`VLocal i), Dvar (`VLocal j)) when i = j ->
        env0, []

    | DIAssign ((`VLocal i) as x, e) ->
        let env = Mint.remove i env0 in
        let e   = expr_cttprop env e in
        let efv = expr_fv e in
        let env = Mint.filter (fun j _ -> not (Sint.mem j efv)) env in

        let env = Mint.add i e env in


        env, [DIAssign (x, e)]

    | DIAssign (x, e) ->
        env0, [DIAssign (x, expr_cttprop env0 e)]

    | DIIf (e, (c1, c2)) ->
        let wr  = Sint.union (code_wr c1) (code_wr c2) in
        let env = Sint.fold Mint.remove wr env0 in
        let _, c1 = code_cttprop env0 c1 in
        let _, c2 = code_cttprop env0 c2 in

        env, [DIIf (expr_cttprop env0 e, (c1, c2))]
          
    | DIWhile (e, c) ->
        let wr   = code_wr c in
        let env  = Sint.fold Mint.remove wr env0 in
        let _, c = code_cttprop env c in

        env, [DIWhile (expr_cttprop env0 e, c)]

    | DIIter (x, e, c) ->

        let x    = as_vlocal x in
        let wr   = Sint.remove x (code_wr c) in
        let env  = Sint.fold Mint.remove wr env0 in
        let _, c = code_cttprop env c in

        env, [DIIter (`VLocal x, expr_cttprop env0 e, c)]

    | DIMatch (e, bs) ->
        let wr =
          let for1 (_, p, c) =
            let fv = List.map pattern_fv p in
            let fv = List.fold_left Sint.union Sint.empty fv in
            Sint.fold Sint.remove fv (code_wr c) in
          List.fold_left Sint.union Sint.empty (List.map for1 bs) in

        let env = Sint.fold Mint.remove wr env0 in

        let bs =
          let for1 (x, p, c) = (x, p, snd (code_cttprop env0 c)) in
          List.map for1 bs in

        env, [DIMatch (expr_cttprop env0 e, bs)]

    | DIFailwith e ->
        env0, [DIFailwith (expr_cttprop env0 e)]

  and code_cttprop (env : cttenv) (code : dcode) =
    let env, code =
      List.fold_left_map instr_cttprop env code
    in env, List.flatten code

  (* -------------------------------------------------------------------- *)
  let rec instr_kill (keep : Sint.t) (instr : dinstr) =
    match instr with
    | DIAssign (`VLocal i, e)
          when not (Sint.mem i (Sint.union keep (expr_fv e)))
      -> keep, []

    | DIAssign (`VLocal i, e) ->
        Sint.add i (Sint.union keep (expr_fv e)), [instr]

    | DIAssign (_, e) ->
        Sint.union keep (expr_fv e), [instr]

    | DIIf (e, (c1, c2)) ->
        let keep1, c1 = code_kill keep c1 in
        let keep2, c2 = code_kill keep c2 in

        Sint.union (expr_fv e) (Sint.union keep1 keep2),
        [DIIf (e, (c1, c2))]

    | DIWhile (e, c) ->
        let keep, c =
          code_kill (Sint.union keep (expr_fv e)) c
        in keep, [DIWhile (e, c)]

    | DIIter (x, e, c) ->
        let x = as_vlocal x in
        let keep, c =
          code_kill (Sint.add x (Sint.union keep (expr_fv e))) c in

        Sint.remove x keep, [DIIter (`VLocal x, e, c)]

    | DIMatch (e, bs) ->
        let for1 (x, pv, c) =
          let pfv = List.map pattern_fv pv in
          let pfv = List.fold_left Sint.union Sint.empty pfv in
          let keep, c = code_kill (Sint.union keep pfv) c in
          Sint.diff keep pfv, (x, pv, c) in

        let keep, bs = List.split (List.map for1 bs) in
        let keep = List.fold_left Sint.union Sint.empty keep in

        keep, [DIMatch (e, bs)]

    | DIFailwith e ->
        Sint.union keep (expr_fv e), [instr]

  and code_kill (keep : Sint.t) (code : dcode) =
    let keep, code = List.fold_left_map instr_kill keep (List.rev code) in
    keep, List.flatten (List.rev code)

  (* -------------------------------------------------------------------- *)
  let decompile (michelson : michelson) =
    let aty = michelson.storage in
    let pty = michelson.parameter in
    let code = match michelson.code with | SEQ l -> l | x -> [x] in

    let args prefix =
      let mkvar i : rstack1 =
        `VGlobal (Printf.sprintf "%s%d" prefix i) in

      let rec create i : _ -> rstack1 = function
        | { node = Tpair (_, ty); _} ->
          `Paired (mkvar i, create (i + 1) ty)
        | _ ->
          mkvar i
      in create 1 in

    let pst = args "args_" pty in
    let ast = args "sto_"  aty in

    let { stack = ost; code = dc; } =
      decompile_s [`Paired (`VGlobal "ops", ast)] code in

    let code =
      match ost with
      | [`Paired (px, ax)] ->
        let pr1 = write_var (dexpr_of_rstack1 pst) px in
        let pr2 = write_var (dexpr_of_rstack1 ast) ax in
        pr1 @ pr2 @ dc
      | _ -> assert false in

    let _, code = code_cttprop Mint.empty code in
    let _, code = code_kill Sint.empty code in

    code
end

let to_dir (michelson, env : T.michelson * env) =
  let tstorage   = michelson.storage in
  let tparameter = michelson.parameter in
  let name = env.name in

  let storage_data =
    match tstorage.node with
    | T.Tunit   -> T.Dunit
    | T.Tnat
    | T.Tint    -> T.Dint Big_int.zero_big_int
    | T.Tstring -> T.Dstring ""
    | _ -> T.Dunit (* FIXME*)
  in

  let code = Decomp_dir.decompile michelson in

  (T.mk_dprogram tstorage tparameter storage_data name code), env




(* let rec interp (env : ir_env) (sys : T.dinstruction list) (instrs : T.code list) (stack : (T.dexpr) list) =
   ()
   in ()

   let args prefix =
   let mkvar i : rstack1 =
    `VGlobal (Printf.sprintf "%s%d" prefix i) in

   let rec create i : _ -> rstack1 = function
    | T_Pair (_, ty) ->
      `Paired (mkvar i, create (i+1) ty)
    | _ ->
      mkvar i
   in create 1 in

   let pst = args "args_" pty in
   let ast = args "sto_"  aty in

   let ost, dc = decompile [`Paired (`VGlobal "ops", ast)] code in

   let dc =
   match ost with
   | [`Paired (px, ax)] ->
      let (pr1 , pr2 ), _ = unify pst px in
      let (pr'1, pr'2), _ = unify ast ax in
      let pr = List.map (fun pr ->
          List.map (fun (x, e) -> DIAssign (`VGlobal x, `Dup e)) pr
        ) [pr1; pr'1; pr2; pr'2] in
      List.flatten pr @ dc
   | _ -> assert false in

   Format.eprintf "%a@." pp_dcmd (compress_c dc) *)

(* -------------------------------------------------------------------- *)




(* let map_dinstruction_gen (fe : dexpr -> dexpr) (f : dinstruction -> dinstruction) = function
   | Ddecl     (id, v)            -> Ddecl     (id, Option.map fe v)
   | Dassign   (e, v)             -> Dassign   (fe e, fe v)
   | Dfail      e                 -> Dfail     (fe e)
   | Dif       (c, t, e)          -> Dif       (fe c, List.map f t, List.map f e)
   | Difcons   (c, ihs, it, t, e) -> Difcons   (fe c, ihs, it, List.map f t, List.map f e)
   | Difleft   (c, it, t, ie, e)  -> Difleft   (fe c, it, List.map f t, ie, List.map f e)
   | Difnone   (c, n, iv, v)      -> Difnone   (fe c, List.map f n, iv, List.map f v)
   | Dloop     (c, b)             -> Dloop     (fe c, List.map f b)
   | Diter     (c, b)             -> Diter     (fe c, List.map f b)


   let map_dexpr_gen (ft : type_ -> type_) (fd : data -> data) (f : dexpr -> dexpr) = function
   | Dalpha i                 -> Dalpha i
   | Dvar t                   -> Dvar t
   | Dstorage t               -> Dstorage (ft t)
   | Doperations              -> Doperations
   | Dlbdparam                -> Dlbdparam
   | Dlbdresult               -> Dlbdresult
   | Ddata d                  -> Ddata (fd d)
   | Dzop op                  -> Dzop op
   | Duop (op, x)             -> Duop (op, f x)
   | Dbop (op, x, y)          -> Dbop (op, f x, f y)
   | Dtop (op, x, y, z)       -> Dtop (op, f x, f y, f z)
   | Dapply (l, a)            -> Dapply (f l, f a)
   | Dexec (l, a)             -> Dexec (f l, f a)
   | Dlambda (at, rt, instrs) -> Dlambda (ft at, ft rt, instrs)
   | Dloopleft (l, is)        -> Dloopleft (f l, is)
   | Dmap (l, is)             -> Dmap (f l, is)

   and map_dinstruction_gen (fe : dexpr -> dexpr) (f : dinstruction -> dinstruction) = function
   | Ddecl     (id, v)                -> Ddecl      id
   | Dassign   (e, v)             -> Dassign   (fe e, fe v)
   | Dfail      e                 -> Dfail     (fe e)
   | Dif       (c, t, e)          -> Dif       (fe c, List.map f t, List.map f e)
   | Difcons   (c, ihs, it, t, e) -> Difcons   (fe c, ihs, it, List.map f t, List.map f e)
   | Difleft   (c, it, t, ie, e)  -> Difleft   (fe c, it, List.map f t, ie, List.map f e)
   | Difnone   (c, n, iv, v)      -> Difnone   (fe c, List.map f n, iv, List.map f v)
   | Dloop     (c, b)             -> Dloop     (fe c, List.map f b)
   | Diter     (c, b)             -> Diter     (fe c, List.map f b)

   let map_dexpr = map_dexpr_gen id id

   let map_dinstruction = map_dinstruction_gen id

   let fold_dexpr f accu = function
   | Dalpha _                 -> accu
   | Dvar _                   -> accu
   | Dstorage _               -> accu
   | Doperations              -> accu
   | Dlbdparam                -> accu
   | Dlbdresult               -> accu
   | Ddata _                  -> accu
   | Dzop _                   -> accu
   | Duop (_, x)              -> f accu x
   | Dbop (_, x, y)           -> f (f accu x) y
   | Dtop (_, x, y, z)        -> f (f (f accu x) y) z
   | Dapply (l, a)            -> f (f accu l) a
   | Dexec (l, a)             -> f (f accu l) a
   | Dlambda (_, _, _instrs)  -> accu
   | Dloopleft (l, _)         -> f accu l
   | Dmap (l, _)              -> f accu l

   let rec fold_dinstruction_dexpr f accu = function
   | Ddecl      _              -> accu
   | Dassign   (e, v)          -> f (f accu e) v
   | Dfail      e              -> f accu e
   | Dif       (c, t, e)       -> List.fold_left (fold_dinstruction_dexpr f) (List.fold_left (fold_dinstruction_dexpr f) (f accu c) t) e
   | Difcons   (c, _, _, t, e) -> List.fold_left (fold_dinstruction_dexpr f) (List.fold_left (fold_dinstruction_dexpr f) (f accu c) t) e
   | Difleft   (c, _, t, _, e) -> List.fold_left (fold_dinstruction_dexpr f) (List.fold_left (fold_dinstruction_dexpr f) (f accu c) t) e
   | Difnone   (c, n, _, v)    -> List.fold_left (fold_dinstruction_dexpr f) (List.fold_left (fold_dinstruction_dexpr f) (f accu c) n) v
   | Dloop     (c, b)          -> List.fold_left (fold_dinstruction_dexpr f) (f accu c) b
   | Diter     (c, b)          -> List.fold_left (fold_dinstruction_dexpr f) (f accu c) b

   let fold_dinstruction f accu = function
   | Ddecl      _              -> accu
   | Dassign    _              -> accu
   | Dfail      _              -> accu
   | Dif       (_, t, e)       -> List.fold_left f (List.fold_left f accu t) e
   | Difcons   (_, _, _, t, e) -> List.fold_left f (List.fold_left f accu t) e
   | Difleft   (_, _, t, _, e) -> List.fold_left f (List.fold_left f accu t) e
   | Difnone   (_, n, _, v)    -> List.fold_left f (List.fold_left f accu n) v
   | Dloop     (_, b)          -> List.fold_left f accu b
   | Diter     (_, b)          -> List.fold_left f accu b *)


(*
Dlet _to_red_dir (dir, env : T.dprogram * env) : T.dprogram * env =
  let rec doit (accu : T.dcode) (code : T.dcode) =


    let replace src dst instrs =

      let map_dexpr_gen (ft : T.type_ -> T.type_) (fd : T.data -> T.data) (f : T.dexpr -> T.dexpr) = function
        | T.Dalpha i                 -> T.Dalpha i
        | T.Dvar t                   -> T.Dvar t
        | T.Dstorage t               -> T.Dstorage (ft t)
        | T.Doperations              -> T.Doperations
        | T.Dlbdparam                -> T.Dlbdparam
        | T.Dlbdresult               -> T.Dlbdresult
        | T.Ddata d                  -> T.Ddata (fd d)
        | T.Dzop op                  -> T.Dzop op
        | T.Duop (op, x)             -> T.Duop (op, f x)
        | T.Dbop (op, x, y)          -> T.Dbop (op, f x, f y)
        | T.Dtop (op, x, y, z)       -> T.Dtop (op, f x, f y, f z)
        | T.Dapply (l, a)            -> T.Dapply (f l, f a)
        | T.Dexec (l, a)             -> T.Dexec (f l, f a)
        | T.Dlambda (at, rt, instrs) -> T.Dlambda (ft at, ft rt, instrs)
        | T.Dloopleft (l, is)        -> T.Dloopleft (f l, is)
        | T.Dmap (l, is)             -> T.Dmap (f l, is)
      in
      let map_dexpr f = map_dexpr_gen id id f in

      let rec for_expr (e : T.dexpr) : T.dexpr =
        if T.cmp_dexpr e src
        then dst
        else map_dexpr for_expr e
      in

      let rec replace (fe : T.dexpr -> T.dexpr) (x : T.dinstruction) =
        let f = replace fe in
        match x with
        | T.Ddecl     (id, v)            -> T.Ddecl     (id, Option.map fe v)
        | T.Dassign   (e, v)             -> T.Dassign   (fe e, fe v)
        | T.Dfail      e                 -> T.Dfail     (fe e)
        | T.Dif       (c, t, e)          -> T.Dif       (fe c, List.map f t, List.map f e)
        | T.Difcons   (c, ihs, it, t, e) -> T.Difcons   (fe c, ihs, it, List.map f t, List.map f e)
        | T.Difleft   (c, it, t, ie, e)  -> T.Difleft   (fe c, it, List.map f t, ie, List.map f e)
        | T.Difnone   (c, n, iv, v)      -> T.Difnone   (fe c, List.map f n, iv, List.map f v)
        | T.Dloop     (c, b)             -> T.Dloop     (fe c, List.map f b)
        | T.Diter     (c, b)             -> T.Diter     (fe c, List.map f b)
      in
      List.map (replace for_expr) instrs
    in

    match code with
    | Dassign (i, v)::tl when (function | T.Dalpha _ -> true | _ -> false) i -> let instrs = replace i v accu in doit instrs tl
    (* | Ddecl (id, v)::tl                 -> assert false
       | Dfail v:: tl                      -> assert false
       | Dif (c, t, e)::tl                 -> assert false
       | Difcons (c, hdid, tlid, t, e)::tl -> assert false
       | Difleft (c, lid, l, rid, r)::tl   -> assert false
       | Difnone (c, n, id, v)::tl         -> assert false
       | Dloop (c, body)::tl               -> assert false
       | Diter (c, body)::tl               -> assert false *)
    | a::tl                             -> doit (a::accu) tl
    | [] -> accu
  in
  { dir with
    code = dir.code |> List.rev |> doit []
  }, env *)

let to_red_dir (dir, env : T.dprogram * env) : T.dprogram * env = dir, env

let to_ir (dir, env : T.dprogram * env) : T.ir * env =
  let tstorage     = dir.storage in
  let tparameter   = dir.parameter in
  let storage_data = dir.storage_data in

  (* let rec for_expr (e : T.dexpr) : T.instruction =
     let f = for_expr in
     let g = for_instr in
     let seq x = T.Iseq (List.map g x) in

     match e with
     (* | Dalpha _i              -> assert false
     | Dvar t                 -> Ivar (Option.get t.annotation)
     | Dstorage _t            -> assert false
     | Doperations            -> assert false
     | Dlbdparam              -> assert false
     | Dlbdresult             -> assert false
     | Ddata d                -> Iconst (T.tnat, d)
     | Dzop op                -> Izop op
     | Duop (op, a)           -> Iunop (op, f a)
     | Dbop (op, a, b)        -> Ibinop (op, f a, f b)
     | Dtop (op, a, b, c)     -> Iterop (op, f a, f b, f c)
     | Dapply (a, l)          -> Ibinop (Bapply, f a, f l)
     | Dexec (a, l)           -> Ibinop (Bexec, f a, f l)
     | Dlambda (at, rt, l)    -> Ilambda (at, "", rt, seq l)
     | Dloopleft (c, b)       -> Iloopleft (f c, "", seq b)
     | Dmap (c, b)            -> Imap_ (f c, "", seq b) *)
     | _ -> assert false
     and for_instr (i : T.dinstr) : T.dinstr =
     let f = for_expr in
     let g = for_instr in
     let seq x = T.Iseq (List.map g x) in

     let to_ident n = Format.sprintf "x%i" n in
     match i with
     | Ddecl     (_id, _v)    -> assert false
     | Dassign   (e, v)       -> begin
        let v = f v in
        match e with
        | Dvar t when Option.is_some t.annotation -> Iassign(Option.get t.annotation, v)
        | Doperations -> Iassign("operations", v)
        | _ -> assert false
      end
     | Dfail      e                 -> Iunop   (Ufail, f e)
     | Dif       (c, t, e)          -> Iif     (f c, seq t, seq e, T.tunit)
     | Difcons   (c, ihd, it, t, e) -> Iifcons (f c, to_ident ihd, to_ident it, seq t, seq e, T.tunit)
     | Difleft   (c, il, l, ir, r)  -> Iifleft (f c, to_ident il, seq l, to_ident ir, seq r, T.tunit)
     | Difnone   (c, n, iv, v)      -> Iifnone (f c, seq n, to_ident iv, seq v, T.tunit)
     | Dloop     (c, b)             -> Iloop   (f c, seq b)
     | Diter     (c, b)             -> Iiter   ([], f c,  seq b)
     in *)

  let storage_list =
    let rec aux (x : T.type_) =
      match x.node, x.annotation with
      | _, Some a  -> [a, x]
      | T.Tpair (a, b), _ -> begin
          match aux a, aux b with
          | [], _
          | _, [] -> []
          | x, y  -> x @ y
        end
      | _ -> []
    in
    let r = aux tstorage in
    match r with
    | [] -> ["storage", tstorage]
    | _  -> r
  in

  (* let args =
     let rec aux (x : T.type_) =
      match x.node, x.annotation with
      | _, Some a  -> [a, x]
      | T.Tpair (a, b), _ -> begin
          match aux a, aux b with
          | [], _
          | _, [] -> []
          | x, y  -> x @ y
        end
      | _ -> []
     in
     let r = aux tparameter in
     match r with
     | [] -> ["storage", tparameter]
     | _  -> r
     in *)

  (* let code = T.Iseq (List.map for_instr dir.code) in *)
  (* let entry = T.mk_entry "default" args [] code in *)

  let name = dir.name in
  let funs = [] in
  let entries = [] in

  T.mk_ir name tstorage storage_data storage_list tparameter funs entries, env

let rec ttype_to_mtype (t : T.type_) : M.type_ =
  let f = ttype_to_mtype in
  match t.node with
  | Tkey                   -> M.tkey
  | Tunit                  -> M.tunit
  | Tsignature             -> M.tsignature
  | Toption    t           -> M.toption (f t)
  | Tlist      t           -> M.tlist   (f t)
  | Tset       t           -> M.tset    (f t)
  | Toperation             -> M.toperation
  | Tcontract  t           -> M.tcontract (f t)
  | Tpair      (lt, rt)    -> M.ttuple [f lt; f rt]
  | Tor        (lt, rt)    -> M.tor(f lt) (f rt)
  | Tlambda    (at, rt)    -> M.tlambda (f at) (f rt)
  | Tmap       (kt, vt)    -> M.tmap (f kt) (f vt)
  | Tbig_map   (kt, vt)    -> M.tbig_map (f kt) (f vt)
  | Tchain_id              -> M.tchainid
  | Tint                   -> M.tint
  | Tnat                   -> M.tnat
  | Tstring                -> M.tstring
  | Tbytes                 -> M.tbytes
  | Tmutez                 -> M.ttez
  | Tbool                  -> M.tbool
  | Tkey_hash              -> M.tkeyhash
  | Ttimestamp             -> M.ttimestamp
  | Taddress               -> M.taddress
  | Tticket t              -> M.tticket (f t)
  | Tsapling_transaction n -> M.tsapling_transaction n
  | Tsapling_state       n -> M.tsapling_state n
  | Tbls12_381_fr          -> assert false
  | Tbls12_381_g1          -> assert false
  | Tbls12_381_g2          -> assert false
  | Tnever                 -> assert false

let to_model (ir, env : T.ir * env) : M.model * env =

  let tunknown = M.tunit in

  let for_type (t : T.type_) : M.type_ = ttype_to_mtype t in

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
    | Dvar (_x, _t)    -> assert false
  in

  let rec for_instr (i : T.instruction) : M.mterm =
    let f = for_instr in
    match i with
    | Iseq l                       -> M.seq (List.map f l)
    | IletIn (_id, _v, _b, _)      -> assert false
    | Ivar id                      -> M.mk_mvar (dumloc id) M.tunit
    | Icall (_id, _args, _)        -> assert false
    | Iassign (id, v)              -> M.mk_mterm (M.Massign (ValueAssign, M.tunit, Avarstore (dumloc id), f v)) M.tunit
    | Iif (c, t, e, ty) -> begin
        let ce = f c in
        let te = f t in
        let ee = f e in
        match ty.node with
        | T.Tunit -> begin
            let ee =
              match ee with
              | {node = Mseq []} -> None
              | v -> Some v
            in
            M.mk_mterm (M.Mif (ce, te, ee)) M.tunit
          end
        | _ -> M.mk_mterm (M.Mexprif (ce, te, ee)) (for_type ty)
      end
    | Iifnone (x, ne, id, v, ty) -> begin
        let xe  = f x in
        let nee = f ne in
        let ve  = f v in
        match ty.node with
        | T.Tunit -> M.mk_mterm (M.Minstrmatchoption (xe, dumloc id, ve, nee)) M.tunit
        | _       -> M.mk_mterm (M.Mmatchoption (xe, dumloc id, ve, nee)) (for_type ty)
      end
    | Iifleft (x, lid, l, rid, r, ty) -> begin
        let xe = f x in
        let le = f l in
        let re = f r in
        match ty.node with
        | T.Tunit -> M.mk_mterm (M.Minstrmatchor (xe, dumloc lid, le, dumloc rid, re)) M.tunit
        | _       -> M.mk_mterm (M.Mmatchor (xe, dumloc lid, le, dumloc rid, re)) (for_type ty)
      end
    | Iifcons (x, hid, tid, ht, n, ty) -> begin
        let xe  = f x in
        let hte = f ht in
        let ne  = f n in
        match ty.node with
        | T.Tunit -> M.mk_mterm (M.Minstrmatchlist (xe, dumloc hid, dumloc tid, hte, ne)) M.tunit
        | _       -> M.mk_mterm (M.Mmatchlist (xe, dumloc hid, dumloc tid, hte, ne)) (for_type ty)
      end
    | Iloopleft (l, i, b)          -> let be = f b in M.mk_mterm (M.Mfold (f l, dumloc i, be)) be.type_
    | Ilambda (_rt, _id, _at, _e)  -> assert false
    | Iloop (c, b)                 -> M.mk_mterm (M.Mwhile (f c, f b, None)) M.tunit
    | Iiter (_ids, _c, _b)         -> assert false
    | Izop op -> begin
        match op with
        | Znow                   -> M.mk_mterm  Mnow         (M.tdate)
        | Zamount                -> M.mk_mterm  Mtransferred (M.ttez)
        | Zbalance               -> M.mk_mterm  Mbalance     (M.ttez)
        | Zsource                -> M.mk_mterm  Msource      (M.taddress)
        | Zsender                -> M.mk_mterm  Mcaller      (M.taddress)
        | Zaddress               -> assert false
        | Zchain_id              -> M.mk_mterm  Mchainid     (M.tchainid)
        | Zself _                -> assert false
        | Zself_address          -> M.mk_mterm  Mselfaddress (M.taddress)
        | Znone t                -> M.mk_mterm  Mnone        (M.toption (for_type t))
        | Zunit                  -> M.mk_mterm  Munit         M.tunit
        | Znil t                 -> M.mk_mterm (Mlitlist []) (M.tlist (for_type t))
        | Zemptyset t            -> M.mk_mterm (Mlitset [])  (M.tset  (for_type t))
        | Zemptymap (k, v)       -> M.mk_mterm (Mlitmap (false, [])) (M.tmap  (for_type k) (for_type v))
        | Zemptybigmap (k, v)    -> M.mk_mterm (Mlitmap (true, [])) (M.tbig_map (for_type k) (for_type v))
        | Ztotalvotingpower      -> M.mk_mterm  Mtotalvotingpower (M.tnat)
        | Zlevel                 -> M.mk_mterm  Mlevel (M.tnat)
        | Zsapling_empty_state n -> M.mk_mterm (Msapling_empty_state n) (M.tsapling_state n)
      end
    | Iunop (op, e) -> begin
        match op with
        | Ucar               -> M.mk_mterm (Mtupleaccess (f e, Big_int.zero_big_int))  (M.tunit)
        | Ucdr               -> M.mk_mterm (Mtupleaccess (f e, Big_int.unit_big_int))  (M.tunit)
        | Uleft  t           -> let ee = f e in let t = for_type t in M.mk_mterm (Mleft  (t, f e)) (M.tor ee.type_ t)
        | Uright t           -> let ee = f e in let t = for_type t in M.mk_mterm (Mright (t, f e)) (M.tor t ee.type_)
        | Uneg               -> M.mk_mterm (Muminus (f e)) M.tint
        | Uint               -> M.mk_mterm (Mnattoint (f e)) M.tint
        | Unot               -> M.mk_mterm (Mnot (f e)) M.tbool
        | Uabs               -> M.mk_mterm (Mabs (f e)) (M.tnat)
        | Uisnat             -> assert false
        | Usome              -> let ee = f e in M.mk_mterm (Mabs ee) (M.toption ee.type_)
        | Usize              -> M.mk_mterm (Mlength (f e)) M.tnat
        | Upack              -> M.mk_mterm (Mpack (f e)) M.tbytes
        | Uunpack t          -> M.mk_mterm (Munpack (for_type t, f e)) (for_type t)
        | Ublake2b           -> M.mk_mterm (Mblake2b (f e)) M.tbytes
        | Usha256            -> M.mk_mterm (Msha256 (f e)) M.tbytes
        | Usha512            -> M.mk_mterm (Msha512 (f e)) M.tbytes
        | Usha3              -> M.mk_mterm (Msha3 (f e)) M.tbytes
        | Ukeccak            -> M.mk_mterm (Mkeccak (f e)) M.tbytes
        | Uhash_key          -> M.mk_mterm (Mhashkey (f e)) M.tkeyhash
        | Ufail              -> M.failg (f e)
        | Ucontract (_t, _a) -> assert false
        | Usetdelegate       -> assert false
        | Uimplicitaccount   -> assert false
        | Ueq                -> assert false
        | Une                -> assert false
        | Ugt                -> assert false
        | Uge                -> assert false
        | Ult                -> assert false
        | Ule                -> assert false
        | Uvotingpower       -> M.mk_mterm (Mvotingpower (f e)) M.tkeyhash
        | Ureadticket        -> assert false
        | Ujointickets       -> assert false
        | Upairing_check     -> assert false
      end
    | Ibinop (op, a, b) -> begin
        match op with
        | Badd       -> M.mk_mterm (Mplus (f a, f b)) M.tnat
        | Bsub       -> M.mk_mterm (Mminus (f a, f b)) M.tint
        | Bmul       -> M.mk_mterm (Mmult (f a, f b)) M.tint
        | Bediv      -> M.mk_mterm (Mdivmod (f a, f b)) tunknown
        | Blsl       -> M.mk_mterm (Mshiftleft (f a, f b)) M.tnat
        | Blsr       -> M.mk_mterm (Mshiftright (f a, f b)) M.tnat
        | Bor        -> M.mk_mterm (Mand (f a, f b)) M.tbool
        | Band       -> M.mk_mterm (Mor (f a, f b)) M.tbool
        | Bxor       -> M.mk_mterm (Mxor (f a, f b)) tunknown
        | Bcompare   -> M.mk_mterm (MthreeWayCmp (f a, f b)) M.tint
        | Bget       -> assert false
        | Bmem       -> assert false
        | Bconcat    -> assert false
        | Bcons      -> assert false
        | Bpair      -> M.mk_mterm (Mtuple [f a; f b]) (M.tunit)
        | Bexec      -> assert false
        | Bapply     -> assert false
        | Bcreateticket -> assert false
        | Bsplitticket  -> assert false
        | Bsapling_verify_update -> M.mk_mterm (Msapling_verify_update (f a, f b)) (M.toption (M.ttuple [M.tint; M.tsapling_state 0]))
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
    | Imap (_b, _k, _v, _l)             -> assert false
    | Irecord _l                        -> assert false
    | Irecupdate (_x, _l)               -> assert false
    | Ifold (_ix, _iy, _ia, _c, _a, _b) -> assert false
    | Imap_ (x, id, e)                  -> let ee = f e in M.mk_mterm (Mmap (f x, dumloc id, ee)) (M.tlist ee.type_)
    | Imichelson (_a, _c, _v)           -> assert false
  in

  let rec get_default_value (t : T.type_) =
    let f = get_default_value in
    match t.node with
    | Tnat
    | Tint    -> T.Dint Big_int.zero_big_int
    | Tstring -> T.Dstring ""
    | Tpair (a, b) -> T.Dpair (f a, f b)
    | _ -> assert false
  in
  let storage =
    List.map (fun (id, t) ->
        M.mk_storage_item (dumloc id) MTvar (for_type t) (for_data ~t:t (get_default_value t))
      ) ir.storage_list
  in

  let for_entry (e : T.entry) : M.function__ =
    let name = dumloc e.name in
    let args = List.map (fun (id, t) -> (dumloc id, for_type t, None) ) e.args in
    let body = for_instr e.body in
    let fn : M.function_struct = M.mk_function_struct name body ~args:args in
    let node : M.function_node = M.Entry fn in
    M.mk_function node
  in
  let functions = List.map for_entry ir.entries in

  let model = M.mk_model (dumloc ir.name) ~functions:functions ~storage:storage in
  model, env

module Decomp_model : sig
  val decompile : T.dprogram * env -> M.model * env
end = struct
  open Ident
  open Michelson
  open Model

  let for_type (t : T.type_) : M.type_ = ttype_to_mtype t

  let for_data ?t (d : T.data) : M.mterm =
    let is_nat = Option.map_dfl (fun (t : T.type_) -> match t.node with | T.Tnat -> true | _ -> false) false in
    match d with
    | Dint v when is_nat t -> M.mk_bnat v
    | Dint    v        -> mk_bint v
    | Dstring v        -> mk_string v
    | Dbytes  v        -> mk_bytes v
    | Dunit            -> unit
    | Dtrue            -> mtrue
    | Dfalse           -> mfalse
    | Dpair  (_ld, _rd)-> assert false
    | Dleft   _d       -> assert false
    | Dright  _d       -> assert false
    | Dsome   _d       -> assert false
    | Dnone            -> assert false
    | Dlist  _l        -> assert false
    | Delt _           -> assert false
    | Dvar _           -> assert false

  let get_storage_list tstorage =
    let rec aux (x : T.type_) =
      match x.node, x.annotation with
      | _, Some a  -> [a, x]
      | T.Tpair (a, b), _ -> begin
          match aux a, aux b with
          | [], _
          | _, [] -> []
          | x, y  -> x @ y
        end
      | _ -> []
    in
    let r = aux tstorage in
    match r with
    | [] -> ["storage", tstorage]
    | _  -> r

  let rec get_default_value (t : T.type_) =
    let f = get_default_value in
    match t.node with
    | Tnat
    | Tint         -> T.Dint Big_int.zero_big_int
    | Tstring      -> T.Dstring ""
    | Tpair (a, b) -> T.Dpair (f a, f b)
    | _ -> T.Dint Big_int.zero_big_int(* assert false *)

  let for_code (code : dcode) : mterm =
    let ft = for_type in

    let for_dvar (v : dvar) : ident =
      match v with
      | `VLocal  x  -> "$" ^ (string_of_int x)
      | `VGlobal id -> id
    in

    let rec for_expr (e : dexpr) : mterm =
      let f = for_expr in
      let tunknown = tunit in
      match e with
      | Dvar v          -> mk_mvar (dumloc (for_dvar v)) tunit
      | Ddata d         -> for_data d
      | Depair _        -> assert false
      | Dfun (`Uop Ueq, [Dfun (`Bop Bcompare, [a; b])]) -> mk_mterm (Mequal  (tint, f a, f b)) tbool
      | Dfun (`Uop Une, [Dfun (`Bop Bcompare, [a; b])]) -> mk_mterm (Mnequal (tint, f a, f b)) tbool
      | Dfun (`Uop Ugt, [Dfun (`Bop Bcompare, [a; b])]) -> mk_mterm (Mgt     (f a, f b)) tbool
      | Dfun (`Uop Uge, [Dfun (`Bop Bcompare, [a; b])]) -> mk_mterm (Mge     (f a, f b)) tbool
      | Dfun (`Uop Ult, [Dfun (`Bop Bcompare, [a; b])]) -> mk_mterm (Mlt     (f a, f b)) tbool
      | Dfun (`Uop Ule, [Dfun (`Bop Bcompare, [a; b])]) -> mk_mterm (Mle     (f a, f b)) tbool
      | Dfun (op, args) -> begin
          match op, args with
          | `Zop Znow,                       [] -> mnow
          | `Zop Zamount,                    [] -> mtransferred
          | `Zop Zbalance,                   [] -> mbalance
          | `Zop Zsource,                    [] -> msource
          | `Zop Zsender,                    [] -> mcaller
          | `Zop Zaddress,                   [] -> assert false
          | `Zop Zchain_id,                  [] -> mchainid
          | `Zop Zself _a,                   [] -> assert false
          | `Zop Zself_address,              [] -> mselfaddress
          | `Zop Znone t,                    [] -> mk_none (ft t)
          | `Zop Zunit,                      [] -> unit
          | `Zop Znil t,                     [] -> mk_mterm (Mlitlist []) (tlist (ft t))
          | `Zop Zemptyset  t,               [] -> mk_mterm (Mlitset [])  (tlist (ft t))
          | `Zop Zemptymap (tk, tv),         [] -> mk_mterm (Mlitmap (false, [])) (tmap (ft tk) (ft tv))
          | `Zop Zemptybigmap (tk, tv),      [] -> mk_mterm (Mlitmap (true, [])) (tmap (ft tk) (ft tv))
          | `Uop Ucar,                    [ a ] -> mk_tupleaccess 0 (f a)
          | `Uop Ucdr,                    [ a ] -> mk_tupleaccess 1 (f a)
          | `Uop Uleft t,                 [ a ] -> mk_left  (ft t) (f a)
          | `Uop Uright t,                [ a ] -> mk_right (ft t) (f a)
          | `Uop Uneg,                    [ a ] -> mk_mterm (Muminus (f a)) tint
          | `Uop Uint,                    [ a ] -> mk_nat_to_int (f a)
          | `Uop Unot,                    [ a ] -> mnot (f a)
          | `Uop Uabs,                    [ a ] -> mk_mterm (Mabs (f a)) tnat
          | `Uop Uisnat,                  [ _a ] -> assert false
          | `Uop Usome,                   [ a ] -> mk_some (f a)
          | `Uop Usize,                   [ a ] -> mk_mterm (Mlistlength (tunknown, f a)) tnat
          | `Uop Upack,                   [ a ] -> mk_pack (f a)
          | `Uop Uunpack t,               [ a ] -> mk_unpack (ft t) (f a)
          | `Uop Ublake2b,                [ a ] -> mk_blake2b (f a)
          | `Uop Usha256,                 [ a ] -> mk_sha256 (f a)
          | `Uop Usha512,                 [ a ] -> mk_sha512 (f a)
          | `Uop Uhash_key,               [ a ] -> mk_hashkey (f a)
          | `Uop Ufail,                   [ a ] -> failg (f a)
          | `Uop Ucontract (_t, _an),     [ _a ] -> assert false
          | `Uop Usetdelegate,            [ _a ] -> assert false
          | `Uop Uimplicitaccount,        [ _a ] -> assert false
          | `Uop Ueq,                     [ _ ] -> assert false
          | `Uop Une,                     [ _ ] -> assert false
          | `Uop Ugt,                     [ _ ] -> assert false
          | `Uop Uge,                     [ _ ] -> assert false
          | `Uop Ult,                     [ _ ] -> assert false
          | `Uop Ule,                     [ _ ] -> assert false
          | `Bop Badd,                 [ a; b ] -> mk_mterm (Mplus (f a, f b)) tint
          | `Bop Bsub,                 [ a; b ] -> mk_mterm (Mminus (f a, f b)) tint
          | `Bop Bmul,                 [ a; b ] -> mk_mterm (Mmult (f a, f b)) tint
          | `Bop Bediv,                [ _a; _b ] -> assert false
          | `Bop Blsl,                 [ _a; _b ] -> assert false
          | `Bop Blsr,                 [ _a; _b ] -> assert false
          | `Bop Bor,                  [ a; b ] -> mk_mterm (Mor  (f a, f b)) tbool
          | `Bop Band,                 [ a; b ] -> mk_mterm (Mand (f a, f b)) tbool
          | `Bop Bxor,                 [ a; b ] -> mk_mterm (Mxor (f a, f b)) tbool
          | `Bop Bcompare,             [ _; _ ] -> assert false
          | `Bop Bget,                 [ a; b ] -> mk_mterm (Mmapget (tunknown, tunknown, f a, f b)) tunknown
          | `Bop Bmem,                 [ a; b ] -> mk_mterm (Mmapcontains(tunknown, tunknown, f a, f b)) tunknown
          | `Bop Bconcat,              [ a; b ] -> mk_mterm (Mconcat (f a, f b)) tunknown
          | `Bop Bcons,                [ a; b ] -> mk_mterm (Mlistprepend (tunknown, f a, f b)) tunknown
          | `Bop Bpair,                [ a; b ] -> mk_tuple [f a; f b]
          | `Bop Bexec,                [ _a; _b ] -> assert false
          | `Bop Bapply,               [ _a; _b ] -> assert false
          | `Top Tcheck_signature,  [ a; b; c ] -> mk_checksignature (f a) (f b) (f c)
          | `Top Tslice,            [ a; b; c ] -> mk_mterm (Mslice (f a, f b, f c)) tunknown
          | `Top Tupdate,           [ a; b; c ] -> mk_mterm (Mmapput (tunknown, tunknown, f a, f b, f c)) tunknown
          | `Top Ttransfer_tokens,  [ _a; _b; _c ] -> assert false
          | _ -> assert false
        end

    in

    let rec for_instr i : mterm =
      let f = for_instr in
      let g = for_expr in
      let seq (c : dcode) : mterm =
        let instrs = List.map f c in
        seq instrs
      in
      begin
        match i with
        | DIAssign (x, e) ->
          let id = for_dvar x in
          let e = g e in
          mk_mterm (Massign (ValueAssign, tunit, Avar (dumloc id), e)) tunit

        | DIIf (c, (b1, b2)) ->
          mk_mterm (Mif (g c, seq b1, Some (seq b2))) tunit

        (* | DIMatch (c, bs) ->
           let rec pp_pattern fmt = function
            | DVar x ->
              Format.fprintf fmt "%a" pp_var (`VLocal x)
            | DPair (p1, p2) ->
              Format.fprintf fmt "(%a, %a)" pp_pattern p1 pp_pattern p2 in

           let pp_branch fmt (c, ptn, body)  =
            Format.fprintf fmt "@[<v 2>%s %a =>@\n%a@]" c
              (Format.pp_print_list
                 ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
                 pp_pattern) ptn
              pp_dcode body
           in

           Format.fprintf fmt "match (%a) with@\n%a@\nend" pp_expr c
            (Format.pp_print_list
               ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
               pp_branch) bs *)

        | DIMatch (c, [("left", _lv, lc) ; ("right", _rv, rc)]) ->
          mk_mterm (Minstrmatchor (g c, dumloc "_", seq rc, dumloc "_", seq lc)) tunit

        | DIFailwith e -> failg (for_expr e)
        | _ -> assert false
      end
    in
    let instrs = List.map for_instr code in
    seq instrs

  let decompile (dprogram, env : dprogram * env) =
    let code = for_code dprogram.code in
    let functions = [mk_function (Entry (mk_function_struct (dumloc "default") code ~args:[dumloc "arg", for_type dprogram.parameter, None])) ] in

    let storage_list = get_storage_list dprogram.storage in

    let storage =
      List.map (fun (id, t) ->
          M.mk_storage_item (dumloc id) MTvar (for_type t) (for_data ~t:t (get_default_value t))
        ) storage_list
    in
    let model = M.mk_model (dumloc dprogram.name) ~functions:functions ~storage:storage in
    model, env
end

let dir_to_model (dir, env : T.dprogram * env) : M.model * env =
  Decomp_model.decompile (dir, env)

let to_archetype (model, _env : M.model * env) : A.archetype =
  let rec for_type (t : M.type_) : A.type_t =
    let f = for_type in
    match M.get_ntype t with
    | Tasset id              -> A.tref (unloc id)
    | Tenum id               -> A.tref (unloc id)
    | Tstate                 -> assert false
    | Tbuiltin Bunit         -> A.tunit
    | Tbuiltin Bbool         -> A.tbool
    | Tbuiltin Bint          -> A.tint
    | Tbuiltin Brational     -> A.trational
    | Tbuiltin Bdate         -> A.tdate
    | Tbuiltin Bduration     -> A.tduration
    | Tbuiltin Btimestamp    -> assert false
    | Tbuiltin Bstring       -> A.tstring
    | Tbuiltin Baddress      -> A.taddress
    | Tbuiltin Brole         -> A.trole
    | Tbuiltin Bcurrency     -> A.ttez
    | Tbuiltin Bsignature    -> A.tsignature
    | Tbuiltin Bkey          -> A.tkey
    | Tbuiltin Bkeyhash      -> A.tkey_hash
    | Tbuiltin Bbytes        -> A.tbytes
    | Tbuiltin Bnat          -> A.tnat
    | Tbuiltin Bchainid      -> A.tchain_id
    | Tbuiltin Bbls12_381_fr -> A.tbls12_381_fr
    | Tbuiltin Bbls12_381_g1 -> A.tbls12_381_g1
    | Tbuiltin Bbls12_381_g2 -> A.tbls12_381_g2
    | Tbuiltin Bnever        -> A.tnever
    | Tcontainer (t, c)      -> A.mk_tcontainer (f t) (match c with | Collection -> assert false | Aggregate -> A.Aggregate | Partition -> A.Partition | View -> A.View)
    | Tlist t                -> A.mk_tlist (f t)
    | Toption t              -> A.mk_toption (f t)
    | Ttuple tl              -> A.mk_ttuple (List.map f tl)
    | Tset t                 -> A.mk_tset (f t)
    | Tmap (_, kt, vt)       -> A.mk_tmap (f kt) (f vt)
    | Tor (lt, rt)           -> A.mk_tor (f lt) (f rt)
    | Trecord id             -> A.tref (unloc id)
    | Tlambda _              -> assert false
    | Tunit                  -> A.tunit
    | Tstorage               -> assert false
    | Toperation             -> A.toperation
    | Tcontract t            -> A.mk_tcontract (f t)
    | Tprog _                -> assert false
    | Tvset _                -> assert false
    | Ttrace _               -> assert false
    | Tticket t              -> A.mk_tticket (f t)
    | Tsapling_state n       -> A.mk_sapling_state (Big_int.big_int_of_int n)
    | Tsapling_transaction n -> A.mk_sapling_transaction (Big_int.big_int_of_int n)
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

    | Mif (c, t, e)              -> A.eif ?e:(Option.map f e) (f c) (f t)
    | Mmatchwith (_e, _l)        -> assert false
    | Minstrmatchoption _        -> assert false
    | Minstrmatchor     _        -> assert false
    | Minstrmatchlist   _        -> assert false
    | Mfor (_i, _c, _b, _l)      -> assert false
    | Miter (_i, _a, _b, _c, _l) -> assert false
    | Mwhile (_c, _b, _l)        -> assert false
    | Mseq l                     -> begin
        match List.rev l with
        | []   -> assert false
        | [e]  -> f e
        | e::t -> List.fold_left (fun accu x -> A.eseq (f x) accu) (f e) t
      end
    | Mreturn _x                 -> assert false
    | Mlabel _i                  -> assert false
    | Mmark (_i, _x)             -> assert false


    (* effect *)

    | Mfail ft           -> begin
        let v =
          match ft with
          | Invalid e -> f e
          | _ -> assert false
        in
        A.efail v
      end
    | Mtransfer _tr      -> assert false


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
    | Mrational (_n, _d) -> assert false
    | Mstring v          -> A.estring v
    | Mcurrency (_v, _c) -> assert false
    | Maddress v         -> A.eaddress v
    | Mdate _v           -> assert false
    | Mduration _v       -> assert false
    | Mtimestamp _v      -> assert false
    | Mbytes v           -> A.ebytes v
    | Munit              -> A.etuple []


    (* control expression *)

    | Mexprif (_c, _t, _e)                   -> assert false
    | Mexprmatchwith (_e, _l)                -> assert false
    | Mmatchoption (_x, _i, _ve, _ne)        -> assert false
    | Mmatchor (_x, _lid, _le, _rid, _re)    -> assert false
    | Mmatchlist (_x, _hid, _tid, _hte, _ee) -> assert false
    | Mfold (_e, _i, _l)                     -> assert false
    | Mmap (_e, _i, _l)                      -> assert false
    | Mexeclambda (_l, _a)                   -> assert false
    | Mapplylambda (_l, _a)                  -> assert false


    (* composite type constructors *)

    | Mleft (_t, _x)  -> assert false
    | Mright (_t, _x) -> assert false
    | Mnone           -> A.eoption (ONone None)
    | Msome v         -> A.eoption (OSome (f v))
    | Mtuple l        -> A.etuple (List.map f l)
    | Masset _l       -> assert false
    | Massets _l      -> assert false
    | Mlitset _l      -> assert false
    | Mlitlist l      -> A.earray (List.map f l)
    | Mlitmap (_b, _l)-> assert false
    | Mlitrecord _l   -> assert false
    | Mlambda (_rt, _id, _at, _e) -> assert false

    (* access *)

    | Mdot (_e, _i)                 -> assert false
    | Mdotassetfield (_an, _k, _fn) -> assert false


    (* comparison operators *)

    | Mequal (_t, l, r)  -> A.eapp (Foperator (dumloc (A.Cmp Equal))) [f l; f r]
    | Mnequal (_t, l, r) -> A.eapp (Foperator (dumloc (A.Cmp Nequal))) [f l; f r]
    | Mgt (l, r)         -> A.eapp (Foperator (dumloc (A.Cmp Gt))) [f l; f r]
    | Mge (l, r)         -> A.eapp (Foperator (dumloc (A.Cmp Ge))) [f l; f r]
    | Mlt (l, r)         -> A.eapp (Foperator (dumloc (A.Cmp Lt))) [f l; f r]
    | Mle (l, r)         -> A.eapp (Foperator (dumloc (A.Cmp Le))) [f l; f r]
    | Mmulticomp (_e, _l)  -> assert false


    (* arithmetic operators *)

    | Mand (_l, _r)    -> assert false
    | Mor (_l, _r)     -> assert false
    | Mxor (_l, _r)    -> assert false
    | Mnot _e          -> assert false
    | Mplus (l, r)     -> A.eapp (A.Foperator (dumloc (A.Arith A.Plus))) [f l; f r]
    | Mminus (_l, _r)  -> assert false
    | Mmult (_l, _r)   -> assert false
    | Mdivrat (_l, _r) -> assert false
    | Mdiveuc (_l, _r) -> assert false
    | Mmodulo (_l, _r) -> assert false
    | Mdivmod (_l, _r) -> assert false
    | Muminus _e       -> assert false
    | MthreeWayCmp (_l, _r) -> assert false
    | Mshiftleft   (_l, _r) -> assert false
    | Mshiftright  (_l, _r) -> assert false

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
    | Mlistlength (_, _c)                  -> assert false
    | Mlistcontains (_, _c, _a)            -> assert false
    | Mlistnth (_, _c, _a)                 -> assert false
    | Mlistreverse (_, _l)                 -> assert false
    | Mlistconcat (_, _l, _m)              -> assert false
    | Mlistfold (_t, _ix, _ia, _c, _a, _b) -> assert false


    (* map api expression *)

    | Mmapput (_, _, c, k, v)               -> A.eapp (A.Fident (dumloc "put")) [f c; f k; f v]
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
    | Mlength x           -> A.eapp (A.Fident (dumloc "length")) [f x]
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
    | Msha3    _x                  -> assert false
    | Mkeccak  _x                  -> assert false
    | Mhashkey _x                  -> assert false
    | Mchecksignature (_k, _s, _x) -> assert false


    (* voting *)

    | Mtotalvotingpower            -> assert false
    | Mvotingpower _x              -> assert false


    (* ticket *)

    | Mcreateticket (_x, _a)    -> assert false
    | Mreadticket _x            -> assert false
    | Msplitticket (_x, _a, _b) -> assert false
    | Mjointickets (_x, _y)     -> assert false


    (* sapling *)

    | Msapling_empty_state   _ -> assert false
    | Msapling_verify_update _ -> assert false


    (* bls curve *)

    | Mpairing_check _ -> assert false


    (* constants *)

    | Mnow           -> A.eterm (dumloc A.cst_now)
    | Mtransferred   -> A.eterm (dumloc A.cst_transferred)
    | Mcaller        -> A.eterm (dumloc A.cst_caller)
    | Mbalance       -> A.eterm (dumloc A.cst_balance)
    | Msource        -> A.eterm (dumloc A.cst_source)
    | Mselfaddress   -> A.eterm (dumloc A.cst_selfaddress)
    | Mchainid       -> A.eterm (dumloc A.cst_chainid)
    | Mmetadata      -> A.eterm (dumloc A.cst_metadata)
    | Mlevel         -> assert false


    (* variable *)

    | Mvar (_an, Vassetstate _k, _t, _d) -> assert false
    | Mvar(v, Vstorevar, t, d)           -> A.eterm v ?temp:(for_temp t) ?delta:(for_delta d)
    | Mvar(v, Vstorecol, t, d)           -> A.eterm v ?temp:(for_temp t) ?delta:(for_delta d)
    | Mvar(_v, Vdefinition, _t, _d)      -> assert false
    | Mvar(v, Vlocal, t, d)              -> A.eterm v ?temp:(for_temp t) ?delta:(for_delta d)
    | Mvar(v, Vparam, t, d)              -> A.eterm v ?temp:(for_temp t) ?delta:(for_delta d)
    | Mvar(_v, Vfield, _t, _d)           -> assert false
    | Mvar(_, Vthe, _t, _d)              -> assert false
    | Mvar(_, Vstate, _t, _d)            -> assert false
    | Mvar(v, Vparameter, t, d)          -> A.eterm v ?temp:(for_temp t) ?delta:(for_delta d)
    | Menumval (id, args, _e)             -> begin
        match args with
        | [] -> A.eterm id
        | _  -> A.eapp (A.Fident id) []
      end

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
        let args = List.map (fun (id, t, _) -> (id, for_type t, None) ) fs.args in

        let ep = A.mk_entry_properties () in
        let ed = A.mk_entry_decl ~args:args id ep ~body:(body, exts) in

        A.mk_entry ed
      end
  in

  let decls =
    List.map for_storage_item model.storage
    @ List.map for_fun model.functions
  in

  A.mk_archetype () ~decls:((A.mk_darchetype model.name)::decls)
