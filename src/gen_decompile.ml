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

let to_ir (michelson, env : T.michelson * env) : T.ir * env =
  let storage_type = michelson.storage in
  let parameter    = michelson.parameter in
  let storage_data =
    match storage_type.node with
    | T.Tnat
    | T.Tint    -> T.Dint Big_int.zero_big_int
    | T.Tstring -> T.Dstring ""
    | _ -> assert false
  in
  let storage_list = [] in
  let funs = [] in
  let rec interp instrs (stack : (T.data * T.type_) list) =
    let f = interp in
    match instrs, stack with
    | T.SEQ l::it, _       -> f (l @ it) stack
    | T.DROP n::it, _      -> f it (Tools.foldi (fun x -> match x with | _::st -> st | _ -> assert false) stack n)
    | T.SWAP::it, a::b::st -> f it (b::a::st)
    | T.PUSH (t, d)::it, _ -> f it ((d, t)::stack)
    | T.NIL t::it, _       -> f it ((Dlist [], T.tlist t)::stack)
    | T.PAIR::it, a::b::st -> f it ((Dpair (fst a, fst b), (T.tpair (snd a) (snd b)))::st)
    | T.UNPAIR::it, (T.Dpair (a, b), {node = T.Tpair (ta, tb)})::st -> f it ((a, ta)::(b, tb)::st)
    | [], _ -> stack
    | _ -> assert false
  in
  let init_stack : (T.data * T.type_) list = T.[Dpair (Dvar "parameter", Dvar "storage"), T.tpair parameter storage_type ] in
  let stack = interp [michelson.code] init_stack in
  let st, sv =
    match stack with
    | [T.Dpair (_, v), {node = T.Tpair (_, t)}] -> v, t
    | _ -> assert false
  in
  let args = [] in
  let body = T.Iassign ("storage", T.Iconst (sv, st)) in
  let entries = T.[mk_entry "default" args [] body] in
  T.mk_ir storage_type storage_data storage_list parameter funs entries, env


type ir_env = {
  cpt_alpha : int;
}

let mk_ir_env ?(cpt_alpha=0) _ : ir_env =
  { cpt_alpha }

let to_ir2 (michelson, _ : T.michelson * 'a) =
  let tstorage   = michelson.storage in
  let tparameter = michelson.parameter in
  (* let storage_data =
     match storage_type.node with
     | T.Tnat
     | T.Tint    -> T.Dint Big_int.zero_big_int
     | T.Tstring -> T.Dstring ""
     | _ -> assert false
     in *)

  let inc_cpt_alpha env = { env with cpt_alpha = env.cpt_alpha + 1 } in

  let pp_stack fmt (st : T.dexpr list) =
    List.iteri (fun i (c : T.dexpr) ->
        Format.fprintf fmt "%i. %a@\n" i Printer_michelson.pp_dexpr c) st
  in

  let pp_trace fmt (instr, stack : T.code * (T.dexpr) list) =
    Format.fprintf fmt "instr: %a@\nstack:@\n%a@." Printer_michelson.pp_code instr pp_stack stack
  in

  let trace (instrs : T.code list) (stack : (T.dexpr) list) =
    match !Options.opt_trace, instrs with
    | true, i::_ -> Format.eprintf "%a" pp_trace (i, stack)
    | _ -> ()
  in

  let rec interp (env : ir_env) (accu : T.sysofequations) (instrs : T.code list) (stack : (T.dexpr) list) =
    let f = interp in

    trace instrs stack;

    match instrs, stack with
    | T.SEQ l::it, _       -> begin
        f env accu (it @ List.rev l) stack
      end

    | T.DROP 1::it, _      -> begin
        let d = T.dalpha env.cpt_alpha in
        let env = inc_cpt_alpha env in
        f env accu it (d::stack)
      end

    | T.SWAP::it, a::b::st -> begin
        f env accu it (b::a::st)
      end

    | T.DUG n::it, a::st      -> begin
        let rec insert idx e l =
          if idx < 0 then assert false;
          if idx = 0
          then e::l
          else insert (idx - 1) e l
        in

        let stack = insert (n - 1) a st in
        f env accu it stack
      end

    | T.DIG n::it, _      -> begin
        let rec aux accu idx l =
          match l with
          | e::t when idx = 0 -> (e::accu @ t)
          | e::t -> aux (accu @ [e]) (idx - 1) t
          | [] -> assert false
        in

        let stack = aux [] n stack in
        f env accu it stack
      end

    | T.DIP (1, instrs)::it, a::st -> begin
        let accu, stack = interp env accu instrs st in
        f env accu it (a::stack)
      end

    | T.PUSH (_t, d)::it, _ -> begin
        let data = d in
        let stack, eq = match stack with | a::t -> t, (a, T.Ddata data) | _ -> assert false in
        let accu = eq::accu in
        f env accu it stack
      end

    | T.NIL _::it, _       -> begin
        let data = T.Dlist [] in
        let stack, eq = match stack with | a::t -> t, (a, T.Ddata data) | _ -> assert false in
        let accu = eq::accu in
        f env accu it stack
      end

    | T.PAIR::it, a::st -> begin
        let env, stack, eqs  =
          match a with
          | T.Dbop (Bpair, x, y) -> env, x::y::st, []
          | _ -> begin
              let x = T.dalpha env.cpt_alpha in
              let env = inc_cpt_alpha env in
              let y = T.dalpha env.cpt_alpha in
              let env = inc_cpt_alpha env in
              env, x::y::st, []
            end
        in
        f env (eqs @ accu) it stack
      end

    | T.UNPAIR::it, x::y::st -> begin
        f env accu it (T.Dbop (Bpair, x, y)::st)
      end

    | [], [Dbop (Bpair, a, b)] -> (T.Dparameter tparameter, a)::(T.Dinitstorage tstorage, b)::accu, stack
    | [], _   -> accu, stack
    | i::_, _ -> Format.eprintf "error:@\n %a@." pp_trace (i, stack); assert false
  in

  let env = mk_ir_env () in
  let init_stack : (T.dexpr) list = T.[Dbop (Bpair, Doperations, Dstorage tstorage)] in
  let sys, _ = interp env [] [michelson.code] init_stack in
  Format.printf "sys:@\n%a@." Printer_michelson.pp_sysofequations sys


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
    | Dplist _l        -> assert false
    | Dvar id          -> M.mk_pvar (dumloc id) M.tunit
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
        | Znow                -> assert false
        | Zamount             -> assert false
        | Zbalance            -> assert false
        | Zsource             -> assert false
        | Zsender             -> assert false
        | Zaddress            -> assert false
        | Zchain_id           -> assert false
        | Zself_address       -> assert false
        | Znone _t            -> assert false
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
