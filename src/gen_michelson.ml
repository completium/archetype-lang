open Ident
open Location
open Tools
open Printer_tools

module M = Model
module T = Michelson

exception Anomaly of string

type error_desc =
  | FieldNotFoundFor of string * string
  | UnsupportedTerm of string
  | StackEmptyDec
  | StackIdNotFound of string * string list
  | TODO
[@@deriving show {with_path = false}]

let pp_error_desc fmt e =
  let pp s = Format.fprintf fmt s in
  match e with
  | TODO                       -> pp "TODO"
  | FieldNotFoundFor (rn, fn)  -> pp "Field not found for record '%s' and field '%s'" rn fn
  | UnsupportedTerm s          -> pp "UnsupportedTerm: %s" s
  | StackEmptyDec              -> pp "StackEmptyDec"
  | StackIdNotFound (id, vars) -> pp "StackIdNotFound: %s on [%a]" id (pp_list "; " (fun fmt x -> Format.fprintf fmt "%s" x)) vars

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

type env_ir = {
  function_p: (ident * (ident * T.type_) list) option
}

let mk_env ?function_p _ =
  { function_p }

let to_ir (model : M.model) : T.ir =

  let to_one_gen init f l =
    match List.rev l with
    | [] -> init
    | i::q -> List.fold_left (fun accu x -> f x accu) i q
  in

  let to_one_type (l : T.type_ list) : T.type_ = to_one_gen T.tunit (fun x accu -> (T.mk_type (T.Tpair (x, accu)))) l in

  let to_one_data (l : T.data list) : T.data = to_one_gen (T.Dunit) (fun x accu -> (T.Dpair (x, accu))) l in

  let rec to_type ?annotation (t : M.type_) : T.type_ =
    let node =
      match t with
      | Tasset _   -> assert false
      | Tenum _    -> assert false
      | Tstate     -> assert false
      | Tbuiltin b -> begin
          match b with
          | Bunit      -> T.Tunit
          | Bbool      -> T.Tbool
          | Bint       -> T.Tint
          | Brational  -> assert false
          | Bdate      -> T.Ttimestamp
          | Bduration  -> T.Tint
          | Btimestamp -> T.Ttimestamp
          | Bstring    -> T.Tstring
          | Baddress   -> T.Taddress
          | Brole      -> T.Taddress
          | Bcurrency  -> T.Tmutez
          | Bsignature -> T.Tsignature
          | Bkey       -> T.Tkey
          | Bkeyhash   -> T.Tkey_hash
          | Bbytes     -> T.Tbytes
          | Bnat       -> T.Tnat
          | Bchainid   -> T.Tchain_id
        end
      | Tcontainer _ -> assert false
      | Tlist t      -> T.Tlist (to_type t)
      | Toption t    -> T.Toption (to_type t)
      | Ttuple lt    -> to_one_type (List.map to_type lt) |> fun x -> x.node
      | Tset t -> T.Tset (to_type t)
      | Tmap (b, k, v) -> if b then T.Tbig_map (to_type k, to_type v) else T.Tmap (to_type k, to_type v)
      | Trecord id -> begin
          let r = M.Utils.get_record model (unloc id) in
          let lt = List.map (fun (x : M.record_field) -> x.type_) r.fields in
          to_one_type (List.map to_type lt) |> fun x -> x.node
        end
      | Tunit -> T.Tunit
      | Toperation -> T.Toperation
      | Tentrysig t -> T.Tcontract (to_type t)
      | Tstorage -> assert false
      | Tprog  _ -> assert false
      | Tvset  _ -> assert false
      | Ttrace _ -> assert false
    in
    T.mk_type ?annotation node
  in

  let rec to_data (mt : M.mterm) : T.data =
    match mt.node with
    | Munit             -> T.Dunit
    | Mbool      v      -> (if v then T.Dtrue else T.Dfalse)
    | Mint       n      -> T.Dint n
    | Mnat       n      -> T.Dint n
    | Mstring    s      -> T.Dstring s
    | Mcurrency  (v, _) -> T.Dint v
    | Maddress   v      -> T.Dstring v
    | Mdate      v      -> T.Dint (Core.date_to_timestamp v)
    | Mduration  n      -> T.Dint (Core.duration_to_timestamp n)
    | Mtimestamp v      -> T.Dint v
    | Mbytes     b      -> T.Dbytes b
    | Mnone             -> T.Dnone
    | Msome      v      -> T.Dsome (to_data v)
    | Mtuple     l      -> to_one_data (List.map to_data l)
    | Mlitset    l      -> T.Dlist (List.map to_data l)
    | Mlitlist   l      -> T.Dlist (List.map to_data l)
    | Mlitmap    l      -> T.Dplist (List.map (fun (x, y) -> to_data x, to_data y) l)
    | Muminus    v      -> to_data v |> (function | T.Dint n -> T.Dint (Big_int.mult_int_big_int (-1) n) | _ -> assert false )
    | Mlitrecord l      -> to_one_data (List.map (to_data |@ snd) l)
    | Mnattoint v
    | Mnattorat v
    | Minttorat v
    | Mcast (_, _, v) -> to_data v
    | _ -> Format.printf "%a@." M.pp_mterm mt; assert false

  in

  let get_record_index (rv : M.mterm) fn =
    match rv.type_ with
    | M.Trecord rn -> begin
        let rn = unloc rn in
        let r : M.record = M.Utils.get_record model rn in
        let res = List.fold_lefti (fun i accu (x : M.record_field) ->
            if String.equal fn (unloc x.name) then i else accu) (-1) r.fields in
        match res with
        | -1 -> emit_error (FieldNotFoundFor (rn, fn))
        | _ -> res
      end
    | _ -> assert false
  in

  let l = List.map (
      fun (si : M.storage_item) ->
        (unloc si.id), to_type ~annotation:(unloc si.id) si.typ, to_data si.default)
      model.storage
  in

  let remove_annot (t : T.type_) = {t with annotation = None} in

  let storage_type, storage_data =
    match l with
    | []  -> T.mk_type Tunit, T.Dunit
    | [_, t, d] -> remove_annot t, d
    | _   -> let _, lt, ld = List.split3 l in to_one_type lt, to_one_data ld
  in

  let rec mterm_to_intruction env (mtt : M.mterm) : T.instruction =
    let f = mterm_to_intruction env in
    let ft = to_type in
    match mtt.node with

    (* lambda *)

    | Mletin ([id], v, _, b, _)    -> T.IletIn (unloc id, f v, f b)
    | Mletin _                     -> assert false
    | Mdeclvar (_ids, _t, _v)      -> assert false
    | Mapp (_e, _args)             -> assert false

    (* assign *)

    | Massign (_op, _, Avar id, v)                 -> T.Iassign (unloc id, Local, f v)
    | Massign (_op, _, Avarstore id, v)            -> T.Iassign (unloc id, Storage, f v)
    | Massign (_op, _, Aasset (_an, _fn, _k), _v)  -> assert false
    | Massign (_op, _, Arecord (_rn, _fn, _r), _v) -> assert false
    | Massign (_op, _, Astate, _x)                 -> assert false
    | Massign (_op, _, Aassetstate (_an, _k), _v)  -> assert false
    | Massign (_op, _, Aoperations, _v)            -> assert false

    (* control *)

    | Mif (c, t, Some e)         -> T.Iif (f c, f t, f e)
    | Mif (c, t, None)           -> T.Iif (f c, f t, T.Iseq [])
    | Mmatchwith (_e, _l)        -> assert false
    | Mfor (id, c, b, _)         -> begin
        let ids =
          match id with
          | M.FIsimple x      -> [x]
          | M.FIdouble (x, y) -> [x; y]
        in
        let ids = List.map unloc ids in
        let c =
          match c with
          | ICKcoll  _
          | ICKview  _
          | ICKfield _ -> assert false
          | ICKset   c
          | ICKlist  c
          | ICKmap   c -> f c
        in
        let b = f b in
        T.Iiter (ids, c, b)
      end
    | Miter (_i, _a, _b, _c, _)  -> assert false
    | Mwhile (c, b, _)           -> T.Iwhile (f c, f b)
    | Mseq is                    -> T.Iseq (List.map f is)
    | Mreturn _x                 -> assert false
    | Mlabel _                   -> assert false
    | Mmark  _                   -> assert false


    (* effect *)

    | Mfail ft          -> begin
        let x =
          match ft with
          | Invalid v            -> f v
          | InvalidCaller        -> T.istring "InvalidCaller"
          | InvalidCondition lbl -> T.istring ("InvalidCondition" ^ (match lbl with | Some lbl -> ": " ^ lbl | _ -> ""))
          | NoTransfer           -> T.istring "NoTransfer"
          | AssignNat            -> T.istring "AssignNat"
          | InvalidState         -> T.istring "InvalidState"
        in
        T.Iunop  (Ufail, x)
      end
    | Mtransfer (_v, _k) -> assert false

    (* entrypoint *)

    | Mentrypoint (_, _a, _s) -> assert false
    | Mself _id               -> assert false


    (* operation *)

    | Moperations               -> assert false
    | Mmkoperation (_v, _d, _a) -> assert false


    (* literals *)

    | Mint  v            -> T.Iconst (T.mk_type Tint, Dint v)
    | Mnat  v            -> T.Iconst (T.mk_type Tnat, Dint v)
    | Mbool v            -> T.Iconst (T.mk_type Tbool, if v then Dtrue else Dfalse)
    | Menum _v           -> assert false
    | Mrational (_n, _d) -> assert false
    | Mstring v          -> T.Iconst (T.mk_type Tstring, Dstring v)
    | Mcurrency (v, Utz) -> T.Iconst (T.mk_type Tmutez, Dint v)
    | Mcurrency _        -> assert false
    | Maddress _v        -> assert false
    | Mdate _v           -> assert false
    | Mduration _v       -> assert false
    | Mtimestamp _v      -> assert false
    | Mbytes v           -> T.Iconst (T.mk_type Tbytes, Dbytes v)
    | Munit              -> T.Iconst (T.mk_type Tunit, Dunit)

    (* control expression *)

    | Mexprif (c, t, e)       -> T.Iif (f c, f t, f e)
    | Mexprmatchwith (_e, _l) -> assert false


    (* composite type constructors *)

    | Mnone    -> assert false
    | Msome _v -> assert false

    | Mtuple     l -> begin
        match List.rev l with
        | []  -> T.iunit
        | [e] -> f e
        | e::t -> List.fold_left (fun accu x -> T.Ibinop (Bpair, f x, accu)) (f e) t
      end
    | Masset     _l -> assert false
    | Massets    _l -> assert false
    | Mlitset    l -> begin
        match mtt.type_ with
        |  M.Tset t -> T.Iset (ft t, List.map f l)
        | _ -> assert false
      end
    | Mlitlist   l ->  begin
        match mtt.type_ with
        |  M.Tlist t -> T.Ilist (ft t, List.map f l)
        | _ -> assert false
      end
    | Mlitmap    l -> begin
        match mtt.type_ with
        | M.Tmap (true, k, v)  (* TODO: big map *)
        | M.Tmap (false, k, v) -> T.Imap (ft k, ft v, List.map (fun (x, y) -> f x, f y) l)
        | _ -> assert false
      end
    | Mlitrecord l -> T.Irecord (List.map (fun (_, x) -> f x) l)

    (* access *)

    | Mdot (e, i)           -> let n = get_record_index e (unloc i) in Tools.foldi (fun x -> T.Iunop (Ucdr, x)) (f e) n
    | Mdotassetfield _      -> emit_error (UnsupportedTerm ("dotassetfield"))
    | Mdotcontract (_e, _i) -> assert false
    | Maccestuple (_e, _i)  -> assert false

    (* comparison operators *)

    | Mequal (_, l, r)  -> T.Icompare (Ceq, f l, f r)
    | Mnequal (_, l, r) -> T.Icompare (Cne, f l, f r)
    | Mgt (l, r)        -> T.Icompare (Cgt, f l, f r)
    | Mge (l, r)        -> T.Icompare (Cge, f l, f r)
    | Mlt (l, r)        -> T.Icompare (Clt, f l, f r)
    | Mle (l, r)        -> T.Icompare (Cle, f l, f r)
    | Mmulticomp _      -> emit_error (UnsupportedTerm ("multicomp"))


    (* arithmetic operators *)

    | Mand (l, r)      -> T.Ibinop (Band, f l, f r)
    | Mor (l, r)       -> T.Ibinop (Bor, f l, f r)
    | Mxor (l, r)      -> T.Ibinop (Bxor, f l, f r)
    | Mnot e           -> T.Iunop  (Unot, f e)
    | Mplus (l, r)     -> T.Ibinop (Badd, f l, f r)
    | Mminus (l, r)    -> T.Ibinop (Bsub, f l, f r)
    | Mmult (l, r)     -> T.Ibinop (Bmul, f l, f r)
    | Mdivrat _        -> emit_error (UnsupportedTerm ("divrat"))
    | Mdiveuc (_l, _r) -> assert false
    | Mmodulo (_l, _r) -> assert false
    | Muplus _e        -> assert false
    | Muminus e        -> T.Iunop  (Uneg, f e)

    (* asset api effect *)

    | Maddasset (_an, _i)               -> assert false
    | Maddfield (_an, _fn, _c, _i)      -> assert false
    | Mremoveasset (_an, _i)            -> assert false
    | Mremoveall (_an, _fn, _a)         -> assert false
    | Mremovefield (_an, _fn, _c, _i)   -> assert false
    | Mremoveif (_an, _c, _la, _lb, _a) -> assert false
    | Mclear (_an, _v)                  -> assert false
    | Mset (_c, _l, _k, _v)             -> assert false
    | Mupdate (_an, _k, _l)             -> assert false
    | Maddupdate _                      -> emit_error (UnsupportedTerm ("addupdate"))


    (* asset api expression *)

    | Mget (_an, _c, _k)              -> assert false
    | Mselect (_an, _c, _la, _lb, _a) -> assert false
    | Msort (_an, _c, _l)             -> assert false
    | Mcontains (_an, _c, _i)         -> assert false
    | Mnth (_an, _c, _i)              -> assert false
    | Mcount (_an, _c)                -> assert false
    | Msum (_an, _c, _p)              -> assert false
    | Mhead (_an, _c, _i)             -> assert false
    | Mtail (_an, _c, _i)             -> assert false


    (* utils *)

    | Mcast (_src, _dst, _v) -> assert false
    | Mtupleaccess (x, n)    -> Tools.foldi (fun x -> T.Iunop (Ucdr, x)) (f x) (Big_int.int_of_big_int n)

    (* set api expression *)

    | Msetadd (_, c, a)        -> T.Iterop (Tupdate, f a, T.itrue,  f c)
    | Msetremove (_, c, a)     -> T.Iterop (Tupdate, f a, T.ifalse, f c)
    | Msetcontains (_, c, k)   -> T.Ibinop (Bmem, f k, f c)
    | Msetlength (_, c)        -> T.Iunop  (Usize, f c)


    (* list api expression *)

    | Mlistprepend (_t, i, l)    -> T.Ibinop (Bcons, f l, f i)
    | Mlistcontains (_t, _c, _a) -> assert false
    | Mlistlength (_, l)         -> T.Iunop (Usize, f l)
    | Mlistnth (_t, _c, _a)      -> assert false


    (* map api expression *)

    | Mmapput (_, _, c, k, v)     -> T.Iterop (Tupdate, f k, T.isome (f v),   f c)
    | Mmapremove (_, tv, c, k)    -> T.Iterop (Tupdate, f k, T.inone (ft tv), f c)
    | Mmapget (_, _, _c, _k)      -> assert false
    | Mmapgetopt (_, _, c, k)     -> T.Ibinop (Bget, f k, f c)
    | Mmapcontains (_, _, c, k)   -> T.Ibinop (Bmem, f k, f c)
    | Mmaplength (_, _, c)        -> T.Iunop (Usize, f c)


    (* builtin functions *)

    | Mmax (_l, _r)      -> assert false
    | Mmin (_l, _r)      -> assert false
    | Mabs x             -> T.Iunop (Uabs, f x)
    | Mconcat (x, y)     -> T.Ibinop (Bconcat, f x, f y)
    | Mslice (x, s, e)   -> T.Iterop (Tslice, f x, f s, f e)
    | Mlength x          -> T.Iunop (Usize, f x)
    | Misnone _x         -> assert false
    | Missome _x         -> assert false
    | Moptget _x         -> assert false
    | Mfloor _x          -> assert false
    | Mceil _x           -> assert false
    | Mtostring (_t, _x) -> assert false
    | Mpack x            -> T.Iunop (Upack,  f x)
    | Munpack (t, x)     -> T.Iunop (Uunpack (ft t),  f x)

    (* crypto functions *)

    | Mblake2b x                -> T.Iunop  (Ublake2b,  f x)
    | Msha256  x                -> T.Iunop  (Usha256,   f x)
    | Msha512  x                -> T.Iunop  (Usha512,   f x)
    | Mhashkey x                -> T.Iunop  (Uhash_key, f x)
    | Mchecksignature (k, s, x) -> T.Iterop (Tcheck_signature, f k, f s, f x)


    (* constants *)

    | Mnow           -> T.Izop Znow
    | Mtransferred   -> T.Izop Zamount
    | Mcaller        -> T.Izop Zsender
    | Mbalance       -> T.Izop Zbalance
    | Msource        -> T.Izop Zsource
    | Mselfaddress   -> T.Izop Zself_address
    | Mchainid       -> T.Izop Zchain_id


    (* variable *)

    | Mvar (_an, Vassetstate _k, _, _) -> assert false
    | Mvar (v, Vstorevar, _, _)        -> T.Ivar (unloc v)
    | Mvar (_v, Vstorecol, _, _)       -> assert false
    | Mvar (_v, Venumval, _, _)        -> assert false
    | Mvar (_v, Vdefinition, _, _)     -> assert false
    | Mvar (v, Vlocal, _, _)           -> T.Ivar (unloc v)
    | Mvar (v, Vparam, _, _)           -> T.Ivar (unloc v)
    | Mvar (_v, Vfield, _, _)          -> assert false
    | Mvar (_, Vthe, _, _)             -> assert false
    | Mvar (_, Vstate, _, _)           -> assert false

    (* rational *)

    | Mrateq (_l, _r)         -> assert false
    | Mratcmp (_op, _l, _r)   -> assert false
    | Mratarith (_op, _l, _r) -> assert false
    | Mratuminus _v           -> assert false
    | Mrattez (_c, _t)        -> assert false
    | Mdivtez (_c, _t)        -> assert false
    | Mnattoint e             -> T.Iunop (Uint, f e)
    | Mnattorat _e            -> assert false
    | Minttorat _e            -> assert false
    | Mratdur (_c, _t)        -> assert false


    (* functional *)

    | Mfold _ -> emit_error (UnsupportedTerm ("fold"))


    (* imperative *)

    | Mbreak -> emit_error (UnsupportedTerm ("break"))


    (* quantifiers *)

    | Mforall _ -> emit_error (UnsupportedTerm ("forall"))
    | Mexists _ -> emit_error (UnsupportedTerm ("exists"))


    (* formula operators *)

    | Mimply _ -> emit_error (UnsupportedTerm ("imply"))
    | Mequiv _ -> emit_error (UnsupportedTerm ("equiv"))


    (* formula asset collection *)

    | Msetiterated  _ -> emit_error (UnsupportedTerm ("setiterated"))
    | Msettoiterate _ -> emit_error (UnsupportedTerm ("settoiterate"))


    (* formula asset collection methods *)

    | Mempty     _ -> emit_error (UnsupportedTerm ("empty"))
    | Msingleton _ -> emit_error (UnsupportedTerm ("singleton"))
    | Msubsetof  _ -> emit_error (UnsupportedTerm ("subsetof"))
    | Misempty   _ -> emit_error (UnsupportedTerm ("isempty"))
    | Munion     _ -> emit_error (UnsupportedTerm ("union"))
    | Minter     _ -> emit_error (UnsupportedTerm ("inter"))
    | Mdiff      _ -> emit_error (UnsupportedTerm ("diff"))
  in

  let env = mk_env () in

  let entries =
    let for_fs env (fs : M.function_struct) : T.entry =
      let name = unloc fs.name in
      let args = List.map (fun (id, t, _) -> unloc id, to_type t) fs.args in
      let env = {env with function_p = Some (name, args)} in
      let body = mterm_to_intruction env fs.body in
      T.mk_entry name args body
    in

    List.fold_right (fun (x : M.function__) accu -> match x.node with | Entry fs -> (for_fs env fs)::accu | _ -> accu ) model.functions []
  in

  let annot a (t : T.type_) = { t with annotation = Some a} in
  let l = List.map (fun (x, y, _) -> (x, y)) l in
  let parameter : T.type_ =
    let for_entry (e : T.entry) =
      match List.rev e.args with
      | []   -> T.tunit
      | [e]  -> snd e
      | (id, te)::t -> List.fold_left (fun accu (id, te) -> T.mk_type (T.Tpair (annot id te, accu))) (annot id te) t
    in
    match List.rev entries with
    | [] -> T.tunit
    | [e] -> for_entry e
    | i::t -> begin
        let for_entry e = e |> for_entry |> (fun (x : T.type_) -> annot e.name x) in
        List.fold_left (fun accu x -> (T.mk_type (T.Tor (for_entry x, accu)))) (for_entry i) t
      end
  in
  T.mk_ir storage_type storage_data l parameter entries

type env = {
  vars : ident list;
}
[@@deriving show {with_path = false}]

let mk_env ?(vars=[]) () = { vars = vars}
let inc_env (env : env) = { env with vars = "_"::env.vars }
let dec_env (env : env) = { env with vars = match env.vars with | _::t -> t | _ -> emit_error StackEmptyDec }
let add_var_env (env : env) id = { env with vars = id::env.vars }
let get_sp_for_id (env : env) id =
  match List.index_of (String.equal id) env.vars with
  | -1 -> emit_error (StackIdNotFound (id, env.vars))
  | v -> v

let print_env str env =
  Format.eprintf "%s: %a@." str pp_env env
(* Format.eprintf "var %s: %i@." id n; *)

let to_michelson (ir : T.ir) : T.michelson =
  let storage = ir.storage_type in
  (* let default = T.SEQ [T.CDR; T.NIL (T.mk_type Toperation); T.PAIR ] in *)

  let rec instruction_to_code env (i : T.instruction) : T.code * env =
    let fe env = instruction_to_code env in
    let f = fe env in

    let seq env l =
      match l with
      | []   -> T.SEQ [], env
      | [e]  -> f e
      | e::t ->
        List.fold_left (fun (a, env) x -> begin
              let v, env = fe env x in (T.SEQ [a; v], env)
            end ) (f e) t
    in

    match i with
    | Iseq l               -> seq env l

    | IletIn (id, v, b)    -> begin
        let v, _ = f v in
        let env0 = add_var_env env id in
        let b, _ = fe env0 b in
        T.SEQ [v; b; T.DROP 1], env
      end

    | Ivar id              -> begin
        let n = get_sp_for_id env id in
        let c =
          if n = 0
          then T.DUP
          else T.SEQ [ T.DIG n; T.DUP; T.DUG (n + 1)]
        in
        c, inc_env env
      end

    | Icall (_id, _args)   -> assert false

    | Iassign (id, _, v)  -> begin
        let n = get_sp_for_id env id in
        let v, _env0 = f v in
        (* Format.eprintf "%a@." pp_env _env0; *)
        (* Format.eprintf "assign %s: %i@." id n; *)
        let c =
          if n <= 0
          then T.SEQ [ v; T.SWAP; T.DROP 1 ]
          else T.SEQ [ v; (T.DIP (1, [T.DIG n; T.DROP 1])); T.DUG n]
        in
        c, env
      end

    | Iif (c, t, e) -> begin
        let c, _ = f c in
        let t, _ = f t in
        let e, _ = f e in

        T.SEQ [ c; T.IF ([t], [e]) ], env
      end

    | Iwhile (c, b) -> begin
        let c, _ = f c in
        let b, _ = f b in
        T.SEQ [c; T.LOOP [b; c]] , env
      end

    | Iiter (ids, c, b) -> begin
        let c, _ = f c in
        match ids with
        | [id] -> begin
            let env0 = add_var_env env id in
            let b, _ = fe env0 b in
            T.SEQ [c; T.ITER [b; T.DROP 1]] , env
          end
        | [k; v] -> begin
            let env0 = add_var_env (add_var_env env v) k in
            let b, _ = fe env0 b in
            T.SEQ [c; T.ITER [T.UNPAIR; b; T.DROP 2]] , env
          end
        | _ -> assert false
      end

    | Izop op -> begin
        let c =
          match op with
          | Znow               -> T.NOW
          | Zamount            -> T.AMOUNT
          | Zbalance           -> T.BALANCE
          | Zsource            -> T.SOURCE
          | Zsender            -> T.SENDER
          | Zaddress           -> T.ADDRESS
          | Zchain_id          -> T.CHAIN_ID
          | Zself_address      -> T.SEQ [T.SELF; T.ADDRESS]
          | Znone t            -> T.NONE t
        in
        c, inc_env env
      end
    | Iunop (op, e) -> begin
        let c =
          let op =
            match op with
            | Ucar      -> T.CAR
            | Ucdr      -> T.CDR
            | Uneg      -> T.NEG
            | Uint      -> T.INT
            | Unot      -> T.NOT
            | Uabs      -> T.ABS
            | Uisnat    -> T.ISNAT
            | Usome     -> T.SOME
            | Usize     -> T.SIZE
            | Upack     -> T.PACK
            | Uunpack t -> T.UNPACK t
            | Ublake2b  -> T.BLAKE2B
            | Usha256   -> T.SHA256
            | Usha512   -> T.SHA512
            | Uhash_key -> T.HASH_KEY
            | Ufail     -> T.FAILWITH
          in
          let e, _ = f e in
          T.SEQ [e; op]
        in
        c, env
      end
    | Ibinop (op, lhs, rhs) -> begin
        let c =
          let op =
            match op with
            | Badd       -> T.ADD
            | Bsub       -> T.SUB
            | Bmul       -> T.MUL
            | Bediv      -> T.EDIV
            | Blsl       -> T.LSL
            | Blsr       -> T.LSR
            | Bor        -> T.OR
            | Band       -> T.AND
            | Bxor       -> T.XOR
            | Bcompare   -> T.COMPARE
            | Bget       -> T.GET
            | Bmem       -> T.MEM
            | Bconcat    -> T.CONCAT
            | Bcons      -> T.CONS
            | Bpair      -> T.PAIR
          in
          let rhs, env = f rhs in
          let lhs, _   = fe env lhs in
          T.SEQ [rhs; lhs; op]
        in
        c, env
      end
    | Iterop (op, a1, a2, a3) -> begin
        let c =
          let op =
            match op with
            | Tcheck_signature -> T.CHECK_SIGNATURE
            | Tslice           -> T.SLICE
            | Tupdate          -> T.UPDATE
          in
          let a3, env = fe env a3 in
          let a2, env = fe env a2 in
          let a1, _   = fe env a1 in
          T.SEQ [a3; a2; a1; op]
        in
        c, env
      end
    | Iconst (t, e) -> T.PUSH (t, e), inc_env env
    | Icompare (op, lhs, rhs) -> begin
        let c =
          let op =
            match op with
            | Ceq -> T.EQ
            | Cne -> T.NEQ
            | Clt -> T.LT
            | Cle -> T.LE
            | Cgt -> T.GT
            | Cge -> T.GE
          in
          let r, _ = f rhs in
          let l, _ = f lhs in
          T.SEQ [r; l; T.COMPARE; op]
        in
        c, env
      end

    | Iset (t, l) -> begin
        T.SEQ ((T.EMPTY_SET t)::(l |> List.rev |> List.map (fun x -> let x, _ = f x in T.SEQ [T.ctrue; x; T.UPDATE ] ))), inc_env env
      end
    | Ilist (t, l) -> begin
        T.SEQ ((T.NIL t)::(l |> List.rev |> List.map (fun x -> let x, _ = f x in T.SEQ [ x; T.CONS ] ))), inc_env env
      end
    | Imap (k, v, l) -> begin
        T.SEQ ([T.EMPTY_MAP (k, v)] @
               (l
                |> List.rev
                |> List.map (fun (x, y) ->
                    let y, _ = f y in
                    let x, _ = f x in
                    T.SEQ [y; T.SOME; x; T.UPDATE ] ))), inc_env env
      end
    | Irecord l -> begin
        match List.rev l with
        | []  -> T.SEQ [], inc_env env
        | [e] -> f e
        | a::q -> begin
            (T.SEQ [let a, _ = f a in List.fold_left (fun accu x -> let x, _ = f x in T.SEQ [accu; x; T.PAIR] ) a q ]), inc_env env
          end
      end
  in

  let build_code _ =

    let unfold = foldi (fun x -> T.UNPAIR::T.SWAP::x ) [] in

    let vars = List.map fst ir.storage_list |> List.rev in
    let env = mk_env () ~vars in
    let nb_storage_item = List.length ir.storage_list in
    let nb_fs = nb_storage_item - 1 in
    let unfold_storage = unfold nb_fs  in
    let fold_storage = foldi (fun x -> T.SWAP::T.PAIR::x ) [] nb_fs in

    let code =
      match List.rev ir.entries with
      | []   -> assert false
      | [e]  -> begin
          match e.args with
          | [] -> begin
              let code, _ = instruction_to_code env e.body in
              T.SEQ ([T.CDR] @ unfold_storage @ [code] @ fold_storage @ [T.NIL (T.toperation); T.PAIR ])
            end
          | l -> begin
              let nb_args = List.length l in
              let nb_as = nb_args - 1 in
              let unfold_args = unfold nb_as in
              let args = List.map fst l |> List.rev in
              let env = { env with vars = args @ env.vars } in
              let code, _ = instruction_to_code env e.body in
              T.SEQ ([T.DUP; T.CDR] @ unfold_storage @ [T.DIG nb_storage_item; T.CAR] @ unfold_args @ [code] @ [T.DROP nb_args] @ fold_storage @ [T.NIL (T.toperation); T.PAIR ])
            end
        end
      | e::l -> begin
          let for_entry (e : T.entry) =
            let nb_args = List.length e.args in
            let nb_as = nb_args - 1 in
            let unfold_args = unfold nb_as in
            let args = List.map fst e.args |> List.rev in
            let env = { env with vars = args @ env.vars } in
            let code, _ = instruction_to_code env e.body in
            T.SEQ (unfold_args @ [code] @ [T.DROP nb_args] @ fold_storage @ [T.NIL (T.toperation); T.PAIR ])
          in
          let c : T.code = List.fold_left (fun accu x -> T.IF_LEFT ([for_entry x], [accu])) (for_entry e) l in
          T.SEQ ([T.DUP; T.CDR] @ unfold_storage @ [T.DIG nb_storage_item; T.CAR; c])
        end
    in
    code
    |> T.Utils.flat
    |> T.Utils.optim
  in

  let code = build_code () in
  T.mk_michelson storage ir.parameter code
