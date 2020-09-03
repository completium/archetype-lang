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
  | NoConcreteImplementationFor of string
  | TODO
[@@deriving show {with_path = false}]

let pp_error_desc fmt e =
  let pp s = Format.fprintf fmt s in
  match e with
  | TODO                          -> pp "TODO"
  | FieldNotFoundFor (rn, fn)     -> pp "Field not found for record '%s' and field '%s'" rn fn
  | UnsupportedTerm s             -> pp "UnsupportedTerm: %s" s
  | StackEmptyDec                 -> pp "StackEmptyDec"
  | StackIdNotFound (id, vars)    -> pp "StackIdNotFound: %s on [%a]" id (pp_list "; " (fun fmt x -> Format.fprintf fmt "%s" x)) vars
  | NoConcreteImplementationFor s -> pp "No concrete implementation for: %s" s

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

let get_fun_name = T.Utils.get_fun_name Printer_michelson.show_pretty_type

let operations = "_ops"
let fun_result = "_fun_res"

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
    | Mbool      true   -> T.Dtrue
    | Mbool      false  -> T.Dfalse
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

  let get_record_size (rt : M.type_) : int =
    match rt with
    | M.Trecord rn -> begin
        let rn = unloc rn in
        let r : M.record = M.Utils.get_record model rn in
        List.length r.fields
      end
    | _ -> Format.eprintf "%a@." M.pp_type_ rt; assert false
  in

  let get_record_index (rt : M.type_) fn =
    match rt with
    | M.Trecord rn -> begin
        let rn = unloc rn in
        let r : M.record = M.Utils.get_record model rn in
        let res = List.fold_lefti (fun i accu (x : M.record_field) ->
            if String.equal fn (unloc x.name) then i else accu) (-1) r.fields in
        match res with
        | -1 -> emit_error (FieldNotFoundFor (rn, fn))
        | _ -> res
      end
    | _ -> Format.eprintf "%a@." M.pp_type_ rt; assert false
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

  let builtins = ref [] in

  let add_builtin b =
    if not (List.exists (T.cmp_builtin b) !builtins)
    then builtins := b::!builtins;
  in

  let get_builtin_fun b =
    let return x = T.Iassign (fun_result, x) in
    let name = T.Utils.get_fun_name Printer_michelson.show_pretty_type b in
    match b with
    | Bmin t
    | Bmax t -> begin
        let targ = T.tpair t t in
        let tret = t in
        T.mk_func name targ tret (T.Abstract b)
      end
    | Bfloor
    | Bceil -> begin
        let targ = T.trat in
        let tret  = T.tint in
        T.mk_func name targ tret (T.Abstract b)
      end
    | BlistContains t -> begin
        let targ = T.tpair (T.tlist t) t in
        let tret = T.tbool in
        T.mk_func name targ tret (T.Abstract b)
      end
    | BlistNth t -> begin
        let targ = T.tpair (T.tlist t) T.tnat in
        let tret = t in
        let args, body = begin
          let arg_name  = "idx" in
          let list_name = "l" in
          let args      = [list_name, T.tlist t; arg_name, T.tnat] in
          let res_name  = "r" in
          let iter_name = "i" in
          let e_name    = "e" in
          let varg      = T.Ivar arg_name in
          let vlist     = T.Ivar list_name in
          let vres      = T.Ivar res_name in
          let viter     = T.Ivar iter_name in
          let ve        = T.Ivar e_name in
          let return    = T.Iassign (fun_result, T.Iifnone (vres, T.ifail "NoneValue", id, "_var_ifnone")) in
          let cond      = T.Icompare (Cle, viter, varg) in
          let vheadtail = T.Iifcons (vlist, T.iskip, T.ifail "EmptyList") in
          let ares      = T.Iassign (res_name, T.isome(T.icar ve)) in
          let alist     = T.Iassign (list_name, T.icdr ve) in
          let aiter     = T.Iassign (iter_name, T.Ibinop (Badd, viter, T.inat Big_int.unit_big_int)) in
          let bloop     = T.IletIn(e_name, vheadtail, T.Iseq [ares; alist; aiter]) in
          let loop      = T.Iwhile (cond, bloop) in
          let body      = T.IletIn(res_name, T.inone t, IletIn(iter_name, T.inat Big_int.zero_big_int, T.Iseq [loop; return])) in
          args, body
        end
        in
        T.mk_func name targ tret (T.Concrete (args, body))
      end
    | Btostring t -> begin
        let targ = t in
        let tret = T.tstring in
        let args, body = begin
          let arg_name     = "x" in
          let args         = [arg_name, t] in
          let res_name     = "res" in
          let map_name     = "m" in
          let pair_name    = "p" in
          let zero   = T.inat Big_int.zero_big_int in
          let ten    = T.inat (Big_int.big_int_of_int 10) in
          let varg   = T.Ivar arg_name in
          let vres   = T.Ivar res_name in
          let vmap   = T.Ivar map_name in
          let vpair  = T.Ivar pair_name in
          let cond   = T.Icompare(Cgt, varg, zero) in
          let map    = T.Imap (T.tnat, T.tstring, [T.inat (Big_int.big_int_of_int 0), T.istring "0";
                                                   T.inat (Big_int.big_int_of_int 1), T.istring "1";
                                                   T.inat (Big_int.big_int_of_int 2), T.istring "2";
                                                   T.inat (Big_int.big_int_of_int 3), T.istring "3";
                                                   T.inat (Big_int.big_int_of_int 4), T.istring "4";
                                                   T.inat (Big_int.big_int_of_int 5), T.istring "5";
                                                   T.inat (Big_int.big_int_of_int 6), T.istring "6";
                                                   T.inat (Big_int.big_int_of_int 7), T.istring "7";
                                                   T.inat (Big_int.big_int_of_int 8), T.istring "8";
                                                   T.inat (Big_int.big_int_of_int 9), T.istring "9"]) in
          let get_map    = T.Iifnone (T.Ibinop (Bget, T.Iunop (Ucdr, vpair), vmap), T.ifail "GetNoneValue", id, "_var_ifnone") in
          let concat     = T.Ibinop (Bconcat, get_map, vres) in
          let assign_res = T.Iassign (res_name, concat) in
          let assign_arg = T.Iassign (arg_name, T.Iunop (Ucar, vpair)) in
          let vpair      = T.Iifnone (T.Ibinop (Bediv, varg, ten), T.ifail "DivByZero", id, "_var_ifnone") in
          let b          = T.IletIn(pair_name, vpair, T.Iseq [assign_res; assign_arg]) in
          let loop       = T.Iwhile (cond, b) in
          let a          = T.IletIn(res_name, T.istring "", IletIn(map_name, map, T.Iseq [loop; return vres]) ) in
          args, T.Iif (cond, a, return (T.istring "0"))
        end
        in
        T.mk_func name targ tret (T.Concrete (args, body))
      end
    | Bratcmp -> begin
        let targ = T.tpair (T.tpair T.trat T.trat) (T.tor T.tunit (T.tor (T.tor T.tunit T.tunit) (T.tor T.tunit T.tunit))) in
        let tret = T.tbool in
        T.mk_func name targ tret (T.Abstract b)
      end
    | Bratnorm -> begin
        let targ = T.trat in
        let tret = T.trat in
        T.mk_func name targ tret (T.Abstract b)
      end
    | Brataddsub -> begin
        let targ = T.tpair (T.tpair T.trat T.trat) (T.tor T.tunit T.tunit) in
        let tret = T.trat in
        T.mk_func name targ tret (T.Abstract b)
      end
    | Bratmul
    | Bratdiv -> begin
        let targ = T.tpair T.trat T.trat in
        let tret = T.trat in
        T.mk_func name targ tret (T.Abstract b)
      end
    | Bratuminus -> begin
        let targ = T.trat in
        let tret = T.trat in
        T.mk_func name targ tret (T.Abstract b)
      end
    | Brattez -> begin
        let targ = T.tpair T.trat T.tmutez in
        let tret = T.tmutez in
        T.mk_func name targ tret (T.Abstract b)
      end
    | Bratdur -> begin
        let targ = T.tpair T.trat T.tint in
        let tret = T.tint in
        T.mk_func name targ tret (T.Abstract b)
      end
  in

  let rec mterm_to_intruction env (mtt : M.mterm) : T.instruction =
    let f = mterm_to_intruction env in
    let ft = to_type in

    let vops = T.Ivar operations in
    let contract_internal a t d = T.Iifnone (T.Iunop (Ucontract (t, a), d), T.ifail "BadContract", id, "_var_ifnone") in
    let get_contract      t d = contract_internal  None     t d in
    let get_entrypoint id t d = contract_internal (Some ("%" ^ id)) t d in
    let get_self_entrypoint id =
      let fs = M.Utils.get_fs model id in
      let ts = List.map proj3_2 fs.args in
      get_entrypoint id (to_one_type (List.map to_type ts)) (T.Izop Zself_address)
    in

    let mk_tuple l =
      match List.rev l with
      | []  -> T.iunit
      | [e] -> e
      | e::t -> List.fold_left (fun accu x -> T.Ibinop (Bpair, x, accu)) e t
    in

    let access_record n x =
      if n = 0
      then T.icar x
      else Tools.foldi T.icdr x n
    in

    match mtt.node with

    (* lambda *)

    | Mletin ([id], v, _, b, _) -> T.IletIn (unloc id, f v, f b)
    | Mletin _                  -> emit_error (UnsupportedTerm ("Mletin"))
    | Mdeclvar _                -> emit_error (UnsupportedTerm ("Mdeclvar"))
    | Mapp (e, args)            -> T.Icall (unloc e, List.map f args)

    (* assign *)

    | Massign (_op, _, Avar id, v)                 -> T.Iassign (unloc id, f v)
    | Massign (_op, _, Avarstore id, v)            -> T.Iassign (unloc id, f v)
    | Massign (_op, _, Aasset (_an, _fn, _k), _v)  -> emit_error TODO
    | Massign (_op, _, Arecord (_rn, fn, {node = Mvar (id, _, _, _); type_ = t}), v) -> begin
        let s = get_record_size t in
        let n = get_record_index t (unloc fn) in T.IassignRec (unloc id, s, n, f v)
      end
    | Massign (_op, _, Arecord _, _v)              -> T.iskip
    | Massign (_op, _, Astate, _x)                 -> emit_error TODO
    | Massign (_op, _, Aassetstate (_an, _k), _v)  -> emit_error TODO
    | Massign (_op, _, Aoperations, v)             -> T.Iassign (operations, f v)

    (* control *)

    | Mif (c, t, Some e)         -> T.Iif (f c, f t, f e)
    | Mif (c, t, None)           -> T.Iif (f c, f t, T.iskip)
    | Mmatchwith (_e, _l)        -> emit_error (UnsupportedTerm ("Mmatchwith"))
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
          | ICKfield _ -> emit_error TODO
          | ICKset   c
          | ICKlist  c
          | ICKmap   c -> f c
        in
        let b = f b in
        T.Iiter (ids, c, b)
      end
    | Miter (_i, _a, _b, _c, _)  -> emit_error TODO
    | Mwhile (c, b, _)           -> T.Iwhile (f c, f b)
    | Mseq is                    -> T.Iseq (List.map f is)
    | Mreturn x                  -> T.Iassign (fun_result, f x)
    | Mlabel _                   -> T.iskip
    | Mmark  _                   -> T.iskip


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
    | Mtransfer (v, k) -> begin
        let arg, entry =
          match k with
          | TKsimple d           -> T.iunit, get_contract T.tunit (f d)
          | TKcall (id, t, d, a) -> f a, get_entrypoint id (to_type t) (f d)
          | TKentry (e, a)       -> f a, f e
          | TKself (id, args)    -> begin
              let a = T.Irecord (List.map (fun (_, x) -> f x) args) in
              a, get_self_entrypoint id
            end
        in
        T.Iassign (operations, T.Ibinop (Bcons, T.Iterop (Ttransfer_tokens, arg, f v, entry), vops))
      end

    (* entrypoint *)

    | Mentrypoint (t, id, d)  -> T.Iunop (Ucontract (to_type t, Some (unloc id)), f d)
    | Mself id                -> get_self_entrypoint (unloc id)


    (* operation *)

    | Moperations            -> vops
    | Mmkoperation (v, e, a) -> T.Iterop (Ttransfer_tokens, f a, f v, f e)


    (* literals *)

    | Mint  v            -> T.Iconst (T.mk_type Tint, Dint v)
    | Mnat  v            -> T.Iconst (T.mk_type Tnat, Dint v)
    | Mbool true         -> T.Iconst (T.mk_type Tbool, Dtrue)
    | Mbool false        -> T.Iconst (T.mk_type Tbool, Dfalse)
    | Menum _            -> emit_error (UnsupportedTerm ("Menum"))
    | Mrational _        -> emit_error (UnsupportedTerm ("Mrational"))
    | Mstring v          -> T.Iconst (T.mk_type Tstring, Dstring v)
    | Mcurrency (v, Utz) -> T.Iconst (T.mk_type Tmutez, Dint v)
    | Mcurrency _        -> emit_error (UnsupportedTerm ("Mcurrency"))
    | Maddress v         -> T.Iconst (T.mk_type Taddress, Dstring v)
    | Mdate v            -> T.Iconst (T.mk_type Ttimestamp, Dint (Core.date_to_timestamp v))
    | Mduration v        -> T.Iconst (T.mk_type Tint, Dint (Core.duration_to_timestamp v))
    | Mtimestamp v       -> T.Iconst (T.mk_type Ttimestamp, Dint v)
    | Mbytes v           -> T.Iconst (T.mk_type Tbytes, Dbytes v)
    | Munit              -> T.Iconst (T.mk_type Tunit, Dunit)

    (* control expression *)

    | Mexprif (c, t, e)       -> T.Iif (f c, f t, f e)
    | Mexprmatchwith (_e, _l) -> emit_error (UnsupportedTerm ("Mexprmatchwith"))


    (* composite type constructors *)

    | Mnone    -> begin
        let t =
          match mtt.type_ with
          | M.Toption t -> to_type t
          | _ -> assert false
        in
        T.Izop (T.Znone t)
      end

    | Msome v -> T.Iunop (Usome, f v)

    | Mtuple l      -> mk_tuple (List.map f l)
    | Masset     _l -> emit_error (UnsupportedTerm ("Masset"))
    | Massets    _l -> emit_error (UnsupportedTerm ("Massets"))
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

    | Mdot (e, i)           -> let n = get_record_index e.type_ (unloc i) in access_record n (f e)
    | Mdotassetfield _      -> emit_error (UnsupportedTerm ("Mdotassetfield"))

    (* comparison operators *)

    | Mequal (_, l, r)  -> T.Icompare (Ceq, f l, f r)
    | Mnequal (_, l, r) -> T.Icompare (Cne, f l, f r)
    | Mgt (l, r)        -> T.Icompare (Cgt, f l, f r)
    | Mge (l, r)        -> T.Icompare (Cge, f l, f r)
    | Mlt (l, r)        -> T.Icompare (Clt, f l, f r)
    | Mle (l, r)        -> T.Icompare (Cle, f l, f r)
    | Mmulticomp _      -> emit_error (UnsupportedTerm ("Mmulticomp"))


    (* arithmetic operators *)

    | Mand (l, r)      -> T.Ibinop (Band, f l, f r)
    | Mor (l, r)       -> T.Ibinop (Bor, f l, f r)
    | Mxor (l, r)      -> T.Ibinop (Bxor, f l, f r)
    | Mnot e           -> T.Iunop  (Unot, f e)
    | Mplus (l, r)     -> T.iadd (f l) (f r)
    | Mminus (l, r)    -> T.isub (f l) (f r)
    | Mmult (l, r)     -> T.imul (f l) (f r)
    | Mdivrat _        -> emit_error (UnsupportedTerm ("Mdivrat"))
    | Mdiveuc (l, r)   -> T.idiv (f l) (f r)
    | Mmodulo (l, r)   -> T.imod (f l) (f r)
    | Muminus e        -> T.Iunop  (Uneg, f e)

    (* asset api effect *)

    | Maddasset _    -> emit_error (UnsupportedTerm ("Maddasset"))
    | Maddfield _    -> emit_error (UnsupportedTerm ("Maddfield"))
    | Mremoveasset _ -> emit_error (UnsupportedTerm ("Mremoveasset"))
    | Mremoveall _   -> emit_error (UnsupportedTerm ("Mremoveall"))
    | Mremovefield _ -> emit_error (UnsupportedTerm ("Mremovefield"))
    | Mremoveif _    -> emit_error (UnsupportedTerm ("Mremoveif"))
    | Mclear _       -> emit_error (UnsupportedTerm ("Mclear"))
    | Mset _         -> emit_error (UnsupportedTerm ("Mset"))
    | Mupdate _      -> emit_error (UnsupportedTerm ("Mupdate"))
    | Maddupdate _   -> emit_error (UnsupportedTerm ("Maddupdate"))
    | Maddforce _    -> emit_error (UnsupportedTerm ("Maddforce"))


    (* asset api expression *)

    | Mget      _ -> emit_error (UnsupportedTerm ("Mget"))
    | Mselect   _ -> emit_error (UnsupportedTerm ("Mselect"))
    | Msort     _ -> emit_error (UnsupportedTerm ("Msort"))
    | Mcontains _ -> emit_error (UnsupportedTerm ("Mcontains"))
    | Mnth      _ -> emit_error (UnsupportedTerm ("Mnth"))
    | Mcount    _ -> emit_error (UnsupportedTerm ("Mcount"))
    | Msum      _ -> emit_error (UnsupportedTerm ("Msum"))
    | Mhead     _ -> emit_error (UnsupportedTerm ("Mhead"))
    | Mtail     _ -> emit_error (UnsupportedTerm ("Mtail"))


    (* utils *)

    | Mcast (src, dst, v) -> begin
        match src, dst with
        | M.Tbuiltin Baddress, M.Tentrysig t    -> get_contract (to_type t) (f v)
        | M.Tbuiltin Bcurrency, M.Tbuiltin Bnat -> T.idiv (f v) (T.imutez Big_int.unit_big_int)
        | _ -> f v
      end
    | Mtupleaccess (x, n) -> access_record (Big_int.int_of_big_int n) (f x)
    | Mrecupdate (x, l) ->
      let s = get_record_size mtt.type_ in
      let ll =
        l
        |> List.map (fun (i, v) -> get_record_index mtt.type_ i, f v)
        |> List.sort (fun (i1, _) (i2, _) -> i1 - i2)
      in
      T.Irecupdate (f x, s, ll)

    (* set api expression *)

    | Msetadd (_, c, a)        -> T.Iterop (Tupdate, f a, T.itrue,  f c)
    | Msetremove (_, c, a)     -> T.Iterop (Tupdate, f a, T.ifalse, f c)
    | Msetcontains (_, c, k)   -> T.Ibinop (Bmem, f k, f c)
    | Msetlength (_, c)        -> T.Iunop  (Usize, f c)


    (* list api expression *)

    | Mlistprepend (_t, i, l)    -> T.Ibinop (Bcons, f l, f i)
    | Mlistheadtail (_t, l)      -> T.Iifcons (f l, T.iskip, T.ifail "EmptyList")
    | Mlistlength (_, l)         -> T.Iunop (Usize, f l)
    | Mlistcontains (t, c, a)    -> let b = T.BlistContains (to_type t) in add_builtin b; T.Icall (get_fun_name b, [f c; f a])
    | Mlistnth (t, c, a)         -> let b = T.BlistNth (to_type t) in add_builtin b; T.Icall (get_fun_name b, [f c; f a])

    (* map api expression *)

    | Mmapput (_, _, c, k, v)     -> T.Iterop (Tupdate, f k, T.isome (f v),   f c)
    | Mmapremove (_, tv, c, k)    -> T.Iterop (Tupdate, f k, T.inone (ft tv), f c)
    | Mmapget (_, _, c, k)        -> T.Iifnone (T.Ibinop (Bget, f k, f c), T.ifail "GetNoneValue", id, "_var_ifnone")
    | Mmapgetopt (_, _, c, k)     -> T.Ibinop (Bget, f k, f c)
    | Mmapcontains (_, _, c, k)   -> T.Ibinop (Bmem, f k, f c)
    | Mmaplength (_, _, c)        -> T.Iunop (Usize, f c)


    (* builtin functions *)

    | Mmax (l, r)        -> let b = T.Bmax (to_type l.type_) in add_builtin b; T.Icall (get_fun_name b, [f l; f r])
    | Mmin (l, r)        -> let b = T.Bmin (to_type l.type_) in add_builtin b; T.Icall (get_fun_name b, [f l; f r])
    | Mabs x             -> T.Iunop (Uabs, f x)
    | Mconcat (x, y)     -> T.Ibinop (Bconcat, f x, f y)
    | Mslice (x, s, e)   -> T.Iterop (Tslice, f x, f s, f e)
    | Mlength x          -> T.Iunop (Usize, f x)
    | Misnone x          -> T.Iifnone (f x, T.itrue,  (fun _ -> T.ifalse), "_var_ifnone")
    | Missome x          -> T.Iifnone (f x, T.ifalse, (fun _ -> T.itrue), "_var_ifnone")
    | Moptget x          -> T.Iifnone (f x, T.ifail "NoneValue", id, "_var_ifnone")
    | Mfloor  x          -> let b = T.Bfloor           in add_builtin b; T.Icall (get_fun_name b, [f x])
    | Mceil   x          -> let b = T.Bceil            in add_builtin b; T.Icall (get_fun_name b, [f x])
    | Mtostring (t, x)   -> let b = T.Btostring (ft t) in add_builtin b; T.Icall (get_fun_name b, [f x])
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
    | Mvar (v, Vstorecol, _, _)        -> T.Ivar (unloc v)
    | Mvar (_v, Venumval, _, _)        -> assert false
    | Mvar (_v, Vdefinition, _, _)     -> assert false
    | Mvar (v, Vlocal, _, _)           -> T.Ivar (unloc v)
    | Mvar (v, Vparam, _, _)           -> T.Ivar (unloc v)
    | Mvar (_v, Vfield, _, _)          -> assert false
    | Mvar (_, Vthe, _, _)             -> assert false
    | Mvar (_, Vstate, _, _)           -> assert false

    (* rational *)

    | Mrateq (l, r)           -> let b = T.Bratcmp in add_builtin b; T.Icall (get_fun_name b, [T.Irecord [f l; f r]; T.ileft (T.tor (T.tor T.tunit T.tunit) (T.tor T.tunit T.tunit)) T.iunit])
    | Mratcmp (op, l, r)   ->
      let op =
        let u    = T.iunit in
        let tu   = T.tunit in
        let tou  = T.tor tu tu in
        (* let toou = T.tor tou tou in *)
        match op with
        | Lt -> T.iright tu (T.ileft  tou (T.ileft  tu u))
        | Le -> T.iright tu (T.ileft  tou (T.iright tu u))
        | Gt -> T.iright tu (T.iright tou (T.ileft  tu u))
        | Ge -> T.iright tu (T.iright tou (T.iright tu u))
      in
      let b = T.Bratcmp in add_builtin b; T.Icall (get_fun_name b, [T.Irecord [f l; f r]; op])
    | Mratarith (op, l, r)    -> begin
        (* let norm x = let b = T.Bratnorm in add_builtin b; T.Icall (get_fun_name b, [x]) in *)
        let norm x = x in
        match op with
        | Rplus  -> let b = T.Brataddsub in add_builtin b; norm (T.Icall (get_fun_name b, [T.Irecord [f l; f r]; T.ileft  T.tunit T.iunit]))
        | Rminus -> let b = T.Brataddsub in add_builtin b; norm (T.Icall (get_fun_name b, [T.Irecord [f l; f r]; T.iright T.tunit T.iunit]))
        | Rmult  -> let b = T.Bratmul    in add_builtin b; norm (T.Icall (get_fun_name b, [f l; f r]))
        | Rdiv   -> let b = T.Bratdiv    in add_builtin b; norm (T.Icall (get_fun_name b, [f l; f r]))
      end
    | Mratuminus v            -> let b = T.Bratuminus in add_builtin b; T.Icall (get_fun_name b, [f v])
    | Mrattez  (c, t)         -> let b = T.Brattez    in add_builtin b; T.Icall (get_fun_name b, [f c; f t])
    | Mnattoint e             -> T.Iunop (Uint, f e)
    | Mnattorat e             -> T.Irecord [T.Iunop (Uint, f e); T.inat Big_int.unit_big_int]
    | Minttorat e             -> T.Irecord [f e; T.inat Big_int.unit_big_int]
    | Mratdur  (c, t)         -> let b = T.Bratdur    in add_builtin b; T.Icall (get_fun_name b, [f c; f t])


    (* functional *)

    | Mfold _ -> emit_error (UnsupportedTerm ("Mfold"))


    (* imperative *)

    | Mbreak -> emit_error (UnsupportedTerm ("Mbreak"))


    (* quantifiers *)

    | Mforall _ -> emit_error (UnsupportedTerm ("Mforall"))
    | Mexists _ -> emit_error (UnsupportedTerm ("Mexists"))


    (* formula operators *)

    | Mimply _ -> emit_error (UnsupportedTerm ("Mimply"))
    | Mequiv _ -> emit_error (UnsupportedTerm ("Mequiv"))


    (* formula asset collection *)

    | Msetiterated  _ -> emit_error (UnsupportedTerm ("Msetiterated"))
    | Msettoiterate _ -> emit_error (UnsupportedTerm ("Msettoiterate"))


    (* formula asset collection methods *)

    | Mempty     _ -> emit_error (UnsupportedTerm ("Mempty"))
    | Msingleton _ -> emit_error (UnsupportedTerm ("Msingleton"))
    | Msubsetof  _ -> emit_error (UnsupportedTerm ("Msubsetof"))
    | Misempty   _ -> emit_error (UnsupportedTerm ("Misempty"))
    | Munion     _ -> emit_error (UnsupportedTerm ("Munion"))
    | Minter     _ -> emit_error (UnsupportedTerm ("Minter"))
    | Mdiff      _ -> emit_error (UnsupportedTerm ("Mdiff"))
  in

  let env = mk_env () in

  let funs, entries =

    let for_fs env (fs : M.function_struct) =
      let name = unloc fs.name in
      let args = List.map (fun (id, t, _) -> unloc id, to_type t) fs.args in
      let env = {env with function_p = Some (name, args)} in
      let body = mterm_to_intruction env fs.body in
      name, args, body
    in

    let for_fs_fun env (fs : M.function_struct) ret : T.func =
      let tret = to_type ret in
      let name, args, body = for_fs env fs in
      let targ = to_one_type (List.map snd args) in
      T.mk_func name targ tret (T.Concrete (args, body))
    in

    let for_fs_entry env (fs : M.function_struct) : T.entry =
      let name, args, body = for_fs env fs in
      T.mk_entry name args body
    in

    List.fold_right (fun (x : M.function__) (funs, entries) ->
        match x.node with
        | Entry fs -> (funs, (for_fs_entry env fs)::entries)
        | Function (fs, ret) -> ((for_fs_fun env fs ret)::funs, entries) ) model.functions ([], [])
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
  let with_operations = M.Utils.with_operations model in

  let funs = List.fold_left (fun accu x -> (get_builtin_fun x)::accu) funs !builtins in

  T.mk_ir storage_type storage_data l parameter funs entries ~with_operations:with_operations


(* -------------------------------------------------------------------- *)

let concrete_michelson b =
  let error _ = emit_error (NoConcreteImplementationFor (get_fun_name b)) in
  match b with
  | T.Bmin _          -> T.SEQ [DUP; UNPAIR; COMPARE; LT; IF ([CAR], [CDR])]
  | T.Bmax _          -> T.SEQ [DUP; UNPAIR; COMPARE; LT; IF ([CDR], [CAR])]
  | T.Bfloor          -> T.SEQ [UNPAIR; EDIV; IF_NONE ([T.cfail "DivByZero"], [CAR])]
  | T.Bceil           -> T.SEQ [UNPAIR; EDIV; IF_NONE ([T.cfail "DivByZero"], [UNPAIR; SWAP; INT; EQ; IF ([], [PUSH (T.tint, T.Dint Big_int.unit_big_int); ADD])])]
  | T.BlistContains _ -> T.SEQ [UNPAIR; PUSH (T.tbool, T.Dfalse); SWAP; ITER [DIG 2; DUP; DUG 3; COMPARE; EQ; OR; ]; DIP (1, [DROP 1])]
  | T.BlistNth _      -> error ()
  | T.Btostring _     -> error ()
  | T.Bratcmp         -> T.SEQ [UNPAIR; UNPAIR; DIP (1, [UNPAIR]); UNPAIR; DUG 3; MUL; DIP (1, [MUL]); SWAP; COMPARE; SWAP;
                                IF_LEFT ([DROP 1; EQ], [IF_LEFT ([IF_LEFT ([DROP 1; LT], [DROP 1; LE])],
                                                                 [IF_LEFT ([DROP 1; GT], [DROP 1; GE])])])]
  | T.Bratnorm        -> T.SEQ []
  | T.Brataddsub      -> T.SEQ [UNPAIR; UNPAIR; DIP (1, [UNPAIR; SWAP; DUP]); UNPAIR; SWAP; DUP; DIG 3; MUL; DUG 4; DIG 3; MUL; DIP (1, [MUL]); DIG 3; IF_LEFT ([DROP 1; ADD], [DROP 1; SWAP; SUB]); PAIR;]
  | T.Bratmul         -> T.SEQ [UNPAIR; DIP (1, [UNPAIR]); UNPAIR; DIP (1, [SWAP]); MUL; DIP (1, [MUL]); PAIR ]
  | T.Bratdiv         -> T.SEQ [UNPAIR; DIP (1, [UNPAIR]); UNPAIR; DIG 3; PUSH (T.tint, T.Dint Big_int.zero_big_int); DIG 4; DUP; DUG 5; COMPARE; GE; IF ([INT], [NEG]); MUL; DIP (1, [MUL; ABS]); PAIR ]
  | T.Bratuminus      -> T.SEQ [UNPAIR; NEG; PAIR]
  | T.Brattez         -> T.SEQ [UNPAIR; UNPAIR; ABS; DIG 2; MUL; EDIV; IF_NONE ([T.cfail "DivByZero"], []); CAR;]
  | T.Bratdur         -> T.SEQ [UNPAIR; UNPAIR; DIG 2; MUL; EDIV; IF_NONE ([T.cfail "DivByZero"], []); CAR;]

type env = {
  vars : ident list;
  fail : bool;
}
[@@deriving show {with_path = false}]

let mk_env ?(vars=[]) () = { vars = vars; fail = false }
let fail_env (env : env) = { env with fail = true }
let inc_env (env : env) = { env with vars = "_"::env.vars }
let dec_env (env : env) = { env with vars = match env.vars with | _::t -> t | _ -> emit_error StackEmptyDec }
let add_var_env (env : env) id = { env with vars = id::env.vars }
let get_sp_for_id (env : env) id =
  match List.index_of (String.equal id) env.vars with
  | -1 -> emit_error (StackIdNotFound (id, env.vars))
  | v -> v

let print_env ?(str="") env =
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
      | [e]  -> fe env e
      | e::t ->
        List.fold_left (fun (a, env) x -> begin
              let v, env = fe env x in (T.SEQ [a; v], env)
            end ) (fe env e) t
    in

    let fold env l =
      match List.rev l with
      | []   -> T.SEQ [], env
      | [e]  -> fe env e
      | e::t ->
        List.fold_left (fun (a, env) x -> begin
              let v, env = fe env x in (T.SEQ [a; v; T.PAIR], dec_env env)
            end ) (fe env e) t
    in

    let assign env id v =
      let n = get_sp_for_id env id in
      let c =
        if n <= 0
        then T.SEQ [ v; T.SWAP; T.DROP 1 ]
        else T.SEQ [ v; (T.DIP (1, [T.DIG n; T.DROP 1])); T.DUG n]
      in
      c, env
    in

    match i with
    | Iseq l               -> seq env l

    | IletIn (id, v, b)    -> begin
        let v, _ = f v in
        let env0 = add_var_env env id in
        let b, _ = fe env0 b in
        T.SEQ [v; b; T.DROP 1], env
      end

    | Ivar id -> begin
        let n = get_sp_for_id env id in
        let c =
          if n = 0
          then T.DUP
          else T.SEQ [ T.DIG n; T.DUP; T.DUG (n + 1)]
        in
        c, inc_env env
      end

    | Icall (id, args)   -> begin
        let fid, env   = fe env (Ivar id) in
        let cargs, env = fold env args in

        T.SEQ [fid; cargs; T.EXEC], dec_env env
      end

    | Iassign (id, v)  -> begin
        let v, _ = f v in
        assign env id v
      end

    | IassignRec (id, s, n, v) ->
      let v, _ = fe env (Irecupdate (Ivar id, s, [n, v])) in
      assign env id v

    | Iif (c, t, e) -> begin
        (* let c, _   = f c in
           let t, envt = f t in
           let e, enve = f e in *)

        let c, env = fe env c in
        let t, envt = fe (dec_env env) t in
        let e, enve = fe (dec_env env) e in

        let env =
          (* TODO: check if `envt` and `enve` have the same stack *)
          match envt.fail, enve.fail with
          | false, false -> envt
          | false, true  -> envt
          | true,  false -> enve
          | true,  true  -> envt
        in
        T.SEQ [ c; T.IF ([t], [e]) ], env
      end

    | Iifnone (v, t, e, id) -> begin
        let v, _   = f v in
        let t, env = f t in
        let env0   = add_var_env env id in
        let e, _   = fe env0 (e (T.Ivar id)) in

        T.SEQ [ v; T.IF_NONE ([t], [e; T.SWAP; T.DROP 1]) ], env
      end

    | Iifcons (l, t, e) -> begin
        let l, _     = f l in
        let t, _envt = f t in
        let e, _enve = f e in

        T.SEQ [ l; T.IF_CONS ([T.PAIR; t], [e]) ], (inc_env env)
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
          | Znow          -> T.NOW
          | Zamount       -> T.AMOUNT
          | Zbalance      -> T.BALANCE
          | Zsource       -> T.SOURCE
          | Zsender       -> T.SENDER
          | Zaddress      -> T.ADDRESS
          | Zchain_id     -> T.CHAIN_ID
          | Zself_address -> T.SEQ [T.SELF; T.ADDRESS]
          | Znone t       -> T.NONE t
        in
        c, inc_env env
      end
    | Iunop (op, e) -> begin
        let op =
          match op with
          | Ucar             -> T.CAR
          | Ucdr             -> T.CDR
          | Uleft t          -> T.LEFT t
          | Uright t         -> T.RIGHT t
          | Uneg             -> T.NEG
          | Uint             -> T.INT
          | Unot             -> T.NOT
          | Uabs             -> T.ABS
          | Uisnat           -> T.ISNAT
          | Usome            -> T.SOME
          | Usize            -> T.SIZE
          | Upack            -> T.PACK
          | Uunpack        t -> T.UNPACK t
          | Ublake2b         -> T.BLAKE2B
          | Usha256          -> T.SHA256
          | Usha512          -> T.SHA512
          | Uhash_key        -> T.HASH_KEY
          | Ufail            -> T.FAILWITH
          | Ucontract (t, a) -> T.CONTRACT (t, a)
        in
        let e, env = fe env e in
        let env = match op with T.FAILWITH -> fail_env env | _ -> env in
        T.SEQ [e; op], env
      end
    | Ibinop (op, lhs, rhs) -> begin
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
        let rhs, env = fe env rhs in
        let lhs, env = fe env lhs in
        T.SEQ [rhs; lhs; op], (dec_env env)
      end
    | Iterop (op, a1, a2, a3) -> begin
        let op =
          match op with
          | Tcheck_signature -> T.CHECK_SIGNATURE
          | Tslice           -> T.SLICE
          | Tupdate          -> T.UPDATE
          | Ttransfer_tokens -> T.TRANSFER_TOKENS
        in
        let a3, env = fe env a3 in
        let a2, env = fe env a2 in
        let a1, env = fe env a1 in
        T.SEQ [a3; a2; a1; op], (dec_env (dec_env env))
      end
    | Iconst (t, e) -> T.PUSH (t, e), inc_env env
    | Icompare (op, lhs, rhs) -> begin
        let op =
          match op with
          | Ceq -> T.EQ
          | Cne -> T.NEQ
          | Clt -> T.LT
          | Cle -> T.LE
          | Cgt -> T.GT
          | Cge -> T.GE
        in
        let r, env = fe env rhs in
        let l, env = fe env lhs in
        T.SEQ [r; l; T.COMPARE; op], dec_env env
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
    | Irecord l -> fold env l
    | Irecupdate (x, s, l) -> begin
        let x, env = fe env x in
        let rec g (l, env) x =
          match l with
          | [] -> []
          | (n, v)::q -> begin
              if x = n
              then let v, _ = fe env v in [T.DROP 1; v] @ g (q, env) x
              else begin
                if s = x + 2
                then [T.SWAP ] @ g (l, env) (x + 1) @ [T.SWAP]
                else [T.SWAP; T.UNPAIR ] @ g (l, inc_env env) (x + 1) @ [T.PAIR; T.SWAP]
              end
            end
        in
        let a = g (l, env) 0 in
        let b =
          if s = 1
          then a
          else [T.UNPAIR] @ a @ [T.PAIR]
        in
        T.SEQ ([ x ] @ b), env
      end

    | Imichelson (a, c, v) -> begin
        let a, _ = seq env a in
        T.SEQ [a; c], { env with vars = v @ env.vars }
      end
  in

  let build_code _ =

    let unfold = foldi (fun x -> T.UNPAIR::T.SWAP::x ) [] in

    let get_funs _ : T.code list * ident list =
      let funs = List.map (
          fun (x : T.func) ->
            let code =
              match x.body with
              | Concrete (args, body) ->
                let env = mk_env ~vars:(args |> List.map fst |> List.rev) () in
                let nb_args = List.length args in
                let nb_as = nb_args - 1 in
                let unfold_args = unfold nb_as in
                let res = T.PUSH (T.tunit, T.Dunit) in
                let env = add_var_env env fun_result in
                let code, _ = instruction_to_code env body in unfold_args @ [res] @ code::[T.DUG nb_args; T.DROP nb_args]
              | Abstract b    -> [concrete_michelson b]
            in
            T.LAMBDA (x.targ, x.tret, code ), x.name
        ) ir.funs
      in
      List.split funs
    in

    let funs, funids = get_funs () in
    let cfuns, df =
      let n = (List.length funs) in
      if n = 0 then [], 0
      else funs, List.length funs
    in

    let ops, ops_var, eops, opsf = if ir.with_operations then [T.NIL T.toperation], [operations], [T.DIG 1], 1 else [], [], [T.NIL T.toperation], 0 in

    let fff, eee = let n = df + opsf in (if n > 0 then [T.DIG n] else []), (if df > 0 then [T.DIP (1, [T.DROP df]) ] else []) in

    let vars = List.rev (let l = ir.storage_list in if List.is_empty l then ["_"] else List.map fst l) @ ops_var @ List.rev funids in
    let env = mk_env () ~vars in
    let nb_storage_item = List.length ir.storage_list in
    let nb_fs = nb_storage_item - 1 in
    let unfold_storage = unfold nb_fs  in
    let fold_storage = foldi (fun x -> T.SWAP::T.PAIR::x ) [] nb_fs in


    let for_entry (e : T.entry) =
      (* stack state : functions, (operations list)?, unfolded storage, argument *)
      match e.args with
      | [] -> begin
          let code, _ = instruction_to_code env e.body in
          T.SEQ ([T.DROP 1] @ [code] @ fold_storage @ eops @ [T.PAIR])
        end
      | l -> begin
          let nb_args = List.length l in
          let nb_as = nb_args - 1 in
          let unfold_args = unfold nb_as in
          let args = List.map fst l |> List.rev in
          let env = { env with vars = args @ env.vars } in
          let code, _ = instruction_to_code env e.body in
          T.SEQ (unfold_args @ [code] @ [T.DROP nb_args] @ fold_storage @ eops @ [T.PAIR])
        end
    in

    let code =
      match List.rev ir.entries with
      | []   -> assert false
      | [e]  -> T.SEQ [for_entry e]
      | e::l -> begin
          let c : T.code = List.fold_left (fun accu x -> T.IF_LEFT ([for_entry x], [accu])) (for_entry e) l in
          T.SEQ ([c])
        end
    in
    let us = if nb_storage_item > 1 then [T.DIP (1, unfold_storage)] else [] in
    T.SEQ (cfuns @ ops @ fff @ [T.UNPAIR] @ us @ [code] @ eee)
    |> T.Utils.flat
    |> T.Utils.optim
  in

  let code = build_code () in
  T.mk_michelson storage ir.parameter code
