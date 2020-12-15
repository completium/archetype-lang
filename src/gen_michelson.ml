open Ident
open Location
open Tools
open Printer_tools

module M = Model
module T = Michelson

module MapString = Map.Make(String)

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

let is_rat t = match M.get_ntype t with | M.Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)] -> true     | _ -> false

let get_fun_name = T.Utils.get_fun_name Printer_michelson.show_pretty_type

let operations = "_ops"
let fun_result = "_fun_res"

let mk_fannot x = "%" ^ x

let rar t =
  let rec aux (t : T.type_) : T.type_ =
    let t = T.map_type aux t in
    { t with annotation = None }
  in
  aux t

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

    let annotation =
      match annotation with
      | Some _ -> annotation
      | _ -> begin
          match snd t with
          | Some a when String.equal "%_" (unloc a) -> None
          | Some a -> Some (unloc a)
          | None -> None
        end
    in

    match M.get_ntype t with
    | Tasset _   -> assert false
    | Tenum _    -> T.mk_type ?annotation T.Tint
    | Tstate     -> T.mk_type ?annotation T.Tint
    | Tbuiltin b -> T.mk_type ?annotation begin
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
    | Tcontainer _   -> assert false
    | Tlist t        -> T.mk_type ?annotation (T.Tlist (to_type t))
    | Toption t      -> T.mk_type ?annotation (T.Toption (to_type t))
    | Ttuple lt      -> T.mk_type ?annotation (to_one_type (List.map to_type lt) |> fun x -> x.node)
    | Tset t         -> T.mk_type ?annotation (T.Tset (to_type t))
    | Tmap (b, k, v) -> T.mk_type ?annotation (if b then T.Tbig_map (to_type k, to_type v) else T.Tmap (to_type k, to_type v))
    | Tor (l, r)     -> T.mk_type ?annotation (T.Tor (to_type l, to_type r))
    | Trecord id     -> begin
        let r = M.Utils.get_record model (unloc id) in
        let lt = List.map (fun (x : M.record_field) ->
            match snd x.type_ with
            | Some _ -> x.type_
            | None -> fst x.type_, Some (dumloc (mk_fannot (unloc x.name)))) r.fields in
        match r.pos with
        | Pnode [] -> T.mk_type ?annotation (to_one_type (List.map to_type lt) |> fun x -> x.node)
        | p -> begin
            let ltt = ref lt in
            let rec aux p =
              match p with
              | M.Ptuple ids -> begin
                  let length = List.length ids in
                  let ll0, ll1 = List.cut length !ltt in
                  ltt := ll1;
                  let ll0 : M.type_ list = List.map2 (fun id (x : M.type_) ->
                      let annot =
                        match id with
                        | "_" -> None
                        | _ -> Some (dumloc ("%" ^ id))
                      in
                      M.mktype ?annot (M.get_ntype x)) ids ll0 in
                  to_one_type (List.map to_type ll0)
                end
              | M.Pnode l -> to_one_type (List.map aux l)
            in
            aux p
          end
      end
    | Tlambda (a, r) -> T.mk_type ?annotation (Tlambda (to_type a, to_type r))
    | Tunit          -> T.mk_type ?annotation (T.Tunit)
    | Toperation     -> T.mk_type ?annotation (T.Toperation)
    | Tcontract t    -> T.mk_type ?annotation (T.Tcontract (to_type t))
    | Tstorage       -> assert false
    | Tprog  _       -> assert false
    | Tvset  _       -> assert false
    | Ttrace _       -> assert false
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
    | Mlitmap    (_, l) -> T.Dlist (List.map (fun (x, y) -> T.Delt (to_data x, to_data y)) l)
    | Muminus    v      -> to_data v |> (function | T.Dint n -> T.Dint (Big_int.mult_int_big_int (-1) n) | _ -> assert false )
    | Mnow              -> T.Dint (Unix.time () |> int_of_float |> Big_int.big_int_of_int)
    | Mlitrecord l      -> begin
        let data = List.map (to_data |@ snd) l in
        match M.get_ntype mt.type_ with
        | Trecord rn -> begin
            let r = M.Utils.get_record model (unloc rn) in
            match r.pos with
            | Pnode [] -> to_one_data data
            | _ -> begin
                let ndata = ref data in

                let rec aux p =
                  match p with
                  | M.Ptuple ids  -> begin
                      let l = List.length ids in
                      let ll0, ll1 = List.cut l !ndata in
                      ndata := ll1;
                      to_one_data ll0
                    end
                  | M.Pnode  nodes -> begin
                      to_one_data (List.map aux nodes)
                    end
                in

                aux r.pos
              end
          end
        | _ -> to_one_data data
      end
    | Mleft (_, x)      -> T.Dleft (to_data x)
    | Mright (_, x)     -> T.Dright (to_data x)
    | Mcast (_, _, v)   -> to_data v
    | Mvar (x, Vparameter, _, _) -> T.Dvar (unloc x)
    | _ -> Format.printf "%a@." M.pp_mterm mt; assert false

  in

  let l = List.map (
      fun (si : M.storage_item) ->
        (unloc si.id), to_type ~annotation:(mk_fannot (unloc si.id)) si.typ, to_data si.default)
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

  let is_inline = function
    | T.Bmin           _ -> true
    | T.Bmax           _ -> true
    | T.Bfloor           -> true
    | T.Bceil            -> true
    | T.BlistContains  _ -> false
    | T.BlistNth       _ -> false
    | T.Btostring      _ -> false
    | T.Bratcmp          -> false
    | T.Bratnorm         -> true
    | T.Brataddsub       -> true
    | T.Bratdiv          -> true
    | T.Bratmul          -> true
    | T.Bratuminus       -> true
    | T.Bratabs          -> true
    | T.Brattez          -> true
    | T.Bratdur          -> true
  in

  let add_builtin b =
    if not (is_inline b) && not (List.exists (T.cmp_builtin b) !builtins)
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
          let return    = T.Iassign (fun_result, T.Iifnone (vres, T.ifail "NoneValue", "_var_ifnone", Ivar "_var_ifnone", t)) in
          let cond      = T.Icompare (Cle, viter, varg) in
          let vheadtail = T.Imichelson ([vlist], T.SEQ [ IF_CONS ([PAIR], [T.cstring "EmptyList"; T.FAILWITH])], []) in
          let ares      = T.Iassign (res_name, T.isome(T.icar ve)) in
          let alist     = T.Iassign (list_name, T.icdr ve) in
          let aiter     = T.Iassign (iter_name, T.Ibinop (Badd, viter, T.inat Big_int.unit_big_int)) in
          let bloop     = T.IletIn(e_name, vheadtail, T.Iseq [ares; alist; aiter], true) in
          let loop      = T.Iloop (cond, bloop) in
          let body      = T.IletIn(res_name, T.inone t, IletIn(iter_name, T.inat Big_int.zero_big_int, T.Iseq [loop; return], true), true) in
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
          let map    = T.Imap (false, T.tnat, T.tstring, [T.inat (Big_int.big_int_of_int 0), T.istring "0";
                                                          T.inat (Big_int.big_int_of_int 1), T.istring "1";
                                                          T.inat (Big_int.big_int_of_int 2), T.istring "2";
                                                          T.inat (Big_int.big_int_of_int 3), T.istring "3";
                                                          T.inat (Big_int.big_int_of_int 4), T.istring "4";
                                                          T.inat (Big_int.big_int_of_int 5), T.istring "5";
                                                          T.inat (Big_int.big_int_of_int 6), T.istring "6";
                                                          T.inat (Big_int.big_int_of_int 7), T.istring "7";
                                                          T.inat (Big_int.big_int_of_int 8), T.istring "8";
                                                          T.inat (Big_int.big_int_of_int 9), T.istring "9"]) in
          let get_map    = T.Iifnone (T.Ibinop (Bget, T.Iunop (Ucdr, vpair), vmap), T.ifail "GetNoneValue", "_var_ifnone", Ivar "_var_ifnone", T.tstring) in
          let concat     = T.Ibinop (Bconcat, get_map, vres) in
          let assign_res = T.Iassign (res_name, concat) in
          let assign_arg = T.Iassign (arg_name, T.Iunop (Ucar, vpair)) in
          let vpair      = T.Iifnone (T.Ibinop (Bediv, varg, ten), T.ifail "DivByZero", "_var_ifnone", Ivar "_var_ifnone", T.tpair T.tint T.tnat) in
          let b          = T.IletIn(pair_name, vpair, T.Iseq [assign_res; assign_arg], true) in
          let loop       = T.Iloop (cond, b) in
          let a          = T.IletIn(res_name, T.istring "", IletIn(map_name, map, T.Iseq [loop; return vres], true), true) in
          args, T.Iif (cond, a, return (T.istring "0"), T.tunit)
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
    | Bratuminus
    | Bratabs -> begin
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

  let extra_args : (ident * (ident * T.type_) list) list ref  = ref [] in

  let rec mterm_to_intruction env (mtt : M.mterm) : T.instruction =
    let f = mterm_to_intruction env in
    let ft = to_type in

    let vops = T.Ivar operations in
    let contract_internal a t d = T.Iifnone (T.Iunop (Ucontract (t, a), d), T.ifail "BadContract", "_var_ifnone", Ivar "_var_ifnone", T.tint) in
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

    let access_tuple s i x =
      if i = 0 && s = 1
      then x
      else begin
        let x = Tools.foldi T.icdr x i in
        let x =
          if i < s - 1
          then T.icar x
          else x
        in
        x
      end
    in

    let access_record (e : M.mterm) fn =
      let fn = unloc fn in
      match M.get_ntype e.type_ with
      | M.Trecord rn -> begin
          let rn = unloc rn in
          let pos = M.Utils.get_record_pos model rn fn in
          List.fold_left (fun accu (i, s) -> access_tuple s i accu) (f e) pos
        end
      | _ -> Format.eprintf "access_record: %a@." M.pp_type_ e.type_; assert false
    in

    let make_ru ?ru rn fn v : T.ruitem =
      let res =
        let l = Model.Utils.get_record_pos model rn fn in
        let res, l =
          match List.rev l with
          | (p, s)::t -> T.RUassign(s, [p, f v]), List.rev t
          | _ -> assert false
        in
        List.fold_right (fun (p, s) accu -> T.RUnodes(s, [p, accu])) l res
      in

      let rec merge r1 r2 : T.ruitem =
        let sort l = List.sort (fun (x, _) (y, _) -> x - y) l in
        let doit (s1, l1) (s2, l2) g f =
          if s1 <> s2
          then begin
            if !Options.opt_trace
            then Format.printf "%a@\n%a@." Printer_michelson.pp_ruitem r1 Printer_michelson.pp_ruitem r2;
            assert false
          end;
          let l = List.fold_left (fun accu (p, v2) ->
              let a = List.assoc_opt p accu in
              match a with
              | None    -> (p, v2)::accu
              | Some v1 -> f accu p v1 v2
            ) l1 l2 in
          let l = sort l in
          g s1 l
        in
        match r1, r2 with
        | T.RUassign (s1, l1), T.RUassign (s2, l2) -> doit (s1, l1) (s2, l2) (fun s x -> T.RUassign (s, x)) (fun accu p _  v2 -> List.put p v2 accu)
        | T.RUnodes  (s1, l1), T.RUnodes  (s2, l2) -> doit (s1, l1) (s2, l2) (fun s x -> T.RUnodes (s, x))  (fun accu p v1 v2 -> List.put p (merge v1 v2) accu)
        | _ -> assert false
      in

      match ru with
      | Some v -> merge v res
      | None -> res
    in

    match mtt.node with

    (* lambda *)

    | Mletin ([id], v, _, b, _) -> let is_unit = match M.get_ntype mtt.type_ with Tunit -> true | _ -> false in T.IletIn (unloc id, f v, f b, is_unit)
    | Mletin _                  -> emit_error (UnsupportedTerm ("Mletin"))
    | Mdeclvar _                -> emit_error (UnsupportedTerm ("Mdeclvar"))
    | Mapp (e, args)            -> begin
        let eargs =
          match List.assoc_opt (unloc e) !extra_args with
          | Some l -> List.map (fun (id, _t) -> T.Ivar id) l
          | _ -> []
        in
        T.Icall (unloc e, List.map f args @ eargs, false)
      end

    (* assign *)

    | Massign (_op, _, Avar id, v)                 -> T.Iassign (unloc id, f v)
    | Massign (_op, _, Avarstore id, v)            -> T.Iassign (unloc id, f v)
    | Massign (_op, _, Aasset (_an, _fn, _k), _v)  -> emit_error (UnsupportedTerm ("Massign: Aasset"))
    | Massign (_op, _, Arecord (_rn, fn, {node = Mvar (id, _, _, _); type_ = t}), v) -> begin
        let id = unloc id in
        let rn =
          match M.get_ntype t with
          | M.Trecord rn -> unloc rn
          | _ -> assert false
        in
        let ru = make_ru rn (unloc fn) v in
        let a = T.Irecupdate (T.Ivar id, ru) in
        T.Iassign (id, a)
      end
    | Massign (_op, _, Arecord _, _v)              -> T.iskip
    | Massign (_op, _, Astate, _x)                 -> emit_error (UnsupportedTerm ("Massign: Astate"))
    | Massign (_op, _, Aassetstate (_an, _k), _v)  -> emit_error (UnsupportedTerm ("Massign: Aassetstate"))
    | Massign (_op, _, Aoperations, v)             -> T.Iassign (operations, f v)

    (* control *)

    | Mif (c, t, Some e)         -> T.Iif (f c, f t, f e, T.tunit)
    | Mif (c, t, None)           -> T.Iif (f c, f t, T.iskip, T.tunit)
    | Mmatchwith (_e, _l)        -> emit_error (UnsupportedTerm ("Mmatchwith"))
    | Minstrmatchoption (x, i, ve, ne)       -> T.Iifnone (f x, f ne, unloc i, f ve, T.tunit)
    | Minstrmatchor (x, lid, le, rid, re)    -> T.Iifleft (f x, unloc lid, f le, unloc rid, f re, T.tunit)
    | Minstrmatchlist (x, hid, tid, hte, ee) -> T.Iifcons (f x, unloc hid, unloc tid, f hte, f ee, T.tunit)
    | Mfor (id, c, b, _)         -> begin
        let ids =
          match id with
          | M.FIsimple x      -> [x]
          | M.FIdouble (x, y) -> [x; y]
        in
        let ids = List.map unloc ids in
        let c =
          match c with
          | ICKcoll  _ -> emit_error (UnsupportedTerm ("ICKcoll"))
          | ICKview  _ -> emit_error (UnsupportedTerm ("ICKview"))
          | ICKfield _ -> emit_error (UnsupportedTerm ("ICKfield"))
          | ICKset   c
          | ICKlist  c
          | ICKmap   c -> f c
        in
        let b = f b in
        T.Iiter (ids, c, b)
      end
    | Miter (_i, _a, _b, _c, _)  -> emit_error TODO
    | Mwhile (c, b, _)           -> T.Iloop (f c, f b)
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
    | Mtransfer tr -> begin
        let op =
          match tr with
          | TKsimple (v, d)         -> T.Iterop (Ttransfer_tokens, T.iunit, f v, get_contract T.tunit (f d))
          | TKcall (v, id, t, d, a) -> T.Iterop (Ttransfer_tokens, f a, f v, get_entrypoint id (to_type t) (f d))
          | TKentry (v, e, a)       -> T.Iterop (Ttransfer_tokens, f a, f v, f e)
          | TKself (v, id, args)    -> begin
              let a =
                match args with
                | []  -> T.iunit
                | [e] -> f (snd e)
                | _   -> T.isrecord (List.map (fun (_, x) -> f x) args)
              in
              T.Iterop (Ttransfer_tokens, a, f v, get_self_entrypoint id)
            end
          | TKoperation op -> f op
        in
        T.Iassign (operations, T.Ibinop (Bcons, op, vops))
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
    | Maddress v         -> T.Iconst (T.mk_type Taddress, Dstring v)
    | Mdate v            -> T.Iconst (T.mk_type Ttimestamp, Dint (Core.date_to_timestamp v))
    | Mduration v        -> T.Iconst (T.mk_type Tint, Dint (Core.duration_to_timestamp v))
    | Mtimestamp v       -> T.Iconst (T.mk_type Ttimestamp, Dint v)
    | Mbytes v           -> T.Iconst (T.mk_type Tbytes, Dbytes v)
    | Munit              -> T.Iconst (T.mk_type Tunit, Dunit)

    (* control expression *)

    | Mexprif (c, t, e)                      -> T.Iif (f c, f t, f e, ft mtt.type_)
    | Mexprmatchwith (_e, _l)                -> emit_error (UnsupportedTerm ("Mexprmatchwith"))
    | Mmatchoption (x, i, ve, ne)            -> T.Iifnone (f x, f ne, unloc i, f ve, ft mtt.type_)
    | Mmatchor (x, lid, le, rid, re)         -> T.Iifleft (f x, unloc lid, f le, unloc rid, f re, ft mtt.type_)
    | Mmatchlist (x, hid, tid, hte, ee)      -> T.Iifcons (f x, unloc hid, unloc tid, f hte, f ee, ft mtt.type_)
    | Mloopleft (e, i, l)                    -> T.Iloopleft (f e, unloc i, f l)
    | Mmap (e, i, l)                         -> T.Imap_ (f e, unloc i, f l)
    | Mexeclambda (l, a)                     -> T.Ibinop (Bexec, f a, f l)
    | Mapplylambda (l, a)                    -> T.Ibinop (Bapply, f a, f l)

    (* composite type constructors *)

    | Mleft  (t, v) -> T.Iunop (Uleft (ft t), f v)
    | Mright (t, v) -> T.Iunop (Uright (ft t), f v)
    | Mnone    -> begin
        let t =
          match M.get_ntype mtt.type_ with
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
        match M.get_ntype mtt.type_ with
        | M.Tset t -> T.Iset (ft t, List.map f l)
        | _ -> assert false
      end
    | Mlitlist   l ->  begin
        match M.get_ntype mtt.type_ with
        | M.Tlist t -> T.Ilist (ft t, List.map f l)
        | _ -> assert false
      end
    | Mlitmap (_, l) -> begin
        match M.get_ntype mtt.type_ with
        | M.Tmap (b, k, v) -> T.Imap (b, ft k, ft v, List.map (fun (x, y) -> f x, f y) l)
        | _ -> assert false
      end
    | Mlitrecord l -> begin
        let ri =
          let ll = List.map (fun (_, x) -> f x) l in
          let mk_default _ = T.Rtuple ll in
          match M.get_ntype mtt.type_ with
          | M.Trecord rn -> begin
              let r = M.Utils.get_record model (unloc rn) in
              match r.pos with
              | Pnode [] -> mk_default ()
              | _ -> begin
                  let ndata = ref ll in

                  let rec aux p =
                    match p with
                    | M.Ptuple ids  -> begin
                        let l = List.length ids in
                        let ll0, ll1 = List.cut l !ndata in
                        ndata := ll1;
                        T.Rtuple ll0
                      end
                    | M.Pnode l -> T.Rnodes (List.map aux l)
                  in

                  aux r.pos
                end
            end
          | _ -> mk_default ()
        in
        T.Irecord ri
      end
    | Mlambda (rt, id, at, e) -> T.Ilambda (ft rt, unloc id, ft at, f e)

    (* access *)

    | Mdot (e, i)           -> access_record e i
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

    | Mand (l, r)        -> T.Ibinop (Band, f l, f r)
    | Mor (l, r)         -> T.Ibinop (Bor, f l, f r)
    | Mxor (l, r)        -> T.Ibinop (Bxor, f l, f r)
    | Mnot e             -> T.Iunop  (Unot, f e)
    | Mplus (l, r)       -> T.iadd (f l) (f r)
    | Mminus (l, r)      -> T.isub (f l) (f r)
    | Mmult (l, r)       -> T.imul (f l) (f r)
    | Mdivrat _          -> emit_error (UnsupportedTerm ("Mdivrat"))
    | Mdiveuc (l, r)     -> T.idiv (f l) (f r)
    | Mmodulo (l, r)     -> T.imod (f l) (f r)
    | Mdivmod (l, r)     -> T.Ibinop (Bediv, f l, f r)
    | Muminus e          -> T.Iunop  (Uneg, f e)
    | MthreeWayCmp (l, r)-> T.Ibinop (Bcompare, f l, f r)
    | Mshiftleft (l, r)  -> T.Ibinop (Blsl, f l, f r)
    | Mshiftright (l, r) -> T.Ibinop (Blsr, f l, f r)


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
        match M.get_ntype src, M.get_ntype dst, v.node with
        | M.Tbuiltin Baddress, M.Tcontract t, _                -> get_contract (to_type t) (f v)
        | M.Tbuiltin Bcurrency, M.Tbuiltin Bnat, _             -> T.idiv (f v) (T.imutez Big_int.unit_big_int)
        | M.Tbuiltin Bstring, M.Tbuiltin Bkey, Mstring s       -> T.Iconst (T.mk_type Tkey, Dstring s)
        | M.Tbuiltin Bstring, M.Tbuiltin Bsignature, Mstring s -> T.Iconst (T.mk_type Tsignature, Dstring s)
        | _ -> f v
      end
    | Mtupleaccess (x, n) -> let s = (match M.get_ntype x.type_ with | Ttuple l -> List.length l | _ -> 0) in access_tuple s (Big_int.int_of_big_int n) (f x)
    | Mrecupdate (x, l) ->
      let t = mtt.type_ in
      let rn =
        match M.get_ntype t with
        | M.Trecord rn -> unloc rn
        | _ -> assert false
      in
      let ru = List.fold_left (fun (ru : T.ruitem option) (fn, v) -> Some (make_ru ?ru rn fn v)) None l in
      let ru = match ru with | None -> assert false | Some v -> v in
      T.Irecupdate (f x, ru)

    (* set api expression *)

    | Msetadd (_, c, a)             -> T.Iterop (Tupdate, f a, T.itrue,  f c)
    | Msetremove (_, c, a)          -> T.Iterop (Tupdate, f a, T.ifalse, f c)
    | Msetcontains (_, c, k)        -> T.Ibinop (Bmem, f k, f c)
    | Msetlength (_, c)             -> T.Iunop  (Usize, f c)
    | Msetfold (_, ix, ia, c, a, b) -> T.Ifold (unloc ix, None, unloc ia, f c, f a, T.Iassign (unloc ia, f b))

    (* list api expression *)

    | Mlistprepend (_t, i, l)    -> T.Ibinop (Bcons, f l, f i)
    | Mlistheadtail (_t, l)      -> T.Imichelson ([f l], T.SEQ [ IF_CONS ([T.PAIR], [T.cstring "EmptyList"; T.FAILWITH])], [])
    | Mlistlength (_, l)         -> T.Iunop (Usize, f l)
    | Mlistcontains (t, c, a)    -> let b = T.BlistContains (to_type t) in add_builtin b; T.Icall (get_fun_name b, [f c; f a], is_inline b)
    | Mlistnth (t, c, a)         -> let b = T.BlistNth (to_type t) in add_builtin b; T.Icall (get_fun_name b, [f c; f a], is_inline b)
    | Mlistreverse _             -> emit_error (UnsupportedTerm ("Mlistreverse"))
    | Mlistconcat _              -> emit_error (UnsupportedTerm ("Mlistconcat"))
    | Mlistfold (_, ix, ia, c, a, b) -> T.Ifold (unloc ix, None, unloc ia, f c, f a, T.Iassign (unloc ia, f b))

    (* map api expression *)

    | Mmapput (_, _, c, k, v)     -> T.Iterop (Tupdate, f k, T.isome (f v),   f c)
    | Mmapremove (_, tv, c, k)    -> T.Iterop (Tupdate, f k, T.inone (ft tv), f c)
    | Mmapget (_, _, c, k)        -> T.Iifnone (T.Ibinop (Bget, f k, f c), T.ifail "GetNoneValue", "_var_ifnone", Ivar "_var_ifnone", ft mtt.type_)
    | Mmapgetopt (_, _, c, k)     -> T.Ibinop (Bget, f k, f c)
    | Mmapcontains (_, _, c, k)   -> T.Ibinop (Bmem, f k, f c)
    | Mmaplength (_, _, c)        -> T.Iunop (Usize, f c)
    | Mmapfold (_, ik, iv, ia, c, a, b) -> T.Ifold (unloc ik, Some (unloc iv), unloc ia, f c, f a, T.Iassign (unloc ia, f b))


    (* builtin functions *)

    | Mmax (l, r)        -> let b = T.Bmax (to_type l.type_) in add_builtin b; T.Icall (get_fun_name b, [f l; f r], is_inline b)
    | Mmin (l, r)        -> let b = T.Bmin (to_type l.type_) in add_builtin b; T.Icall (get_fun_name b, [f l; f r], is_inline b)
    | Mabs x when is_rat x.type_ -> let b = T.Bratabs        in add_builtin b; T.Icall (get_fun_name b, [f x], is_inline b)
    | Mabs x             -> T.Iunop (Uabs, f x)
    | Mconcat (x, y)     -> T.Ibinop (Bconcat, f x, f y)
    | Mslice (x, s, e)   -> T.Iifnone (T.Iterop (Tslice, f s, f e, f x), T.ifail "SliceError", "_x", Ivar "_x", ft mtt.type_)
    | Mlength x          -> T.Iunop (Usize, f x)
    | Misnone x          -> T.Iifnone (f x, T.itrue,  "_var_ifnone", T.ifalse, T.tbool)
    | Missome x          -> T.Iifnone (f x, T.ifalse, "_var_ifnone", T.itrue, T.tbool)
    | Moptget x          -> T.Iifnone (f x, T.ifail "NoneValue", "_var_ifnone", Ivar "_var_ifnone", ft mtt.type_)
    | Mfloor  x          -> let b = T.Bfloor           in add_builtin b; T.Icall (get_fun_name b, [f x], is_inline b)
    | Mceil   x          -> let b = T.Bceil            in add_builtin b; T.Icall (get_fun_name b, [f x], is_inline b)
    | Mtostring (t, x)   -> let b = T.Btostring (ft t) in add_builtin b; T.Icall (get_fun_name b, [f x], is_inline b)
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
    | Mmetadata      -> assert false


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
    | Mvar (_, Vparameter, _, _)       -> assert false

    (* rational *)

    | Mrateq (l, r)           -> let b = T.Bratcmp in add_builtin b; T.Icall (get_fun_name b, [T.isrecord [f l; f r]; T.ileft (T.tor (T.tor T.tunit T.tunit) (T.tor T.tunit T.tunit)) T.iunit], is_inline b)
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
      let b = T.Bratcmp in add_builtin b; T.Icall (get_fun_name b, [T.isrecord [f l; f r]; op], is_inline b)
    | Mratarith (op, l, r)    -> begin
        (* let norm x = let b = T.Bratnorm in add_builtin b; T.Icall (get_fun_name b, [x]) in *)
        let norm x = x in
        match op with
        | Rplus  -> let b = T.Brataddsub in add_builtin b; norm (T.Icall (get_fun_name b, [T.isrecord [f l; f r]; T.ileft  T.tunit T.iunit], is_inline b))
        | Rminus -> let b = T.Brataddsub in add_builtin b; norm (T.Icall (get_fun_name b, [T.isrecord [f l; f r]; T.iright T.tunit T.iunit], is_inline b))
        | Rmult  -> let b = T.Bratmul    in add_builtin b; norm (T.Icall (get_fun_name b, [f l; f r], is_inline b))
        | Rdiv   -> let b = T.Bratdiv    in add_builtin b; norm (T.Icall (get_fun_name b, [f l; f r], is_inline b))
      end
    | Mratuminus v            -> let b = T.Bratuminus in add_builtin b; T.Icall (get_fun_name b, [f v], is_inline b)
    | Mrattez  (c, t)         -> let b = T.Brattez    in add_builtin b; T.Icall (get_fun_name b, [f c; f t], is_inline b)
    | Mnattoint e             -> T.Iunop (Uint, f e)
    | Mnattorat e             -> T.isrecord [T.Iunop (Uint, f e); T.inat Big_int.unit_big_int]
    | Minttorat e             -> T.isrecord [f e; T.inat Big_int.unit_big_int]
    | Mratdur  (c, t)         -> let b = T.Bratdur    in add_builtin b; T.Icall (get_fun_name b, [f c; f t], is_inline b)


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
      let eargs = List.map (fun (id, t, _) -> unloc id, to_type t) fs.eargs in
      let env = {env with function_p = Some (name, args)} in
      let body = mterm_to_intruction env fs.body in
      name, args, eargs, body
    in

    let mapargs : 'a MapString.t ref = ref MapString.empty in

    let get_extra_args (mt : M.mterm) : (ident * T.type_) list =
      let rec aux accu (mt : M.mterm) : (ident * T.type_) list =
        let doit accu mt b : (ident * T.type_) list  =
          if is_inline b
          then (M.fold_term aux accu mt)
          else
            let fu = get_builtin_fun b in
            (fu.name, T.tlambda fu.targ fu.tret)::(M.fold_term aux accu mt)
        in

        match mt.node with
        | Mmax _                  -> (doit accu mt (T.Bmax (to_type mt.type_)))
        | Mmin _                  -> (doit accu mt (T.Bmin (to_type mt.type_)))
        | Mfloor _                -> (doit accu mt (T.Bfloor                ))
        | Mceil  _                -> (doit accu mt (T.Bceil                 ))
        | Mlistcontains (t, _, _) -> (doit accu mt (T.BlistContains (to_type t)))
        | Mlistnth (t, _, _)      -> (doit accu mt (T.BlistNth (to_type t)))
        | Mtostring (t, _)        -> (doit accu mt (T.Btostring (to_type t) ))

        | Mrateq _
        | Mratcmp _                          -> (doit accu mt (T.Bratcmp   ))
        | Mratarith ((Rplus | Rminus), _, _) -> (doit accu mt (T.Brataddsub))
        | Mratarith (Rmult, _, _)            -> (doit accu mt (T.Bratmul   ))
        | Mratarith (Rdiv, _, _)             -> (doit accu mt (T.Bratdiv   ))
        | Mratuminus _                       -> (doit accu mt (T.Bratcmp   ))
        | Mabs _        when is_rat mt.type_ -> (doit accu mt (T.Bratabs   ))
        | Mrattez _                          -> (doit accu mt (T.Brattez   ))
        | Mratdur _                          -> (doit accu mt (T.Bratdur   ))

        | Mapp (fid, _)                   ->
          let fid  = unloc fid in
          let targs, tret = MapString.find fid !mapargs in
          let eargs = match List.assoc_opt fid !extra_args with None -> [] | Some l -> l in
          (fid, T.tlambda targs tret)::(eargs @ M.fold_term aux accu mt) |> List.dedup

        | _ -> M.fold_term aux accu mt
      in
      aux [] mt |> List.dedup
    in

    let for_fs_fun env (fs : M.function_struct) ret : T.func =
      let fid = unloc fs.name in
      let tret = to_type ret in
      let name, args, _eargs, body = for_fs env fs in
      let eargs = get_extra_args fs.body in
      extra_args := (fid, eargs)::!extra_args;
      let args = args @ eargs in
      let targ = to_one_type (List.map snd args) in
      mapargs := MapString.add fid (targ, tret) !mapargs;
      T.mk_func name targ tret (T.Concrete (args, body))
    in

    let for_fs_entry env (fs : M.function_struct) : T.entry =
      let name, args, eargs, body = for_fs env fs in
      T.mk_entry name args eargs body
    in

    List.fold_left (fun (funs, entries) (x : M.function__) ->
        match x.node with
        | Entry fs -> (funs, entries @ [for_fs_entry env fs])
        | Getter _ -> emit_error (UnsupportedTerm ("Getter"))
        | Function (fs, ret) -> funs @ [for_fs_fun env fs ret], entries) ([], []) model.functions
  in
  let annot a (t : T.type_) = { t with annotation = Some (mk_fannot a)} in
  let l = List.map (fun (x, y, _) -> (x, y)) l in
  let parameter : T.type_ =
    let for_entry (e : T.entry) =
      let f l =
        match List.rev l with
        | []   -> T.tunit
        | [e]  -> annot (fst e) (snd e)
        | (id, te)::t -> List.fold_left (fun accu (id, te) -> T.mk_type (T.Tpair (annot id te, accu))) (annot id te) t
      in
      let  args : T.type_ = f e.args in
      let eargs : T.type_ = f e.eargs |> remove_annot in

      match args.node, eargs.node with
      | T.Tunit, T.Tunit -> T.tunit
      |       _, T.Tunit -> args
      | T.Tunit, _       -> T.tpair T.tunit eargs
      | _                -> T.tpair args eargs
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

  let name = unloc model.name in
  let parameters = List.map (fun (x : M.parameter) -> unloc x.name) model.parameters in
  T.mk_ir name storage_type storage_data l parameter funs entries ~with_operations:with_operations ~parameters


(* -------------------------------------------------------------------- *)

let map_implem = [
  get_fun_name (T.Bmin T.tunit)  , T.[DUP; UNPAIR; COMPARE; LT; IF ([CAR], [CDR])];
  get_fun_name (T.Bmax T.tunit)  , T.[DUP; UNPAIR; COMPARE; LT; IF ([CDR], [CAR])];
  get_fun_name T.Bratcmp         , T.[UNPAIR; UNPAIR; DIP (1, [UNPAIR]); UNPAIR; DUG 3; MUL; DIP (1, [MUL]); SWAP; COMPARE; SWAP;
                                      IF_LEFT ([DROP 1; EQ], [IF_LEFT ([IF_LEFT ([DROP 1; LT], [DROP 1; LE])],
                                                                       [IF_LEFT ([DROP 1; GT], [DROP 1; GE])])])];
  get_fun_name T.Bfloor          , T.[UNPAIR; EDIV; IF_NONE ([T.cfail "DivByZero"], [CAR])];
  get_fun_name T.Bceil           , T.[UNPAIR; EDIV; IF_NONE ([T.cfail "DivByZero"], [UNPAIR; SWAP; INT; EQ; IF ([], [PUSH (T.tint, T.Dint Big_int.unit_big_int); ADD])])];
  get_fun_name T.Bratnorm        ,   [];
  get_fun_name T.Brataddsub      , T.[UNPAIR; UNPAIR; DIP (1, [UNPAIR; SWAP; DUP]); UNPAIR; SWAP; DUP; DIG 3; MUL; DUG 4; DIG 3; MUL; DIP (1, [MUL]); DIG 3; IF_LEFT ([DROP 1; ADD], [DROP 1; SWAP; SUB]); PAIR;];
  get_fun_name T.Bratmul         , T.[UNPAIR; DIP (1, [UNPAIR]); UNPAIR; DIP (1, [SWAP]); MUL; DIP (1, [MUL]); PAIR ];
  get_fun_name T.Bratdiv         , T.[UNPAIR; DIP (1, [UNPAIR]); UNPAIR; DIG 3; PUSH (T.tint, T.Dint Big_int.zero_big_int); DIG 4; DUP; DUG 5; COMPARE; GE; IF ([INT], [NEG]); MUL; DIP (1, [MUL; ABS]); PAIR ];
  get_fun_name T.Bratuminus      , T.[UNPAIR; NEG; PAIR];
  get_fun_name T.Bratabs         , T.[UNPAIR; ABS; INT; PAIR];
  get_fun_name T.Brattez         , T.[UNPAIR; UNPAIR; ABS; DIG 2; MUL; EDIV; IF_NONE ([T.cfail "DivByZero"], []); CAR;];
  get_fun_name T.Bratdur         , T.[UNPAIR; UNPAIR; DIG 2; MUL; EDIV; IF_NONE ([T.cfail "DivByZero"], []); CAR;];
]

let concrete_michelson b =
  let error _ = emit_error (NoConcreteImplementationFor (get_fun_name b)) in
  let get_implem b = List.assoc (get_fun_name b) map_implem in
  match b with
  | T.Bmin _          -> T.SEQ (get_implem (Bmin T.tunit))
  | T.Bmax _          -> T.SEQ (get_implem (Bmax T.tunit))
  | T.Bfloor          -> T.SEQ (get_implem b)
  | T.Bceil           -> T.SEQ (get_implem b)
  | T.BlistContains _ -> T.SEQ [UNPAIR; PUSH (T.tbool, T.Dfalse); SWAP; ITER [DIG 2; DUP; DUG 3; COMPARE; EQ; OR; ]; DIP (1, [DROP 1])]
  | T.BlistNth _      -> error ()
  | T.Btostring _     -> error ()
  | T.Bratcmp         -> T.SEQ [UNPAIR; UNPAIR; DIP (1, [UNPAIR]); UNPAIR; DUG 3; MUL; DIP (1, [MUL]); SWAP; COMPARE; SWAP;
                                IF_LEFT ([DROP 1; EQ], [IF_LEFT ([IF_LEFT ([DROP 1; LT], [DROP 1; LE])],
                                                                 [IF_LEFT ([DROP 1; GT], [DROP 1; GE])])])]
  | T.Bratnorm        -> T.SEQ (get_implem b)
  | T.Brataddsub      -> T.SEQ (get_implem b)
  | T.Bratmul         -> T.SEQ (get_implem b)
  | T.Bratdiv         -> T.SEQ (get_implem b)
  | T.Bratuminus      -> T.SEQ (get_implem b)
  | T.Bratabs         -> T.SEQ (get_implem b)
  | T.Brattez         -> T.SEQ (get_implem b)
  | T.Bratdur         -> T.SEQ (get_implem b)

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

    let fold_gen g env l =
      match List.rev l with
      | []   -> T.SEQ [], env
      | [e]  -> g env e
      | e::t ->
        List.fold_left (fun (a, env) x -> begin
              let v, env = g env x in (T.SEQ [a; v; T.PAIR], dec_env env)
            end ) (g env e) t
    in

    let fold env l = fold_gen fe env l in

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

    | IletIn (id, v, b, u)    -> begin
        let v, _ = f v in
        let env0 = add_var_env env id in
        let b, _ = fe env0 b in
        if u
        then T.SEQ [v; b; T.DROP 1], env
        else T.SEQ [v; b; T.DIP (1, [T.DROP 1])], inc_env env
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

    | Icall (id, args, inline)   -> begin
        let get_args env =
          match args with
          | [] -> T.UNIT, inc_env env
          | _ -> fold env args
        in
        match inline, List.assoc_opt id map_implem with
        | true, Some body_fun ->
          let cargs, env = get_args env in
          T.SEQ (cargs::body_fun), env
        | _ -> begin
            let fid, env   = fe env (Ivar id) in
            let cargs, env = get_args env in
            T.SEQ [fid; cargs; T.EXEC], dec_env env
          end
      end

    | Iassign (id, v)  -> begin
        let v, _ = f v in
        assign env id v
      end

    | Iif (c, t, e, ty) -> begin
        let c, env0 = fe env c in
        let t, envt = fe (dec_env env0) t in
        let e, enve = fe (dec_env env0) e in

        let env =
          match envt.fail, enve.fail with
          | true, true  -> {env with fail = true}
          | _           -> env
        in
        let env =
          match ty.node with
          | T.Tunit -> env
          | _ -> inc_env env
        in
        T.SEQ [ c; T.IF ([t], [e]) ], env
      end

    | Iifnone (v, t, id, s, ty) -> begin
        let v, _ = fe env v in
        let t, _ = fe env t in
        let e, _ = fe (add_var_env env id) s in

        let ee, env =
          match ty.node with
          | T.Tunit -> [T.DROP 1], env
          | _       -> [T.SWAP; T.DROP 1], inc_env env
        in

        T.SEQ [ v; T.IF_NONE ([t], [e] @ ee) ], env
      end

    | Iifleft (v, lid, le, rid, re, ty) -> begin
        let v, _ = fe env v in
        let l, _ = fe (add_var_env env lid) le in
        let r, _ = fe (add_var_env env rid) re in

        let ee, env =
          match ty.node with
          | T.Tunit -> [T.DROP 1], env
          | _       -> [T.SWAP; T.DROP 1], inc_env env
        in

        T.SEQ [ v; T.IF_LEFT ([l] @ ee, [r] @ ee) ], env
      end

    | Iifcons (x, hd, tl, hte, ne, ty) -> begin
        let x, _ = fe env x in
        let t, _ = fe (add_var_env (add_var_env env tl) hd) hte in
        let n, _ = fe env ne in

        let ee, env =
          match ty.node with
          | T.Tunit -> [T.DROP 2], env
          | _       -> [T.DUG 2; T.DROP 2], inc_env env
        in

        T.SEQ [ x; T.IF_CONS ([t] @ ee, [n]) ], env
      end

    | Iloop (c, b) -> begin
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

    | Iloopleft (l, i, b) -> begin
        let l, _ = f l in
        let b, env = fe (add_var_env env i) b in

        T.SEQ [l; T.LOOP_LEFT [b; SWAP; DROP 1]], env
      end

    | Ilambda (rt, id, at, e) -> begin
        let e, _env = fe (add_var_env env id) e in

        T.LAMBDA (rt, at, [e; SWAP; DROP 1]), env
      end

    | Izop op -> begin
        let c =
          match op with
          | Znow                -> T.NOW
          | Zamount             -> T.AMOUNT
          | Zbalance            -> T.BALANCE
          | Zsource             -> T.SOURCE
          | Zsender             -> T.SENDER
          | Zaddress            -> T.ADDRESS
          | Zchain_id           -> T.CHAIN_ID
          | Zself a             -> T.SELF a
          | Zself_address       -> T.SEQ [T.SELF None; T.ADDRESS]
          | Znone t             -> T.NONE (rar t)
          | Zunit               -> T.UNIT
          | Znil t              -> T.NIL (rar t)
          | Zemptyset t         -> T.EMPTY_SET (rar t)
          | Zemptymap (k, v)    -> T.EMPTY_MAP (rar k, rar v)
          | Zemptybigmap (k, v) -> T.EMPTY_BIG_MAP (rar k, rar v)
        in
        c, inc_env env
      end
    | Iunop (op, e) -> begin
        let op =
          match op with
          | Ucar             -> T.CAR
          | Ucdr             -> T.CDR
          | Uleft t          -> T.LEFT (rar t)
          | Uright t         -> T.RIGHT (rar t)
          | Uneg             -> T.NEG
          | Uint             -> T.INT
          | Unot             -> T.NOT
          | Uabs             -> T.ABS
          | Uisnat           -> T.ISNAT
          | Usome            -> T.SOME
          | Usize            -> T.SIZE
          | Upack            -> T.PACK
          | Uunpack        t -> T.UNPACK (rar t)
          | Ublake2b         -> T.BLAKE2B
          | Usha256          -> T.SHA256
          | Usha512          -> T.SHA512
          | Uhash_key        -> T.HASH_KEY
          | Ufail            -> T.FAILWITH
          | Ucontract (t, a) -> T.CONTRACT (rar t, a)
          | Usetdelegate     -> T.SET_DELEGATE
          | Uimplicitaccount -> T.IMPLICIT_ACCOUNT
          | Ueq              -> T.EQ
          | Une              -> T.NEQ
          | Ugt              -> T.GT
          | Uge              -> T.GE
          | Ult              -> T.LT
          | Ule              -> T.LE
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
          | Bexec      -> T.EXEC
          | Bapply     -> T.APPLY
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
    | Iconst (t, e) -> T.PUSH (rar t, e), inc_env env
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
        T.SEQ ((T.EMPTY_SET t)::(l |> List.rev |> List.map (fun x -> let x, _ = fe (inc_env (inc_env env)) x in T.SEQ [T.ctrue; x; T.UPDATE ] ))), inc_env env
      end
    | Ilist (t, l) -> begin
        T.SEQ ((T.NIL t)::(l |> List.rev |> List.map (fun x -> let x, _ = fe (inc_env env) x in T.SEQ [ x; T.CONS ] ))), inc_env env
      end
    | Imap (b, k, v, l) -> begin
        let a = if b then T.EMPTY_BIG_MAP (k, v) else T.EMPTY_MAP (k, v) in
        T.SEQ ([a] @
               (l
                |> List.rev
                |> List.map (fun (x, y) ->
                    let y, _ = fe (inc_env env) y in
                    let x, _ = fe (inc_env (inc_env env)) x in
                    T.SEQ [y; T.SOME; x; T.UPDATE ] ))), inc_env env
      end
    | Irecord ri -> begin
        let rec aux env = function
          | T.Rtuple l -> fold env l
          | T.Rnodes l -> fold_gen aux env l
        in
        aux env ri
      end
    | Irecupdate (x, ru) -> begin
        let x, env = fe env x in
        let assign env s l h =
          let rec g (l, env) x =
            match l with
            | [] -> []
            | (n, v)::q -> begin
                if x = n
                then h env v s @ g (q, env) x
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
          T.SEQ b, env
        in
        let rec aux env ru =
          match ru with
          | T.RUassign (s, l) -> assign env s l (fun env v s ->
              let env = if s = 1 then dec_env env else env in
              let v, _ = fe env v in
              [T.DROP 1; v]
            )
          | T.RUnodes  (s, l) -> assign env s l (fun env v _ ->
              let v, _ = aux (inc_env env) v in
              [v]
            )
        in
        let v, _ = aux env ru in
        T.SEQ ([x; v]), env
      end

    | Ifold (ix, iy, ia, c, a, b) -> begin
        let a, _env0 = fe env a in
        let c, _env1 = fe (add_var_env env ia) c in
        let env2, pi, n =
          let env_= add_var_env env ia in
          match iy with
          | Some iy -> add_var_env (add_var_env env_ iy) ix, T.UNPAIR, 2
          | None -> add_var_env env_ ix, T.cskip, 1
        in
        let b, _env2 = fe env2 b in
        T.SEQ [a; c; T.ITER [pi; b; T.DROP n]], inc_env env
      end

    | Imap_ (x, id, e) -> begin
        let x, _env0 = fe env x in
        let e, _env1 = fe (add_var_env env id) e in
        T.SEQ [x; T.MAP [e; T.SWAP; T.DROP 1]], inc_env env
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
                let es = if nb_args = 0 then [T.SWAP; T.DROP 1] else [T.DUG nb_args; T.DROP nb_args] in
                let code, _ = instruction_to_code env body in unfold_args @ [res] @ code::es
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
      (* stack state : functions, (operations list)?, unfolded storage, extra_argument? , argument *)
      match e.args, e.eargs with
      | [], [] -> begin
          let code, _ = instruction_to_code env e.body in
          T.SEQ ([T.DROP 1] @ [code] @ fold_storage @ eops @ [T.PAIR])
        end
      | l, m -> begin
          let nb_eargs = List.length m in
          let eargs = List.map fst m |> List.rev in
          let unfold_eargs =
            if List.length m > 0
            then if List.length l = 0
              then [T.UNPAIR; T.DROP 1]
              else [T.UNPAIR]
            else []
          in

          let nb_args = List.length l in
          let nb_as = nb_args - 1 in
          let unfold_args = unfold nb_as in
          let args = List.map fst l |> List.rev in
          let env = { env with vars = args @ eargs @ env.vars } in
          let code, _ = instruction_to_code env e.body in
          T.SEQ (unfold_eargs @ unfold_args @ [code] @ [T.DROP (nb_args + nb_eargs)] @ fold_storage @ eops @ [T.PAIR])
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
  let parameters = ir.parameters in
  T.mk_michelson ~parameters storage ir.parameter code
