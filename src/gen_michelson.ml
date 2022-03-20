open Ident
open Location
open Tools
open Printer_tools

module M = Model
module T = Michelson

module MapString = Map.Make(String)

exception Anomaly of string

let complete_tree_entrypoints = true
let with_macro = false
let divbyzero = "DivByZero"
let entrynotfound = "EntryNotFound"
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

let shape_entrypoints f n l =
  match List.rev l with
  | [] -> n
  | [e] -> e
  | i::t -> begin
      if complete_tree_entrypoints
      then begin
        let l = List.rev (i::t) in
        make_full_tree f l
      end
      else List.fold_left f (i) t
    end

let to_one_gen init f l =
  match List.rev l with
  | [] -> init
  | i::q -> List.fold_left (fun accu x -> f x accu) i q

let to_one_type (l : T.type_ list) : T.type_ = to_one_gen T.tunit (fun x accu -> (T.mk_type (T.Tpair (x, accu)))) l

let to_one_type_or (l : T.type_ list) : T.type_ = to_one_gen T.tunit (fun x accu -> (T.mk_type (T.Tor (x, accu)))) l

let to_one_data (l : T.data list) : T.data = to_one_gen (T.Dunit) (fun x accu -> (T.Dpair (x, accu))) l

let to_one_gen init f l =
  match List.rev l with
  | [] -> init
  | i::q -> List.fold_left (fun accu x -> f x accu) i q

let rec to_type (model : M.model) ?annotation (t : M.type_) : T.type_ =
  let to_type = to_type model in
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

  let process_record (f : M.model -> ident -> M.record) (id : M.lident)  =
    let r = f model (unloc id) in
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
        let res = aux p in
        { res with annotation = Option.map unloc (M.get_atype t) }
      end
  in

  let process_enum ?annotation (id : lident) =
    let e_opt : M.enum option = M.Utils.get_enum_opt model (unloc id) in
    match e_opt with
    | Some e when  List.for_all (fun (x : M.enum_item) -> List.is_empty x.args) e.values -> begin
        let lt = List.map (fun (x : M.enum_item) : T.type_ ->
            T.mk_type ~annotation:(mk_fannot (unloc x.name)) (to_one_type (List.map to_type x.args) |> fun x -> x.node)
          ) e.values
        in
        T.mk_type ?annotation (to_one_type_or lt |> fun x -> x.node)
      end
    | _ -> T.mk_type ?annotation T.Tint
  in

  match M.get_ntype t with
  | Tasset _   -> assert false
  | Tenum id   -> process_enum ?annotation id
  | Tstate     -> T.mk_type ?annotation T.Tint
  | Tbuiltin b -> T.mk_type ?annotation begin
      match b with
      | Bunit         -> T.Tunit
      | Bbool         -> T.Tbool
      | Bint          -> T.Tint
      | Brational     -> T.Tpair (T.mk_type Tint, T.mk_type Tnat)
      | Bdate         -> T.Ttimestamp
      | Bduration     -> T.Tint
      | Btimestamp    -> T.Ttimestamp
      | Bstring       -> T.Tstring
      | Baddress      -> T.Taddress
      | Bcurrency     -> T.Tmutez
      | Bsignature    -> T.Tsignature
      | Bkey          -> T.Tkey
      | Bkeyhash      -> T.Tkey_hash
      | Bbytes        -> T.Tbytes
      | Bnat          -> T.Tnat
      | Bchainid      -> T.Tchain_id
      | Bbls12_381_fr -> T.Tbls12_381_fr
      | Bbls12_381_g1 -> T.Tbls12_381_g1
      | Bbls12_381_g2 -> T.Tbls12_381_g2
      | Bnever        -> T.Tnever
      | Bchest        -> T.Tchest
      | Bchest_key    -> T.Tchest_key
    end
  | Tcontainer _   -> assert false
  | Tlist t        -> T.mk_type ?annotation (T.Tlist (to_type t))
  | Toption t      -> T.mk_type ?annotation (T.Toption (to_type t))
  | Ttuple lt      -> T.mk_type ?annotation (to_one_type (List.map to_type lt) |> fun x -> x.node)
  | Tset t         -> T.mk_type ?annotation (T.Tset (to_type t))
  | Tmap (b, k, v) -> T.mk_type ?annotation (if b then T.Tbig_map (to_type k, to_type v) else T.Tmap (to_type k, to_type v))
  | Tor (l, r)     -> T.mk_type ?annotation (T.Tor (to_type l, to_type r))
  | Trecord id     -> process_record M.Utils.get_record id
  | Tevent id      -> process_record M.Utils.get_event id
  | Tlambda (a, r) -> T.mk_type ?annotation (Tlambda (to_type a, to_type r))
  | Tunit          -> T.mk_type ?annotation (T.Tunit)
  | Toperation     -> T.mk_type ?annotation (T.Toperation)
  | Tcontract t    -> T.mk_type ?annotation (T.Tcontract (to_type t))
  | Tstorage       -> assert false
  | Tprog  _       -> assert false
  | Tvset  _       -> assert false
  | Ttrace _       -> assert false
  | Tticket t      -> T.mk_type ?annotation (T.Tticket (to_type t))
  | Tsapling_state n       -> T.mk_type ?annotation (T.Tsapling_state n)
  | Tsapling_transaction n -> T.mk_type ?annotation (T.Tsapling_transaction n)


let to_ir (model : M.model) : T.ir =

  let remove_annot (t : T.type_) = {t with annotation = None} in

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
    | T.Bsubnat          -> true
    | T.Bmuteztonat      -> true
  in

  let add_builtin b =
    if not (is_inline b) && not (List.exists (T.cmp_builtin b) !builtins)
    then builtins := b::!builtins;
  in

  let get_builtin_fun b : T.func =
    let return x = T.Iassign (fun_result, x) in
    let name = T.Utils.get_fun_name Printer_michelson.show_pretty_type b in
    let ctx = T.mk_ctx_func () in
    match b with
    | Bmin t
    | Bmax t -> begin
        let targ = T.tpair t t in
        let tret = t in
        T.mk_func name targ tret ctx (T.Abstract b)
      end
    | Bfloor
    | Bceil -> begin
        let targ = T.trat in
        let tret  = T.tint in
        T.mk_func name targ tret ctx (T.Abstract b)
      end
    | BlistContains t -> begin
        let targ = T.tpair (T.tlist t) t in
        let tret = T.tbool in
        T.mk_func name targ tret ctx (T.Abstract b)
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
          let return    = T.Iassign (fun_result, T.Iifnone (vres, T.ifail "NotFound", "_var_ifnone", Ivar "_var_ifnone", t)) in
          let cond      = T.Icompare (Cle, viter, varg) in
          let vheadtail = T.Imichelson ([vlist], T.cseq [ T.mk_code (IF_CONS ([T.mk_code PAIR], [T.cstring "EmptyList"; T.cfailwith]))], []) in
          let ares      = T.Iassign (res_name, T.isome(T.icar ve)) in
          let alist     = T.Iassign (list_name, T.icdr ve) in
          let aiter     = T.Iassign (iter_name, T.Ibinop (Badd, viter, T.inat Big_int.unit_big_int)) in
          let bloop     = T.IletIn(e_name, vheadtail, T.Iseq [ares; alist; aiter], true) in
          let loop      = T.Iloop (cond, bloop) in
          let body      = T.IletIn(res_name, T.inone t, IletIn(iter_name, T.inat Big_int.zero_big_int, T.Iseq [loop; return], true), true) in
          args, body
        end
        in
        T.mk_func name targ tret ctx (T.Concrete (args, body))
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
          let get_map    = T.Iifnone (T.Ibinop (Bget, T.Iunop (Ucdr, vpair), vmap), T.ifail "NotFound", "_var_ifnone", Ivar "_var_ifnone", T.tstring) in
          let concat     = T.Ibinop (Bconcat, get_map, vres) in
          let assign_res = T.Iassign (res_name, concat) in
          let assign_arg = T.Iassign (arg_name, T.Iunop (Ucar, vpair)) in
          let vpair      = T.Iifnone (T.Ibinop (Bediv, varg, ten), T.ifail divbyzero, "_var_ifnone", Ivar "_var_ifnone", T.tpair T.tint T.tnat) in
          let b          = T.IletIn(pair_name, vpair, T.Iseq [assign_res; assign_arg], true) in
          let loop       = T.Iloop (cond, b) in
          let a          = T.IletIn(res_name, T.istring "", IletIn(map_name, map, T.Iseq [loop; return vres], true), true) in
          args, T.Iif (cond, a, return (T.istring "0"), T.tunit)
        end
        in
        T.mk_func name targ tret ctx (T.Concrete (args, body))
      end
    | Bratcmp -> begin
        let targ = T.tpair (T.tpair T.trat T.trat) (T.tor T.tunit (T.tor (T.tor T.tunit T.tunit) (T.tor T.tunit T.tunit))) in
        let tret = T.tbool in
        T.mk_func name targ tret ctx (T.Abstract b)
      end
    | Bratnorm -> begin
        let targ = T.trat in
        let tret = T.trat in
        T.mk_func name targ tret ctx (T.Abstract b)
      end
    | Brataddsub -> begin
        let targ = T.tpair (T.tpair T.trat T.trat) (T.tor T.tunit T.tunit) in
        let tret = T.trat in
        T.mk_func name targ tret ctx (T.Abstract b)
      end
    | Bratmul
    | Bratdiv -> begin
        let targ = T.tpair T.trat T.trat in
        let tret = T.trat in
        T.mk_func name targ tret ctx (T.Abstract b)
      end
    | Bratuminus
    | Bratabs -> begin
        let targ = T.trat in
        let tret = T.trat in
        T.mk_func name targ tret ctx (T.Abstract b)
      end
    | Brattez -> begin
        let targ = T.tpair T.trat T.tmutez in
        let tret = T.tmutez in
        T.mk_func name targ tret ctx (T.Abstract b)
      end
    | Bratdur -> begin
        let targ = T.tpair T.trat T.tint in
        let tret = T.tint in
        T.mk_func name targ tret ctx (T.Abstract b)
      end
    | Bsubnat -> begin
        let targ = T.tpair T.tnat T.tnat in
        let tret = T.tnat in
        T.mk_func name targ tret ctx (T.Abstract b)
      end
    | Bmuteztonat -> begin
        let targ = T.tmutez in
        let tret = T.tnat in
        T.mk_func name targ tret ctx (T.Abstract b)
      end
  in

  let extra_args : (ident * (ident * T.type_) list) list ref  = ref [] in


  let instr_update (ak : M.assign_kind) (op : T.aoperator) =
    match ak with
    | Avar id       -> T.Iupdate (Uvar (unloc id), op)
    | Avarstore id  -> T.Iupdate (Uvar (unloc id), op)
    | Aasset _      -> emit_error (UnsupportedTerm "Aasset")
    | Arecord (rn, fn, {node = Mvar(id, _, _, _)}) -> begin
        let l = Model.Utils.get_record_pos model (unloc rn) (unloc fn) in
        T.Iupdate (Urec (unloc id, l), op)
      end
    | Arecord _     -> emit_error (UnsupportedTerm "Arecord")
    | Astate        -> emit_error (UnsupportedTerm "Astate")
    | Aassetstate _ -> emit_error (UnsupportedTerm "Aassetstate")
    | Aoperations   -> T.Iupdate (Uvar operations, op)
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
    | Mchain_id  v      -> T.Dstring v
    | Mkey       v      -> T.Dstring v
    | Mkey_hash  v      -> T.Dstring v
    | Msignature v      -> T.Dstring v
    | Mbls12_381_fr   v -> T.Dbytes v
    | Mbls12_381_fr_n n -> T.Dint n
    | Mbls12_381_g1   v -> T.Dbytes v
    | Mbls12_381_g2   v -> T.Dbytes v
    | MsaplingStateEmpty _ -> T.Dlist []
    | MsaplingTransaction (_, b) -> T.Dbytes b
    | Mchest     b      -> T.Dbytes b
    | Mchest_key b      -> T.Dbytes b
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
    | Mvar (x, Vparameter, _, _) -> T.Dvar (unloc x, to_type model mt.type_)
    | Mlambda (_rt, id, _at, e) -> begin
        let env = mk_env () in
        let ir = mterm_to_intruction env e ~view:false in
        T.DIrCode (unloc id, ir)
      end
    | _ -> Format.printf "%a@." M.pp_mterm mt; assert false

  and mterm_to_intruction env (mtt : M.mterm) ?(view = false) : T.instruction =
    let f = mterm_to_intruction env ~view in
    let ft = to_type model in

    let get_entrypoint_annot ?(pref="") id =
      match id with
      | "" | "default" | "%default" -> None
      | _ -> Some (pref ^ id)
    in

    let contract_internal id a t d =
      let fdata =
        match id with
        | Some v -> (T.ipair (T.istring entrynotfound) (T.istring v))
        | None -> T.istring entrynotfound
      in
      T.Iifnone (T.Iunop (Ucontract (t, a), d), T.ifaild fdata, "_var_ifnone", Ivar "_var_ifnone", T.tint) in
    let get_entrypoint id t d =
      let annot = get_entrypoint_annot ~pref:"%" id in
      contract_internal (Some id) annot t d
    in

    let vops = T.Ivar operations in
    let get_contract   id t d = contract_internal id None     t d in
    let get_self_entrypoint id =
      let fs = M.Utils.get_fs model id in
      let ts = List.map proj3_2 fs.args in
      get_entrypoint id (to_one_type (List.map (to_type model) ts)) (T.Izop Zself_address)
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
      else if with_macro then begin
        if i + 1 = s
        then T.icdrn i x
        else T.icarn i x
      end
      else begin
        let x = Tools.foldi (T.icdr) x i in
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
        | T.RUassign (s1, l1), T.RUassign (s2, l2) -> doit (s1, l1) (s2, l2) (fun s x -> T.RUassign (s, x)) (fun accu p _  v2 -> List.addput p v2 accu)
        | T.RUnodes  (s1, l1), T.RUnodes  (s2, l2) -> doit (s1, l1) (s2, l2) (fun s x -> T.RUnodes (s, x))  (fun accu p v1 v2 -> List.addput p (merge v1 v2) accu)
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
    (* | Mreturn x when view        -> f x *)
    | Mreturn x                  -> T.Iassign (fun_result, f x)
    | Mlabel _                   -> T.iskip
    | Mmark  _                   -> T.iskip


    (* effect *)

    | Mfail ft          -> begin
        let x =
          match ft with
          | Invalid v              -> f v
          | InvalidCaller          -> T.istring "InvalidCaller"
          | InvalidCondition lbl   -> T.ipair (T.istring "InvalidCondition") (T.istring lbl)
          | NotFound               -> T.istring "NotFound"
          | AssetNotFound an       -> T.ipair (T.istring "AssetNotFound") (T.istring an)
          | KeyExists an           -> T.ipair (T.istring "KeyExists") (T.istring an)
          | KeyExistsOrNotFound an -> T.ipair (T.istring "KeyExistsOrNotFound") (T.istring an)
          | OutOfBound             -> T.istring "OutOfBound"
          | DivByZero              -> T.istring divbyzero
          | NatAssign              -> T.istring "NatAssign"
          | NoTransfer             -> T.istring "NoTransfer"
          | InvalidState           -> T.istring "InvalidState"
        in
        T.Iunop  (Ufail, x)
      end
    | Mtransfer tr -> begin
        let op =
          match tr with
          | TKsimple (v, d)         -> T.Iterop (Ttransfer_tokens, T.iunit, f v, get_contract None T.tunit (f d))
          | TKcall (v, id, t, d, a) -> T.Iterop (Ttransfer_tokens, f a, f v, get_entrypoint id (to_type model t) (f d))
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
    | Memit _                       -> T.iskip

    (* entrypoint *)

    | Mentrypoint (t, id, d, r)  ->
      let annot = get_entrypoint_annot (unloc id) in
      let a = T.Iunop (Ucontract (to_type model t, annot), f d) in
      begin
        match r with
        | Some r -> T.Iifnone (a, Iunop (Ufail, f r), "_var_ifnone", Ivar "_var_ifnone", ft mtt.type_)
        | None -> a
      end

    | Mcallview (t, a, b, c)  -> begin
        T.Ibinop (Bview (unloc b, to_type model t), f c, f a)
      end
    | Mself id                -> get_self_entrypoint (unloc id)


    (* operation *)

    | Moperations            -> vops
    | Mmkoperation (v, e, a) -> T.Iterop (Ttransfer_tokens, f a, f v, f e)


    (* literals *)

    | Mint  v            -> T.Iconst (T.mk_type Tint, Dint v)
    | Mnat  v            -> T.Iconst (T.mk_type Tnat, Dint v)
    | Mbool true         -> T.Iconst (T.mk_type Tbool, Dtrue)
    | Mbool false        -> T.Iconst (T.mk_type Tbool, Dfalse)
    | Mrational _        -> emit_error (UnsupportedTerm ("Mrational"))
    | Mstring v          -> T.Iconst (T.mk_type Tstring, Dstring v)
    | Mcurrency (v, Utz) -> T.Iconst (T.mk_type Tmutez, Dint v)
    | Maddress v         -> T.Iconst (T.mk_type Taddress, Dstring v)
    | Mdate v            -> T.Iconst (T.mk_type Ttimestamp, Dint (Core.date_to_timestamp v))
    | Mduration v        -> T.Iconst (T.mk_type Tint, Dint (Core.duration_to_timestamp v))
    | Mtimestamp v       -> T.Iconst (T.mk_type Ttimestamp,    Dint v)
    | Mbytes v           -> T.Iconst (T.mk_type Tbytes,        Dbytes v)
    | Mchain_id v        -> T.Iconst (T.mk_type Tchain_id,     Dstring v)
    | Mkey v             -> T.Iconst (T.mk_type Tkey,          Dstring v)
    | Mkey_hash v        -> T.Iconst (T.mk_type Tkey_hash,     Dstring v)
    | Msignature v       -> T.Iconst (T.mk_type Tsignature,    Dstring v)
    | Mbls12_381_fr v    -> T.Iconst (T.mk_type Tbls12_381_fr, Dbytes v)
    | Mbls12_381_fr_n v  -> T.Iconst (T.mk_type Tbls12_381_fr, Dint v)
    | Mbls12_381_g1 v    -> T.Iconst (T.mk_type Tbls12_381_g1, Dbytes v)
    | Mbls12_381_g2 v    -> T.Iconst (T.mk_type Tbls12_381_g2, Dbytes v)
    | Munit              -> T.Iconst (T.mk_type Tunit, Dunit)
    | MsaplingStateEmpty n -> T.Iconst (T.mk_type (Tsapling_state n), Dlist [])
    | MsaplingTransaction (n, v) -> T.Iconst (T.mk_type (Tsapling_transaction n), Dbytes v)
    | Mchest v           -> T.Iconst (T.mk_type Tchest, Dbytes v)
    | Mchest_key v       -> T.Iconst (T.mk_type Tchest_key, Dbytes v)

    (* control expression *)

    | Mexprif (c, t, e)                      -> T.Iif (f c, f t, f e, ft mtt.type_)
    | Mexprmatchwith (_e, _l)                -> emit_error (UnsupportedTerm ("Mexprmatchwith"))
    | Mmatchoption (x, i, ve, ne)            -> T.Iifnone (f x, f ne, unloc i, f ve, ft mtt.type_)
    | Mmatchor (x, lid, le, rid, re)         -> T.Iifleft (f x, unloc lid, f le, unloc rid, f re, ft mtt.type_)
    | Mmatchlist (x, hid, tid, hte, ee)      -> T.Iifcons (f x, unloc hid, unloc tid, f hte, f ee, ft mtt.type_)
    | Mfold (e, i, l)                        -> T.Iloopleft (f e, unloc i, f l)
    | Mmap (e, i, l)                         -> T.Imap_ (f e, unloc i, f l)
    | Mexeclambda (l, a)                     -> T.Ibinop (Bexec, f a, f l)
    | Mapplylambda (l, a)                    -> T.Ibinop (Bapply, f a, f l)

    (* composite type constructors *)

    | Mleft  (t, v) -> T.Iunop (Uleft (ft t), f v)
    | Mright (t, v) -> T.Iunop (Uright (ft t), f v)
    | Mnone    -> begin
        let t =
          match M.get_ntype mtt.type_ with
          | M.Toption t -> to_type model t
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
    | Mlitrecord l
    | Mlitevent l -> begin
        match l with
        | [] -> T.Iconst (T.mk_type Tunit, Dunit)
        | _ ->
          let ri =
            let ll = List.map (fun (_, x) -> f x) l in
            let mk_default _ = T.Rtuple ll in
            let doit f rn =
              let r : M.record = f model (unloc rn) in
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
            in
            match M.get_ntype mtt.type_ with
            | M.Trecord rn -> doit M.Utils.get_record rn
            | M.Tevent  rn -> doit M.Utils.get_event  rn
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
    | Msubnat (l, r)     -> let b = T.Bsubnat in add_builtin b; T.Icall (get_fun_name b, [f l; f r], is_inline b)


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
        | M.Tbuiltin Baddress, M.Tcontract t, _                -> get_contract None (to_type model t) (f v)
        | M.Tbuiltin Bcurrency, M.Tbuiltin Bnat, _             -> T.idiv (f v) (T.imutez Big_int.unit_big_int)
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

    (* set api instruction *)

    | Msetinstradd    (_, ak, v) -> instr_update ak (Aterop (Tupdate, f v, T.itrue))
    | Msetinstrremove (_, ak, v) -> instr_update ak (Aterop (Tupdate, f v, T.ifalse))

    (* list api expression *)

    | Mlistprepend (_t, i, l)    -> T.Ibinop (Bcons, f l, f i)
    | Mlistlength (_, l)         -> T.Iunop (Usize, f l)
    | Mlistcontains (t, c, a)    -> let b = T.BlistContains (to_type model t) in add_builtin b; T.Icall (get_fun_name b, [f c; f a], is_inline b)
    | Mlistnth (t, c, a)         -> let b = T.BlistNth (to_type model t) in add_builtin b; T.Icall (get_fun_name b, [f c; f a], is_inline b)
    | Mlistreverse _             -> emit_error (UnsupportedTerm ("Mlistreverse"))
    | Mlistconcat _              -> emit_error (UnsupportedTerm ("Mlistconcat"))
    | Mlistfold (_, ix, ia, c, a, b) -> T.Ifold (unloc ix, None, unloc ia, f c, f a, T.Iassign (unloc ia, f b))

    (* list api instruction *)

    | Mlistinstrprepend (_, ak, v) -> instr_update ak (Abinop (Bcons, f v))
    | Mlistinstrconcat  (_, ak, v) -> instr_update ak (Abinop (Bconcat, f v))


    (* map api expression *)

    | Mmapput (_, _, c, k, v)     -> T.Iterop (Tupdate, f k, T.isome (f v),   f c)
    | Mmapremove (_, tv, c, k)    -> T.Iterop (Tupdate, f k, T.inone (ft tv), f c)
    | Mmapupdate (_, _, c, k, v)  -> T.Iterop (Tupdate, f k, f v, f c)
    | Mmapget (_, _, c, k, oan)   ->
      let err = match oan with | Some an -> T.ipair (T.istring "AssetNotFound") (T.istring an) | None -> T.istring "NotFound" in
      T.Iifnone (T.Ibinop (Bget, f k, f c), T.ifaild err, "_var_ifnone", Ivar "_var_ifnone", ft mtt.type_)
    | Mmapgetopt (_, _, c, k)     -> T.Ibinop (Bget, f k, f c)
    | Mmapcontains (_, _, c, k)   -> T.Ibinop (Bmem, f k, f c)
    | Mmaplength (_, _, c)        -> T.Iunop (Usize, f c)
    | Mmapfold (_, ik, iv, ia, c, a, b) -> T.Ifold (unloc ik, Some (unloc iv), unloc ia, f c, f a, T.Iassign (unloc ia, f b))

    (* map api instruction *)

    | Mmapinstrput    (_, _,  ak, k, v) -> instr_update ak (Aterop (Tupdate, f k, T.isome (f v)))
    | Mmapinstrremove (_, tv, ak, k)    -> instr_update ak (Aterop (Tupdate, f k, T.inone (ft tv)))
    | Mmapinstrupdate (_, _,  ak, k, v) -> instr_update ak (Aterop (Tupdate, f k, f v) )

    (* builtin functions *)

    | Mmax (l, r)        -> let b = T.Bmax (to_type model l.type_) in add_builtin b; T.Icall (get_fun_name b, [f l; f r], is_inline b)
    | Mmin (l, r)        -> let b = T.Bmin (to_type model l.type_) in add_builtin b; T.Icall (get_fun_name b, [f l; f r], is_inline b)
    | Mabs x when is_rat x.type_ -> let b = T.Bratabs        in add_builtin b; T.Icall (get_fun_name b, [f x], is_inline b)
    | Mabs x             -> T.Iunop (Uabs, f x)
    | Mconcat (x, y)     -> T.Ibinop (Bconcat, f x, f y)
    | Mconcatlist x      -> T.Iunop (Uconcat, f x)
    | Mslice (x, s, e)   -> T.Iterop (Tslice, f s, f e, f x)
    | Mlength x          -> T.Iunop (Usize, f x)
    | Misnone x          -> T.Iifnone (f x, T.itrue,  "_var_ifnone", T.ifalse, T.tbool)
    | Missome x          -> T.Iifnone (f x, T.ifalse, "_var_ifnone", T.itrue, T.tbool)
    | Misnat x           -> T.Iunop (Uisnat, f x)
    | Mtonat x           -> T.Iifnone (T.Iunop (Uisnat, f x), T.ifail "NEG_VALUE", "_var_to_nat", Ivar "_var_to_nat", ft mtt.type_)
    | Moptget x          -> T.Iifnone (f x, T.ifail "NotFound", "_var_ifnone", Ivar "_var_ifnone", ft mtt.type_)
    | Mrequiresome (x, y)-> T.Iifnone (f x, Iunop (Ufail, f y), "_var_ifnone", Ivar "_var_ifnone", ft mtt.type_)
    | Mfloor  x          -> let b = T.Bfloor           in add_builtin b; T.Icall (get_fun_name b, [f x], is_inline b)
    | Mceil   x          -> let b = T.Bceil            in add_builtin b; T.Icall (get_fun_name b, [f x], is_inline b)
    | Mtostring (t, x)   -> let b = T.Btostring (ft t) in add_builtin b; T.Icall (get_fun_name b, [f x], is_inline b)
    | Mpack x            -> T.Iunop (Upack,  f x)
    | Munpack (t, x)     -> T.Iunop (Uunpack (ft t), f x)
    | Msetdelegate x     -> T.Iunop (Usetdelegate, f x)
    | Mimplicitaccount x -> T.Iunop (Uimplicitaccount, f x)
    | Mcontractaddress x -> T.Iunop (Uaddress, f x)
    | Maddresscontract x ->
      T.Iifnone (T.Iunop (Ucontract(T.tunit, None), f x), T.ifail "NotImplicitContract", "_var_ifnone", Ivar "_var_ifnone", ft mtt.type_)
    | Mkeyaddress      x -> T.Iunop (Uaddress, T.Iunop (Uimplicitaccount, T.Iunop  (Uhash_key, f x)))

    (* crypto functions *)

    | Mblake2b x                -> T.Iunop  (Ublake2b,  f x)
    | Msha256  x                -> T.Iunop  (Usha256,   f x)
    | Msha512  x                -> T.Iunop  (Usha512,   f x)
    | Msha3    x                -> T.Iunop  (Usha3,   f x)
    | Mkeccak  x                -> T.Iunop  (Ukeccak,   f x)
    | Mhashkey x                -> T.Iunop  (Uhash_key, f x)
    | Mchecksignature (k, s, x) -> T.Iterop (Tcheck_signature, f k, f s, f x)


    (* crypto functions *)

    | Mtotalvotingpower         -> T.Izop  (Ztotalvotingpower)
    | Mvotingpower  x           -> T.Iunop (Uvotingpower, f x)


    (* ticket *)

    | Mcreateticket (x, a)   -> T.Ibinop (Bcreateticket, f x, f a)
    | Mreadticket x          -> T.Imichelson ([Iunop (Ureadticket, f x)], T.cseq [T.cswap; T.cdrop 1], [])
    | Msplitticket (x, a, b) -> T.Ibinop (Bsplitticket,  f x, mk_tuple [f a; f b])
    | Mjointickets (x, y)    -> T.Iunop  (Ujointickets,  mk_tuple [f x; f y])


    (* sapling *)

    | Msapling_empty_state n        -> T.Izop (Zsapling_empty_state n)
    | Msapling_verify_update (s, t) -> T.Ibinop (Bsapling_verify_update, f s, f t)


    (* bls curve *)

    | Mpairing_check x -> T.Iunop (Upairing_check, f x)


    (* timelock *)

    | Mopen_chest (x, y, z) -> T.Iterop (Topen_chest, f x, f y, f z)


    (* constants *)

    | Mnow           -> T.Izop Znow
    | Mtransferred   -> T.Izop Zamount
    | Mcaller        -> T.Izop Zsender
    | Mbalance       -> T.Izop Zbalance
    | Msource        -> T.Izop Zsource
    | Mselfaddress   -> T.Izop Zself_address
    | Mchainid       -> T.Izop Zchain_id
    | Mmetadata      -> assert false
    | Mlevel         -> T.Izop Zlevel


    (* variable *)

    | Mvar (_an, Vassetstate _k, _, _) -> assert false
    | Mvar (v, Vstorevar, _, _)        -> T.Ivar (unloc v)
    | Mvar (v, Vstorecol, _, _)        -> T.Ivar (unloc v)
    | Mvar (_v, Vdefinition, _, _)     -> assert false
    | Mvar (v, Vlocal, _, _)           -> T.Ivar (unloc v)
    | Mvar (v, Vparam, _, _)           -> T.Ivar (unloc v)
    | Mvar (_v, Vfield, _, _)          -> assert false
    | Mvar (_, Vthe, _, _)             -> assert false
    | Mvar (_, Vstate, _, _)           -> assert false
    | Mvar (_, Vparameter, _, _)       -> assert false
    | Menumval (_id, _args, _e)        -> assert false

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


    (* utils *)

    | Mdatefromtimestamp _ -> emit_error (UnsupportedTerm ("Mdatefromtimestamp"))
    | Mmuteztonat v        -> let b = T.Bmuteztonat in add_builtin b; T.Icall (get_fun_name b, [f v], is_inline b)


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

  let storage_list = List.map (
      fun (si : M.storage_item) ->
        (unloc si.id), to_type model ~annotation:(mk_fannot (unloc si.id)) si.typ, to_data si.default)
      model.storage
  in

  let storage_type, storage_data =
    match storage_list with
    | []  -> T.mk_type Tunit, T.Dunit
    | [_, t, d] -> remove_annot t, d
    | _   -> let _, lt, ld = List.split3 storage_list in to_one_type lt, to_one_data ld
  in

  let env = mk_env () in

  let funs, entries, views =

    let for_fs _env (fs : M.function_struct) ?(view= false) =
      let name = unloc fs.name in
      let args = List.map (fun (id, t, _) -> unloc id, to_type model t) fs.args in
      let eargs = List.map (fun (id, t, _) -> unloc id, to_type model t) fs.eargs in
      let env = {function_p = Some (name, args)} in
      let body = mterm_to_intruction env fs.body ~view in
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
        | Mmax _                  -> (doit accu mt (T.Bmax (to_type model mt.type_)))
        | Mmin _                  -> (doit accu mt (T.Bmin (to_type model mt.type_)))
        | Mfloor _                -> (doit accu mt (T.Bfloor                ))
        | Mceil  _                -> (doit accu mt (T.Bceil                 ))
        | Mlistcontains (t, _, _) -> (doit accu mt (T.BlistContains (to_type model t)))
        | Mlistnth (t, _, _)      -> (doit accu mt (T.BlistNth (to_type model t)))
        | Mtostring (t, _)        -> (doit accu mt (T.Btostring (to_type model t) ))

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

    let for_fs_fun env (fs : M.function_struct) ret ?(view : bool = false) : T.func =
      let fid = unloc fs.name in
      let tret = to_type model ret in
      let name, args, _eargs, body = for_fs env fs ~view in
      let eargs = get_extra_args fs.body in
      extra_args := (fid, eargs)::!extra_args;
      let args = args @ eargs in
      let targ = to_one_type (List.map snd args) in
      let ctx = T.mk_ctx_func () ~args ~stovars:fs.stovars in
      mapargs := MapString.add fid (targ, tret) !mapargs;
      T.mk_func name targ tret ctx (T.Concrete (args, body))
    in

    let for_fs_entry env (fs : M.function_struct) ?(view= false) : T.entry =
      let name, args, eargs, body = for_fs env fs ~view in
      T.mk_entry name args eargs body
    in

    List.fold_left (fun (funs, entries, views) (x : M.function__) ->
        match x.node with
        | Entry fs -> (funs, entries @ [for_fs_entry env fs ~view:false], views)
        | Getter _ -> emit_error (UnsupportedTerm ("Getter"))
        | Function (fs, ret) -> funs @ [for_fs_fun env fs ret ~view:false], entries, views
        | View (fs, ret) -> (funs, entries, views @ [for_fs_fun env fs ret ~view:true ])
      ) ([], [], []) model.functions
  in
  let annot a (t : T.type_) = id{ t with annotation = Some (mk_fannot a)} in
  let parameter : T.type_ =
    let for_entry (e : T.entry) =
      let f l =
        match List.rev l with
        | []   -> T.tunit
        | [e]  -> annot (fst e) (snd e)
        | (id, te)::t -> List.fold_left (fun accu (id, te) -> T.mk_type (T.Tpair (annot id te, accu))) (annot id te) t
      in
      let  args : T.type_ = f e.args in
      let eargs : T.type_ =
        match e.eargs with
        | [] -> T.tunit
        | [t] -> snd t
        | ts -> f ts |> remove_annot
      in

      match args.node, eargs.node with
      | T.Tunit, T.Tunit -> T.tunit
      |       _, T.Tunit -> args
      | T.Tunit, _       -> T.tpair T.tunit eargs
      | _                -> T.tpair args eargs
    in
    let for_entry e = e |> for_entry |> (fun (x : T.type_) -> annot e.name x) in
    entries
    |> List.map for_entry
    |> shape_entrypoints (fun x y -> T.mk_type (T.Tor(x, y))) T.tunit
  in
  let with_operations = M.Utils.with_operations model in

  let funs = List.fold_left (fun accu x -> (get_builtin_fun x)::accu) funs !builtins in

  let name = unloc model.name in
  let parameters = List.map (fun (x : M.parameter) -> unloc x.name) model.parameters in
  T.mk_ir name storage_type storage_data storage_list parameter funs views entries ~with_operations:with_operations ~parameters


(* -------------------------------------------------------------------- *)

let map_implem : (string * T.code list) list = [
  get_fun_name (T.Bmin T.tunit)  , T.[cdup; cunpair; ccompare; clt; cif ([ccar], [ccdr])];
  get_fun_name (T.Bmax T.tunit)  , T.[cdup; cunpair; ccompare; clt; cif ([ccdr], [ccar])];
  get_fun_name T.Bratcmp         , T.[cunpair; cunpair; cdip (1, [cunpair]); cunpair; cdug 3; cmul; cdip (1, [cmul]); cswap; ccompare; cswap;
                                      cifleft ([cdrop 1; ceq], [cifleft ([cifleft ([cdrop 1; clt], [cdrop 1; cle])],
                                                                         [cifleft ([cdrop 1; cgt], [cdrop 1; cge])])])];
  get_fun_name T.Bfloor          , T.[cunpair; cediv; cifnone ([cfail divbyzero], [ccar])];
  get_fun_name T.Bceil           , T.[cunpair; cediv; cifnone ([cfail divbyzero], [cunpair; cswap; cint; ceq; cif ([], [cpush (tint, Dint Big_int.unit_big_int); cadd])])];
  get_fun_name T.Bratnorm        ,   [];
  get_fun_name T.Brataddsub      , T.[cunpair; cunpair; cdip (1, [cunpair; cswap; cdup]); cunpair; cswap; cdup; cdig 3; cmul; cdup; cpush (tnat, Dint Big_int.zero_big_int);
                                      ccompare; ceq; cif ([cfail divbyzero], []); cdug 4; cdig 3; cmul; cdip (1, [cmul]); cdig 3; cifleft ([cdrop 1; cadd], [cdrop 1; cswap; csub]); cpair;];
  get_fun_name T.Bratmul         , T.[cunpair; cdip (1, [cunpair]); cunpair; cdip (1, [cswap]); cmul;
                                      cdip (1, [cmul; cdup; cpush (tnat, Dint Big_int.zero_big_int); ccompare; ceq; cif ([cfail divbyzero], [])]); cpair ];
  get_fun_name T.Bratdiv         , T.[cunpair; cdip (1, [cunpair]); cunpair; cdig 3;
                                      cdup; cdig 3; cdup; cdug 4; cmul;
                                      cpush (tnat, T.Dint Big_int.zero_big_int); ccompare; ceq; cif ([cfail divbyzero], []);
                                      cpush (tint, T.Dint Big_int.zero_big_int); cdig 4; cdup; cdug 5; ccompare; cge; cif ([cint], [cneg]); cmul; cdip (1, [cmul; cabs]); cpair ];
  get_fun_name T.Bratuminus      , T.[cunpair; cneg; cpair];
  get_fun_name T.Bratabs         , T.[cunpair; cabs; cint; cpair];
  get_fun_name T.Brattez         , T.[cunpair; cunpair; cabs; cdig 2; cmul; cediv; cifnone ([cfail divbyzero], []); ccar;];
  get_fun_name T.Bratdur         , T.[cunpair; cunpair; cdig 2; cmul; cediv; cifnone ([cfail divbyzero], []); ccar;];
  get_fun_name T.Bsubnat         , T.[cunpair; csub; cdup; cpush (T.tint, T.Dint Big_int.zero_big_int); ccompare; cgt; cif ([cfail "NegResult"], []); cabs ];
  get_fun_name T.Bmuteztonat     , T.[cpush (tmutez, T.Dint Big_int.unit_big_int); cswap; cediv; cifnone ([T.cfail "DivByZero"], []); ccar;];
]

let concrete_michelson b : T.code =
  let error _ = emit_error (NoConcreteImplementationFor (get_fun_name b)) in
  let get_implem b : T.code list = List.assoc (get_fun_name b) map_implem in
  match b with
  | T.Bmin _          -> T.cseq (get_implem (Bmin T.tunit))
  | T.Bmax _          -> T.cseq (get_implem (Bmax T.tunit))
  | T.Bfloor          -> T.cseq (get_implem b)
  | T.Bceil           -> T.cseq (get_implem b)
  | T.BlistContains _ -> T.cseq T.[cunpair; cfalse; cswap; citer [cdig 2; cdup; cdug 3; ccompare; ceq; cor; ]; cdip (1, [cdrop 1])]
  | T.BlistNth _      -> error ()
  | T.Btostring _     -> error ()
  | T.Bratcmp         -> T.cseq T.[cunpair; cunpair; cdip (1, [cunpair]); cunpair; cdug 3; cmul; cdip (1, [cmul]); cswap; ccompare; cswap;
                                   cifleft ([cdrop 1; ceq], [cifleft ([cifleft ([cdrop 1; clt], [cdrop 1; cle])],
                                                                      [cifleft ([cdrop 1; cgt], [cdrop 1; cge])])])]
  | T.Bratnorm        -> T.cseq (get_implem b)
  | T.Brataddsub      -> T.cseq (get_implem b)
  | T.Bratmul         -> T.cseq (get_implem b)
  | T.Bratdiv         -> T.cseq (get_implem b)
  | T.Bratuminus      -> T.cseq (get_implem b)
  | T.Bratabs         -> T.cseq (get_implem b)
  | T.Brattez         -> T.cseq (get_implem b)
  | T.Bratdur         -> T.cseq (get_implem b)
  | T.Bsubnat         -> T.cseq (get_implem b)
  | T.Bmuteztonat     -> T.cseq (get_implem b)

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
let head_env (env : env) id =
  let i = get_sp_for_id env id in
  let rec remove i = function
    | _::tl when i = 0 -> tl
    | e::tl -> e::(remove (i - 1) tl)
    | _ -> assert false
  in
  let l = remove i env.vars in
  { env with vars = id::l }

let print_env ?(str="") env =
  Format.eprintf "%s: %a@." str pp_env env
(* Format.eprintf "var %s: %i@." id n; *)

let rec instruction_to_code env (i : T.instruction) : T.code * env =
  let fe env = instruction_to_code env in
  let f = fe env in

  let seq env l =
    match l with
    | []   -> T.cseq [], env
    | [e]  -> fe env e
    | e::t ->
      List.fold_left (fun (a, env) x -> begin
            let v, env = fe env x in (T.cseq [a; v], env)
          end ) (fe env e) t
  in

  let fold_gen g env l =
    match List.rev l with
    | []   -> T.cseq [], env
    | [e]  -> g env e
    | e::t ->
      List.fold_left (fun (a, env) x -> begin
            let v, env = g env x in (T.cseq [a; v; T.cpair], dec_env env)
          end ) (g env e) t
  in

  let fold env l = fold_gen fe env l in

  let assign env id v =
    let n = get_sp_for_id env id in
    let c =
      if n <= 0
      then T.cseq [ v; T.cswap; T.cdrop 1 ]
      else T.cseq [ v; (T.cdip (1, [T.cdig n; T.cdrop 1])); T.cdug n]
    in
    c, env
  in

  let z_op_to_code = function
    | T.Znow                   -> T.cnow
    | T.Zamount                -> T.camount
    | T.Zbalance               -> T.cbalance
    | T.Zsource                -> T.csource
    | T.Zsender                -> T.csender
    | T.Zaddress               -> T.caddress
    | T.Zchain_id              -> T.cchain_id
    | T.Zself a                -> T.cself a
    | T.Zself_address          -> T.cself_address
    | T.Znone t                -> T.cnone (rar t)
    | T.Zunit                  -> T.cunit
    | T.Znil t                 -> T.cnil (rar t)
    | T.Zemptyset t            -> T.cempty_set (rar t)
    | T.Zemptymap (k, v)       -> T.cempty_map (rar k, rar v)
    | T.Zemptybigmap (k, v)    -> T.cempty_big_map (rar k, rar v)
    | T.Ztotalvotingpower      -> T.ctotal_voting_power
    | T.Zlevel                 -> T.clevel
    | T.Zsapling_empty_state n -> T.csapling_empty_state n
  in

  let un_op_to_code = function
    | T.Ucar             -> T.ccar
    | T.Ucdr             -> T.ccdr
    | T.Uleft t          -> T.cleft (rar t)
    | T.Uright t         -> T.cright (rar t)
    | T.Uneg             -> T.cneg
    | T.Uint             -> T.cint
    | T.Unot             -> T.cnot
    | T.Uabs             -> T.cabs
    | T.Uisnat           -> T.cisnat
    | T.Usome            -> T.csome
    | T.Usize            -> T.csize
    | T.Upack            -> T.cpack
    | T.Uunpack        t -> T.cunpack (rar t)
    | T.Ublake2b         -> T.cblake2b
    | T.Usha256          -> T.csha256
    | T.Usha512          -> T.csha512
    | T.Usha3            -> T.csha3
    | T.Ukeccak          -> T.ckeccak
    | T.Uhash_key        -> T.chash_key
    | T.Ufail            -> T.cfailwith
    | T.Ucontract (t, a) -> T.ccontract (rar t, a)
    | T.Usetdelegate     -> T.cset_delegate
    | T.Uimplicitaccount -> T.cimplicit_account
    | T.Ueq              -> T.ceq
    | T.Une              -> T.cneq
    | T.Ugt              -> T.cgt
    | T.Uge              -> T.cge
    | T.Ult              -> T.clt
    | T.Ule              -> T.cle
    | T.Uvotingpower     -> T.cvoting_power
    | T.Ureadticket      -> T.cread_ticket
    | T.Ujointickets     -> T.cjoin_tickets
    | T.Upairing_check   -> T.cpairing_check
    | T.Uconcat          -> T.cconcat
    | T.Uaddress         -> T.caddress
    | T.UcarN n          -> T.ccarn n
    | T.UcdrN n          -> T.ccdrn n  in

  let bin_op_to_code = function
    | T.Badd                   -> T.cadd
    | T.Bsub                   -> T.csub
    | T.Bmul                   -> T.cmul
    | T.Bediv                  -> T.cediv
    | T.Blsl                   -> T.clsl
    | T.Blsr                   -> T.clsr
    | T.Bor                    -> T.cor
    | T.Band                   -> T.cand
    | T.Bxor                   -> T.cxor
    | T.Bcompare               -> T.ccompare
    | T.Bget                   -> T.cget
    | T.Bmem                   -> T.cmem
    | T.Bconcat                -> T.cconcat
    | T.Bcons                  -> T.ccons
    | T.Bpair                  -> T.cpair
    | T.Bexec                  -> T.cexec
    | T.Bapply                 -> T.capply
    | T.Bcreateticket          -> T.cticket
    | T.Bsplitticket           -> T.csplit_ticket
    | T.Bsapling_verify_update -> T.csapling_verify_update
    | T.Bview (c, t)           -> T.cview (c, t)
  in

  let ter_op_to_code = function
    | T.Tcheck_signature -> T.ccheck_signature
    | T.Tslice           -> T.cslice
    | T.Tupdate          -> T.cupdate
    | T.Ttransfer_tokens -> T.ctransfer_tokens
    | T.Topen_chest      -> T.copen_chest
  in

  let aop_to_code env = function
    | T.Aunop   op       ->
      let op = un_op_to_code op in
      [op]
    | T.Abinop (op, a) ->
      let a, _env = fe env a in
      let op = bin_op_to_code op in
      [a; op]
    | T.Aterop (op, a1, a2) ->
      let a2, env  = fe env a2 in
      let a1, _env = fe env a1 in
      let op = ter_op_to_code op in
      [a2; a1; op]
  in

  match i with
  | Iseq l               -> seq env l

  | IletIn (id, v, b, u)    -> begin
      let v, _ = f v in
      let env0 = add_var_env env id in
      let b, _ = fe env0 b in
      if u
      then T.cseq [v; b; T.cdrop 1], env
      else T.cseq [v; b; T.cdip (1, [T.cdrop 1])], inc_env env
    end

  | Ivar id -> begin
      let n = get_sp_for_id env id in
      let c =
        if n = 0
        then T.cdup
        else T.cdup_n (n + 1)
      in
      c, inc_env env
    end

  | Icall (id, args, inline)   -> begin
      let get_args env =
        match args with
        | [] -> T.cunit, inc_env env
        | _ -> fold env args
      in
      match inline, List.assoc_opt id map_implem with
      | true, Some body_fun ->
        let cargs, env = get_args env in
        T.cseq (cargs::body_fun), env
      | _ -> begin
          let fid, env   = fe env (Ivar id) in
          let cargs, env = get_args env in
          T.cseq [fid; cargs; T.cexec], dec_env env
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
      T.cseq [ c; T.cif ([t], [e]) ], env
    end

  | Iifnone (v, t, id, s, ty) -> begin
      let v, _ = fe env v in
      let t, _ = fe env t in
      let e, _ = fe (add_var_env env id) s in

      let ee, env =
        match ty.node with
        | T.Tunit -> [T.cdrop 1], env
        | _       -> [T.cswap; T.cdrop 1], inc_env env
      in

      T.cseq [ v; T.cifnone ([t], [e] @ ee) ], env
    end

  | Iifleft (v, lid, le, rid, re, ty) -> begin
      let v, _ = fe env v in
      let l, _ = fe (add_var_env env lid) le in
      let r, _ = fe (add_var_env env rid) re in

      let ee, env =
        match ty.node with
        | T.Tunit -> T.[cdrop 1], env
        | _       -> T.[cswap; cdrop 1], inc_env env
      in

      T.cseq [ v; T.cifleft ([l] @ ee, [r] @ ee) ], env
    end

  | Iifcons (x, hd, tl, hte, ne, ty) -> begin
      let x, _ = fe env x in
      let t, _ = fe (add_var_env (add_var_env env tl) hd) hte in
      let n, _ = fe env ne in

      let ee, env =
        match ty.node with
        | T.Tunit -> T.[cdrop 2], env
        | _       -> T.[cdug 2; cdrop 2], inc_env env
      in

      T.cseq T.[ x; cifcons ([t] @ ee, [n]) ], env
    end

  | Iloop (c, b) -> begin
      let c, _ = f c in
      let b, _ = f b in
      T.cseq T.[c; cloop [b; c]] , env
    end

  | Iiter (ids, c, b) -> begin
      let c, _ = f c in
      match ids with
      | [id] -> begin
          let env0 = add_var_env env id in
          let b, _ = fe env0 b in
          T.cseq T.[c; citer [b; cdrop 1]] , env
        end
      | [k; v] -> begin
          let env0 = add_var_env (add_var_env env v) k in
          let b, _ = fe env0 b in
          T.cseq T.[c; citer [cunpair; b; cdrop 2]] , env
        end
      | _ -> assert false
    end

  | Iloopleft (l, i, b) -> begin
      let l, _ = f l in
      let b, env = fe (add_var_env env i) b in

      T.cseq T.[l; cloop_left [b; cswap; cdrop 1]], env
    end

  | Ilambda (rt, id, at, e) -> begin
      let e, _env = fe (add_var_env env id) e in

      T.clambda (rt, at, T.[e; cswap; cdrop 1]), env
    end

  | Izop op -> begin
      let c = z_op_to_code op in
      c, inc_env env
    end
  | Iunop (op, e) -> begin
      let op = un_op_to_code op in
      let e, env = fe env e in
      let env = match op.node with T.FAILWITH -> fail_env env | _ -> env in
      T.cseq [e; op], env
    end
  | Ibinop (op, lhs, rhs) -> begin
      let op = bin_op_to_code op in
      let rhs, env = fe env rhs in
      let lhs, env = fe env lhs in
      T.cseq [rhs; lhs; op], (dec_env env)
    end
  | Iterop (op, a1, a2, a3) -> begin
      let op = ter_op_to_code op in
      let a3, env = fe env a3 in
      let a2, env = fe env a2 in
      let a1, env = fe env a1 in
      T.cseq [a3; a2; a1; op], (dec_env (dec_env env))
    end
  | Iupdate (ku, aop) -> begin
      match ku with
      | Uvar id -> begin
          let n = get_sp_for_id env id in
          let f env = aop_to_code env aop in
          let c =
            if n <= 0
            then T.cseq (f env)
            else T.cseq ([ T.cdig n ] @ (f (head_env env id)) @ [ T.cdug n ])
          in
          c, env
        end
      | Urec (id, l) -> begin
          let rec g env l x =
            (* Format.eprintf "x: %i@\n" x; *)
            match l with
            | [] -> assert false
            | (k, s)::q -> begin
                (* Format.eprintf "k, s: (%i, %i)@\n" k s; *)
                let unpair x = [T.cunpair] @ x @ [T.cpair] in
                let swap   x = [T.cswap]   @ x @ [T.cswap] in
                let h env =
                  match q with
                  | [] -> aop_to_code env aop
                  | _  -> g env q 0
                in
                if x = k
                then begin
                  if x < s - 1
                  then unpair (h (inc_env env))
                  else h env
                end
                else begin
                  if s = 1
                  then g env l (x + 1)
                  else begin
                    if x = s - 1
                    then  swap            (g env l (x + 1))
                    else (unpair |@ swap) (g (inc_env env) l (x + 1))
                  end
                end
              end
          in

          let c =
            let n = get_sp_for_id env id in
            if n <= 0
            then T.cseq (g env l 0)
            else T.cseq ([ T.cdig n ] @ (g (head_env env id) l 0) @ [ T.cdug n ])
          in
          c, env
        end
    end
  | Iconst (t, e) -> T.cpush (rar t, e), inc_env env
  | Icompare (op, lhs, rhs) -> begin
      let op =
        match op with
        | Ceq -> T.ceq
        | Cne -> T.cneq
        | Clt -> T.clt
        | Cle -> T.cle
        | Cgt -> T.cgt
        | Cge -> T.cge
      in
      let r, env = fe env rhs in
      let l, env = fe env lhs in
      T.cseq [r; l; T.ccompare; op], dec_env env
    end

  | Iset (t, l) -> begin
      T.cseq ((T.cempty_set t)::(l |> List.rev |> List.map (fun x -> let x, _ = fe (inc_env (inc_env env)) x in T.cseq [T.ctrue; x; T.cupdate ] ))), inc_env env
    end
  | Ilist (t, l) -> begin
      T.cseq ((T.cnil t)::(l |> List.rev |> List.map (fun x -> let x, _ = fe (inc_env env) x in T.cseq [ x; T.ccons ] ))), inc_env env
    end
  | Imap (b, k, v, l) -> begin
      let a = if b then T.cempty_big_map (k, v) else T.cempty_map (k, v) in
      T.cseq ([a] @
              (l
               |> List.rev
               |> List.map (fun (x, y) ->
                   let y, _ = fe (inc_env env) y in
                   let x, _ = fe (inc_env (inc_env env)) x in
                   T.cseq [y; T.csome; x; T.cupdate ] ))), inc_env env
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
                then [T.cswap ] @ g (l, env) (x + 1) @ [T.cswap]
                else [T.cswap; T.cunpair ] @ g (l, inc_env env) (x + 1) @ [T.cpair; T.cswap]
              end
            end
        in
        let a = g (l, env) 0 in
        let b =
          if s = 1
          then a
          else [T.cunpair] @ a @ [T.cpair]
        in
        T.cseq b, env
      in
      let rec aux env ru =
        match ru with
        | T.RUassign (s, l) -> assign env s l (fun env v s ->
            let env = if s = 1 then dec_env env else env in
            let v, _ = fe env v in
            [T.cdrop 1; v]
          )
        | T.RUnodes  (s, l) -> assign env s l (fun env v _ ->
            let v, _ = aux (inc_env env) v in
            [v]
          )
      in
      let v, _ = aux env ru in
      T.cseq ([x; v]), env
    end

  | Ifold (ix, iy, ia, c, a, b) -> begin
      let a, _env0 = fe env a in
      let c, _env1 = fe (add_var_env env ia) c in
      let env2, pi, n =
        let env_= add_var_env env ia in
        match iy with
        | Some iy -> add_var_env (add_var_env env_ iy) ix, T.cunpair, 2
        | None -> add_var_env env_ ix, T.cskip, 1
      in
      let b, _env2 = fe env2 b in
      T.cseq [a; c; T.citer [pi; b; T.cdrop n]], inc_env env
    end

  | Imap_ (x, id, e) -> begin
      let x, _env0 = fe env x in
      let e, _env1 = fe (add_var_env env id) e in
      T.cseq [x; T.cmap [e; T.cswap; T.cdrop 1]], inc_env env
    end

  | Imichelson (a, c, v) -> begin
      let a, _ = seq env a in
      T.cseq [a; c], { env with vars = v @ env.vars }
    end

and process_data (d : T.data) : T.data =
  let rec aux (d : T.data) : T.data =
    match d with
    | DIrCode (id, ir) -> begin
        let env = mk_env () in
        let env = add_var_env env id in
        let code, _env = instruction_to_code env ir in
        let code = T.cseq T.[code; cdip (1, [cdrop 1])]
                   |> T.Utils.flat
                   |> T.Utils.optim
        in
        Dcode code
      end
    | _ -> T.map_data aux d
  in
  aux d

and to_michelson (ir : T.ir) : T.michelson =
  let storage = ir.storage_type in
  (* let default = T.cseq [T.CDR; T.NIL (T.mk_type Toperation); T.PAIR ] in *)

  let build_code _ =

    let unfold = foldi (fun x -> T.cunpair::T.cswap::x ) [] in

    let unfold_n x =
      match x with
      | 0 | 1 -> []
      | _ -> [T.cunpair_n x]
    in

    let fold_n x =
      match x with
      | 0 | 1 -> []
      | _ -> [T.cpair_n x]
    in

    let get_funs _ : T.code list * ident list =
      let funs = List.map (
          fun (x : T.func) ->
            let code =
              match x.body with
              | Concrete (args, body) ->
                let env = mk_env ~vars:(args |> List.map fst) () in
                let nb_args = List.length args in
                (* let nb_as = nb_args - 1 in *)
                let unfold_args = unfold_n nb_args in
                let res = T.cpush (T.tunit, T.Dunit) in
                let env = add_var_env env fun_result in
                let es = if nb_args = 0 then T.[cswap; cdrop 1] else T.[cdug nb_args; T.cdrop nb_args] in
                let code, _ = instruction_to_code env body in unfold_args @ [res] @ code::es
              | Abstract b    -> [concrete_michelson b]
            in
            T.clambda (x.targ, x.tret, code), x.name
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

    let ops, ops_var, eops, opsf = if ir.with_operations then [T.cnil T.toperation], [operations], [T.cdig 1], 1 else [], [], [T.cnil T.toperation], 0 in

    let fff, eee = let n = df + opsf in (if n > 0 then [T.cdig n] else []), (if df > 0 then [T.cdip (1, [T.cdrop df]) ] else []) in

    let vars            = (let l = ir.storage_list in if List.is_empty l then ["_"] else List.map (fun (x, _, _) -> x) l) @ ops_var @ List.rev funids in
    let env             = mk_env () ~vars in
    let nb_storage_item = List.length ir.storage_list in
    let unfold_storage  = unfold_n nb_storage_item  in
    let fold_storage    = fold_n nb_storage_item in


    let for_entry (e : T.entry) =
      (* stack state : functions, (operations list)?, unfolded storage, extra_argument? , argument *)
      match e.args, e.eargs with
      | [], [] -> begin
          let code, _ = instruction_to_code env e.body in
          T.cseq ([T.cdrop 1] @ [code] @ fold_storage @ eops @ [T.cpair])
        end
      | l, m -> begin
          let nb_eargs = List.length m in
          let eargs = List.map fst m |> List.rev in
          let unfold_eargs =
            if List.length m > 0
            then if List.length l = 0
              then [T.cunpair; T.cdrop 1]
              else [T.cunpair]
            else []
          in

          let nb_args = List.length l in
          let nb_as = nb_args - 1 in
          let unfold_args = unfold nb_as in
          let args = List.map fst l |> List.rev in
          let env = { env with vars = args @ eargs @ env.vars } in
          let code, _ = instruction_to_code env e.body in
          T.cseq (unfold_eargs @ unfold_args @ [code] @ [T.cdrop (nb_args + nb_eargs)] @ fold_storage @ eops @ [T.cpair])
        end
    in

    let code =
      match ir.entries with
      | [] -> [T.ccdr; T.cnil (T.toperation); T.cpair]
      | entries ->
        let us = if nb_storage_item > 1 then [T.cdip (1, unfold_storage)] else [] in
        let e =
          entries
          |> List.map for_entry
          |> shape_entrypoints (fun x y -> T.cifleft ([x], [y])) (T.cseq [])
        in
        [T.cunpair] @ us @ [e]
    in
    T.cseq (cfuns @ ops @ fff @ code @ eee)
    |> T.Utils.flat
    |> T.Utils.optim
  in

  let build_view storage_list (v : T.func) =
    let id    = v.name in
    let param : T.type_ = v.targ in
    let ret   : T.type_ = v.tret in

    let unfold_all = function
      | []     -> []
      | [_]    -> []
      | [_; _] -> [T.cunpair]
      | l      -> [T.cunpair_n (List.length l)]
    in

    let extract_storage_vars stovars =

      (* Format.eprintf "extract_storage_vars: storage_list: [%a]@\n" (pp_list "; " (fun fmt (x, _, _) -> Format.fprintf fmt "%s" x) ) storage_list; *)
      (* Format.eprintf "extract_storage_vars: stovars: [%a]@\n" (pp_list "; " (fun fmt x -> Format.fprintf fmt "%s" x) ) stovars; *)

      let stos : (int * ident) list =
        storage_list
        |> List.mapi (fun i (x, _, _) -> (i, x))
        |> List.filter (fun (_, x) -> List.mem x stovars)
      in

      (* Format.eprintf "extract_storage_vars: stos: [%a]@\n" (pp_list "; " (fun fmt (x, y) -> Format.fprintf fmt "(%i, %s)" x y) ) stos; *)

      let rec doit (n : int) (s : int) (code, ids) stos : T.code list * ident list =
        match stos with
        | (k, id)::q -> begin
            (* Format.eprintf "extract_storage_vars: k: %i@\n" k; *)
            let found = k = n in
            (* Format.eprintf "extract_storage_vars: found: %b@\n" found; *)
            let last = n = s - 1 in
            (* Format.eprintf "extract_storage_vars: last: %b@\n" last; *)
            match found, last with
            | false, false -> doit (n + 1) s (code @ [T.ccdr], ids) stos
            | false, true  -> code @ [T.cdrop 1], ids
            | true, false  -> doit (n + 1) s (code @ [T.cunpair; T.cswap], id::ids) q
            | true, true   -> code, id::ids
          end
        | [] -> code @ [T.cdrop 1], ids
      in

      let n = List.length storage_list in
      (* Format.eprintf "extract_storage_vars: n: %i@\n" n; *)
      let code, ids = doit 0 n ([], []) stos in
      code, ids
    in

    let env = mk_env () in

    let fold_vars, env, nb_args =
      match v.ctx.args, v.ctx.stovars with
      | [], [] -> [T.cdrop 1], env, 0

      | args, [] ->
        let code = [T.ccar] @ unfold_all args in
        let env = { env with vars = List.map fst args @ env.vars } in
        code, env, List.length args

      | [], stovars ->
        let scode, svs = extract_storage_vars stovars in
        (* Format.eprintf "RES: scode: @[%a@], sys: [%a]@\n" (pp_list "; " Printer_michelson.pp_code) scode (pp_list "; " pp_ident) svs; *)
        let code = [T.ccdr] @ scode in
        let env = { env with vars = svs @ env.vars } in
        code, env, List.length svs

      | args, stovars ->
        let scode, svs = extract_storage_vars stovars in
        let acode = unfold_all args in
        let scode = match scode with | [] -> [] | _ -> [T.cdip (1, scode)] in
        let code = [T.cunpair] @ scode @ acode in
        let avs = List.map fst args in
        let env = { env with vars = avs @ svs @ env.vars } in
        code, env, List.length (svs @ avs)

    in

    let fold_vars = fold_vars @ [T.cunit] in

    let env = add_var_env env fun_result in
    (* print_env env; *)

    let code, _env =
      match v.body with
      | Concrete (_args, instr) -> instruction_to_code env instr
      | Abstract _ -> assert false
    in

    let post =
      match nb_args with
      | 0 -> []
      | _ -> [T.cdip (1, [T.cdrop nb_args])]
    in

    let code = T.cseq (code::post) in

    let body  : T.code  =
      T.cseq (fold_vars @ [code])
      |> T.Utils.flat
      |> T.Utils.optim
    in
    T.mk_view_struct id param ret body
  in

  let code = build_code () in
  let parameters = ir.parameters in
  let views = List.map (build_view ir.storage_list) ir.views in
  T.mk_michelson ~parameters ~views storage ir.parameter code
