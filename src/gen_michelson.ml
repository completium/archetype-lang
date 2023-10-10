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

type error_desc =
  | FieldNotFoundFor of string * string
  | UnsupportedTerm of string
  | StackEmptyDec
  | StackIdNotFound of string * string list
  | StackIdNotPopulated of string * string list
  | NoConcreteImplementationFor of string
  | TODO
[@@deriving show {with_path = false}]

let pp_error_desc fmt e =
  let pp s = Format.fprintf fmt s in
  match e with
  | TODO                            -> pp "TODO"
  | FieldNotFoundFor (rn, fn)       -> pp "Field not found for record '%s' and field '%s'" rn fn
  | UnsupportedTerm s               -> pp "UnsupportedTerm: %s" s
  | StackEmptyDec                   -> pp "StackEmptyDec"
  | StackIdNotFound (id, stack)     -> pp "StackIdNotFound: %s on [%a]" id (pp_list "; " (fun fmt x -> Format.fprintf fmt "%s" x)) stack
  | StackIdNotPopulated (id, stack) -> pp "StackIdNotPopulated: %s on [%a]" id (pp_list "; " (fun fmt x -> Format.fprintf fmt "%s" x)) stack
  | NoConcreteImplementationFor s   -> pp "No concrete implementation for: %s" s

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

let is_rat t = match M.get_ntype t with | M.Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)] -> true     | _ -> false

let is_ticket_type (model : M.model) ty =
  let rec aux (accu : bool) (ty : M.type_) : bool =
    match fst ty with
    | M.Tticket _ -> true
    | M.Tcontract _ -> accu
    | M.Trecord id -> begin
        let r = M.Utils.get_record model id in
        List.fold_left (fun accu (x : M.record_field) -> accu || aux accu (x.type_)) accu r.fields
      end
    | _ -> M.fold_typ aux accu ty
  in
  aux false ty

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

let remove_annot (t : T.type_) = {t with annotation = None}

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

let to_one_type_or (l : T.type_ list) : T.type_ = to_one_gen T.tunit (fun x accu -> (T.mk_type (T.Tor (x, accu)))) l

let to_one_nary_gen init f l =
  match l with
  | [] -> init
  | [v] -> v
  | _ -> f l

let to_one_type (l : T.type_ list) : T.type_ = to_one_nary_gen T.tunit (fun x -> (T.tpair x)) l

let to_one_data (l : T.data list) : T.data = to_one_nary_gen T.Dunit (fun x -> (T.Dpair x)) l

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

  let process_record (f : M.model -> mident -> M.record) (id : M.mident)  =
    let r = f model id in
    let lt = List.map (fun (x : M.record_field) ->
        match snd x.type_ with
        | Some _ -> x.type_
        | None -> fst x.type_, Some (dumloc (mk_fannot (M.unloc_mident x.name)))) r.fields in
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

  let process_enum ?annotation (id : mident) =
    let e_opt : M.enum option = M.Utils.get_enum_opt model (M.unloc_mident id) in
    match e_opt with
    | Some e when not (List.for_all (fun (x : M.enum_item) -> List.is_empty x.args) e.values) -> begin
        let lt = List.map (fun (x : M.enum_item) : T.type_ ->
            T.mk_type ~annotation:(mk_fannot (M.unloc_mident x.name)) (to_one_type (List.map to_type x.args) |> fun x -> x.node)
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
      | Brational     -> T.Tpair [T.mk_type Tint; T.mk_type Tnat]
      | Bdate         -> T.Ttimestamp
      | Bduration     -> T.Tint
      | Btimestamp    -> T.Ttimestamp
      | Bstring       -> T.Tstring
      | Baddress      -> T.Taddress
      | Btez          -> T.Tmutez
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
  | Tcontainer ((Tasset an, _), View) -> begin
      let _, ty = M.Utils.get_asset_key model an in
      T.mk_type ?annotation (T.Tlist (to_type ty))
    end
  | Tcontainer ((Tasset an, _), (Aggregate | Partition)) -> begin
      let _, ty = M.Utils.get_asset_key model an in
      T.mk_type ?annotation (T.Tset (to_type ty))
    end
  | Tcontainer ((Tasset an, _), AssetKey) -> begin
      let _, ty = M.Utils.get_asset_key model an in
      to_type ty
    end
  | Tcontainer ((Tasset an, _), AssetValue) -> begin
      let ty = M.Utils.get_asset_value model an in
      to_type ty
    end
  | Tcontainer _               -> assert false
  | Tlist t                    -> T.mk_type ?annotation (T.Tlist (to_type t))
  | Toption t                  -> T.mk_type ?annotation (T.Toption (to_type t))
  | Ttuple lt                  -> T.mk_type ?annotation (to_one_type (List.map to_type lt) |> fun x -> x.node)
  | Tset t                     -> T.mk_type ?annotation (T.Tset (to_type t))
  | Tmap (k, v)                -> T.mk_type ?annotation (T.Tmap (to_type k, to_type v))
  | Tbig_map (k, v)            -> T.mk_type ?annotation (T.Tbig_map (to_type k, to_type v))
  | Titerable_big_map (k, v)   -> begin
      let kt = k |> to_type |> remove_annot in
      let vt = v |> to_type |> remove_annot in
      T.mk_type
        ?annotation
        (to_one_type
           [
             T.mk_type ~annotation:"%values" (Tbig_map (kt, T.mk_type ~annotation:"%values" (T.Tpair [T.mk_type ~annotation:"%index" Tnat; T.mk_type ~annotation:"%value" vt.node])));
             T.mk_type ~annotation:"%keys" (Tbig_map (T.tnat, kt));
             T.mk_type ~annotation:"%size" Tnat
           ] |> fun x -> x.node)
    end
  | Tor (l, r)                 -> T.mk_type ?annotation (T.Tor (to_type l, to_type r))
  | Trecord id                 -> let t = process_record M.Utils.get_record id in T.mk_type ?annotation t.node
  | Tevent id                  -> let t = process_record M.Utils.get_event id in T.mk_type ?annotation t.node
  | Tlambda (a, r)             -> T.mk_type ?annotation (Tlambda (to_type a, to_type r))
  | Tunit                      -> T.mk_type ?annotation (T.Tunit)
  | Toperation                 -> T.mk_type ?annotation (T.Toperation)
  | Tcontract t                -> T.mk_type ?annotation (T.Tcontract (to_type t))
  | Tticket t                  -> T.mk_type ?annotation (T.Tticket (to_type t))
  | Tsapling_state n           -> T.mk_type ?annotation (T.Tsapling_state n)
  | Tsapling_transaction n     -> T.mk_type ?annotation (T.Tsapling_transaction n)

let rec to_simple_data (model : M.model) (mt : M.mterm) : T.data option =
  let f = to_simple_data model in
  let dolist (l : M.mterm list) = List.fold_right (fun x accu -> match accu with | Some l -> (match f x with | Some v -> Some (v::l) | None -> None) | None -> None) l (Some []) in
  let dolist2 (l : (M.mterm * M.mterm) list) = List.fold_right (fun (x, y) accu -> match accu with | Some l -> (match f x, f y with | Some v, Some w -> Some ((v, w)::l) | _ -> None) | None -> None) l (Some []) in
  match mt.node with
  | Munit                      -> Some (T.Dunit)
  | Mbool      true            -> Some (T.Dtrue)
  | Mbool      false           -> Some (T.Dfalse)
  | Mint       n               -> Some (T.Dint n)
  | Mnat       n               -> Some (T.Dint n)
  | Mstring    s               -> Some (T.Dstring s)
  | Mmutez  v                  -> Some (T.Dint v)
  | Maddress   v               -> Some (T.Dstring v)
  | Mdate      v               -> Some (T.Dint (Core.date_to_timestamp v))
  | Mduration  n               -> Some (T.Dint (Core.duration_to_timestamp n))
  | Mtimestamp v               -> Some (T.Dint v)
  | Mbytes     b               -> Some (T.Dbytes b)
  | Mchain_id  v               -> Some (T.Dstring v)
  | Mkey       v               -> Some (T.Dstring v)
  | Mkey_hash  v               -> Some (T.Dstring v)
  | Msignature v               -> Some (T.Dstring v)
  | Mbls12_381_fr   v          -> Some (T.Dbytes v)
  | Mbls12_381_fr_n n          -> Some (T.Dint n)
  | Mbls12_381_g1   v          -> Some (T.Dbytes v)
  | Mbls12_381_g2   v          -> Some (T.Dbytes v)
  | MsaplingStateEmpty _       -> Some (T.Dlist [])
  | MsaplingTransaction (_, b) -> Some (T.Dbytes b)
  | Mchest     b               -> Some (T.Dbytes b)
  | Mchest_key b               -> Some (T.Dbytes b)
  | Mtz_expr   v               -> Some (T.Dconstant v)
  | Mnone                      -> Some (T.Dnone)
  | Msome      v               -> let v = f v in (match v with | Some x -> Some (T.Dsome x) | None -> None)
  | Mtuple     l               -> let v = dolist  l in (match v with | Some l -> Some (to_one_data l) | None -> None)
  | Mlitset    l               -> let v = dolist  l in (match v with | Some l -> Some (T.Dlist l) | None -> None)
  | Mlitlist   l               -> let v = dolist  l in (match v with | Some l -> Some (T.Dlist l) | None -> None)
  | Mlitmap    (_, l)          -> let v = dolist2 l in (match v with | Some l -> Some (T.Dlist (List.map (fun (x, y) -> T.Delt (x, y)) l)) | None -> None)
  | Muminus    v               -> let v = f v in (match v with | Some (Dint n) -> Some (T.Dint (Big_int.mult_int_big_int (-1) n)) | _ -> None)
  | Mleft (_, x)               -> let x = f x in (match x with | Some x -> Some (T.Dleft x) | None -> None)
  | Mright (_, x)              -> let x = f x in (match x with | Some x -> Some (T.Dright x) | None -> None)
  | Mcast (_, _, v)            -> f v
  | Mvar (x, Vparameter)       -> Some (T.Dvar (M.unloc_mident x, to_type model mt.type_, false))
  | _ -> None

let to_ir (model : M.model) : T.ir =

  let builtins = ref [] in

  let is_inline = function
    | T.Bmin           _ -> true
    | T.Bmax           _ -> true
    | T.Bfloor           -> true
    | T.Bceil            -> true
    | T.BlistContains  _ -> false
    | T.BlistNth       _ -> false
    | T.BlistHead      _ -> false
    | T.BlistTail      _ -> false
    | T.Bratcmp          -> false
    | T.Bratnorm         -> true
    | T.Brataddsub       -> true
    | T.Bratdiv          -> true
    | T.Bratmul          -> true
    | T.Bratuminus       -> true
    | T.Bratabs          -> true
    | T.Brattez          -> true
    | T.Bratdur          -> true
    | T.Bmuteztonat      -> true
    | T.Bsimplify_rational -> false
    | T.Bis_implicit_address -> false
  in

  let add_builtin b =
    if not (is_inline b) && not (List.exists (T.cmp_builtin b) !builtins)
    then builtins := b::!builtins;
  in

  let get_builtin_fun b loc : T.func =
    let name = T.Utils.get_fun_name Printer_michelson.show_pretty_type b in
    let ctx = T.mk_ctx_func () in
    match b with
    | Bmin t
    | Bmax t -> begin
        let targ = T.tpair [t; t] in
        let tret = t in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | Bfloor
    | Bceil -> begin
        let targ = T.trat in
        let tret  = T.tint in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | BlistContains t -> begin
        let targ = T.tpair [(T.tlist t); t] in
        let tret = T.tbool in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | BlistNth t -> begin
        let targ = T.tpair [(T.tlist t); T.tnat] in
        let tret = T.toption t in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | BlistHead t -> begin
        let targ = T.tpair [(T.tlist t); T.tnat] in
        let tret = T.tlist t in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | BlistTail t -> begin
        let targ = T.tpair [(T.tlist t); T.tnat] in
        let tret = T.tlist t in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | Bratcmp -> begin
        let targ = T.tpair [(T.tpair [T.trat; T.trat]); (T.tor T.tunit (T.tor (T.tor T.tunit T.tunit) (T.tor T.tunit T.tunit)))] in
        let tret = T.tbool in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | Bratnorm -> begin
        let targ = T.trat in
        let tret = T.trat in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | Brataddsub -> begin
        let targ = T.tpair [(T.tpair [T.trat; T.trat]); (T.tor T.tunit T.tunit)] in
        let tret = T.trat in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | Bratmul
    | Bratdiv -> begin
        let targ = T.tpair [T.trat; T.trat] in
        let tret = T.trat in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | Bratuminus
    | Bratabs -> begin
        let targ = T.trat in
        let tret = T.trat in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | Brattez -> begin
        let targ = T.tpair [T.trat; T.tmutez] in
        let tret = T.tmutez in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | Bratdur -> begin
        let targ = T.tpair [T.trat; T.tint] in
        let tret = T.tint in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | Bmuteztonat -> begin
        let targ = T.tmutez in
        let tret = T.tnat in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | Bsimplify_rational -> begin
        let targ = T.trat in
        let tret = T.trat in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
    | Bis_implicit_address -> begin
        let targ = T.taddress in
        let tret = T.tbool in
        T.mk_func name targ tret ctx (T.Abstract b) loc
      end
  in

  let extra_args : (ident * (ident * T.type_) list) list ref  = ref [] in


  let instr_update (ak : M.assign_kind) (op : T.aoperator) loc : T.instruction =
    match ak with
    | Avar id       -> T.iupdate ~loc (Uvar (M.unloc_mident id)) op
    | Avarstore id  -> T.iupdate ~loc (Uvar (M.unloc_mident id)) op
    | Aasset _      -> emit_error (UnsupportedTerm "Aasset")
    | Arecord ({node = Mvar(id, _)}, rn, fn) -> begin
        let l = Model.Utils.get_record_pos model rn (M.unloc_mident fn) in
        T.iupdate ~loc (Urec (M.unloc_mident id, l)) op
      end
    | Arecord _     -> emit_error (UnsupportedTerm "Arecord")
    | Atuple ({node = Mvar(id, _)}, n, l) -> T.iupdate ~loc (Urec (M.unloc_mident id, [n, l])) op
    | Atuple _      -> emit_error (UnsupportedTerm "Atuple")
    | Astate        -> emit_error (UnsupportedTerm "Astate")
    | Aoperations   -> T.iupdate ~loc (Uvar operations) op
  in

  let rec to_data (mt : M.mterm) : T.data =
    match mt.node with
    | Munit             -> T.Dunit
    | Mbool      true   -> T.Dtrue
    | Mbool      false  -> T.Dfalse
    | Mint       n      -> T.Dint n
    | Mnat       n      -> T.Dint n
    | Mstring    s      -> T.Dstring s
    | Mmutez     v      -> T.Dint v
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
    | Mtz_expr   v      -> T.Dconstant v
    | Mnone             -> T.Dnone
    | Msome      v      -> T.Dsome (to_data v)
    | Mtuple     l      -> to_one_data (List.map to_data l)
    | Mlitset    l      -> T.Dlist (List.map to_data l)
    | Mlitlist   l      -> T.Dlist (List.map to_data l)
    | Mlitmap    (_, l) -> T.Dlist (List.map (fun (x, y) -> T.Delt (to_data x, to_data y)) l)
    | Muminus    v      -> to_data v |> (function | T.Dint n -> T.Dint (Big_int.mult_int_big_int (-1) n) | _ -> assert false )
    | Mnow              -> T.Dint (Unix.time () |> int_of_float |> Big_int.big_int_of_int)
    | Mleft (_, x)      -> T.Dleft (to_data x)
    | Mright (_, x)     -> T.Dright (to_data x)
    | Mcast (_, _, v)   -> to_data v
    | Mvar (x, Vparameter) -> T.Dvar (M.unloc_mident x, to_type model mt.type_, false)
    | Mlitrecord l      -> begin
        let data = List.map (to_data |@ snd) l in
        match M.get_ntype mt.type_ with
        | Trecord rn -> begin
            let r = M.Utils.get_record model rn in
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
    | Mlambda (_rt, id, _at, e) -> begin
        let env = mk_env () in
        let ir = mterm_to_intruction env e ~view:false in
        T.DIrCode (M.unloc_mident id, ir)
      end
    | _ -> Format.printf "%a@." M.pp_mterm mt; assert false

  and mterm_to_intruction ?(view = false) env (mtt : M.mterm) : T.instruction =
    let f = mterm_to_intruction env ~view in
    let ft = to_type model in

    let get_entrypoint_annot ?(pref="") id =
      match id with
      | "" | "default" | "%default" -> None
      | _ -> Some (pref ^ id)
    in

    let contract_internal id a t d loc =
      let fdata =
        match id with
        | Some v -> (T.ipair (T.istring M.fail_msg_ENTRY_NOT_FOUND) (T.istring v))
        | None -> T.istring M.fail_msg_ENTRY_NOT_FOUND
      in
      T.iifnone ~loc (T.iunop (Ucontract (t, a)) d) (T.ifaild fdata) "_var_ifnone" (T.ivar "_var_ifnone") T.tint
    in

    let get_entrypoint id t d loc =
      let annot = get_entrypoint_annot ~pref:"%" id in
      contract_internal (Some id) annot t d loc
    in

    let vops = T.ivar operations in

    let get_contract   id t d loc = contract_internal id None t d loc in
    let get_self_entrypoint id loc =
      let fs = M.Utils.get_fs model id in
      let ts = List.map proj3_2 fs.args in
      get_entrypoint id (to_one_type (List.map (to_type model) ts)) (T.izop Zself_address) loc
    in

    let mk_tuple ?loc l : T.instruction =
      match List.rev l with
      | []  -> T.iunit ?loc ()
      | [e] -> e
      | e::t -> List.fold_left (fun accu x -> T.ibinop Bpair x accu) e t
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
      let fn = M.unloc_mident fn in
      match M.get_ntype e.type_ with
      | M.Trecord rn -> begin
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

    let gen_var_access (mt : M.mterm) : T.instruction option =
      let original_type = mt.type_ in
      let rec aux (accu : T.access_item list) (mt : M.mterm) : T.instruction option =
        match mt.node with
        | M.Mdot (x, id) -> begin
            let fn = M.unloc_mident id in
            match M.get_ntype x.type_ with
            | M.Trecord rn -> begin
                let pos = M.Utils.get_record_pos model rn fn in
                let path = List.map (fun (idx, length) -> T.{ai_index = idx; ai_length = length}) pos in
                aux (path @ accu) x
              end
            | _ -> None
          end
        | M.Mtupleaccess (x, n) -> begin
            let idx : int = Big_int.int_of_big_int n in
            let length = match M.get_ntype x.type_ with | Ttuple l -> List.length l | _ -> 0 in
            aux (T.{ai_index = idx; ai_length = length}::accu) x
          end
        | M.Mvar (id, kind) -> begin

            let f x =
              T.ivar_access ~loc:mtt.loc {
                av_ident = x;
                av_path = accu;
                av_source_no_dup = is_ticket_type model mt.type_;
                av_value_no_dup = is_ticket_type model original_type;
              }
            in

            match kind with
            | Vassetstate _k   -> None
            | Vstorevar        -> Some (f (M.unloc_mident id))
            | Vstorecol        -> Some (f (M.unloc_mident id))
            | Vlocal           -> Some (f (M.unloc_mident id))
            | Vparam           -> Some (f (M.unloc_mident id))
            | Vfield           -> None
            | Vthe             -> None
            | Vstate           -> None
            | Vparameter       -> None

          end
        | _ -> None
      in
      aux [] mt
    in

    match mtt.node with

    (* lambda *)

    | Mletin (ids, LVsimple v, _, b, _) -> begin
        let is_unit = match M.get_ntype mtt.type_ with Tunit -> true | _ -> false in
        T.iletIn ~loc:mtt.loc (List.map M.unloc_mident ids) (f v) (f b) is_unit
      end
    | Mletin ([id], LVreplace (idv, dk, fa), _, b, _) -> begin
        let to_dk = function
          | M.DK_option (ty, _) -> T.KLVoption (ft ty)
          | M.DK_map (kt, _, k) -> T.KLVmap (ft kt, f k)
        in
        let is_unit = match M.get_ntype mtt.type_ with Tunit -> true | _ -> false in

        let v = T.ireplace ~loc:mtt.loc (M.unloc_mident id) (M.unloc_mident idv) (to_dk dk) (f fa) in
        T.iletIn ~loc:mtt.loc [M.unloc_mident id] v (f b) is_unit
      end    | Mletin _                  -> emit_error (UnsupportedTerm ("Mletin"))
    | Mdeclvar _                -> emit_error (UnsupportedTerm ("Mdeclvar"))
    | Mdeclvaropt _             -> emit_error (UnsupportedTerm ("Mdeclvaropt"))
    | Mapp (e, args)            -> begin
        let eargs =
          match List.assoc_opt (M.unloc_mident e) !extra_args with
          | Some l -> List.map (fun (id, _t) -> T.ivar id) l
          | _ -> []
        in
        T.icall ~loc:mtt.loc (M.unloc_mident e) (List.map f args @ eargs) false
      end

    (* assign *)

    | Massign (_op, _, Avar id, v)                 -> T.iassign  ~loc:mtt.loc (M.unloc_mident id) (f v)
    | Massign (_op, _, Avarstore id, v)            -> T.iassign  ~loc:mtt.loc (M.unloc_mident id) (f v)
    | Massign (_op, _, Aasset (_an, _fn, _k), _v)  -> emit_error (UnsupportedTerm ("Massign: Aasset"))
    | Massign (_op, _, Arecord ({node = Mvar (id, _); type_ = t}, _rn, fn), v) -> begin
        let lid, id = aspair (snd id) in
        let rn =
          match M.get_ntype t with
          | M.Trecord rn -> rn
          | _ -> assert false
        in
        let ru = make_ru rn (M.unloc_mident fn) v in
        let a = T.irecupdate (T.ivar ~loc:lid id) ru in
        T.iassign ~loc:mtt.loc id a
      end
    | Massign (_op, _, Arecord _, _v)              -> emit_error (UnsupportedTerm ("Record is not a var"))
    | Massign (_op, _, Atuple ({node = Mvar (id, _)}, n, l), v) -> let id = M.unloc_mident id in T.iassigntuple ~loc:mtt.loc id n l (f v)
    | Massign (_op, _, Atuple _, _v)               -> emit_error (UnsupportedTerm ("Tuple is not a var"))
    | Massign (_op, _, Astate, _x)                 -> emit_error (UnsupportedTerm ("Massign: Astate"))
    | Massign (_op, _, Aoperations, v)             -> T.iassign ~loc:mtt.loc operations (f v)
    | Massignopt _ -> emit_error (UnsupportedTerm ("Massignopt"))

    (* control *)

    | Mif (c, t, Some e)         -> T.iif ~loc:mtt.loc (f c) (f t) (f e) T.tunit
    | Mif (c, t, None)           -> T.iif ~loc:mtt.loc (f c) (f t) (T.iskip ()) T.tunit
    | Mmatchwith (_e, _l)        -> emit_error (UnsupportedTerm ("Mmatchwith"))
    | Minstrmatchoption (x, i, ve, ne)       -> T.iifnone ~loc:mtt.loc (f x) (f ne) (M.unloc_mident i) (f ve) T.tunit
    | Minstrmatchor (x, lid, le, rid, re)    -> T.iifleft ~loc:mtt.loc (f x) (M.unloc_mident lid) (f le) (M.unloc_mident rid) (f re) T.tunit
    | Minstrmatchlist (x, hid, tid, hte, ee) -> T.iifcons ~loc:mtt.loc (f x) (M.unloc_mident hid) (M.unloc_mident tid) (f hte) (f ee) T.tunit
    | Minstrmatchdetach (dk, i, ve, ne) -> begin
        let id, klv =
          match dk with
          | DK_option (ty, id) -> id, (T.KLVoption (ft ty))
          | DK_map (ty, id, k) -> id, (T.KLVmap (ft ty, f k))
        in
        T.iifnone ~loc:mtt.loc (T.irep id klv) (f ne) (M.unloc_mident i) (f ve) T.tunit
      end
    | Mfor (id, c, b) -> begin
        let ids =
          match id with
          | M.FIsimple x      -> [x]
          | M.FIdouble (x, y) -> [x; y]
        in
        let ids = List.map M.unloc_mident ids in
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
        T.iiter ~loc:mtt.loc ids c b
      end
    | Miter (_i, _a, _b, _c, _) -> emit_error (UnsupportedTerm ("Miter"))
    | Mwhile (c, b)             -> T.iloop ~loc:mtt.loc (f c) (f b)
    | Mseq is                   -> T.iseq ~loc:mtt.loc (List.map f is)
    | Mreturn x                 -> T.iassign ~loc:mtt.loc fun_result (f x)


    (* effect *)

    | Mfail ft          -> begin
        let x =
          match ft with
          | Invalid v              -> f v
          | InvalidCaller          -> T.istring M.fail_msg_INVALID_CALLER
          | InvalidSource          -> T.istring M.fail_msg_INVALID_SOURCE
          | InvalidCondition (lbl, v) -> (match v with | None -> T.ipair (T.istring M.fail_msg_INVALID_CONDITION) (T.istring lbl) | Some v -> f v)
          | NotFound               -> T.istring M.fail_msg_NOT_FOUND
          | AssetNotFound an       -> T.ipair (T.istring M.fail_msg_ASSET_NOT_FOUND) (T.istring an)
          | KeyExists an           -> T.ipair (T.istring M.fail_msg_KEY_EXISTS) (T.istring an)
          | KeyExistsOrNotFound an -> T.ipair (T.istring M.fail_msg_KEY_EXISTS_OR_NOT_FOUND) (T.istring an)
          | DivByZero              -> T.istring M.fail_msg_DIV_BY_ZERO
          | NatNegAssign           -> T.istring M.fail_msg_NAT_NEG_ASSIGN
          | NoTransfer             -> T.istring M.fail_msg_NO_TRANSFER
          | InvalidState           -> T.istring M.fail_msg_INVALID_STATE
        in
        T.iunop ~loc:mtt.loc Ufail x
      end
    | Mfailexpr e -> T.iunop ~loc:mtt.loc Ufail (f e)
    | Mfailsome _ -> emit_error (UnsupportedTerm ("Mfailsome"))
    | Mtransfer tr -> begin
        let op =
          match tr with
          | TKsimple (v, d)         -> T.iterop ~loc:mtt.loc Ttransfer_tokens (T.iunit ()) (f v) (get_contract None T.tunit (f d) mtt.loc)
          | TKcall (v, id, t, d, a) -> T.iterop ~loc:mtt.loc Ttransfer_tokens (f a) (f v) (get_entrypoint id (to_type model t) (f d) mtt.loc)
          | TKentry (v, e, a)       -> T.iterop ~loc:mtt.loc Ttransfer_tokens (f a) (f v) (f e)
          | TKgen (v, id, _cn, t, a, arg) -> T.iterop ~loc:mtt.loc Ttransfer_tokens (f arg) (f v) (get_entrypoint id (to_type model t) (f a) mtt.loc)
          | TKself (v, id, args)    -> begin
              let a =
                match args with
                | []  -> T.iunit ()
                | [e] -> f (snd e)
                | _   -> T.isrecord ~loc:mtt.loc (List.map (fun (_, x) -> f x) args)
              in
              T.iterop ~loc:mtt.loc Ttransfer_tokens a (f v) (get_self_entrypoint id mtt.loc)
            end
          | TKoperation op -> f op
        in
        T.iassign operations ~loc:mtt.loc (T.ireverse T.toperation (T.ibinop Bcons op (T.ireverse T.toperation vops)))
      end
    | Memit (e, value)  -> begin
        let op = T.iunop ~loc:mtt.loc (Uemit((to_type model (M.tevent e)), Some ("%" ^ (M.unloc_mident e)))) (f value) in
        T.iassign ~loc:mtt.loc operations (T.ireverse T.toperation (T.ibinop Bcons op (T.ireverse T.toperation vops)))
      end
    | Mdetach _  -> emit_error (UnsupportedTerm ("Mdetach"))
    | Mmicheline micheline  -> T.imicheline ~loc:mtt.loc micheline [] []

    (* entrypoint *)

    | Mgetentrypoint (t, id, d)  ->
      let annot = get_entrypoint_annot (M.unloc_mident id) in
      T.iunop ~loc:mtt.loc (Ucontract (to_type model t, annot)) (f d)

    | Mcallview (t, a, b, c)           -> T.ibinop ~loc:mtt.loc (Bview (M.unloc_mident b, to_type model t)) (f c) (f a)
    | Mimportcallview (_t, _a, _b, _c) -> emit_error (UnsupportedTerm ("Mimportcallview"))
    | Mself id                         -> get_self_entrypoint (M.unloc_mident id) mtt.loc
    | Mselfcallview (_t, _id, _args)   -> emit_error (UnsupportedTerm ("Mselfcallview"))


    (* operation *)

    | Moperations                    -> vops
    | Mmakeoperation (v, e, a)       -> T.iterop ~loc:mtt.loc Ttransfer_tokens (f a) (f v) (f e)
    | Mmakeevent (t, id, a)          -> T.iunop ~loc:mtt.loc (Uemit (to_type model t, Some (M.unloc_mident id))) (f a)
    | Mcreatecontract (cc, d, a) -> begin
        match cc with
        | CCTz (tz, arg) -> T.iunop ~loc:mtt.loc UforcePair (T.iterop (Tcreate_contract tz.ms_content) (f d) (f a) (f arg))
        | _ -> assert false
      end



    (* literals *)

    | Mint  v                    -> T.iconst ~loc:mtt.loc T.tint                     (Dint v)
    | Mnat  v                    -> T.iconst ~loc:mtt.loc T.tnat                     (Dint v)
    | Mbool true                 -> T.iconst ~loc:mtt.loc T.tbool                     Dtrue
    | Mbool false                -> T.iconst ~loc:mtt.loc T.tbool                     Dfalse
    | Mrational _                -> emit_error                                       (UnsupportedTerm ("Mrational"))
    | Mstring v                  -> T.iconst ~loc:mtt.loc T.tstring                  (Dstring v)
    | Mmutez v                   -> T.iconst ~loc:mtt.loc T.tmutez                   (Dint v)
    | Maddress v                 -> T.iconst ~loc:mtt.loc T.taddress                 (Dstring v)
    | Mdate v                    -> T.iconst ~loc:mtt.loc T.ttimestamp               (Dint (Core.date_to_timestamp v))
    | Mduration v                -> T.iconst ~loc:mtt.loc T.tint                     (Dint (Core.duration_to_timestamp v))
    | Mtimestamp v               -> T.iconst ~loc:mtt.loc T.ttimestamp               (Dint v)
    | Mbytes v                   -> T.iconst ~loc:mtt.loc T.tbytes                   (Dbytes v)
    | Mchain_id v                -> T.iconst ~loc:mtt.loc T.tchain_id                (Dstring v)
    | Mkey v                     -> T.iconst ~loc:mtt.loc T.tkey                     (Dstring v)
    | Mkey_hash v                -> T.iconst ~loc:mtt.loc T.tkey_hash                (Dstring v)
    | Msignature v               -> T.iconst ~loc:mtt.loc T.tsignature               (Dstring v)
    | Mbls12_381_fr v            -> T.iconst ~loc:mtt.loc T.tbls12_381_fr            (Dbytes v)
    | Mbls12_381_fr_n v          -> T.iconst ~loc:mtt.loc T.tbls12_381_fr            (Dint v)
    | Mbls12_381_g1 v            -> T.iconst ~loc:mtt.loc T.tbls12_381_g1            (Dbytes v)
    | Mbls12_381_g2 v            -> T.iconst ~loc:mtt.loc T.tbls12_381_g2            (Dbytes v)
    | Munit                      -> T.iconst ~loc:mtt.loc T.tunit                     Dunit
    | MsaplingStateEmpty n       -> T.iconst ~loc:mtt.loc (T.tsapling_state n)       (Dlist [])
    | MsaplingTransaction (n, v) -> T.iconst ~loc:mtt.loc (T.tsapling_transaction n) (Dbytes v)
    | Mchest v                   -> T.iconst ~loc:mtt.loc T.tchest                   (Dbytes v)
    | Mchest_key v               -> T.iconst ~loc:mtt.loc T.tchest_key               (Dbytes v)
    | Mtz_expr _                 -> emit_error                                       (UnsupportedTerm ("Mtz_expr"))

    (* control expression *)

    | Mexprif (c, t, e)                      -> T.iif ~loc:mtt.loc (f c) (f t) (f e) (ft mtt.type_)
    | Mexprmatchwith (_e, _l)                -> emit_error (UnsupportedTerm ("Mexprmatchwith"))
    | Mmatchoption (x, i, ve, ne)            -> T.iifnone ~loc:mtt.loc (f x) (f ne) (M.unloc_mident i) (f ve) (ft mtt.type_)
    | Mmatchor (x, lid, le, rid, re)         -> T.iifleft ~loc:mtt.loc (f x) (M.unloc_mident lid) (f le) (M.unloc_mident rid) (f re) (ft mtt.type_)
    | Mmatchlist (x, hid, tid, hte, ee)      -> T.iifcons ~loc:mtt.loc (f x) (M.unloc_mident hid) (M.unloc_mident tid) (f hte) (f ee) (ft mtt.type_)
    | Mternarybool (_c, _a, _b)              -> emit_error (UnsupportedTerm ("Mternarybool"))
    | Mternaryoption (_c, _a, _b)            -> emit_error (UnsupportedTerm ("Mternaryoption"))
    | Mfold (e, i, l)                        -> T.iloopleft ~loc:mtt.loc (f e) (M.unloc_mident i) (f l)
    | Mmap (e, i, l)                         -> T.imap_ ~loc:mtt.loc (f e) (M.unloc_mident i) (f l)
    | Mexeclambda (l, a)                     -> T.ibinop ~loc:mtt.loc (Bexec) (f a) (f l)
    | Mapplylambda (l, a)                    -> T.ibinop ~loc:mtt.loc (Bapply) (f a) (f l)

    (* composite type constructors *)

    | Mleft  (t, v) -> T.iunop ~loc:mtt.loc (Uleft (ft t)) (f v)
    | Mright (t, v) -> T.iunop ~loc:mtt.loc (Uright (ft t)) (f v)
    | Mnone    -> begin
        let t =
          match M.get_ntype mtt.type_ with
          | M.Toption t -> to_type model t
          | _ -> assert false
        in
        T.izop ~loc:mtt.loc (T.Znone t)
      end

    | Msome v -> T.iunop ~loc:mtt.loc Usome (f v)

    | Mtuple l      -> mk_tuple ~loc:mtt.loc (List.map f l)
    | Masset     _l -> emit_error (UnsupportedTerm ("Masset"))
    | Massets    _l -> emit_error (UnsupportedTerm ("Massets"))
    | Mlitset    l -> begin
        match M.get_ntype mtt.type_ with
        | M.Tset t -> T.iset ~loc:mtt.loc (ft t) (List.map f l)
        | _ -> assert false
      end
    | Mlitlist   l ->  begin
        match M.get_ntype mtt.type_ with
        | M.Tlist t -> T.ilist ~loc:mtt.loc (ft t) (List.map f l)
        | _ -> assert false
      end
    | Mlitmap (_, l) -> begin
        match M.get_ntype mtt.type_ with
        | M.Tmap (k, v) -> T.imap ~loc:mtt.loc false (ft k) (ft v) (List.map (fun (x, y) -> f x, f y) l)
        | M.Tbig_map (k, v) -> T.imap ~loc:mtt.loc true (ft k) (ft v) (List.map (fun (x, y) -> f x, f y) l)
        | _ -> assert false
      end
    | Mlitrecord l
    | Mlitevent l -> begin
        match l with
        | [] -> T.iconst ~loc:mtt.loc (T.mk_type Tunit) Dunit
        | _ ->
          let ri =
            let ll = List.map (fun (_, x) -> f x) l in
            let mk_default _ = T.Rtuple ll in
            let doit f rn =
              let r : M.record = f model rn in
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
          T.irecord ~loc:mtt.loc ri
      end
    | Mlambda (rt, id, at, e) -> T.ilambda ~loc:mtt.loc (ft rt) (M.unloc_mident id) (ft at) (f e)
    | Mlambda_michelson (it, rt, body) -> T.ilambda_michelson ~loc:mtt.loc (ft it) (ft rt) body
    | Mmicheline_expr (t, m, a) -> T.imicheline ~loc:mtt.loc m [ft t] (List.map f a)

    (* access *)

    | Mdot (e, i)           -> (match gen_var_access mtt with | Some v -> v | _ -> access_record e i)
    | Mdotassetfield _      -> emit_error (UnsupportedTerm ("Mdotassetfield"))
    | Mquestionoption _     -> emit_error (UnsupportedTerm ("Mquestionoption"))

    (* comparison operators *)

    | Mequal (_, l, r)  -> T.icompare ~loc:mtt.loc (Ceq) (f l) (f r)
    | Mnequal (_, l, r) -> T.icompare ~loc:mtt.loc (Cne) (f l) (f r)
    | Mgt (l, r)        -> T.icompare ~loc:mtt.loc (Cgt) (f l) (f r)
    | Mge (l, r)        -> T.icompare ~loc:mtt.loc (Cge) (f l) (f r)
    | Mlt (l, r)        -> T.icompare ~loc:mtt.loc (Clt) (f l) (f r)
    | Mle (l, r)        -> T.icompare ~loc:mtt.loc (Cle) (f l) (f r)
    | Mmulticomp _      -> emit_error (UnsupportedTerm ("Mmulticomp"))


    (* arithmetic operators *)

    | Mand (l, r)        -> T.ibinop ~loc:mtt.loc Band (f l) (f r)
    | Mor (l, r)         -> T.ibinop ~loc:mtt.loc Bor  (f l) (f r)
    | Mgreedyand (l, r)  -> T.ibinop ~loc:mtt.loc Band (f l) (f r)
    | Mgreedyor (l, r)   -> T.ibinop ~loc:mtt.loc Bor  (f l) (f r)
    | Mxor (l, r)        -> T.ibinop ~loc:mtt.loc Bxor (f l) (f r)
    | Mnot e             -> T.iunop ~loc:mtt.loc  Unot (f e)
    | Mplus (l, r)       -> T.iadd ~loc:mtt.loc (f l) (f r)
    | Mminus (l, r)      -> begin
        match M.get_ntype mtt.type_ with
        | M.Tbuiltin Btez -> T.iifnone ~loc:mtt.loc (T.isub_mutez (f l) (f r)) (T.ifail M.fail_msg_NAT_NEG_ASSIGN) "_var_ifnone" (T.ivar "_var_ifnone") (ft mtt.type_)
        | _ -> T.isub ~loc:mtt.loc (f l) (f r)
      end
    | Mmult (l, r)       -> T.imul ~loc:mtt.loc (f l) (f r)
    | Mdivrat _          -> emit_error (UnsupportedTerm ("Mdivrat"))
    | Mdiveuc (l, r)     -> T.idiv ~loc:mtt.loc (f l) (f r)
    | Mmodulo (l, r)     -> T.imod ~loc:mtt.loc (f l) (f r)
    | Mdivmod (l, r)     -> T.ibinop ~loc:mtt.loc Bediv (f l) (f r)
    | Muminus e          -> T.iunop ~loc:mtt.loc  Uneg (f e)
    | MthreeWayCmp (l, r)-> T.ibinop ~loc:mtt.loc Bcompare (f l) (f r)
    | Mshiftleft (l, r)  -> T.ibinop ~loc:mtt.loc Blsl (f l) (f r)
    | Mshiftright (l, r) -> T.ibinop ~loc:mtt.loc Blsr (f l) (f r)
    | Msubnat (l, r)     -> T.iunop ~loc:mtt.loc Uisnat (T.ibinop Bsub (f l) (f r))
    | Msubmutez (l, r)   -> T.isub_mutez ~loc:mtt.loc (f l) (f r)


    (* asset api effect *)

    | Maddasset _       -> emit_error (UnsupportedTerm ("Maddasset"))
    | Mputsingleasset _ -> emit_error (UnsupportedTerm ("Mputsingleasset"))
    | Mputasset _       -> emit_error (UnsupportedTerm ("Mputasset"))
    | Maddfield _       -> emit_error (UnsupportedTerm ("Maddfield"))
    | Mremoveasset _    -> emit_error (UnsupportedTerm ("Mremoveasset"))
    | Mremoveall _      -> emit_error (UnsupportedTerm ("Mremoveall"))
    | Mremovefield _    -> emit_error (UnsupportedTerm ("Mremovefield"))
    | Mremoveif _       -> emit_error (UnsupportedTerm ("Mremoveif"))
    | Mclear _          -> emit_error (UnsupportedTerm ("Mclear"))
    | Mset _            -> emit_error (UnsupportedTerm ("Mset"))
    | Mupdate _         -> emit_error (UnsupportedTerm ("Mupdate"))
    | Mupdateall _      -> emit_error (UnsupportedTerm ("Mupdateall"))
    | Maddupdate _      -> emit_error (UnsupportedTerm ("Maddupdate"))
    | Mputremove _      -> emit_error (UnsupportedTerm ("Mputremove"))


    (* asset api expression *)

    | Mget      _ -> emit_error (UnsupportedTerm ("Mget"))
    | Mgetsome  _ -> emit_error (UnsupportedTerm ("Mgetsome"))
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
        | M.Tbuiltin Baddress, M.Tcontract t, _ -> get_contract None (to_type model t) (f v) mtt.loc
        | M.Tbuiltin Btez, M.Tbuiltin Bnat, _   -> T.idiv ~loc:mtt.loc (f v) (T.imutez Big_int.unit_big_int)
        | _ -> f v
      end
    | Mtupleaccess (x, n) -> begin
        match gen_var_access mtt with
        | Some v -> v
        | _ -> begin
            let s = (match M.get_ntype x.type_ with | Ttuple l -> List.length l | _ -> 0) in access_tuple s (Big_int.int_of_big_int n) (f x)
          end
      end
    | Mrecupdate (x, l) ->
      let t = mtt.type_ in
      let rn =
        match M.get_ntype t with
        | M.Trecord rn -> rn
        | _ -> assert false
      in
      let ru = List.fold_left (fun (ru : T.ruitem option) (fn, v) -> Some (make_ru ?ru rn fn v)) None l in
      let ru = match ru with | None -> assert false | Some v -> v in
      T.irecupdate ~loc:mtt.loc (f x) ru
    | Mmakeasset _ -> emit_error (UnsupportedTerm ("Mmakeasset"))
    | Mtocontainer _ -> emit_error (UnsupportedTerm ("Mtocontainer"))
    | Mglobal_constant (ty, v) -> T.iconst ~loc:mtt.loc (ft ty) (to_data v)

    (* set api expression *)

    | Msetadd (_, c, a)             -> T.iterop ~loc:mtt.loc (Tupdate) (f a) (T.itrue ()) (f c)
    | Msetremove (_, c, a)          -> T.iterop ~loc:mtt.loc (Tupdate) (f a) (T.ifalse ()) (f c)
    | Msetupdate (_, c, b, v)       -> T.iterop ~loc:mtt.loc (Tupdate) (f v) (f b) (f c)
    | Msetcontains (_, c, k)        -> T.ibinop ~loc:mtt.loc  Bmem     (f k) (f c)
    | Msetlength (_, c)             -> T.iunop  ~loc:mtt.loc  Usize     (f c)
    | Msetfold (_, ix, ia, c, a, b) -> T.ifold  ~loc:mtt.loc (M.unloc_mident ix) None (M.unloc_mident ia) (f c) (f a) (T.iassign (M.unloc_mident ia) (f b))

    (* set api instruction *)

    | Msetinstradd    (_, ak, v) -> instr_update ak (Aterop (Tupdate, f v, T.itrue ())) mtt.loc
    | Msetinstrremove (_, ak, v) -> instr_update ak (Aterop (Tupdate, f v, T.ifalse ())) mtt.loc

    (* list api expression *)

    | Mlistprepend (_t, i, l)    -> T.ibinop Bcons (f l) (f i)
    | Mlistlength (_, l)         -> T.iunop Usize (f l)
    | Mlistcontains (t, c, a)    -> let b = T.BlistContains (to_type model t) in add_builtin b; T.icall (get_fun_name b) [f c; f a] (is_inline b)
    | Mlistnth (t, c, a)         -> let b = T.BlistNth (to_type model t) in add_builtin b;      T.icall (get_fun_name b) [f c; f a] (is_inline b)
    | Mlisthead (t, c, a)        -> let b = T.BlistHead (to_type model t) in add_builtin b;     T.icall (get_fun_name b) [f c; f a] (is_inline b)
    | Mlisttail (t, c, a)        -> let b = T.BlistTail (to_type model t) in add_builtin b;     T.icall (get_fun_name b) [f c; f a] (is_inline b)
    | Mlistreverse (t, l)        -> T.ireverse (to_type model t) (f l)
    | Mlistconcat _              -> emit_error (UnsupportedTerm ("Mlistconcat"))
    | Mlistfold (_, ix, ia, c, a, b) -> T.ifold (M.unloc_mident ix) None (M.unloc_mident ia) (f c) (f a) (T.iassign (M.unloc_mident ia) (f b))

    (* list api instruction *)

    | Mlistinstrprepend (_, ak, v) -> instr_update ak (Abinop (Bcons, f v)) mtt.loc
    | Mlistinstrconcat  (_, ak, v) -> instr_update ak (Abinop (Bconcat, f v)) mtt.loc


    (* map api expression *)

    | Mmapput (_, _, _, c, k, v)     -> T.iterop ~loc:mtt.loc Tupdate (f k) (T.isome (f v)) (f c)
    | Mmapremove (_, _, tv, c, k)    -> T.iterop ~loc:mtt.loc Tupdate (f k) (T.inone (ft tv)) (f c)
    | Mmapupdate (_, _, _, c, k, v)  -> T.iterop ~loc:mtt.loc Tupdate (f k) (f v) (f c)
    | Mmapget (_, _, _, _, _, _)     -> emit_error (UnsupportedTerm ("Mmapget"))
    | Mmapgetopt (_, _, _, c, k)     -> T.ibinop ~loc:mtt.loc Bget (f k) (f c)
    | Mmapcontains (_, _, _, c, k)   -> T.ibinop ~loc:mtt.loc Bmem (f k) (f c)
    | Mmaplength (_, _, _, c)        -> T.iunop ~loc:mtt.loc Usize (f c)
    | Mmapfold (_, _, ik, iv, ia, c, a, b) -> T.ifold ~loc:mtt.loc (M.unloc_mident ik) (Some (M.unloc_mident iv)) (M.unloc_mident ia) (f c) (f a) (T.iassign (M.unloc_mident ia) (f b))

    (* map api instruction *)

    | Mmapinstrput    (_, _, _,  ak, k, v) -> instr_update ak (Aterop (Tupdate, f k, T.isome (f v))) mtt.loc
    | Mmapinstrremove (_, _, tv, ak, k)    -> instr_update ak (Aterop (Tupdate, f k, T.inone (ft tv))) mtt.loc
    | Mmapinstrupdate (_, _, _,  ak, k, v) -> instr_update ak (Aterop (Tupdate, f k, f v) ) mtt.loc

    (* builtin functions *)

    | Mmax (l, r)                -> let b = T.Bmax (to_type model l.type_) in add_builtin b; T.icall ~loc:mtt.loc (get_fun_name b) [f l; f r] (is_inline b)
    | Mmin (l, r)                -> let b = T.Bmin (to_type model l.type_) in add_builtin b; T.icall ~loc:mtt.loc (get_fun_name b) [f l; f r] (is_inline b)
    | Mabs x when is_rat x.type_ -> let b = T.Bratabs in add_builtin b; T.icall ~loc:mtt.loc (get_fun_name b) [f x] (is_inline b)
    | Mabs x                     -> T.iunop   ~loc:mtt.loc Uabs (f x)
    | Mconcat (x, y)             -> T.ibinop  ~loc:mtt.loc Bconcat (f x) (f y)
    | Mconcatlist x              -> T.iunop   ~loc:mtt.loc Uconcat (f x)
    | Mslice (x, s, e)           -> T.iterop  ~loc:mtt.loc Tslice (f s) (f e) (f x)
    | Mlength x                  -> T.iunop   ~loc:mtt.loc Usize (f x)
    | Misnone x                  -> T.iifnone ~loc:mtt.loc (f x) (T.itrue ())  "_var_ifnone" (T.ifalse ()) T.tbool
    | Missome x                  -> T.iifnone ~loc:mtt.loc (f x) (T.ifalse ()) "_var_ifnone" (T.itrue ()) T.tbool
    | Minttonat x                -> T.iunop   ~loc:mtt.loc Uisnat (f x)
    | Mfloor  x                  -> let b = T.Bfloor in add_builtin b; T.icall ~loc:mtt.loc (get_fun_name b) [f x] (is_inline b)
    | Mceil   x                  -> let b = T.Bceil  in add_builtin b; T.icall ~loc:mtt.loc (get_fun_name b) [f x] (is_inline b)
    | Mnattostring _             -> emit_error (UnsupportedTerm ("Mnattostring"))
    | Mbytestonat x              -> T.iunop ~loc:mtt.loc  Unat                    (f x)
    | Mnattobytes x              -> T.iunop ~loc:mtt.loc  Ubytes                  (f x)
    | Mbytestoint x              -> T.iunop ~loc:mtt.loc  Uint                    (f x)
    | Minttobytes x              -> T.iunop ~loc:mtt.loc  Ubytes                  (f x)
    | Mpack x                    -> T.iunop ~loc:mtt.loc  Upack                   (f x)
    | Munpack (t, x)             -> T.iunop ~loc:mtt.loc (Uunpack (ft t))         (f x)
    | Msetdelegate x             -> T.iunop ~loc:mtt.loc  Usetdelegate            (f x)
    | Mkeyhashtocontract x       -> T.iunop ~loc:mtt.loc  Uimplicitaccount        (f x)
    | Mcontracttoaddress x       -> T.iunop ~loc:mtt.loc  Uaddress                (f x)
    | Maddresstocontract (t, x)  -> T.iunop ~loc:mtt.loc (Ucontract (ft t, None)) (f x)
    | Mkeytoaddress    x         -> T.iunop ~loc:mtt.loc  Uaddress                (T.iunop Uimplicitaccount (T.iunop Uhash_key (f x)))
    | Msimplify_rational x       -> let b = T.Bsimplify_rational in add_builtin b; T.icall ~loc:mtt.loc (get_fun_name b) ([f x]) (is_inline b)
    | Mget_numerator     x       -> T.iunop ~loc:mtt.loc Ucar                     (f x)
    | Mget_denominator   x       -> T.iunop ~loc:mtt.loc Ucdr                     (f x)
    | Misimplicitaddress x       -> let b = T.Bis_implicit_address in add_builtin b; T.icall ~loc:mtt.loc (get_fun_name b) ([f x]) (is_inline b)
    | Mexp_horner (_x, _s)       -> emit_error (UnsupportedTerm ("Mexp_horner"))

    (* crypto functions *)

    | Mblake2b x                -> T.iunop  ~loc:mtt.loc Ublake2b         (f x)
    | Msha256  x                -> T.iunop  ~loc:mtt.loc Usha256          (f x)
    | Msha512  x                -> T.iunop  ~loc:mtt.loc Usha512          (f x)
    | Msha3    x                -> T.iunop  ~loc:mtt.loc Usha3            (f x)
    | Mkeccak  x                -> T.iunop  ~loc:mtt.loc Ukeccak          (f x)
    | Mkeytokeyhash x           -> T.iunop  ~loc:mtt.loc Uhash_key        (f x)
    | Mchecksignature (k, s, x) -> T.iterop ~loc:mtt.loc Tcheck_signature (f k) (f s) (f x)


    (* crypto functions *)

    | Mtotalvotingpower         -> T.izop  ~loc:mtt.loc Ztotalvotingpower
    | Mvotingpower  x           -> T.iunop ~loc:mtt.loc Uvotingpower (f x)


    (* ticket *)

    | Mcreateticket (x, a)   -> T.ibinop      ~loc:mtt.loc Bcreateticket (f x) (f a)
    | Mreadticket x          -> T.ireadticket ~loc:mtt.loc (f x)
    | Msplitticket (x, a, b) -> T.ibinop      ~loc:mtt.loc Bsplitticket (f x) (mk_tuple [f a; f b])
    | Mjointickets (x, y)    -> T.iunop       ~loc:mtt.loc Ujointickets (mk_tuple [f x; f y])


    (* sapling *)

    | Msapling_empty_state n        -> T.izop   ~loc:mtt.loc (Zsapling_empty_state n)
    | Msapling_verify_update (s, t) -> T.ibinop ~loc:mtt.loc Bsapling_verify_update (f s) (f t)


    (* bls curve *)

    | Mpairing_check x -> T.iunop ~loc:mtt.loc (Upairing_check) (f x)


    (* timelock *)

    | Mopen_chest (x, y, z) -> T.iterop ~loc:mtt.loc (Topen_chest) (f x) (f y) (f z)


    (* constants *)

    | Mnow           -> T.izop ~loc:mtt.loc Znow
    | Mtransferred   -> T.izop ~loc:mtt.loc Zamount
    | Mcaller        -> T.izop ~loc:mtt.loc Zsender
    | Mbalance       -> T.izop ~loc:mtt.loc Zbalance
    | Msource        -> T.izop ~loc:mtt.loc Zsource
    | Mselfaddress   -> T.izop ~loc:mtt.loc Zself_address
    | Mselfchainid   -> T.izop ~loc:mtt.loc Zchain_id
    | Mmetadata      -> emit_error (UnsupportedTerm ("Mmetadata"))
    | Mlevel         -> T.izop ~loc:mtt.loc Zlevel
    | Mminblocktime  -> T.izop ~loc:mtt.loc Zmin_block_time


    (* variable *)

    | Mvar (v, kind) -> begin
        let f =
          if is_ticket_type model mtt.type_
          then fun x -> T.ivar_access  ~loc:mtt.loc {
              av_ident = x;
              av_path = [];
              av_source_no_dup = true;
              av_value_no_dup = true
            }
          else fun x -> T.ivar ~loc:mtt.loc x
        in

        match kind with
        | Vassetstate _k   -> assert false
        | Vstorevar        -> f (M.unloc_mident v)
        | Vstorecol        -> f (M.unloc_mident v)
        | Vlocal           -> f (M.unloc_mident v)
        | Vparam           -> f (M.unloc_mident v)
        | Vfield           -> assert false
        | Vthe             -> assert false
        | Vstate           -> assert false
        | Vparameter       -> T.iwildcard ~loc:mtt.loc (ft mtt.type_) (M.unloc_mident v)
      end
    | Menumval (_id, _args, _e)        -> assert false

    (* rational *)

    | Mrateq (l, r)           -> let b = T.Bratcmp in add_builtin b; T.icall ~loc:mtt.loc (get_fun_name b) [T.isrecord [f l; f r]; T.ileft (T.tor (T.tor T.tunit T.tunit) (T.tor T.tunit T.tunit)) (T.iunit ())] (is_inline b)
    | Mratcmp (op, l, r)   ->
      let op =
        let u    = T.iunit () in
        let tu   = T.tunit in
        let tou  = T.tor tu tu in
        (* let toou = T.tor tou tou in *)
        match op with
        | Lt -> T.iright tu (T.ileft  tou (T.ileft  tu u))
        | Le -> T.iright tu (T.ileft  tou (T.iright tu u))
        | Gt -> T.iright tu (T.iright tou (T.ileft  tu u))
        | Ge -> T.iright tu (T.iright tou (T.iright tu u))
      in
      let b = T.Bratcmp in add_builtin b; T.icall ~loc:mtt.loc (get_fun_name b) [T.isrecord [f l; f r]; op] (is_inline b)
    | Mratarith (op, l, r)    -> begin
        (* let norm x = let b = T.Bratnorm in add_builtin b; T.Icall (get_fun_name b, [x]) in *)
        let norm x = x in
        match op with
        | Rplus  -> let b = T.Brataddsub in add_builtin b; norm (T.icall ~loc:mtt.loc (get_fun_name b) [T.isrecord [f l; f r]; T.ileft  T.tunit (T.iunit ())] (is_inline b))
        | Rminus -> let b = T.Brataddsub in add_builtin b; norm (T.icall ~loc:mtt.loc (get_fun_name b) [T.isrecord [f l; f r]; T.iright T.tunit (T.iunit ())] (is_inline b))
        | Rmult  -> let b = T.Bratmul    in add_builtin b; norm (T.icall ~loc:mtt.loc (get_fun_name b) [f l; f r] (is_inline b))
        | Rdiv   -> let b = T.Bratdiv    in add_builtin b; norm (T.icall ~loc:mtt.loc (get_fun_name b) [f l; f r] (is_inline b))
      end
    | Mratuminus v            -> let b = T.Bratuminus in add_builtin b; T.icall ~loc:mtt.loc (get_fun_name b) [f v] (is_inline b)
    | Mrattez  (c, t)         -> let b = T.Brattez    in add_builtin b; T.icall ~loc:mtt.loc (get_fun_name b) [f c; f t] (is_inline b)
    | Mnattoint e             -> T.iunop ~loc:mtt.loc (Uint) (f e)
    | Mnattorat e             -> T.isrecord ~loc:mtt.loc [T.iunop Uint (f e); T.inat Big_int.unit_big_int]
    | Minttorat e             -> T.isrecord ~loc:mtt.loc [f e; T.inat Big_int.unit_big_int]
    | Mratdur  (c, t)         -> let b = T.Bratdur    in add_builtin b; T.icall ~loc:mtt.loc (get_fun_name b) ([f c; f t]) (is_inline b)


    (* utils *)

    | Minttodate _         -> emit_error (UnsupportedTerm ("Minttodate"))
    | Mmuteztonat v        -> let b = T.Bmuteztonat in add_builtin b; T.icall ~loc:mtt.loc (get_fun_name b) [f v] (is_inline b)

  in

  let storage_list = model.storage |> List.filter (fun (x : M.storage_item) -> not x.no_storage) |> List.map (
      fun (si : M.storage_item) ->
        (M.unloc_mident si.id), to_type model ~annotation:(mk_fannot (M.unloc_mident si.id)) si.typ, to_data si.default)
  in

  let storage_type, storage_data =
    match storage_list with
    | []  -> T.mk_type Tunit, T.Dunit
    | [_, t, d] -> remove_annot t, d
    | _   -> let _, lt, ld = List.split3 storage_list in to_one_type lt, to_one_data ld
  in

  let env = mk_env () in

  let funs, entries, views, offchain_views =

    let for_fs  ?(view= false) _env (fs : M.function_struct) =
      let name = M.unloc_mident fs.name in
      let args = List.map (fun (id, t, _) -> M.unloc_mident id, to_type model t) fs.args in
      let eargs = List.map (fun (id, t, _) -> M.unloc_mident id, to_type model t) fs.eargs in
      let env = {function_p = Some (name, args)} in
      let body = mterm_to_intruction env fs.body ~view in
      name, args, eargs, body
    in

    let mapargs : 'a MapString.t ref = ref MapString.empty in

    let get_extra_args (mt : M.mterm) : (ident * T.type_) list =
      let rec aux accu (mt : M.mterm) : (ident * T.type_) list =
        let doit accu (mt : M.mterm) b : (ident * T.type_) list  =
          if is_inline b
          then (M.fold_term aux accu mt)
          else
            let fu = get_builtin_fun b mt.loc in
            (fu.name, T.tlambda fu.targ fu.tret)::(M.fold_term aux accu mt)
        in

        match mt.node with
        | Mmax _                  -> (doit accu mt (T.Bmax (to_type model mt.type_)))
        | Mmin _                  -> (doit accu mt (T.Bmin (to_type model mt.type_)))
        | Mfloor _                -> (doit accu mt (T.Bfloor))
        | Mceil  _                -> (doit accu mt (T.Bceil))
        | Mlistcontains (t, _, _) -> (doit accu mt (T.BlistContains (to_type model t)))
        | Mlistnth (t, _, _)      -> (doit accu mt (T.BlistNth (to_type model t)))
        | Mlisthead (t, _, _)     -> (doit accu mt (T.BlistHead (to_type model t)))
        | Mlisttail (t, _, _)     -> (doit accu mt (T.BlistTail (to_type model t)))
        | Msimplify_rational _    -> (doit accu mt (T.Bsimplify_rational))

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
          let fid  = M.unloc_mident fid in
          let targs, tret = MapString.find fid !mapargs in
          let eargs = match List.assoc_opt fid !extra_args with None -> [] | Some l -> l in
          (fid, T.tlambda targs tret)::(eargs @ M.fold_term aux accu mt) |> List.dedup

        | _ -> M.fold_term aux accu mt
      in
      aux [] mt |> List.dedup
    in

    let for_fs_fun ?(view : bool = false) env (fs : M.function_struct) ret : T.func =
      let fid = M.unloc_mident fs.name in
      let tret = to_type model ret in
      let name, args, _eargs, body = for_fs env fs ~view in
      let eargs = get_extra_args fs.body in
      extra_args := (fid, eargs)::!extra_args;
      let args = args @ eargs in
      let targ = to_one_type (List.map snd args) in
      let ctx = T.mk_ctx_func () ~args ~stovars:fs.stovars in
      mapargs := MapString.add fid (targ, tret) !mapargs;
      T.mk_func name targ tret ctx (T.Concrete (args, body)) fs.loc
    in

    let for_fs_entry ?(view= false) env (fs : M.function_struct) : T.entry =
      let name, args, eargs, body = for_fs env fs ~view in
      T.mk_entry name args eargs body fs.loc
    in

    let is_onchain_view vv = match vv with  | M.VVonchain  | M.VVonoffchain -> true | _ -> false in
    let is_offchain_view vv = match vv with | M.VVoffchain | M.VVonoffchain -> true | _ -> false in
    List.fold_left (fun (funs, entries, views, offchain_views) (x : M.function_node) ->
        match x with
        | Entry fs -> (funs, entries @ [for_fs_entry env fs ~view:false], views, offchain_views)
        | Getter _ -> emit_error (UnsupportedTerm ("Getter"))
        | Function (_, Void) -> emit_error (UnsupportedTerm ("Void function"))
        | Function (fs, Typed ret) -> funs @ [for_fs_fun env fs ret ~view:false], entries, views, offchain_views
        | View (fs, ret, vv) -> (funs, entries,
                                 (views @ (if is_onchain_view  vv then [for_fs_fun env fs ret ~view:true ] else [])),
                                 (offchain_views @ (if is_offchain_view vv then [for_fs_fun env fs ret ~view:true ] else [])))
      ) ([], [], [], []) model.functions
  in
  let annot a (t : T.type_) = id{ t with annotation = Some (mk_fannot a)} in
  let parameter : T.type_ =
    let for_entry (e : T.entry) =
      let f l =
        match List.rev l with
        | []   -> T.tunit
        | [e]  -> annot (fst e) (snd e)
        | (id, te)::t -> List.fold_left (fun accu (id, te) -> T.mk_type (T.Tpair [annot id te; accu])) (annot id te) t
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
      | T.Tunit, _       -> T.tpair [T.tunit; eargs]
      | _                -> T.tpair [args; eargs]
    in
    let for_entry e = e |> for_entry |> (fun (x : T.type_) -> annot e.name x) in
    entries
    |> List.map for_entry
    |> shape_entrypoints (fun x y -> T.mk_type (T.Tor(x, y))) T.tunit
  in
  let with_operations = M.Utils.with_operations model in

  let loc = dummy in (* TODO: get location *)
  let funs = List.fold_left (fun accu x -> (get_builtin_fun x loc)::accu) funs !builtins in

  let name = unloc model.name in
  let parameters = List.map (fun (x : M.parameter) -> M.unloc_mident x.name) model.parameters in
  T.mk_ir name storage_type storage_data storage_list parameter funs views offchain_views entries ~with_operations:with_operations ~parameters


(* -------------------------------------------------------------------- *)

let data_map_bytes_nat : T.data list = List.int_fold (fun accu idx -> (T.Delt (Dbytes (Format.asprintf "%02x" idx), Dint (Big_int.big_int_of_int idx)))::accu) [] 256 |> List.rev
let data_map_nat_bytes : T.data list = List.int_fold (fun accu idx -> (T.Delt (Dint (Big_int.big_int_of_int idx), Dbytes (Format.asprintf "%02x" idx)))::accu) [] 256 |> List.rev

let map_implem : (string * T.code list) list = [
  get_fun_name (T.Bmin T.tunit)  , T.[cdup (); cunpair (); ccompare (); clt (); cif ([ccar ()], [ccdr ()])];
  get_fun_name (T.Bmax T.tunit)  , T.[cdup (); cunpair (); ccompare (); clt (); cif ([ccdr ()], [ccar ()])];
  get_fun_name T.Bratcmp         , T.[cunpair (); cunpair (); cdip (1, [cunpair ()]); cunpair (); cdug 3; cmul (); cdip (1, [cmul ()]); cswap (); ccompare (); cswap ();
                                      cifleft ([cdrop 1; ceq ()], [cifleft ([cifleft ([cdrop 1; clt ()], [cdrop 1; cle ()])],
                                                                            [cifleft ([cdrop 1; cgt ()], [cdrop 1; cge ()])])])];
  get_fun_name T.Bfloor          , T.[cunpair (); cediv (); cifnone ([cfail M.fail_msg_DIV_BY_ZERO], [ccar ()])];
  get_fun_name T.Bceil           , T.[cunpair (); cediv (); cifnone ([cfail M.fail_msg_DIV_BY_ZERO], [cunpair (); cswap (); cint (); ceq (); cif ([], [cpush (tint, Dint Big_int.unit_big_int); cadd ()])])];
  get_fun_name T.Bratnorm        ,   [];
  get_fun_name T.Brataddsub      , T.[cunpair (); cunpair (); cdip (1, [cunpair (); cswap (); cdup ()]); cunpair (); cswap (); cdup (); cdig 3; cmul (); cdup (); cpush (tnat, Dint Big_int.zero_big_int);
                                      ccompare (); ceq (); cif ([cfail M.fail_msg_DIV_BY_ZERO], []); cdug 4; cdig 3; cmul (); cdip (1, [cmul ()]); cdig 3; cifleft ([cdrop 1; cadd ()], [cdrop 1; cswap (); csub ()]); cpair ();];
  get_fun_name T.Bratmul         , T.[cunpair (); cdip (1, [cunpair ()]); cunpair (); cdip (1, [cswap ()]); cmul ();
                                      cdip (1, [cmul (); cdup (); cpush (tnat, Dint Big_int.zero_big_int); ccompare (); ceq (); cif ([cfail M.fail_msg_DIV_BY_ZERO], [])]); cpair () ];
  get_fun_name T.Bratdiv         , T.[cunpair (); cdip (1, [cunpair ()]); cunpair (); cdig 3;
                                      cdup (); cdig 3; cdup (); cdug 4; cmul ();
                                      cpush (tnat, T.Dint Big_int.zero_big_int); ccompare (); ceq (); cif ([cfail M.fail_msg_DIV_BY_ZERO], []);
                                      cpush (tint, T.Dint Big_int.zero_big_int); cdig 4; cdup (); cdug 5; ccompare (); cge (); cif ([cint ()], [cneg ()]); cmul (); cdip (1, [cmul (); cabs ()]); cpair () ];
  get_fun_name T.Bratuminus      , T.[cunpair (); cneg (); cpair ()];
  get_fun_name T.Bratabs         , T.[cunpair (); cabs (); cint (); cpair ()];
  get_fun_name T.Brattez         , T.[cunpair (); cunpair ();
                                      cdip(2, [cpush (tmutez, T.Dint Big_int.unit_big_int); cswap (); cediv (); cifnone ([T.cfail M.fail_msg_DIV_BY_ZERO], []) ; ccar ()]);
                                      cabs (); cdig 2; cmul (); cediv (); cifnone ([cfail M.fail_msg_DIV_BY_ZERO], []); ccar (); cpush (tmutez, T.Dint Big_int.unit_big_int); cmul ()];
  get_fun_name T.Bratdur         , T.[cunpair (); cunpair (); cdig 2; cmul (); cediv (); cifnone ([cfail M.fail_msg_DIV_BY_ZERO], []); ccar ()];
  get_fun_name T.Bmuteztonat     , T.[cpush (tmutez, T.Dint Big_int.unit_big_int); cswap (); cediv (); cifnone ([T.cfail M.fail_msg_DIV_BY_ZERO], []); ccar ()];

  get_fun_name T.Bsimplify_rational, T.[
      cpush (tunit, T.Dunit);
      cdup_n 2;
      ccar ();
      cdup_n 3;
      ccdr ();
      cdup_n 2;
      cdup_n 2;
      cpush (tnat, T.Dint Big_int.zero_big_int);
      cdup_n 2;
      ccompare ();
      cneq ();
      cloop [
        cdup ();
        cdup_n 2;
        cint ();
        cdup_n 4;
        cediv ();
        cifnone ([cpush (tstring, T.Dstring "DIV_BY_ZERO"); cfailwith ()], [ cdup (); ccdr (); cswap (); cdrop 1]);
        cdip (1, [cdig 1; cdrop 1]);
        cdug 1;
        cdup ();
        cint ();
        cdip (1, [cdig 2; cdrop 1]);
        cdug 2;
        cdrop 1;
        cpush (tnat, T.Dint Big_int.zero_big_int);
        cdup_n 2;
        ccompare ();
        cneq ()
      ];
      cpush (tnat, T.Dint Big_int.unit_big_int);
      cdup_n 3;
      cdup_n 5;
      cint ();
      cediv ();
      cifnone ([cpush (tstring, T.Dstring "DIV_BY_ZERO"); cfailwith ()], [ cdup (); ccar (); cswap (); cdrop 1]);
      cpair ();
      cpush (tnat, T.Dint Big_int.unit_big_int);
      cdup_n 4;
      cdup_n 7;
      cediv ();
      cifnone ([cpush (tstring, T.Dstring "DIV_BY_ZERO"); cfailwith ()], [ cdup (); ccar (); cswap (); cdrop 1]);
      cpair ();
      cpair ();
      cunpair ();
      cdip (1, [cunpair ()]);
      cunpair ();
      cdig 3;
      cdup ();
      cdig 3;
      cdup ();
      cdug 4;
      cmul ();
      cpush (tnat, T.Dint Big_int.zero_big_int);
      ccompare ();
      ceq ();
      cif ([cpush (tstring, T.Dstring "DIV_BY_ZERO"); cfailwith ()], []);
      cpush (tint, T.Dint Big_int.zero_big_int);
      cdig 4;
      cdup ();
      cdug 5;
      ccompare ();
      cge ();
      cif ([cint ()], [cneg ()]);
      cmul ();
      cdip (1, [cmul (); cabs ()]);
      cpair ();
      cdip (1, [cdig 4; cdrop 1]);
      cdug 4;
      cdrop 4;
      cdug 1;
      cdrop 1
    ];
  get_fun_name T.Bis_implicit_address, T.[
      cpack ();
      cpush (tnat, Dint Big_int.unit_big_int);
      cpush (tnat, Dint (Big_int.big_int_of_int 6));
      cslice ();
      cifnone ([cpush (tstring, Dstring "ERROR"); cfailwith ()], []);
      cpush (tbytes, Dbytes ("00"));
      ccompare ();
      ceq ()
    ]
]

let concrete_michelson b : T.code =
  let get_implem b : T.code list = List.assoc (get_fun_name b) map_implem in
  match b with
  | T.Bmin _          -> T.cseq (get_implem (Bmin T.tunit))
  | T.Bmax _          -> T.cseq (get_implem (Bmax T.tunit))
  | T.Bfloor          -> T.cseq (get_implem b)
  | T.Bceil           -> T.cseq (get_implem b)
  | T.BlistContains _ -> T.cseq T.[cunpair (); cfalse (); cswap (); citer [cdig 2; cdup (); cdug 3; ccompare (); ceq (); cor () ]; cdip (1, [cdrop 1])]
  | T.BlistNth t      -> T.cseq T.[cunpair (); cpush (tnat, T.Dint Big_int.zero_big_int); cpush (toption t, T.Dnone); cdig 2;
                                   citer [cdup_n 3; cdup_n 5; ccompare (); ceq (); cif ([csome (); cswap (); cdrop 1], [cdrop 1]); cswap (); cpush (tnat, T.Dint Big_int.unit_big_int); cadd (); cswap ()];
                                   cdip (1, [cdrop 2]) ]
  | T.BlistHead t -> T.cseq T.[
      cunpair ();
      cdup_n 2;
      cdup_n 2;
      csize ();
      cpair ();
      cdup ();
      cunpair ();
      ccompare ();
      clt ();
      cif ([ccar ()], [ccdr ()]);
      cnil t;
      cdup_n 2;
      cint ();
      cpush (tint, Dint Big_int.zero_big_int);
      cdup_n 2;
      cdup_n 2;
      ccompare ();
      clt ();
      cloop [
        cdig 2;
        cdup_n 5;
        cifcons ([], [cpush (tstring, Dstring "ERROR"); cfailwith ()]);
        cswap ();
        cdig 6;
        cdrop 1;
        cdug 5;
        ccons ();
        cdug 2;
        cpush (tint, Dint Big_int.unit_big_int);
        cdup_n 2;
        cadd ();
        cswap ();
        cdrop 1;
        cdup_n 2;
        cdup_n 2;
        ccompare ();
        clt ()
      ];
      cdrop 2;
      cnil t;
      cdig 1;
      citer [ccons ()];
      cdip (1, [cdrop 3])
    ]
  | T.BlistTail t -> T.cseq T.[
      cunpair ();
      cnil t;
      cdig 1;
      citer [ccons ()];
      cdup_n 2;
      cdup_n 2;
      csize ();
      cpair ();
      cdup ();
      cunpair ();
      ccompare ();
      clt ();
      cif ([ccar ()], [ccdr ()]);
      cnil t;
      cdup_n 2;
      cint ();
      cpush (tint, Dint Big_int.zero_big_int);
      cdup_n 2;
      cdup_n 2;
      ccompare ();
      clt ();
      cloop [
        cdig 2;
        cdup_n 5;
        cifcons ([], [cpush (tstring, Dstring "ERROR"); cfailwith ()]);
        cswap ();
        cdig 6;
        cdrop 1;
        cdug 5;
        ccons ();
        cdug 2;
        cpush (tint, Dint Big_int.unit_big_int);
        cdup_n 2;
        cadd ();
        cswap ();
        cdrop 1;
        cdup_n 2;
        cdup_n 2;
        ccompare ();
        clt ()
      ];
      cdrop 2;
      cdip (1, [cdrop 3])
    ]
  | T.Bratcmp         -> T.cseq T.[cunpair (); cunpair (); cdip (1, [cunpair ()]); cunpair (); cdug 3; cmul (); cdip (1, [cmul ()]); cswap (); ccompare (); cswap ();
                                   cifleft ([cdrop 1; ceq ()], [cifleft ([cifleft ([cdrop 1; clt ()], [cdrop 1; cle ()])],
                                                                         [cifleft ([cdrop 1; cgt ()], [cdrop 1; cge ()])])])]
  | T.Bratnorm        -> T.cseq (get_implem b)
  | T.Brataddsub      -> T.cseq (get_implem b)
  | T.Bratmul         -> T.cseq (get_implem b)
  | T.Bratdiv         -> T.cseq (get_implem b)
  | T.Bratuminus      -> T.cseq (get_implem b)
  | T.Bratabs         -> T.cseq (get_implem b)
  | T.Brattez         -> T.cseq (get_implem b)
  | T.Bratdur         -> T.cseq (get_implem b)
  | T.Bmuteztonat     -> T.cseq (get_implem b)
  | T.Bsimplify_rational -> T.cseq (get_implem b)
  | T.Bis_implicit_address -> T.cseq (get_implem b)

type stack_item = {
  id: string;
  populated: bool
}
[@@deriving show {with_path = false}]

type var_location =
  | VLstorage
  | VLargument
  | VLlocal
[@@deriving show {with_path = false}]

type env = {
  stack : stack_item list;
  vars : (string * var_location) list;
  fail : bool;
}
[@@deriving show {with_path = false}]

let process_debug ?decl_bound ?loc (env : env) : T.debug =
  let stack : T.stack_item list = List.map (fun (x : stack_item) ->
      let kind = (match (List.assoc_opt x.id env.vars) with | Some VLstorage -> "storage" | Some VLargument -> "argument" | Some VLlocal | None -> "local") in
      T.{stack_item_name = x.id; stack_item_kind = kind; stack_item_type = None} ) env.stack in
  let res : T.debug = { decl_bound; stack; loc } in
  res

let rec assign_last_seq (debug : T.debug) (cs : T.code list) =
  match List.rev cs with
  | a::l -> begin
      match a.node with
      | T.SEQ ll -> List.rev ({a with node = T.SEQ (assign_last_seq debug ll)}::l)
      | _ -> List.rev ({a with debug = Some debug}::l)
    end
  | [] -> []

let mk_env ?(stack=[]) ?(vars=[]) () = { stack = stack; vars = vars; fail = false; }
let fail_env (env : env) = { env with fail = true }
let inc_env (env : env) = { env with stack = { id = "_"; populated = true}::env.stack }
let dec_env (env : env) = { env with stack = match env.stack with | _::t -> t | _ -> emit_error StackEmptyDec }
let add_var_env (env : env) id = { env with stack = { id = id; populated= true}::env.stack }
let get_sp_for_id (env : env) id =
  let rec aux accu l =
    match l with
    | x::_ when String.equal x.id id -> if x.populated then accu else (emit_error (StackIdNotPopulated (id, List.map (fun x -> x.id) env.stack)))
    | x::tl -> aux (accu + (if x.populated then 1 else 0)) tl
    | [] -> emit_error (StackIdNotFound (id, List.map (fun x -> x.id) env.stack))
  in
  aux 0 env.stack
let index_of_id_in_env (env : env) id =
  let rec aux accu l =
    match l with
    | x::_ when String.equal x.id id -> if x.populated then accu else -1
    | x::tl -> aux (accu + (if x.populated then 1 else 0)) tl
    | [] -> -1
  in
  aux 0 env.stack
let rm_var_env (env : env) id =
  let nstack =
    let rec aux accu a =
      match a with
      | x::tl when String.equal id x.id -> accu @ tl
      | x::tl -> aux (accu @ [x]) tl
      | [] -> assert false
    in
    aux [] env.stack
  in
  { env with stack = nstack }

let populate_env (env : env) id =
  let nstack =
    let rec aux accu a =
      match a with
      | x::tl when String.equal id x.id -> accu @ [{x with populated = true }] @ tl
      | x::tl -> aux (accu @ [x]) tl
      | [] -> assert false
    in
    aux [] env.stack
  in
  { env with stack = nstack }

let dig_env env id =
  let nstack =
    let rec aux accu a =
      match a with
      | x::tl when String.equal id x.id -> [{id = "_"; populated = true}] @ accu @ [{ x with populated = false }] @ tl
      | x::tl -> aux (accu @ [x]) tl
      | [] -> assert false
    in
    aux [] env.stack
  in
  { env with stack = nstack }

let dug_env env id =
  let nstack =
    let rec aux accu a =
      match a with
      | x::tl when String.equal id x.id -> (match accu with _::tl -> tl | [] -> assert false) @ [{ x with populated = true }] @ tl
      | x::tl -> aux (accu @ [x]) tl
      | [] -> assert false
    in
    aux [] env.stack
  in
  { env with stack = nstack }

let get_pos_stack_item env id =
  let b = List.exists (fun x -> String.equal x.id id) env.stack in
  if (not b) then assert false;
  let rec aux accu l =
    match l with
    | x::_ when String.equal x.id id -> accu, x
    | x::tl -> aux (accu + (if x.populated then 1 else 0)) tl
    | _ -> assert false
  in
  aux 0 env.stack

let print_env ?(str="") env =
  Format.eprintf "%s: %a@." str pp_env env
(* Format.eprintf "var %s: %i@." id n; *)

let print_code ?(str="") (code : T.code) =
  Format.eprintf "%s: %a@." str Printer_michelson.pp_code code

let rec instruction_to_code env (i : T.instruction) : T.code * env =
  let fe env = instruction_to_code env in
  let f = fe env in

  let get_remove_code env id =
    let n, si = get_pos_stack_item env id in
    match n with
    | -1 -> []
    | 0  -> ((if si.populated then [T.cdrop 1] else []))
    | 1  -> ((if si.populated then [T.cswap (); T.cdrop 1 ] else []))
    | _  -> ((if si.populated then [T.cdip (n, [T.cdrop 1])] else []))
  in

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
            let v, env = g env x in (T.cseq [a; v; T.cpair ()], dec_env env)
          end ) (g env e) t
  in

  let fold env l = fold_gen fe env l in

  let assign ?loc env id v =
    (* let n = get_sp_for_id env id in *)
    let n, si = get_pos_stack_item env id in
    if si.populated
    then begin
      (* print_env ~str:("assign:before " ^ id) env; *)
      (* Format.eprintf "assign n: %d@." n; *)
      let cs, nenv =
        match n with
        | 0 -> [ v ], env
        | 1 -> [ v; T.cswap (); T.cdrop 1 ], dec_env env
        | _ -> [ v; (T.cdip (1, [T.cdig (n - 1); T.cdrop 1])); T.cdug (n - 1)], dec_env env
      in
      let debug = process_debug ?loc nenv in
      let cs = assign_last_seq debug cs in
      (* print_env ~str:("assign:after " ^ id) nenv; *)
      T.cseq cs, nenv
    end
    else begin
      (* print_env ~str:("assign:before " ^ id) env; *)
      let nenv = dug_env env id in
      (* print_env ~str:("assign:after " ^ id) nenv; *)
      T.cseq [ v; T.cdug (n - 1)], nenv
    end
  in

  let z_op_to_code = function
    | T.Znow                   -> T.cnow ()
    | T.Zamount                -> T.camount ()
    | T.Zbalance               -> T.cbalance ()
    | T.Zsource                -> T.csource ()
    | T.Zsender                -> T.csender ()
    | T.Zaddress               -> T.caddress ()
    | T.Zchain_id              -> T.cchain_id ()
    | T.Zself a                -> T.cself a
    | T.Zself_address          -> T.cself_address ()
    | T.Znone t                -> T.cnone (rar t)
    | T.Zunit                  -> T.cunit ()
    | T.Znil t                 -> T.cnil (rar t)
    | T.Zemptyset t            -> T.cempty_set (rar t)
    | T.Zemptymap (k, v)       -> T.cempty_map (rar k, rar v)
    | T.Zemptybigmap (k, v)    -> T.cempty_big_map (rar k, rar v)
    | T.Ztotalvotingpower      -> T.ctotal_voting_power ()
    | T.Zlevel                 -> T.clevel ()
    | T.Zmin_block_time        -> T.cmin_block_time ()
    | T.Zsapling_empty_state n -> T.csapling_empty_state n
  in

  let un_op_to_code = function
    | T.Ucar             -> T.ccar ()
    | T.Ucdr             -> T.ccdr ()
    | T.Uleft t          -> T.cleft (rar t)
    | T.Uright t         -> T.cright (rar t)
    | T.Uneg             -> T.cneg ()
    | T.Unat             -> T.cnat ()
    | T.Uint             -> T.cint ()
    | T.Ubytes           -> T.cbytes ()
    | T.Unot             -> T.cnot ()
    | T.Uabs             -> T.cabs ()
    | T.Uisnat           -> T.cisnat ()
    | T.Usome            -> T.csome ()
    | T.Usize            -> T.csize ()
    | T.Upack            -> T.cpack ()
    | T.Uunpack        t -> T.cunpack (rar t)
    | T.Ublake2b         -> T.cblake2b ()
    | T.Usha256          -> T.csha256 ()
    | T.Usha512          -> T.csha512 ()
    | T.Usha3            -> T.csha3 ()
    | T.Ukeccak          -> T.ckeccak ()
    | T.Uhash_key        -> T.chash_key ()
    | T.Ufail            -> T.cfailwith ()
    | T.Ucontract (t, a) -> T.ccontract (rar t, a)
    | T.Usetdelegate     -> T.cset_delegate ()
    | T.Uimplicitaccount -> T.cimplicit_account ()
    | T.Ueq              -> T.ceq ()
    | T.Une              -> T.cneq ()
    | T.Ugt              -> T.cgt ()
    | T.Uge              -> T.cge ()
    | T.Ult              -> T.clt ()
    | T.Ule              -> T.cle ()
    | T.Uvotingpower     -> T.cvoting_power ()
    | T.Ureadticket      -> T.cread_ticket ()
    | T.Ujointickets     -> T.cjoin_tickets ()
    | T.Upairing_check   -> T.cpairing_check ()
    | T.Uconcat          -> T.cconcat ()
    | T.Uaddress         -> T.caddress ()
    | T.UcarN n          -> T.ccarn n
    | T.UcdrN n          -> T.ccdrn n
    | T.UforcePair       -> T.cpair ()
    | T.Uemit (t, id)    -> T.cemit (t, id)
  in

  let bin_op_to_code = function
    | T.Badd                   -> T.cadd ()
    | T.Bsub                   -> T.csub ()
    | T.Bmul                   -> T.cmul ()
    | T.Bediv                  -> T.cediv ()
    | T.Blsl                   -> T.clsl ()
    | T.Blsr                   -> T.clsr ()
    | T.Bor                    -> T.cor ()
    | T.Band                   -> T.cand ()
    | T.Bxor                   -> T.cxor ()
    | T.Bcompare               -> T.ccompare ()
    | T.Bget                   -> T.cget ()
    | T.Bmem                   -> T.cmem ()
    | T.Bconcat                -> T.cconcat ()
    | T.Bcons                  -> T.ccons ()
    | T.Bpair                  -> T.cpair ()
    | T.Bexec                  -> T.cexec ()
    | T.Bapply                 -> T.capply ()
    | T.Bcreateticket          -> T.cticket ()
    | T.Bsplitticket           -> T.csplit_ticket ()
    | T.Bsapling_verify_update -> T.csapling_verify_update ()
    | T.Bview (c, t)           -> T.cview (c, t)
    | T.Bsubmutez              -> T.csub_mutez ()
  in

  let ter_op_to_code = function
    | T.Tcheck_signature   -> T.ccheck_signature ()
    | T.Tslice             -> T.cslice ()
    | T.Tupdate            -> T.cupdate ()
    | T.Ttransfer_tokens   -> T.ctransfer_tokens ()
    | T.Topen_chest        -> T.copen_chest ()
    | T.Tcreate_contract c -> T.ccreate_contract c
  in

  match i.node with
  | Iseq l               -> seq env l

  | IletIn (ids, v, b, _u)    -> begin
      let n = List.length ids in
      let v, env = f v in
      (* Format.eprintf "%a@\n" (pp_list "," pp_str) ids; *)
      let v = if n <= 1 then v else T.cseq [v; T.cunpair_n n] in
      let env = List.fold_right (fun x env -> add_var_env env x) ids (dec_env env) in
      let debug = process_debug ?loc:i.loc env in
      let v = {v with debug = Some debug} in
      (* print_env ~str:("IletIn " ^ id ^ " before") env; *)
      let b, env = fe env b in
      (* print_env ~str:("IletIn " ^ id ^ " after") env; *)
      let cs, nenv =
        List.fold_left (fun (b, env) id -> begin
              let idx, si = get_pos_stack_item env id in
              let env = rm_var_env env id in
              (* Format.eprintf "IletIn: id:%s idx:%d si:%a " id idx pp_stack_item si; *)
              let c =
                match idx with
                | -1 -> b
                | 0  -> b @ (if si.populated then [T.cdrop 1] else [])
                | 1  -> b @ (if si.populated then [T.cswap (); T.cdrop 1 ] else [])
                | _  -> b @ (if si.populated then [T.cdip (idx, [T.cdrop 1])] else [])
              in
              (* print_env ~str:("IletIn " ^ id ^ " final") nenv; *)
              c, env
            end ) ([v; b], env) ids
      in
      T.cseq cs, nenv
    end

  | Ivar_access av -> begin
      let n = get_sp_for_id env av.av_ident in
      let compute_path_n (ai : T.access_item) =
        if ai.ai_index == ai.ai_length - 1
        then ai.ai_index * 2
        else (1 + ai.ai_index * 2)
      in
      if av.av_source_no_dup
      then begin
        if av.av_value_no_dup
        then begin
          let p = List.map (fun ai -> T.cget_n (compute_path_n ai)) av.av_path in
          let nenv = dig_env env av.av_ident |> (if List.is_empty p then (fun x -> x) else (fun x -> x |> dec_env |> inc_env)) in
          T.cseq ((if n == 0 then [] else [T.cdig n]) @ p), nenv
        end
        else begin
          let c =
            if n = 0
            then []
            else [T.cdig n] in
          let d =
            if n = 0
            then []
            else [T.cdug n] in
          let p = List.map (fun (ai : T.access_item) ->
              ((T.cunpair_n ai.ai_length)::[T.cdig (ai.ai_index)])) av.av_path |> List.flatten in
          let r = List.map (fun (ai : T.access_item) ->
              ([T.cdug (ai.ai_index)] @ [T.cpair_n ai.ai_length])) (List.rev av.av_path) |> List.flatten in
          let nenv = inc_env env in
          (* print_env ~str:"Ivar_access" nenv; *)
          (T.cseq (c @ p @ [T.cdup ()] @ [T.cdip (1, r @ d)])), nenv
        end
      end
      else begin
        let c =
          if n = 0
          then T.cdup ()
          else T.cdup_n (n + 1)
        in
        let p = List.map (fun ai -> T.cget_n (compute_path_n ai)) av.av_path in
        (T.cseq (c::p)), inc_env env
      end
    end

  | Icall (id, args, inline)   -> begin
      let get_args env =
        match args with
        | [] -> T.cunit (), inc_env env
        | _ -> fold env args
      in
      match inline, List.assoc_opt id map_implem with
      | true, Some body_fun ->
        let cargs, env = get_args env in
        T.cseq (cargs::body_fun), env
      | _ -> begin
          let fid, env   = fe env (T.ivar id) in
          let cargs, env = get_args env in
          T.cseq [fid; cargs; T.cexec ()], dec_env env
        end
    end

  | Iassign (id, v)  -> begin
      (* print_env ~str:("Iassign:before " ^ id) env; *)
      let v, env = f v in
      (* print_env ~str:("Iassign:after " ^ id) env; *)
      assign ?loc:i.loc env id v
    end

  | Iassigntuple (id, i, l, v) -> begin
      let fid, env = f (T.ivar id) in
      let v, env = fe env v in
      let n = if i = l - 1 then i * 2 else i * 2 + 1 in
      assign (dec_env env) id (T.cseq [fid; v; T.cupdate_n n])
    end

  | Iif (c, t, e, _ty) -> begin
      let c, env0 = fe env c in
      let t, envt = fe (dec_env env0) t in
      let e, enve = fe (dec_env env0) e in

      let env =
        match envt.fail, enve.fail with
        | true, true  -> {envt with fail = true}
        | true, _     -> enve
        | _, true     -> envt
        | _           -> enve
      in
      T.cseq [ c; T.cif ([t], [e]) ], env
    end

  | Iifnone (v, t, id, s, _ty) -> begin
      let v, env0 = fe env v in
      let t, env_none = fe (dec_env env0) t in
      let e, env_some = fe (add_var_env (dec_env env0) id) s in

      let rm = get_remove_code env_some id in
      let nenv =
        match env_none.fail, env_some.fail with
        | true, true  -> {env_none with fail = true}
        | true, _     -> rm_var_env env_some id
        | _, true     -> env_none
        | _           -> rm_var_env env_some id
      in
      T.cseq [ v; T.cifnone ([t], [e] @ rm) ], nenv
    end

  | Iifleft (v, lid, le, rid, re, _ty) -> begin
      let v, env0 = fe env v in
      let l, env_left = fe (add_var_env (dec_env env0) lid) le in
      let r, env_right = fe (add_var_env (dec_env env0) rid) re in

      let rm_left = get_remove_code env_left lid in
      let rm_right = get_remove_code env_right rid in

      let nenv =
        match env_left.fail, env_right.fail with
        | true, true  -> {env_left with fail = true}
        | true, _     -> rm_var_env env_right rid
        | _, true     -> rm_var_env env_left lid
        | _           -> rm_var_env env_right rid
      in

      T.cseq [ v; T.cifleft ([l] @ rm_left, [r] @ rm_right) ], nenv
    end

  | Iifcons (x, hd, tl, hte, ne, _ty) -> begin
      let x, env0 = fe env x in
      let t, env_cons = fe (add_var_env (add_var_env (dec_env env0) tl) hd) hte in
      let n, env_empty = fe (dec_env env0) ne in

      let rm_hd = get_remove_code env_cons hd in
      let rm_tl = get_remove_code (rm_var_env env_cons hd) tl in

      let nenv =
        match env_cons.fail, env_empty.fail with
        | true, true  -> {env_empty with fail = true}
        | true, _     -> env_empty
        | _, true     -> rm_var_env (rm_var_env env_cons hd) tl
        | _           -> rm_var_env (rm_var_env env_cons hd) tl
      in

      T.cseq T.[ x; cifcons ([t] @ rm_hd @ rm_tl, [n]) ], nenv
    end

  | Iloop (c, b) -> begin
      let c, env0 = fe env c in
      let b, _ = fe (dec_env env0) b in
      T.cseq T.[c; cloop [b; c]], (dec_env env0)
    end

  | Iiter (ids, c, b) -> begin
      let c, env = f c in
      match ids with
      | [id] -> begin
          let nenv = dec_env env in
          let env_body = add_var_env nenv id in
          let b, env_body2 = fe env_body b in
          let _, si = get_pos_stack_item env_body2 id in
          T.cseq T.[c; citer ([b] @ (if si.populated then [cdrop 1] else []))], nenv
        end
      | [k; v] -> begin
          let nenv = dec_env env in
          let env_body = add_var_env (add_var_env nenv v) k in
          let b, env_body2 = fe env_body b in
          let _, si_k = get_pos_stack_item env_body2 k in
          let _, si_v = get_pos_stack_item env_body2 v in
          T.cseq T.[c; citer ([cunpair (); b] @ (if si_k.populated then [cdrop 1] else []) @ (if si_v.populated then [cdrop 1] else []))], nenv
        end
      | _ -> assert false
    end

  | Iloopleft (l, i, b) -> begin
      let l, env0 = f l in
      let b, _ = fe (add_var_env (dec_env env0) i) b in

      T.cseq T.[l; cloop_left [b; cswap (); cdrop 1]], inc_env (dec_env env0)
    end

  | Ilambda (rt, id, at, e) -> begin
      let e, env = fe (add_var_env env id) e in
      let rm = get_remove_code env id in
      let nenv = rm_var_env env id in

      T.clambda (rt, at, ([e] @ rm)), nenv
    end

  | Ilambda_michelson (it, rt, body) -> begin
      let body = T.remove_seq_obj_micheline body in
      T.clambda (it, rt, (List.map T.ccustom body)), inc_env env
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
      let aop_to_code env = function
        | T.Aunop   op       ->
          let op = un_op_to_code op in
          [op], env
        | T.Abinop (op, a) ->
          let op = bin_op_to_code op in
          let a, env = fe env a in
          [a; op], dec_env env
        | T.Aterop (op, a1, a2) ->
          let op = ter_op_to_code op in
          let a2, env = fe env a2 in
          let a1, env = fe env a1 in
          [a2; a1; op], dec_env (dec_env env)
      in
      match ku with
      | Uvar id -> begin
          let n = get_sp_for_id env id in
          if n <= 0
          then begin
            let a, env = aop_to_code env aop in
            T.cseq a, env
          end
          else begin
            let tenv = dig_env env id in
            let c, ntenv = aop_to_code tenv aop in
            (* print_env ~str:"ntenv" ntenv; *)
            let nn = get_pos_stack_item (dec_env ntenv) id |> fst in
            (* Format.eprintf "id=%s nn=%d\n" id nn; *)
            let nenv = dug_env ntenv id in
            T.cseq ([ T.cdig n ] @ c @ [ T.cdug nn ]), nenv
          end
        end
      | Urec (id, l) -> begin
          let rec g env l x =
            (* Format.eprintf "x: %i@\n" x; *)
            match l with
            | [] -> assert false
            | (k, s)::q -> begin
                (* Format.eprintf "k, s: (%i, %i)@\n" k s; *)
                let unpair x = [T.cunpair ()] @ x @ [T.cpair ()] in
                let swap   x = [T.cswap ()]   @ x @ [T.cswap ()] in
                let h env =
                  match q with
                  | [] -> aop_to_code env aop |> fst
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

          let n = get_sp_for_id env id in
          if n <= 0
          then begin
            T.cseq (g env l 0), env
          end
          else begin
            let tenv = dig_env env id in
            let c = g tenv l 0 in
            let nn = get_pos_stack_item (dec_env tenv) id |> fst in
            T.cseq ([ T.cdig n ] @ c @ [ T.cdug nn ]), env
          end
        end
    end

  | Iconst (t, e) -> begin
      let nenv = inc_env env in
      let debug = process_debug ?loc:i.loc nenv in
      T.cpush ~debug (rar t, e), nenv
    end

  | Icompare (op, lhs, rhs) -> begin
      let op =
        match op with
        | Ceq -> T.ceq ()
        | Cne -> T.cneq ()
        | Clt -> T.clt ()
        | Cle -> T.cle ()
        | Cgt -> T.cgt ()
        | Cge -> T.cge ()
      in
      let r, env = fe env rhs in
      let l, env = fe env lhs in
      T.cseq [r; l; T.ccompare (); op], dec_env env
    end

  | Iset (t, l) -> begin
      let l, nenv =
        List.fold_right (fun x (l, env) -> let x, env = fe (inc_env env) x in ((T.cseq [ T.ctrue (); x; T.cupdate () ])::l, dec_env (dec_env env))) (List.rev l) ([], inc_env env)
      in
      (T.cseq ((T.cempty_set t)::l), nenv)
    end
  | Ilist (t, l) -> begin
      let l, nenv =
        List.fold_right (fun x (l, env) -> let x, env = fe env x in ((T.cseq [ x; T.ccons () ])::l, dec_env env)) (List.rev l) ([], inc_env env)
      in
      (T.cseq ((T.cnil t)::l), nenv)
    end
  | Imap (b, k, v, l) -> begin
      let a = if b then T.cempty_big_map (k, v) else T.cempty_map (k, v) in
      T.cseq ([a] @
              (l
               |> List.rev
               |> List.map (fun (x, y) ->
                   let y, _ = fe (inc_env env) y in
                   let x, _ = fe (inc_env (inc_env env)) x in
                   T.cseq [y; T.csome (); x; T.cupdate () ] ))), inc_env env
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
                then [T.cswap () ] @ g (l, env) (x + 1) @ [T.cswap ()]
                else [T.cswap (); T.cunpair () ] @ g (l, inc_env env) (x + 1) @ [T.cpair (); T.cswap ()]
              end
            end
        in
        let a = g (l, env) 0 in
        let b =
          if s = 1
          then a
          else [T.cunpair ()] @ a @ [T.cpair ()]
        in
        T.cseq b, env
      in
      let rec aux env ru =
        match ru with
        | T.RUassign (s, l) -> assign env s l (fun env v s ->
            let env = if s = 1 then dec_env env else env in
            let v, _ = fe env v in
            T.[cdrop 1; v]
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
        | Some iy -> add_var_env (add_var_env env_ iy) ix, T.cunpair (), 2
        | None -> add_var_env env_ ix, T.cskip (), 1
      in
      let b, _env2 = fe env2 b in
      T.cseq [a; c; T.citer [pi; b; T.cdrop n]], inc_env env
    end

  | Imap_ (x, id, e) -> begin
      let x, _env0 = fe env x in
      let e, env_body = fe (add_var_env env id) e in
      let rm = get_remove_code env_body id in
      T.cseq [x; T.cmap (e::rm)], inc_env env
    end

  | Ireverse (t, x) -> begin
      let x, env = fe (inc_env env) x in
      T.cseq [T.cnil t; x; T.citer [T.ccons ()]], dec_env env
    end

  | Imichelson (a, c, v) -> begin
      let a, _ = seq env a in
      T.cseq [a; c], { env with stack = (List.map (fun id -> {id = id; populated = true}) v) @ env.stack }
    end

  | Iwildcard (ty, id) -> begin
      let id = T.id_to_const_id id in
      let data : T.data = T.Dvar(id, ty, true) in
      T.cpush (ty, data), inc_env env
    end

  | Ireplace (id, v, k, fa) -> begin
      (* print_env ~str:"Ireplace before" env; *)
      let n = get_sp_for_id env v in
      let a, _ = fe (inc_env env) fa in
      let nenv = add_var_env env id in
      (* print_env ~str:"Ireplace after" nenv; *)
      let b =
        match k with
        | KLVoption ty -> T.[cifnone ([a; cfailwith ()], [cnone ty; cswap ()])]
        | KLVmap (ty, k) -> begin
            let k, _ = fe (inc_env env) k in
            T.[cnone ty; k; cget_and_update (); T.cifnone ([cpush (T.tstring, (Dstring M.fail_msg_KEY_NOT_FOUND)); cfailwith ()], [])]
          end
      in
      let nenv = populate_env nenv v in
      T.cseq ([T.cdig n] @ b @ [(if n = 0 then T.cseq [] else T.cdip (1, [T.cdug n]))]), nenv
    end

  | Ireadticket x -> begin
      (* print_env ~str:"Ireadticket" env; *)
      match x.node with
      | T.Ivar_access v -> begin
          let n = get_sp_for_id env v.av_ident in
          let ccdig = if n == 0 then [] else [T.cdig n] in
          let ccdug = if n == 0 then [] else [T.cdug n] in
          let p = List.map (fun (ai : T.access_item) ->
              ((T.cunpair_n ai.ai_length)::[T.cdig (ai.ai_index)])) v.av_path |> List.flatten in
          let r = List.map (fun (ai : T.access_item) ->
              ([T.cdug (ai.ai_index)] @ [T.cpair_n ai.ai_length])) (List.rev v.av_path) |> List.flatten in

          T.cseq (ccdig @ p @ T.[cread_ticket ()] @ [T.cdip(1, r @ ccdug)]), inc_env env
        end
      | _ -> begin
          let v, env = f x in
          T.cseq T.[v; cread_ticket (); cswap (); cdrop 1], inc_env env
        end
    end

  | Imicheline (micheline, ts, args) -> begin
      let env, l = List.fold_left (fun (env, accu) x -> begin
            let x, env = fe env x in
            (env, accu @ [x])
          end) (env, []) args in
      let env = List.fold_left (fun env _x -> dec_env env) env args in
      let env = List.fold_left (fun env _x -> inc_env env) env ts in
      T.cseq (l @ (List.map T.ccustom (T.remove_seq_obj_micheline micheline))), env
    end

  | Irep (id, klv) -> begin
      let n = get_sp_for_id env id in

      let seq =
        match klv with
        | KLVoption ty -> [T.cdig n; T.cnone ty; T.cdug (n + 1)]
        | KLVmap (ty, k) ->
          let k, _ = fe (inc_env (dig_env env id)) k in
          [T.cdig n; T.cnone ty; k; T.cget_and_update (); T.cswap (); T.cdug (n + 1)]
      in
      T.cseq seq, inc_env env
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

and build_view storage_list (v : T.func) : T.view_struct =
  let id    = v.name in
  let param : T.type_ = v.targ in
  let ret   : T.type_ = v.tret in

  let unfold_all = function
    | []     -> []
    | [_]    -> []
    | [_; _] -> [T.cunpair ()]
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
          | false, false -> doit (n + 1) s (code @ [T.ccdr ()], ids) stos
          | false, true  -> code @ [T.cdrop 1], ids
          | true, false  -> doit (n + 1) s (code @ [T.cunpair (); T.cswap ()], id::ids) q
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
      let code = [T.ccar ()] @ unfold_all args in
      let env = { env with
                  vars = (List.map (fun x -> (fst x, VLargument))) args;
                  stack = (List.map (fun x -> {id = fst x; populated = true}) args) @ env.stack } in
      code, env, List.length args

    | [], stovars ->
      let scode, svs = extract_storage_vars stovars in
      (* Format.eprintf "RES: scode: @[%a@], sys: [%a]@\n" (pp_list "; " Printer_michelson.pp_code) scode (pp_list "; " pp_ident) svs; *)
      let code = [T.ccdr ()] @ scode in
      let env = { env with
                  vars = (List.map (fun x -> (x, VLstorage))) stovars;
                  stack = (List.map (fun x -> {id = x; populated = true}) svs) @ env.stack } in
      code, env, List.length svs

    | args, stovars ->
      let scode, svs = extract_storage_vars stovars in
      let acode = unfold_all args in
      let scode = match scode with | [] -> [] | _ -> [T.cdip (1, scode)] in
      let code = [T.cunpair ()] @ scode @ acode in
      let avs = List.map fst args in
      let env = { env with
                  vars = ((List.map (fun x -> (x, VLargument))) avs) @ ((List.map (fun x -> (x, VLstorage))) svs);
                  stack = (List.map (fun x -> {id = x; populated = true}) (avs @ svs)) @ env.stack } in
      code, env, List.length (svs @ avs)

  in

  let fold_vars = fold_vars @ [T.cunit ()] in

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

and to_michelson (ir : T.ir) : T.michelson =
  let storage = ir.storage_type in
  (* let default = T.cseq [T.CDR; T.NIL (T.mk_type Toperation); T.PAIR ] in *)

  let build_code _ =

    let unfold = foldi (fun x -> T.cunpair ()::T.cswap ()::x ) [] in

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
                let env = mk_env ~vars:(args |> List.map (fun x -> (fst x, VLargument))) ~stack:(args |> List.map (fun x -> {id = fst x; populated = true})) () in
                let nb_args = List.length args in
                (* let nb_as = nb_args - 1 in *)
                let unfold_args = unfold_n nb_args in
                let res = T.cpush (T.tunit, T.Dunit) in
                let env = add_var_env env fun_result in
                let es = if nb_args = 0 then T.[cswap (); cdrop 1] else T.[cdug nb_args; T.cdrop nb_args] in
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

    let stack           = (let l = ir.storage_list in if List.is_empty l then [{id = "_"; populated = true}] else List.map (fun (x, _, _) -> {id = x; populated = true}) l) @ (List.map (fun x -> {id = x; populated = true}) ops_var) @ (List.rev (List.map (fun x -> {id = x; populated = true}) funids)) in
    let vars            = (List.map (fun x -> (Tools.proj3_1 x, VLstorage)) ir.storage_list) in
    let env             = mk_env () ~vars ~stack in
    let nb_storage_item = List.length ir.storage_list in
    let unfold_storage  = unfold_n nb_storage_item  in
    let fold_storage    = fold_n nb_storage_item in


    let for_entry (e : T.entry) =
      (* stack state : functions, (operations list)?, unfolded storage, extra_argument? , argument *)
      match e.args, e.eargs with
      | [], [] -> begin
          let debug_begin = process_debug ~decl_bound:{db_kind = "entry"; db_name= e.name; db_bound = "begin"} env in
          let code, env = instruction_to_code env e.body in
          let debug_end = process_debug ~decl_bound:{db_kind = "entry"; db_name= e.name; db_bound = "end"} env in
          let seq_code = [code] in
          let seq_code = assign_last_seq debug_end seq_code in
          T.cseq ([T.cdrop ~debug:debug_begin 1] @ seq_code @ fold_storage @ eops @ [T.cpair ()])
        end
      | l, m -> begin
          let nb_eargs = List.length m in
          let eargs = List.map fst m |> List.rev in
          let unfold_eargs =
            if List.length m > 0
            then if List.length l = 0
              then [T.cunpair (); T.cdrop 1]
              else [T.cunpair ()]
            else []
          in

          let nb_args = List.length l in
          let nb_as = nb_args - 1 in
          let unfold_args = unfold nb_as in
          let args = List.map fst l |> List.rev in
          let env = { env with vars = env.vars @ (List.map (fun x -> (x, VLargument)) args) ; stack = (List.map (fun x -> {id = x; populated = true}) (args @ eargs)) @ env.stack } in
          let code, nenv = instruction_to_code env e.body in
          let diff = List.count (fun x -> not x.populated) nenv.stack in
          (* Format.eprintf "diff: %n\n" diff; *)
          T.cseq (unfold_eargs @ unfold_args @ [code] @ [T.cdrop (nb_args + nb_eargs - diff)] @ fold_storage @ eops @ [T.cpair ()])
        end
    in

    let code =
      match ir.entries with
      | [] -> T.[ccdr (); cnil (T.toperation); cpair ()]
      | entries ->
        let us = if nb_storage_item > 1 then [T.cdip (1, unfold_storage)] else [] in
        let e =
          entries
          |> List.map for_entry
          |> shape_entrypoints (fun x y -> T.cifleft ([x], [y])) (T.cseq [])
        in
        [T.cunpair ()] @ us @ [e]
    in
    T.cseq (cfuns @ ops @ fff @ code @ eee)
    |> T.Utils.flat
    |> T.Utils.optim
  in

  let code = build_code () in
  let parameters = ir.parameters in
  let views = List.map (build_view ir.storage_list) ir.views in
  T.mk_michelson ~parameters ~views storage ir.parameter code

and generate_offchain_view (ir : T.ir) : T.offchain_view list =
  let mk (view_struct : T.view_struct) : T.offchain_view_implem_kind =
    let res : T.michelson_storage_view_struct = {
      code = T.Utils.code_to_micheline view_struct.body;
      parameter = Some (T.Utils.type_to_micheline view_struct.param);
      returnType = Some (T.Utils.type_to_micheline view_struct.ret);
      annotations = [];
      version = None;
    }
    in T.OVIKMichelsonStorageView res
  in
  let mk_offchain_view (vs : T.view_struct) : T.offchain_view = T.{ name = vs.id; implementations = [mk vs] } in
  let l = List.map (build_view ir.storage_list) ir.offchain_views in
  List.map mk_offchain_view l
