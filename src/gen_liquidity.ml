(* open Tools *)
open Ident
open Location

module M = Model
module T = Mltree

exception Anomaly of string

type error_desc =
  | UnsupportedDefaultValue of string
  | CannotConvertLogicalOperator of string
  | UnsupportedTranslationOf of string
  | NoStorageRecordFound
  | UnsupportedFormulaType
  | UnsupportedFormulaExpression
  | NoDefaultValue of ident
  | TODO
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

let btyp_to_basic = function
  | M.Bbool         -> T.Tbool
  | M.Bint          -> T.Tint
  | M.Buint         -> T.Tnat
  | M.Brational     -> emit_error TODO
  | M.Bdate         -> T.Ttimestamp
  | M.Bduration     -> T.Tint
  | M.Bstring       -> T.Tstring
  | M.Baddress      -> T.Taddress
  | M.Brole         -> T.Taddress
  | M.Bcurrency _   -> T.Ttez
  | M.Bkey          -> T.Tkey
[@@deriving show {with_path = false}]

let rec to_type = function
  | M.Tasset id
  | M.Tenum id
  | M.Tcontract id      -> T.Tlocal (unloc id)
  | M.Tbuiltin bt       -> T.Tbasic (btyp_to_basic bt)
  | M.Tcontainer (t, _) -> T.Tlist (to_type t)
  | M.Toption t         -> T.Toption (to_type t)
  | M.Ttuple l          -> T.Ttuple (List.map to_type l)
  | M.Tunit             -> T.Tbasic Tunit
  | M.Tentry
  | M.Tprog _
  | M.Tvset _
  | M.Ttrace _ -> emit_error UnsupportedFormulaType

let type_storage  = T.Tlocal "storage"
let current_id id = T.Eapp (T.Edot (T.Evar "Current", id), [])

let todo : T.expr = T.Evar "todo"

let pattern_to_pattern (p : M.pattern) : T.pattern =
  match p.node with
  | M.Pwild    -> T.Pwild
  | M.Pconst i -> T.Pid (unloc i)

let mterm_to_expr (mt : M.mterm) : T.expr =
  let fi = unloc in
  let rec f (mt : M.mterm) =
    match mt.node with
    | Mif (c, t, e)      -> T.Eif (f c, f t, f e)
    | Mmatchwith (w, l)  -> T.Ematchwith (f w, List.map (fun ((p, e) : M.pattern * M.mterm) -> ([pattern_to_pattern p], f e)) l)
    | Mapp (i, args)     -> T.Eapp (Evar (fi i), List.map f args)
    (* | Mexternal     of 'id * 'id * 'term * ('term) list *)
    (* | Mget          of 'term * 'term *)
    (* | Mset          of 'term * 'term * 'term *)
    (* | Maddasset     of ident * 'term * 'term * 'term list *)
    (* | Maddfield     of ident * ident * 'term * 'term  * 'term list asset_name * field_name * asset instance * item * shalow values *)
    (* | Maddlocal     of 'term * 'term *)
    (* | Mremoveasset  of ident * 'term * 'term *)
    (* | Mremovefield  of ident * ident * 'term * 'term *)
    (* | Mremovelocal  of 'term * 'term *)
    (* | Mclearasset   of ident * 'term *)
    (* | Mclearfield   of ident * ident * 'term *)
    (* | Mclearlocal   of 'term *)
    (* | Mreverseasset of ident * 'term *)
    (* | Mreversefield of ident * ident * 'term *)
    (* | Mreverselocal of 'term *)
    (* | Mselect       of ident * 'term * 'term *)
    (* | Msort         of ident * 'term * ident * sort_kind *)
    (* | Mcontains     of ident * 'term * 'term *)
    (* | Mnth          of ident * 'term * 'term *)
    (* | Mcount        of ident * 'term *)
    (* | Msum          of ident * 'id * 'term *)
    (* | Mmin          of ident * 'id * 'term *)
    (* | Mmax          of ident * 'id * 'term *)
    (* | Mmathmax      of 'term * 'term *)
    (* | Mmathmin      of 'term * 'term *)
    | Mfail e         -> T.Eapp (T.Edot (T.Evar "Current", "failwith"), [f e])
    | Mand (l, r)     -> T.Ebin (And,      f l, f r)
    | Mor (l, r)      -> T.Ebin (Or,       f l, f r)
    | Mnot e          -> T.Eunary (Not,    f e)
    | Mequal (l, r)   -> T.Ebin (Equal,    f l, f r)
    | Mnequal (l, r)  -> T.Ebin (Nequal,   f l, f r)
    | Mgt (l, r)      -> T.Ebin (Gt,       f l, f r)
    | Mge (l, r)      -> T.Ebin (Ge,       f l, f r)
    | Mlt (l, r)      -> T.Ebin (Lt,       f l, f r)
    | Mle (l, r)      -> T.Ebin (Le,       f l, f r)
    | Mplus (l, r)    -> T.Ebin (Plus,     f l, f r)
    | Mminus (l, r)   -> T.Ebin (Minus,    f l, f r)
    | Mmult (l, r)    -> T.Ebin (Mult,     f l, f r)
    | Mdiv (l, r)     -> T.Ebin (Div,      f l, f r)
    | Mmodulo (l, r)  -> T.Ebin (Modulo,   f l, f r)
    | Muplus e        -> T.Eunary (Uplus,  f e)
    | Muminus e       -> T.Eunary (Uminus, f e)
    (* | Mrecord l       -> T.Erecord (None, List.map (fun (i, e) -> (fi i, f e)) l) *)
    (* | Mletin (i, e, l, e) -> T.Eletin (List.map (fun (ll, ee) -> (List.map (fun (i, t) -> (i, to_type t)) ll, f ee)) l, f e) *)
    (* | Mvarstorevar  of 'id *)
    (* | Mvarstorecol  of 'id *)
    (* | Mvarenumval   of 'id *)
    (* | Mvarlocal     of 'id *)
    (* | Mvarfield     of 'id *)
    (* | Mvarthe *)
    | Mstate              -> T.Elit (Lraw "state")
    | Mnow                -> current_id "time"
    | Mtransferred        -> current_id "amount"
    | Mcaller             -> current_id "sender"
    | Mbalance            -> current_id "balance"
    | Marray l            -> T.Econtainer (List.map f l)
    | Mint v              -> T.Elit (Lint v)
    | Muint v             -> T.Elit (Lint v)
    | Mbool b             -> T.Elit (Lbool b)
    | Menum v             -> T.Elit (Lraw v)
    | Mrational _         -> emit_error TODO
    | Mdate v             -> T.Elit (Lraw v)
    | Mstring str         -> T.Elit (Lstring str)
    | Mcurrency (v, _)    -> T.Elit (T.Lraw (Big_int.string_of_big_int v ^ "tz"))
    | Maddress v          -> T.Elit (Lraw v)
    | Mduration v         -> T.Elit (Lraw v)
    | Mdotasset (e, i)    -> T.Edot (f e, fi i)
    | Mdotcontract (e, i) -> T.Edot (f e, fi i)
    | Mtuple l            -> T.Etuple (List.map f l)
    (* | Mfor          of ('id * 'term * 'term) *)
    | Mseq _              -> emit_error (UnsupportedTranslationOf "sequence")
    | Massign _           -> emit_error (UnsupportedTranslationOf "assignment value")
    | Massignfield _      -> emit_error (UnsupportedTranslationOf "assignment field")
    (* | Mtransfer     of ('term * bool * 'id qualid_gen option) *)
    (* | Mbreak *)
    (* | Massert       of 'term *)
    (* | Mreturn       of 'term *)
    (* | Mtokeys       of ident * 'term *)

    (* formula mterms *)
    | Mimply _
    | Mequiv _
    | Mforall _
    | Mexists _
    | Msetbefore _
    | Msetunmoved _
    | Msetadded _
    | Msetremoved _
    | Msetiterated _
    | Msettoiterate _
    | MsecMayBePerformedOnlyByRole _
    | MsecMayBePerformedOnlyByAction _
    | MsecMayBePerformedByRole _
    | MsecMayBePerformedByAction _
    | MsecTransferredBy _
    | MsecTransferredTo _ -> emit_error UnsupportedFormulaExpression
    | _ ->
      Format.eprintf "expr: %a@\n" M.pp_mterm mt;
      (* W.Evar "s", W.Tstorage; *)
      assert false
  in
  f mt

type sig_ = (ident list * T.type_) list * T.type_ * T.expr

module Utils : sig
  val get_asset      : M.model -> ident -> sig_
  val contains_asset : M.model -> ident -> sig_
end = struct
  open Mltree
  let failwith str = Eapp (Evar "failwith", [Elit (Lstring str)])

  let get_asset (model : M.model) (asset_name : ident) : sig_ =
    let asset = dumloc asset_name in
    let _, key_type = M.Utils.get_record_key model asset in
    let args = [(["s"; "key"], Ttuple [type_storage; Tbasic (btyp_to_basic key_type)])] in
    let ret  = Tlocal asset_name in
    let body =
      Ematchwith (Eapp (Edot (Evar "Map", "find"),
                        [
                          Evar "key";
                          Edot (Evar "s",
                                asset_name ^ "_assets")
                        ]),
                  [
                    ([Pexpr (Eapp (Evar "Some", [Evar "v"]))], Evar "v");
                    ([Pwild],  failwith "not_found") ])
    in
    args, ret, body

  let contains_asset (model : M.model) (asset_name : ident) : sig_ =
    let asset = dumloc asset_name in
    let _, key_type = M.Utils.get_record_key model asset in
    let args = [(["s"; "key"], Ttuple [type_storage; Tbasic (btyp_to_basic key_type)])] in
    let ret  = Tbasic Tbool in
    let body =
      Ematchwith (Eapp (Edot (Evar "Map", "find"),
                        [
                          Evar "key";
                          Edot (Evar "s",
                                asset_name ^ "_assets")
                        ]),
                  [
                    ([Pexpr (Eapp (Evar "Some", [Evar "_"]))], Elit (Lbool true));
                    ([Pwild],  Elit (Lbool false))])
    in
    args, ret, body
end

let to_liquidity (model : M.model) : T.tree =

  let add_decls (decl : M.decl_node) : T.decl list =
    match decl with
    | M.Denum (e : M.enum) ->
      let name = unloc e.name in
      let values : (ident * T.type_ option) list = List.map (fun (x : M.enum_item) -> (unloc x.name, None)) e.values in
      [T.Dtype (T.mk_type name ~values:values)]
    | M.Drecord (r : M.record) ->
      let name = unloc r.name in
      let fields = List.map (fun (ri : M.record_item) ->
          let id = unloc ri.name in
          let t  = ri.type_ in
          let dv = T.Elit (Lbool false) in (* get_value id t ri.default *)
          (id, to_type t, dv)) r.values in
      [T.Dstruct (T.mk_struct name ~fields:fields)]
    | M.Dcontract _ ->
      emit_error TODO
  in

  let rec ftype_to_type = function
    | M.FBasic b              -> T.Tbasic (btyp_to_basic b)
    | M.FAssetKeys (v, a)     -> T.Tlist (Tbasic (btyp_to_basic v))
    | M.FAssetRecord (v, a)   -> T.Tmap (Tbasic (btyp_to_basic v), Tlocal (unloc a))
    | M.FRecordCollection a   -> T.Tlist (Tlocal (unloc a))
    | M.FRecord a             -> T.Tlocal (unloc a)
    | M.FEnum a               -> T.Tlocal (unloc a)
    | M.FContainer (_, t)     -> T.Tlist (ftype_to_type t)
  in

  let get_dv = function
    | M.Bbool        -> T.Elit (Lbool false)
    | M.Bint         -> T.Elit (Lint Big_int.zero_big_int)
    | M.Buint        -> T.Elit (Lint Big_int.zero_big_int)
    | M.Brational    -> emit_error TODO
    | M.Bdate        -> emit_error (UnsupportedDefaultValue "date")
    | M.Bduration    -> T.Elit (Lint Big_int.zero_big_int)
    | M.Bstring      -> T.Elit (Lstring "")
    | M.Baddress     -> emit_error (UnsupportedDefaultValue "address")
    | M.Brole        -> emit_error (UnsupportedDefaultValue "role")
    | M.Bcurrency _  -> T.Elit (Lraw "0tz")
    | M.Bkey         -> emit_error (UnsupportedDefaultValue "key")
  in

  let generate_storage (storage : M.storage) : T.decl * T.decl =
    let fields : (ident * T.type_ * T.expr) list = List.fold_left (fun accu (x : M.lident M.storage_item_gen) ->
        List.fold_left (fun (accu : (ident * T.type_ * T.expr) list) (f : M.item_field) ->
            let id : ident  = unloc f.name in
            let t : T.type_ = ftype_to_type f.typ in
            (* let dv = get_value id t f.default in *)
            let dv : T.expr =
              match f.default with
              | Some v -> mterm_to_expr v
              | None ->
                match f.typ with
                | M.FBasic b            -> get_dv b
                | M.FAssetKeys (v, a)   -> T.Econtainer []
                | M.FAssetRecord (v, a) -> T.Elit (Lmap (Tbasic (btyp_to_basic v), Tlocal (unloc a)))
                | M.FRecordCollection a -> T.Econtainer []
                | M.FRecord a           -> emit_error (UnsupportedDefaultValue (unloc a))
                | M.FEnum a             -> emit_error (UnsupportedDefaultValue (unloc a))
                | M.FContainer (_, t)   -> T.Econtainer []
            in
            accu @ [(id, t, dv)])
          accu x.fields) [] storage in
    let args = [["_"], T.Tbasic Tunit] in
    let body = T.Erecord (None, List.map (
        fun (id, _, init) ->
          (id, init)
      ) fields) in
    T.Dstruct (T.mk_struct "storage" ~fields:fields), T.Dfun (T.mk_fun "initialize" Init args type_storage body)
  in

  let generate_api (x : M.api_item) : T.decl =
    let def = ([["_"], T.Tbasic Tunit], T.Tlocal "storage", todo) in
    let generate_api_storage = function
      | M.Get id -> Utils.get_asset model id
      | _ -> def
    in
    let generate_api_container = function
      | _ -> def
    in
    let generate_api_function = function
      | M.Contains id -> Utils.contains_asset model id
      | _ -> def
    in
    let generate_api_builtin = function
      | _ -> def
    in
    let (name, (args, ret, body)) : ident * sig_ =
      match x.node with
      | M.APIStorage   v -> M.Utils.function_name_from_storage_const v,   generate_api_storage v
      | M.APIContainer v -> M.Utils.function_name_from_container_const v, generate_api_container v
      | M.APIFunction  v -> M.Utils.function_name_from_function_const v,  generate_api_function v
      | M.APIBuiltin   v -> M.Utils.function_name_from_builtin_const v,   generate_api_builtin v
    in
    T.Dfun (T.mk_fun name Inline args ret body)
  in

  let generate_function (x : M.function__) : T.decl =
    let node, f, ret = match x.node with
      | Function (a, b) -> T.None, a, to_type b
      | Entry a -> T.Entry, a, T.Ttuple [Tlist (Tlocal "operation"); Tlocal "storage"]
    in

    let name = unloc f.name in
    let args =
      match f.args with
      | [] -> [["_"], T.Tbasic Tunit]
      | l  -> List.map (fun (id, t, _) -> ([unloc id], to_type t)) l
    in
    let body = todo in (*mterm_to_expr f.body in*)
    T.Dfun (T.mk_fun name node args ret body)
  in

  let name = unloc model.name in
  let decls = List.fold_left (fun accu (d : M.decl_node) -> accu @ add_decls d) [] model.decls in
  let storage, fun_init = generate_storage model.storage in
  let apis = List.fold_left (fun accu (x : M.api_item) ->
      if x.only_formula
      then accu
      else accu @ [generate_api x]) [] model.api_items in
  let funs = List.fold_left (fun accu x -> accu @ [generate_function x]) [] model.functions in

  let ds = decls @ [storage] @ [fun_init] @ apis @ funs in
  T.mk_tree name ~decls:ds
