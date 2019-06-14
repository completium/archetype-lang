open Tools

module A = Ast
module W = Model_wse
module T = Mltree

exception Anomaly of string

type error_desc =
  | UnsupportedDefaultValue of string
  | CannotConvertLogicalOperator of string
  | NoStorageRecordFound
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

let to_basic = function
  | A.VTbool       -> T.Tbool
  | A.VTint        -> T.Tint
  | A.VTuint       -> T.Tnat
  | A.VTrational   -> assert false (* TODO *)
  | A.VTdate       -> T.Ttimestamp
  | A.VTduration   -> T.Tint
  | A.VTstring     -> T.Tstring
  | A.VTaddress    -> T.Taddress
  | A.VTrole       -> T.Tkey_hash
  | A.VTcurrency _ -> T.Ttez
  | A.VTkey        -> T.Tkey

let rec to_type = function
  | W.Tstorage     -> T.Tlocal "storage"
  | W.Toperations  -> T.Tlist  (T.Tlocal "operation")
  | W.Tbool        -> T.Tbasic Tbool
  | W.Tint         -> T.Tbasic Tint
  | W.Tuint        -> T.Tbasic T.Tnat
  | W.Trational    -> assert false (* TODO *)
  | W.Tdate        -> T.Tbasic T.Ttimestamp
  | W.Tduration    -> T.Tbasic Tint
  | W.Tstring      -> T.Tbasic Tstring
  | W.Taddress     -> T.Tbasic Taddress
  | W.Trole        -> T.Tbasic Tkey_hash
  | W.Tcurrency _  -> T.Tbasic Ttez
  | W.Tkey         -> T.Tbasic Tkey
  | W.Trecord id
  | W.Tenum id     -> T.Tlocal id
  | W.Ttuple types -> T.Ttuple (List.map to_type types)
  | W.Tcontainer t -> T.Tlist  (to_type t)
  | W.Tcontract c  -> T.Tcontract c
  | W.Tmap (k, v)  -> T.Tmap   (to_type k, to_type v)
  | W.Tunit        -> T.Tbasic Tunit

let to_literal (l : 'a A.bval_poly) =
  match l.node with
  | A.BVint       n      -> T.Lint n
  | A.BVuint      n      -> T.Lint n
  | A.BVbool      b      -> T.Lbool b
  | A.BVenum      s      -> T.Lraw s
  | A.BVrational (n, d)  -> assert false
  | A.BVdate      s      -> T.Lraw s
  | A.BVstring    s      -> T.Lstring s
  | A.BVcurrency  (_, n) -> T.Lraw (Big_int.string_of_big_int n ^ "tz")
  | A.BVaddress   s      -> T.Lraw s
  | A.BVduration  s      -> T.Lraw s

let get_default_value_from_type = function
  | W.Tstorage     -> emit_error (UnsupportedDefaultValue "storage")
  | W.Toperations  -> emit_error (UnsupportedDefaultValue "operations")
  | W.Tbool        -> T.Elit (Lbool false)
  | W.Tint         -> T.Elit (Lint Big_int.zero_big_int)
  | W.Tuint        -> T.Elit (Lint Big_int.zero_big_int)
  | W.Trational    -> T.Elit (Lint Big_int.zero_big_int)
  | W.Tdate        -> emit_error (UnsupportedDefaultValue "date")
  | W.Tduration    -> T.Elit (Lint Big_int.zero_big_int)
  | W.Tstring      -> T.Elit (Lstring "")
  | W.Taddress     -> emit_error (UnsupportedDefaultValue "address")
  | W.Trole        -> emit_error (UnsupportedDefaultValue "role")
  | W.Tcurrency _  -> T.Elit (Lraw "0tz")
  | W.Tkey         -> emit_error (UnsupportedDefaultValue "key")
  | W.Trecord _    -> emit_error (UnsupportedDefaultValue "record")
  | W.Tenum _      -> emit_error (UnsupportedDefaultValue "enum")
  | W.Ttuple _     -> emit_error (UnsupportedDefaultValue "tuple")
  | W.Tcontainer _ -> Econtainer []
  | W.Tcontract _  -> emit_error (UnsupportedDefaultValue "contract")
  | W.Tmap (k, v)  -> T.Elit (Lmap (to_type k, to_type v))
  | W.Tunit        -> emit_error (UnsupportedDefaultValue "unit")

let op_to_bin_operator = function
  | `Logical A.And   -> T.And
  | `Logical A.Or    -> T.Or
  | `Logical A.Imply -> emit_error (CannotConvertLogicalOperator "imply")
  | `Logical A.Equiv -> emit_error (CannotConvertLogicalOperator "equiv")
  | `Cmp A.Equal     -> T.Equal
  | `Cmp A.Nequal    -> T.Nequal
  | `Cmp A.Gt        -> T.Gt
  | `Cmp A.Ge        -> T.Ge
  | `Cmp A.Lt        -> T.Lt
  | `Cmp A.Le        -> T.Le
  | `Arith A.Plus    -> T.Plus
  | `Arith A.Minus   -> T.Minus
  | `Arith A.Mult    -> T.Mult
  | `Arith A.Div     -> T.Div
  | `Arith A.Modulo  -> T.Modulo

let op_to_unary_operator = function
  | A.Uplus   -> T.Uplus
  | A.Uminus  -> T.Uminus

let rec pattern_to_pattern = function
  | W.Pwild    -> T.Pwild
  | W.Pexpr e -> T.Pexpr (expr_to_expr e)
  | W.Pconst i -> T.Pid i

and  expr_to_expr (x : W.expr) : T.expr =
  match x with
  | W.Eif (c, t, e)     -> T.Eif (expr_to_expr c, expr_to_expr t, expr_to_expr e)
  | W.Ematchwith (w, l) -> T.Ematchwith (expr_to_expr w, List.map (fun (p, e) -> ([pattern_to_pattern p], expr_to_expr e)) l)
  | W.Ecall (i, args)   -> T.Eapp (expr_to_expr i, List.map expr_to_expr args)
  | W.Eand (l, r)       -> T.Ebin (And, expr_to_expr l, expr_to_expr l)
  | W.Eor (l, r)        -> T.Ebin (Or, expr_to_expr l, expr_to_expr l)
  | W.Enot e            -> T.Eunary (Not, expr_to_expr e)
  | W.Eequal (l, r)     -> T.Ebin (Equal, expr_to_expr l, expr_to_expr l)
  | W.Enequal (l, r)    -> T.Ebin (Nequal, expr_to_expr l, expr_to_expr l)
  | W.Egt (l, r)        -> T.Ebin (Gt, expr_to_expr l, expr_to_expr l)
  | W.Ege (l, r)        -> T.Ebin (Ge, expr_to_expr l, expr_to_expr l)
  | W.Elt (l, r)        -> T.Ebin (Lt, expr_to_expr l, expr_to_expr l)
  | W.Ele (l, r)        -> T.Ebin (Le, expr_to_expr l, expr_to_expr l)
  | W.Eplus (l, r)      -> T.Ebin (Plus, expr_to_expr l, expr_to_expr l)
  | W.Eminus (l, r)     -> T.Ebin (Minus, expr_to_expr l, expr_to_expr l)
  | W.Emult (l, r)      -> T.Ebin (Mult, expr_to_expr l, expr_to_expr l)
  | W.Ediv (l, r)       -> T.Ebin (Div, expr_to_expr l, expr_to_expr l)
  | W.Emodulo (l, r)    -> T.Ebin (Modulo, expr_to_expr l, expr_to_expr l)
  | W.Euplus e          -> T.Eunary (Uplus, expr_to_expr e)
  | W.Euminus e         -> T.Eunary (Uminus, expr_to_expr e)
  | W.Erecord l         -> T.Erecord (None ,List.map (fun (i, e) -> (i, expr_to_expr e)) l)
  | W.Eletin (l, e)     -> T.Eletin (List.map (fun (ll, ee) -> (List.map (fun (i, t) -> (i, to_type t)) ll, expr_to_expr ee)) l, expr_to_expr e)
  | W.Evar s            -> T.Evar s
  | W.Earray l          -> T.Econtainer (List.map expr_to_expr l)
  | W.Elitint i         -> T.Elit (Lint i)
  | W.Elitbool b        -> T.Elit (Lbool b)
  | W.Elitstring s      -> T.Elit (Lstring s)
  | W.Elitmap (k, v)    -> T.Elit (Lmap (to_type k, to_type v))
  | W.Elitraw s         -> T.Elit (Lraw s)
  | W.Edot (e, i)       -> T.Edot (expr_to_expr e, i)
  | W.Etuple l          -> T.Etuple (List.map expr_to_expr l)
  | W.Efold _           -> assert false (* TODO *)

let generate_init_instr (model : W.model) : T.expr =
  let storage = List.fold_left (fun accu (x : W.record_struct) ->
      match x with
      | {name = name; _} when String.equal name "storage" -> Some x
      | _ -> accu) None model.records in

  let s : W.record_struct =
    match storage with
    | Some x -> x
    | _ -> emit_error NoStorageRecordFound in

  T.Erecord (None, List.map (
      fun (id, _, init) ->
        (id, expr_to_expr init)
    ) s.values)


let to_liquidity (model : W.model) : T.tree =

  let cont f x d = d @ f x in

  let add_enums =
    List.map (fun (x : W.enum_struct) ->
        let name = x.name in
        let values = List.map (fun x -> (x, None)) x.values in
        T.Dtype (T.mk_type name ~values:values))
  in

  let add_structs =
    List.map (fun (x : W.record_struct) ->
        let name = x.name in
        let fields = List.map (fun (id, t, e) -> (id, to_type t, expr_to_expr e)) x.values in
        T.Dstruct (T.mk_struct name ~fields:fields))
  in

  let add_init decls : T.decl list =
    let name = "initialize" in
    let args = [] in
    let ret = to_type Tstorage in
    let body = generate_init_instr model in
    decls @ [T.Dfun (T.mk_fun name Init args ret body)]
  in

  let add_funs =
    List.map (fun (x : W.function_struct) ->
        let name = x.name in
        let node : T.fun_node =
          match x.kind with
          | Function -> None
          | Entry -> Entry
        in
        let args = List.map (fun (id, t) -> (id, to_type t) ) x.args in
        let ret = to_type x.ret in
        let body = expr_to_expr x.body in
        T.Dfun (T.mk_fun name node args ret body))
  in

  let name = model.name in
  let decls =
    []
    |> cont add_enums model.enums
    |> cont add_structs model.records
    |> add_init
    |> cont add_funs model.funs
  in
  T.mk_tree name ~decls:decls
