open Location
open Tools

module A = Ast
module W = Model_wse
module T = Mltree

let to_ident ident = unloc ident

exception Anomaly of string

let emit_error msg = raise (Anomaly msg)

let to_basic = function
  | A.VTbool       -> T.Tbool
  | A.VTint        -> T.Tint
  | A.VTuint       -> T.Tnat
  | A.VTrational   -> assert false
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
  | W.Tbuiltin b   -> T.Tbasic (to_basic b)
  | W.Trecord id
  | W.Tenum id     -> T.Tlocal (to_ident id)
  | W.Ttuple types -> T.Ttuple (List.map to_type types)
  | W.Tcontainer t -> T.Tlist  (to_type t)
  | W.Tmap (k, v)  -> T.Tmap   (to_type k, to_type v)

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

let get_default_value_from_basic = function
  | A.VTbool -> T.Elit (Lbool false)
  | A.VTint -> T.Elit (Lint Big_int.zero_big_int)
  | A.VTuint -> T.Elit (Lint Big_int.zero_big_int)
  | A.VTrational -> T.Elit (Lint Big_int.zero_big_int)
  | A.VTdate-> emit_error "cannot have a default value for date"
  | A.VTduration -> T.Elit (Lint Big_int.zero_big_int)
  | A.VTstring -> T.Elit (Lstring "")
  | A.VTaddress -> emit_error "cannot have a default value for address"
  | A.VTrole -> emit_error "cannot have a default value for role"
  | A.VTcurrency _ -> T.Elit (Lraw "0tz")
  | A.VTkey -> emit_error "cannot have a default value for key"

let get_default_value_from_type = function
  | W.Tstorage -> emit_error "cannot have a default value for storage"
  | W.Toperations -> emit_error "cannot have a default value for contract"
  | W.Tbuiltin b -> get_default_value_from_basic b
  | W.Trecord _ -> emit_error "cannot have a default value for record"
  | W.Tenum _ -> emit_error "cannot have a default value for enum"
  | W.Ttuple _ -> emit_error "cannot have a default value for tuple"
  | W.Tcontainer _ -> Econtainer []
  | W.Tmap (k, v) -> T.Elit (Lmap (to_type k, to_type v))

let op_to_bin_operator = function
  | `Logical A.And   -> T.And
  | `Logical A.Or    -> T.Or
  | `Logical A.Imply -> emit_error "imply is not supported here"
  | `Logical A.Equiv -> emit_error "equiv is not supported here"
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

let rec expr_to_expr (x : W.expr) : T.expr =
  match x.node with
  | `Eexpr Lquantifer _ ->
    emit_error "quantifier is not supported here"

  | `Eexpr Pif (c, t, e) ->
    Eif (expr_to_expr c, expr_to_expr t, expr_to_expr e)

  | `Eexpr Pmatchwith (id, l) -> assert false
  | `Eexpr Pcall (_, c, args) -> assert false
  | `Eexpr Pnot e ->
    Eunary (Not, expr_to_expr e)

  | `Eexpr Plogical (op, lhs, rhs) ->
    Ebin (op_to_bin_operator (`Logical op), expr_to_expr lhs, expr_to_expr rhs)

  | `Eexpr Pcomp (op, lhs, rhs) ->
    Ebin (op_to_bin_operator (`Cmp op), expr_to_expr lhs, expr_to_expr rhs)

  | `Eexpr Parith (op, lhs, rhs) ->
    Ebin (op_to_bin_operator (`Arith op), expr_to_expr lhs, expr_to_expr rhs)

  | `Eexpr Puarith (op, e) ->
    Eunary (op_to_unary_operator op, expr_to_expr e)

  | `Eexpr Precord l -> assert false
  | `Eexpr Pletin (id, i, b, _) -> assert false
  | `Eexpr Pvar id ->
    Evar (to_ident id)

  | `Eexpr Parray l ->
    Econtainer (List.map expr_to_expr l)

  | `Eexpr Plit l ->
    Elit (to_literal l)

  | `Eexpr Pdot (e, id) ->
    Edot (expr_to_expr e, to_ident id)

  | `Eexpr Pconst c -> assert false
  | `Eexpr Ptuple l ->
    Etuple (List.map expr_to_expr l)

  | `Ewse  Efold (l, e, w) -> assert false
  | `Einstr i -> instruction_to_expr i

and instruction_to_expr (i : W.instruction) : T.expr =
  match i.node with
  | Iletin (l, e, b) -> Eletin ([[], expr_to_expr e], instruction_to_expr b)
  | Ituple l -> Etuple (List.map (fun x -> T.Evar (to_ident x)) l)

let generate_init_instr (model : W.model) : T.expr =
  let storage = List.fold_left (fun accu (x : W.record_struct) ->
      match x with
      | {name = name; _} when String.equal (to_ident name) "storage" -> Some x
      | _ -> accu) None model.records in

  let s : W.record_struct =
    match storage with
    | Some x -> x
    | _ -> emit_error "no storage found" in

  T.Erecord (None, List.map (
      fun (id, _, init) ->
        (to_ident id, expr_to_expr init)
    ) s.values)


let to_liquidity (model : W.model) : T.tree =

  let cont f x d = d @ f x in

  let add_enums =
    List.map (fun (x : W.enum_struct) ->
        let name = to_ident x.name in
        let values = List.map (fun x -> (to_ident x, None)) x.values in
        T.Dtype (T.mk_type name ~values:values))
  in

  let add_structs =
    List.map (fun (x : W.record_struct) ->
        let name = to_ident x.name in
        let fields = List.map (fun (id, t, e) -> (to_ident id, to_type t, expr_to_expr e)) x.values in
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
        let name = to_ident x.name in
        let node : T.fun_node =
          match x.kind with
          | Function -> None
          | Entry -> Entry
        in
        let args = List.map (fun (id, t) -> (to_ident id, to_type t) ) x.args in
        let ret = to_type x.ret in
        let body = instruction_to_expr x.body in
        T.Dfun (T.mk_fun name node args ret body))
  in

  let name = to_ident model.name in
  let decls =
    []
    |> cont add_enums model.enums
    |> cont add_structs model.records
    |> add_init
    |> cont add_funs model.funs
  in
  T.mk_tree name ~decls:decls
