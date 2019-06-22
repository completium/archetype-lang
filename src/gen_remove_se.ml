open Tools
open Location
open Ident

module M = Model
module W = Model_wse

exception Anomaly of string
type error_desc =
  | UnsupportedContainer of string
  | UnsupportedType of string
  | RecordNotFound
  | NotSupportedType of string
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

let storage_id = "s"

let byp_to_type = function
  | M.Bbool       -> W.Tbool
  | M.Bint        -> W.Tint
  | M.Buint       -> W.Tuint
  | M.Brational   -> W.Trational
  | M.Bdate       -> W.Tdate
  | M.Bduration   -> W.Tduration
  | M.Bstring     -> W.Tstring
  | M.Baddress    -> W.Taddress
  | M.Brole       -> W.Trole
  | M.Bcurrency c -> W.Tcurrency c
  | M.Bkey        -> W.Tkey

let rec item_type_to_type = function
  | M.FBasic v              -> byp_to_type v
  | M.FAssetKeys (v, a)     -> W.Tcontainer (byp_to_type v)
  | M.FAssetRecord (v, a)   -> W.Tmap (byp_to_type v, Trecord (unloc a))
  | M.FRecordCollection a   -> W.Tcontainer (Trecord (unloc a))
  | M.FRecord a             -> W.Trecord (unloc a)
  | M.FEnum a               -> W.Tenum (unloc a)
  | M.FContainer (_, t)     -> W.Tcontainer (item_type_to_type t)

let to_expr (e : M.mterm) : W.expr =
  match e.node with
  | M.Mlit (BVint v)     -> W.Elitint v
  | M.Mlit (BVaddress v) -> W.Elitraw v
  | _ -> assert false

let rec to_type t =
  match t with
  | M.Tasset a          -> W.Trecord (unloc a)
  | M.Tenum e           -> W.Tenum (unloc e)
  | M.Tcontract c       -> W.Tcontract (unloc c)
  | M.Tbuiltin v        -> byp_to_type v
  | M.Tcontainer (t, _) -> W.Tcontainer (to_type t)
  | M.Ttuple l          -> W.Ttuple (List.map to_type l)
  | M.Tprog _
  | M.Tvset _
  | M.Ttrace _          -> emit_error (NotSupportedType (Format.asprintf "%a@." M.pp_type_ t))

let get_default_expr_from_type = function
  | M.FBasic Bbool        -> W.Elitbool false
  | M.FBasic Bint         -> W.Elitint Big_int.zero_big_int
  | M.FBasic Buint        -> W.Elitint Big_int.zero_big_int
  | M.FBasic Brational    -> W.Elitint Big_int.zero_big_int
  | M.FBasic Bdate        -> W.Elitint Big_int.zero_big_int
  | M.FBasic Bduration    -> W.Elitint Big_int.zero_big_int
  | M.FBasic Bstring      -> W.Elitstring ""
  | M.FBasic Baddress     -> assert false
  | M.FBasic Brole        -> assert false
  | M.FBasic Bcurrency _  -> W.Elitraw "0tz"
  | M.FBasic Bkey         -> W.Elitint Big_int.zero_big_int
  | M.FAssetKeys (k, a)   -> W.Earray []
  | M.FAssetRecord (k, a) -> W.Elitmap (byp_to_type k, Trecord (unloc a))
  | M.FRecordCollection a -> assert false
  | M.FRecord _           -> assert false
  | M.FEnum _             -> assert false
  | M.FContainer (c, i)   -> assert false

type sig_ = W.kind_function * (ident list * W.type_) list * W.type_ * W.expr

let current_id id = W.Ecall (W.Edot (W.Evar "Current", id), [])

module Utils : sig
  val get_asset      : M.model -> M.lident              -> sig_
  val add_asset      : M.model -> M.lident              -> sig_
  val contains_asset : M.model -> M.lident              -> sig_
  val add_container  : M.model -> (M.lident * M.lident) -> sig_
end = struct
  open Model_wse

  let failwith str = Ecall (Evar "failwith", [Elitstring str])

  let get_asset model asset =
    let asset_name = unloc asset in
    let _, key_type = M.Utils.get_record_key model asset in
    let args = [(["s"], Tstorage); (["key"], byp_to_type key_type) ] in
    let ret  = Trecord asset_name in
    let body =
      Ematchwith (Ecall (Edot (Evar "Map", "find"),
                         [
                           Evar "key";
                           Edot (Evar "s",
                                 asset_name ^ "_assets")
                         ]),
                  [
                    (Pexpr (Ecall (Evar "Some", [Evar "v"])), Evar "v");
                    (Pwild,  failwith "not_found") ])
    in
    Function, args, ret, body

  let add_asset model asset =
    let asset_name = unloc asset in
    let key_id, _ = M.Utils.get_record_key model asset |> fun (x, y) -> unloc x , y in
    let new_asset = "new_asset" in
    let col_asset_id = asset_name ^ "_assets" in
    let col_keys_id = asset_name ^ "_keys" in
    let args = [([storage_id], Tstorage); ([new_asset], Trecord asset_name) ] in
    let ret  = Tstorage in
    let s_col_asset_id = Edot (Evar storage_id, col_asset_id) in
    let s_col_keys_id = Edot (Evar storage_id, col_keys_id) in
    let new_asset_key = Edot (Evar new_asset, key_id) in
    let body =
      Eletin (
        [
          [storage_id, Tstorage], Eassign (s_col_asset_id, Ecall (Edot (Evar "Map","add"), [new_asset_key; Evar new_asset; s_col_asset_id]));
          [storage_id, Tstorage], Eassign (s_col_keys_id, Eaddlist (new_asset_key, s_col_keys_id))
        ], Evar storage_id)
    in
    Function, args, ret, body

  let contains_asset model asset =
    let asset_name = unloc asset in
    let _, key_type = M.Utils.get_record_key model asset in
    let args = [(["s"], Tstorage); (["key"], byp_to_type key_type) ] in
    let ret  = Tbool in
    let body =
      Ematchwith (Ecall (Edot (Evar "Map", "find"),
                         [
                           Evar "key";
                           Edot (Evar "s",
                                 asset_name ^ "_assets")
                         ]),
                  [
                    (Pexpr (Ecall (Evar "Some", [Evar "_"])), Elitbool true);
                    (Pwild,  Elitbool false)])
    in
    Function, args, ret, body

  let add_container_collection model (asset, field) t = assert false

  let add_container_partition model ((asset, field) : M.lident * 'id M.record_item_gen) t =
    let field_ptype = to_type t in
    let asset2_name = unloc asset in
    let asset2_key_id, _ = M.Utils.get_record_key model asset in
    let asset_name = match field_ptype with
      | Trecord v -> v
      | _ -> emit_error RecordNotFound in
    let new_asset_id = "new_asset" in
    let args = [([storage_id], Tstorage); (["a"], Trecord asset2_name); ([new_asset_id], field_ptype)] in
    let ret  = Tstorage in
    let key_id, _ = M.Utils.get_record_key model (dumloc asset_name) |> fun (x, y) -> unloc x , y in
    let col_asset2_id   = asset2_name ^ "_assets" in
    let col_asset_id    = asset_name  ^ "_assets" in
    let col_keys_id     = asset_name  ^ "_keys" in
    let s_col_asset2_id = Edot (Evar storage_id,   col_asset2_id) in
    let s_col_asset_id  = Edot (Evar storage_id,   col_asset_id) in
    let s_col_keys_id   = Edot (Evar storage_id,   col_keys_id) in
    let new_asset_key   = Edot (Evar new_asset_id, key_id) in
    let aaa = Edot (Evar "v", unloc field.name) in
    let body =
      Eletin (
        [
          [storage_id, Tstorage], Eassign (s_col_asset_id, Ecall (Edot (Evar "Map","add"), [new_asset_key; Evar new_asset_id; s_col_asset_id]));
          [storage_id, Tstorage], Eassign (s_col_keys_id, Eaddlist (new_asset_key, s_col_keys_id));
          [storage_id, Tstorage], Eassign (s_col_asset2_id, Ematchwith (Ecall (Edot (Evar "Map", "find"),
                                                                               [
                                                                                 Edot (Evar "a", unloc asset2_key_id);
                                                                                 Edot (Evar storage_id, col_asset2_id)
                                                                               ]),
                                                                        [
                                                                          (Pexpr (Ecall (Evar "Some", [Evar "v"])),

                                                                           Eletin ([["new_casset", Trecord asset2_name], Eassign (aaa, Eaddlist (Evar new_asset_id, aaa))],
                                                                                   Ecall (Edot (Evar "Map","update"), [Edot (Evar "a", unloc asset2_key_id); Ecall (Evar "(Some", [Evar "new_casset)"]); s_col_asset2_id]))
                                                                          );
                                                                          (Pwild, failwith "not found")]))
        ], Evar storage_id)
    in
    Function, args, ret, body


  let add_container model (asset, field) =
    let field = M.Utils.get_record_field model (asset, field) in
    match field.type_ with
    | Tcontainer (t, M.Collection) -> add_container_collection model (asset, field) t
    | Tcontainer (t, M.Partition)  -> add_container_partition  model (asset, field) t
    (* | Tcontainer (t, c) -> emit_error (UnsupportedContainer (Format.asprintf "%a@." M.pp_container c)) *)
    | t -> emit_error (UnsupportedType (Format.asprintf "%a@." M.pp_type_ t))

end

let op_to_constr op =
  match op with
  | `Logical M.And    -> (fun (x, y) -> W.Eand (x, y))
  | `Logical M.Or     -> (fun (x, y) -> W.Eor (x, y))
  | `Cmp     M.Equal  -> (fun (x, y) -> W.Eequal (x, y))
  | `Cmp     M.Nequal -> (fun (x, y) -> W.Enequal (x, y))
  | `Cmp     M.Gt     -> (fun (x, y) -> W.Egt (x, y))
  | `Cmp     M.Ge     -> (fun (x, y) -> W.Ege (x, y))
  | `Cmp     M.Lt     -> (fun (x, y) -> W.Elt (x, y))
  | `Cmp     M.Le     -> (fun (x, y) -> W.Ele (x, y))
  | `Arith   M.Plus   -> (fun (x, y) -> W.Eplus (x, y))
  | `Arith   M.Minus  -> (fun (x, y) -> W.Eminus (x, y))
  | `Arith   M.Mult   -> (fun (x, y) -> W.Emult (x, y))
  | `Arith   M.Div    -> (fun (x, y) -> W.Ediv (x, y))
  | `Arith   M.Modulo -> (fun (x, y) -> W.Emodulo (x, y))
  | _ ->
    Format.eprintf "op_to_constr: %a@\n" M.pp_operator op;
    raise (Anomaly (Format.asprintf "Unsupported operator %a@." M.pp_operator op) )

let rec instr_to_expr model (instr : M.instruction) : W.expr * W.type_ =
  let compute_return_type t1 t2 : W.type_ =

    match t1, t2 with
    | W.Tstorage, W.Tstorage       -> W.Tstorage
    | W.Toperations, W.Toperations -> W.Toperations
    | W.Tstorage, W.Toperations    -> W.Ttuple [W.Toperations; W.Tstorage]
    | W.Toperations, W.Tstorage    -> W.Ttuple [W.Toperations; W.Tstorage]
    | _ -> W.Tunit
  in

  let rec expr_to_expr (model : M.model) (expr : M.mterm) : W.expr * W.type_ =
    match expr.node with
    | M.Mcall (None , id, args) ->
      let c_id = match id with
        | Cid id -> W.Evar (unloc id)
        | Cconst c ->
          Format.eprintf "Cconst: %a@\n" M.pp_const c;
          raise (Anomaly "no const")
        | Cstorage s -> W.Evar (M.function_name_from_function_node (M.Storage s))
      in
      let args =
        W.Evar "s"::(List.map (fun x ->
            match x with
            | M.AExpr e -> expr_to_expr model e |> fst
            | _ -> assert false
          ) args) in
      W.Ecall (c_id, args), to_type expr.type_

    | M.Mrecord l ->
      let asset_name = match expr.type_ with
        | M.Tasset v -> v
        | _ -> assert false
      in
      let field_list = M.Utils.get_named_field_list model asset_name l in

      let field_list = List.map (fun (a, b) -> (unloc a, (fst |@ expr_to_expr model) b) ) field_list in

      W.Erecord field_list, to_type expr.type_

    | M.Marray l ->
      let list = List.map (fun x -> (fst |@ expr_to_expr model) x) l in
      W.Earray list, to_type expr.type_

    | M.Mdot (a, b) ->
      let expr_a, _ = expr_to_expr model a in
      let id_b = unloc b in
      W.Edot (expr_a, id_b), to_type expr.type_

    | M.Mvar s ->
      let a =
        if Model.Utils.is_storage_attribute model s
        then W.Edot (W.Evar storage_id, unloc s)
        else W.Evar (unloc s)
      in
      a, to_type expr.type_

    | M.Mlit (BVint i)
    | M.Mlit (BVuint i) -> W.Elitint i, to_type expr.type_
    | M.Mlit (BVbool b) -> W.Elitbool b, to_type expr.type_
    | M.Mlit (BVstring str) -> W.Elitstring str, to_type expr.type_

    | M.Mlit (BVduration _) -> raise (Anomaly "Unsuported duration") (*W.Elitraw str, to_type expr.type_*)
    | M.Mlit (BVrational _) -> raise (Anomaly "Unsuported rational") (*W.Elitraw str, to_type expr.type_*)

    | M.Mlit (BVenum str)
    | M.Mlit (BVdate str)
    | M.Mlit (BVaddress str)  ->
      W.Elitraw str, to_type expr.type_

    | M.Mlit (BVcurrency (_cur, v)) ->
      W.Elitraw (Format.asprintf "%stz" (Big_int.string_of_big_int v)), to_type expr.type_

    | M.Mnot e ->
      let expr_e, _ = expr_to_expr model e in
      W.Enot expr_e, to_type expr.type_

    | M.Mlogical (op, l, r) ->
      let expr_l, _ = expr_to_expr model l in
      let expr_r, _ = expr_to_expr model r in
      (op_to_constr (`Logical op)) (expr_l, expr_r), to_type expr.type_

    | M.Mcomp (op, l, r) ->
      let expr_l, _ = expr_to_expr model l in
      let expr_r, _ = expr_to_expr model r in
      (op_to_constr (`Cmp op)) (expr_l, expr_r), to_type expr.type_

    | M.Marith (op, l, r) ->
      let expr_l, _ = expr_to_expr model l in
      let expr_r, _ = expr_to_expr model r in
      (op_to_constr (`Arith op)) (expr_l, expr_r), to_type expr.type_

    | M.Mconst c ->
      (
        match c with
        | M.Ccaller      -> current_id "sender",  to_type expr.type_
        | M.Ctransferred -> current_id "amount",  to_type expr.type_
        | M.Cbalance     -> current_id "balance", to_type expr.type_
        | M.Cnow         -> current_id "time",    to_type expr.type_

        | _ ->
          Format.eprintf "to_const: %a@\n" M.pp_const c;
          raise (Anomaly "Not supported yet")
      )
    | _ ->
      Format.eprintf "expr: %a@\n" M.pp_mterm expr;
      W.Evar "s", W.Tstorage
  in

  match instr.node with
  | M.Iif (c, t, e) ->
    let expr_cond, _ = expr_to_expr model c in
    let expr_then, type_then = instr_to_expr model t in
    let expr_else, type_else = instr_to_expr model e in

    let ret = compute_return_type type_then type_else in
    W.Eif (expr_cond, expr_then, expr_else), ret

  | M.Icall (None, id, args) ->
    let i_id, args, t = match id with
      | Cid id ->
        let args =
          W.Evar "s"::(List.map (fun e -> (match e with | M.AExpr e -> expr_to_expr model e | _ -> assert false) |> fst) args)
        in
        W.Evar (unloc id), args, W.Tstorage
      | Cconst Cfail ->
        let args = (List.map (fun e -> (match e with | M.AExpr e -> expr_to_expr model e | _ -> assert false) |> fst) args) in
        W.Edot (W.Evar "Current", "failwith"), args, W.Tstorage
      | Cconst c ->
        Format.eprintf "Cconst: %a@\n" M.pp_const c;
        raise (Anomaly "no const")
      | Cstorage s ->
        let args =
          W.Evar "s"::(List.map (fun e -> (match e with | M.AExpr e -> expr_to_expr model e | _ -> assert false) |> fst) args)
        in
        W.Evar (M.function_name_from_function_node (M.Storage s)), args, W.Tstorage
    in
    W.Ecall (i_id, args), t

  | M.Ifor (id, col, body) ->
    let expr_body, type_body = instr_to_expr model body in
    expr_body, W.Tstorage

  | _ ->
    Format.eprintf "instr: %a@\n" M.pp_instruction instr;
    W.Evar "s", W.Tstorage

let compute_body_entry model (fs : M.function_struct) =
  let e, ret = instr_to_expr model fs.body in
  match ret with
  | W.Tstorage ->
    W.Eletin (
      [([storage_id, Tstorage], e)],
      Etuple [Earray []; W.Evar storage_id])
  | _ ->
    raise (Anomaly (Format.asprintf "compute_body_entry: %a@\n" W.pp_type_ ret))


let mk_function_struct model (f : 'id M.function__) =
  let name : ident = M.function_name_from_function_node f.node in
  let kind, args, ret, body =
    match f.node with
    | M.Entry fs ->
      let itargs, ttargs = List.fold_left (fun (args, typs) (arg, typ, _) ->
          (args @ [unloc arg], typs @ [to_type typ])
        ) ([], []) fs.args in
      let type_arg =
        match ttargs with
        | [] -> W.Tunit
        | _  -> W.Ttuple ttargs in
      let args = [(itargs, type_arg); ([storage_id], W.Tstorage) ] in
      let ret  = W.Ttuple [W.Toperations; W.Tstorage] in
      let body =
        if (String.equal (unloc fs.name) "add") (* || (String.equal (unloc fs.name) "clear_expired")*)
        then compute_body_entry model fs
        else Etuple [Earray []; W.Evar storage_id] in
      W.Entry, args, ret, body

    | M.Storage Get asset                   -> Utils.get_asset model asset
    | M.Storage AddAsset asset              -> Utils.add_asset model asset
    | M.Storage ContainsAsset asset         -> Utils.contains_asset model asset
    | M.Storage AddContainer (asset, field) -> Utils.add_container model (asset, field)

    | _ ->
      let args = [[""], W.Tunit] in
      let ret  = W.Tunit in
      let body = W.Etuple [] in
      W.Function, args, ret, body
  in

  W.mk_function name kind ret body ~args:args

let remove_se (model : M.model) : W.model =
  let name = model.name in
  let enums = List.fold_left (fun accu x ->
      match x with
      | M.TNenum e ->
        let name = unloc e.name in
        let values : ident list = List.map (fun (x : 'id M.enum_item) -> unloc x.name) e.values in
        let enum = W.mk_enum name ~values:values in
        accu @ [enum]
      | _ -> accu) [] model.decls in
  let records = List.fold_left (fun accu x ->
      match x with
      | M.TNrecord r ->
        let values : (ident * W.type_ * W.expr) list =
          List.map (fun (x : 'id M.record_item_gen) -> (unloc x.name, to_type x.type_, W.Elitbool false)) r.values
        in
        accu @
        [W.mk_record (unloc r.name) ~values:values]
      | M.TNstorage s ->
        let values : (ident * W.type_ * W.expr) list =
          List.fold_left (fun accu (x : 'id M.storage_item_gen) ->
              accu @ List.map (fun (i : 'id M.item_field_gen) : (ident * W.type_ * W.expr) ->
                  (unloc i.name, item_type_to_type i.typ, (
                      match i.default with
                      | Some v -> to_expr v
                      | None -> get_default_expr_from_type i.typ
                    ))) x.fields
            ) [] s
        in
        accu @
        [W.mk_record "storage" ~values:values]
      | _ -> accu
    ) [] model.decls in
  let funs = List.fold_left (fun accu x ->
      match x with
      | M.TNfunction f ->
        let func = mk_function_struct model f in
        accu @ [func]
      | _ -> accu) [] model.decls in
  W.mk_model (unloc name) ~enums:enums ~records:records ~funs:funs
