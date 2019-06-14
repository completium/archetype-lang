open Tools
open Location
open Ident

module A = Ast
module M = Model
module W = Model_wse

let vtyp_to_type = function
  | A.VTbool       -> W.Tbool
  | A.VTint        -> W.Tint
  | A.VTuint       -> W.Tuint
  | A.VTrational   -> W.Trational
  | A.VTdate       -> W.Tdate
  | A.VTduration   -> W.Tduration
  | A.VTstring     -> W.Tstring
  | A.VTaddress    -> W.Taddress
  | A.VTrole       -> W.Trole
  | A.VTcurrency c -> W.Tcurrency c
  | A.VTkey        -> W.Tkey

let rec to_type = function
  | M.FBasic v              -> vtyp_to_type v
  | M.FAssetKeys (v, a)     -> W.Tcontainer (vtyp_to_type v)
  | M.FAssetRecord (v, a)   -> W.Tmap (vtyp_to_type v, Trecord (unloc a))
  | M.FRecordCollection a   -> W.Tcontainer (Trecord (unloc a))
  | M.FRecord a             -> W.Trecord (unloc a)
  | M.FEnum a               -> W.Tenum (unloc a)
  | M.FContainer (_, t)     -> W.Tcontainer (to_type t)

let to_expr (e : A.pterm) : W.expr =
  match e.node with
  | A.Plit {node = BVint v; }     -> W.Elitint v
  | A.Plit {node = BVaddress v; } -> W.Elitraw v
  | _ -> assert false

let rec ptyp_to_type = function
  | A.Tasset a          -> W.Trecord (unloc a)
  | A.Tenum e           -> W.Tenum (unloc e)
  | A.Tcontract c       -> W.Tcontract (unloc c)
  | A.Tbuiltin v        -> vtyp_to_type v
  | A.Tcontainer (t, _) -> W.Tcontainer (ptyp_to_type t)
  | A.Ttuple l          -> W.Ttuple (List.map ptyp_to_type l)

let get_default_expr_from_type = function
  | M.FBasic VTbool       -> W.Elitbool false
  | M.FBasic VTint        -> W.Elitint Big_int.zero_big_int
  | M.FBasic VTuint       -> W.Elitint Big_int.zero_big_int
  | M.FBasic VTrational   -> W.Elitint Big_int.zero_big_int
  | M.FBasic VTdate       -> W.Elitint Big_int.zero_big_int
  | M.FBasic VTduration   -> W.Elitint Big_int.zero_big_int
  | M.FBasic VTstring     -> W.Elitstring ""
  | M.FBasic VTaddress    -> assert false
  | M.FBasic VTrole       -> assert false
  | M.FBasic VTcurrency _ -> W.Elitraw "0tz"
  | M.FBasic VTkey        -> W.Elitint Big_int.zero_big_int
  | M.FAssetKeys (k, a)   -> W.Earray []
  | M.FAssetRecord (k, a) -> W.Elitmap (vtyp_to_type k, Trecord (unloc a))
  | M.FRecordCollection a -> assert false
  | M.FRecord _           -> assert false
  | M.FEnum _             -> assert false
  | M.FContainer (c, i)   -> assert false

let mk_function_struct (f : M.function__) =
  let name : ident = M.function_name_from_function_node f.node in
  let kind, args, ret, body =
    match f.node with
    | M.Entry _ ->
      let args = [("params", W.Tunit); ("s", W.Tstorage) ] in
      let ret  = W.Ttuple [W.Toperations; W.Tstorage] in
      let body = W.Etuple [W.Earray []; W.Evar "s"] in
      W.Entry, args, ret, body

    | M.Get asset ->
      let asset_name = unloc asset in
      let args = [("s", W.Tstorage); ("key", W.Tstring) ] in
      let ret  = W.Trecord asset_name in
      let body = W.Evar "s" in
      W.Function, args, ret, body

    | _ ->
      let args = ["", W.Tunit] in
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
        let values : ident list = List.map (fun (x : M.enum_item) -> unloc x.name) e.values in
        let enum = W.mk_enum name ~values:values in
        accu @ [enum]
      | _ -> accu) [] model.decls in
  let records = List.fold_left (fun accu x ->
      match x with
      | M.TNrecord r ->
        let values : (ident * W.type_ * W.expr) list =
          List.map (fun (x : M.record_item) -> (unloc x.name, ptyp_to_type x.type_, W.Elitbool false)) r.values
        in
        accu @
        [W.mk_record (unloc r.name) ~values:values]
      | M.TNstorage s ->
        let values : (ident * W.type_ * W.expr) list =
          List.fold_left (fun accu (x : M.storage_item) ->
              accu @ List.map (fun (i : M.item_field) : (ident * W.type_ * W.expr) ->
                  (unloc i.name, to_type i.typ, (
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
        let func = mk_function_struct f in
        accu @ [func]
      | _ -> accu) [] model.decls in
  W.mk_model (unloc name) ~enums:enums ~records:records ~funs:funs
