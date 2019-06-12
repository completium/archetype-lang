open Tools
open Location

module A = Ast
module M = Model
module W = Model_wse

let rec to_type = function
  | M.FBasic v              -> W.Tbuiltin v
  | M.FKeyCollection (a, v) -> assert false
  | M.FRecordMap a
  | M.FRecordCollection a
  | M.FRecord a             -> Trecord a
  | M.FEnum a               -> Tenum a
  | M.FContainer (_, t)     -> Tcontainer (to_type t)

let to_expr (e : A.pterm) : W.expr =

  match e.node with
  | A.Plit v -> A.mk_sp (`Eexpr (A.Plit (A.mk_sp (v.node))))
  | _ -> assert false

let remove_se (model : M.model) : W.model =
  let name = model.name in
  let records = List.fold_left (fun accu x ->
      match x with
      | M.TNstorage s ->
        let values : (W.lident * W.type_ * W.expr) list =
          List.fold_left (fun accu (x : M.storage_item) ->
              accu @ List.map (fun (i : M.item_field) : (W.lident * W.type_ * W.expr) ->
                  (i.name, to_type i.typ, to_expr (Option.get i.default))) x.fields
            ) [] s
        in
        accu @
        [W.mk_record (dumloc "storage") ~values:values]
      | _ -> accu
    ) [] model.decls in

  W.mk_model name ~records:records
