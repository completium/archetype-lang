open Tools
open Location
open Ident

module A = Ast
module M = Model
module W = Model_wse

let rec to_type = function
  | M.FBasic v              -> W.Tbuiltin v
  | M.FKeyCollection (a, v) -> assert false
  | M.FRecordMap a
  | M.FRecordCollection a
  | M.FRecord a             -> Trecord (unloc a)
  | M.FEnum a               -> Tenum (unloc a)
  | M.FContainer (_, t)     -> Tcontainer (to_type t)
let to_expr (e : A.pterm) : W.expr =

  match e.node with
  | A.Plit {node = BVint v; } -> W.Elitint v
  | A.Plit {node = BVaddress v; } -> W.Elitraw v
  | _ -> assert false

let remove_se (model : M.model) : W.model =
  let name = model.name in
  let records = List.fold_left (fun accu x ->
      match x with
      | M.TNstorage s ->
        let values : (ident * W.type_ * W.expr) list =
          List.fold_left (fun accu (x : M.storage_item) ->
              accu @ List.map (fun (i : M.item_field) : (ident * W.type_ * W.expr) ->
                  (unloc i.name, to_type i.typ, to_expr (Option.get i.default))) x.fields
            ) [] s
        in
        accu @
        [W.mk_record "storage" ~values:values]
      | _ -> accu
    ) [] model.decls in

  W.mk_model (unloc name) ~records:records
