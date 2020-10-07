open Model
open Gen_transform

let remove_operations_nil (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Massign(ValueAssign, _, Avarstore {pldesc = "operations"}, { node = (Mlitlist []) }) -> seq []
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let optimize (model, env : model * 'a) =
  let model =
    model
    |> remove_operations_nil
    |> flat_sequence
  in
  model, env
