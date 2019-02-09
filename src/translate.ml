open Location
open Model
open ParseTree

exception ModelError of string

let get_name_model (pt : ParseTree.model) : lident =
  let ptu = Location.unloc pt in
  match ptu with
  | Mmodel decls ->
    (let res = List.fold_left (fun acc i -> (
           let decl_u = Location.unloc i in
           match decl_u with
           | Dmodel id -> (match acc with
               | None -> Some (Location.unloc id)
               | _ -> raise (ModelError "only one name can be set to model."))
           | _ -> acc)) None decls
     in
      match res with
      | Some id -> (dumloc id)
      | _ -> raise (ModelError "no name for model found."))
  | _ -> raise (ModelError "only ParseTree.model can be translated into Model.model.")

let parseTree_to_model (pt : ParseTree.model) : Model.model =
  mkloc (loc pt) {
    name          = get_name_model pt;
    roles         = [];
    variables     = [];
    assets        = [];
    functions     = [];
    transactions  = [];
    stmachines    = [];
    enums         = [];
    spec          = None;
  }
