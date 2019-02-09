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

let to_rexpr e =
  let value = unloc e in
  match value with
  | Eliteral l -> (
      match l with
      | Laddress a -> Raddress a
      | _ -> raise (ModelError "a") )
  | _ -> raise (ModelError "a")

let with_option f v =
  match v with
  | Some e -> Some (f e)
  | None -> None

let get_roles decls =
  List.fold_left ( fun acc i -> (
    let decl_u = Location.unloc i in
      match decl_u with
        | Drole (id, dv, _) -> {name = id; default = (with_option to_rexpr dv)}::acc
        | _ -> acc)) [] decls


let parseTree_to_model (pt : ParseTree.model) : Model.model =
  let ptu = Location.unloc pt in
  let decls = match ptu with
  | Mmodel decls -> decls
  | _ -> [] in

  mkloc (loc pt) {
    name          = get_name_model pt;
    roles         = get_roles decls;
    variables     = [];
    assets        = [];
    functions     = [];
    transactions  = [];
    stmachines    = [];
    enums         = [];
    spec          = None;
  }
