open Location
open ParseTree
open Omd

let to_str lident = Location.unloc lident

let decl_to_element d : Omd.element option=
  match unloc d with
  | Darchetype (i, _)                  -> Some (H1 [Text (to_str i)])
  | Dvariable (id, _, _, _, _, _)      -> Some (H2 [Text ("todo: variable: " ^ (to_str id))])
  | Denum (EKenum id, _, _)            -> Some (H2 [Text ("todo: enum: " ^ (to_str id))])
  | Denum (EKstate, _, _)              -> Some (H2 [Text ("todo: states")])
  | Dasset (id, _, _, _, _, _)         -> Some (H2 [Text ("todo: asset: " ^ (to_str id))])
  | Daction (id, _, _, _, _)           -> Some (H2 [Text ("todo: action: " ^ (to_str id))])
  | Dtransition (id, _, _, _, _, _, _) -> Some (H2 [Text ("todo: transition: " ^ (to_str id))])
  | Dcontract (id, _, _, _)            -> Some (H2 [Text ("todo: contract: " ^ (to_str id))])
  | Dfunction f                        -> Some (H2 [Text ("todo: function: " ^ (to_str f.name))])
  | _ -> None


let pt_to_ast_omd (pt : archetype) : Omd.t =
  match unloc pt with
  | Marchetype ds -> List.fold_left (fun accu i ->
      let item = decl_to_element i in
      match item with
      | Some v -> accu @ [v]
      | None -> accu
    ) [] ds
  | _ -> []


let pt_to_md_output (pt : archetype) : string =
  let ast = pt_to_ast_omd pt in
  Omd.to_markdown ast
