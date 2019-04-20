open Location
open Model

let sanity_check model : Model.model =
  let _check_dv model : Model.model =
    (model
     |> unloc
     |> (fun x -> x.variables)
     |> List.iter (fun v ->
         let d = v.decl in
         match d.typ, d.default with
         (*       | Some ({pldesc=Tbuiltin VTaddress; _}), None -> raise (Modelinfo.DefaultValueAssignment d.name)*)
         | _ -> ()));
    model in
  model
(*  |> _check_dv*)

let reduce_model (model : Model.model) =
model
|> sanity_check
