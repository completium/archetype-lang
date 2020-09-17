open Location
open Tools

open ParseTree

module MapString = Map.Make(String)

let remove_spec_decl (pt : archetype) : archetype =

  let for_spec_gen map pred mapper (ds : declaration list) =
    ds
    |> List.map (mapper map)
    |> (fun l -> List.fold_right (fun x accu -> if pred x then accu else x::accu) l [])
  in

  let for_asset (ds : declaration list) =
    let map = List.fold_left (fun accu (d : declaration) ->
        match unloc d with
        | Dspecasset (id, l) -> MapString.add (unloc id) l accu
        | _ -> accu ) MapString.empty ds
    in

    let for_decl map (decl : declaration) : declaration =
      match unloc decl with
      | Dasset (a, b, c, d, e, f, g) when MapString.mem (unloc a) map ->
        { decl with pldesc = Dasset (a, b, c, d, (APOconstraints (MapString.find (unloc a) map))::e, f, g)}
      | _ -> decl
    in

    let is_specvar decl =
      match unloc decl with
      | Dspecasset _ -> true
      | _ -> false
    in
    for_spec_gen map is_specvar for_decl ds
  in

  let for_variable (ds : declaration list) =
    let map = List.fold_left (fun accu (d : declaration) ->
        match unloc d with
        | Dspecvariable (id, l) -> MapString.add (unloc id) l accu
        | _ -> accu ) MapString.empty ds
    in

    let for_decl map (decl : declaration) : declaration =
      match unloc decl with
      | Dvariable (a, b, c, d, e, f) when MapString.mem (unloc a) map ->
        { decl with pldesc = Dvariable (a, b, c, d, e , f)}
      | _ -> decl
    in

    let is_specvar decl =
      match unloc decl with
      | Dspecvariable _ -> true
      | _ -> false
    in
    for_spec_gen map is_specvar for_decl ds
  in

  let for_entry (ds : declaration list) =
    let map = List.fold_left (fun accu (d : declaration) ->
        match unloc d with
        | Dspecfun (_, id, _, s) -> MapString.add (unloc id) s accu
        | _ -> accu ) MapString.empty ds
    in

    let for_decl map (decl : declaration) : declaration =
      match unloc decl with
      | Dentry (a, b, c, d, e) when MapString.mem (unloc a) map ->
        let c = { c with spec_fun = Some (MapString.find (unloc a) map)} in
        { decl with pldesc = Dentry (a, b, c, d, e) }
      | Dfunction a when MapString.mem (unloc a.name) map ->
        { decl with pldesc =  Dfunction {a with spec = Some (MapString.find (unloc a.name) map)} }
      | _ -> decl
    in

    let is_specvar decl =
      match unloc decl with
      | Dspecfun _ -> true
      | _ -> false
    in

    for_spec_gen map is_specvar for_decl ds
  in

  match unloc pt with
  | Marchetype ds -> { pt with pldesc = Marchetype (ds |> for_variable |> for_asset |> for_entry)}
  | _ -> pt
