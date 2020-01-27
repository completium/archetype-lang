open Location
open Model
open Tools

let replace_update_by_assignment (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Mupdate (an, k, l) ->
      begin
        (* let asset = Utils.get_asset model an in *)
        let is_asset_name (pterm : mterm) an : bool =
          match pterm with
          | {type_ = Tasset asset_name} -> String.equal an (unloc asset_name)
          | _ -> false
        in

        let _, t = Utils.get_asset_key model an in

        let type_asset = Tasset (dumloc an) in
        let type_container_asset = Tcontainer (type_asset, Collection) in

        let var_name = dumloc (an ^ "_") in
        let var_mterm : mterm = mk_mterm (Mvarlocal var_name) type_asset in

        (* let asset_mterm : mterm = mk_mterm (Mvarstorecol (dumloc (asset_name))) type_container_asset in *)

        let asset_aaa =
          match k.node with
          | Mdotasset (a, _) when is_asset_name a an -> Some a
          | _ -> None
        in

        let key_name = "k_" in
        let key_loced : lident = dumloc (key_name) in
        let key_mterm : mterm =
          match asset_aaa with
          | Some _ -> k
          | _ ->
            mk_mterm (Mvarlocal key_loced) type_container_asset
        in

        (* let set_mterm : mterm = mk_mterm (Mset (an, List.map (fun (id, _, _) -> unloc id) l, key_mterm, var_mterm)) Tunit in

           let lref : (Ident.ident * (assignment_operator * mterm)) list = List.map (fun (x, y, z) -> (unloc x, (y, z))) l in
           let lassetitems =
           List.fold_left (fun accu (x : asset_item) ->
              let v = List.assoc_opt (unloc x.name) lref in
              let type_ = x.type_ in
              let var = mk_mterm (Mdotasset (var_mterm, x.name)) type_ in
              match v with
              | Some y ->
                accu @ [
                  let value = snd y in
                  match y |> fst with
                  | ValueAssign -> value
                  | PlusAssign  -> mk_mterm (Mplus (var, value)) type_
                  | MinusAssign -> mk_mterm (Mminus (var, value)) type_
                  | MultAssign  -> mk_mterm (Mmult (var, value)) type_
                  | DivAssign   -> mk_mterm (Mdiv (var, value)) type_
                  | AndAssign   -> mk_mterm (Mand (var, value)) type_
                  | OrAssign    -> mk_mterm (Mor (var, value)) type_
                ]
              | _ -> accu @ [var]
            ) [] asset.values in
           let asset : mterm = mk_mterm (Masset lassetitems) type_asset in

           let letinasset : mterm = mk_mterm (Mletin ([var_name],
                                                   asset,
                                                   Some (type_asset),
                                                   set_mterm,
                                                   None
                                                  )) Tunit in *)

        let ll = List.map (
            fun (id, op, v) ->
              mk_mterm (Massignfield (op, Tunit, var_mterm, id, v)) Tunit
          ) l in
        let seq_node = Mseq ll in
        let seq = mk_mterm seq_node Tunit in


        let get_mterm : mterm =
          match asset_aaa with
          | Some a -> a
          | _ ->
            mk_mterm (Mget (an, key_mterm)) type_asset
        in

        let letinasset : mterm = mk_mterm (Mletin ([var_name],
                                                   get_mterm,
                                                   Some (type_asset),
                                                   seq,
                                                   None
                                                  ))
            Tunit in

        let res : mterm__node =
          match asset_aaa with
          | Some _ -> letinasset.node
          | _ ->
            Mletin ([key_loced],
                    k,
                    Some (Tbuiltin t),
                    letinasset,
                    None
                   ) in

        mk_mterm res Tunit
      end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model
