open Tools
open Location
(* open Ident *)
open Model

let asset_keys an = an ^ "_keys"
let asset_assets an = an ^ "_assets"

let split_key_values (model : model) : model =
  let storage =
    List.fold_right (fun x accu ->
        match x with
        | {asset = Some an; _} as f ->
          let k, t = Utils.get_asset_key model an in
          let type_key = Tcontainer (Tbuiltin t, Collection) in
          let init_keys, init_assets =
            (* TODO: initialize with f.default value*)
            Marray [], Marray []
          in
          let asset_keys =
            mk_storage_item (dumloc (asset_keys (unloc an)))
              type_key
              (mk_mterm init_keys type_key)
              ~asset:an
              ~invariants:f.invariants
              ~loc:f.loc
          in
          let type_asset = Tassoc (t, Tasset an) in
          let asset_assets =
            mk_storage_item (dumloc (asset_assets (unloc an)))
              type_asset
              (mk_mterm init_assets type_asset)
              ~asset:an
              ~invariants:f.invariants
              ~loc:f.loc
          in
          asset_keys::asset_assets::accu
        | _ -> x::accu)
      model.storage []
  in

  { model with
    storage = storage
  }
