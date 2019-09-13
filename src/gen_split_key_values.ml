open Tools
open Location
(* open Ident *)
open Model

let asset_keys an = an ^ "_keys"
let asset_assets an = an ^ "_assets"

let to_lident f lident =
  let l, v = deloc lident in
  mkloc l (f v)

let lident_asset_keys = to_lident asset_keys
let lident_asset_assets = to_lident asset_assets

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

  let rec f (ctx : ctx_model) (x : mterm) : mterm =
    match x.node with
    | Mselect (an, col, pred) ->
      let col = f ctx col in
      let pred = f ctx pred in
      let k, t = Utils.get_asset_key model (dumloc an) in
      { x with node = Mselect (an, col, pred); type_ = Tcontainer (Tbuiltin t, Collection)}

    | Mletin (ids, init, _, body) ->
      let init = f ctx init in
      let body = f ctx body in
      { x with node = Mletin (ids, init, Some (init.type_), body); type_ = body.type_}

    | Mdotasset (e, i) ->
      let asset = Utils.get_asset_type e in
      let partitions = Utils.get_asset_partitions model (asset |> unloc) in
      if List.exists (fun (pi, pt, pd) ->
          compare (i |> unloc) pi = 0) partitions then
        let rec get_partition_type = function
          | (pi,pt,pd)::tl
            when compare (i |> unloc) pi = 0 -> pt
          | r::tl -> get_partition_type tl
          | [] -> assert false in
        let ty = get_partition_type partitions in
        let pa = Utils.dest_partition ty |> unloc in
        mk_mterm (Mshallow (pa, { x with node = Mdotasset (f ctx e, i) })) ty
      else
        { x with node = Mdotasset (f ctx e, i) }



    | Mvarstorecol an ->
      (
        let k, t = Utils.get_asset_key model an in
        { x with node = Mvarstorecol (lident_asset_keys an); type_ = Tcontainer (Tbuiltin t, Collection) }
      )
    | Mfor (id, col, body, lbl) ->

      let is_argument_plain_asset_collection (col : mterm) =
        let id =
          match col.node with
          | Mvarparam an -> Some an
          | _ -> None
        in

        match id, ctx.fs with
        | Some an, Some ({args = args }) ->
          List.fold_left (fun accu (name, type_, _) ->
              match type_ with
              | Tcontainer (Tasset _, _) when String.equal (unloc an) (unloc name) -> true
              | _ -> accu
            ) false args
        | _ -> false
      in

      let an =
        match col.type_ with
        | Tcontainer (Tasset an, _) -> an
        | _ -> assert false
      in
      let k, t = Utils.get_asset_key model an in

      let col = f ctx col in
      let body = f ctx body in
      let body =
        if is_argument_plain_asset_collection col
        then
          body
        else
          let key = mk_mterm (Mvarlocal id) (Tbuiltin t) in
          let get = mk_mterm (Mget (unloc an, key)) (Tasset an) in
          let body = mk_mterm (Mletin ([id], get, Some (Tasset an), body)) (body.type_) in
          body
      in
      { x with node =  Mfor (id, col, body, lbl) }
    | _ -> map_mterm (f ctx) x
  in

  let model = map_mterm_model f model in

  { model with
    storage = storage
  }
