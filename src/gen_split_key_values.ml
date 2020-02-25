open Tools
open Location
(* open Ident *)
open Model

(* let asset_keys an = an ^ "_keys" *)
let asset_assets an = an ^ "_assets"

let to_lident f lident =
  let l, v = deloc lident in
  mkloc l (f v)

(* let lident_asset_keys = to_lident asset_keys *)
let lident_asset_assets = to_lident asset_assets

let split_key_values (model : model) : model =
  let storage =
    List.fold_right (fun x accu ->
        match x.model_type with
        | MTasset an ->
          let an = dumloc an in
          let _k, t = Utils.get_asset_key model (unloc an) in
          (* let type_key = Tcontainer (Tbuiltin t, Collection) in *)
          (* let init_keys, init_assets = *)
          (* TODO: initialize with f.default value*)
          (*Marray [], Marray [] *)
          (* in *)
          (* let asset_keys =
             mk_storage_item (dumloc (asset_keys (unloc an)))
              (MTasset (unloc an))
              type_key
              (mk_mterm init_keys type_key)
              ~loc:x.loc
             in *)
          let type_asset = Tassoc (t, Tasset an) in
          let asset_assets =
            mk_storage_item (dumloc (asset_assets (unloc an)))
              (MTasset (unloc an))
              type_asset
              (mk_mterm (Marray []) type_asset)
              ~loc:x.loc
          in
          asset_assets::accu
        | _ -> x::accu)
      model.storage []
  in

  let rec f (ctx : ctx_model) (x : mterm) : mterm =
    match x.node with
    | Mselect (an, col, pred) ->
      let col = f ctx col in
      let pred = f ctx pred in
      let _k, t = Utils.get_asset_key model an in
      { x with node = Mselect (an, col, pred); type_ = Tcontainer (Tbuiltin t, Collection)}

    | Mhead (an, col, idx) ->
      let col = f ctx col in
      let idx = f ctx idx in
      let _k, t = Utils.get_asset_key model an in
      { x with node = Mhead (an, col, idx); type_ = Tcontainer (Tbuiltin t, Collection)}

    | Mtail (an, col, idx) ->
      let col = f ctx col in
      let idx = f ctx idx in
      let _k, t = Utils.get_asset_key model an in
      { x with node = Mtail (an, col, idx); type_ = Tcontainer (Tbuiltin t, Collection)}

    | Mletin (ids, init, _, body, o) ->
      let init = f ctx init in
      let body = f ctx body in
      { x with node = Mletin (ids, init, Some (init.type_), body, o); type_ = body.type_}

    | Mdotasset (e, i) ->
      let asset = Utils.get_asset_type e in
      let containers = Utils.get_asset_containers model asset in
      if List.exists (fun (pi, _pt, _pd) ->
          compare (i |> unloc) pi = 0) containers then
        let rec get_container_type = function
          | (pi,pt,_pd)::_tl
            when compare (i |> unloc) pi = 0 -> pt
          | _r::tl -> get_container_type tl
          | [] -> assert false in
        let ty = get_container_type containers in
        let pa = Utils.dest_container ty in
        mk_mterm (Mshallow (pa, { x with node = Mdotasset (f ctx e, i) })) ty
      else
        { x with node = Mdotasset (f ctx e, i) }



    | Mvarstorecol an ->
      (
        let _k, t = Utils.get_asset_key model (unloc an) in
        { x with node = Mcoltokeys (unloc an); type_ = Tcontainer (Tbuiltin t, Collection) }
      )
    | Mfor (id, col, body, lbl) ->

      let is_argument_plain_asset_collection (col : mterm) =
        let id =
          match col.node with
          | Mvarparam an -> Some an
          | _ -> None
        in

        match id, ctx.fs with
        | Some an, Some ({args = args; src = Endo }) ->
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
      let _k, t = Utils.get_asset_key model (unloc an) in

      let col = f ctx col in
      let body = f ctx body in
      let body =
        if is_argument_plain_asset_collection col
        then
          body
        else
          let key = mk_mterm (Mvarlocal id) (Tbuiltin t) in
          let get_node =
            match Utils.get_source_for model ctx col with
            | Some c -> Mgetfrommap (unloc an, key, c)
            | _ -> Mget (unloc an, key)
          in
          let get = mk_mterm get_node (Tasset an) in
          let body = mk_mterm (Mletin ([id], get, Some (Tasset an), body, None)) (body.type_) in
          body
      in
      { x with node =  Mfor (id, col, body, lbl) }
    | _ -> map_mterm (f ctx) x
  in

  let model = map_mterm_model f model in

  { model with
    storage = storage
  }
