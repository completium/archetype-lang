open Location
open Model
open Tools

let remove_label (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Mlabel _ -> mk_mterm (Mseq []) Tunit
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let flat_sequence (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Mseq l ->
      begin
        match l with
        | [] -> mt
        | [e] -> e
        | l ->
          let l = List.fold_right (fun (x : mterm) accu ->
              match x.node with
              | Mseq [] -> accu
              | _ -> x::accu) l [] in
          begin
            match l with
            | [] -> mk_mterm (Mseq []) Tunit
            | [e] -> e
            | _ -> mk_mterm (Mseq l) (List.last l).type_
          end
      end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let replace_lit_address_by_role (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Maddress _ as node -> mk_mterm node (Tbuiltin Brole)
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

(* transforms vars "to_iter" and "itereated" to M.toiterated and M.iterated
   with iterated collection as argument
   it works if loop lables are unique over the contract
*)
let extend_loop_iter (model : model) : model =
  let get_for_collections () =
    let rec internal_get_for (ctx : ctx_model) acc (t : mterm) =
      match t.node with
      | Mfor (_, c, _, Some id) -> acc @ [id,c]
      | _ -> fold_term (internal_get_for ctx) acc t
    in
    fold_model internal_get_for model [] in
  let for_colls = get_for_collections () in
  let map_invariant_iter () =
    let rec internal_map_inv_iter (ctx : ctx_model) (t : mterm) : mterm =
      let mk_term const =
        let loop_id = Tools.Option.get ctx.invariant_id |> unloc in
        if List.mem_assoc loop_id for_colls then
          let coll = List.assoc loop_id for_colls in
          match const with
          | `Toiterate -> mk_mterm (Msettoiterate coll) (coll.type_)
          | `Iterated ->  mk_mterm (Msetiterated coll) (coll.type_)
        else
          t in
      match t.node with
      | Mvarlocal v when cmp_lident v (dumloc "toiterate") -> mk_term `Toiterate
      | Mvarlocal v when cmp_lident v (dumloc "iterated") -> mk_term `Iterated
      | _ -> map_mterm (internal_map_inv_iter ctx) t in
    map_mterm_model internal_map_inv_iter model in
  map_invariant_iter ()

type loop_ctx = (lident, Ident.ident list) ctx_model_gen

let extend_removeif (model : model) : model =
  let loop_ids = ref [] in
  let idx = ref 0 in
  let rec internal_extend (ctx : loop_ctx) (t : mterm) : mterm =
    match t.node with
    | Mfor (i, c, b, Some lbl) ->
      let new_ctx = { ctx with custom = ctx.custom @ [lbl] } in
      mk_mterm (Mfor (i,
                      internal_extend new_ctx c,
                      internal_extend new_ctx b,
                      Some lbl)) t.type_
    | Mremoveif (asset, p, q) ->
      let lasset = dumloc asset in
      let type_asset = Tasset lasset in

      let assetv_str = dumloc ("_" ^ asset) in
      let asset_var = mk_mterm (Mvarlocal assetv_str) type_asset in

      let key, key_type = Utils.get_asset_key model lasset in
      let asset_key : mterm = mk_mterm (Mdotasset (asset_var,dumloc key)) (Tbuiltin key_type) in

      let assets_var_name = dumloc ("_assets") in
      let type_assets = Tcontainer (Tasset lasset, Collection) in
      let assets_var = mk_mterm (Mvarlocal assets_var_name) type_assets in

      let select : mterm =  mk_mterm (Mselect (asset, p, q) ) type_asset in

      let remove : mterm = mk_mterm (Mremoveasset (asset, asset_key)) Tunit in

      let id = "removeif_loop" ^ (string_of_int !idx) in
      idx := succ !idx;
      if List.length ctx.custom > 0 then
        let upper_id = List.hd (List.rev ctx.custom) in
        loop_ids := !loop_ids @ [upper_id,id]
      else ();

      let for_ = mk_mterm (Mfor (assetv_str, assets_var, remove, Some id)) Tunit in

      let res : mterm__node = Mletin ([assets_var_name], select, Some type_assets, for_) in
      mk_mterm res Tunit
    | _ -> map_mterm (internal_extend ctx) t in
  map_mterm_model_gen [] internal_extend model |>
  fun m ->  {
    m with
    functions = List.map (fun (f : function__) -> {
          f with
          verif = Option.map (fun (v : verification) -> {
                v with
                specs = List.map (fun (spec : specification) -> {
                      spec with
                      invariants =
                        List.fold_left (fun acc (inv : invariant) ->
                            if List.mem_assoc (unloc inv.label) !loop_ids then
                              let loop_id = List.assoc (unloc inv.label) !loop_ids in
                              let loop_inv = { inv with label = dumloc loop_id } in
                              acc @ [inv; loop_inv]
                            else acc @ [inv]
                          ) [] spec.invariants
                    }) v.specs
              }) f.verif
        }) m.functions
  }

let process_single_field_storage (model : model) : model =
  match model.storage with
  | [i] ->
    let field_name = unloc i.name in
    let storage_id = dumloc "_s" in
    let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
      match mt.node with
      | Mvarstorevar a when String.equal (unloc a) field_name ->
        mk_mterm (Mvarlocal storage_id) mt.type_
      | Massign (op, a, v) when String.equal (unloc a) field_name ->
        let vv = map_mterm (aux ctx) v in
        mk_mterm (Massign (op, storage_id, vv)) mt.type_
      | _ -> map_mterm (aux ctx) mt
    in
    map_mterm_model aux model
  | _   -> model
