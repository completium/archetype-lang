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

      let assetv_str = dumloc (asset ^ "_") in
      let asset_var = mk_mterm (Mvarlocal assetv_str) type_asset in

      let key, key_type = Utils.get_asset_key model lasset in
      let asset_key : mterm = mk_mterm (Mdotasset (asset_var,dumloc key)) (Tbuiltin key_type) in

      let assets_var_name = dumloc ("assets_") in
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

      let res : mterm__node = Mletin ([assets_var_name], select, Some type_assets, for_, None) in
      mk_mterm res Tunit
    | _ -> map_mterm (internal_extend ctx) t in
  map_mterm_model_gen [] internal_extend model |>
  fun m ->  {
    m with
    functions = List.map (fun (f : function__) -> {
          f with
          spec = Option.map (fun (v : specification) -> {
                v with
                postconditions = List.map (fun (postcondition : postcondition) -> {
                      postcondition with
                      invariants =
                        List.fold_left (fun acc (inv : invariant) ->
                            if List.mem_assoc (unloc inv.label) !loop_ids then
                              let loop_id = List.assoc (unloc inv.label) !loop_ids in
                              let loop_inv = { inv with label = dumloc loop_id } in
                              acc @ [inv; loop_inv]
                            else acc @ [inv]
                          ) [] postcondition.invariants
                    }) v.postconditions
              }) f.spec
        }) m.functions
  }

let process_single_field_storage (model : model) : model =
  match model.storage with
  | [i] ->
    begin
      match i.id with
      | SIname name ->
        begin
          let storage_id = dumloc "_s" in
          let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
            match mt.node with
            | Mvarstorevar a when String.equal (unloc a) (unloc name) ->
              mk_mterm (Mvarlocal storage_id) mt.type_
            | Massign (op, a, v) when String.equal (unloc a) (unloc name) ->
              let vv = map_mterm (aux ctx) v in
              mk_mterm (Massign (op, storage_id, vv)) mt.type_
            | _ -> map_mterm (aux ctx) mt
          in
          map_mterm_model aux model
        end
      | SIstate -> model
    end
  | _   -> model

(* raises errors if direct update/add/remove to partitioned asset *)
let check_partition_access (env : Typing.env) (model : model) : model =
  let partitions = Utils.get_partitions model in
  let partitionned_assets =
    partitions
    |> List.map (fun (_,_,t) -> Utils.type_to_asset t)
    |> List.map unloc
  in
  let get_partitions a =
    List.fold_left (fun acc (_,f,t) ->
        if compare a (unloc (Utils.type_to_asset t)) = 0 then
          acc @ [f]
        else acc
      ) [] partitions in
  let emit_error loc a =
    Typing.Env.emit_error env (
      loc,
      Typing.AssetPartitionnedby (a, get_partitions a));
    true in
  (* woud need a model iterator here *)
  let raise_access_error () =
    let rec internal_raise (ctx : ctx_model) acc (t : mterm) =
      match t.node with
      | Maddasset (a, _) when List.mem a partitionned_assets -> emit_error t.loc a
      | Mremoveasset (a, _) when List.mem a partitionned_assets -> emit_error t.loc a
      | Mremoveif(a, { node = (Mvarstorecol _); loc = _}, _) when List.mem a partitionned_assets -> emit_error t.loc a
      | _ -> fold_term (internal_raise ctx) acc t
    in
    fold_model internal_raise model false in
  let _ = raise_access_error () in
  model

let prune_properties (model : model) : model =
  match !Options.opt_property_focused with
  | "" -> model
  | fp_id ->
    let p_ids =
      fp_id::
      (match Model.Utils.retrieve_property model fp_id with
       | Ppostcondition (p, _f_id) -> List.map unloc p.uses
       | _ -> [])
    in

    let p_funs =
      begin
        let all_funs =
          model.functions
          |> List.map (fun (x : function__) -> x.node)
          |> List.map ((function | Entry fs -> fs | Function (fs, _) -> fs))
          |> List.map (fun (x : function_struct) -> unloc x.name) in
        let add l x = if List.mem x l then l else x::l in
        List.fold_left (fun accu _p_id ->
            match Model.Utils.retrieve_property model fp_id with
            | Ppostcondition (_p, Some f_id) -> add accu f_id
            | Ppostcondition _     -> all_funs
            | PstorageInvariant _  -> all_funs
            | PsecurityPredicate _ -> all_funs
          ) [] p_ids
      end
    in

    let remain_id id = List.exists (String.equal id) p_ids in
    let remain_function id = List.exists (String.equal id) p_funs in
    let prune_mterm (mt : mterm) : mterm =
      let rec aux (mt : mterm) : mterm =
        match mt.node with
        | Mseq l ->
          let ll =
            l
            |> List.map aux
            |> List.filter (fun (x : mterm) -> match x.node with | Mlabel id -> remain_id (unloc id) | _ -> true)
          in
          { mt with node = Mseq ll }
        | _ -> map_mterm aux mt
      in
      aux mt
    in
    let prune_specs (spec : specification) : specification =
      { spec with
        postconditions = List.filter (fun (x : postcondition) -> remain_id (unloc x.name)) spec.postconditions
      } in
    let prune_function__ (f : function__) : function__ =
      let prune_function_node (fn : function_node) : function_node =
        let prune_function_struct (fs : function_struct) : function_struct =
          { fs with
            body = prune_mterm fs.body } in
        match fn with
        | Function (fs, r) -> Function (prune_function_struct fs, r)
        | Entry fs -> Entry (prune_function_struct fs)
      in
      { f with
        node = prune_function_node f.node;
        spec = Option.map prune_specs f.spec; }
    in
    let prune_secs (sec : security) : security =
      { sec with
        items = List.filter (fun (x : security_item) -> remain_id (unloc x.label)) sec.items
      } in
    let process_asset model : model =
      let prune_storage_item (model, s : model * storage_item) : model * storage_item =
        match s.asset with
        | Some an ->
          let model, invs =
            List.fold_left (fun (model, accu: model * lident label_term_gen list) (x : lident label_term_gen) ->
                if Option.is_some x.label && remain_id (unloc (Option.get x.label)) then
                  (model, accu @ [x])
                else
                  ({model with api_verif = model.api_verif @ [StorageInvariant ((unloc (Option.get x.label)), unloc an, x.term) ] }, accu)
              ) (model,[]) s.invariants in
          model, { s with invariants = invs}
        | _ -> (model, s)
      in
      let (model, storage) : model * storage_item list =
        List.fold_left_map
          (fun (state : model) (x : storage_item) ->
             prune_storage_item (state, x)
          ) model model.storage in
      {
        model with
        storage = storage;
      }
    in
    let model =
      model
      |> process_asset
    in
    let f1 = (fun (fs : function_struct) -> remain_function (unloc fs.name)) in
    let f2 = (fun (x : function__) -> match x.node with | Entry fs -> fs | Function (fs, _) -> fs) in
    { model with
      functions = List.map prune_function__ (List.filter (f1 |@ f2) model.functions);
      specification = prune_specs model.specification;
      security = prune_secs model.security
    }

let replace_declvar_by_letin (model : model) : model =
  let rec aux c (mt : mterm) : mterm =
    match mt.node with
    | Mseq l ->
      let ll = List.fold_right (fun (x : mterm) accu ->
          match x.node with
          | Mdeclvar (ids, t, v) ->
            begin
              let init = aux c v in
              let body =
                match accu with
                | [] -> assert false
                | [i] -> i
                | lll -> mk_mterm (Mseq accu) (List.last lll).type_
              in
              let res = mk_mterm (Mletin(ids, init, t, body, None)) body.type_ in
              [ res ]
            end
          | _ ->
            begin
              let t = aux c x in
              t::accu
            end
        ) l [] in
      { mt with node = Mseq ll }
    | Mdeclvar _ -> assert false
    | _ -> map_mterm (aux c) mt
  in
  Model.map_mterm_model aux model

let remove_get_dot (model : model) : model =
  let extract_get_dot (mt : mterm) : mterm * (lident * mterm) list =
    let rec aux (accu : (lident * mterm) list) (mt : mterm) : mterm * (lident * mterm) list =
      match mt.node with
      | Mdotasset ({node = Mget _ ; _ } as v , id) ->
        begin
          let var_id = dumloc "a" in (* TODO: get a fresh id *)
          let var = mk_mterm (Mvarlocal var_id) v.type_ in
          let new_mt = mk_mterm (Mdotasset (var, id)) mt.type_ in
          (new_mt, (var_id, v)::accu)
        end
      | _ ->
        let g (x : mterm__node) : mterm = { mt with node = x; } in
        Model.fold_map_term g aux accu mt
    in
    aux [] mt
  in
  let is_get_dot (mt : mterm) : bool =
    mt
    |> extract_get_dot
    |> snd
    |> List.is_not_empty
  in
  let rec aux c (mt : mterm) : mterm =
    match mt.node with
    | Mletin (ids, init, t, body, o) when is_get_dot init ->
      begin
        let (new_init, l) : mterm * (lident * mterm) list = extract_get_dot init in
        List.fold_right
          (fun (id, v) accu ->
             mk_mterm (Mletin ([id], v, Some v.type_, accu, None)) accu.type_
          ) l (mk_mterm (Mletin (ids, new_init, t, body, o)) mt.type_)
      end
    | _ -> map_mterm (aux c) mt
  in
  Model.map_mterm_model aux model
