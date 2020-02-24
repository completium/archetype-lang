open Ident
open Location
open Model
open Tools

type error_desc =
  | AssetPartitionnedby of string * string list
  | CannotBuildAsset of string * string

let pp_error_desc fmt = function
  | AssetPartitionnedby (i, l)         ->
    Format.fprintf fmt
      "Cannot access asset collection: asset %s is partitionned by field(s) (%a)"
      i (Printer_tools.pp_list ", " Printer_tools.pp_str) l

  | CannotBuildAsset (an, fn) ->
    Format.fprintf fmt "Cannot build an asset %s, default value of field '%s' is missing" an fn

type error = Location.t * error_desc

let emit_error (lc, error) =
  let str : string = Format.asprintf "%a@." pp_error_desc error in
  let pos : Position.t list = [location_to_position lc] in
  Error.error_alert pos str (fun _ -> ())

let remove_add_update (model : model) : model =
  let error = ref false in
  let f_error (l : Location.t) (an : string) (fn : string) = emit_error(l, CannotBuildAsset (an, fn)); error := true in
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Maddupdate (an, k, l) ->
      begin
        let type_asset = Tasset (dumloc an) in
        let mk_asset (an, k, l) =
          let dummy_mterm = mk_mterm (Mseq []) Tunit in
          let asset = Utils.get_asset model an in
          let lref : (Ident.ident * (assignment_operator * mterm * Location.t)) list = List.map (fun (x, y, z) -> (unloc x, (y, z, loc x))) l in
          let l = List.map (
              fun (f : asset_item) ->
                let f_name = (unloc f.name) in
                if String.equal asset.key f_name
                then k
                else
                  begin
                    let op, v, lo =
                      match List.assoc_opt f_name lref with
                      | Some v -> v
                      | None ->
                        begin
                          match f.default with
                          | Some v -> (ValueAssign, v, Location.dummy)
                          | _ -> f_error mt.loc an f_name; (ValueAssign, dummy_mterm, Location.dummy)
                        end
                    in
                    match op with
                    | ValueAssign -> v
                    | _ ->
                      begin
                        let dv =
                          match f.default with
                          | Some v -> v
                          | _ -> f_error lo an f_name; mk_mterm (Mseq []) Tunit
                        in
                        let type_ = dv.type_ in
                        match op with
                        | PlusAssign  -> mk_mterm (Mplus (dv, v)) type_
                        | MinusAssign -> mk_mterm (Mminus (dv, v)) type_
                        | MultAssign  -> mk_mterm (Mmult (dv, v)) type_
                        | DivAssign   -> mk_mterm (Mdiv (dv, v)) type_
                        | AndAssign   -> mk_mterm (Mand (dv, v)) type_
                        | OrAssign    -> mk_mterm (Mor (dv, v)) type_
                        | _ -> f_error lo an f_name; dummy_mterm
                      end
                  end
            ) asset.values in
          mk_mterm (Masset l) type_asset
        in
        let col    = mk_mterm (Mvarstorecol (dumloc an)) (Tcontainer (type_asset, Collection)) in
        let cond   = mk_mterm (Mcontains (an, col, k)) Tunit in
        let asset  = mk_asset (an, k, l) in
        let add    = mk_mterm (Maddasset (an, asset)) Tunit in
        let update = mk_mterm (Mupdate (an, k, l)) Tunit in
        let if_node = Mif (cond, update, Some add) in
        mk_mterm if_node Tunit
      end
    | _ -> map_mterm (aux ctx) mt
  in
  let res = map_mterm_model aux model in
  if !error
  then raise (Error.Stop 5)
  else res

(* myasset.update k {f1 = v1; f2 = v2}

   let _k = k in
   let _myasset = myasset.get _k in
   let _myasset = {id = _myasset.id; f1 = v1; f2 = v2} in
   set_myasset s _k _myasset *)

let replace_update_by_set (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Mupdate (an, k, l) ->
      begin
        let asset = Utils.get_asset model an in
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

        let set_mterm : mterm = mk_mterm (Mset (an, List.map (fun (id, _, _) -> unloc id) l, key_mterm, var_mterm)) Tunit in

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
                                                  )) Tunit in

        (* let seq : mterm list = (List.map (fun ((id, op, term) : ('a * A.operator * 'c)) -> mk_mterm
                                              (Massignfield (to_assign_operator op, var_name, id, term))
                                              Tunit
                                          ) e) @ [set_mterm] in *)

        (* let body : mterm = mk_mterm (Mseq seq) Tunit in *)

        let get_mterm : mterm =
          match asset_aaa with
          | Some a -> a
          | _ ->
            mk_mterm (Mget (an, key_mterm)) type_asset
        in

        let letinasset : mterm = mk_mterm (Mletin ([var_name],
                                                   get_mterm,
                                                   Some (type_asset),
                                                   letinasset,
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

let remove_label (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Mlabel _ -> mk_mterm (Mseq []) Tunit
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let rec flat_sequence_mterm (mt : mterm) =
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
  | _ -> map_mterm flat_sequence_mterm mt

let flat_sequence (model : model) : model =
  let aux (_ctx : ctx_model) (mt : mterm) : mterm =
    flat_sequence_mterm mt
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

      let key, key_type = Utils.get_asset_key model (unloc lasset) in
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
      let storage_id = dumloc "_s" in
      let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
        match mt.node with
        | Mvarstorevar a when String.equal (unloc a) (unloc i.id) ->
          mk_mterm (Mvarlocal storage_id) mt.type_
        | Massign (op, t, a, v) when String.equal (unloc a) (unloc i.id) ->
          let vv = map_mterm (aux ctx) v in
          mk_mterm (Massign (op, t, storage_id, vv)) mt.type_
        | _ -> map_mterm (aux ctx) mt
      in
      map_mterm_model aux model
    end
  | _   -> model

(* raises errors if direct update/add/remove to partitioned asset *)
let check_partition_access (model : model) : model =
  let partitions = Utils.get_partitions model in
  let partitionned_assets =
    partitions
    |> List.map (fun (_,_,t) -> Utils.type_to_asset t)
  in
  let get_partitions a =
    List.fold_left (fun acc (_,f,t) ->
        if compare a (Utils.type_to_asset t) = 0 then
          acc @ [f]
        else acc
      ) [] partitions in
  let emit_error loc a =
    emit_error (
      loc,
      AssetPartitionnedby (a, get_partitions a));
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
    let with_error = fold_model internal_raise model false in
    if with_error
    then raise (Error.Stop 5)
  in
  let _ = raise_access_error () in
  model

let prune_properties (model : model) : model =
  match !Options.opt_property_focused with
  | "" -> model
  | fp_id ->
    let _is_inv, uses =
      (match Model.Utils.retrieve_property model fp_id with
       | PstorageInvariant _ -> true, []
       | Ppostcondition (p, _f_id) -> false, List.map unloc p.uses
       | _ -> false, [])
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
          ) [] (fp_id::uses)
      end
    in

    let api_verifs : api_verif list =
      (* let is_api_verif id = List.exists (String.equal id) uses in *)
      let props = Model.Utils.retrieve_all_properties model in
      List.fold_left (fun accu (label, prop) ->
          match prop with
          | PstorageInvariant ({term = formula; _}, an) -> StorageInvariant (label, an, formula)::accu
          | _ -> accu
        ) [] props
    in

    let remain_id id = String.equal id fp_id in
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
    let prune_decl = function
      | Dasset r -> Dasset {r with invariants = List.filter (fun (x : label_term) -> remain_id (unloc (x.label))) r.invariants }
      | Denum e ->
        begin
          let values = List.map (
              fun (x : enum_item) ->
                {x with invariants = List.filter (fun (x : label_term) -> remain_id (unloc (x.label))) x.invariants }
            ) e.values in
          Denum { e with values = values; }
        end
      | _ as x -> x
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
    let f1 = (fun (fs : function_struct) -> remain_function (unloc fs.name)) in
    let f2 = (fun (x : function__) -> match x.node with | Entry fs -> fs | Function (fs, _) -> fs) in
    { model with
      api_verif = api_verifs;
      decls = List.map prune_decl model.decls;
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

let assign_loop_label (model : model) : model =
  let loop_labels = ref
      begin
        let rec aux ctx accu (mt : mterm) : Ident.ident list =
          match mt.node with
          | Mfor (_, col, body, Some label) ->
            begin
              let accu = fold_term (aux ctx) accu col in
              let accu = fold_term (aux ctx) accu body in
              label::accu
            end
          | Miter (_, min, max, body, Some label) ->
            begin
              let accu = fold_term (aux ctx) accu min in
              let accu = fold_term (aux ctx) accu max in
              let accu = fold_term (aux ctx) accu body in
              label::accu
            end
          | _ -> fold_term (aux ctx) accu mt
        in
        fold_model aux model []
      end
  in

  let get_loop_label ctx : Ident.ident =
    let prefix =
      "loop_" ^
      (match ctx.fs, ctx.spec_id, ctx.label, ctx.invariant_id with
       | Some fs, _, _, _ -> unloc fs.name
       | _, Some a, _, _
       | _, _, Some a, _
       | _, _, _, Some a  -> unloc a
       | _ -> assert false)
      ^ "_"
    in
    let n = ref 0 in
    begin
      while List.mem (prefix ^ (string_of_int !n)) !loop_labels do
        n := !n + 1
      done;
      let res = prefix ^ (string_of_int !n) in
      loop_labels := res::!loop_labels;
      res
    end
  in


  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mfor (a, col, body, None) ->
      begin
        let ncol  = map_mterm (aux ctx) col in
        let nbody = map_mterm (aux ctx) body in
        let label = get_loop_label ctx in
        { mt with node = Mfor (a, ncol, nbody, Some label)}
      end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let remove_enum_matchwith (model : model) : model =
  let type_int = Tbuiltin Bint in
  let type_bool = Tbuiltin Bbool in
  let mk_enum_ident a b = String.lowercase_ascii (a ^ "_" ^ b) in
  let process_decls decls =
    let process_enum (enum : enum) =
      let mk_const id i =
        let id_loc, id_v = deloc id in
        let ty_v = unloc enum.name in
        let name = mk_enum_ident ty_v id_v in
        Dvar (
          mk_var
            (dumloc name)
            type_int
            type_int
            ~constant:true
            ~default:(mk_mterm (Mint (Big_int.big_int_of_int i)) type_int)
            ~loc:id_loc
        ) in
      List.mapi (fun i (x : enum_item) ->
          mk_const x.name i
        ) enum.values
    in
    List.fold_left (fun accu x ->
        accu @
        (
          match x with
          | Denum v -> process_enum v
          | _ -> [x]
        )
      ) [] decls
  in
  let rec process_mterm ctx (mt : mterm) : mterm =
    let mk_id (prefix : string) (id : lident) : lident =
      mkloc (loc id) (mk_enum_ident prefix (unloc id)) in
    match mt.node, mt.type_ with
    | Mvarlocal id, Tstate -> mk_mterm (Mvarlocal (mk_id "state" id)) type_int
    | Mvarlocal id, Tenum e -> mk_mterm (Mvarlocal (mk_id (unloc e) id)) type_int
    | Mmatchwith (v, ps), _ ->
      let v = process_mterm ctx v in
      let type_v = v.type_ in
      let val_v =
        match type_v with
        | Tstate -> "state"
        | Tenum v -> unloc v
        | _ -> assert false
      in
      begin
        let default_ = mk_mterm (Mseq []) Tunit in
        let else_ = List.fold_left (fun accu (x : (pattern * mterm)) ->
            match x with
            | {node = Pwild; _}, e -> process_mterm ctx e
            | _ -> accu) default_ ps in
        List.fold_right (fun (x : (pattern * mterm)) accu ->
            let mk_cond id =
              begin
                let val_enum id = mk_mterm (Mvarlocal (mk_id val_v id)) type_v in
                mk_mterm (Mequal (v, val_enum id)) type_bool
              end
            in
            let mk_if cond then_ else_ = mk_mterm (Mif (cond, then_, Some else_)) Tunit in
            match x with
            | {node = Pconst id; _}, e ->
              begin
                let e = process_mterm ctx e in
                let cond = mk_cond id in
                mk_if cond e accu
              end
            | _ -> accu
          ) ps else_
      end
    | _ -> map_mterm (process_mterm ctx) mt
  in
  { model with
    decls = process_decls model.decls
  }
  |> map_mterm_model process_mterm

let remove_wild_pattern (model : model) : model =
  let rec aux c (mt : mterm) : mterm =
    match mt.node with
    | Mmatchwith (e, l) ->
      let e = aux c e in
      let l = List.map (fun (x, y) -> x, aux c y) l in

      let pl : (string * mterm) list =
        begin
          let values : string list =
            begin
              let enum : enum =
                match e.type_ with
                | Tstate   -> Model.Utils.get_enum model "state"
                | Tenum id -> Model.Utils.get_enum model (unloc id)
                | _ -> assert false
              in
              enum.values
              |> List.map (fun (x : enum_item) -> unloc x.name)
            end
          in
          let mterm_default : mterm option =
            List.fold_left (
              fun accu (p, e : pattern * mterm) ->
                match p.node with
                | Pwild -> Some e
                | _ -> accu
            ) None l in
          let seek_mterm x =
            List.fold_left (
              fun accu (p, e : pattern * mterm) ->
                match p.node with
                | Pconst id when String.equal (Location.unloc id) x -> Some e
                | _ -> accu
            ) None l in
          List.map (fun x ->
              let e = seek_mterm x in
              match e with
              | Some e -> (x, e)
              | None -> x, Option.get mterm_default
            ) values
        end
      in
      let l = List.map (fun (id, e) -> mk_pattern (Pconst (dumloc id)), e) pl in
      mk_mterm (Mmatchwith (e, l)) mt.type_
    | _ -> map_mterm (aux c) mt
  in
  Model.map_mterm_model aux model

let remove_cmp_bool (model : model) : model =
  let rec aux c (mt : mterm) : mterm =
    match mt.node with
    | Mequal (lhs, {node = Mbool true; _})  -> lhs
    | Mequal (lhs, {node = Mbool false; _}) -> mk_mterm (Mnot lhs) (Tbuiltin Bbool)
    | Mequal ({node = Mbool true; _}, rhs)  -> rhs
    | Mequal ({node = Mbool false; _}, rhs) -> mk_mterm (Mnot rhs) (Tbuiltin Bbool)

    | Mnequal (lhs, {node = Mbool true; _})  -> mk_mterm (Mnot lhs) (Tbuiltin Bbool)
    | Mnequal (lhs, {node = Mbool false; _}) -> lhs
    | Mnequal ({node = Mbool true; _}, rhs)  -> mk_mterm (Mnot rhs) (Tbuiltin Bbool)
    | Mnequal ({node = Mbool false; _}, rhs) -> rhs

    | _ -> map_mterm (aux c) mt
  in
  Model.map_mterm_model aux model

let ligo_move_get_in_condition (model : model) : model =
  let contains_getter (mt : mterm) : bool =
    let rec aux accu (mt : mterm) : bool =
      match mt.node with
      | Mget _ -> true
      | _ -> Model.fold_term aux accu mt
    in
    aux false mt
  in
  let extract_getter (mt : mterm) : (mterm * (lident * mterm) list) =
    let cpt : int ref = ref 0 in
    let rec aux (accu : (lident * mterm) list) (mt : mterm) : mterm * (lident * mterm) list =
      match mt.node with
      | Mget _ ->
        begin
          let var_id = "tmp_" ^ string_of_int (!cpt) in
          cpt := !cpt + 1;
          let var = mk_mterm (Mvarlocal (dumloc var_id)) mt.type_ in
          var, (dumloc var_id, mt)::accu
        end
      | _ ->
        let g (x : mterm__node) : mterm = { mt with node = x; } in
        Model.fold_map_term g aux accu mt
    in
    aux [] mt
  in
  let rec aux c (mt : mterm) : mterm =
    match mt.node with
    | Mif (cond, e, t) when contains_getter cond ->
      begin
        let (cond, ll) = extract_getter cond in
        let res : mterm = mk_mterm (Mif (cond, e, t)) Tunit in
        let res : mterm = List.fold_right
            (fun (id, x : lident * mterm) accu ->
               let node : mterm__node = Mletin ([id], x, Some (x.type_), accu, None) in
               mk_mterm node Tunit)
            ll res in
        res
      end
    | _ -> map_mterm (aux c) mt
  in
  Model.map_mterm_model aux model

let remove_rational (model : model) : model =
  let type_int = Tbuiltin Bint in
  let type_bool= Tbuiltin Bbool in
  let type_cur = Tbuiltin Bcurrency in
  let type_rational = Ttuple [type_int; type_int] in
  let one = mk_mterm (Mint (Big_int.unit_big_int)) type_int in
  let mk_rat n d = mk_mterm (Mtuple [n ; d]) type_rational in
  let int_to_rat e = mk_mterm (Minttorat e) type_rational in
  let process_type t : type_ =
    let rec aux t =
      match t with
      | Tbuiltin Brational -> type_rational
      | _ -> map_type aux t
    in
    aux t
  in
  let to_rat (x : mterm) =
    match x.type_ with
    | Tbuiltin Bint -> mk_rat x one
    | Ttuple [Tbuiltin Bint; Tbuiltin Bint] -> x
    | _ -> assert false
  in
  let process_mterm mt =
    let rec aux (mt : mterm) : mterm =
      let is_t_rat (x : type_) = match x with     | Tbuiltin Brational -> true | _ -> false in
      let is_int (x : mterm) = match x.type_ with | Tbuiltin Bint      -> true | _ -> false in
      let is_rat (x : mterm) = match x.type_ with | Tbuiltin Brational -> true | _ -> false in
      let is_cur (x : mterm) = match x.type_ with | Tbuiltin Bcurrency -> true | _ -> false in
      let is_num (x : mterm) = is_rat x || is_int x in
      let is_rats (a, b) = is_rat a || is_rat b in
      let process_eq neg (a, b) =
        let lhs = (to_rat |@ aux) a in
        let rhs = (to_rat |@ aux) b in
        let res = mk_mterm (Mrateq (lhs, rhs)) type_bool in
        if neg
        then mk_mterm (Mnot res) type_bool
        else res
      in
      let process_cmp op (a, b) =
        let lhs = (to_rat |@ aux) a in
        let rhs = (to_rat |@ aux) b in
        mk_mterm (Mratcmp (op, lhs, rhs)) type_bool
      in
      let process_arith op (a, b) =
        let lhs = (to_rat |@ aux) a in
        let rhs = (to_rat |@ aux) b in
        mk_mterm (Mratarith (op, lhs, rhs)) type_rational
      in
      let process_rattez (n : mterm) (t : mterm) : mterm =
        let coef = (to_rat |@ aux) n in
        let t = aux t in
        mk_mterm (Mrattez (coef, t)) type_cur
      in
      match mt.node, mt.type_ with
      | _ as node, Tbuiltin Brational ->
        begin
          match node with
          | Mrational (a, b) ->
            let make_int (x : Core.big_int) = mk_mterm (Mint x) type_int in
            mk_mterm (Mtuple [make_int a; make_int b]) type_rational
          | Mplus   (a, b) -> process_arith Rplus  (a, b)
          | Mminus  (a, b) -> process_arith Rminus (a, b)
          | Mmult   (a, b) -> process_arith Rmult  (a, b)
          | Mdiv    (a, b) -> process_arith Rdiv   (a, b)
          | Mdivrat (a, b) -> mk_rat a b
          | _ -> { mt with type_ = type_rational }
        end
      | _ as node, Tbuiltin Bcurrency ->
        begin
          match node with
          | Mcurrency (v, Tz)  -> { mt with node = Mcurrency  (Big_int.mult_int_big_int 1000000 v, Utz) }
          | Mcurrency (v, Mtz) -> { mt with node = Mcurrency  (Big_int.mult_int_big_int    1000 v, Utz) }
          | Mmult  (a, b) when is_num a && is_cur b -> process_rattez a b
          | _ -> map_mterm aux mt
        end
      | Mequal  (a, b), _ when is_rats (a, b) -> process_eq  false (a, b)
      | Mnequal (a, b), _ when is_rats (a, b) -> process_eq  true  (a, b)
      | Mlt     (a, b), _ when is_rats (a, b) -> process_cmp Lt    (a, b)
      | Mle     (a, b), _ when is_rats (a, b) -> process_cmp Le    (a, b)
      | Mgt     (a, b), _ when is_rats (a, b) -> process_cmp Gt    (a, b)
      | Mge     (a, b), _ when is_rats (a, b) -> process_cmp Ge    (a, b)
      | Mletin (ids, v, t, body, o), _ when is_int v && Option.map_dfl is_t_rat false t ->
        { mt with
          node = Mletin (ids, (int_to_rat |@ aux) v, Option.map process_type t, aux body, Option.map aux o)
        }
      | Mletin (ids, v, t, body, o), _ ->
        { mt with
          node = Mletin (ids, aux v, Option.map process_type t, aux body, Option.map aux o)
        }
      | Mdeclvar (ids, t, v), _ when is_int v && Option.map_dfl is_t_rat false t ->
        { mt with
          node = Mdeclvar (ids, Option.map process_type t, (int_to_rat |@ aux) v)
        }
      | Mdeclvar (ids, t, v), _ ->
        { mt with
          node = Mdeclvar (ids, Option.map process_type t, aux v)
        }
      | Massign (op, t, i, v), _ when is_int v && is_t_rat t ->
        { mt with
          node = Massign (op, process_type t, i, (int_to_rat |@ aux) v)
        }
      | Massignvarstore (op, t, i, v), _  when is_int v && is_t_rat t ->
        { mt with
          node = Massignvarstore (op, process_type t, i, (int_to_rat |@ aux) v)
        }
      | Massignfield (op, t, a, fn, v), _ when is_int v && is_t_rat t ->
        { mt with
          node = Massignfield (op, process_type t, a, fn, (int_to_rat |@ aux) v)
        }
      | Mforall (id, t, a, b), _ ->
        { mt with
          node = Mforall (id, process_type t, Option.map aux a, aux b)
        }
      | Mexists (id, t, a, b), _ ->
        { mt with
          node = Mexists (id, process_type t, Option.map aux a, aux b)
        }
      | _ -> map_mterm aux mt
    in
    aux mt
  in
  let process_arg (type_ : type_) (default_value : mterm option) : (type_ * mterm option) =
    let t = process_type type_ in
    t, Option.map ((fun dv ->
        match t with
        | Ttuple [Tbuiltin Bint; Tbuiltin Bint] -> to_rat dv
        | _ -> dv
      ) |@ process_mterm) default_value
  in
  let process_decls = List.map (function
      | Dvar v ->
        let t, dv = process_arg v.type_ v.default in
        Dvar
          { v with
            type_   = t;
            default = dv;
          }
      | Dasset a ->
        Dasset
          {a with
           values = a.values |> List.map
                      (fun (ai : asset_item) ->
                         { ai with
                           type_   = ai.type_   |> process_type;
                           default = ai.default |> Option.map process_mterm;
                         })
          }
      | Dcontract c ->
        Dcontract
          {c with
           signatures = c.signatures |> List.map
                          (fun (cs : contract_signature) ->
                             {
                               cs with
                               args = cs.args |> List.map (fun (a, b) -> (a, process_type b))
                             }
                          );
           init = c.init |> Option.map process_mterm
          }
      | _ as x -> x)
  in
  { model with
    decls     = model.decls     |> process_decls;
    functions = model.functions |> List.map (fun f ->
        {f with
         node = (
           let process_fs (fs : function_struct) =
             {fs with
              args = fs.args |> List.map (fun (id, t, dv) -> (id, process_type t, Option.map process_mterm dv));
              body = fs.body |> process_mterm;
             }
           in
           match f.node with
           | Function (fs, ret) -> Function (process_fs fs, process_type ret)
           | Entry fs           -> Entry (process_fs fs)
         );
        })
  }


let replace_date_duration_by_timestamp (model : model) : model =
  let type_timestamp = Tbuiltin Btimestamp in
  let type_int = Tbuiltin Bint in
  let process_type t : type_ =
    let rec aux t =
      match t with
      | Tbuiltin Bdate     -> type_timestamp
      | Tbuiltin Bduration -> type_int
      | _ -> map_type aux t
    in
    aux t
  in
  let to_timestamp (x : mterm) =
    match x.node with
    | Mdate d     -> mk_mterm (Mtimestamp (Core.date_to_timestamp d)) type_timestamp
    (* | Mduration d -> mk_mterm (Mint (Core.duration_to_timestamp d)) type_int *)
    | Mnow
    | Mtimestamp _ -> x
    | _ ->
      begin
        Format.eprintf "cannot transform to timestamp: %a" Printer_model.pp_mterm x;
        assert false
      end
  in
  let process_mterm mt =
    let rec aux (mt : mterm) : mterm =
      match mt.node, mt.type_ with
      | Mdate d,_      -> mk_mterm (Mtimestamp (Core.date_to_timestamp d)) type_timestamp
      | Mduration d, _ -> mk_mterm (Mint (Core.duration_to_timestamp d)) type_int
      | Mletin (ids, v, t, body, o), _ ->
        { mt with
          node = Mletin (ids, aux v, Option.map process_type t, aux body, Option.map aux o)
        }
      | Mdeclvar (ids, t, v), _ ->
        { mt with
          node = Mdeclvar (ids, Option.map process_type t, aux v)
        }
      | Mforall (id, t, a, b), _ ->
        { mt with
          node = Mforall (id, process_type t, Option.map aux a, aux b)
        }
      | Mexists (id, t, a, b), _ ->
        { mt with
          node = Mexists (id, process_type t, Option.map aux a, aux b)
        }
      | _ -> map_mterm aux mt
    in
    aux mt
  in
  let process_decls =
    let process_arg (type_ : type_) (default_value : mterm option) : (type_ * mterm option) =
      let t = process_type type_ in
      t, Option.map ((fun dv ->
          match t with
          | Tbuiltin Btimestamp -> to_timestamp dv
          | _ -> dv
        ) |@ process_mterm) default_value
    in
    List.map (function
        | Dvar v ->
          let t, dv = process_arg v.type_ v.default in
          Dvar
            { v with
              type_   = t;
              default = dv;
            }
        | Dasset a ->
          Dasset
            {a with
             values = a.values |> List.map
                        (fun (ai : asset_item) ->
                           { ai with
                             type_   = ai.type_   |> process_type;
                             default = ai.default |> Option.map process_mterm;
                           })
            }
        | Dcontract c ->
          Dcontract
            {c with
             signatures = c.signatures |> List.map
                            (fun (cs : contract_signature) ->
                               {
                                 cs with
                                 args = cs.args |> List.map (fun (a, b) -> (a, process_type b))
                               }
                            );
             init = c.init |> Option.map process_mterm
            }
        | _ as x -> x)
  in
  { model with
    decls     = model.decls     |> process_decls;
    functions = model.functions |> List.map (fun f ->
        {f with
         node = (
           let process_fs (fs : function_struct) =
             {fs with
              args = fs.args |> List.map (fun (id, t, dv) -> (id, process_type t, Option.map process_mterm dv));
              body = fs.body |> process_mterm;
             }
           in
           match f.node with
           | Function (fs, ret) -> Function (process_fs fs, process_type ret)
           | Entry fs           -> Entry (process_fs fs)
         );
        })
  }

let abs_tez model : model =
  let is_cur (mt : mterm) =
    match mt.type_ with
    | Tbuiltin Bcurrency -> true
    | _ -> false
  in
  let is_int (mt : mterm) =
    match mt.type_, mt.node with
    | _, Mfunabs _ -> false
    | Tbuiltin Bint, _ -> true
    | _ -> false
  in
  let abs mt = mk_mterm (Mfunabs mt) mt.type_ in
  let rec aux c (mt : mterm) : mterm =
    match mt.node with
    | Mmult (lhs, rhs) when is_int lhs && is_cur rhs ->
      mk_mterm (Mmult (abs(lhs), rhs)) mt.type_
    | Mmult (lhs, rhs) when is_cur lhs && is_int rhs ->
      mk_mterm (Mmult (lhs, abs(rhs))) mt.type_
    | _ -> map_mterm (aux c) mt
  in
  Model.map_mterm_model aux model

let exec_process model =
  model
  |> replace_lit_address_by_role
  |> remove_label
  |> flat_sequence
  |> remove_cmp_bool


let replace_assignfield_by_update (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Massignfield (op, _, {node = Mget(an, key); _ }, fn, v) ->
      let l = [(fn, op, v)] in
      mk_mterm (Mupdate (an, key, l)) Tunit
    | Massignfield (op, _, a, fn, v) ->
      let an =
        begin
          match a.type_ with
          | Tasset an -> unloc an
          | _ -> assert false
        end
      in
      let k, t = Utils.get_asset_key model an in
      let key : mterm = mk_mterm (Mdotasset (a, dumloc k)) (Tbuiltin t) in
      let l = [(fn, op, v)] in
      mk_mterm (Mupdate (an, key, l)) Tunit

    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let eval_variable_initial_value (model : model) : model =
  let map_value : (ident * mterm) list =
    List.fold_left (fun accu x ->
        match x with
        | Dvar v when Option.is_some v.default -> (unloc v.name, Option.get v.default)::accu
        | _ -> accu
      ) [] model.decls in
  { model with
    decls = List.map (
        fun x ->
          match x with
          | Dvar v -> Dvar { v with default = Option.map (Model.Utils.eval map_value) v.default }
          | _ -> x) model.decls;
  }


let add_explicit_sort (model : model) : model =

  let rec aux (env : ident list) (ctx : ctx_model) (mt : mterm) : mterm =
    let is_sorted x : bool =
      let rec aux accu x : bool =
        match mt.node with
        | Msort _ -> true
        | _ -> fold_term aux accu x
      in
      aux false x
    in
    let rec is_implicit_sort env (mt : mterm) : bool =
      match mt.node with
      | Mvarstorecol _ -> true

      | Mvarlocal id -> not (List.exists (fun a -> String.equal a (unloc id)) env)

      (* asset api *)
      | Mselect (_, c, _) -> is_implicit_sort env c
      | Mhead   (_, c, _) -> is_implicit_sort env c
      | Mtail   (_, c, _) -> is_implicit_sort env c

      | _ -> false
    in
    let get_crit an : (ident * sort_kind) list =
      let k, _ = Model.Utils.get_asset_key model an in
      [(k, SKasc)]
    in
    let create_sort an c =
      let crit = get_crit an in
      mk_mterm (Msort (an, c, crit)) c.type_
    in
    let extract_asset_name (c : mterm) =
      match c.type_ with
      | Tcontainer (Tasset an, _) -> unloc an
      | _ -> assert false
    in
    match mt.node with
    | Mletin ([id], v, Some (Tcontainer (Tasset an, c)), body, o) ->
      let env =
        match is_sorted body with
        | true -> (unloc id)::env
        | _ -> env
      in
      let body = aux env ctx body in
      { mt with node = Mletin ([id], v, Some (Tcontainer (Tasset an, c)), body, o)}

    | Mnth (an, c, idx) when is_implicit_sort env c ->
      { mt with node = Mnth (an, create_sort an c, idx) }

    | Mhead (an, c, idx) when is_implicit_sort env c ->
      { mt with node = Mhead (an, create_sort an c, idx) }

    | Mtail (an, c, idx) when is_implicit_sort env c ->
      { mt with node = Mtail (an, create_sort an c, idx) }

    | Mfor (a, c, body, lbl) when is_implicit_sort env c ->
      let body = aux env ctx body in
      let an = extract_asset_name c in
      mk_mterm (Mfor (a, create_sort an c, body, lbl)) Tunit

    | _ -> map_mterm (aux env ctx) mt
  in
  map_mterm_model (aux []) model


let remove_cmp_enum (model : model) : model =
  let mk_exprmatchwith dir v id =
    let tbool = Tbuiltin Bbool in
    let t = mk_mterm (Mbool true) tbool in
    let f = mk_mterm (Mbool false)tbool in
    let cv, wv =
      match dir with
      | `Pos -> t, f
      | `Neg -> f, t
    in
    let pattern_const = mk_pattern (Pconst id), cv in
    let pattern_wild = mk_pattern Pwild, wv in

    let l = [pattern_const; pattern_wild] in
    mk_mterm (Mexprmatchwith (v, l)) tbool
  in
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Mequal ({type_ = (Tstate | Tenum _)} as v, {node = Mvarenumval id})    -> mk_exprmatchwith `Pos v id
    | Mequal ({node = Mvarenumval id}, ({type_ = (Tstate | Tenum _)} as v))  -> mk_exprmatchwith `Pos v id
    | Mnequal ({type_ = (Tstate | Tenum _)} as v, {node = Mvarenumval id})   -> mk_exprmatchwith `Neg v id
    | Mnequal ({node = Mvarenumval id}, ({type_ = (Tstate | Tenum _)} as v)) -> mk_exprmatchwith `Neg v id
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model


let replace_whyml_ident (model : model) : model =
  let f _env id =
    match id with
    | "val" -> "_val"
    | "type" -> "_type"
    | _ -> id
  in
  replace_ident_model f model

let replace_ident_model_val (model : model) : model =
  let f _env id =
    match id with
    | "val" -> "val_"
    | _ -> id

  (* match env with
     | KIaction ->
     begin
      match id with
      | "val" -> "val_"
      | _ -> id
     end
     | _ -> id *)
  in
  replace_ident_model f model


(* set remove update *)


let replace_key_by_asset (model : model) : model =
  let rec aux c (mt : mterm) : mterm =
    let mk n = mk_mterm n Tunit in
    let get an k = mk_mterm (Mget (an, k)) (Tasset (dumloc an)) in
    match mt.node with
    | Mremoveasset (an, k) ->
      let k_c, _ =  Utils.get_asset_key model an in
      let nk =
        match k.node with
        | Mdotasset (({type_ = Tasset tan; _} as a), b) when String.equal (unloc tan) an && String.equal k_c (unloc b) -> a
        | _ -> get an k
      in
      mk (Mremoveasset (an, nk))
    | Mremovefield (an, fn, a, k) ->
      let can, _ = Utils.get_field_container model an fn in
      let k_c, _ =  Utils.get_asset_key model can in
      let nk =
        match k.node with
        | Mdotasset (({type_ = Tasset tan; _} as a), b) when String.equal (unloc tan) can && String.equal k_c (unloc b) -> a
        | _ -> get can k
      in
      mk (Mremovefield (an, fn, a, nk))
    | Mset (an, fns, k, a) ->
      let k_c, _ =  Utils.get_asset_key model an in
      let nk =
        match k.node with
        | Mdotasset (({type_ = Tasset tan; _} as a), b) when String.equal (unloc tan) an && String.equal k_c (unloc b) -> a
        | _ -> get an k
      in
      mk (Mset (an, fns, nk, a))
    | _ -> map_mterm (aux c) mt
  in
  Model.map_mterm_model aux model

let merge_update (model : model) : model =
  let contains l (ref, _, _) = List.fold_left (fun accu (id, _, _) -> accu || (String.equal (unloc ref) (unloc id))) false l in
  let replace l (ref_id, ref_op, ref_v) =
    List.fold_right (fun (id, op, v) accu ->
        if (String.equal (unloc ref_id) (unloc id))
        then (id, ref_op, ref_v)::accu
        else (id, op, v)::accu
      ) l [] in
  let compute_nl l1 l2 = List.fold_left
      (fun accu x ->
         if contains accu x
         then replace accu x
         else accu @ [ x ]) l1 l2  in
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mseq l ->
      begin
        let l : mterm list = List.map (aux ctx) l in
        let rec f (accu : mterm list) (y : mterm list) =
          match y with
          | ({ node = Mupdate(an1, k1, l1); _ })::({ node = Mupdate(an2, k2, l2); _ })::t
            when String.equal an1 an2 && Model.cmp_mterm k1 k2
                 && List.fold_left (fun accu (_, op, _) -> accu && (match op with | ValueAssign -> accu | _ -> false)) true l2
            ->
            begin
              let nl = compute_nl l1 l2 in
              let node = Mupdate(an1, k1, nl) in
              let mt = mk_mterm node Tunit in
              f accu (mt::t)
            end
          | a::t -> a::(f accu t)
          | [] -> []
        in
        let ll = f [] l in
        mk_mterm (Mseq ll) mt.type_
      end
    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model


let process_asset_state (model : model) : model =
  let get_state_lident an = dumloc ("state_" ^ an) in

  let for_decl (d : decl_node) : decl_node =
    let for_asset (a : asset) =
      match a.state with
      | Some id ->
        begin
          let enum    = Utils.get_enum model (unloc id) in
          let name    = get_state_lident (unloc a.name) in
          let typ     = Tenum id in
          let e_val   = enum.initial in
          let default = mk_mterm (Mvarenumval e_val) typ in

          let item = mk_asset_item name typ typ ~default:default in
          { a with values = a.values @ [item]; state = None }
        end
      | None -> a
    in
    match d with
    | Dasset a -> Dasset (for_asset a)
    | _ -> d
  in

  let model = { model with decls = List.map for_decl model.decls} in

  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mvarassetstate (an, k) ->
      begin
        let i = get_state_lident an in
        let get_mt = mk_mterm (Mget (an, k)) (Tasset (dumloc an)) in
        mk_mterm (Mdotasset (get_mt, i)) mt.type_
      end
    | Massignassetstate (an, k, v) ->
      let i = get_state_lident an in
      mk_mterm (Mupdate (an, k, [(i, ValueAssign, v) ])) Tunit
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model
