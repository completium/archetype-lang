open Ident
open Location
open Model
open Tools

type error_desc =
  | AssetPartitionnedby of string * string list
  | CannotBuildAsset of string * string
  | ContainersInAssetContainers of string * string * string
  | NoEmptyContainerForInitAsset of string * string * container
  | CallerNotSetInInit
  | NoEntrypoint

let pp_error_desc fmt = function
  | AssetPartitionnedby (i, l)         ->
    Format.fprintf fmt
      "Cannot access asset collection: asset %s is partitionned by field(s) (%a)."
      i (Printer_tools.pp_list ", " Printer_tools.pp_str) l

  | CannotBuildAsset (an, fn) ->
    Format.fprintf fmt "Cannot build an asset %s, default value of field '%s' is missing." an fn

  | ContainersInAssetContainers (an, fn, an2) ->
    Format.fprintf fmt "Cannot build an asset '%s', '%s' is a container field, which refers to an asset '%s', which contains a container field itself."
      an fn an2

  | NoEmptyContainerForInitAsset (an, fn, c) ->
    Format.fprintf fmt "Field '%s' of '%s' asset is a %a, which must be initialized by an empty collection."
      fn an
      Printer_model.pp_container c

  | CallerNotSetInInit ->
    Format.fprintf fmt "'caller' is used in initialization of contract, please set caller value with '--set-caller-init'"

  | NoEntrypoint -> Format.fprintf fmt "No entrypoint found (action or transtion)"

type error = Location.t * error_desc

let emit_error (lc, error : Location.t * error_desc) =
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
        let asset_col : mterm = mk_mterm (Mvarstorecol (dumloc an)) type_asset in
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
            mk_mterm (Mget (an, asset_col, key_mterm)) type_asset
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
    | Mremoveif (asset, p, la, lb, a) ->
      let lasset = dumloc asset in
      let type_asset = Tasset lasset in

      let assetv_str = dumloc (asset ^ "_") in
      let asset_var = mk_mterm (Mvarlocal assetv_str) type_asset in

      let key, key_type = Utils.get_asset_key model (unloc lasset) in
      let asset_key : mterm = mk_mterm (Mdotasset (asset_var,dumloc key)) (Tbuiltin key_type) in

      let assets_var_name = dumloc ("assets_") in
      let type_assets = Tcontainer (type_asset, View) in
      let assets_var = mk_mterm (Mvarlocal assets_var_name) type_assets in

      let view : mterm = mk_mterm (Mcast (Tcontainer (type_asset, Collection), type_assets, p)) type_assets in
      let select : mterm =  mk_mterm (Mselect (asset, view, la, lb, a) ) type_asset in

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
      | Mremoveif(a, { node = (Mvarstorecol _); loc = _}, _, _, _) when List.mem a partitionned_assets -> emit_error t.loc a
      | _ -> fold_term (internal_raise ctx) acc t
    in
    let with_error = fold_model internal_raise model false in
    if with_error
    then raise (Error.Stop 5)
  in
  let _ = raise_access_error () in
  model

let check_number_entrypoint (model : model) : model =
  let nb_entrypoints = model.functions |> List.filter (fun (f : function__) -> match f.node with | Entry _ -> true | _ -> false) |> List.length in
  if nb_entrypoints = 0 then (emit_error (model.loc, NoEntrypoint); raise (Error.Stop 5));
  model

let check_containers_asset (model : model) : model =
  let assets = Utils.get_assets model in
  let is_container = function
    | Tcontainer _ -> true
    | _ -> false
  in
  let is_asset_with_container (an : lident) : bool =
    let an = unloc an in
    let a = Utils.get_asset model an in
    List.exists (fun item -> is_container item.type_) a.values
  in
  let l : (string * string * string * Location.t) list =
    List.fold_left (fun accu (a : asset) ->
        List.fold_left (fun accu (item : asset_item) ->
            match item.type_ with
            | Tcontainer (Tasset an, _) when is_asset_with_container an -> (unloc a.name, unloc item.name, unloc an, item.loc)::accu
            | _ -> accu
          ) accu a.values
      ) [] assets
  in
  List.iter (fun (an, fn, a2, l) -> emit_error (l, ContainersInAssetContainers (an, fn, a2))) l;
  if List.is_not_empty l then raise (Error.Stop 5);
  model

let check_empty_container_on_initializedby (model : model) : model =
  let l : (string * string * container * Location.t) list =
    List.fold_right (fun asset accu -> ((List.fold_right (fun (value : mterm) accu ->
        (fun (asset : asset) (value : mterm) accu ->
           match value.node with
           | Masset l ->
             (List.fold_right2 (fun (field : asset_item) (y : mterm) accu ->
                  match field.type_, y.node with
                  | Tcontainer (Tasset _, c), Massets ll when List.is_not_empty ll -> [unloc asset.name, unloc field.name, c, y.loc]
                  | _ -> accu
                ) asset.values l [])::accu
           | _ -> accu
        )
          asset value accu) asset.init []) |> List.flatten)::accu) (Utils.get_assets model) [] |> List.flatten
  in
  List.iter (fun (an, fn, c, l) -> emit_error (l, (NoEmptyContainerForInitAsset (an, fn, c)))) l;
  if List.is_not_empty l then raise (Error.Stop 5);
  model

let check_and_replace_init_caller  (model : model) : model =
  let caller = !Options.opt_caller in
  let for_decl accu (d : decl_node) : decl_node * bool =
    let for_mterm accu (mt : mterm) : mterm * bool =
      let rec aux accu (mt : mterm) : mterm * bool =
        match mt.node with
        | Mcaller -> mk_mterm (Maddress caller) (Tbuiltin Baddress), true
        | _ ->
          let g (x : mterm__node) : mterm = { mt with node = x; } in
          fold_map_term g aux accu mt
      in
      aux accu mt
    in
    match d with
    | Dvar ({default = Some v} as a) ->
      begin
        let v, def = for_mterm accu v in
        (Dvar { a with default = Some v }, def)
      end
    | Dasset a ->
      let def, l =
        List.fold_right (
          fun e (accu, decls) ->
            let d, v = for_mterm accu e in
            (v, d::decls)) a.init (false, [])
      in
      (Dasset { a with init = l }, def)
    | _ -> d, accu
  in
  let b, decls =
    List.fold_right (fun d (accu, decls) ->
        let d, b = for_decl accu d in
        (b || accu, d::decls)) model.decls (false, [])
  in
  begin
    match b, caller with
    | true, "" -> emit_error (model.loc, CallerNotSetInInit)
    | _ -> ()
  end;
  { model with decls = decls }

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
  let empty : mterm = mk_mterm (Mseq []) Tunit in
  let process_declvar (ids, t, v) f accu =
    begin
      let init = f v in
      let body =
        match accu with
        | [] -> empty
        | [i] -> i
        | lll -> mk_mterm (Mseq accu) (List.last lll).type_
      in
      mk_mterm (Mletin(ids, init, t, body, None)) body.type_
    end
  in
  let rec aux c (mt : mterm) : mterm =
    match mt.node with
    | Mseq l ->
      let ll = List.fold_right (fun (x : mterm) accu ->
          match x.node with
          | Mdeclvar (ids, t, v) ->
            let res =  process_declvar (ids, t, v) (aux c) accu in
            [ res ]
          | _ ->
            begin
              let t = aux c x in
              t::accu
            end
        ) l [] in
      { mt with node = Mseq ll }
    | Mdeclvar (ids, t, v) -> process_declvar (ids, t, v) (aux c) []
    | _ -> map_mterm (aux c) mt
  in
  Model.map_mterm_model aux model

let replace_label_by_mark (model : model) : model =
  let empty : mterm = mk_mterm (Mseq []) Tunit in
  let process_internal id accu =
    begin
      let body =
        match accu with
        | [] -> empty
        | [i] -> i
        | lll -> mk_mterm (Mseq accu) (List.last lll).type_
      in
      mk_mterm (Mmark(id, body)) body.type_
    end
  in
  let rec aux c (mt : mterm) : mterm =
    match mt.node with
    | Mseq l ->
      let ll = List.fold_right (fun (x : mterm) accu ->
          match x.node with
          | Mlabel id ->
            let res =  process_internal id accu in
            [ res ]
          | _ ->
            begin
              let t = aux c x in
              t::accu
            end
        ) l [] in
      { mt with node = Mseq ll }
    | Mlabel id -> process_internal id []
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
              let accu = aux ctx accu col in
              let accu = aux ctx accu body in
              label::accu
            end
          | Miter (_, min, max, body, Some label) ->
            begin
              let accu = aux ctx accu min in
              let accu = aux ctx accu max in
              let accu = aux ctx accu body in
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
      let ncol  = aux ctx col in
      let nbody = aux ctx body in
      let label = get_loop_label ctx in
      { mt with node = Mfor (a, ncol, nbody, Some label)}

    | Miter (a, min, max, body, None) ->
      let nmin  = aux ctx min in
      let nmax  = aux ctx max in
      let nbody = aux ctx body in
      let label = get_loop_label ctx in
      { mt with node = Miter (a, nmin, nmax, nbody, Some label)}

    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let remove_enum_matchwith (model : model) : model =
  let type_int = Tbuiltin Bint in
  let type_bool = Tbuiltin Bbool in
  let mk_enum_ident a b =
    let a =
      match a with
      | "$state" -> "state"
      | _ -> a
    in
    String.lowercase_ascii (a ^ "_" ^ b)
  in
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
    let process_asset_struct (a : asset) =
      {
        a with
        values = List.map
            (fun (ai : asset_item)
              ->
                {
                  ai with
                  type_ =
                    match ai.type_ with
                    | Tenum _ -> type_int
                    | v -> v
                }) a.values;
      }
    in
    List.fold_left (fun accu x ->
        accu @
        (
          match x with
          | Denum v -> process_enum v
          | Dasset a -> [Dasset (process_asset_struct a)]
          | _ -> [x]
        )
      ) [] decls
  in
  let rec process_mterm ctx (mt : mterm) : mterm =
    let mk_id (prefix : string) (id : lident) : lident =
      mkloc (loc id) (mk_enum_ident prefix (unloc id)) in
    match mt.node, mt.type_ with
    | Mvarlocal id, Tstate -> mk_mterm (Mvarlocal (mk_id "state" id)) type_int
    | Mvarenumval id, Tenum e -> mk_mterm (Mvarlocal (mk_id (unloc e) id)) type_int
    | Mexprmatchwith (v, ps), _
    | Mmatchwith (v, ps), _ ->
      let val_v =
        match v.type_ with
        | Tstate -> "state"
        | Tenum v -> unloc v
        | t -> (Format.printf "%a@." pp_type_ t ; assert false)
      in
      let v = process_mterm ctx v in
      let type_v = v.type_ in
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
  let process_type t =
    match t with
    | Tenum _ -> Tbuiltin Bint
    | _ -> t
  in
  let process_functions f =
    let process_fs (fs : function_struct) =
      fs
    in
    let process_node (node : function_node) : function_node =
      match node with
      | Function (fs, type_) -> Function (process_fs fs, process_type type_)
      | Entry fs -> Entry (process_fs fs)
    in
    { f with
      node = process_node f.node;
    }
  in
  { model with
    decls = process_decls model.decls;
    functions = List.map process_functions model.functions;
  }
  |> map_mterm_model process_mterm

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
      let process_uminus v =
        let v = (to_rat |@ aux) v in
        mk_mterm (Mratuminus v) type_rational
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
          | Mcast (Tbuiltin Bint, Tbuiltin Brational, v) ->
            let nv = aux v in
            int_to_rat nv
          | Mrational (a, b) ->
            let make_int (x : Core.big_int) = mk_mterm (Mint x) type_int in
            mk_mterm (Mtuple [make_int a; make_int b]) type_rational
          | Mplus   (a, b) -> process_arith Rplus  (a, b)
          | Mminus  (a, b) -> process_arith Rminus (a, b)
          | Mmult   (a, b) -> process_arith Rmult  (a, b)
          | Mdiv    (a, b) -> process_arith Rdiv   (a, b)
          | Muminus v      -> process_uminus v
          | Mdivrat (a, b) -> mk_rat a b
          | Mmax    (a, b) when is_rats (a, b) -> let lhs, rhs = pair_sigle_map (to_rat |@ aux) (a, b) in mk_mterm (Mmax (lhs, rhs)) type_rational
          | Mmin    (a, b) when is_rats (a, b) -> let lhs, rhs = pair_sigle_map (to_rat |@ aux) (a, b) in mk_mterm (Mmin (lhs, rhs)) type_rational
          | _ ->
            let mt = map_mterm aux mt in
            { mt with type_ = type_rational }
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
  let process_specification (spec : specification) : specification =
    let for_label_term (lt : label_term) : label_term =
      {
        lt with
        term  = process_mterm lt.term;
      }
    in
    let for_predicate (p : predicate) : predicate =
      {
        p with
        args = List.map (fun (x, y) -> x, process_type y) p.args;
        body = process_mterm p.body;
      }
    in
    let for_definition (d : definition) : definition =
      {
        d with
        typ  = process_type d.typ;
        body = process_mterm d.body;
      }
    in
    let for_variable (v : variable) : variable =
      let for_argument (arg : argument) : argument =
        let a, b, c = arg in
        a, process_type b, Option.map process_mterm c
      in
      let for_qualid (q : qualid) : qualid =
        {
          q with
          type_ = process_type q.type_;
        }
      in
      {
        decl         = for_argument v.decl;
        constant     = v.constant;
        from         = Option.map for_qualid v.from;
        to_          = Option.map for_qualid v.to_;
        loc          = v.loc;
      }
    in
    let for_invariant (i : invariant) : invariant =
      {
        i with
        formulas = List.map process_mterm i.formulas;
      }
    in
    let for_postcondition (p : postcondition) : postcondition =
      {
        p with
        formula    = process_mterm p.formula;
        invariants = List.map for_invariant p.invariants;
      }
    in
    {
      predicates     = List.map for_predicate     spec.predicates;
      definitions    = List.map for_definition    spec.definitions;
      lemmas         = List.map for_label_term    spec.lemmas;
      theorems       = List.map for_label_term    spec.theorems;
      variables      = List.map for_variable      spec.variables;
      invariants     = List.map (fun (x, y) -> x, List.map for_label_term y) spec.invariants;
      effects        = List.map process_mterm     spec.effects;
      postconditions = List.map for_postcondition spec.postconditions;
      loc            = spec.loc;
    }
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
         spec = Option.map process_specification f.spec;
        });
    specification = process_specification model.specification;
  }


let replace_date_duration_by_timestamp (model : model) : model =
  let type_timestamp = Tbuiltin Btimestamp in
  let type_int = Tbuiltin Bint in
  let is_date     = function | Tbuiltin Bdate -> true     | _ -> false in
  let is_duration = function | Tbuiltin Bduration -> true | _ -> false in
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
      | Mnow, _        -> mk_mterm (Mnow) type_timestamp
      | Mmax (a, b), t when is_duration t || is_date t ->
        let a = aux a in
        let b = aux b in
        mk_mterm (Mmax(a, b)) (process_type t)
      | Mmin (a, b), t when is_duration t || is_date t ->
        let a = aux a in
        let b = aux b in
        mk_mterm (Mmin(a, b)) (process_type t)
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
    | _, Mabs _ -> false
    | Tbuiltin Bint, _ -> true
    | _ -> false
  in
  let abs mt = mk_mterm (Mabs mt) (Tbuiltin Bnat) in
  let rec aux c (mt : mterm) : mterm =
    match mt.node with
    | Mmult (lhs, rhs) when is_int lhs && is_cur rhs ->
      mk_mterm (Mmult (abs(lhs), rhs)) mt.type_
    | Mmult (lhs, rhs) when is_cur lhs && is_int rhs ->
      mk_mterm (Mmult (lhs, abs(rhs))) mt.type_
    | _ -> map_mterm (aux c) mt
  in
  Model.map_mterm_model aux model

let replace_assignfield_by_update (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Massignfield (op, _, {node = Mget(an, _, key); _ }, fn, v) ->
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
      | Mselect (_, c, _, _, _) -> is_implicit_sort env c
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

let replace_ligo_ident (model : model) : model =
  let f _env id =
    match id with
    | "type" -> "type_"
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
    let get an k = mk_mterm (Mget (an, Utils.get_asset_collection an, k)) (Tasset (dumloc an)) in
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
        let get_mt = mk_mterm (Mget (an, Utils.get_asset_collection an, k)) (Tasset (dumloc an)) in
        mk_mterm (Mdotasset (get_mt, i)) mt.type_
      end
    | Massignassetstate (an, k, v) ->
      let i = get_state_lident an in
      mk_mterm (Mupdate (an, k, [(i, ValueAssign, v) ])) Tunit
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let extract_term_from_instruction f (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    let process (mt : mterm) l : mterm =
      List.fold_right
        (fun (id, v) accu ->
           mk_mterm (Mletin ([id], v, Some v.type_, accu, None)) accu.type_
        ) l mt
    in

    match mt.node with
    (* lambda *)

    | Mletin (i, a, t, b, o) ->
      let ae, aa = f a in
      let be = aux ctx b in
      let oe = Option.map (aux ctx) o in
      process (mk_mterm (Mletin (i, ae, t, be, oe)) mt.type_) aa

    | Mdeclvar (i, t, v) ->
      let ve, va = f v in
      process (mk_mterm (Mdeclvar (i, t, ve)) mt.type_) va


    (* assign *)

    | Massign (op, t, l, r) ->
      let re, ra = f r in
      process (mk_mterm (Massign (op, t, l, re)) mt.type_) ra

    | Massignvarstore (op, t, l, r)  ->
      let re, ra = f r in
      process (mk_mterm (Massignvarstore (op, t, l, re)) mt.type_) ra

    | Massignfield (op, t, a, fi, r) ->
      let re, ra = f r in
      process (mk_mterm (Massignfield (op, t, a, fi, re)) mt.type_) ra

    | Massignstate x ->
      let xe, xa = f x in
      process (mk_mterm (Massignstate xe) mt.type_) xa

    | Massignassetstate (an, k, v) ->
      let ke, ka = f k in
      let ve, va = f v in
      process (mk_mterm (Massignassetstate (an, ke, ve)) mt.type_) (ka @ va)


    (* control *)

    | Mif (c, t, e) ->
      let ce, ca = f c in
      let te = aux ctx t in
      let ee = Option.map (aux ctx) e in
      process (mk_mterm (Mif (ce, te, ee)) mt.type_) ca

    | Mmatchwith (e, l) ->
      let ee, ea = f e in
      let ll = List.map (fun (p, e) -> (p, aux ctx e)) l in
      process (mk_mterm (Mmatchwith (ee, ll)) mt.type_) ea

    | Mfor (i, c, b, lbl) ->
      let ce, ca = f c in
      let be = aux ctx b in
      process (mk_mterm (Mfor (i, ce, be, lbl)) mt.type_) ca

    | Miter (i, a, b, c, lbl) ->
      let ae, aa = f a in
      let be, ba = f b in
      let ce = aux ctx c in
      process (mk_mterm (Miter (i, ae, be, ce, lbl)) mt.type_) (aa @ ba)

    | Mreturn x ->
      let xe, xa = f x in
      process (mk_mterm (Mreturn (xe)) mt.type_) xa


    (* effect *)

    | Mfail v ->
      let ve, va = match v with
        | Invalid x ->
          let xe, xa = f x in
          Invalid (xe), xa
        | _ -> v, []
      in
      process (mk_mterm (Mfail (ve)) mt.type_) va

    | Mtransfer (v, d) ->
      let ve, va = f v in
      let de, da = f d in
      process (mk_mterm (Mtransfer (ve, de)) mt.type_) (va @ da)

    | Mentrycall (v, d, t, func, args) ->
      let ve, va = f v in
      let de, da = f d in
      let ae, aa = List.fold_right (fun (t, i) (xe, xa) ->
          let ie, ia = f i in
          ((t, ie)::xe, ia @ xa)) args ([], []) in
      process (mk_mterm (Mentrycall (ve, de, t, func, ae)) mt.type_) (va @ da @ aa)


    (* asset api effect *)

    | Maddasset (an, i) ->
      let ie, ia = f i in
      process (mk_mterm (Maddasset (an, ie)) mt.type_) ia

    | Maddfield (an, fn, c, i) ->
      let ce, ca = f c in
      let ie, ia = f i in
      process (mk_mterm (Maddfield (an, fn, ce, ie)) mt.type_) (ca @ ia)

    | Mremoveasset (an, i) ->
      let ie, ia = f i in
      process (mk_mterm (Mremoveasset (an, ie)) mt.type_) ia

    | Mremovefield (an, fn, c, i) ->
      let ce, ca = f c in
      let ie, ia = f i in
      process (mk_mterm (Mremovefield (an, fn, ce, ie)) mt.type_) (ca @ ia)

    | Mclearfield (an, fn, a) ->
      let ae, aa = f a in
      process (mk_mterm (Mclearfield (an, fn, ae)) mt.type_) aa

    | Mset (an, l, k, v) ->
      let ke, ka = f k in
      let ve, va = f v in
      process (mk_mterm (Mset (an, l, ke, ve)) mt.type_) (ka @ va)

    | Mupdate (an, k, l) ->
      let ke, ka = f k in
      let le, la = List.fold_right (fun (id, op, v) (xe, xa) ->
          let ve, va = f v in
          ((id, op, ve)::xe, va @ xa)) l ([], []) in
      process (mk_mterm (Mupdate (an, ke, le)) mt.type_) (ka @ la)

    | Mremoveif (an, c, la, lb, a) ->
      let lbe, lba = f lb in
      let ae, aa = List.fold_right (fun v (xe, xa) ->
          let ve, va = f v in
          (ve::xe, va @ xa)) a ([], []) in
      process (mk_mterm (Mremoveif (an, c, la, lbe, ae)) mt.type_) (lba @ aa)

    | Maddupdate (an, k, l) ->
      let ke, ka = f k in
      let le, la = List.fold_right (fun (id, op, v) (xe, xa) ->
          let ve, va = f v in
          ((id, op, ve)::xe, va @ xa)) l ([], []) in
      process (mk_mterm (Maddupdate (an, ke, le)) mt.type_) (ka @ la)

    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model

let remove_fun_dotasset (model : model) : model =
  let extract_fun_dotasset (mt: mterm) : mterm * (lident * mterm) list =
    let cpt : int ref = ref 0 in
    let prefix = "tmp_" in

    let rec efd_aux (accu : (lident * mterm) list) (mt : mterm) : mterm * (lident * mterm) list =
      let is_fun (mt : mterm) : bool =
        match mt.node with
        | Mget _ | Mnth _-> true
        | _ -> false
      in
      match mt.node with
      | Mdotasset (l, r) when is_fun l ->
        begin
          let var_id = prefix ^ string_of_int (!cpt) in
          cpt := !cpt + 1;
          let var = mk_mterm (Mvarlocal (dumloc var_id)) l.type_ in
          let nmt = mk_mterm (Mdotasset (var, r)) mt.type_ in
          nmt, (dumloc var_id, l)::accu
        end
      | _ ->
        let g (x : mterm__node) : mterm = { mt with node = x; } in
        Model.fold_map_term g efd_aux accu mt
    in
    efd_aux [] mt
  in
  extract_term_from_instruction extract_fun_dotasset model

let remove_letin_from_expr (model : model) : model =
  let aux (mt: mterm) : mterm * (lident * mterm) list =
    let rec f (accu : (lident * mterm) list) (mt : mterm) : mterm * (lident * mterm) list =
      match mt.node with
      | Mletin ([i], a, _, b, None) ->
        let b, accu = f accu b in
        b, (i, a)::accu
      | _ ->
        let g (x : mterm__node) : mterm = { mt with node = x; } in
        Model.fold_map_term g f accu mt
    in
    f [] mt
  in
  extract_term_from_instruction aux model


let process_internal_string (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node, mt.type_ with
    | Mplus (l, r), Tbuiltin Bstring ->
      let l = aux ctx l in
      let r = aux ctx r in
      mk_mterm (Mconcat (l, r)) (Tbuiltin Bstring)
    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model

let replace_asset_by_key (model : model) : model =
  let rec for_type (t : type_) : type_ =
    match t with
    | Tasset an -> (Tbuiltin (Utils.get_asset_key model (unloc an) |> snd))
    | Tcontainer (Tasset an, _) -> (Tlist (Tbuiltin (Utils.get_asset_key model (unloc an) |> snd)))
    | Ttuple l -> Ttuple (List.map for_type l)
    | _ -> t
  in
  let to_key (mt : mterm) : mterm =
    match mt.node, mt.type_ with
    | Mdotasset ({type_ = Tasset an} as a, k), _ when String.equal (Utils.get_asset_key model (unloc an) |> fst) (unloc k) -> a
    | Mget (_, _, k), _ -> k
    | _ -> mt
  in
  let for_mterm (mt : mterm) : mterm =
    let rec replace_get (mt : mterm) : mterm =
      match mt.node with
      (* | Mget (_, _, ({node = (Mvarlocal _ | Mvarparam _); _} as k)) -> k *)
      | _ -> map_mterm replace_get mt
    in
    let rec aux (mt : mterm) : mterm =
      match mt.node, mt.type_ with
      | Maddasset _, _ -> mt
      | Maddfield (an, fn, c, asset), _ -> mk_mterm (Maddfield (an, fn, to_key c, asset)) mt.type_
      | Mremovefield (an, fn, c, asset), _ -> mk_mterm (Mremovefield (an, fn, to_key c, aux asset)) mt.type_
      | Mselect (an, c, a, b, l), _ -> mk_mterm (Mselect (an, aux c, a, b, l)) mt.type_
      | Msum (an, c, a), _ -> mk_mterm (Msum (an, aux c, a)) mt.type_
      | Mletin (l, {node = Mget (_, _, k)}, Some t, b, o), _ -> mk_mterm (Mletin (l, k, Some (for_type t), aux b, Option.map aux o)) mt.type_
      (* | Mletin (ids, a, Some t, b, o), _ ->
        mk_mterm (Mletin (ids, aux a, Some (for_type t), (aux b), Option.map aux o)) mt.type_ *)
      | Massets l, Tcontainer (Tasset an, _) ->
        let l = List.map aux l in
        mk_mterm (Mlitlist l) (Tlist (Tbuiltin (Utils.get_asset_key model (unloc an) |> snd)))
      | Mdotasset ({type_ = Tasset an} as a, k), _ when String.equal (Utils.get_asset_key model (unloc an) |> fst) (unloc k) -> a
      (* | Mdotasset ({type_ = Tasset _; node = Mget _}, _), _ -> mt
      | Mdotasset ({type_ = Tasset an} as a, k), _ ->
        let storevar : mterm = mk_mterm (Mvarstorecol an) (Tcontainer (Tasset an, Collection)) in
        let cast : mterm = mk_mterm (Mcast (Tcontainer (Tasset an, Collection), Tcontainer (Tasset an, View), storevar)) (Tcontainer (Tasset an, View)) in
        let get = mk_mterm (Mget (unloc an, cast, a)) (Tasset an) in
        mk_mterm (Mdotasset(get, k)) mt.type_ *)
      | Masset l, Tasset an ->
        begin
          let l = List.map aux l in
          List.nth l (Utils.get_key_pos model (unloc an))
        end
      | Mvarlocal _, Tasset an ->
        begin
          let dan = an in
          let an = unloc an in
          let asset_collection : mterm = mk_mterm (Mvarstorecol dan) (Tcontainer (Tasset dan, Collection)) in
          mk_mterm (Mget (an, asset_collection, mt)) mt.type_
        end
      | _ -> map_mterm aux mt
    in
    mt |> replace_get |> aux
  in

  let for_function (f : function__) =
    let for_function_node (fn : function_node) =
      let for_function_struct (fs : function_struct) =
        {fs with body = for_mterm fs.body }
      in
      match fn with
      | Entry fs -> Entry (for_function_struct fs)
      | Function (fs, ret) -> Function (for_function_struct fs, ret)
    in
    { f with node = for_function_node f.node }
  in
  { model with functions = List.map for_function model.functions }

let split_key_values (model : model) : model =

  (* let rec f (ctx : ctx_model) (x : mterm) : mterm =
     match x.node with
     | Mselect (an, col, lambda_args, lambda_body, args) ->
      let col = f ctx col in
      let lambda_body = f ctx lambda_body in
      let args = List.map (f ctx) args in
      let _k, t = Utils.get_asset_key model an in
      { x with node = Mselect (an, col, lambda_args, lambda_body, args); type_ = Tcontainer (Tbuiltin t, Collection)}

     | Msort (an, col, l) ->
      let col = f ctx col in
      let _k, t = Utils.get_asset_key model an in
      { x with node = Msort (an, col, l); type_ = Tcontainer (Tbuiltin t, Collection)}

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

     | Mcast ((Tcontainer ((Tasset _), _)), (Tcontainer ((Tasset _), View)), x) -> f ctx x

     | Mvarstorecol an ->
      (
        let _k, t = Utils.get_asset_key model (unloc an) in
        { x with node = Mcoltokeys (unloc an); type_ = Tcontainer (Tbuiltin t, Collection) }
      )
     | _ -> map_mterm (f ctx) x
     in *)

  let asset_assets an = an ^ "_assets" in

  let get_asset_assoc_key_value (asset_name : ident) (asset_value : mterm) : mterm * mterm=
    match asset_value.node with
    | Masset l ->
      begin
        let asset : asset = Model.Utils.get_asset model asset_name in
        let asset_key = asset.key in

        let assoc_fields = List.map2 (fun (ai : asset_item) (x : mterm) -> (unloc ai.name, x)) asset.values l in

        List.find (fun (id, _) -> String.equal asset_key id) assoc_fields |> snd,
        { asset_value with
          node = Masset (List.map (fun (x : mterm) ->
              match x with
              | { type_ = Tcontainer (Tasset an, c); _} -> { x with type_ = let k = Utils.get_asset_key model (unloc an) |> snd in Tcontainer (Tbuiltin k, c) }
              | _ -> x) l)
        }
      end
    | _ -> assert false
  in

  let storage =
    List.fold_right (fun x accu ->
        match x.model_type with
        | MTasset an ->
          let an = dumloc an in
          let _k, t = Utils.get_asset_key model (unloc an) in
          let type_asset = Tmap (t, Tasset an) in
          let default =
            match x.default.node with
            | Massets l -> mk_mterm (Mlitmap (List.map (fun x -> get_asset_assoc_key_value (unloc an) x) l)) type_asset
            | _ -> assert false
          in
          let asset_assets =
            mk_storage_item (dumloc (asset_assets (unloc an)))
              (MTasset (unloc an))
              type_asset
              default
              ~loc:x.loc
          in
          asset_assets::accu
        | _ -> x::accu)
      model.storage []
  in

  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Mcast (Tcontainer (Tasset _, _), Tcontainer (Tasset _, _), v) -> aux ctx v
    | Mvarstorecol an ->
      mk_mterm (Mcoltokeys (unloc an)) (Tlist (Tbuiltin (Utils.get_asset_key model (unloc an) |> snd)))
    | _ -> map_mterm (aux ctx) mt
  in

  { model with
    storage = storage
  } |> map_mterm_model aux

let replace_get_on_view (model : model) : model =
  let rec is_not_varcol (a : mterm) =
    match a.node with
    | Mvarstorecol _ -> false
    | Mcast (_, _, a) -> is_not_varcol a
    | _ -> true
  in

  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mget (an, c, k) when is_not_varcol c ->
      let type_asset = Tasset (dumloc an) in
      let c = Utils.get_asset_collection an in
      let get_asset = mk_mterm (Mget (an, Utils.get_asset_collection an, k)) type_asset in
      let ffail = mk_mterm (Mfail (Invalid (mk_mterm (Mstring "do not contain key") (Tbuiltin Bstring)))) Tunit in
      let cond = mk_mterm (Mcontains (an, c, k)) (Tbuiltin Bbool) in
      mk_mterm (Mexprif (cond, get_asset, ffail)) type_asset
    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model


let replace_for_to_iter (model : model) : model =
  let is_asset (col : mterm) : bool =
    match col.type_ with
    | Tcontainer (Tasset _, _) -> true
    | _ -> false
  in

  let extract_asset (col : mterm) =
    match col.type_ with
    | Tcontainer (Tasset an, _) -> an
    | _ -> assert false
  in

  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mfor (id, col, body, Some lbl) when is_asset col ->
      let nbody = aux ctx body in
      let asset_name = extract_asset col in
      let an = unloc asset_name in
      let type_asset = Tasset asset_name in
      let type_col = Tcontainer (type_asset,Collection) in
      let type_view = Tcontainer (type_asset,View) in
      let view =
        begin match col.type_ with
          | Tcontainer (_,Collection) -> mk_mterm (Mcast (type_col,type_view,col)) type_view
          | _ -> col
        end in
      let idx_id = "_i_" ^ lbl in
      let idx = mk_mterm (Mvarlocal (dumloc idx_id)) (Tbuiltin Bint) in
      let nth = mk_mterm (Mnth(an, view, idx)) type_asset in
      let letin = mk_mterm (Mletin ([id], nth, Some type_asset, nbody, None)) Tunit in
      let bound_min = mk_mterm (Mint Big_int.zero_big_int) (Tbuiltin Bint) in
      let bound_max = mk_mterm (Mcount (an, view)) (Tbuiltin Bint) in
      let iter = Miter (dumloc idx_id, bound_min, bound_max, letin, Some lbl) in
      mk_mterm iter mt.type_
    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model

let remove_duplicate_key (model : model) : model =
  let remove_key_value_for_asset_node (mt : mterm) : mterm =
    match mt.node, mt.type_ with
    | Masset l, Tasset an ->
      begin
        let an = unloc an in
        let k, _ = Utils.get_asset_key model an in
        let l =
          Utils.get_labeled_value_from model an l
          |> List.filter (fun (lbl, _) -> not (String.equal lbl k))
        in
        mk_mterm (Mlitrecord l) (Tasset (dumloc (an ^ "_storage")))
      end
    | _ -> mt
  in

  let storage =
    List.fold_right (fun x accu ->
        match x.model_type with
        | MTasset an ->
          if Utils.is_asset_single_field model an
          then
            begin
              let _k, t = Utils.get_asset_key model an in
              let type_asset = Tset t in
              let default =
                match x.default.node with
                | Mlitmap l ->  mk_mterm (Mlitset (List.map fst l)) type_asset
                | _ -> assert false
              in
              let asset_assets =
                mk_storage_item x.id
                  (MTasset an)
                  type_asset
                  default
                  ~loc:x.loc
              in
              asset_assets::accu
            end
          else
            let default, typ =
              match x.default.node, x.typ with
              | Mlitmap l, Tmap (b, Tasset an) ->
                let t = Tmap (b, Tasset (dumloc ((unloc an) ^ "_storage"))) in
                let mt = mk_mterm (Mlitmap (List.map (fun (k, v) -> (k, remove_key_value_for_asset_node v)) l)) t in
                mt, t
              | _ -> x.default, x.typ
            in
            { x with default = default; typ = typ }::accu
        | _ -> x::accu)
      model.storage []
  in

  { model with
    storage = storage
  }

let remove_assign_operator (model : model) : model =
  let compute op lhs t (v : mterm) : mterm =
    match op with
    | ValueAssign -> v
    | PlusAssign  -> mk_mterm (Mplus (lhs, v)) t
    | MinusAssign -> mk_mterm (Mminus (lhs, v)) t
    | MultAssign  -> mk_mterm (Mmult (lhs, v)) t
    | DivAssign   -> mk_mterm (Mdiv (lhs, v)) t
    | AndAssign   -> mk_mterm (Mand (lhs, v)) t
    | OrAssign    -> mk_mterm (Mor (lhs, v)) t
  in
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Massign (op, t, id, v) ->
      let lhs = mk_mterm (Mvarlocal id) v.type_ in
      let v = compute op lhs v.type_ v in
      mk_mterm (Massign (ValueAssign, t, id, v)) mt.type_
    | Massignvarstore (op, t, id, v) ->
      let lhs = mk_mterm (Mvarstorevar id) v.type_ in
      let v = compute op lhs v.type_ v in
      mk_mterm (Massignvarstore (ValueAssign, t, id, v)) mt.type_
    | Massignfield    (op, t, mt, id, v) ->
      let lhs = mk_mterm (Mdotasset (mt, id)) v.type_ in
      let v = compute op lhs v.type_ v in
      mk_mterm (Massignfield (ValueAssign, t, mt, id, v)) mt.type_
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model


let extract_item_collection_from_add_asset (model : model) : model =
  let extract_item_collection_from_add_asset (an : ident) (l : mterm list) =
    let asset = Utils.get_asset model an in
    List.fold_right2
      (fun (ai : asset_item) (mt : mterm) (add_fields, items) ->
         match ai.type_, mt.node with
         | Tcontainer (Tasset ann, _), Massets l when not (List.is_empty l) ->
           begin
             let mas = mk_mterm (Massets []) ai.type_ in
             let assets = [unloc ai.name, unloc ann, l] in
             (mas::add_fields, assets @ items)
           end
         | _ -> (mt::add_fields, items))
      asset.values l ([], [])
  in
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    begin
      match mt.node with
      | Maddasset (an, { node = (Masset l); type_ = Tasset _; _}) ->
        begin
          let dan = dumloc an in
          let add_fields, labeled_assets = extract_item_collection_from_add_asset an l in
          if List.is_empty labeled_assets
          then mt
          else
            begin
              let k = List.nth l (Utils.get_key_pos model an) in (* FIXME *)
              let add = mk_mterm (Maddasset (an, mk_mterm (Masset add_fields) (Tasset dan))) Tunit in
              let instrs : mterm list =
                labeled_assets
                |> List.map
                  (fun (fn, _ann, assetss) ->
                     assetss
                     |> List.map (fun asset ->
                         let store_asset = mk_mterm (Mvarstorecol dan) (Tcontainer (Tasset dan, Collection)) in
                         let src = mk_mterm (Mget (an, store_asset, k)) (Tasset dan) in
                         (mk_mterm (Maddfield (an, fn, src, asset)) Tunit))
                  )
                |> List.flatten
              in
              mk_mterm (Mseq (add::instrs)) Tunit
            end
        end
      | _ -> map_mterm (aux ctx) mt
    end
  in
  map_mterm_model aux model


let check_if_asset_in_function (model : model) : model =
  let rec for_type (t : type_) : type_ =
    match t with
    | Tasset _ -> exit 8
    | _ -> map_type for_type t
  in
  let for_function (f : function__) =
    let for_function_node (fn : function_node) =
      let for_function_struct (fs : function_struct) =
        let _ = List.iter (fun (_, t, _) -> let _ = for_type t in ()) fs.args in
        ()
      in
      match fn with
      | Entry fs -> for_function_struct fs
      | Function (fs, _) -> for_function_struct fs
    in
    for_function_node f.node
  in
  List.iter for_function model.functions;
  model
