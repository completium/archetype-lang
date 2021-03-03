open Ident
open Location
open Model
module MapString = Map.Make(String)
module SetString = Set.Make(String)
open Tools

type error_desc =
  | AssetPartitionnedby of string * string list
  | CannotBuildAsset of string * string
  | ContainersInAssetContainers of string * string * string
  | NoEmptyContainerForDefaultValue of string * string * container
  | NoClearForPartitionAsset of ident
  | DefaultValueOnKeyAsset of ident
  | CallerNotSetInInit
  | DuplicatedKeyAsset of ident
  | OnlyLiteralInAssetInit
  | NoEntrypoint
  | UnknownContract of ident
  | NoSortOnKeyWithMultiKey of ident
  | NoInitValueForParameter of ident
  | NoInitForPartitionAsset of ident

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

  | NoEmptyContainerForDefaultValue (an, fn, c) ->
    Format.fprintf fmt "Field '%s' of '%s' asset is a %a, which must be initialized by an empty container."
      fn an
      Printer_model.pp_container c

  | NoClearForPartitionAsset an ->
    Format.fprintf fmt "Clear is not allowed for asset '%s', because this asset is used in a partition." an

  | CallerNotSetInInit ->
    Format.fprintf fmt "'caller' is used in initialization of contract, please set caller value with '--set-caller-init'"

  | DuplicatedKeyAsset an ->
    Format.fprintf fmt "duplicate key for '%s'" an

  | OnlyLiteralInAssetInit ->
    Format.fprintf fmt "only literal is allowed for asset field initialisation"

  | DefaultValueOnKeyAsset an ->
    Format.fprintf fmt "default value on key for asset \"%s\"" an

  | UnknownContract id ->
    Format.fprintf fmt "cannot find type for '%s'" id

  | NoEntrypoint -> Format.fprintf fmt "No entrypoint found (action or transtion)"

  | NoSortOnKeyWithMultiKey f -> Format.fprintf fmt "No sort on key with multi key: %s" f

  | NoInitValueForParameter id -> Format.fprintf fmt "No initialized value for parameter: %s" id

  | NoInitForPartitionAsset an -> Format.fprintf fmt "Asset '%s' is used in a partition, no asset must initialized" an

type error = Location.t * error_desc

let emit_error (lc, error : Location.t * error_desc) =
  let str : string = Format.asprintf "%a@." pp_error_desc error in
  let pos : Position.t list = [location_to_position lc] in
  Error.error_alert pos str (fun _ -> ())

let prune_formula (model : model) : model =
  let empty_sec = mk_security () in
  let empty_spec = mk_specification () in

  let prune_decl = function
    | Dvar dvar       -> Dvar   {dvar with invariants = []}
    | Dasset dasset   -> Dasset {dasset with invariants = [] }
    | Denum denum     -> Denum  {denum with values = List.map (fun (ei : enum_item) -> {ei with invariants = []}) denum.values }
    | Drecord drecord -> Drecord drecord
  in
  let prune_function_ (f : function__) : function__ = { f with spec = None; }
  in
  { model with
    decls = List.map prune_decl model.decls;
    functions = List.map prune_function_ model.functions;
    specification = empty_spec;
    security = empty_sec
  }

let flat_sequence_mterm (mt : mterm) =
  let rec aux (mt : mterm) : mterm =
    match mt.node with
    | Mseq l ->
      begin
        match l with
        | [] -> mt
        | [e] -> aux e
        | l ->
          let l = List.fold_right (fun (x : mterm) accu ->
              match x.node with
              | Mseq [] -> accu
              | _ -> (aux x)::accu) l [] in
          begin
            match l with
            | [] -> mk_mterm (Mseq []) tunit
            | [e] -> aux e
            | _ -> mk_mterm (Mseq (List.map aux l)) (List.last l).type_
          end
      end
    | _ -> map_mterm aux mt
  in
  aux mt

let flat_sequence (model : model) : model =
  let aux (_ctx : ctx_model) (mt : mterm) : mterm =
    flat_sequence_mterm mt
  in
  map_mterm_model aux model

let process_assign_op op (t : type_) (lhs : mterm) (v : mterm) : mterm =
  match op, get_ntype t with
  | ValueAssign , _ -> v
  | PlusAssign, Tcontainer ((Tasset _, _), _) -> v
  | PlusAssign  , _ -> mk_mterm (Mplus (lhs, v)) t
  | MinusAssign , Tbuiltin Bnat -> begin
      let a = mk_mterm (Mminus (lhs, v)) tint in
      let zero = mk_mterm (Mint Big_int.zero_big_int) tint in
      let cond = mk_mterm (Mge (a, zero)) tbool in
      let v = mk_mterm (Mabs a) tnat in
      let f = mk_mterm (Mfail AssignNat) tunit in
      let c = mk_mterm (Mcast (tunit, tnat, f)) tnat in
      mk_mterm (Mexprif (cond, v, c)) tnat
    end
  | MinusAssign , _ -> mk_mterm (Mminus (lhs, v)) t
  | MultAssign  , _ -> mk_mterm (Mmult (lhs, v)) t
  | DivAssign   , _ -> mk_mterm (Mdivrat (lhs, v)) t
  | AndAssign   , _ -> mk_mterm (Mand (lhs, v)) t
  | OrAssign    , _ -> mk_mterm (Mor (lhs, v)) t

let remove_add_update ?(with_force = false) ?(isformula = false) (model : model) : model =
  let error = ref false in
  let f_error (l : Location.t) (an : string) (fn : string) = emit_error(l, CannotBuildAsset (an, fn)); error := true in
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Maddupdate (an, c, k, l) ->
      begin
        let type_asset = tasset (dumloc an) in
        let is_standalone = List.fold_left (fun accu (_, op, _) -> match op with | ValueAssign -> accu | _ -> false) true l in
        let mk_asset (an, k, l) =
          let dummy_mterm = mk_mterm (Mseq []) tunit in
          let asset = Utils.get_asset model an in
          let lref : (Ident.ident * (assignment_operator * mterm * Location.t)) list = List.map (fun (x, y, z) -> (unloc x, (y, z, loc x))) l in
          let l = List.map (
              fun (f : asset_item) ->
                let f_name = (unloc f.name) in
                if List.exists (String.equal f_name) asset.keys
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
                          | _ -> f_error lo an f_name; mk_mterm (Mseq []) tunit
                        in
                        process_assign_op op f.type_ dv v
                      end
                  end
            ) asset.values in
          mk_mterm (Masset l) type_asset
        in
        let cond   = mk_mterm (
            match c with
            | CKfield (_, _, ({node = Mdotassetfield (andat, kdat, fn)} as a), t, d) ->
              let c = (if isformula then a else kdat) in Mcontains (an, CKfield(unloc andat, unloc fn, c, t, d), k)
            | CKcoll _ -> Mcontains (an, c, k)
            | _ -> assert false) tunit in
        let asset  = mk_asset (an, k, l) in
        if with_force && is_standalone
        then mk_mterm (Maddforce (an, asset)) tunit
        else begin
          let add = mk_mterm (
              match c with
              | CKfield (_, _, {node = Mdotassetfield (an, k, fn)}, _, _) -> Maddfield (unloc an, unloc fn, k, asset)
              | CKcoll _ -> Maddasset (an, asset)
              | _ -> assert false) tunit in
          let update = mk_mterm (Mupdate (an, k, l)) tunit in
          let if_node = Mif (cond, update, Some add) in
          mk_mterm if_node tunit
        end
      end
    | _ -> map_mterm (aux ctx) mt
  in
  let res = map_mterm_model aux model in
  if !error
  then raise (Error.Stop 5)
  else res


let remove_container_op_in_update (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    let is_field_container asset (fn, _, _) =
      let f = List.find (fun (x : asset_item) -> String.equal (unloc x.name) (unloc fn)) asset.values in
      match get_ntype f.original_type with
      | Tcontainer _ -> true
      | _ -> false
    in
    let with_container an l =
      let asset = Model.Utils.get_asset model an in
      List.exists (is_field_container asset) l
    in
    match mt.node with
    | Mupdate (an, k, l) when with_container an l -> begin
        let asset = Model.Utils.get_asset model an in
        let newl, instrs =
          List.fold_right (fun (fn, op, v : lident * assignment_operator * mterm) (accu_l, accu_instrs) ->
              match is_field_container asset (fn, op, v) with
              | false -> ((fn, op, v)::accu_l, accu_instrs)
              | true  -> begin
                  let fn = unloc fn in
                  let process ?(with_remove=false) kind =
                    let fnode = begin
                      match kind with
                      | `Add    -> fun x -> Maddfield (an, fn, k, x)
                      | `Remove -> fun x -> Mremovefield (an, fn, k, x)
                    end
                    in
                    let instrs : mterm list = begin
                      match v.node with
                      | Massets  ll
                      | Mlitlist ll -> List.map (fun a -> mk_mterm (fnode a) tunit) ll
                      | _ -> []
                    end
                    in
                    let instrs =
                      match with_remove with
                      | true -> (mk_mterm (Mremoveall (an, fn, k)) tunit)::instrs
                      | _ -> instrs
                    in
                    (accu_l, instrs @ accu_instrs)
                  in
                  match op with
                  | ValueAssign -> process `Add ~with_remove:true
                  | PlusAssign  -> process `Add
                  | MinusAssign -> process `Remove
                  | _ -> assert false
                end
            ) l ([], [])
        in
        let mterm_update = { mt with node = Mupdate (an, k, newl) } in
        match instrs with
        | [] -> mterm_update
        | _ -> mk_mterm (Mseq (mterm_update::instrs)) tunit
      end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model
  |> flat_sequence

let build_col_asset (an : ident) =
  let dan = dumloc an in
  let type_asset = Tasset dan in
  let type_col = tcollection dan in
  let type_view = tview dan in
  let col : mterm  = mk_mterm (Mvar (dan, Vstorecol, Tnone, Dnone)) type_col in
  mk_mterm (Mcast (type_col, type_view, col)) type_view, type_asset

let build_get (an : ident) v = mk_mterm (Mget (an, CKcoll (Tnone, Dnone), v)) (tasset (dumloc an))

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

        (* let _, t = Utils.get_asset_key model an in *)

        let type_asset = tasset (dumloc an) in

        let var_name = dumloc (an ^ "_") in
        let var_mterm : mterm = mk_mterm (Mvar (var_name, Vlocal, Tnone, Dnone)) type_asset in

        (* let key_name = "k_" in *)
        (* let key_loced : lident = dumloc (key_name) in *)
        (* let asset_col : mterm = mk_mterm (Mvarstorecol (dumloc an)) type_asset in *)
        let key_mterm : mterm = k in

        let set_mterm : mterm = mk_mterm (Mset (an, List.map (fun (id, _, _) -> unloc id) l, key_mterm, var_mterm)) tunit in

        let lref : (Ident.ident * (assignment_operator * mterm)) list = List.map (fun (x, y, z) -> (unloc x, (y, z))) l in
        let lassetitems =
          List.fold_left (fun accu (x : asset_item) ->
              let v = List.assoc_opt (unloc x.name) lref in
              let type_ = x.type_ in
              let var = mk_mterm (Mdot (var_mterm, x.name)) type_ in
              match v with
              | Some y ->
                accu @ [
                  let value = snd y in
                  let op = fst y in
                  process_assign_op op type_ var value
                ]
              | _ -> accu @ [var]
            ) [] asset.values in
        let asset : mterm = mk_mterm (Masset lassetitems) type_asset in

        let letinasset : mterm = mk_mterm (Mletin ([var_name],
                                                   asset,
                                                   Some (type_asset),
                                                   set_mterm,
                                                   None
                                                  )) tunit in

        (* let seq : mterm list = (List.map (fun ((id, op, term) : ('a * A.operator * 'c)) -> mk_mterm
                                              (Massignfield (to_assign_operator op, var_name, id, term))
                                              Tunit
                                          ) e) @ [set_mterm] in *)

        (* let body : mterm = mk_mterm (Mseq seq) Tunit in *)

        let get_mterm : mterm = build_get an key_mterm in

        let letinasset : mterm = mk_mterm (Mletin ([var_name],
                                                   get_mterm,
                                                   Some (type_asset),
                                                   letinasset,
                                                   None
                                                  ))
            tunit in

        letinasset

        (* let res : mterm__node =
           Mletin ([key_loced],
                  k,
                  Some (Tbuiltin t),
                  letinasset,
                  None
                 ) in

           mk_mterm res Tunit *)
      end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let remove_label (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Mlabel _ -> mk_mterm (Mseq []) tunit
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let replace_lit_address_by_role (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Maddress _ as node -> mk_mterm node trole
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
      | Mfor (_, c, _, Some id) -> acc @ [id, c]
      | _ -> fold_term (internal_get_for ctx) acc t
    in
    fold_model internal_get_for model [] in
  let for_colls = get_for_collections () in
  let map_invariant_iter () =
    let rec internal_map_inv_iter (ctx : ctx_model) (t : mterm) : mterm =
      let mk_term const =
        let loop_id = Tools.Option.get ctx.invariant_id |> unloc in
        if List.mem_assoc loop_id for_colls then
          let coll : iter_container_kind = List.assoc loop_id for_colls in
          let f t = match get_ntype t with | Tcontainer ((Tasset an, _), _) -> an | _ -> assert false in
          let tcoll =
            match coll with
            | ICKcoll an         -> tview (dumloc an)
            | ICKview c          -> tview (f c.type_)
            | ICKfield (_, _, c) -> tview (f c.type_)
            | ICKset  c  -> c.type_
            | ICKlist c  -> c.type_
            | ICKmap  c  -> c.type_
          in
          match const with
          | `Toiterate -> mk_mterm (Msettoiterate coll) tcoll
          | `Iterated ->  mk_mterm (Msetiterated coll) tcoll
        else
          t in
      match t.node with
      | Mvar (v, Vlocal, _, _) when cmp_lident v (dumloc "toiterate") -> mk_term `Toiterate
      | Mvar (v, Vlocal, _, _) when cmp_lident v (dumloc "iterated") -> mk_term `Iterated
      | _ -> map_mterm (internal_map_inv_iter ctx) t in
    map_mterm_model internal_map_inv_iter model in
  map_invariant_iter ()

let process_single_field_storage (model : model) : model =
  match model.storage with
  | [i] ->
    begin
      let storage_id = dumloc "_s" in
      let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
        match mt.node with
        | Mvar (a, Vstorevar, d, t) when String.equal (unloc a) (unloc i.id) ->
          mk_mterm (Mvar (storage_id, Vlocal, d, t)) mt.type_
        | Massign (op, t, Avar a, v) when String.equal (unloc a) (unloc i.id) ->
          let vv = map_mterm (aux ctx) v in
          mk_mterm (Massign (op, t, Avar storage_id, vv)) mt.type_
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
    emit_error (loc, a);
    true in
  (* woud need a model iterator here *)
  let raise_access_error () =
    let rec internal_raise (ctx : ctx_model) acc (t : mterm) =
      match t.node with
      | Maddasset (a, _) when List.mem a partitionned_assets -> emit_error t.loc (AssetPartitionnedby (a, get_partitions a))
      | Mremoveasset (a, _) when List.mem a partitionned_assets -> emit_error t.loc (AssetPartitionnedby (a, get_partitions a))
      | Mclear (a, _) when List.mem a partitionned_assets -> emit_error t.loc (NoClearForPartitionAsset a)
      | _ -> fold_term (internal_raise ctx) acc t
    in
    let with_error = fold_model internal_raise model false in
    if with_error
    then raise (Error.Stop 5)
  in
  let _ = raise_access_error () in
  model

let check_number_entrypoint (model : model) : model =
  let nb_entrypoints = model.functions |> List.filter (fun (f : function__) -> match f.node with | Entry _ | Getter _-> true | _ -> false) |> List.length in
  if nb_entrypoints = 0 then (emit_error (model.loc, NoEntrypoint); raise (Error.Stop 5));
  model

let check_containers_asset (model : model) : model =
  let assets = Utils.get_assets model in
  let is_container t =
    match get_ntype t with
    | Tcontainer _ -> true
    | _ -> false
  in
  let is_asset_with_container (an : lident) : bool =
    let an = unloc an in
    let a = Utils.get_asset model an in
    List.exists (fun (item : asset_item) -> is_container item.type_) a.values
  in
  let l : (string * string * string * Location.t) list =
    List.fold_left (fun accu (a : asset) ->
        List.fold_left (fun accu (item : asset_item) ->
            match get_ntype item.type_ with
            | Tcontainer ((Tasset an, _), _) when is_asset_with_container an -> (unloc a.name, unloc item.name, unloc an, item.loc)::accu
            | _ -> accu
          ) accu a.values
      ) [] assets
  in
  List.iter (fun (an, fn, a2, l) -> emit_error (l, ContainersInAssetContainers (an, fn, a2))) l;
  if List.is_not_empty l then raise (Error.Stop 5);
  model

let check_empty_container_on_asset_default_value (model : model) : model =
  let assets = Utils.get_assets model in
  let is_emtpy_container (omt : mterm) =
    match omt with
    | {node = ((Mlitlist [] | Massets []))} -> true
    | _ -> false
  in
  let l : (ident * ident * container * Location.t) list =
    List.fold_left (fun accu (a : asset) ->
        List.fold_left (fun accu (item : asset_item) ->
            match get_ntype item.type_, item.default with
            | Tcontainer ((Tasset an, _), c), Some dv when not (is_emtpy_container dv) -> (unloc an, unloc a.name, c, dv.loc)::accu
            | _ -> accu
          ) accu a.values
      ) [] assets
  in
  List.iter (fun (an, fn, c, l) -> emit_error (l, NoEmptyContainerForDefaultValue (an, fn, c))) l;
  if List.is_not_empty l then raise (Error.Stop 5);
  model

let check_asset_key (model : model) : model =
  let errors : (Location.t * error_desc) list ref = ref [] in
  List.iter
    (fun d ->
       match d with
       | Dasset dasset -> begin
           List.iter (fun s ->
               if List.exists (String.equal (unloc s)) dasset.keys
               then errors := ((loc s), NoSortOnKeyWithMultiKey (unloc s))::!errors
             ) dasset.sort;
           try
             let field = List.find (fun (x : asset_item) -> List.exists (String.equal (unloc x.name)) dasset.keys) dasset.values in
             match field.default with
             | Some dv -> errors := (dv.loc, DefaultValueOnKeyAsset (unloc dasset.name))::!errors
             | _ -> ()
           with Not_found -> assert false
         end
       | _ -> ()) model.decls;
  List.iter emit_error !errors;
  model

let check_and_replace_init_caller ?(doit=false) (model : model) : model =
  let caller = !Options.opt_caller in
  let for_decl accu (d : decl_node) : decl_node * bool =
    let for_mterm accu (mt : mterm) : mterm * bool =
      let rec aux accu (mt : mterm) : mterm * bool =
        match mt.node with
        | Mcaller -> mk_mterm (Maddress caller) taddress, true
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
    match doit, b, caller with
    | true, true, "" -> emit_error (model.loc, CallerNotSetInInit)
    | _ -> ()
  end;
  { model with decls = decls }

let rec is_literal (mt : mterm) : bool =
  match mt.node with
  | Munit
  | Mbool      _
  | Mint       _
  | Mnat       _
  | Mrational  _
  | Mstring    _
  | Mcurrency  _
  | Maddress   _
  | Mdate      _
  | Mduration  _
  | Mtimestamp _
  | Mbytes     _
  | Mleft      _
  | Mright      _
  | Mnone
  | Msome      _
  | Mtuple     _
  | Masset     _
  | Massets    _
  | Mlitset    _
  | Mlitlist   _
  | Mlitmap    _
  | Mlitrecord _
  | Mcaller
    -> true
  | Mnattoint v
  | Mnattorat v
  | Minttorat v
  | Mcast (_, _, v) -> is_literal v
  | _ -> false

let check_init_partition_in_asset (model : model) : model =

  let an_partition : ident list =
    List.map (function
        | Dasset da ->
          List.fold_left (fun accu (ai : asset_item) ->
              match ai.type_ with
              | Tcontainer ((Tasset an, _), Partition), _ -> (unloc an)::accu
              | _ -> accu
            ) [] da.values
        | _ -> []
      ) model.decls |> List.flatten
  in

  let errors : (Location.t * error_desc) list =
    List.map (function
        | Dasset da -> begin
            let an : ident = unloc da.name in
            let init : mterm list = da.init in
            if
              List.exists (String.equal an) an_partition &&
              not (List.is_empty init)
            then
              let loc =
                init
                |> List.map (fun (x : mterm) -> x.loc)
                |> Location.mergeall
              in
              [loc, NoInitForPartitionAsset an]
            else []
          end
        | _ -> []) model.decls |> List.flatten
  in
  List.iter emit_error errors;
  model

let check_duplicated_keys_in_asset (model : model) : model =
  let map_keys = ref MapString.empty in
  let add_key an key =
    let l =
      match MapString.find_opt an !map_keys with
      | None   -> []
      | Some l -> l
    in
    map_keys := MapString.add an (key::l) !map_keys
  in
  let contains_key an key =
    match MapString.find_opt an !map_keys with
    | None   -> false
    | Some l -> List.exists (cmp_mterm key) l
  in
  let errors : (Location.t * error_desc) list ref = ref [] in

  let check_asset_key an l =
    let dasset = Utils.get_asset model an in
    let asset : asset = Model.Utils.get_asset model an in
    let asset_keys = dasset.keys in
    let assoc_fields = List.map2 (fun (ai : asset_item) (x : mterm) -> (unloc ai.name, x)) asset.values l in
    let value_key =  List.find (fun (id, _) -> List.exists (String.equal id) asset_keys) assoc_fields |> snd in
    if not (is_literal value_key)
    then errors := (value_key.loc, OnlyLiteralInAssetInit)::!errors
    else (
      if contains_key an value_key
      then errors := (value_key.loc, DuplicatedKeyAsset an)::!errors
      else add_key an value_key)
  in
  List.iter
    (fun d ->
       match d with
       | Dasset dasset -> begin
           let an = unloc dasset.name in
           List.iter (fun (value_asset : mterm) ->
               match value_asset.node with
               | Masset l -> begin
                   check_asset_key an l;
                   List.iter2 (fun (ai : asset_item) (mt : mterm) ->
                       match ai.type_ with
                       | Tcontainer ((Tasset aan, _), Partition), _ -> begin
                           match mt.node with
                           | Massets ll -> begin
                               List.iter (fun (x : mterm) ->
                                   match x.node with
                                   | Masset lll -> check_asset_key (unloc aan) lll
                                   | _ -> ()
                                 ) ll
                             end
                           | _ -> ()
                         end
                       | _ -> ()) dasset.values l
                 end
               | _ -> ()
             ) dasset.init;
         end
       | _ -> ()
    ) model.decls;
  List.iter emit_error !errors;
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
          |> List.map ((function | Entry fs -> fs | Getter (fs, _) | Function (fs, _) -> fs))
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
        | Getter   (fs, r) -> Getter   (prune_function_struct fs, r)
        | Entry     fs     -> Entry    (prune_function_struct fs)
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
    let f2 = (fun (x : function__) -> match x.node with | Entry fs -> fs | Getter (fs, _) | Function (fs, _) -> fs) in
    { model with
      api_verif = api_verifs;
      decls = List.map prune_decl model.decls;
      functions = List.map prune_function__ (List.filter (f1 |@ f2) model.functions);
      specification = prune_specs model.specification;
      security = prune_secs model.security
    }

let move_partition_init_asset (model : model) : model =
  let extract_assets model : (mterm list) MapString.t * model =
    let add_map k l m =
      let ll = match MapString.find_opt k m with
        | None -> []
        | Some l -> l
      in
      MapString.add k (ll @ l) m
    in
    let map : (mterm list) MapString.t = MapString.empty in
    let map, decls = List.fold_left (
        fun (map, decls) decl ->
          match decl with
          | Dasset a -> begin
              let map, init = List.fold_left (
                  fun (map, assets) (asset : mterm) ->
                    let map, assets =
                      match asset.node with
                      | Masset fields -> begin
                          let map, fields =
                            List.fold_left2 (fun (map, fields) (ai : asset_item) (fv : mterm) ->
                                match fv.node, ai.type_ with
                                | Massets l, (Tcontainer ((Tasset aan, _), Partition), _) -> begin
                                    let aan = unloc aan in
                                    let _, kt = Utils.get_asset_key model aan in
                                    let extract_key (mt : mterm)=
                                      match mt.node with
                                      | Masset l -> List.nth l (Utils.get_key_pos model aan)
                                      | _ -> assert false
                                    in
                                    let keys = List.map extract_key l in
                                    let map = add_map aan l map in
                                    map, fields @ [mk_mterm (Mlitset keys) (tset kt)]
                                  end
                                | _ -> map, (fields @ [fv])
                              ) (map, []) a.values fields
                          in
                          map, assets @ [{ asset with node = Masset fields }]
                        end
                      | _ -> assert false
                    in
                    map, assets
                ) (map, []) a.init in
              map, (decls @ [Dasset {a with init = init}])
            end
          | _ -> (map, decls @ [decl])
      ) (map, []) model.decls in
    map, {
      model with
      decls = decls
    }
  in

  let add_assets (map, model : (mterm list) MapString.t * model) : model =
    let f (d : decl_node) : decl_node =
      match d with
      | Dasset a when MapString.mem (unloc a.name) map -> Dasset { a with init = a.init @ (MapString.find (unloc a.name) map )}
      | _ -> d
    in
    { model with
      decls = List.map f model.decls }
  in
  model
  |> extract_assets
  |> add_assets

let replace_declvar_by_letin (model : model) : model =
  let empty : mterm = mk_mterm (Mseq []) tunit in
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
  let empty : mterm = mk_mterm (Mseq []) tunit in
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
          | Mfor (_, c, body, Some label) ->
            begin
              let col =
                match c with
                | ICKcoll an -> mk_mterm (Mvar (dumloc an, Vstorecol, Tnone, Dnone)) (tcollection (dumloc an))
                | ICKview c
                | ICKfield (_, _, c)
                | ICKset  c
                | ICKlist c
                | ICKmap  c -> c
              in
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
          | Mwhile (cond, body, Some label) ->
            begin
              let accu = aux ctx accu cond in
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
    | Mfor (a, c, body, None) ->
      let nc = map_iter_container_kind (fun x -> x) (aux ctx) c in
      let nbody = aux ctx body in
      let label = get_loop_label ctx in
      { mt with node = Mfor (a, nc, nbody, Some label)}

    | Miter (a, min, max, body, None) ->
      let nmin  = aux ctx min in
      let nmax  = aux ctx max in
      let nbody = aux ctx body in
      let label = get_loop_label ctx in
      { mt with node = Miter (a, nmin, nmax, nbody, Some label)}

    | Mwhile (cond, body, None) ->
      let ncond = aux ctx cond in
      let nbody = aux ctx body in
      let label = get_loop_label ctx in
      { mt with node = Mwhile (ncond, nbody, Some label)}

    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let remove_enum000 (model : model) : model =
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
            tint
            tint
            VKconstant
            ~default:(mk_mterm (Mint (Big_int.big_int_of_int i)) tint)
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
                    match get_ntype ai.type_ with
                    | Tenum _ -> tint
                    | _ -> ai.type_
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
  (* let rec process_mterm ctx (mt : mterm) : mterm =
     let mk_id (prefix : string) (id : lident) : lident =
      mkloc (loc id) (mk_enum_ident prefix (unloc id)) in
     match mt.node, get_ntype mt.type_ with
     | Mvar (id, Vlocal, t, d), Tstate -> mk_mterm (Mvar (mk_id "state" id, Vlocal, t, d)) tint
     (* | Mvar (id, Venumval, t, d), Tenum e -> mk_mterm (Mvar (mk_id (unloc e) id, Vlocal, t, d)) tint *)
     | Mexprmatchwith (v, ps), _
     | Mmatchwith (v, ps), _ ->
      let val_v =
        match get_ntype v.type_ with
        | Tstate -> "state"
        | Tenum v -> unloc v
        | _ -> (Format.printf "%a@." pp_type_ v.type_ ; assert false)
      in
      let v = process_mterm ctx v in
      let type_v = v.type_ in
      begin
        let default_ = if (match mt.node with | Mexprmatchwith _ -> true | _ -> false) then fail("") else skip in
        let else_ = List.fold_left (fun accu (x : (pattern * mterm)) ->
            match x with
            | {node = Pwild; _}, e -> process_mterm ctx e
            | _ -> accu) default_ ps in
        List.fold_right (fun (x : (pattern * mterm)) accu ->
            let mk_cond id =
              begin
                let val_enum id = mk_mterm (Mvar (mk_id val_v id, Vlocal, Tnone, Dnone)) type_v in
                mk_mterm (Mequal (type_v, v, val_enum id)) tbool
              end
            in
            let mk_if cond then_ else_ =
              match mt.node with
              | Mexprmatchwith _ -> mk_mterm (Mexprif (cond, then_, else_))  mt.type_
              | Mmatchwith     _ -> mk_mterm (Mif (cond, then_, Some else_)) tunit
              | _ -> assert false
            in
            match x with
            | {node = Pconst (id, _); _}, e -> (* FIXME: matchwith *)
              begin
                let e = process_mterm ctx e in
                let cond = mk_cond id in
                mk_if cond e accu
              end
            | _ -> accu
          ) ps else_
      end
     | _ -> map_mterm (process_mterm ctx) mt
     in *)
  let process_type t =
    match get_ntype t with
    | Tenum _ -> tint
    | _ -> t
  in
  let process_functions f =
    let process_fs (fs : function_struct) =
      fs
    in
    let process_node (node : function_node) : function_node =
      match node with
      | Function (fs, type_) -> Function (process_fs fs, process_type type_)
      | Getter   (fs, type_) -> Getter   (process_fs fs, process_type type_)
      | Entry     fs         -> Entry    (process_fs fs)
    in
    { f with
      node = process_node f.node;
    }
  in

  let remove_enum model =
    let rec for_type (t : type_) : type_ =
      match get_ntype t with
      | Tenum _ -> tint
      | _ -> map_type for_type t
    in

    let for_mterm (mt : mterm) : mterm =
      let rec aux (mt : mterm) : mterm =
        {
          mt with
          node = (map_term_node_internal id id for_type) aux mt.node;
          type_ = for_type mt.type_;
        }
      in
      aux mt
    in

    let for_functions f =
      let for_fs (fs : function_struct) =
        { fs with
          args = List.map (fun ((a, b, c) : argument) -> a, for_type b, Option.map for_mterm c ) fs.args;
          body = for_mterm fs.body;
        }
      in
      let for_fnode (node : function_node) : function_node =
        match node with
        | Function (fs, type_) -> Function (for_fs fs, for_type type_)
        | Getter   (fs, type_) -> Getter   (for_fs fs, for_type type_)
        | Entry     fs         -> Entry    (for_fs fs)
      in
      { f with
        node = for_fnode f.node;
      }
    in

    let for_storage_item (si : storage_item) : storage_item =
      { si with
        typ     = for_type si.typ;
        default = for_mterm si.default;
      }
    in

    let for_decl (d : decl_node) : decl_node =
      match d with
      | Dvar v -> Dvar {v with default = Option.map for_mterm v.default;}
      | Dasset a -> Dasset { a with
                             values = List.map
                                 (fun (ai : asset_item) ->
                                    { ai with
                                      type_ = for_type ai.type_;
                                      default = Option.map for_mterm ai.default
                                    }) a.values;
                             init = List.map for_mterm a.init}
      | _ -> d
    in

    { model with
      functions = List.map for_functions model.functions;
      storage = List.map for_storage_item model.storage;
      decls = List.map for_decl model.decls;
    }
  in


  { model with
    decls = process_decls model.decls;
    functions = List.map process_functions model.functions;
  }
  (* |> map_mterm_model process_mterm *)
  |> remove_enum

(* end enum *)

type enum_info = {
  type_   : type_;
  fitems  : (mterm list -> mterm) MapString.t;
  fmatch  : (type_ option) -> mterm -> (pattern * mterm) list -> mterm;
}

let remove_enum (model : model) : model =

  let _remove_cmp_enum (model : model) : model =
    let mk_exprmatchwith dir v id =
      let t = mk_mterm (Mbool true) tbool in
      let f = mk_mterm (Mbool false)tbool in
      let cv, wv =
        match dir with
        | `Pos -> t, f
        | `Neg -> f, t
      in
      let pattern_const = mk_pattern (Pconst (id, [])), cv in (* FIXME: matchwith *)
      let pattern_wild  = mk_pattern Pwild, wv in

      let l = [pattern_const; pattern_wild] in
      mk_mterm (Mexprmatchwith (v, l)) tbool
    in
    let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
      match mt.node with
      | Mequal  (_, ({type_ = ((Tstate | Tenum _), _)} as v), {node = Menumval (id, _, _)}) -> mk_exprmatchwith `Pos v id
      | Mequal  (_, {node = Menumval (id, _, _)}, ({type_ = ((Tstate | Tenum _), _)} as v)) -> mk_exprmatchwith `Pos v id
      | Mnequal (_, ({type_ = ((Tstate | Tenum _), _)} as v), {node = Menumval (id, _, _)}) -> mk_exprmatchwith `Neg v id
      | Mnequal (_, {node = Menumval (id, _, _)}, ({type_ = ((Tstate | Tenum _), _)} as v)) -> mk_exprmatchwith `Neg v id
      | _ -> map_mterm (aux ctx) mt
    in
    map_mterm_model aux model
  in

  let process_asset_state (model : model) : model =
    let get_state_lident an = dumloc ("state_" ^ an) in

    let map = ref MapString.empty in

    let for_decl (d : decl_node) : decl_node =
      let for_asset (a : asset) =
        match a.state with
        | Some id ->
          begin
            let enum    = Utils.get_enum model (unloc id) in
            let name    = get_state_lident (unloc a.name) in
            let typ     = tenum id in
            let e_val   = enum.initial in
            let default = mk_enum_value e_val id in

            map := MapString.add (unloc a.name) default !map;
            let item = mk_asset_item name typ typ ~default:default in
            let init_items = List.map (fun (x : mterm) ->
                match x.node with
                | Masset l -> {x with node = Masset(l @ [default]) }
                | _ -> x) a.init in
            { a with values = a.values @ [item]; state = None; init = init_items }
          end
        | None -> a
      in
      match d with
      | Dasset a -> Dasset (for_asset a)
      | _ -> d
    in

    let model = { model with decls = List.map for_decl model.decls} in

    let rec aux ctx (mt : mterm) : mterm =
      match mt.node, mt.type_ with
      | Mvar (an, Vassetstate k, _, _), _ -> begin
          let an = unloc an in
          let i = get_state_lident an in
          mk_mterm (Mdotassetfield (dumloc an, k, i)) mt.type_
        end
      | Massign (op, _, Aassetstate (an, k), v), _ ->
        let i = get_state_lident an in
        mk_mterm (Mupdate (an, k, [(i, op, v) ])) tunit

      | Masset l, (Tasset an, _) when MapString.mem (unloc an) !map ->
        let default : mterm = MapString.find (unloc an) !map in
        let l = List.map (aux ctx) l in
        {mt with node = Masset (l @ [default]) }

      | _ -> map_mterm (aux ctx) mt
    in
    map_mterm_model aux model
  in

  let map =
    let mk_enum_info (e : enum) : enum_info =
      let without_args = List.for_all (fun (x : enum_item) -> List.is_empty x.args) e.values in
      let mk_args_type args =
        match args with
        | []  -> tunit
        | [t] -> t
        | _   -> ttuple args
      in
      let mk_or l =
        match List.rev l with
        | z::q -> List.fold_right (fun x accu -> tor x accu) (List.rev q) z
        | _    -> assert false
      in
      let mk_type _  =
        if without_args
        then tnat
        else begin
          let f = mk_args_type in
          let l = List.map (fun (x : enum_item) -> f x.args) e.values in
          match List.rev l with
          | []        -> assert false
          | [a]       -> a
          | _         -> mk_or l
        end
      in
      let mk_items _ =
        if without_args
        then
          begin
            List.fold_lefti (fun i accu (x : enum_item) ->
                MapString.add (unloc x.name) (fun _ -> mk_nat i) accu)
              MapString.empty e.values
          end
        else begin
          let f = mk_args_type in
          let g xs =
            match xs with
            | [] -> unit
            | [x] -> x
            | _ -> mk_tuple xs
          in
          let values = e.values in
          let l = List.map (fun (x : enum_item) -> f x.args) values in
          List.fold_lefti (fun i accu (x : enum_item) ->
              let fr l init = List.fold_right (fun x accu -> mk_right x accu) l init in
              let remove_last l =
                match List.rev l with
                | [] -> l
                | _::q -> List.rev q
              in
              let f =
                if (i = List.length values - 1)
                then
                  fun xs -> fr (remove_last l) (g xs)
                else
                  let l0, l1 = List.cut (i + 1) l in
                  let l0 = remove_last l0 in
                  let lt = mk_or l1 in
                  (fun xs -> fr l0 (mk_left (lt) (g xs)))
              in
              MapString.add (unloc x.name) f accu
            ) MapString.empty values
        end
      in
      let mk_match (rt : type_ option) ev (ps : (pattern * mterm) list) =
        if without_args
        then
          begin
            match ps with
            | [{node = Pwild; _}, v] -> v
            | _ -> begin
                let (init, lps) : mterm * ((pattern * mterm) list) =
                  match List.rev ps with
                  | [] -> assert false
                  | ({node = Pwild; _}, v)::q -> v, q
                  | ({node = (Pconst (_, _)); _}, v)::q -> v, q
                in
                let map : mterm MapString.t =
                  List.fold_lefti
                    (fun i accu (x : enum_item) ->
                       MapString.add (unloc x.name) (mk_nat i) accu)
                    MapString.empty e.values
                in
                let ivar = dumloc "_tmp" in
                let mvar : mterm = mk_mvar ivar tnat in
                let mk_cond (id : ident) = mk_mterm (Mequal (tnat, MapString.find id map, mvar)) tbool in
                let mk_if (id : ident) (v : mterm) (accu : mterm) : mterm =
                  match rt with
                  | None    -> mk_mterm (Mif (mk_cond id, v, Some accu)) tunit
                  | Some rt -> mk_mterm (Mexprif (mk_cond id, v, accu))  rt
                in
                let v =
                  List.fold_left (fun accu (p, v: pattern * mterm) ->
                      match p.node with
                      | Pwild -> assert false
                      | Pconst (id, _) -> mk_if (unloc id) v accu)
                    init lps
                in
                mk_letin ivar ev v
              end
          end
        else begin
          let dvopt = List.fold_left (fun accu (p, v : pattern * mterm) -> match p.node with | Pwild -> Some v | _ -> accu) None ps in
          let seek_value (enum_item : enum_item) =
            let v = List.fold_lefti (fun idx accu (p, v : pattern * mterm)  ->
                match p.node with
                | Pconst (i, args) when String.equal (unloc i) (unloc enum_item.name) -> Some (args, v, idx)
                | _ -> accu) None ps
            in
            let id_var_or = dumloc ("_var_or") in
            match v, dvopt with
            | Some ([], v, _), _  -> id_var_or, v
            | Some ([a], v, _), _ -> a, v
            | Some (l, v, _), _ -> begin
                let ts : type_ list =  enum_item.args in
                let var = mk_mvar id_var_or (ttuple ts) in
                let l2 : (int * lident) list = List.mapi (fun (i : int) (x : lident) -> i, x) l in
                let v = List.fold_right (fun (i, x : int * lident) (accu : mterm) -> accu |> mk_letin x (mk_tupleaccess i var)) l2 v in
                id_var_or, v
              end
            | _, Some v        -> id_var_or, v
            | _                -> assert false
          in

          let lvalues = List.map (fun (x : enum_item) -> seek_value x) e.values in
          let mk_matchor (e, a, b, c, d) : mterm =
            match rt with
            | None   -> mk_mterm (Minstrmatchor (e, a, b, c, d)) tunit
            | Some t -> mk_mterm (Mmatchor (e, a, b, c, d)) t
          in

          match lvalues with
          | []   -> assert false
          | (a, b)::q ->
            let x, y = begin
              match List.rev q with
              | [] -> assert false
              | (c, d)::u -> List.fold_right (fun (g, h) (i, accu) ->
                  let v = mk_mvar g (tunit) in
                  g, mk_matchor (v, g, h, i, accu)
                ) (List.rev u) (c, d)
            end
            in
            mk_matchor (ev, a, b, x, y)

        end
      in
      {
        type_  = mk_type ();
        fitems = mk_items ();
        fmatch = (fun rt e ps -> mk_match rt e ps);
      } in
    List.fold_left (fun accu x ->
        match x with
        | Denum e -> begin
            match unloc e.name with
            | "state" -> List.fold_left (fun accu x -> MapString.add x (mk_enum_info e) accu) accu ["state"; "$state"]
            | _       -> MapString.add (unloc e.name) (mk_enum_info e) accu
          end
        | _ -> accu) MapString.empty model.decls
  in

  let get_enum_id_opt t = match get_ntype t with | Tstate -> Some "$state" | Tenum eid -> Some (unloc eid) | _ -> None in
  let get_enum_id t     = t |> get_enum_id_opt |> Option.get in
  let is_tenum t        = t |> get_enum_id_opt |> Option.is_some in

  let get_info eid =
    if not (MapString.mem eid map) then (Format.eprintf "error get_info: %s@\n" eid; assert false);
    MapString.find eid map
  in

  let for_type t : type_ =
    let rec aux t =
      match get_ntype t with
      | Tstate -> tnat
      | Tenum id -> begin
          let info : enum_info = get_info (unloc id) in
          info.type_
        end
      | _ -> map_type aux t
    in
    aux t
  in

  let for_mterm (mt : mterm) : mterm =
    let rec aux (mt : mterm) =
      let state = "_state" in
      let dstate = dumloc state in
      let for_matchwith rt (e : mterm) ps : mterm =
        let eid = get_enum_id e.type_ in
        let info : enum_info = get_info eid in
        let e = aux e in
        let ps = List.map (fun (p, x) -> p, aux x) ps in
        info.fmatch rt e ps
      in
      match mt.node with
      | Mvar (_, Vstate, _, _)    -> mk_svar dstate tnat
      | Massign (_, _, Astate, v) -> {mt with node = Massign (ValueAssign, tnat, Avarstore dstate, aux v)}
      | Menumval (id, args, eid)  -> begin
          let args = List.map aux args in
          let info : enum_info = get_info eid in
          let f = MapString.find (unloc id) info.fitems in
          f args
        end
      | Mmatchwith     (e, ps) when is_tenum (e.type_) -> for_matchwith None e ps
      | Mexprmatchwith (e, ps) when is_tenum (e.type_) -> for_matchwith (Some mt.type_) e ps
      | _ -> let mt = map_mterm ~ft:for_type aux mt in { mt with type_ = for_type mt.type_ }
    in
    aux mt
  in

  let clean model =
    let decls =
      List.fold_right (fun x accu ->
          match x with
          | Denum ({name = {pldesc = "state"}} as e) ->
            let initial = e.initial in
            let info : enum_info = get_info "state" in
            let dv = (MapString.find (unloc initial) info.fitems) [] in
            (Dvar (mk_var (dumloc "_state") tnat tnat VKvariable ~default:dv ))::accu
          | Denum _ -> accu
          | x -> x::accu
        ) model.decls []
    in
    { model with decls = decls }
  in
  model
  |> process_asset_state
  |> clean
  |> map_model (fun _ x -> x) for_type for_mterm


let remove_cmp_bool (model : model) : model =
  let rec aux c (mt : mterm) : mterm =
    let f = aux c in
    let not x = mk_mterm (Mnot x) tbool in
    let vtrue = mk_mterm (Mbool true) tbool in
    let vfalse = mk_mterm (Mbool false) tbool in
    match mt.node with
    | Mequal (_, {node = Mbool false; _}, {node = Mbool false; _})  -> vtrue
    | Mequal (_, {node = Mbool false; _}, {node = Mbool true; _})   -> vfalse
    | Mequal (_, {node = Mbool true; _},  {node = Mbool false; _})  -> vfalse
    | Mequal (_, {node = Mbool true; _},  {node = Mbool true; _})   -> vtrue

    | Mnequal (_, {node = Mbool false; _}, {node = Mbool false; _}) -> vfalse
    | Mnequal (_, {node = Mbool false; _}, {node = Mbool true; _})  -> vtrue
    | Mnequal (_, {node = Mbool true; _},  {node = Mbool false; _}) -> vtrue
    | Mnequal (_, {node = Mbool true; _},  {node = Mbool true; _})  -> vfalse

    | Mequal (_, lhs, {node = Mbool true; _})  -> f lhs
    | Mequal (_, lhs, {node = Mbool false; _}) -> not (f lhs)
    | Mequal (_, {node = Mbool true; _}, rhs)  -> f rhs
    | Mequal (_, {node = Mbool false; _}, rhs) -> not (f rhs)

    | Mnequal (_, lhs, {node = Mbool true; _})  -> not (f lhs)
    | Mnequal (_, lhs, {node = Mbool false; _}) -> f lhs
    | Mnequal (_, {node = Mbool true; _}, rhs)  -> not (f rhs)
    | Mnequal (_, {node = Mbool false; _}, rhs) -> f rhs

    | _ -> map_mterm (aux c) mt
  in
  Model.map_mterm_model aux model

let update_nat_int_rat (model : model) : model =
  let lit_num_some (x : mterm) =
    match x.node with
    | Mint n
    | Mnat n -> Some n
    | _ -> None
  in
  let is_lit_num      (x : mterm) = x |> lit_num_some |> Option.is_some in
  let extract_lit_num (x : mterm) = x |> lit_num_some |> Option.get in

  let lit_rat_some (x : mterm) =
    match x.node with
    | Mtuple [{node = Mint n}; {node = Mnat d}] -> Some (n, d)
    | _ -> None
  in
  let is_lit_rat      (x : mterm) = x |> lit_rat_some |> Option.is_some in
  let extract_lit_rat (x : mterm) = x |> lit_rat_some |> Option.get in

  let neg x = Big_int.sub_big_int Big_int.zero_big_int x in

  let rec aux c (mt : mterm) : mterm =
    match mt.node with
    | Mnattoint {node = Mnat n; _} -> {mt with node = Mint n}
    | Mnattorat x
    | Minttorat x -> begin
        let x = aux c x in
        if is_lit_num x
        then x |> extract_lit_num |> (fun x -> Utils.mk_rat x (Big_int.unit_big_int))
        else mt
      end
    | Muminus x -> begin
        let x = aux c x in
        if is_lit_num x
        then x |> extract_lit_num |> neg |> (fun x -> mk_mterm (Mint x) tint)
        else mt
      end
    | Mratuminus x -> begin
        let x = aux c x in
        if is_lit_rat x
        then x |> extract_lit_rat |> (fun (x, y) -> (neg x, y)) |> (fun (x, y) -> Utils.mk_rat x y)
        else mt
      end
    | Mratarith (op, lhs, rhs) -> begin
        let lhs, rhs = (aux c lhs), (aux c rhs) in
        if (is_lit_rat lhs) && (is_lit_rat rhs)
        then begin
          let an, ad = extract_lit_rat lhs in
          let bn, bd = extract_lit_rat rhs in
          let (+) a b   = Big_int.add_big_int a b in
          let (-) a b   = Big_int.sub_big_int a b in
          let ( * ) a b = Big_int.mult_big_int a b in
          let x, y =
            match op with
            | Rplus  -> ((an * bd) + (bn * ad), ad * bd)
            | Rminus -> ((an * bd) - (bn * ad), ad * bd)
            | Rmult  -> (an * bn, ad * bd)
            | Rdiv   -> (an * bd, ad * bn)
          in
          Utils.mk_rat x y
        end
        else mt
      end
    | _ -> map_mterm (aux c) mt
  in
  Model.map_mterm_model aux model

let remove_rational (model : model) : model =
  let one           = mk_mterm (Mnat (Big_int.unit_big_int)) tnat in
  let mk_rat_one x  = mk_mterm (Mtuple [x ; one]) trat in
  let nat_to_int e  = mk_mterm (Mnattoint e) tint in

  let for_type (t : type_) : type_ =
    let rec aux t =
      match get_ntype t with
      | Tbuiltin Brational -> trat
      | _ -> map_type aux t
    in
    aux t
  in

  let to_int (x : mterm) =
    match get_ntype x.type_ with
    | Tbuiltin Bnat -> mk_mterm (Mnattoint x) tint
    | Tbuiltin Bint -> x
    | _ -> assert false
  in
  let to_rat (x : mterm) =
    match get_ntype x.type_ with
    | Tbuiltin Bnat -> mk_rat_one (nat_to_int x)
    | Tbuiltin Bduration
    | Tbuiltin Bint -> mk_rat_one x
    | Tbuiltin Brational
    | Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)] -> x
    | _ -> Format.printf "%a@." pp_type_ x.type_; assert false
  in
  let for_mterm mt =
    let rec aux (mt : mterm) : mterm =
      let ret = mt.type_ in

      let for_unary op (v : mterm) =
        match op, get_ntype ret, get_ntype v.type_ with
        | `Uminus, (Tbuiltin Brational | Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)]), Tbuiltin Brational ->
          let v = v |> aux |> to_rat in
          mk_mterm (Mratuminus v) trat
        | _ -> map_mterm aux mt
      in

      let for_arith op (a, b : mterm * mterm) =
        match get_ntype ret with
        | Tbuiltin Brational
        | Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)] -> begin
            match op, a.type_, b.type_ with
            | _ -> begin
                let f =
                  match op with
                  | `Plus   -> Some (fun x y -> mk_mterm (Mratarith (Rplus,  x, y)) trat)
                  | `Minus  -> Some (fun x y -> mk_mterm (Mratarith (Rminus, x, y)) trat)
                  | `Mult   -> Some (fun x y -> mk_mterm (Mratarith (Rmult,  x, y)) trat)
                  | `Divrat -> Some (fun x y -> mk_mterm (Mratarith (Rdiv,   x, y)) trat)
                  | _       -> None
                in
                match f with
                | Some f ->
                  let lhs = a |> aux |> to_rat in
                  let rhs = b |> aux |> to_rat in
                  f lhs rhs
                | None -> map_mterm aux mt
              end
          end
        | Tbuiltin Bcurrency -> begin
            match op, get_ntype a.type_, get_ntype b.type_ with
            | `Mult, Tbuiltin Bnat,      Tbuiltin Bcurrency
            | `Mult, Tbuiltin Bint,      Tbuiltin Bcurrency
            | `Mult, Tbuiltin Brational, Tbuiltin Bcurrency -> begin
                let lhs = a |> aux |> to_rat in
                let rhs = b |> aux in
                mk_mterm (Mrattez (lhs, rhs)) ttez
              end
            | `Diveuc, Tbuiltin Bcurrency, Tbuiltin Bnat
            | `Diveuc, Tbuiltin Bcurrency, Tbuiltin Bint -> begin
                let inv_rat v = mk_mterm (Mratarith (Rdiv, to_rat one, v)) trat in
                let lhs = b |> aux |> to_rat |> inv_rat in
                let rhs = a |> aux in
                mk_mterm (Mrattez (lhs, rhs)) ttez
              end
            | _ -> map_mterm aux mt
          end
        | Tbuiltin Bduration -> begin
            match op, get_ntype a.type_, get_ntype b.type_ with
            | `Mult, Tbuiltin Bnat,      Tbuiltin Bduration
            | `Mult, Tbuiltin Bint,      Tbuiltin Bduration -> begin
                let lhs = a |> aux |> to_rat in
                let rhs = b |> aux in
                mk_mterm (Mmult (lhs, rhs)) tduration
              end
            | _ -> map_mterm aux mt
          end
        | _ -> map_mterm aux mt
      in

      let for_fun (f : mterm list -> type_ -> mterm) (l : mterm list) : mterm =
        let l = List.map (fun (x : mterm) ->
            match get_ntype ret with
            | Tbuiltin Brational
            | Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)] -> begin
                x |> aux |> to_rat
              end
            | Tbuiltin Bint -> begin
                x |> aux |> to_int
              end
            | _ -> x |> aux) l
        in
        let ret =
          match get_ntype ret with
          |  Tbuiltin Brational -> trat
          | _ -> ret
        in
        f l ret
      in

      let for_cmp op (a, b : mterm * mterm) =
        match get_ntype a.type_, get_ntype b.type_ with
        | Tbuiltin Bint, Tbuiltin Bnat
        | Tbuiltin Bnat, Tbuiltin Bint ->
          let f =
            match op with
            | `Eq -> (fun x y -> mk_mterm (Mequal  (tint, x, y)) tbool)
            | `Ne -> (fun x y -> mk_mterm (Mnequal (tint, x, y)) tbool)
            | `Le -> (fun x y -> mk_mterm (Mle     (x, y))       tbool)
            | `Lt -> (fun x y -> mk_mterm (Mlt     (x, y))       tbool)
            | `Ge -> (fun x y -> mk_mterm (Mge     (x, y))       tbool)
            | `Gt -> (fun x y -> mk_mterm (Mgt     (x, y))       tbool)
          in
          let lhs = a |> aux |> to_int in
          let rhs = b |> aux |> to_int in
          f lhs rhs

        | Tbuiltin Brational, _
        | Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)], _
        | _, Tbuiltin Brational
        | _, Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)] -> begin
            let f =
              match op with
              | `Eq -> (fun x y -> mk_mterm (Mrateq  (x, y)) tbool)
              | `Ne -> (fun x y -> mk_mterm (Mrateq  (x, y)) tbool |> (fun x -> mk_mterm (Mnot x) tbool))
              | `Le -> (fun x y -> mk_mterm (Mratcmp (Le,  x, y)) tbool)
              | `Lt -> (fun x y -> mk_mterm (Mratcmp (Lt,  x, y)) tbool)
              | `Ge -> (fun x y -> mk_mterm (Mratcmp (Ge,  x, y)) tbool)
              | `Gt -> (fun x y -> mk_mterm (Mratcmp (Gt,  x, y)) tbool)
            in
            let lhs = a |> aux |> to_rat in
            let rhs = b |> aux |> to_rat in
            f lhs rhs
          end
        | _ -> map_mterm aux mt
      in

      match mt.node with
      | Mrational (n, d)   -> Utils.mk_rat n d
      | Mplus     (a, b)   -> for_arith `Plus   (a, b)
      | Mminus    (a, b)   -> for_arith `Minus  (a, b)
      | Mmult     (a, b)   -> for_arith `Mult   (a, b)
      | Mdivrat   (a, b)   -> for_arith `Divrat (a, b)
      | Mdiveuc   (a, b)   -> for_arith `Diveuc (a, b)
      | Mmodulo   (a, b)   -> for_arith `Modulo (a, b)
      | Muminus    v       -> for_unary `Uminus v
      | Mmax      (a, b)   -> for_fun (fun l ret -> mk_mterm (Mmax(List.nth l 0, List.nth l 1)) ret) [a; b]
      | Mmin      (a, b)   -> for_fun (fun l ret -> mk_mterm (Mmin(List.nth l 0, List.nth l 1)) ret) [a; b]
      | Mequal  (_, a, b)  -> for_cmp `Eq (a, b)
      | Mnequal (_, a, b)  -> for_cmp `Ne (a, b)
      | Mlt     (a, b)     -> for_cmp `Lt (a, b)
      | Mle     (a, b)     -> for_cmp `Le (a, b)
      | Mgt     (a, b)     -> for_cmp `Gt (a, b)
      | Mge     (a, b)     -> for_cmp `Ge (a, b)
      | _                  -> map_mterm aux mt ~ft:for_type
    in
    aux mt
  in

  model
  |> map_model (fun _ x -> x) for_type for_mterm
  |> update_nat_int_rat


let replace_date_duration_by_timestamp (model : model) : model =
  let is_rat      t = match get_ntype t with | Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)] -> true     | _ -> false in
  let is_date     t = match get_ntype t with | Tbuiltin Bdate -> true     | _ -> false in
  let is_duration t = match get_ntype t with | Tbuiltin Bduration -> true | _ -> false in
  let process_type t : type_ =
    let rec aux t =
      match get_ntype t with
      | Tbuiltin Bdate     -> ttimestamp
      | Tbuiltin Bduration -> tint
      | _ -> map_type aux t
    in
    aux t
  in
  let rec to_timestamp (x : mterm) =
    let mk n = mk_mterm (Mtimestamp n) ttimestamp in
    let extract (x : mterm) = match x.node with | Mtimestamp n -> n | _ -> assert false in
    let f = to_timestamp in
    let g = extract |@ f in
    match x.node with
    | Mdate d       -> mk (Core.date_to_timestamp d)
    | Mduration d   -> mk (Core.duration_to_timestamp d)
    | Mint i        -> mk i
    | Mnat n        -> mk n
    | Mnow          -> mk (Unix.time () |> int_of_float |> Big_int.big_int_of_int)
    | Mplus  (a, b) -> mk (Big_int.add_big_int (g a) (g b))
    | Mminus (a, b) -> mk (Big_int.sub_big_int (g a) (g b))
    | Mtimestamp _  -> x
    | _ ->
      begin
        Format.eprintf "cannot transform to timestamp: %a@.%a@." Printer_model.pp_mterm x pp_mterm x;
        assert false
      end
  in
  let process_mterm mt =
    let rec aux (mt : mterm) : mterm =
      match mt.node, mt.type_ with
      | Mdate d,_      -> mk_mterm (Mtimestamp (Core.date_to_timestamp d)) ttimestamp
      | Mduration d, _ -> mk_mterm (Mint (Core.duration_to_timestamp d)) tint
      | Mnow, _        -> mk_mterm (Mnow) ttimestamp
      | Mmult (a, b), t when is_duration t && is_rat a.type_ && is_duration b.type_ ->
        let a = aux a in
        let b = aux b in
        mk_mterm (Mratdur (a, b)) tint
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
          match get_ntype t with
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
                           });
             init = List.map process_mterm a.init;
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
           | Getter (fs, ret)   -> Getter   (process_fs fs, process_type ret)
           | Entry fs           -> Entry    (process_fs fs)
         );
        })
  }
  |> update_nat_int_rat

let abs_tez model : model =
  let is_cur (mt : mterm) =
    match get_ntype mt.type_ with
    | Tbuiltin Bcurrency -> true
    | _ -> false
  in
  let is_int (mt : mterm) =
    match get_ntype mt.type_, mt.node with
    | _, Mabs _ -> false
    | Tbuiltin Bint, _ -> true
    | _ -> false
  in
  let abs mt = mk_mterm (Mabs mt) tnat in
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
    | Massign (op, _, Aasset (an, fn, key), v) ->
      let an = unloc an in
      let l = [(fn, op, v)] in
      mk_mterm (Mupdate (an, key, l)) tunit

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
      | Mvar (_, Vstorecol, _, _) -> true

      | Mvar (id, Vlocal, _, _) -> not (List.exists (fun a -> String.equal a (unloc id)) env)

      (* asset api *)
      | Mselect (_, CKview c, _, _, _) -> is_implicit_sort env c
      | Mhead   (_, CKview c, _) -> is_implicit_sort env c
      | Mtail   (_, CKview c, _) -> is_implicit_sort env c

      | _ -> false
    in
    let get_crit an : (ident * sort_kind) list =
      let k, _ = Model.Utils.get_asset_key model an in
      [(k, SKasc)]
    in
    let create_sort an c =
      let crit = get_crit an in
      mk_mterm (Msort (an, CKview c, crit)) c.type_
    in
    let extract_asset_name (c : mterm) =
      match get_ntype c.type_ with
      | Tcontainer ((Tasset an, _), _) -> unloc an
      | _ -> assert false
    in
    match mt.node with
    | Mletin ([id], v, Some (Tcontainer ((Tasset an, _), c), _), body, o) ->
      let env =
        match is_sorted body with
        | true -> (unloc id)::env
        | _ -> env
      in
      let body = aux env ctx body in
      { mt with node = Mletin ([id], v, Some (mktype (Tcontainer (tasset an, c))), body, o)}

    | Mnth (an, CKview c, idx) when is_implicit_sort env c ->
      { mt with node = Mnth (an, CKview (create_sort an c), idx) }

    | Mhead (an, CKview c, idx) when is_implicit_sort env c ->
      { mt with node = Mhead (an, CKview (create_sort an c), idx) }

    | Mtail (an, CKview c, idx) when is_implicit_sort env c ->
      { mt with node = Mtail (an, CKview (create_sort an c), idx) }

    | Mfor (a, ICKfield (aan, fn, c), body, lbl) when is_implicit_sort env c ->
      let body = aux env ctx body in
      let an = extract_asset_name c in
      mk_mterm (Mfor (a, ICKfield (aan, fn, create_sort an c), body, lbl)) tunit

    | Mfor (a, ICKview c, body, lbl) when is_implicit_sort env c ->
      let body = aux env ctx body in
      let an = extract_asset_name c in
      mk_mterm (Mfor (a, ICKview (create_sort an c), body, lbl)) tunit

    | Mfor (a, ICKset c, body, lbl) when is_implicit_sort env c ->
      let body = aux env ctx body in
      let an = extract_asset_name c in
      mk_mterm (Mfor (a, ICKset (create_sort an c), body, lbl)) tunit

    | Mfor (a, ICKlist c, body, lbl) when is_implicit_sort env c ->
      let body = aux env ctx body in
      let an = extract_asset_name c in
      mk_mterm (Mfor (a, ICKlist (create_sort an c), body, lbl)) tunit

    | _ -> map_mterm (aux env ctx) mt
  in
  map_mterm_model (aux []) model

let is_whyml_keyword = function
  |  "abstract"
  |  "absurd"
  |  "alias"
  |  "any"
  |  "as"
  |  "assert"
  |  "assume"
  |  "at"
  |  "axiom"
  |  "begin"
  |  "break"
  |  "by"
  |  "check"
  |  "clone"
  |  "coinductive"
  |  "constant"
  |  "continue"
  |  "diverges"
  |  "do"
  |  "done"
  |  "downto"
  |  "else"
  |  "end"
  |  "ensures"
  |  "epsilon"
  |  "exception"
  |  "exists"
  |  "export"
  |  "false"
  |  "float"
  |  "for"
  |  "forall"
  |  "fun"
  |  "function"
  |  "ghost"
  |  "goal"
  |  "if"
  |  "import"
  |  "in"
  |  "inductive"
  |  "invariant"
  |  "label"
  |  "lemma"
  |  "let"
  |  "match"
  |  "meta"
  |  "module"
  |  "mutable"
  |  "not"
  |  "old"
  |  "partial"
  |  "predicate"
  |  "private"
  |  "pure"
  |  "raise"
  |  "raises"
  |  "range"
  |  "reads"
  |  "rec"
  |  "ref"
  |  "requires"
  |  "return"
  |  "returns"
  |  "scope"
  |  "so"
  |  "then"
  |  "theory"
  |  "to"
  |  "true"
  |  "try"
  |  "type"
  |  "use"
  |  "val"
  |  "variant"
  |  "while"
  |  "with"
  |  "writes"
    -> true
  | _ -> false


let replace_whyml_ident (model : model) : model =
  let f _env id = if is_whyml_keyword id then ("_"^id) else id in
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


(* let replace_key_by_asset (model : model) : model =
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
   Model.map_mterm_model aux model *)

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
              let mt = mk_mterm node tunit in
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

    | Massign (op, t, Avar l, r) ->
      let re, ra = f r in
      process (mk_mterm (Massign (op, t, Avar l, re)) mt.type_) ra

    | Massign (op, t, Avarstore l, r)  ->
      let re, ra = f r in
      process (mk_mterm (Massign (op, t, Avarstore l, re)) mt.type_) ra

    | Massign (op, t, Aasset (an, fn, k), v) ->
      let ke, ka = f k in
      let ve, va = f v in
      process (mk_mterm (Massign (op, t, Aasset (an, fn, ke), ve)) mt.type_) (ka @ va)

    | Massign (op, t, Arecord (rn, fn, r), v) ->
      let re, ra = f r in
      let ve, va = f v in
      process (mk_mterm (Massign (op, t, Arecord (rn, fn, re), ve)) mt.type_) (ra @ va)

    | Massign (op, t, Astate, x) ->
      let xe, xa = f x in
      process (mk_mterm (Massign (op, t, Astate, xe)) mt.type_) xa

    | Massign (op, t, Aassetstate (an, k), v) ->
      let ke, ka = f k in
      let ve, va = f v in
      process (mk_mterm (Massign (op, t, Aassetstate (an, ke), ve)) mt.type_) (ka @ va)


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

    | Minstrmatchoption (x, i, ve, ne) ->
      let xe, xa   = f x  in
      let vee, vea = f ve in
      let nee, nea = f ne in
      process (mk_mterm (Minstrmatchoption (xe, i, vee, nee)) mt.type_) (xa @ vea @ nea)

    | Minstrmatchor (x, lid, le, rid, re) ->
      let xe, xa   = f x  in
      process (mk_mterm (Minstrmatchor (xe, lid, le, rid, re)) mt.type_) (xa)

    | Minstrmatchlist (x, hid, tid, hte, ee) ->
      let xe, xa     = f x  in
      process (mk_mterm (Minstrmatchlist (xe, hid, tid, hte, ee)) mt.type_) (xa)

    | Mfor (i, c, b, lbl) ->
      let ce, ca =
        match c with
        | ICKcoll  an          -> ICKcoll an, []
        | ICKview  v           -> let ve, va = f v in ICKview  ve, va
        | ICKfield (an, fn, v) -> let ve, va = f v in ICKfield (an, fn, ve), va
        | ICKset   v           -> let ve, va = f v in ICKset   ve, va
        | ICKlist  v           -> let ve, va = f v in ICKlist  ve, va
        | ICKmap   v           -> let ve, va = f v in ICKmap   ve, va
      in
      let be = aux ctx b in
      process (mk_mterm (Mfor (i, ce, be, lbl)) mt.type_) ca

    | Miter (i, a, b, c, lbl) ->
      let ae, aa = f a in
      let be, ba = f b in
      let ce = aux ctx c in
      process (mk_mterm (Miter (i, ae, be, ce, lbl)) mt.type_) (aa @ ba)

    | Mwhile (c, b, lbl) ->
      let ce, ca = f c in
      let be = aux ctx b in
      process (mk_mterm (Mwhile (ce, be, lbl)) mt.type_) ca

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

    | Mtransfer (k) ->
      let tre, tra =
        match k with
        | TKsimple (v, d)         -> let ve, va = f v in let de, da = f d in TKsimple (ve, de), va @ da
        | TKcall (v, id, t, d, a) -> let ve, va = f v in let de, da = f d in let ae, aa = f a in TKcall (ve, id, t, de, ae), va @ da @ aa
        | TKentry (v, e, a)       -> let ve, va = f v in let ee, ea = f e in let ae, aa = f a in TKentry (ve, ee, ae), va @ ea @ aa
        | TKself (v, id, args)    -> let ve, va = f v in let args, accu = List.fold_left (fun (args, accu) (id, a) -> let ae, aa = f a in (args @ [id, ae], accu @ aa)) ([], []) args  in TKself (ve, id, args), va @ accu
        | TKoperation op          -> let ope, opa = f op in TKoperation ope, opa
      in
      process (mk_mterm (Mtransfer tre) mt.type_) tra


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

    | Mclear (an, v) ->
      let ve, va =
        match v with
        | CKcoll (d, t) -> CKcoll (d, t), []
        | CKview v  -> let ve, va = f v in CKview ve, va
        | CKfield (an, fn, v, d, t) -> let ve, va = f v in CKfield (an, fn, ve, d, t), va
        | CKdef v -> CKdef v, []
      in
      process (mk_mterm (Mclear (an, ve)) mt.type_) va

    | Mremoveif (an, v, la, b, a) ->
      let ve, va =
        match v with
        | CKcoll (d, t) -> CKcoll (d, t), []
        | CKview v  -> let ve, va = f v in CKview ve, va
        | CKfield (an, fn, v, d, t) -> let ve, va = f v in CKfield (an, fn, ve, d, t), va
        | CKdef v -> CKdef v, []
      in
      let be, ba = f b in
      let ae, aa = List.fold_right (fun v (xe, xa) ->
          let ve, va = f v in
          (ve::xe, va @ xa)) a ([], []) in
      process (mk_mterm (Mremoveif (an, ve, la, be, ae)) mt.type_) (va @ ba @ aa)

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

    | Maddupdate (an, c, k, l) ->
      let ce, ca =
        match c with
        | CKcoll (d, t) -> CKcoll (d, t), []
        | CKview c  -> let ce, ca = f c in CKview ce, ca
        | CKfield (an, fn, c, d, t) -> let ce, ca = f c in CKfield (an, fn, ce, d, t), ca
        | CKdef v -> CKdef v, []
      in
      let ke, ka = f k in
      let le, la = List.fold_right (fun (id, op, v) (xe, xa) ->
          let ve, va = f v in
          ((id, op, ve)::xe, va @ xa)) l ([], []) in
      process (mk_mterm (Maddupdate (an, ce, ke, le)) mt.type_) (ca @ ka @ la)

    | Maddforce (an, v) ->
      let ve, va = f v in
      process (mk_mterm (Maddforce (an, ve)) mt.type_) va

    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model

let replace_dotassetfield_by_dot (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mdotassetfield (an, k, fn) ->
      begin
        let k = aux ctx k in
        let get = build_get (unloc an) k in
        mk_mterm (Mdot (get, fn)) mt.type_
      end
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
      | Mdot (l, r) when is_fun l ->
        begin
          let l, accu = efd_aux accu l in
          let var_id = prefix ^ string_of_int (!cpt) in
          cpt := !cpt + 1;
          let var = mk_mterm (Mvar (dumloc var_id, Vlocal, Tnone, Dnone)) l.type_ in
          let nmt = mk_mterm (Mdot (var, r)) mt.type_ in
          nmt, accu @ [(dumloc var_id, l)]
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
    match mt.node, get_ntype mt.type_ with
    | Mplus (l, r), Tbuiltin Bstring ->
      let l = aux ctx l in
      let r = aux ctx r in
      mk_mterm (Mconcat (l, r)) tstring
    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model

let build_col (an : ident) =
  let dan = dumloc an in
  let type_col = tcollection dan in
  let type_view = tview dan in
  let col : mterm  = mk_mterm (Mvar (dan, Vstorecol, Tnone, Dnone)) type_col in
  mk_mterm (Mcast (type_col, type_view, col)) type_view

let change_type_of_nth (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mnth (an, CKview c, i) ->
      build_get an (mk_mterm (Mnth (an, CKview (aux ctx c), aux ctx i)) ((Utils.get_asset_key model an |> snd)))
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let add_contain_on_get (model : model) : model =

  let for_mterm (mt : mterm) : mterm =

    let rec for_instruction
        (g : 'b -> 'a -> mterm -> mterm)
        (f : 'a -> mterm -> 'a)
        (env : 'b)
        (accu : 'a)
        (mt : mterm) : mterm =
      let aux_env env = for_instruction g f env [] in
      let aux = for_instruction g f env [] in
      let gg = g env in
      match mt.node with

      | Mletin (i, a, t, b, o) ->
        let accu = f accu a in
        let be = aux b in
        let oe = Option.map aux o in
        gg accu (mk_mterm (Mletin (i, a, t, be, oe)) mt.type_)

      | Mdeclvar (_i, _t, v) ->
        let accu = f accu v in
        gg accu mt

      (* assign *)

      | Massign (_op, _, Avar _l, r) ->
        let accu = f accu r in
        gg accu mt

      | Massign (_op, _, Avarstore _l, r)  ->
        let accu = f accu r in
        gg accu mt

      | Massign (_op, _, Aasset (_an, _fn, k), v) ->
        let accu = f accu k in
        let accu = f accu v in
        gg accu mt

      | Massign (_op, _, Arecord (_rn, _fn, r), v) ->
        let accu = f accu r in
        let accu = f accu v in
        gg accu mt

      | Massign (_op, _, Astate, x) ->
        let accu = f accu x in
        gg accu mt

      | Massign (_op, _, Aassetstate (_an, k), v) ->
        let accu = f accu k in
        let accu = f accu v in
        gg accu mt


      (* control *)

      | Mif (c, t, e) ->
        let accu = f accu c in
        let env =
          match c.node with
          | Mcontains(an, _, k) -> [an, k]
          | _ -> []
        in
        let te = aux_env env t in
        let ee = Option.map aux e in
        g env accu (mk_mterm (Mif (c, te, ee)) mt.type_)

      | Mmatchwith (e, l) ->
        let accu = f accu e in
        let ll = List.map (fun (p, e) -> (p, aux e)) l in
        gg accu (mk_mterm (Mmatchwith (e, ll)) mt.type_)

      | Minstrmatchoption (x, i, ve, ne) ->
        let accu = f accu x in
        let vee = aux_env env ve in
        let nee = aux_env env ne in
        gg accu (mk_mterm (Minstrmatchoption (x, i, vee, nee)) mt.type_)

      | Minstrmatchor (x, lid, le, rid, re) ->
        let accu = f accu x in
        let lee = aux_env env le in
        let ree = aux_env env re in
        gg accu (mk_mterm (Minstrmatchor (x, lid, lee, rid, ree)) mt.type_)

      | Minstrmatchlist (x, hid, tid, hte, ee) ->
        let accu = f accu x in
        let htee = aux_env env hte in
        let eee = aux_env env ee in
        gg accu (mk_mterm (Minstrmatchlist (x, hid, tid, htee, eee)) mt.type_)

      | Mfor (i, c, b, lbl) ->
        let accu = fold_iter_container_kind f accu c in
        let be = aux b in
        gg accu (mk_mterm (Mfor (i, c, be, lbl)) mt.type_)

      | Miter (i, a, b, c, lbl) ->
        let accu = f accu a in
        let accu = f accu b in
        let ce = aux c in
        gg accu (mk_mterm (Miter (i, a, b, ce, lbl)) mt.type_)

      | Mwhile (c, b, lbl) ->
        let accu = f accu c in
        let be = aux b in
        gg accu (mk_mterm (Mwhile (c, be, lbl)) mt.type_)

      | Mreturn x ->
        let accu = f accu x in
        gg accu (mk_mterm (Mreturn (x)) mt.type_)


      (* effect *)

      | Mfail v ->
        let accu = match v with
          | Invalid x -> f accu x
          | _ -> []
        in
        gg accu mt

      | Mtransfer tr ->
        let accu = fold_transfer_kind f accu tr in
        gg accu mt


      (* asset api effect *)

      | Maddasset (_an, i) ->
        let accu = f accu i in
        gg accu mt

      | Maddfield (_an, _fn, c, i) ->
        let accu = f accu c in
        let accu = f accu i in
        gg accu mt

      | Mremoveasset (_an, i) ->
        let accu = f accu i in
        gg accu mt

      | Mremovefield (_an, _fn, c, i) ->
        let accu = f accu c in
        let accu = f accu i in
        gg accu mt

      | Mremoveif (_an, v, _la, b, a) ->
        let accu =
          match v with
          | CKcoll _                 -> accu
          | CKview c                 -> f accu c
          | CKfield (_, _, c, _, _)  -> f accu c
          | CKdef _                  -> accu
        in
        let accu = f accu b in
        let accu = List.fold_right (fun v accu -> f accu v) a accu in
        gg accu mt

      | Mclear (_an, v) ->
        let accu =
          match v with
          | CKcoll _                -> accu
          | CKview c                -> f accu c
          | CKfield (_, _, c, _, _) -> f accu c
          | CKdef _                 -> accu
        in
        gg accu mt

      | Mset (_an, _l, k, v) ->
        let accu = f accu k in
        let accu = f accu v in
        gg accu mt

      | Mupdate (_an, k, l) ->
        let accu = f accu k in
        let accu = List.fold_right (fun (_, _, v) accu -> f accu v) l accu in
        gg accu mt

      | Maddupdate (_an, c, k, l) ->
        let accu =
          match c with
          | CKcoll _                -> accu
          | CKview c                -> f accu c
          | CKfield (_, _, c, _, _) -> f accu c
          | CKdef _                 -> accu
        in
        let accu = f accu k in
        let accu = List.fold_right (fun (_, _, v) accu -> f accu v) l accu in
        gg accu mt

      | Maddforce (_an, v) ->
        let accu = f accu v in
        gg accu mt

      | _ -> map_mterm (for_instruction g f env accu) mt

    in

    let g (env : (ident * mterm) list) (accu : (ident * mterm) list) (mt : mterm) : mterm =
      match accu with
      | [] -> mt
      | _ ->
        begin
          let build_contains (an, k) : mterm =
            let contains = mk_mterm (Mcontains(an, CKcoll (Tnone, Dnone), k)) tbool in
            let not_contains = mk_mterm (Mnot contains) tbool in
            let str_fail : mterm = mk_mterm (Mstring "get failed") tstring in
            let fail = mk_mterm (Mfail (Invalid str_fail)) tunit in
            let mif : mterm = mk_mterm (Mif(not_contains, fail, None)) tunit in
            mif
          in
          let cmp (an1, k1 : ident * mterm) (an2, k2 : ident * mterm) : bool =
            cmp_ident an1 an2 && cmp_mterm k1 k2
          in
          let ll = List.filter (fun x -> not (List.exists (cmp x) env)) accu in
          let l = List.map build_contains ll @ [mt] in
          mk_mterm (Mseq l) mt.type_
        end
    in
    let rec f (accu : (ident * mterm) list) (mt : mterm) : (ident * mterm) list =
      match mt.node with
      | Mget(an, _, k) ->
        let accu = f accu k in
        (an, k)::accu
      | _ -> fold_term f accu mt
    in
    for_instruction g f [] [] mt
  in

  let for_function (f : function__) =
    let for_function_node (fn : function_node) =
      let for_function_struct (fs : function_struct) =
        {fs with body = for_mterm fs.body }
      in
      match fn with
      | Entry     fs       -> Entry    (for_function_struct fs)
      | Getter   (fs, ret) -> Getter   (for_function_struct fs, ret)
      | Function (fs, ret) -> Function (for_function_struct fs, ret)
    in
    { f with node = for_function_node f.node; }
  in
  { model with functions = List.map for_function model.functions }

let split_key_values (model : model) : model =

  let asset_assets an = an in

  let get_asset_assoc_key_value (asset_name : ident) (asset_value : mterm) : mterm * mterm=
    match asset_value.node with
    | Masset l ->
      begin
        let asset : asset = Model.Utils.get_asset model asset_name in
        let asset_keys = asset.keys in

        let assoc_fields = List.map2 (fun (ai : asset_item) (x : mterm) -> (unloc ai.name, x)) asset.values l in

        List.find (fun (id, _) -> List.exists (String.equal id) asset_keys) assoc_fields |> snd,
        { asset_value with
          node = Masset (List.map (fun (x : mterm) ->
              match x with
              | { type_ = (Tcontainer ((Tasset an, _), c), _); _} -> { x with type_ = let k = Utils.get_asset_key model (unloc an) |> snd in (mktype (Tcontainer (k, c))) }
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
          let asset = Utils.get_asset model (unloc an) in
          let _k, t = Utils.get_asset_key model (unloc an) in
          let type_asset = tbmap asset.big_map t (tasset an) in
          let default =
            match x.default.node with
            | Massets l -> mk_mterm (Mlitmap (asset.big_map, List.map (fun x -> get_asset_assoc_key_value (unloc an) x) l)) type_asset
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
    match mt.node, mt.type_ with
    | Mcast ((Tcontainer ((Tasset _, _), _), _), (Tcontainer ((Tasset _, _), _), _), v), _ -> aux ctx v
    | Massets l, (Tcontainer ((Tasset an, _), _), _) ->
      let l = List.map (aux ctx) l in
      mk_mterm (Mlitset l) (tset ((Utils.get_asset_key model (unloc an) |> snd)))
    | _ -> map_mterm (aux ctx) mt
  in

  { model with
    storage = storage
  } |> map_mterm_model aux

let replace_for_to_iter (model : model) : model =

  let extract_asset (col : iter_container_kind) =
    match col with
    | ICKcoll an -> an
    | ICKview {type_ = (Tcontainer ((Tasset an, _), _), _)} -> unloc an
    | ICKfield (_, _, {type_ = (Tcontainer ((Tasset an, _), _), _)}) -> unloc an
    | _ -> assert false
  in

  let rec aux ctx (mt : mterm) : mterm =
    let process ids col t body lbl =
      let nbody = aux ctx body in
      let idx_id = "_i_" ^ lbl in
      let idx = mk_mterm (Mvar (dumloc idx_id, Vlocal, Tnone, Dnone)) tint in
      let nth = mk_mterm (Mlistnth(t, col, idx)) t in
      let letin = mk_mterm (Mletin (ids, nth, Some t, nbody, None)) tunit in
      let bound_min = mk_mterm (Mint Big_int.zero_big_int) tint in
      let bound_max = mk_mterm (Mlistlength (t, col)) tint in
      let iter = Miter (dumloc idx_id, bound_min, bound_max, letin, Some lbl) in
      mk_mterm iter mt.type_
    in
    match mt.node with
    | Mfor (FIsimple id, ICKset ({node = _; type_ = (Tset t, _)} as col), body, Some lbl) ->
      let col = mk_mterm (Mcast(tset t, tlist t, col)) (tlist t) in
      process [id] col t body lbl

    | Mfor (FIsimple id, ICKlist ({node = _; type_ = (Tlist t, _)} as col), body, Some lbl) ->
      process [id] col t body lbl

    | Mfor (FIdouble (kid, vid), ICKmap ({node = _; type_ = (Tmap (b, kt, vt), _)} as col), body, Some lbl) ->
      let t = ttuple [kt; vt] in
      let col = mk_mterm (Mcast(tbmap b kt vt, (tlist t), col)) (tlist t) in
      process [kid; vid] col t body lbl

    | Mfor (FIsimple id, col, body, Some lbl) ->
      let nbody = aux ctx body in
      let an = extract_asset col in
      let type_asset = tasset (dumloc an) in
      let ck =
        begin match col with
          | ICKcoll _ -> CKcoll (Tnone, Dnone)
          | ICKview x -> CKview x
          | ICKfield (an, fn, x) -> CKfield (an, fn, x, Tnone, Dnone)
          | _ -> assert false
        end in
      let idx_id = "_i_" ^ lbl in
      let idx = mk_mterm (Mvar (dumloc idx_id, Vlocal, Tnone, Dnone)) tint in
      let nth = mk_mterm (Mnth(an, ck, idx)) type_asset in
      let letin = mk_mterm (Mletin ([id], nth, Some type_asset, nbody, None)) tunit in
      let bound_min = mk_mterm (Mint Big_int.zero_big_int) tint in
      let bound_max = mk_mterm (Mcount (an, ck)) tint in
      let iter = Miter (dumloc idx_id, bound_min, bound_max, letin, Some lbl) in
      mk_mterm iter mt.type_
    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model

let remove_duplicate_key (model : model) : model =
  let remove_key_value_for_asset_node (mt : mterm) : mterm =
    match mt.node, get_ntype mt.type_ with
    | Masset l, Tasset an ->
      begin
        let an = unloc an in
        let k, _ = Utils.get_asset_key model an in
        let l =
          Utils.get_labeled_value_from model an l
          |> List.filter (fun (lbl, _) -> not (String.equal lbl k))
        in
        mk_mterm (Mlitrecord l) (tasset (dumloc (an ^ "_storage")))
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
              let type_asset = tset t in
              let default =
                match x.default.node with
                | Mlitmap (_, l) ->  mk_mterm (Mlitset (List.map fst l)) type_asset
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
              match x.default.node, get_ntype x.typ with
              | Mlitmap (_, l), Tmap (b, kt, (Tasset an, _)) ->
                let t = tbmap b kt (tasset (dumloc ((unloc an) ^ "_storage"))) in
                let mt = mk_mterm (Mlitmap (b, List.map (fun (k, v) -> (k, remove_key_value_for_asset_node v)) l)) t in
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
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Massign (op, t, Avar id, v) ->
      let lhs = mk_mterm (Mvar (id, Vlocal, Tnone, Dnone)) v.type_ in
      let v = process_assign_op op t lhs v in
      mk_mterm (Massign (ValueAssign, t, Avar id, v)) mt.type_
    | Massign (op, t, Avarstore id, v) ->
      let lhs = mk_mterm (Mvar (id, Vstorevar, Tnone, Dnone)) v.type_ in
      let v = process_assign_op op t lhs v in
      mk_mterm (Massign (ValueAssign, t, Avarstore id, v)) mt.type_
    | Massign (op, t, Aasset (an, fn, k), v) ->
      let lhs = mk_mterm (Mdotassetfield (an, k, fn)) v.type_ in
      let v = process_assign_op op t lhs v in
      mk_mterm (Massign (ValueAssign, t, Aasset (an, fn, k), v)) mt.type_
    | Massign (op, t, Arecord (rn, fn, r), v) ->
      let v = process_assign_op op t r v in
      mk_mterm (Massign (ValueAssign, t, Arecord (rn, fn, r), v)) mt.type_
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model


let extract_item_collection_from_add_asset (model : model) : model =
  let extract_item_collection_from_add_asset (an : ident) (l : mterm list) =
    let asset = Utils.get_asset model an in
    List.fold_right2
      (fun (ai : asset_item) (mt : mterm) (add_fields, items) ->
         match get_ntype ai.type_, mt.node with
         | Tcontainer ((Tasset ann, _), Partition), Massets l when not (List.is_empty l) ->
           begin
             let mas = mk_mterm (Massets []) ai.type_ in
             let assets = [unloc ai.name, unloc ann, l] in
             (mas::add_fields, assets @ items)
           end
         | Tcontainer ((Tasset ann, _), Aggregate), Mlitlist l when not (List.is_empty l) ->
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
      | Maddasset (an, { node = (Masset l); type_ = (Tasset _, _); _}) ->
        begin
          let dan = dumloc an in
          let add_fields, labeled_assets = extract_item_collection_from_add_asset an l in
          if List.is_empty labeled_assets
          then mt
          else
            begin
              let k = List.nth l (Utils.get_key_pos model an) in (* FIXME *)
              let add = mk_mterm (Maddasset (an, mk_mterm (Masset add_fields) (tasset dan))) tunit in
              let instrs : mterm list =
                labeled_assets
                |> List.map
                  (fun (fn, _ann, assetss) ->
                     assetss
                     |> List.map (fun asset ->
                         (mk_mterm (Maddfield (an, fn, k, asset)) tunit))
                  )
                |> List.flatten
              in
              mk_mterm (Mseq (add::instrs)) tunit
            end
        end
      | _ -> map_mterm (aux ctx) mt
    end
  in
  map_mterm_model aux model


let check_if_asset_in_function (model : model) : model =
  let rec for_type (t : type_) : type_ =
    match get_ntype t with
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
      | Entry     fs     -> for_function_struct fs
      | Getter   (fs, _) -> for_function_struct fs
      | Function (fs, _) -> for_function_struct fs
    in
    for_function_node f.node
  in
  List.iter for_function model.functions;
  model

let replace_instr_verif (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mremoveasset (an, k) ->
      begin
        let cond : mterm = mk_mterm (Mcontains (an, CKcoll (Tnone, Dnone), k)) tbool in
        let get = build_get an k in
        let i : mterm = mk_mterm (Mremoveasset (an, get)) tunit in
        let mif : mterm = mk_mterm (Mif (cond, i, None)) tunit in
        mif
      end
    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model

let rename_shadow_variable (model : model) : model =
  let for_function__ (f__ : function__) : function__ =
    let fun_id =  match f__.node with | Entry fs | Getter (fs, _) | Function (fs, _) -> unloc fs.name in
    let for_specification spec : specification =
      let map_ids = ref MapString.empty in
      let rename_variables spec : specification =
        { spec with
          variables = List.map (fun x ->
              { x with
                decl =
                  let id, t, dv = x.decl in
                  let id = unloc id in
                  let newid = id ^ "_" ^ fun_id in
                  map_ids := MapString.add id newid !map_ids;
                  dumloc newid, t, dv
              }) spec.variables
        }
      in
      let for_mterm _ctx mt : mterm =
        let rec aux (mt : mterm) : mterm =
          match mt.node with
          | Massign (op, t, Avar id, b) when MapString.mem (unloc id) !map_ids -> begin
              let newb = aux b in
              let newid : ident = MapString.find (unloc id) !map_ids in
              { mt with node = Massign (op, t, Avarstore (dumloc newid), newb) }
            end
          | Mvar (id, Vlocal, t, d) when MapString.mem(unloc id) !map_ids -> begin
              let newid : ident = MapString.find (unloc id) !map_ids in
              { mt with node = Mvar (dumloc newid, Vstorevar, t, d) }
            end
          | _ -> map_mterm aux mt
        in
        aux mt
      in
      spec
      |> rename_variables
      |> map_specification (mk_ctx_model ()) for_mterm
    in
    { f__ with
      spec = Option.map for_specification f__.spec;
    }
  in
  { model with
    functions = List.map for_function__ model.functions;
  }

let concat_shadown_effect_to_exec (model : model) : model =
  let set_storage_ident = ref SetString.empty in
  List.iter (fun (si : storage_item) ->
      match si.model_type with
      | MTvar -> set_storage_ident := SetString.add (unloc si.id) !set_storage_ident
      | _ -> ()
    ) model.storage;
  let for_mterm _ctx (x : mterm) : mterm =
    let rec aux (mt : mterm) : mterm =
      match mt.node with
      | Mvar (id, _, t, d) when SetString.mem (unloc id) !set_storage_ident -> { mt with node = Mvar (id, Vstorevar, t, d)}
      | Massign (op, t, Avar id, v) when SetString.mem (unloc id) !set_storage_ident  -> begin
          let nv = aux v in
          { mt with node = (Massign (op, t, Avarstore id, nv)) }
        end
      | _ -> map_mterm aux mt
    in
    aux x
  in
  let for_function__ (f__ : function__) : function__ =
    let shadow_effect_opt = Option.map (fun s -> s.effects) f__.spec in
    let shadow_effects =
      match shadow_effect_opt with
      | Some l -> List.map (for_mterm ()) l
      | _ -> []
    in
    let for_function_node (fn : function_node) : function_node =
      let for_function_struct (fs : function_struct) : function_struct =
        {
          fs with
          body = (mk_mterm (Mseq (fs.body::shadow_effects)) tunit) |> flat_sequence_mterm;
        }
      in
      match fn with
      | Function (fs, t) -> Function (for_function_struct fs, t)
      | Getter   (fs, t) -> Getter   (for_function_struct fs, t)
      | Entry     fs     -> Entry    (for_function_struct fs)
    in
    let remove_shadow_effect (spec : specification) : specification =
      spec
      |> map_specification (mk_ctx_model ()) for_mterm
      |> (fun spec -> {spec with effects = [] })
    in
    { f__ with
      node = for_function_node f__.node;
      spec = Option.map remove_shadow_effect f__.spec}
  in
  { model with
    functions = List.map for_function__ model.functions;
  }

let transfer_shadow_variable_to_storage (model : model) : model =
  let model = rename_shadow_variable model in
  let storage_items : storage_item list =
    model.functions
    |> List.map (fun x -> x.spec)
    |> (fun x -> Some(model.specification)::x)
    |> List.map (function | None -> [] | Some x -> x.variables)
    |> List.flatten
    |> List.map (fun v ->
        let id, t, dv = v.decl in
        mk_storage_item id MTvar t (Option.get dv) ~ghost:true) in
  {
    model with
    storage = model.storage @ storage_items
  }
  |> concat_shadown_effect_to_exec

let create_var_before_for (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mfor (id, ((ICKview c | ICKfield (_, _, c) | ICKlist c) as k), body, Some lbl) ->
      begin
        let var_id = "col_" ^ lbl ^ "_" in
        let var : mterm = mk_mterm (Mvar (dumloc var_id, Vlocal, Tnone, Dnone)) c.type_ in
        let ickvar : iter_container_kind =
          match k with
          | ICKview _  -> ICKview var
          | ICKfield (an, fn, _) -> ICKfield (an, fn, var)
          | ICKlist _  -> ICKlist var
          | _ -> assert false
        in
        let newfor : mterm  = mk_mterm (Mfor (id, ickvar, body, Some lbl)) mt.type_ in
        mk_mterm (Mletin ([dumloc var_id], c, Some c.type_, newfor, None)) mt.type_
      end
    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model


let replace_col_by_key_for_ckfield (model : model) =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    (* asset api effect *)
    (* | Mclear (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}))             -> { mt with node = Mclear (an, CKfield (fan, ffn, kdat)) } *)
    (* | Maddupdate (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}), k, l)   -> { mt with node = Maddupdate (an, CKfield (fan, ffn, kdat), k, l) } *)
    (* asset api expression *)
    | Mget (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}, t, d), k)            -> { mt with node = Mget (an, CKfield (fan, ffn, kdat, t, d), k) }
    | Mselect (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}, t, d), la, lb, a) -> { mt with node = Mselect (an, CKfield (fan, ffn, kdat, t, d), la, lb, a) }
    | Msort (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}, t, d), l)           -> { mt with node = Msort (an, CKfield (fan, ffn, kdat, t, d), l) }
    | Mcontains (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}, t, d), i)       -> { mt with node = Mcontains (an, CKfield (fan, ffn, kdat, t, d), i) }
    | Mnth (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}, t, d), i)            -> { mt with node = Mnth (an, CKfield (fan, ffn, kdat, t, d), i) }
    | Mcount (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}, t, d))             -> { mt with node = Mcount (an, CKfield (fan, ffn, kdat, t, d)) }
    | Msum (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}, t, d), p)            -> { mt with node = Msum (an, CKfield (fan, ffn, kdat, t, d), p) }
    | Mhead (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}, t, d), i)           -> { mt with node = Mhead (an, CKfield (fan, ffn, kdat, t, d), i) }
    | Mtail (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}, t, d), i)           -> { mt with node = Mtail (an, CKfield (fan, ffn, kdat, t, d), i) }
    (* default *)
    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model

let optimize (model : model) =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    (* asset api effect *)
    | Mnot ({node = Mnot x; _}) -> aux ctx x
    (* default *)
    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model

(* let filter_api_storage (model : model) =
   let filter (l : api_storage list) =
    let cmp c1 c2 =
      match c1.node_item, c2.node_item with
      | APIAsset (Clear (an1, c1))           , APIAsset (Clear (an2, c2))             -> cmp_ident an1 an2 && cmp_container_kind c1 c2
      | APIAsset (Update (an1, l1))          , APIAsset (Update (an2, l2))            -> cmp_ident an1 an2 && List.for_all2 (fun (i1, op1, v1) (i2, op2, v2) -> cmp_ident i1 i2 && cmp_assign_op op1 op2 && cmp_mterm v1 v2) l1 l2
      | APIAsset (FieldAdd (an1, fn1))       , APIAsset (FieldAdd (an2, fn2))         -> cmp_ident an1 an2 && cmp_ident fn1 fn2
      | APIAsset (FieldRemove (an1, fn1))    , APIAsset (FieldRemove (an2, fn2))      -> cmp_ident an1 an2 && cmp_ident fn1 fn2
      | APIAsset (RemoveAll (an1, fn1))      , APIAsset (RemoveAll (an2, fn2))        -> cmp_ident an1 an2 && cmp_ident fn1 fn2
      | APIAsset (RemoveIf (an1, c1, l1, p1)), APIAsset (RemoveIf (an2, c2, l2, p2))  -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && List.for_all2 (fun (i1, t1) (i2, t2) -> cmp_ident i1 i2 && cmp_type t1 t2) l1 l2 && cmp_mterm p1 p2
      | APIAsset (Contains (an1, c1))        , APIAsset (Contains (an2, c2))          -> cmp_ident an1 an2 && cmp_container_kind c1 c2
      | APIAsset (Nth (an1, c1))             , APIAsset (Nth (an2, c2))               -> cmp_ident an1 an2 && cmp_container_kind c1 c2
      | APIAsset (Select (an1, c1, l1, p1))  , APIAsset (Select (an2, c2, l2, p2))    -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && List.for_all2 (fun (i1, t1) (i2, t2) -> cmp_ident i1 i2 && cmp_type t1 t2) l1 l2 && cmp_mterm p1 p2
      | APIAsset (Sort (an1, c1, l1))        , APIAsset (Sort (an2, c2, l2))          -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && List.for_all2 (fun (fn1, k1) (fn2, k2) -> cmp_ident fn1 fn2 && k1 = k2) l1 l2
      | APIAsset (Count (an1, c1))           , APIAsset (Count (an2, c2))             -> cmp_ident an1 an2 && cmp_container_kind c1 c2
      | APIAsset (Sum (an1, c1, t1, p1))     , APIAsset (Sum (an2, c2, t2, p2))       -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp_type t1 t2 && cmp_mterm p1 p2
      | APIAsset (Head (an1, c1))            , APIAsset (Head (an2, c2))              -> cmp_ident an1 an2 && cmp_container_kind c1 c2
      | APIAsset (Tail (an1, c1))            , APIAsset (Tail (an2, c2))              -> cmp_ident an1 an2 && cmp_container_kind c1 c2
      (* | APIAsset (Sum (an1, Coll , t1, p1)), APIAsset (Sum (an2, View, t2, p2)) -> cmp_ident an1 an2 && cmp_type t1 t2 && cmp_mterm p1 p2
      | APIAsset (Sum (an1, View , t1, p1)), APIAsset (Sum (an2, Coll, t2, p2)) -> cmp_ident an1 an2 && cmp_type t1 t2 && cmp_mterm p1 p2 *)
      | _, _ -> cmp_api_item_node c1.node_item c2.node_item
    in

    List.fold_right (fun (x : api_storage) accu ->
        if List.exists (cmp x) accu
        then accu
        else x::accu) l []
   in

   { model with
    api_items = filter model.api_items
   } *)


let filter_api_storage (model : model) =
  let filter (l : api_storage list) =
    let l = List.map (
        fun (x : api_storage) ->
          let node =
            match x.node_item with
            | APIAsset (Clear (an, _))            -> APIAsset (Clear (an, View))
            | APIAsset (Contains (an, _))         -> APIAsset (Contains (an, View))
            | APIAsset (Nth (an, _))              -> APIAsset (Nth (an, View))
            | APIAsset (Select (an, _, l, p))     -> APIAsset (Select (an, View, l, p))
            | APIAsset (Sort (an, _, l))          -> APIAsset (Sort (an, View, l))
            | APIAsset (Count (an, _))            -> APIAsset (Count (an, View))
            | APIAsset (Sum (an, _, t, p))        -> APIAsset (Sum (an, View, t, p))
            | APIAsset (Head (an, _))             -> APIAsset (Head (an, View))
            | APIAsset (Tail (an, _))             -> APIAsset (Tail (an, View))
            | z -> z
          in
          {x with node_item = node}
      ) l in

    let cmp (a : api_storage) (b : api_storage) =
      match a.node_item, b.node_item with
      | APIAsset (Nth _), APIAsset (Nth _) -> cmp_api_storage a b
      | _ -> cmp_api_item_node a.node_item b.node_item
    in

    List.fold_right (fun (x : api_storage) accu ->
        if List.exists (cmp x) accu
        then accu
        else Utils.add_api_storage_in_list accu x) l []
  in

  { model with
    api_items = filter model.api_items |> Utils.sort_api_storage model true
  }

let process_multi_keys (model : model) : model =
  let fold (model : model) (asset_name : ident) : model =
    match (Utils.get_asset model asset_name).keys with
    | [] | [_] -> model
    | _ -> begin
        let r : (ident * type_ * (ident * int) list) ref = ref ("", tunit, []) in
        let build_asset keys_index new_key_type (l : mterm list) : mterm list =
          let l0, l1 =
            List.fold_lefti (fun i (l0, l1) x ->
                if List.exists (fun (_, x) -> x = i) keys_index
                then (l0 @ [x], l1)
                else (l0, l1 @ [x])) ([], []) l
          in
          (mk_mterm (Mtuple l0) new_key_type)::l1
        in
        let for_decl_node (d : decl_node) : decl_node =
          let for_asset (a : asset) : asset =
            r := ("", tunit, []);
            let keys = a.keys in
            let new_key = List.fold_left (fun str x -> match str with | "" -> x | _ -> str ^ "_" ^ x) "" keys in
            let keys_fields = List.map (fun x -> Utils.get_asset_field model (asset_name, x)) keys in
            let new_key_type = ttuple (List.map (fun (_, x, _) -> x) keys_fields) in
            let new_key_field = mk_asset_item (dumloc new_key) new_key_type new_key_type in
            let new_values = List.fold_right (fun (f : asset_item) accu -> if List.exists (String.equal (unloc f.name)) keys then accu else f::accu ) a.values [] in
            let keys_index = List.fold_lefti (fun i accu (x : asset_item) ->
                let id = unloc x.name in
                if List.exists (String.equal id) keys
                then accu @ [id, i]
                else accu) [] a.values
            in
            let for_init (mt : mterm) =
              match mt.node with
              | Masset l -> {mt with node = Masset (build_asset keys_index new_key_type l)}
              | _ -> mt
            in
            r := new_key, new_key_type, keys_index;
            {
              a with
              values = new_key_field::new_values;
              keys = [new_key];
              init = List.map for_init a.init;
            }
          in
          match d with
          | Dasset a when String.equal (unloc a.name) asset_name -> Dasset (for_asset a)
          | _ -> d
        in
        let rec aux ctx (mt : mterm) : mterm =
          let new_key, new_key_type, keys_index = !r in
          let check (an, fn) = String.equal (unloc an) asset_name && List.exists (fun (id, _) -> String.equal (unloc fn) id) keys_index in
          let process fn node : mterm =
            let fn = unloc fn in
            let idx = List.assoc fn keys_index in
            let x : mterm = mk_mterm node new_key_type in
            let node = Mtupleaccess (x, Big_int.big_int_of_int idx) in
            mk_mterm node mt.type_
          in
          match mt with
          | {node = Masset l; type_ = (Tasset an, _)} when String.equal (unloc an) asset_name ->
            {mt with node = Masset (build_asset keys_index new_key_type l)}
          | { node = Mdotassetfield (an, k, fn)} when check (an, fn) -> begin
              let k = aux ctx k in
              let node = Mdotassetfield (an, k, dumloc new_key) in
              process fn node
            end
          | {node = Mdot (({type_ = (Tasset an, _)} as a), fn)} when check (an, fn) -> begin
              let a = aux ctx a in
              let node = Mdot (a, dumloc new_key) in
              process fn node
            end
          | _ -> map_mterm (aux ctx) mt
        in
        { model with
          decls = List.map for_decl_node model.decls;
        }
        |> map_mterm_model aux
      end
  in
  model.decls
  |> (fun decls -> List.fold_right (fun d accu -> match d with | Dasset d -> d::accu | _ -> accu) decls [])
  |> List.map (fun (asset : asset) -> unloc asset.name)
  |> List.fold_left fold model

let eval_storage (model : model) : model =
  let map : mterm MapString.t =
    List.fold_left (fun (accu : mterm MapString.t) decl ->
        match decl with
        | Dvar v when Option.is_some v.default -> MapString.add (unloc v.name) (Option.get v.default) accu
        | _ -> accu
      ) MapString.empty model.decls
  in

  let for_storage_item (map : mterm MapString.t) (si : storage_item) : storage_item =
    let for_mterm (mt : mterm) : mterm =
      let rec aux (mt : mterm) : mterm =
        match mt.node with
        | Mvar (id, _, _, _) when MapString.mem (unloc id) map -> MapString.find (unloc id) map
        | _ -> map_mterm aux mt
      in
      aux mt
    in
    { si with
      default = for_mterm si.default
    }
  in
  { model with
    storage = List.map (for_storage_item map) model.storage;
  }

let remove_storage_field_in_function (model : model) : model =

  let extract_storage_var map (fs : function_struct) : mterm * (argument list * mterm list MapString.t) =
    let fs_name = unloc fs.name in

    let rec aux (accu, map) (mt : mterm) : mterm * (argument list * mterm list MapString.t) =
      let g (x : mterm__node) : mterm = { mt with node = x; } in
      match mt.node with
      | Mvar (id, (Vstorecol | Vstorevar), d, t) -> begin
          let a =
            if List.exists (fun (c, _, _) -> String.equal (unloc id) (unloc c) ) accu
            then accu, map
            else
              let args = if MapString.mem fs_name map then MapString.find fs_name map else [] in
              ((id, mt.type_, None)::accu |> List.dedup, (MapString.add fs_name (mt::args) map))
          in
          g (Mvar (id, Vparam, d, t)), a
        end
      | Mapp (id, app_args) when MapString.mem (unloc id) map -> begin

          let ((app_args, (accu, map)) : 'c list * 'a) =
            List.fold_left
              (fun (pterms, accu) x ->
                 let p, accu = aux accu x in
                 pterms @ [p], accu) ([], (accu, map)) app_args
          in

          let aaa = MapString.find (unloc id) map in
          let args = if MapString.mem fs_name map then MapString.find fs_name map else [] in
          let a = (List.map (fun (mt : mterm) ->
              ((match mt.node with Mvar(id, _, _, _) -> id | _ -> assert false), mt.type_, None) ) aaa) in
          let dargs = List.dedupcmp (fun (a : argument) (b : argument) -> cmp_lident (proj3_1 a) (proj3_1 b)) (a @ accu) in
          let dvars = List.dedupcmp cmp_mterm (aaa @ args) in
          { mt with node = Mapp (id, app_args) }, (dargs, (MapString.add fs_name dvars map))
        end
      | _ -> fold_map_term g aux (accu, map) mt
    in
    aux ([], map) fs.body
  in

  let rec apply_args map (mt : mterm) : mterm =
    match mt.node with
    | Mapp (id, args) when MapString.mem (unloc id) map -> begin
        let args = List.map (apply_args map) args in
        let nargs = MapString.find (unloc id) map in
        { mt with node = Mapp (id, nargs @ args) }
      end
    | _ -> map_mterm (apply_args map) mt
  in

  let for_function__ map (f__ : function__) : function__ * mterm list MapString.t =
    let for_function_node map (fn : function_node) : function_node * mterm list MapString.t =
      let for_function_struct map (fs : function_struct) : function_struct * mterm list MapString.t =
        let nbody, (nargs, nmap) = extract_storage_var map fs in
        match nargs with
        | [] -> fs, map
        | _ ->
          { fs with
            args = nargs @ fs.args;
            body = nbody;
          }, nmap
      in
      match fn with
      | Function (fs, t) -> let nfs, nmap = for_function_struct map fs in Function (nfs, t), nmap
      | _ -> fn, map
    in
    let nnode, nmap = for_function_node map f__.node in
    { f__ with
      node = nnode;
    }, nmap
  in
  let funs, map = List.fold_left (fun (fs, map) f -> let n, nmap = for_function__ map f in (fs @ [n], nmap)) ([], MapString.empty) model.functions in
  map_model (fun _ -> id) id (apply_args map) { model with functions = funs; }

let remove_asset (model : model) : model =

  let for_type an =
    let asset = Utils.get_asset model an in
    let fields = List.fold_left (fun fields (x : asset_item) ->
        if List.exists (String.equal (unloc x.name)) asset.keys
        then fields
        else (
          let type_ =
            match get_ntype x.type_ with
            | Tcontainer ((Tasset an, _), _) -> tset (Utils.get_asset_key model (unloc an) |> snd)
            | Tcontainer (b, _) -> tset (b)
            | _ -> x.type_
          in
          fields @ [unloc x.name, type_]
        )) [] asset.values
    in
    match fields with
    | [] -> assert false
    | [_, t] -> t, fields, true
    | _ -> (trecord (dumloc an)), fields, false
  in

  let for_asset_type an =
    let _, kt = Utils.get_asset_key model an in
    if Utils.is_asset_single_field model an
    then kt
    else ttuple [kt; proj3_1 (for_type an)]
  in

  let process_storage (model : model) : model * ((bool * bool) * type_) MapString.t =
    let for_storage_item map (x : storage_item) =
      let map_storage_mterm (mt : mterm) : mterm =
        match mt with
        | { node = Mlitmap (b, l) } -> begin
            let rec aux (mt : mterm) : mterm =
              match mt with
              | { node = Massets _l; type_ = (Tcontainer ((Tasset an, _), (Partition | Aggregate)), _)} ->
                let kt = Utils.get_asset_key model (unloc an) |> snd in
                (* let extract_key (l : mterm list) : mterm =
                   List.nth l (Utils.get_key_pos model (unloc an))
                   in
                   let extract_asset (mt : mterm) : mterm list =
                   match mt.node with
                   | Masset l -> l
                   | _ -> assert false
                   in
                   let ll = List.map extract_asset l in
                   let lll = List.map extract_key ll in *)
                mk_mterm (Mlitset []) (tset kt)
              | { node = Massets l; type_ = (Tcontainer (kt, (Partition | Aggregate)), _)} ->
                mk_mterm (Mlitset l) (tset kt)
              | _ -> map_mterm aux mt
            in
            let l = List.map (fun (x, y) -> (x, aux y)) l in
            { mt with node = Mlitmap (b, l) }
          end
        | { node = Massets _l; type_ = (Tcontainer (kt, (Partition | Aggregate)), _)} -> begin
            mk_mterm (Mlitset []) (tset kt)
          end
        | _ -> mt
      in
      match x.model_type, get_ntype x.typ with
      | MTasset an, Tmap (b, k, (Tasset _, _)) -> begin
          let ts, fields, is_single_record = for_type an in
          let type_ = tbmap b k ts in
          let default = map_storage_mterm x.default in
          let d = match fields with | [] | [_] -> [] | _ -> [Drecord (mk_record (dumloc an) ~fields:(List.map (fun (id, t) -> mk_record_field (dumloc id) t) fields) )] in
          { x with typ = type_; default = default; }, d, MapString.add an ((true, is_single_record), type_) map
        end
      | MTasset an, (Tset st) -> x, [], MapString.add an ((false, false), tset st) map
      | _ -> x, [], map
    in
    let nstorage, decls, map = List.fold_left (fun (accu, decls, map) x -> let a, ds, map = for_storage_item map x in (accu @ [a], decls @ ds, map)) ([], [], MapString.empty) model.storage in
    { model with
      storage = nstorage;
      decls = model.decls @ decls;
    }, map
  in


  let process_mterm (map : ((bool * bool) * type_) MapString.t) (model : model) : model =
    let is_single_simple_record an =
      MapString.find an map
      |> fst
    in

    let is_simple_record an =
      is_single_simple_record an |> snd
    in

    let get_type_for_asset_container an =
      MapString.find an map
      |> snd
    in

    let is_key an fn =
      let kn, _ = Utils.get_asset_key model an in
      String.equal kn fn
    in

    let get_asset_global_id an = dumloc (an) in

    let get_asset_global an =
      let type_ = get_type_for_asset_container an in
      let id = get_asset_global_id an in
      mk_mterm (Mvar (id, Vstorecol, Tnone, Dnone)) type_
    in

    let get_contains_va (va : mterm) (k : mterm) : mterm =
      let node =
        match get_ntype va.type_ with
        | Tset kt          -> Msetcontains (kt, va, k)
        | Tmap (_, kt, kv) -> Mmapcontains (kt, kv, va, k)
        | _ -> assert false
      in
      mk_mterm node tbool
    in

    let msg_KeyAlreadyExists              = "KeyAlreadyExists" in
    let msg_KeyNotFound                   = "KeyNotFound" in
    let msg_KeyNotFoundOrKeyAlreadyExists = "KeyNotFoundOrKeyAlreadyExists" in

    (* let get_asset_key_type x = Utils.get_asset_key model x |> snd in *)

    let extract_key x = Utils.extract_key_value_from_masset model x in

    let extract_key_value (v : mterm) : mterm * mterm * (ident * mterm) list * (ident * mterm * mterm) list =
      match v with
      | {node = (Masset l); type_ = (Tasset an, _) } ->
        let an = unloc an in
        let asset : asset = Utils.get_asset model an in
        let asset_key = match asset.keys with [k] -> k | _ -> assert false in
        let assoc_fields : (ident * type_ * mterm) list = List.map2 (fun (ai : asset_item) (x : mterm) -> (unloc ai.name, ai.type_, x)) asset.values l in
        let k, l, ags, pts = List.fold_left (fun (sk, sv, ags, pts) (x : ident * type_ * mterm) ->
            let id, t, v = x in
            if String.equal asset_key id
            then Some v, sv, ags, pts
            else begin
              let to_litset an l =
                let _, kt = Utils.get_asset_key model (unloc an) in
                let set_t = tset kt in
                List.fold_left (fun accu x -> mk_mterm (Msetadd(kt, accu, x)) set_t) (mk_mterm (Mlitset []) set_t) l
              in
              let v, nags, npts =
                match v.node, get_ntype t with
                | Mlitlist l, Tcontainer ((Tasset an, _), Aggregate) -> begin
                    to_litset an l, List.map (fun x -> unloc an, x) l, []
                  end
                | Mlitset l, Tcontainer ((Tasset an, _), Partition) -> begin
                    to_litset an (List.map extract_key l), [], List.map (fun x -> unloc an, extract_key x, x) l
                  end
                | _ -> v, [], []
              in
              (sk, sv @ [id, v], ags @ nags, pts @ npts)
            end
          ) (None, [], [], []) assoc_fields in
        let k =
          match k with
          | Some k -> k
          | None -> assert false
        in
        let v =
          match l with
          | [] -> mk_mterm (Munit) tunit
          | [v] -> snd v
          | _ -> mk_mterm (Mlitrecord l) (trecord (dumloc an))
        in
        k, v, ags, pts
      | _ -> raise Not_found
    in

    let get_list_assets_partition pts =
      List.fold_left (fun accu (an, _, v) ->
          let rec f l =
            match l with
            | (a, b)::t when String.equal a an -> (an, v::b)::t
            | x::t -> x::(f t)
            | [] -> [an, [v]]
          in
          f accu) [] pts
    in

    let get_partitions an : (ident * ident) list =
      let asset : asset = Utils.get_asset model an in
      List.fold_left (fun accu (x : asset_item) ->
          match get_ntype x.original_type with
          | Tcontainer ((Tasset an, _), Partition) -> (unloc x.name, unloc an)::accu
          | _ -> accu
        ) [] asset.values
      |> List.rev
    in

    let get_instrs_add_with_partition pts =
      let ll = get_list_assets_partition pts in
      let linstrs = List.map (fun (an, l) ->
          let va = get_asset_global an in
          let a = List.fold_left (fun accu x -> begin
                let k, v, _, _ = extract_key_value x in
                match get_ntype va.type_ with
                | Tset kt          ->
                  mk_mterm (Msetadd (kt, accu, k)) va.type_
                | Tmap (_, kt, vt) ->
                  mk_mterm (Mmapput (kt, vt, accu, k, v)) va.type_
                | _ -> assert false
              end
            ) va l in
          mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), a)) tunit
        ) ll
      in linstrs
    in

    let create_contains_asset_key an x =
      let va = get_asset_global an in
      let f x =
        match get_ntype va.type_ with
        | Tset tk          -> Msetcontains (tk, va, x)
        | Tmap (_, tk, tv) -> Mmapcontains (tk, tv, va, x)
        | _ -> Format.eprintf "%a@." pp_type_ va.type_; assert false
      in
      mk_mterm (f x) tbool
    in

    let add_asset ?(force=false) f an v =
      begin
        let v = f v in
        (* let is_single, is_record = is_single_simple_record an in *)
        let va = get_asset_global an in
        let k, v, ags, pts = extract_key_value v in

        let cond = get_contains_va va k in

        let assign =
          match get_ntype va.type_ with
          | Tset kt          ->  begin
              let a = mk_mterm (Msetadd (kt, va, k)) va.type_ in
              mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), a)) tunit
            end
          | Tmap (_, kt, kv) -> begin
              let a = mk_mterm (Mmapput (kt, kv, va, k, v)) va.type_ in
              let b = mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), a)) tunit in
              match ags, pts with
              | [], []   -> b
              | (an, a)::t, [] -> begin
                  let f = create_contains_asset_key in
                  let c = List.fold_left (fun accu (an, x) -> mk_mterm (Mand (f an x, accu)) tbool) (f an a) t in
                  mk_mterm (Mif (c, b, Some (fail msg_KeyNotFound))) tunit
                end
              | [], (an, k, _)::t -> begin
                  let f = create_contains_asset_key in
                  let c = List.fold_left (fun accu (an, x, _) -> mk_mterm (Mor (f an x, accu)) tbool) (f an k) t in
                  let linstrs = get_instrs_add_with_partition pts in
                  let seq = mk_mterm (Mseq (b::linstrs)) tunit in
                  mk_mterm (Mif (c, fail msg_KeyAlreadyExists, Some seq)) tunit
                end
              | (aan, aa)::at, pt ->
                let f = create_contains_asset_key in
                let c = List.fold_left (fun accu (an, x) -> mk_mterm (Mand (f an x, accu)) tbool) (f aan aa) at in
                let c = List.fold_left (fun accu (an, x, _) -> mk_mterm (Mand (mnot (f an x), accu)) tbool) c pt in
                let linstrs = get_instrs_add_with_partition pts in
                let seq = mk_mterm (Mseq (b::linstrs)) tunit in
                mk_mterm (Mif (c, seq, Some (fail msg_KeyNotFoundOrKeyAlreadyExists))) tunit
            end
          | _ -> assert false
        in

        if force
        then assign
        else mk_mterm (Mif (cond, fail msg_KeyAlreadyExists, Some assign)) tunit
      end
    in

    let remove_asset f an k =
      let k = f k in
      let va = get_asset_global an in

      let new_value =
        let node =
          match get_ntype va.type_ with
          | Tset kt          -> Msetremove (kt, va, k)
          | Tmap (_, kt, kv) -> Mmapremove (kt, kv, va, k)
          | _ -> assert false
        in
        mk_mterm node va.type_
      in

      let assign = mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), new_value)) tunit in

      let partitions : (ident * ident) list = get_partitions an in

      match partitions with
      | [] -> assign
      | _ -> begin
          let l : mterm list = List.map (fun (fn, aan) ->
              let viter_name = "_viter" in
              let viter_id = dumloc viter_name in
              let pk = Utils.get_asset_key model aan |> snd in
              let viter    : mterm = mk_mterm (Mvar (viter_id, Vstorecol, Tnone, Dnone)) pk in
              let set      : mterm = mk_mterm (Mdot((mk_mterm (Mget(an, CKcoll(Tnone, Dnone), k)) (tasset (dumloc an))), dumloc fn)) pk |> f in
              let passign2 : mterm = mk_mterm (Mremoveasset (aan, viter)) tunit |> f in
              mk_mterm (Mfor (FIsimple viter_id, ICKset set, passign2, None)) tunit
            ) partitions in
          mk_mterm (Mseq (l @ [assign])) tunit
        end
    in

    let remove_field f (an, fn, ak, b) =
      let ak = f ak in
      let bk = f b in

      let va = get_asset_global an in
      let kt, vt =
        match get_ntype va.type_ with
        | Tmap (_, kt, vt) -> kt, vt
        | _ -> assert false
      in

      let _, is_record = is_single_simple_record an in
      let aan, c = Utils.get_field_container model an fn in
      let atk = Utils.get_asset_key model aan |> snd in

      let mk_assign bk =
        let ts = tset atk in
        let remove_set set = mk_mterm (Msetremove (atk, set, bk)) ts in
        let get_ t = mk_mterm (Mmapget(kt, vt, va, ak)) t in
        let v : mterm =
          if is_record
          then begin
            remove_set (get_ ts)
          end
          else begin
            let tr = trecord (dumloc an) in
            let get : mterm = get_ tr in
            let set : mterm = mk_mterm (Mdot(get, dumloc fn)) (tset atk) in
            mk_mterm (Mrecupdate(get, [fn, remove_set set])) tr
          end
        in
        let nmap : mterm = mk_mterm (Mmapput (kt, vt, va, ak, v) ) va.type_ in
        mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), nmap)) tunit
      in

      match c with
      | Aggregate -> mk_assign bk
      | Partition ->
        let assign = mk_assign bk in
        mk_mterm (Mseq [remove_asset f aan b; assign]) tunit
      | _ -> assert false
    in

    let fold_ck ?(with_value=true) f (an, ck : ident * container_kind) (init : mterm) mk =
      let tr = init.type_ in
      let atk = Utils.get_asset_key model an |> snd in

      match ck with
      | CKcoll _ -> begin
          let va = get_asset_global an in
          match get_ntype va.type_ with
          | Tset skt -> begin

              let iid = dumloc "_sid" in
              let vid = mk_mterm (Mvar (iid, Vlocal, Tnone, Dnone)) skt in

              let iaccu = dumloc "_accu" in
              let vaccu = mk_mterm (Mvar (iaccu, Vlocal, Tnone, Dnone)) tr in

              let act = mk vid None vaccu in
              mk_mterm (Msetfold(atk, iid, iaccu, va, init, act)) tr
            end
          | Tmap (_, mkt, mkv) -> begin

              let ikid = dumloc "_kid" in
              let vkid = mk_mterm (Mvar (ikid, Vlocal, Tnone, Dnone)) mkt in

              let ivid = dumloc "_vid" in
              let vvid = mk_mterm (Mvar (ivid, Vlocal, Tnone, Dnone)) mkv in

              let iaccu = dumloc "_accu" in
              let vaccu = mk_mterm (Mvar (iaccu, Vlocal, Tnone, Dnone)) tr in

              let act = mk vkid (Some vvid) vaccu in
              mk_mterm (Mmapfold(atk, ikid, ivid, iaccu, va, init, act)) tr
            end
          | _ -> assert false
        end
      | CKfield (an, fn, k, _, _) -> begin
          let va = get_asset_global an in
          let get =
            match get_ntype va.type_ with
            | Tmap (_, kt, vt) -> mk_mterm (Mmapget (kt, vt, va, f k)) vt
            | _ -> assert false
          in

          let aan, _ = Utils.get_field_container model an fn in
          let _ , aatk = Utils.get_asset_key model aan in

          let set =
            let _, is_record = is_single_simple_record an in
            if is_record
            then get
            else mk_mterm (Mdot(get, dumloc fn)) (tset aatk)
          in

          let iid = dumloc "_sid" in
          let vid = mk_mterm (Mvar (iid, Vlocal, Tnone, Dnone)) aatk in

          let iaccu = dumloc "_accu" in
          let vaccu = mk_mterm (Mvar (iaccu, Vlocal, Tnone, Dnone)) tr in

          let vaa = get_asset_global aan in
          let geta =
            match get_ntype vaa.type_ with
            | Tmap (_, kt, vt) -> Some (mk_mterm (Mmapget (kt, vt, vaa, vid)) vt)
            | _ -> None
          in

          let body =
            match geta with
            | Some g -> begin
                let ival = dumloc "_v" in
                let vval = mk_mterm (Mvar(ival, Vlocal, Tnone, Dnone)) g.type_ in

                if with_value
                then let act : mterm = mk vid (Some vval) vaccu in mk_mterm (Mletin([ival], g, Some g.type_, act, None)) tr
                else mk vid None vaccu
              end
            | None -> begin
                let act = mk vid None vaccu in
                act
              end
          in

          mk_mterm (Msetfold(atk, iid, iaccu, set, init, body)) tr

        end
      | CKview c -> begin
          let l = f c in

          let iid = dumloc "_sid" in
          let vid = mk_mterm (Mvar (iid, Vlocal, Tnone, Dnone)) atk in

          let iaccu = dumloc "_accu" in
          let vaccu = mk_mterm (Mvar (iaccu, Vlocal, Tnone, Dnone)) tr in

          let vaa = get_asset_global an in
          let geta =
            match get_ntype vaa.type_ with
            | Tmap (_, kt, vt) -> Some (mk_mterm (Mmapget (kt, vt, vaa, vid)) vt)
            | _ -> None
          in

          let body =
            match geta with
            | Some g -> begin
                let ival = dumloc "_v" in
                let vval = mk_mterm (Mvar(ival, Vlocal, Tnone, Dnone)) g.type_ in

                if with_value
                then let act = mk vid (Some vval) vaccu in mk_mterm (Mletin([ival], g, Some g.type_, act, None)) tr
                else mk vid None vaccu
              end
            | None -> begin
                let act = mk vid None vaccu in
                act
              end
          in

          mk_mterm (Mlistfold(atk, iid, iaccu, l, init, body)) tr
        end
      | _ -> assert false
    in

    let rec fm ctx (mt : mterm) : mterm =
      match mt.node with

      (* access *)

      | Mdot (({node = _; type_ = (Tasset an, _)} as a), _) when Utils.is_asset_single_field model (unloc an) -> fm ctx a
      | Mdot ({node = Mget (_, CKcoll _, k); type_ = (Tasset an, _)}, fn) when is_key (unloc an) (unloc fn) -> begin
          fm ctx k
        end
      | Mdot (({node = _; type_ = (Tasset an, _)} as a), fn) -> begin
          let an = unloc an in
          let mt_get = fm ctx a in
          if is_simple_record an
          then mt_get
          else
            mk_mterm (Mdot(mt_get, fn)) mt.type_
        end
      | Mget (an, CKcoll _, k) when Utils.is_asset_single_field model an -> fm ctx k

      | Mget (an, CKcoll _, k) -> begin
          let k = fm ctx k in
          let va = get_asset_global an in
          let kt, vt =
            match get_ntype va.type_ with
            | Tmap (_, kt, vt) -> kt, vt
            | _ -> assert false
          in
          let map_get = Mmapget (kt, vt, va, k) in
          mk_mterm map_get vt
        end

      | Mget (an, CKview c, k) -> begin
          let get = mk_mterm (Mget(an, CKcoll(Tnone, Dnone), k)) mt.type_ |> fm ctx in
          let cond = mk_mterm (Mcontains(an, CKview c, k)) tbool |> fm ctx in
          mk_mterm (Mexprif(cond, get, fail "NotFound")) get.type_
        end

      (* control *)

      | Mfor (FIsimple id, ICKcoll an, b, lbl) -> begin
          let b = fm ctx b in
          let va = get_asset_global an in
          let node =
            match get_ntype va.type_ with
            | Tset _ -> Mfor (FIsimple id, ICKset va, b, lbl)
            | Tmap _ -> Mfor (FIdouble (id, dumloc "_v"), ICKmap va, b, lbl)
            | _ -> assert false
          in
          { mt with node = node }
        end

      | Mfor (FIsimple id, ICKfield (_, _, c), b, lbl) -> begin
          let b = fm ctx b in
          let c = fm ctx c in
          let node = Mfor (FIsimple id, ICKset c, b, lbl) in
          { mt with node = node }
        end

      | Mfor (FIsimple id, ICKview v, b, lbl) -> begin
          let b = fm ctx b in
          let v = fm ctx v in
          let node = Mfor (FIsimple id, ICKlist v, b, lbl) in
          { mt with node = node }
        end

      (* effect *)

      | Maddasset (an, v) -> add_asset (fm ctx) an v

      | Maddfield (an, fn, ak, b) -> begin
          let ak = fm ctx ak in

          let va = get_asset_global an in
          let kt, vt =
            match get_ntype va.type_ with
            | Tmap (_, kt, vt) -> kt, vt
            | _ -> assert false
          in

          let _, is_record = is_single_simple_record an in
          let aan, c = Utils.get_field_container model an fn in
          let atk = Utils.get_asset_key model aan |> snd in

          let mk_assign bk =
            let ts = tset atk in
            let add_set set = mk_mterm (Msetadd (atk, set, bk)) ts in
            let get_ t = mk_mterm (Mmapget(kt, vt, va, ak)) t in
            let v : mterm =
              if is_record
              then begin
                add_set (get_ ts)
              end
              else begin
                let tr = trecord (dumloc an) in
                let get : mterm = get_ tr in
                let set : mterm = mk_mterm (Mdot(get, dumloc fn)) (tset atk) in
                mk_mterm (Mrecupdate(get, [fn, add_set set])) tr
              end
            in
            let nmap : mterm = mk_mterm (Mmapput (kt, vt, va, ak, v) ) va.type_ in
            mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), nmap)) tunit
          in

          match c with
          | Aggregate ->
            let bk = fm ctx b in
            let assign = mk_assign bk in
            let cond = create_contains_asset_key aan bk in
            mk_mterm (Mif (cond, assign, Some (fail msg_KeyNotFound))) tunit
          | Partition ->
            let bk = extract_key b in
            let bk = fm ctx bk in
            let assign = mk_assign bk in
            mk_mterm (Mseq [add_asset (fm ctx) aan b; assign]) tunit
          | _ -> assert false
        end

      | Mremoveasset (an, k) -> remove_asset (fm ctx) an k

      | Mremovefield (an, fn, ak, b) -> remove_field (fm ctx) (an, fn, ak, b)

      | Mremoveall (an, fn, k) -> begin
          let kk = fm ctx k in

          let va = get_asset_global an in
          let kt, vt =
            match get_ntype va.type_ with
            | Tmap (_, kt, vt) -> kt, vt
            | _ -> assert false
          in

          let _, is_record = is_single_simple_record an in
          let aan, c = Utils.get_field_container model an fn in
          let atk = Utils.get_asset_key model aan |> snd in


          let get_ t = mk_mterm (Mmapget(kt, vt, va, kk)) t in

          let mk_loop _ =
            let iter_var = dumloc "_iter_var" in
            let ivar = mk_mterm (Mvar(iter_var, Vlocal, Tnone, Dnone)) atk in

            let body = remove_asset (fm ctx) aan ivar in

            let set =
              let ts = tset atk in
              if is_record
              then get_ ts
              else begin
                let tr = trecord (dumloc an) in
                let get : mterm = get_ tr in
                mk_mterm (Mdot(get, dumloc fn)) ts
              end
            in

            mk_mterm (Mfor (FIsimple iter_var, ICKset set, body, None)) tunit
          in

          let mk_assign _ =
            let ts = tset atk in
            let empty = mk_mterm (Mlitset []) ts in
            let v : mterm =
              if is_record
              then empty
              else begin
                let tr = trecord (dumloc an) in
                let get : mterm = get_ tr in
                mk_mterm (Mrecupdate(get, [fn, empty])) tr
              end
            in
            let nmap : mterm = mk_mterm (Mmapput (kt, vt, va, kk, v) ) va.type_ in
            mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), nmap)) tunit
          in

          let assign = mk_assign () in

          match c with
          | Aggregate -> assign
          | Partition -> mk_mterm (Mseq [mk_loop (); assign]) tunit
          | _ -> assert false
        end

      | Mremoveif (an, ck, _, b, _) -> begin

          let get_val an v fn t : mterm =
            let _, is_record = is_single_simple_record an in
            if is_record
            then v
            else mk_mterm (Mdot(v, fn)) t
          in

          let mk_cond an vkey vval x =
            let akn, _akt = Utils.get_asset_key model an in
            let rec aux (mt : mterm) : mterm =
              match mt.node with
              | Mdot ({node = Mvar ({pldesc = "the"}, _, _, _); _}, fn) when String.equal (unloc fn) akn -> vkey
              | Mdot ({node = Mvar ({pldesc = "the"}, _, _, _); _}, fn) -> get_val an (Option.get vval) fn mt.type_
              | _ -> map_mterm aux mt
            in
            aux x
          in

          match ck with
          | CKcoll _ -> begin
              let va = get_asset_global an in
              match get_ntype va.type_ with
              | Tset kt -> begin
                  let ikey = dumloc "_k" in
                  let vkey = mk_mterm (Mvar(ikey, Vlocal, Tnone, Dnone)) kt in

                  let cond = fm ctx (mk_cond an vkey None b) in
                  let remove = remove_asset (fm ctx) an vkey in
                  let body = mk_mterm (Mif (cond, remove, None)) tunit in
                  let loop = mk_mterm (Mfor(FIsimple ikey, ICKset va, body, None) ) tunit in
                  loop
                end
              | Tmap (_, kt, vt) -> begin
                  let ikey = dumloc "_k" in
                  let vkey = mk_mterm (Mvar(ikey, Vlocal, Tnone, Dnone)) kt in

                  let ival = dumloc "_v" in
                  let vval = mk_mterm (Mvar(ival, Vlocal, Tnone, Dnone)) vt in

                  let cond = fm ctx (mk_cond an vkey (Some vval) b) in
                  let remove = remove_asset (fm ctx) an vkey in
                  let body = mk_mterm (Mif (cond, remove, None)) tunit in
                  let loop = mk_mterm (Mfor(FIdouble (ikey, ival), ICKmap va, body, None) ) tunit in
                  loop
                end
              | _ -> assert false
            end

          | CKfield (an, fn, k, _, _) -> begin
              let va = get_asset_global an in
              let get =
                match get_ntype va.type_ with
                | Tmap (_, kt, vt) -> mk_mterm (Mmapget (kt, vt, va, fm ctx k)) vt
                | _ -> assert false
              in

              let aan, _ = Utils.get_field_container model an fn in
              let _ , aatk = Utils.get_asset_key model aan in

              let set =
                let _, is_record = is_single_simple_record an in
                if is_record
                then get
                else mk_mterm (Mdot(get, dumloc fn)) (tset aatk)
              in

              let ikey = dumloc "_k" in
              let vkey = mk_mterm (Mvar(ikey, Vlocal, Tnone, Dnone)) aatk in


              let vaa = get_asset_global aan in
              let geta =
                match get_ntype vaa.type_ with
                | Tmap (_, kt, vt) -> Some (mk_mterm (Mmapget (kt, vt, vaa, vkey)) vt)
                | _ -> None
              in

              let body =
                match geta with
                | Some g -> begin
                    let ival = dumloc "_v" in
                    let vval = mk_mterm (Mvar(ival, Vlocal, Tnone, Dnone)) g.type_ in

                    let cond = fm ctx (mk_cond aan vkey (Some vval) b) in
                    let remove = remove_field (fm ctx) (an, fn, k, vkey) in
                    let body = mk_mterm (Mif (cond, remove, None)) tunit in
                    mk_mterm (Mletin([ival], g, Some g.type_, body, None)) tunit
                  end
                | None -> begin
                    let cond = fm ctx (mk_cond aan vkey None b) in
                    let remove = remove_field (fm ctx) (an, fn, k, vkey) in
                    mk_mterm (Mif (cond, remove, None)) tunit
                  end
              in

              let loop = mk_mterm (Mfor(FIsimple ikey, ICKset set, body, None) ) tunit in
              loop
            end
          | _ -> assert false
        end

      | Mclear (an, ck) -> begin
          match ck with
          | CKcoll _ -> begin
              let va = get_asset_global an in
              let _, is_record = is_single_simple_record an in
              let empty =
                let node =
                  match get_ntype va.type_ with
                  | Tset _ -> Mlitset []
                  | Tmap (b, _, _) -> Mlitmap (b, [])
                  | _ -> assert false
                in
                mk_mterm node va.type_
              in

              let mk_loop fn aan =
                let tv =
                  match get_ntype va.type_ with
                  | Tmap (_, _, tv) -> tv
                  | _ -> assert false
                in
                let var_id = dumloc "_v" in
                let var_value : mterm = mk_mterm (Mvar (var_id, Vlocal, Tnone, Dnone)) tv in
                let atk = Utils.get_asset_key model aan |> snd in
                let set =
                  if is_record
                  then var_value
                  else mk_mterm (Mdot (var_value, dumloc fn)) (tset atk)
                in
                let loop : mterm =
                  let var_id = dumloc "_ak" in
                  let var_value = mk_mvar var_id atk in
                  let b : mterm = remove_asset (fm ctx) aan var_value in
                  mk_mterm (Mfor (FIsimple var_id, ICKset set, b, None)) tunit
                in
                mk_mterm (Mfor (FIdouble (dumloc "_k", var_id), ICKmap va, loop, None)) tunit
              in

              let assign = mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), empty)) tunit in
              let partitions = get_partitions an in
              match partitions with
              | [] -> assign
              | _ -> mk_mterm (Mseq ((List.map (fun (fn, aan) -> mk_loop fn aan) partitions) @ [assign])) tunit
            end
          | CKfield (an, fn, k, _, _) -> begin
              let kk = fm ctx k in

              let va = get_asset_global an in
              let _, is_record = is_single_simple_record an in

              let aan, _ = Utils.get_field_container model an fn in
              let atk = Utils.get_asset_key model aan |> snd in

              let tk, tv =
                match get_ntype va.type_ with
                | Tmap (_, tk, tv) -> tk, tv
                | _ -> assert false
              in

              let set =
                let get = mk_mterm (Mmapget (tk, tv, va, kk)) tv in
                if is_record
                then get
                else mk_mterm (Mdot(get, dumloc fn)) (tset atk)
              in

              let var_id = dumloc "_ak" in
              let var_value = mk_mterm (Mvar (var_id, Vlocal, Tnone, Dnone)) atk in
              let b : mterm = remove_asset (fm ctx) aan var_value in
              let loop = mk_mterm (Mfor (FIsimple var_id, ICKset set, b, None)) tunit in
              let assign = fm ctx (mk_mterm (Mremoveall (an, fn, k)) tunit) in
              mk_mterm (Mseq [loop; assign]) tunit
            end
          | CKview l -> begin
              let tk = Utils.get_asset_key model an |> snd in

              let l = fm ctx l in

              let var_id = dumloc "_ak" in
              let var_value = mk_mterm (Mvar (var_id, Vlocal, Tnone, Dnone)) tk in

              let b : mterm = remove_asset (fm ctx) an var_value in

              mk_mterm (Mfor (FIsimple var_id, ICKlist l, b, None)) tunit
            end
          | CKdef _ -> assert false
        end

      | Mupdate (an, k, l) -> begin
          let k = fm ctx k in

          let va = get_asset_global an in

          let _, is_record = is_single_simple_record an in

          let kt, tasset =
            match get_ntype (get_type_for_asset_container an) with
            | Tmap (_, kt, vt) -> kt, vt
            | _ -> assert false
          in

          let var_id = dumloc "_asset" in

          let mk_letin x =
            let get = mk_mterm (Mmapget(kt, tasset, va, k)) tasset in
            mk_mterm (Mletin ([var_id], get, Some tasset, x, None)) tunit
          in

          let get_val id =
            let var = mk_mterm (Mvar(var_id, Vlocal, Tnone, Dnone)) tasset in
            if is_record
            then var
            else begin
              let _, ts, _ = Utils.get_asset_field model (an, unloc id) in
              mk_mterm (Mdot(var, id)) ts
            end
          in

          let add_val id x = mk_mterm (Mplus   (get_val id, x)) x.type_ in

          let sub_val id (x : mterm) =
            let _, t, _ = Utils.get_asset_field model (an, unloc id) in
            match get_ntype t with
            |  Tbuiltin Bnat -> begin
                let a = mk_mterm (Mminus (get_val id, x)) tint in
                let zero = mk_mterm (Mint Big_int.zero_big_int) tint in
                let cond = mk_mterm (Mge (a, zero)) tbool in
                let v = mk_mterm (Mabs a) tnat in
                let f = mk_mterm (Mfail AssignNat) (tunit) in
                let c = mk_mterm (Mcast (tunit, tnat, f)) tnat in
                mk_mterm (Mexprif (cond, v, c)) tnat
              end
            | _ ->  mk_mterm (Mminus  (get_val id, x)) x.type_
          in

          let mul_val id x = mk_mterm (Mmult   (get_val id, x)) x.type_ in
          let div_val id x = mk_mterm (Mdiveuc (get_val id, x)) x.type_ in
          let and_val id x = mk_mterm (Mand    (get_val id, x)) x.type_ in
          let or_val  id x = mk_mterm (Mor     (get_val id, x)) x.type_ in

          let extract_listlit (v : mterm) : mterm list =
            match v.node with
            | Mlitlist l -> l
            | _ -> assert false
          in

          let extract_litset (v : mterm) : mterm list =
            match v.node with
            | Mlitset l -> l
            | _ -> Format.eprintf "%a@." pp_mterm v; assert false
          in

          let l, b, ags = List.fold_right (fun (id, op, v) (accu, b, ags) ->
              let _, ts, _ = Utils.get_asset_field model (an, unloc id) in
              let v = fm ctx v in
              match get_ntype ts, op with
              | Tcontainer((Tasset aan, _), Aggregate), ValueAssign -> begin
                  let aan = unloc aan in
                  let _, tk = Utils.get_asset_key model aan in
                  let ll = List.map (fm ctx) (extract_listlit v) in
                  let set = mk_mterm (Mlitset ll) (tset tk) in
                  (id, ValueAssign, set)::accu, b, (unloc id, aan, `Aggregate, `Replace, ll)::ags
                end

              | Tcontainer((Tasset aan, _), Aggregate), PlusAssign -> begin
                  let aan = unloc aan in
                  let _, tk = Utils.get_asset_key model aan in
                  let ts = tset tk in
                  let ll = List.map (fm ctx) (extract_listlit v) in
                  let get : mterm = get_val id in
                  let set = List.fold_left (fun accu (x : mterm) -> mk_mterm (Msetadd (tk, accu, x)) ts) get ll in
                  (id, ValueAssign, set)::accu, true, (unloc id, aan, `Aggregate, `Add, ll)::ags
                end

              | Tcontainer((Tasset aan, _), Aggregate), MinusAssign  ->  begin
                  let aan = unloc aan in
                  let _, tk = Utils.get_asset_key model aan in
                  let ts = tset tk in
                  let ll = List.map (fm ctx) (extract_listlit v) in
                  let get : mterm = get_val id in
                  let set = List.fold_left (fun accu (x : mterm) -> mk_mterm (Msetremove (tk, accu, x)) ts) get ll in
                  (id, ValueAssign, set)::accu, true, (unloc id, aan, `Aggregate, `Remove, ll)::ags
                end

              | Tcontainer((Tasset aan, _), Partition), ValueAssign -> begin
                  let aan = unloc aan in
                  let _, tk = Utils.get_asset_key model aan in
                  let ll =  extract_litset v in
                  let lll = List.map (fm ctx) ll in
                  let lkeys = List.map (extract_key |@ (fm ctx)) ll in
                  let set = mk_mterm (Mlitset lkeys) (tset tk) in
                  (id, ValueAssign, set)::accu, b, (unloc id, aan, `Partition, `Replace, lll)::ags
                end

              | Tcontainer((Tasset aan, _), Partition), PlusAssign -> begin
                  let aan = unloc aan in
                  let _, tk = Utils.get_asset_key model aan in
                  let ts = tset tk in
                  let ll =  extract_litset v in
                  let lll = List.map (fm ctx) ll in
                  let lkeys = List.map (extract_key |@ (fm ctx)) ll in
                  let get : mterm = get_val id in
                  let set = List.fold_left (fun accu (x : mterm) -> mk_mterm (Msetadd (tk, accu, x)) ts) get lkeys in
                  (id, ValueAssign, set)::accu, true, (unloc id, aan, `Partition, `Add, lll)::ags
                end

              | Tcontainer((Tasset aan, _), Partition), MinusAssign  ->  begin
                  let aan = unloc aan in
                  let _, tk = Utils.get_asset_key model aan in
                  let ts = tset tk in
                  let ll = List.map (fm ctx) (extract_listlit v) in
                  let get : mterm = get_val id in
                  let set = List.fold_left (fun accu (x : mterm) -> mk_mterm (Msetremove (tk, accu, x)) ts) get ll in
                  (id, ValueAssign, set)::accu, true, (unloc id, aan, `Partition, `Remove, ll)::ags
                end

              | _, ValueAssign  -> (id, op, v)::accu, b, ags
              | _, PlusAssign   -> (id, ValueAssign, add_val id v)::accu , true, ags
              | _, MinusAssign  -> (id, ValueAssign, sub_val id v)::accu , true, ags
              | _, MultAssign   -> (id, ValueAssign, mul_val id v)::accu , true, ags
              | _, DivAssign    -> (id, ValueAssign, div_val id v)::accu , true, ags
              | _, AndAssign    -> (id, ValueAssign, and_val id v)::accu , true, ags
              | _, OrAssign     -> (id, ValueAssign, or_val  id v)::accu , true, ags
            ) l ([], false, [])
          in

          let v : mterm =
            if is_record
            then begin
              match l with
              | [] -> assert false
              | [(_, op, v)] -> begin
                  match op with
                  | ValueAssign -> let v = fm ctx v in v
                  | _ -> assert false
                end
              | _ -> assert false
            end
            else begin
              let get : mterm = mk_mterm (Mmapget(kt, tasset, va, k)) tasset in
              let ll = List.map (fun (id, op, v) ->
                  match op with
                  | ValueAssign -> begin
                      let v = fm ctx v in
                      unloc id, v
                    end
                  | _ -> assert false
                ) l in
              mk_mterm (Mrecupdate(get, ll)) tasset
            end
          in

          let nmap = mk_mterm (Mmapput (kt, tasset, va, k, v)) va.type_ in

          let assign = mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), nmap)) tunit in

          let pts : (ident * mterm) list =
            let rec add_res accu (an, x) =
              match accu with
              | [] -> [(an, x)]
              | (bn, _)::t when String.equal an bn -> (an, x)::t
              | y::t -> y::(add_res t (an, x))
            in

            let process_partition aan accu l mk_value =
              let vaa = get_asset_global aan in

              let init =
                match List.assoc_opt aan accu with
                | Some v -> v
                | None -> vaa
              in

              let mmtt = List.fold_left (fun (accu : mterm) (x : mterm) -> mk_value vaa accu x) init l in
              add_res accu (aan, mmtt)
            in

            List.fold_left (fun accu x ->
                match x with
                | _, aan, `Partition, (`Add | `Replace), l -> begin

                    let mk_value (vaa : mterm) (c : mterm) (v : mterm) : mterm =
                      let node =
                        let a, b, _, _ = extract_key_value v in
                        match get_ntype vaa.type_ with
                        | Tset kt          -> Msetadd (kt, c, a)
                        | Tmap (_, kt, kv) -> begin
                            Mmapput (kt, kv, c, a, b)
                          end
                        | _ -> assert false
                      in
                      mk_mterm node vaa.type_
                    in

                    process_partition aan accu l mk_value
                  end
                | _, aan, `Partition, `Remove, l -> begin

                    let mk_value (vaa : mterm) (c : mterm) (k : mterm) : mterm =
                      let node =
                        match get_ntype vaa.type_ with
                        | Tset kt          -> Msetremove (kt, c, k)
                        | Tmap (_, kt, kv) -> Mmapremove (kt, kv, c, k)
                        | _ -> assert false
                      in
                      mk_mterm node vaa.type_
                    in

                    process_partition aan accu l mk_value
                  end
                | _ -> accu
              ) [] ags
          in

          let mk_assign_partition (an, v) : mterm =
            let va = get_asset_global an in
            mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), v)) tunit
          in

          let assign =
            match pts with
            | [] -> assign
            | _ -> mk_mterm (Mseq (assign::(List.map mk_assign_partition pts))) tunit
          in

          let assign = if b then mk_letin assign else assign in
          let elts_cond = List.fold_left (fun accu x ->
              match x with
              | _, aan, `Aggregate, (`Add | `Replace), l -> List.map (fun x -> true,  aan, x) l @ accu
              | _, aan, `Partition, (`Add | `Replace), l -> List.map (fun x -> false, aan, extract_key x) l @ accu
              | _ -> accu
            ) [] ags in

          let msg = List.fold_left (fun accu x ->
              match accu, x with
              | msg, ( _, _, `Aggregate, (`Add | `Replace), _) when String.equal msg msg_KeyAlreadyExists -> msg_KeyNotFoundOrKeyAlreadyExists
              | msg, ( _, _, `Partition, (`Add | `Replace), _) when String.equal msg msg_KeyNotFound      -> msg_KeyNotFoundOrKeyAlreadyExists
              | _, ( _, _, `Aggregate, (`Add | `Replace), _) -> msg_KeyNotFound
              | _, ( _, _, `Partition, (`Add | `Replace), _) -> msg_KeyAlreadyExists
              | _ -> accu
            ) "" ags in

          let assign : mterm =
            match elts_cond with
            | [] -> assign
            | (b, an, mt)::l -> begin
                let mk_cond b an mt = let x = create_contains_asset_key an mt in if b then x else mk_mterm (Mnot(x)) tbool in
                let init : mterm = mk_cond b an mt in
                let cond : mterm = List.fold_left (fun accu (b, an, x) -> mk_mterm (Mand(accu, mk_cond b an x)) tbool ) init l in
                mk_mterm (Mif (cond, assign, Some (fail msg))) tunit
              end
          in
          assign
        end

      | Maddforce (an, v) -> add_asset (fm ctx) an v ~force:true

      (* expression *)

      | Mselect (an, ck, _, b, _) -> begin
          let mk vkid vvid (vaccu : mterm) : mterm =

            let get_val an v fn t : mterm =
              let _, is_record = is_single_simple_record an in
              if is_record
              then v
              else mk_mterm (Mdot(v, fn)) t
            in

            let mk_cond an vkey vval x =
              let akn, _akt = Utils.get_asset_key model an in
              let rec aux (mt : mterm) : mterm =
                match mt.node with
                | Mdot ({node = Mvar ({pldesc = "the"}, _, _, _); _}, fn) when String.equal (unloc fn) akn -> vkey
                | Mdot ({node = Mvar ({pldesc = "the"}, _, _, _); _}, fn) -> get_val an (Option.get vval) fn mt.type_
                | _ -> map_mterm aux mt
              in
              aux x
            in

            let tr    = vaccu.type_ in

            let atk =
              match get_ntype tr with
              | Tlist atk -> atk
              | _ -> assert false
            in

            let cond  = mk_cond an vkid vvid b in
            let mthen = mk_mterm (Mlistprepend(atk, vaccu, vkid)) tr in
            let mif   = mk_mterm (Mif (cond, mthen, Some vaccu)) tr in
            mif
          in

          let atk, tr =
            let a =
              match ck with
              | CKfield (an, fn, _, _, _) -> Utils.get_field_container model an fn |> fst |> Utils.get_asset_key model |> snd
              | _ -> Utils.get_asset_key model an |> snd
            in
            a, tlist a
          in

          let empty = mk_mterm (Mlitlist []) tr in
          let r = fold_ck (fm ctx) (an, ck) empty mk in
          mk_mterm (Mlistreverse(atk, r)) (tlist atk)
        end

      | Msort (an, ck, crits) -> begin
          let atk, tr =
            let a =
              match ck with
              | CKfield (an, fn, _, _, _) -> Utils.get_field_container model an fn |> fst |> Utils.get_asset_key model |> snd
              | _ -> Utils.get_asset_key model an |> snd
            in
            a, tlist a
          in

          let sort (vkid : mterm) (vvid : mterm option) (vaccu : mterm) : mterm =

            let get_val k (v : mterm option) fn : mterm =
              let _, t, _ = Utils.get_asset_field model (an, fn) in
              let is_key = String.equal fn (Utils.get_asset_key model an |> fst) in

              let _, is_record = is_single_simple_record an in

              match is_key, is_record, v with
              | true, _, _      -> k
              | _, true, Some v -> v
              | _, _, Some v    -> mk_mterm (Mdot(v, dumloc fn)) t
              | _ -> assert false
            in

            let iinit_0 = mk_some vkid in
            let iinit_1 = mk_mterm (Mlitlist []) tr in
            let iinit   = mk_tuple [iinit_0; iinit_1] in

            let ixins = dumloc "_x_insert" in
            let vxins = mk_mvar ixins atk in

            let iains = dumloc "_accu_insert" in
            let vains = mk_mvar iains iinit.type_ in

            let prepend x l = mk_mterm (Mlistprepend(atk, l, x)) tr in

            let insert : mterm =

              let ia0 = dumloc "_ia0" in
              let va0 : mterm = mk_mvar ia0 iinit_0.type_ in

              let ia1 = dumloc "_ia1" in
              let va1 : mterm = mk_mvar ia1 iinit_1.type_ in

              let ivb = dumloc "_b" in

              let va = get_asset_global an in
              let add_letin x =
                match get_ntype va.type_ with
                | Tset _ -> x
                | Tmap (_, kt, vt) ->
                  let mk_get x = mk_mterm (Mmapget (kt, vt, va, x)) vt in
                  x |> mk_letin ivb (mk_get vxins)
                | _ -> assert false
              in

              let vvb : mterm option =
                match get_ntype va.type_ with
                | Tset _ -> None
                | Tmap (_, _, vt) -> Some (mk_mvar ivb vt)
                | _ -> assert false
              in

              let crit =
                let is_rat t =
                  match get_ntype t with
                  | Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)] -> true
                  | _ -> false
                in
                let zero = mk_int 0 in
                let one = mk_int 1 in
                List.fold_right (fun (id, sk) accu ->
                    let vl = get_val vxins vvb   id in
                    let vr = get_val vkid  vvid  id in
                    let gt =
                      let node =
                        if is_rat vl.type_
                        then Mratcmp (Gt, vl, vr)
                        else Mgt (vl, vr)
                      in
                      mk_mterm node tbool
                    in
                    let lt =
                      let node =
                        if is_rat vl.type_
                        then Mratcmp (Lt, vl, vr)
                        else Mlt (vl, vr)
                      in
                      mk_mterm node tbool in
                    let mk_if c t e = mk_mterm (Mexprif (c, t, e)) tint in
                    match sk with
                    | SKasc  -> mk_if gt one accu
                    | SKdesc -> mk_if lt one accu
                  ) crits zero |> add_letin
              in

              let cond = mk_mterm (Mgt (crit, mk_int 0)) tbool in

              let neutral = mk_tuple [va0; prepend vxins va1] in
              let act = mk_tuple [mk_none (vkid.type_); prepend vxins (prepend vkid va1)] in

              let mif = mk_mterm (Mif (cond, act, Some neutral)) neutral.type_ in

              let matchsome : mterm = mk_mterm (Mmatchoption (va0, dumloc "", mif, neutral)) vains.type_ in

              matchsome
              |> mk_letin ia1 (mk_tupleaccess 1 vains)
              |> mk_letin ia0 (mk_tupleaccess 0 vains)
            in

            let iz0 = dumloc "_iz0" in
            let vz0 : mterm = mk_mvar iz0 iinit_0.type_ in

            let iz1 = dumloc "_iz1" in
            let vz1 : mterm = mk_mvar iz1 iinit_1.type_ in

            let a = mk_mterm (Mlistfold(atk, ixins, iains, vaccu, iinit, insert)) iinit.type_ in

            let i0 = dumloc "_i0" in
            let v0 : mterm = mk_mvar i0 a.type_ in

            let matchsome : mterm = mk_mterm (Mmatchoption (vz0, dumloc "", prepend vkid vz1, vz1)) tr in

            matchsome
            |> (fun x -> mk_mterm (Mlistreverse(atk, x)) (tlist atk))
            |> mk_letin iz1 (mk_tupleaccess 1 v0)
            |> mk_letin iz0 (mk_tupleaccess 0 v0)
            |> mk_letin i0 a
          in

          let init = mk_mterm (Mlitlist []) tr in

          fold_ck (fm ctx) (an, ck) init sort ~with_value:true
        end

      | Mcontains (an, ck, k) -> begin
          let k = fm ctx k in
          let node =
            match ck with
            | CKcoll _ -> begin
                let va = get_asset_global an in
                match get_ntype va.type_ with
                | Tset tk          -> Msetcontains (tk, va, k)
                | Tmap (_, tk, tv) -> Mmapcontains (tk, tv, va, k)
                | _ -> assert false
              end
            | CKview v -> begin
                let tk = Utils.get_asset_key model an |> snd in
                let v = fm ctx v in
                Mlistcontains (tk, v, k)
              end
            | CKfield (an, fn, kk, _, _) -> begin
                let kk = fm ctx kk in

                let va = get_asset_global an in

                let _, is_record = is_single_simple_record an in
                let aan, _ = Utils.get_field_container model an fn in
                let atk = Utils.get_asset_key model aan |> snd in

                let tk, tv =
                  match get_ntype va.type_ with
                  | Tmap (_, tk, tv) -> tk, tv
                  | _ -> assert false
                in

                let set =
                  let get = mk_mterm (Mmapget (tk, tv, va, kk)) tv in
                  if is_record
                  then get
                  else mk_mterm (Mdot(get, dumloc fn)) (tset atk)
                in

                Msetcontains(atk, set, k)
              end
            | CKdef _ -> assert false
          in
          mk_mterm node tbool
        end

      | Mnth (an, ck, n) -> begin
          let n = fm ctx n in
          let mk vkid _vvid (vaccu : mterm) : mterm =
            let tr = vaccu.type_ in

            let vtn = mk_tupleaccess 0 vaccu in
            let vtr = mk_tupleaccess 1 vaccu in

            let inc = mk_mterm (Mplus(vtn, mk_nat 1)) tnat in

            let cond = mk_mterm (Mequal (tr, vtn, n)) tbool in
            let mthen = mk_tuple [inc; mk_some vkid] in
            let melse = mk_tuple [inc; vtr] in
            mk_mterm (Mif(cond, mthen, Some melse)) tr
          in

          let atk =
            match ck with
            | CKfield (an, fn, _, _, _) -> Utils.get_field_container model an fn |> fst |> Utils.get_asset_key model |> snd
            | _ -> Utils.get_asset_key model an |> snd
          in

          let init_0 = mk_nat 0 in
          let init_1 = mk_mterm (Mnone) (toption atk) in
          let init   = mk_tuple [init_0; init_1] in
          fold_ck (fm ctx) (an, ck) init mk ~with_value:false
          |> mk_tupleaccess 1
          |> mk_optget
        end

      | Mcount (an, ck) -> begin
          let node =
            match ck with
            | CKcoll _ -> begin
                let va = get_asset_global an in
                match get_ntype va.type_ with
                | Tset tk          -> Msetlength (tk, va)
                | Tmap (_, tk, tv) -> Mmaplength (tk, tv, va)
                | _ -> assert false
              end
            | CKview v -> begin
                let tk = Utils.get_asset_key model an |> snd in
                let v = fm ctx v in
                Mlistlength (tk, v)
              end
            | CKfield (an, fn, kk, _, _) -> begin
                let kk = fm ctx kk in

                let va = get_asset_global an in

                let _, is_record = is_single_simple_record an in
                let aan, _ = Utils.get_field_container model an fn in
                let atk = Utils.get_asset_key model aan |> snd in

                let tk, tv =
                  match get_ntype va.type_ with
                  | Tmap (_, tk, tv) -> tk, tv
                  | _ -> assert false
                in

                let set =
                  let get = mk_mterm (Mmapget (tk, tv, va, kk)) tv in
                  if is_record
                  then get
                  else mk_mterm (Mdot(get, dumloc fn)) (tset atk)
                in

                Msetlength(atk, set)
              end
            | CKdef _ -> assert false
          in
          mk_mterm node tnat
        end

      | Msum (an, ck, p) -> begin
          let mk vkid vvid (vaccu : mterm) : mterm =

            let get_val an v fn t : mterm =
              let _, is_record = is_single_simple_record an in
              if is_record
              then v
              else mk_mterm (Mdot(v, fn)) t
            in

            let mk_val an vkey vval x =
              let akn, _akt = Utils.get_asset_key model an in
              let rec aux (mt : mterm) : mterm =
                match mt.node with
                | Mdot ({node = Mvar ({pldesc = "the"}, _, _, _); _}, fn) when String.equal (unloc fn) akn -> vkey
                | Mdot ({node = Mvar ({pldesc = "the"}, _, _, _); _}, fn) -> get_val an (Option.get vval) fn mt.type_
                | _ -> map_mterm aux mt
              in
              aux x
            in

            let tr = vaccu.type_ in

            let v  = mk_val an vkid vvid p in
            let node =
              match get_ntype tr with
              | Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)] -> Mratarith(Rplus, vaccu, v)
              | _ -> Mplus(vaccu, v)
            in
            mk_mterm node tr
          in

          let init =
            match get_ntype mt.type_ with
            | Tbuiltin Bnat -> mk_mterm (Mnat Big_int.zero_big_int) tnat
            | Tbuiltin Bint -> mk_mterm (Mint Big_int.zero_big_int) tint
            | Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)] -> Utils.mk_rat Big_int.zero_big_int Big_int.unit_big_int
            | _ -> assert false
          in
          fold_ck (fm ctx) (an, ck) init mk
        end

      | Mhead (an, ck, n) -> begin
          let n = fm ctx n in

          let atk, tr =
            let a =
              match ck with
              | CKfield (an, fn, _, _, _) -> Utils.get_field_container model an fn |> fst |> Utils.get_asset_key model |> snd
              | _ -> Utils.get_asset_key model an |> snd
            in
            a, tlist a
          in

          let mk vkid _vvid (vaccu : mterm) : mterm =

            let vtn = mk_tupleaccess 0 vaccu in
            let vtr = mk_tupleaccess 1 vaccu in

            let inc = mk_mterm (Mplus(vtn, mk_nat 1)) tnat in

            let cond  = mk_mterm (Mlt(vtn, n)) tbool in
            let add   = mk_mterm (Mlistprepend(atk, vtr, vkid)) tr in
            let mthen = mk_tuple [inc; add] in
            let melse = mk_tuple [inc; vtr] in
            let mif   = mk_mterm (Mif (cond, mthen, Some melse)) vaccu.type_ in
            mif
          in

          let init_0 = mk_nat 0 in
          let init_1 = mk_mterm (Mlitlist []) tr in
          let init   = mk_tuple [init_0; init_1] in

          fold_ck (fm ctx) (an, ck) init mk ~with_value:false
          |> mk_tupleaccess 1
          |> (fun x -> mk_mterm (Mlistreverse(atk, x)) (tlist atk))
        end

      | Mtail (an, ck, n) -> begin
          let n = fm ctx n in

          let atk, tr =
            let a =
              match ck with
              | CKfield (an, fn, _, _, _) -> Utils.get_field_container model an fn |> fst |> Utils.get_asset_key model |> snd
              | _ -> Utils.get_asset_key model an |> snd
            in
            a, tlist a
          in

          let rev vkid _vvid (vaccu : mterm) : mterm =
            mk_mterm (Mlistprepend(atk, vaccu, vkid)) tr
          in

          let head n l =

            let init_0 = mk_nat 0 in
            let init_1 = mk_mterm (Mlitlist []) tr in
            let init   = mk_tuple [init_0; init_1] in

            let iid = dumloc "_hd" in
            let vid = mk_mterm (Mvar (iid, Vlocal, Tnone, Dnone)) atk in

            let iaccu = dumloc "_haccu" in
            let vaccu = mk_mterm (Mvar (iaccu, Vlocal, Tnone, Dnone)) init.type_ in

            let body =
              let vtn = mk_tupleaccess 0 vaccu in
              let vtr = mk_tupleaccess 1 vaccu in

              let inc = mk_mterm (Mplus(vtn, mk_nat 1)) tnat in

              let cond  = mk_mterm (Mlt(vtn, n)) tbool in
              let add   = mk_mterm (Mlistprepend(atk, vtr, vid)) tr in
              let mthen = mk_tuple [inc; add] in
              let melse = mk_tuple [inc; vtr] in
              let mif   = mk_mterm (Mif (cond, mthen, Some melse)) vaccu.type_ in
              mif
            in

            mk_mterm (Mlistfold(atk, iid, iaccu, l, init, body)) init.type_
          in

          let init = mk_mterm (Mlitlist []) tr in

          fold_ck (fm ctx) (an, ck) init rev ~with_value:false
          |> head n
          |> mk_tupleaccess 1
        end

      | _ -> map_mterm (fm ctx) mt
    in
    map_mterm_model fm model
  in

  let remove_type_asset (model : model) : model =
    let rec ft t : type_ =
      match get_ntype t with
      | Tcontainer ((Tasset an, _), View) -> tlist (Utils.get_asset_key model (unloc an) |> snd)
      | Tasset an -> for_asset_type (unloc an)
      | _ -> map_type ft t
    in
    map_model (fun _ -> id) ft id model
  in

  let model, map = process_storage model in
  process_mterm map model
  |> remove_type_asset

let remove_high_level_model (model : model)  =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    let f = aux ctx in
    match mt.node with
    | Mlistreverse (t, l) -> begin
        let tl = tlist t in
        let empty = mk_mterm (Mlitlist []) tl in
        let iid = dumloc "_liid" in
        let vid = mk_mvar iid t in
        let iaccu = dumloc "_accu" in
        let vaccu = mk_mvar iaccu t in
        let b =  mk_mterm (Mlistprepend(t, vaccu, vid)) tl in
        mk_mterm (Mlistfold(t, iid, iaccu, f l, empty, b)) tl
      end
    | Mlistconcat (t, l, m) -> begin
        let tl = tlist t in
        let iter = f (mk_mterm (Mlistreverse (t, f l)) tl) in
        let iid = dumloc "_liid" in
        let vid = mk_mvar iid t in
        let iaccu = dumloc "_accu" in
        let vaccu = mk_mvar iaccu t in
        let b =  mk_mterm (Mlistprepend(t, vaccu, vid)) tl in
        mk_mterm (Mlistfold(t, iid, iaccu, iter, f m, b)) tl
      end
    | Miter (i, a, b, c, lbl) -> begin
        let a = f a in
        let b = f b in
        let c = f c in

        let vi = mk_mvar i a.type_ in

        let ie = dumloc "_e" in
        let ve = mk_mvar ie b.type_ in

        let vinc : mterm = mk_mterm (Mplus(vi, mk_int 1)) tint in
        let inc  : mterm = mk_mterm (Massign(ValueAssign, tint, Avar i, vinc)) tunit in
        let body : mterm = mk_mterm (Mseq([c; inc])) tunit |> flat_sequence_mterm in
        let cond : mterm = mk_mterm (Mle (vi, ve)) tbool in
        let loop : mterm = mk_mterm (Mwhile (cond, body, lbl)) tunit in

        loop
        |> mk_letin i  a
        |> mk_letin ie b
      end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let normalize_storage (model : model) : model =

  let replace_var_in_storage (model : model) : model =

    let for_mterm map (mt : mterm) : mterm =
      let rec aux (mt : mterm) : mterm =
        match mt.node with
        | Mvar (id, _, _, _) when MapString.mem (unloc id) map -> MapString.find (unloc id) map
        | _ -> map_mterm aux mt
      in
      aux mt
    in

    let for_storage_item map (si : storage_item) : storage_item =
      {si with default = for_mterm map si.default}
    in

    let map = MapString.empty in
    let _, storage = List.fold_left (fun (map, l) si ->
        let si = for_storage_item map si in
        let map = MapString.add (unloc si.id) si.default map in
        (map, l @ [si])) (map, []) model.storage in
    { model with
      storage = storage }
  in

  let sort_container (model : model) : model =

    let for_mterm (mt : mterm) : mterm =
      let sort = List.sort (fun (x1, _) (x2, _) -> Utils.cmp x1 x2) in

      let rec aux (mt : mterm) : mterm =
        match mt.node with
        | Mlitset l -> begin
            {mt with node = Mlitset (l |> List.map (fun x -> x, unit) |> sort |> List.map fst)}
          end
        | Mlitmap (b, l) -> begin
            {mt with node = Mlitmap (b, sort l)}
          end
        | _ -> map_mterm aux mt
      in
      aux mt
    in

    let for_storage_item (si : storage_item) : storage_item =
      {si with default = for_mterm si.default}
    in

    { model with
      storage = List.map for_storage_item model.storage }
  in

  model
  |> replace_var_in_storage
  |> sort_container

let remove_constant (model : model) : model =

  let for_mterm map _ (mt : mterm) : mterm =
    let rec aux (mt : mterm) : mterm =
      match mt.node with
      | Mvar (id, _, _, _) when MapString.mem (unloc id) map -> MapString.find (unloc id) map
      | _ -> map_mterm aux mt
    in
    aux mt
  in

  let map = MapString.empty in

  let map, decls = List.fold_left (fun (map, dns) dn ->
      match dn with
      | Dvar dvar when ((function | VKconstant -> true | _ -> false) dvar.kind) && Option.is_some(dvar.default)-> (MapString.add (unloc dvar.name) (Option.get dvar.default) map, dns)
      | _ -> (map, dns @ [dn])) (map, []) model.decls in

  let map, storage = List.fold_left (fun (map, l) si ->
      match si.model_type with
      | MTconst -> (MapString.add (unloc si.id) si.default map, l)
      | _ -> (map, l @ [si])) (map, []) model.storage in
  { model with
    decls = decls;
    storage = storage }
  |> map_mterm_model (for_mterm map)


let eval_storage (model : model) : model =
  let sis, _ = List.fold_left (fun (sis, map) (si : storage_item) ->
      let mt = Model.Utils.eval map si.default in
      let map = (unloc si.id, mt)::map in
      (sis @ [{si with default = mt}], map)
    ) ([], []) model.storage
  in
  { model with
    storage = sis;
  }

let getter_to_entry ?(no_underscore=false) ?(extra=false) (model : model) : model =
  let for_function__ (f__ : function__) : function__ =
    let for_function_node (fn : function_node) : function_node =
      let for_function_struct (t : type_) (fs : function_struct) : function_struct =
        let process () =
          let icallback = dumloc ((if no_underscore then "" else "_") ^ "cb" )in
          let tcallback = tcontract t in
          let vcallback = mk_pvar icallback tcallback in
          let rec aux (mt : mterm) : mterm =
            match mt.node with
            | Mreturn x -> mk_mterm (Mtransfer(TKentry(mtransferred, vcallback, x))) tunit
            | _ -> map_mterm aux mt
          in
          (icallback, tcallback, None), aux fs.body
        in
        let arg, body = process () in
        let args, eargs = if extra then fs.args, [arg] else fs.args @ [arg], [] in
        {
          fs with
          args  = args;
          eargs = eargs;
          body  = body;
        }
      in
      match fn with
      | Getter(fs, t) -> Entry (for_function_struct t fs)
      | _ -> fn
    in
    { f__ with
      node = for_function_node f__.node;
    }
  in
  { model with
    functions = List.map for_function__ model.functions;
  }

let process_metadata (model : model) : model =
  let check_if_not_metadata _ =
    List.for_all (String.equal "") [!Options.opt_metadata_uri; !Options.opt_metadata_storage]
  in

  let with_metadata _ =
    let rec aux ctx (accu : bool) (mt : mterm) : bool =
      match mt.node with
      | Mmetadata -> true
      | _ -> fold_term (aux ctx) accu mt
    in
    fold_model aux model false
  in

  if check_if_not_metadata () && not (with_metadata ())
  then model
  else begin
    let model =
      let rec aux ctx (mt : mterm) : mterm =
        match mt.node with
        | Mmetadata -> mk_svar (dumloc "metadata") mt.type_
        | _ -> map_mterm (aux ctx) mt
      in
      map_mterm_model aux model
    in
    let tmetadata = tmap tstring tbytes in
    let dmap =
      let mk_map _ =

        let key = "here" in

        let mk_uri uri =
          let empty = mk_string "" in
          let uri =
            let euridata = match Hex.of_string uri with `Hex str -> str in
            mk_bytes euridata
          in
          empty, uri
        in

        let mk_data path =
          let read_whole_file filename =
            let ch = open_in filename in
            let s = really_input_string ch (in_channel_length ch) in
            close_in ch;
            s
          in
          let vkey = mk_string key in
          let input = read_whole_file path in
          let metadata =
            mk_bytes (match Hex.of_string input with `Hex str -> str)
          in
          vkey, metadata
        in

        let v =
          match !Options.opt_metadata_uri, !Options.opt_metadata_storage with
          | "", ""            -> []
          | uri, ""           -> [mk_uri uri]
          | "", metadata_path -> [mk_uri ("tezos-storage:" ^ key); mk_data metadata_path]
          | _ -> assert false
        in

        mk_mterm (Mlitmap (true, v)) tmetadata in
      if not (String.equal "" !Options.opt_metadata_uri)
      then mk_map ()
      else mk_map ()
    in
    let dvar = mk_var (dumloc "metadata") tmetadata tmetadata ~default:dmap VKvariable in
    let decl_metadata = Dvar dvar in
    let decls = model.decls @ [decl_metadata] in
    { model with
      decls = decls }
  end

let reverse_operations (model : model) : model =
  let add_reverse_operations mt =
    let reverse = mk_mterm (Mlistreverse (toperation, operations)) toperations in
    let assign = mk_mterm (Massign (ValueAssign, toperations, Aoperations, reverse)) tunit in
    seq [mt; assign] |> flat_sequence_mterm
  in
  let for_mterm mt =
    if Utils.with_operations_for_mterm mt
    then add_reverse_operations mt
    else mt
  in
  let for_functions f =
    let for_fs (fs : function_struct) =
      { fs with
        body = for_mterm fs.body;
      }
    in
    let for_fnode (node : function_node) : function_node =
      match node with
      | Function (fs, type_) -> Function (for_fs fs, type_)
      | Getter   (fs, type_) -> Getter   (for_fs fs, type_)
      | Entry     fs         -> Entry    (for_fs fs)
    in
    { f with
      node = for_fnode f.node;
    }
  in
  { model with functions = List.map for_functions model.functions }

let process_parameter (model : model) : model =
  let for_parameter (param : parameter) =
    let t = param.typ in
    let name = param.name in
    let default : mterm =
      match param.value, param.default with
      | Some v, _
      | _, Some v -> v
      | _ -> mk_parameter name t
    in
    let var : var = mk_var name t t VKvariable ~default ~loc:param.loc in
    Dvar var
  in
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mvar(a, Vparameter, c, d) -> { mt with node = Mvar(a, Vstorevar, c, d) }
    | _ -> map_mterm (aux ctx) mt
  in
  let model = map_mterm_model aux model in
  let params = List.map for_parameter model.parameters in
  { model with decls = params @ model.decls }

let fix_container (model : model) =
  let rec aux ctx (mt : mterm) =
    match mt with
    | { node = Masset fields; type_ = (Tasset an, _)} -> begin
        let an = unloc an in
        let asset = Utils.get_asset model an in
        let types = List.map (fun (x : asset_item) -> x.type_) asset.values in
        (* Format.printf "fields: %i; infos: %i@\n" (List.length fields) (List.length types); *)
        assert (List.length fields == List.length types);
        let l = List.map2 (fun t (mt : mterm) ->
            match get_ntype t, mt.node with
            | Tcontainer (((Tasset _), None), Aggregate), Mlitlist [] -> mk_mterm (Massets []) t
            | _ -> mt
          ) types fields in
        mk_mterm (Masset l) mt.type_
      end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model
