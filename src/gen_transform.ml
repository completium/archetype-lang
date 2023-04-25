open Ident
open Location
open Model
module MapString = Map.Make(String)
module SetString = Set.Make(String)
open Tools

type error_desc =
  | AssetPartitionnedby of string * string list
  | CallerNotSetInInit
  | CannotBuildAsset of string * string
  | ContainersInAssetContainers of string * string * string
  | DefaultValueOnKeyAsset of ident
  | DuplicatedKeyAsset of ident
  | InvalidInitValue
  | NoClearForPartitionAsset of ident
  | NoEmptyContainerForDefaultValue of string * string * container
  | NoEntrypoint
  | NoInitForPartitionAsset of ident
  | NoInitValueForConstParam of ident
  | NoInitValueForParameter of ident
  | NoPutRemoveForIterableBigMapAsset
  | NoSortOnKeyWithMultiKey of ident
  | OnlyLiteralInAssetInit
  | UnknownContract of ident
  | UnusedArgument of ident
  | UnusedVariable of ident

let pp_error_desc fmt = function
  | AssetPartitionnedby (i, l) -> Format.fprintf fmt "Cannot access asset collection: asset %s is partitionned by field(s) (%a)." i (Printer_tools.pp_list ", " Printer_tools.pp_str) l
  | CallerNotSetInInit -> Format.fprintf fmt "'caller' is used in initialization of contract, please set caller value with '--set-caller-init'"
  | CannotBuildAsset (an, fn) -> Format.fprintf fmt "Cannot build an asset %s, default value of field '%s' is missing." an fn
  | ContainersInAssetContainers (an, fn, an2) -> Format.fprintf fmt "Cannot build an asset '%s', '%s' is a container field, which refers to an asset '%s', which contains a container field itself." an fn an2
  | DefaultValueOnKeyAsset an -> Format.fprintf fmt "Default value on key for asset \"%s\"" an
  | DuplicatedKeyAsset an -> Format.fprintf fmt "duplicate key for '%s'" an
  | InvalidInitValue -> Format.fprintf fmt "Invalid value for initialization"
  | NoClearForPartitionAsset an -> Format.fprintf fmt "Clear is not allowed for asset '%s', because this asset is used in a partition." an
  | NoEmptyContainerForDefaultValue (an, fn, c) -> Format.fprintf fmt "Field '%s' of '%s' asset is a %a, which must be initialized by an empty container." fn an Printer_model.pp_container c
  | NoEntrypoint -> Format.fprintf fmt "No entrypoint found (action or transtion)"
  | NoInitForPartitionAsset an -> Format.fprintf fmt "Asset '%s' is used in a partition, no asset must initialized" an
  | NoInitValueForConstParam id -> Format.fprintf fmt "No initialized value for const parameter: %s" id
  | NoInitValueForParameter id -> Format.fprintf fmt "No initialized value for parameter: %s" id
  | NoPutRemoveForIterableBigMapAsset -> Format.fprintf fmt "NoPutRemoveForIterableBigMapAsset"
  | NoSortOnKeyWithMultiKey f -> Format.fprintf fmt "No sort on key with multi key: %s" f
  | OnlyLiteralInAssetInit -> Format.fprintf fmt "Only literal is allowed for asset field initialisation"
  | UnknownContract id -> Format.fprintf fmt "Cannot find type for '%s'" id
  | UnusedArgument id -> Format.fprintf fmt "Unused argument '%s'" id
  | UnusedVariable id -> Format.fprintf fmt "Unused variable '%s'" id

type error = Location.t * error_desc

let emit_error (lc, error : Location.t * error_desc) =
  let str : string = Format.asprintf "%a@." pp_error_desc error in
  let pos : Position.t list = [location_to_position lc] in
  Error.error_alert pos str (fun _ -> ())

let emit_warning (lc, error : Location.t * error_desc) =
  let str : string = Format.asprintf "%a@." pp_error_desc error in
  let pos : Position.t list = [location_to_position lc] in
  Error.add_warning pos str (fun _ -> ())

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
      let f = mk_mterm (Mfail NatNegAssign) tunit in
      let c = mk_mterm (Mcast (tunit, tnat, f)) tnat in
      mk_mterm (Mexprif (cond, v, c)) tnat
    end
  | MinusAssign , _ -> mk_mterm (Mminus (lhs, v)) t
  | MultAssign  , _ -> mk_mterm (Mmult (lhs, v)) t
  | DivAssign   , _ -> mk_mterm (Mdivrat (lhs, v)) t
  | AndAssign   , _ -> mk_mterm (Mand (lhs, v)) t
  | OrAssign    , _ -> mk_mterm (Mor (lhs, v)) t

let remove_add_update ?(isformula = false) (model : model) : model =
  let error = ref false in
  let f_error (l : Location.t) (an : string) (fn : string) = emit_error(l, CannotBuildAsset (an, fn)); error := true in
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Maddupdate (an, c, k, l) ->
      begin
        let type_asset = tasset (mk_mident (dumloc an)) in
        let is_put =
          let asset = Utils.get_asset model an in
          let fields_ref =
            asset.values
            |> List.remove_if (fun (x : asset_item) -> x.shadow || Option.is_some x.default)
            |> List.map (fun (x : asset_item) -> unloc_mident x.name)
          in

          let fields_actual = List.map (fun (id,_,_) -> unloc_mident id) l in

          let is_all = List.for_all (fun (f : ident) -> List.exists (String.equal f) fields_ref) fields_actual in

          let is_standalone = List.fold_left (fun accu (_, op, _) -> match op with | ValueAssign -> accu | _ -> false) true l in

          is_all && is_standalone
        in
        let mk_asset (an, k, l) =
          let dummy_mterm = mk_mterm (Mseq []) tunit in
          let asset = Utils.get_asset model an in
          let lref : (Ident.ident * (assignment_operator * mterm * Location.t)) list = List.map (fun (x, y, z) -> (unloc_mident x, (y, z, loc_mident x))) l in
          let l = List.map (
              fun (f : asset_item) ->
                let f_name = (unloc_mident f.name) in
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

        let asset = mk_asset (an, k, l) in
        if is_put
        then mk_mterm (Mputsingleasset (an, asset)) tunit
        else
          let cond = mk_mterm (
              match c with
              | CKfield (_, _, ({node = Mdotassetfield (andat, kdat, fn)} as a)) ->
                let c = (if isformula then a else kdat) in Mcontains (an, CKfield(unloc_mident andat, unloc_mident fn, c), k)
              | CKcoll -> Mcontains (an, c, k)
              | _ -> assert false) tunit
          in
          match c with
          | CKfield (_, ckcol, {node = Mdotassetfield (dan, dk, dfn)}) when Utils.is_partition model (unloc_mident dan) (unloc_mident dfn) -> begin
              let cond = mk_mterm (Mcontains(an, CKcoll, k)) tbool in
              let fail_ = failc (Invalid (mk_tuple [mk_string fail_msg_KEY_NOT_FOUND; k])) in
              let cond_nested = mk_mterm (Mcontains(an, CKfield (unloc_mident dan, ckcol, dk), k)) tbool in
              let update = mk_mterm (Mupdate (an, k, l)) tunit in
              let if_nested = mk_mterm (Mif (cond_nested, update, Some fail_)) tunit in
              let add = mk_mterm (Maddfield (unloc_mident dan, unloc_mident dfn, dk, asset)) tunit in
              let r = mk_mterm (Mif (cond, if_nested, Some add)) tunit in
              r
            end
          | _ -> begin
              let add = mk_mterm (
                  match c with
                  | CKfield (_, _, {node = Mdotassetfield (an, k, fn)}) -> Maddfield (unloc_mident an, unloc_mident fn, k, asset)
                  | CKcoll -> Mputsingleasset (an, asset)
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
      let f = List.find (fun (x : asset_item) -> String.equal (unloc_mident x.name) (unloc_mident fn)) asset.values in
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
          List.fold_right (fun (fn, op, v : mident * assignment_operator * mterm) (accu_l, accu_instrs) ->
              if is_field_container asset (fn, op, v)
              then begin
                let fn = unloc_mident fn in
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
                    let aan, _ = Utils.get_field_container model an fn in
                    match with_remove with
                    | true -> (mk_mterm (Mremoveall (an, CKfield(aan, fn, k))) tunit)::instrs
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
              else ((fn, op, v)::accu_l, accu_instrs)) l ([], [])
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


let remove_container_op_in_update_exec (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    let is_basic_container asset (fn, _, _) =
      let f = List.find (fun (x : asset_item) -> String.equal (unloc_mident x.name) (unloc_mident fn)) asset.values in
      match get_ntype f.original_type with
      | Tset _ | Tmap _ -> true
      | _ -> false
    in
    let with_basic_container an l =
      let asset = Model.Utils.get_asset model an in
      List.exists (is_basic_container asset) l
    in
    match mt.node with
    | Mupdate (an, k, l) when with_basic_container an l -> begin
        let asset = Model.Utils.get_asset model an in
        let newl, instrs =
          List.fold_right (fun (fn, op, v : mident * assignment_operator * mterm) (accu_l, accu_instrs) ->
              let def_value = ((fn, op, v)::accu_l, accu_instrs) in
              if is_basic_container asset (fn, op, v)
              then begin
                let a, tty =
                  let f = List.find (fun (x : asset_item) -> String.equal (unloc_mident x.name) (unloc_mident fn)) asset.values in
                  mk_mterm (Mdotassetfield (mk_mident (dumloc an), k, f.name)) f.original_type,
                  match get_ntype f.original_type with
                  | Tset ty       -> `Set ty
                  | Tmap (kt, vt) -> `Map (kt, vt)
                  | _ -> assert false
                in

                let process kind =
                  let mk accu (x : mterm) : mterm = begin
                    match kind with
                    | `Add    -> begin
                        match tty with
                        | `Set  ty      -> mk_mterm (Msetadd (ty, accu, x)) (tset ty)
                        | `Map (kt, vt) -> mk_mterm (Mmapput (MKMap, kt, vt, accu, mk_tupleaccess 0 x, mk_tupleaccess 1 x)) (tmap kt vt)
                      end
                    | `Remove -> begin
                        match tty with
                        | `Set  ty      -> mk_mterm (Msetremove (ty, accu, x)) (tset ty)
                        | `Map (kt, vt) -> mk_mterm (Mmapremove (MKMap, kt, vt, accu, x)) (tmap kt vt)
                      end
                  end
                  in

                  let v : mterm = begin
                    match v.node with
                    | Massets  ll
                    | Mlitlist ll -> List.fold_right (fun (a : mterm) accu -> mk accu a) ll a
                    | _ -> match kind with | `Add -> mk_mterm (Mplus (a, v)) a.type_ | `Remove -> mk_mterm (Mminus (a, v)) a.type_
                  end
                  in
                  ((fn, ValueAssign, v)::accu_l, accu_instrs)
                in
                match op with
                | PlusAssign  -> process `Add
                | MinusAssign -> process `Remove
                | _ -> def_value
              end
              else def_value) l ([], [])
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

let remove_empty_update (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Mupdate (_, _, l) when List.is_empty l-> skip
    | Mupdateall (_, _, l) when List.is_empty l-> skip
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let build_get (an : ident) v = mk_mterm (Mget (an, CKcoll, v)) (tasset (mk_mident (dumloc an)))

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

let check_containers_asset (model : model) : model =
  let assets = Utils.get_assets model in
  let is_container t =
    match get_ntype t with
    | Tcontainer _ -> true
    | _ -> false
  in
  let is_asset_with_container (an : mident) : bool =
    let an = unloc_mident an in
    let a = Utils.get_asset model an in
    List.exists (fun (item : asset_item) -> is_container item.type_) a.values
  in
  let l : (string * string * string * Location.t) list =
    List.fold_left (fun accu (a : asset) ->
        List.fold_left (fun accu (item : asset_item) ->
            match get_ntype item.type_ with
            | Tcontainer ((Tasset an, _), _) when is_asset_with_container an -> (unloc_mident a.name, unloc_mident item.name, unloc_mident an, item.loc)::accu
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
            | Tcontainer ((Tasset an, _), c), Some dv when not (is_emtpy_container dv) -> (unloc_mident an, unloc_mident a.name, c, dv.loc)::accu
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
               if List.exists (String.equal (unloc_mident s)) dasset.keys
               then errors := ((loc_mident s), NoSortOnKeyWithMultiKey (unloc_mident s))::!errors
             ) dasset.sort;
           try
             let field = List.find (fun (x : asset_item) -> List.exists (String.equal (unloc_mident x.name)) dasset.keys) dasset.values in
             match field.default with
             | Some dv -> errors := (dv.loc, DefaultValueOnKeyAsset (unloc_mident dasset.name))::!errors
             | _ -> ()
           with Not_found -> assert false
         end
       | _ -> ()) model.decls;
  List.iter emit_error !errors;
  model

let check_invalid_init_value (model : model) : model =
  let for_decl (d : decl_node) : unit =
    let for_mterm (mt : mterm) : unit =
      let rec aux accu (mt : mterm) : unit =
        match mt.node with
        | Mbalance
        | Mcaller
        | Mlevel
        | Mnow
        | Mselfaddress
        | Mselfchainid
        | Msource
        | Mtransferred
        | Mtotalvotingpower
        | Mpack _
        | Munpack _
          -> emit_error (mt.loc, InvalidInitValue)
        | _ ->
          fold_term aux accu mt
      in
      aux () mt
    in
    match d with
    | Dvar ({default = Some v; kind = k}) when (match k with | VKvariable -> true | _ -> false) -> for_mterm v
    | Dasset a -> List.iter for_mterm a.init
    | _ -> ()
  in
  List.iter for_decl model.decls;
  model

let rec is_literal (mt : mterm) : bool =
  match mt.node with
  | Munit
  | Mbool      _
  | Mint       _
  | Mnat       _
  | Mrational  _
  | Mstring    _
  | Mmutez     _
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
              | Tcontainer ((Tasset an, _), Partition), _ -> (unloc_mident an)::accu
              | _ -> accu
            ) [] da.values
        | _ -> []
      ) model.decls |> List.flatten
  in

  let errors : (Location.t * error_desc) list =
    List.map (function
        | Dasset da -> begin
            let an : ident = unloc_mident da.name in
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
    let assoc_fields = List.map2 (fun (ai : asset_item) (x : mterm) -> (unloc_mident ai.name, x)) asset.values l in
    let value_key_opt =
      match List.find (fun (id, _) -> List.exists (String.equal id) asset_keys) assoc_fields |> snd with
      | {node = Mvar (id, _); _} -> begin
          let const = Model.Utils.get_vars model |> List.find_opt (fun (x : var) -> cmp_ident (unloc_mident id) (unloc_mident x.name)) in
          match const with
          | Some c -> c.default
          | None -> None
        end
      | x -> Some x
    in
    match value_key_opt with
    | Some value_key -> begin
        if not (is_literal value_key)
        then errors := (value_key.loc, OnlyLiteralInAssetInit)::!errors
        else (
          if contains_key an value_key
          then errors := (value_key.loc, DuplicatedKeyAsset an)::!errors
          else add_key an value_key)
      end
    | None -> ()
  in
  List.iter
    (fun d ->
       match d with
       | Dasset dasset -> begin
           let an = unloc_mident dasset.name in
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
                                   | Masset lll -> check_asset_key (unloc_mident aan) lll
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
                                    let aan = unloc_mident aan in
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
      | Dasset a when MapString.mem (unloc_mident a.name) map -> Dasset { a with init = a.init @ (MapString.find (unloc_mident a.name) map )}
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
  let process_declvar (ids, t, init) accu =
    begin
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
          | Mdeclvar (ids, t, v, _) ->
            let res =  process_declvar (ids, t, LVsimple (aux c v)) accu in
            [ res ]
          | Mdetach (id, dk, tya, fa) ->
            let va = match dk with | DK_option (_, id) -> mk_mident (dumloc id) | DK_map (_, id, _) -> mk_mident (dumloc id) in
            let res = process_declvar ([id], Some tya, LVreplace (va, dk, aux c fa)) accu in
            [ res ]
          | _ ->
            begin
              let t = aux c x in
              t::accu
            end
        ) l [] in
      { mt with node = Mseq ll }
    | Mdeclvar (ids, t, v, _) -> process_declvar (ids, t, LVsimple (aux c v)) []
    | Mdetach (id, dk, tya, fa) -> begin
        let va = match dk with | DK_option (_, id) -> mk_mident (dumloc id) | DK_map (_, id, _) -> mk_mident (dumloc id) in
        process_declvar ([id], Some tya, LVreplace (va, dk, aux c fa)) []
      end
    | _ -> map_mterm (aux c) mt
  in
  Model.map_mterm_model aux model

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

  let map =
    let mk_enum_info (e : enum) : enum_info =
      let without_args = List.for_all (fun (x : enum_item) -> List.is_empty x.args) e.values in
      let mk_args_type (annot : lident) (args : type_ list) =
        let annot = mkfannot annot in
        match args with
        | []  -> mktype Tunit ?annot
        | [t] -> mktype (get_ntype t) ?annot
        | _   -> mktype (get_ntype (ttuple args)) ?annot
      in
      let mk_or l =
        match List.rev l with
        | z::q -> List.fold_right (fun x accu -> tor x accu) (List.rev q) z
        | _    -> assert false
      in
      let mk_type _  =
        if without_args
        then tint
        else begin
          let f = mk_args_type in
          let l = List.map (fun (x : enum_item) -> f (snd x.name) x.args) e.values in
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
                MapString.add (normalize_mident (mk_mident ?namespace:(fst e.name) (snd x.name))) (fun _ -> mk_int i) accu)
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
          let l = List.map (fun (x : enum_item) -> f (snd x.name) x.args) values in
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
              MapString.add (normalize_mident x.name) f accu
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
                       MapString.add (normalize_mident x.name) (mk_int i) accu)
                    MapString.empty e.values
                in
                let ivar = mk_mident (dumloc "_tmp") in
                let mvar : mterm = mk_mvar ivar tint in
                let mk_cond (id : ident) = mk_mterm (Mequal (tint, MapString.find id map, mvar)) tbool in
                let mk_if (id : ident) (v : mterm) (accu : mterm) : mterm =
                  match rt with
                  | None    -> mk_mterm (Mif (mk_cond id, v, Some accu)) tunit
                  | Some rt -> mk_mterm (Mexprif (mk_cond id, v, accu))  rt
                in
                let v =
                  List.fold_left (fun accu (p, v: pattern * mterm) ->
                      match p.node with
                      | Pwild -> assert false
                      | Pconst (id, _) -> mk_if (normalize_mident id) v accu)
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
                | Pconst (i, args) when String.equal (normalize_mident i) (normalize_mident enum_item.name) -> Some (args, v, idx)
                | _ -> accu) None ps
            in
            let id_var_or = mk_mident (dumloc ("_var_or")) in
            match v, dvopt with
            | Some ([], v, _), _  -> id_var_or, v
            | Some ([a], v, _), _ -> mk_mident a, v
            | Some (l, v, _), _ -> begin
                let ts : type_ list =  enum_item.args in
                let var = mk_mvar id_var_or (ttuple ts) in
                let l2 : (int * mident) list = List.mapi (fun (i : int) (x : lident) -> i, mk_mident x) l in
                let v = List.fold_right (fun (i, x : int * mident) (accu : mterm) -> accu |> mk_letin x (mk_tupleaccess i var)) l2 v in
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
            match normalize_mident e.name with
            | "state" -> List.fold_left (fun accu x -> MapString.add x (mk_enum_info e) accu) accu ["state"; "$state"]
            | _       -> MapString.add (normalize_mident e.name) (mk_enum_info e) accu
          end
        | _ -> accu) MapString.empty model.decls
  in

  let get_enum_id_opt t = match get_ntype t with | Tstate -> Some "$state" | Tenum eid -> Some (normalize_mident eid) | _ -> None in
  let get_enum_id t     = t |> get_enum_id_opt |> Option.get in
  let is_tenum t        = t |> get_enum_id_opt |> Option.is_some in

  let get_info eid =
    if not (MapString.mem eid map) then (
      Format.eprintf "error get_info: %s@\n" eid;
      MapString.iter (fun x _ -> Format.eprintf "key: %s@\n"  x) map;
      assert false);
    MapString.find eid map
  in

  let for_type t : type_ =
    let rec aux t =
      match get_ntype t with
      | Tstate -> tint
      | Tenum id -> begin
          let info : enum_info = get_info (normalize_mident id) in
          info.type_
        end
      | _ -> map_type aux t
    in
    aux t
  in

  let for_mterm (mt : mterm) : mterm =
    let rec aux (mt : mterm) =
      let state = "_state" in
      let dstate = mk_mident (dumloc state) in
      let for_matchwith rt (e : mterm) ps : mterm =
        let eid = get_enum_id e.type_ in
        let info : enum_info = get_info eid in
        let e = aux e in
        let ps = List.map (fun (p, x) -> p, aux x) ps in
        info.fmatch rt e ps
      in
      match mt.node with
      | Mvar (_, Vstate)    -> mk_svar dstate tint
      | Massign (_, _, Astate, v) -> {mt with node = Massign (ValueAssign, tint, Avarstore dstate, aux v)}
      | Menumval (id, args, eid)  -> begin
          let args = List.map aux args in
          let info : enum_info = get_info (normalize_mident eid) in
          let f =
            match MapString.find_opt (normalize_mident id) info.fitems with
            | Some f -> f
            | None -> begin
                Format.eprintf "NotFound: %s@\n" (normalize_mident id);
                MapString.iter (fun x _ -> Format.eprintf "key: %s@\n"  x) info.fitems;
                assert false
              end
          in
          f args
        end
      | Mmatchwith     (e, ps) when is_tenum (e.type_) -> for_matchwith None e ps
      | Mexprmatchwith (e, ps) when is_tenum (e.type_) -> for_matchwith (Some mt.type_) e ps
      | _ -> let mt = map_mterm ~ft:for_type aux mt in { mt with type_ = for_type mt.type_ }
    in
    aux mt
  in

  let add_od_enum (model : model) : model =
    let lll = List.fold_left (fun accu x -> begin
          match x with
          | Denum e -> begin
              let ename : string = normalize_mident e.name in
              let te = for_type (tenum (e.name)) in
              ODEnum (mk_odel_enum ename te)::accu
            end
          | _ -> accu
        end ) [] model.decls in

    {model with extra = {original_decls = (lll @ model.extra.original_decls)}}
  in

  let clean model =
    let decls =
      List.fold_right (fun x accu ->
          match x with
          | Denum ({name = (_, {pldesc = "state"})} as e) ->
            let initial = e.initial in
            let info : enum_info = get_info "state" in
            let dv = (MapString.find (unloc_mident initial) info.fitems) [] in
            (Dvar (mk_var (mk_mident (dumloc "_state")) tint tint VKvariable ~default:dv ))::accu
          | Denum _ -> accu
          | x -> x::accu
        ) model.decls []
    in
    { model with decls = decls }
  in
  model
  (* |> process_asset_state *)
  |> add_od_enum
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
  let is_rat      t = match get_ntype t with | Tbuiltin Brational -> true | _ -> false in

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
            match op, get_ntype a.type_, get_ntype b.type_ with
            | `Divrat, Tbuiltin Btez, Tbuiltin Btez -> begin
                let lhs = a |> aux |> mk_muteztonat |> mk_nattoint in
                let rhs = b |> aux |> mk_muteztonat in
                mk_tuple [lhs; rhs]
              end
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
        | Tbuiltin Btez -> begin
            match op, get_ntype a.type_, get_ntype b.type_ with
            | `Mult, Tbuiltin Bnat,      Tbuiltin Btez
            | `Mult, Tbuiltin Bint,      Tbuiltin Btez
            | `Mult, Tbuiltin Brational, Tbuiltin Btez -> begin
                let lhs = a |> aux |> to_rat in
                let rhs = b |> aux in
                mk_mterm (Mrattez (lhs, rhs)) ttez
              end
            | `Diveuc, Tbuiltin Btez, Tbuiltin Bnat
            | `Diveuc, Tbuiltin Btez, Tbuiltin Bint -> begin
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

      let do_fun op (lhs : mterm) (rhs : mterm) : mterm =
        let id_lhs = mk_mident (dumloc "_lhs") in
        let id_rhs = mk_mident (dumloc "_rhs") in
        let vlhs = mk_mvar id_lhs trat in
        let vrhs = mk_mvar id_rhs trat in
        let lhs = lhs |> aux |> to_rat in
        let rhs = rhs |> aux |> to_rat in
        let c : mterm = mk_mterm (Mratcmp ((match op with | `Min -> Lt | `Max -> Gt), vlhs, vrhs)) tbool in
        let mt : mterm = mk_mterm (Mexprif (c, vlhs, vrhs)) trat in
        mt
        |> mk_letin id_rhs rhs
        |> mk_letin id_lhs lhs
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
      | Mmax      (a, b) when is_rat mt.type_  -> do_fun `Max a b
      | Mmin      (a, b) when is_rat mt.type_  -> do_fun `Min a b
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
      let annot = get_atype t in
      match get_ntype t with
      | Tbuiltin Bdate     -> mktype ?annot (Tbuiltin Btimestamp)
      | Tbuiltin Bduration -> mktype ?annot (Tbuiltin Bint)
      | _ -> map_type aux t
    in
    aux t
  in
  let rec to_timestamp (x : mterm) =
    let mk n = mk_mterm (Mtimestamp n) ttimestamp in
    let extract (x : mterm) =
      match x.node with
      | Mtimestamp n -> n
      | Mvar (_id, Vparam) -> begin
          assert false
        end
      | _ -> assert false
    in
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
    | Minttodate v  -> mk (g v)
    | Mtimestamp _  -> x
    | Mvar (_, _) -> x
    | Mmin _ -> x
    | Mmax _ -> x
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
      | Minttodate _, _ -> to_timestamp mt
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
          node = Mletin (ids, (match v with | LVsimple v -> LVsimple (aux v) | LVreplace (id, k, fa) -> LVreplace (id, k, aux fa)), Option.map process_type t, aux body, Option.map aux o)
        }
      | Mdeclvar (ids, t, v, c), _ ->
        { mt with
          node = Mdeclvar (ids, Option.map process_type t, aux v, c)
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
           | View (fs, ret, vv) -> View     (process_fs fs, process_type ret, vv)
           | Entry fs           -> Entry    (process_fs fs)
         );
        })
  }
  |> update_nat_int_rat

let abs_tez model : model =
  let is_cur (mt : mterm) =
    match get_ntype mt.type_ with
    | Tbuiltin Btez -> true
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
      let an = unloc_mident an in
      let l = [(fn, op, v)] in
      mk_mterm (Mupdate (an, key, l)) tunit

    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let eval_variable_initial_value (model : model) : model =
  let map_value : (ident * mterm) list =
    List.fold_left (fun accu x ->
        match x with
        | Dvar v when Option.is_some v.default -> (unloc_mident v.name, Option.get v.default)::accu
        | _ -> accu
      ) [] model.decls in
  { model with
    decls = List.map (
        fun x ->
          match x with
          | Dvar v -> Dvar { v with default = Option.map (Model.Utils.eval map_value) v.default }
          | _ -> x) model.decls;
  }

let merge_update (model : model) : model =
  let contains l (ref, _, _) = List.fold_left (fun accu (id, _, _) -> accu || (String.equal (unloc_mident ref) (unloc_mident id))) false l in
  let replace l (ref_id, ref_op, ref_v) =
    List.fold_right (fun (id, op, v) accu ->
        if (String.equal (unloc_mident ref_id) (unloc_mident id))
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

let replace_dotassetfield_by_dot (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mdotassetfield (an, k, fn) ->
      begin
        let k = aux ctx k in
        let get = build_get (unloc_mident an) k in
        mk_mterm (Mdot (get, fn)) mt.type_
      end
    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model

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

let split_key_values (model : model) : model =

  let asset_assets an = an in

  let get_asset_assoc_key_value (asset_name : ident) (asset_value : mterm) : mterm * mterm=
    match asset_value.node with
    | Masset l ->
      begin
        let asset : asset = Model.Utils.get_asset model asset_name in
        let asset_keys = asset.keys in

        let assoc_fields = List.map2 (fun (ai : asset_item) (x : mterm) -> (unloc_mident ai.name, x)) asset.values l in

        List.find (fun (id, _) -> List.exists (String.equal id) asset_keys) assoc_fields |> snd,
        { asset_value with
          node = Masset (List.map (fun (x : mterm) ->
              match x with
              | { type_ = (Tcontainer ((Tasset an, _), c), _); _} -> { x with type_ = let k = Utils.get_asset_key model (unloc_mident an) |> snd in (mktype (Tcontainer (k, c))) }
              | _ -> x) l)
        }
      end
    | _ -> assert false
  in

  let storage =
    List.fold_right (fun x accu ->
        match x.model_type with
        | MTasset an ->
          let an = mk_mident (dumloc an) in
          let asset = Utils.get_asset model (unloc_mident an) in
          let _k, t = Utils.get_asset_key model (unloc_mident an) in
          let type_asset = (match asset.map_kind with | MKIterableBigMap -> titerable_big_map | MKBigMap -> tbig_map | MKMap -> tmap) t (tasset an) in
          let default =
            match x.default.node with
            | Massets l -> mk_mterm (Mlitmap (asset.map_kind, List.map (fun x -> get_asset_assoc_key_value (unloc_mident an) x) l)) type_asset
            | _ -> assert false
          in
          let asset_assets =
            mk_storage_item (mk_mident (dumloc (asset_assets (unloc_mident an))))
              (MTasset (unloc_mident an))
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
      mk_mterm (Mlitset l) (tset ((Utils.get_asset_key model (unloc_mident an) |> snd)))
    | _ -> map_mterm (aux ctx) mt
  in

  { model with
    storage = storage
  } |> map_mterm_model aux

let remove_duplicate_key (model : model) : model =
  let remove_key_value_for_asset_node (mt : mterm) : mterm =
    match mt.node, get_ntype mt.type_ with
    | Masset l, Tasset an ->
      begin
        let an = unloc_mident an in
        let k, _ = Utils.get_asset_key model an in
        let l =
          Utils.get_labeled_value_from model an l
          |> List.filter (fun (lbl, _) -> not (String.equal lbl k))
        in
        mk_mterm (Mlitrecord l) (tasset (mk_mident (dumloc (an))))
      end
    | _ -> mt
  in

  let storage =
    List.fold_right (fun x accu ->
        match x.model_type with
        | MTasset an ->
          if Utils.is_asset_single_field model an && Utils.is_asset_map model an
          then
            begin
              let _k, t = Utils.get_asset_key model an in
              let type_asset = tset t in
              let default =
                match x.default.node with
                | Mlitmap (_, l) -> mk_mterm (Mlitset (List.map fst l)) type_asset
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
              | Mlitmap (_, l), ((Titerable_big_map (kt, (Tasset an, _)) | Tbig_map (kt, (Tasset an, _)) | (Tmap (kt, (Tasset an, _)))) as map) ->
                let mkm, mkmm =
                  match map with
                  | Titerable_big_map _ -> titerable_big_map, MKIterableBigMap
                  | Tbig_map _ -> tbig_map, MKBigMap
                  | Tmap _ -> tmap, MKMap
                  | _ -> assert false
                in
                let t = mkm kt (tasset an) in
                let mt = mk_mterm (Mlitmap (mkmm, List.map (fun (k, v) -> (k, remove_key_value_for_asset_node v)) l)) t in
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
      let lhs = mk_mterm (Mvar (id, Vlocal)) v.type_ in
      let v = process_assign_op op t lhs v in
      mk_mterm (Massign (ValueAssign, t, Avar id, v)) mt.type_
    | Massign (op, t, Avarstore id, v) ->
      let lhs = mk_mterm (Mvar (id, Vstorevar)) v.type_ in
      let v = process_assign_op op t lhs v in
      mk_mterm (Massign (ValueAssign, t, Avarstore id, v)) mt.type_
    | Massign (op, t, Aasset (an, fn, k), v) ->
      let lhs = mk_mterm (Mdotassetfield (an, k, fn)) v.type_ in
      let v = process_assign_op op t lhs v in
      mk_mterm (Massign (ValueAssign, t, Aasset (an, fn, k), v)) mt.type_
    | Massign (op, t, Arecord (lv, rn, fn), v) ->
      let lhs = mk_mterm (Mdot (lv, fn)) v.type_ in
      let v = process_assign_op op t lhs v in
      mk_mterm (Massign (ValueAssign, t, Arecord (lv, rn, fn), v)) mt.type_
    | Massign (op, t, Atuple (lv, i, l), v) ->
      let lhs = mk_tupleaccess i lv in
      let v = process_assign_op op t lhs v in
      mk_mterm (Massign (ValueAssign, t, Atuple (lv, i, l), v)) mt.type_
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let replace_col_by_key_for_ckfield (model : model) =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    (* asset api effect *)
    (* | Mclear (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}))             -> { mt with node = Mclear (an, CKfield (fan, ffn, kdat)) } *)
    (* | Maddupdate (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}), k, l)   -> { mt with node = Maddupdate (an, CKfield (fan, ffn, kdat), k, l) } *)
    (* asset api expression *)
    | Mget (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}), k)            -> { mt with node = Mget (an, CKfield (fan, ffn, kdat), k) }
    | Mselect (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}), la, lb, a) -> { mt with node = Mselect (an, CKfield (fan, ffn, kdat), la, lb, a) }
    | Msort (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}), l)           -> { mt with node = Msort (an, CKfield (fan, ffn, kdat), l) }
    | Mcontains (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}), i)       -> { mt with node = Mcontains (an, CKfield (fan, ffn, kdat), i) }
    | Mnth (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}), i)            -> { mt with node = Mnth (an, CKfield (fan, ffn, kdat), i) }
    | Mcount (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}))             -> { mt with node = Mcount (an, CKfield (fan, ffn, kdat)) }
    | Msum (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}), p)            -> { mt with node = Msum (an, CKfield (fan, ffn, kdat), p) }
    | Mhead (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}), i)           -> { mt with node = Mhead (an, CKfield (fan, ffn, kdat), i) }
    | Mtail (an, CKfield (fan, ffn, {node = Mdotassetfield (_, kdat, _)}), i)           -> { mt with node = Mtail (an, CKfield (fan, ffn, kdat), i) }
    (* default *)
    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model

let optimize (model : model) =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    (* asset api effect *)
    | Mnot ({node = Mnot x; _}) -> aux ctx x
    | Mtupleaccess ({node = Mtuple l; _}, n) -> let x = List.nth l (Big_int.int_of_big_int n) in aux ctx x
    (* default *)
    | _ -> map_mterm (aux ctx) mt
  in
  Model.map_mterm_model aux model

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
            let new_key_field = mk_asset_item (mk_mident (dumloc new_key)) new_key_type new_key_type in
            let new_values = List.fold_right (fun (f : asset_item) accu -> if List.exists (String.equal (unloc_mident f.name)) keys then accu else f::accu ) a.values [] in
            let keys_index = List.fold_lefti (fun i accu (x : asset_item) ->
                let id = unloc_mident x.name in
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
          | Dasset a when String.equal (unloc_mident a.name) asset_name -> Dasset (for_asset a)
          | _ -> d
        in
        let rec aux ctx (mt : mterm) : mterm =
          let new_key, new_key_type, keys_index = !r in
          let check (an, fn) = String.equal (unloc_mident an) asset_name && List.exists (fun (id, _) -> String.equal (unloc_mident fn) id) keys_index in
          let process fn node : mterm =
            let fn = unloc_mident fn in
            let idx = List.assoc fn keys_index in
            let x : mterm = mk_mterm node new_key_type in
            let node = Mtupleaccess (x, Big_int.big_int_of_int idx) in
            mk_mterm node mt.type_
          in
          match mt with
          | {node = Masset l; type_ = (Tasset an, _)} when String.equal (unloc_mident an) asset_name ->
            {mt with node = Masset (build_asset keys_index new_key_type l)}
          | { node = Mdotassetfield (an, k, fn)} when check (an, fn) -> begin
              let k = aux ctx k in
              let node = Mdotassetfield (an, k, mk_mident (dumloc new_key)) in
              process fn node
            end
          | {node = Mdot (({type_ = (Tasset an, _)} as a), fn)} when check (an, fn) -> begin
              let a = aux ctx a in
              let node = Mdot (a, mk_mident (dumloc new_key)) in
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
  |> List.map (fun (asset : asset) -> unloc_mident asset.name)
  |> List.fold_left fold model

let eval_storage (model : model) : model =
  let map : mterm MapString.t =
    List.fold_left (fun (accu : mterm MapString.t) decl ->
        match decl with
        | Dvar v when Option.is_some v.default -> MapString.add (unloc_mident v.name) (Option.get v.default) accu
        | _ -> accu
      ) MapString.empty model.decls
  in

  let for_storage_item (map : mterm MapString.t) (si : storage_item) : storage_item =
    let for_mterm (mt : mterm) : mterm =
      let rec aux (mt : mterm) : mterm =
        match mt.node with
        | Mvar (id, _) when MapString.mem (unloc_mident id) map -> MapString.find (unloc_mident id) map
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
    let fs_name = unloc_mident fs.name in

    let is_not_constant (model : model) (id : mident) =
      let ovar = Utils.get_var_opt model id in
      let cst =
        match ovar with
        | Some dvar -> (match dvar.kind with | VKconstant -> true | _ -> false)
        | None -> false
      in
      not cst
    in

    let rec aux (accu, map) (mt : mterm) : mterm * (argument list * mterm list MapString.t) =
      let g (x : mterm__node) : mterm = { mt with node = x; } in
      match mt.node with
      | Mvar (id, (Vstorecol | Vstorevar)) when is_not_constant model id -> begin
          let a =
            if List.exists (fun (c, _, _) -> String.equal (unloc_mident id) (unloc_mident c) ) accu
            then accu, map
            else
              let args = if MapString.mem fs_name map then MapString.find fs_name map else [] in
              ((id, mt.type_, None)::accu |> List.dedup, (MapString.add fs_name (mt::args) map))
          in
          g (Mvar (id, Vparam)), a
        end
      | Mapp (id, app_args) when MapString.mem (unloc_mident id) map -> begin

          let ((app_args, (accu, map)) : 'c list * 'a) =
            List.fold_left
              (fun (pterms, accu) x ->
                 let p, accu = aux accu x in
                 pterms @ [p], accu) ([], (accu, map)) app_args
          in

          let aaa = MapString.find (unloc_mident id) map in
          let args = if MapString.mem fs_name map then MapString.find fs_name map else [] in
          let a = (List.map (fun (mt : mterm) ->
              ((match mt.node with Mvar(id, _) -> id | _ -> assert false), mt.type_, None) ) aaa) in
          let dargs = List.dedupcmp (fun (a : argument) (b : argument) -> cmp_mident (proj3_1 a) (proj3_1 b)) (a @ accu) in
          let dvars = List.dedupcmp cmp_mterm (aaa @ args) in
          { mt with node = Mapp (id, app_args) }, (dargs, (MapString.add fs_name dvars map))
        end
      | _ -> fold_map_term g aux (accu, map) mt
    in
    aux ([], map) fs.body
  in

  let rec apply_args map (mt : mterm) : mterm =
    match mt.node with
    | Mapp (id, args) when MapString.mem (unloc_mident id) map -> begin
        let args = List.map (apply_args map) args in
        let nargs = MapString.find (unloc_mident id) map in
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
        if List.exists (String.equal (unloc_mident x.name)) asset.keys
        then fields
        else (
          let type_ =
            match get_ntype x.type_ with
            | Tcontainer ((Tasset an, _), _) -> tset (Utils.get_asset_key model (unloc_mident an) |> snd)
            | Tcontainer (b, _) -> tset (b)
            | _ -> x.type_
          in
          fields @ [unloc_mident x.name, type_]
        )) [] asset.values
    in
    match fields with
    | [] -> tunit, ["_v", tunit], false
    | [_, t] -> t, fields, true
    | _ -> (trecord (mk_mident (dumloc an))), fields, false
  in

  let for_asset_type an =
    let _, kt = Utils.get_asset_key model an in
    if Utils.is_asset_single_field model an && Utils.is_asset_map model an
    then kt
    else ttuple [kt; proj3_1 (for_type an)]
  in

  let process_storage (model : model) : model * ((bool * bool) * (type_ * type_)) MapString.t =
    let for_storage_item map (x : storage_item) =
      let map_storage_mterm (mt : mterm) : mterm =
        match mt with
        | { node = Mlitmap (b, l) } -> begin
            let rec aux (mt : mterm) : mterm =
              match mt with
              | { node = Massets _l; type_ = (Tcontainer ((Tasset an, _), (Partition | Aggregate)), _)} ->
                let kt = Utils.get_asset_key model (unloc_mident an) |> snd in
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
      | MTasset an, ((Tmap (k, (Tasset _, _)) | Tbig_map (k, (Tasset _, _)) | Titerable_big_map (k, (Tasset _, _))) as mmap) -> begin
          let mkmm = match mmap with | Tmap _ -> tmap | Tbig_map _ -> tbig_map | Titerable_big_map _ -> titerable_big_map | _ -> assert false in
          let ts, fields, is_single_record = for_type an in
          let type_ = mkmm k ts in
          let default = map_storage_mterm x.default in
          let d = match fields with | [] | [_] -> [] | _ -> [Drecord (mk_record (mk_mident (dumloc an)) ~fields:(List.map (fun (id, t) -> mk_record_field (mk_mident (dumloc id)) t) fields) )] in
          { x with typ = type_; default = default; }, d, MapString.add an ((true, is_single_record), (type_, ts)) map
        end
      | MTasset an, (Tset st) -> x, [], MapString.add an ((false, false), (tset st, tunit)) map
      | _ -> x, [], map
    in
    let nstorage, decls, map = List.fold_left (fun (accu, decls, map) x -> let a, ds, map = for_storage_item map x in (accu @ [a], decls @ ds, map)) ([], [], MapString.empty) model.storage in
    { model with
      storage = nstorage;
      decls = model.decls @ decls;
    }, map
  in


  let is_single_simple_record (map : ((bool * bool) * (type_ * type_)) MapString.t) an =
    MapString.find an map
    |> fst
  in

  let is_simple_record (map : ((bool * bool) * (type_ * type_)) MapString.t) an =
    is_single_simple_record map an |> snd
  in

  let get_type_for_asset_container (map : ((bool * bool) * (type_ * type_)) MapString.t) an =
    MapString.find an map
    |> snd |> fst
  in

  let get_type_for_asset_value (map : ((bool * bool) * (type_ * type_)) MapString.t) an =
    MapString.find an map
    |> snd |> snd
  in

  let is_key an fn =
    let kn, _ = Utils.get_asset_key model an in
    String.equal kn fn
  in

  let get_asset_global_id an = mk_mident (dumloc an) in

  let get_asset_global (map : ((bool * bool) * (type_ * type_)) MapString.t) an =
    let type_ = get_type_for_asset_container map an in
    let id = get_asset_global_id an in
    mk_mterm (Mvar (id, Vstorecol)) type_
  in

  let process_mterm (map : ((bool * bool) * (type_ * type_)) MapString.t) (model : model) : model =

    let is_single_simple_record an = is_single_simple_record map an in
    let is_simple_record an = is_simple_record map an in
    let get_type_for_asset_container an = get_type_for_asset_container map an in
    let get_asset_global an = get_asset_global map an in


    let get_contains_va (va : mterm) (k : mterm) : mterm =
      let node =
        match get_ntype va.type_ with
        | Tset kt                    -> Msetcontains (kt, va, k)
        | Tmap (kt, kv)              -> Mmapcontains (MKMap, kt, kv, va, k)
        | Tbig_map (kt, kv)          -> Mmapcontains (MKBigMap, kt, kv, va, k)
        | Titerable_big_map (kt, kv) -> Mmapcontains (MKIterableBigMap, kt, kv, va, k)
        | _ -> assert false
      in
      mk_mterm node tbool
    in

    (* let get_asset_key_type x = Utils.get_asset_key model x |> snd in *)

    let extract_key x = Utils.extract_key_value_from_masset model x in

    let extract_key_value (v : mterm) : mterm * mterm * (ident * mterm) list * (ident * mterm * mterm) list =
      match v with
      | {node = (Masset l); type_ = (Tasset an, _) } ->
        let an = unloc_mident an in
        let asset : asset = Utils.get_asset model an in
        let asset_key = match asset.keys with [k] -> k | _ -> assert false in
        let assoc_fields : (ident * type_ * mterm) list = List.map2 (fun (ai : asset_item) (x : mterm) -> (unloc_mident ai.name, ai.type_, x)) asset.values l in
        let k, l, ags, pts = List.fold_left (fun (sk, sv, ags, pts) (x : ident * type_ * mterm) ->
            let id, t, v = x in
            if String.equal asset_key id
            then Some v, sv, ags, pts
            else begin
              let to_litset an l =
                let _, kt = Utils.get_asset_key model (unloc_mident an) in
                let set_t = tset kt in
                List.fold_left (fun accu x -> mk_mterm (Msetadd(kt, accu, x)) set_t) (mk_mterm (Mlitset []) set_t) l
              in
              let v, nags, npts =
                match v.node, get_ntype t with
                | Mlitlist l, Tcontainer ((Tasset an, _), Aggregate) -> begin
                    to_litset an l, List.map (fun x -> unloc_mident an, x) l, []
                  end
                | Mlitset l, Tcontainer ((Tasset an, _), Partition) -> begin
                    to_litset an (List.map extract_key l), [], List.map (fun x -> unloc_mident an, extract_key x, x) l
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
          | _ -> mk_mterm (Mlitrecord l) (trecord (mk_mident (dumloc an)))
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
          | Tcontainer ((Tasset an, _), Partition) -> (unloc_mident x.name, unloc_mident an)::accu
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
                | Tmap (kt, vt) ->
                  mk_mterm (Mmapput (MKMap, kt, vt, accu, k, v)) va.type_
                | Tbig_map (kt, vt) ->
                  mk_mterm (Mmapput (MKBigMap, kt, vt, accu, k, v)) va.type_
                | Titerable_big_map (kt, vt) ->
                  mk_mterm (Mmapput (MKIterableBigMap, kt, vt, accu, k, v)) va.type_
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
        | Tset tk           -> Msetcontains (tk, va, x)
        | Tmap (tk, tv)     -> Mmapcontains (MKMap, tk, tv, va, x)
        | Tbig_map (tk, tv) -> Mmapcontains (MKBigMap, tk, tv, va, x)
        | Titerable_big_map (tk, tv) -> Mmapcontains (MKIterableBigMap, tk, tv, va, x)
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
          | ((Tmap (kt, kv) | Tbig_map (kt, kv) | Titerable_big_map (kt, kv)) as bmap) -> begin
              let mkm = match bmap with | Tmap _ -> MKMap | Tbig_map _ -> MKBigMap | Titerable_big_map _ -> MKIterableBigMap | _ -> assert false in
              let a = mk_mterm (Mmapput (mkm, kt, kv, va, k, v)) va.type_ in
              let b = mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), a)) tunit in
              match ags, pts with
              | [], [] -> b
              | (an, a)::t, [] -> begin
                  let f = create_contains_asset_key in
                  let c = List.fold_left (fun accu (an, x) -> mk_mterm (Mand (f an x, accu)) tbool) (f an a) t in
                  mk_mterm (Mif (c, b, Some (failc NotFound))) tunit
                end
              | [], (an, k, _)::t -> begin
                  let f = create_contains_asset_key in
                  let c = List.fold_left (fun accu (an, x, _) -> mk_mterm (Mor (f an x, accu)) tbool) (f an k) t in
                  let linstrs = get_instrs_add_with_partition pts in
                  let seq = mk_mterm (Mseq (b::linstrs)) tunit in
                  mk_mterm (Mif (c, failc (KeyExists an), Some seq)) tunit
                end
              | (aan, aa)::at, pt ->
                let f = create_contains_asset_key in
                let c = List.fold_left (fun accu (an, x) -> mk_mterm (Mand (f an x, accu)) tbool) (f aan aa) at in
                let c = List.fold_left (fun accu (an, x, _) -> mk_mterm (Mand (mnot (f an x), accu)) tbool) c pt in
                let linstrs = get_instrs_add_with_partition pts in
                let seq = mk_mterm (Mseq (b::linstrs)) tunit in
                mk_mterm (Mif (c, seq, Some (failc (KeyExistsOrNotFound aan)))) tunit
            end
          | _ -> assert false
        in

        if force
        then assign
        else mk_mterm (Mif (cond, failc (KeyExists an), Some assign)) tunit
      end
    in

    let remove_asset f an k =
      let k = f k in
      let va = get_asset_global an in

      let new_value =
        let node =
          match get_ntype va.type_ with
          | Tset kt           -> Msetremove (kt, va, k)
          | Tmap (kt, kv)     -> Mmapremove (MKMap, kt, kv, va, k)
          | Tbig_map (kt, kv) -> Mmapremove (MKBigMap, kt, kv, va, k)
          | Titerable_big_map (kt, kv) -> Mmapremove (MKIterableBigMap, kt, kv, va, k)
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
              let viter_id = mk_mident (dumloc viter_name) in
              let pk = Utils.get_asset_key model aan |> snd in
              let viter    : mterm = mk_mterm (Mvar (viter_id, Vstorecol)) pk in
              let set      : mterm = mk_mterm (Mdot((mk_mterm (Mget(an, CKcoll, k)) (tasset (mk_mident (dumloc an)))), mk_mident (dumloc fn))) pk |> f in
              let passign2 : mterm = mk_mterm (Mremoveasset (aan, viter)) tunit |> f in
              mk_mterm (Mfor (FIsimple viter_id, ICKset set, passign2)) tunit
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
        | Tmap (kt, vt)
        | Tbig_map (kt, vt) -> kt, vt
        | Titerable_big_map (kt, vt) -> kt, vt
        | _ -> assert false
      in

      let _, is_record = is_single_simple_record an in
      let aan, c = Utils.get_field_container model an fn in
      let atk = Utils.get_asset_key model aan |> snd in
      let aasset = Utils.get_asset model an in

      let mk_assign bk =
        let ts = tset atk in
        let remove_set set = mk_mterm (Msetremove (atk, set, bk)) ts in
        let get_ t = mk_mterm (Mmapget(aasset.map_kind, kt, vt, va, ak, Some an)) t in
        let v : mterm =
          if is_record
          then begin
            remove_set (get_ ts)
          end
          else begin
            let tr = trecord (mk_mident (dumloc an)) in
            let get : mterm = get_ tr in
            let set : mterm = mk_mterm (Mdot(get, mk_mident (dumloc fn))) (tset atk) in
            mk_mterm (Mrecupdate(get, [fn, remove_set set])) tr
          end
        in
        let nmap : mterm = mk_mterm (Mmapput (aasset.map_kind, kt, vt, va, ak, v) ) va.type_ in
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
      let aasset = Utils.get_asset model an in

      match ck with
      | CKcoll -> begin
          let va = get_asset_global an in
          match get_ntype va.type_ with
          | Tset skt -> begin

              let iid = mk_mident (dumloc "_sid") in
              let vid = mk_mterm (Mvar (iid, Vlocal)) skt in

              let iaccu = mk_mident (dumloc "_accu") in
              let vaccu = mk_mterm (Mvar (iaccu, Vlocal)) tr in

              let act = mk vid None vaccu in
              mk_mterm (Msetfold(atk, iid, iaccu, va, init, act)) tr
            end
          | Tmap (mkt, mkv)
          | Tbig_map (mkt, mkv)
          | Titerable_big_map (mkt, mkv) -> begin

              let ikid = mk_mident (dumloc "_kid") in
              let vkid = mk_mterm (Mvar (ikid, Vlocal)) mkt in

              let ivid = mk_mident (dumloc "_vid") in
              let vvid = mk_mterm (Mvar (ivid, Vlocal)) mkv in

              let iaccu = mk_mident (dumloc "_accu") in
              let vaccu = mk_mterm (Mvar (iaccu, Vlocal)) tr in

              let act = mk vkid (Some vvid) vaccu in
              mk_mterm (Mmapfold(aasset.map_kind, atk, ikid, ivid, iaccu, va, init, act)) tr
            end
          | _ -> assert false
        end
      | CKfield (an, fn, k) -> begin
          let va = get_asset_global an in
          let get =
            match get_ntype va.type_ with
            | Tmap (kt, vt) -> mk_mterm (Mmapget (MKMap, kt, vt, va, f k, Some an)) vt
            | Tbig_map (kt, vt) -> mk_mterm (Mmapget (MKBigMap, kt, vt, va, f k, Some an)) vt
            | Titerable_big_map (kt, vt) -> mk_mterm (Mmapget (MKIterableBigMap, kt, vt, va, f k, Some an)) vt
            | _ -> assert false
          in

          let aan, _ = Utils.get_field_container model an fn in
          let _ , aatk = Utils.get_asset_key model aan in

          let set =
            let _, is_record = is_single_simple_record an in
            if is_record
            then get
            else mk_mterm (Mdot(get, mk_mident (dumloc fn))) (tset aatk)
          in

          let iid = mk_mident (dumloc "_sid") in
          let vid = mk_mterm (Mvar (iid, Vlocal)) aatk in

          let iaccu = mk_mident (dumloc "_accu") in
          let vaccu = mk_mterm (Mvar (iaccu, Vlocal)) tr in

          let vaa = get_asset_global aan in
          let geta =
            match get_ntype vaa.type_ with
            | Tmap (kt, vt)     -> Some (mk_mterm (Mmapget (MKMap, kt, vt, vaa, vid, Some an)) vt)
            | Tbig_map (kt, vt) -> Some (mk_mterm (Mmapget (MKBigMap, kt, vt, vaa, vid, Some an)) vt)
            | Titerable_big_map (kt, vt) -> Some (mk_mterm (Mmapget (MKIterableBigMap, kt, vt, vaa, vid, Some an)) vt)
            | _ -> None
          in

          let body =
            match geta with
            | Some g -> begin
                let ival = mk_mident (dumloc "_v") in
                let vval = mk_mterm (Mvar(ival, Vlocal)) g.type_ in

                if with_value
                then let act : mterm = mk vid (Some vval) vaccu in mk_mterm (Mletin([ival], LVsimple g, Some g.type_, act, None)) tr
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

          let iid = mk_mident (dumloc "_sid") in
          let vid = mk_mterm (Mvar (iid, Vlocal)) atk in

          let iaccu = mk_mident (dumloc "_accu") in
          let vaccu = mk_mterm (Mvar (iaccu, Vlocal)) tr in

          let vaa = get_asset_global an in
          let geta =
            match get_ntype vaa.type_ with
            | Tmap (kt, vt) -> Some (mk_mterm (Mmapget (MKMap, kt, vt, vaa, vid, Some an)) vt)
            | Tbig_map (kt, vt) -> Some (mk_mterm (Mmapget (MKBigMap, kt, vt, vaa, vid, Some an)) vt)
            | Titerable_big_map (kt, vt) -> Some (mk_mterm (Mmapget (MKIterableBigMap, kt, vt, vaa, vid, Some an)) vt)
            | _ -> None
          in

          let body =
            match geta with
            | Some g -> begin
                let ival = mk_mident (dumloc "_v") in
                let vval = mk_mterm (Mvar(ival, Vlocal)) g.type_ in

                if with_value
                then let act = mk vid (Some vval) vaccu in mk_mterm (Mletin([ival], LVsimple g, Some g.type_, act, None)) tr
                else mk vid None vaccu
              end
            | None -> begin
                let act = mk vid None vaccu in
                act
              end
          in

          mk_mterm (Mlistfold(atk, iid, iaccu, l, init, body)) tr
        end
    in

    let rec fm ctx (mt : mterm) : mterm =
      match mt.node with

      (* access *)

      | Mdot (({node = _; type_ = (Tasset an, _)} as a), _)
        when Utils.is_asset_single_field model (unloc_mident an) && Utils.is_asset_map model (unloc_mident an) -> fm ctx a
      | Mdot ({node = Mget (_, CKcoll, k); type_ = (Tasset an, _)}, fn) when is_key (unloc_mident an) (unloc_mident fn) -> begin
          fm ctx k
        end
      | Mdot (({node = _; type_ = ((Tasset an | (Tcontainer ((Tasset an, _), AssetValue))), _)} as a), fn) -> begin
          let an = unloc_mident an in
          let mt_get = fm ctx a in
          if is_simple_record an
          then mt_get
          else
            mk_mterm (Mdot({mt_get with type_ = trecord (mk_mident (dumloc an))}, fn)) mt.type_
        end
      | Mquestionoption ({node = Mget (an, c, k); _}, fn) -> begin
          let otyp = mt.type_ in
          let typ = match get_ntype otyp with | Toption v -> v | _ -> assert false in

          let mt = mk_mterm (Mgetsome(an, c, k)) (toption (tassetvalue (mk_mident (dumloc an)))) |> fm ctx in
          let id = mk_mident (dumloc "_q_opt") in
          let vid = mk_mvar id (tassetvalue (mk_mident (dumloc an))) in
          let dt = mk_mterm (Mdot (vid, fn)) typ in
          let vt = mk_some dt |> fm ctx in
          let nonevalue = mk_mterm Mnone otyp in
          mk_mterm (Mmatchoption(mt, id, vt, nonevalue)) otyp
        end

      | Mget (an, CKcoll, k) when Utils.is_asset_single_field model an && Utils.is_asset_map model an -> fm ctx k

      | Mget (an, CKcoll, k) -> begin
          let k = fm ctx k in
          let va = get_asset_global an in

          let mkm, kt, vt =
            match get_ntype va.type_ with
            | Tmap (kt, vt) -> MKMap, kt, vt
            | Tbig_map (kt, vt) -> MKBigMap, kt, vt
            | Titerable_big_map (kt, vt) -> MKIterableBigMap, kt, vt
            | _ -> assert false
          in
          let map_get = Mmapget (mkm, kt, vt, va, k, Some an) in
          mk_mterm map_get vt
        end

      | Mget (an, CKview c, k) -> begin
          let get = mk_mterm (Mget(an, CKcoll, k)) mt.type_ |> fm ctx in
          let cond = mk_mterm (Mcontains(an, CKview c, k)) tbool |> fm ctx in
          mk_mterm (Mexprif(cond, get, failc (AssetNotFound an))) get.type_
        end

      | Mgetsome (an, _, k) -> begin
          let k = fm ctx k in
          let va = get_asset_global an in

          let mkm, kt, vt =
            match get_ntype va.type_ with
            | Tmap (kt, vt) -> MKMap, kt, vt
            | Tbig_map (kt, vt) -> MKBigMap, kt, vt
            | Titerable_big_map (kt, vt) -> MKIterableBigMap, kt, vt
            | _ -> assert false
          in
          let map_get_opt = Mmapgetopt (mkm, kt, vt, va, k) in
          mk_mterm map_get_opt (toption vt)
        end

      (* control *)

      | Mfor (FIsimple id, ICKcoll an, b) -> begin
          let b = fm ctx b in
          let va = get_asset_global an in
          let node =
            match get_ntype va.type_ with
            | Tset _ -> Mfor (FIsimple id, ICKset va, b)
            | Tmap _ -> Mfor (FIdouble (id, mk_mident (dumloc "_v")), ICKmap va, b)
            | Titerable_big_map _ -> Mfor (FIdouble (id, mk_mident (dumloc "_v")), ICKmap va, b)
            | _ -> assert false
          in
          { mt with node = node }
        end

      | Mfor (FIsimple id, ICKfield (_, _, c), b) -> begin
          let b = fm ctx b in
          let c = fm ctx c in
          let node = Mfor (FIsimple id, ICKset c, b) in
          { mt with node = node }
        end

      | Mfor (FIsimple id, ICKview v, b) -> begin
          let b = fm ctx b in
          let v = fm ctx v in
          let node = Mfor (FIsimple id, ICKlist v, b) in
          { mt with node = node }
        end

      (* effect *)

      | Maddasset (an, v) -> add_asset (fm ctx) an v

      | Mputsingleasset (an, v) -> add_asset (fm ctx) an v ~force:true

      | Maddfield (an, fn, ak, b) -> begin
          let ak = fm ctx ak in

          let va = get_asset_global an in
          let mkm, kt, vt =
            match get_ntype va.type_ with
            | Tmap (kt, vt) -> MKMap, kt, vt
            | Tbig_map (kt, vt) -> MKBigMap, kt, vt
            | Titerable_big_map (kt, vt) -> MKIterableBigMap, kt, vt
            | _ -> assert false
          in

          let _, is_record = is_single_simple_record an in
          let aan, c = Utils.get_field_container model an fn in
          let atk = Utils.get_asset_key model aan |> snd in

          let mk_assign bk =
            let ts = tset atk in
            let add_set set = mk_mterm (Msetadd (atk, set, bk)) ts in
            let get_ t = mk_mterm (Mmapget(mkm, kt, vt, va, ak, Some an)) t in
            let v : mterm =
              if is_record
              then begin
                add_set (get_ ts)
              end
              else begin
                let tr = trecord (mk_mident (dumloc an)) in
                let get : mterm = get_ tr in
                let set : mterm = mk_mterm (Mdot(get, mk_mident (dumloc fn))) (tset atk) in
                mk_mterm (Mrecupdate(get, [fn, add_set set])) tr
              end
            in
            let nmap : mterm = mk_mterm (Mmapput (mkm, kt, vt, va, ak, v) ) va.type_ in
            mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), nmap)) tunit
          in

          match c with
          | Aggregate ->
            let bk = fm ctx b in
            let assign = mk_assign bk in
            let cond = create_contains_asset_key aan bk in
            mk_mterm (Mif (cond, assign, Some (failc NotFound))) tunit
          | Partition ->
            let bk = extract_key b in
            let bk = fm ctx bk in
            let assign = mk_assign bk in
            mk_mterm (Mseq [add_asset (fm ctx) aan b; assign]) tunit
          | _ -> assert false
        end

      | Mremoveasset (an, k) -> remove_asset (fm ctx) an k

      | Mremovefield (an, fn, ak, b) -> remove_field (fm ctx) (an, fn, ak, b)

      | Mremoveall (an, ck) -> begin
          match ck with
          | CKcoll -> begin
              let va = get_asset_global an in
              let _, is_record = is_single_simple_record an in
              let empty =
                let node =
                  match get_ntype va.type_ with
                  | Tset _ -> Mlitset []
                  | Tmap (_, _) -> Mlitmap (MKMap, [])
                  | Tbig_map (_, _) -> Mlitmap (MKBigMap, [])
                  | Titerable_big_map (_, _) -> Mlitmap (MKIterableBigMap, [])
                  | _ -> assert false
                in
                mk_mterm node va.type_
              in

              let mk_loop fn aan =
                let tv =
                  match get_ntype va.type_ with
                  | Tmap (_, tv)
                  | Tbig_map (_, tv) -> tv
                  | Titerable_big_map (_, tv) -> tv
                  | _ -> assert false
                in
                let var_id = mk_mident (dumloc "_v") in
                let var_value : mterm = mk_mterm (Mvar (var_id, Vlocal)) tv in
                let atk = Utils.get_asset_key model aan |> snd in
                let set =
                  if is_record
                  then var_value
                  else mk_mterm (Mdot (var_value, mk_mident (dumloc fn))) (tset atk)
                in
                let loop : mterm =
                  let var_id = mk_mident (dumloc "_ak") in
                  let var_value = mk_mvar var_id atk in
                  let b : mterm = remove_asset (fm ctx) aan var_value in
                  mk_mterm (Mfor (FIsimple var_id, ICKset set, b)) tunit
                in
                mk_mterm (Mfor (FIdouble (mk_mident (dumloc "_k"), var_id), ICKmap va, loop)) tunit
              in

              let assign = mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), empty)) tunit in
              let partitions = get_partitions an in
              match partitions with
              | [] -> assign
              | _ -> mk_mterm (Mseq ((List.map (fun (fn, aan) -> mk_loop fn aan) partitions) @ [assign])) tunit
            end
          | CKfield (_, fn, k) -> begin
              let kk = fm ctx k in

              let va = get_asset_global an in
              let mkm, kt, vt =
                match get_ntype va.type_ with
                | Tmap (kt, vt) -> MKMap, kt, vt
                | Tbig_map (kt, vt) -> MKBigMap, kt, vt
                | Titerable_big_map (kt, vt) -> MKIterableBigMap, kt, vt
                | _ -> assert false
              in

              let _, is_record = is_single_simple_record an in
              let aan, c = Utils.get_field_container model an fn in
              let atk = Utils.get_asset_key model aan |> snd in


              let get_ t = mk_mterm (Mmapget(mkm, kt, vt, va, kk, Some an)) t in

              let mk_loop _ =
                let iter_var = mk_mident (dumloc "_iter_var") in
                let ivar = mk_mterm (Mvar(iter_var, Vlocal)) atk in

                let body = remove_asset (fm ctx) aan ivar in

                let set =
                  let ts = tset atk in
                  if is_record
                  then get_ ts
                  else begin
                    let tr = trecord (mk_mident (dumloc an)) in
                    let get : mterm = get_ tr in
                    mk_mterm (Mdot(get, mk_mident (dumloc fn))) ts
                  end
                in

                mk_mterm (Mfor (FIsimple iter_var, ICKset set, body)) tunit
              in

              let mk_assign _ =
                let ts = tset atk in
                let empty = mk_mterm (Mlitset []) ts in
                let v : mterm =
                  if is_record
                  then empty
                  else begin
                    let tr = trecord (mk_mident (dumloc an)) in
                    let get : mterm = get_ tr in
                    mk_mterm (Mrecupdate(get, [fn, empty])) tr
                  end
                in
                let nmap : mterm = mk_mterm (Mmapput (mkm, kt, vt, va, kk, v) ) va.type_ in
                mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), nmap)) tunit
              in

              let assign = mk_assign () in

              match c with
              | Aggregate -> assign
              | Partition -> mk_mterm (Mseq [mk_loop (); assign]) tunit
              | _ -> assert false
            end
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
              | Mdot ({node = Mvar ((_, {pldesc = "the"}), _); _}, fn) when String.equal (unloc_mident fn) akn -> vkey
              | Mdot ({node = Mvar ((_, {pldesc = "the"}), _); _}, fn) -> get_val an (Option.get vval) fn mt.type_
              | _ -> map_mterm aux mt
            in
            aux x
          in

          match ck with
          | CKcoll -> begin
              let va = get_asset_global an in
              match get_ntype va.type_ with
              | Tset kt -> begin
                  let ikey = mk_mident (dumloc "_k") in
                  let vkey = mk_mterm (Mvar(ikey, Vlocal)) kt in

                  let cond = fm ctx (mk_cond an vkey None b) in
                  let remove = remove_asset (fm ctx) an vkey in
                  let body = mk_mterm (Mif (cond, remove, None)) tunit in
                  let loop = mk_mterm (Mfor(FIsimple ikey, ICKset va, body) ) tunit in
                  loop
                end
              | Tmap (kt, vt)
              | Tbig_map (kt, vt)
              | Titerable_big_map (kt, vt) -> begin
                  let ikey = mk_mident (dumloc "_k") in
                  let vkey = mk_mterm (Mvar(ikey, Vlocal)) kt in

                  let ival = mk_mident (dumloc "_v") in
                  let vval = mk_mterm (Mvar(ival, Vlocal)) vt in

                  let cond = fm ctx (mk_cond an vkey (Some vval) b) in
                  let remove = remove_asset (fm ctx) an vkey in
                  let body = mk_mterm (Mif (cond, remove, None)) tunit in
                  let loop = mk_mterm (Mfor(FIdouble (ikey, ival), ICKmap va, body) ) tunit in
                  loop
                end
              | _ -> assert false
            end

          | CKfield (an, fn, k) -> begin
              let va = get_asset_global an in
              let get =
                match get_ntype va.type_ with
                | Tmap (kt, vt) -> mk_mterm (Mmapget (MKMap, kt, vt, va, fm ctx k, Some an)) vt
                | Tbig_map (kt, vt) -> mk_mterm (Mmapget (MKBigMap, kt, vt, va, fm ctx k, Some an)) vt
                | Titerable_big_map (kt, vt) -> mk_mterm (Mmapget (MKIterableBigMap, kt, vt, va, fm ctx k, Some an)) vt
                | _ -> assert false
              in

              let aan, _ = Utils.get_field_container model an fn in
              let _ , aatk = Utils.get_asset_key model aan in

              let set =
                let _, is_record = is_single_simple_record an in
                if is_record
                then get
                else mk_mterm (Mdot(get, mk_mident (dumloc fn))) (tset aatk)
              in

              let ikey = mk_mident (dumloc "_k") in
              let vkey = mk_mterm (Mvar(ikey, Vlocal)) aatk in


              let vaa = get_asset_global aan in
              let geta =
                match get_ntype vaa.type_ with
                | Tmap (kt, vt) -> Some (mk_mterm (Mmapget (MKMap, kt, vt, vaa, vkey, Some an)) vt)
                | Tbig_map (kt, vt) -> Some (mk_mterm (Mmapget (MKBigMap, kt, vt, vaa, vkey, Some an)) vt)
                | Titerable_big_map (kt, vt) -> Some (mk_mterm (Mmapget (MKIterableBigMap, kt, vt, vaa, vkey, Some an)) vt)
                | _ -> None
              in

              let body =
                match geta with
                | Some g -> begin
                    let ival = mk_mident (dumloc "_v") in
                    let vval = mk_mterm (Mvar(ival, Vlocal)) g.type_ in

                    let cond = fm ctx (mk_cond aan vkey (Some vval) b) in
                    let remove = remove_field (fm ctx) (an, fn, k, vkey) in
                    let body = mk_mterm (Mif (cond, remove, None)) tunit in
                    mk_mterm (Mletin([ival], LVsimple g, Some g.type_, body, None)) tunit
                  end
                | None -> begin
                    let cond = fm ctx (mk_cond aan vkey None b) in
                    let remove = remove_field (fm ctx) (an, fn, k, vkey) in
                    mk_mterm (Mif (cond, remove, None)) tunit
                  end
              in

              let loop = mk_mterm (Mfor(FIsimple ikey, ICKset set, body) ) tunit in
              loop
            end
          | _ -> assert false
        end

      | Mclear (an, ck) -> begin
          match ck with
          | CKcoll -> begin
              let va = get_asset_global an in
              let _, is_record = is_single_simple_record an in
              let empty =
                let node =
                  match get_ntype va.type_ with
                  | Tset _ -> Mlitset []
                  | Tmap (_, _) -> Mlitmap (MKMap, [])
                  | Tbig_map (_, _) -> Mlitmap (MKBigMap, [])
                  | Titerable_big_map (_, _) -> Mlitmap (MKIterableBigMap, [])
                  | _ -> assert false
                in
                mk_mterm node va.type_
              in

              let mk_loop fn aan =
                let tv =
                  match get_ntype va.type_ with
                  | Tmap (_, tv)
                  | Tbig_map (_, tv) -> tv
                  | Titerable_big_map (_, tv) -> tv
                  | _ -> assert false
                in
                let var_id = mk_mident (dumloc "_v") in
                let var_value : mterm = mk_mterm (Mvar (var_id, Vlocal)) tv in
                let atk = Utils.get_asset_key model aan |> snd in
                let set =
                  if is_record
                  then var_value
                  else mk_mterm (Mdot (var_value, mk_mident (dumloc fn))) (tset atk)
                in
                let loop : mterm =
                  let var_id = mk_mident (dumloc "_ak") in
                  let var_value = mk_mvar var_id atk in
                  let b : mterm = remove_asset (fm ctx) aan var_value in
                  mk_mterm (Mfor (FIsimple var_id, ICKset set, b)) tunit
                in
                mk_mterm (Mfor (FIdouble (mk_mident (dumloc "_k"), var_id), ICKmap va, loop)) tunit
              in

              let assign = mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), empty)) tunit in
              let partitions = get_partitions an in
              match partitions with
              | [] -> assign
              | _ -> mk_mterm (Mseq ((List.map (fun (fn, aan) -> mk_loop fn aan) partitions) @ [assign])) tunit
            end
          | CKfield (an, fn, k) -> begin
              let kk = fm ctx k in

              let va = get_asset_global an in
              let _, is_record = is_single_simple_record an in

              let aan, _ = Utils.get_field_container model an fn in
              let atk = Utils.get_asset_key model aan |> snd in

              let mkm, tk, tv =
                match get_ntype va.type_ with
                | Tmap (tk, tv) -> MKMap, tk, tv
                | Tbig_map (tk, tv) -> MKBigMap, tk, tv
                | Titerable_big_map (tk, tv) -> MKIterableBigMap, tk, tv
                | _ -> assert false
              in

              let set =
                let get = mk_mterm (Mmapget (mkm, tk, tv, va, kk, Some an)) tv in
                if is_record
                then get
                else mk_mterm (Mdot(get, mk_mident (dumloc fn))) (tset atk)
              in

              let var_id = mk_mident (dumloc "_ak") in
              let var_value = mk_mterm (Mvar (var_id, Vlocal)) atk in
              let b : mterm = remove_asset (fm ctx) aan var_value in
              let loop = mk_mterm (Mfor (FIsimple var_id, ICKset set, b)) tunit in
              let assign = fm ctx (mk_mterm (Mremoveall (an, CKfield(aan, fn, k))) tunit) in
              mk_mterm (Mseq [loop; assign]) tunit
            end
          | CKview l -> begin
              let tk = Utils.get_asset_key model an |> snd in

              let l = fm ctx l in

              let var_id = mk_mident (dumloc "_ak") in
              let var_value = mk_mterm (Mvar (var_id, Vlocal)) tk in

              let b : mterm = remove_asset (fm ctx) an var_value in

              mk_mterm (Mfor (FIsimple var_id, ICKlist l, b)) tunit
            end
        end

      | Mupdate (an, k, l) -> begin
          let k = fm ctx k in

          let va = get_asset_global an in

          let _, is_record = is_single_simple_record an in

          let mkm, kt, tasset =
            match get_ntype (get_type_for_asset_container an) with
            | Tmap (kt, vt) -> MKMap, kt, vt
            | Tbig_map (kt, vt) -> MKBigMap, kt, vt
            | Titerable_big_map (kt, vt) -> MKIterableBigMap, kt, vt
            | _ -> assert false
          in

          let var_id = mk_mident (dumloc "_asset") in

          let mk_letin x =
            let get = mk_mterm (Mmapget(mkm, kt, tasset, va, k, Some an)) tasset in
            mk_mterm (Mletin ([var_id], LVsimple get, Some tasset, x, None)) tunit
          in

          let get_val (id : mident) =
            let var = mk_mterm (Mvar(var_id, Vlocal)) tasset in
            if is_record
            then var
            else begin
              let _, ts, _ = Utils.get_asset_field model (an, unloc_mident id) in
              mk_mterm (Mdot(var, id)) ts
            end
          in

          let add_val id x = mk_mterm (Mplus   (get_val id, x)) x.type_ in

          let sub_val (id : mident) (x : mterm) =
            let _, t, _ = Utils.get_asset_field model (an, unloc_mident id) in
            match get_ntype t with
            |  Tbuiltin Bnat -> begin
                let a = mk_mterm (Mminus (get_val id, x)) tint in
                let zero = mk_mterm (Mint Big_int.zero_big_int) tint in
                let cond = mk_mterm (Mge (a, zero)) tbool in
                let v = mk_mterm (Mabs a) tnat in
                let f = mk_mterm (Mfail NatNegAssign) (tunit) in
                let c = mk_mterm (Mcast (tunit, tnat, f)) tnat in
                mk_mterm (Mexprif (cond, v, c)) tnat
              end
            | _ ->  mk_mterm (Mminus  (get_val id, x)) x.type_
          in

          let arith_rval op id x = mk_mterm (Mratarith (op, get_val id, x)) x.type_ in

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
              let _, ts, _ = Utils.get_asset_field model (an, unloc_mident id) in

              let v =
                let rec aux (mt : mterm) : mterm =
                  match mt.node with
                  (* | Mdot ({node = Mvar ({pldesc = "the"}, _, _, _); _}, fn) when String.equal (unloc fn) akn -> vkey *)
                  | Mdot ({node = Mvar ((_, {pldesc = "the"}), _); _}, fn) -> get_val fn
                  | _ -> map_mterm aux mt
                in
                aux v
              in

              let v = fm ctx v in

              let is_rat t = match get_ntype t with | Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)] -> true     | _ -> false in
              match get_ntype ts, op with
              | Tcontainer((Tasset aan, _), Aggregate), ValueAssign -> begin
                  let aan = unloc_mident aan in
                  let _, tk = Utils.get_asset_key model aan in
                  let ll = List.map (fm ctx) (extract_listlit v) in
                  let set = mk_mterm (Mlitset ll) (tset tk) in
                  (id, ValueAssign, set)::accu, b, (unloc_mident id, aan, `Aggregate, `Replace, ll)::ags
                end

              | Tcontainer((Tasset aan, _), Aggregate), PlusAssign -> begin
                  let aan = unloc_mident aan in
                  let _, tk = Utils.get_asset_key model aan in
                  let ts = tset tk in
                  let ll = List.map (fm ctx) (extract_listlit v) in
                  let get : mterm = get_val id in
                  let set = List.fold_left (fun accu (x : mterm) -> mk_mterm (Msetadd (tk, accu, x)) ts) get ll in
                  (id, ValueAssign, set)::accu, true, (unloc_mident id, aan, `Aggregate, `Add, ll)::ags
                end

              | Tcontainer((Tasset aan, _), Aggregate), MinusAssign  ->  begin
                  let aan = unloc_mident aan in
                  let _, tk = Utils.get_asset_key model aan in
                  let ts = tset tk in
                  let ll = List.map (fm ctx) (extract_listlit v) in
                  let get : mterm = get_val id in
                  let set = List.fold_left (fun accu (x : mterm) -> mk_mterm (Msetremove (tk, accu, x)) ts) get ll in
                  (id, ValueAssign, set)::accu, true, (unloc_mident id, aan, `Aggregate, `Remove, ll)::ags
                end

              | Tcontainer((Tasset aan, _), Partition), ValueAssign -> begin
                  let aan = unloc_mident aan in
                  let _, tk = Utils.get_asset_key model aan in
                  let ll =  extract_litset v in
                  let lll = List.map (fm ctx) ll in
                  let lkeys = List.map (extract_key |@ (fm ctx)) ll in
                  let set = mk_mterm (Mlitset lkeys) (tset tk) in
                  (id, ValueAssign, set)::accu, b, (unloc_mident id, aan, `Partition, `Replace, lll)::ags
                end

              | Tcontainer((Tasset aan, _), Partition), PlusAssign -> begin
                  let aan = unloc_mident aan in
                  let _, tk = Utils.get_asset_key model aan in
                  let ts = tset tk in
                  let ll =  extract_litset v in
                  let lll = List.map (fm ctx) ll in
                  let lkeys = List.map (extract_key |@ (fm ctx)) ll in
                  let get : mterm = get_val id in
                  let set = List.fold_left (fun accu (x : mterm) -> mk_mterm (Msetadd (tk, accu, x)) ts) get lkeys in
                  (id, ValueAssign, set)::accu, true, (unloc_mident id, aan, `Partition, `Add, lll)::ags
                end

              | Tcontainer((Tasset aan, _), Partition), MinusAssign  ->  begin
                  let aan = unloc_mident aan in
                  let _, tk = Utils.get_asset_key model aan in
                  let ts = tset tk in
                  let ll = List.map (fm ctx) (extract_listlit v) in
                  let get : mterm = get_val id in
                  let set = List.fold_left (fun accu (x : mterm) -> mk_mterm (Msetremove (tk, accu, x)) ts) get ll in
                  (id, ValueAssign, set)::accu, true, (unloc_mident id, aan, `Partition, `Remove, ll)::ags
                end

              | _, PlusAssign  when is_rat ts  -> (id, ValueAssign, arith_rval Rplus  id v)::accu , true, ags
              | _, MinusAssign when is_rat ts  -> (id, ValueAssign, arith_rval Rminus id v)::accu , true, ags
              | _, MultAssign  when is_rat ts  -> (id, ValueAssign, arith_rval Rmult  id v)::accu , true, ags
              | _, DivAssign   when is_rat ts  -> (id, ValueAssign, arith_rval Rdiv   id v)::accu , true, ags

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
              let get : mterm = mk_mterm (Mmapget(mkm, kt, tasset, va, k, Some an)) tasset in
              let ll = List.map (fun (id, op, v) ->
                  match op with
                  | ValueAssign -> begin
                      let v = fm ctx v in
                      unloc_mident id, v
                    end
                  | _ -> assert false
                ) l in
              mk_mterm (Mrecupdate(get, ll)) tasset
            end
          in

          let nmap = mk_mterm (Mmapput (mkm, kt, tasset, va, k, v)) va.type_ in

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
                        | Tset kt           -> Msetadd (kt, c, a)
                        | Tmap (kt, kv)     -> Mmapput (MKMap, kt, kv, c, a, b)
                        | Tbig_map (kt, kv) -> Mmapput (MKBigMap, kt, kv, c, a, b)
                        | Titerable_big_map (kt, kv) -> Mmapput (MKIterableBigMap, kt, kv, c, a, b)
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
                        | Tset kt           -> Msetremove (kt, c, k)
                        | Tmap (kt, kv)     -> Mmapremove (MKMap, kt, kv, c, k)
                        | Tbig_map (kt, kv) -> Mmapremove (MKBigMap, kt, kv, c, k)
                        | Titerable_big_map (kt, kv) -> Mmapremove (MKIterableBigMap, kt, kv, c, k)
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
              | msg, ( _, _, `Aggregate, (`Add | `Replace), _) when match msg with (KeyExists _) -> true | _ -> false -> (KeyExistsOrNotFound an)
              | msg, ( _, _, `Partition, (`Add | `Replace), _) when match msg with NotFound  -> true | _ -> false -> (KeyExistsOrNotFound an)
              | _, ( _, _, `Aggregate, (`Add | `Replace), _) -> NotFound
              | _, ( _, _, `Partition, (`Add | `Replace), _) -> KeyExists an
              | _ -> accu
            ) (Invalid (mk_string "")) ags in

          let assign : mterm =
            match elts_cond with
            | [] -> assign
            | (b, an, mt)::l -> begin
                let mk_cond b an mt = let x = create_contains_asset_key an mt in if b then x else mk_mterm (Mnot(x)) tbool in
                let init : mterm = mk_cond b an mt in
                let cond : mterm = List.fold_left (fun accu (b, an, x) -> mk_mterm (Mand(accu, mk_cond b an x)) tbool ) init l in
                mk_mterm (Mif (cond, assign, Some (failc msg))) tunit
              end
          in
          assign
        end

      | Mputremove (an, _c, k, v) -> begin
          let k = fm ctx k in
          let v = fm ctx v in

          let va = get_asset_global an in
          let tmap = get_type_for_asset_container an in
          let container = mk_mterm (Mvar (mk_mident (dumloc an), Vstorevar)) tmap in

          let value =
            match get_ntype tmap with
            | Tset t -> begin
                let b = mk_mterm (Missome v) tbool in
                mk_mterm (Msetupdate(t, container, b, k)) tmap
              end
            | ((Tmap (kt, vt) | Tbig_map (kt, vt)) as t ) -> begin
                let mkm =
                  match t with
                  | Tmap _ -> MKMap
                  | Tbig_map _ -> MKBigMap
                  | _ -> assert false
                in
                mk_mterm (Mmapupdate(mkm, kt, vt, container, k, v)) tmap
              end
            | Titerable_big_map (_kt, _vt) -> (emit_error (mt.loc, NoPutRemoveForIterableBigMapAsset); raise (Error.Stop 5))
            | _ -> assert false
          in
          mk_mterm (Massign (ValueAssign, va.type_, Avarstore (get_asset_global_id an), value)) tunit
        end

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
                | Mdot ({node = Mvar ((_, {pldesc = "the"}), _); _}, fn) when String.equal (unloc_mident fn) akn -> vkey
                | Mdot ({node = Mvar ((_, {pldesc = "the"}), _); _}, fn) -> get_val an (Option.get vval) fn mt.type_
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
              | CKfield (an, fn, _) -> Utils.get_field_container model an fn |> fst |> Utils.get_asset_key model |> snd
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
              | CKfield (an, fn, _) -> Utils.get_field_container model an fn |> fst |> Utils.get_asset_key model |> snd
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
              | _, _, Some v    -> mk_mterm (Mdot(v, mk_mident (dumloc fn))) t
              | _ -> assert false
            in

            let iinit_0 = mk_some vkid in
            let iinit_1 = mk_mterm (Mlitlist []) tr in
            let iinit   = mk_tuple [iinit_0; iinit_1] in

            let ixins = mk_mident (dumloc "_x_insert") in
            let vxins = mk_mvar ixins atk in

            let iains = mk_mident (dumloc "_accu_insert") in
            let vains = mk_mvar iains iinit.type_ in

            let prepend x l = mk_mterm (Mlistprepend(atk, l, x)) tr in

            let insert : mterm =

              let ia0 = mk_mident (dumloc "_ia0") in
              let va0 : mterm = mk_mvar ia0 iinit_0.type_ in

              let ia1 = mk_mident (dumloc "_ia1") in
              let va1 : mterm = mk_mvar ia1 iinit_1.type_ in

              let ivb = mk_mident (dumloc "_b") in

              let va = get_asset_global an in
              let add_letin x =
                match get_ntype va.type_ with
                | Tset _ -> x
                | Tmap (kt, vt) ->
                  let mk_get x = mk_mterm (Mmapget (MKMap, kt, vt, va, x, Some an)) vt in
                  x |> mk_letin ivb (mk_get vxins)
                | Tbig_map (kt, vt) ->
                  let mk_get x = mk_mterm (Mmapget (MKBigMap, kt, vt, va, x, Some an)) vt in
                  x |> mk_letin ivb (mk_get vxins)
                | Titerable_big_map (kt, vt) ->
                  let mk_get x = mk_mterm (Mmapget (MKIterableBigMap, kt, vt, va, x, Some an)) vt in
                  x |> mk_letin ivb (mk_get vxins)
                | _ -> assert false
              in

              let vvb : mterm option =
                match get_ntype va.type_ with
                | Tset _ -> None
                | Tmap (_, vt)
                | Tbig_map (_, vt)
                | Titerable_big_map (_, vt) -> Some (mk_mvar ivb vt)
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

              let matchsome : mterm = mk_mterm (Mmatchoption (va0, mk_mident (dumloc ""), mif, neutral)) vains.type_ in

              matchsome
              |> mk_letin ia1 (mk_tupleaccess 1 vains)
              |> mk_letin ia0 (mk_tupleaccess 0 vains)
            in

            let iz0 = mk_mident (dumloc "_iz0") in
            let vz0 : mterm = mk_mvar iz0 iinit_0.type_ in

            let iz1 = mk_mident (dumloc "_iz1") in
            let vz1 : mterm = mk_mvar iz1 iinit_1.type_ in

            let a = mk_mterm (Mlistfold(atk, ixins, iains, vaccu, iinit, insert)) iinit.type_ in

            let i0 = mk_mident (dumloc "_i0") in
            let v0 : mterm = mk_mvar i0 a.type_ in

            let matchsome : mterm = mk_mterm (Mmatchoption (vz0, mk_mident (dumloc ""), prepend vkid vz1, vz1)) tr in

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
            | CKcoll -> begin
                let va = get_asset_global an in
                match get_ntype va.type_ with
                | Tset tk           -> Msetcontains (tk, va, k)
                | Tmap (tk, tv)     -> Mmapcontains (MKMap, tk, tv, va, k)
                | Tbig_map (tk, tv) -> Mmapcontains (MKBigMap, tk, tv, va, k)
                | Titerable_big_map (tk, tv) -> Mmapcontains (MKIterableBigMap, tk, tv, va, k)
                | _ -> assert false
              end
            | CKview v -> begin
                let tk = Utils.get_asset_key model an |> snd in
                let v = fm ctx v in
                Mlistcontains (tk, v, k)
              end
            | CKfield (an, fn, kk) -> begin
                let kk = fm ctx kk in

                let va = get_asset_global an in

                let _, is_record = is_single_simple_record an in
                let aan, _ = Utils.get_field_container model an fn in
                let atk = Utils.get_asset_key model aan |> snd in

                let mkm, tk, tv =
                  match get_ntype va.type_ with
                  | Tmap (tk, tv)     -> MKMap, tk, tv
                  | Tbig_map (tk, tv) -> MKBigMap, tk, tv
                  | Titerable_big_map (tk, tv) -> MKIterableBigMap, tk, tv
                  | _ -> assert false
                in

                let set =
                  let get = mk_mterm (Mmapget (mkm, tk, tv, va, kk, Some an)) tv in
                  if is_record
                  then get
                  else mk_mterm (Mdot(get, mk_mident (dumloc fn))) (tset atk)
                in

                Msetcontains(atk, set, k)
              end
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
            | CKfield (an, fn, _) -> Utils.get_field_container model an fn |> fst |> Utils.get_asset_key model |> snd
            | _ -> Utils.get_asset_key model an |> snd
          in

          let init_0 = mk_nat 0 in
          let init_1 = mk_mterm (Mnone) (toption atk) in
          let init   = mk_tuple [init_0; init_1] in
          fold_ck (fm ctx) (an, ck) init mk ~with_value:false
          |> mk_tupleaccess 1
        end

      | Mcount (an, ck) -> begin
          let node =
            match ck with
            | CKcoll -> begin
                let va = get_asset_global an in
                match get_ntype va.type_ with
                | Tset tk           -> Msetlength (tk, va)
                | Tmap (tk, tv)     -> Mmaplength (MKMap, tk, tv, va)
                | Tbig_map (tk, tv) -> Mmaplength (MKBigMap, tk, tv, va)
                | Titerable_big_map (tk, tv) -> Mmaplength (MKIterableBigMap, tk, tv, va)
                | _ -> assert false
              end
            | CKview v -> begin
                let tk = Utils.get_asset_key model an |> snd in
                let v = fm ctx v in
                Mlistlength (tk, v)
              end
            | CKfield (an, fn, kk) -> begin
                let kk = fm ctx kk in

                let va = get_asset_global an in

                let _, is_record = is_single_simple_record an in
                let aan, _ = Utils.get_field_container model an fn in
                let atk = Utils.get_asset_key model aan |> snd in

                let mkm, tk, tv =
                  match get_ntype va.type_ with
                  | Tmap (tk, tv) -> MKMap, tk, tv
                  | Tbig_map (tk, tv) -> MKBigMap, tk, tv
                  | Titerable_big_map (tk, tv) -> MKIterableBigMap, tk, tv
                  | _ -> assert false
                in

                let set =
                  let get = mk_mterm (Mmapget (mkm, tk, tv, va, kk, Some an)) tv in
                  if is_record
                  then get
                  else mk_mterm (Mdot(get, mk_mident (dumloc fn))) (tset atk)
                in

                Msetlength(atk, set)
              end
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
                | Mdot ({node = Mvar ((_, {pldesc = "the"}), _); _}, fn) when String.equal (unloc_mident fn) akn -> vkey
                | Mdot ({node = Mvar ((_, {pldesc = "the"}), _); _}, fn) -> get_val an (Option.get vval) fn mt.type_
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
            | Tbuiltin Btez -> mk_mterm (Mmutez (Big_int.zero_big_int)) ttez
            | _ -> assert false
          in
          fold_ck (fm ctx) (an, ck) init mk
        end

      | Mhead (an, ck, n) -> begin
          let n = fm ctx n in

          let atk, tr =
            let a =
              match ck with
              | CKfield (an, fn, _) -> Utils.get_field_container model an fn |> fst |> Utils.get_asset_key model |> snd
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
              | CKfield (an, fn, _) -> Utils.get_field_container model an fn |> fst |> Utils.get_asset_key model |> snd
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

            let iid = mk_mident (dumloc "_hd") in
            let vid = mk_mterm (Mvar (iid, Vlocal)) atk in

            let iaccu = mk_mident (dumloc "_haccu") in
            let vaccu = mk_mterm (Mvar (iaccu, Vlocal)) init.type_ in

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

      | Mmakeasset (an, k, v) -> begin
          let k = fm ctx k in

          let mk l = mk_mterm (Masset l) (tasset (mk_mident (dumloc an))) in

          let res =
            if Utils.is_asset_single_field model an
            then mk [k]
            else begin
              let v = fm ctx v in
              if is_simple_record an
              then mk [k; v]
              else begin
                let asset = Utils.get_asset model an in
                (* Format.eprintf "KEYS: [%a]@\n" (Printer_tools.pp_list "," (fun fmt x -> Format.fprintf fmt "%s" x)) asset.keys; *)
                let vs = List.fold_right (fun x accu ->
                    if x.shadow || List.exists (fun k -> String.equal (unloc_mident x.name) k) asset.keys
                    then accu
                    else (x.name, x.type_)::accu
                  ) asset.values [] in
                (* Format.eprintf "VS: [%a]@\n" (Printer_tools.pp_list "," (fun fmt (x, t) -> Format.fprintf fmt "%s : %a" (unloc x) Printer_model.pp_type t)) vs; *)
                let l = List.fold_right (fun (id, t) accu ->
                    let mt = mk_mterm (Mdot (v, id)) t in mt::accu) vs [] in
                mk (k::l)
              end
            end
          in
          fm ctx res
        end

      | Mtocontainer an -> begin
          mk_mterm (Mvar (mk_mident (dumloc an), Vstorevar)) mt.type_
        end

      | _ -> map_mterm (fm ctx) mt
    in
    map_mterm_model fm model
  in

  let rec to_type_remove_asset (map : ((bool * bool) * (type_ * type_)) MapString.t) t : type_ =
    let ft = to_type_remove_asset map in
    match get_ntype t with
    | Tcontainer ((Tasset an, _), View) -> tlist (Utils.get_asset_key model (unloc_mident an) |> snd)
    | Tcontainer ((Tasset an, _), AssetContainer) -> get_type_for_asset_container map (unloc_mident an)
    | Tcontainer ((Tasset an, _), AssetKey) -> Utils.get_asset_key model (unloc_mident an) |> snd
    | Tcontainer ((Tasset an, _), AssetValue) ->
      let uan = unloc_mident an in
      if Utils.is_asset_single_field model uan
      then tunit
      else begin
        if is_simple_record map uan
        then get_type_for_asset_value map uan
        else trecord an
      end
    | Tasset an -> for_asset_type (unloc_mident an)
    | _ -> map_type ft t
  in

  let remove_type_asset (map : ((bool * bool) * (type_ * type_)) MapString.t) (model : model) : model =
    let ft = to_type_remove_asset map in
    let rec fm x = map_mterm ~ft fm x in
    map_model (fun _ -> id) ft fm model
  in

  let add_extra_asset (map : ((bool * bool) * (type_ * type_)) MapString.t) (model : model) : model =
    let ft = to_type_remove_asset map in
    let assets = List.fold_right (fun x accu -> match x with | Dasset a -> a::accu | _ -> accu) model.decls [] in
    let items = List.map (fun (x : asset) -> let an = x.name in ODAsset (mk_odel_asset (unloc_mident an) (ft (tassetcontainer an)) (ft (tassetkey an)) (ft (tassetvalue an)))) assets in
    let original_decls = model.extra.original_decls @ items in
    { model with extra = { original_decls = original_decls } }
  in

  let remove_decl_asset (model : model) : model =
    {model with decls = List.remove_if (function | Dasset _ -> true | _ -> false) model.decls }
  in

  let model, map = process_storage model in
  process_mterm map model
  |> remove_type_asset map
  |> add_extra_asset map
  |> remove_decl_asset

let remove_high_level_model (model : model)  =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    let f = aux ctx in
    match mt.node with
    | Mlistconcat (t, l, m) -> begin
        let tl = tlist t in
        let iter = f (mk_mterm (Mlistreverse (t, f l)) tl) in
        let iid = mk_mident (dumloc "_liid") in
        let vid = mk_mvar iid t in
        let iaccu = mk_mident (dumloc "_accu") in
        let vaccu = mk_mvar iaccu t in
        let b =  mk_mterm (Mlistprepend(t, vaccu, vid)) tl in
        mk_mterm (Mlistfold(t, iid, iaccu, iter, f m, b)) tl
      end
    | Miter (i, a, b, c, nat) -> begin
        let a = f a in
        let b = f b in
        let c = f c in

        let vi = mk_mvar i a.type_ in

        let ie = mk_mident (dumloc "_e") in
        let ve = mk_mvar ie b.type_ in

        let vinc : mterm = mk_mterm (Mplus(vi, (if nat then mk_nat else mk_int) 1)) tint in
        let inc  : mterm = mk_mterm (Massign(ValueAssign, tint, Avar i, vinc)) tunit in
        let body : mterm = mk_mterm (Mseq([c; inc])) tunit |> flat_sequence_mterm in
        let cond : mterm = mk_mterm (Mle (vi, ve)) tbool in
        let loop : mterm = mk_mterm (Mwhile (cond, body)) tunit in

        loop
        |> mk_letin i  a
        |> mk_letin ie b
      end
    | Mmapget (mkm, kt, vt, m, k, oan) ->
      let mapgetopt = mk_mterm (Mmapgetopt (mkm, kt, vt, f m, f k)) (toption vt) in
      let id = mk_mident (dumloc "_map_getopt_value") in
      let some_value = mk_mvar id vt in
      let none_value = match oan with | Some an -> failg (mk_tuple [mk_string fail_msg_ASSET_NOT_FOUND; mk_string an]) | None -> fail fail_msg_NOT_FOUND in

      mk_mterm (Mmatchoption (mapgetopt, id, some_value, none_value)) vt

    | Mfailsome v ->
      let v = f v in
      let vt = v.type_ in
      let id = mk_mident (dumloc "_v") in
      let some_value = failg (mk_mvar id vt) in
      let none_value = seq[] in
      mk_mterm (Mmatchoption (v, id, some_value, none_value)) vt
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let normalize_storage (model : model) : model =

  let replace_var_in_storage (model : model) : model =

    let for_mterm map (mt : mterm) : mterm =
      let rec aux (mt : mterm) : mterm =
        match mt.node with
        | Mvar (id, _) when MapString.mem (unloc_mident id) map -> MapString.find (unloc_mident id) map
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
        let map = MapString.add (unloc_mident si.id) si.default map in
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
      | Mvar (id, _) when MapString.mem (normalize_mident id) map -> MapString.find (normalize_mident id) map
      | _ -> map_mterm aux mt
    in
    aux mt
  in

  let map = MapString.empty in

  let map, decls = List.fold_left (fun (map, dns) dn ->
      match dn with
      | Dvar dvar when ((function | VKconstant -> true | _ -> false) dvar.kind) && Option.is_some(dvar.default)-> (MapString.add (normalize_mident dvar.name) (Option.get dvar.default) map, dns)
      | _ -> (map, dns @ [dn])) (map, []) model.decls in

  let map, storage = List.fold_left (fun (map, l) si ->
      match si.model_type with
      | MTconst -> (MapString.add (normalize_mident si.id) si.default map, l)
      | _ -> (map, l @ [si])) (map, []) model.storage in
  { model with
    decls = decls;
    storage = storage }
  |> map_mterm_model (for_mterm map)


let eval_storage (model : model) : model =
  let sis, _ = List.fold_left (fun (sis, map) (si : storage_item) ->
      let mt = Model.Utils.eval map si.default in
      let map = (unloc_mident si.id, mt)::map in
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
          let icallback = mk_mident (dumloc ((if no_underscore then "" else "_") ^ "cb" )) in
          let tcallback = mktype (Tcontract t) ~annot:(dumloc "%callback") in
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
    && Option.is_none model.metadata
  in

  let with_metadata _ =
    let rec aux ctx (accu : bool) (mt : mterm) : bool =
      match mt.node with
      | Mmetadata -> true
      | _ -> fold_term (aux ctx) accu mt
    in
    let with_offchain_view model =
      let is_offchain = function | VVoffchain | VVonoffchain -> true | VVonchain -> false in
      List.exists (fun f -> match f.node with | View (_, _, vv) -> is_offchain vv | _ -> false) model.functions
    in
    with_offchain_view model || fold_model aux model false
  in

  let js = match !Options.target with | Javascript -> true | _ -> false in
  let simple_metadata = js && !Options.opt_with_metadata in

  if check_if_not_metadata () && not (with_metadata ()) && not simple_metadata
  then model
  else begin
    let model =
      let rec aux ctx (mt : mterm) : mterm =
        match mt.node with
        | Mmetadata -> mk_svar (mk_mident (dumloc "metadata")) mt.type_
        | _ -> map_mterm (aux ctx) mt
      in
      map_mterm_model aux model
    in
    let model =
      if simple_metadata
      then
        let param : parameter = {
          name    = mk_mident (dumloc "metadata");
          typ     = tbytes;
          default = None;
          value   = None;
          const   = false;
          loc     = Location.dummy;
        } in
        { model with parameters = model.parameters @ [param] }
      else model
    in
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

        let mk_data input =
          let vkey = mk_string key in
          let metadata =
            mk_bytes (match Hex.of_string input with `Hex str -> str)
          in
          vkey, metadata
        in

        let do_uri             uri = [mk_uri uri] in
        let do_json           data = [mk_uri ("tezos-storage:" ^ key); data] in
        let do_json_with_path    p = do_json (p |> Tools.get_content |> mk_data) in
        let do_json_with_content i = do_json (mk_data i) in

        let v =
          match !Options.opt_metadata_uri, !Options.opt_metadata_storage, model.metadata, simple_metadata with
          | _, _, _, true        ->
            let mk_m _ =
              let vkey = mk_string key in
              vkey, (mk_mterm (Mvar(mk_mident (dumloc "metadata"), Vparameter)) tbytes)
            in
            do_json (mk_m ())
          | "", "", None, _         -> []
          | uri, "", _, _           when not (String.equal "" uri) -> do_uri uri
          | "", metadata_path, _, _ when not (String.equal "" metadata_path) -> do_json_with_path metadata_path
          | _, _, Some MKuri uri, _ -> do_uri (unloc uri)
          | _, _, Some MKjson i,  _ -> do_json_with_content (unloc i)
          | _ -> assert false
        in

        mk_metadata v in
      if not (String.equal "" !Options.opt_metadata_uri)
      then mk_map ()
      else mk_map ()
    in
    let dvar = mk_var (mk_mident (dumloc "metadata")) tmetadata tmetadata ~default:dmap VKvariable in
    let decl_metadata = Dvar dvar in
    let decls = model.decls @ [decl_metadata] in
    { model with
      decls = decls }
  end

let process_parameter (model : model) : model =
  let for_parameter (param : parameter) =
    let t = param.typ in
    let name = param.name in
    let mk_parameter id t = mk_mterm (Mvar(id, Vparameter )) t in
    let default : mterm =
      match param.value, param.default with
      | Some v, _
      | _, Some v -> v
      (* | _ when param.const && not js -> (emit_error (param.loc, NoInitValueForConstParam (unloc name)); raise (Error.Stop 5)) *)
      | _ -> mk_parameter name t
    in
    let var : var = mk_var name t t (if param.const then VKconstant else VKvariable) ~default ~loc:param.loc in
    Dvar var
  in
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mvar(a, Vparameter) -> { mt with node = Mvar(a, Vstorevar) }
    | _ -> map_mterm (aux ctx) mt
  in
  let model = map_mterm_model aux model in
  let params = List.map for_parameter model.parameters in
  { model with decls = params @ model.decls }

let expr_to_instr (model : model) =
  let is_compatible (ak : assign_kind) (c : mterm) =
    match ak, c.node with
    | Avar id0, (Mvar (id1, Vlocal))         -> String.equal (unloc_mident id0) (unloc_mident id1)
    | Avarstore id0, (Mvar (id1, Vstorevar)) -> String.equal (unloc_mident id0) (unloc_mident id1)
    | Arecord (lv0, rn0, fn0), (Mdot (({ node = _; type_ = ((Trecord rn1), None)}) as lv1, fn1))
      -> String.equal (unloc_mident rn0) (unloc_mident rn1) && String.equal (unloc_mident fn0) (unloc_mident fn1) && cmp_mterm lv0 lv1
    | _ -> false
  in
  let rec aux ctx (mt : mterm) =
    match mt.node, mt.type_ with

    | Massign (ValueAssign, _, ak, {node = Msetadd(t, c, k)}), tyinstr when is_compatible ak c  ->
      mk_mterm (Msetinstradd (t, ak, k)) tyinstr
    | Massign (ValueAssign, _, ak, {node = Msetremove(t, c, k)}), tyinstr when is_compatible ak c  ->
      mk_mterm (Msetinstrremove (t, ak, k)) tyinstr

    | Massign (ValueAssign, _, ak, {node = Mlistprepend(t, c, k)}), tyinstr when is_compatible ak c  ->
      mk_mterm (Mlistinstrprepend (t, ak, k)) tyinstr
    (* | Massign (ValueAssign, _, ak, {node = Mlistconcat(t, c, k)}), tyinstr when is_compatible ak c  ->
       mk_mterm (Mlistinstrconcat (t, ak, k)) tyinstr *)

    | Massign (ValueAssign, _, ak, {node = Mmapput(mky, tk, vk, c, k, v)}), tyinstr when is_compatible ak c  ->
      mk_mterm (Mmapinstrput (mky, tk, vk, ak, k, v)) tyinstr
    | Massign (ValueAssign, _, ak, {node = Mmapremove(mky, tk, vk, c, k)}), tyinstr when is_compatible ak c  ->
      mk_mterm (Mmapinstrremove (mky, tk, vk, ak, k)) tyinstr
    | Massign (ValueAssign, _, ak, {node = Mmapupdate(mky, tk, vk, c, k, v)}), tyinstr when is_compatible ak c  ->
      mk_mterm (Mmapinstrupdate (mky, tk, vk, ak, k, v)) tyinstr

    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let instr_to_expr_exec (model : model) =
  let is_used ak l =
    match ak with
    | Arecord (lv, _, _) ->
      List.exists (fold_term (fun accu x -> accu || cmp_mterm x lv) false) l
    | _ -> false
  in

  let extract_rev_var ak ty =
    match ak with
    | Arecord (lv, _, fn) -> mk_mterm (Mdot (lv, fn)) ty
    | _ -> assert false
  in

  let mk ak ty fnode =
    let rv : mterm = extract_rev_var ak ty in
    let v : mterm = mk_mterm (fnode rv) ty in
    mk_mterm (Massign (ValueAssign, ty, ak, v)) tunit
  in

  let rec aux ctx (mt : mterm) =
    match mt.node with
    | Msetinstradd (sty, ak, k) when is_used ak [k] ->
      mk ak (tset sty) (fun x -> Msetadd(sty, x, k))

    | Msetinstrremove (sty, ak, k) when is_used ak [k] ->
      mk ak (tset sty) (fun x -> Msetremove(sty, x, k))

    | Mlistinstrprepend (lty, ak, i) when is_used ak [i] ->
      mk ak (tlist lty) (fun x -> Mlistprepend(lty, x, i))

    | Mlistinstrconcat (lty, ak, i) when is_used ak [i] ->
      mk ak (tlist lty) (fun x -> Mlistconcat(lty, x, i))

    | Mmapinstrput(mky, kty, vty, ak, k, v) when is_used ak [k; v] ->
      mk ak (tmap kty vty) (fun x -> Mmapput(mky, kty, vty, x, k, v))

    | Mmapinstrremove(mky, kty, vty, ak, k) when is_used ak [k] ->
      mk ak (tmap kty vty) (fun x -> Mmapremove(mky, kty, vty, x, k))

    | Mmapinstrupdate (mky, kty, vty, ak, k, v) when is_used ak [k; v] ->
      mk ak (tmap kty vty) (fun x -> Mmapupdate(mky, kty, vty, x, k, v))

    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let fill_stovars (model : model) : model =
  let for_function_struct (fs : function_struct) : function_struct =
    let rec aux acc (mt : mterm) : ident list =
      match mt.node with
      | Mvar (id, Vstorevar) -> (unloc_mident id)::acc
      | Mvar (id, Vstorecol) -> (unloc_mident id)::acc
      | _ -> fold_term aux acc mt
    in
    {
      fs with
      stovars = aux [] fs.body
    }
  in
  let for_function_node (fn : function_node) : function_node =
    match fn with
    | Function (fs, t)     -> Function (for_function_struct fs, t)
    | Getter   (fs, t)     -> Getter   (for_function_struct fs, t)
    | View     (fs, t, vv) -> View     (for_function_struct fs, t, vv)
    | Entry     fs         -> Entry    (for_function_struct fs)
  in
  let for_functions (functions_ : function__) =
    {
      node = for_function_node functions_.node
    }
  in
  {
    model with
    functions = List.map for_functions model.functions
  }

let patch_fa2 (model : model) : model =
  let for_function__ (f__ : function__) : function__ =
    let for_function_node (fn : function_node) : function_node =
      let for_fs (fs : function_struct) : function_struct =
        let args =
          match unloc_mident fs.name with
          | s when String.equal s "update_operators" -> begin
              match fs.args with
              | [(arga, argt, argd)] -> begin
                  match argt with
                  | Tlist (Tor ((c, _), (d, _)), x), z ->
                    [(arga, (Tlist (Tor ((c, Some (dumloc "%add_operator")), (d, Some (dumloc "%remove_operator"))), x), z), argd)]
                  | _ -> fs.args
                end;
              | _ -> fs.args

            end
          | _ -> fs.args
        in
        {
          fs with
          args  = args;
        }
      in
      match fn with
      | Entry(fs) -> Entry (for_fs fs)
      | _ -> fn
    in
    { f__ with
      node = for_function_node f__.node;
    }
  in
  { model with
    functions = List.map for_function__ model.functions;
  }

let remove_iterable_big_map (model : model) : model =
  let process_type ?(id : mident option) (t : type_) : type_ =
    match t with
    | (Titerable_big_map (kt, vt), annot) ->
      let id =
        match Option.map unloc_mident id, annot with
        | Some id, _ -> Some id
        | _, Some annot -> Some (unloc annot)
        | _ -> None
      in

      let with_annot = Option.is_some id in

      let content = mktype (Tbig_map (kt, ttuple [
          mktype (Tbuiltin Bnat) ?annot:(if with_annot then Some (dumloc "%index") else None);
          mktype  (get_ntype vt) ?annot:(if with_annot then Some (dumloc "%value") else None)
        ])) ?annot:(if with_annot then Some (dumloc "%values") else None) in
      let index   = mktype (Tbig_map (tnat, kt))              ?annot:(if with_annot then Some (dumloc "%keys")   else None) in
      let counter = mktype (Tbuiltin Bnat)                    ?annot:(if with_annot then Some (dumloc "%size") else None) in
      (Ttuple [content; index; counter], Option.map dumloc id)
    | _ -> t
  in
  let rec aux ctx (mt : mterm) =
    match mt with
    (* instruction *)
    | { node = Massign (ValueAssign, ((Titerable_big_map (_, _)), _), ((Avar _ | Avarstore _) as assign_value),
                        { node = (Mmapput (MKIterableBigMap, kt, vt, map, key, value));
                          type_ = ((Titerable_big_map (_, _)), _); })} -> begin
        let ibm_id = mk_mident (dumloc "_ibm") in
        let ibm_type : type_ = process_type map.type_ in
        let ibm_value = mk_mvar ibm_id ibm_type in
        let ibm_init : mterm = aux ctx map in

        let vvt = ttuple [tnat; vt] in
        let tbm = (Tbig_map(kt, vvt), None) in

        let map_content : mterm = mk_tupleaccess 0 ibm_value in
        let map_index   : mterm = mk_tupleaccess 1 ibm_value in
        let map_counter : mterm = mk_tupleaccess 2 ibm_value in

        let key = aux ctx key in
        let value = aux ctx value in

        let idx_id = "_idx" in
        let idx_id_loced = mk_mident (dumloc idx_id) in
        let idx_var : mterm = mk_mvar idx_id_loced tnat in
        let init_value : mterm = mk_mterm (Mplus (map_counter, mk_nat 1)) tnat in

        let matchinstr : mterm =
          let getopt : mterm = mk_mterm (Mmapgetopt (MKBigMap, kt, vt, map_content, key)) (toption vvt) in
          let tmp_id = "_tmp_id" in
          let tmp_id_loced = mk_mident (dumloc tmp_id) in
          let tmp_var : mterm = mk_mvar tmp_id_loced vvt in
          let some_value =
            let v0 : mterm = mk_tupleaccess 0 tmp_var in
            mk_mterm (Massign (ValueAssign, tnat, (Avar idx_id_loced), v0)) tunit
          in
          let none_value : mterm =
            let assign_counter : mterm =
              mk_mterm (Massign (ValueAssign, tnat, (Atuple (mk_mvar ibm_id ibm_type, 2, 3)), idx_var)) tunit
            in
            let put =
              let put = mk_mterm (Mmapput (MKBigMap, tnat, kt, map_index, map_counter, key)) tbm in
              mk_mterm (Massign (ValueAssign, map_index.type_, (Atuple (mk_mvar ibm_id ibm_type, 1, 3)), put)) tunit
            in
            seq [assign_counter; put]
          in
          mk_mterm (Minstrmatchoption (getopt, tmp_id_loced, some_value, none_value)) tunit
        in
        let update_map : mterm =
          let put = mk_mterm (Mmapput (MKBigMap, kt, vvt, map_content, key, mk_tuple [idx_var; value])) tbm in
          mk_mterm (Massign (ValueAssign, tbm, (Atuple (mk_mvar ibm_id ibm_type, 0, 3)), put)) tunit
        in
        let instr_assign : mterm =
          mk_mterm (Massign (ValueAssign, ibm_value.type_, assign_value, ibm_value)) tunit
        in
        let body : mterm = seq [matchinstr; update_map; instr_assign] in

        body
        |> (fun x -> mk_mterm (Mletin ([idx_id_loced], LVsimple init_value, Some tnat, x, None)) tunit)
        |> (fun x -> mk_mterm (Mletin ([ibm_id], LVsimple ibm_init, Some ibm_type, x, None)) tunit)
      end

    | { node = Massign (ValueAssign, ((Titerable_big_map (_, _)), _), ((Avar _ | Avarstore _) as assign_value),
                        { node = (Mmapremove (MKIterableBigMap, kt, vt, map, key));
                          type_ = ((Titerable_big_map (_, _)), _); })} -> begin
        let map = aux ctx map in

        let ibm_id = mk_mident (dumloc "_ibm") in
        let ibm_type : type_ = process_type map.type_ in
        let ibm_value = mk_mvar ibm_id ibm_type in
        let ibm_init : mterm = aux ctx map in

        let vvt  = ttuple [tnat; vt] in
        let tbm  = tbig_map kt  vvt in
        let tbmi = tbig_map tnat kt in

        let map_content : mterm = mk_tupleaccess 0 ibm_value in
        let map_index   : mterm = mk_tupleaccess 1 ibm_value in
        let map_counter : mterm = mk_tupleaccess 2 ibm_value in

        let key = aux ctx key in

        let matchinstr : mterm =
          let getopt : mterm = mk_mterm (Mmapgetopt (MKBigMap, kt, vt, map_content, key)) (toption vvt) in
          let tmp_id = "_tmp_id" in
          let tmp_id_loced = mk_mident (dumloc tmp_id) in
          let tmp_var : mterm = mk_mvar tmp_id_loced vvt in

          let some_value : mterm =
            let idx_id = "_idx" in
            let idx_id_loced = mk_mident (dumloc idx_id) in
            let idx_var : mterm = mk_mvar idx_id_loced tnat in

            let last_key_id = "_last_key" in
            let last_key_id_loced = mk_mident (dumloc last_key_id) in
            let last_key_var : mterm = mk_mvar last_key_id_loced kt in

            let last_value_id = "_last_value" in
            let last_value_id_loced = mk_mident (dumloc last_value_id) in
            let last_value_var : mterm = mk_mvar last_value_id_loced vvt in

            let remove_content =
              let rem = mk_mterm (Mmapremove (MKBigMap, kt, vvt, map_content, key)) tbm in
              mk_mterm (Massign (ValueAssign, tbm, (Atuple (mk_mvar ibm_id ibm_type, 0, 3)), rem)) tunit
            in
            let update_last_value =
              let put = mk_mterm (Mmapput (MKBigMap, kt, vvt, map_content, last_key_var, mk_tuple [idx_var; mk_tupleaccess 1 last_value_var])) tbm in
              mk_mterm (Massign (ValueAssign, tbm, (Atuple (mk_mvar ibm_id ibm_type, 0, 3)), put)) tunit
            in
            let remove_index_counter =
              let rem = mk_mterm (Mmapremove (MKBigMap, tnat, kt, map_index, map_counter)) tbm in
              mk_mterm (Massign (ValueAssign, tbmi, (Atuple (mk_mvar ibm_id ibm_type, 1, 3)), rem)) tunit
            in
            let put_index =
              let put = mk_mterm (Mmapput (MKBigMap, tnat, kt, map_index, idx_var, last_key_var)) tbm in
              mk_mterm (Massign (ValueAssign, tbmi, (Atuple (mk_mvar ibm_id ibm_type, 1, 3)), put)) tunit
            in
            let dec :mterm =
              let subnat = mk_mterm (Msubnat (map_counter, mk_nat 1)) (toption tnat) in
              let idv = mk_mident (dumloc "_v") in
              let s = mk_mvar idv tnat in
              let mw = mk_mterm (Mmatchoption(subnat, idv, s, fail fail_msg_OPTION_IS_NONE)) tnat in
              mk_mterm (Massign (ValueAssign, tnat, (Atuple (mk_mvar ibm_id ibm_type, 2, 3)), mw)) tunit
            in
            let instr_assign : mterm =
              mk_mterm (Massign (ValueAssign, ibm_value.type_, assign_value, ibm_value)) tunit
            in
            seq [put_index; update_last_value; remove_index_counter; dec; remove_content; instr_assign]
            |> (fun x -> mk_mterm (Mletin ([last_value_id_loced], LVsimple (mk_mterm (Mmapget(MKBigMap, kt, vvt, map_content, last_key_var, None)) vvt), Some vvt, x, None)) tunit)
            |> (fun x -> mk_mterm (Mletin ([last_key_id_loced], LVsimple (mk_mterm (Mmapget(MKBigMap, tnat, kt, map_index, map_counter, None)) kt), Some kt, x, None)) tunit)
            |> (fun x -> mk_mterm (Mletin ([idx_id_loced], LVsimple (mk_tupleaccess 0 tmp_var), Some tnat, x, None)) tunit)
          in
          let none_value : mterm = seq [] in
          mk_mterm (Minstrmatchoption (getopt, tmp_id_loced, some_value, none_value)) tunit
        in
        matchinstr
        |> (fun x -> mk_mterm (Mletin ([ibm_id], LVsimple ibm_init, Some ibm_type, x, None)) tunit)
      end

    (* expression *)
    | { node = Mmapget (MKIterableBigMap, kt, vt, map, k, io)} ->
      mk_mterm (Mmapget (MKBigMap, kt, vt, aux ctx map |> mk_tupleaccess 0, aux ctx k, io)) (ttuple [tnat; vt]) |> mk_tupleaccess 1

    | { node = Mmapgetopt (MKIterableBigMap, kt, vt, map, k)} ->
      mk_mterm (Mmapgetopt (MKBigMap, kt, vt, aux ctx map |> mk_tupleaccess 0, aux ctx k)) (toption (ttuple [tnat; vt]))
      |> (fun (x : mterm) ->
          let var = mk_mident (dumloc "_var_ibm_getopt") in
          let mvar : mterm = mk_mvar var (ttuple [tnat; vt]) in
          mk_mterm (Mmap (x, var, mk_tupleaccess 1 mvar)) (toption vt))

    | { node = Mmapcontains (MKIterableBigMap, kt, vt, map, k)} ->
      mk_mterm (Mmapcontains (MKBigMap, kt, vt, aux ctx map |> mk_tupleaccess 0, aux ctx k)) (tbool)

    | { node = Mmaplength (MKIterableBigMap, _, _, map)} -> begin
        aux ctx map |> mk_tupleaccess 2
      end

    (* map_kind * type_ * 'id   * 'id   * 'id   * 'term * 'term * 'term*)
    | { node = Mmapfold (MKIterableBigMap, kt, ikid, ivid, iaccu, map, init, act); type_ = vt } -> begin
        let body = mk_mterm (Mfor (FIdouble(ikid, ivid), ICKmap map, mk_mterm (Massign (ValueAssign, kt, Avar iaccu, act)) tunit)) tunit in
        seq [aux ctx body; mk_mvar iaccu kt]
        |> (fun x -> mk_mterm (Mletin ([iaccu], LVsimple init, Some vt, x, None)) tunit)
      end

    (* control *)
    | { node = Mfor (FIdouble(id_k, id_v), (ICKmap ({ type_ = (Titerable_big_map (kt, vt), _) } as map)), body)} -> begin
        let ibm_id = mk_mident (dumloc "_ibm") in
        let ibm_type : type_ = process_type map.type_ in
        let ibm_value = mk_mvar ibm_id ibm_type in
        let ibm_init : mterm = aux ctx map in

        let map_content : mterm = mk_tupleaccess 0 ibm_value in
        let map_index   : mterm = mk_tupleaccess 1 ibm_value in
        let map_counter : mterm = mk_tupleaccess 2 ibm_value in
        let idx_id = mk_mident (dumloc "_idx_ibm") in
        let var_idx : mterm  = mk_mvar idx_id tint in
        let var_k : mterm  = mk_mvar id_k tnat in
        let one = mk_nat 1 in
        let bound_min = one in
        let bound_max : mterm = map_counter in
        let value_k = mk_mterm (Mmapget (MKBigMap, tnat, kt, map_index, var_idx, None)) kt in
        let value_v = mk_mterm (Mmapget (MKBigMap, kt, ttuple [tnat; vt], map_content, var_k, None)) (ttuple [tnat; vt]) |> mk_tupleaccess 1 in
        let letin =
          aux ctx body
          |> (fun x -> mk_mterm (Mletin ([id_v], LVsimple value_v, Some vt, x, None)) tunit)
          |> (fun x -> mk_mterm (Mletin ([id_k], LVsimple value_k, Some kt, x, None)) tunit)
        in

        mk_mterm (Miter (idx_id, bound_min, bound_max, letin, true)) tunit
        |> (fun x -> mk_mterm (Mletin ([ibm_id], LVsimple ibm_init, Some ibm_type, x, None)) tunit)
      end

    | { node = Mlitmap (MKIterableBigMap, original_values); type_ = (Titerable_big_map (kt, vt), _)} -> begin
        let content_type = tbig_map kt (ttuple [tnat; vt]) in
        let index_type   = tbig_map tnat kt in
        let content_value = mk_mterm (Mlitmap (MKBigMap, (List.mapi (fun i (k, v) -> (k, mk_tuple[mk_nat (i + 1); v]))) original_values)) content_type in
        let index_value   = mk_mterm (Mlitmap (MKBigMap, (List.mapi (fun i (k, _) -> (mk_nat (i + 1), k))               original_values))) index_type in
        let counter_value = mk_nat (List.length original_values) in
        mk_tuple [content_value; index_value; counter_value]
      end

    | { type_ = (Titerable_big_map (_, _), _) } -> begin
        { mt with type_ = process_type mt.type_ }
      end

    | _ -> map_mterm (aux ctx) mt
  in
  let process_mterm (input : mterm) : mterm = aux () input in
  let map_decl (x : decl_node) : decl_node =
    match x with
    | Dvar dvar -> begin
        match dvar.type_ with
        | (Titerable_big_map (_k, _v), _) -> begin
            (* let loc = dvar.loc in *)
            let name = dvar.name in
            Dvar { dvar with
                   type_ = process_type dvar.type_ ~id:name;
                   default = Option.map process_mterm dvar.default;
                 }
          end
        | _ -> x
      end
    | Drecord record -> begin
        Drecord { record with
                  fields = List.map (fun (field : record_field) ->
                      (match field.type_ with
                       | (Titerable_big_map (_k, _v), _) -> begin
                           let name = mk_mident (dumloc ("%" ^ (unloc_mident field.name))) in
                           { field with
                             type_ = process_type field.type_ ~id:name
                           }
                         end
                       | _ -> field)
                    ) record.fields
                }
      end
    | _ -> x
  in
  let model =
    { model with
      decls = List.map map_decl model.decls;
      storage = List.map (fun (x : storage_item) -> { x with typ = process_type x.typ ~id:x.id; default = process_mterm x.default }) model.storage
    }
  in
  map_mterm_model aux model

let lazy_eval_condition (model : model) : model =
  let mk_if (cond : mterm) (then_ : mterm) (else_ : mterm) : mterm = mk_mterm (Mexprif(cond, then_, else_)) tbool in
  let rec aux ctx (mt : mterm) : mterm =
    let f = aux ctx in
    match mt with
    | { node = Mand(a, b); type_ = (Tbuiltin Bbool, _) } -> mk_if (f a) (mk_if (f b) mtrue mfalse) mfalse
    | { node = Mor(a, b); type_ = (Tbuiltin Bbool, _) }  -> mk_if (f a) mtrue (mk_if (f b) mtrue mfalse)
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let remove_ternary_operator (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    let f = aux ctx in
    match mt with
    | { node = Mternarybool (c, a, b) }   -> { mt with node = Mexprif(f c, f a, f b) }
    | { node = Mternaryoption (c, a, b) } -> { mt with node = Mmatchoption (f c, mk_mident (dumloc "the"), f a, f b) }
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let remove_update_all (model : model) =
  let rec aux ctx (mt : mterm) : mterm =
    let f = aux ctx in
    match mt.node with
    | Mupdateall (an, c, l) -> begin
        let ick =
          match c with
          | CKcoll -> ICKcoll an
          | CKview v -> ICKview (f v)
          | CKfield (an, fn, v) -> ICKfield (an, fn, f v)
        in
        let k_id = mk_mident (dumloc "_update_all_key") in
        let (_, kt) = Utils.get_asset_key model an in
        let k = mk_mvar k_id kt in
        let update = mk_mterm (Mupdate(an, k, l)) tunit in
        { mt with node = Mfor(FIsimple k_id, ick, update)}
      end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let remove_decl_var_opt (model : model) =
  let rec aux ctx (mt : mterm) : mterm =
    let f = aux ctx in
    match mt.node with
    | Mdeclvaropt (ids, tyy, v, fa, c) -> begin
        let ty = match tyy with | Some ty -> ty | None -> v.type_ in
        let idv = mk_mident (dumloc "_v") in
        let s = mk_mvar idv ty in
        let fa =
          match fa with
          | Some fa -> failg (f fa)
          | None    -> fail fail_msg_OPTION_IS_NONE
        in
        let mw = mk_mterm (Mmatchoption(f v, idv, s, fa)) ty in
        { mt with node = Mdeclvar (ids, Some ty, mw, c) }
      end
    | Massignopt (op, ty, k, v, fa) -> begin
        let idv = mk_mident (dumloc "_v") in
        let s = mk_mvar idv ty in
        let mw = mk_mterm (Mmatchoption(f v, idv, s, failg (f fa))) ty in
        { mt with node = Massign (op, ty, k, mw) }
      end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let process_arith_container (model : model) =
  let doit_gen a (c : mterm) ctyp ityp fv : mterm =
    let cid = "_l" in
    let lcid = mk_mident (dumloc cid) in
    let container = mk_mvar lcid ctyp in

    let xid = mk_mident (dumloc "_x") in
    let x = mk_mvar xid ityp in
    let mapput : mterm = fv container x in
    let assign : mterm = mk_mterm (Massign (ValueAssign, ctyp, Avar lcid, mapput)) tunit in
    let ick = match get_ntype c.type_ with | Tset _ -> ICKset c | Tlist _ -> ICKlist c | _ -> assert false in
    let loop : mterm = mk_mterm (Mfor (FIsimple xid, ick, assign)) tunit in
    let seq = mk_mterm (Mseq [loop; container]) container.type_  in
    mk_letin lcid a seq
  in
  let rec aux ctx (mt : mterm) : mterm =
    let f = aux ctx in
    match mt.node with
    | Mplus (({type_ = ((Tmap (kt, vt), _) as t)} as a), ({type_ = (Tlist (Ttuple [lkt; lvt], _), _); node = Mlitlist l})) when cmp_type kt lkt && cmp_type vt lvt -> begin
        List.fold_right (fun x accu -> mk_mterm (Mmapput (MKMap, kt, vt, accu, mk_tupleaccess 0 x, mk_tupleaccess 1 x)) t) l (f a)
      end
    | Mplus (({type_ = ((Tmap (kt, vt), _) as tmap)} as a), (({type_ = ((Tlist (Ttuple [lkt; lvt], _) | Tset (Ttuple [lkt; lvt], _)), _)}) as c))  when cmp_type kt lkt && cmp_type vt lvt -> begin
        doit_gen a c tmap (ttuple [lkt; lvt]) (fun container x -> mk_mterm (Mmapput (MKMap, kt, vt, container, mk_tupleaccess 0 x, mk_tupleaccess 1 x)) tmap)
      end
    | Mplus (({type_ = ((Tset ty, _) as t)} as a), ({type_ = (Tlist lty, _); node = Mlitlist l})) when cmp_type ty lty -> begin
        List.fold_right (fun x accu -> mk_mterm (Msetadd (ty, accu, x)) t) l (f a)
      end
    | Mplus (({type_ = ((Tset ty, _) as tset)} as a), ({type_ = ((Tlist lty | Tset lty), _)} as c)) when cmp_type ty lty -> begin
        doit_gen a c tset lty (fun container x -> mk_mterm (Msetadd (lty, container, x)) tset)
      end
    | Mminus (({type_ = ((Tmap (kt, vt), _) as t)} as a), ({type_ = (Tlist lty, _); node = Mlitlist l})) when cmp_type kt lty -> begin
        List.fold_right (fun x accu -> mk_mterm (Mmapremove (MKMap, kt, vt, accu, x)) t) l (f a)
      end
    | Mminus (({type_ = ((Tmap (kt, vt), _) as tmap)} as a), ({type_ = ((Tlist lty | Tset lty), _)} as c)) when cmp_type kt lty -> begin
        doit_gen a c tmap kt (fun container x -> mk_mterm (Mmapremove (MKMap, kt, vt, container, x)) tmap)
      end
    | Mminus (({type_ = ((Tset ty, _) as t)} as a), ({type_ = (Tlist lty, _); node = Mlitlist l})) when cmp_type ty lty -> begin
        List.fold_right (fun x accu -> mk_mterm (Msetremove (ty, accu, x)) t) l (f a)
      end
    | Mminus (({type_ = ((Tset ty, _) as tset)} as a), ({type_ = ((Tlist lty | Tset lty), _)} as c)) when cmp_type ty lty -> begin
        doit_gen a c tset ty (fun container x -> mk_mterm (Msetremove (lty, container, x)) tset)
      end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let check_unused_variables (model : model) =
  let for_function (f : function__) =
    let fs =
      match f.node with
      | Function (fs, _)    -> fs
      | Getter   (fs, _)    -> fs
      | View     (fs, _, _) -> fs
      | Entry    fs         -> fs
    in
    let check_argument (fs : function_struct) =
      let args = fs.args in
      let ids = List.map proj3_1 args in
      let contains id = List.exists (fun x -> String.equal (unloc_mident id) (unloc_mident x)) ids in
      let rec aux accu (mt : mterm) =
        match mt.node with
        | Mvar (id, _) when contains id-> id::accu
        | _ -> fold_term aux accu mt
      in
      let is = aux [] fs.body in
      let js = List.fold_right (fun x accu -> if List.exists (fun y -> String.equal (unloc_mident y) (unloc_mident x)) is then accu else x::accu) ids [] in
      List.iter (fun x -> emit_warning (loc_mident x, UnusedArgument (unloc_mident x))) js
    in
    let check_variables (fs : function_struct) =
      let contains (accu : mident list) (id : mident) = List.exists (fun x -> String.equal (unloc_mident id) (unloc_mident x)) accu in
      let add accu (ids : mident list) : mident list = List.fold_left (fun accu (id : mident) -> if contains accu id then accu else id::accu) accu ids in
      let remove (accu : mident list) (id : mident) : mident list = List.fold_right (fun iid accu -> if String.equal (unloc_mident id) (unloc_mident iid) then accu else iid::accu) accu [] in
      let rec aux (accu : mident list) (mt : mterm) : mident list =
        match mt.node with
        | Mdeclvar (ids, _, v, _) -> add (aux accu v) ids
        | Mdeclvaropt (ids, _, v, _, _) -> add (aux accu v) ids
        | Mvar (id, _) -> remove accu id
        | _ -> fold_term aux accu mt
      in
      let l = aux [] fs.body in
      List.iter (fun x -> emit_warning (loc_mident x, UnusedVariable (unloc_mident x))) l
    in
    check_argument fs;
    check_variables fs
  in
  List.iter for_function model.functions;
  model

let remove_import_mterm (model : model) =
  let rec aux ctx (mt : mterm) : mterm =
    let f = aux ctx in
    match mt.node with
    | Mimportcallview (t, a, b, c) -> mk_mterm (Mcallview (t, f a, b, f c)) (mt.type_)
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let process_fail (model : model) =
  let rec aux ctx (mt : mterm) : mterm =
    let f = aux ctx in
    match mt.node with
    | Mfail (InvalidCondition(_, Some v)) -> begin
        {mt with node = Mfail (Invalid(f v))}
      end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model
