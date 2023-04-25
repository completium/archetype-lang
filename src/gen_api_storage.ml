open Tools
open Ident
open Model

exception Anomaly of string
type error_desc =
  | TODO
[@@deriving show {with_path = false}]

let generate_api_storage ?(verif=false) (model : model) : model =

  let to_ck = function
    | CKcoll              -> Coll
    | CKview _            -> View
    | CKfield (an, fn, _) -> Field (an, fn)
  in

  let rec f (ctx : ctx_model) (accu : api_storage list) (term : mterm) : api_storage list =
    let api_items : api_storage_node list =
      let mt_type = term.type_ in
      let is_rat = match get_ntype mt_type with | Tbuiltin Brational | Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)] -> true | _ -> false in
      let extract_option_type t = match get_ntype t with | Toption x -> x | _ -> assert false in
      match term.node with
      | Mget (asset_name, _, _) ->
        [APIAsset (Get asset_name)]
      | Mset (asset_name, _, _, _) ->
        [APIAsset (Set asset_name)]
      | Mupdate (asset_name, _, l) ->
        [APIAsset (Update (asset_name, List.map (fun (x, y, z) -> (unloc_mident x, y, z)) l))]
      | Maddasset (asset_name, _) ->
        [APIAsset (Add asset_name)]
      | Maddfield (asset_name, field_name, _, _) ->
        let (pa,_,_) = Utils.get_container_asset_key model asset_name field_name in
        [APIAsset (Add (unloc_mident pa)); APIAsset (FieldAdd (asset_name, field_name))]
      | Mremoveasset (asset_name, _) ->
        let ans : ident list = Utils.get_asset_partitions model (string_to_mident asset_name) |> List.map snd in
        List.map (fun x -> APIAsset (Remove x)) (asset_name::ans)
      | Mremovefield (asset_name, field_name, _, _) ->
        let (pa,_,_) = Utils.get_container_asset_key model asset_name field_name in
        [APIAsset (Remove (unloc_mident pa)); APIAsset (FieldRemove (asset_name, field_name))]
      | Mremoveall (asset_name, CKcoll) ->
        let ans : ident list = Utils.get_asset_partitions model (string_to_mident asset_name) |> List.map snd in
        List.map (fun x -> APIAsset (Remove x)) (asset_name::ans) @ [APIAsset (RemoveAll (asset_name, to_ck CKcoll))]
      | Mremoveall (asset_name, ((CKfield (aan, field_name, _)) as c)) ->
        let (pa,_,_) = Utils.get_container_asset_key model aan field_name in
        [APIAsset (Get asset_name); APIAsset (Remove (unloc_mident pa)); APIAsset (FieldRemove (asset_name, field_name)); APIAsset (RemoveAll (asset_name, to_ck c))]
      | Mremoveif (asset_name, CKcoll, la, lb, _) ->
        let ans : ident list = Utils.get_asset_partitions model (string_to_mident asset_name) |> List.map snd in
        let l = List.map (fun x -> APIAsset (Remove x)) (asset_name::ans) in
        [APIAsset (Get asset_name); APIAsset (Remove asset_name); APIAsset (RemoveIf (asset_name, to_ck CKcoll, la, lb))] @ l
      | Mremoveif (_, ((CKfield (an, fn, _)) as c), la, lb, _) ->
        let _, t, _ = Utils.get_asset_field model (string_to_mident an, fn) in
        let aan, l =
          match get_ntype t with
          | Tcontainer ((Tasset aan, _), Aggregate) -> unloc_mident aan, []
          | Tcontainer ((Tasset aan, _), Partition) -> unloc_mident aan, [APIAsset (Remove (unloc_mident aan))]
          | _ -> assert false
        in
        [APIAsset (Get aan); APIAsset (FieldRemove (an, fn)); APIAsset (RemoveIf (an, to_ck c, la, lb))] @ l
      | Mclear (an , ((CKview _) as c)) ->
        let ans : ident list = Utils.get_asset_partitions model (string_to_mident an) |> List.map snd in
        List.map (fun x -> APIAsset (Remove x)) (an::ans) @ [APIAsset (Clear (an, to_ck c))]
      | Mclear (an , ((CKfield (aan, fn, _)) as c)) ->
        let (pa,_,_) = Utils.get_container_asset_key model aan fn in
        [APIAsset (Remove (unloc_mident pa)); APIAsset (FieldRemove (aan, fn)); APIAsset (Clear (an, to_ck c))]
      | Mselect (asset_name, c, la, lb, _) ->
        [APIAsset (Get asset_name); APIAsset (Select (asset_name, to_ck c, la, lb))]
      | Msort (asset_name, c, l) ->
        [APIAsset (Get asset_name); APIAsset (Sort (asset_name, to_ck c, l))]
      | Mcontains (asset_name, c, _) ->
        [APIAsset (Contains (asset_name, to_ck c))]
      | Mnth (asset_name, c, _) ->
        [APIAsset (Get asset_name); APIAsset (Nth (asset_name, to_ck c))]
      | Mcount (asset_name, c) ->
        [APIAsset (Count (asset_name, to_ck c))]
      | Msum (asset_name, c, p) ->
        [APIAsset (Get asset_name); APIAsset (Sum (asset_name, to_ck c, p.type_, p))]
      | Mhead (asset_name, c, _) ->
        [APIAsset (Head (asset_name, to_ck c))]
      | Mtail (asset_name, c, _) ->
        [APIAsset (Tail (asset_name, to_ck c))]
      | Mlistprepend (t, _, _) ->
        [APIList (Lprepend t)]
      | Mlistcontains (t, _, _) ->
        [APIList (Lcontains t)]
      | Mlistlength (t, _) ->
        [APIList (Llength t)]
      | Mlistnth (t, _, _) ->
        [APIList (Lnth t)]
      | Mlistreverse (t, _) ->
        [APIList (Lreverse t)]
      | Mmax _ when is_rat ->
        [APIInternal (RatCmp) ; APIBuiltin (Bmax mt_type)]
      | Mmax _ ->
        [APIBuiltin (Bmax mt_type)]
      | Mmin _ when is_rat ->
        [APIInternal (RatCmp) ; APIBuiltin (Bmin mt_type)]
      | Mmin _ ->
        [APIBuiltin (Bmin mt_type)]
      | Mabs _ ->
        [APIBuiltin (Babs mt_type)]
      | Mconcat _ ->
        [APIBuiltin (Bconcat mt_type)]
      | Mslice _ ->
        [APIBuiltin (Bslice mt_type)]
      | Mlength x ->
        [APIBuiltin (Blength x.type_)]
      | Misnone x ->
        [APIBuiltin (Bisnone (extract_option_type x.type_))]
      | Missome x ->
        [APIBuiltin (Bissome (extract_option_type x.type_))]
      | Mfloor _ ->
        [APIBuiltin (Bfloor)]
      | Mceil _ ->
        [APIBuiltin (Bceil)]
      | Mnattostring _ ->
        [APIBuiltin (Bnattostring)]
      | Mrateq _ ->
        [APIInternal (RatEq)]
      | Mratcmp _ ->
        [APIInternal (RatCmp)]
      | Mratarith _ ->
        [APIInternal (RatArith)]
      | Mratuminus _ ->
        [APIInternal (RatUminus)]
      | Mrattez _ ->
        [APIInternal (RatTez)]
      | Mratdur _ ->
        [APIInternal (RatDur)]
      | Mfail (Invalid mt) when Utils.is_not_string_nat_int mt.type_ ->
        [APIBuiltin (Bfail mt.type_)]
      | _ -> []
    in
    let accu = List.fold_left (fun accu v -> Utils.add_api_storage_in_list accu (Model.mk_api_item v)) accu api_items in
    fold_term (f ctx) accu term
  in
  let l = fold_model f model []
          |> Utils.sort_api_storage model verif
  in
  { model with api_items = l }

