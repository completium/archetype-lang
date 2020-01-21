open Tools
open Location
open Ident
open Model

exception Anomaly of string
type error_desc =
  | TODO
[@@deriving show {with_path = false}]


let process_api_storage (model : model) : model =

  let add (ctx : ctx_model) (l : api_storage list) (i :  api_storage) =
    let item = { i with only_formula = ctx.formula } in
    let res, l = List.fold_left (fun (res, accu) (x : api_storage) ->
        if cmp_api_item_node x.node_item i.node_item
        then (true, { item with only_formula = x.only_formula && ctx.formula }::accu)
        else (res, x::accu)) (false, []) l in
    if res then
      l
    else
      item::l
  in

  let rec f (ctx : ctx_model) (accu : api_storage list) (term : mterm) : api_storage list =
    let accu = fold_term (f ctx) accu term in
    let api_items : api_storage_node list =
      match term.node with
      | Mget (asset_name, _) ->
        [APIAsset (Get asset_name)]
      | Mset (asset_name, _, _, _) ->
        [APIAsset (Set asset_name)]
      | Maddasset (asset_name, _) ->
        [APIAsset (Add asset_name)]
      | Maddfield (asset_name, field_name, _, _) ->
        let (pa,_,_) = Utils.get_container_asset_key model asset_name field_name in
        [APIAsset (Add pa); APIAsset (UpdateAdd (asset_name, field_name))]
      | Mremoveasset (asset_name, _) ->
        [APIAsset (Remove asset_name)]
      | Mremovefield (asset_name, field_name, _, _) ->
        let (pa,_,_) = Utils.get_container_asset_key model asset_name field_name in
        [APIAsset (Remove pa);APIAsset (UpdateRemove (asset_name, field_name))]
      | Mselect (asset_name, _, p) ->
        [APIAsset (Get asset_name); APIAsset (Select (asset_name, p))]
      | Msort (asset_name, _, field_name, _) ->
        [APIAsset (Sort (asset_name, field_name))]
      | Mcontains (asset_name, _, _) ->
        [APIAsset (Contains asset_name)]
      | Mnth (asset_name, _, _) ->
        [APIAsset (Get asset_name); APIAsset (Nth asset_name)]
      | Mcount (asset_name, _) ->
        [APIAsset (Count asset_name)]
      | Msum (asset_name, field_name, _) ->
        [APIAsset (Sum (asset_name, unloc field_name))]
      | Mmin (asset_name, field_name, _) ->
        [APIAsset (Min (asset_name, unloc field_name))]
      | Mmax (asset_name, field_name, _) ->
        [APIAsset (Max (asset_name, unloc field_name))]
      | Mshallow (asset_name, _) ->
        [APIAsset (Shallow asset_name)]
      | Munshallow (asset_name, _) ->
        [APIAsset (Unshallow asset_name)]
      | Mlisttocoll (asset_name, _) ->
        [APIAsset (Listtocoll asset_name)]
      | Mhead (asset_name, _, _) ->
        [APIAsset (Head asset_name)]
      | Mtail (asset_name, _, _) ->
        [APIAsset (Tail asset_name)]
      | Mcoltokeys asset_name ->
        [APIAsset (ColToKeys asset_name)]
      | Mlistprepend ({type_=t; _}, _) ->
        [APIList (Lprepend t)]
      | Mlistcontains ({type_=t; _}, _) ->
        [APIList (Lcontains t)]
      | Mlistcount ({type_=t; _}) ->
        [APIList (Lcount t)]
      | Mlistnth ({type_=t; _}, _) ->
        [APIList (Lnth t)]
      | Mrateq _ ->
        [APIBuiltin (RatEq)]
      | Mratcmp _ ->
        [APIBuiltin (RatCmp)]
      | Mratarith _ ->
        [APIBuiltin (RatArith)]
      | Mrattez _ ->
        [APIBuiltin (RatTez)]
      | _ -> []
    in
    List.fold_left (fun accu v -> add ctx accu (Model.mk_api_item v)) accu api_items
  in
  let l = fold_model f model []
          |> List.sort
            (fun (i1 : api_storage) (i2 : api_storage) ->
               let criteria_asset_name () : int =
                 let default = "_" in
                 let get_asset_name = function
                   | APIAsset (Get              an)     -> an
                   | APIAsset (Set              an)     -> an
                   | APIAsset (Add              an)     -> an
                   | APIAsset (Remove           an)     -> an
                   | APIAsset (UpdateAdd       (an, _)) -> an
                   | APIAsset (UpdateRemove    (an, _)) -> an
                   | APIAsset (ToKeys           an)     -> an
                   | APIAsset (ColToKeys        an)     -> an
                   | APIAsset (Select          (an, _)) -> an
                   | APIAsset (Sort            (an, _)) -> an
                   | APIAsset (Contains         an)     -> an
                   | APIAsset (Nth              an)     -> an
                   | APIAsset (Count            an)     -> an
                   | APIAsset (Sum             (an, _)) -> an
                   | APIAsset (Min             (an, _)) -> an
                   | APIAsset (Max             (an, _)) -> an
                   | APIList _                          -> default
                   | APIBuiltin _                       -> default
                   | APIAsset (Shallow          an)     -> an
                   | APIAsset (Unshallow        an)     -> an
                   | APIAsset (Listtocoll       an)     -> an
                   | APIAsset (Head             an)     -> an
                   | APIAsset (Tail             an)     -> an
                 in
                 let asset_list : ident list = List.fold_left (fun accu (x : decl_node) ->
                     match x with
                     | Dasset r -> accu @ [unloc r.name]
                     | _ -> accu
                   ) [] model.decls in
                 let get_idx (i : api_storage) = List.index_of (fun x -> String.equal (get_asset_name i.node_item) x) asset_list in
                 let idx1 = get_idx i1 in
                 let idx2 = get_idx i2 in
                 idx1 - idx2
               in

               let criteria_kind () : int =
                 let get_kind = function
                   | APIAsset   (Nth           _) ->  1
                   | APIAsset   (Count         _) ->  2
                   | APIAsset   (Sum           _) ->  3
                   | APIAsset   (Min           _) ->  4
                   | APIAsset   (Max           _) ->  5
                   | APIAsset   (Get           _) ->  6
                   | APIAsset   (Set           _) ->  7
                   | APIAsset   (Add           _) ->  8
                   | APIAsset   (Remove        _) ->  9
                   | APIAsset   (UpdateAdd     _) -> 10
                   | APIAsset   (UpdateRemove  _) -> 11
                   | APIAsset   (ToKeys        _) -> 12
                   | APIAsset   (Select        _) -> 13
                   | APIAsset   (Sort          _) -> 14
                   | APIAsset   (Contains      _) -> 15
                   | APIList    (Lprepend      _) -> 16
                   | APIList    (Lcontains     _) -> 17
                   | APIList    (Lcount        _) -> 18
                   | APIList    (Lnth          _) -> 19
                   | APIBuiltin (MinBuiltin    _) -> 20
                   | APIBuiltin (MaxBuiltin    _) -> 21
                   | APIAsset   (Shallow       _) -> 22
                   | APIAsset   (Unshallow     _) -> 23
                   | APIAsset   (Listtocoll    _) -> 24
                   | APIAsset   (Head          _) -> 25
                   | APIAsset   (Tail          _) -> 26
                   | APIAsset   (ColToKeys     _) -> 27
                   | APIBuiltin (RatEq          ) -> 28
                   | APIBuiltin (RatCmp         ) -> 29
                   | APIBuiltin (RatArith       ) -> 30
                   | APIBuiltin (RatTez         ) -> 31
                 in
                 let idx1 = get_kind i1.node_item in
                 let idx2 = get_kind i2.node_item in
                 idx1 - idx2
               in

               let c1 = criteria_asset_name () in
               if c1 = 0
               then criteria_kind ()
               else c1
            )
  in
  { model with api_items = l }

let generate_api_storage (model : model) : model =
  model
  |> process_api_storage
