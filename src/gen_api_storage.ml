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
    let api_items : api_storage_node list =
      match term.node with
      | Mapifget (asset_name, _, _)
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
        [APIAsset (Remove pa); APIAsset (UpdateRemove (asset_name, field_name))]
      | Mclearasset an ->
        [APIAsset (Clear an)]
      | Mclearfield (an, fn) ->
        [APIAsset (UpdateClear (an, fn))]
      | Mapifselect (asset_name, _, p)
      | Mselect (asset_name, _, p) ->
        [APIAsset (Get asset_name); APIAsset (Select (asset_name, p))]
      | Mapifsort (asset_name, _, l)
      | Msort (asset_name, _, l) ->
        [APIAsset (Sort (asset_name, l))]
      | Mapifcontains (asset_name, _, _)
      | Mcontains (asset_name, _, _) ->
        [APIAsset (Contains asset_name)]
      | Mapifnth (asset_name, _, _)
      | Mnth (asset_name, _, _) ->
        [APIAsset (Get asset_name); APIAsset (Nth asset_name)]
      | Mapifcount (asset_name, _)
      | Mcount (asset_name, _) ->
        [APIAsset (Count asset_name)]
      | Mapifsum (asset_name, _, p)
      | Msum (asset_name, _, p) ->
        [APIAsset (Sum (asset_name, p.type_, p))]
      | Mapifmin (asset_name, field_name, _)
      | Mmin (asset_name, field_name, _) ->
        [APIAsset (Min (asset_name, unloc field_name))]
      | Mapifmax (asset_name, field_name, _)
      | Mmax (asset_name, field_name, _) ->
        [APIAsset (Max (asset_name, unloc field_name))]
      | Mshallow (asset_name, _) ->
        [APIAsset (Shallow asset_name)]
      | Munshallow (asset_name, _) ->
        [APIAsset (Unshallow asset_name)]
      | Mlisttocoll (asset_name, _) ->
        [APIAsset (Listtocoll asset_name)]
      | Mapifhead (asset_name, _, _)
      | Mhead (asset_name, _, _) ->
        [APIAsset (Head asset_name)]
      | Mapiftail (asset_name, _, _)
      | Mtail (asset_name, _, _) ->
        [APIAsset (Tail asset_name)]
      | Mcoltokeys asset_name ->
        [APIAsset (ColToKeys asset_name)]
      | Mlistprepend (t, _, _) ->
        [APIList (Lprepend t)]
      | Mlistcontains (t, _, _) ->
        [APIList (Lcontains t)]
      | Mlistcount (t, _) ->
        [APIList (Lcount t)]
      | Mlistnth (t, _, _) ->
        [APIList (Lnth t)]
      | Mrateq _ ->
        [APIInternal (RatEq)]
      | Mratcmp _ ->
        [APIInternal (RatCmp)]
      | Mratarith _ ->
        [APIInternal (RatArith)]
      | Mrattez _ ->
        [APIInternal (RatTez)]
      | _ -> []
    in
    let accu = List.fold_left (fun accu v -> add ctx accu (Model.mk_api_item v)) accu api_items in
    fold_term (f ctx) accu term
  in
  let l = fold_model f model []
          |> List.sort
            (fun (i1 : api_storage) (i2 : api_storage) ->
               let criteria_asset_name () : int =
                 let default = "_" in
                 let get_asset_name = function
                   | APIAsset (Get           an)        -> an
                   | APIAsset (Set           an)        -> an
                   | APIAsset (Add           an)        -> an
                   | APIAsset (Remove        an)        -> an
                   | APIAsset (Clear         an)        -> an
                   | APIAsset (UpdateAdd    (an, _))    -> an
                   | APIAsset (UpdateRemove (an, _))    -> an
                   | APIAsset (UpdateClear  (an, _))    -> an
                   | APIAsset (ToKeys        an)        -> an
                   | APIAsset (ColToKeys     an)        -> an
                   | APIAsset (Select       (an, _))    -> an
                   | APIAsset (Sort         (an, _))    -> an
                   | APIAsset (Contains      an)        -> an
                   | APIAsset (Nth           an)        -> an
                   | APIAsset (Count         an)        -> an
                   | APIAsset (Sum          (an, _, _)) -> an
                   | APIAsset (Min          (an, _))    -> an
                   | APIAsset (Max          (an, _))    -> an
                   | APIList _                          -> default
                   | APIBuiltin _                       -> default
                   | APIInternal _                      -> default
                   | APIAsset (Shallow       an)        -> an
                   | APIAsset (Unshallow     an)        -> an
                   | APIAsset (Listtocoll    an)        -> an
                   | APIAsset (Head          an)        -> an
                   | APIAsset (Tail          an)        -> an
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
                   | APIAsset   (Clear         _) -> 10
                   | APIAsset   (UpdateAdd     _) -> 11
                   | APIAsset   (UpdateRemove  _) -> 12
                   | APIAsset   (UpdateClear   _) -> 13
                   | APIAsset   (ToKeys        _) -> 14
                   | APIAsset   (Select        _) -> 15
                   | APIAsset   (Sort          _) -> 16
                   | APIAsset   (Contains      _) -> 17
                   | APIList    (Lprepend      _) -> 18
                   | APIList    (Lcontains     _) -> 19
                   | APIList    (Lcount        _) -> 20
                   | APIList    (Lnth          _) -> 21
                   | APIBuiltin (MinBuiltin    _) -> 22
                   | APIBuiltin (MaxBuiltin    _) -> 23
                   | APIAsset   (Shallow       _) -> 24
                   | APIAsset   (Unshallow     _) -> 25
                   | APIAsset   (Listtocoll    _) -> 26
                   | APIAsset   (Head          _) -> 27
                   | APIAsset   (Tail          _) -> 28
                   | APIAsset   (ColToKeys     _) -> 29
                   | APIInternal (RatEq         ) -> 30
                   | APIInternal (RatCmp        ) -> 31
                   | APIInternal (RatArith      ) -> 32
                   | APIInternal (RatTez        ) -> 33
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
