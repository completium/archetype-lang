open Tools
open Location
open Ident
open Model

exception Anomaly of string
type error_desc =
  | TODO
[@@deriving show {with_path = false}]


let process_api_storage (model : model) : model =

  let add (ctx : ctx_model) (l : api_item list) (i :  api_item) =
    let item = { i with only_formula = ctx.formula } in
    let res, l = List.fold_left (fun (res, accu) (x : api_item) ->
        if cmp_api_item_node x.node_item i.node_item
        then (true, { item with only_formula = x.only_formula && ctx.formula }::accu)
        else (res, x::accu)) (false, []) l in
    if res then
      l
    else
      item::l
  in

  let rec f (ctx : ctx_model) (accu : api_item list) (term : mterm) : api_item list =
    let accu = fold_term (f ctx) accu term in
    let api_items : api_item_node list =
      match term.node with
      | Mget (asset_name, _) ->
        [APIStorage (Get asset_name)]
      | Mset (asset_name, _, _, _) ->
        [APIStorage (Set asset_name)]
      | Maddasset (asset_name, _) ->
        [APIStorage (Add asset_name)]
      | Maddfield (asset_name, field_name, _, _) ->
        let (pa,_,_) = Utils.get_container_asset_key model asset_name field_name in
        [APIStorage (Add pa); APIStorage (UpdateAdd (asset_name, field_name))]
      | Mremoveasset (asset_name, _) ->
        [APIStorage (Remove asset_name)]
      | Mremovefield (asset_name, field_name, _, _) ->
        let (pa,_,_) = Utils.get_container_asset_key model asset_name field_name in
        [APIStorage (Remove pa);APIStorage (UpdateRemove (asset_name, field_name))]
      | Mselect (asset_name, _, p) ->
        [APIStorage (Get asset_name); APIFunction (Select (asset_name, p))]
      | Msort (asset_name, _, field_name, _) ->
        [APIFunction (Sort (asset_name, field_name))]
      | Mcontains (asset_name, _, _) ->
        [APIFunction (Contains asset_name)]
      | Mnth (asset_name, _, _) ->
        [APIStorage (Get asset_name); APIFunction (Nth asset_name)]
      | Mcount (asset_name, _) ->
        [APIFunction (Count asset_name)]
      | Msum (asset_name, field_name, _) ->
        [APIFunction (Sum (asset_name, unloc field_name))]
      | Mmin (asset_name, field_name, _) ->
        [APIFunction (Min (asset_name, unloc field_name))]
      | Mmax (asset_name, field_name, _) ->
        [APIFunction (Max (asset_name, unloc field_name))]
      | Mshallow (asset_name, _) ->
        [APIFunction (Shallow asset_name)]
      | Munshallow (asset_name, _) ->
        [APIFunction (Unshallow asset_name)]
      | Mlisttocoll (asset_name, _) ->
        [APIFunction (Listtocoll asset_name)]
      | Mhead (asset_name, _, _) ->
        [APIFunction (Head asset_name)]
      | Mtail (asset_name, _, _) ->
        [APIFunction (Tail asset_name)]
      | Mcoltokeys asset_name ->
        [APIStorage (ColToKeys asset_name)]
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
            (fun (i1 : api_item) (i2 : api_item) ->
               let criteria_asset_name () : int =
                 let default = "_" in
                 let get_asset_name = function
                   | APIStorage (Get              an)     -> an
                   | APIStorage (Set              an)     -> an
                   | APIStorage (Add              an)     -> an
                   | APIStorage (Remove           an)     -> an
                   | APIStorage (UpdateAdd       (an, _)) -> an
                   | APIStorage (UpdateRemove    (an, _)) -> an
                   | APIStorage (ToKeys           an)     -> an
                   | APIStorage (ColToKeys        an)     -> an
                   | APIFunction (Select         (an, _)) -> an
                   | APIFunction (Sort           (an, _)) -> an
                   | APIFunction (Contains        an)     -> an
                   | APIFunction (Nth             an)     -> an
                   | APIFunction (Count           an)     -> an
                   | APIFunction (Sum            (an, _)) -> an
                   | APIFunction (Min            (an, _)) -> an
                   | APIFunction (Max            (an, _)) -> an
                   | APIContainer _                       -> default
                   | APIBuiltin _                         -> default
                   | APIFunction (Shallow         an)     -> an
                   | APIFunction (Unshallow       an)     -> an
                   | APIFunction (Listtocoll      an)     -> an
                   | APIFunction (Head            an)     -> an
                   | APIFunction (Tail            an)     -> an
                 in
                 let asset_list : ident list = List.fold_left (fun accu (x : decl_node) ->
                     match x with
                     | Dasset r -> accu @ [unloc r.name]
                     | _ -> accu
                   ) [] model.decls in
                 let get_idx (i : api_item) = List.index_of (fun x -> String.equal (get_asset_name i.node_item) x) asset_list in
                 let idx1 = get_idx i1 in
                 let idx2 = get_idx i2 in
                 idx1 - idx2
               in

               let criteria_kind () : int =
                 let get_kind = function
                   | APIFunction  (Nth           _) ->  1
                   | APIFunction  (Count         _) ->  2
                   | APIFunction  (Sum           _) ->  3
                   | APIFunction  (Min           _) ->  4
                   | APIFunction  (Max           _) ->  5
                   | APIStorage   (Get           _) ->  6
                   | APIStorage   (Set           _) ->  7
                   | APIStorage   (Add           _) ->  8
                   | APIStorage   (Remove        _) ->  9
                   | APIStorage   (UpdateAdd     _) -> 10
                   | APIStorage   (UpdateRemove  _) -> 11
                   | APIStorage   (ToKeys        _) -> 12
                   | APIFunction  (Select        _) -> 13
                   | APIFunction  (Sort          _) -> 14
                   | APIFunction  (Contains      _) -> 15
                   | APIContainer (AddItem       _) -> 16
                   | APIContainer (RemoveItem    _) -> 17
                   | APIBuiltin   (MinBuiltin    _) -> 18
                   | APIBuiltin   (MaxBuiltin    _) -> 19
                   | APIFunction  (Shallow       _) -> 20
                   | APIFunction  (Unshallow     _) -> 21
                   | APIFunction  (Listtocoll    _) -> 22
                   | APIFunction  (Head          _) -> 23
                   | APIFunction  (Tail          _) -> 24
                   | APIStorage   (ColToKeys     _) -> 25
                   | APIBuiltin   (RatEq          ) -> 26
                   | APIBuiltin   (RatCmp         ) -> 27
                   | APIBuiltin   (RatArith       ) -> 28
                   | APIBuiltin   (RatTez         ) -> 29
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
