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
        if x.node = i.node
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
      | Mset (asset_name, _, _) ->
        [APIStorage (Set asset_name)]
      | Maddasset (asset_name, _, _) ->
        [APIStorage (Add asset_name)]
      | Maddfield (asset_name, field_name, _, _, _) ->
        [APIStorage (UpdateAdd (asset_name, field_name))]
      | Mremoveasset (asset_name, _) ->
        [APIStorage (Remove asset_name)]
      | Mremovefield (asset_name, field_name, _, _) ->
        [APIStorage (UpdateRemove (asset_name, field_name))]
      | Mclearasset (asset_name) ->
        [APIStorage (Clear asset_name)]
      | Mclearfield (asset_name, field_name, _) ->
        [APIStorage (UpdateClear (asset_name, field_name))]
      | Mreverseasset (asset_name) ->
        [APIStorage (Reverse asset_name)]
      | Mreversefield (asset_name, field_name, _) ->
        [APIStorage (UpdateReverse (asset_name, field_name))]
      | Mselect (asset_name, _, _) ->
        [APIStorage (Get asset_name); APIFunction (Select asset_name)]
      | Msort (asset_name, _, field_name, _) ->
        [APIFunction (Sort (asset_name, field_name))]
      | Mcontains (asset_name, _, _) ->
        [APIFunction (Contains asset_name)]
      | Mnth (asset_name, _, _) ->
        [APIFunction (Nth asset_name)]
      | Mcount (asset_name, _) ->
        [APIFunction (Count asset_name)]
      | Msum (asset_name, field_name, _) ->
        [APIFunction (Sum (asset_name, unloc field_name))]
      | Mmin (asset_name, field_name, _) ->
        [APIFunction (Min (asset_name, unloc field_name))]
      | Mmax (asset_name, field_name, _) ->
        [APIFunction (Max (asset_name, unloc field_name))]
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
                   | APIStorage (Clear            an)     -> an
                   | APIStorage (Reverse          an)     -> an
                   | APIStorage (UpdateAdd       (an, _)) -> an
                   | APIStorage (UpdateRemove    (an, _)) -> an
                   | APIStorage (UpdateClear     (an, _)) -> an
                   | APIStorage (UpdateReverse   (an, _)) -> an
                   | APIStorage (ToKeys           an)     -> an
                   | APIFunction (Select          an)     -> an
                   | APIFunction (Sort           (an, _)) -> an
                   | APIFunction (Contains        an)     -> an
                   | APIFunction (Nth             an)     -> an
                   | APIFunction (Count           an)     -> an
                   | APIFunction (Sum            (an, _)) -> an
                   | APIFunction (Min            (an, _)) -> an
                   | APIFunction (Max            (an, _)) -> an
                   | APIContainer _                       -> default
                   | APIBuiltin _                         -> default
                 in
                 let asset_list : ident list = List.fold_left (fun accu (x : decl_node) ->
                     match x with
                     | Drecord r -> accu @ [unloc r.name]
                     | _ -> accu
                   ) [] model.decls in
                 let get_idx (i : api_item) = List.index_of (fun x -> String.equal (get_asset_name i.node) x) asset_list in
                 let idx1 = get_idx i1 in
                 let idx2 = get_idx i2 in
                 idx1 - idx2
               in

               let criteria_kind () : int =
                 let get_kind = function
                   | APIStorage   (Get           _) ->  1
                   | APIStorage   (Set           _) ->  2
                   | APIStorage   (Add           _) ->  3
                   | APIStorage   (Remove        _) ->  4
                   | APIStorage   (Clear         _) ->  5
                   | APIStorage   (Reverse       _) ->  6
                   | APIStorage   (UpdateAdd     _) ->  7
                   | APIStorage   (UpdateRemove  _) ->  8
                   | APIStorage   (UpdateClear   _) ->  9
                   | APIStorage   (UpdateReverse _) -> 10
                   | APIStorage   (ToKeys        _) -> 11
                   | APIFunction  (Select        _) -> 12
                   | APIFunction  (Sort          _) -> 13
                   | APIFunction  (Contains      _) -> 14
                   | APIFunction  (Nth           _) -> 15
                   | APIFunction  (Count         _) -> 16
                   | APIFunction  (Sum           _) -> 17
                   | APIFunction  (Min           _) -> 18
                   | APIFunction  (Max           _) -> 19
                   | APIContainer (Add           _) -> 20
                   | APIContainer (Remove        _) -> 21
                   | APIContainer (Clear         _) -> 22
                   | APIContainer (Reverse       _) -> 23
                   | APIBuiltin   (Min           _) -> 24
                   | APIBuiltin   (Max           _) -> 25
                 in
                 let idx1 = get_kind i1.node in
                 let idx2 = get_kind i2.node in
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
