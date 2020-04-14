open Tools
open Location
open Ident
open Model

exception Anomaly of string
type error_desc =
  | TODO
[@@deriving show {with_path = false}]


let process_api_storage (model : model) : model =

  let add (_ctx : ctx_model) (l : api_storage list) (i :  api_storage) =
    let res, l = List.fold_left (fun (res, accu) (x : api_storage) ->
        if cmp_api_item_node x.node_item i.node_item
        then (true,
              { i with api_loc =
                         match x.api_loc, i.api_loc with
                         | _, ExecFormula
                         | ExecFormula, _
                         | OnlyExec, OnlyFormula
                         | OnlyFormula, OnlyExec -> ExecFormula
                         | _ -> i.api_loc
              }::accu)
        else (res, x::accu)) (false, []) l in
    if res then
      l
    else
      i::l
  in

  let rec f (ctx : ctx_model) (accu : api_storage list) (term : mterm) : api_storage list =
    let api_items : api_storage_node list =
      let mt_type = term.type_ in
      let is_rat = match mt_type with | Tbuiltin Brational | Ttuple [Tbuiltin Bint; Tbuiltin Bint] -> true | _ -> false in
      let extract_option_type = function | Toption x -> x | _ -> assert false in
      match term.node with
      | Mapifget (asset_name, _, _)
      | Mget (asset_name, _, _) ->
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
      | Mclearfield (an, fn, _) ->
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
      | Mgetopt x ->
        [APIBuiltin (Bgetopt (extract_option_type x.type_))]
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
      | _ -> []
    in
    let accu = List.fold_left (fun accu v -> add ctx accu (Model.mk_api_item v  (match ctx.formula with | true -> OnlyFormula | false -> OnlyExec))) accu api_items in
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
                   | APIInternal (RatEq         ) ->  1
                   | APIInternal (RatCmp        ) ->  2
                   | APIInternal (RatArith      ) ->  3
                   | APIInternal (RatUminus     ) ->  4
                   | APIInternal (RatTez        ) ->  5
                   | APIAsset   (Count         _) ->  6
                   | APIAsset   (Sum           _) ->  7
                   | APIAsset   (Min           _) ->  8
                   | APIAsset   (Max           _) ->  9
                   | APIAsset   (Get           _) -> 10
                   | APIAsset   (Set           _) -> 11
                   | APIAsset   (Add           _) -> 12
                   | APIAsset   (Remove        _) -> 13
                   | APIAsset   (Clear         _) -> 14
                   | APIAsset   (UpdateAdd     _) -> 15
                   | APIAsset   (UpdateRemove  _) -> 16
                   | APIAsset   (UpdateClear   _) -> 17
                   | APIAsset   (ToKeys        _) -> 18
                   | APIAsset   (Select        _) -> 19
                   | APIAsset   (Sort          _) -> 20
                   | APIAsset   (Contains      _) -> 21
                   | APIAsset   (Nth           _) -> 22
                   | APIList    (Lprepend      _) -> 23
                   | APIList    (Lcontains     _) -> 24
                   | APIList    (Lcount        _) -> 25
                   | APIList    (Lnth          _) -> 26
                   | APIBuiltin (Bmin          _) -> 27
                   | APIBuiltin (Bmax          _) -> 28
                   | APIBuiltin (Babs          _) -> 29
                   | APIBuiltin (Bconcat       _) -> 30
                   | APIBuiltin (Bslice        _) -> 31
                   | APIBuiltin (Blength       _) -> 32
                   | APIBuiltin (Bisnone       _) -> 33
                   | APIBuiltin (Bissome       _) -> 34
                   | APIBuiltin (Bgetopt       _) -> 35
                   | APIAsset   (Shallow       _) -> 36
                   | APIAsset   (Unshallow     _) -> 37
                   | APIAsset   (Listtocoll    _) -> 38
                   | APIAsset   (Head          _) -> 39
                   | APIAsset   (Tail          _) -> 40
                   | APIAsset   (ColToKeys     _) -> 41
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
