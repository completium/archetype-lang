open Tools
open Location
open Ident
open Model

exception Anomaly of string
type error_desc =
  | TODO
[@@deriving show {with_path = false}]

let generate_api_storage ?(verif=false) (model : model) : model =

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
      | Mapifselect (asset_name, _, la, lb, _)
      | Mselect (asset_name, _, la, lb, _) ->
        [APIAsset (Get asset_name); APIAsset (Select (asset_name, la, lb))]
      | Mapifsort (asset_name, _, l)
      | Msort (asset_name, _, l) ->
        [APIAsset (Get asset_name); APIAsset (Sort (asset_name, l))]
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
        [APIAsset (Get asset_name); APIAsset (Sum (asset_name, p.type_, p))]
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
                   | APIAsset (Select       (an, _, _)) -> an
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
                   | APIAsset   (Nth           _) -> if verif then 7 else 12
                   | APIAsset   (Count         _) ->  8
                   | APIAsset   (Sum           _) ->  9
                   | APIAsset   (Min           _) -> 10
                   | APIAsset   (Max           _) -> 11
                   | APIAsset   (Get           _) -> if verif then 12 else 7
                   | APIAsset   (Set           _) -> 13
                   | APIAsset   (Add           _) -> 14
                   | APIAsset   (Remove        _) -> 15
                   | APIAsset   (Clear         _) -> 16
                   | APIAsset   (UpdateAdd     _) -> 17
                   | APIAsset   (UpdateRemove  _) -> 18
                   | APIAsset   (UpdateClear   _) -> 19
                   | APIAsset   (ToKeys        _) -> 20
                   | APIAsset   (Select        _) -> 21
                   | APIAsset   (Sort          _) -> 22
                   | APIAsset   (Contains      _) -> 23
                   | APIList    (Lprepend      _) -> 24
                   | APIList    (Lcontains     _) -> 25
                   | APIList    (Lcount        _) -> 26
                   | APIList    (Lnth          _) -> 27
                   | APIBuiltin (Bmin          _) -> 28
                   | APIBuiltin (Bmax          _) -> 29
                   | APIBuiltin (Babs          _) -> 30
                   | APIBuiltin (Bconcat       _) -> 31
                   | APIBuiltin (Bslice        _) -> 32
                   | APIBuiltin (Blength       _) -> 33
                   | APIBuiltin (Bisnone       _) -> 34
                   | APIBuiltin (Bissome       _) -> 35
                   | APIBuiltin (Bgetopt       _) -> 36
                   | APIAsset   (Shallow       _) -> 37
                   | APIAsset   (Unshallow     _) -> 38
                   | APIAsset   (Listtocoll    _) -> 39
                   | APIAsset   (Head          _) -> 40
                   | APIAsset   (Tail          _) -> 41
                   | APIAsset   (ColToKeys     _) -> 42
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

