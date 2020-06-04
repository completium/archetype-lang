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

  let to_ck = function
    | CKcoll   -> Coll
    | CKview _ -> View
  in

  let rec f (ctx : ctx_model) (accu : api_storage list) (term : mterm) : api_storage list =
    let api_items : api_storage_node list =
      let mt_type = term.type_ in
      let is_rat = match mt_type with | Tbuiltin Brational | Ttuple [Tbuiltin Bint; Tbuiltin Bint] -> true | _ -> false in
      let extract_option_type = function | Toption x -> x | _ -> assert false in
      match term.node with
      | Mapifget (asset_name, _, _)
      | Mget (asset_name, _) ->
        [APIAsset (Get asset_name)]
      | Mset (asset_name, _, _, _) ->
        [APIAsset (Set asset_name)]
      | Mupdate (asset_name, _, l) ->
        [APIAsset (Update (asset_name, List.map (fun (x, y, z) -> (unloc x, y, z)) l))]
      | Maddasset (asset_name, _) ->
        [APIAsset (Add asset_name)]
      | Maddfield (asset_name, field_name, _, _) ->
        let (pa,_,_) = Utils.get_container_asset_key model asset_name field_name in
        [APIAsset (Add pa); APIAsset (UpdateAdd (asset_name, field_name))]
      | Mremoveasset (asset_name, _) ->
        let ans : ident list =
          begin
            let asset = Utils.get_asset model asset_name in
            List.fold_left (fun accu (x : asset_item) ->
                match x.original_type with
                | Tcontainer (Tasset an, Partition) -> (unloc an)::accu
                | _ -> accu
              ) [] asset.values
          end
        in
        List.map (fun x -> APIAsset (Remove x)) (asset_name::ans)
      | Mremovefield (asset_name, field_name, _, _) ->
        let (pa,_,_) = Utils.get_container_asset_key model asset_name field_name in
        [APIAsset (Remove pa); APIAsset (UpdateRemove (asset_name, field_name))]
      | Mremoveall (asset_name, field_name, _) ->
        let (pa,_,_) = Utils.get_container_asset_key model asset_name field_name in
        [APIAsset (Get asset_name); APIAsset (Remove pa); APIAsset (UpdateRemove (asset_name, field_name)); APIAsset (RemoveAll (asset_name, field_name))]
      | Mclear (an , _) ->
        [APIAsset (Clear an)]
      | Mapifselect (asset_name, _, la, lb, _) ->
        [APIAsset (Get asset_name); APIAsset (Select (asset_name, Coll, la, lb))]
      | Mselect (asset_name, c, la, lb, _) ->
        [APIAsset (Get asset_name); APIAsset (Select (asset_name, to_ck c, la, lb))]
      | Mapifsort (asset_name, _, l) ->
        [APIAsset (Get asset_name); APIAsset (Sort (asset_name, Coll, l))]
      | Msort (asset_name, c, l) ->
        [APIAsset (Get asset_name); APIAsset (Sort (asset_name, to_ck c, l))]
      | Mapifcontains (asset_name, _, _) ->
        [APIAsset (Contains (asset_name, Coll))]
      | Mcontains (asset_name, c, _) ->
        [APIAsset (Contains (asset_name, to_ck c))]
      | Mapifnth (asset_name, _, _) ->
        [APIAsset (Get asset_name); APIAsset (Nth (asset_name, Coll))]
      | Mnth (asset_name, c, _) ->
        [APIAsset (Get asset_name); APIAsset (Nth (asset_name, to_ck c))]
      | Mapifcount (asset_name, _) ->
        [APIAsset (Count (asset_name, Coll))]
      | Mcount (asset_name, c) ->
        [APIAsset (Count (asset_name, to_ck c))]
      | Mapifsum (asset_name, _, p) ->
        [APIAsset (Get asset_name); APIAsset (Sum (asset_name, Coll, p.type_, p))]
      | Msum (asset_name, c, p) ->
        [APIAsset (Get asset_name); APIAsset (Sum (asset_name, to_ck c, p.type_, p))]
      | Mshallow (asset_name, _) ->
        [APIAsset (Shallow asset_name)]
      | Munshallow (asset_name, _) ->
        [APIAsset (Unshallow asset_name)]
      | Mlisttocoll (asset_name, _) ->
        [APIAsset (Listtocoll asset_name)]
      | Mapifhead (asset_name, _, _) ->
        [APIAsset (Head (asset_name, Coll))]
      | Mhead (asset_name, c, _) ->
        [APIAsset (Head (asset_name, to_ck c))]
      | Mapiftail (asset_name, _, _) ->
        [APIAsset (Tail (asset_name, Coll))]
      | Mtail (asset_name, c, _) ->
        [APIAsset (Tail (asset_name, to_ck c))]
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
      | Mfloor _ ->
        [APIBuiltin (Bfloor)]
      | Mceil _ ->
        [APIBuiltin (Bceil)]
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
                   | APIAsset (Get           an)         -> an
                   | APIAsset (Set           an)         -> an
                   | APIAsset (Add           an)         -> an
                   | APIAsset (Remove        an)         -> an
                   | APIAsset (Clear         an)         -> an
                   | APIAsset (Update       (an, _))     -> an
                   | APIAsset (UpdateAdd    (an, _))     -> an
                   | APIAsset (UpdateRemove (an, _))     -> an
                   | APIAsset (RemoveAll    (an, _))     -> an
                   | APIAsset (ToKeys        an)         -> an
                   | APIAsset (ColToKeys     an)         -> an
                   | APIAsset (Min          (an, _))     -> an
                   | APIAsset (Max          (an, _))     -> an
                   | APIList _                           -> default
                   | APIBuiltin _                        -> default
                   | APIInternal _                       -> default
                   | APIAsset (Shallow     an)           -> an
                   | APIAsset (Unshallow   an)           -> an
                   | APIAsset (Listtocoll  an)           -> an
                   | APIAsset (Contains   (an, _))       -> an
                   | APIAsset (Nth        (an, _))       -> an
                   | APIAsset (Select     (an, _, _, _)) -> an
                   | APIAsset (Sort       (an, _, _))    -> an
                   | APIAsset (Count      (an, _))       -> an
                   | APIAsset (Sum        (an, _, _, _)) -> an
                   | APIAsset (Head       (an, _))       -> an
                   | APIAsset (Tail       (an, _))       -> an
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
                   | APIAsset   (Nth           _) -> if verif then 6 else 9
                   | APIAsset   (Min           _) -> 7
                   | APIAsset   (Max           _) -> 8
                   | APIAsset   (Get           _) -> if verif then 9 else 6
                   | APIAsset   (Set           _) -> 10
                   | APIAsset   (Add           _) -> 11
                   | APIAsset   (Remove        _) -> 12
                   | APIAsset   (Clear         _) -> 13
                   | APIAsset   (Update        _) -> 14
                   | APIAsset   (UpdateAdd     _) -> 15
                   | APIAsset   (UpdateRemove  _) -> 16
                   | APIAsset   (RemoveAll     _) -> 17
                   | APIAsset   (ToKeys        _) -> 18
                   | APIAsset   (Shallow       _) -> 19
                   | APIAsset   (Unshallow     _) -> 20
                   | APIAsset   (Listtocoll    _) -> 21
                   | APIAsset   (ColToKeys     _) -> 22
                   | APIAsset   (Contains      _) -> 23
                   | APIAsset   (Select        _) -> 24
                   | APIAsset   (Sort          _) -> 25
                   | APIAsset   (Count         _) -> 26
                   | APIAsset   (Sum           _) -> 27
                   | APIAsset   (Head          _) -> 28
                   | APIAsset   (Tail          _) -> 29
                   | APIList    (Lprepend      _) -> 30
                   | APIList    (Lcontains     _) -> 31
                   | APIList    (Lcount        _) -> 32
                   | APIList    (Lnth          _) -> 33
                   | APIBuiltin (Bmin          _) -> 34
                   | APIBuiltin (Bmax          _) -> 35
                   | APIBuiltin (Babs          _) -> 36
                   | APIBuiltin (Bconcat       _) -> 37
                   | APIBuiltin (Bslice        _) -> 38
                   | APIBuiltin (Blength       _) -> 39
                   | APIBuiltin (Bisnone       _) -> 40
                   | APIBuiltin (Bissome       _) -> 41
                   | APIBuiltin (Bgetopt       _) -> 42
                   | APIBuiltin (Bfloor         ) -> 43
                   | APIBuiltin (Bceil          ) -> 44
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

