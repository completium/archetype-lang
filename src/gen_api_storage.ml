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
      | Mapifselect (asset_name, _, la, lb, _)
      | Mcselect (asset_name, la, lb, _) ->
        [APIAsset (Get asset_name); APIAsset (Cselect (asset_name, la, lb))]
      | Mvselect (asset_name, _, la, lb, _) ->
        [APIAsset (Get asset_name); APIAsset (Vselect (asset_name, la, lb))]
      | Mapifsort (asset_name, _, l)
      | Mcsort (asset_name, l) ->
        [APIAsset (Get asset_name); APIAsset (Csort (asset_name, l))]
      | Mvsort (asset_name, _, l) ->
        [APIAsset (Get asset_name); APIAsset (Vsort (asset_name, l))]
      | Mapifcontains (asset_name, _, _)
      | Mccontains (asset_name, _) ->
        [APIAsset (Ccontains asset_name)]
      | Mvcontains (asset_name, _, _) ->
        [APIAsset (Vcontains asset_name)]
      | Mapifnth (asset_name, _, _)
      | Mcnth (asset_name, _) ->
        [APIAsset (Get asset_name); APIAsset (Cnth asset_name)]
      | Mvnth (asset_name, _, _) ->
        [APIAsset (Get asset_name); APIAsset (Vnth asset_name)]
      | Mapifcount (asset_name, _)
      | Mccount (asset_name) ->
        [APIAsset (Ccount asset_name)]
      | Mvcount (asset_name, _) ->
        [APIAsset (Vcount asset_name)]
      | Mapifsum (asset_name, _, p)
      | Mcsum (asset_name, p) ->
        [APIAsset (Get asset_name); APIAsset (Csum (asset_name, p.type_, p))]
      | Mvsum (asset_name, _, p) ->
        [APIAsset (Get asset_name); APIAsset (Vsum (asset_name, p.type_, p))]
      | Mshallow (asset_name, _) ->
        [APIAsset (Shallow asset_name)]
      | Munshallow (asset_name, _) ->
        [APIAsset (Unshallow asset_name)]
      | Mlisttocoll (asset_name, _) ->
        [APIAsset (Listtocoll asset_name)]
      | Mapifhead (asset_name, _, _)
      | Mvhead (asset_name, _, _) ->
        [APIAsset (Vhead asset_name)]
      | Mchead (asset_name, _) ->
        [APIAsset (Chead asset_name)]
      | Mapiftail (asset_name, _, _)
      | Mctail (asset_name, _) ->
        [APIAsset (Ctail asset_name)]
      | Mvtail (asset_name, _, _) ->
        [APIAsset (Vtail asset_name)]
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
                   | APIAsset (Get           an)        -> an
                   | APIAsset (Set           an)        -> an
                   | APIAsset (Add           an)        -> an
                   | APIAsset (Remove        an)        -> an
                   | APIAsset (Clear         an)        -> an
                   | APIAsset (Update       (an, _))    -> an
                   | APIAsset (UpdateAdd    (an, _))    -> an
                   | APIAsset (UpdateRemove (an, _))    -> an
                   | APIAsset (RemoveAll    (an, _))    -> an
                   | APIAsset (ToKeys        an)        -> an
                   | APIAsset (ColToKeys     an)        -> an
                   | APIAsset (Min          (an, _))    -> an
                   | APIAsset (Max          (an, _))    -> an
                   | APIList _                          -> default
                   | APIBuiltin _                       -> default
                   | APIInternal _                      -> default
                   | APIAsset (Shallow       an)        -> an
                   | APIAsset (Unshallow     an)        -> an
                   | APIAsset (Listtocoll    an)        -> an
                   | APIAsset (Ccontains     an)        -> an
                   | APIAsset (Vcontains     an)        -> an
                   | APIAsset (Cnth          an)        -> an
                   | APIAsset (Vnth          an)        -> an
                   | APIAsset (Cselect      (an, _, _)) -> an
                   | APIAsset (Vselect      (an, _, _)) -> an
                   | APIAsset (Csort        (an, _))    -> an
                   | APIAsset (Vsort        (an, _))    -> an
                   | APIAsset (Ccount        an)        -> an
                   | APIAsset (Vcount        an)        -> an
                   | APIAsset (Csum         (an, _, _)) -> an
                   | APIAsset (Vsum         (an, _, _)) -> an
                   | APIAsset (Chead         an)        -> an
                   | APIAsset (Vhead         an)        -> an
                   | APIAsset (Ctail         an)        -> an
                   | APIAsset (Vtail         an)        -> an
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
                   | APIAsset   (Cnth          _) -> if verif then 6 else 12
                   | APIAsset   (Vnth          _) -> if verif then 7 else 13
                   | APIAsset   (Min           _) -> 8
                   | APIAsset   (Max           _) -> 9
                   | APIAsset   (Get           _) -> if verif then 10 else 6
                   | APIAsset   (Set           _) -> 11
                   | APIAsset   (Add           _) -> 12
                   | APIAsset   (Remove        _) -> 13
                   | APIAsset   (Clear         _) -> 14
                   | APIAsset   (Update        _) -> 15
                   | APIAsset   (UpdateAdd     _) -> 16
                   | APIAsset   (UpdateRemove  _) -> 17
                   | APIAsset   (RemoveAll     _) -> 18
                   | APIAsset   (ToKeys        _) -> 19
                   | APIAsset   (Shallow       _) -> 20
                   | APIAsset   (Unshallow     _) -> 21
                   | APIAsset   (Listtocoll    _) -> 22
                   | APIAsset   (ColToKeys     _) -> 23
                   | APIAsset   (Ccontains     _) -> 24
                   | APIAsset   (Vcontains     _) -> 25
                   | APIAsset   (Cselect       _) -> 26
                   | APIAsset   (Vselect       _) -> 27
                   | APIAsset   (Csort         _) -> 28
                   | APIAsset   (Vsort         _) -> 29
                   | APIAsset   (Ccount        _) -> 30
                   | APIAsset   (Vcount        _) -> 31
                   | APIAsset   (Csum          _) -> 32
                   | APIAsset   (Vsum          _) -> 33
                   | APIAsset   (Chead         _) -> 34
                   | APIAsset   (Vhead         _) -> 35
                   | APIAsset   (Ctail         _) -> 36
                   | APIAsset   (Vtail         _) -> 37
                   | APIList    (Lprepend      _) -> 38
                   | APIList    (Lcontains     _) -> 39
                   | APIList    (Lcount        _) -> 40
                   | APIList    (Lnth          _) -> 41
                   | APIBuiltin (Bmin          _) -> 42
                   | APIBuiltin (Bmax          _) -> 43
                   | APIBuiltin (Babs          _) -> 44
                   | APIBuiltin (Bconcat       _) -> 45
                   | APIBuiltin (Bslice        _) -> 46
                   | APIBuiltin (Blength       _) -> 47
                   | APIBuiltin (Bisnone       _) -> 48
                   | APIBuiltin (Bissome       _) -> 49
                   | APIBuiltin (Bgetopt       _) -> 50
                   | APIBuiltin (Bfloor         ) -> 51
                   | APIBuiltin (Bceil          ) -> 52
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

