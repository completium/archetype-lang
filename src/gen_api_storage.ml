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
    | CKcoll              -> Coll
    | CKview _            -> View
    | CKfield (an, fn, _) -> Field (an, fn)
  in

  let rec f (ctx : ctx_model) (accu : api_storage list) (term : mterm) : api_storage list =
    let api_items : api_storage_node list =
      let mt_type = term.type_ in
      let is_rat = match mt_type with | Tbuiltin Brational | Ttuple [Tbuiltin Bint; Tbuiltin Bint] -> true | _ -> false in
      let extract_option_type = function | Toption x -> x | _ -> assert false in
      match term.node with
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
        [APIAsset (Add pa); APIAsset (FieldAdd (asset_name, field_name))]
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
        [APIAsset (Remove pa); APIAsset (FieldRemove (asset_name, field_name))]
      | Mremoveall (asset_name, field_name, _) ->
        let (pa,_,_) = Utils.get_container_asset_key model asset_name field_name in
        [APIAsset (Get asset_name); APIAsset (Remove pa); APIAsset (FieldRemove (asset_name, field_name)); APIAsset (RemoveAll (asset_name, field_name))]
      | Mremoveif (asset_name, (CKcoll as c), la, lb, _) ->
        [APIAsset (Get asset_name); APIAsset (Remove asset_name); APIAsset (RemoveIf (asset_name, to_ck c, la, lb))]
      | Mremoveif (asset_name, ((CKfield (_, _, {node = Mdot ({type_ = Tasset aan}, fn)})) as c), la, lb, _) ->
        Format.printf "mt: %a@." pp_mterm term;
        let fn = unloc fn in
        let aan = unloc aan in
        (* let (pa,_,_) = Utils.get_container_asset_key model (unloc aan) fn in *)
        (* APIAsset (Remove aan);  *)
        [APIAsset (Get asset_name); APIAsset (FieldRemove (aan, fn)); APIAsset (RemoveIf (aan, to_ck c, la, lb))]
      | Mclear (an , c) ->
        [APIAsset (Clear (an, to_ck c))]
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
      | Mdivtez _ ->
        [APIInternal (DivTez)]
      | Mratdur _ ->
        [APIInternal (RatDur)]
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
                   | APIAsset (Clear        (an, _))     -> an
                   | APIAsset (Update       (an, _))     -> an
                   | APIAsset (FieldAdd    (an, _))      -> an
                   | APIAsset (FieldRemove (an, _))      -> an
                   | APIAsset (RemoveAll  (an, _))       -> an
                   | APIAsset (RemoveIf   (an, _, _, _)) -> an
                   | APIList _                           -> default
                   | APIBuiltin _                        -> default
                   | APIInternal _                       -> default
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
                   | APIInternal (DivTez        ) ->  6
                   | APIInternal (RatDur        ) ->  7
                   | APIAsset   (Nth           _) -> if verif then 8 else 9
                   | APIAsset   (Get           _) -> if verif then 9 else 8
                   | APIAsset   (Set           _) -> 10
                   | APIAsset   (Add           _) -> 11
                   | APIAsset   (Remove        _) -> 12
                   | APIAsset   (Clear         _) -> 13
                   | APIAsset   (Update        _) -> 14
                   | APIAsset   (FieldAdd      _) -> 15
                   | APIAsset   (FieldRemove   _) -> 16
                   | APIAsset   (RemoveAll     _) -> 17
                   | APIAsset   (RemoveIf      _) -> 18
                   | APIAsset   (Contains      _) -> 19
                   | APIAsset   (Select        _) -> 20
                   | APIAsset   (Sort          _) -> 21
                   | APIAsset   (Count         _) -> 22
                   | APIAsset   (Sum           _) -> 23
                   | APIAsset   (Head          _) -> 24
                   | APIAsset   (Tail          _) -> 25
                   | APIList    (Lprepend      _) -> 26
                   | APIList    (Lcontains     _) -> 27
                   | APIList    (Lcount        _) -> 28
                   | APIList    (Lnth          _) -> 29
                   | APIBuiltin (Bmin          _) -> 30
                   | APIBuiltin (Bmax          _) -> 31
                   | APIBuiltin (Babs          _) -> 32
                   | APIBuiltin (Bconcat       _) -> 33
                   | APIBuiltin (Bslice        _) -> 34
                   | APIBuiltin (Blength       _) -> 35
                   | APIBuiltin (Bisnone       _) -> 36
                   | APIBuiltin (Bissome       _) -> 37
                   | APIBuiltin (Bgetopt       _) -> 38
                   | APIBuiltin (Bfloor         ) -> 39
                   | APIBuiltin (Bceil          ) -> 40
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

