open Tools

module A = Model
module M = Storage

type record = (A.lident, A.type_, A.pterm) A.decl_gen
type field = (A.lident, A.type_, A.pterm) A.decl_gen
type variable = (A.lident, A.type_, A.pterm) A.variable

let ast_to_model (ast : A.model) : M.model =
  let process_enums list =
    let process_enum (e : A.enum) : M.type_node =
      let e = M.mk_enum e.name in
      M.TNenum {
        e with
        values = List.map (fun x -> x) e.values;
      }
    in
    list @ List.map (fun x -> process_enum x) ast.enums in

  let process_records list =
    let process_asset (a : A.asset) : M.type_node =
      let e = M.mk_record a.name in
      M.TNrecord {
        e with
        values = List.map (fun (x : (A.lident, A.type_, A.pterm) A.decl_gen) -> M.mk_record_item x.name (get x.typ)) a.fields;
      }
    in
    list @ List.map (fun x -> process_asset x) ast.assets in

  let process_storage list =
    let variable_to_storage_items (var : variable) : M.storage_item =
      let arg = var.decl in
      let compute_fields (type_ : A.type_) : M.item_field list =
        match type_ with
        | Tbuiltin vtyp -> (
            let item_field =
              let m = M.mk_item_field arg.name (FBasic vtyp) in
              {m with default = arg.default} in
            [item_field])
        | Tasset id -> (
            let item_field =
              let m = M.mk_item_field arg.name (FRecord id) in
              {m with default = arg.default} in
            [item_field])
        | Tcontainer (ptyp, container) -> (
            let item =
              let f = function
                | A.Tbuiltin vtyp -> M.FBasic vtyp
                | _ -> assert false in
              let m = M.mk_item_field arg.name (FContainer (container, f ptyp)) in
              {m with default = arg.default} in
            [item]
          )
        (* | Ttuple l -> List.map (fun x -> compute_fields x) l |> List.flatten *)
        | _ -> assert false
      in

      let storage_item = M.mk_storage_item arg.name in
      let typ : A.type_ = get arg.typ in {
        storage_item with
        fields = compute_fields typ;
        init = [];
      }
    in

    let asset_to_storage_items (asset : A.asset) : M.storage_item =
      let id = asset.name in
      let compute_fields =
        let type_id : A.vtyp =
          let res : A.vtyp option = List.fold_left (fun accu (x : field) ->
              if String.equal (Location.unloc x.name) (Location.unloc (get asset.key))
              then (
                match x.typ with
                | Some (Tbuiltin v) -> Some v
                | _ -> accu
              )
              else accu
            ) None asset.fields in
          match res with
          | Some t -> t
          | _ -> get res
        in
        let mk a =
          let m = M.mk_item_field id a in
          {m with
           asset = Some id;
           (* default = arg.default; *)
          } in
        [mk (FKeyCollection (id, type_id)); mk (FRecordMap id)] in
      let item = M.mk_storage_item asset.name in
      {item with
       fields = compute_fields;
       invariants = asset.specs;
       (* init = asset.init; *)
      }
    in

    let cont f x l = List.map f x in
    []
    |> cont variable_to_storage_items ast.variables
    |> cont asset_to_storage_items ast.assets
    |> (fun x -> list @ [M.TNstorage x])
  in

  []
  |> process_enums
  |> process_records
  |> process_storage