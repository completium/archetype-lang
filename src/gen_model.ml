open Tools

module A = Ast
module M = Model

exception Anomaly

type enum_item_struct = (A.lident, A.type_, A.pterm) A.enum_item_struct
type verification     = (A.lident, A.type_, A.pterm) A.verification
type record           = (A.lident, A.type_, A.pterm) A.decl_gen
type field            = (A.lident, A.type_, A.pterm) A.decl_gen
type variable         = (A.lident, A.type_, A.pterm) A.variable

let to_model (ast : A.model) : M.model =
  let process_enums list =
    let process_enum (e : A.enum) : M.decl_node =
      let enum = M.mk_enum e.name in
      M.TNenum {
        enum with
        values = List.map (fun (x : enum_item_struct) ->
            let id : M.lident = x.name in
            let enum_item = M.mk_enum_item id in
            {
              enum_item with
              invariants = Option.map_dfl (fun (x : verification) -> x.specs) [] x.verification;
            }
          ) e.items;
      }
    in
    list @ List.map (fun x -> process_enum x) ast.enums in

  let process_records list =
    let process_asset (a : A.asset) : M.decl_node =
      let e = M.mk_record a.name in
      M.TNrecord {
        e with
        values = List.map (fun (x : (A.lident, A.type_, A.pterm) A.decl_gen) -> M.mk_record_item x.name (Option.get x.typ)) a.fields;
      }
    in
    list @ List.map (fun x -> process_asset x) ast.assets in

  let process_contracts list =
    list @ List.map (fun x -> M.TNcontract x) ast.contracts in

  let process_storage list =
    let variable_to_storage_items (var : variable) : M.storage_item =
      let arg = var.decl in
      let compute_field (type_ : A.type_) : M.item_field =
        let rec ptyp_to_item_field_type = function
          | A.Tbuiltin vtyp -> M.FBasic vtyp
          | A.Tenum id      -> M.FEnum id
          | A.Tasset id     -> M.FRecord id
          | A.Tcontract x   -> M.FBasic VTrole
          | A.Tcontainer (ptyp, container) -> M.FContainer (container, ptyp_to_item_field_type ptyp)
          | A.Ttuple _      -> assert false
        in
        let a = ptyp_to_item_field_type type_ in
        M.mk_item_field arg.name a ?default:arg.default
      in

      let storage_item = M.mk_storage_item arg.name in
      let typ : A.type_ = Option.get arg.typ in {
        storage_item with
        fields = [compute_field typ];
        init = [];
      }
    in

    let asset_to_storage_items (asset : A.asset) : M.storage_item =
      let id = asset.name in
      let compute_fields =
        let type_id : A.vtyp =
          let res : A.vtyp option = List.fold_left (fun accu (x : field) ->
              if String.equal (Location.unloc x.name) ((Location.unloc |@ Option.get) asset.key)
              then (
                match x.typ with
                | Some (Tbuiltin v) -> Some v
                | _ -> accu
              )
              else accu
            ) None asset.fields in
          match res with
          | Some t -> t
          | _ -> Option.get res
        in
        let asset_name = id in
        let keys_id = Location.mkloc (Location.loc asset_name) ((Location.unloc asset_name) ^ "_keys") in
        let asset_name = Location.mkloc (Location.loc asset_name) ((Location.unloc asset_name) ^ "_assets") in
        [M.mk_item_field id (FKeyCollection (keys_id, type_id)) ~asset:id (* ~default:arg.default TODO: uncomment this*);
         M.mk_item_field id (FRecordMap asset_name) ~asset:id (* ~default:arg.default TODO: uncomment this*)] in
      M.mk_storage_item asset.name ~fields:compute_fields ~invariants:asset.specs (*~init:asset.init TODO: uncomment this *)
    in

    let cont f x l = l @ (List.map f x) in
    []
    |> cont variable_to_storage_items ast.variables
    |> cont asset_to_storage_items ast.assets
    |> (fun x -> list @ [M.TNstorage x])
  in

  let process_functions list : M.decl_node list =
    let extract_function_from_instruction (instr : A.instruction) (list : M.decl_node list) : (A.instruction * M.decl_node list) =
      let add l i = i::l in (* if i exists in l then ignore *)
      let mk_function t field_name c : M.function__ option =
        let node = match t, field_name, c with
          | A.Tcontainer (Tasset asset, Collection), None,    A.Cget      -> Some (M.Get asset)
          | A.Tcontainer (Tasset asset, Collection), None,    A.Cadd      -> Some (M.AddAsset asset)
          | A.Tcontainer (Tasset asset, Collection), None,    A.Cremove   -> Some (M.RemoveAsset asset)
          | A.Tcontainer (Tasset asset, Collection), None,    A.Cclear    -> Some (M.ClearAsset asset)
          | A.Tcontainer (Tasset asset, Collection), None,    A.Cupdate   -> Some (M.UpdateAsset asset)
          | A.Tcontainer (Tasset asset, Collection), None,    A.Ccontains -> Some (M.ContainsAsset asset)
          | A.Tcontainer (Tasset asset, Collection), None,    A.Cnth      -> Some (M.NthAsset asset)
          | A.Tcontainer (Tasset asset, Collection), None,    A.Cselect   -> Some (M.SelectAsset asset)
          | A.Tcontainer (Tasset asset, Collection), None,    A.Creverse  -> Some (M.SortAsset asset)
          | A.Tcontainer (Tasset asset, Collection), None,    A.Csort     -> Some (M.SortAsset asset)
          | A.Tcontainer (Tasset asset, Collection), None,    A.Ccount    -> Some (M.CountAsset asset)
          | A.Tcontainer (Tasset asset, Collection), None,    A.Csum      -> Some (M.SumAsset asset)
          | A.Tcontainer (Tasset asset, Collection), None,    A.Cmin      -> Some (M.MinAsset asset)
          | A.Tcontainer (Tasset asset, Collection), None,    A.Cmax      -> Some (M.MaxAsset asset)
          | A.Tasset asset, Some field, A.Cadd                            -> Some (M.AddContainer (asset, field))
          | A.Tasset asset, Some field, A.Cremove                         -> Some (M.RemoveContainer (asset, field))
          | A.Tasset asset, Some field, A.Cclear                          -> Some (M.ClearContainer (asset, field))
          | A.Tasset asset, Some field, A.Ccontains                       -> Some (M.ContainsContainer (asset, field))
          | A.Tasset asset, Some field, A.Cnth                            -> Some (M.NthContainer (asset, field))
          | A.Tasset asset, Some field, A.Cselect                         -> Some (M.SelectContainer (asset, field))
          | A.Tasset asset, Some field, A.Creverse                        -> Some (M.ReverseContainer (asset, field))
          | A.Tasset asset, Some field, A.Csort                           -> Some (M.SortContainer (asset, field))
          | A.Tasset asset, Some field, A.Ccount                          -> Some (M.CountContainer (asset, field))
          | A.Tasset asset, Some field, A.Csum                            -> Some (M.SumContainer (asset, field))
          | A.Tasset asset, Some field, A.Cmax                            -> Some (M.MaxContainer (asset, field))
          | A.Tasset asset, Some field, A.Cmin                            -> Some (M.MinContainer (asset, field))
          | _ -> None in
        Option.map (fun node ->
            let ret = t (*TODO: get_type node*) in
            let sig_ : M.signature = M.mk_signature (Location.dumloc (M.function_name_from_function_node node)) ~ret:ret in
            M.mk_function node sig_) node in

      let ge (e : ('a, 'b) A.struct_poly) = (fun node -> {e with node = node}) in

      let rec fe accu (term : A.pterm) : A.pterm * M.decl_node list =
        match term.node with
        | A.Pcall (Some asset_name, Cconst c, args) -> (
            let _, accu = A.fold_map_term (fun node -> {term with node = node} ) fe accu term in
            let function__ = mk_function (A.Tcontainer (Tasset asset_name, Collection)) None c in
            let term, accu =
              match function__ with
              | Some f -> (
                  let node = f.node in
                  let fun_name = Location.dumloc (M.function_name_from_function_node node) in
                  {term with node = A.Pcall (None, Cid fun_name, args) }, add accu (M.TNfunction f)
                )
              | None -> term, accu in
            term, accu
          )
        | _ -> A.fold_map_term (ge term) fe accu term in

      let process_instr accu t (c : A.const) field_name gi fi node args =
        let a =
          match node with
          | A.Icall (a, _, _) -> a
          | _ -> raise Anomaly in

        let xe, xa =
          match a with
          | Some x -> fe accu x |> (fun (a, b) -> (Some a, b))
          | None -> None, accu in

        let (argss, argsa) =
          List.fold_left
            (fun (pterms, accu) x ->
               let p, accu = fe accu x in
               [p] @ pterms, accu) ([], xa) args
        in

        let new_args = Option.map_dfl (fun x -> x::argss) argss xe in

        let function__ = mk_function (Option.get t) field_name c in
        let instr, accu =
          match function__ with
          | Some f -> (
              let node = f.node in
              let fun_name = Location.dumloc (M.function_name_from_function_node node) in
              {instr with node = A.Icall (None, Cid fun_name, new_args) }, add argsa (M.TNfunction f)
            )
          | None -> instr, accu in
        instr, accu in

      let rec fi accu (instr : A.instruction) : A.instruction * M.decl_node list =
        let gi = (fun node -> {instr with node = node}) in
        match instr.node with
        | A.Icall (Some {node = A.Pdot ({type_ = t; _}, id); _}, Cconst c, args) ->
          process_instr accu t c (Some id) gi fi instr.node args

        | A.Icall (Some {type_ = t; _}, Cconst c, args) ->
          process_instr accu t c None gi fi instr.node args

        | _ ->
          A.fold_map_instr_term (fun node -> { instr with node = node} ) ge fi fe accu instr

      in
      fi list instr in

    let cont f x l = List.fold_left (fun accu x -> f x accu) l x in

    let process_fun_gen name body loc verif f (list : M.decl_node list) : M.decl_node list =
      let instr, list = extract_function_from_instruction body list in
      let sig_ = M.mk_signature name (*TODO: put arguments *) in
      let node = f (M.mk_function_struct name instr ~loc:loc) in
      list @ [TNfunction (M.mk_function ?verif:verif node sig_)]
    in

    let process_function (function_ : A.function_) (list : M.decl_node list) : M.decl_node list =
      let name  = function_.name in
      let body  = function_.body in
      let loc   = function_.loc in
      let verif = function_.verification in
      process_fun_gen name body loc verif (fun x -> M.Function x) list
    in

    let process_transaction (transaction : A.transaction) (list : M.decl_node list) : M.decl_node list =
      let list  = list |> cont process_function ast.functions in
      let name  = transaction.name in
      let body  = Option.get transaction.effect in
      let loc   = transaction.loc in
      let verif = transaction.verification in
      process_fun_gen name body loc verif (fun x -> M.Entry x) list
    in

    list
    |> cont process_function ast.functions
    |> cont process_transaction ast.transactions
  in

  let name = ast.name in
  let decls =
    []
    |> process_enums
    |> process_records
    |> process_contracts
    |> process_storage
    |> process_functions
  in
  M.mk_model name ~decls:decls
