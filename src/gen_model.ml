(* open Location *)
open Tools

module A = Ast
module M = Model

exception Anomaly of string
type error_desc =
  | NotSupportedContainer of string
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

type enum_item_struct = (A.lident, A.type_, A.pterm) A.enum_item_struct
type verification     = (A.lident, A.type_, A.pterm) A.verification
type record           = (A.lident, A.type_, A.pterm) A.decl_gen
type field            = (A.lident, A.type_, A.pterm) A.decl_gen
type variable         = (A.lident, A.type_, A.pterm) A.variable

let to_container c =
  match c with
  | A.Collection -> M.Collection
  | A.Partition  -> M.Partition
  | _            -> emit_error (NotSupportedContainer (Format.asprintf "%a@." A.pp_container c))

let to_currency = function
  | A.Tez   -> M.Tez
  | A.Mutez -> M.Mutez

let vtyp_to_btyp = function
  | A.VTbool       -> M.Bbool
  | A.VTint        -> M.Bint
  | A.VTuint       -> M.Buint
  | A.VTrational   -> M.Brational
  | A.VTdate       -> M.Bdate
  | A.VTduration   -> M.Bduration
  | A.VTstring     -> M.Bstring
  | A.VTaddress    -> M.Baddress
  | A.VTrole       -> M.Brole
  | A.VTcurrency c -> M.Bcurrency (to_currency c)
  | A.VTkey        -> M.Bkey

let rec ptyp_to_ptyp t : M.type_ =
  match t with
  | A.Tasset id          -> M.Tasset id
  | A.Tenum id           -> M.Tenum id
  | A.Tcontract id       -> M.Tcontract id
  | A.Tbuiltin b         -> M.Tbuiltin (vtyp_to_btyp b)
  | A.Tcontainer (t, c)  -> M.Tcontainer (ptyp_to_ptyp t, to_container c)
  | A.Ttuple l           -> M.Ttuple (List.map ptyp_to_ptyp l)

let to_vset = function
  | A.VSremoved -> M.VSremoved
  | A.VSadded   -> M.VSadded
  | A.VSstable  -> M.VSstable
  | A.VSbefore  -> M.VSbefore
  | A.VSafter   -> M.VSafter
  | A.VSfixed   -> M.VSfixed

let to_trtyp = function
  | A.TRentry  -> M.TRentry
  | A.TRaction -> M.TRaction
  | A.TRasset  -> M.TRasset
  | A.TRfield  -> M.TRfield

let rec ltyp_to_ptyp t : M.type_ =
  match t with
  | A.LTprog t      -> ptyp_to_ptyp t
  | A.LTvset (v, t) -> M.Tvset (to_vset v, ltyp_to_ptyp t)
  | A.LTtrace tr    -> M.Ttrace (to_trtyp tr)

let to_logical_operator = function
  | A.And   -> M.And
  | A.Or    -> M.Or
  | A.Imply -> M.Imply
  | A.Equiv -> M.Equiv

let to_comparison_operator = function
  | A.Equal  -> M.Equal
  | A.Nequal -> M.Nequal
  | A.Gt     -> M.Gt
  | A.Ge     -> M.Ge
  | A.Lt     -> M.Lt
  | A.Le     -> M.Le

let to_arithmetic_operator = function
  | A.Plus   -> M.Plus
  | A.Minus  -> M.Minus
  | A.Mult   -> M.Mult
  | A.Div    -> M.Div
  | A.Modulo -> M.Modulo

let to_unary_arithmetic_operator = function
  | A.Uplus  -> M.Uplus
  | A.Uminus -> M.Uminus

let to_assignment_operator = function
  | A.ValueAssign  -> M.ValueAssign
  | A.PlusAssign   -> M.PlusAssign
  | A.MinusAssign  -> M.MinusAssign
  | A.MultAssign   -> M.MultAssign
  | A.DivAssign    -> M.DivAssign
  | A.AndAssign    -> M.AndAssign
  | A.OrAssign     -> M.OrAssign

let to_operator = function
  | `Logical op -> `Logical (to_logical_operator op)
  | `Cmp     op -> `Cmp     (to_comparison_operator op)
  | `Arith   op -> `Arith   (to_arithmetic_operator op)
  | `Unary   op -> `Unary   (to_unary_arithmetic_operator op)
  | `Assign  op -> `Assign  (to_assignment_operator op)

let to_const = function
  | A.Cstate                      -> M.Cstate
  | A.Cnow                        -> M.Cnow
  | A.Ctransferred                -> M.Ctransferred
  | A.Ccaller                     -> M.Ccaller
  | A.Cfail                       -> M.Cfail
  | A.Cbalance                    -> M.Cbalance
  | A.Cconditions                 -> M.Cconditions
  | A.Cactions                    -> M.Cactions
  | A.Cnone                       -> M.Cnone
  | A.Cany                        -> M.Cany
  | A.Canyaction                  -> M.Canyaction
  | A.Cget                        -> M.Cget
  | A.Cadd                        -> M.Cadd
  | A.Caddnofail                  -> M.Caddnofail
  | A.Cremove                     -> M.Cremove
  | A.Cremovenofail               -> M.Cremovenofail
  | A.Cremoveif                   -> M.Cremoveif
  | A.Cupdate                     -> M.Cupdate
  | A.Cupdatenofail               -> M.Cupdatenofail
  | A.Cclear                      -> M.Cclear
  | A.Ccontains                   -> M.Ccontains
  | A.Cnth                        -> M.Cnth
  | A.Creverse                    -> M.Creverse
  | A.Cselect                     -> M.Cselect
  | A.Csort                       -> M.Csort
  | A.Ccount                      -> M.Ccount
  | A.Csum                        -> M.Csum
  | A.Cmax                        -> M.Cmax
  | A.Cmin                        -> M.Cmin
  | A.Cbefore                     -> M.Cbefore
  | A.Cunmoved                    -> M.Cunmoved
  | A.Cadded                      -> M.Cadded
  | A.Cremoved                    -> M.Cremoved
  | A.Citerated                   -> M.Citerated
  | A.Ctoiterate                  -> M.Ctoiterate
  | A.Cmaybeperformedonlybyrole   -> M.Cmaybeperformedonlybyrole
  | A.Cmaybeperformedonlybyaction -> M.Cmaybeperformedonlybyaction
  | A.Cmaybeperformedbyrole       -> M.Cmaybeperformedbyrole
  | A.Cmaybeperformedbyaction     -> M.Cmaybeperformedbyaction


let rec to_qualid_node (q : ('id, 'qualid) A.qualid_node) : ('id, 'qualid) M.qualid_node =
  match q with
  | A.Qident i    -> M.Qident i
  | A.Qdot (d, i) -> M.Qdot (to_qualid_gen d, i)

and  to_qualid_gen (q : A.qualid) : M.qualid =
  let node = to_qualid_node q.node in
  let type_ = ptyp_to_ptyp (Option.get q.type_) in
  M.mk_qualid node type_


let to_model (ast : A.model) : M.model =
  let process_enums list =
    let process_enum (e : A.enum) : 'id M.decl_node =
      let enum = M.mk_enum e.name in
      M.TNenum {
        enum with
        values = List.map (fun (x : enum_item_struct) ->
            let id : M.lident = x.name in
            let enum_item = M.mk_enum_item id in
            {
              enum_item with
              invariants = [] (* TODO : Option.map_dfl (fun (x : verification) -> x.specs) [] x.verification*);
            }
          ) e.items;
      }
    in
    list @ List.map (fun x -> process_enum x) ast.enums in

  let process_records list =
    let process_asset (a : A.asset) : 'id M.decl_node =
      let r : 'id M.record = M.mk_record a.name in
      let r = {
        r with
        key = a.key;
        values=(List.map (fun (x : (A.lident, A.type_, A.pterm) A.decl_gen) -> M.mk_record_item x.name (Option.get x.typ) ?default:x.default) a.fields);
      }
      in
      M.TNrecord r
    in
    list @ List.map (fun x -> process_asset x) ast.assets in

  let process_contracts list =
    list @ List.map (fun x -> M.TNcontract x) ast.contracts in

  let process_storage list =
    let variable_to_storage_items (var : variable) : 'id M.storage_item =
      let arg = var.decl in
      let compute_field (type_ : A.type_) : 'id M.item_field =
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

    let asset_to_storage_items (asset : A.asset) : 'id M.storage_item =
      let asset_name = asset.name in
      let compute_fields =
        let _, key_type = A.Utils.get_asset_key ast asset_name in
        let key_asset_name = Location.mkloc (Location.loc asset_name) ((Location.unloc asset_name) ^ "_keys") in
        let map_asset_name = Location.mkloc (Location.loc asset_name) ((Location.unloc asset_name) ^ "_assets") in
        [M.mk_item_field key_asset_name (FAssetKeys (key_type, asset_name))
           ~asset:asset_name
        (*?default:None TODO: uncomment this*);
         M.mk_item_field map_asset_name (FAssetRecord (key_type, asset_name))
           ~asset:asset_name
           (* ~default:arg.default TODO: uncomment this*)] in
      M.mk_storage_item asset.name ~fields:compute_fields ~invariants:asset.specs (*~init:asset.init TODO: uncomment this *)
    in

    let cont f x l = l @ (List.map f x) in
    []
    |> cont variable_to_storage_items ast.variables
    |> cont asset_to_storage_items ast.assets
    |> (fun x -> list @ [M.TNstorage x])
  in

  let process_functions list : 'id M.decl_node list =
    let extract_function_from_instruction (instr : A.instruction) (list : 'id  M.decl_node list) : (A.instruction * 'id M.decl_node list) =
      let add l i =
        let e = List.fold_left (fun accu x ->
            if x = i
            then true
            else accu) false l in
        if e then
          l
        else
          i::l
      in
      let mk_function t field_name c (e : (A.lident, A.pterm) A.term_arg option) : ('id M.function__ * (A.lident, A.pterm) A.term_arg option) option =
        let is_global_asset (asset_name : A.lident) (e : (A.lident, A.pterm) A.term_arg option) =
          match e with
          | Some AExpr {node = Pvar {pldesc = id; _}; _} when String.equal (Location.unloc asset_name) id -> true
          | _ -> false
        in
        let get_first_arg asset_name (e : (A.lident, A.pterm) A.term_arg option) : (A.lident, A.pterm) A.term_arg option =
          if (is_global_asset asset_name e)
          then None
          else e
        in
        let node = match t, field_name, c, e with
          | A.Tcontainer (Tasset asset, Collection), None, A.Cget,      _ when is_global_asset asset e -> Some (M.Get asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cadd,      _ when is_global_asset asset e -> Some (M.AddAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cremove,   _ when is_global_asset asset e -> Some (M.RemoveAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cclear,    _ when is_global_asset asset e -> Some (M.ClearAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cupdate,   _ when is_global_asset asset e -> Some (M.UpdateAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Ccontains, _ when is_global_asset asset e -> Some (M.ContainsAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cnth,      _ when is_global_asset asset e -> Some (M.NthAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cselect,   _ when is_global_asset asset e -> Some (M.SelectAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Creverse,  _ when is_global_asset asset e -> Some (M.ReverseAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Csort,     _ when is_global_asset asset e -> Some (M.SortAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Ccount,    _ when is_global_asset asset e -> Some (M.CountAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Csum,      _ when is_global_asset asset e -> Some (M.SumAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cmin,      _ when is_global_asset asset e -> Some (M.MinAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cmax,      _ when is_global_asset asset e -> Some (M.MaxAsset asset, get_first_arg asset e)
          | A.Tasset asset, Some field, A.Cadd,      Some AExpr {node = A.Pdot (a, _)}  -> Some (M.AddContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Cremove,   Some AExpr {node = A.Pdot (a, _)}  -> Some (M.RemoveContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Cclear,    Some AExpr {node = A.Pdot (a, _)}  -> Some (M.ClearContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Ccontains, Some AExpr {node = A.Pdot (a, _)}  -> Some (M.ContainsContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Cnth,      Some AExpr {node = A.Pdot (a, _)}  -> Some (M.NthContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Cselect,   Some AExpr {node = A.Pdot (a, _)}  -> Some (M.SelectContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Creverse,  Some AExpr {node = A.Pdot (a, _)}  -> Some (M.ReverseContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Csort,     Some AExpr {node = A.Pdot (a, _)}  -> Some (M.SortContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Ccount,    Some AExpr {node = A.Pdot (a, _)}  -> Some (M.CountContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Csum,      Some AExpr {node = A.Pdot (a, _)}  -> Some (M.SumContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Cmax,      Some AExpr {node = A.Pdot (a, _)}  -> Some (M.MaxContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Cmin,      Some AExpr {node = A.Pdot (a, _)}  -> Some (M.MinContainer (asset, field), Some (AExpr a))
          | _ -> None in
        match node with
        | Some (node, x) -> Some (M.mk_function node, x)
        | _ -> None
      in

      let ge (e : A.pterm) = (fun node -> {e with node = node}) in

      let rec fe accu (term : A.pterm) : A.pterm * 'id M.decl_node list =
        match term.node with
        | A.Pcall (Some asset_name, Cconst c, args) -> (
            let _, accu = A.fold_map_term (fun node -> {term with node = node} ) fe accu term in
            let function__ = mk_function (A.Tcontainer (Tasset asset_name, Collection)) None c (Some (AExpr (A.mk_sp (A.Pvar asset_name)))) in
            let term, accu =
              match function__ with
              | Some (f, _) -> (
                  let node = f.node in
                  let fun_name = Location.dumloc (M.function_name_from_function_node node) in
                  {term with node = A.Pcall (None, Cid fun_name, args) }, add accu (M.TNfunction f)
                )
              | None -> term, accu in
            term, accu
          )
        | _ -> A.fold_map_term (ge term) fe accu term in

      let process_instr accu t (c : A.const) field_name gi fi node (args : (A.lident, A.pterm) A.term_arg list) =
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
            (fun (pterms, accu) arg ->
               match arg with
               | A.AExpr x ->
                 let p, accu = fe accu x in
                 [A.AExpr p] @ pterms, accu
               | _ -> (pterms, accu) (*TODO*))
            ([], xa) args
        in

        let first_arg = Option.map (fun x -> A.AExpr x) xe in
        let function__ = mk_function (Option.get t) field_name c first_arg in
        let instr, accu =
          match function__ with
          | Some (f, arg) -> (
              let new_args =
                match argss, arg with
                | _, Some arg -> arg::argss
                | _ -> argss in
              let node = f.node in
              let fun_name = Location.dumloc (M.function_name_from_function_node node) in
              {instr with node = A.Icall (None, Cid fun_name, new_args) }, add argsa (M.TNfunction f)
            )
          | None -> instr, accu in
        instr, accu in

      let rec fi accu (instr : A.instruction) : A.instruction * 'id M.decl_node list =
        let gi = (fun node -> {instr with node = node}) in
        match instr.node with
        | A.Icall (Some {node = A.Pdot ({type_ = t; _}, id); _}, Cconst c, args) ->
          process_instr accu t c (Some id) gi fi instr.node args

        | A.Icall (Some {type_ = t; _}, Cconst c, args) ->
          process_instr accu t c None gi fi instr.node args

        | _ ->
          A.fold_map_instr_term (fun node -> { instr with node = node } ) ge fi fe accu instr

      in
      fi list instr in

    let cont f x l = List.fold_left (fun accu x -> f x accu) l x in

    let process_fun_gen name args body loc verif f (list : 'id M.decl_node list) : 'id M.decl_node list =
      let instr, list = extract_function_from_instruction body list in
      let node = f (M.mk_function_struct name instr
                      ~args:(List.map (fun (x : ('id, 'typ, 'term) A.decl_gen) -> (x.name, Option.get x.typ, None)) args)
                      ~loc:loc) in
      list @ [TNfunction (M.mk_function ?verif:verif node)]
    in

    let process_function (function_ : A.function_) (list : 'id M.decl_node list) : 'id M.decl_node list =
      let name  = function_.name in
      let args  = function_.args in
      let body  = function_.body in
      let loc   = function_.loc in
      let ret   = function_.return in
      let verif = function_.verification in
      process_fun_gen name args body loc verif (fun x -> M.Function (x, ret)) list
    in

    let process_transaction (transaction : A.transaction) (list : 'id M.decl_node list) : 'id M.decl_node list =
      let list  = list |> cont process_function ast.functions in
      let name  = transaction.name in
      let args  = transaction.args in
      let body  = Option.get transaction.effect in
      let loc   = transaction.loc in
      let verif = transaction.verification in
      process_fun_gen name args body loc verif (fun x -> M.Entry x) list
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
