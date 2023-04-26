(* open Location *)
open Model

exception Anomaly of string
type error_desc =
  | NoInitExprFor of string
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

let generate_storage (model : model) : model =

  let asset_to_storage_items (asset : asset) : storage_item =
    let asset_name = asset.name in
    let typ_ = tcollection asset_name in
    mk_storage_item
      ~no_storage:asset.no_storage
      ?namespace:(asset.name |> fst |> Option.map Location.unloc)
      asset_name
      (MTasset asset_name)
      typ_
      (mk_mterm (Massets asset.init) typ_)
  in

  let state_to_storage_items (e : enum) : storage_item list =
    match e with
    | _ when String.equal (unloc_mident e.name) "state" ->
      begin
        let iv = e.initial in
        let dv = mk_mterm (Mvar (iv, Vlocal)) tstate in
        [ mk_storage_item e.name MTstate tstate dv ]
      end
    | _ -> []
  in

  let variable_to_storage_items (var : var) : storage_item =

    let init_default_value ty =
      match get_ntype ty with
      | Tbuiltin Bunit           -> mk_mterm (Munit) ty
      | Tbuiltin Bbool           -> mk_mterm (Mbool false) ty
      | Tbuiltin Bint            -> mk_mterm (Mint (Big_int.zero_big_int)) ty
      | Tbuiltin Brational       -> mk_mterm (Mrational (Big_int.zero_big_int, Big_int.unit_big_int)) ty
      | Tbuiltin Bdate           -> emit_error (NoInitExprFor "date")
      | Tbuiltin Bduration       -> mk_mterm (Mduration (Core.mk_duration ())) ty
      | Tbuiltin Btimestamp      -> emit_error (NoInitExprFor "timestamp")
      | Tbuiltin Bstring         -> mk_mterm (Mstring "") ty
      | Tbuiltin Baddress        -> emit_error (NoInitExprFor "address")
      | Tbuiltin Btez            -> mk_mterm (Mmutez (Big_int.zero_big_int)) ty
      | Tbuiltin Bkey            -> emit_error (NoInitExprFor "key")
      | Tbuiltin Bkeyhash        -> emit_error (NoInitExprFor "key_hash")
      | Tbuiltin Bsignature      -> emit_error (NoInitExprFor "signature")
      | Tbuiltin Bbytes          -> mk_mterm (Mbytes ("0x0")) ty
      | Tbuiltin Bnat            -> mk_mterm (Mint (Big_int.zero_big_int)) ty
      | Tbuiltin Bchainid        -> emit_error (NoInitExprFor "chainid")
      | Tcontainer _             -> mk_mterm (Massets []) ty
      | Tlist _                  -> mk_mterm (Mlitlist []) ty
      | Toption _                -> mk_mterm (Mnone) ty
      | Tasset v
      | Tenum v                  -> emit_error (NoInitExprFor (unloc_mident v))
      | Ttuple _                 -> emit_error (NoInitExprFor "tuple")
      | Tset _                   -> mk_mterm   (Mlitset []) ty
      | Tmap (_, _)              -> mk_mterm   (Mlitmap (MKMap, [])) ty
      | Tbig_map (_, _)          -> mk_mterm   (Mlitmap (MKBigMap, [])) ty
      | Titerable_big_map (_, _) -> mk_mterm   (Mlitmap (MKIterableBigMap, [])) ty
      | Tor _                    -> emit_error (NoInitExprFor "or")
      | Trecord _                -> emit_error (NoInitExprFor "record")
      | Tevent _                 -> emit_error (NoInitExprFor "event")
      | Tlambda _                -> emit_error (NoInitExprFor "lambda")
      | Tunit                    -> emit_error (NoInitExprFor "unit")
      | Toperation               -> emit_error (NoInitExprFor "operation")
      | Tcontract _              -> emit_error (NoInitExprFor "contract")
      | Tstate                   -> emit_error (NoInitExprFor "state")
      | Tticket _                -> emit_error (NoInitExprFor "ticket")
      | Tsapling_state n         -> mk_mterm   (MsaplingStateEmpty n) ty
      | Tsapling_transaction _   -> emit_error (NoInitExprFor "sapling_transaction")
      | Tbuiltin Bbls12_381_fr   -> mk_mterm (Mint (Big_int.zero_big_int)) ty
      | Tbuiltin Bbls12_381_g1   -> mk_mterm (Mint (Big_int.zero_big_int)) ty
      | Tbuiltin Bbls12_381_g2   -> mk_mterm (Mint (Big_int.zero_big_int)) ty
      | Tbuiltin Bnever          -> emit_error (NoInitExprFor "never")
      | Tbuiltin Bchest          -> emit_error (NoInitExprFor "chest")
      | Tbuiltin Bchest_key      -> emit_error (NoInitExprFor "chest_key")
    in

    let constant = match var.kind with | VKconstant -> true | _ -> false in
    let (name, type_, dv) = var.name, var.type_, var.default in
    let mt = if constant then MTconst else MTvar in
    let dv =
      match dv with
      | Some v -> v
      | None   -> init_default_value type_
    in
    mk_storage_item name mt type_ dv ~const:constant
  in

  let process_storage_item d : storage_item list =
    match d with
    | Dvar v      -> [variable_to_storage_items v]
    | Denum e     -> state_to_storage_items e
    | Dasset a    -> [asset_to_storage_items a]
    | Drecord _   -> []
    | Devent  _   -> []
  in

  let storage = List.map process_storage_item model.decls |> List.flatten in

  let process_mterm (model : model) : model =
    let rec aux c (mt : mterm) : mterm =
      match mt.node with
      | Massign (op, t, Avar id, v) when Model.Utils.is_field_storage model (unloc_mident id) ->
        begin
          let vv = aux c v in
          mk_mterm (Massign (op, t, Avarstore id, vv)) tunit
        end
      | Mvar (id, Vlocal) when Model.Utils.is_field_storage model (unloc_mident id) ->
        mk_mterm (Mvar (id, Vstorevar)) mt.type_
      | _ -> map_mterm (aux c) mt
    in
    Model.map_mterm_model aux model
  in

  let model = {
    model with
    storage = storage;
  } in
  process_mterm model
