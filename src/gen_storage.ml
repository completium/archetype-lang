open Location
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
      asset_name
      (MTasset (unloc asset_name))
      typ_
      (mk_mterm (Massets asset.init) typ_)
  in

  let state_to_storage_items (e : enum) : storage_item list =
    match e with
    | _ when String.equal (unloc e.name) "state" ->
      begin
        let iv = e.initial in
        let dv = mk_mterm (Mvar (iv, Vlocal, Tnone, Dnone)) tstate in
        [ mk_storage_item e.name MTstate tstate dv ]
      end
    | _ -> []
  in

  let variable_to_storage_items (var : var) : storage_item =

    let init_default_value ty =
      match get_ntype ty with
      | Tbuiltin Bunit       -> mk_mterm (Munit) ty
      | Tbuiltin Bbool       -> mk_mterm (Mbool false) ty
      | Tbuiltin Bint        -> mk_mterm (Mint (Big_int.zero_big_int)) ty
      | Tbuiltin Brational   -> mk_mterm (Mrational (Big_int.zero_big_int, Big_int.unit_big_int)) ty
      | Tbuiltin Bdate       -> emit_error (NoInitExprFor "date")
      | Tbuiltin Bduration   -> mk_mterm (Mduration (Core.mk_duration ())) ty
      | Tbuiltin Btimestamp  -> emit_error (NoInitExprFor "timestamp")
      | Tbuiltin Bstring     -> mk_mterm (Mstring "") ty
      | Tbuiltin Baddress    -> emit_error (NoInitExprFor "address")
      | Tbuiltin Brole       -> emit_error (NoInitExprFor "role")
      | Tbuiltin Bcurrency   -> mk_mterm (Mcurrency (Big_int.zero_big_int, Utz)) ty
      | Tbuiltin Bkey        -> emit_error (NoInitExprFor "key")
      | Tbuiltin Bkeyhash    -> emit_error (NoInitExprFor "key_hash")
      | Tbuiltin Bsignature  -> emit_error (NoInitExprFor "signature")
      | Tbuiltin Bbytes      -> mk_mterm (Mbytes ("0x0")) ty
      | Tbuiltin Bnat        -> mk_mterm (Mint (Big_int.zero_big_int)) ty
      | Tbuiltin Bchainid    -> emit_error (NoInitExprFor "chainid")
      | Tcontainer _         -> mk_mterm (Massets []) ty
      | Tlist _              -> mk_mterm (Mlitlist []) ty
      | Toption _            -> mk_mterm (Mnone) ty
      | Tasset v
      | Tenum v              -> emit_error (NoInitExprFor (unloc v))
      | Ttuple _             -> emit_error (NoInitExprFor "tuple")
      | Tset _               -> mk_mterm   (Mlitset []) ty
      | Tmap (b, _, _)       -> mk_mterm   (Mlitmap (b, [])) ty
      | Tor _                -> emit_error (NoInitExprFor "or")
      | Trecord _            -> emit_error (NoInitExprFor "record")
      | Tlambda _            -> emit_error (NoInitExprFor "lambda")
      | Tunit                -> emit_error (NoInitExprFor "unit")
      | Tstorage             -> emit_error (NoInitExprFor "storage")
      | Toperation           -> emit_error (NoInitExprFor "operation")
      | Tcontract _          -> emit_error (NoInitExprFor "contract")
      | Tprog _              -> emit_error (NoInitExprFor "prog")
      | Tvset _              -> emit_error (NoInitExprFor "vset")
      | Ttrace _             -> emit_error (NoInitExprFor "trace")
      | Tstate               -> emit_error (NoInitExprFor "state")
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
  in

  let storage = List.map process_storage_item model.decls |> List.flatten in

  let process_mterm (model : model) : model =
    let rec aux c (mt : mterm) : mterm =
      match mt.node with
      | Massign (op, t, Avar id, v) when Model.Utils.is_field_storage model (unloc id) ->
        begin
          let vv = aux c v in
          mk_mterm (Massign (op, t, Avarstore id, vv)) tunit
        end
      | Mvar (id, Vlocal, t, d) when Model.Utils.is_field_storage model (unloc id) ->
        mk_mterm (Mvar (id, Vstorevar, t, d)) mt.type_
      | _ -> map_mterm (aux c) mt
    in
    Model.map_mterm_model aux model
  in

  let model = {
    model with
    storage = storage;
  } in
  process_mterm model
