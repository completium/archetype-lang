open Location
open Ident
open Tools
open Model

(* -------------------------------------------------------------------------- *)

exception Anomaly of string

type error_desc =
  | UnsupportedTerm of string
  | UnsupportedValue of string
  | TODO of string
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

(* -------------------------------------------------------------------------- *)

type env = {
  f: function_node option;
  update_preds: (ident * assignment_operator * mterm) list list;
  select_preds: mterm list;
  sum_preds: mterm list;
  removeif_preds: mterm list;
  consts: (ident * mterm) list;
}
[@@deriving show {with_path = false}]

let mk_env ?f ?(update_preds=[]) ?(select_preds=[]) ?(sum_preds=[]) ?(removeif_preds=[]) ?(consts=[]) () : env =
  { f; update_preds; select_preds; sum_preds; removeif_preds; consts }

let cmp_update l1 l2 = List.for_all2 (fun (i1, op1, v1) (i2, op2, v2) -> Model.cmp_ident i1 i2 && Model.cmp_assign_op op1 op2 && Model.cmp_mterm v1 v2) l1 l2

let compute_env model =
  let update_preds =
    List.fold_right (fun x accu ->
        match x.node_item with
        | APIAsset (Update (_, l)) ->
          if not (List.exists (cmp_update l) accu)
          then l::accu
          else accu
        | _ -> accu
      ) model.api_items []
  in
  let select_preds =
    List.fold_right (fun x accu ->
        match x.node_item with
        | APIAsset (Select (_, _, _, pred)) ->
          if not (List.exists (Model.cmp_mterm pred) accu)
          then pred::accu
          else accu
        | _ -> accu
      ) model.api_items []
  in
  let sum_preds =
    List.fold_right (fun x accu ->
        match x.node_item with
        | APIAsset (Sum (_, _, _, pred)) ->
          if not (List.exists (Model.cmp_mterm pred) accu)
          then pred::accu
          else accu
        | _ -> accu
      ) model.api_items []
  in
  let removeif_preds =
    List.fold_right (fun x accu ->
        match x.node_item with
        | APIAsset (RemoveIf (_, _, _, pred)) ->
          if not (List.exists (Model.cmp_mterm pred) accu)
          then pred::accu
          else accu
        | _ -> accu
      ) model.api_items []
  in
  let consts =
    List.fold_right (fun (x : decl_node) accu ->
        match x with
        | Dvar v when ((function | VKconstant -> true | _ -> false) v.kind) -> (Model.unloc_mident v.name, Option.get v.default)::accu
        | _ -> accu
      ) model.decls [] in
  mk_env ~update_preds:update_preds ~select_preds:select_preds ~sum_preds:sum_preds ~removeif_preds:removeif_preds ~consts:consts ()

(* -------------------------------------------------------------------------- *)

exception Found

let is_internal l (id : lident) : bool =
  try
    List.iter (fun (x : ident * mterm) -> if (String.equal (unloc id) (fst x)) then raise Found) l ;
    false
  with
  | Found -> true

let is_const (env : env) (id : lident) : bool = is_internal env.consts id

let get_const_dv (env : env) (id : lident) : mterm =
  List.assoc (unloc id) env.consts

let get_preds_index l e : int =
  match List.index_of (fun x -> Model.cmp_mterm x e) l with
  | -1 -> assert false
  | _ as i -> i

let get_preds_index_gen cmp l e : int =
  match List.index_of (fun x -> cmp x e) l with
  | -1 -> assert false
  | _ as i -> i
