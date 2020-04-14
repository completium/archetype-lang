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
  f: function__ option;
  select_preds: mterm list;
  sum_preds: mterm list;
  consts: (ident * mterm) list;
}
[@@deriving show {with_path = false}]

let mk_env ?f ?(select_preds=[]) ?(sum_preds=[]) ?(consts=[]) () : env =
  { f; select_preds; sum_preds; consts }

let compute_env model =
  let select_preds =
    List.fold_right (fun x accu ->
        match x.api_loc, x.node_item with
        | (OnlyExec | ExecFormula), APIAsset (Select (_, _, pred)) ->
          if not (List.exists (Model.cmp_mterm pred) accu)
          then pred::accu
          else accu
        | _ -> accu
      ) model.api_items []
  in
  let sum_preds =
    List.fold_right (fun x accu ->
        match x.api_loc, x.node_item with
        | (OnlyExec | ExecFormula), APIAsset (Sum (_, _, pred)) ->
          if not (List.exists (Model.cmp_mterm pred) accu)
          then pred::accu
          else accu
        | _ -> accu
      ) model.api_items []
  in
  let consts =
    List.fold_right (fun (x : decl_node) accu ->
        match x with
        | Dvar v when v.constant -> (unloc v.name, Option.get v.default)::accu
        | _ -> accu
      ) model.decls [] in
  mk_env ~select_preds:select_preds ~sum_preds:sum_preds ~consts:consts ()

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
