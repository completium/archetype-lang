(* open Tools *)
open Location

module M = Model
module T = Mltree

exception Anomaly of string

type error_desc =
  | UnsupportedDefaultValue of string
  | CannotConvertLogicalOperator of string
  | NoStorageRecordFound
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)


let to_liquidity (model : M.model) : T.tree =
  let name = unloc model.name in
  T.mk_tree name
