(* open Tools
   open Location
   open Ident *)
open Model

exception Anomaly of string
type error_desc =
  | UnsupportedContainer of string
  | UnsupportedType of string
  | RecordNotFound
  | NotSupportedType of string
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

let shallow_asset (model : model) : model =
  model
