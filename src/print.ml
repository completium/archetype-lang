(* -------------------------------------------------------------------- *)
open Core
open Location
open Ast

(* -------------------------------------------------------------------- *)
let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp

(* -------------------------------------------------------------------- *)
let pp_id fmt (id : lident) =
  Format.fprintf fmt "%s" (unloc id)

(* -------------------------------------------------------------------- *)
let pp_field fmt { pldesc = f } =
  match f with
  | Ffield (id, typ) -> Format.fprintf fmt "%a : %a;" pp_id id pp_id typ

let pp_extension fmt { pldesc = e } =
  match e with
  | Eextension (id, _) -> Format.fprintf fmt "[%%%a]" pp_id id

(* -------------------------------------------------------------------- *)
let pp_declaration fmt { pldesc = e } =
  match e with
  | Duse id ->
      Format.fprintf fmt "use %a" pp_id id

  | Dmodel id ->
      Format.fprintf fmt "model %a" pp_id id

  | Dconstant (id, typ, _) ->
      Format.fprintf fmt "constant %a %a" pp_id id pp_id typ

  | Dvalue (id, typ, _) ->
      Format.fprintf fmt "value %a %a" pp_id id pp_id typ

  | Drole (id, _,  _) ->
      Format.fprintf fmt "role %a" pp_id id

  | Denum (id, ids) ->
      Format.fprintf fmt "enum %a =\n  | %a" pp_id id (pp_list "\n  | " pp_id) ids

  | Dstates (None, ids) ->
      Format.fprintf fmt "states\n  | %a" (pp_list "\n  | " pp_id) ids

  | Dstates (Some id, ids) ->
      Format.fprintf fmt "states %a =\n  | %a" pp_id id (pp_list "\n  | " pp_id) ids

  | Dasset (id, fields) ->
      Format.fprintf fmt "asset %a = {@[<v 2>]@,%a@]}"
        pp_id id (pp_list "@," pp_field) fields

  | Dtransition (id, from, _to, _) ->
      Format.fprintf fmt "transition %a from %a to %a = {}" pp_id id pp_id from pp_id _to

  | Dtransaction (id, _) ->
      Format.fprintf fmt "transaction %a = {}" pp_id id


(* -------------------------------------------------------------------- *)
let pp_model fmt (Mmodel es) =
  Format.fprintf fmt "%a" (pp_list "@," pp_declaration) es

(* -------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a" pp x

(* -------------------------------------------------------------------- *)
let field_to_str  = string_of__of_pp pp_field
let entity_to_str = string_of__of_pp pp_declaration
let model_to_str  = string_of__of_pp pp_model
