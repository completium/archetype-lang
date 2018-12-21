(* -------------------------------------------------------------------- *)
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
  | Tfield (id, typ) -> Format.fprintf fmt "%a : %a" pp_id id pp_id typ

(* -------------------------------------------------------------------- *)
let pp_entity fmt { pldesc = e } =
  match e with
  | Tuse id ->
      Format.fprintf fmt "use %a" pp_id id

  | Tmodel id ->
      Format.fprintf fmt "model %a" pp_id id

  | Tconstant (id, typ) ->
      Format.fprintf fmt "constant %a %a" pp_id id pp_id typ

  | Trole id ->
      Format.fprintf fmt "role %a" pp_id id

  | Tasset (id, fields) ->
      Format.fprintf fmt "assert %a = {@[<v 2>]@,%a@]}"
        pp_id id (pp_list "@," pp_field) fields

  | Textension ids ->
      Format.fprintf fmt "[%%%a]" (pp_list "@ " pp_id) ids

(* -------------------------------------------------------------------- *)
let pp_model fmt (Imodel es) =
  Format.fprintf fmt "%a" (pp_list "@," pp_entity) es

(* -------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a" pp x

(* -------------------------------------------------------------------- *)
let field_to_str  = string_of__of_pp pp_field
let entity_to_str = string_of__of_pp pp_entity
let model_to_str  = string_of__of_pp pp_model
