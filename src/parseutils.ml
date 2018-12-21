(* -------------------------------------------------------------------- *)
open Core

(* -------------------------------------------------------------------- *)
type perror =
  | PE_LexicalError of string
  | PE_Unknown

exception ParseError of (Location.t option * perror)

(* -------------------------------------------------------------------- *)
let pp_perror fmt = function
  | PE_LexicalError x ->
      Format.fprintf fmt "lexical error: %s" x

  | PE_Unknown ->
      Format.fprintf fmt "syntax error"

(* -------------------------------------------------------------------- *)
let pp_parse_error fmt (loc, ppe) =
  match loc with
  | None ->
      pp_perror fmt ppe

  | Some loc ->
      Format.fprintf fmt "%s: %a"
        (Location.tostring loc) pp_perror ppe

(* -------------------------------------------------------------------- *)
let string_of_perror ppe =
  Format.asprintf "%a" pp_perror ppe

(* -------------------------------------------------------------------- *)
let string_of_parse_error ppe =
  Format.asprintf "%a" pp_parse_error ppe
