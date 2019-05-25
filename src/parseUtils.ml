(* -------------------------------------------------------------------- *)
open Core

(* -------------------------------------------------------------------- *)
type perror =
  | PE_LexicalError of Location.t * string
  | PE_Unclosed of Location.t * string * string
  | PE_Not_Expecting of Location.t * string
  | PE_Message of string
  | PE_Unknown of Location.t

exception ParseError of perror list

(* -------------------------------------------------------------------- *)
let pp_perror fmt = function
  | PE_LexicalError (loc, x) ->
    Format.fprintf fmt "lexical error: '%s' at %s" x (Location.tostring loc)

  | PE_Unclosed (_loc, x, _y) ->
    Format.fprintf fmt "Unclosed bracket '%s' detected" x

  | PE_Not_Expecting (loc, x) ->
    Format.fprintf fmt "Not expecting '%s' at %s" x (Location.tostring loc)

  | PE_Message x ->
    Format.fprintf fmt "%s" x

  | PE_Unknown loc ->
    Format.fprintf fmt "syntax error at %s" (Location.tostring loc)

(* -------------------------------------------------------------------- *)
let pp_parse_error fmt ppe = pp_perror fmt ppe

(* -------------------------------------------------------------------------- *)
let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp

let pp_parse_errors ppes = pp_list "@\n" pp_perror ppes

(* -------------------------------------------------------------------- *)
let string_of_perror ppe =
  Format.asprintf "%a" pp_perror ppe
