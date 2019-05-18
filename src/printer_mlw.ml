open Why3
open Ptree

(** grammar : http://why3.lri.fr/doc-1.2.0/syntaxref.html#sec85 *)

let pp_str fmt str =
  Format.fprintf fmt "%s" str

(* -------------------------------------------------------------------------- *)
let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp

let pp_ident fmt ident =
  Format.fprintf fmt "%s" ident.id_str

let rec pp_qualid fmt q =
  match q with
  | Qident id ->
    Format.fprintf fmt "%a"
      pp_ident id

  | Qdot (q, id) ->
    Format.fprintf fmt "%a.%a"
      pp_qualid q
      pp_ident id

(* -------------------------------------------------------------------------- *)

let pp_decl fmt decl =
  match decl with
  | Dtype _tds ->
    Format.fprintf fmt "FIXME"

  | Dlogic _lds ->
    Format.fprintf fmt "FIXME"

  | Dind (_ind_sign, _ind_decls) ->
    Format.fprintf fmt "FIXME"

  | Dprop (_prop_kin, _id, _terms) ->
    Format.fprintf fmt "FIXME"

  | Dlet (id, _ghost, _kind, _e) ->
    Format.fprintf fmt "let %a = FIXME"
      pp_ident id

  | Drec _fundefs ->
    Format.fprintf fmt "FIXME"

  | Dexn (_id, _pty, _mask) ->
    Format.fprintf fmt "FIXME"

  | Dmeta (_id, _metargs) ->
    Format.fprintf fmt "FIXME"

  | Dclone (_q, _css) ->
    Format.fprintf fmt "FIXME"

  | Duse q ->
    Format.fprintf fmt "use export %a"
      pp_qualid q


let pp_mlw fmt decls =
  Format.fprintf fmt "%a"
    (pp_list "@\n" pp_decl) decls

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a" pp x

let show_mlw (x : decl list) = string_of__of_pp pp_mlw x
