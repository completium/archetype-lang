open Mltree

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_ident = pp_str

(* -------------------------------------------------------------------------- *)
let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp


(* -------------------------------------------------------------------------- *)

let pp_type fmt _s =
  Format.fprintf fmt "TODO: type@."

let pp_struct fmt _s =
  Format.fprintf fmt "TODO: struct@."

let pp_fun fmt _s =
  Format.fprintf fmt "TODO: fun@."

let pp_decl fmt decl =
  match decl with
  | Dtype s ->
    pp_type fmt s

  | Dstruct s ->
    pp_struct fmt s

  | Dfun s ->
    pp_fun fmt s

let pp_tree fmt tree =
  Format.fprintf fmt "%a@."
    (pp_list "@\n" pp_decl) tree.decls

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_tree (x : tree) = string_of__of_pp pp_tree x
