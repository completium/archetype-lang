open Mlwtree

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_id = pp_str

(* -------------------------------------------------------------------------- *)
let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp

let pp_decl fmt = function
  | Duse _     -> Format.fprintf fmt "TODO: use"
  | Dclone _   -> Format.fprintf fmt "TODO: clone"
  | Drecord _  -> Format.fprintf fmt "TODO: record"
  | Dstorage _ -> Format.fprintf fmt "TODO: storage"
  | Daxiom _   -> Format.fprintf fmt "TODO: axiom"
  | Dfun _     -> Format.fprintf fmt "TODO: fun"

let pp_mlw_tree fmt (tree : mlw_tree) =
  Format.fprintf fmt "%a"
    (pp_list "@\n" pp_decl) tree.decls

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a" pp x

let show_mlw_tree x = string_of__of_pp pp_mlw_tree x
