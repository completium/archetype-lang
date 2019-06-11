open Location

module M = Model
module T = Mlwtree

let to_whyml (model : M.model) : T.mlw_tree  =
  let name = unloc model.name in
  { name = name;  decls = []; }
