module M = Model

type liq_tree = string list
[@@deriving show {with_path = false}]

let model_to_liq_tree (_model : M.model) : liq_tree =
  []
