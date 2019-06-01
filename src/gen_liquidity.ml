module S = Storage

type liq_tree = string list
[@@deriving show {with_path = false}]

let model_to_liq_tree (_model : S.model) : liq_tree =
  []
