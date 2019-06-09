module W = Model_wse

type liq_tree = string list
[@@deriving show {with_path = false}]

let model_to_liq_tree (_model : W.model) : liq_tree =
  []
