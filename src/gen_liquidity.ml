open Location

module W = Model_wse
module T = Mltree

let to_liquidity (model : W.model) : T.tree =
  let name = model.name |> unloc in
  T.mk_tree name
