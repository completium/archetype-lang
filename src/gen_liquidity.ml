open Location

module W = Model_wse
module T = Mltree

let to_liquidity (model : W.model) : T.tree =
  let name = model.name |> unloc in
  let instr : T.expr =
    Eletin (
      [
        ["ops", Tlist (Tlocal "operation")], Econtainer []
      ],
      Etuple [Evar "ops"; Evar "s"]) in
  let decls = [
    T.Dtype (T.mk_type "myenum" ~values:([("First", None); ("Second", None)]));
    T.Dstruct (T.mk_struct "storage" ~fields:[("i", Tbasic T.Tint); ("str", Tbasic T.Tstring)]);
    T.Dfun (T.mk_fun "main" T.Entry ["params", Tbasic Tunit; "s", Tlocal "storage"] instr)

  ] in
  T.mk_tree name ~decls:decls
