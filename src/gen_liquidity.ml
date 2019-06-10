open Location

module W = Model_wse
module T = Mltree

let to_liquidity (model : W.model) : T.tree =
  let name = model.name |> unloc in
  let instr : T.expr =
    Eletin (
      [
        ["ops", Tlocal "storage"], Econtainer []
      ],
      Etuple [Evar "s"; Evar "ops"]) in
  let decls = [
    T.Dtype (T.mk_type "myenum" ?values:(Some ([("First", []); ("Second", [])])));
    T.Dstruct (T.mk_struct "storage" ?fields:(Some [("i", Tbasic T.Tint); ("str", Tbasic T.Tstring)]));
    T.Dfun (T.mk_fun "main" T.Entry [] instr)

  ] in
  T.mk_tree name ?decls:(Some decls)
