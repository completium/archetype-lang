(* open Tools *)
(* open Location *)

module M = Model
module W = Model_wse

let to_wse (model : M.model) : W.model =
  let name = model.name in
  W.mk_model name
