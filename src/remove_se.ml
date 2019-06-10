(* open Tools *)
(* open Location *)

module M = Model
module W = Model_wse

let remove_se (model : M.model) : W.model =
  let name = model.name in
  W.mk_model name
