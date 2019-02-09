let _ =
  let modelws = Modelws.model_to_modelws (Miles.mk_miles_model()) in
  Format.printf "%a\n" Modelws.pp_model_with_storage modelws
