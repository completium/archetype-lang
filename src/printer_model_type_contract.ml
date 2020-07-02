open Model
open Printer_tools

let pp_ptc_model fmt (model : model) =
  let pp_function fmt (f : function__) =
  match f.node with
    | Entry fs ->
      Format.fprintf fmt
        "entry %a(%a)"
        pp_id fs.name
        (pp_list ", " (fun fmt (x, t, _) -> Format.fprintf fmt "%a : %a" pp_id x Printer_model.pp_type t)) fs.args
    | _ -> ()
  in
  Format.fprintf fmt
    "contract %a {@\n  @[%a@]@\n}"
    pp_id model.name
    (pp_list "@\n" pp_function) model.functions
