open Why3
open Ptree

exception ExtractError

let mk_id ~loc name : ident =
  { id_str = name; id_ats = []; id_loc = loc }

let print (_ds : Ptree.decl list) =
  let liqmlw_path = "./why3" in
  let dname = "./why3/liquidity.drv" in
  let mk_loadpath main = (Why3.Whyconf.loadpath main) @ [liqmlw_path] in
  let config = Why3.Whyconf.read_config None in
  let main = Why3.Whyconf.get_main config in
  let env = Why3.Env.create_env (mk_loadpath main) in
  let driver = Why3.Pdriver.load_driver env dname [] in
  List.iter (fun s -> Format.printf "%s@\n@." s) driver.drv_prelude(*;*)

  (*  let mm = Env.read_channel Pmodule.mlw_language env filename channel in*)
(*  List.iter (fun (d : Ptree.decl) -> Format.printf "%a@\n@." print_module d) ds;*)

    (*let opt_driver = Pdriver.load_driver env f ef "liquidity" in
      Why3.Pdriver.register_printer opt_driver ArchetypeLib.Liq_printer.liq_printer;
      let (pargs, _printer) = Why3.Pdriver.lookup_printer opt_driver in
      let print_prelude =
      print_prelude pargs.Pdriver.prelude*)

(*  let loc = Loc.dummy_position in

  let use_import (f, m) =
    let m = mk_id ~loc m in
    Typing.open_scope loc m;
    Typing.add_decl loc (Ptree.Duse (Qdot (Qident (mk_id ~loc f), m)));
    Typing.close_scope loc ~import:true in

    List.iter use_import
    ["option", "Option";
     "liq", "Utils";
     "liq", "Int";
     "liq", "Nat";
     "liq", "Address";
     "liq", "Timestamp";
     "liq", "Tez";
     "liq", "String";
     "liq", "List";
     "liq", "Map";
     "liq", "Set";
     "liq", "Msg";
     "liq", "Current";
     "liq", "Contract"];

  List.iter (fun d ->
      try Typing.add_decl Loc.dummy_position d
      with e ->
        Format.printf "Exception raised during typing of d:@ %a@."
          Exn_printer.exn_printer e) ds*)
