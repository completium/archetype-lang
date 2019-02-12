open CmlLib
open Why3
open Pmodule
open Ptree
open Wstdlib

let liqmlw_path = "/home/dev/cml-lang/src/liq"

let mk_loadpath main = (Whyconf.loadpath main) @ [liqmlw_path]

let debug = Debug.register_flag "cml"
  ~desc:"cml plugin debug flag"

(* NO! this will be executed at plugin load, thus
disabling the warning for ALL WHY3 USERS even if they don't
use the python front-end
let () = Debug.set_flag Dterm.debug_ignore_unused_var
*)

let mk_id ~loc name =
  { id_str = name; id_ats = []; id_loc = loc }

let translate dl =
  let model = Translate.parseTree_to_model dl in
  let modelws = Modelws.model_to_modelws model in
  let modelw3liq = Modelw3liq.modelws_to_modelw3liq modelws in
  List.iter (Typing.add_decl Loc.dummy_position) modelw3liq

let read_channel env path file (c: in_channel) =
  let f = Io.parse_model ~name:file c in
  Debug.dprintf debug "%s parsed successfully.@." file;
  let file = Filename.basename file in
  let file = Filename.chop_extension file in
  let name = Strings.capitalize file in
  Debug.dprintf debug "building module %s.@." name;
  Typing.open_file env path;
  let loc = Loc.user_position file 0 0 0 in
  Typing.open_module (mk_id ~loc name);
  let use_import (f, m) =
    let m = mk_id ~loc m in
    Typing.open_scope loc m;
    Typing.add_decl loc (Ptree.Duse (Qdot (Qident (mk_id ~loc f), m)));
    Typing.close_scope loc ~import:true in
  List.iter use_import
    ["option", "Option";
     "liq", "Utils";
     "liq", "Int";
     "liq", "Uint";
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
  translate f;
  Typing.close_module loc;
  let mm = Typing.close_file () in
  if path = [] && Debug.test_flag debug then begin
    let add_m _ m modm = Ident.Mid.add m.mod_theory.Theory.th_name m modm in
    let print_m _ m = Pmodule.print_module Format.err_formatter m in
    Ident.Mid.iter print_m (Mstr.fold add_m mm Ident.Mid.empty)
  end;
  mm

let () =
  Env.register_format mlw_language "cml_lang" ["cml"] read_channel
    ~desc:"cml format";
  Pdriver.register_printer "liquidity" Liq_printer.liq_printer
