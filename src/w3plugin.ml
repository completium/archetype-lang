open ArchetypeLib
open Why3
open Pmodule
open Ptree
open Wstdlib

let debug = Debug.register_flag "archetype"
  ~desc:"archetype plugin debug flag"

(* NO! this will be executed at plugin load, thus
disabling the warning for ALL WHY3 USERS even if they don't
use the python front-end
let () = Debug.set_flag Dterm.debug_ignore_unused_var
*)

let mk_id ~loc name =
  { id_str = name; id_ats = []; id_loc = loc }

let translate dl =
  let model = Translate.parseTree_to_model dl in
  let modelr = Reduce.reduce_model model in
  let info = Modelinfo.mk_info (Location.unloc modelr) in
  let modelws = Modelws.model_to_modelws info modelr in
  let modelliq = Modelliq.modelws_to_modelliq info modelws in
  List.iter (Typing.add_decl Loc.dummy_position) modelliq

let read_channel env path file (c: in_channel) =
  let f = Io.parse_archetype ~name:file c in
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
     "liquidity", "Utils";
     "liquidity", "Int";
     "liquidity", "Nat";
     "liquidity", "Address";
     "liquidity", "Timestamp";
     "liquidity", "Tez";
     "liquidity", "String";
     "liquidity", "List";
     "liquidity", "Map";
     "liquidity", "Set";
     "liquidity", "Msg";
     "liquidity", "Current";
     "liquidity", "Contract"];
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
  Env.register_format mlw_language "archetype_lang" ["archetype"] read_channel
    ~desc:"archetype format";
  Pdriver.register_printer "liquidity" Liq_printer.liq_printer
