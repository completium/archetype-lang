open Format
open Why3
open Wstdlib
open Pmodule
open Compile

let () = Pdriver.register_printer "liquidity" Liq_printer.liq_printer

let translate_module =
  let memo = Ident.Hid.create 16 in
  fun m ->
    let name = m.mod_theory.Theory.th_name in
    try Ident.Hid.find memo name with Not_found ->
      let pm = Translate.module_ m in
      let pm = Transform.module_ pm in
      Ident.Hid.add memo name pm;
      pm

let print_mdecls ?fname m mdecls deps =
  let config = Whyconf.read_config None in
  let main = Whyconf.get_main config in
  let env = Env.create_env (Whyconf.loadpath main) in
  let dname = "liq.drv" in
  let driver = Pdriver.load_driver env dname [] in
  let pargs, printer = Pdriver.lookup_printer driver in
  let fg = printer.Pdriver.file_gen in
  let pr = printer.Pdriver.decl_printer in
  let test_decl_not_driver decl =
    let decl_name = Mltree.get_decl_name decl in
    let test_id_not_driver id =
      Printer.query_syntax pargs.Pdriver.syntax id = None in
    List.exists test_id_not_driver decl_name in
  let prelude_exists =
    Ident.Mid.mem m.mod_theory.Theory.th_name pargs.Pdriver.thprelude in
  if List.exists test_decl_not_driver mdecls || prelude_exists
  then begin
      let flat = false (*opt_modu_flat = Flat*) in
      (*let thname = m.mod_theory.Theory.th_name in*)
      let cout, old = stdout, None in (*get_cout_old fg m ?fname in*)
      let fmt = formatter_of_out_channel cout in
      (* print module prelude *)
      printer.Pdriver.prelude_printer pargs ?old ?fname ~flat deps fmt m;
      (* print driver prelude *)
      (*    Printer.print_prelude fmt pargs.Pdriver.prelude;
    let pm = pargs.Pdriver.thprelude i  n
    print_preludes thname fmt pm;*)
      (* print decls *)
      let pr_decl fmt d = fprintf fmt "%a" (pr pargs ?old ?fname ~flat m) d in
      Pp.print_list Pp.nothing pr_decl fmt mdecls;
      if cout <> stdout then close_out cout;
      true end
  else false

let _ =
  let config = Whyconf.read_config None in
  let main = Whyconf.get_main config in
  let env = Env.create_env (Whyconf.loadpath main) in
  let fname = "miles_with_expiration.mlw" in
  let cin = open_in fname in
  let mm = Env.read_channel Pmodule.mlw_language env fname cin in
  close_in cin;
  let extract_to =
    let memo = Ident.Hid.create 16 in
    fun ?fname ?decl m deps ->
    match m.mod_theory.Theory.th_path with
    | _ -> let name = m.mod_theory.Theory.th_name in
        if not (Ident.Hid.mem memo name) then begin
          let mdecls = match decl with
            | None   -> (translate_module m).Mltree.mod_decl
            | Some d -> Translate.pdecl_m m d in
          let file_exists = print_mdecls ?fname m mdecls deps in
          Ident.Hid.add memo name file_exists;
          file_exists
          end
        else Ident.Hid.find memo name in
  let do_m _ m = ignore (extract_to ~fname m []) in
  Mstr.iter do_m mm
