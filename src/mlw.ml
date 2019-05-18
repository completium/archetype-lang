(* -------------------------------------------------------------------- *)
open Archetype
open Core

let close dispose channel =
  if dispose then close_in channel

(* -------------------------------------------------------------------- *)
let main () =
  let libraries = ref [] in
  let arg_list = Arg.align [
      "-L",
      Arg.String (fun s -> libraries := s::!libraries),
      " add <dir> to the library search path"
    ] in
  let arg_usage = String.concat "\n" [
      "mlw [OPTIONS] FILE";
      "";
      "Available options:"
    ]  in

  let ofilename = ref "" in
  let ochannel : in_channel option ref = ref None  in
  Arg.parse arg_list (fun s -> (ofilename := s;
                                ochannel := Some (open_in s))) arg_usage;

  let filename, channel, dispose =
    match !ochannel with
    | Some c -> (!ofilename, c, true)
    | _ -> ("<stdin>", stdin, false) in
  try
    let mk_loadpath main = (Why3.Whyconf.loadpath main) @ !libraries in
    let config = Why3.Whyconf.read_config None in
    let main = Why3.Whyconf.get_main config in
    let env = Why3.Env.create_env (mk_loadpath main) in

    let _mm = Why3.Env.read_channel Why3.Pmodule.mlw_language env filename channel in
    let pt = [] in
    Format.printf "%a\n" Printer_mlw.pp_mlw pt

  with
  | ParseUtils.ParseError exn ->
    close dispose channel;
    Format.eprintf "%a@." ParseUtils.pp_parse_error exn;
    exit 1

(* -------------------------------------------------------------------- *)
let _ = main ()
