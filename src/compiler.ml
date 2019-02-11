(* -------------------------------------------------------------------- *)
open CmlLib
open Core

let opt_parse = ref false
let opt_model = ref false
let opt_modelws = ref false
let opt_modelw3liq = ref false

exception Compiler_error
exception E_arg

(* -------------------------------------------------------------------- *)
let compile_and_print (filename, channel) =
  let pt = Io.parse_model ~name:filename channel in
  if !opt_parse
  then Format.printf "%a@." Printer.pp_model pt
  else (
    let model = Translate.parseTree_to_model pt in
    if !opt_model
    then Format.printf "%a\n" Model.pp_model model
    else (
      let modelws = Modelws.model_to_modelws model in
      if !opt_modelws
      then Format.printf "%a\n" Modelws.pp_model_with_storage modelws
      else (
        let _modelw3liq= Modelw3liq.modelws_to_modelw3liq modelws in
        if !opt_modelw3liq then () else ()
    )))

(* -------------------------------------------------------------------- *)
let main () =
  let arg_list = Arg.align [
      "-P", Arg.Set opt_parse, "Parse";
      "-M", Arg.Set opt_model, "Model";
      "-W", Arg.Set opt_modelws, "model_With_storage"
    ] in
  let arg_usage = String.concat "\n" [
      "compiler [OPTIONS] FILE";
      "";
      "Available options:";
    ]  in

  try
    let ofilename = ref "" in
    let ochannel : BatInnerIO.input option ref = ref None  in
    Arg.parse arg_list (fun s -> (ofilename := s;
                                  ochannel := Some (open_in s))) arg_usage;
    let filename, channel, dispose =
      match !ochannel with
      | Some c -> (!ofilename, c, true)
      | _ -> ("<stdin>", stdin, false) in

(*    let filename, channel, dispose =
      if Array.length Sys.argv > 1 then
        let filename = Sys.argv.(1) in
        (filename, open_in filename, true)
      else ("<stdin>", stdin, false)
      in*)

    finally
      (fun () -> if dispose then close_in channel)
      compile_and_print (filename, channel)

  with
  | ParseUtils.ParseError exn ->
    Format.eprintf "%a@." ParseUtils.pp_parse_error exn; exit 1
  | Compiler_error -> Arg.usage arg_list arg_usage; exit 1
  | Modelws.UnsupportedVartype l ->
    Printf.eprintf "Unsupported var type at %s.\n"  (Location.tostring l); exit 1

(* -------------------------------------------------------------------- *)
let _ = main ()
