(* -------------------------------------------------------------------- *)
open Archetype
open Core

let opt_lsp = ref false
let opt_json = ref false
let opt_pretty_print = ref false
let opt_parse = ref false
let opt_model = ref false
let opt_modelr = ref false
let opt_modelws = ref false
let opt_modelliq = ref false
let opt_pterm = ref false
let debug_mode = ref false

exception Compiler_error
exception E_arg
exception ArgError of string

(* -------------------------------------------------------------------- *)
let compile_and_print (filename, channel) =
  Tools.debug_mode := !debug_mode;
  if !opt_lsp
  then () (*Lsp.process (filename, channel)*)
  else (
    let pt = Io.parse_archetype ~name:filename channel in
    if !opt_json
    then Format.printf "%s\n" (Yojson.Safe.to_string (ParseTree.archetype_to_yojson pt))
    else (
      if !opt_pretty_print
      then () (*Format.printf "%a" Printer.pp_archetype pt*)
      else (
        if !opt_parse
        then Format.printf "%a\n" ParseTree.pp_archetype pt
        else (
          let model =  Model.mk_model (Location.dumloc "mymodel") (*Translate.parseTree_to_model pt*) in
          if !opt_model
          then Format.printf "%a\n" Model.pp_model model
          else (
            let modelr = Reduce.reduce_model model in
            if !opt_modelr
            then Format.printf "%a\n" Model.pp_model modelr
            (* else (
               let info  = Modelinfo.mk_info (Location.unloc modelr) in
               let modelws = Modelws.model_to_modelws info modelr in
               if !opt_modelws
               then Format.printf "%a\n" Modelws.pp_model_with_storage modelws
               else (
                let modelw3liq = Modelliq.modelws_to_modelliq info modelws in
                if !opt_modelliq
                then Extract.print modelw3liq
                else ()
               ) *)
          )))))

let close dispose channel =
  if dispose then close_in channel

(* -------------------------------------------------------------------- *)
let main () =
  let arg_list = Arg.align [
      "--json", Arg.Set opt_json, " Output Archetype in JSON representation";
      "-PP", Arg.Set opt_pretty_print, " Pretty print";
      "-P", Arg.Set opt_parse, " Print raw parse tree";
      "-M", Arg.Set opt_model, " Print raw model";
      "-R", Arg.Set opt_modelr, " Print raw model reduced";
      "-W", Arg.Set opt_modelws, " Print raw model_with_storage";
      "-L", Arg.Set opt_modelliq, " Output Archetype in liquidity";
      "-T", Arg.Set opt_pterm, " Print pterm";
      "--lsp", Arg.Set opt_lsp, "LSP mode";
      "-d", Arg.Set debug_mode, " Debug mode";
      "--storage-policy",
      Arg.String (fun s -> match s with
          (* | "flat" -> Modelinfo.storage_policy := Flat
             | "record" -> Modelinfo.storage_policy := Record *)
          |  s ->
            Format.eprintf
              "Unknown policy %s (use record, flat)@." s;
            exit 2),
      " Set storage policy"
    ] in
  let arg_usage = String.concat "\n" [
      "compiler [OPTIONS] FILE";
      "";
      "Available options:";
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
    (* if !opt_pterm
       then (
       let str = input_line channel in
       let pterm = Translate.string_to_pterm str in
       Format.printf "%a\n" Model.pp_pterm pterm
       )
       else  *)
    compile_and_print (filename, channel);
    close dispose channel

  (*    let filename, channel, dispose =
        if Array.length Sys.argv > 1 then
          let filename = Sys.argv.(1) in
          (filename, open_in filename, true)
        else ("<stdin>", stdin, false)
        in*)

  with
  | ParseUtils.ParseError exn ->
    close dispose channel;
    Format.eprintf "%a@." ParseUtils.pp_parse_error exn;
    exit 1
  | Compiler_error ->
    close dispose channel;
    Arg.usage arg_list arg_usage;
    exit 1
  | ArgError s ->
    close dispose channel;
    Printf.eprintf "%s.\n" s;
    exit 1
  | Reduce.ReduceError (msg, l) ->
    close dispose channel;
    Printf.eprintf "%s%s.\n" msg (match l with | None -> "" | Some l -> (Location.tostring l));
    exit 1
  | Reduce.ErrorAcceptTransfer (str, loc, locs) ->
    close dispose channel;
    Printf.eprintf "Error: accept transfer must be set to '%s' at %s because 'transferred' is read at %s.\n" str (Location.tostring loc) (List.fold_right (fun i accu -> (Location.tostring i) ^ accu) locs "");
    exit 1
(* | Translate.ModelError0 msg ->
   close dispose channel;
   Printf.eprintf "%s.\n" msg;
   exit 1
   | Translate.ModelError (msg, l) ->
   close dispose channel;
   Printf.eprintf "%s at %s.\n" msg (Location.tostring l);
   exit 1
   | Translate.ModelError2 (msg, l0, l1) ->
   close dispose channel;
   Printf.eprintf "%s at %s and %s.\n" msg (Location.tostring l0) (Location.tostring l1);
   exit 1
   | Modelinfo.DefaultValueAssignment l ->
   close dispose channel;
   Printf.eprintf "'%s' field at %s must be assigned by a value.\n"
    (Location.unloc l)
    (l |> Location.loc |> Location.tostring);
   exit 1
   | Modelinfo.WrongTypeAsset (a, b, l) ->
   close dispose channel;
   Printf.eprintf "This asset is flaged '%s' but was expected '%s' at %s.\n"
    a b (Location.tostring l);
   exit 1
   | Modelinfo.TypeError (a, b, l) ->
   close dispose channel;
   Printf.eprintf "This expression has type %s but an expression was expected of %s at %s.\n"
    a b (Location.tostring l);
   exit 1
   | Modelinfo.UnsupportedFeature (msg, l) ->
   close dispose channel;
   Printf.eprintf "%s at %s.\n" msg (Location.tostring l);
   exit 1
   | Modelinfo.UnsupportedVartype l ->
   close dispose channel;
   Printf.eprintf "Unsupported var type at %s.\n"  (Location.tostring l);
   exit 1 *)

(* -------------------------------------------------------------------- *)
let _ = main ()
