(* -------------------------------------------------------------------- *)
open Archetype
open Core

exception Compiler_error
exception E_arg
exception ArgError of string
exception Stop


let parse (filename, channel) =
  let pt = Io.parse_archetype_strict ~name:filename channel in
  if !Option.opt_json then (Format.printf "%s\n" (Yojson.Safe.to_string (ParseTree.archetype_to_yojson pt)); raise Stop)
  else if !Option.opt_parse then (Format.printf "%a\n" ParseTree.pp_archetype pt; raise Stop)
  else if !Option.opt_pretty_print then (Format.printf "%a" Printer.pp_archetype pt; raise Stop)
  else pt

let preprocess_ext pt =
  if !Option.opt_pre_json then (Format.printf "%s\n" (Yojson.Safe.to_string (ParseTree.archetype_to_yojson pt)); raise Stop)
  else if !Option.opt_pre_parse then (Format.printf "%a\n" ParseTree.pp_archetype pt; raise Stop)
  else if !Option.opt_pre_pretty_print then (Format.printf "%a" Printer.pp_archetype pt; raise Stop)
  else pt

let type_ pt =
  let ast = Model.mk_model (Location.dumloc "mymodel") (*Translate.parseTree_to_model pt*) in
  if !Option.opt_ast
  then (Format.printf "%a\n" Model.pp_model ast; raise Stop)
  else ast

let reduce_ast ast =
  let rast = Reduce.reduce_ast ast in
  if !Option.opt_astr
  then (Format.printf "%a\n" Model.pp_model rast; raise Stop)
  else rast

let model ast =
  let model = Translate.ast_to_model ast in
  if !Option.opt_model
  then (Format.printf "%a\n" Storage.pp_model model; raise Stop)
  else model

let generate_target model =
  match !Option.target with
  | Liquidity -> (
      let liq_tree = Gen_liquidity.model_to_liq_tree model in
      if !Option.opt_raw_target
      then Format.printf "%a\n" Gen_liquidity.pp_liq_tree liq_tree
      else () (*TODO: pretty print liquidity tree *)
    )
  | Whyml     -> (
      let decls = Gen_whyml.model_to_liq_tree model in
      if !Option.opt_raw_target
      then () (*TODO: raw print ptree whyml tree *)
      else (Format.printf "%a\n" Printer_mlw.pp_mlw decls; raise Stop)
    )
  | Markdown  -> () (*TODO*)
  | None      -> () (*TODO*)

(* -------------------------------------------------------------------- *)
let compile (filename, channel) =
  Tools.debug_mode := !Option.debug_mode;
  (filename, channel)
  |> parse
  |> preprocess_ext
  |> type_
  |> reduce_ast
  |> model
  |> generate_target

let close dispose channel =
  if dispose then close_in channel

(* -------------------------------------------------------------------- *)
let main () =
  let f = function
    | "liquidity" -> Option.target := Liquidity
    | "whyml" -> Option.target := Whyml
    | "markdown" -> Option.target := Markdown
    |  s ->
      Format.eprintf
        "Unknown target %s (--list-target to see available target)@." s;
      exit 2 in

  let arg_list = Arg.align [
      "-t", Arg.String f, "<lang> Transcode to <lang> language";
      "--target", Arg.String f, " Same as -t";
      "--list-target", Arg.Unit (fun _ -> Format.printf "target available:@\n  liquidity@\n  whyml@\n  markdown@\n"; exit 0), " List available target languages";
      "--json", Arg.Set Option.opt_json, " Output Archetype in JSON representation";
      "--storage-policy", Arg.String (fun s -> match s with
          | "flat" -> Option.storage_policy := Flat
          | "record" -> Option.storage_policy := Record
          |  s ->
            Format.eprintf
              "Unknown policy %s (use record, flat)@." s;
            exit 2), "<policy> Set storage policy";
      "--list-storage-policy", Arg.Unit (fun _ -> Format.printf "storage policy available:@\n  record@\n  flat@\n"; exit 0), " List storage policy";
      "-PP", Arg.Set Option.opt_pretty_print, " Pretty print parse tree";
      "--pretty-print", Arg.Set Option.opt_pretty_print, " Same as -PP";
      "-PPP", Arg.Set Option.opt_pretty_print, " Pretty print parse tree without extension";
      "--preprocess-pretty-print", Arg.Set Option.opt_pretty_print, " Same as -PPP";
      "-P", Arg.Set Option.opt_parse, " Print raw parse tree";
      "-A", Arg.Set Option.opt_ast, " Print raw ast";
      "--ast", Arg.Set Option.opt_ast, " Same as -A";
      "-RA", Arg.Set Option.opt_astr, " Print raw ast";
      "--reduced-ast", Arg.Set Option.opt_astr, " Same as -RA";
      "-M", Arg.Set Option.opt_model, " Print raw model";
      "--model", Arg.Set Option.opt_model, " Same as -M";
      "--lsp", Arg.String (fun s -> match s with
          | "errors" -> Option.opt_lsp := true; Lsp.kind := Errors
          | "outline" -> Option.opt_lsp := true; Lsp.kind := Outline
          |  s ->
            Format.eprintf
              "Unknown lsp commands %s (use errors, outline)@." s;
            exit 2), "LSP mode";
      "-d", Arg.Set Option.debug_mode, " Debug mode";
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
    if !Option.opt_lsp
    then Lsp.process (filename, channel)
    else compile (filename, channel);
    close dispose channel

  with
  | Stop ->
    close dispose channel
  | ParseUtils.ParseError exns ->
    close dispose channel;
    Format.eprintf "%a@." ParseUtils.pp_parse_errors exns;
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

(* -------------------------------------------------------------------- *)
let _ = main ()
