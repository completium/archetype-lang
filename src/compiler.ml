(* -------------------------------------------------------------------- *)
open Archetype
open Core

exception Compiler_error
exception E_arg
exception ArgError of string
exception Stop


let parse (filename, channel) =
  let pt =
    (if !Options.opt_cwse
     then Io.parse_archetype
     else Io.parse_archetype_strict) ~name:filename channel in
  if !Options.opt_json then (Format.printf "%s\n" (Yojson.Safe.to_string (ParseTree.archetype_to_yojson pt)); raise Stop)
  else if !Options.opt_parse then (Format.printf "%a\n" ParseTree.pp_archetype pt; raise Stop)
  else if !Options.opt_pretty_print then (Format.printf "%a" Printer.pp_archetype pt; raise Stop)
  else pt

let preprocess_ext pt =
  if !Options.opt_pre_json then (Format.printf "%s\n" (Yojson.Safe.to_string (ParseTree.archetype_to_yojson pt)); raise Stop)
  else if !Options.opt_pre_parse then (Format.printf "%a\n" ParseTree.pp_archetype pt; raise Stop)
  else if !Options.opt_pre_pretty_print then (Format.printf "%a" Printer.pp_archetype pt; raise Stop)
  else pt

let type_ pt =
  let ast = Typing.typing Typing.empty pt in
  if !Options.opt_ast
  then (Format.printf "%a@." Ast.pp_model ast; raise Stop)
  else ast

let reduce_ast ast =
  if !Options.no_reduce
  then ast
  else
    let rast = Gen_reduce.reduce_ast ast in
    if !Options.opt_astr
    then (Format.printf "%a@." Ast.pp_model rast; raise Stop)
    else rast

let model ast =
  let model = Gen_model.to_model ast in
  if !Options.opt_model
  then (Format.printf "%a@." Model.pp_model model; raise Stop)
  else model

let remove_side_effect model =
  let wse = Gen_remove_se.remove_se model in
  if !Options.opt_wse
  then (Format.printf "%a@." Model_wse.pp_model wse; raise Stop)
  else wse

let generate_liquidity wse =
  let tree = Gen_liquidity.to_liquidity wse in
  if !Options.opt_raw_target
  then (Format.printf "%a@." Mltree.pp_tree tree; raise Stop)
  else Format.asprintf "%a@." Printer_mltree.pp_tree tree

let output_liquidity str =
  if !Options.opt_liq_url
  then
    let encoded_src = Uri.pct_encode str in
    let url = "http://www.liquidity-lang.org/edit/?source=" ^ encoded_src in
    Format.printf "%s@\n" url
  else Format.printf "%s" str

let generate_whyml model =
  let mlw = Gen_why3.to_whyml model in
  if !Options.opt_raw_target
  then Format.printf "%a@." Mlwtree.pp_mlw_tree mlw
  else Format.printf "%a@." Printer_mlwtree.pp_mlw_tree mlw

let generate_target_pt pt =
  match !Options.target with
  | Markdown  -> (
      let md = Gen_markdown.pt_to_ast_omd pt in
      Format.printf "%s@." (Omd.to_markdown md);
      raise Stop
    )
  | _ -> pt

let generate_target model =
  match !Options.target with
  | Liquidity ->
    model
    |> remove_side_effect
    |> generate_liquidity
    |> output_liquidity

  | Whyml ->
    model
    |> generate_whyml

  | _ -> ()

(* -------------------------------------------------------------------- *)

let compile (filename, channel) =
  Tools.debug_mode := !Options.debug_mode;
  (filename, channel)
  |> parse
  |> preprocess_ext
  |> generate_target_pt
  |> type_
  |> reduce_ast
  |> model
  |> generate_target

let close dispose channel =
  if dispose then close_in channel

(* -------------------------------------------------------------------- *)
let main () =
  let f = function
    | "liquidity" -> Options.target := Liquidity
    | "ligo"      -> Options.target := Ligo
    | "whyml"     -> Options.target := Whyml
    | "markdown"  -> Options.target := Markdown
    |  s ->
      Format.eprintf
        "Unknown target %s (--list-target to see available target)@." s;
      exit 2 in

  let arg_list = Arg.align [
      "-t", Arg.String f, "<lang> Transcode to <lang> language";
      "--target", Arg.String f, " Same as -t";
      "--list-target", Arg.Unit (fun _ -> Format.printf "target available:@\n  ligo@\n  whyml@\n  markdown@\n"; exit 0), " List available target languages";
      "--json", Arg.Set Options.opt_json, " Output Archetype in JSON representation";
      "--storage-policy", Arg.String (fun s -> match s with
          | "flat" -> Options.storage_policy := Flat
          | "record" -> Options.storage_policy := Record
          |  s ->
            Format.eprintf
              "Unknown policy %s (use record, flat)@." s;
            exit 2), "<policy> Set storage policy";
      "--list-storage-policy", Arg.Unit (fun _ -> Format.printf "storage policy available:@\n  record@\n  flat@\n"; exit 0), " List storage policy";
      "-PP", Arg.Set Options.opt_pretty_print, " Pretty print parse tree";
      "--pretty-print", Arg.Set Options.opt_pretty_print, " Same as -PP";
      "-PPP", Arg.Set Options.opt_pretty_print, " Pretty print parse tree without extension";
      "--preprocess-pretty-print", Arg.Set Options.opt_pretty_print, " Same as -PPP";
      "-P", Arg.Set Options.opt_parse, " Print raw parse tree";
      "-A", Arg.Set Options.opt_ast, " Print raw ast";
      "--ast", Arg.Set Options.opt_ast, " Same as -A";
      "-RA", Arg.Set Options.opt_astr, " Print raw ast";
      "--reduced-ast", Arg.Set Options.opt_astr, " Same as -RA";
      "-M", Arg.Set Options.opt_model, " Print raw model";
      "--model", Arg.Set Options.opt_model, " Same as -M";
      "-W", Arg.Set Options.opt_wse, " Print raw model without side effect";
      "--without-side-effect", Arg.Set Options.opt_wse, " Same as -W";
      "-RTT", Arg.Set Options.opt_raw_target, " Print raw target tree";
      "--raw-target-tree", Arg.Set Options.opt_raw_target, " Same as -RTT";
      "-LU", Arg.Set Options.opt_liq_url, " Print url of try liquidity";
      "--liquidity-url", Arg.Set Options.opt_liq_url, " Same as -LU";
      "-CWSE", Arg.Set Options.opt_cwse, " Continue with syntax errors";
      "--continue-with-syntax-errors", Arg.Set Options.opt_cwse, " Same as -CWSE";
      "--lsp", Arg.String (fun s -> match s with
          | "errors" -> Options.opt_lsp := true; Lsp.kind := Errors
          | "outline" -> Options.opt_lsp := true; Lsp.kind := Outline
          |  s ->
            Format.eprintf
              "Unknown lsp commands %s (use errors, outline)@." s;
            exit 2), "LSP mode";
      "-NR", Arg.Set Options.no_reduce, " No reduce processing";
      "--no-reduce", Arg.Set Options.no_reduce, " Same as -NR";
      "-d", Arg.Set Options.debug_mode, " Debug mode";
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
    if !Options.opt_lsp
    then Lsp.process (filename, channel)
    else compile (filename, channel);
    close dispose channel

  with
  | Stop ->
    close dispose channel
  | Compiler_error ->
    close dispose channel;
    Arg.usage arg_list arg_usage;
    exit 1
  | ArgError s ->
    close dispose channel;
    Printf.eprintf "%s.\n" s;
    exit 1
  | Error.ParseError l ->
    close dispose channel;
    (* List.map (fun (_ps, _s) -> ()) l; *)
    (* Format.eprintf "%s.\n" s *)
    exit 1
(* | Reduce.ReduceError (msg, l) ->
   close dispose channel;
   Printf.eprintf "%s%s.\n" msg (match l with | None -> "" | Some l -> (Location.tostring l));
   exit 1
   | Reduce.ErrorAcceptTransfer (str, loc, locs) ->
   close dispose channel;
   Printf.eprintf "Error: accept transfer must be set to '%s' at %s because 'transferred' is read at %s.\n" str (Location.tostring loc) (List.fold_right (fun i accu -> (Location.tostring i) ^ accu) locs "");
   exit 1 *)

(* -------------------------------------------------------------------- *)
let _ = main ()
