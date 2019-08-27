(* -------------------------------------------------------------------- *)
open Archetype
open Core

exception Compiler_error
exception E_arg
exception ArgError of string
exception Stop

let is_false_ast () : bool = !Options.fake_ast || !Options.fake_ast2

let output_pt (pt : ParseTree.archetype) =
  if !Options.opt_json
  then (Format.printf "%s\n" (Yojson.Safe.to_string (ParseTree.archetype_to_yojson pt)); raise Stop)
  else if !Options.opt_raw
  then Format.printf "%a@." ParseTree.pp_archetype pt
  else Format.printf "%a@." Printer_pt.pp_archetype pt

let output_tast (ast : Ast.model) =
  if !Options.opt_raw
  then Format.printf "%a@." Ast.pp_model ast
  else Format.printf "%a@." Printer_ast.pp_ast ast

let output_model (model : Model.model) =
  if !Options.opt_raw
  then Format.printf "%a@." Model.pp_model model
  else Format.printf "%a@." Printer_model.pp_model model

let parse (filename, channel) =
  if is_false_ast()
  then ParseTree.mk_archetype()
  else
    Io.parse_archetype ~name:filename channel
(* if !Options.opt_cwse
   then Io.parse_archetype
   else Io.parse_archetype_strict) ~name:filename channel *)


let preprocess_ext (pt : ParseTree.archetype) : ParseTree.archetype =
  if is_false_ast()
  then pt
  else pt (* TODO: add extension process *)

(* let type_ (pt : ParseTree.archetype) : Ast.model =
   if !Options.fake_ast
   then Ast.create_miles_with_expiration_ast ()
   else if !Options.fake_ast2
   then Ast.create_test_shallow_ast ()
   else Typing.typing Typing.empty pt *)

let type_ (pt : ParseTree.archetype) : Ast.model =
  Typing.typing Typing.empty pt

let generate_target_pt (pt : ParseTree.archetype) : ParseTree.archetype =
  match !Options.target with
  | Markdown  -> (
      let md = Gen_markdown.pt_to_ast_omd pt in
      Format.printf "%s@." (Omd.to_markdown md);
      raise Stop
    )
  | _ -> pt

let generate_model       = Gen_model.to_model
let shallow_asset        = Gen_shallow_asset.shallow_asset
let extend_iter          = Gen_transform.extend_loop_iter
let split_key_values     = Gen_split_key_values.split_key_values
let remove_side_effect   = Gen_reduce.reduce
let generate_api_storage = Gen_api_storage.generate_api_storage
let exec_process model   = model |> Gen_transform.replace_lit_address_by_role |> Gen_transform.remove_label |> Gen_transform.flat_sequence

let output_liquidity model =
  if !Options.opt_raw
  then Format.printf "%a@." Model.pp_model model
  else Format.printf "%a@." Printer_model_liq.pp_model model

let output_liquidity_url model =
  if !Options.opt_raw
  then Format.printf "%a@." Model.pp_model model
  else
    let str = Printer_model_liq.show_model model in
    let encoded_src = Uri.pct_encode str in
    let encoded_src = Str.global_replace (Str.regexp "\\+") "%2B" encoded_src in
    let url = "http://www.liquidity-lang.org/edit/?source=" ^ encoded_src in
    Format.printf "%s@\n" url

let output_ocaml =
  if !Options.opt_raw
  then Format.printf "%a@." Model.pp_model
  else Format.printf "%a@." Printer_model_ocaml.pp_model

let output_ligo =
  if !Options.opt_raw
  then Format.printf "%a@." Model.pp_model
  else Format.printf "%a@." Printer_model_ligo.pp_model

let output_smartpy =
  if !Options.opt_raw
  then Format.printf "%a@." Model.pp_model
  else Format.printf "%a@." Printer_model_smartpy.pp_model

let output_whyml model =
  let mlw = Gen_why3.to_whyml model in
  if !Options.opt_raw
  then Format.printf "%a@." Mlwtree.pp_mlw_tree mlw
  else Format.printf "%a@." Printer_mlwtree.pp_mlw_tree mlw



let generate_target model =

  let cont c a = if c then a else (fun x -> x) in

  match !Options.target with
  | None ->
    model
    |> cont !Options.opt_sa  shallow_asset
    |> cont !Options.opt_skv split_key_values
    |> cont !Options.opt_nse remove_side_effect
    |> generate_api_storage
    |> output_model

  | Liquidity
  | LiquidityUrl ->
    model
    |> exec_process
    |> shallow_asset
    |> split_key_values
    |> remove_side_effect
    |> generate_api_storage
    |> (match !Options.target with
        | Liquidity -> output_liquidity
        | LiquidityUrl -> output_liquidity_url
        | _ -> assert false)

  | Ligo ->
    model
    |> exec_process
    |> shallow_asset
    |> remove_side_effect
    |> generate_api_storage
    |> output_ligo

  | SmartPy ->
    model
    |> exec_process
    |> shallow_asset
    |> generate_api_storage
    |> output_smartpy

  | Ocaml ->
    model
    |> exec_process
    |> shallow_asset
    |> remove_side_effect
    |> generate_api_storage
    |> output_liquidity

  | Whyml ->
    model
    |> extend_iter
    |> shallow_asset
    |> generate_api_storage
    |> output_whyml

  | _ -> ()

(* -------------------------------------------------------------------- *)

let compile (filename, channel) =
  let cont c a x = if c then (a x; raise Stop) else x in

  (filename, channel)
  |> parse
  |> cont !Options.opt_pt output_pt
  |> preprocess_ext
  |> cont !Options.opt_pt output_pt
  |> generate_target_pt
  |> type_
  |> cont !Options.opt_ast output_tast
  |> generate_model
  |> generate_target

let close dispose channel =
  if dispose then close_in channel

(* -------------------------------------------------------------------- *)
let set_margin i =
  Format.pp_set_margin Format.std_formatter i;
  Format.pp_set_margin Format.err_formatter i

(* -------------------------------------------------------------------- *)
let main () =
  set_margin 300;
  let f = function
    | "liquidity"     -> Options.target := Liquidity
    | "liquidity_url" -> Options.target := LiquidityUrl
    | "ligo"          -> Options.target := Ligo
    | "smartpy"       -> Options.target := SmartPy
    | "ocaml"         -> Options.target := Ocaml
    | "whyml"         -> Options.target := Whyml
    | "markdown"      -> Options.target := Markdown
    |  s ->
      Format.eprintf
        "Unknown target %s (--list-target to see available target)@." s;
      exit 2 in

  let arg_list = Arg.align [
      "-t", Arg.String f, "<lang> Transcode to <lang> language";
      "--target", Arg.String f, " Same as -t";
      "--list-target", Arg.Unit (fun _ -> Format.printf "target available:@\n  liquidity@\n  liquidity_url@\n  ligo@\n  smartpy@\n  ocaml@\n  whyml@\n  markdown@\n"; exit 0), " List available target languages";
      (* "--storage-policy", Arg.String (fun s -> match s with
          | "flat" -> Options.storage_policy := Flat
          | "record" -> Options.storage_policy := Record
          |  s ->
            Format.eprintf
              "Unknown policy %s (use record, flat)@." s;
            exit 2), "<policy> Set storage policy";
         "--list-storage-policy", Arg.Unit (fun _ -> Format.printf "storage policy available:@\n  record@\n  flat@\n"; exit 0), " List storage policy"; *)
      "-pt", Arg.Set Options.opt_pt, " Generate parse tree";
      "--parse-tree", Arg.Set Options.opt_pt, " Same as -pt";
      "-ext", Arg.Set Options.opt_ext, " Process extensions";
      "--extensions", Arg.Set Options.opt_ext, " Same as -ext";
      "-ast", Arg.Set Options.opt_ast, " Generate typed ast";
      "--typed-ast", Arg.Set Options.opt_ast, " Same as -ast";
      "--typed", Arg.Set Options.opt_typed, " Display type in ast output";
      "-sa", Arg.Set Options.opt_sa, " Transform to shallow asset";
      "--shallow-asset", Arg.Set Options.opt_sa, " Same as -sa";
      "-skv", Arg.Set Options.opt_skv, " Split key value of collection of asset";
      "--split-key-values", Arg.Set Options.opt_skv, " Same as -skv";
      "-nse", Arg.Set Options.opt_nse, " Transform to no side effect";
      "--no-side-effect", Arg.Set Options.opt_nse, " Same as -nse";
      "-lsp", Arg.String (fun s -> match s with
          | "errors" -> Options.opt_lsp := true; Lsp.kind := Errors
          | "outline" -> Options.opt_lsp := true; Lsp.kind := Outline
          |  s ->
            Format.eprintf
              "Unknown lsp commands %s (use errors, outline)@." s;
            exit 2), "<request> Generate language server protocol response to <resquest>";
      "--list-lsp-request", Arg.Unit (fun _ -> Format.printf "request available:@\n  errors@\n  outline@\n"; exit 0), " List available request for lsp";
      "-r", Arg.Set Options.opt_raw, " Print raw tree";
      "--raw", Arg.Set Options.opt_raw, " Same as -r";
      "-json", Arg.Set Options.opt_json, " Print JSON format";
      "-v", Arg.String (fun s -> Options.add_vids s), "<id> process verification identifiers";
      "-F", Arg.Set Options.fake_ast, " Fake ast";
      "-F2", Arg.Set Options.fake_ast2, " Fake ast test shallow";
    ] in
  let arg_usage = String.concat "\n" [
      "usage : archetype [-t <lang> | -pt | -ext | -tast | [-sa] [-skv] [-nse] | -lsp <request>] [-r | -json] <file>";
      "";
      "Available options:";
    ]  in

  let check_flags_consistency () =
    match !Options.target with
    | None -> ()
    | _ ->
      if !Options.opt_nse
      then Format.printf "Error: side effect removing is not compatible with language: %a.@\n"
          Options.pp_target_lang !Options.target;

      if !Options.opt_sa
      then Format.printf "Error: asset shallowing is not compatible with language: %a.@\n"
          Options.pp_target_lang !Options.target;

      if !Options.opt_skv
      then Format.printf "Error: key values spliting of asset collection is not compatible with language: %a.@\n"
          Options.pp_target_lang !Options.target;

      if !Options.opt_nse || !Options.opt_sa || !Options.opt_skv
      then exit 1
  in

  let ofilename = ref "" in
  let ochannel : in_channel option ref = ref None  in
  Arg.parse arg_list (fun s -> (ofilename := s;
                                ochannel := Some (open_in s))) arg_usage;

  (* if List.length !Options.opt_vids > 0
     then (
     List.iter (fun x -> Format.printf "%s@\n" x) !Options.opt_vids;
     exit 1
     ); *)

  check_flags_consistency();

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
