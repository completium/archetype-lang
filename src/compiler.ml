(* -------------------------------------------------------------------- *)
open Archetype
open Core

exception Compiler_error
exception E_arg
exception ArgError of string

(* -------------------------------------------------------------------- *)
let compile_and_print (filename, channel) =
  Tools.debug_mode := !Option.debug_mode;
  if !Option.opt_lsp
  then () (*Lsp.process (filename, channel)*)
  else (
    let pt = Io.parse_archetype ~name:filename channel in
    if !Option.opt_json
    then Format.printf "%s\n" (Yojson.Safe.to_string (ParseTree.archetype_to_yojson pt))
    else (
      if !Option.opt_pretty_print
      then () (*Format.printf "%a" Printer.pp_archetype pt*)
      else (
        if !Option.opt_parse
        then Format.printf "%a\n" ParseTree.pp_archetype pt
        else (
          let ast = Model.mk_model (Location.dumloc "mymodel") (*Translate.parseTree_to_model pt*) in
          if !Option.opt_ast
          then Format.printf "%a\n" Model.pp_model ast
          else (
            let ast = Reduce.reduce_ast ast in
            if !Option.opt_astr
            then Format.printf "%a\n" Model.pp_model ast
            else (
              let model = Translate.ast_to_model ast in
              if !Option.opt_model
              then Format.printf "%a\n" Storage.pp_model model
              else (
                match !Option.target with
                | Liquidity -> (
                    let liq_tree = Gen_liquidity.model_to_liq_tree model in
                    if !Option.opt_raw_target
                    then  Format.printf "%a\n" Gen_liquidity.pp_liq_tree liq_tree
                    else () (*TODO: pretty print liquidity tree *)
                  )
                | Whyml      -> (
                    let _decls = Gen_whyml.model_to_liq_tree model in
                    if !Option.opt_raw_target
                    then () (*TODO: raw print ptree whyml tree *)
                    else () (*TODO: pretty print ptree whyml tree *)
                  )
                | Markdown  -> () (*TODO*)
                | None      -> () (*TODO*)
              )
            ))))))

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
      "-PP", Arg.Set Option.opt_pretty_print, " Pretty print";
      "--pretty-print", Arg.Set Option.opt_pretty_print, " Same as -PP";
      "-P", Arg.Set Option.opt_parse, " Print raw parse tree";
      "-A", Arg.Set Option.opt_ast, " Print raw ast";
      "--ast", Arg.Set Option.opt_ast, " Same as -A";
      "-RA", Arg.Set Option.opt_astr, " Print raw ast";
      "--reduced-ast", Arg.Set Option.opt_astr, " Same as -RA";
      "-M", Arg.Set Option.opt_model, " Print raw model";
      "--model", Arg.Set Option.opt_model, " Same as -M";
      "--lsp", Arg.Set Option.opt_lsp, "LSP mode";
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
