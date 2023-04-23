open Archetype
open Compile
open Core

(* -------------------------------------------------------------------- *)

let close dispose channel =
  if dispose then close_in channel

let set_margin i =
  Format.pp_set_margin Format.std_formatter i;
  Format.pp_set_margin Format.err_formatter i


(* -------------------------------------------------------------------- *)

let main () : unit =
  set_margin 1000;
  let f = function
    | "michelson"         -> Options.target := Michelson
    | "michelson-storage" -> Options.target := MichelsonStorage
    | "offchain-views"    -> Options.target := OffchainViews
    | "contract-metadata" -> Options.target := ContractMetadata
    | "javascript"        -> Options.target := Javascript
    | "markdown"          -> Options.target := Markdown
    | "bindings-js"       -> Options.target := BindingsJs
    | "bindings-ts"       -> Options.target := BindingsTs
    |  s ->
      Format.eprintf
        "Unknown target %s (--list-target to see available target)@." s;
      exit 2 in

  let c   = Arg.Unit (fun _ -> f "michelson") in

  let arg_list = Arg.align [
      "-c", c, "compile to michelson";
      "--compile", c, " Same as -c";
      "-d", Arg.Set Options.opt_decomp, "decompile from michelson";
      "--decompile", Arg.Set Options.opt_decomp, " Same as -d";
      "-t", Arg.String f, "<lang> Transcode to <lang> language";
      "--target", Arg.String f, " Same as -t";
      "--list-target", Arg.Unit (fun _ -> Format.printf "target available:@\n  michelson@\n  michelson-storage@\n  offchain-views@\n  javascript@\n  bindings-js@\n  bindings-ts@\n"; exit 0), " List available target languages";
      "-o", Arg.String (fun s -> Options.opt_out := s), " Place the output into <file>";
      "--output", Arg.String (fun s -> Options.opt_out := s), " Same as -o";
      "-pt", Arg.Set Options.opt_pt, " Generate parse tree";
      "--parse-tree", Arg.Set Options.opt_pt, " Same as -pt";
      "-ast", Arg.Set Options.opt_ast, " Generate typed ast";
      "--typed-ast", Arg.Set Options.opt_ast, " Same as -ast";
      "-mdl", Arg.Set Options.opt_mdl, " Generate model";
      "--model", Arg.Set Options.opt_mdl, " Same as -mdl";
      "-omdl", Arg.Set Options.opt_omdl, " Generate optimized model";
      "--optimized-model", Arg.Set Options.opt_omdl, " Same as -omdl";
      "-sci", Arg.String (fun s -> Options.opt_caller := s), " Set caller address for initialization";
      "--set-caller-init", Arg.String (fun s -> Options.opt_caller := s), " Same as -sci";
      "-mu", Arg.String (fun s -> Options.opt_metadata_uri := s), " Set metadata uri";
      "--metadata-uri", Arg.String (fun s -> Options.opt_metadata_uri := s), " Same as -mu";
      "-ms", Arg.String (fun s -> Options.opt_metadata_storage := s), " Set metadata in storage";
      "--metadata-storage", Arg.String (fun s -> Options.opt_metadata_storage := s), " Same as -ms";
      "-wmt", Arg.Set Options.opt_with_metadata, " Add metadata big_map for javascript output";
      "--with-metadata", Arg.Set Options.opt_with_metadata, " Same as -wmt";
      "-lsp", Arg.String (fun s ->
          try
            Options.opt_lsp_kind := Some (Options.string_to_kind s)
          with
          | _ ->
            Format.eprintf
              "Unknown lsp commands %s (use errors, outline)@." s;
            exit 2), "<request> Generate language server protocol response to <resquest>";
      "--list-lsp-request", Arg.Unit (fun _ -> Format.printf "request available:@\n  errors@\n  outline@\n"; exit 0), " List available request for lsp";
      "--list-services", Arg.Unit (fun _ -> Format.printf "services available:@\n  get_properties@\n"; exit 0), " List available services";
      "-p", Arg.String (fun s -> Options.opt_path := s), " Set path";
      "--path", Arg.String (fun s -> Options.opt_path := s), " Same as -p";
      "-m", Arg.Set Options.opt_m, " Pretty print model tree";
      "--model", Arg.Set Options.opt_m, " Same as -m";
      "-r", Arg.Set Options.opt_raw, " Print raw model tree";
      "--raw", Arg.Set Options.opt_raw, " Same as -r";
      "-ir", Arg.Set Options.opt_ir, " Generate intermediate representation";
      "--intermediate-representation", Arg.Set Options.opt_ir, " Same as -ir";
      "-dir", Arg.Set Options.opt_dir, " Generate intermediate decompilation";
      "--d-intermediate-representation", Arg.Set Options.opt_dir, " Same as -dir";
      "-sdir", Arg.Set Options.opt_sdir, " ";
      "-mici", Arg.Set Options.opt_mici, " Output micheline";
      "-mi", Arg.Set Options.opt_mic, " Output michelson";
      "-ri", Arg.Set Options.opt_raw_ir, " Print raw intermediate representation";
      "--raw-ir", Arg.Set Options.opt_raw_ir, " Same as -ri";
      "-rm", Arg.Set Options.opt_raw_michelson, " Print raw michelson";
      "--raw-michelson", Arg.Set Options.opt_raw_michelson, " Same as -rm";
      "-j", Arg.Set Options.opt_json, " Json";
      "--json", Arg.Set Options.opt_json, " Same as -j";
      "-rj", Arg.Set Options.opt_rjson, " Raw Json";
      "--raw-json", Arg.Set Options.opt_rjson, " Same as -rj";
      "--trace", Arg.Set Options.opt_trace, " Activate trace";
      "--expr", Arg.String (fun s -> Options.opt_expr := Some s), " ";
      "--type", Arg.String (fun s -> Options.opt_type := Some s), " ";
      "--with-contract", Arg.Set Options.opt_with_contract, " ";
      "--entrypoint", Arg.String (fun s -> Options.opt_entrypoint := Some s), " ";
      "--only-code", Arg.Set Options.opt_code_only, " ";
      "--only-expr", Arg.Set Options.opt_expr_only, " ";
      "--init", Arg.String (fun s -> Options.opt_init := s), " Initialize parameters";
      "--no-js-header", Arg.Set Options.opt_no_js_header, " No javascript header";
      "--with-parameters", Arg.Set Options.opt_with_parameters, " With parameters";
      "--get-storage-values", Arg.Set Options.opt_get_storage_values, " Get storage values";
      "-V", Arg.String (fun s -> Options.add_vids s), "<id> process specication identifiers";
      "-v", Arg.Unit (fun () -> print_version ()), " Show version number and exit";
      "--show-entries", Arg.Set Options.opt_show_entries, " Show entries from tz file";
      "--show-contract-interface", Arg.Set Options.opt_contract_interface, " Show contract interface";
      "--show-contract-interface-michelson", Arg.Set Options.opt_contract_interface_michelson, " Show contract interface from michelson";
      "--test-mode", Arg.Set Options.opt_test_mode, " Test mode";
      "--event-well-address", Arg.String (fun s -> Options.opt_event_well_address := Some s), " Deprecated";
      "--debug", Arg.Set Options.debug, " Debug";
      "-g", Arg.Set Options.opt_g, " No michelson optimization";
      "--version", Arg.Unit (fun () -> print_version ()), " Same as -v";
    ] in
  let arg_usage = String.concat "\n" [
      "usage : archetype \
       [-t <lang> | -pt | -ast | -mdl | -ir | -c | -lsp <request> ] \
       [ -sci <caller_address> ] \
       [ -mu <json_metatdata_uri> | -ms <path_to_json_metatdata> ] \
       [ -r ] \
       <file>";
      "";
      "Available options:";
    ]  in

  let ofilename = ref "" in
  let ochannel : in_channel option ref = ref None  in
  Arg.parse arg_list (fun s -> (ofilename := s;
                                ochannel := Some (open_in s))) arg_usage;

  let output str =
    let fmt =
      match !Options.opt_out with
      | "" -> Format.std_formatter
      | v ->
        let oc = open_out v in
        Format.formatter_of_out_channel oc
    in
    Format.fprintf fmt "%s" str
  in
  if (!Options.opt_trace)
  then set_margin 3000;

  match !Options.opt_expr with
  | Some v when not !Options.opt_with_contract -> output (process_expr v)
  | _ -> begin

      let filename, channel, dispose =
        match !ochannel with
        | Some c -> (!ofilename, c, true)
        | _ -> (path_input_string None, stdin, false) in

      try
        let input = FIChannel (filename, channel) in
        begin
          let res =
            match !Options.opt_lsp_kind, !Options.opt_decomp, !Options.opt_expr, !Options.opt_get_storage_values, !Options.opt_with_parameters, !Options.opt_show_entries, !Options.opt_contract_interface, !Options.opt_contract_interface_michelson with
            | _, _, _, true, _  , _, _, _ -> get_storage_values input
            | _, _, _, _, true  , _, _, _ -> with_parameters input
            | Some k, _, _, _, _, _, _, _ -> Lsp.process k input
            | _, true, _  , _, _, _, _, _ -> decompile input
            | _, _, Some v, _, _, _, _, _ -> process_expr ~tinput:input v
            | _, _, _, _, _, true  , _, _ -> show_entries_from_input input
            | _, _, _, _, _, _, true  , _ -> show_contract_interface input
            | _, _, _, _, _, _, _  , true -> show_contract_interface_michelson input
            | _                           -> compile input
          in
          output res
        end;
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
      | Error.ParseError _ ->
        close dispose channel;
        exit 1
      | Error.Stop i
      | Stop_error i ->
        close dispose channel;
        exit i
    end

(* -------------------------------------------------------------------- *)
let _ = main ()
