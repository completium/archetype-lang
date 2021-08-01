open Archetype
open Compile

(* -------------------------------------------------------------------- *)
let main () =
  set_margin 300;
  let f = function
    | "michelson"         -> Options.target := Michelson
    | "michelson-storage" -> Options.target := MichelsonStorage
    | "whyml"             -> Options.target := Whyml
    | "javascript"        -> Options.target := Javascript
    | "markdown"          -> Options.target := Markdown
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
      "--list-target", Arg.Unit (fun _ -> Format.printf "target available:@\n  michelson@\n  michelson-storage@\n  whyml@\n  javascript@\n"; exit 0), " List available target languages";
      "-pt", Arg.Set Options.opt_pt, " Generate parse tree";
      "--parse-tree", Arg.Set Options.opt_pt, " Same as -pt";
      "-ast", Arg.Set Options.opt_ast, " Generate typed ast";
      "--typed-ast", Arg.Set Options.opt_ast, " Same as -ast";
      "-mdl", Arg.Set Options.opt_mdl, " Generate model";
      "--model", Arg.Set Options.opt_mdl, " Same as -mdl";
      "-omdl", Arg.Set Options.opt_omdl, " Generate optimized model";
      "--optimized-model", Arg.Set Options.opt_omdl, " Same as -omdl";
      "-fp", Arg.String (fun s -> Options.opt_property_focused := s), " Focus property (with whyml target only)";
      "--focus-property", Arg.String (fun s -> Options.opt_property_focused := s), " Same as -fp";
      "-sci", Arg.String (fun s -> Options.opt_caller := s), " Set caller address for initialization";
      "--set-caller-init", Arg.String (fun s -> Options.opt_caller := s), " Same as -sci";
      "-mu", Arg.String (fun s -> Options.opt_metadata_uri := s), " Set metadata uri";
      "--metadata-uri", Arg.String (fun s -> Options.opt_metadata_uri := s), " Same as -mu";
      "-ms", Arg.String (fun s -> Options.opt_metadata_storage := s), " Set metadata in storage";
      "--metadata-storage", Arg.String (fun s -> Options.opt_metadata_storage := s), " Same as -ms";
      "-wmt", Arg.Set Options.opt_with_metadata, " Add metadata big_map for javascript output";
      "--with-metadata", Arg.Set Options.opt_with_metadata, " Same as -wmt";
      "-lsp", Arg.String (fun s -> match s with
          | "errors" -> Options.opt_lsp := true; Lsp.kind := Errors
          | "outline" -> Options.opt_lsp := true; Lsp.kind := Outline
          |  s ->
            Format.eprintf
              "Unknown lsp commands %s (use errors, outline)@." s;
            exit 2), "<request> Generate language server protocol response to <resquest>";
      "--list-lsp-request", Arg.Unit (fun _ -> Format.printf "request available:@\n  errors@\n  outline@\n"; exit 0), " List available request for lsp";
      "--service", Arg.String (fun s -> match s with
          | "get_properties" -> Options.opt_service := true; Options.with_init_caller := false; Services.service := GetProperties
          |  s ->
            Format.eprintf
              "Unknown service %s (--list-services to view all services)@." s;
            exit 2), "<service> Generate service response to <service>";
      "--list-services", Arg.Unit (fun _ -> Format.printf "services available:@\n  get_properties@\n"; exit 0), " List available services";
      "-m", Arg.Set Options.opt_m, " Pretty print model tree";
      "--model", Arg.Set Options.opt_m, " Same as -m";
      "-r", Arg.Set Options.opt_raw, " Print raw model tree";
      "--raw", Arg.Set Options.opt_raw, " Same as -r";
      "-ry", Arg.Set Options.opt_raw_whytree, " Print raw model tree";
      "--raw-whytree", Arg.Set Options.opt_raw_whytree, " Same as -r";
      "-ir", Arg.Set Options.opt_ir, " Generate intermediate representation";
      "--intermediate-representation", Arg.Set Options.opt_ir, " Same as -ir";
      "-dir", Arg.Set Options.opt_dir, " Generate intermediate decompilation";
      "--d-intermediate-representation", Arg.Set Options.opt_dir, " Same as -dir";
      "-rdir", Arg.Set Options.opt_red_dir, " Generate reduced intermediate decompilation";
      "--reduced-d-intermediate-representation", Arg.Set Options.opt_red_dir, " Same as -rdir";
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
      "-V", Arg.String (fun s -> Options.add_vids s), "<id> process specication identifiers";
      "-v", Arg.Unit (fun () -> print_version ()), " Show version number and exit";
      "--test-mode", Arg.Set Options.opt_test_mode, " Test mode";
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

  match !Options.opt_expr with
  | Some v when not !Options.opt_with_contract -> process_expr v
  | _ -> begin

      let filename, channel, dispose =
        match !ochannel with
        | Some c -> (!ofilename, c, true)
        | _ -> ("<stdin>", stdin, false) in

      try
        begin
          match !Options.opt_lsp, !Options.opt_service, !Options.opt_decomp, !Options.opt_expr with
          | true, _, _, _   -> Lsp.process (filename, channel)
          | _, true, _, _   -> Services.process (filename, channel)
          | _, _, true, _   -> decompile (filename, channel)
          | _, _, _, Some v -> process_expr_type_channel (filename, channel) v
          | _ ->
            let res = compile_from_channel (filename, channel) in
            let fmt =
              match !Options.opt_fmt with
              | Some v -> v
              | _ -> Format.std_formatter
            in Format.fprintf fmt "%s" res
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
