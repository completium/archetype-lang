open Archetype
open Options
open Js_of_ocaml
open Tools

let set_options settings =
  Error.errors := [];
  let process_target () : target_lang =
    let target_value = Js.Unsafe.get settings "target" in
    if (Js.Opt.test target_value)
    then string_to_target_lang (Js.to_string target_value)
    else Michelson
  in

  let process f default_value property_name =
    let value = Js.Unsafe.get settings property_name in
    if (Js.Opt.test value)
    then f value
    else default_value
  in

  let process_boolean = process Js.to_bool in
  let process_string = process Js.to_string in
  let process_string_option = process (fun x -> x |> Js.to_string |> Option.some) None in

  let process_boolean_false = process_boolean false in
  let process_string_empty  = process_string "" in

  Archetype.Options.target               := process_target ();
  Archetype.Options.with_init_caller     := process_boolean true "with_init_caller";
  Archetype.Options.opt_json             := process_boolean_false "json";
  Archetype.Options.opt_rjson            := process_boolean_false "rjson";
  Archetype.Options.opt_pt               := process_boolean_false "pt";
  Archetype.Options.opt_extpt            := process_boolean_false "extpt";
  Archetype.Options.opt_ext              := process_boolean_false "ext";
  Archetype.Options.opt_ast              := process_boolean_false "ast";
  Archetype.Options.opt_mdl              := process_boolean_false "mdl";
  Archetype.Options.opt_omdl             := process_boolean_false "omdl";
  Archetype.Options.opt_typed            := process_boolean_false "typed";
  Archetype.Options.opt_ir               := process_boolean_false "ir";
  Archetype.Options.opt_dir              := process_boolean_false "dir";
  Archetype.Options.opt_mic              := process_boolean_false "mic";
  Archetype.Options.opt_mici             := process_boolean_false "mici";
  Archetype.Options.opt_all_parenthesis  := process_boolean_false "all_parenthesis";
  Archetype.Options.opt_m                := process_boolean_false "m";
  Archetype.Options.opt_raw              := process_boolean_false "raw";
  Archetype.Options.opt_raw_ir           := process_boolean_false "raw_ir";
  Archetype.Options.opt_raw_michelson    := process_boolean_false "raw_michelson";
  Archetype.Options.opt_caller           := process_string "$CALLER_ADDRESS" "caller";
  Archetype.Options.opt_decomp           := process_boolean_false "decomp";
  Archetype.Options.opt_trace            := process_boolean_false "trace";
  Archetype.Options.opt_metadata_uri     := process_string_empty "metadata_uri";
  Archetype.Options.opt_metadata_storage := process_string_empty "metadata_storage";
  Archetype.Options.opt_with_metadata    := process_boolean_false "with_metadata";
  Archetype.Options.opt_expr             := process_string_option "expr";
  Archetype.Options.opt_entrypoint       := process_string_option "entrypoint";
  Archetype.Options.opt_type             := process_string_option "type";
  Archetype.Options.opt_with_contract    := process_boolean_false "with_contract";
  Archetype.Options.opt_code_only        := process_boolean_false "code_only";
  Archetype.Options.opt_expr_only        := process_boolean_false "expr_only";
  Archetype.Options.opt_init             := process_string_empty  "init";
  Archetype.Options.opt_no_js_header     := process_boolean_false "no_js_header";
  Archetype.Options.opt_sdir             := process_boolean_false "sdir";
  Archetype.Options.opt_test_mode        := process_boolean_false "test_mode";
  Archetype.Options.opt_property_focused := process_string_empty "property_focused";
  Archetype.Options.opt_get_storage_values := process_boolean_false "get_storage_values";
  Archetype.Options.opt_with_parameters := process_boolean_false "with_parameters";
  Archetype.Options.opt_contract_interface := process_boolean_false "contract_interface";
  Archetype.Options.opt_contract_interface_michelson := process_boolean_false "contract_interface_michelson";
  Archetype.Options.opt_sandbox_exec_address := process_string_option "sandbox_exec_address";
  Archetype.Options.opt_g := process_boolean_false "g";

  (* Archetype.Options.opt_vids             := "vids" []; *)
  ()

let get_lsp_kind k =
  k
  |> Js.to_string
  |> Options.string_to_kind

let _ =
  Options.quiet := true;
  Options.opt_no_js_header := true;
  let doit f input =
    try
      input
      |> Js.to_string
      |> f
      |> Js.string
    with
    | exn ->
      let errors = !Error.errors in
      if not (List.is_empty errors)
      then begin
        let f x = Js.Unsafe.js_expr "(function (exn) { throw (new Error(exn)) })" x in
        let positions_to_string (ps : Position.t list) : string =
          Format.asprintf "%a" (Printer_tools.pp_list " " (fun fmt x ->
              Format.fprintf fmt "%s" (Position.string_of_pos x))
            ) ps
        in
        let perrors = List.map (fun (ps, e : Position.t list * string) -> Format.asprintf "%s: %s" (positions_to_string ps) e) errors in
        let input = String.concat "\n" perrors in
        f (Js.string input)
      end
      else begin
        match Js.Opt.to_option (Js.js_error_of_exn exn) with
        | None -> raise exn
        | Some err -> Js.raise_js_error err
      end
  in
  Js.export_all
    (object%js
      method compile i s = begin
        set_options s;
        doit Compile.compile_from_path i
      end
      method compileFromString i s = begin
        set_options s;
        doit Compile.compile_from_string i
      end
      method decompile i s = begin
        set_options s;
        doit Compile.decompile_from_path i
      end
      method decompileFromString i s = begin
        set_options s;
        doit Compile.decompile_from_string i
      end
      method getExpr i s = begin
        set_options s;
        doit Compile.process_expr i
      end
      method getExprType i t s = begin
        set_options s;
        let tinput = Js.to_string t in
        doit (Compile.process_expr_type_from_string ~tinput) i
      end
      method showEntries i s = begin
        set_options s;
        doit Compile.show_entries i
      end
      method lsp k p i = begin
        let kind = get_lsp_kind k in
        let p = Js.to_string p in
        doit (Lsp.process_from_string kind p) i
      end
      val version = Js.string Options.version
    end)
