(* -------------------------------------------------------------------- *)
open Archetype
open Core
open Gen_transform
open Gen_decompile

exception Compiler_error
exception E_arg
exception ArgError of string
exception Stop
exception Stop_error of int

let parse_error      = 2
let type_error       = 3
let model_error      = 4
let post_model_error = 5
let gen_output_error = 6
let other_error      = 7

let raise_if_error code f x =
  let res = f x in
  if Tools.List.is_not_empty !Error.errors then
    raise (Stop_error code)
  else
    res

let output_pt (pt : ParseTree.archetype) =
  if !Options.opt_json
  then (Format.printf "%s\n" (Yojson.Safe.to_string (ParseTree.archetype_to_yojson pt)); raise Stop)
  else if !Options.opt_raw
  then Format.printf "%a@." ParseTree.pp_archetype pt
  else Format.printf "%a@." Printer_pt.pp_archetype pt

let output_tast (ast : Ast.ast) =
  if !Options.opt_raw
  then Format.printf "%a@." Ast.pp_ast ast
  else Format.printf "%a@." Printer_ast.pp_ast ast

let output_tmdl (model : Model.model) =
  if !Options.opt_raw
  then Format.printf "%a@." Model.pp_model model
  else Format.printf "%a@." Printer_model.pp_model model

let output_ir (ir : Michelson.ir) =
  if !Options.opt_raw
  then Format.printf "%a@." Michelson.pp_ir ir
  else Format.printf "%a@." Printer_michelson.pp_ir ir

let output_dprogram (dp : Michelson.dprogram) =
  if !Options.opt_raw
  then Format.printf "%a@." Michelson.pp_dprogram dp
  else Format.printf "%a@." Printer_michelson.pp_dprogram dp

let output_michelson (m : Michelson.michelson) =
  if !Options.opt_raw
  then Format.printf "%a@." Michelson.pp_michelson m
  else Format.printf "%a@." Printer_michelson.pp_michelson m

let output (model : Model.model) =
  match !Options.opt_raw, !Options.opt_m with
  | true, _ -> Format.printf "%a@." Model.pp_model model
  | _, true -> Format.printf "%a@." Printer_model.pp_model model
  | _ ->
    begin
      let printer =
        match !Options.target with
        | Debug        -> Printer_model.pp_model
        | Ligo         -> Printer_model_ligo.pp_model
        | LigoStorage  -> Printer_model_ligo.pp_storage
        | SmartPy      -> Printer_model_smartpy.pp_model
        | Scaml        -> Printer_model_scaml.pp_model
        | Michelson
        | MichelsonStorage -> begin
            fun fmt model ->
              let ir = Gen_michelson.to_ir model in
              if !Options.opt_raw_ir
              then Format.fprintf fmt "%a@." Michelson.pp_ir ir
              else begin
                if !Options.opt_ir
                then Format.fprintf fmt "%a@." Printer_michelson.pp_ir ir
                else begin
                  match !Options.target with
                  | MichelsonStorage -> Format.fprintf fmt "%a@." Printer_michelson.pp_data ir.storage_data
                  | Michelson ->
                    let michelson = Gen_michelson.to_michelson ir in
                    if !Options.opt_raw_michelson
                    then Format.fprintf fmt "%a@." Michelson.pp_michelson michelson
                    else begin
                      Format.fprintf fmt "# %a@.%a@."
                        Printer_michelson.pp_data ir.storage_data
                        Printer_michelson.pp_michelson michelson
                    end
                  | _ -> assert false
                end
              end
          end
        | Whyml        ->
          fun fmt model ->
            let mlw = raise_if_error gen_output_error Gen_why3.to_whyml model in
            if !Options.opt_raw_whytree
            then Format.fprintf fmt "%a@." Mlwtree.pp_mlw_tree mlw
            else Format.fprintf fmt "%a@." Printer_mlwtree.pp_mlw_tree mlw
        | _            -> fun _fmt _ -> ()
      in
      Format.printf "%a@." printer model
    end

let parse (filename, channel) =
  Io.parse_archetype ~name:filename channel
(* if !Options.opt_cwse
   then Io.parse_archetype
   else Io.parse_archetype_strict) ~name:filename channel *)


let preprocess_ext (pt : ParseTree.archetype) : ParseTree.archetype =
  pt (* TODO: add extension process *)

(* let type_ (pt : ParseTree.archetype) : Ast.model =
   if !Options.fake_ast
   then Ast.create_miles_with_expiration_ast ()
   else if !Options.fake_ast2
   then Ast.create_test_shallow_ast ()
   else Typing.typing Typing.empty pt *)

let type_ (pt : ParseTree.archetype) : Ast.ast =
  Typing.typing Typing.empty pt

let generate_target_pt (pt : ParseTree.archetype) : ParseTree.archetype =
  match !Options.target with
  | Markdown  -> (
      Format.printf "%a@." Printer_pt_markdown.pp_archetype pt;
      raise Stop
    )
  | _ -> pt

let generate_model            = Gen_model.to_model
let generate_storage          = Gen_storage.generate_storage
let generate_api_storage      = Gen_api_storage.generate_api_storage

let generate_target model =

  match !Options.target with
  | Ligo
  | LigoStorage ->
    model
    |> replace_ligo_ident
    |> getter_to_entry ~no_underscore:true
    |> process_multi_keys
    |> replace_col_by_key_for_ckfield
    |> process_asset_state
    |> replace_assignfield_by_update
    |> remove_add_update
    |> remove_container_op_in_update
    |> merge_update
    |> remove_assign_operator
    |> extract_item_collection_from_add_asset
    |> process_internal_string
    |> remove_rational
    |> abs_tez
    |> replace_date_duration_by_timestamp
    |> eval_variable_initial_value
    |> replace_dotassetfield_by_dot
    |> generate_storage
    |> replace_declvar_by_letin
    |> remove_enum_matchwith
    |> replace_lit_address_by_role
    |> remove_label
    |> flat_sequence
    |> remove_cmp_bool
    |> split_key_values
    |> remove_duplicate_key
    |> assign_loop_label
    |> remove_letin_from_expr
    |> remove_fun_dotasset
    |> eval_storage
    |> optimize
    |> generate_api_storage
    |> output

  | SmartPy ->
    model
    |> replace_col_by_key_for_ckfield
    |> getter_to_entry
    |> process_multi_keys
    |> process_asset_state
    |> replace_assignfield_by_update
    |> remove_add_update
    |> remove_container_op_in_update
    |> merge_update
    |> remove_assign_operator
    |> extract_item_collection_from_add_asset
    |> process_internal_string
    |> remove_rational
    |> abs_tez
    |> replace_date_duration_by_timestamp
    |> eval_variable_initial_value
    |> replace_dotassetfield_by_dot
    |> generate_storage
    |> replace_declvar_by_letin
    |> remove_enum_matchwith
    |> replace_lit_address_by_role
    |> remove_label
    |> flat_sequence
    |> remove_cmp_bool
    |> split_key_values
    (* |> remove_duplicate_key *)
    |> assign_loop_label
    |> remove_letin_from_expr
    (* |> remove_fun_dotasset *)
    |> remove_asset
    |> optimize
    |> generate_api_storage
    |> output

  | Scaml ->
    model
    |> remove_add_update
    |> getter_to_entry
    |> process_multi_keys
    |> replace_update_by_set
    |> generate_storage
    |> replace_declvar_by_letin
    |> replace_lit_address_by_role
    |> remove_label
    |> flat_sequence
    |> remove_cmp_bool
    |> process_single_field_storage
    |> split_key_values
    |> optimize
    |> generate_api_storage
    |> output

  | Michelson
  | MichelsonStorage ->
    model
    |> prune_formula
    |> getter_to_entry ~extra:true
    |> process_multi_keys
    |> replace_col_by_key_for_ckfield
    |> process_asset_state
    |> replace_assignfield_by_update
    |> remove_add_update ~with_force:true
    |> merge_update
    |> remove_assign_operator
    |> process_internal_string
    |> remove_rational
    |> abs_tez
    |> replace_date_duration_by_timestamp
    |> eval_variable_initial_value
    |> replace_dotassetfield_by_dot
    |> generate_storage
    |> replace_declvar_by_letin
    |> remove_enum_matchwith
    |> replace_lit_address_by_role
    |> remove_label
    |> flat_sequence
    |> remove_cmp_bool
    |> split_key_values
    |> remove_duplicate_key
    |> assign_loop_label
    |> remove_letin_from_expr
    |> remove_asset
    |> remove_storage_field_in_function
    |> remove_high_level_model
    |> normalize_storage
    |> remove_constant
    |> remove_state
    |> eval_storage
    |> optimize
    |> generate_api_storage
    |> output

  | Whyml ->
    model
    |> replace_whyml_ident
    |> getter_to_entry
    |> process_multi_keys
    |> replace_assignfield_by_update
    |> process_asset_state
    |> remove_add_update ~isformula:true
    |> remove_container_op_in_update
    |> merge_update
    |> remove_assign_operator
    |> extract_item_collection_from_add_asset
    |> process_internal_string
    |> remove_rational
    |> replace_date_duration_by_timestamp
    |> eval_variable_initial_value
    |> generate_storage
    |> replace_declvar_by_letin
    (* |> add_explicit_sort *)
    (* |> remove_enum_matchwith *)
    (* |> remove_fun_dotasset *)
    |> replace_lit_address_by_role
    |> replace_label_by_mark
    |> flat_sequence
    |> remove_cmp_bool
    |> prune_properties
    (* |> shallow_asset_verif *)
    (* |> split_key_values *)
    |> Gen_transform.assign_loop_label
    |> create_var_before_for
    |> extend_loop_iter
    |> replace_for_to_iter
    |> replace_assignfield_by_update
    |> replace_update_by_set
    |> remove_cmp_enum
    |> remove_cmp_bool
    |> replace_dotassetfield_by_dot
    |> transfer_shadow_variable_to_storage
    (* |> replace_instr_verif *)
    |> eval_storage
    |> optimize
    |> generate_api_storage ~verif:true
    |> filter_api_storage
    |> output

  | Debug ->
    model
    |> raise_if_error post_model_error prune_properties
    |> process_multi_keys
    |> replace_declvar_by_letin
    |> generate_api_storage
    (* |> (fun (model : Model.model) -> Format.printf "%a@." (Printer_tools.pp_list "@\n" Printer_model.pp_type) (Model.Utils.get_all_fail_types model)) *)
    |> output

  | _ -> ()

(* -------------------------------------------------------------------- *)

let compile (filename, channel) =
  let cont c a x = if c then (a x; raise Stop) else x in

  (filename, channel)
  |> raise_if_error parse_error parse
  |> cont !Options.opt_pt output_pt
  |> raise_if_error parse_error preprocess_ext
  |> cont !Options.opt_extpt output_pt
  |> raise_if_error parse_error generate_target_pt
  |> raise_if_error type_error type_
  |> cont !Options.opt_ast output_tast
  |> raise_if_error model_error generate_model
  |> raise_if_error post_model_error check_number_entrypoint
  |> raise_if_error post_model_error check_partition_access
  |> raise_if_error post_model_error check_containers_asset
  |> raise_if_error post_model_error check_empty_container_on_initializedby
  |> raise_if_error post_model_error check_empty_container_on_asset_default_value
  |> raise_if_error post_model_error (check_and_replace_init_caller ~doit:!Options.with_init_caller)
  |> raise_if_error post_model_error check_duplicated_keys_in_asset
  |> raise_if_error post_model_error check_asset_key
  |> process_metadata
  |> cont !Options.opt_mdl output_tmdl
  |> generate_target

let decompile (filename, channel) =
  let cont c p (m, e) = if c then (p m; raise Stop); (m, e) in

  (filename, channel)
  |> parse_michelson
  |> cont !Options.opt_mic output_michelson
  |> to_dir
  |> cont !Options.opt_dir  output_dprogram
  |> to_ir
  |> cont !Options.opt_ir  output_ir
  |> to_model
  |> cont !Options.opt_mdl output_tmdl
  |> to_archetype
  |> output_pt

let close dispose channel =
  if dispose then close_in channel

(* -------------------------------------------------------------------- *)
let set_margin i =
  Format.pp_set_margin Format.std_formatter i;
  Format.pp_set_margin Format.err_formatter i

let print_version () =
  Format.printf "%s@\n" Options.version;
  exit 0

(* -------------------------------------------------------------------- *)
let main () =
  set_margin 300;
  let f = function
    | "ligo"              -> Options.target := Ligo
    | "ligo-storage"      -> Options.target := LigoStorage
    | "smartpy"           -> Options.target := SmartPy
    | "scaml"             -> Options.target := Scaml
    | "whyml"             -> Options.target := Whyml
    | "michelson"         -> Options.target := Michelson
    | "michelson-storage" -> Options.target := MichelsonStorage
    | "markdown"          -> Options.target := Markdown
    | "debug"             -> Options.target := Debug
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
      "--list-target", Arg.Unit (fun _ -> Format.printf "target available:@\n  ligo@\n  scaml (beta)@\n  whyml@\n"; exit 0), " List available target languages";
      "-pt", Arg.Set Options.opt_pt, " Generate parse tree";
      "--parse-tree", Arg.Set Options.opt_pt, " Same as -pt";
      "-ast", Arg.Set Options.opt_ast, " Generate typed ast";
      "--typed-ast", Arg.Set Options.opt_ast, " Same as -ast";
      "-mdl", Arg.Set Options.opt_mdl, " Generate model";
      "--model", Arg.Set Options.opt_mdl, " Same as -mdl";
      "-fp", Arg.String (fun s -> Options.opt_property_focused := s), " Focus property (with whyml target only)";
      "--focus-property", Arg.String (fun s -> Options.opt_property_focused := s), " Same as -fp";
      "-sci", Arg.String (fun s -> Options.opt_caller := s), " Set caller address for initialization";
      "--set-caller-init", Arg.String (fun s -> Options.opt_caller := s), " Same as -sci";
      "-mu", Arg.String (fun s -> Options.opt_metadata_uri := s), " Set metadata uri";
      "--metadata-uri", Arg.String (fun s -> Options.opt_metadata_uri := s), " Same as -mu";
      "-ms", Arg.String (fun s -> Options.opt_metadata_storage := s), " Set metadata in storage";
      "--metadata-storage", Arg.String (fun s -> Options.opt_metadata_storage := s), " Same as -ms";
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
      "-mi", Arg.Set Options.opt_mic, " Output michelson";
      "-ri", Arg.Set Options.opt_raw_ir, " Print raw intermediate representation";
      "--raw-ir", Arg.Set Options.opt_raw_ir, " Same as -ri";
      "-rm", Arg.Set Options.opt_raw_michelson, " Print raw michelson";
      "--raw-michelson", Arg.Set Options.opt_raw_michelson, " Same as -rm";
      "--trace", Arg.Set Options.opt_trace, " Activate trace";
      "-V", Arg.String (fun s -> Options.add_vids s), "<id> process specication identifiers";
      "-v", Arg.Unit (fun () -> print_version ()), " Show version number and exit";
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

  let filename, channel, dispose =
    match !ochannel with
    | Some c -> (!ofilename, c, true)
    | _ -> ("<stdin>", stdin, false) in

  try
    begin
      match !Options.opt_lsp, !Options.opt_service, !Options.opt_decomp with
      | true, _, _ -> Lsp.process (filename, channel)
      | _, true, _ -> Services.process (filename, channel)
      | _, _, true -> decompile (filename, channel)
      | _ -> compile (filename, channel)
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

(* -------------------------------------------------------------------- *)
let _ = main ()
