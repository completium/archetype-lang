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

let output_expr_pt (e : ParseTree.expr) =
  if !Options.opt_json
  then (Format.printf "%s\n" (Yojson.Safe.to_string (ParseTree.expr_to_yojson e)); raise Stop)
  else if !Options.opt_raw
  then Format.printf "%a@." ParseTree.pp_expr e
  else Format.printf "%a@." Printer_pt.pp_simple_expr e

let output_tast (ast : Ast.ast) =
  if !Options.opt_raw
  then Format.printf "%a@." Ast.pp_ast ast
  else Format.printf "%a@." Printer_ast.pp_ast ast

let output_tmdl (model : Model.model) =
  if !Options.opt_raw
  then Format.printf "%a@." Model.pp_model model
  else Format.printf "%a@." Printer_model.pp_model model

let output_mdl_mterm (mt : Model.mterm) =
  if !Options.opt_raw
  then Format.printf "%a@." Model.pp_mterm mt
  else Format.printf "%a@." Printer_model.pp_mterm mt

let output_ir (ir : Michelson.ir) =
  if !Options.opt_raw
  then Format.printf "%a@." Michelson.pp_ir ir
  else Format.printf "%a@." Printer_michelson.pp_ir ir

let output_dprogram (dp : Michelson.dprogram) =
  if !Options.opt_raw
  then Format.printf "%a@." Michelson.pp_dprogram dp
  else Format.printf "%a@." Printer_michelson.pp_dprogram dp

let output_obj_micheline (o : Michelson.obj_micheline) =
  if !Options.opt_raw
  then Format.printf "%a@." Michelson.pp_obj_micheline o
  else Format.printf "%a@." Printer_michelson.pp_obj_micheline o

let output_micheline (m : Michelson.micheline) =
  if !Options.opt_raw
  then Format.printf "%a@." Michelson.pp_micheline m
  else Format.printf "%a@." Printer_michelson.pp_micheline m

let output_michelson (m : Michelson.michelson) =
  if !Options.opt_raw
  then Format.printf "%a@." Michelson.pp_michelson m
  else Format.printf "%a@." Printer_michelson.pp_michelson m

let output_data (d : Michelson.data) =
  if !Options.opt_raw
  then Format.printf "%a@." Michelson.pp_data d
  else Format.printf "%a@." Printer_michelson.pp_data d

let output_obj_micheline (x : Michelson.obj_micheline) =
  if !Options.opt_rjson
  then Format.printf "%a@." Michelson.pp_obj_micheline x
  else Format.printf "%a@." Printer_michelson.pp_obj_micheline x

let remove_newline (input : string) : string =
  let a : string ref = ref "" in
  let w : bool ref = ref false in
  for i = 0 to String.length input - 1 do
    let c : char = input.[i] in
    if c == '\n'
    then ()
    else begin
      if c == ' '
      then begin
        if !w
        then ()
        else a := !a ^ " ";
        w := true
      end
      else begin
        w := false;
        a := !a ^ (String.make 1 c)
      end
    end
  done;
  !a

let output (model : Model.model) =
  match !Options.opt_raw, !Options.opt_m with
  | true, _ -> Format.printf "%a@." Model.pp_model model
  | _, true -> Format.printf "%a@." Printer_model.pp_model model
  | _ ->
    begin
      let printer =
        match !Options.target with
        | Michelson
        | MichelsonStorage
        | Javascript -> begin
            fun fmt model ->
              let ir = Gen_michelson.to_ir model in
              let storage_data = Gen_michelson.process_data ir.storage_data in
              if !Options.opt_raw_ir
              then Format.fprintf fmt "%a@." Michelson.pp_ir ir
              else begin
                if !Options.opt_ir
                then Format.fprintf fmt "%a@." Printer_michelson.pp_ir ir
                else begin
                  let michelson = Gen_michelson.to_michelson ir in
                  match !Options.target with
                  | MichelsonStorage ->
                    let output = Format.asprintf "%a" Printer_michelson.pp_data storage_data |> remove_newline in
                    Format.fprintf fmt "%s" output
                  | Michelson ->
                    if !Options.opt_raw_michelson
                    then Format.fprintf fmt "%a@." Michelson.pp_michelson michelson
                    else begin
                      if !Options.opt_json then
                        let micheline = Michelson.Utils.to_micheline michelson storage_data in
                        if !Options.opt_code_only
                        then begin
                          let code = micheline.code in
                          output_obj_micheline (Michelson.Oarray code)
                        end
                        else begin
                          if !Options.opt_rjson
                          then Format.fprintf fmt "%a@." Michelson.pp_micheline micheline
                          else Format.fprintf fmt "%a@." Printer_michelson.pp_micheline micheline
                        end
                      else
                        let sdata = Format.asprintf "%a" Printer_michelson.pp_data storage_data |> remove_newline in
                        Format.fprintf fmt "# %s@.%a@."
                          sdata
                          Printer_michelson.pp_michelson michelson
                    end
                  | Javascript -> begin
                      let micheline = Michelson.Utils.to_micheline michelson storage_data in
                      Format.fprintf fmt "%a@\n@." Printer_michelson.pp_javascript micheline
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
  let pt = Io.parse_archetype ~name:filename channel in
  Pt_helper.check_json pt;
  match !Error.errors with
  | [] -> pt
  | (_, str)::_ ->
    Format.eprintf "%s" str;
    raise (Error.ParseError !Error.errors)


let preprocess_ext (pt : ParseTree.archetype) : ParseTree.archetype =
  pt (* TODO: add extension process *)

(* let type_ (pt : ParseTree.archetype) : Ast.model =
   if !Options.fake_ast
   then Ast.create_miles_with_expiration_ast ()
   else if !Options.fake_ast2
   then Ast.create_test_shallow_ast ()
   else Typing.typing Typing.empty pt *)

let type_ (pt : ParseTree.archetype) : Ast.ast =
  let init = match !Options.opt_init with
    | "" -> None
    | x  -> Some (Io.parse_expr x)
  in
  Typing.typing Typing.empty pt ?init

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

  let _print_model m =
    Format.eprintf "%a@\n" Printer_model.pp_model m;
    m
  in

  let js = match !Options.target with | Javascript -> true | _ -> false in

  match !Options.target with
  | Michelson
  | MichelsonStorage
  | Javascript ->
    model
    |> prune_formula
    |> getter_to_entry ~extra:true
    |> process_parameter ~js:js
    |> process_multi_keys
    |> replace_col_by_key_for_ckfield
    |> move_partition_init_asset
    |> remove_enum
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
    |> remove_label
    |> flat_sequence
    |> remove_cmp_bool
    |> split_key_values
    |> remove_duplicate_key
    |> assign_loop_label
    |> remove_asset
    |> remove_storage_field_in_function
    |> remove_high_level_model
    |> normalize_storage
    |> remove_constant
    |> eval_storage
    |> expr_to_instr
    |> instr_to_expr_exec
    |> optimize
    |> generate_api_storage
    |> output

  | Whyml ->
    model
    |> replace_whyml_ident
    |> getter_to_entry
    |> process_parameter
    |> process_multi_keys
    |> replace_assignfield_by_update
    |> remove_enum
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
    |> replace_label_by_mark
    |> flat_sequence
    |> remove_cmp_bool
    |> prune_properties
    |> Gen_transform.assign_loop_label
    |> create_var_before_for
    |> extend_loop_iter
    |> replace_for_to_iter
    |> replace_assignfield_by_update
    |> replace_update_by_set
    |> remove_cmp_bool
    |> replace_dotassetfield_by_dot
    |> transfer_shadow_variable_to_storage
    |> eval_storage
    |> optimize
    |> generate_api_storage ~verif:true
    |> filter_api_storage
    |> fix_container
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
  (* |> raise_if_error post_model_error check_empty_container_on_initializedby *)
  |> raise_if_error post_model_error check_empty_container_on_asset_default_value
  |> raise_if_error post_model_error (check_and_replace_init_caller ~doit:!Options.with_init_caller)
  |> raise_if_error post_model_error check_init_partition_in_asset
  |> raise_if_error post_model_error check_duplicated_keys_in_asset
  |> raise_if_error post_model_error check_asset_key
  |> process_metadata
  |> cont !Options.opt_mdl output_tmdl
  |> generate_target

let decompile (filename, channel) =
  let cont c p (m, e) = if c then (p m; raise Stop); (m, e) in

  (filename, channel)
  |> parse_micheline
  |> cont !Options.opt_mici output_obj_micheline
  |> to_michelson
  |> cont !Options.opt_mic output_michelson
  |> to_dir
  |> cont !Options.opt_dir output_dprogram
  |> to_red_dir
  |> cont !Options.opt_red_dir output_dprogram
  |> to_ir
  |> cont !Options.opt_ir  output_ir
  |> to_model
  |> cont !Options.opt_mdl output_tmdl
  |> Opt_model.optimize
  |> cont !Options.opt_omdl output_tmdl
  |> to_archetype
  |> output_pt

let showEntries (filename, channel) =
  (filename, channel)
  |> parse_micheline
  |> (fun (m, _) -> Gen_extra.show_entries m)

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

let process_expr (input : string) =
  let cont c p x = if c then (p x; raise Stop); x in

  input
  |> Io.parse_expr
  |> cont !Options.opt_pt output_expr_pt
  |> Gen_extra.to_model_expr
  |> begin
    fun x ->
      if !Options.opt_json then begin
        let micheline = Michelson.Utils.data_to_micheline x in
        output_obj_micheline micheline;
        match !Options.opt_type with
        | Some t -> begin
            if not !Options.opt_expr_only then
              let micheline = Michelson.Utils.type_to_micheline (Gen_extra.string_to_ttype t) in
              output_obj_micheline micheline;
          end
        | None -> ()
      end
      else output_data x
  end

let process_expr_type_channel (filename, channel) (input : string) =
  Options.opt_type :=
    (filename, channel)
    |> parse_micheline ~ijson:true
    |> (fun (x, _) -> Gen_extra.extract_from_micheline "parameter" x)
    |> (fun x -> Michelson.to_type x)
    |> (fun x -> Format.asprintf "%a@." Printer_michelson.pp_type x)
    |> (fun x -> Some x);
  (* Format.printf "%s@\n@." (!Options.opt_type |> Option.get) *)
  process_expr input

let process_expr_type_string (input : string) =
  try
    process_expr input
  with
  | Stop -> ()
  | Stop_error n -> exit n
  | Error.ParseError _ -> assert false
  | _ -> ()

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
      "--show-entries", Arg.Set Options.opt_show_entries, " Show entries";
      "--entrypoint", Arg.String (fun s -> Options.opt_entrypoint := Some s), " ";
      "--only-code", Arg.Set Options.opt_code_only, " ";
      "--only-expr", Arg.Set Options.opt_expr_only, " ";
      "--init", Arg.String (fun s -> Options.opt_init := s), " Initialize parameters";
      "--no-js-header", Arg.Set Options.opt_no_js_header, " No javascript header";
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

  match !Options.opt_expr with
  | Some v when not !Options.opt_with_contract -> process_expr v
  | _ -> begin

      let filename, channel, dispose =
        match !ochannel with
        | Some c -> (!ofilename, c, true)
        | _ -> ("<stdin>", stdin, false) in

      try
        begin
          match !Options.opt_lsp, !Options.opt_service, !Options.opt_decomp, !Options.opt_show_entries, !Options.opt_expr with
          | true, _, _, _, _ -> Lsp.process (filename, channel)
          | _, true, _, _, _ -> Services.process (filename, channel)
          | _, _, true, _, _ -> decompile (filename, channel)
          | _, _, _, true, _ -> showEntries (filename, channel)
          | _, _, _, _, Some v -> process_expr_type_channel (filename, channel) v
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
    end

(* -------------------------------------------------------------------- *)
let _ = main ()
