(* -------------------------------------------------------------------- *)
open Archetype
open Core
open Gen_transform

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

let output_tast (ast : Ast.model) =
  if !Options.opt_raw
  then Format.printf "%a@." Ast.pp_model ast
  else Format.printf "%a@." Printer_ast.pp_ast ast

let output (model : Model.model) =
  match !Options.opt_raw, !Options.opt_m with
  | true, _ -> Format.printf "%a@." Model.pp_model model
  | _, true -> Format.printf "%a@." Printer_model.pp_model model
  | _ ->
    begin
      let printer =
        match !Options.target with
        | None         -> Printer_model.pp_model
        | Ligo         -> Printer_model_ligo.pp_model
        | LigoStorage  -> Printer_model_ligo.pp_storage
        | SmartPy      -> Printer_model_smartpy.pp_model
        | Scaml        -> Printer_model_scaml.pp_model
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

let type_ (pt : ParseTree.archetype) : Ast.model =
  Typing.typing Typing.empty pt

let generate_target_pt (pt : ParseTree.archetype) : ParseTree.archetype =
  if (!Options.opt_ptc) then (
    Format.printf "%a@." Printer_pt_type_contract.pp_archetype pt;
    raise Stop
  );
  match !Options.target with
  | Markdown  -> (
      Format.printf "%a@." Printer_pt_markdown.pp_archetype pt;
      raise Stop
    )
  | _ -> pt

let generate_model            = Gen_model.to_model
let generate_storage          = Gen_storage.generate_storage
let remove_side_effect        = Gen_reduce.reduce
let generate_api_storage      = Gen_api_storage.generate_api_storage

let generate_target model =

  match !Options.target with
  | None ->
    model
    |> raise_if_error post_model_error prune_properties
    |> replace_declvar_by_letin
    |> generate_api_storage
    |> output

  | Ligo
  | LigoStorage ->
    model
    |> replace_ligo_ident
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
    |> optimize
    |> generate_api_storage
    |> output

  | SmartPy ->
    model
    |> process_asset_state
    |> replace_assignfield_by_update
    |> remove_add_update
    |> merge_update
    |> replace_update_by_set
    |> process_internal_string
    |> remove_rational
    |> abs_tez
    |> replace_date_duration_by_timestamp
    |> eval_variable_initial_value
    |> generate_storage
    |> replace_declvar_by_letin
    |> remove_enum_matchwith
    |> remove_letin_from_expr
    (* |> remove_fun_dotasset *)
    |> replace_lit_address_by_role
    |> remove_label
    |> flat_sequence
    |> remove_cmp_bool
    |> split_key_values
    |> Gen_transform.assign_loop_label
    |> optimize
    |> generate_api_storage
    |> output

  | Scaml ->
    model
    |> remove_add_update
    |> replace_update_by_set
    |> generate_storage
    |> replace_declvar_by_letin
    |> replace_lit_address_by_role
    |> remove_label
    |> flat_sequence
    |> remove_cmp_bool
    |> process_single_field_storage
    |> split_key_values
    |> remove_side_effect
    |> optimize
    |> generate_api_storage
    |> output

  | Whyml ->
    model
    |> replace_whyml_ident
    |> replace_assignfield_by_update
    |> process_asset_state
    |> remove_add_update
    |> remove_container_op_in_update
    |> merge_update
    |> remove_assign_operator
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
    |> optimize
    |> generate_api_storage ~verif:true
    |> output

  | _ -> ()

(* -------------------------------------------------------------------- *)

let compile (filename, channel) =
  let cont c a x = if c then (a x; raise Stop) else x in

  (filename, channel)
  |> raise_if_error parse_error parse
  |> cont !Options.opt_pt output_pt
  |> raise_if_error parse_error preprocess_ext
  |> cont !Options.opt_pt output_pt
  |> raise_if_error parse_error generate_target_pt
  |> raise_if_error type_error type_
  |> cont !Options.opt_ast output_tast
  |> raise_if_error model_error generate_model
  |> raise_if_error post_model_error check_number_entrypoint
  |> raise_if_error post_model_error check_partition_access
  |> raise_if_error post_model_error check_containers_asset
  |> raise_if_error post_model_error check_empty_container_on_initializedby
  |> raise_if_error post_model_error check_and_replace_init_caller
  |> raise_if_error post_model_error check_duplicated_keys_in_asset
  |> generate_target

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
    | "ligo"          -> Options.target := Ligo
    | "ligo-storage"  -> Options.target := LigoStorage
    | "smartpy"       -> Options.target := SmartPy
    | "scaml"         -> Options.target := Scaml
    | "whyml"         -> Options.target := Whyml
    | "markdown"      -> Options.target := Markdown
    |  s ->
      Format.eprintf
        "Unknown target %s (--list-target to see available target)@." s;
      exit 2 in

  let arg_list = Arg.align [
      "-t", Arg.String f, "<lang> Transcode to <lang> language";
      "--target", Arg.String f, " Same as -t";
      "--list-target", Arg.Unit (fun _ -> Format.printf "target available:@\n  ligo@\n  scaml@\n  whyml@\n"; exit 0), " List available target languages";
      "-pt", Arg.Set Options.opt_pt, " Generate parse tree";
      "--parse-tree", Arg.Set Options.opt_pt, " Same as -pt";
      "-ext", Arg.Set Options.opt_ext, " Process extensions";
      "--extensions", Arg.Set Options.opt_ext, " Same as -ext";
      "-ast", Arg.Set Options.opt_ast, " Generate typed ast";
      "--typed-ast", Arg.Set Options.opt_ast, " Same as -ast";
      "--typed", Arg.Set Options.opt_typed, " Display type in ast output";
      "-ap", Arg.Set Options.opt_all_parenthesis, " Display all parenthesis in printer";
      "--typed", Arg.Set Options.opt_all_parenthesis, " Same as -ap";
      "-fp", Arg.String (fun s -> Options.opt_property_focused := s), " Focus property (with whyml target only)";
      "--focus-property", Arg.String (fun s -> Options.opt_property_focused := s), " Same as -fp";
      "-sci", Arg.String (fun s -> Options.opt_caller := s), " Set caller address for initialization";
      "--set-caller-init", Arg.String (fun s -> Options.opt_caller := s), " Same as -sci";
      "-ptc", Arg.Set Options.opt_ptc, " Print type contract in archetype syntax";
      "--print-type-contract", Arg.Set Options.opt_ptc, " Same as -ptc";
      "-lsp", Arg.String (fun s -> match s with
          | "errors" -> Options.opt_lsp := true; Lsp.kind := Errors
          | "outline" -> Options.opt_lsp := true; Lsp.kind := Outline
          |  s ->
            Format.eprintf
              "Unknown lsp commands %s (use errors, outline)@." s;
            exit 2), "<request> Generate language server protocol response to <resquest>";
      "--list-lsp-request", Arg.Unit (fun _ -> Format.printf "request available:@\n  errors@\n  outline@\n"; exit 0), " List available request for lsp";
      "--service", Arg.String (fun s -> match s with
          | "get_properties" -> Options.opt_service := true; Services.service := GetProperties
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
      "-json", Arg.Set Options.opt_json, " Print JSON format";
      "-V", Arg.String (fun s -> Options.add_vids s), "<id> process specication identifiers";
      "-v", Arg.Unit (fun () -> print_version ()), " Show version number and exit";
      "--version", Arg.Unit (fun () -> print_version ()), " Same as -v";
    ] in
  let arg_usage = String.concat "\n" [
      "usage : archetype [-t <lang> | -pt | -ext | -tast | [-ws] [-sa] [-skv] [-nse] | -lsp <request>] [-r | -json] <file>";
      "";
      "Available options:";
    ]  in

  let ofilename = ref "" in
  let ochannel : in_channel option ref = ref None  in
  Arg.parse arg_list (fun s -> (ofilename := s;
                                ochannel := Some (open_in s))) arg_usage;

  (* if List.length !Options.opt_vids > 0
     then (
     List.iter (fun x -> Format.printf "%s@\n" x) !Options.opt_vids;
     exit 1
     ); *)

  let filename, channel, dispose =
    match !ochannel with
    | Some c -> (!ofilename, c, true)
    | _ -> ("<stdin>", stdin, false) in

  try
    begin
      match !Options.opt_lsp, !Options.opt_service with
      | true, _ -> Lsp.process (filename, channel)
      | _, true -> Services.process (filename, channel)
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
    (* List.map (fun (_ps, _s) -> ()) l; *)
    (* Format.eprintf "%s.\n" s *)
    exit 1
  | Error.Stop i
  | Stop_error i ->
    close dispose channel;
    exit i
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
