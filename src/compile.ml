(* -------------------------------------------------------------------- *)
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

let path_input_string p =
  match p with
  | Some v -> v
  | _ -> !Options.opt_path

let raise_if_error code f x =
  let res = f x in
  if Tools.List.is_not_empty !Error.errors then
    raise (Stop_error code)
  else
    res

let extract_pt (pt : ParseTree.archetype) : string =
  if !Options.opt_json
  then Yojson.Safe.to_string (ParseTree.archetype_to_yojson pt)
  else if !Options.opt_raw
  then Format.asprintf "%a@." ParseTree.pp_archetype pt
  else Format.asprintf "%a@." Printer_pt.pp_archetype pt

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

let output_model (mdl : Model.model) =
  if !Options.opt_raw
  then Format.printf "%a@." Model.pp_model mdl
  else Format.printf "%a@." Printer_model.pp_model mdl

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

let extract_data (d : Michelson.data) : string  =
  if !Options.opt_raw
  then Format.asprintf "%a@." Michelson.pp_data d
  else Format.asprintf "%a@." Printer_michelson.pp_data d

let output_data (d : Michelson.data) =
  if !Options.opt_raw
  then Format.printf "%a@." Michelson.pp_data d
  else Format.printf "%a@." Printer_michelson.pp_data d

let extract_obj_micheline (x : Michelson.obj_micheline) : string =
  if !Options.opt_rjson
  then Format.asprintf "%a@." Michelson.pp_obj_micheline x
  else Format.asprintf "%a@." Printer_michelson.pp_obj_micheline x

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

let output (model : Model.model) : string =
  match !Options.opt_raw, !Options.opt_m with
  | true, _ -> Format.asprintf "%a@." Model.pp_model model
  | _, true -> Format.asprintf "%a@." Printer_model.pp_model model
  | _ ->
    begin
      let printer =
        match !Options.target with
        | Michelson
        | MichelsonStorage
        | OffchainViews
        | ContractMetadata
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
                    let tz_storage_data = storage_data |> Michelson.Utils.data_to_micheline |> Michelson.to_tz_micheline in
                    Micheline_printer.print_expr fmt tz_storage_data
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
                        let micheline : Michelson.micheline = Michelson.Utils.to_micheline michelson storage_data in
                        let tz_micheline = Michelson.micheline_to_tz_micheline micheline in
                        Micheline_printer.print_expr fmt tz_micheline
                    end
                  | Javascript -> begin
                      let micheline = Michelson.Utils.to_micheline michelson storage_data in
                      Format.fprintf fmt "%a@\n@." Printer_michelson.pp_javascript micheline
                    end
                  | OffchainViews -> begin
                      let offchain_views : Michelson.offchain_view list = Gen_michelson.generate_offchain_view ir in
                      Format.fprintf fmt "%s" (Gen_extra.generate_contract_metadata ~only_views:true model offchain_views)
                    end
                  | ContractMetadata -> begin
                      let offchain_views : Michelson.offchain_view list = Gen_michelson.generate_offchain_view ir in
                      Format.fprintf fmt "%s" (Gen_extra.generate_contract_metadata ~only_views:false model offchain_views)
                    end
                  | _ -> assert false
                end
              end
          end
        | _            -> fun _fmt _ -> ()
      in
      let res = Format.asprintf "%a@." printer model in
      res
    end

let parse input : A.archetype * string =
  let path : string =
    match input with
    | FIChannel (path, _) -> path
    | FIString (path, _) -> path
  in
  let pt = Io.parse_archetype input in
  Pt_helper.check_json pt;
  match !Error.errors with
  | [] -> pt, path
  | (_, str)::_ ->
    Format.eprintf "%s" str;
    raise (Error.ParseError !Error.errors)

let type_ ?path (pt : ParseTree.archetype) : Typing.env * Ast.ast =
  let init = match !Options.opt_init with
    | "" -> None
    | x  -> Some (Io.parse_expr (FIString ((Tools.Option.map_dfl (fun x -> x) "." path), x)))
  in
  Typing.typing (fun name -> Typing.empty ?path name) pt ?init

let generate_model            = Gen_model.to_model
let generate_storage          = Gen_storage.generate_storage
let generate_api_storage      = Gen_api_storage.generate_api_storage

let print_debug b id  (f : M.model -> M.model) (model : M.model) : M.model =
  let model = f model in
  if b then (Format.printf "BEGIN TRANSFORMATION: %s@\n" id; output_model model; Format.printf "END TRANSFORMATION: %s@\n" id);
  model

let rec toolchain ?(debug=false) model =
  let process_create_contract (model : Model.model) : Model.model =
    let rec aux (mt : Model.mterm) : Model.mterm =
      match mt.node with
      | Mcreatecontract (Model.CCArl(id, args), okh, am) -> begin
          let cc_model = List.find (fun (x : Model.model) -> String.equal id (Location.unloc x.name)) model.cc_models in
          let cc_model = {cc_model with parameters = List.map (fun (p : M.parameter) ->
              if p.const
              then begin
                let ov : M.mterm option = List.assoc_opt (M.unloc_mident p.name) args in
                match ov with
                | Some v -> {p with value = Some v}
                | None -> p
              end
              else p
            ) cc_model.parameters } in
          let low_cc_model = toolchain (process_metadata cc_model)  in
          let ir = low_cc_model |> Gen_michelson.to_ir in
          let michelson = ir |> Gen_michelson.to_michelson in
          let storage_data = low_cc_model.storage |> List.filter (fun (x : M.storage_item) -> not x.no_storage) |> List.map (fun (x : M.storage_item) ->
              let v : M.mterm = match List.assoc_opt (M.unloc_mident x.id) args with | Some v -> v | None -> x.default in
              let aux (mt : M.mterm) : M.mterm =
                let rec ft (ty : M.type_) : M.type_ =
                  let process_id mid = M.mk_mident ~namespace:(cc_model.name) (snd mid) in
                  let is_empty_nm mid = Option.is_none (fst mid) in
                  (match M.get_ntype ty with
                   | M.Tasset mid when is_empty_nm mid -> (M.Tasset (process_id mid), snd ty)
                   | M.Tenum mid when is_empty_nm mid -> (M.Tenum (process_id mid), snd ty)
                   | M.Trecord mid when is_empty_nm mid -> (M.Trecord (process_id mid), snd ty)
                   | M.Tevent mid when is_empty_nm mid -> (M.Tevent (process_id mid), snd ty)
                   | _ -> M.map_type ft ty)
                in
                M.map_mterm ~ft aux mt
              in
              let v = aux v in
              (* Format.eprintf "storage: %s: %a@\n" (M.unloc_mident x.id) Printer_model.pp_mterm v; *)
              v
            ) |> M.mk_pair in
          let ms_content = Michelson.Utils.michelson_to_obj_micheline michelson in
          Model.mk_mterm (Model.Mcreatecontract(Model.CCTz({ms_content = ms_content}, storage_data), okh, am)) Model.tunit
        end
      | _ -> Model.map_mterm aux mt
    in
    Model.map_model (fun _ x -> x) (fun x -> x) aux model
  in
  let f = print_debug debug in
  model
  |> f "process_create_contract" process_create_contract
  |> f "remove_unused_function" remove_unused_function
  |> f "process_fail" process_fail
  |> f "process_inline_function" process_inline_function
  |> f "remove_import_mterm" remove_import_mterm
  |> f "getter_to_entry" (getter_to_entry ~extra:true)
  |> f "process_parameter" process_parameter
  |> f "remove_decl_var_opt" remove_decl_var_opt
  |> f "remove_ternary_operator" remove_ternary_operator
  |> f "process_multi_keys" process_multi_keys
  |> f "replace_col_by_key_for_ckfield" replace_col_by_key_for_ckfield
  |> f "move_partition_init_asset" move_partition_init_asset
  |> f "remove_enum" remove_enum
  |> f "replace_assignfield_by_update" replace_assignfield_by_update
  |> f "remove_update_all" remove_update_all
  |> f "remove_add_update" remove_add_update
  |> f "remove_container_op_in_update_exec" remove_container_op_in_update_exec
  |> f "merge_update" merge_update
  |> f "remove_empty_update" remove_empty_update
  |> f "remove_assign_operator" remove_assign_operator
  |> f "process_internal_string" process_internal_string
  |> f "remove_rational" remove_rational
  |> f "abs_tez" abs_tez
  |> f "replace_date_duration_by_timestamp" replace_date_duration_by_timestamp
  |> f "eval_variable_initial_value" eval_variable_initial_value
  |> f "replace_dotassetfield_by_dot" replace_dotassetfield_by_dot
  |> f "expr_to_instr" expr_to_instr
  |> f "generate_storage" generate_storage
  |> f "replace_declvar_by_letin" replace_declvar_by_letin
  |> f "flat_sequence" flat_sequence
  |> f "lazy_eval_condition" lazy_eval_condition
  |> f "remove_cmp_bool" remove_cmp_bool
  |> f "split_key_values" split_key_values
  |> f "remove_duplicate_key" remove_duplicate_key
  |> f "remove_asset" remove_asset
  |> f "remove_iterable_big_map" remove_iterable_big_map
  |> f "remove_storage_field_in_function" remove_storage_field_in_function
  |> f "remove_high_level_model" remove_high_level_model
  |> f "normalize_storage" normalize_storage
  |> f "remove_constant" remove_constant
  |> f "eval_storage" eval_storage
  |> f "process_arith_container" process_arith_container
  |> f "instr_to_expr_exec" instr_to_expr_exec
  |> f "optimize" optimize
  |> f "generate_api_storage" generate_api_storage
  |> f "fill_stovars" fill_stovars
  |> patch_fa2

let generate_target model =
  let _print_model m =
    Format.eprintf "%a@\n" Printer_model.pp_model m;
    m
  in

  (* let js = match !Options.target with | Javascript -> true | _ -> false in *)
  let debug = !Options.debug in

  match !Options.target with
  | Michelson
  | MichelsonStorage
  | OffchainViews
  | ContractMetadata
  | Javascript ->
    model
    |> toolchain ~debug
    |> output

  | BindingsJs ->
    Binding.process Options.Javascript model

  | BindingsTs ->
    Binding.process Options.Typescript model

  | _ -> ""

(* -------------------------------------------------------------------- *)

let compile_model (pt, path) =
  let cont c a x = if c then (a x; raise Stop) else x in
  pt
  |> cont !Options.opt_extpt output_pt
  |> raise_if_error type_error (type_ ~path)
  |> (fun c a x -> if c then (a (snd x); raise Stop) else x ) !Options.opt_ast output_tast
  |> raise_if_error model_error generate_model
  |> raise_if_error post_model_error check_partition_access
  |> raise_if_error post_model_error check_containers_asset
  (* |> raise_if_error post_model_error check_empty_container_on_initializedby *)
  |> raise_if_error post_model_error check_empty_container_on_asset_default_value
  |> raise_if_error post_model_error check_invalid_init_value
  |> raise_if_error post_model_error check_init_partition_in_asset
  |> raise_if_error post_model_error check_duplicated_keys_in_asset
  |> raise_if_error post_model_error check_asset_key
  |> process_metadata
  |> cont !Options.opt_mdl output_tmdl

let parse input =
  let pt, path = input |> raise_if_error parse_error parse in
  if !Options.opt_pt
  then (output_pt pt; raise Stop)
  else (pt, path)

let compile input =
  input
  |> parse
  |> compile_model
  |> generate_target

(* -------------------------------------------------------------------- *)

let decompile input : string =
  let cont c p (m, e) = if c then (p m; raise Stop); (m, e) in

  input
  |> parse_micheline
  |> cont !Options.opt_mici output_obj_micheline
  |> to_michelson
  |> tycheck_michelson
  |> cont !Options.opt_mic output_michelson
  |> to_dir
  |> cont !Options.opt_dir output_dprogram
  |> dir_to_model
  |> cont !Options.opt_mdl output_tmdl
  |> Opt_model.optimize
  |> cont !Options.opt_omdl output_tmdl
  |> to_archetype
  |> extract_pt

let decompile_from_string ?path (input : string) = decompile (FIString ((Tools.Option.map_dfl (fun x -> x) "." path), input))

let decompile_from_path path =
  let inc = open_in path in
  let output = decompile (FIChannel (path, inc)) in
  close_in inc;
  output

(* -------------------------------------------------------------------- *)

let print_version () =
  Format.printf "%s@\n" Options.version;
  exit 0
(* -------------------------------------------------------------------- *)

let process_expr ?tinput (input : string) : string =
  let cont c p x = if c then (p x; raise Stop); x in

  Option.iter (fun tinput ->
      Options.opt_type :=
        tinput
        |> parse_micheline ~ijson:true
        |> (fun (x, _) -> Gen_extra.extract_from_micheline "parameter" x)
        |> (fun x -> Michelson.to_type x)
        |> (fun x -> Format.asprintf "%a@." Printer_michelson.pp_type x)
        |> (fun x -> Some x)) tinput;

  FIString (path_input_string None, input)
  |> Io.parse_expr
  |> cont !Options.opt_pt output_expr_pt
  |> Gen_extra.to_model_expr
  |> begin
    fun x ->
      if !Options.opt_json then begin
        let micheline = Michelson.Utils.data_to_micheline x in
        let y = extract_obj_micheline micheline in
        match !Options.opt_type with
        | Some t -> begin
            if not !Options.opt_expr_only then
              let micheline = Michelson.Utils.type_to_micheline (Gen_extra.string_to_ttype t) in
              String.concat "" [y; extract_obj_micheline micheline]
            else y
          end
        | None -> y
      end
      else extract_data x
  end

let process_expr_type_from_string ?(tinput : string option) (input : string) =
  let tinput =
    match tinput with
    | Some v -> Some (FIString (path_input_string None, v))
    | None -> None
  in
  process_expr input ?tinput

(* let process_expr_type_string (input : string) =
   try
    process_expr input
   with
   | Stop -> ()
   | Stop_error n -> exit n
   | Error.ParseError _ -> assert false
   | _ -> () *)

(* -------------------------------------------------------------------- *)

let show_entries (input : string) =
  FIString (path_input_string None, input)
  |> parse_micheline
  |> (fun (m, _) -> Gen_extra.show_entries m)

let show_entries_from_input (input : from_input) =
  input
  |> parse_micheline
  |> (fun (m, _) -> Gen_extra.show_entries m)

(* -------------------------------------------------------------------- *)

let with_parameters input : string =
  let parameters =
    input
    |> parse
    |> compile_model
    |> (fun m -> m.parameters)
  in

  match parameters with
  | [] -> ""
  | _ ->
    Format.asprintf "(%a)@\n"
      (
        Printer_tools.pp_list ", " (fun fmt (p : Model.parameter) ->
            Format.fprintf fmt "%a : %a"
              Printer_tools.pp_mid p.name
              Printer_model.pp_type p.typ
          )) parameters

(* -------------------------------------------------------------------- *)

let show_contract_interface input : string =
  let model =
    input
    |> parse
    |> compile_model
    (* transformations *)
    (* |> process_metadata *)
  in
  let low_model = toolchain model in
  let tz = low_model |> Gen_michelson.to_ir |> Gen_michelson.to_michelson in

  Gen_contract_interface.model_to_contract_interface_json model low_model tz

let show_contract_interface_michelson input : string =
  let tze =
    input
    |> parse_micheline
    |> to_michelson
    (* |> tycheck_michelson *)
  in
  Gen_contract_interface.tz_to_contract_interface_json tze

(* -------------------------------------------------------------------- *)

type parameter = {
  name: string;
  type_: string;
  const: bool;
  default_value: string option;
}
[@@deriving yojson, show {with_path = false}]

let with_parameters input : string =
  let mk_param (m : Model.model) (p : Model.parameter) : parameter =
    let ty = Format.asprintf "%a" Printer_michelson.pp_type (Gen_michelson.to_type m p.typ) in
    let dv : string option = None in (* Option.map (fun x -> Format.asprintf "%a" Printer_michelson.pp_data (Gen_michelson.to_data x)) p.default in *)
    {
      name  = Model.unloc_mident p.name;
      type_ = ty;
      const = p.const;
      default_value = dv;
    } in

  let parameters =
    input
    |> parse
    |> compile_model
    |> (fun m -> List.map (mk_param m) m.parameters)
  in

  match parameters with
  | [] -> ""
  | _ -> Format.asprintf "[@[%a@]]@\n"
           (Printer_tools.pp_list ",@\n" (fun fmt p -> Format.fprintf fmt "%s" (Yojson.Safe.to_string (parameter_to_yojson p)))) parameters

(* -------------------------------------------------------------------- *)

let contract_interface input : string =
  let model =
    input
    |> parse
    |> compile_model
  in
  let low_model = toolchain model in
  let tz = low_model |> Gen_michelson.to_ir |> Gen_michelson.to_michelson in
  Gen_contract_interface.model_to_contract_interface_json model low_model tz

(* -------------------------------------------------------------------- *)

let get_storage_values input : string =
  let model =
    input
    |> parse
    |> compile_model
    |> toolchain
  in
  Gen_extra.get_storage_values model

let get_storage_values_from_string (input : string) : string = get_storage_values (FIString (path_input_string None, input))

(* -------------------------------------------------------------------- *)

let print_version () =
  Format.printf "%s@\n" Options.version;
  exit 0

(* -------------------------------------------------------------------- *)

let compile_gen input =
  match !Options.opt_get_storage_values, !Options.opt_with_parameters, !Options.opt_contract_interface, !Options.opt_contract_interface_michelson with
  | true, _, _, _ -> get_storage_values input
  | _, true, _, _ -> with_parameters input
  | _, _, true, _ -> contract_interface input
  | _, _, _, true -> show_contract_interface_michelson input
  | _       -> compile input

let compile_from_string input = compile (FIString (path_input_string None, input))

let compile_from_path path =
  let inc = open_in path in
  let fi = FIChannel (path, inc) in
  let output = compile_gen fi in
  close_in inc;
  output
