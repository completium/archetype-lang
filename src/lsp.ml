open Options
open Tools

module PT = ParseTree

type status =
  | Passed
  | Error
  | Crash
[@@deriving yojson, show {with_path = false}]

type position = {
  line : int;
  col : int;
  char : int;
}
[@@deriving yojson, show {with_path = false}]

type range = {
  start: position;
  end_: position [@key "end"];
}
[@@deriving yojson, show {with_path = false}]

let severity_error       = 1
let severity_warning     = 2
let severity_information = 3
let severity_hint        = 4

type item = {
  severity: int;
  range : range;
  message : string;
}
[@@deriving yojson, show {with_path = false}]

type result = {
  status : status;
  items : item list;
}
[@@deriving yojson, show {with_path = false}]

type outline = {
  children: outline list;
  name : string;
  kind : int;
  start: position;
  end_: position [@key "end"];
}
[@@deriving yojson, show {with_path = false}]

type result_outline = {
  status : status;
  outlines : outline list;
}
[@@deriving yojson, show {with_path = false}]

let mk_position (line, col) char : position = {
  line = line - 1;
  col = col;
  char = char;
}

let mk_range (loc : Location.t) = {
  start = mk_position loc.loc_start loc.loc_bchar;
  end_  = mk_position loc.loc_end loc.loc_echar;
}

let mk_item (loc : Location.t) severity msg = {
  severity = severity;
  range = mk_range loc;
  message = msg;
}

let mk_outline (name, kind, (loc : Location.t)) = {
  children = [];
  name  = name;
  kind  = kind;
  start = mk_position loc.loc_start loc.loc_bchar;
  end_  = mk_position loc.loc_end loc.loc_echar;
}

let mk_result status items = {
  status = status;
  items = items;
}

let mk_result status items = {
  status = status;
  items = items;
}

type symbol_kind =
  | File
  | Module
  | Namespace
  | Package
  | Class
  | Method
  | Property
  | Field
  | Constructor
  | Enum
  | Interface
  | Function
  | Variable
  | Constant
  | String
  | Number
  | Boolean
  | Array
  | Object
  | Key
  | Null
  | EnumMember
  | Struct
  | Event
  | Operator
  | TypeParameter

let symbol_kind_to_int = function
  | File -> 1
  | Module -> 2
  | Namespace -> 3
  | Package -> 4
  | Class -> 5
  | Method -> 6
  | Property -> 7
  | Field -> 8
  | Constructor -> 9
  | Enum -> 10
  | Interface -> 11
  | Function -> 12
  | Variable -> 13
  | Constant -> 14
  | String -> 15
  | Number -> 16
  | Boolean -> 17
  | Array -> 18
  | Object -> 19
  | Key -> 20
  | Null -> 21
  | EnumMember -> 22
  | Struct -> 23
  | Event -> 24
  | Operator -> 25
  | TypeParameter -> 26

type label_exprs = label_expr list

and label_expr = (PT.lident * PT.expr) Location.loced

let mk_outline_from_label_exprs (x : label_exprs) =
  (List.map (fun (x : label_expr)  -> let id, _ = Location.unloc x in mk_outline (Location.unloc id, symbol_kind_to_int Property, Location.loc id)) x)

let make_outline_from_enum ((ek, li, l) : (PT.enum_kind * 'a * 'b) ) =
  let outline = mk_outline ((match ek with | EKenum i -> (Location.unloc i) | EKstate -> "states"), symbol_kind_to_int Enum, l) in
  outline :: (List.map (fun (id, _, _) -> mk_outline(Location.unloc id, symbol_kind_to_int EnumMember, Location.loc id) ) li) (* FIXME: enum *)
(*

  {outline with
   children = List.map (fun (id, _) -> mk_outline(Location.unloc id, symbol_kind_to_int EnumMember, Location.loc id) ) li } *)

let make_outline_from_decl (d : PT.declaration) gl =
  let l, v = Location.deloc d in
  match v with
  | Darchetype (id, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Class, gl)]
  | Dvariable (id, _, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Variable, l)]
  | Denum (ek, li) -> make_outline_from_enum (ek, li, l)
  | Dasset (id, _, _, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Struct, l)]
  | Dentry (id, _, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Function, l)]
  | Dtransition (id, _, _, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Function, l)]
  | Dfunction s -> [mk_outline (Location.unloc s.name, symbol_kind_to_int Function, l)]
  | Dnamespace (id, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Namespace, l)]
  | _ -> []

let process_crash () = Format.asprintf "%s\n" (Yojson.Safe.to_string (result_to_yojson (mk_result Crash [])))

let process_errors  ?status () =
  Format.asprintf "%s\n" (Yojson.Safe.to_string (result_to_yojson (
      let process severity ((l, str) : (Position.t list * string)) =
        let p = (
          match l with
          | [] -> Position.dummy
          | i::_ -> i
        ) in
        let s : Lexing.position = Position.start_of_position p in
        let e : Lexing.position = Position.end_of_position p in
        let loc = Location.make s e in
        mk_item loc severity str
      in
      if List.is_empty !Error.errors && List.is_empty !Error.warnings
      then mk_result (match status with | Some v -> v | None -> Passed) []
      else mk_result Error ((List.map (process severity_error) !Error.errors) @ (List.map (process severity_warning) !Error.warnings))
    )))

let process (kind : lsp_kind) (input : Core.from_input) : string =
  Options.quiet := true;
  Error.errors := [];
  Error.warnings := [];
  match kind with
  | Outline -> (
      let pt = Io.parse_archetype input in
      let gl, v =  Location.deloc pt in
      (
        let lis = List.fold_left (fun accu d  ->
            let t = make_outline_from_decl d gl in
            if List.length t = 0
            then accu
            else t@accu) [] v in
        let res = {
          status = Passed;
          outlines = lis;
        } in
        Format.asprintf "%s\n" (Yojson.Safe.to_string (result_outline_to_yojson res)))
    )
  | Errors ->
    try
      let opt =
        try
          Some (Io.parse_archetype input)
        with
        | _ -> None
      in
      match opt with
      | Some pt -> begin
          Pt_helper.check_json pt;
          if (List.is_empty !Error.errors)
          then
            let res_ast = Typing.typing (fun _ -> Typing.empty0) pt in
            if List.is_empty !Error.errors
            then
              let _ = res_ast
                      |> Gen_model.to_model
                      |> Gen_transform.check_partition_access
                      |> Gen_transform.check_containers_asset
                      |> Gen_transform.check_empty_container_on_asset_default_value
                      |> Gen_transform.remove_add_update
                      |> Gen_transform.check_init_partition_in_asset
                      |> Gen_transform.check_duplicated_keys_in_asset
                      |> Gen_transform.check_asset_key
                      |> Gen_transform.check_invalid_init_value
                      |> Gen_transform.check_unused_variables
              in
              ();
              process_errors ()
            else
              process_errors ()
          else
            process_errors ()
        end
      | None -> process_errors ~status:Crash ()
    with
    | _ -> process_errors ()

let process_from_string (kind : lsp_kind) input = process kind (FIString input)
