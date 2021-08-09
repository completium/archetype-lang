open Options
open Tools

module PT = ParseTree

type status =
  | Passed
  | Error
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

type item = {
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

let mk_item (loc : Location.t) msg = {
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

let mk_outline_from_label_exprs (x : PT.label_exprs) =
  (List.map (fun (x : PT.label_expr)  -> let id, _ = Location.unloc x in mk_outline (Location.unloc id, symbol_kind_to_int Property, Location.loc id)) x)

let mk_outline_post_options (post_options : PT.asset_post_option list) =
  let aux (a : PT.asset_post_option) =
    match a with
    | PT.APOconstraints l ->
      List.map (fun (x : PT.label_expr) ->
          let lo, v = Location.deloc x in
          let id, _formula = v in
          [mk_outline (Location.unloc id, symbol_kind_to_int Property, lo)]
        ) l
      |> List.flatten
    | _ -> []
  in
  post_options
  |> List.map aux
  |> List.flatten

let mk_outline_from_invariants (invariants : PT.invariants) =
  List.map (fun x ->
      let id, _ = x in
      mk_outline (Location.unloc id, symbol_kind_to_int Property, Location.loc id)
    ) invariants

let mk_outline_from_specification (spec : PT.specification) =
  let vis, _ = Location.unloc spec in

  List.fold_right (fun (i : PT.specification_item) accu ->
      let l, v = Location.deloc i in
      match v with
      | Vassert (id, _, ivs, _)
      | Vpostcondition (id, _, ivs, _, Some PKPost) ->
        [(mk_outline (Location.unloc id, symbol_kind_to_int Property, l))]
        @ mk_outline_from_invariants ivs
        @ accu
      | _ -> accu) vis []

let mk_outline_from_security (sec : PT.security) =
  let sec_loc, sec = Location.deloc sec in
  mk_outline ("security", symbol_kind_to_int Namespace, sec_loc)
  ::(
    List.fold_right (fun (x : PT.security_item) accu ->
        let secitem_loc, (id, _, _) = Location.deloc x in
        (mk_outline (Location.unloc id, symbol_kind_to_int Property, secitem_loc))::accu) (fst sec) []
  )


let make_outline_from_enum ((ek, li, l) : (PT.enum_kind * 'a * 'b) ) =
  let outline = mk_outline ((match ek with | EKenum i -> (Location.unloc i) | EKstate -> "states"), symbol_kind_to_int Enum, l) in
  outline :: (List.map (fun (id, _, _) -> mk_outline(Location.unloc id, symbol_kind_to_int EnumMember, Location.loc id) ) li) (* FIXME: enum *)
(*

  {outline with
   children = List.map (fun (id, _) -> mk_outline(Location.unloc id, symbol_kind_to_int EnumMember, Location.loc id) ) li } *)

let make_outline_from_decl (d : PT.declaration) gl =
  let l, v = Location.deloc d in
  match v with
  | Darchetype (id, _, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Class, gl)]
  | Dvariable (id, _, _, _, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Variable, l)]
  | Denum (ek, (li, _)) -> make_outline_from_enum (ek, li, l)
  | Dasset (id, _, _, _, post_options, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Struct, l)] @ mk_outline_post_options post_options
  | Dentry (id, _, ap, _, _) -> mk_outline (Location.unloc id, symbol_kind_to_int Function, l) :: (Option.map_dfl mk_outline_from_specification [] ap.spec_fun)
  | Dtransition (id, _, _, _, _, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Function, l)]
  | Dfunction s -> [mk_outline (Location.unloc s.name, symbol_kind_to_int Function, l)]
  | Dnamespace (id, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Namespace, l)]
  | Dspecification spec -> mk_outline_from_specification spec
  | Dsecurity sec -> mk_outline_from_security sec
  | _ -> []

let process_errors () =
  Format.asprintf "%s\n" (Yojson.Safe.to_string (result_to_yojson (
      let li = Error.errors in
      match !li with
      | [] -> mk_result Passed []
      | l -> mk_result Error (List.map (fun (p : (Position.t list * string)) ->
          let l, str = p in
          let p = (
            match l with
            | [] -> Position.dummy
            | i::_ -> i
          ) in
          let s : Lexing.position = Position.start_of_position p in
          let e : Lexing.position = Position.end_of_position p in
          let loc = Location.make s e in
          mk_item loc str) l)
    )))

let process (kind : lsp_kind) (input : Core.from_input) : string =
  Error.errors := [];
  match kind with
  | Outline -> (
      let pt = Io.parse_archetype input in
      let gl, v =  Location.deloc pt in
      match v with
      | Marchetype m -> (
          let lis = List.fold_left (fun accu d  ->
              let t = make_outline_from_decl d gl in
              if List.length t = 0
              then accu
              else t@accu) [] m in
          let res = {
            status = Passed;
            outlines = lis;
          } in
          Format.asprintf "%s\n" (Yojson.Safe.to_string (result_outline_to_yojson res)))
      | _ -> ""
    )
  | Errors ->
    try
      let pt = Io.parse_archetype input in
      Pt_helper.check_json pt;
      if (List.is_empty !Error.errors)
      then
        let ast = Typing.typing Typing.empty pt in
        if List.is_empty !Error.errors
        then
          let _ = ast
                  |> Gen_model.to_model
                  |> Gen_transform.check_number_entrypoint
                  |> Gen_transform.check_partition_access
                  |> Gen_transform.check_containers_asset
                  |> Gen_transform.check_empty_container_on_asset_default_value
                  |> Gen_transform.remove_add_update
                  |> Gen_transform.check_init_partition_in_asset
                  |> Gen_transform.check_duplicated_keys_in_asset
                  |> Gen_transform.check_asset_key
          in
          ();
          process_errors ()
        else
          process_errors ()
      else
        process_errors ()
    with
    | _ -> process_errors ()

let process_from_string (kind : lsp_kind) input = process kind (FIString input)
