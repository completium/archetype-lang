open Archetype
open Tools

type kind =
  | Errors
  | Outline
[@@deriving yojson, show {with_path = false}]

let kind = ref Errors

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

let mk_outline_from_label_exprs (x : ParseTree.label_exprs) =
  (List.map (fun (x : ParseTree.label_expr)  -> let i, _ = Location.unloc x in let id = (Option.get i) in mk_outline (Location.unloc id, symbol_kind_to_int Property, Location.loc id)) x)

let mk_outline_post_options (post_options : ParseTree.asset_post_option list) =
  let aux (a : ParseTree.asset_post_option) =
    match a with
    | ParseTree.APOconstraints l ->
      List.map (fun (x : ParseTree.label_expr) ->
          let lo, v = Location.deloc x in
          let id, formula = v in
          match id with
          | Some id -> [mk_outline (Location.unloc id, symbol_kind_to_int Property, lo)]
          | _ -> []
        ) l
      |> List.flatten
    | _ -> []
  in
  post_options
  |> List.map aux
  |> List.flatten

let mk_outline_from_invariants (invariants : ParseTree.invariants) =
  List.map (fun x ->
      let id, _ = x in
      mk_outline (Location.unloc id, symbol_kind_to_int Property, Location.loc id)
    ) invariants

let mk_outline_from_verification (verif : ParseTree.verification) =
  let vis, _ = Location.unloc verif in

  List.fold_right (fun (i : ParseTree.verification_item) accu ->
      let l, v = Location.deloc i in
      match v with
      | Vassert (id, _, ivs)
      | Vspecification (id, _, ivs) ->
        [(mk_outline (Location.unloc id, symbol_kind_to_int Property, l))]
        @ mk_outline_from_invariants ivs
        @ accu
      | _ -> accu) vis []

let make_outline_from_enum ((ek, li, l) : (ParseTree.enum_kind * 'a * 'b) ) =
  let outline = mk_outline ((match ek with | EKenum i -> (Location.unloc i) | EKstate -> "states"), symbol_kind_to_int Enum, l) in
  outline :: (List.map (fun (id, _) -> mk_outline(Location.unloc id, symbol_kind_to_int EnumMember, Location.loc id) ) li)
(*

  {outline with
   children = List.map (fun (id, _) -> mk_outline(Location.unloc id, symbol_kind_to_int EnumMember, Location.loc id) ) li } *)

let make_outline_from_decl (d : ParseTree.declaration) gl =
  let l, v = Location.deloc d in
  match v with
  | Darchetype (id, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Class, gl)]
  | Dvariable (id, _, _, _, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Variable, l)]
  | Dinstance (id, _, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Variable, l)]
  | Denum (ek, (li, _)) -> make_outline_from_enum (ek, li, l)
  | Dasset (id, _, _, post_options, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Struct, l)] @ mk_outline_post_options post_options
  | Daction (id, _, ap, _, _) -> mk_outline (Location.unloc id, symbol_kind_to_int Function, l) :: (Option.map_dfl mk_outline_from_verification [] ap.verif)
  | Dtransition (id, _, _, _, _, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Function, l)]
  | Dcontract (id, _, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Object, l)]
  | Dfunction s -> [mk_outline (Location.unloc s.name, symbol_kind_to_int Function, l)]
  | Dnamespace (id, _) -> [mk_outline (Location.unloc id, symbol_kind_to_int Namespace, l)]
  | Dverification verif -> mk_outline_from_verification verif
  | _ -> []

let process_errors () =
  Format.printf "%s\n" (Yojson.Safe.to_string (result_to_yojson (
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

let process (filename, channel) =
  match !kind with
  | Outline -> (
      let pt = Io.parse_archetype ~name:filename channel in
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
          Format.printf "%s\n" (Yojson.Safe.to_string (result_outline_to_yojson res)))
      | _ -> ()
    )
  | Errors ->
    try
      let pt = Io.parse_archetype ~name:filename channel in
      if (List.is_empty !Error.errors)
      then ( let _ = Typing.typing Typing.empty pt in ());
      process_errors ()
    with
    | _ -> process_errors ()
