open Archetype
(* open ParseUtils *)

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

let make_outline_from_enum ((ek, li, l) : (ParseTree.enum_kind * 'a * 'b) ) =
  let outline = mk_outline ((match ek with | EKenum i -> (Location.unloc i) | EKstate -> "states"), symbol_kind_to_int Enum, l) in
  {outline with
   children = List.map (fun (id, _) -> mk_outline(Location.unloc id, symbol_kind_to_int EnumMember, Location.loc id) ) li }

let make_outline_from_decl (d : ParseTree.declaration) =
  let l, v = Location.deloc d in
  match v with
  | Dvariable (id, _, _, _, _, _) -> mk_outline (Location.unloc id, symbol_kind_to_int Variable, l)
  | Denum (ek, li, _) -> make_outline_from_enum (ek, li, l)
  | Dasset (id, _, _, _, _, _) -> mk_outline (Location.unloc id, symbol_kind_to_int Struct, l)
  | Daction (id, _, _, _, _) -> mk_outline (Location.unloc id, symbol_kind_to_int Function, l)
  | Dtransition (id, _, _, _, _, _, _) -> mk_outline (Location.unloc id, symbol_kind_to_int Function, l)
  | Dcontract (id, _, _, _) -> mk_outline (Location.unloc id, symbol_kind_to_int Object, l)
  | Dfunction s -> mk_outline (Location.unloc s.name, symbol_kind_to_int Function, l)
  | Dnamespace (id, _) -> mk_outline (Location.unloc id, symbol_kind_to_int Namespace, l)
  | _ -> mk_outline ("", -1, Location.dummy)

let process (filename, channel) =
  let pt = Io.parse_archetype ~name:filename channel in
  begin
    match !kind with
    | Outline -> (
        match Location.unloc pt with
        | Marchetype m -> (
            let lis = List.fold_left (fun accu d  ->
                let t = make_outline_from_decl d in
                if t.kind = -1
                then accu
                else t::accu) [] m in
            let res = {
              status = Passed;
              outlines = lis;
            } in
            Format.printf "%s\n" (Yojson.Safe.to_string (result_outline_to_yojson res)))
        | _ -> ()
      )
    | Errors -> (
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
          ))))
  end
(* with
   | ParseUtils.ParseError l ->
   mk_result Error (List.map (fun x ->
      let loc : Location.t = (
        match x with
        | PE_LexicalError (lo, _) -> lo
        | PE_Unclosed (lo, _ , _) -> lo
        | PE_Not_Expecting (lo, _) -> lo
        | PE_Unknown lo -> lo
        | _ -> Location.dummy
      ) in
      mk_item loc (string_of_perror x)) l) *)
(* ) *)
