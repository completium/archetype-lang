open Archetype
open ParseUtils

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

type item = {
  start : position;
  end_ : position;
  message : string;
}
[@@deriving yojson, show {with_path = false}]

type result = {
  status : status;
  items : item list;
}
[@@deriving yojson, show {with_path = false}]

let mk_position (line, col) char : position = {
  line = line;
  col = col;
  char = char;
}

let mk_item (loc : Location.t) msg = {
  start = mk_position loc.loc_start loc.loc_bchar;
  end_ = mk_position loc.loc_end loc.loc_echar;
  message = msg;
}

let mk_result status items = {
  status = status;
  items = items;
}

let process (filename, channel) =
  let r = (
    try
      let _ = Io.parse_archetype ~name:filename channel in
      mk_result Passed []
    with
    | ParseUtils.ParseError l ->
      mk_result Error (List.map (fun x ->
          let loc : Location.t = (
            match x with
            | PE_LexicalError (lo, _) -> lo
            | PE_Unclosed (lo, _ , _) -> lo
            | PE_Not_Expecting (lo, _) -> lo
            | PE_Unknown lo -> lo
          ) in
          mk_item loc (string_of_perror x)) l)) in

  Format.printf "%s\n" (Yojson.Safe.to_string (result_to_yojson r))
