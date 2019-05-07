open Archetype

type status =
  | Passed
  | Error
[@@deriving yojson, show {with_path = false}]

type position = {
  line : int;
  col : int;
}
[@@deriving yojson, show {with_path = false}]

type result = {
  status : status;
  start : position;
  end_ : position;
  message : string;
}
[@@deriving yojson, show {with_path = false}]

let mk_position (line, col) : position = {
  line = line;
  col = col;
}

let mk_result status (loc : Location.t) msg = {
  status = status;
  start = mk_position loc.loc_start;
  end_ = mk_position loc.loc_end;
  message = msg;
}

let process (filename, channel) =
  let st, loc, msg = (
    try
      let _ = Io.parse_archetype ~name:filename channel in
      (Passed, Location.dummy, "")
    with
    | ParseUtils.ParseError (loc, error) ->
      let msg : string = (match error with
          | PE_LexicalError s -> s
          | PE_Unknown -> "syntax error") in

      let loc : Location.t = (match loc with
          | Some l -> l
          | None -> Location.dummy) in
      (Error, loc,msg)) in
  let r : result = mk_result st loc msg in
  Format.printf "%s\n" (Yojson.Safe.to_string (result_to_yojson r))
