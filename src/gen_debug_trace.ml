open Michelson

type step = {
  range: range;
  micheline_location: int list;
  stack: obj_stack list;
}
[@@deriving yojson, show {with_path = false}]

type decl = {
  name: string;
  kind: string;
  steps: step list;
}
[@@deriving yojson, show {with_path = false}]

type debug_trace = {
  name: string;
  path: string;
  decls: decl list;
}
[@@deriving yojson, show {with_path = false}]

let generate_debug_trace_json (michelson : michelson) : debug_trace =
  let res : debug_trace = {name = ""; path = ""; decls = []} in
  res

let to_trace_json (debug_trace : debug_trace) : string =
  Format.asprintf "%s\n" (Yojson.Safe.to_string (debug_trace_to_yojson debug_trace))
