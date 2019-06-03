(* -------------------------------------------------------------------- *)
type ident = string
[@@deriving yojson, show {with_path = false}]

(* -------------------------------------------------------------------- *)
let cmp_ident = (String.compare : ident -> ident -> int)

(* -------------------------------------------------------------------- *)
module Mid = Tools.Map.Make(String)
module Sid = Tools.Set.Make(String)
