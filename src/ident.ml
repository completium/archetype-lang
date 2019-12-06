(* -------------------------------------------------------------------- *)
type ident = string
[@@deriving yojson, show {with_path = false},
 visitors { variety = "map"; name = "ident_map"; polymorphic = true },
 visitors { variety = "iter"; name = "ident_iter"; polymorphic = true },
 visitors { variety = "reduce"; name = "ident_reduce"; polymorphic = true },
 visitors { variety = "reduce2"; name = "ident_reduce2"; polymorphic = true }
]

(* -------------------------------------------------------------------- *)
let cmp_ident = (String.compare : ident -> ident -> int)

(* -------------------------------------------------------------------- *)
module Mid = Tools.Map.Make(String)
module Sid = Tools.Set.Make(String)
