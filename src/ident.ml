(* -------------------------------------------------------------------- *)
type ident = string
[@@deriving yojson]

let pp_ident pp fmt (x : ident) = Format.fprintf fmt "%a" pp x


(* -------------------------------------------------------------------- *)
let cmp_ident = (String.compare : ident -> ident -> int)
