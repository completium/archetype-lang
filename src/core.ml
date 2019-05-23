(* -------------------------------------------------------------------- *)
(*include BatPervasives*)

(* -------------------------------------------------------------------- *)
(*module String  = BatString
  module List    = List
  module Int     = BatInt
  module Ord     = BatOrd
  module Set     = BatSet
  module Map     = BatMap
  module Num     = BatNum
  module Opt     = BatOption
  module IO      = BatIO
  module Lexing  = BatLexing*)

(* -------------------------------------------------------------------- *)
module Format = struct
  include Format

  type 'a pp = Format.formatter -> 'a -> unit
end

type big_int = Big_int.big_int

let pp_big_int fmt n = Format.fprintf fmt "%s" (Big_int.string_of_big_int n)

let big_int_of_yojson (s : Yojson.Safe.json) : (big_int, string) Result.result =
  try Ok (Big_int.big_int_of_string (Yojson.Safe.to_string s))
  with _ -> Error "big_int_of_yojson"

let big_int_to_yojson (n : big_int) : Yojson.Safe.json = Yojson.Safe.from_string (Big_int.string_of_big_int n)
