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

let pp_big_int fmt n = Format.fprintf fmt "%s" (Big_int.string_of_big_int n)
type big_int = Big_int.big_int
