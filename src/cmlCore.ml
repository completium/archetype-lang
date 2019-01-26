(* -------------------------------------------------------------------- *)
include BatPervasives

(* -------------------------------------------------------------------- *)
module String  = BatString
module List    = List
module Int     = BatInt
module Ord     = BatOrd
module Set     = BatSet
module Map     = BatMap
module Num     = BatNum
module Opt     = BatOption
module IO      = BatIO
module Lexing  = BatLexing

(* -------------------------------------------------------------------- *)
module Format = struct
  include Format

  type 'a pp = Format.formatter -> 'a -> unit
end
