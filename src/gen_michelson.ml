module M = Model
module T = Michelson

(* module T = Tezos_micheline.Micheline *)

let to_michelson (_model : M.model) =
  let storage = T.Tstring in
  let parameter = T.Tunit in
  let code = T.SEQ [T.PUSH (Tstring, T.Dstring "hello"); T.NIL (Toperation); T.PAIR; T.DIP (0, [T.DROP 0]) ] in
  T.mk_michelson storage parameter code
