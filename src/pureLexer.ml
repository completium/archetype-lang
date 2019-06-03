
open Parser
open Parser.MenhirInterpreter

type ptoken = token * Lexing.position * Lexing.position

module Lexer : sig
  type t
  val initialize : Lexing.lexbuf -> unit
  val start : t
  val next  : t -> ptoken * t
  val get   : t -> ptoken
  val get'  : t -> token
  val current_position : t -> Position.t
  val skip_until_before : (token -> bool) -> t -> t
end = struct

  let buffer =
    ref []

  let size =
    ref 0

  let more = ref (fun () -> assert false)

  let initialize lexbuf =
    more := lexer_lexbuf_to_supplier Lexer.token lexbuf

  type t = int

  let start = 0

  let get pos =
    List.nth !buffer (!size - pos)

  let token_of_ptoken (p, _, _) = p

  let current_position_of_ptoken (_, start, stop) =
    Position.lex_join start stop

  let current_position pos =
    current_position_of_ptoken (get pos)

  let get' pos =
    token_of_ptoken (get pos)

  let next pos =
    if pos >= !size - 1 then (
      buffer := !more () :: !buffer;
      incr size;
    );
    let pos = pos + 1 in
    (get pos, pos)

  let skip_until_before pred pos =
    let rec aux pos =
      let token, _, _ = get pos in
      if token = EOF then pos
      else if pred token then pos - 1
      else aux (snd (next pos))
    in
    aux pos

end
