(* -------------------------------------------------------------------- *)
module P = Parser
module L = Lexing
module I = P.MenhirInterpreter

exception ParseError

(* -------------------------------------------------------------------- *)
let parserfun_entry =
  MenhirLib.Convert.Simplified.traditional2revised P.main

(* -------------------------------------------------------------------- *)
let lexbuf_from_channel = fun name channel ->
  let lexbuf = Lexing.from_channel channel in
  lexbuf.Lexing.lex_curr_p <- {
    Lexing.pos_fname = name;
    Lexing.pos_lnum  = 1;
    Lexing.pos_bol   = 0;
    Lexing.pos_cnum  = 0
  };
  lexbuf

(* -------------------------------------------------------------------- *)
let lexer (lexbuf : L.lexbuf) =
  let token = Lexer.token lexbuf in
  (token, L.lexeme_start_p lexbuf, L.lexeme_end_p lexbuf)

let parse_archetype ?(name = "") (inc : in_channel) =
  let reader = lexbuf_from_channel name inc in
  parserfun_entry (fun () -> lexer reader)


(* The loop which drives the parser. At each iteration, we analyze a
   checkpoint produced by the parser, and act in an appropriate manner.
   [lexbuf] is the lexing buffer. [checkpoint] is the last checkpoint produced
   by the parser. *)
let rec loop lexbuf (checkpoint : ParseTree.archetype I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
    (* The parser needs a token. Request one from the lexer,
       and offer it to the parser, which will produce a new
       checkpoint. Then, repeat. *)
    let token = (match Lexer.token lexbuf with
        | P.LBRACE -> P.LBRACE
        | v -> v) in
    let startp = lexbuf.lex_start_p
    and endp = lexbuf.lex_curr_p in

    let checkpoint = I.offer checkpoint (token, startp, endp) in
    loop lexbuf checkpoint
  | I.Shifting _
  | I.AboutToReduce _ ->
    let checkpoint = I.resume checkpoint in
    loop lexbuf checkpoint
  | I.HandlingError _env ->
    (* The parser has suspended itself because of a syntax error. Stop. *)
    Printf.fprintf stderr
      "At offset %d: syntax error.\n%!"
      (L.lexeme_start lexbuf);
    let token = P.BREAK in
    let startp = L.dummy_pos
    and endp = L.dummy_pos in
    let checkpoint = I.offer checkpoint (token, startp, endp) in
    loop lexbuf checkpoint
  | I.Accepted v ->
    (* The parser has succeeded and produced a semantic value. Print it. *)
    v
  | I.Rejected ->
    (* The parser rejects this input. This cannot happen, here, because
       we stop as soon as the parser reports [HandlingError]. *)
    assert false

let parse_archetype2 ?(name = "") (inc : in_channel) =
  let _ = name in
  let lexbuf = L.from_channel inc in
  loop lexbuf (P.Incremental.main lexbuf.lex_curr_p)
