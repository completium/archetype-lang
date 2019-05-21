(* -------------------------------------------------------------------- *)
open Parser
open Parser.Incremental
open Parser.MenhirInterpreter
open Lexing
open PureLexer
open ParseError


(* -------------------------------------------------------------------- *)
(* let parserfun_entry =
   MenhirLib.Convert.Simplified.traditional2revised P.main *)

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
(* let lexer (lexbuf : L.lexbuf) =
   let token = Lexer.token lexbuf in
   (token, L.lexeme_start_p lexbuf, L.lexeme_end_p lexbuf)

   let parse_archetype ?(name = "") (inc : in_channel) =
   let reader = lexbuf_from_channel name inc in
   parserfun_entry (fun () -> lexer reader) *)




(* let check_balance_paren lexbuf =
   let errors, st = List.fold_left (fun (errors, st) i ->
      match Lexer.token i with
      | P.LBRACE -> (errors, (Stack.push st i))
      | P.RBRACE ->
        begin
          try
            (errors, (Stack.pop i))
          with
          | Stack.Empty -> PE_UnbalancedParenthesis kind,  q lexbuf
        end
      | _ -> (errors, q)) ([], Stack.create() ) lexbuf in
   if (not (Stack.is_empty st))
   then (
    let t = Stack.pop st in
    let loc = Location.mkloc () in
    raise (ParseError (PE_MissingParenthesis , loc))) *)




let resume_on_error last_reduction lex =
  match last_reduction with
  | `FoundCommandAt checkpoint ->
    let lex =
      Lexer.skip_until_before (fun t -> t = SEMI_COLON || t = RBRACE) lex
    in
    let lex =
      if Lexer.get' lex = SEMI_COLON then snd (Lexer.next lex) else lex
    in
    let checkpoint = Parser.MenhirInterpreter.offer checkpoint (Parser.EXPR_ERROR, dummy_pos, dummy_pos) in
    (lex, checkpoint)
  | (`FoundNothingAt checkpoint | `FoundDefinitionAt checkpoint) ->
    (Lexer.skip_until_before
       (function EOF | ACTION -> true | _ -> false)
       lex,
     checkpoint)

(** This function updates the last fully correct state of the parser. *)
let update_last_reduction checkpoint production last_reduction =
  match lhs production with
  | X (N N_expr_r) ->
    `FoundCommandAt checkpoint
  | X (N N_declaration_r) ->
    `FoundDefinitionAt checkpoint
  | _ ->
    last_reduction

let parse lexbuf =
  Lexer.initialize lexbuf;

  let rec on_error last_reduction lexer checkpoint =
    contextual_error_msg lexer checkpoint (fun () ->
        resume_on_error last_reduction lexer
      )

  (** [run] is the loop function of the parser.

      We maintain [last_reduction] as seen earlier but we also
      save [input_needed] which is the last state of the automaton
      that asked for a token. Since we can change the next token
      observe by this state when we skip tokens, it is the right state from
      which a recovering can be triggered.

      [lexer] and [checkpoint] are the (purely functional) state of
      the lexer and the parser respectively.
  *)
  and run last_reduction input_needed lexer checkpoint =
    match checkpoint with
    | InputNeeded _ ->
      let token, lexer = Lexer.next lexer in
      (** Notice that we update [input_needed] here. *)
      run last_reduction checkpoint lexer (offer checkpoint token)
    | Accepted x ->
      (** We will always return a semantic value. *)
      x
    | Rejected
    | HandlingError _ ->
      (** [on_error] is responsible for recovering from the parsing
          error by returning a lexer state and a parser state that can
          work together to complete the analysis if the suffix of the
          input is syntactically correct. *)
      let lexer, after_error = on_error last_reduction lexer input_needed in
      run last_reduction input_needed lexer after_error
    | Shifting _ ->
      (** Nothing special here, we simply resume parsing. *)
      run last_reduction input_needed lexer (resume checkpoint)
    | AboutToReduce (_, production) ->
      (** At this point, we recall that the prefix of the input has been
          successfully recognized as a nonterminal. *)
      run
        (update_last_reduction input_needed production last_reduction)
        input_needed
        lexer
        (resume checkpoint)
  in
  let checkpoint = main lexbuf.lex_curr_p in
  let lexer = Lexer.start in
  run (`FoundNothingAt checkpoint) checkpoint lexer checkpoint

let parse_archetype ?(name = "") (inc : in_channel) =
  Error.resume_on_error ();
  let lexbuf = lexbuf_from_channel name inc in
  parse lexbuf
