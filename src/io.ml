(* -------------------------------------------------------------------- *)
open Parser
open Parser.Incremental
open Parser.MenhirInterpreter
open Lexing
open PureLexer
open ParseError

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
  | `FoundExprAt checkpoint ->
    (* let checkpoint = Parser.MenhirInterpreter.offer checkpoint (Parser.INVALID_EXPR, dummy_pos, dummy_pos) in *)
    let lex =
      Lexer.skip_until_before (fun t -> t = SEMI_COLON || t = RBRACE) lex
    in
    let lex =
      if Lexer.get' lex = SEMI_COLON
      then snd (Lexer.next lex)
      else lex
    in
    (lex, checkpoint)
  | `FoundDeclarationAt checkpoint ->
    let lex =
      Lexer.skip_until_before (function EOF | CONSTANT | VARIABLE | ENUM | STATES | ASSET | ACTION | TRANSITION | NAMESPACE | CONTRACT -> true | _ -> false) lex
    in
    (* let checkpoint = Parser.MenhirInterpreter.offer checkpoint (Parser.INVALID_DECL, dummy_pos, dummy_pos) in *)
    (lex, checkpoint)
  | `FoundNothingAt checkpoint ->
    (Lexer.skip_until_before
       (function EOF | CONSTANT | VARIABLE | ENUM | STATES | ASSET | ACTION | TRANSITION | NAMESPACE | CONTRACT -> true | _ -> false)
       lex,
     checkpoint)

let update_last_reduction checkpoint production last_reduction =
  (* Printf.eprintf "update_last_reduction: %s\n" (Symbol.string_of_symbol (lhs production)); *)
  match lhs production with
  | X (N N_expr_r) ->
    `FoundExprAt checkpoint
  | X (N N_simple_expr_r) ->
    `FoundExprAt checkpoint
  | X (N N_declaration_r) ->
    `FoundDeclarationAt checkpoint
  | _ ->
    last_reduction

let parse lexbuf =
  Lexer.initialize lexbuf;

  let rec on_error last_reduction lexer checkpoint =
    contextual_error_msg lexer checkpoint (fun () ->
        resume_on_error last_reduction lexer
      )
  and run last_reduction input_needed lexer checkpoint =
    match checkpoint with
    | InputNeeded _ ->
      let token, lexer = Lexer.next lexer in
      run last_reduction checkpoint lexer (offer checkpoint token)
    | Accepted x ->
      x
    | Rejected
    | HandlingError _ ->
      let lexer, after_error = on_error last_reduction lexer input_needed in
      run last_reduction input_needed lexer after_error
    | Shifting _ ->
      run last_reduction input_needed lexer (resume checkpoint)
    | AboutToReduce (_, production) ->
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
