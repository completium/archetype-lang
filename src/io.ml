(* -------------------------------------------------------------------- *)
open Parser
open Parser.Incremental
open Parser.MenhirInterpreter
open Lexing
open PureLexer
open ParseError
open ParseUtils

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

let check_brackets_balance () =
  let string_of_token_bracket = function
    | LPAREN -> "("
    | RPAREN -> ")"
    | LBRACKET  -> "["
    | RBRACKET -> "]"
    | LBRACE  -> "{"
    | RBRACE -> "}"
    | LBRACKETPERCENT  -> "[%"
    | PERCENTRBRACKET -> "%]"
    | _ -> assert false in
  let aux ((op, cp) : token * token) : ParseUtils.perror list =
    let rec aux_internal (st : ptoken Stack.t) ((op, cp) : token * token) pos : ParseUtils.perror list =
      let t  = Lexer.get pos in
      let token, _, _ = t in
      let next () = aux_internal st (op, cp) (snd (Lexer.next pos)) in
      if token = EOF
      then (
        if not (Stack.is_empty st)
        then
          (Stack.fold (fun (accu : ParseUtils.perror list) (x : ptoken) ->
               let token, sl, el = x in
               let loc = Location.make sl el in
               (PE_Unclosed (loc, string_of_token_bracket token, string_of_token_bracket cp)) :: accu
             ) [] st )
        else []
      )
      else (
        if token = op
        then (Stack.push t st; next())
        else (if token = cp
              then (
                try let _ = Stack.pop st in
                  next()
                with
                | Stack.Empty -> (let token, sl, el = t in
                                  let loc = Location.make sl el in
                                  [PE_Not_Expecting (loc, string_of_token_bracket token)])
              ) else next()
             )
      )
    in
    let st = Stack.create() in
    let lexer = Lexer.start in
    let _, lexer = Lexer.next lexer in
    aux_internal st (op, cp) lexer in

  let errors =
    aux (LPAREN   , RPAREN  ) @
    aux (LBRACKET , RBRACKET) @
    aux (LBRACE   , RBRACE  ) @
    aux (LBRACKETPERCENT , PERCENTRBRACKET) in
  if List.length errors > 0
  then raise (ParseUtils.ParseError errors)

(* -------------------------------------------------------------------- *)


let resume_on_error last_reduction lex =
  match last_reduction with
  | `FoundExprAt checkpoint ->
    (* let checkpoint = Parser.MenhirInterpreter.offer checkpoint (Parser.INVALID_EXPR, dummy_pos, dummy_pos) in *)
    let lex =
      Lexer.skip_until_before (fun t -> t = SEMI_COLON || t = RBRACE || t = EOF) lex
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
  check_brackets_balance ();
  let lexer = Lexer.start in
  run (`FoundNothingAt checkpoint) checkpoint lexer checkpoint

let parse_archetype ?(name = "") (inc : in_channel) =
  Error.resume_on_error ();
  let lexbuf = lexbuf_from_channel name inc in
  parse lexbuf
