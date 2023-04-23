(* -------------------------------------------------------------------- *)
open Parser
open Parser.Incremental
open Parser.MenhirInterpreter
open Lexing
open PureLexer
open ParseError
open Core

exception ParenError

type error_desc =
  | LexicalError of string
  | Unclosed of string * string
  | NotExpecting of string
[@@deriving show {with_path = false}]

type error = Location.t * error_desc

let emit_error lc error_desc =
  let str : string = Format.asprintf "%a@." pp_error_desc error_desc in
  let pos : Position.t list = [Tools.location_to_position lc] in
  Error.error_alert pos str (fun _ -> ())

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

let lexbuf_from_string = fun name str ->
  let lexbuf = Lexing.from_string str in
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
    | _ -> assert false in
  let aux ((op, cp) : token * token) : unit =
    let rec aux_internal (st : ptoken Stack.t) ((op, cp) : token * token) pos : unit =
      let t  = Lexer.get pos in
      let token, _, _ = t in
      let next () = aux_internal st (op, cp) (snd (Lexer.next pos)) in
      if token = EOF
      then (
        if not (Stack.is_empty st)
        then
          (Stack.iter (fun (x : ptoken) ->
               let token, sl, el = x in
               let loc = Location.make sl el in
               emit_error loc (Unclosed (string_of_token_bracket token, string_of_token_bracket cp))
             ) st )
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
                                  emit_error loc (NotExpecting (string_of_token_bracket token)))
              ) else next()
             )
      )
    in
    let st = Stack.create() in
    let lexer = Lexer.start in
    let _, lexer = Lexer.next lexer in
    aux_internal st (op, cp) lexer in

  aux (LPAREN   , RPAREN  );
  aux (LBRACKET , RBRACKET);
  aux (LBRACE   , RBRACE  )

(* -------------------------------------------------------------------- *)
let resume_on_error last_reduction lex =
  let invalids_tokens = [Parser.INVALID_EFFECT; Parser.INVALID_EXPR; Parser.INVALID_DECL] in
  match last_reduction with
  | `FoundExprAt checkpoint ->
    let _, checkpoint =
      List.fold_left (fun (cont, check) x ->
          if cont && Parser.MenhirInterpreter.acceptable checkpoint x dummy_pos
          then false, Parser.MenhirInterpreter.offer checkpoint (x, dummy_pos, dummy_pos)
          else (cont, check)
        ) (true, checkpoint) invalids_tokens
    in
    let lex = Lexer.skip_until_before (function SEMI_COLON | RBRACE -> true | _ -> false) lex in
    let lex =
      if Lexer.get' lex = SEMI_COLON
      then snd (Lexer.next lex)
      else lex in
    (lex, checkpoint)

  | `FoundEffect checkpoint ->
    let _, checkpoint =
      List.fold_left (fun (cont, check) x ->
          if cont && Parser.MenhirInterpreter.acceptable checkpoint x dummy_pos
          then false, Parser.MenhirInterpreter.offer checkpoint (x, dummy_pos, dummy_pos)
          else (cont, check)
        ) (true, checkpoint) invalids_tokens
    in
    let lex = Lexer.skip_until_before (function RBRACE -> true | _ -> false) lex in
    (lex, checkpoint)

  | `FoundDeclarationAt checkpoint ->
    let lex =
      Lexer.skip_until_before (function EOF | CONSTANT | VARIABLE | ENUM | STATES | ASSET | ENTRY | TRANSITION | NAMESPACE | CONTRACT -> true | _ -> false) lex
    in
    (lex, checkpoint)

  | `FoundNothingAt checkpoint ->
    let lex =
      Lexer.skip_until_before (function EOF | CONSTANT | VARIABLE | ENUM | STATES | ASSET | ENTRY | TRANSITION | NAMESPACE | CONTRACT | RBRACE -> true | _ -> false) lex
    in
    let _, checkpoint =
      List.fold_left (fun (cont, check) x ->
          if cont && Parser.MenhirInterpreter.acceptable checkpoint x dummy_pos
          then false, Parser.MenhirInterpreter.offer checkpoint (x, dummy_pos, dummy_pos)
          else (cont, check)
        ) (true, checkpoint) invalids_tokens
    in
    (lex, checkpoint)

let update_last_reduction checkpoint production last_reduction =
  (* Printf.eprintf "update_last_reduction: %s\n" (Symbol.string_of_symbol (lhs production)); *)
  match lhs production with
  | X (N N_expr_r) ->
    `FoundExprAt checkpoint
  | X (N N_simple_expr_r) ->
    `FoundExprAt checkpoint
  | X (N N_effect) ->
    `FoundEffect checkpoint
  | X (N N_declaration_r) ->
    `FoundDeclarationAt checkpoint
  | _ ->
    last_reduction

let parse r lexbuf =
  Lexer.initialize lexbuf;

  let rec on_error _last_last_reduction lexer checkpoint =
    contextual_error_msg lexer checkpoint (fun () ->
        raise (Error.ParseError [])
        (* resume_on_error last_reduction lexer *)
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
  let checkpoint = r lexbuf.lex_curr_p in
  check_brackets_balance ();
  match !Error.errors with
  | [] ->
    begin
      let lexer = Lexer.start in
      run (`FoundNothingAt checkpoint) checkpoint lexer checkpoint
    end
  | _ -> raise (Error.ParseError !Error.errors)

let parse_archetype input =
  Error.resume_on_error ();
  let lexbuf =
    match input with
    | FIChannel (path, inc) -> lexbuf_from_channel path inc
    | FIString (path, input) -> lexbuf_from_string path input
  in
  parse main lexbuf

let parse_archetype_strict input =
  let pt = parse_archetype input in
  match !Error.errors with
  | [] -> pt
  | _ -> raise (Error.ParseError !Error.errors)

let parse_expr input =
  Error.resume_on_error ();
    let lexbuf =
    match input with
    | FIChannel (path, inc) -> lexbuf_from_channel path inc
    | FIString (path, input) -> lexbuf_from_string path input
  in
  let e = parse start_expr lexbuf in
  match !Error.errors with
  | [] -> e
  | _ -> raise (Error.ParseError !Error.errors)
