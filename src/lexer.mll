(* Lexer *)
{
  open Parser

  let lex_error lexbuf msg =
    let loc = Location.of_lexbuf lexbuf in
    raise (Parseutils.ParseError (Some loc, PE_LexicalError msg))
}

(* -------------------------------------------------------------------- *)
let blank   = [' ' '\t' '\r']
let newline = '\n'
let digit   = ['0'-'9']
let ident   = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

(* -------------------------------------------------------------------- *)
rule token = parse
  | newline               { Lexing.new_line lexbuf; token lexbuf }
  | blank+                { token lexbuf }
  | "(*"                  { comment lexbuf; token lexbuf }
  | "use"                 { USE }
  | "model"               { MODEL }
  | "constant"            { CONSTANT }
  | "role"                { ROLE }
  | "identified"          { IDENTIFIED }
  | "by"                  { BY }
  | "ref"                 { REF }
  | "asset"               { ASSET }
  | "enum"                { ENUM }
  | "states"              { STATES }
  | "ensure"              { ENSURE }
  | "transition"          { TRANSITION }
  | "transaction"         { TRANSACTION }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | "[%"                  { BEGIN_EXTENSION }
  | "["                   { LBRACKET }
  | "]"                   { RBRACKET }
  | "{"                   { LBRACE }
  | "}"                   { RBRACE }
  | "="                   { EQUAL }
  | ","                   { COMMA }
  | ":"                   { COLON }
  | ";"                   { SEMI_COLON }
  | "|"                   { PIPE }
  | "."                   { DOT }
  | ":="                  { COLONEQUAL }
  | "+="                  { PLUSEQ }
  | "-="                  { MINUSEQ }
  | ident as s            { IDENT s }
  | digit+ as d           { NUMBER (int_of_string d) } (* FIXME: overflow *)
  | eof                   { EOF }
  | _ as c                {
      lex_error lexbuf (Printf.sprintf "unexpected char: %c" c)
    }

(* -------------------------------------------------------------------- *)
and comment = parse
  | "*)" { () }
  | "(*" { comment lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { lex_error lexbuf "unterminated comment" }
