(* Lexer *)
{
  open Parser

  exception Error of char
}
let space = [ ' ' '\t' '\n' ]+
let digits = ['0'-'9']+
let ident_pattern = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule token = parse
  | space	          { token lexbuf }
  | "(*"                  { comment lexbuf; token lexbuf }
  | "use"                 { USE }
  | "model"               { MODEL }
  | "constant"            { CONSTANT }
  | "role"                { ROLE }
  | "identified"          { IDENTIFIED }
  | "by"                  { BY }
  | "ref"                 { REF }
  | "asset"               { ASSET }
  | "[%"                  { BEGIN_EXTENTION }
  | "["                   { LBRACKET }
  | "]"                   { RBRACKET }
  | "{"                   { LBRACE }
  | "}"                   { RBRACE }
  | "="                   { EQUAL }
  | ":"                   { COLON }
  | ";"                   { SEMI_COLON }
  | ident_pattern as s    { IDENT (s) }
  | digits as d           { NUMBER (int_of_string d) }
  | eof                   { EOF }
  | _ as c                { raise (Error (c)) }
and comment = parse
  | "*)" { () }
  | "(*" { comment lexbuf; comment lexbuf }
  | _    { comment lexbuf }
  | eof  { failwith "comment error" }
