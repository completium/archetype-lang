(* Lexer *)
{
  open Parser

  exception LexError

  let emit_error loc msg =
    let str : string = "lexical error: " ^ msg in
    let pos : Position.t list = [Tools.location_to_position loc] in
    Error.error_alert pos str (fun _ -> ())

  let lex_error lexbuf msg =
    let loc = Location.of_lexbuf lexbuf in
    emit_error loc msg;
    raise LexError

  let keywords = Hashtbl.create 0

  let keywords_ =
    [
      "action"              , ACTION         ;
      "and"                 , AND            ;
      "archetype"           , ARCHETYPE      ;
      "assert"              , ASSERT         ;
      "asset"               , ASSET          ;
      "at"                  , AT             ;
      "before"              , BEFORE         ;
      "break"               , BREAK          ;
      "but"                 , BUT            ;
      "by"                  , BY             ;
      "call"                , CALL           ;
      "called"              , CALLED         ;
      "collection"          , COLLECTION     ;
      "constant"            , CONSTANT       ;
      "contract"            , CONTRACT       ;
      "definition"          , DEFINITION     ;
      "div"                 , DIV            ;
      "do"                  , DO             ;
      "done"                , DONE           ;
      "effect"              , EFFECT         ;
      "else"                , ELSE           ;
      "end"                 , END            ;
      "enum"                , ENUM           ;
      "exists"              , EXISTS         ;
      "extension"           , EXTENSION      ;
      "failif"              , FAILIF         ;
      "false"               , FALSE          ;
      "for"                 , FOR            ;
      "forall"              , FORALL         ;
      "from"                , FROM           ;
      "function"            , FUNCTION       ;
      "identified"          , IDENTIFIED     ;
      "if"                  , IF             ;
      "in"                  , IN             ;
      "initial"             , INITIAL        ;
      "initialized"         , INITIALIZED    ;
      "invariant"           , INVARIANT      ;
      "iter"                , ITER           ;
      "pkey"                , PKEY           ;
      "label"               , LABEL          ;
      "let"                 , LET            ;
      "list"                , LIST           ;
      "match"               , MATCH          ;
      "namespace"           , NAMESPACE      ;
      "none"                , NONE           ;
      "not"                 , NOT            ;
      "of"                  , OF             ;
      "on"                  , ON             ;
      "option"              , OPTION         ;
      "or"                  , OR             ;
      "otherwise"           , OTHERWISE      ;
      "partition"           , PARTITION      ;
      "predicate"           , PREDICATE      ;
      "postcondition"       , POSTCONDITION  ;
      "record"              , RECORD         ;
      "ref"                 , REF            ;
      "require"             , REQUIRE        ;
      "return"              , RETURN         ;
      "security"            , SECURITY       ;
      "shadow"              , SHADOW         ;
      "sorted"              , SORTED         ;
      "some"                , SOME           ;
      "specification"       , SPECIFICATION  ;
      "states"              , STATES         ;
      "then"                , THEN           ;
      "to"                  , TO             ;
      "transfer"            , TRANSFER       ;
      "transition"          , TRANSITION     ;
      "true"                , TRUE           ;
      "use"                 , USE            ;
      "var"                 , VAR            ;
      "variable"            , VARIABLE       ;
      "when"                , WHEN           ;
      "with"                , WITH           ;
    ]

  let () =
    List.iter (fun (k, v) -> Hashtbl.add keywords k v) keywords_

}

(* -------------------------------------------------------------------- *)
let blank    = [' ' '\t' '\r']
let newline  = '\n'
let digit    = ['0'-'9']
let dec      = digit+ '.' digit+
let tz       = digit+ "tz"
let mtz      = digit+ "mtz"
let utz      = digit+ "utz"
let var      = "<%" ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* '>'
let ident    = (['a'-'z' 'A'-'Z'] | var)  (['a'-'z' 'A'-'Z' '0'-'9' '_' ] | var)*
let address  = '@'['a'-'z' 'A'-'Z' '0'-'9' '_' ]+
let duration = (digit+ 'w')? (digit+ 'd')? (digit+ 'h')? (digit+ 'm')? (digit+ 's')?
let day      = digit digit digit digit '-' digit digit '-' digit digit
let hour     = digit digit ':' digit digit ( ':' digit digit )?
let timezone = (('+' | '-') digit digit ':' digit digit | 'Z')
let date     = day ('T' hour ( timezone )?)?
let accept_transfer = "accept" blank+ "transfer"
let refuse_transfer = "refuse" blank+ "transfer"
let bytes    = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+

(* -------------------------------------------------------------------- *)
rule token = parse
  | newline               { Lexing.new_line lexbuf; token lexbuf }
  | accept_transfer       { ACCEPT_TRANSFER }
  | refuse_transfer       { REFUSE_TRANSFER }
  | blank+                { token lexbuf }

  | "@add"                { AT_ADD }
  | "@remove"             { AT_REMOVE }
  | "@update"             { AT_UPDATE }
  | ident as id           { try  Hashtbl.find keywords id with Not_found -> IDENT id }
  | tz as t               { TZ   (Big_int.big_int_of_string (String.sub t 0 ((String.length t) - 2))) }
  | mtz as t              { MTZ  (Big_int.big_int_of_string (String.sub t 0 ((String.length t) - 3))) }
  | utz as t              { UTZ  (Big_int.big_int_of_string (String.sub t 0 ((String.length t) - 3))) }
  | dec as input          { DECIMAL (input) }
  | digit+ as n           { NUMBER (Big_int.big_int_of_string n) }
  | address as a          { ADDRESS (String.sub a 1 ((String.length a) - 1)) }
  | duration as d         { DURATION (d) }
  | date as d             { DATE (d) }
  | bytes as v            { BYTES (String.sub v 2 ((String.length v) - 2)) }


  | "//"                  { comment_line lexbuf; token lexbuf }
  | "(*"                  { comment lexbuf; token lexbuf }
  | "/*"                  { comment2 lexbuf; token lexbuf }
  | "\""                  { STRING (Buffer.contents (string (Buffer.create 0) lexbuf)) }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | "[%"                  { LBRACKETPERCENT }
  | "%]"                  { PERCENTRBRACKET }
  | "["                   { LBRACKET }
  | "]"                   { RBRACKET }
  | "{"                   { LBRACE }
  | "}"                   { RBRACE }
  | ":="                  { COLONEQUAL }
  | ","                   { COMMA }
  | ":"                   { COLON }
  | ";"                   { SEMI_COLON }
  | "%"                   { PERCENT }
  | "|"                   { PIPE }
  | "."                   { DOT }
  | "+="                  { PLUSEQUAL }
  | "-="                  { MINUSEQUAL }
  | "*="                  { MULTEQUAL }
  | "/="                  { DIVEQUAL }
  | "&="                  { ANDEQUAL }
  | "|="                  { OREQUAL }
  | "->"                  { IMPLY }
  | "<->"                 { EQUIV }
  | "="                   { EQUAL }
  | "<>"                  { NEQUAL }
  | "<"                   { LESS }
  | "<="                  { LESSEQUAL }
  | ">"                   { GREATER }
  | ">="                  { GREATEREQUAL }
  | "+"                   { PLUS }
  | "-"                   { MINUS }
  | "*"                   { MULT }
  | "/"                   { SLASH }
  | "_"                   { UNDERSCORE }
  | eof                   { EOF }
  | _ as c                {
      lex_error lexbuf (Printf.sprintf "unexpected char: %c" c)
    }

(* -------------------------------------------------------------------- *)
and comment = parse
  | "*)"    { () }
  | "(*"    { comment lexbuf; comment lexbuf }
  | newline { Lexing.new_line lexbuf; comment lexbuf }
  | _       { comment lexbuf }
  | eof     { lex_error lexbuf "unterminated comment" }

and comment2 = parse
  | "*/"    { () }
  | "/*"    { comment2 lexbuf; comment2 lexbuf }
  | newline { Lexing.new_line lexbuf; comment2 lexbuf }
  | _       { comment2 lexbuf }
  | eof     { lex_error lexbuf "unterminated comment" }

and comment_line = parse
  | newline { Lexing.new_line lexbuf }
  | _       { comment_line lexbuf }
  | eof     { () }

and string buf = parse
  | "\""          { buf }
  | "\\n"         { Buffer.add_char buf '\n'; string buf lexbuf }
  | "\\r"         { Buffer.add_char buf '\r'; string buf lexbuf }
  | "\\" (_ as c) { Buffer.add_char buf c   ; string buf lexbuf }
  | newline       { Buffer.add_string buf (Lexing.lexeme lexbuf); string buf lexbuf }
  | _ as c        { Buffer.add_char buf c   ; string buf lexbuf }
  | eof           { lex_error lexbuf "unterminated string"  }
