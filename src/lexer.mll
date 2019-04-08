(* Lexer *)
{
  open Parser

  let lex_error lexbuf msg =
    let loc = Location.of_lexbuf lexbuf in
    raise (ParseUtils.ParseError (Some loc, PE_LexicalError msg))

  let keywords = Hashtbl.create 0

  let keywords_ =
    [ "use"                 , USE            ;
      "model"               , MODEL          ;
      "constant"            , CONSTANT       ;
      "variable"            , VARIABLE       ;
      "identified"          , IDENTIFIED     ;
      "sorted"              , SORTED         ;
      "by"                  , BY             ;
      "as"                  , AS             ;
      "from"                , FROM           ;
      "to"                  , TO             ;
      "on"                  , ON             ;
      "when"                , WHEN           ;
      "ref"                 , REF            ;
      "fun"                 , FUN            ;
      "initialized"         , INITIALIZED    ;
      "collection"          , COLLECTION     ;
      "queue"               , QUEUE          ;
      "stack"               , STACK          ;
      "set"                 , SET            ;
      "partition"           , PARTITION      ;
      "asset"               , ASSET          ;
      "match"               , MATCH          ;
      "with"                , WITH           ;
      "end"                 , END            ;
      "assert"              , ASSERT         ;
      "object"              , OBJECT         ;
      "key"                 , KEY            ;
      "of"                  , OF             ;
      "enum"                , ENUM           ;
      "states"              , STATES         ;
      "initial"             , INITIAL        ;
      "transition"          , TRANSITION     ;
      "action"              , ACTION         ;
      "effect"              , EFFECT         ;
      "called"              , CALLED         ;
      "condition"           , CONDITION      ;
      "verification"        , VERIFICATION   ;
      "predicate"           , PREDICATE      ;
      "definition"          , DEFINITION     ;
      "axiom"               , AXIOM          ;
      "theorem"             , THEOREM        ;
      "invariant"           , INVARIANT      ;
      "specification"       , SPECIFICATION  ;
      "function"            , FUNCTION       ;
      "let"                 , LET            ;
      "otherwise"           , OTHERWISE      ;
      "if"                  , IF             ;
      "then"                , THEN           ;
      "else"                , ELSE           ;
      "for"                 , FOR            ;
      "in"                  , IN             ;
      "break"               , BREAK          ;
      "transfer"            , TRANSFER       ;
      "back"                , BACK           ;
      "extension"           , EXTENSION      ;
      "namespace"           , NAMESPACE      ;
      "contract"            , CONTRACT       ;
      "and"                 , AND            ;
      "or"                  , OR             ;
      "not"                 , NOT            ;
      "forall"              , FORALL         ;
      "exists"              , EXISTS         ;
      "true"                , TRUE           ;
      "false"               , FALSE          ;
    ]

  let () =
    List.iter (fun (k, v) -> Hashtbl.add keywords k v) keywords_

  let compute_irr_fract (n, d) =
    let rec gcd a b =
      if Big_int.eq_big_int b Big_int.zero_big_int
      then a
      else gcd b (Big_int.mod_big_int a b) in
    let g = gcd n d in
    (Big_int.div_big_int n g), (Big_int.div_big_int d g)

}

(* -------------------------------------------------------------------- *)
let blank   = [' ' '\t' '\r']
let newline = '\n'
let digit   = ['0'-'9']
let dec     = digit+ '.' digit+
let div     = digit+ blank+ "div" blank+ digit+
let tz      = digit+ "tz"
let var     = "<%" ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* '>'
let ident   = (['a'-'z' 'A'-'Z'] | var)  (['a'-'z' 'A'-'Z' '0'-'9' '_' ] | var)*
let address = '@'['a'-'z' 'A'-'Z' '0'-'9' '_' ]+
let duration = (digit+ 'Y')? (digit+ 'M')? (digit+ 'W')? (digit+ 'D')? (digit+ 'h')? (digit+ 'm')? (digit+ 's')?
let day      = digit digit digit digit '-' digit digit '-' digit digit
let hour     = digit digit ':' digit digit ( ':' digit digit )?
let timezone = ('+' digit digit ':' digit digit | 'Z')
let date     = day ('T' hour ( timezone )?)?

(* -------------------------------------------------------------------- *)
rule token = parse
  | newline               { Lexing.new_line lexbuf; token lexbuf }
  | blank+                { token lexbuf }

  | "@add"                { AT_ADD }
  | "@remove"             { AT_REMOVE }
  | "@update"             { AT_UPDATE }
  | ident as id           { try  Hashtbl.find keywords id with Not_found -> IDENT id }
  | tz as t               { TZ (Big_int.big_int_of_string (String.sub t 0 ((String.length t) - 2))) }
  | dec as input          {
      let l = Str.split (Str.regexp "\\.") input in
      let n = Big_int.big_int_of_string ((List.nth l 0) ^ (List.nth l 1)) in
      let d = Big_int.big_int_of_string ("1" ^ (String.make (String.length (List.nth l 1)) '0')) in
      let n, d = compute_irr_fract (n, d) in
      RATIONAL (n, d) }
  | div as input          {
      let l = Str.split (Str.regexp "[ \t\r]+div[ \t\r]+") input in
      let n, d = (Big_int.big_int_of_string (List.nth l 0), Big_int.big_int_of_string (List.nth l 1)) in
      let n, d = compute_irr_fract (n, d) in
      RATIONAL (n, d) }
  | digit+ as n           { NUMBER (Big_int.big_int_of_string n) }
  | address as a          { ADDRESS (String.sub a 1 ((String.length a) - 1)) }
  | duration as d         { DURATION (d) }
  | date as d             { DATE (d) }


  | "(*"                  { comment lexbuf; token lexbuf }
  | "\""                  { STRING (Buffer.contents (string (Buffer.create 0) lexbuf)) }
  | "=>"                  { EQUALGREATER }
  | "::"                  { COLONCOLON }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | "[%"                  { LBRACKETPERCENT }
  | "["                   { LBRACKET }
  | "]"                   { RBRACKET }
  | "{"                   { LBRACE }
  | "}"                   { RBRACE }
  | "="                   { EQUAL }
  | ","                   { COMMA }
  | ":"                   { COLON }
  | ";"                   { SEMI_COLON }
  | "%"                   { PERCENT }
  | "|"                   { PIPE }
  | "."                   { DOT }
  | ":="                  { COLONEQUAL }
  | "+="                  { PLUSEQUAL }
  | "-="                  { MINUSEQUAL }
  | "*="                  { MULTEQUAL }
  | "/="                  { DIVEQUAL }
  | "&="                  { ANDEQUAL }
  | "|="                  { OREQUAL }
  | "->"                  { IMPLY }
  | "<->"                 { EQUIV }
  | "<>"                  { NEQUAL }
  | "<"                   { LESS }
  | "<="                  { LESSEQUAL }
  | ">"                   { GREATER }
  | ">="                  { GREATEREQUAL }
  | "+"                   { PLUS }
  | "-"                   { MINUS }
  | "*"                   { MULT }
  | "/"                   { DIV }
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

and string buf = parse
  | "\""          { buf }
  | "\\n"         { Buffer.add_char buf '\n'; string buf lexbuf }
  | "\\r"         { Buffer.add_char buf '\r'; string buf lexbuf }
  | "\\" (_ as c) { Buffer.add_char buf c   ; string buf lexbuf }
  | newline       { Buffer.add_string buf (Lexing.lexeme lexbuf); string buf lexbuf }
  | _ as c        { Buffer.add_char buf c   ; string buf lexbuf }
  | eof           { lex_error lexbuf "unterminated string"  }
