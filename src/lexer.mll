(* Lexer *)
{
  open Parser

  let lex_error lexbuf msg =
    let loc = Location.of_lexbuf lexbuf in
    raise (ParseUtils.ParseError ([PE_LexicalError (loc, msg)]))

  let keywords = Hashtbl.create 0

  let keywords_ =
    [
      "action"              , ACTION         ;
      "and"                 , AND            ;
      "archetype"           , ARCHETYPE      ;
      "assert"              , ASSERT         ;
      "asset"               , ASSET          ;
      "at"                  , AT             ;
      "axiom"               , AXIOM          ;
      "back"                , BACK           ;
      "break"               , BREAK          ;
      "by"                  , BY             ;
      "called"              , CALLED         ;
      "collection"          , COLLECTION     ;
      "constant"            , CONSTANT       ;
      "contract"            , CONTRACT       ;
      "definition"          , DEFINITION     ;
      "effect"              , EFFECT         ;
      "else"                , ELSE           ;
      "end"                 , END            ;
      "enum"                , ENUM           ;
      "exists"              , EXISTS         ;
      "extension"           , EXTENSION      ;
      "failif"              , FAILIF         ;
      "false"               , FALSE          ;
      "forall"              , FORALL         ;
      "for"                 , FOR            ;
      "from"                , FROM           ;
      "function"            , FUNCTION       ;
      "identified"          , IDENTIFIED     ;
      "if"                  , IF             ;
      "in"                  , IN             ;
      "initial"             , INITIAL        ;
      "initialized"         , INITIALIZED    ;
      "invariants"          , INVARIANTS     ;
      "label"               , LABEL          ;
      "let"                 , LET            ;
      "match"               , MATCH          ;
      "namespace"           , NAMESPACE      ;
      "not"                 , NOT            ;
      "on"                  , ON             ;
      "or"                  , OR             ;
      "otherwise"           , OTHERWISE      ;
      "partition"           , PARTITION      ;
      "predicate"           , PREDICATE      ;
      "queue"               , QUEUE          ;
      "record"              , RECORD         ;
      "ref"                 , REF            ;
      "require"             , REQUIRE        ;
      "return"              , RETURN         ;
      "set"                 , SET            ;
      "sorted"              , SORTED         ;
      "specification"       , SPECIFICATION  ;
      "stack"               , STACK          ;
      "states"              , STATES         ;
      "then"                , THEN           ;
      "theorem"             , THEOREM        ;
      "to"                  , TO             ;
      "transfer"            , TRANSFER       ;
      "transition"          , TRANSITION     ;
      "true"                , TRUE           ;
      "variable"            , VARIABLE       ;
      "verification"        , VERIFICATION   ;
      "when"                , WHEN           ;
      "with"                , WITH           ;
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
let accept_transfer = "accept" blank+ "transfer"
let op_spec1 = "may" blank+ "be" blank+ "performed" blank+ "only" blank+ "by" blank+ "role"
let op_spec2 = "may" blank+ "be" blank+ "performed" blank+ "only" blank+ "by" blank+ "action"
let op_spec3 = "may" blank+ "be" blank+ "performed" blank+ "by" blank+ "role"
let op_spec4 = "may" blank+ "be" blank+ "performed" blank+ "by" blank+ "action"

(* -------------------------------------------------------------------- *)
rule token = parse
  | newline               { Lexing.new_line lexbuf; token lexbuf }
  | accept_transfer       { ACCEPT_TRANSFER }
  | op_spec1              { OP_SPEC1 }
  | op_spec2              { OP_SPEC2 }
  | op_spec3              { OP_SPEC3 }
  | op_spec4              { OP_SPEC4 }
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
  | "::"                  { COLONCOLON }
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
