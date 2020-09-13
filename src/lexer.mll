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
      "added"               , ADDED          ;
      "aggregate"           , AGGREGATE      ;
      "and"                 , AND            ;
      "any"                 , ANY            ;
      "archetype"           , ARCHETYPE      ;
      "assert"              , ASSERT         ;
      "asset"               , ASSET          ;
      "at"                  , AT             ;
      "before"              , BEFORE         ;
      "but"                 , BUT            ;
      "by"                  , BY             ;
      "call"                , CALL           ;
      "called"              , CALLED         ;
      "constant"            , CONSTANT       ;
      "contract"            , CONTRACT       ;
      "definition"          , DEFINITION     ;
      "div"                 , DIV            ;
      "do"                  , DO             ;
      "dofailif"            , DOFAILIF       ;
      "done"                , DONE           ;
      "dorequire"           , DOREQUIRE      ;
      "effect"              , EFFECT         ;
      "else"                , ELSE           ;
      "end"                 , END            ;
      "entry"               , ENTRY          ;
      "entrypoint"          , ENTRYPOINT     ;
      "enum"                , ENUM           ;
      "exists"              , EXISTS         ;
      "extension"           , EXTENSION      ;
      "fail"                , FAIL           ;
      "failif"              , FAILIF         ;
      "fails"               , FAILS          ;
      "false"               , FALSE          ;
      "for"                 , FOR            ;
      "forall"              , FORALL         ;
      "from"                , FROM           ;
      "function"            , FUNCTION       ;
      "getter"              , GETTER         ;
      "identified"          , IDENTIFIED     ;
      "if"                  , IF             ;
      "in"                  , IN             ;
      "initial"             , INITIAL        ;
      "initialized"         , INITIALIZED    ;
      "invariant"           , INVARIANT      ;
      "iter"                , ITER           ;
      "label"               , LABEL          ;
      "let"                 , LET            ;
      "list"                , LIST           ;
      "map"                 , MAP            ;
      "match"               , MATCH          ;
      "namespace"           , NAMESPACE      ;
      "none"                , NONE           ;
      "not"                 , NOT            ;
      "on"                  , ON             ;
      "option"              , OPTION         ;
      "or"                  , OR             ;
      "otherwise"           , OTHERWISE      ;
      "partition"           , PARTITION      ;
      "pkey"                , PKEY           ;
      "postcondition"       , POSTCONDITION  ;
      "predicate"           , PREDICATE      ;
      "record"              , RECORD         ;
      "ref"                 , REF            ;
      "removed"             , REMOVED        ;
      "require"             , REQUIRE        ;
      "return"              , RETURN         ;
      "security"            , SECURITY       ;
      "set"                 , SET            ;
      "self"                , SELF           ;
      "shadow"              , SHADOW         ;
      "some"                , SOME           ;
      "sorted"              , SORTED         ;
      "specification"       , SPECIFICATION  ;
      "states"              , STATES         ;
      "then"                , THEN           ;
      "to"                  , TO             ;
      "transfer"            , TRANSFER       ;
      "transition"          , TRANSITION     ;
      "true"                , TRUE           ;
      "unmoved"             , UNMOVED        ;
      "unpack"              , UNPACK         ;
      "use"                 , USE            ;
      "var"                 , VAR            ;
      "view"                , VIEW           ;
      "variable"            , VARIABLE       ;
      "when"                , WHEN           ;
      "while"               , WHILE          ;
      "with"                , WITH           ;
      "xor"                 , XOR           ;
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
let pep515_item = '_' digit digit digit
let pep515   = digit? digit? digit pep515_item+
let var      = "<%" ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* '>'
let ident    = (['a'-'z' 'A'-'Z'] | var)  (['a'-'z' 'A'-'Z' '0'-'9' '_' ] | var)*
let pident   = '%' ident
let address  = '@'['a'-'z' 'A'-'Z' '0'-'9' '_' ]+
let duration = (digit+ 'w')? (digit+ 'd')? (digit+ 'h')? (digit+ 'm')? (digit+ 's')?
let day      = digit digit digit digit '-' digit digit '-' digit digit
let hour     = digit digit ':' digit digit ( ':' digit digit )?
let timezone = (('+' | '-') digit digit ':' digit digit | 'Z')
let date     = day ('T' hour ( timezone )?)?
let accept_transfer = "accept" blank+ "transfer"
let refuse_transfer = "refuse" blank+ "transfer"
let bytes    = "0x" ['0'-'9' 'a'-'f' 'A'-'F']+
let percent  = digit+ "%"

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
  | pident as id          { IDENT (String.sub id 1 ((String.length id) - 1)) }
  | tz as t               { TZ   (Big_int.big_int_of_string (String.sub t 0 ((String.length t) - 2))) }
  | mtz as t              { MTZ  (Big_int.big_int_of_string (String.sub t 0 ((String.length t) - 3))) }
  | utz as t              { UTZ  (Big_int.big_int_of_string (String.sub t 0 ((String.length t) - 3))) }
  | dec as input          { DECIMAL (input) }
  | (digit+ as n) 'i'     { NUMBERINT (Big_int.big_int_of_string n) }
  | (digit+ as n)         { NUMBERNAT (Big_int.big_int_of_string n) }
  | (pep515 as input) 'i' { NUMBERINT (Big_int.big_int_of_string input) }
  | pep515 as input       { NUMBERNAT (Big_int.big_int_of_string input) }
  | address as a          { ADDRESS (String.sub a 1 ((String.length a) - 1)) }
  | duration as d         { DURATION (d) }
  | date as d             { DATE (d) }
  | bytes as v            { BYTES (String.sub v 2 ((String.length v) - 2)) }
  | percent as v          { PERCENT_LIT (Big_int.big_int_of_string (String.sub v 0 ((String.length v) - 1))) }


  | "//"                  { comment_line lexbuf; token lexbuf }
  | "(*"                  { comment lexbuf; token lexbuf }
  | "/*"                  { comment2 lexbuf; token lexbuf }
  | "\""                  { STRING (Buffer.contents (string (Buffer.create 0) lexbuf)) }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | "[%%"                 { LBRACKETPERCENT }
  | "%%]"                 { PERCENTRBRACKET }
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
  | "&="                  { AMPEQUAL }
  | "|="                  { PIPEEQUAL }
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
