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
      "address_to_contract" , ADDRESS_TO_CONTRACT ;
      "aggregate"           , AGGREGATE           ;
      "and"                 , AND                 ;
      "any"                 , ANY                 ;
      "archetype"           , ARCHETYPE           ;
      "as"                  , AS                  ;
      "asset"               , ASSET               ;
      "asset_container"     , ASSET_CONTAINER     ;
      "asset_key"           , ASSET_KEY           ;
      "asset_value"         , ASSET_VALUE         ;
      "asset_view"          , ASSET_VIEW          ;
      "begin"               , BEGIN               ;
      "big_map"             , BIG_MAP             ;
      "by"                  , BY                  ;
      "call"                , CALL                ;
      "called"              , CALLED              ;
      "call_view"           , CALL_VIEW           ;
      "const"               , CONST               ;
      "constant"            , CONSTANT            ;
      "contract"            , CONTRACT            ;
      "detach"              , DETACH              ;
      "div"                 , DIV                 ;
      "do"                  , DO                  ;
      "do_fail_if"          , DO_FAIL_IF          ;
      "done"                , DONE                ;
      "do_require"          , DO_REQUIRE          ;
      "effect"              , EFFECT              ;
      "else"                , ELSE                ;
      "emit"                , EMIT                ;
      "end"                 , END                 ;
      "entry"               , ENTRY               ;
      "get_entrypoint"      , GET_ENTRYPOINT      ;
      "enum"                , ENUM                ;
      "event"               , EVENT               ;
      "fail"                , FAIL                ;
      "fail_some"           , FAILSOME            ;
      "false"               , FALSE               ;
      "fold"                , FOLD                ;
      "for"                 , FOR                 ;
      "from"                , FROM                ;
      "function"            , FUNCTION            ;
      "getter"              , GETTER              ;
      "global_constant"     , GLOBAL_CONSTANT     ;
      "identified"          , IDENTIFIED          ;
      "if"                  , IF                  ;
      "import"              , IMPORT              ;
      "in"                  , IN                  ;
      "initial"             , INITIAL             ;
      "initialized"         , INITIALIZED         ;
      "is"                  , IS                  ;
      "iter"                , ITER                ;
      "iterable_big_map"    , ITERABLE_BIG_MAP    ;
      "lambda"              , LAMBDA              ;
      "lambda_michelson"    , LAMBDA_MICHELSON    ;
      "left"                , LEFT                ;
      "let"                 , LET                 ;
      "list"                , LIST                ;
      "map"                 , MAP                 ;
      "match"               , MATCH               ;
      "make_asset"          , MAKE_ASSET          ;
      "make_big_map"        , MAKE_BIG_MAP        ;
      "make_event"          , MAKE_EVENT          ;
      "make_list"           , MAKE_LIST           ;
      "make_map"            , MAKE_MAP            ;
      "make_set"            , MAKE_SET            ;
      "michelson"           , MICHELSON           ;
      "namespace"           , NAMESPACE           ;
      "none"                , NONE                ;
      "not"                 , NOT                 ;
      "offchain"            , OFFCHAIN            ;
      "onchain"             , ONCHAIN             ;
      "option"              , OPTION              ;
      "or"                  , OR                  ;
      "otherwise"           , OTHERWISE           ;
      "partition"           , PARTITION           ;
      "record"              , RECORD              ;
      "require_entrypoint"  , REQUIRE_ENTRYPOINT  ;
      "require"             , REQUIRE             ;
      "return"              , RETURN              ;
      "right"               , RIGHT               ;
      "sapling_state"       , SAPLING_STATE       ;
      "sapling_transaction" , SAPLING_TRANSACTION ;
      "self"                , SELF                ;
      "set"                 , SET                 ;
      "some"                , SOME                ;
      "sorted"              , SORTED              ;
      "sourced"             , SOURCED             ;
      "states"              , STATES              ;
      "then"                , THEN                ;
      "ticket"              , TICKET              ;
      "to"                  , TO                  ;
      "transfer"            , TRANSFER            ;
      "transition"          , TRANSITION          ;
      "true"                , TRUE                ;
      "type"                , TYPE                ;
      "unpack"              , UNPACK              ;
      "Unit"                , UNIT                ;
      "var"                 , VAR                 ;
      "variable"            , VARIABLE            ;
      "view"                , VIEW                ;
      "when"                , WHEN                ;
      "while"               , WHILE               ;
      "with"                , WITH                ;
      "xor"                 , XOR
    ]

  let () =
    List.iter (fun (k, v) -> Hashtbl.add keywords k v) keywords_

}

(* -------------------------------------------------------------------- *)
let blank    = [' ' '\t' '\r']
let newline  = '\n'
let digit    = ['0'-'9']
let dec      = digit+ '.' digit+
let tz       = (digit+ | dec) "tz"
let mtz      = (digit+ | dec) "mtz"
let utz      = (digit+ | dec) "utz"
let pep515_item = '_' digit digit digit
let pep515   = digit? digit? digit pep515_item+
let ptz       = pep515 "tz"
let pmtz      = pep515 "mtz"
let putz      = pep515 "utz"
let var      = "<%" ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' ]* '>'
let ident    = (['a'-'z' 'A'-'Z'] | var)  (['a'-'z' 'A'-'Z' '0'-'9' '_' ] | var)*
let pident   = '%' ['a'-'z' 'A'-'Z' '0'-'9' '_' ]+
let address  = '@'['a'-'z' 'A'-'Z' '0'-'9' '_' ]+
let duration = (digit+ 'w')? (digit+ 'd')? (digit+ 'h')? (digit+ 'm')? (digit+ 's')?
let day      = digit digit digit digit '-' digit digit '-' digit digit
let hour     = digit digit ':' digit digit ( ':' digit digit )?
let timezone = (('+' | '-') digit digit ':' digit digit | 'Z')
let date     = day ('T' hour ( timezone )?)?
let accept_transfer = "accept" blank+ "transfer"
let no_transfer = "no" blank+ "transfer"
let fail_if  = "fail" blank+ "if"
let bytes    = "0x" ['0'-'9' 'a'-'f' 'A'-'F']*
let percent  = (digit+ | dec) "%"
let number   = ('-')? digit+
let tz_addr  = (("tz" ('1' | '2' | '3' | '4')) | "KT1") ['0'-'9' 'a'-'z' 'A'-'Z']+
let tz_expr  = "expr" ['0'-'9' 'a'-'z' 'A'-'Z']+
let annot    = ['@' ':' '$' '&' '%' '!' '?'] (['_' '0'-'9' 'a'-'z' 'A'-'Z' '%' '@'])+

(* -------------------------------------------------------------------- *)
rule token = parse
  | newline               { Lexing.new_line lexbuf; token lexbuf }
  | accept_transfer       { ACCEPT_TRANSFER }
  | no_transfer           { NO_TRANSFER }
  | fail_if               { FAIL_IF }
  | blank+                { token lexbuf }

  | "state" blank+ "is"      { STATE_IS }
  | "with" blank+ "metadata" { WITH_METADATA }

  | tz_expr as e          { if (String.length e <> 54) then lex_error lexbuf (Printf.sprintf "invalid expr: %s" e); TZ_EXPR e }
  | tz_addr as a          { if (String.length a <> 36) then lex_error lexbuf (Printf.sprintf "invalid address: %s" a); ADDRESS a }
  | ident as id           { try  Hashtbl.find keywords id with Not_found -> (if (String.length id > 254) then lex_error lexbuf "Invalid identifier size, must be less than 255 charaters"; IDENT id) }
  | pident as id          { PIDENT (String.sub id 1 ((String.length id) - 1)) }
  | tz as t               { TZ   (String.sub t 0 ((String.length t) - 2)) }
  | mtz as t              { MTZ  (String.sub t 0 ((String.length t) - 3)) }
  | utz as t              { UTZ  (String.sub t 0 ((String.length t) - 3)) }
  | ptz as t              { TZ   (Big_int.string_of_big_int (Big_int.big_int_of_string (String.sub t 0 ((String.length t) - 2)))) }
  | pmtz as t             { MTZ  (Big_int.string_of_big_int (Big_int.big_int_of_string (String.sub t 0 ((String.length t) - 3)))) }
  | putz as t             { UTZ  (Big_int.string_of_big_int (Big_int.big_int_of_string (String.sub t 0 ((String.length t) - 3)))) }
  | dec as input          { DECIMAL (input) }
  | (digit+ as n) 'i'     { NUMBERINT (Big_int.big_int_of_string n) }
  | (digit+ as n)         { NUMBERNAT (Big_int.big_int_of_string n) }
  | (pep515 as input) 'i' { NUMBERINT (Big_int.big_int_of_string input) }
  | pep515 as input       { NUMBERNAT (Big_int.big_int_of_string input) }
  | (bytes as v) "g1"     { BYTES_G1 (String.sub v 2 ((String.length v) - 2)) }
  | (bytes as v) "g2"     { BYTES_G2 (String.sub v 2 ((String.length v) - 2)) }
  | (bytes as v) "fr"     { BYTES_FR (String.sub v 2 ((String.length v) - 2)) }
  | (number as input) "fr" { NUMBER_FR (Big_int.big_int_of_string input) }
  | address as a          { ADDRESS (String.sub a 1 ((String.length a) - 1)) }
  | duration as d         { DURATION (d) }
  | date as d             { DATE (d) }
  | bytes as v            { BYTES (String.sub v 2 ((String.length v) - 2)) }
  | percent as v          { PERCENT_LIT (String.sub v 0 ((String.length v) - 1)) }

  | "//"                  { comment_line lexbuf; token lexbuf }
  | "(*"                  { comment lexbuf; token lexbuf }
  | "/*"                  { comment2 lexbuf; token lexbuf }
  | "`"                   { let s, p = lexbuf.lex_start_pos, lexbuf.lex_start_p in let buf = string_ext (Buffer.create 0) lexbuf in lexbuf.lex_start_pos <- s; lexbuf.lex_start_p <- p; STRING_EXT (Buffer.contents buf) }
  | "\""                  { let s, p = lexbuf.lex_start_pos, lexbuf.lex_start_p in let buf = string     (Buffer.create 0) lexbuf in lexbuf.lex_start_pos <- s; lexbuf.lex_start_p <- p; STRING     (Buffer.contents buf) }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | "["                   { LBRACKET }
  | "]"                   { RBRACKET }
  | "{"                   { LBRACE }
  | "}"                   { RBRACE }
  | "::"                  { COLONCOLON }
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
  | "="                   { EQUAL }
  | "<>"                  { NEQUAL }
  | "<"                   { LESS }
  | "<="                  { LESSEQUAL }
  | ">"                   { GREATER }
  | ">="                  { GREATEREQUAL }
  | "|>>"                 { PIPE_GREATER_GREATER }
  | "<<|"                 { LESS_LESS_PIPE }
  | "<=>"                 { LESS_EQUAL_GREATER }
  | "+"                   { PLUS }
  | "-"                   { MINUS }
  | "*"                   { MULT }
  | "/"                   { SLASH }
  | "/%"                  { SLASHPERCENT }
  | "_"                   { UNDERSCORE }
  | "?"                   { QUESTION }
  | "?."                  { QUESTIONDOT }
  | "?="                  { QUESTIONEQUAL }
  | "?:="                 { QUESTIONCOLONEQUAL }
  | "?is"                 { QUESTIONIS }
  | eof                   { EOF }
  | annot as s            { ANNOTATION (s)}
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
  | newline       { Lexing.new_line lexbuf; Buffer.add_string buf (Lexing.lexeme lexbuf); string buf lexbuf }
  | _ as c        { Buffer.add_char buf c   ; string buf lexbuf }
  | eof           { lex_error lexbuf "unterminated string"  }

and string_ext buf = parse
  | "`"           { buf }
  | newline       { Buffer.add_char buf '\n'; string_ext buf lexbuf }
  | _ as c        { Buffer.add_char buf c   ; string_ext buf lexbuf }
  | eof           { lex_error lexbuf "unterminated string_ext"  }
