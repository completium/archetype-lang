(* Lexer *)
{
  open Michelson_parser

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
      (* "ABS"              , ABS                ; *)
      (* "ADD"              , ADD                ; *)
      (* "ADDRESS"          , ADDRESS            ; *)
      (* "AMOUNT"           , AMOUNT             ; *)
      (* "AND"              , AND                ; *)
      (* "ASSERT_EQ"        , ASSERT_EQ          ; *)
      (* "ASSERT_GE"        , ASSERT_GE          ; *)
      (* "ASSERT_GT"        , ASSERT_GT          ; *)
      (* "ASSERT_LE"        , ASSERT_LE          ; *)
      (* "ASSERT_LT"        , ASSERT_LT          ; *)
      (* "ASSERT_NEQ"       , ASSERT_NEQ         ; *)
      (* "BALANCE"          , BALANCE            ; *)
      (* "BLAKE2B"          , BLAKE2B            ; *)
      (* "CAR"              , CAR                ; *)
      (* "CAST"             , CAST               ; *)
      (* "CDR"              , CDR                ; *)
      (* "CHAIN_ID"         , CHAIN_ID           ; *)
      (* "CHECK_SIGNATURE"  , CHECK_SIGNATURE    ; *)
      (* "COMPARE"          , COMPARE            ; *)
      (* "CONCAT"           , CONCAT             ; *)
      (* "CONS"             , CONS               ; *)
      (* "CONTRACT"         , CONTRACT           ; *)
      (* "CREATE_ACCOUNT"   , CREATE_ACCOUNT     ; *)
      (* "CREATE_CONTRACT"  , CREATE_CONTRACT    ; *)
      (* "DIG"              , DIG                ; *)
      (* "DIP"              , DIP                ; *)
      "DROP"             , DROP               ;
      (* "DUG"              , DUG                ; *)
      (* "DUP"              , DUP                ; *)
      (* "EDIV"             , EDIV               ; *)
      (* "EMPTY_BIG_MAP"    , EMPTY_BIG_MAP      ; *)
      (* "EMPTY_MAP"        , EMPTY_MAP          ; *)
      (* "EMPTY_SET"        , EMPTY_SET          ; *)
      (* "EQ"               , EQ                 ; *)
      (* "EXEC"             , EXEC               ; *)
      "FAILWITH"         , FAILWITH           ;
      (* "GE"               , GE                 ; *)
      (* "GET"              , GET                ; *)
      (* "GT"               , GT                 ; *)
      (* "HASH_KEY"         , HASH_KEY           ; *)
      (* "IF_CONS"          , IF_CONS            ; *)
      (* "IF_LEFT"          , IF_LEFT            ; *)
      (* "IF_NONE"          , IF_NONE            ; *)
      (* "IF"               , IF                 ; *)
      (* "IMPLICIT_ACCOUNT" , IMPLICIT_ACCOUNT   ; *)
      (* "INT"              , INT                ; *)
      (* "ISNAT"            , ISNAT              ; *)
      (* "ITER"             , ITER               ; *)
      (* "LAMBDA"           , LAMBDA             ; *)
      (* "LE"               , LE                 ; *)
      (* "LEFT"             , LEFT               ; *)
      (* "LOOP_LEFT"        , LOOP_LEFT          ; *)
      (* "LOOP"             , LOOP               ; *)
      (* "LSL"              , LSL                ; *)
      (* "LSR"              , LSR                ; *)
      (* "LT"               , LT                 ; *)
      (* "MAP"              , MAP                ; *)
      (* "MEM"              , MEM                ; *)
      (* "MUL"              , MUL                ; *)
      (* "NEG"              , NEG                ; *)
      (* "NEQ"              , NEQ                ; *)
      "NIL"              , NIL                ;
      (* "NONE"             , NONE               ; *)
      (* "NOT"              , NOT                ; *)
      (* "NOW"              , NOW                ; *)
      (* "OR"               , OR                 ; *)
      (* "PACK"             , PACK               ; *)
      "PAIR"             , PAIR               ;
      "PUSH"             , PUSH               ;
      (* "RENAME"           , RENAME             ; *)
      (* "RIGHT"            , RIGHT              ; *)
      (* "SELF"             , SELF               ; *)
      (* "SENDER"           , SENDER             ; *)
      (* "SEQ"              , SEQ                ; *)
      (* "SET_DELEGATE"     , SET_DELEGATE       ; *)
      (* "SHA256"           , SHA256             ; *)
      (* "SHA512"           , SHA512             ; *)
      (* "SIZE"             , SIZE               ; *)
      (* "SLICE"            , SLICE              ; *)
      (* "SOME"             , SOME               ; *)
      (* "SOURCE"           , SOURCE             ; *)
      (* "STEPS_TO_QUOTA"   , STEPS_TO_QUOTA     ; *)
      (* "SUB"              , SUB                ; *)
      "SWAP"             , SWAP               ;
      (* "TRANSFER_TOKENS"  , TRANSFER_TOKENS    ; *)
      "UNIT"             , UNIT               ;
      (* "UNPACK"           , UNPACK             ; *)
      "UNPAIR"           , UNPAIR             ;
      (* "UPDATE"           , UPDATE             ; *)
      (* "XOR"              , XOR                ; *)

      "code"             , CODE               ;
      "storage"          , STORAGE            ;
      "parameter"        , PARAMETER          ;

      "address"          , ADDRESS            ;
      "big_map"          , BIG_MAP            ;
      "bool"             , BOOL               ;
      "bytes"            , BYTES              ;
      "chain_id"         , CHAIN_ID           ;
      "contract"         , CONTRACT           ;
      "int"              , INT                ;
      "key"              , KEY                ;
      "key_hash"         , KEY_HASH           ;
      "lambda"           , LAMBDA             ;
      "list"             , LIST               ;
      "map"              , MAP                ;
      "mutez"            , MUTEZ              ;
      "nat"              , NAT                ;
      "operation"        , OPERATION          ;
      "option"           , OPTION             ;
      "or"               , OR                 ;
      "pair"             , TPAIR              ;
      "set"              , SET                ;
      "signature"        , SIGNATURE          ;
      "string"           , STRING             ;
      "timestamp"        , TIMESTAMP          ;
      "unit"             , TUNIT              ;

      ]
    let () =
    List.iter (fun (k, v) -> Hashtbl.add keywords k v) keywords_

}

(* -------------------------------------------------------------------- *)
let blank    = [' ' '\t' '\r']
let newline  = '\n'
let digit    = ['0'-'9']
let ident    = (['a'-'z' 'A'-'Z']) (['a'-'z' 'A'-'Z' '0'-'9' '_' ])*
let annot    = '%' ident

(* -------------------------------------------------------------------- *)
rule token = parse
  | newline               { Lexing.new_line lexbuf; token lexbuf }
  | blank+                { token lexbuf }

  | ident as id           { try  Hashtbl.find keywords id with Not_found -> assert false (*IDENT id*) }
  | (digit+ as n)         { NUMBER (Big_int.big_int_of_string n) }
  | annot as s            { ANNOTATION (String.sub s 1 ((String.length s) - 1))}

  | "#"                   { comment_line lexbuf; token lexbuf }
  | "(*"                  { comment lexbuf; token lexbuf }
  | "/*"                  { comment2 lexbuf; token lexbuf }
  | "\""                  { VSTRING (Buffer.contents (string (Buffer.create 0) lexbuf)) }
  | "("                   { LPAREN }
  | ")"                   { RPAREN }
  | "["                   { LBRACKET }
  | "]"                   { RBRACKET }
  | "{"                   { LBRACE }
  | "}"                   { RBRACE }
  | ";"                   { SEMI_COLON }
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
