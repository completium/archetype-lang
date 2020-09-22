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
      (* "ABS"              , IABS                ; *)
      (* "ADD"              , IADD                ; *)
      (* "ADDRESS"          , IADDRESS            ; *)
      (* "AMOUNT"           , IAMOUNT             ; *)
      (* "AND"              , IAND                ; *)
      (* "ASSERT_EQ"        , IASSERT_EQ          ; *)
      (* "ASSERT_GE"        , IASSERT_GE          ; *)
      (* "ASSERT_GT"        , IASSERT_GT          ; *)
      (* "ASSERT_LE"        , IASSERT_LE          ; *)
      (* "ASSERT_LT"        , IASSERT_LT          ; *)
      (* "ASSERT_NEQ"       , IASSERT_NEQ         ; *)
      (* "BALANCE"          , IBALANCE            ; *)
      (* "BLAKE2B"          , IBLAKE2B            ; *)
      (* "CAR"              , ICAR                ; *)
      (* "CAST"             , ICAST               ; *)
      (* "CDR"              , ICDR                ; *)
      (* "CHAIN_ID"         , ICHAIN_ID           ; *)
      (* "CHECK_SIGNATURE"  , ICHECK_SIGNATURE    ; *)
      (* "COMPARE"          , ICOMPARE            ; *)
      (* "CONCAT"           , ICONCAT             ; *)
      (* "CONS"             , ICONS               ; *)
      (* "CONTRACT"         , ICONTRACT           ; *)
      (* "CREATE_ACCOUNT"   , ICREATE_ACCOUNT     ; *)
      (* "CREATE_CONTRACT"  , ICREATE_CONTRACT    ; *)
      (* "DIG"              , IDIG                ; *)
      (* "DIP"              , IDIP                ; *)
      "DROP"             , IDROP               ;
      (* "DUG"              , IDUG                ; *)
      (* "DUP"              , IDUP                ; *)
      (* "EDIV"             , IEDIV               ; *)
      (* "EMPTY_BIG_MAP"    , IEMPTY_BIG_MAP      ; *)
      (* "EMPTY_MAP"        , IEMPTY_MAP          ; *)
      (* "EMPTY_SET"        , IEMPTY_SET          ; *)
      (* "EQ"               , IEQ                 ; *)
      (* "EXEC"             , IEXEC               ; *)
      (* "FAILWITH"         , IFAILWITH           ; *)
      (* "GE"               , IGE                 ; *)
      (* "GET"              , IGET                ; *)
      (* "GT"               , IGT                 ; *)
      (* "HASH_KEY"         , IHASH_KEY           ; *)
      (* "IF_CONS"          , IIF_CONS            ; *)
      (* "IF_LEFT"          , IIF_LEFT            ; *)
      (* "IF_NONE"          , IIF_NONE            ; *)
      (* "IF"               , IIF                 ; *)
      (* "IMPLICIT_ACCOUNT" , IIMPLICIT_ACCOUNT   ; *)
      (* "INT"              , IINT                ; *)
      (* "ISNAT"            , IISNAT              ; *)
      (* "ITER"             , IITER               ; *)
      (* "LAMBDA"           , ILAMBDA             ; *)
      (* "LE"               , ILE                 ; *)
      (* "LEFT"             , ILEFT               ; *)
      (* "LOOP_LEFT"        , ILOOP_LEFT          ; *)
      (* "LOOP"             , ILOOP               ; *)
      (* "LSL"              , ILSL                ; *)
      (* "LSR"              , ILSR                ; *)
      (* "LT"               , ILT                 ; *)
      (* "MAP"              , IMAP                ; *)
      (* "MEM"              , IMEM                ; *)
      (* "MUL"              , IMUL                ; *)
      (* "NEG"              , INEG                ; *)
      (* "NEQ"              , INEQ                ; *)
      "NIL"              , INIL                ;
      (* "NONE"             , INONE               ; *)
      (* "NOT"              , INOT                ; *)
      (* "NOW"              , INOW                ; *)
      (* "OR"               , IOR                 ; *)
      (* "PACK"             , IPACK               ; *)
      "PAIR"             , IPAIR               ;
      "PUSH"             , IPUSH               ;
      (* "RENAME"           , IRENAME             ; *)
      (* "RIGHT"            , IRIGHT              ; *)
      (* "SELF"             , ISELF               ; *)
      (* "SENDER"           , ISENDER             ; *)
      (* "SEQ"              , ISEQ                ; *)
      (* "SET_DELEGATE"     , ISET_DELEGATE       ; *)
      (* "SHA256"           , ISHA256             ; *)
      (* "SHA512"           , ISHA512             ; *)
      (* "SIZE"             , ISIZE               ; *)
      (* "SLICE"            , ISLICE              ; *)
      (* "SOME"             , ISOME               ; *)
      (* "SOURCE"           , ISOURCE             ; *)
      (* "STEPS_TO_QUOTA"   , ISTEPS_TO_QUOTA     ; *)
      (* "SUB"              , ISUB                ; *)
      "SWAP"             , ISWAP               ;
      (* "TRANSFER_TOKENS"  , ITRANSFER_TOKENS    ; *)
      (* "UNIT"             , IUNIT               ; *)
      (* "UNPACK"           , IUNPACK             ; *)
      "UNPAIR"           , IUNPAIR             ;
      (* "UPDATE"           , IUPDATE             ; *)
      (* "XOR"              , IXOR                ; *)

      "code"             , CODE               ;
      "storage"          , STORAGE            ;
      "parameter"        , PARAMETER          ;

      "address"          , ADDRESS            ;
      (*"big_map"          , BIG_MAP            ;*)
      "bool"             , BOOL               ;
      "bytes"            , BYTES              ;
      "chain_id"         , CHAIN_ID           ;
      (*"contract"         , CONTRACT           ;*)
      "int"              , INT                ;
      "key"              , KEY                ;
      "key_hash"         , KEY_HASH           ;
      (*"lambda"           , LAMBDA             ;*)
      (*"list"             , LIST               ;*)
      (*"map"              , MAP                ;*)
      "mutez"            , MUTEZ              ;
      "nat"              , NAT                ;
      "operation"        , OPERATION          ;
      (*"option"           , OPTION             ;*)
      (*"or"               , OR                 ;*)
      (*"pair"             , PAIR               ;*)
      (*"set"              , SET                ;*)
      "signature"        , SIGNATURE          ;
      "string"           , STRING             ;
      "timestamp"        , TIMESTAMP          ;
      "unit"             , UNIT               ;

      ]
    let () =
    List.iter (fun (k, v) -> Hashtbl.add keywords k v) keywords_

}

(* -------------------------------------------------------------------- *)
let blank    = [' ' '\t' '\r']
let newline  = '\n'
let digit    = ['0'-'9']
let ident    = (['a'-'z' 'A'-'Z']) (['a'-'z' 'A'-'Z' '0'-'9' '_' ])*

(* -------------------------------------------------------------------- *)
rule token = parse
  | newline               { Lexing.new_line lexbuf; token lexbuf }
  | blank+                { token lexbuf }

  | ident as id           { try  Hashtbl.find keywords id with Not_found -> assert false (*IDENT id*) }
  | (digit+ as n)         { NUMBER (Big_int.big_int_of_string n) }

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
