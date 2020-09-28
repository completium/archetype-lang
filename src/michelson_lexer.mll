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
      "ABS"                      , ABS                      ;
      "ADD"                      , ADD                      ;
      "ADDRESS"                  , ADDRESS                  ;
      "AMOUNT"                   , AMOUNT                   ;
      "AND"                      , AND                      ;
      "APPLY"                    , APPLY                    ;
      "BALANCE"                  , BALANCE                  ;
      "BLAKE2B"                  , BLAKE2B                  ;
      "CAR"                      , CAR                      ;
      "CAST"                     , CAST                     ;
      "CDR"                      , CDR                      ;
      "CHAIN_ID"                 , CHAIN_ID                 ;
      "CHECK_SIGNATURE"          , CHECK_SIGNATURE          ;
      "COMPARE"                  , COMPARE                  ;
      "CONCAT"                   , CONCAT                   ;
      "CONS"                     , CONS                     ;
      "CONTRACT"                 , CONTRACT                 ;
      "CREATE_ACCOUNT"           , CREATE_ACCOUNT           ;
      "CREATE_CONTRACT"          , CREATE_CONTRACT          ;
      "DIG"                      , DIG                      ;
      "DIP"                      , DIP                      ;
      "DROP"                     , DROP                     ;
      "DUG"                      , DUG                      ;
      "DUP"                      , DUP                      ;
      "EDIV"                     , EDIV                     ;
      "EMPTY_BIG_MAP"            , EMPTY_BIG_MAP            ;
      "EMPTY_MAP"                , EMPTY_MAP                ;
      "EMPTY_SET"                , EMPTY_SET                ;
      "EQ"                       , EQ                       ;
      "EXEC"                     , EXEC                     ;
      "FAILWITH"                 , FAILWITH                 ;
      "GE"                       , GE                       ;
      "GET"                      , GET                      ;
      "GT"                       , GT                       ;
      "HASH_KEY"                 , HASH_KEY                 ;
      "IF_CONS"                  , IF_CONS                  ;
      "IF_LEFT"                  , IF_LEFT                  ;
      "IF_NONE"                  , IF_NONE                  ;
      "IF"                       , IF                       ;
      "IMPLICIT_ACCOUNT"         , IMPLICIT_ACCOUNT         ;
      "INT"                      , INT                      ;
      "ISNAT"                    , ISNAT                    ;
      "ITER"                     , ITER                     ;
      "KECCAK"                   , KECCAK                   ;
      "LAMBDA"                   , LAMBDA                   ;
      "LE"                       , LE                       ;
      "LEFT"                     , LEFT                     ;
      "LEVEL"                    , LEVEL                    ;
      "LOOP_LEFT"                , LOOP_LEFT                ;
      "LOOP"                     , LOOP                     ;
      "LSL"                      , LSL                      ;
      "LSR"                      , LSR                      ;
      "LT"                       , LT                       ;
      "MAP"                      , MAP                      ;
      "MEM"                      , MEM                      ;
      "MUL"                      , MUL                      ;
      "NEG"                      , NEG                      ;
      "NEQ"                      , NEQ                      ;
      "NEVER"                    , NEVER                    ;
      "NIL"                      , NIL                      ;
      "NONE"                     , NONE                     ;
      "NOT"                      , NOT                      ;
      "NOW"                      , NOW                      ;
      "OR"                       , OR                       ;
      "PACK"                     , PACK                     ;
      "PAIR"                     , PAIR                     ;
      "PAIRING_CHECK"            , PAIRING_CHECK            ;
      "PUSH"                     , PUSH                     ;
      "RENAME"                   , RENAME                   ;
      "RIGHT"                    , RIGHT                    ;
      "SAPLING_EMPTY_STATE"      , SAPLING_EMPTY_STATE      ;
      "SAPLING_VERIFY_UPDATE"    , SAPLING_VERIFY_UPDATE    ;
      "SELF_ADDRESS"             , SELF_ADDRESS             ;
      "SELF"                     , SELF                     ;
      "SENDER"                   , SENDER                   ;
      "SET_BAKER_ACTIVE"         , SET_BAKER_ACTIVE         ;
      "SET_BAKER_CONSENSUS_KEY"  , SET_BAKER_CONSENSUS_KEY  ;
      "SET_BAKER_PVSS_KEY"       , SET_BAKER_PVSS_KEY       ;
      "SET_DELEGATE"             , SET_DELEGATE             ;
      "SHA256"                   , SHA256                   ;
      "SHA3"                     , SHA3                     ;
      "SHA512"                   , SHA512                   ;
      "SIZE"                     , SIZE                     ;
      "SLICE"                    , SLICE                    ;
      "SOME"                     , SOME                     ;
      "SOURCE"                   , SOURCE                   ;
      "STEPS_TO_QUOTA"           , STEPS_TO_QUOTA           ;
      "SUB"                      , SUB                      ;
      "SUBMIT_BALLOT"            , SUBMIT_BALLOT            ;
      "SUBMIT_PROPOSALS"         , SUBMIT_PROPOSALS         ;
      "SWAP"                     , SWAP                     ;
      "TOGGLE_BAKER_DELEGATIONS" , TOGGLE_BAKER_DELEGATIONS ;
      "TOTAL_VOTING_POWER"       , TOTAL_VOTING_POWER       ;
      "TRANSFER_TOKENS"          , TRANSFER_TOKENS          ;
      "UNIT"                     , UNIT                     ;
      "UNPACK"                   , UNPACK                   ;
      "UNPAIR"                   , UNPAIR                   ;
      "UPDATE"                   , UPDATE                   ;
      "VOTING_POWER"             , VOTING_POWER             ;
      "XOR"                      , XOR                      ;

      "code"                , CODE                 ;
      "storage"             , STORAGE              ;
      "parameter"           , PARAMETER            ;

      "address"             , TADDRESS             ;
      "baker_hash"          , TBAKER_HASH          ;
      "baker_operation"     , TBAKER_OPERATION     ;
      "big_map"             , TBIG_MAP             ;
      "bls12_381_fr"        , TBLS12_381_FR        ;
      "bls12_381_g1"        , TBLS12_381_G1        ;
      "bls12_381_g2"        , TBLS12_381_G2        ;
      "bool"                , TBOOL                ;
      "bytes"               , TBYTES               ;
      "chain_id"            , TCHAIN_ID            ;
      "contract"            , TCONTRACT            ;
      "int"                 , TINT                 ;
      "key_hash"            , TKEY_HASH            ;
      "key"                 , TKEY                 ;
      "lambda"              , TLAMBDA              ;
      "list"                , TLIST                ;
      "map"                 , TMAP                 ;
      "mutez"               , TMUTEZ               ;
      "nat"                 , TNAT                 ;
      "never"               , TNEVER               ;
      "operation"           , TOPERATION           ;
      "option"              , TOPTION              ;
      "or"                  , TOR                  ;
      "pair"                , TPAIR                ;
      "pvss_key"            , TPVSS_KEY            ;
      "sapling_state"       , TSAPLING_STATE       ;
      "sapling_transaction" , TSAPLING_TRANSACTION ;
      "set"                 , TSET                 ;
      "signature"           , TSIGNATURE           ;
      "string"              , TSTRING              ;
      "timestamp"           , TTIMESTAMP           ;
      "unit"                , TUNIT                ;

      "Unit"                , DUNIT                ;
      "True"                , DTRUE                ;
      "False"               , DFALSE               ;
      "Right"               , DRIGHT               ;
      "Pair"                , DPAIR                ;
      "Left"                , DLEFT                ;
      "Some"                , DSOME                ;
      "None"                , DNONE                ;
      "Elt"                 , DELT                 ;

      ]
    let () =
    List.iter (fun (k, v) -> Hashtbl.add keywords k v) keywords_

}

(* -------------------------------------------------------------------- *)
let blank    = [' ' '\t' '\r']
let newline  = '\n'
let digit    = ['0'-'9']
let ident    = (['a'-'z' 'A'-'Z' '0'-'9' '_' ])+
let annot    = '%' ident
let bytes    = "0x" (['a'-'f' 'A'-'F' '0'-'9' ])+
let number   = ("-")* digit+

(* -------------------------------------------------------------------- *)
rule token = parse
  | newline               { Lexing.new_line lexbuf; token lexbuf }
  | blank+                { token lexbuf }

  | (bytes as v)          { BYTES v }
  | (number as n)         { NUMBER (Big_int.big_int_of_string n) }
  | annot as s            { ANNOTATION (String.sub s 1 ((String.length s) - 1))}
  | ident as id           { try  Hashtbl.find keywords id with Not_found -> assert false }

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
