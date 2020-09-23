/* michelson parser */
%{
  open Michelson

  let emit_error loc =
    let str : string = "syntax error" in
    let pos : Position.t list = [Tools.location_to_position loc] in
    Error.error_alert pos str (fun _ -> ())

  let emit_error_msg loc msg =
    let str : string = "syntax error: " ^ msg in
    let pos : Position.t list = [Tools.location_to_position loc] in
    Error.error_alert pos str (fun _ -> ())

%}

/* %token ABS */
/* %token ADD */
/* %token ADDRESS */
/* %token AMOUNT */
/* %token AND */
/* %token ASSERT_EQ */
/* %token ASSERT_GE */
/* %token ASSERT_GT */
/* %token ASSERT_LE */
/* %token ASSERT_LT */
/* %token ASSERT_NEQ */
/* %token BALANCE */
/* %token BLAKE2B */
/* %token CAR */
/* %token CAST */
/* %token CDR */
/* %token CHAIN_ID */
/* %token CHECK_SIGNATURE */
/* %token COMPARE */
/* %token CONCAT */
/* %token CONS */
/* %token CONTRACT */
/* %token CREATE_ACCOUNT */
/* %token CREATE_CONTRACT */
/* %token DIG */
/* %token DIP */
%token DROP
/* %token DUG */
/* %token DUP */
/* %token EDIV */
/* %token EMPTY_BIG_MAP */
/* %token EMPTY_MAP */
/* %token EMPTY_SET */
/* %token EQ */
/* %token EXEC */
%token FAILWITH
/* %token GE */
/* %token GET */
/* %token GT */
/* %token HASH_KEY */
/* %token IF_CONS */
/* %token IF_LEFT */
/* %token IF_NONE */
/* %token IF */
/* %token IMPLICIT_ACCOUNT */
/* %token INT */
/* %token ISNAT */
/* %token ITER */
/* %token LAMBDA */
/* %token LE */
/* %token LEFT */
/* %token LOOP_LEFT */
/* %token LOOP */
/* %token LSL */
/* %token LSR */
/* %token LT */
/* %token MAP */
/* %token MEM */
/* %token MUL */
/* %token NEG */
/* %token NEQ */
%token NIL
/* %token NONE */
/* %token NOT */
/* %token NOW */
/* %token OR */
/* %token PACK */
%token PAIR
%token PUSH
/* %token RENAME */
/* %token RIGHT */
/* %token SELF */
/* %token SENDER */
/* %token SEQ */
/* %token SET_DELEGATE */
/* %token SHA256 */
/* %token SHA512 */
/* %token SIZE */
/* %token SLICE */
/* %token SOME */
/* %token SOURCE */
/* %token STEPS_TO_QUOTA */
/* %token SUB */
%token SWAP
/* %token TRANSFER_TOKENS */
%token UNIT
/* %token UNPACK */
%token UNPAIR
/* %token UPDATE */
/* %token XOR */

%token CODE
%token STORAGE
%token PARAMETER

%token ADDRESS
%token BIG_MAP
%token BOOL
%token BYTES
%token CHAIN_ID
%token CONTRACT
%token INT
%token KEY
%token KEY_HASH
%token LAMBDA
%token LIST
%token MAP
%token MUTEZ
%token NAT
%token OPERATION
%token OPTION
%token OR
%token TPAIR
%token SET
%token SIGNATURE
%token STRING
%token TIMESTAMP
%token TUNIT

/* %token <string> IDENT */
%token <string> ANNOTATION
%token <string> VSTRING
%token <Big_int.big_int> NUMBER

%token LPAREN
%token RPAREN
%token LBRACKET
%token RBRACKET
%token LBRACE
%token RBRACE
%token SEMI_COLON
%token EOF

%type <Michelson.michelson> main

%start main

%%

%inline paren(X):
| LPAREN x=X RPAREN { x }

%inline braced(X):
| LBRACE x=X RBRACE { x }

%inline bracket(X):
| LBRACKET x=X RBRACKET { x }

main:
 | LBRACE s=storage p=parameter c=code RBRACE EOF { mk_michelson s p c }

storage:
 | STORAGE t=type_ SEMI_COLON { t }

parameter:
 | PARAMETER t=type_ SEMI_COLON { t }

code:
 | CODE i=instruction SEMI_COLON { i }

annotation:
|              { None }
| s=ANNOTATION { Some s }

type_:
  | t=paren(type_)          { t }
  | ADDRESS                 { mk_type (Taddress) }
  | BIG_MAP k=type_ v=type_ { mk_type (Tbig_map (k, v)) }
  | BOOL                    { mk_type (Tbool) }
  | BYTES                   { mk_type (Tbytes) }
  | CHAIN_ID                { mk_type (Tchain_id) }
  | CONTRACT t=type_        { mk_type (Tcontract t) }
  | INT a=annotation        { mk_type ?annotation:a (Tint) }
  | KEY                     { mk_type (Tkey) }
  | KEY_HASH                { mk_type (Tkey_hash) }
  | LAMBDA a=type_ b=type_  { mk_type (Tlambda (a, b)) }
  | LIST t=type_            { mk_type (Tlist t) }
  | MAP k=type_ v=type_     { mk_type (Tmap (k, v)) }
  | MUTEZ                   { mk_type (Tmutez) }
  | NAT a=annotation        { mk_type ?annotation:a (Tnat) }
  | OPERATION               { mk_type (Toperation) }
  | OPTION t=type_          { mk_type (Toption t) }
  | OR a=type_ b=type_      { mk_type (Tor (a, b)) }
  | TPAIR a=type_ b=type_   { mk_type (Tpair (a, b)) }
  | SET t=type_             { mk_type (Tset t) }
  | SIGNATURE               { mk_type (Tsignature) }
  | STRING                  { mk_type (Tstring) }
  | TIMESTAMP               { mk_type (Ttimestamp) }
  | TUNIT                   { mk_type (Tunit) }

data:
 | n=NUMBER  { Dint n }
 | s=VSTRING { Dstring s }

instruction:
 | LBRACE xs=separated_list(SEMI_COLON, instruction) RBRACE { SEQ xs }
/* %token ABS */
/* %token ADD */
/* %token ADDRESS */
/* %token AMOUNT */
/* %token AND */
/* %token ASSERT_EQ */
/* %token ASSERT_GE */
/* %token ASSERT_GT */
/* %token ASSERT_LE */
/* %token ASSERT_LT */
/* %token ASSERT_NEQ */
/* %token BALANCE */
/* %token BLAKE2B */
/* %token CAR */
/* %token CAST */
/* %token CDR */
/* %token CHAIN_ID */
/* %token CHECK_SIGNATURE */
/* %token COMPARE */
/* %token CONCAT */
/* %token CONS */
/* %token CONTRACT */
/* %token CREATE_ACCOUNT */
/* %token CREATE_CONTRACT */
/* %token DIG */
/* %token DIP */
| DROP                                                     { DROP 1 }
/* %token DUG */
/* %token DUP */
/* %token EDIV */
/* %token EMPTY_BIG_MAP */
/* %token EMPTY_MAP */
/* %token EMPTY_SET */
/* %token EQ */
/* %token EXEC */
| FAILWITH                                                 { FAILWITH }
/* %token GE */
/* %token GET */
/* %token GT */
/* %token HASH_KEY */
/* %token IF_CONS */
/* %token IF_LEFT */
/* %token IF_NONE */
/* %token IF */
/* %token IMPLICIT_ACCOUNT */
/* %token INT */
/* %token ISNAT */
/* %token ITER */
/* %token LAMBDA */
/* %token LE */
/* %token LEFT */
/* %token LOOP_LEFT */
/* %token LOOP */
/* %token LSL */
/* %token LSR */
/* %token LT */
/* %token MAP */
/* %token MEM */
/* %token MUL */
/* %token NEG */
/* %token NEQ */
| NIL t=type_                                              { NIL t }
/* %token NONE */
/* %token NOT */
/* %token NOW */
/* %token OR */
/* %token PACK */
| PAIR                                                     { PAIR  }
| PUSH t=type_ d=data                                      { PUSH (t, d) }
/* %token RENAME */
/* %token RIGHT */
/* %token SELF */
/* %token SENDER */
/* %token SEQ */
/* %token SET_DELEGATE */
/* %token SHA256 */
/* %token SHA512 */
/* %token SIZE */
/* %token SLICE */
/* %token SOME */
/* %token SOURCE */
/* %token STEPS_TO_QUOTA */
/* %token SUB */
| SWAP                                                     { SWAP }
/* %token TRANSFER_TOKENS */
| UNIT                                                     { UNIT }
/* %token UNPACK */
| UNPAIR                                                   { UNPAIR }
/* %token UPDATE */
/* %token XOR */







