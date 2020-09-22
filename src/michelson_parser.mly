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

/* %token IABS */
/* %token IADD */
/* %token IADDRESS */
/* %token IAMOUNT */
/* %token IAND */
/* %token IASSERT_EQ */
/* %token IASSERT_GE */
/* %token IASSERT_GT */
/* %token IASSERT_LE */
/* %token IASSERT_LT */
/* %token IASSERT_NEQ */
/* %token IBALANCE */
/* %token IBLAKE2B */
/* %token ICAR */
/* %token ICAST */
/* %token ICDR */
/* %token ICHAIN_ID */
/* %token ICHECK_SIGNATURE */
/* %token ICOMPARE */
/* %token ICONCAT */
/* %token ICONS */
/* %token ICONTRACT */
/* %token ICREATE_ACCOUNT */
/* %token ICREATE_CONTRACT */
/* %token IDIG */
/* %token IDIP */
%token IDROP
/* %token IDUG */
/* %token IDUP */
/* %token IEDIV */
/* %token IEMPTY_BIG_MAP */
/* %token IEMPTY_MAP */
/* %token IEMPTY_SET */
/* %token IEQ */
/* %token IEXEC */
/* %token IFAILWITH */
/* %token IGE */
/* %token IGET */
/* %token IGT */
/* %token IHASH_KEY */
/* %token IIF_CONS */
/* %token IIF_LEFT */
/* %token IIF_NONE */
/* %token IIF */
/* %token IIMPLICIT_ACCOUNT */
/* %token IINT */
/* %token IISNAT */
/* %token IITER */
/* %token ILAMBDA */
/* %token ILE */
/* %token ILEFT */
/* %token ILOOP_LEFT */
/* %token ILOOP */
/* %token ILSL */
/* %token ILSR */
/* %token ILT */
/* %token IMAP */
/* %token IMEM */
/* %token IMUL */
/* %token INEG */
/* %token INEQ */
%token INIL
/* %token INONE */
/* %token INOT */
/* %token INOW */
/* %token IOR */
/* %token IPACK */
%token IPAIR
%token IPUSH
/* %token IRENAME */
/* %token IRIGHT */
/* %token ISELF */
/* %token ISENDER */
/* %token ISEQ */
/* %token ISET_DELEGATE */
/* %token ISHA256 */
/* %token ISHA512 */
/* %token ISIZE */
/* %token ISLICE */
/* %token ISOME */
/* %token ISOURCE */
/* %token ISTEPS_TO_QUOTA */
/* %token ISUB */
%token ISWAP
/* %token ITRANSFER_TOKENS */
/* %token IUNIT */
/* %token IUNPACK */
%token IUNPAIR
/* %token IUPDATE */
/* %token IXOR */

%token CODE
%token STORAGE
%token PARAMETER

%token ADDRESS
/* %token BIG_MAP */
%token BOOL
%token BYTES
%token CHAIN_ID
/* %token CONTRACT */
%token INT
%token KEY
%token KEY_HASH
/* %token LAMBDA */
/* %token LIST */
/* %token MAP */
%token MUTEZ
%token NAT
%token OPERATION
/* %token OPTION */
/* %token OR */
/* %token PAIR */
/* %token SET */
%token SIGNATURE
%token STRING
%token TIMESTAMP
%token UNIT

/* %token <string> IDENT */
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

%type <michelson> main

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

type_:
| t=type_node { mk_type t }

type_node:
  | ADDRESS    { Taddress }
/*  | BIG_MAP    { Tbig_map   of type_ * type_ }*/
  | BOOL       { Tbool }
  | BYTES      { Tbytes }
  | CHAIN_ID   { Tchain_id }
/*  | CONTRACT   { Tcontract  of type_ }*/
  | INT        { Tint }
  | KEY        { Tkey }
  | KEY_HASH   { Tkey_hash }
/*  | LAMBDA     { Tlambda    of type_ * type_ } */
/*  | LIST       { Tlist      of type_ } */
/*  | MAP        { Tmap       of type_ * type_ } */
  | MUTEZ      { Tmutez }
  | NAT        { Tnat }
  | OPERATION  { Toperation }
/*  | OPTION     { Toption    of type_ } */
/*  | OR         { Tor        of type_ * type_ } */
/*  | PAIR       { Tpair      of type_ * type_ } */
/*  | SET        { Tset       of type_ } */
  | SIGNATURE  { Tsignature }
  | STRING     { Tstring }
  | TIMESTAMP  { Ttimestamp }
  | UNIT       { Tunit }

data:
 | n=NUMBER  { Dint n }
 | s=VSTRING { Dstring s }

instruction:
 | LBRACE xs=separated_list(SEMI_COLON, instruction) RBRACE { SEQ xs }
 | IDROP                                                    { DROP 1 }
 | INIL t=type_                                             { NIL t }
 | IPAIR                                                    { PAIR  }
 | IPUSH t=type_ d=data                                     { PUSH (t, d) }
 | ISWAP                                                    { SWAP }
 | IUNPAIR                                                  { UNPAIR }
