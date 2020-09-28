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

%token ABS
%token ADD
%token ADDRESS
%token AMOUNT
%token AND
%token APPLY
%token BALANCE
%token BLAKE2B
%token CAR
%token CAST
%token CDR
%token CHAIN_ID
%token CHECK_SIGNATURE
%token COMPARE
%token CONCAT
%token CONS
%token CONTRACT
%token CREATE_ACCOUNT
%token CREATE_CONTRACT
%token DIG
%token DIP
%token DROP
%token DUG
%token DUP
%token EDIV
%token EMPTY_BIG_MAP
%token EMPTY_MAP
%token EMPTY_SET
%token EQ
%token EXEC
%token FAILWITH
%token GE
%token GET
%token GT
%token HASH_KEY
%token IF
%token IF_CONS
%token IF_LEFT
%token IF_NONE
%token IMPLICIT_ACCOUNT
%token INT
%token ISNAT
%token ITER
%token KECCAK
%token LAMBDA
%token LE
%token LEFT
%token LEVEL
%token LOOP
%token LOOP_LEFT
%token LSL
%token LSR
%token LT
%token MAP
%token MEM
%token MUL
%token NEG
%token NEQ
%token NEVER
%token NIL
%token NONE
%token NOT
%token NOW
%token OR
%token PACK
%token PAIR
%token PAIRING_CHECK
%token PUSH
%token RENAME
%token RIGHT
%token SAPLING_EMPTY_STATE
%token SAPLING_VERIFY_UPDATE
%token SELF
%token SELF_ADDRESS
%token SENDER
%token SET_BAKER_ACTIVE
%token SET_BAKER_CONSENSUS_KEY
%token SET_BAKER_PVSS_KEY
%token SET_DELEGATE
%token SHA256
%token SHA3
%token SHA512
%token SIZE
%token SLICE
%token SOME
%token SOURCE
%token STEPS_TO_QUOTA
%token SUB
%token SUBMIT_BALLOT
%token SUBMIT_PROPOSALS
%token SWAP
%token TOGGLE_BAKER_DELEGATIONS
%token TOTAL_VOTING_POWER
%token TRANSFER_TOKENS
%token UNIT
%token UNPACK
%token UNPAIR
%token UPDATE
%token VOTING_POWER
%token XOR

%token CODE
%token STORAGE
%token PARAMETER

%token TADDRESS
%token TBIG_MAP
%token TBOOL
%token TBYTES
%token TCHAIN_ID
%token TCONTRACT
%token TINT
%token TKEY
%token TKEY_HASH
%token TLAMBDA
%token TLIST
%token TMAP
%token TMUTEZ
%token TNAT
%token TOPERATION
%token TOPTION
%token TOR
%token TPAIR
%token TSET
%token TSIGNATURE
%token TSTRING
%token TTIMESTAMP
%token TUNIT
%token TSAPLING_TRANSACTION
%token TSAPLING_STATE
%token TNEVER
%token TBLS12_381_G1
%token TBLS12_381_G2
%token TBLS12_381_FR
%token TBAKER_HASH
%token TBAKER_OPERATION
%token TPVSS_KEY

%token DUNIT
%token DTRUE
%token DFALSE
%token DPAIR
%token DLEFT
%token DRIGHT
%token DSOME
%token DNONE
%token DELT

%token <string> ANNOTATION
%token <string> VSTRING
%token <string> BYTES
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
 | LBRACE p=parameter s=storage c=code RBRACE EOF { mk_michelson s p c }

storage:
 | STORAGE t=type_ SEMI_COLON { t }

parameter:
 | PARAMETER t=type_ SEMI_COLON { t }

code:
 | CODE i=instruction SEMI_COLON? { i }

annot:
|              { None }
| s=ANNOTATION { Some s }

type_:
 | t=paren(type_)                     { t }
 | TADDRESS a=annot                   { mk_type ?annotation:a (Taddress) }
 | TBAKER_HASH a=annot                { mk_type ?annotation:a (Tbaker_hash) }
 | TBAKER_OPERATION a=annot           { mk_type ?annotation:a (Tbaker_operation) }
 | TBIG_MAP a=annot k=type_ v=type_   { mk_type ?annotation:a (Tbig_map (k, v)) }
 | TBLS12_381_FR a=annot              { mk_type ?annotation:a (Tbls12_381_fr) }
 | TBLS12_381_G1 a=annot              { mk_type ?annotation:a (Tbls12_381_g1) }
 | TBLS12_381_G2 a=annot              { mk_type ?annotation:a (Tbls12_381_g2) }
 | TBOOL a=annot                      { mk_type ?annotation:a (Tbool) }
 | TBYTES a=annot                     { mk_type ?annotation:a (Tbytes) }
 | TCHAIN_ID a=annot                  { mk_type ?annotation:a (Tchain_id) }
 | TCONTRACT a=annot t=type_          { mk_type ?annotation:a (Tcontract t) }
 | TINT a=annot                       { mk_type ?annotation:a (Tint) }
 | TKEY a=annot                       { mk_type ?annotation:a (Tkey) }
 | TKEY_HASH a=annot                  { mk_type ?annotation:a (Tkey_hash) }
 | TLAMBDA a=annot ta=type_ tb=type_  { mk_type ?annotation:a (Tlambda (ta, tb)) }
 | TLIST a=annot t=type_              { mk_type ?annotation:a (Tlist t) }
 | TMAP a=annot k=type_ v=type_       { mk_type ?annotation:a (Tmap (k, v)) }
 | TMUTEZ a=annot                     { mk_type ?annotation:a (Tmutez) }
 | TNAT a=annot                       { mk_type ?annotation:a (Tnat) }
 | TNEVER a=annot                     { mk_type ?annotation:a (Tnever) }
 | TOPERATION a=annot                 { mk_type ?annotation:a (Toperation) }
 | TOPTION a=annot t=type_            { mk_type ?annotation:a (Toption t) }
 | TOR a=annot ta=type_ tb=type_      { mk_type ?annotation:a (Tor (ta, tb)) }
 | TPAIR a=annot ta=type_ tb=type_    { mk_type ?annotation:a (Tpair (ta, tb)) }
 | TPVSS_KEY a=annot                  { mk_type ?annotation:a (Tpvss_key) }
 | TSAPLING_STATE a=annot             { mk_type ?annotation:a (Tsapling_state) }
 | TSAPLING_TRANSACTION a=annot       { mk_type ?annotation:a (Tsapling_transaction) }
 | TSET a=annot t=type_               { mk_type ?annotation:a (Tset t) }
 | TSIGNATURE a=annot                 { mk_type ?annotation:a (Tsignature) }
 | TSTRING a=annot                    { mk_type ?annotation:a (Tstring) }
 | TTIMESTAMP a=annot                 { mk_type ?annotation:a (Ttimestamp) }
 | TUNIT a=annot                      { mk_type ?annotation:a (Tunit) }


data:
 | x=paren(data)       { x }
 | n=NUMBER            { Dint n }
 | s=VSTRING           { Dstring s }
 | v=BYTES             { Dbytes v }
 | DUNIT               { Dunit }
 | DTRUE               { Dtrue }
 | DFALSE              { Dfalse }
 | DPAIR a=data b=data { Dpair (a, b) }
 | DLEFT d=data        { Dleft d }
 | DRIGHT d=data       { Dright d }
 | DSOME d=data        { Dsome d }
 | DNONE               { Dnone }
 | DELT a=data b=data  { Delt (a, b) }
 | LBRACE xs=separated_list(SEMI_COLON, data) RBRACE { Dlist xs }


neo:
 |           { 1 }
 | n=NUMBER  { Big_int.int_of_big_int n }

seq:
| LBRACE xs=separated_list(SEMI_COLON, instruction) RBRACE { xs }

instruction:
 | xs=seq                            { SEQ xs }
 | ABS                               { ABS }
 | ADD                               { ADD }
 | ADDRESS                           { ADDRESS }
 | AMOUNT                            { AMOUNT }
 | AND                               { AND }
 | APPLY                             { APPLY }
 | BALANCE                           { BALANCE }
 | BLAKE2B                           { BLAKE2B }
 | CAR                               { CAR }
 | CAST                              { CAST }
 | CDR                               { CDR }
 | CHAIN_ID                          { CHAIN_ID }
 | CHECK_SIGNATURE                   { CHECK_SIGNATURE }
 | COMPARE                           { COMPARE }
 | CONCAT                            { CONCAT }
 | CONS                              { CONS }
 | CONTRACT a=annot t=type_          { CONTRACT (t, a) }
 | CREATE_ACCOUNT                    { CREATE_ACCOUNT }
 | CREATE_CONTRACT xs=seq            { CREATE_CONTRACT xs }
 | DIG n=neo                         { DIG n }
 | DIP n=neo xs=seq                  { DIP (n, xs) }
 | DROP n=neo                        { DROP n }
 | DUG n=neo                         { DUG n }
 | DUP                               { DUP }
 | EDIV                              { EDIV }
 | EMPTY_BIG_MAP k=type_ v=type_     { EMPTY_BIG_MAP (k, v) }
 | EMPTY_MAP k=type_ v=type_         { EMPTY_MAP (k, v) }
 | EMPTY_SET t=type_                 { EMPTY_SET t }
 | EQ                                { EQ }
 | EXEC                              { EXEC }
 | FAILWITH                          { FAILWITH }
 | GE                                { GE }
 | GET                               { GET }
 | GT                                { GT }
 | HASH_KEY                          { HASH_KEY }
 | IF t=seq e=seq                    { IF (t, e) }
 | IF_CONS t=seq e=seq               { IF_CONS (t, e) }
 | IF_LEFT t=seq e=seq               { IF_LEFT (t, e) }
 | IF_NONE t=seq e=seq               { IF_NONE (t, e) }
 | IMPLICIT_ACCOUNT                  { IMPLICIT_ACCOUNT }
 | INT                               { INT }
 | ISNAT                             { ISNAT }
 | ITER xs=seq                       { ITER xs }
 | KECCAK                            { KECCAK }
 | LAMBDA a=type_ r=type_ xs=seq     { LAMBDA (a, r, xs) }
 | LE                                { LE }
 | LEFT t=type_                      { LEFT t }
 | LEVEL                             { LEVEL }
 | LOOP xs=seq                       { LOOP xs }
 | LOOP_LEFT xs=seq                  { LOOP_LEFT xs }
 | LSL                               { LSL }
 | LSR                               { LSR }
 | LT                                { LT }
 | MAP xs=seq                        { MAP xs }
 | MEM                               { MEM }
 | MUL                               { MUL }
 | NEG                               { NEG }
 | NEQ                               { NEQ }
 | NEVER                             { NEVER }
 | NIL t=type_                       { NIL t }
 | NONE t=type_                      { NONE t }
 | NOT                               { NOT }
 | NOW                               { NOW }
 | OR                                { OR }
 | PACK                              { PACK }
 | PAIR                              { PAIR }
 | PAIRING_CHECK                     { PAIRING_CHECK }
 | PUSH t=type_ d=data               { PUSH (t, d) }
 | RENAME                            { RENAME }
 | RIGHT t=type_                     { RIGHT t }
 | SAPLING_EMPTY_STATE               { SAPLING_EMPTY_STATE }
 | SAPLING_VERIFY_UPDATE             { SAPLING_VERIFY_UPDATE }
 | SELF                              { SELF }
 | SELF_ADDRESS                      { SELF_ADDRESS }
 | SENDER                            { SENDER }
 | SET_BAKER_ACTIVE                  { SET_BAKER_ACTIVE }
 | SET_BAKER_CONSENSUS_KEY           { SET_BAKER_CONSENSUS_KEY }
 | SET_BAKER_PVSS_KEY                { SET_BAKER_PVSS_KEY }
 | SET_DELEGATE                      { SET_DELEGATE }
 | SHA256                            { SHA256 }
 | SHA3                              { SHA3 }
 | SHA512                            { SHA512 }
 | SIZE                              { SIZE }
 | SLICE                             { SLICE }
 | SOME                              { SOME }
 | SOURCE                            { SOURCE }
 | STEPS_TO_QUOTA                    { STEPS_TO_QUOTA }
 | SUB                               { SUB }
 | SUBMIT_BALLOT                     { SUBMIT_BALLOT }
 | SUBMIT_PROPOSALS                  { SUBMIT_PROPOSALS }
 | SWAP                              { SWAP }
 | TOGGLE_BAKER_DELEGATIONS          { TOGGLE_BAKER_DELEGATIONS }
 | TOTAL_VOTING_POWER                { TOTAL_VOTING_POWER }
 | TRANSFER_TOKENS                   { TRANSFER_TOKENS }
 | UNIT                              { UNIT }
 | UNPACK t=type_                    { UNPACK t }
 | UNPAIR                            { UNPAIR }
 | UPDATE                            { UPDATE }
 | VOTING_POWER                      { VOTING_POWER }
 | XOR                               { XOR }
