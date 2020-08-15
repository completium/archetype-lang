type type_ =
  | Tkey
  | Tunit
  | Tsignature
  | Toption    of type_
  | Tlist      of type_
  | Tset       of type_
  | Toperation
  | Tcontract  of type_
  | Tpair      of type_ * type_
  | Tor        of type_ * type_
  | Tlambda    of type_ * type_
  | Tmap       of type_ * type_
  | Tbig_map   of type_ * type_
  | Tchain_id
  | Tint
  | Tnat
  | Tstring
  | Tbytes
  | Tmutez
  | Tbool
  | Tkey_hash
  | Ttimestamp
  | Taddress

type instruction =
  | SEQ                of instruction list
  | DROP               of int
  | DUP
  | SWAP
  | DIG                of int
  | DUG                of int
  | PUSH               of type_* data
  | SOME
  | NONE               of type_
  | UNIT
  | IF_NONE            of instruction list * instruction list
  | PAIR
  | CAR
  | CDR
  | LEFT               of type_
  | RIGHT              of type_
  | IF_LEFT            of instruction list * instruction list
  | NIL                of type_
  | CONS
  | IF_CONS            of instruction list * instruction list
  | SIZE
  | EMPTY_SET          of type_
  | EMPTY_MAP          of type_
  | EMPTY_BIG_MAP      of type_
  | MAP                of instruction list
  | ITER               of instruction list
  | MEM
  | GET
  | UPDATE
  | IF                 of instruction list * instruction list
  | LOOP               of instruction list
  | LOOP_LEFT          of instruction list
  | LAMBDA             of type_ * type_ * instruction list
  | EXEC
  | DIP                of int * instruction list
  | FAILWITH
  | CAST
  | RENAME
  | CONCAT
  | SLICE
  | PACK
  | UNPACK             of type_
  | ADD
  | SUB
  | MUL
  | EDIV
  | ABS
  | ISNAT
  | INT
  | NEG
  | LSL
  | LSR
  | OR
  | AND
  | XOR
  | NOT
  | COMPARE
  | EQ
  | NEQ
  | LT
  | GT
  | LE
  | GE
  | SELF
  | CONTRACT           of type_
  | TRANSFER_TOKENS
  | SET_DELEGATE
  | CREATE_ACCOUNT
  | CREATE_CONTRACT    of instruction list
  | IMPLICIT_ACCOUNT
  | NOW
  | AMOUNT
  | BALANCE
  | CHECK_SIGNATURE
  | BLAKE2B
  | SHA256
  | SHA512
  | HASH_KEY
  | STEPS_TO_QUOTA
  | SOURCE
  | SENDER
  | ADDRESS
  | CHAIN_ID
and data =
  | Dint               of Core.big_int
  | Dstring            of string
  | Dbytes             of string
  | Dunit
  | Dtrue
  | Dfalse
  | Dpair              of data * data
  | Dleft              of data
  | Dright             of data
  | Dsome              of data
  | Dnone
  | Dlist              of data list
  | Dplist             of (data * data) list

type michelson = {
  storage: type_;
  parameter: type_;
  code: instruction;
}

let mk_michelson storage parameter code =
  { storage; parameter; code }