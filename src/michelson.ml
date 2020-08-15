open Ident

type 'a with_annot = {
  node:       'a;
  annotation: ident option;
}
[@@deriving show {with_path = false}]

type type_node =
  | Taddress
  | Tbig_map   of type_ * type_
  | Tbool
  | Tbytes
  | Tchain_id
  | Tcontract  of type_
  | Tint
  | Tkey
  | Tkey_hash
  | Tlambda    of type_ * type_
  | Tlist      of type_
  | Tmap       of type_ * type_
  | Tmutez
  | Tnat
  | Toperation
  | Toption    of type_
  | Tor        of type_ * type_
  | Tpair      of type_ * type_
  | Tset       of type_
  | Tsignature
  | Tstring
  | Ttimestamp
  | Tunit
[@@deriving show {with_path = false}]

and type_ = type_node with_annot
[@@deriving show {with_path = false}]

type data =
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
[@@deriving show {with_path = false}]

type ir = {
  storage: type_ * data;
}
[@@deriving show {with_path = false}]

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
[@@deriving show {with_path = false}]

type michelson = {
  storage: type_;
  parameter: type_;
  code: instruction;
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

let mk_type ?annotation node : type_ =
  {node; annotation}

let mk_ir storage : ir =
  {storage}

let mk_michelson storage parameter code =
  { storage; parameter; code }
