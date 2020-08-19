open Ident
open Tools

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

type z_operator =
  | Znow
  | Zamount
  | Zbalance
  | Zsource
  | Zsender
  | Zaddress
  | Zchain_id
  | Zself_address
[@@deriving show {with_path = false}]

type un_operator =
  | Ucar
  | Ucdr
  | Uneg
  | Uint
  | Unot
  | Uabs
  | Uisnat
  | Usome
  | Usize
  | Upack
  | Uunpack  of type_
  | Ublake2b
  | Usha256
  | Usha512
  | Uhash_key
[@@deriving show {with_path = false}]

type bin_operator =
  | Badd
  | Bsub
  | Bmul
  | Bediv
  | Blsl
  | Blsr
  | Bor
  | Band
  | Bxor
  | Bcompare
  | Beq
  | Bneq
  | Blt
  | Bgt
  | Ble
  | Bge
  | Bget
  | Bmem
  | Bconcat
  | Bcons
[@@deriving show {with_path = false}]

type ter_operator =
  | Tcheck_signature
  | Tslice
  | Tupdate
[@@deriving show {with_path = false}]

type place =
  | Local
  | Storage
[@@deriving show {with_path = false}]

type instruction =
  | IletIn  of ident * instruction * instruction
  | Ivar    of ident
  | Icall   of ident * instruction list
  | Iassign of ident * place * instruction
  | Izop    of z_operator
  | Iunop   of un_operator * instruction
  | Ibinop  of bin_operator * instruction * instruction
  | Iterop  of ter_operator * instruction * instruction * instruction
  | Iconst  of type_ * data
  | Iif     of instruction * instruction * instruction
  | Ifail   of instruction
  | Iseq    of instruction list
[@@deriving show {with_path = false}]

type func = {
  name: ident;
  args: (ident * type_) list;
  ret: type_;
  body: instruction;
}
[@@deriving show {with_path = false}]

type entry = {
  name: ident;
  args: (ident * type_) list;
  body: instruction;
}
[@@deriving show {with_path = false}]

type ir = {
  storage: type_ * data;
  funs: func list;
  entries: entry list;
}
[@@deriving show {with_path = false}]

type code =
  | SEQ                of code list
  | DROP               of int
  | DUP
  | SWAP
  | DIG                of int
  | DUG                of int
  | PUSH               of type_* data
  | SOME
  | NONE               of type_
  | UNIT
  | IF_NONE            of code list * code list
  | PAIR
  | CAR
  | CDR
  | LEFT               of type_
  | RIGHT              of type_
  | IF_LEFT            of code list * code list
  | NIL                of type_
  | CONS
  | IF_CONS            of code list * code list
  | SIZE
  | EMPTY_SET          of type_
  | EMPTY_MAP          of type_
  | EMPTY_BIG_MAP      of type_
  | MAP                of code list
  | ITER               of code list
  | MEM
  | GET
  | UPDATE
  | IF                 of code list * code list
  | LOOP               of code list
  | LOOP_LEFT          of code list
  | LAMBDA             of type_ * type_ * code list
  | EXEC
  | DIP                of int * code list
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
  | CREATE_CONTRACT    of code list
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
  code: code;
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

let mk_type ?annotation node : type_ =
  {node; annotation}

let mk_func name args ret body : func =
  {name; args; ret; body}

let mk_entry name args body : entry =
  {name; args; body}

let mk_ir ?(funs=[]) storage entries : ir =
  {storage; funs; entries}

let mk_michelson storage parameter code =
  { storage; parameter; code }

(* -------------------------------------------------------------------- *)

let itrue  = Iconst (mk_type Tbool, Dtrue)
let ifalse = Iconst (mk_type Tbool, Dfalse)
let iint n = Iconst (mk_type Tint,  Dint n)
let inat n = Iconst (mk_type Tnat,  Dint n)

(* -------------------------------------------------------------------- *)

let map_code_gen (fc : code -> code) (fd : data -> data) (ft : type_ -> type_) = function
  | SEQ l                   -> SEQ (List.map fc l)
  | DROP n                  -> DROP n
  | DUP                     -> DUP
  | SWAP                    -> SWAP
  | DIG n                   -> DIG n
  | DUG n                   -> DUG n
  | PUSH (t, d)             -> PUSH (ft t, fd d)
  | SOME                    -> SOME
  | NONE t                  -> NONE (ft t)
  | UNIT                    -> UNIT
  | IF_NONE (then_, else_)  -> IF_NONE (List.map fc then_, List.map fc else_)
  | PAIR                    -> PAIR
  | CAR                     -> CAR
  | CDR                     -> CDR
  | LEFT t                  -> LEFT (ft t)
  | RIGHT t                 -> RIGHT (ft t)
  | IF_LEFT (then_, else_)  -> IF_LEFT (List.map fc then_, List.map fc else_)
  | NIL t                   -> NIL (ft t)
  | CONS                    -> CONS
  | IF_CONS (then_, else_)  -> IF_CONS (List.map fc then_, List.map fc else_)
  | SIZE                    -> SIZE
  | EMPTY_SET      t        -> EMPTY_SET     (ft t)
  | EMPTY_MAP      t        -> EMPTY_MAP     (ft t)
  | EMPTY_BIG_MAP  t        -> EMPTY_BIG_MAP (ft t)
  | MAP  l                  -> MAP  (List.map fc l)
  | ITER l                  -> ITER (List.map fc l)
  | MEM                     -> MEM
  | GET                     -> GET
  | UPDATE                  -> UPDATE
  | IF (then_, else_)       -> IF (List.map fc then_, List.map fc else_)
  | LOOP l                  -> LOOP (List.map fc l)
  | LOOP_LEFT l             -> LOOP_LEFT (List.map fc l)
  | LAMBDA (at, rt, body)   -> LAMBDA (ft at, ft rt, List.map fc body)
  | EXEC                    -> EXEC
  | DIP (n, l)              -> DIP (n, List.map fc l)
  | FAILWITH                -> FAILWITH
  | CAST                    -> CAST
  | RENAME                  -> RENAME
  | CONCAT                  -> CONCAT
  | SLICE                   -> SLICE
  | PACK                    -> PACK
  | UNPACK t                -> UNPACK (ft t)
  | ADD                     -> ADD
  | SUB                     -> SUB
  | MUL                     -> MUL
  | EDIV                    -> EDIV
  | ABS                     -> ABS
  | ISNAT                   -> ISNAT
  | INT                     -> INT
  | NEG                     -> NEG
  | LSL                     -> LSL
  | LSR                     -> LSR
  | OR                      -> OR
  | AND                     -> AND
  | XOR                     -> XOR
  | NOT                     -> NOT
  | COMPARE                 -> COMPARE
  | EQ                      -> EQ
  | NEQ                     -> NEQ
  | LT                      -> LT
  | GT                      -> GT
  | LE                      -> LE
  | GE                      -> GE
  | SELF                    -> SELF
  | CONTRACT t              -> CONTRACT (ft t)
  | TRANSFER_TOKENS         -> TRANSFER_TOKENS
  | SET_DELEGATE            -> SET_DELEGATE
  | CREATE_ACCOUNT          -> CREATE_ACCOUNT
  | CREATE_CONTRACT l       -> CREATE_CONTRACT (List.map fc l)
  | IMPLICIT_ACCOUNT        -> IMPLICIT_ACCOUNT
  | NOW                     -> NOW
  | AMOUNT                  -> AMOUNT
  | BALANCE                 -> BALANCE
  | CHECK_SIGNATURE         -> CHECK_SIGNATURE
  | BLAKE2B                 -> BLAKE2B
  | SHA256                  -> SHA256
  | SHA512                  -> SHA512
  | HASH_KEY                -> HASH_KEY
  | STEPS_TO_QUOTA          -> STEPS_TO_QUOTA
  | SOURCE                  -> SOURCE
  | SENDER                  -> SENDER
  | ADDRESS                 -> ADDRESS
  | CHAIN_ID                -> CHAIN_ID

let map_code (fc : code -> code) = map_code_gen fc id id

let rec flat (c : code) : code =
  match c with
  | SEQ l -> SEQ (List.fold_right (fun x accu -> match flat x with | SEQ l -> l @ accu | a -> a::accu) l [])
  (* TODO: handle constructors with code list arg*)
  | _ -> map_code flat c
