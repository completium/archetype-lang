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
  | Znone of type_
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
  | Ufail
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
  | Bget
  | Bmem
  | Bconcat
  | Bcons
  | Bpair
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

type cmp_operator =
  | Ceq
  | Cne
  | Cgt
  | Cge
  | Clt
  | Cle
[@@deriving show {with_path = false}]

type instruction =
  | Iseq     of instruction list
  | IletIn   of ident * instruction * instruction
  | Ivar     of ident
  | Icall    of ident * instruction list
  | Iassign  of ident * place * instruction
  | Iif      of instruction * instruction * instruction
  | Iwhile   of instruction * instruction
  | Iiter    of ident list * instruction * instruction
  | Izop     of z_operator
  | Iunop    of un_operator * instruction
  | Ibinop   of bin_operator * instruction * instruction
  | Iterop   of ter_operator * instruction * instruction * instruction
  | Iconst   of type_ * data
  | Icompare of cmp_operator * instruction * instruction
  | Iset     of type_ * instruction list
  | Ilist    of type_ * instruction list
  | Imap     of type_ * type_ * (instruction * instruction) list
  | Irecord  of instruction list
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
  storage_type: type_;
  storage_data : data;
  storage_list: (ident * type_) list;
  parameter: type_;
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
  | UNPAIR
  | LEFT               of type_
  | RIGHT              of type_
  | IF_LEFT            of code list * code list
  | NIL                of type_
  | CONS
  | IF_CONS            of code list * code list
  | SIZE
  | EMPTY_SET          of type_
  | EMPTY_MAP          of type_ * type_
  | EMPTY_BIG_MAP      of type_ * type_
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
  | ASSERT_EQ
  | ASSERT_NEQ
  | ASSERT_LT
  | ASSERT_LE
  | ASSERT_GT
  | ASSERT_GE
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

let mk_ir ?(funs=[]) storage_type storage_data storage_list parameter entries : ir =
  {storage_type; storage_data; storage_list; parameter; funs; entries}

let mk_michelson storage parameter code =
  { storage; parameter; code }

(* -------------------------------------------------------------------- *)

let toperation = mk_type Toperation


(* -------------------------------------------------------------------- *)

let tunit   = mk_type Tunit
let tstring = mk_type Tstring
let tnat    = mk_type Tnat
let tint    = mk_type Tint
let tbool   = mk_type Tbool


(* -------------------------------------------------------------------- *)

let itrue     = Iconst (tbool,    Dtrue)
let ifalse    = Iconst (tbool,    Dfalse)
let iint n    = Iconst (tint,     Dint n)
let inat n    = Iconst (tnat,     Dint n)
let istring s = Iconst (tstring,  Dstring s)
let isome   s = Iunop  (Usome, s)
let inone   t = Izop   (Znone t)
let iunit     = Iconst (tunit, Dunit)

(* -------------------------------------------------------------------- *)

let ctrue     = PUSH (mk_type Tbool, Dtrue)
let cfalse    = PUSH (mk_type Tbool, Dfalse)
let cint n    = PUSH (mk_type Tint,  Dint n)
let cnat n    = PUSH (mk_type Tnat,  Dint n)
let cstring s = PUSH (mk_type Tstring,  Dstring s)

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
  | UNPAIR                  -> UNPAIR
  | LEFT t                  -> LEFT (ft t)
  | RIGHT t                 -> RIGHT (ft t)
  | IF_LEFT (then_, else_)  -> IF_LEFT (List.map fc then_, List.map fc else_)
  | NIL t                   -> NIL (ft t)
  | CONS                    -> CONS
  | IF_CONS (then_, else_)  -> IF_CONS (List.map fc then_, List.map fc else_)
  | SIZE                    -> SIZE
  | EMPTY_SET      t        -> EMPTY_SET     (ft t)
  | EMPTY_MAP      (k, v)   -> EMPTY_MAP     (ft k, ft v)
  | EMPTY_BIG_MAP  (k, v)   -> EMPTY_BIG_MAP (ft k, ft v)
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
  | ASSERT_EQ               -> ASSERT_EQ
  | ASSERT_NEQ              -> ASSERT_NEQ
  | ASSERT_LT               -> ASSERT_LT
  | ASSERT_LE               -> ASSERT_LE
  | ASSERT_GT               -> ASSERT_GT
  | ASSERT_GE               -> ASSERT_GE
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

let rec map_seq f = function
  | SEQ l             -> SEQ (f l)
  | IF_NONE (x, y)    -> IF_NONE (f x, f y)
  | IF_LEFT (x, y)    -> IF_LEFT (f x, f y)
  | IF_CONS (x, y)    -> IF_CONS (f x, f y)
  | MAP x             -> MAP (f x)
  | ITER x            -> ITER (f x)
  | IF (x, y)         -> IF (f x, f y)
  | LOOP x            -> LOOP (f x)
  | LOOP_LEFT x       -> LOOP_LEFT (f x)
  | LAMBDA (a, b, x)  -> LAMBDA (a, b, f x)
  | DIP (n, x)        -> DIP (n, f x)
  | CREATE_CONTRACT x -> CREATE_CONTRACT (f x)
  | x -> map_code (map_seq f) x

module Utils : sig

  val flat : code -> code
  val optim : code -> code

end = struct

  let rec flat (c : code) : code =
    let f l = List.fold_right (fun x accu -> match flat x with | SEQ l -> l @ accu | a -> a::accu) l [] in
    map_seq f c

  let factorize_instrs (c : code) : code =
    let f l =
      let rec aux accu l =
        match l with
        | (DROP x)::(DROP y)::t -> aux accu ((DROP (x + y))::t)
        | e::t -> aux (e::accu) t
        | [] -> List.rev accu
      in
      aux [] l
    in
    map_seq f c

  let optim c =
    c
    |> factorize_instrs

end
