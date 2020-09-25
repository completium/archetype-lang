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
  | Dvar               of string (* for symbolic execution *)
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
  | CONTRACT           of type_ * ident option
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
  | Uleft  of type_
  | Uright of type_
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
  | Ucontract of type_ * ident option
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
  | Ttransfer_tokens
[@@deriving show {with_path = false}]

type cmp_operator =
  | Ceq
  | Cne
  | Cgt
  | Cge
  | Clt
  | Cle
[@@deriving show {with_path = false}]

type builtin =
  | Bmin of type_
  | Bmax of type_
  | Bfloor
  | Bceil
  | BlistContains of type_
  | BlistNth of type_
  | Btostring of type_
  | Bratcmp
  | Bratnorm
  | Brataddsub
  | Bratdiv
  | Bratmul
  | Bratuminus
  | Bratabs
  | Brattez
  | Bratdur
[@@deriving show {with_path = false}]

type instruction =
  | Iseq        of instruction list
  | IletIn      of ident * instruction * instruction * bool
  | Ivar        of ident
  | Icall       of ident * instruction list * bool
  | Iassign     of ident * instruction
  | IassignRec  of ident * int * int * instruction
  | Iif         of instruction * instruction * instruction * type_
  | Iifnone     of instruction * instruction * ident * instruction
  | Iifcons     of instruction * instruction * instruction
  | Iwhile      of instruction * instruction
  | Iiter       of ident list * instruction * instruction
  | Izop        of z_operator
  | Iunop       of un_operator * instruction
  | Ibinop      of bin_operator * instruction * instruction
  | Iterop      of ter_operator * instruction * instruction * instruction
  | Iconst      of type_ * data
  | Icompare    of cmp_operator * instruction * instruction
  | Iset        of type_ * instruction list
  | Ilist       of type_ * instruction list
  | Imap        of type_ * type_ * (instruction * instruction) list
  | Irecord     of instruction list
  | Irecupdate  of instruction * int * (int * instruction) list (* value * size * (index, value) fields *)
  | Ifold       of ident * ident option * ident * instruction * instruction * instruction (* var_iterated * var_accu * container * init * code*)
  | Imichelson  of instruction list * code * ident list
[@@deriving show {with_path = false}]

type implem =
  | Concrete of (ident * type_) list * instruction
  | Abstract of builtin
[@@deriving show {with_path = false}]

type func = {
  name: ident;
  targ: type_;
  tret: type_;
  body: implem;
}
[@@deriving show {with_path = false}]

type entry = {
  name: ident;
  args: (ident * type_) list;
  eargs: (ident * type_) list;
  body: instruction;
}
[@@deriving show {with_path = false}]

type ir = {
  storage_type: type_;
  storage_data : data;
  storage_list: (ident * type_) list;
  with_operations: bool;
  parameter: type_;
  funs: func list;
  entries: entry list;
}
[@@deriving show {with_path = false}]

type michelson = {
  storage: type_;
  parameter: type_;
  code: code;
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)
type alpha_ident = int
[@@deriving show {with_path = false}]

type dexpr =
  | Dalpha of alpha_ident
  | Dstorage of type_
  | Dparameter of type_
  | Dresult of type_
  | Doperations
  | Ddata of data
  | Dzop of z_operator
  | Duop of un_operator  * dexpr
  | Dbop of bin_operator * dexpr * dexpr
  | Dtop of ter_operator * dexpr * dexpr * dexpr
[@@deriving show {with_path = false}]

type equation = dexpr * dexpr

type sys_equation = equation list

(* -------------------------------------------------------------------- *)

let mk_type ?annotation node : type_ =
  {node; annotation}

let mk_func name targ tret body : func =
  {name; targ; tret; body}

let mk_entry name args eargs body : entry =
  {name; args; eargs; body}

let mk_ir storage_type storage_data storage_list ?(with_operations = false) parameter funs entries : ir =
  {storage_type; storage_data; storage_list; with_operations; parameter; funs; entries}

let mk_michelson storage parameter code =
  { storage; parameter; code }


(* -------------------------------------------------------------------- *)

let toperation    = mk_type Toperation
let tunit         = mk_type Tunit
let tstring       = mk_type Tstring
let tnat          = mk_type Tnat
let tint          = mk_type Tint
let tbool         = mk_type Tbool
let tmutez        = mk_type Tmutez
let tpair t1 t2   = mk_type (Tpair (t1, t2))
let tor t1 t2     = mk_type (Tor (t1, t2))
let trat          = tpair tint tnat
let tlist t       = mk_type (Tlist t)
let tset t        = mk_type (Tlist t)
let tmap t1 t2    = mk_type (Tmap (t1, t2))
let tlambda t1 t2 = mk_type (Tlambda (t1, t2))

(* -------------------------------------------------------------------- *)

let itrue      = Iconst (tbool,    Dtrue)
let ifalse     = Iconst (tbool,    Dfalse)
let iint n     = Iconst (tint,     Dint n)
let inat n     = Iconst (tnat,     Dint n)
let istring s  = Iconst (tstring,  Dstring s)
let imutez  v  = Iconst (tmutez,   Dint v)
let isome   s  = Iunop  (Usome, s)
let inone   t  = Izop   (Znone t)
let iunit      = Iconst (tunit, Dunit)
let icar x     = Iunop  (Ucar, x)
let icdr x     = Iunop  (Ucdr, x)
let ifail msg  = Iunop (Ufail, istring msg)
let iskip      = Iseq []
let ileft t x  = Iunop  (Uleft t, x)
let iright t x = Iunop  (Uright t, x)
let ieq  l r   = Icompare (Ceq, l, r)
let iadd l r   = Ibinop (Badd, l, r)
let isub l r   = Ibinop (Bsub, l, r)
let imul l r   = Ibinop (Bmul, l, r)
let idiv l r   = Iifnone (Ibinop (Bediv, l, r), ifail "DivByZero", "_var_ifnone", icar (Ivar ("_var_ifnone")) )
let imod l r   = Iifnone (Ibinop (Bediv, l, r), ifail "DivByZero", "_var_ifnone", icdr (Ivar ("_var_ifnone")) )

(* -------------------------------------------------------------------- *)

let ctrue     = PUSH (mk_type Tbool, Dtrue)
let cfalse    = PUSH (mk_type Tbool, Dfalse)
let cint n    = PUSH (mk_type Tint,  Dint n)
let cnat n    = PUSH (mk_type Tnat,  Dint n)
let cstring s = PUSH (mk_type Tstring,  Dstring s)
let cfail msg = SEQ [PUSH (mk_type Tstring,  Dstring msg); FAILWITH]
let cskip     = SEQ []

(* -------------------------------------------------------------------- *)

let dalpha n  = Dalpha n

(* -------------------------------------------------------------------- *)

let cmp_ident = String.equal

let cmp_type lhs rhs =
  let rec f lhs rhs =
    match lhs.node, rhs.node with
    | Taddress, Taddress                   -> true
    | Tbig_map (a1, b1), Tbig_map (a2, b2) -> f a1 a2 && f b1 b2
    | Tbool, Tbool                         -> true
    | Tbytes, Tbytes                       -> true
    | Tchain_id, Tchain_id                 -> true
    | Tcontract a1, Tcontract a2           -> f a1 a2
    | Tint, Tint                           -> true
    | Tkey, Tkey                           -> true
    | Tkey_hash, Tkey_hash                 -> true
    | Tlambda (a1, b1), Tlambda (a2, b2)   -> f a1 a2 && f b1 b2
    | Tlist a1, Tlist a2                   -> f a1 a2
    | Tmap (a1, b1), Tmap (a2, b2)         -> f a1 a2 && f b1 b2
    | Tmutez, Tmutez                       -> true
    | Tnat, Tnat                           -> true
    | Toperation, Toperation               -> true
    | Toption a1, Toption a2               -> f a1 a2
    | Tor (a1, b1), Tor (a2, b2)           -> f a1 a2 && f b1 b2
    | Tpair (a1, b1), Tpair (a2, b2)       -> f a1 a2 && f b1 b2
    | Tset a1, Tset a2                     -> f a1 a2
    | Tsignature, Tsignature               -> true
    | Tstring, Tstring                     -> true
    | Ttimestamp, Ttimestamp               -> true
    | Tunit, Tunit                         -> true
    | _ -> false
  in
  f lhs rhs

let cmp_data lhs rhs =
  let rec f lhs rhs =
    match lhs, rhs with
    | Dint n1, Dint n2               -> Big_int.eq_big_int n1 n2
    | Dstring s1, Dstring s2         -> String.equal s1 s2
    | Dbytes s1, Dbytes s2           -> String.equal s1 s2
    | Dunit, Dunit                   -> true
    | Dtrue, Dtrue                   -> true
    | Dfalse, Dfalse                 -> true
    | Dpair (a1, b1), Dpair (a2, b2) -> f a1 a2 && f b1 b2
    | Dleft d1, Dleft d2             -> f d1 d2
    | Dright d1, Dright d2           -> f d1 d2
    | Dsome d1, Dsome d2             -> f d1 d2
    | Dnone, Dnone                   -> true
    | Dlist ds1, Dlist ds2           -> List.for_all2 f ds1 ds2
    | Dplist l1, Dplist l2           -> List.for_all2 (fun (k1, v1) (k2, v2) -> f k1 k2 && f v1 v2) l1 l2
    | _ -> false
  in
  f lhs rhs

let cmp_code lhs rhs =
  let rec f lhs rhs =
    match lhs, rhs with
    | SEQ l1, SEQ l2                                 -> List.for_all2 f l1 l2
    | DROP n1, DROP n2                               -> n1 = n2
    | DUP, DUP                                       -> true
    | SWAP, SWAP                                     -> true
    | DIG n1, DIG n2                                 -> n1 = n2
    | DUG n1, DUG n2                                 -> n1 = n2
    | PUSH (t1, d1), PUSH (t2, d2)                   -> cmp_type t1 t2 && cmp_data d1 d2
    | SOME, SOME                                     -> true
    | NONE t1, NONE t2                               -> cmp_type t1 t2
    | UNIT, UNIT                                     -> true
    | IF_NONE (t1, e1), IF_NONE (t2, e2)             -> List.for_all2 f t1 t2 && List.for_all2 f e1 e2
    | PAIR, PAIR                                     -> true
    | CAR, CAR                                       -> true
    | CDR, CDR                                       -> true
    | UNPAIR, UNPAIR                                 -> true
    | LEFT t1, LEFT t2                               -> cmp_type t1 t2
    | RIGHT t1, RIGHT t2                             -> cmp_type t1 t2
    | IF_LEFT (t1, e1), IF_LEFT (t2, e2)             -> List.for_all2 f t1 t2 && List.for_all2 f e1 e2
    | NIL t1, NIL t2                                 -> cmp_type t1 t2
    | CONS, CONS                                     -> true
    | IF_CONS (t1, e1), IF_CONS (t2, e2)             -> List.for_all2 f t1 t2 && List.for_all2 f e1 e2
    | SIZE, SIZE                                     -> true
    | EMPTY_SET t1, EMPTY_SET t2                     -> cmp_type t1 t2
    | EMPTY_MAP (k1, v1), EMPTY_MAP (k2, v2)         -> cmp_type k1 k2 && cmp_type v1 v2
    | EMPTY_BIG_MAP (k1, v1), EMPTY_BIG_MAP (k2, v2) -> cmp_type k1 k2 && cmp_type v1 v2
    | MAP  l1, MAP  l2                               -> List.for_all2 f l1 l2
    | ITER l1, ITER l2                               -> List.for_all2 f l1 l2
    | MEM, MEM                                       -> true
    | GET, GET                                       -> true
    | UPDATE, UPDATE                                 -> true
    | IF (t1, e1), IF (t2, e2)                       -> List.for_all2 f t1 t2 && List.for_all2 f e1 e2
    | LOOP l1, LOOP l2                               -> List.for_all2 f l1 l2
    | LOOP_LEFT l1, LOOP_LEFT l2                     -> List.for_all2 f l1 l2
    | LAMBDA (a1, r1, b1), LAMBDA (a2, r2, b2)       -> cmp_type a1 a2 && cmp_type r1 r2 && List.for_all2 f b1 b2
    | EXEC, EXEC                                     -> true
    | DIP (n1, l1), DIP (n2, l2)                     -> n1 = n2 && List.for_all2 f l1 l2
    | FAILWITH, FAILWITH                             -> true
    | CAST, CAST                                     -> true
    | RENAME, RENAME                                 -> true
    | CONCAT, CONCAT                                 -> true
    | SLICE, SLICE                                   -> true
    | PACK, PACK                                     -> true
    | UNPACK t1, UNPACK t2                           -> cmp_type t1 t2
    | ADD, ADD                                       -> true
    | SUB, SUB                                       -> true
    | MUL, MUL                                       -> true
    | EDIV, EDIV                                     -> true
    | ABS, ABS                                       -> true
    | ISNAT, ISNAT                                   -> true
    | INT, INT                                       -> true
    | NEG, NEG                                       -> true
    | LSL, LSL                                       -> true
    | LSR, LSR                                       -> true
    | OR, OR                                         -> true
    | AND, AND                                       -> true
    | XOR, XOR                                       -> true
    | NOT, NOT                                       -> true
    | COMPARE, COMPARE                               -> true
    | EQ, EQ                                         -> true
    | NEQ, NEQ                                       -> true
    | LT, LT                                         -> true
    | GT, GT                                         -> true
    | LE, LE                                         -> true
    | GE, GE                                         -> true
    | ASSERT_EQ, ASSERT_EQ                           -> true
    | ASSERT_NEQ, ASSERT_NEQ                         -> true
    | ASSERT_LT, ASSERT_LT                           -> true
    | ASSERT_LE, ASSERT_LE                           -> true
    | ASSERT_GT, ASSERT_GT                           -> true
    | ASSERT_GE, ASSERT_GE                           -> true
    | SELF, SELF                                     -> true
    | CONTRACT (t1, a1), CONTRACT (t2, a2)           -> cmp_type t1 t2 && Option.cmp cmp_ident a1 a2
    | TRANSFER_TOKENS, TRANSFER_TOKENS               -> true
    | SET_DELEGATE, SET_DELEGATE                     -> true
    | CREATE_ACCOUNT, CREATE_ACCOUNT                 -> true
    | CREATE_CONTRACT l1, CREATE_CONTRACT l2         -> List.for_all2 f l1 l2
    | IMPLICIT_ACCOUNT, IMPLICIT_ACCOUNT             -> true
    | NOW, NOW                                       -> true
    | AMOUNT, AMOUNT                                 -> true
    | BALANCE, BALANCE                               -> true
    | CHECK_SIGNATURE, CHECK_SIGNATURE               -> true
    | BLAKE2B, BLAKE2B                               -> true
    | SHA256, SHA256                                 -> true
    | SHA512, SHA512                                 -> true
    | HASH_KEY, HASH_KEY                             -> true
    | STEPS_TO_QUOTA, STEPS_TO_QUOTA                 -> true
    | SOURCE, SOURCE                                 -> true
    | SENDER, SENDER                                 -> true
    | ADDRESS, ADDRESS                               -> true
    | CHAIN_ID, CHAIN_ID                             -> true
    | _ -> false
  in
  f lhs rhs

let cmp_builtin lhs rhs =
  match lhs, rhs with
  | Bmin t1, Bmin t2                   -> cmp_type t1 t2
  | Bmax t1, Bmax t2                   -> cmp_type t1 t2
  | Bfloor, Bfloor                     -> true
  | Bceil, Bceil                       -> true
  | BlistContains t1, BlistContains t2 -> cmp_type t1 t2
  | BlistNth t1, BlistNth t2           -> cmp_type t1 t2
  | Btostring t1, Btostring t2         -> cmp_type t1 t2
  | Bratcmp, Bratcmp                   -> true
  | Bratnorm, Bratnorm                 -> true
  | Brataddsub, Brataddsub             -> true
  | Bratdiv, Bratdiv                   -> true
  | Bratmul, Bratmul                   -> true
  | Bratuminus, Bratuminus             -> true
  | Bratabs, Bratabs                   -> true
  | Brattez, Brattez                   -> true
  | Bratdur, Bratdur                   -> true
  | _ -> false

let map_data (f : data -> data) = function
  | Dint n       -> Dint n
  | Dstring v    -> Dstring v
  | Dbytes v     -> Dbytes v
  | Dunit        -> Dunit
  | Dtrue        -> Dtrue
  | Dfalse       -> Dfalse
  | Dpair (l, r) -> Dpair (f l, f r)
  | Dleft v      -> Dleft (f v)
  | Dright v     -> Dright (f v)
  | Dsome v      -> Dsome (f v)
  | Dnone        -> Dnone
  | Dlist l      -> Dlist (List.map f l)
  | Dplist l     -> Dplist (List.map (fun (x, y) -> f x, f y) l)
  | Dvar v       -> Dvar v

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
  | CONTRACT (t, a)         -> CONTRACT (ft t, a)
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

let rec map_seq f x =
  let g x = f (List.map (map_seq f) x) in
  match x with
  | SEQ l             -> SEQ (g l)
  | IF_NONE (x, y)    -> IF_NONE (g x, g y)
  | IF_LEFT (x, y)    -> IF_LEFT (g x, g y)
  | IF_CONS (x, y)    -> IF_CONS (g x, g y)
  | MAP x             -> MAP (g x)
  | ITER x            -> ITER (g x)
  | IF (x, y)         -> IF (g x, g y)
  | LOOP x            -> LOOP (g x)
  | LOOP_LEFT x       -> LOOP_LEFT (g x)
  | LAMBDA (a, b, x)  -> LAMBDA (a, b, g x)
  | DIP (n, x)        -> DIP (n, g x)
  | CREATE_CONTRACT x -> CREATE_CONTRACT (g x)
  | x -> map_code (map_seq f) x

module Utils : sig

  val get_fun_name : (type_ -> ident) -> builtin -> ident
  val flat         : code -> code
  val optim        : code -> code

end = struct

  let get_fun_name ft = function
    | Bmin _t         -> "_min_"(*  ^ (ft t) *)
    | Bmax _t         -> "_max_"(*  ^ (ft t) *)
    | Bfloor          -> "_floor"
    | Bceil           -> "_ceil"
    | BlistContains t -> "_list_contains_" ^ (ft t)
    | BlistNth t      -> "_list_nth_"      ^ (ft t)
    | Btostring t     -> "_to_string_"     ^ (ft t)
    | Bratcmp         -> "_ratcmp"
    | Bratnorm        -> "_ratnorm"
    | Brataddsub      -> "_rataddsub"
    | Bratmul         -> "_ratmul"
    | Bratdiv         -> "_ratdiv"
    | Bratuminus      -> "_ratuminus"
    | Bratabs         -> "_ratabs"
    | Brattez         -> "_rattez"
    | Bratdur         -> "_ratdur"

  let rec flat (c : code) : code =
    let f l = List.fold_right (fun x accu -> match flat x with | SEQ l -> l @ accu | a -> a::accu) l [] in
    map_seq f c

  let handle_failwith (c : code) : code =
    let f l =
      let rec aux accu l =
        match l with
        | (FAILWITH)::_           -> aux (FAILWITH::accu) []
        | e::t -> aux (e::accu) t
        | [] -> List.rev accu
      in
      aux [] l
    in
    map_seq f c

  let factorize_instrs (c : code) : code =
    let f l =
      let rec aux accu l =
        match l with
        | (DROP x)::(DROP y)::t -> aux accu ((DROP (x + y))::t)
        | (DUP)::(DROP x)::t    -> aux accu ((DROP (x - 1))::t)
        | (DUP)::(SWAP)::t      -> aux accu ((DUP)::t)
        | (DROP 0)::t           -> aux accu t
        | e::t -> aux (e::accu) t
        | [] -> List.rev accu
      in
      aux [] l
    in
    map_seq f c

  let rec factorize_double_branches (c : code) : code =
    let g = List.map factorize_double_branches in
    let rec map f x =
      match x with
      | IF (x, y)      -> let a, x, y = f (g x) (g y) in SEQ ([IF (x, y)] @ a)
      | IF_NONE (x, y) -> let a, x, y = f (g x) (g y) in SEQ ([IF_NONE (x, y)] @ a)
      | IF_LEFT (x, y) -> let a, x, y = f (g x) (g y) in SEQ ([IF_LEFT (x, y)] @ a)
      | IF_CONS (x, y) -> let a, x, y = f (g x) (g y) in SEQ ([IF_CONS (x, y)] @ a)
      | x -> map_code (map f) x
    in
    let f x y =
      let rec aux accu a b =
        match a, b with
        | x::t, y::u when cmp_code x y -> aux (x::accu) t u
        | _ -> accu, List.rev a, List.rev b
      in
      aux [] (List.rev x) (List.rev y)
    in
    map f c
    |> flat

  let optim c =
    c
    |> handle_failwith
    |> factorize_instrs
    (* |> factorize_double_branches *)

end
