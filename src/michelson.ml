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
  | Tsapling_transaction
  | Tsapling_state
  | Tnever
  | Tbls12_381_g1
  | Tbls12_381_g2
  | Tbls12_381_fr
  | Tticket of type_
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
  | Delt               of data * data
  | Dvar               of ident
[@@deriving show {with_path = false}]

type code =
  (* Control structures *)
  | SEQ                of code list
  | APPLY
  | EXEC
  | FAILWITH
  | IF                 of code list * code list
  | IF_CONS            of code list * code list
  | IF_LEFT            of code list * code list
  | IF_NONE            of code list * code list
  | ITER               of code list
  | LAMBDA             of type_ * type_ * code list
  | LOOP               of code list
  | LOOP_LEFT          of code list
  (* Stack manipulation *)
  | DIG                of int
  | DIP                of int * code list
  | DROP               of int
  | DUG                of int
  | DUP
  | PUSH               of type_* data
  | SWAP
  (* Arthmetic operations *)
  | ABS
  | ADD
  | COMPARE
  | EDIV
  | EQ
  | GE
  | GT
  | INT
  | ISNAT
  | LE
  | LSL
  | LSR
  | LT
  | MUL
  | NEG
  | NEQ
  | SUB
  (* Boolean operations *)
  | AND
  | NOT
  | OR
  | XOR
  (* Cryptographic operations *)
  | BLAKE2B
  | CHECK_SIGNATURE
  | HASH_KEY
  | SHA256
  | SHA512
  (* Blockchain operations *)
  | ADDRESS
  | AMOUNT
  | BALANCE
  | CHAIN_ID
  | CONTRACT           of type_ * ident option
  | CREATE_CONTRACT    of type_ * type_ * code
  | IMPLICIT_ACCOUNT
  | NOW
  | SELF               of ident option
  | SENDER
  | SET_DELEGATE
  | SOURCE
  | TRANSFER_TOKENS
  (* Operations on data structures *)
  | CAR
  | CDR
  | CONCAT
  | CONS
  | EMPTY_BIG_MAP      of type_ * type_
  | EMPTY_MAP          of type_ * type_
  | EMPTY_SET          of type_
  | GET
  | LEFT               of type_
  | MAP                of code list
  | MEM
  | NIL                of type_
  | NONE               of type_
  | PACK
  | PAIR
  | RIGHT              of type_
  | SIZE
  | SLICE
  | SOME
  | UNIT
  | UNPACK             of type_
  | UPDATE
  (* Other *)
  | UNPAIR
  | SELF_ADDRESS
  | CAST               of type_
  | CREATE_ACCOUNT
  | RENAME
  | STEPS_TO_QUOTA
  | LEVEL
  | SAPLING_EMPTY_STATE
  | SAPLING_VERIFY_UPDATE
  | NEVER
  | VOTING_POWER
  | TOTAL_VOTING_POWER
  | KECCAK
  | SHA3
  | PAIRING_CHECK
  | SUBMIT_PROPOSALS
  | SUBMIT_BALLOT
  | SET_BAKER_ACTIVE
  | TOGGLE_BAKER_DELEGATIONS
  | SET_BAKER_CONSENSUS_KEY
  | SET_BAKER_PVSS_KEY

[@@deriving show {with_path = false}]

type z_operator =
  | Znow
  | Zamount
  | Zbalance
  | Zsource
  | Zsender
  | Zaddress
  | Zchain_id
  | Zself of ident option
  | Zself_address
  | Znone of type_
  | Zunit
  | Znil       of type_
  | Zemptyset  of type_
  | Zemptymap  of type_ * type_
  | Zemptybigmap  of type_ * type_
  | Ztotalvotingpower
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
  | Usha3
  | Ukeccak
  | Uhash_key
  | Ufail
  | Ucontract of type_ * ident option
  | Usetdelegate
  | Uimplicitaccount
  | Ueq
  | Une
  | Ugt
  | Uge
  | Ult
  | Ule
  | Uvotingpower
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
  | Bexec
  | Bapply
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
  | Iif         of instruction * instruction * instruction * type_
  | Iifnone     of instruction * instruction * ident * instruction * type_
  | Iifleft     of instruction * ident * instruction * ident * instruction * type_
  | Iifcons     of instruction * ident * ident * instruction * instruction * type_
  | Iloop       of instruction * instruction
  | Iiter       of ident list * instruction * instruction
  | Iloopleft   of instruction * ident * instruction
  | Ilambda     of type_ * ident * type_ * instruction
  | Izop        of z_operator
  | Iunop       of un_operator * instruction
  | Ibinop      of bin_operator * instruction * instruction
  | Iterop      of ter_operator * instruction * instruction * instruction
  | Iconst      of type_ * data
  | Icompare    of cmp_operator * instruction * instruction
  | Iset        of type_ * instruction list
  | Ilist       of type_ * instruction list
  | Imap        of bool * type_ * type_ * (instruction * instruction) list
  | Irecord     of ritem
  | Irecupdate  of instruction * ruitem
  | Imap_       of instruction * ident * instruction
  | Ifold       of ident * ident option * ident * instruction * instruction * instruction (* var_iterated * var_accu * container * init * code*)
  | Imichelson  of instruction list * code * ident list
[@@deriving show {with_path = false}]

and ritem =
  | Rtuple of instruction list
  | Rnodes of ritem list
[@@deriving show {with_path = false}]

and ruitem =
  | RUnodes  of int * (int * ruitem) list
  | RUassign of int * (int * instruction) list
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
  name: ident;
  storage_type: type_;
  storage_data : data;
  storage_list: (ident * type_) list;
  with_operations: bool;
  parameter: type_;
  funs: func list;
  entries: entry list;
  parameters: ident list;
}
[@@deriving show {with_path = false}]

type michelson = {
  storage: type_;
  parameter: type_;
  code: code;
  parameters: ident list;
}
[@@deriving show {with_path = false}]

type prim = {
  prim: ident;
  args: obj_micheline list;
  annots: ident list;
}
[@@deriving show {with_path = false}]

and obj_micheline =
  | Oprim of prim
  | Ostring of string
  | Obytes of string
  | Oint of string
  | Oarray of obj_micheline list
  | Ovar of ident
[@@deriving show {with_path = false}]

type micheline = {
  code: obj_micheline list;
  storage: obj_micheline;
  parameters: ident list;
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)
type alpha_ident = int
[@@deriving show {with_path = false}]

type dexpr =
  | Dalpha    of alpha_ident
  | Dvar      of type_
  | Dstorage  of type_
  | Doperations
  | Dlbdparam
  | Dlbdresult
  | Ddata     of data
  | Dzop      of z_operator
  | Duop      of un_operator  * dexpr
  | Dbop      of bin_operator * dexpr * dexpr
  | Dtop      of ter_operator * dexpr * dexpr * dexpr
  | Dapply    of dexpr * dexpr
  | Dexec     of dexpr * dexpr
  | Dlambda   of type_ * type_ * dinstruction list
  | Dloopleft of dexpr * dinstruction list
  | Dmap      of dexpr * dinstruction list
[@@deriving show {with_path = false}]

and dinstruction =
  | Ddecl     of alpha_ident * dexpr option
  | Dassign   of dexpr * dexpr
  | Dfail     of dexpr
  | Dif       of dexpr * dinstruction list * dinstruction list
  | Difcons   of dexpr * alpha_ident * alpha_ident * dinstruction list * dinstruction list
  | Difleft   of dexpr * alpha_ident * dinstruction list * alpha_ident * dinstruction list
  | Difnone   of dexpr * dinstruction list * alpha_ident * dinstruction list
  | Dloop     of dexpr * dinstruction list
  | Diter     of dexpr * dinstruction list
[@@deriving show {with_path = false}]

type dprogram = {
  name: ident;
  storage: type_;
  parameter: type_;
  storage_data: data;
  code: dinstruction list;
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

let mk_type ?annotation node : type_ =
  { node; annotation }

let mk_func name targ tret body : func =
  { name; targ; tret; body }

let mk_entry name args eargs body : entry =
  { name; args; eargs; body }

let mk_ir ?(parameters = []) name storage_type storage_data storage_list ?(with_operations = false) parameter funs entries : ir =
  { name; storage_type; storage_data; storage_list; with_operations; parameter; funs; entries; parameters }

let mk_michelson ?(parameters = []) storage parameter code : michelson =
  { storage; parameter; code; parameters }

let mk_prim ?(args=[]) ?(annots=[]) prim : prim =
  { prim; args; annots }

let mk_micheline ?(parameters = []) code storage : micheline =
  { code; storage; parameters }

let mk_dprogram storage parameter storage_data name code =
  { name; storage; parameter; storage_data; code }

(* -------------------------------------------------------------------- *)

let toperation    = mk_type Toperation
let tunit         = mk_type Tunit
let tstring       = mk_type Tstring
let tnat          = mk_type Tnat
let tint          = mk_type Tint
let tbool         = mk_type Tbool
let tmutez        = mk_type Tmutez
let taddress      = mk_type Taddress
let ttimestamp    = mk_type Ttimestamp
let tbytes        = mk_type Tbytes
let tpair t1 t2   = mk_type (Tpair (t1, t2))
let tor t1 t2     = mk_type (Tor (t1, t2))
let trat          = tpair tint tnat
let tlist t       = mk_type (Tlist t)
let tset t        = mk_type (Tlist t)
let tmap t1 t2    = mk_type (Tmap (t1, t2))
let tlambda t1 t2 = mk_type (Tlambda (t1, t2))

(* -------------------------------------------------------------------- *)

let itrue            = Iconst (tbool,    Dtrue)
let ifalse           = Iconst (tbool,    Dfalse)
let iint n           = Iconst (tint,     Dint n)
let inat n           = Iconst (tnat,     Dint n)
let istring s        = Iconst (tstring,  Dstring s)
let imutez  v        = Iconst (tmutez,   Dint v)
let isome   s        = Iunop  (Usome, s)
let inone   t        = Izop   (Znone t)
let iunit            = Izop    Zunit
let inil t           = Izop (Znil t)
let iemptyset t      = Izop (Zemptyset t)
let iemptymap k v    = Izop (Zemptymap (k, v))
let iemptybigmap k v = Izop (Zemptybigmap (k, v))
let icar x           = Iunop  (Ucar, x)
let icdr x           = Iunop  (Ucdr, x)
let ifail msg        = Iunop (Ufail, istring msg)
let iskip            = Iseq []
let ileft t x        = Iunop  (Uleft t, x)
let iright t x       = Iunop  (Uright t, x)
let ieq  l r         = Icompare (Ceq, l, r)
let iadd l r         = Ibinop (Badd, l, r)
let isub l r         = Ibinop (Bsub, l, r)
let imul l r         = Ibinop (Bmul, l, r)
let idiv l r         = Iifnone (Ibinop (Bediv, l, r), ifail "DivByZero", "_var_ifnone", icar (Ivar ("_var_ifnone")), tint )
let imod l r         = Iifnone (Ibinop (Bediv, l, r), ifail "DivByZero", "_var_ifnone", icdr (Ivar ("_var_ifnone")), tnat )
let irecord ir       = Irecord ir
let isrecord l       = irecord (Rtuple l)

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
    | Taddress, Taddress                         -> true
    | Tbig_map (a1, b1), Tbig_map (a2, b2)       -> f a1 a2 && f b1 b2
    | Tbool, Tbool                               -> true
    | Tbytes, Tbytes                             -> true
    | Tchain_id, Tchain_id                       -> true
    | Tcontract a1, Tcontract a2                 -> f a1 a2
    | Tint, Tint                                 -> true
    | Tkey, Tkey                                 -> true
    | Tkey_hash, Tkey_hash                       -> true
    | Tlambda (a1, b1), Tlambda (a2, b2)         -> f a1 a2 && f b1 b2
    | Tlist a1, Tlist a2                         -> f a1 a2
    | Tmap (a1, b1), Tmap (a2, b2)               -> f a1 a2 && f b1 b2
    | Tmutez, Tmutez                             -> true
    | Tnat, Tnat                                 -> true
    | Toperation, Toperation                     -> true
    | Toption a1, Toption a2                     -> f a1 a2
    | Tor (a1, b1), Tor (a2, b2)                 -> f a1 a2 && f b1 b2
    | Tpair (a1, b1), Tpair (a2, b2)             -> f a1 a2 && f b1 b2
    | Tset a1, Tset a2                           -> f a1 a2
    | Tsignature, Tsignature                     -> true
    | Tstring, Tstring                           -> true
    | Ttimestamp, Ttimestamp                     -> true
    | Tunit, Tunit                               -> true
    | Tsapling_transaction, Tsapling_transaction -> true
    | Tsapling_state, Tsapling_state             -> true
    | Tnever, Tnever                             -> true
    | Tbls12_381_g1, Tbls12_381_g1               -> true
    | Tbls12_381_g2, Tbls12_381_g2               -> true
    | Tbls12_381_fr, Tbls12_381_fr               -> true
    | Tticket a1, Tticket a2                     -> f a1 a2
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
    | Delt (a1, b1), Delt (a2, b2)   -> f a1 a2 && f b1 b2
    | _ -> false
  in
  f lhs rhs

let cmp_z_operator lhs rhs =
  match lhs, rhs with
  | Znow, Znow                                   -> true
  | Zamount, Zamount                             -> true
  | Zbalance, Zbalance                           -> true
  | Zsource, Zsource                             -> true
  | Zsender, Zsender                             -> true
  | Zaddress, Zaddress                           -> true
  | Zchain_id, Zchain_id                         -> true
  | Zself a1, Zself a2                           -> Option.cmp String.equal a1 a2
  | Zself_address, Zself_address                 -> true
  | Znone t1, Znone t2                           -> cmp_type t1 t2
  | Zunit, Zunit                                 -> true
  | Znil t1, Znil t2                             -> cmp_type t1 t2
  | Zemptyset t1, Zemptyset t2                   -> cmp_type t1 t2
  | Zemptymap (k1, v1), Zemptymap (k2, v2)       -> cmp_type k1 k2 && cmp_type v1 v2
  | Zemptybigmap (k1, v1), Zemptybigmap (k2, v2) -> cmp_type k1 k2 && cmp_type v1 v2
  | _ -> false

let cmp_un_operator lhs rhs =
  match lhs, rhs with
  | Ucar, Ucar                             -> true
  | Ucdr, Ucdr                             -> true
  | Uleft t1, Uleft t2                     -> cmp_type t1 t2
  | Uright t1, Uright t2                   -> cmp_type t1 t2
  | Uneg, Uneg                             -> true
  | Uint, Uint                             -> true
  | Unot, Unot                             -> true
  | Uabs, Uabs                             -> true
  | Uisnat, Uisnat                         -> true
  | Usome, Usome                           -> true
  | Usize, Usize                           -> true
  | Upack, Upack                           -> true
  | Uunpack t1, Uunpack t2                 -> cmp_type t1 t2
  | Ublake2b, Ublake2b                     -> true
  | Usha256, Usha256                       -> true
  | Usha512, Usha512                       -> true
  | Uhash_key, Uhash_key                   -> true
  | Ufail, Ufail                           -> true
  | Ucontract (t1, i1), Ucontract (t2, i2) -> cmp_type t1 t2 && Option.cmp String.equal i1 i2
  | Usetdelegate, Usetdelegate             -> true
  | Uimplicitaccount, Uimplicitaccount     -> true
  | Ueq, Ueq                               -> true
  | Une, Une                               -> true
  | Ugt, Ugt                               -> true
  | Uge, Uge                               -> true
  | Ult, Ult                               -> true
  | Ule, Ule                               -> true
  | _ -> false

let cmp_bin_operator lhs rhs =
  match lhs, rhs with
  | Badd, Badd         -> true
  | Bsub, Bsub         -> true
  | Bmul, Bmul         -> true
  | Bediv, Bediv       -> true
  | Blsl, Blsl         -> true
  | Blsr, Blsr         -> true
  | Bor, Bor           -> true
  | Band, Band         -> true
  | Bxor, Bxor         -> true
  | Bcompare, Bcompare -> true
  | Bget, Bget         -> true
  | Bmem, Bmem         -> true
  | Bconcat, Bconcat   -> true
  | Bcons, Bcons       -> true
  | Bpair, Bpair       -> true
  | Bexec, Bexec       -> true
  | Bapply, Bapply     -> true
  | _ -> false

let cmp_ter_operator lhs rhs =
  match lhs, rhs with
  | Tcheck_signature, Tcheck_signature -> true
  | Tslice, Tslice                     -> true
  | Tupdate, Tupdate                   -> true
  | Ttransfer_tokens, Ttransfer_tokens -> true
  | _ -> false

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
    | SELF_ADDRESS, SELF_ADDRESS                     -> true
    | APPLY, APPLY                                   -> true
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
    | CAST t1, CAST t2                               -> cmp_type t1 t2
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
    | SELF a1, SELF a2                               -> Option.cmp String.equal a1 a2
    | CONTRACT (t1, a1), CONTRACT (t2, a2)           -> cmp_type t1 t2 && Option.cmp cmp_ident a1 a2
    | TRANSFER_TOKENS, TRANSFER_TOKENS               -> true
    | SET_DELEGATE, SET_DELEGATE                     -> true
    | CREATE_ACCOUNT, CREATE_ACCOUNT                 -> true
    | CREATE_CONTRACT (p1, s1, c1), CREATE_CONTRACT (p2, s2, c2) -> cmp_type p1 p2 && cmp_type s1 s2 && f c1 c2
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

let map_type (f : type_ -> type_) (t : type_) : type_ =
  let node =
    match t.node with
    | Taddress             -> Taddress
    | Tbig_map   (k, v)    -> Tbig_map   (f k, f v)
    | Tbool                -> Tbool
    | Tbytes               -> Tbytes
    | Tchain_id            -> Tchain_id
    | Tcontract  t         -> Tcontract  (f t)
    | Tint                 -> Tint
    | Tkey                 -> Tkey
    | Tkey_hash            -> Tkey_hash
    | Tlambda    (a, r)    -> Tlambda    (f a, f r)
    | Tlist      t         -> Tlist      (f t)
    | Tmap       (k, v)    -> Tmap       (f k, f v)
    | Tmutez               -> Tmutez
    | Tnat                 -> Tnat
    | Toperation           -> Toperation
    | Toption    t         -> Toption    (f t)
    | Tor        (l, r)    -> Tor        (f l, f r)
    | Tpair      (l, r)    -> Tpair      (f l, f r)
    | Tset       t         -> Tset       (f t)
    | Tsignature           -> Tsignature
    | Tstring              -> Tstring
    | Ttimestamp           -> Ttimestamp
    | Tunit                -> Tunit
    | Tsapling_transaction -> Tsapling_transaction
    | Tsapling_state       -> Tsapling_state
    | Tnever               -> Tnever
    | Tbls12_381_g1        -> Tbls12_381_g1
    | Tbls12_381_g2        -> Tbls12_381_g2
    | Tbls12_381_fr        -> Tbls12_381_fr
    | Tticket       t      -> Tticket    (f t)
  in
  {node = node; annotation = t.annotation}

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
  | Delt (l, r)  -> Delt (f l, f r)
  | Dvar c       -> Dvar c

let map_code_gen (fc : code -> code) (fd : data -> data) (ft : type_ -> type_) = function
  (* Control structures *)
  | SEQ l                    -> SEQ (List.map fc l)
  | APPLY                    -> APPLY
  | EXEC                     -> EXEC
  | FAILWITH                 -> FAILWITH
  | IF (then_, else_)        -> IF (List.map fc then_, List.map fc else_)
  | IF_CONS (then_, else_)   -> IF_CONS (List.map fc then_, List.map fc else_)
  | IF_LEFT (then_, else_)   -> IF_LEFT (List.map fc then_, List.map fc else_)
  | IF_NONE (then_, else_)   -> IF_NONE (List.map fc then_, List.map fc else_)
  | ITER l                   -> ITER (List.map fc l)
  | LAMBDA (at, rt, body)    -> LAMBDA (ft at, ft rt, List.map fc body)
  | LOOP l                   -> LOOP (List.map fc l)
  | LOOP_LEFT l              -> LOOP_LEFT (List.map fc l)
  (* Stack manipulation *)
  | DIG n                    -> DIG n
  | DIP (n, l)               -> DIP (n, List.map fc l)
  | DROP n                   -> DROP n
  | DUG n                    -> DUG n
  | DUP                      -> DUP
  | PUSH (t, d)              -> PUSH (ft t, fd d)
  | SWAP                     -> SWAP
  (* Arthmetic operations *)
  | ABS                      -> ABS
  | ADD                      -> ADD
  | COMPARE                  -> COMPARE
  | EDIV                     -> EDIV
  | EQ                       -> EQ
  | GE                       -> GE
  | GT                       -> GT
  | INT                      -> INT
  | ISNAT                    -> ISNAT
  | LE                       -> LE
  | LSL                      -> LSL
  | LSR                      -> LSR
  | LT                       -> LT
  | MUL                      -> MUL
  | NEG                      -> NEG
  | NEQ                      -> NEQ
  | SUB                      -> SUB
  (* Boolean operations *)
  | AND                      -> AND
  | NOT                      -> NOT
  | OR                       -> OR
  | XOR                      -> XOR
  (* Cryptographic operations *)
  | BLAKE2B                  -> BLAKE2B
  | CHECK_SIGNATURE          -> CHECK_SIGNATURE
  | HASH_KEY                 -> HASH_KEY
  | SHA256                   -> SHA256
  | SHA512                   -> SHA512
  (* Blockchain operations *)
  | ADDRESS                  -> ADDRESS
  | AMOUNT                   -> AMOUNT
  | BALANCE                  -> BALANCE
  | CHAIN_ID                 -> CHAIN_ID
  | CONTRACT (t, a)          -> CONTRACT (ft t, a)
  | CREATE_CONTRACT (p, s, c)-> CREATE_CONTRACT (ft p, ft s, fc c)
  | IMPLICIT_ACCOUNT         -> IMPLICIT_ACCOUNT
  | NOW                      -> NOW
  | SELF a                   -> SELF a
  | SENDER                   -> SENDER
  | SET_DELEGATE             -> SET_DELEGATE
  | SOURCE                   -> SOURCE
  | TRANSFER_TOKENS          -> TRANSFER_TOKENS
  (* Operations on data structures *)
  | CAR                      -> CAR
  | CDR                      -> CDR
  | CONCAT                   -> CONCAT
  | CONS                     -> CONS
  | EMPTY_BIG_MAP  (k, v)    -> EMPTY_BIG_MAP (ft k, ft v)
  | EMPTY_MAP      (k, v)    -> EMPTY_MAP     (ft k, ft v)
  | EMPTY_SET      t         -> EMPTY_SET     (ft t)
  | GET                      -> GET
  | LEFT t                   -> LEFT (ft t)
  | MAP  l                   -> MAP  (List.map fc l)
  | MEM                      -> MEM
  | NIL t                    -> NIL (ft t)
  | NONE t                   -> NONE (ft t)
  | PACK                     -> PACK
  | PAIR                     -> PAIR
  | RIGHT t                  -> RIGHT (ft t)
  | SIZE                     -> SIZE
  | SLICE                    -> SLICE
  | SOME                     -> SOME
  | UNIT                     -> UNIT
  | UNPACK t                 -> UNPACK (ft t)
  | UPDATE                   -> UPDATE
  (* Other *)
  | UNPAIR                   -> UNPAIR
  | SELF_ADDRESS             -> SELF_ADDRESS
  | CAST t                   -> CAST (ft t)
  | CREATE_ACCOUNT           -> CREATE_ACCOUNT
  | RENAME                   -> RENAME
  | STEPS_TO_QUOTA           -> STEPS_TO_QUOTA
  | LEVEL                    -> LEVEL
  | SAPLING_EMPTY_STATE      -> SAPLING_EMPTY_STATE
  | SAPLING_VERIFY_UPDATE    -> SAPLING_VERIFY_UPDATE
  | NEVER                    -> NEVER
  | VOTING_POWER             -> VOTING_POWER
  | TOTAL_VOTING_POWER       -> TOTAL_VOTING_POWER
  | KECCAK                   -> KECCAK
  | SHA3                     -> SHA3
  | PAIRING_CHECK            -> PAIRING_CHECK
  | SUBMIT_PROPOSALS         -> SUBMIT_PROPOSALS
  | SUBMIT_BALLOT            -> SUBMIT_BALLOT
  | SET_BAKER_ACTIVE         -> SET_BAKER_ACTIVE
  | TOGGLE_BAKER_DELEGATIONS -> TOGGLE_BAKER_DELEGATIONS
  | SET_BAKER_CONSENSUS_KEY  -> SET_BAKER_CONSENSUS_KEY
  | SET_BAKER_PVSS_KEY       -> SET_BAKER_PVSS_KEY


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
  | x -> map_code (map_seq f) x


(* -------------------------------------------------------------------- *)

let rec cmp_dexpr (lhs : dexpr) (rhs : dexpr) =
  match lhs, rhs with
  | Dalpha i1, Dalpha i2                             -> i1 = i2
  | Dvar t1, Dvar t2                                 -> cmp_type t1 t2
  | Dstorage t1, Dstorage t2                         -> cmp_type t1 t2
  | Doperations, Doperations                         -> true
  | Dlbdparam, Dlbdparam                             -> true
  | Dlbdresult, Dlbdresult                           -> true
  | Ddata d1, Ddata d2                               -> cmp_data d1 d2
  | Dzop op1, Dzop op2                               -> cmp_z_operator op1 op2
  | Duop (op1, x1), Duop (op2, x2)                   -> cmp_un_operator op1 op2 && cmp_dexpr x1 x2
  | Dbop (op1, x1, y1), Dbop (op2, x2, y2)           -> cmp_bin_operator op1 op2 && cmp_dexpr x1 x2 && cmp_dexpr y1 y2
  | Dtop (op1, x1, y1, z1), Dtop (op2, x2, y2, z2)   -> cmp_ter_operator op1 op2 && cmp_dexpr x1 x2 && cmp_dexpr y1 y2 && cmp_dexpr z1 z2
  | Dapply (l1, a1), Dapply (l2, a2)                 -> cmp_dexpr l1 l2 && cmp_dexpr a1 a2
  | Dexec (l1, a1), Dexec (l2, a2)                   -> cmp_dexpr l1 l2 && cmp_dexpr a1 a2
  | Dlambda (at1, rt1, is1), Dlambda (at2, rt2, is2) -> cmp_type at1 at2 && cmp_type rt1 rt2 && List.for_all2 cmp_dinstruction is1 is2
  | Dloopleft (l1, is1), Dloopleft (l2, is2)         -> cmp_dexpr l1 l2 && List.for_all2 cmp_dinstruction is1 is2
  | Dmap (l1, is1), Dmap (l2, is2)                   -> cmp_dexpr l1 l2 && List.for_all2 cmp_dinstruction is1 is2
  | _ -> false

and cmp_dinstruction (lhs : dinstruction) (rhs : dinstruction) =
  match lhs, rhs with
  | Dassign (e1, v1), Dassign (e2, v2) -> cmp_dexpr e1 e2 && cmp_dexpr v1 v2
  | Dif (c1, t1, e1), Dif (c2, t2, e2) -> cmp_dexpr c1 c2 && List.for_all2 cmp_dinstruction t1 t2 && List.for_all2 cmp_dinstruction e1 e2
  | Dfail e1, Dfail e2                 -> cmp_dexpr e1 e2
  | _ -> false


let map_dexpr f fi fai x =
  let g = List.map fi in
  match x with
  | Dalpha     i            -> Dalpha      (fai i)
  | Dvar       t            -> Dvar         t
  | Dstorage   t            -> Dstorage     t
  | Doperations             -> Doperations
  | Dlbdparam               -> Dlbdparam
  | Dlbdresult              -> Dlbdresult
  | Ddata      d            -> Ddata      d
  | Dzop       op           -> Dzop       op
  | Duop      (op, x)       -> Duop      (op, f x)
  | Dbop      (op, x, y)    -> Dbop      (op, f x, f y)
  | Dtop      (op, x, y, z) -> Dtop      (op, f x, f y, f z)
  | Dapply    (l, a)        -> Dapply    (f l, f a)
  | Dexec     (l, a)        -> Dexec     (f l, f a)
  | Dlambda   (at, rt, is)  -> Dlambda   (at, rt, g is)
  | Dloopleft (l, is)       -> Dloopleft (f l, g is)
  | Dmap      (l, is)       -> Dmap      (f l, g is)

let map_dexpr_dinstr fi fe fai x =
  let g = List.map fi in
  match x with
  | Ddecl   (i, e)              -> Ddecl   (fai i, Option.map fe e)
  | Dassign (e, v)              -> Dassign (fe e, fe v)
  | Dfail    v                  -> Dfail   (fe v)
  | Dif     (c, t, e)           -> Dif     (fe c, g t, g e)
  | Difcons (c, hd, tl, ti, ei) -> Difcons (fe c, fai hd, fai tl, g ti, g ei)
  | Difleft (c,  l, ti,  r, ei) -> Difleft (fe c, fai l, g ti, fai r, g ei)
  | Difnone (c, ti, v, ei)      -> Difnone (fe c, g ti, fai v, g ei)
  | Dloop   (e, is)             -> Dloop   (fe e, g is)
  | Diter   (e, is)             -> Diter   (fe e, g is)

let fold_dexpr f fi accu x =
  let g accu is = List.fold_left (fun accu x -> fi accu x) accu is in
  match x with
  | Dalpha     _            -> accu
  | Dvar       _            -> accu
  | Dstorage   _            -> accu
  | Doperations             -> accu
  | Dlbdparam               -> accu
  | Dlbdresult              -> accu
  | Ddata      _            -> accu
  | Dzop       _            -> accu
  | Duop      (_, x)        -> f accu x
  | Dbop      (_, x, y)     -> f (f accu x) y
  | Dtop      (_, x, y, z)  -> f (f (f accu x) y) z
  | Dapply    (l, a)        -> f (f accu l) a
  | Dexec     (l, a)        -> f (f accu l) a
  | Dlambda   (_, _, is)    -> g accu is
  | Dloopleft (l, is)       -> g (f accu l) is
  | Dmap      (l, is)       -> g (f accu l) is

let fold_dexpr_dinstr fi fe accu x =
  let g accu is = List.fold_left (fun accu x -> fi accu x) accu is in
  match x with
  | Ddecl   (_, e)              -> Option.fold fe accu e
  | Dassign (e, v)              -> fe (fe accu e) v
  | Dfail    v                  -> fe accu v
  | Dif     (c, t, e)           -> g (g (fe accu c) t) e
  | Difcons (c, _, _, ti, ei)   -> g (g (fe accu c) ti) ei
  | Difleft (c,  _, ti,  _, ei) -> g (g (fe accu c) ti) ei
  | Difnone (c, ti, _, ei)      -> g (g (fe accu c) ti) ei
  | Dloop   (e, is)             -> g (fe accu e) is
  | Diter   (e, is)             -> g (fe accu e) is

module Utils : sig

  val get_fun_name  : (type_ -> ident) -> builtin -> ident
  val flat          : code -> code
  val optim         : code -> code
  val replace_macro : code -> code
  val data_to_micheline : data -> obj_micheline
  val type_to_micheline : type_ -> obj_micheline
  val to_micheline : michelson -> data -> micheline

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
    let init = (false, []) in
    let rec aux (c : code) =
      let rec for_seq ((b, accu) : bool * code list) l : bool * code list =
        match l with
        | FAILWITH::_ -> for_seq (true, FAILWITH::accu) []
        | e::t        -> begin
            let bb, a = aux e in
            let t = if bb then [] else t in
            for_seq (bb, a::accu) t
          end
        | []          -> b, List.rev accu
      in

      let g x f =
        let _, x = for_seq init x in
        false, f x
      in

      let h x y f =
        let b0, x = for_seq init x in
        let b1, y = for_seq init y in
        b0 && b1, f x y
      in

      match c with
      | SEQ x             -> g x (fun l -> SEQ (l))
      | IF (x, y)         -> h x y (fun a b -> IF (a, b))
      | IF_NONE (x, y)    -> h x y (fun a b -> IF_NONE (a, b))
      | IF_LEFT (x, y)    -> h x y (fun a b -> IF_LEFT (a, b))
      | IF_CONS (x, y)    -> h x y (fun a b -> IF_CONS (a, b))
      | MAP x             -> g x (fun l -> MAP (l))
      | ITER x            -> g x (fun l -> ITER (l))
      | LOOP x            -> g x (fun l -> LOOP (l))
      | LOOP_LEFT x       -> g x (fun l -> LOOP_LEFT (l))
      | DIP (n, x)        -> g x (fun l -> DIP (n, l))
      | _                 -> false, c
    in
    aux c |> snd

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

  let replace_macro c =
    let rec aux c =
      match c with
      | UNPAIR -> SEQ [DUP; CAR; DIP (1, [CDR])]
      | _ -> map_code aux c
    in
    aux c

  let rec type_to_micheline (t : type_) : obj_micheline =
    let prim, args =
      match t.node with
      | Taddress             -> "address", []
      | Tbig_map (k, v)      -> "big_map", [k; v]
      | Tbool                -> "bool", []
      | Tbytes               -> "bytes", []
      | Tchain_id            -> "chain_id", []
      | Tcontract t          -> "contract", [t]
      | Tint                 -> "int", []
      | Tkey                 -> "key", []
      | Tkey_hash            -> "key_hash", []
      | Tlambda (a, r)       -> "lambda", [a; r]
      | Tlist t              -> "list", [t]
      | Tmap (k, v)          -> "map", [k; v]
      | Tmutez               -> "mutez", []
      | Tnat                 -> "nat", []
      | Toperation           -> "operation", []
      | Toption t            -> "option", [t]
      | Tor (l, r)           -> "or", [l; r]
      | Tpair (l, r)         -> "pair", [l; r]
      | Tset t               -> "set", [t]
      | Tsignature           -> "signature", []
      | Tstring              -> "string", []
      | Ttimestamp           -> "timestamp", []
      | Tunit                -> "unit", []
      | Tsapling_transaction -> "sapling_transaction", []
      | Tsapling_state       -> "sapling_state", []
      | Tnever               -> "never", []
      | Tbls12_381_g1        -> "bls12_381_g1", []
      | Tbls12_381_g2        -> "bls12_381_g2", []
      | Tbls12_381_fr        -> "bls12_381_fr", []
      | Tticket t            -> "ticket", [t]
    in
    let args = if List.is_empty args then None else Some (List.map type_to_micheline args) in
    let annots = Option.bind (fun x -> Some [x]) t.annotation in
    let prim = mk_prim ?args ?annots prim in
    Oprim prim

  let rec data_to_micheline (d : data) : obj_micheline =
    let f = data_to_micheline in
    match d with
    | Dint n       -> Oint (Big_int.string_of_big_int n)
    | Dstring v    -> Ostring v
    | Dbytes v     -> Obytes v
    | Dunit        -> Oprim (mk_prim "Unit")
    | Dtrue        -> Oprim (mk_prim "True")
    | Dfalse       -> Oprim (mk_prim "False")
    | Dpair (l, r) -> Oprim (mk_prim ~args:[f l; f r] "Pair")
    | Dleft v      -> Oprim (mk_prim ~args:[f v] "Left")
    | Dright v     -> Oprim (mk_prim ~args:[f v] "Right")
    | Dsome v      -> Oprim (mk_prim ~args:[f v] "Some")
    | Dnone        -> Oprim (mk_prim "None")
    | Dlist l      -> Oarray (List.map f l)
    | Delt (l, r)  -> Oprim (mk_prim ~args:[f l; f r] "Elt")
    | Dvar x       -> Ovar x

  let rec code_to_micheline (c : code) : obj_micheline =
    let f = code_to_micheline in
    let ft = type_to_micheline in
    let fd = data_to_micheline in
    let mk ?(args=[]) ?(annots=[]) x = Oprim (mk_prim ~args ~annots x) in
    let mk_int n = Oint (string_of_int n) in
    let mk_array l = Oarray (List.map f l) in
    let fan = function | Some v -> [v] | None -> [] in
    match c with
    (* Control structures *)
    | SEQ l                    -> mk_array l
    | APPLY                    -> mk "APPLY"
    | EXEC                     -> mk "EXEC"
    | FAILWITH                 -> mk "FAILWITH"
    | IF (t, e)                -> mk ~args:[mk_array t; mk_array e] "IF"
    | IF_CONS (t, e)           -> mk ~args:[mk_array t; mk_array e] "IF_CONS"
    | IF_LEFT (t, e)           -> mk ~args:[mk_array t; mk_array e] "IF_LEFT"
    | IF_NONE (t, e)           -> mk ~args:[mk_array t; mk_array e] "IF_NONE"
    | ITER l                   -> mk ~args:[mk_array l] "ITER"
    | LAMBDA (at, rt, body)    -> mk ~args:[ft at; ft rt; mk_array body] "LAMBDA"
    | LOOP l                   -> mk ~args:[mk_array l] "LOOP"
    | LOOP_LEFT l              -> mk ~args:[mk_array l] "LOOP_LEFT"
    (* Stack manipulation *)
    | DIG n                    -> mk ~args:[mk_int n] "DIG"
    | DIP (n, l)               -> mk ~args:[mk_int n; mk_array l] "DIP"
    | DROP n                   -> mk ~args:[mk_int n] "DROP"
    | DUG n                    -> mk ~args:[mk_int n] "DUG"
    | DUP                      -> mk "DUP"
    | PUSH (t, d)              -> mk ~args:[ft t; fd d] "PUSH"
    | SWAP                     -> mk "SWAP"
    (* Arthmetic operations *)
    | ABS                      -> mk "ABS"
    | ADD                      -> mk "ADD"
    | COMPARE                  -> mk "COMPARE"
    | EDIV                     -> mk "EDIV"
    | EQ                       -> mk "EQ"
    | GE                       -> mk "GE"
    | GT                       -> mk "GT"
    | INT                      -> mk "INT"
    | ISNAT                    -> mk "ISNAT"
    | LE                       -> mk "LE"
    | LSL                      -> mk "LSL"
    | LSR                      -> mk "LSR"
    | LT                       -> mk "LT"
    | MUL                      -> mk "MUL"
    | NEG                      -> mk "NEG"
    | NEQ                      -> mk "NEQ"
    | SUB                      -> mk "SUB"
    (* Boolean operations *)
    | AND                      -> mk "AND"
    | NOT                      -> mk "NOT"
    | OR                       -> mk "OR"
    | XOR                      -> mk "XOR"
    (* Cryptographic operations *)
    | BLAKE2B                  -> mk "BLAKE2B"
    | CHECK_SIGNATURE          -> mk "CHECK_SIGNATURE"
    | HASH_KEY                 -> mk "HASH_KEY"
    | SHA256                   -> mk "SHA256"
    | SHA512                   -> mk "SHA512"
    (* Blockchain operations *)
    | ADDRESS                  -> mk "ADDRESS"
    | AMOUNT                   -> mk "AMOUNT"
    | BALANCE                  -> mk "BALANCE"
    | CHAIN_ID                 -> mk "CHAIN_ID"
    | CONTRACT (t, a)          -> mk ~args:[ft t] ~annots:(fan a) "CONTRACT"
    | CREATE_CONTRACT (p, s, c)-> mk ~args:[mk ~args:[ft p] "parameter"; mk ~args:[ft s] "storage"; mk ~args:[f c] "code"] "CREATE_CONTRACT"
    | IMPLICIT_ACCOUNT         -> mk "IMPLICIT_ACCOUNT"
    | NOW                      -> mk "NOW"
    | SELF a                   -> mk ~annots:(fan a) "SELF"
    | SENDER                   -> mk "SENDER"
    | SET_DELEGATE             -> mk "SET_DELEGATE"
    | SOURCE                   -> mk "SOURCE"
    | TRANSFER_TOKENS          -> mk "TRANSFER_TOKENS"
    (* Operations on data structures *)
    | CAR                      -> mk "CAR"
    | CDR                      -> mk "CDR"
    | CONCAT                   -> mk "CONCAT"
    | CONS                     -> mk "CONS"
    | EMPTY_BIG_MAP  (k, v)    -> mk ~args:[ft k; ft v] "EMPTY_BIG_MAP"
    | EMPTY_MAP      (k, v)    -> mk ~args:[ft k; ft v] "EMPTY_MAP"
    | EMPTY_SET      t         -> mk ~args:[ft t] "EMPTY_SET"
    | GET                      -> mk "GET"
    | LEFT t                   -> mk ~args:[ft t] "LEFT"
    | MAP  l                   -> mk ~args:[mk_array l] "MAP"
    | MEM                      -> mk "MEM"
    | NIL t                    -> mk ~args:[ft t] "NIL"
    | NONE t                   -> mk ~args:[ft t] "NONE"
    | PACK                     -> mk "PACK"
    | PAIR                     -> mk "PAIR"
    | RIGHT t                  -> mk ~args:[ft t] "RIGHT"
    | SIZE                     -> mk "SIZE"
    | SLICE                    -> mk "SLICE"
    | SOME                     -> mk "SOME"
    | UNIT                     -> mk "UNIT"
    | UNPACK t                 -> mk ~args:[ft t] "UNPACK"
    | UPDATE                   -> mk "UPDATE"
    (* Other *)
    | UNPAIR                   -> mk "UNPAIR"
    | SELF_ADDRESS             -> mk "SELF_ADDRESS"
    | CAST t                   -> mk ~args:[ft t] "CAST"
    | CREATE_ACCOUNT           -> mk "CREATE_ACCOUNT"
    | RENAME                   -> mk "RENAME"
    | STEPS_TO_QUOTA           -> mk "STEPS_TO_QUOTA"
    | LEVEL                    -> mk "LEVEL"
    | SAPLING_EMPTY_STATE      -> mk "SAPLING_EMPTY_STATE"
    | SAPLING_VERIFY_UPDATE    -> mk "SAPLING_VERIFY_UPDATE"
    | NEVER                    -> mk "NEVER"
    | VOTING_POWER             -> mk "VOTING_POWER"
    | TOTAL_VOTING_POWER       -> mk "TOTAL_VOTING_POWER"
    | KECCAK                   -> mk "KECCAK"
    | SHA3                     -> mk "SHA3"
    | PAIRING_CHECK            -> mk "PAIRING_CHECK"
    | SUBMIT_PROPOSALS         -> mk "SUBMIT_PROPOSALS"
    | SUBMIT_BALLOT            -> mk "SUBMIT_BALLOT"
    | SET_BAKER_ACTIVE         -> mk "SET_BAKER_ACTIVE"
    | TOGGLE_BAKER_DELEGATIONS -> mk "TOGGLE_BAKER_DELEGATIONS"
    | SET_BAKER_CONSENSUS_KEY  -> mk "SET_BAKER_CONSENSUS_KEY"
    | SET_BAKER_PVSS_KEY       -> mk "SET_BAKER_PVSS_KEY"


  let to_micheline (m : michelson) (s : data) : micheline =
    let storage   = type_to_micheline m.storage in
    let parameter = type_to_micheline m.parameter in
    let code      = code_to_micheline m.code in
    let f tag x   = Oprim (mk_prim ~args:[x] tag) in
    let parameters = m.parameters in
    mk_micheline ~parameters [f "storage" storage; f "parameter" parameter; f "code" code] (data_to_micheline s)

end

(***)

let rec to_type (o : obj_micheline) : type_ =
  let fa l = match l with | a::_ -> Some a | [] -> None in
  let f = to_type in
  match o with
  | Oprim ({prim = "address"; annots; _})                    -> mk_type ?annotation:(fa annots) Taddress
  | Oprim ({prim = "big_map"; annots; args = k::v::_})       -> mk_type ?annotation:(fa annots) (Tbig_map (f k, f v))
  | Oprim ({prim = "bool"; annots; _})                       -> mk_type ?annotation:(fa annots) Tbool
  | Oprim ({prim = "bytes"; annots; _})                      -> mk_type ?annotation:(fa annots) Tbytes
  | Oprim ({prim = "chain_id"; annots; _})                   -> mk_type ?annotation:(fa annots) Tchain_id
  | Oprim ({prim = "contract"; annots; args = t::_})         -> mk_type ?annotation:(fa annots) (Tcontract (f t))
  | Oprim ({prim = "int"; annots; _})                        -> mk_type ?annotation:(fa annots) Tint
  | Oprim ({prim = "key"; annots; _})                        -> mk_type ?annotation:(fa annots) Tkey
  | Oprim ({prim = "key_hash"; annots; _})                   -> mk_type ?annotation:(fa annots) Tkey_hash
  | Oprim ({prim = "lambda"; annots; args = a::r::_})        -> mk_type ?annotation:(fa annots) (Tlambda (f a, f r))
  | Oprim ({prim = "list"; annots; args = t::_})             -> mk_type ?annotation:(fa annots) (Tlist (f t))
  | Oprim ({prim = "map"; annots; args = k::v::_})           -> mk_type ?annotation:(fa annots) (Tmap (f k, f v))
  | Oprim ({prim = "mutez"; annots; _})                      -> mk_type ?annotation:(fa annots) Tmutez
  | Oprim ({prim = "nat"; annots; _})                        -> mk_type ?annotation:(fa annots) Tnat
  | Oprim ({prim = "operation"; annots; _})                  -> mk_type ?annotation:(fa annots) Toperation
  | Oprim ({prim = "option"; annots; args = t::_})           -> mk_type ?annotation:(fa annots) (Toption (f t))
  | Oprim ({prim = "or"; annots; args = a::b::_})            -> mk_type ?annotation:(fa annots) (Tor (f a, f b))
  | Oprim ({prim = "pair"; annots; args = a::l::_})          -> mk_type ?annotation:(fa annots) (Tpair (f a, f l))
  | Oprim ({prim = "set"; annots; args = t::_})              -> mk_type ?annotation:(fa annots) (Tset (f t))
  | Oprim ({prim = "signature"; annots; _})                  -> mk_type ?annotation:(fa annots) Tsignature
  | Oprim ({prim = "string"; annots; _})                     -> mk_type ?annotation:(fa annots) Tstring
  | Oprim ({prim = "timestamp"; annots; _})                  -> mk_type ?annotation:(fa annots) Ttimestamp
  | Oprim ({prim = "unit"; annots; _})                       -> mk_type ?annotation:(fa annots) Tunit
  | Oprim ({prim = "sapling_transaction"; annots; _})        -> mk_type ?annotation:(fa annots) Tsapling_transaction
  | Oprim ({prim = "sapling_state"; annots; _})              -> mk_type ?annotation:(fa annots) Tsapling_state
  | Oprim ({prim = "never"; annots; _})                      -> mk_type ?annotation:(fa annots) Tnever
  | Oprim ({prim = "bls12_381_g1"; annots; _})               -> mk_type ?annotation:(fa annots) Tbls12_381_g1
  | Oprim ({prim = "bls12_381_g2"; annots; _})               -> mk_type ?annotation:(fa annots) Tbls12_381_g2
  | Oprim ({prim = "bls12_381_fr"; annots; _})               -> mk_type ?annotation:(fa annots) Tbls12_381_fr
  | Oprim ({prim = "ticket"; annots; args = t::_})           -> mk_type ?annotation:(fa annots) (Tticket (f t))
  | _ -> Format.eprintf "type unknown %a@." pp_obj_micheline o; assert false


let rec to_data (o : obj_micheline) : data =
  let f = to_data in
  match o with
  | Oint x                                    -> Dint (Big_int.big_int_of_string x)
  | Ostring s                                 -> Dstring s
  | Obytes  s                                 -> Dbytes s
  | Oprim ({prim = "Unit";  _ })              -> Dunit
  | Oprim ({prim = "True";  _ })              -> Dtrue
  | Oprim ({prim = "False"; _ })              -> Dfalse
  | Oprim ({prim = "Pair";  args = a::b::_ }) -> Dpair  (f a, f b)
  | Oprim ({prim = "Left";  args = a::_ })    -> Dleft  (f a)
  | Oprim ({prim = "Right"; args = a::_ })    -> Dright (f a)
  | Oprim ({prim = "Some";  args = a::_ })    -> Dsome  (f a)
  | Oprim ({prim = "None";  _ })              -> Dnone
  | Oarray l                                  -> Dlist (List.map f l)
  | Oprim ({prim = "Elt"; args = a::b::_ })   -> Delt  (f a, f b)
  | _ -> Format.eprintf "data unknown %a@." pp_obj_micheline o; assert false
