open Ident
open Tools

type 'a with_annot = {
  node:       'a;
  annotation: ident option;
}
[@@deriving show {with_path = false}]
type position = {
  line : int;
  col : int;
  char : int;
}
[@@deriving yojson, show {with_path = false}]

type range = {
  start: position;
  end_: position [@key "end"];
}
[@@deriving yojson, show {with_path = false}]

type obj_stack = {
  name: string;
  (* type_: type_ option; *)
}
[@@deriving yojson, show {with_path = false}]

type obj_debug = {
  id: string * string;
  range: range;
  stack: obj_stack;
}
[@@deriving yojson, show {with_path = false}]

type prim = {
  prim: ident;
  args: obj_micheline list;
  annots: ident list;
}
[@@deriving yojson, show {with_path = false}]

and obj_micheline =
  | Oprim of prim
  | Ostring of string
  | Obytes of string
  | Oint of string
  | Oarray of obj_micheline list
  | Ovar of obj_micheline_var
[@@deriving yojson, show {with_path = false}]

and obj_micheline_var =
  | OMVfree   of ident
  | OMVint    of ident * bool
  | OMVstring of ident
  | OMVbytes  of ident
  | OMVif     of ident * obj_micheline * obj_micheline
[@@deriving yojson, show {with_path = false}]

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
  | Tpair      of type_ list
  | Tset       of type_
  | Tsignature
  | Tstring
  | Ttimestamp
  | Tunit
  | Tticket of type_
  | Tsapling_state       of int
  | Tsapling_transaction of int
  | Tbls12_381_fr
  | Tbls12_381_g1
  | Tbls12_381_g2
  | Tnever
  | Tchest
  | Tchest_key
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
  | Dpair              of data list
  | Dleft              of data
  | Dright             of data
  | Dsome              of data
  | Dnone
  | Dlist              of data list
  | Delt               of data * data
  | Dvar               of ident * type_ * bool
  | DIrCode            of ident * instruction
  | Dcode              of code
  | Dlambda_rec        of code
  | Dconstant          of string
[@@deriving show {with_path = false}]

and code_node =
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
  | DUP_N              of int
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
  | NAT
  | INT
  | BYTES
  | ISNAT
  | LE
  | LSL
  | LSR
  | LT
  | MUL
  | NEG
  | NEQ
  | SUB
  | SUB_MUTEZ
  (* Boolean operations *)
  | AND
  | NOT
  | OR
  | XOR
  (* Cryptographic operations *)
  | BLAKE2B
  | CHECK_SIGNATURE
  | HASH_KEY
  | KECCAK
  | PAIRING_CHECK
  | SAPLING_EMPTY_STATE of int
  | SAPLING_VERIFY_UPDATE
  | SHA256
  | SHA512
  | SHA3
  (* Blockchain operations *)
  | ADDRESS
  | AMOUNT
  | BALANCE
  | CHAIN_ID
  | CONTRACT           of type_ * ident option
  | CREATE_CONTRACT    of obj_micheline
  | EMIT               of type_ * ident option
  | IMPLICIT_ACCOUNT
  | LEVEL
  | MIN_BLOCK_TIME
  | NOW
  | SELF               of ident option
  | SELF_ADDRESS
  | SENDER
  | SET_DELEGATE
  | SOURCE
  | TOTAL_VOTING_POWER
  | TRANSFER_TOKENS
  | VOTING_POWER
  (* Operations on data structures *)
  | CAR
  | CDR
  | CONCAT
  | CONS
  | EMPTY_BIG_MAP      of type_ * type_
  | EMPTY_MAP          of type_ * type_
  | EMPTY_SET          of type_
  | GET
  | GET_N              of int
  | GET_AND_UPDATE
  | LEFT               of type_
  | MAP                of code list
  | MEM
  | NEVER
  | NIL                of type_
  | NONE               of type_
  | PACK
  | PAIR
  | PAIR_N             of int
  | RIGHT              of type_
  | SIZE
  | SLICE
  | SOME
  | UNIT
  | UNPACK             of type_
  | UNPAIR
  | UNPAIR_N           of int
  | UPDATE
  | UPDATE_N           of int
  (* Operations on tickets *)
  | JOIN_TICKETS
  | READ_TICKET
  | SPLIT_TICKET
  | TICKET
  (* Other *)
  | CAST               of type_
  | RENAME
  | VIEW               of ident * type_
  | OPEN_CHEST
  (* Macro *)
  | CAR_N              of int
  | CDR_N              of int
  (* Custom *)
  | CUSTOM             of obj_micheline
[@@deriving show {with_path = false}]

and stack_item = {
  stack_item_name: string;
  stack_item_kind: string;
  stack_item_type: type_ option;
}

and stack = stack_item list

and decl_bound = {
  db_kind: string;
  db_name: string;
  db_bound: string;
}

and debug = {
  decl_bound: decl_bound option;
  stack: stack;
  loc: Location.t option;
}

and code = { node : code_node; type_: (type_ list) option ref; debug : debug option }

and z_operator =
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
  | Zlevel
  | Zsapling_empty_state of int
  | Zmin_block_time
[@@deriving show {with_path = false}]

and un_operator =
  | Ucar
  | Ucdr
  | Uleft  of type_
  | Uright of type_
  | Uneg
  | Unat
  | Uint
  | Ubytes
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
  | Ureadticket
  | Ujointickets
  | Upairing_check
  | Uconcat
  | Uaddress
  | UcarN of int
  | UcdrN of int
  | UforcePair
  | Uemit of type_ * ident option
[@@deriving show {with_path = false}]

and bin_operator =
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
  | Bcreateticket
  | Bsplitticket
  | Bsapling_verify_update
  | Bview of ident * type_
  | Bsubmutez
[@@deriving show {with_path = false}]

and ter_operator =
  | Tcheck_signature
  | Tslice
  | Tupdate
  | Ttransfer_tokens
  | Topen_chest
  | Tcreate_contract of obj_micheline
[@@deriving show {with_path = false}]

and g_operator = [`Zop of z_operator | `Uop of un_operator  | `Bop of bin_operator  | `Top of ter_operator ]
[@@deriving show {with_path = false}]

and cmp_operator =
  | Ceq
  | Cne
  | Cgt
  | Cge
  | Clt
  | Cle
[@@deriving show {with_path = false}]

and builtin =
  | Bmin of type_
  | Bmax of type_
  | Bfloor
  | Bceil
  | BlistContains of type_
  | BlistNth of type_
  | BlistHead of type_
  | BlistTail of type_
  | Bratcmp
  | Bratnorm
  | Brataddsub
  | Bratdiv
  | Bratmul
  | Bratuminus
  | Bratabs
  | Brattez
  | Bratdur
  | Bmuteztonat
  | Bsimplify_rational
  | Bis_implicit_address
[@@deriving show {with_path = false}]

and klv =
  | KLVoption of type_
  | KLVmap of (type_ * instruction)
[@@deriving show {with_path = false}]

and access_item = {
  ai_index: int;
  ai_length: int
}
[@@deriving show {with_path = false}]

and access_value = {
  av_ident : ident;
  av_path: access_item list;
  av_source_no_dup: bool;
  av_value_no_dup: bool;
}
[@@deriving show {with_path = false}]

and instruction_node =
  | Iseq         of instruction list
  | IletIn       of ident list * instruction * instruction * bool
  | Ivar_access  of access_value
  | Icall        of ident * instruction list * bool
  | Iassign      of ident * instruction
  | Iassigntuple of ident * int * int * instruction
  | Iif          of instruction * instruction * instruction * type_
  | Iifnone      of instruction * instruction * ident * instruction * type_
  | Iifleft      of instruction * ident * instruction * ident * instruction * type_
  | Iifcons      of instruction * ident * ident * instruction * instruction * type_
  | Iloop        of instruction * instruction
  | Iiter        of ident list * instruction * instruction
  | Iloopleft    of instruction * ident * instruction
  | Ilambda      of type_ * ident * type_ * instruction
  | Ilambda_michelson of type_ * type_ * obj_micheline
  | Izop         of z_operator
  | Iunop        of un_operator * instruction
  | Ibinop       of bin_operator * instruction * instruction
  | Iterop       of ter_operator * instruction * instruction * instruction
  | Iupdate      of ukind * aoperator
  | Iconst       of type_ * data
  | Icompare     of cmp_operator * instruction * instruction
  | Iset         of type_ * instruction list
  | Ilist        of type_ * instruction list
  | Imap         of bool * type_ * type_ * (instruction * instruction) list
  | Irecord      of ritem
  | Irecupdate   of instruction * ruitem
  | Imap_        of instruction * ident * instruction
  | Ifold        of ident * ident option * ident * instruction * instruction * instruction (* var_iterated * var_accu * container * init * code*)
  | Ireverse     of type_ * instruction
  | Imichelson   of instruction list * code * ident list
  | Iwildcard    of type_ * ident
  | Ireplace     of ident * ident * klv * instruction
  | Ireadticket  of instruction
  | Imicheline   of obj_micheline * type_ list * instruction list
  | Irep         of ident * klv
[@@deriving show {with_path = false}]

and instruction = {
  node: instruction_node;
  loc : Location.t option;
}
[@@deriving show {with_path = false}]

and ritem =
  | Rtuple of instruction list
  | Rnodes of ritem list
[@@deriving show {with_path = false}]

and ruitem =
  | RUnodes  of int * (int * ruitem) list
  | RUassign of int * (int * instruction) list
[@@deriving show {with_path = false}]

and aoperator =
  | Aunop  of un_operator
  | Abinop of bin_operator * instruction
  | Aterop of ter_operator * instruction * instruction
[@@deriving show {with_path = false}]

and ukind =
  | Uvar of ident
  | Urec of ident * (int * int) list

type implem =
  | Concrete of (ident * type_) list * instruction
  | Abstract of builtin
[@@deriving show {with_path = false}]

type ctx_func = {
  args: (ident * type_) list;
  stovars: ident list;
}
[@@deriving show {with_path = false}]

type func = {
  name: ident;
  targ: type_;
  tret: type_;
  ctx: ctx_func;
  body: implem;
  loc: Location.t;
}
[@@deriving show {with_path = false}]

type entry = {
  name: ident;
  args: (ident * type_) list;
  eargs: (ident * type_) list;
  body: instruction;
  loc: Location.t;
}
[@@deriving show {with_path = false}]

type ir = {
  name: ident;
  storage_type: type_;
  storage_data : data;
  storage_list: (ident * type_ * data) list;
  with_operations: bool;
  parameter: type_;
  funs: func list;
  views: func list;
  offchain_views: func list;
  entries: entry list;
  parameters: ident list;
}
[@@deriving show {with_path = false}]

type view_struct = {
  id: ident;
  param: type_;
  ret: type_;
  body: code;
}
[@@deriving show {with_path = false}]

type michelson = {
  storage: type_;
  parameter: type_;
  code: code;
  views: view_struct list;
  parameters: ident list;
}
[@@deriving show {with_path = false}]

type micheline = {
  code: obj_micheline list;
  storage: obj_micheline;
  parameters: ident list;
  views: obj_micheline list;
}
[@@deriving show {with_path = false}]

type annotation_struct = {
  name: string;
  description: string;
}
[@@deriving show {with_path = false}]

type michelson_storage_view_struct = {
  code: obj_micheline;
  parameter: obj_micheline option;
  returnType: obj_micheline option;
  annotations: annotation_struct list;
  version: string option;
}
[@@deriving show {with_path = false}]

type rest_api_query_struct = {
  specificationUri: string;
  baseUri: string;
  path: string;
}
[@@deriving show {with_path = false}]

type offchain_view_implem_kind =
  | OVIKMichelsonStorageView of michelson_storage_view_struct
  | OVIKRestApiQuery of rest_api_query_struct
[@@deriving show {with_path = false}]

type offchain_view = {
  name: ident;
  implementations: offchain_view_implem_kind list;
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

(***
   type proposal :

   ```
   type dvar = ident

   and dvkind =
   | DVKvar of ident
   | DVKdup
   | DVKexpr of dexpr option

   and  dexpr =
   | Dvar       of dvar
   | Ddata      of data
   | Dfun       of g_operator * dexpr list
   ```

   with an environment which contains a map
   map (ident -> vkind)

 ***)

type dvar   = [`VLocal of int | `VGlobal of ident]

and  dexpr_node =
  | Dvar       of dvar
  | Depair     of dexpr * dexpr
  | Ddata      of type_ * data
  | Dfun       of g_operator * dexpr list
[@@deriving show {with_path = false}]

and dexpr = { node : dexpr_node; type_ : type_ option }
[@@deriving show {with_path = false}]

let mk_dexpr ?type_ node = {node; type_}

let dvar   dv    = mk_dexpr (Dvar dv)
let depair a  b  = mk_dexpr (Depair (a, b))
let ddata  ty d  = mk_dexpr (Ddata (ty, d))
let dfun   op ld = mk_dexpr (Dfun (op, ld))

type dinstr =
  (* | DIAssign   of dtyvar * dexpr *)
  | DIAssign   of dvar * dexpr
  | DIIf       of dexpr * (dcode * dcode)
  | DIMatch    of dexpr * (ident * dpattern list * dcode) list
  | DIFailwith of dexpr
  | DIWhile    of dexpr * dcode
  | DIIter     of dtyvar * dexpr * dcode
  | DILoop     of dtyvar * dcode
  (* | DICall     of ident * dexpr list *)

(* and dtyvar = dvar * type_ *)
and dtyvar = dvar

and dpattern =
  (* | DVar  of int * type_ *)
  | DVar  of int
  | DPair of dpattern * dpattern

and dcode = dinstr list
[@@deriving show {with_path = false}]

type dprogram = {
  name: ident;
  storage: type_;
  parameter: type_;
  storage_data: data;
  code: dcode;
  procs: (string * (string * type_) list * dcode) list;
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type rstack1 = [dvar | `Paired of rstack1 * rstack1]
[@@deriving show {with_path = false}]

type rstack  = rstack1 list
[@@deriving show {with_path = false}]

let mk_type ?annotation node : type_ =
  { node; annotation }

let mk_ctx_func ?(args = []) ?(stovars = []) _ : ctx_func =
  { args; stovars }

let mk_code ?type_ ?debug node : code =
  { node; type_ = ref type_; debug }

let mk_func name targ tret ctx body loc : func =
  { name; targ; tret; ctx; body; loc }

let mk_entry name args eargs body loc : entry =
  { name; args; eargs; body; loc }

let mk_ir ?(parameters = []) name storage_type storage_data storage_list ?(with_operations = false) parameter funs views offchain_views entries : ir =
  { name; storage_type; storage_data; storage_list; with_operations; parameter; funs; views; offchain_views; entries; parameters }

let mk_view_struct id param ret body : view_struct =
  { id; param; ret; body }

let mk_michelson ?(parameters = []) storage parameter ?(views = []) code : michelson =
  { storage; parameter; code; views; parameters }

let mk_prim ?(args=[]) ?(annots=[]) prim : prim =
  { prim; args; annots }

let mk_micheline ?(parameters = []) ?(views = []) code storage : micheline =
  { code; storage; parameters; views }

let mk_dprogram ?(procs = []) storage parameter storage_data name code =
  { name; storage; parameter; storage_data; code; procs }

(* -------------------------------------------------------------------- *)

let taddress                   = mk_type Taddress
let tbig_map             t1 t2 = mk_type (Tbig_map (t1, t2))
let tbool                      = mk_type Tbool
let tbytes                     = mk_type Tbytes
let tchain_id                  = mk_type Tchain_id
let tcontract            t     = mk_type (Tcontract t)
let tint                       = mk_type Tint
let tkey                       = mk_type Tkey
let tkey_hash                  = mk_type Tkey_hash
let tlambda              t1 t2 = mk_type (Tlambda (t1, t2))
let tlist                t     = mk_type (Tlist t)
let tmap                 t1 t2 = mk_type (Tmap (t1, t2))
let tmutez                     = mk_type Tmutez
let tnat                       = mk_type Tnat
let toperation                 = mk_type Toperation
let toption              t     = mk_type (Toption t)
let tor                  t1 t2 = mk_type (Tor (t1, t2))
let tpair                ts    = mk_type (Tpair ts)
let tset                 t     = mk_type (Tset t)
let tsignature                 = mk_type Tsignature
let tstring                    = mk_type Tstring
let ttimestamp                 = mk_type Ttimestamp
let tunit                      = mk_type Tunit
let tticket              t     = mk_type (Tticket t)
let tsapling_state       n     = mk_type (Tsapling_state       n)
let tsapling_transaction n     = mk_type (Tsapling_transaction n)
let tbls12_381_fr              = mk_type Tbls12_381_fr
let tbls12_381_g1              = mk_type Tbls12_381_g1
let tbls12_381_g2              = mk_type Tbls12_381_g2
let tnever                     = mk_type Tnever
let tchest                     = mk_type Tchest
let tchest_key                 = mk_type Tchest_key

(* -- *)

let trat          = tpair [tint; tnat]

(* -------------------------------------------------------------------- *)

let mk_instruction ?loc node : instruction = {node; loc}

let iseq              ?loc a           = mk_instruction ?loc (Iseq a)
let iletIn            ?loc a b c d     = mk_instruction ?loc (IletIn (a, b, c, d))
let ivar_access       ?loc a           = mk_instruction ?loc (Ivar_access a)
let icall             ?loc a b c       = mk_instruction ?loc (Icall (a, b, c))
let iassign           ?loc a b         = mk_instruction ?loc (Iassign (a, b))
let iassigntuple      ?loc a b c d     = mk_instruction ?loc (Iassigntuple (a, b, c, d))
let iif               ?loc a b c d     = mk_instruction ?loc (Iif (a, b, c, d))
let iifnone           ?loc a b c d e   = mk_instruction ?loc (Iifnone (a, b, c, d, e))
let iifleft           ?loc a b c d e f = mk_instruction ?loc (Iifleft (a, b, c, d, e, f))
let iifcons           ?loc a b c d e f = mk_instruction ?loc (Iifcons (a, b, c, d, e, f))
let iloop             ?loc a b         = mk_instruction ?loc (Iloop (a, b))
let iiter             ?loc a b c       = mk_instruction ?loc (Iiter (a, b, c))
let iloopleft         ?loc a b c       = mk_instruction ?loc (Iloopleft (a, b, c))
let ilambda           ?loc a b c d     = mk_instruction ?loc (Ilambda (a, b, c, d))
let ilambda_michelson ?loc a b c       = mk_instruction ?loc (Ilambda_michelson (a, b, c))
let izop              ?loc a           = mk_instruction ?loc (Izop a)
let iunop             ?loc a b         = mk_instruction ?loc (Iunop (a, b))
let ibinop            ?loc a b c       = mk_instruction ?loc (Ibinop (a, b, c))
let iterop            ?loc a b c d     = mk_instruction ?loc (Iterop (a, b, c, d))
let iupdate           ?loc a b         = mk_instruction ?loc (Iupdate (a, b))
let iconst            ?loc a b         = mk_instruction ?loc (Iconst (a, b))
let icompare          ?loc a b c       = mk_instruction ?loc (Icompare (a, b, c))
let iset              ?loc a b         = mk_instruction ?loc (Iset (a, b))
let ilist             ?loc a b         = mk_instruction ?loc (Ilist (a, b))
let imap              ?loc a b c d     = mk_instruction ?loc (Imap (a, b, c, d))
let irecord           ?loc a           = mk_instruction ?loc (Irecord (a))
let irecupdate        ?loc a b         = mk_instruction ?loc (Irecupdate (a, b))
let imap_             ?loc a b c       = mk_instruction ?loc (Imap_ (a, b, c))
let ifold             ?loc a b c d e f = mk_instruction ?loc (Ifold (a, b, c, d, e, f))
let ireverse          ?loc a b         = mk_instruction ?loc (Ireverse (a, b))
let imichelson        ?loc a b c       = mk_instruction ?loc (Imichelson (a, b, c))
let iwildcard         ?loc a b         = mk_instruction ?loc (Iwildcard (a, b))
let ireplace          ?loc a b c d     = mk_instruction ?loc (Ireplace (a, b, c, d))
let ireadticket       ?loc a           = mk_instruction ?loc (Ireadticket a)
let imicheline        ?loc a b c       = mk_instruction ?loc (Imicheline (a, b, c))
let irep              ?loc a b         = mk_instruction ?loc (Irep (a, b))

(* -- *)

let ivar         ?loc id   = ivar_access ?loc {av_ident = id; av_path = []; av_source_no_dup = false; av_value_no_dup = false}
let itrue        ?loc _    = iconst ?loc tbool Dtrue
let ifalse       ?loc _    = iconst ?loc tbool Dfalse
let iint         ?loc n    = iconst ?loc tint (Dint n)
let inat         ?loc n    = iconst ?loc tnat (Dint n)
let istring      ?loc s    = iconst ?loc tstring (Dstring s)
let imutez       ?loc v    = iconst ?loc tmutez (Dint v)
let isome        ?loc s    = iunop ?loc Usome s
let inone        ?loc t    = izop ?loc (Znone t)
let iunit        ?loc _    = izop ?loc  Zunit
let inil         ?loc t    = izop ?loc (Znil t)
let iemptyset    ?loc t    = izop ?loc (Zemptyset t)
let iemptymap    ?loc k v  = izop ?loc (Zemptymap (k, v))
let iemptybigmap ?loc k v  = izop ?loc (Zemptybigmap (k, v))
let icar         ?loc x    = iunop ?loc Ucar x
let icdr         ?loc x    = iunop ?loc Ucdr x
let ifail        ?loc msg  = iunop ?loc Ufail (istring ?loc msg)
let ifaild       ?loc data = iunop ?loc Ufail data
let iskip        ?loc _    = iseq ?loc []
let ileft        ?loc t x  = iunop ?loc (Uleft t) x
let iright       ?loc t x  = iunop ?loc (Uright t) x
let ieq          ?loc l r  = icompare ?loc Ceq l r
let iadd         ?loc l r  = ibinop ?loc Badd l r
let isub         ?loc l r  = ibinop ?loc Bsub l r
let isub_mutez   ?loc l r  = ibinop ?loc Bsubmutez l r
let imul         ?loc l r  = ibinop ?loc Bmul l r
let iediv        ?loc l r  = ibinop ?loc Bediv l r
let idiv         ?loc l r  = iifnone ?loc (iediv ?loc l r) (ifail ?loc "DIV_BY_ZERO") "_var_ifnone" (icar ?loc (ivar ?loc "_var_ifnone")) tint
let imod         ?loc l r  = iifnone ?loc (iediv ?loc l r) (ifail ?loc "DIV_BY_ZERO") "_var_ifnone" (icdr ?loc (ivar ?loc "_var_ifnone")) tnat
let isrecord     ?loc l    = irecord ?loc (Rtuple l)
let ipair        ?loc x y  = ibinop ?loc Bpair x y
let icarn        ?loc n x  = iunop ?loc (UcarN n) x
let icdrn        ?loc n x  = iunop ?loc (UcdrN n) x

(* -------------------------------------------------------------------- *)

(* Control structures *)
let cseq                   ?debug  a        = mk_code ?debug (SEQ a)
let capply                 ?debug  _        = mk_code ?debug  APPLY
let cexec                  ?debug  _        = mk_code ?debug  EXEC
let cfailwith              ?debug  _        = mk_code ?debug  FAILWITH
let cif                    ?debug (a, b)    = mk_code ?debug (IF (a, b))
let cifcons                ?debug (a, b)    = mk_code ?debug (IF_CONS (a, b))
let cifleft                ?debug (a, b)    = mk_code ?debug (IF_LEFT (a, b))
let cifnone                ?debug (a, b)    = mk_code ?debug (IF_NONE (a, b))
let citer                  ?debug  a        = mk_code ?debug (ITER a)
let clambda                ?debug (a, b, c) = mk_code ?debug (LAMBDA (a, b, c))
let cloop                  ?debug  a        = mk_code ?debug (LOOP a)
let cloop_left             ?debug  a        = mk_code ?debug (LOOP_LEFT a)
(* Stack manipulation *)
let cdig                   ?debug  a        = mk_code ?debug (DIG a)
let cdip                   ?debug (a, b)    = mk_code ?debug (DIP (a, b))
let cdrop                  ?debug  a        = mk_code ?debug (DROP a)
let cdug                   ?debug  a        = mk_code ?debug (DUG a)
let cdup                   ?debug  _        = mk_code ?debug  DUP
let cdup_n                 ?debug  a        = mk_code ?debug (DUP_N a)
let cpush                  ?debug (a, b)    = mk_code ?debug (PUSH (a, b))
let cswap                  ?debug  _        = mk_code ?debug  SWAP
(* Arthmetic operations *)
let cabs                   ?debug  _        = mk_code ?debug  ABS
let cadd                   ?debug  _        = mk_code ?debug  ADD
let ccompare               ?debug  _        = mk_code ?debug  COMPARE
let cediv                  ?debug  _        = mk_code ?debug  EDIV
let ceq                    ?debug  _        = mk_code ?debug  EQ
let cge                    ?debug  _        = mk_code ?debug  GE
let cgt                    ?debug  _        = mk_code ?debug  GT
let cnat                   ?debug  _        = mk_code ?debug  NAT
let cint                   ?debug  _        = mk_code ?debug  INT
let cbytes                 ?debug  _        = mk_code ?debug  BYTES
let cisnat                 ?debug  _        = mk_code ?debug  ISNAT
let cle                    ?debug  _        = mk_code ?debug  LE
let clsl                   ?debug  _        = mk_code ?debug  LSL
let clsr                   ?debug  _        = mk_code ?debug  LSR
let clt                    ?debug  _        = mk_code ?debug  LT
let cmul                   ?debug  _        = mk_code ?debug  MUL
let cneg                   ?debug  _        = mk_code ?debug  NEG
let cneq                   ?debug  _        = mk_code ?debug  NEQ
let csub                   ?debug  _        = mk_code ?debug  SUB
let csub_mutez             ?debug  _        = mk_code ?debug  SUB_MUTEZ
(* Boolean operations *)
let cand                   ?debug  _        = mk_code ?debug  AND
let cnot                   ?debug  _        = mk_code ?debug  NOT
let cor                    ?debug  _        = mk_code ?debug  OR
let cxor                   ?debug  _        = mk_code ?debug  XOR
(* Cryptographic operations *)
let cblake2b               ?debug  _        = mk_code ?debug  BLAKE2B
let ccheck_signature       ?debug  _        = mk_code ?debug  CHECK_SIGNATURE
let chash_key              ?debug  _        = mk_code ?debug  HASH_KEY
let ckeccak                ?debug  _        = mk_code ?debug  KECCAK
let cpairing_check         ?debug  _        = mk_code ?debug  PAIRING_CHECK
let csapling_empty_state   ?debug  a        = mk_code ?debug (SAPLING_EMPTY_STATE a)
let csapling_verify_update ?debug  _        = mk_code ?debug  SAPLING_VERIFY_UPDATE
let csha256                ?debug  _        = mk_code ?debug  SHA256
let csha512                ?debug  _        = mk_code ?debug  SHA512
let csha3                  ?debug  _        = mk_code ?debug  SHA3
(* Blockchain operations *)
let caddress               ?debug  _        = mk_code ?debug  ADDRESS
let camount                ?debug  _        = mk_code ?debug  AMOUNT
let cbalance               ?debug  _        = mk_code ?debug  BALANCE
let cchain_id              ?debug  _        = mk_code ?debug  CHAIN_ID
let ccontract              ?debug (a, b)    = mk_code ?debug (CONTRACT (a, b))
let ccreate_contract       ?debug  c        = mk_code ?debug (CREATE_CONTRACT c)
let cemit                  ?debug (a, b)    = mk_code ?debug (EMIT (a, b))
let cimplicit_account      ?debug  _        = mk_code ?debug  IMPLICIT_ACCOUNT
let clevel                 ?debug  _        = mk_code ?debug  LEVEL
let cmin_block_time        ?debug  _        = mk_code ?debug  MIN_BLOCK_TIME
let cnow                   ?debug  _        = mk_code ?debug  NOW
let cself                  ?debug  a        = mk_code ?debug (SELF a)
let cself_address          ?debug  _        = mk_code ?debug  SELF_ADDRESS
let csender                ?debug  _        = mk_code ?debug  SENDER
let cset_delegate          ?debug  _        = mk_code ?debug  SET_DELEGATE
let csource                ?debug  _        = mk_code ?debug  SOURCE
let ctotal_voting_power    ?debug  _        = mk_code ?debug  TOTAL_VOTING_POWER
let ctransfer_tokens       ?debug  _        = mk_code ?debug  TRANSFER_TOKENS
let cvoting_power          ?debug  _        = mk_code ?debug  VOTING_POWER
(* Operations on data structures *)
let ccar                   ?debug  _        = mk_code ?debug  CAR
let ccdr                   ?debug  _        = mk_code ?debug  CDR
let cconcat                ?debug  _        = mk_code ?debug  CONCAT
let ccons                  ?debug  _        = mk_code ?debug  CONS
let cempty_big_map         ?debug (a, b)    = mk_code ?debug (EMPTY_BIG_MAP (a, b))
let cempty_map             ?debug (a, b)    = mk_code ?debug (EMPTY_MAP (a, b))
let cempty_set             ?debug  a        = mk_code ?debug (EMPTY_SET a)
let cget                   ?debug  _        = mk_code ?debug  GET
let cget_n                 ?debug  n        = mk_code ?debug (GET_N n)
let cget_and_update        ?debug  _        = mk_code ?debug  GET_AND_UPDATE
let cleft                  ?debug  a        = mk_code ?debug (LEFT a)
let cmap                   ?debug  a        = mk_code ?debug (MAP a)
let cmem                   ?debug  _        = mk_code ?debug  MEM
let cnever                 ?debug  _        = mk_code ?debug  NEVER
let cnil                   ?debug  a        = mk_code ?debug (NIL a)
let cnone                  ?debug  a        = mk_code ?debug (NONE a)
let cpack                  ?debug  _        = mk_code ?debug  PACK
let cpair                  ?debug  _        = mk_code ?debug  PAIR
let cpair_n                ?debug  n        = mk_code ?debug (PAIR_N n)
let cright                 ?debug  a        = mk_code ?debug (RIGHT a)
let csize                  ?debug  _        = mk_code ?debug  SIZE
let cslice                 ?debug  _        = mk_code ?debug  SLICE
let csome                  ?debug  _        = mk_code ?debug  SOME
let cunit                  ?debug  _        = mk_code ?debug  UNIT
let cunpair                ?debug  _        = mk_code ?debug  UNPAIR
let cunpair_n              ?debug  n        = mk_code ?debug (UNPAIR_N n)
let cunpack                ?debug  a        = mk_code ?debug (UNPACK a)
let cupdate                ?debug  _        = mk_code ?debug  UPDATE
let cupdate_n              ?debug  n        = mk_code ?debug (UPDATE_N n)
(* Operations on tickets *)
let cjoin_tickets          ?debug  _        = mk_code ?debug  JOIN_TICKETS
let cread_ticket           ?debug  _        = mk_code ?debug  READ_TICKET
let csplit_ticket          ?debug  _        = mk_code ?debug  SPLIT_TICKET
let cticket                ?debug  _        = mk_code ?debug  TICKET
(* Other *)
let ccast                  ?debug  a        = mk_code ?debug (CAST a)
let crename                ?debug  _        = mk_code ?debug  RENAME
let cview                  ?debug (c, t)    = mk_code ?debug (VIEW (c, t))
let copen_chest            ?debug  _        = mk_code ?debug  OPEN_CHEST
let ccreate_contract       ?debug  c        = mk_code ?debug (CREATE_CONTRACT c)
(* Macro *)
let ccarn                  ?debug  k        = mk_code ?debug (CAR_N k)
let ccdrn                  ?debug  k        = mk_code ?debug (CDR_N k)
(* Custom *)
let ccustom                ?debug  m        = mk_code ?debug (CUSTOM m)

(* -- *)

let ctrue        ?debug _ = cpush ?debug (tbool,     Dtrue)
let cfalse       ?debug _ = cpush ?debug (tbool,     Dfalse)
let cpush_int    ?debug n = cpush ?debug (tint,      Dint n)
let cpush_nat    ?debug n = cpush ?debug (tnat,      Dint n)
let cpush_string ?debug s = cpush ?debug (tstring,   Dstring s)
let cfail        ?debug s = cseq  ?debug [cpush_string s; cfailwith ()]
let cskip        ?debug _ = cseq  ?debug []

(* -------------------------------------------------------------------- *)

let cmp_ident = String.equal

let cmp_type (lhs : type_) (rhs : type_) =
  let rec f (lhs : type_) (rhs : type_) =
    match lhs.node, rhs.node with
    | Taddress, Taddress                               -> true
    | Tbig_map (a1, b1), Tbig_map (a2, b2)             -> f a1 a2 && f b1 b2
    | Tbool, Tbool                                     -> true
    | Tbytes, Tbytes                                   -> true
    | Tchain_id, Tchain_id                             -> true
    | Tcontract a1, Tcontract a2                       -> f a1 a2
    | Tint, Tint                                       -> true
    | Tkey, Tkey                                       -> true
    | Tkey_hash, Tkey_hash                             -> true
    | Tlambda (a1, b1), Tlambda (a2, b2)               -> f a1 a2 && f b1 b2
    | Tlist a1, Tlist a2                               -> f a1 a2
    | Tmap (a1, b1), Tmap (a2, b2)                     -> f a1 a2 && f b1 b2
    | Tmutez, Tmutez                                   -> true
    | Tnat, Tnat                                       -> true
    | Toperation, Toperation                           -> true
    | Toption a1, Toption a2                           -> f a1 a2
    | Tor (a1, b1), Tor (a2, b2)                       -> f a1 a2 && f b1 b2
    | Tpair l1, Tpair l2                               -> List.for_all2 f l1 l2
    | Tset a1, Tset a2                                 -> f a1 a2
    | Tsignature, Tsignature                           -> true
    | Tstring, Tstring                                 -> true
    | Ttimestamp, Ttimestamp                           -> true
    | Tunit, Tunit                                     -> true
    | Tticket a1, Tticket a2                           -> f a1 a2
    | Tsapling_state n1, Tsapling_state n2             -> n1 = n2
    | Tsapling_transaction n1, Tsapling_transaction n2 -> n1 = n2
    | Tbls12_381_g1, Tbls12_381_g1                     -> true
    | Tbls12_381_g2, Tbls12_381_g2                     -> true
    | Tbls12_381_fr, Tbls12_381_fr                     -> true
    | Tnever, Tnever                                   -> true
    | Tchest, Tchest                                   -> true
    | Tchest_key, Tchest_key                           -> true
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
    | Dpair l1, Dpair l2             -> List.for_all2 f l1 l2
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
  | Unat, Unat                             -> true
  | Uint, Uint                             -> true
  | Ubytes, Ubytes                         -> true
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
  | UcarN n1, UcarN n2                     -> n1 = n2
  | UcdrN n1, UcdrN n2                     -> n1 = n2
  | _ -> false

let cmp_bin_operator lhs rhs =
  match lhs, rhs with
  | Badd, Badd           -> true
  | Bsub, Bsub           -> true
  | Bsubmutez, Bsubmutez -> true
  | Bmul, Bmul           -> true
  | Bediv, Bediv         -> true
  | Blsl, Blsl           -> true
  | Blsr, Blsr           -> true
  | Bor, Bor             -> true
  | Band, Band           -> true
  | Bxor, Bxor           -> true
  | Bcompare, Bcompare   -> true
  | Bget, Bget           -> true
  | Bmem, Bmem           -> true
  | Bconcat, Bconcat     -> true
  | Bcons, Bcons         -> true
  | Bpair, Bpair         -> true
  | Bexec, Bexec         -> true
  | Bapply, Bapply       -> true
  | _ -> false

let cmp_ter_operator lhs rhs =
  match lhs, rhs with
  | Tcheck_signature, Tcheck_signature -> true
  | Tslice, Tslice                     -> true
  | Tupdate, Tupdate                   -> true
  | Ttransfer_tokens, Ttransfer_tokens -> true
  | _ -> false

let cmp_code (lhs : code) (rhs : code) =
  let rec f (lhs : code) (rhs : code) =
    match lhs.node, rhs.node with
    (* Control structures *)
    | SEQ l1, SEQ l2                                 -> List.for_all2 f l1 l2
    | APPLY, APPLY                                   -> true
    | EXEC, EXEC                                     -> true
    | FAILWITH, FAILWITH                             -> true
    | IF (t1, e1), IF (t2, e2)                       -> List.for_all2 f t1 t2 && List.for_all2 f e1 e2
    | IF_CONS (t1, e1), IF_CONS (t2, e2)             -> List.for_all2 f t1 t2 && List.for_all2 f e1 e2
    | IF_LEFT (t1, e1), IF_LEFT (t2, e2)             -> List.for_all2 f t1 t2 && List.for_all2 f e1 e2
    | IF_NONE (t1, e1), IF_NONE (t2, e2)             -> List.for_all2 f t1 t2 && List.for_all2 f e1 e2
    | ITER l1, ITER l2                               -> List.for_all2 f l1 l2
    | LAMBDA (a1, r1, b1), LAMBDA (a2, r2, b2)       -> cmp_type a1 a2 && cmp_type r1 r2 && List.for_all2 f b1 b2
    | LOOP l1, LOOP l2                               -> List.for_all2 f l1 l2
    | LOOP_LEFT l1, LOOP_LEFT l2                     -> List.for_all2 f l1 l2
    (* Stack manipulation *)
    | DIG n1, DIG n2                                 -> n1 = n2
    | DIP (n1, l1), DIP (n2, l2)                     -> n1 = n2 && List.for_all2 f l1 l2
    | DROP n1, DROP n2                               -> n1 = n2
    | DUG n1, DUG n2                                 -> n1 = n2
    | DUP, DUP                                       -> true
    | DUP_N n1, DUP_N n2                             -> n1 = n2
    | PUSH (t1, d1), PUSH (t2, d2)                   -> cmp_type t1 t2 && cmp_data d1 d2
    | SWAP, SWAP                                     -> true
    (* Arthmetic operations *)
    | ABS, ABS                                       -> true
    | ADD, ADD                                       -> true
    | COMPARE, COMPARE                               -> true
    | EDIV, EDIV                                     -> true
    | EQ, EQ                                         -> true
    | GE, GE                                         -> true
    | GT, GT                                         -> true
    | NAT, NAT                                       -> true
    | INT, INT                                       -> true
    | BYTES, BYTES                                   -> true
    | ISNAT, ISNAT                                   -> true
    | LE, LE                                         -> true
    | LSL, LSL                                       -> true
    | LSR, LSR                                       -> true
    | LT, LT                                         -> true
    | MUL, MUL                                       -> true
    | NEG, NEG                                       -> true
    | NEQ, NEQ                                       -> true
    | SUB, SUB                                       -> true
    | SUB_MUTEZ, SUB_MUTEZ                           -> true
    (* Boolean operations *)
    | AND, AND                                       -> true
    | NOT, NOT                                       -> true
    | OR, OR                                         -> true
    | XOR, XOR                                       -> true
    (* Cryptographic operations *)
    | BLAKE2B, BLAKE2B                               -> true
    | CHECK_SIGNATURE, CHECK_SIGNATURE               -> true
    | HASH_KEY, HASH_KEY                             -> true
    | KECCAK, KECCAK                                 -> true
    | PAIRING_CHECK, PAIRING_CHECK                   -> true
    | SAPLING_EMPTY_STATE n1, SAPLING_EMPTY_STATE n2 -> n1 = n2
    | SAPLING_VERIFY_UPDATE, SAPLING_VERIFY_UPDATE   -> true
    | SHA256, SHA256                                 -> true
    | SHA512, SHA512                                 -> true
    | SHA3, SHA3                                     -> true
    (* Blockchain operations *)
    | ADDRESS, ADDRESS                               -> true
    | AMOUNT, AMOUNT                                 -> true
    | BALANCE, BALANCE                               -> true
    | CHAIN_ID, CHAIN_ID                             -> true
    | CONTRACT (t1, a1), CONTRACT (t2, a2)           -> cmp_type t1 t2 && Option.cmp cmp_ident a1 a2
    | CREATE_CONTRACT _c1, CREATE_CONTRACT _c2       -> true (* TODO *)
    | IMPLICIT_ACCOUNT, IMPLICIT_ACCOUNT             -> true
    | LEVEL, LEVEL                                   -> true
    | MIN_BLOCK_TIME, MIN_BLOCK_TIME                 -> true
    | NOW, NOW                                       -> true
    | SELF a1, SELF a2                               -> Option.cmp String.equal a1 a2
    | SELF_ADDRESS, SELF_ADDRESS                     -> true
    | SENDER, SENDER                                 -> true
    | SET_DELEGATE, SET_DELEGATE                     -> true
    | SOURCE, SOURCE                                 -> true
    | TOTAL_VOTING_POWER, TOTAL_VOTING_POWER         -> true
    | TRANSFER_TOKENS, TRANSFER_TOKENS               -> true
    | EMIT (t1, a1), EMIT (t2, a2)                   -> cmp_type t1 t2 && Option.cmp cmp_ident a1 a2
    | VOTING_POWER, VOTING_POWER                     -> true
    (* Operations on data structures *)
    | CAR, CAR                                       -> true
    | CDR, CDR                                       -> true
    | CONCAT, CONCAT                                 -> true
    | CONS, CONS                                     -> true
    | EMPTY_BIG_MAP (k1, v1), EMPTY_BIG_MAP (k2, v2) -> cmp_type k1 k2 && cmp_type v1 v2
    | EMPTY_MAP (k1, v1), EMPTY_MAP (k2, v2)         -> cmp_type k1 k2 && cmp_type v1 v2
    | EMPTY_SET t1, EMPTY_SET t2                     -> cmp_type t1 t2
    | GET, GET                                       -> true
    | GET_N n1, GET_N n2                             -> n1 = n2
    | GET_AND_UPDATE, GET_AND_UPDATE                 -> true
    | LEFT t1, LEFT t2                               -> cmp_type t1 t2
    | MAP  l1, MAP  l2                               -> List.for_all2 f l1 l2
    | MEM, MEM                                       -> true
    | NEVER, NEVER                                   -> true
    | NIL t1, NIL t2                                 -> cmp_type t1 t2
    | NONE t1, NONE t2                               -> cmp_type t1 t2
    | PACK, PACK                                     -> true
    | PAIR, PAIR                                     -> true
    | PAIR_N n1, PAIR_N n2                           -> n1 = n2
    | RIGHT t1, RIGHT t2                             -> cmp_type t1 t2
    | SIZE, SIZE                                     -> true
    | SLICE, SLICE                                   -> true
    | SOME, SOME                                     -> true
    | UNIT, UNIT                                     -> true
    | UNPACK t1, UNPACK t2                           -> cmp_type t1 t2
    | UNPAIR, UNPAIR                                 -> true
    | UNPAIR_N n1, UNPAIR_N n2                       -> n1 = n2
    | UPDATE, UPDATE                                 -> true
    | UPDATE_N n1, UPDATE_N n2                       -> n1 = n2
    (* Operations on tickets *)
    | JOIN_TICKETS, JOIN_TICKETS                     -> true
    | READ_TICKET, READ_TICKET                       -> true
    | SPLIT_TICKET, SPLIT_TICKET                     -> true
    | TICKET, TICKET                                 -> true
    (* Other *)
    | CAST t1, CAST t2                               -> cmp_type t1 t2
    | RENAME, RENAME                                 -> true
    | VIEW (i1, t1), VIEW (i2, t2)                   -> cmp_ident i1 i2 && cmp_type t1 t2
    | OPEN_CHEST, OPEN_CHEST                         -> true
    (* Macro *)
    | CAR_N n1, CAR_N n2                             -> n1 = n2
    | CDR_N n1, CDR_N n2                             -> n1 = n2
    (* Custom *)
    | CUSTOM m1, CUSTOM m2                           -> m1 = m2
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
  | BlistHead t1, BlistHead t2         -> cmp_type t1 t2
  | BlistTail t1, BlistTail t2         -> cmp_type t1 t2
  | Bratcmp, Bratcmp                   -> true
  | Bratnorm, Bratnorm                 -> true
  | Brataddsub, Brataddsub             -> true
  | Bratdiv, Bratdiv                   -> true
  | Bratmul, Bratmul                   -> true
  | Bratuminus, Bratuminus             -> true
  | Bratabs, Bratabs                   -> true
  | Brattez, Brattez                   -> true
  | Bratdur, Bratdur                   -> true
  | Bsimplify_rational, Bsimplify_rational -> true
  | _ -> false

let map_type (f : type_ -> type_) (t : type_) : type_ =
  let node =
    match t.node with
    | Taddress               -> Taddress
    | Tbig_map   (k, v)      -> Tbig_map   (f k, f v)
    | Tbool                  -> Tbool
    | Tbytes                 -> Tbytes
    | Tchain_id              -> Tchain_id
    | Tcontract  t           -> Tcontract  (f t)
    | Tint                   -> Tint
    | Tkey                   -> Tkey
    | Tkey_hash              -> Tkey_hash
    | Tlambda    (a, r)      -> Tlambda    (f a, f r)
    | Tlist      t           -> Tlist      (f t)
    | Tmap       (k, v)      -> Tmap       (f k, f v)
    | Tmutez                 -> Tmutez
    | Tnat                   -> Tnat
    | Toperation             -> Toperation
    | Toption    t           -> Toption    (f t)
    | Tor        (l, r)      -> Tor        (f l, f r)
    | Tpair      l           -> Tpair      (List.map f l)
    | Tset       t           -> Tset       (f t)
    | Tsignature             -> Tsignature
    | Tstring                -> Tstring
    | Ttimestamp             -> Ttimestamp
    | Tunit                  -> Tunit
    | Tticket       t        -> Tticket    (f t)
    | Tsapling_state       n -> Tsapling_state n
    | Tsapling_transaction n -> Tsapling_transaction n
    | Tbls12_381_fr          -> Tbls12_381_fr
    | Tbls12_381_g1          -> Tbls12_381_g1
    | Tbls12_381_g2          -> Tbls12_381_g2
    | Tnever                 -> Tnever
    | Tchest                 -> Tchest
    | Tchest_key             -> Tchest_key
  in
  {node = node; annotation = t.annotation}

let normalize_type (ty : type_) : type_ =
  let get_opt_last_pair (tys : type_ list) : type_ list option =
    match List.rev tys with
    | {node = Tpair l}::tl -> Some (List.rev tl @ l)
    | _ -> None
  in
  let is_last_pair x = x |> get_opt_last_pair |> Tools.Option.is_some in
  let rec aux (ty : type_) : type_ =
    match ty.node with
    | Tpair l when is_last_pair l -> aux {ty with node = Tpair (l |> get_opt_last_pair |> Option.get)}
    | _ -> map_type aux ty
  in
  aux ty

(* -------------------------------------------------------------------- *)

let rec cmp_dvar (v1 : dvar) (v2 : dvar) =
  match v1, v2 with
  | `VLocal  l1, `VLocal l2  -> l1 = l2
  | `VGlobal g1, `VGlobal g2 -> String.equal g1 g2
  | _ -> false

and cmp_dlocal l1 l2 =
  Option.cmp cmp_dexpr !l1 !l2

and cmp_dexpr_node e1 e2 =
  match e1, e2 with
  | Dvar  v1, Dvar  v2 -> cmp_dvar v1 v2
  | Ddata (_, d1), Ddata (_, d2) -> cmp_data d1 d2
  | Depair (e1, e'1), Depair (e2, e'2) -> cmp_dexpr e1 e2 && cmp_dexpr e'1 e'2
  | Dfun (op1, l1), Dfun (op2, l2) -> op1 = op2 && List.for_all2 cmp_dexpr l1 l2
  | _ -> false

and cmp_dexpr (e1 : dexpr) (e2 : dexpr) =
  cmp_dexpr_node e1.node e2.node && Option.cmp cmp_type e1.type_ e2.type_

(* -------------------------------------------------------------------- *)

let map_data (f : data -> data) = function
  | Dint n          -> Dint n
  | Dstring v       -> Dstring v
  | Dbytes v        -> Dbytes v
  | Dunit           -> Dunit
  | Dtrue           -> Dtrue
  | Dfalse          -> Dfalse
  | Dpair l         -> Dpair (List.map f l)
  | Dleft v         -> Dleft (f v)
  | Dright v        -> Dright (f v)
  | Dsome v         -> Dsome (f v)
  | Dnone           -> Dnone
  | Dlist l         -> Dlist (List.map f l)
  | Delt (l, r)     -> Delt (f l, f r)
  | Dvar (c, t, b)  -> Dvar (c, t, b)
  | DIrCode (id, c) -> DIrCode (id, c)
  | Dcode c         -> Dcode c
  | Dlambda_rec c   -> Dlambda_rec c
  | Dconstant v     -> Dconstant v

let map_code_gen (fc : code -> code) (fd : data -> data) (ft : type_ -> type_) (x : code) : code =
  let node =
    match x.node with
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
    | DUP_N n                  -> DUP_N n
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
    | NAT                      -> NAT
    | INT                      -> INT
    | BYTES                    -> BYTES
    | ISNAT                    -> ISNAT
    | LE                       -> LE
    | LSL                      -> LSL
    | LSR                      -> LSR
    | LT                       -> LT
    | MUL                      -> MUL
    | NEG                      -> NEG
    | NEQ                      -> NEQ
    | SUB                      -> SUB
    | SUB_MUTEZ                -> SUB_MUTEZ
    (* Boolean operations *)
    | AND                      -> AND
    | NOT                      -> NOT
    | OR                       -> OR
    | XOR                      -> XOR
    (* Cryptographic operations *)
    | BLAKE2B                  -> BLAKE2B
    | CHECK_SIGNATURE          -> CHECK_SIGNATURE
    | HASH_KEY                 -> HASH_KEY
    | KECCAK                   -> KECCAK
    | PAIRING_CHECK            -> PAIRING_CHECK
    | SAPLING_EMPTY_STATE n    -> SAPLING_EMPTY_STATE n
    | SAPLING_VERIFY_UPDATE    -> SAPLING_VERIFY_UPDATE
    | SHA256                   -> SHA256
    | SHA512                   -> SHA512
    | SHA3                     -> SHA3
    (* Blockchain operations *)
    | ADDRESS                  -> ADDRESS
    | AMOUNT                   -> AMOUNT
    | BALANCE                  -> BALANCE
    | CHAIN_ID                 -> CHAIN_ID
    | CONTRACT (t, a)          -> CONTRACT (ft t, a)
    | CREATE_CONTRACT c        -> CREATE_CONTRACT c
    | IMPLICIT_ACCOUNT         -> IMPLICIT_ACCOUNT
    | LEVEL                    -> LEVEL
    | MIN_BLOCK_TIME           -> MIN_BLOCK_TIME
    | NOW                      -> NOW
    | SELF a                   -> SELF a
    | SELF_ADDRESS             -> SELF_ADDRESS
    | SENDER                   -> SENDER
    | SET_DELEGATE             -> SET_DELEGATE
    | SOURCE                   -> SOURCE
    | TOTAL_VOTING_POWER       -> TOTAL_VOTING_POWER
    | TRANSFER_TOKENS          -> TRANSFER_TOKENS
    | EMIT (t, a)              -> EMIT (ft t, a)
    | VOTING_POWER             -> VOTING_POWER
    (* Operations on data structures *)
    | CAR                      -> CAR
    | CDR                      -> CDR
    | CONCAT                   -> CONCAT
    | CONS                     -> CONS
    | EMPTY_BIG_MAP  (k, v)    -> EMPTY_BIG_MAP (ft k, ft v)
    | EMPTY_MAP      (k, v)    -> EMPTY_MAP     (ft k, ft v)
    | EMPTY_SET      t         -> EMPTY_SET     (ft t)
    | GET                      -> GET
    | GET_N n                  -> GET_N n
    | GET_AND_UPDATE           -> GET_AND_UPDATE
    | LEFT t                   -> LEFT (ft t)
    | MAP  l                   -> MAP  (List.map fc l)
    | MEM                      -> MEM
    | NEVER                    -> NEVER
    | NIL t                    -> NIL (ft t)
    | NONE t                   -> NONE (ft t)
    | PACK                     -> PACK
    | PAIR                     -> PAIR
    | PAIR_N n                 -> PAIR_N n
    | RIGHT t                  -> RIGHT (ft t)
    | SIZE                     -> SIZE
    | SLICE                    -> SLICE
    | SOME                     -> SOME
    | UNIT                     -> UNIT
    | UNPACK t                 -> UNPACK (ft t)
    | UNPAIR                   -> UNPAIR
    | UNPAIR_N n               -> UNPAIR_N n
    | UPDATE                   -> UPDATE
    | UPDATE_N n               -> UPDATE_N n
    (* Operations on tickets *)
    | JOIN_TICKETS             -> JOIN_TICKETS
    | READ_TICKET              -> READ_TICKET
    | SPLIT_TICKET             -> SPLIT_TICKET
    | TICKET                   -> TICKET
    (* Other *)
    | CAST t                   -> CAST (ft t)
    | RENAME                   -> RENAME
    | VIEW (c, t)              -> VIEW (c, ft t)
    | OPEN_CHEST               -> OPEN_CHEST
    (* Macro *)
    | CAR_N n                  -> CAR_N n
    | CDR_N n                  -> CDR_N n
    (* Custom *)
    | CUSTOM v                 -> CUSTOM v
  in
  let type_ = Option.map (List.map ft) !(x.type_) in
  let debug = x.debug in
  mk_code ?debug ?type_ node


let map_code (fc : code -> code) = map_code_gen fc id id

let rec map_seq (f : code list -> code list) (code : code) =
  let g x = f (List.map (map_seq f) x) in
  match code.node with
  | SEQ l             -> {code with node = SEQ (g l)}
  | IF_NONE (x, y)    -> {code with node = IF_NONE (g x, g y)}
  | IF_LEFT (x, y)    -> {code with node = IF_LEFT (g x, g y)}
  | IF_CONS (x, y)    -> {code with node = IF_CONS (g x, g y)}
  | MAP x             -> {code with node = MAP (g x)}
  | ITER x            -> {code with node = ITER (g x)}
  | IF (x, y)         -> {code with node = IF (g x, g y)}
  | LOOP x            -> {code with node = LOOP (g x)}
  | LOOP_LEFT x       -> {code with node = LOOP_LEFT (g x)}
  | LAMBDA (a, b, x)  -> {code with node = LAMBDA (a, b, g x)}
  | DIP (n, x)        -> {code with node = DIP (n, g x)}
  | _ -> map_code (map_seq f) code


(* -------------------------------------------------------------------- *)

(* let rec cmp_dexpr (lhs : dexpr) (rhs : dexpr) =
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
   | Diter   (e, is)             -> g (fe accu e) is *)

module Utils : sig

  val get_fun_name  : (type_ -> ident) -> builtin -> ident
  val flat          : code -> code
  val optim         : code -> code
  val replace_macro : code -> code
  val data_to_micheline : data -> obj_micheline
  val type_to_micheline : type_-> obj_micheline
  val code_to_micheline : code -> obj_micheline
  val michelson_to_obj_micheline : michelson -> obj_micheline
  val to_micheline : michelson -> data -> micheline
  val is_storable : type_ -> bool

end = struct

  let get_fun_name ft = function
    | Bmin _t         -> "_min_"(*  ^ (ft t) *)
    | Bmax _t         -> "_max_"(*  ^ (ft t) *)
    | Bfloor          -> "_floor"
    | Bceil           -> "_ceil"
    | BlistContains t -> "_list_contains_" ^ (ft t)
    | BlistNth t      -> "_list_nth_"      ^ (ft t)
    | BlistHead t      -> "_list_head_"    ^ (ft t)
    | BlistTail t      -> "_list_tail_"    ^ (ft t)
    | Bsimplify_rational    -> "_simplify_rational_"
    | Bratcmp         -> "_ratcmp"
    | Bratnorm        -> "_ratnorm"
    | Brataddsub      -> "_rataddsub"
    | Bratmul         -> "_ratmul"
    | Bratdiv         -> "_ratdiv"
    | Bratuminus      -> "_ratuminus"
    | Bratabs         -> "_ratabs"
    | Brattez         -> "_rattez"
    | Bratdur         -> "_ratdur"
    | Bmuteztonat     -> "_muteztonat"
    | Bis_implicit_address -> "_is_implicit_address"

  let flat (c : code) : code =
    let f (l : code list) : code list = List.fold_left (fun (accu : code list) (x : code) -> match x.node with | SEQ l -> accu @ l | _ -> accu @ [x]) [] l in
    map_seq f c

  let handle_failwith (c : code) : code =
    let init = (false, []) in
    let rec aux (c : code) =
      let rec for_seq ((b, accu) : bool * code list) (l : code list) : bool * code list =
        match l with
        | {node = FAILWITH}::_ -> for_seq (true, (mk_code FAILWITH)::accu) []
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

      match c.node with
      | SEQ x             -> g x (fun l -> mk_code (SEQ (l)))
      | IF (x, y)         -> h x y (fun a b -> mk_code (IF (a, b)))
      | IF_NONE (x, y)    -> h x y (fun a b -> mk_code (IF_NONE (a, b)))
      | IF_LEFT (x, y)    -> h x y (fun a b -> mk_code (IF_LEFT (a, b)))
      | IF_CONS (x, y)    -> h x y (fun a b -> mk_code (IF_CONS (a, b)))
      | MAP x             -> g x (fun l -> mk_code (MAP (l)))
      | ITER x            -> g x (fun l -> mk_code (ITER (l)))
      | LOOP x            -> g x (fun l -> mk_code (LOOP (l)))
      | LOOP_LEFT x       -> g x (fun l -> mk_code (LOOP_LEFT (l)))
      | DIP (n, x)        -> g x (fun l -> mk_code (DIP (n, l)))
      | LAMBDA (a, b, c)  -> g c (fun l -> mk_code (LAMBDA (a, b, l)))
      | _                 -> false, c
    in
    aux c |> snd

  let factorize_instrs (c : code) : code =
    let f l =
      let rec aux accu (l : code list) =
        let g accu l =
          match accu with
          | x::tl -> aux tl (x::l)
          | [] -> aux accu l
        in
        match l with
        (* | (DIP (x, y))::(DROP z)::t -> aux accu ((DROP (z - 1))::y::t) *)
        (* | (CAR x)::(CAR y)::t   -> aux accu ((CAR (x + y))::t) *)
        (* | (CDR_N x)::(CDR_N y)::t -> aux accu ((CDR_N (x + y))::t) *)
        | ({node = UNPAIR})::({node = PAIR})::t   -> g accu t
        | ({node = PAIR})::({node = UNPAIR})::t   -> g accu t
        | ({node = UNPAIR_N x})::({node = PAIR_N y})::t when x = y -> g accu t
        | ({node = PAIR_N x})::({node = UNPAIR_N y})::t when x = y -> g accu t
        | ({node = PAIR_N 2})::t                  -> g accu ((mk_code (PAIR))::t)
        | ({node = UNPAIR_N 2})::t                -> g accu ((mk_code (UNPAIR))::t)
        | ({node = UNPAIR})::({node = DROP 1})::t -> g accu ((mk_code (CDR))::t)
        | ({node = DROP x})::({node = DROP y})::t -> g accu ((mk_code (DROP (x + y)))::t)
        | ({node = DUP})::({node = DROP x})::t    -> g accu ((mk_code (DROP (x - 1)))::t)
        | ({node = DUP})::({node = SWAP})::t      -> g accu ((mk_code (DUP))::t)
        | ({node = DROP 0})::t                    -> g accu t
        | ({node = DIG 0})::t                     -> g accu t
        | ({node = DUG 0})::t                     -> g accu t
        | ({node = DIP (_, [])})::t               -> g accu t
        | ({node = DIG 1})::t                     -> g accu ((mk_code (SWAP))::t)
        | ({node = DUG 1})::t                     -> g accu ((mk_code (SWAP))::t)
        | ({node = SWAP})::({node = SWAP})::t     -> g accu t
        | e::t -> aux (e::accu) t
        | [] -> List.rev accu
      in
      aux [] l
    in
    map_seq f c

  let optim c =
    let code = c |> handle_failwith in
    if !Options.opt_g
    then code
    else
      code
      |> factorize_instrs

  let replace_macro (c : code) : code =
    let rec aux (c : code) : code  =
      match c.node with
      | UNPAIR -> mk_code (SEQ [mk_code DUP; mk_code CAR; mk_code (DIP (1, [mk_code CDR]))])
      | _ -> map_code aux c
    in
    aux c

  let rec type_to_micheline (t : type_) : obj_micheline =
    let f = type_to_micheline in
    let prim, args =
      match t.node with
      | Taddress               -> "address", []
      | Tbig_map (k, v)        -> "big_map", [f k; f v]
      | Tbool                  -> "bool", []
      | Tbytes                 -> "bytes", []
      | Tchain_id              -> "chain_id", []
      | Tcontract t            -> "contract", [f t]
      | Tint                   -> "int", []
      | Tkey                   -> "key", []
      | Tkey_hash              -> "key_hash", []
      | Tlambda (a, r)         -> "lambda", [f a; f r]
      | Tlist t                -> "list", [f t]
      | Tmap (k, v)            -> "map", [f k; f v]
      | Tmutez                 -> "mutez", []
      | Tnat                   -> "nat", []
      | Toperation             -> "operation", []
      | Toption t              -> "option", [f t]
      | Tor (l, r)             -> "or", [f l; f r]
      | Tpair l                -> "pair", List.map f l
      | Tset t                 -> "set", [f t]
      | Tsignature             -> "signature", []
      | Tstring                -> "string", []
      | Ttimestamp             -> "timestamp", []
      | Tunit                  -> "unit", []
      | Tticket t              -> "ticket", [f t]
      | Tsapling_transaction n -> Format.asprintf "sapling_transaction", [Oint (string_of_int n)]
      | Tsapling_state n       -> Format.asprintf "sapling_state", [Oint (string_of_int n)]
      | Tbls12_381_g1          -> "bls12_381_g1", []
      | Tbls12_381_g2          -> "bls12_381_g2", []
      | Tbls12_381_fr          -> "bls12_381_fr", []
      | Tnever                 -> "never", []
      | Tchest                 -> "chest", []
      | Tchest_key             -> "chest_key", []
    in
    let args = if List.is_empty args then None else Some args in
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
    | Dpair l      -> Oprim (mk_prim ~args:(List.map f l) "Pair")
    | Dleft v      -> Oprim (mk_prim ~args:[f v] "Left")
    | Dright v     -> Oprim (mk_prim ~args:[f v] "Right")
    | Dsome v      -> Oprim (mk_prim ~args:[f v] "Some")
    | Dnone        -> Oprim (mk_prim "None")
    | Dlist l      -> Oarray (List.map f l)
    | Delt (l, r)  -> Oprim (mk_prim ~args:[f l; f r] "Elt")
    | Dvar (x, t, b)  -> begin
        match t.node with
        | Taddress                -> Ovar (OMVstring x)
        | Tbig_map   (_k, _v)     -> Ovar (OMVfree x)
        | Tbool                   -> Ovar (OMVif (x, Oprim (mk_prim "True"), Oprim (mk_prim "False")))
        | Tbytes                  -> Ovar (OMVbytes x)
        | Tchain_id               -> Ovar (OMVfree x)
        | Tcontract  _t           -> Ovar (OMVfree x)
        | Tint                    -> Ovar (OMVint (x, b))
        | Tkey                    -> Ovar (OMVbytes x)
        | Tkey_hash               -> Ovar (OMVbytes x)
        | Tlambda    (_a, _r)     -> Ovar (OMVfree x)
        | Tlist      _t           -> Ovar (OMVfree x)
        | Tmap       (_k, _v)     -> Ovar (OMVfree x)
        | Tmutez                  -> Ovar (OMVint (x, b))
        | Tnat                    -> Ovar (OMVint (x, b))
        | Toperation              -> Ovar (OMVfree x)
        | Toption    _t           -> Ovar (OMVfree x)
        | Tor        (_l, _r)     -> Ovar (OMVfree x)
        | Tpair      _l           -> Ovar (OMVfree x)
        | Tset       _t           -> Ovar (OMVfree x)
        | Tsignature              -> Ovar (OMVbytes x)
        | Tstring                 -> Ovar (OMVstring x)
        | Ttimestamp              -> Ovar (OMVint (x, b))
        | Tunit                   -> Oprim (mk_prim "Unit")
        | Tticket       _t        -> Ovar (OMVfree x)
        | Tsapling_state       _n -> Ovar (OMVfree x)
        | Tsapling_transaction _n -> Ovar (OMVfree x)
        | Tbls12_381_g1           -> Ovar (OMVfree x)
        | Tbls12_381_g2           -> Ovar (OMVfree x)
        | Tbls12_381_fr           -> Ovar (OMVfree x)
        | Tnever                  -> Ovar (OMVfree x)
        | Tchest                  -> Ovar (OMVfree x)
        | Tchest_key              -> Ovar (OMVfree x)
      end
    | DIrCode (_id, _c) -> Oarray ([])
    | Dcode c           -> code_to_micheline c
    | Dlambda_rec c     -> Oprim (mk_prim ~args:[code_to_micheline c] "Lambda_rec")
    | Dconstant v       -> Oprim (mk_prim ~args:[Ostring v] "constant")

  and code_to_micheline (c : code) : obj_micheline =
    let f = code_to_micheline in
    let ft = type_to_micheline in
    let fd = data_to_micheline in
    let mk ?(args=[]) ?(annots=[]) x = Oprim (mk_prim ~args ~annots x) in
    let mk_int n = Oint (string_of_int n) in
    let mk_string s = Ostring s in
    let mk_array l = Oarray (List.map f l) in
    let fan = function | Some v -> [v] | None -> [] in
    match c.node with
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
    | DUP_N n                  -> mk ~args:[mk_int n] "DUP"
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
    | NAT                      -> mk "NAT"
    | INT                      -> mk "INT"
    | BYTES                    -> mk "BYTES"
    | ISNAT                    -> mk "ISNAT"
    | LE                       -> mk "LE"
    | LSL                      -> mk "LSL"
    | LSR                      -> mk "LSR"
    | LT                       -> mk "LT"
    | MUL                      -> mk "MUL"
    | NEG                      -> mk "NEG"
    | NEQ                      -> mk "NEQ"
    | SUB                      -> mk "SUB"
    | SUB_MUTEZ                -> mk "SUB_MUTEZ"
    (* Boolean operations *)
    | AND                      -> mk "AND"
    | NOT                      -> mk "NOT"
    | OR                       -> mk "OR"
    | XOR                      -> mk "XOR"
    (* Cryptographic operations *)
    | BLAKE2B                  -> mk "BLAKE2B"
    | CHECK_SIGNATURE          -> mk "CHECK_SIGNATURE"
    | HASH_KEY                 -> mk "HASH_KEY"
    | KECCAK                   -> mk "KECCAK"
    | PAIRING_CHECK            -> mk "PAIRING_CHECK"
    | SAPLING_EMPTY_STATE n    -> mk "SAPLING_EMPTY_STATE" ~args:[Oint (string_of_int n)]
    | SAPLING_VERIFY_UPDATE    -> mk "SAPLING_VERIFY_UPDATE"
    | SHA256                   -> mk "SHA256"
    | SHA512                   -> mk "SHA512"
    | SHA3                     -> mk "SHA3"
    (* Blockchain operations *)
    | ADDRESS                  -> mk "ADDRESS"
    | AMOUNT                   -> mk "AMOUNT"
    | BALANCE                  -> mk "BALANCE"
    | CHAIN_ID                 -> mk "CHAIN_ID"
    | CONTRACT (t, a)          -> mk ~args:[ft t] ~annots:(fan a) "CONTRACT"
    | CREATE_CONTRACT c        -> mk ~args:[c] "CREATE_CONTRACT"
    | EMIT (t, a)              -> mk ~args:[ft t] ~annots:(fan a) "EMIT"
    | IMPLICIT_ACCOUNT         -> mk "IMPLICIT_ACCOUNT"
    | LEVEL                    -> mk "LEVEL"
    | MIN_BLOCK_TIME           -> mk "MIN_BLOCK_TIME"
    | NOW                      -> mk "NOW"
    | SELF a                   -> mk ~annots:(fan a) "SELF"
    | SELF_ADDRESS             -> mk "SELF_ADDRESS"
    | SENDER                   -> mk "SENDER"
    | SET_DELEGATE             -> mk "SET_DELEGATE"
    | SOURCE                   -> mk "SOURCE"
    | TOTAL_VOTING_POWER       -> mk "TOTAL_VOTING_POWER"
    | TRANSFER_TOKENS          -> mk "TRANSFER_TOKENS"
    | VOTING_POWER             -> mk "VOTING_POWER"
    (* Operations on data structures *)
    | CAR                      -> mk "CAR"
    | CDR                      -> mk "CDR"
    | CONCAT                   -> mk "CONCAT"
    | CONS                     -> mk "CONS"
    | EMPTY_BIG_MAP  (k, v)    -> mk ~args:[ft k; ft v] "EMPTY_BIG_MAP"
    | EMPTY_MAP      (k, v)    -> mk ~args:[ft k; ft v] "EMPTY_MAP"
    | EMPTY_SET      t         -> mk ~args:[ft t] "EMPTY_SET"
    | GET                      -> mk "GET"
    | GET_N n                  -> mk ~args:[mk_int n] "GET"
    | GET_AND_UPDATE           -> mk "GET_AND_UPDATE"
    | LEFT t                   -> mk ~args:[ft t] "LEFT"
    | MAP  l                   -> mk ~args:[mk_array l] "MAP"
    | MEM                      -> mk "MEM"
    | NEVER                    -> mk "NEVER"
    | NIL t                    -> mk ~args:[ft t] "NIL"
    | NONE t                   -> mk ~args:[ft t] "NONE"
    | PACK                     -> mk "PACK"
    | PAIR                     -> mk "PAIR"
    | PAIR_N n                 -> mk ~args:[mk_int n] "PAIR"
    | RIGHT t                  -> mk ~args:[ft t] "RIGHT"
    | SIZE                     -> mk "SIZE"
    | SLICE                    -> mk "SLICE"
    | SOME                     -> mk "SOME"
    | UNIT                     -> mk "UNIT"
    | UNPACK t                 -> mk ~args:[ft t] "UNPACK"
    | UNPAIR                   -> mk "UNPAIR"
    | UNPAIR_N n               -> mk ~args:[mk_int n] "UNPAIR"
    | UPDATE                   -> mk "UPDATE"
    | UPDATE_N n               -> mk ~args:[mk_int n] "UPDATE"
    (* Operations on tickets *)
    | JOIN_TICKETS             -> mk "JOIN_TICKETS"
    | READ_TICKET              -> mk "READ_TICKET"
    | SPLIT_TICKET             -> mk "SPLIT_TICKET"
    | TICKET                   -> mk "TICKET"
    (* Other *)
    | CAST t                   -> mk ~args:[ft t] "CAST"
    | RENAME                   -> mk "RENAME"
    | VIEW (c, t)              -> mk ~args:[mk_string c; ft t] "VIEW"
    | OPEN_CHEST               -> mk "OPEN_CHEST"
    (* Macro *)
    | CAR_N n                  -> mk ~args:[mk_int n] "CAR"
    | CDR_N n                  -> mk ~args:[mk_int n] "CDR"
    (* Custom *)
    | CUSTOM m                 -> m

  let view_to_micheline (view : view_struct) : obj_micheline =

    let mk ?(args=[]) ?(annots=[]) x : obj_micheline = Oprim (mk_prim ~args ~annots x) in
    let mk_string s = Ostring s in

    let id    = view.id in
    let param = type_to_micheline view.param in
    let ret   = type_to_micheline view.ret in
    let body  = code_to_micheline view.body in

    mk ~args:[
      mk_string id;
      param;
      ret;
      body
    ] "view"

  let michelson_to_obj_micheline (michelson : michelson) :  obj_micheline =
    let storage   = type_to_micheline michelson.storage in
    let parameter = type_to_micheline michelson.parameter in
    let code      = code_to_micheline michelson.code in
    let views     = List.map view_to_micheline michelson.views in
    let f tag x   = Oprim (mk_prim ~args:[x] tag) in
    Oarray ([f "storage" storage; f "parameter" parameter; f "code" code] @ views)

  let to_micheline (m : michelson) (s : data) : micheline =
    let storage   = type_to_micheline m.storage in
    let parameter = type_to_micheline m.parameter in
    let code      = code_to_micheline m.code in
    let views     = List.map view_to_micheline m.views in
    let f tag x   = Oprim (mk_prim ~args:[x] tag) in
    let parameters = m.parameters in
    mk_micheline ~parameters ~views [f "storage" storage; f "parameter" parameter; f "code" code] (data_to_micheline s)

  let rec is_storable (t : type_) =
    match t.node with
    | Taddress                -> true
    | Tbig_map   _            -> true
    | Tbool                   -> true
    | Tbytes                  -> true
    | Tchain_id               -> true
    | Tcontract  _            -> false
    | Tint                    -> true
    | Tkey                    -> true
    | Tkey_hash               -> true
    | Tlambda    _            -> true
    | Tlist      t            -> is_storable t
    | Tmap       (k, v)       -> is_storable k && is_storable v
    | Tmutez                  -> true
    | Tnat                    -> true
    | Toperation              -> false
    | Toption    t            -> is_storable t
    | Tor        (l, r)       -> is_storable l && is_storable r
    | Tpair      l            -> List.for_all is_storable l
    | Tset       t            -> is_storable t
    | Tsignature              -> true
    | Tstring                 -> true
    | Ttimestamp              -> true
    | Tunit                   -> true
    | Tticket       t         -> is_storable t
    | Tsapling_state       _n -> true
    | Tsapling_transaction _n -> true
    | Tbls12_381_g1           -> true
    | Tbls12_381_g2           -> true
    | Tbls12_381_fr           -> true
    | Tnever                  -> true
    | Tchest                  -> true
    | Tchest_key              -> true
end

(***)

let rec to_type (o : obj_micheline) : type_ =
  let fa l = match l with | a::_ -> Some a | [] -> None in
  let f = to_type in
  match o with
  | Oprim ({prim = "address"; annots; _})                                 -> mk_type ?annotation:(fa annots)  Taddress
  | Oprim ({prim = "big_map"; annots; args = k::v::_})                    -> mk_type ?annotation:(fa annots) (Tbig_map (f k, f v))
  | Oprim ({prim = "bool"; annots; _})                                    -> mk_type ?annotation:(fa annots)  Tbool
  | Oprim ({prim = "bytes"; annots; _})                                   -> mk_type ?annotation:(fa annots)  Tbytes
  | Oprim ({prim = "chain_id"; annots; _})                                -> mk_type ?annotation:(fa annots)  Tchain_id
  | Oprim ({prim = "contract"; annots; args = t::_})                      -> mk_type ?annotation:(fa annots) (Tcontract (f t))
  | Oprim ({prim = "int"; annots; _})                                     -> mk_type ?annotation:(fa annots)  Tint
  | Oprim ({prim = "key"; annots; _})                                     -> mk_type ?annotation:(fa annots)  Tkey
  | Oprim ({prim = "key_hash"; annots; _})                                -> mk_type ?annotation:(fa annots)  Tkey_hash
  | Oprim ({prim = "lambda"; annots; args = a::r::_})                     -> mk_type ?annotation:(fa annots) (Tlambda (f a, f r))
  | Oprim ({prim = "list"; annots; args = t::_})                          -> mk_type ?annotation:(fa annots) (Tlist (f t))
  | Oprim ({prim = "map"; annots; args = k::v::_})                        -> mk_type ?annotation:(fa annots) (Tmap (f k, f v))
  | Oprim ({prim = "mutez"; annots; _})                                   -> mk_type ?annotation:(fa annots)  Tmutez
  | Oprim ({prim = "nat"; annots; _})                                     -> mk_type ?annotation:(fa annots)  Tnat
  | Oprim ({prim = "operation"; annots; _})                               -> mk_type ?annotation:(fa annots)  Toperation
  | Oprim ({prim = "option"; annots; args = t::_})                        -> mk_type ?annotation:(fa annots) (Toption (f t))
  | Oprim ({prim = "or"; annots; args = a::b::_})                         -> mk_type ?annotation:(fa annots) (Tor (f a, f b))
  | Oprim ({prim = "pair"; annots; args = l})                             -> mk_type ?annotation:(fa annots) (Tpair (List.map f l))
  | Oprim ({prim = "set"; annots; args = t::_})                           -> mk_type ?annotation:(fa annots) (Tset (f t))
  | Oprim ({prim = "signature"; annots; _})                               -> mk_type ?annotation:(fa annots)  Tsignature
  | Oprim ({prim = "string"; annots; _})                                  -> mk_type ?annotation:(fa annots)  Tstring
  | Oprim ({prim = "timestamp"; annots; _})                               -> mk_type ?annotation:(fa annots)  Ttimestamp
  | Oprim ({prim = "unit"; annots; _})                                    -> mk_type ?annotation:(fa annots)  Tunit
  | Oprim ({prim = "ticket"; annots; args = t::_})                        -> mk_type ?annotation:(fa annots) (Tticket (f t))
  | Oprim ({prim = "sapling_state"; annots; args = (Oint n)::_; _})       -> mk_type ?annotation:(fa annots) (Tsapling_state (int_of_string n))
  | Oprim ({prim = "sapling_transaction"; annots; args = (Oint n)::_; _}) -> mk_type ?annotation:(fa annots) (Tsapling_transaction (int_of_string n))
  | Oprim ({prim = "bls12_381_fr"; annots; _})                            -> mk_type ?annotation:(fa annots)  Tbls12_381_fr
  | Oprim ({prim = "bls12_381_g1"; annots; _})                            -> mk_type ?annotation:(fa annots)  Tbls12_381_g1
  | Oprim ({prim = "bls12_381_g2"; annots; _})                            -> mk_type ?annotation:(fa annots)  Tbls12_381_g2
  | Oprim ({prim = "never"; annots; _})                                   -> mk_type ?annotation:(fa annots)  Tnever
  | Oprim ({prim = "chest"; annots; _})                                   -> mk_type ?annotation:(fa annots)  Tchest
  | Oprim ({prim = "chest_key"; annots; _})                               -> mk_type ?annotation:(fa annots)  Tchest_key
  | _ -> Format.eprintf "type unknown %a@." pp_obj_micheline o; assert false

let param_const_check id =
  String.length id > 8 && String.equal "const_" (String.sub id 0 6) && String.equal "__" (String.sub id ((String.length id) - 2) 2)

let id_to_const_id id = "const_" ^ id ^ "__"

let const_id_to_id id : string = String.sub id 6 ((String.length id) - 8)

let rec to_data (o : obj_micheline) : data =
  let f = to_data in
  match o with
  | Oint x                                    -> Dint (Big_int.big_int_of_string x)
  | Ostring s                                 -> Dstring s
  | Obytes  s                                 -> Dbytes s
  | Oprim ({prim = s;  _ }) when param_const_check s -> Dvar (const_id_to_id s, tunit, false)
  | Oprim ({prim = "Unit";  _ })              -> Dunit
  | Oprim ({prim = "True";  _ })              -> Dtrue
  | Oprim ({prim = "False"; _ })              -> Dfalse
  | Oprim ({prim = "Pair";  args        })    -> Dpair  (List.map f args)
  | Oprim ({prim = "Left";  args = a::_ })    -> Dleft  (f a)
  | Oprim ({prim = "Right"; args = a::_ })    -> Dright (f a)
  | Oprim ({prim = "Some";  args = a::_ })    -> Dsome  (f a)
  | Oprim ({prim = "None";  _ })              -> Dnone
  | Oarray l                                  -> Dlist (List.map f l)
  | Oprim ({prim = "Elt"; args = a::b::_ })   -> Delt  (f a, f b)
  | Oprim ({prim = "Lambda_rec"; args = _a::_ }) -> Format.eprintf "TODO Lambda_rec"; assert false
  | Oprim ({prim = "constant"; args = [Ostring s] }) -> Dconstant s
  | _ -> Format.eprintf "data unknown %a@." pp_obj_micheline o; assert false

type tz_micheline = Micheline_printer.node

let to_tz_micheline (input : obj_micheline) : tz_micheline =

  let emptyloc : Micheline_printer.location = {comment = None} in

  let mkint     v = Micheline.Int (emptyloc, v) in
  let mkstring  v = Micheline.String (emptyloc, v) in
  let mkbytes   v = Micheline.Bytes (emptyloc, v) in
  let mkprim    (p, args, annot) = Micheline.Prim (emptyloc, p, args, annot) in
  let mkseq nodes = Micheline.Seq (emptyloc, nodes) in

  let rec doit (i : obj_micheline) : tz_micheline =
    match i with
    | Oprim p -> mkprim (p.prim, List.map doit p.args, p.annots)
    | Ostring v -> mkstring v
    | Obytes v -> mkbytes (Hex.to_bytes (`Hex v))
    | Oint v -> mkint (Big_int.big_int_of_string v)
    | Oarray v -> mkseq (List.map doit v)
    | Ovar v -> begin
        let f id = mkprim (id, [], []) in
        match v with
        | OMVfree id -> f id
        | OMVint (id, _) -> f id
        | OMVstring id -> f id
        | OMVbytes id -> f id
        | OMVif (id, _, _) -> f id
      end
  in

  doit input

let micheline_to_tz_micheline (input : micheline) : tz_micheline =
  let obj : obj_micheline = Oarray (input.code @ input.views) in
  to_tz_micheline obj

let remove_seq_obj_micheline = function
  | Oarray v -> v
  | v -> [v]

let seek_type_from_annot (annot : string) (ty : type_) : type_ option =

  let rec aux (ty : type_) : type_ option =
    match ty with
    | {annotation = a} when Option.cmp String.equal (Some annot) a -> Some ty
    | {node = Tor (l, r)} -> begin
      match aux l with
      | Some v -> Some v
      | None -> aux r
    end
    | _ -> None
  in

  aux ty
