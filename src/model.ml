open Location
open Ident

type lident = ident loced

let pp_ident fmt i = Format.fprintf fmt "%s" i
let pp_lident fmt i = Format.fprintf fmt "%s" (unloc i)

type container =
  | Collection
  | Queue
  | Stack
  | Set
  | Subset
  | Partition
[@@deriving show {with_path = false}]

type currency =
  | Tez
  | Mutez
[@@deriving show {with_path = false}]

type vtyp =
  | VTbool
  | VTint
  | VTuint
  | VTdate
  | VTduration
  | VTstring
  | VTaddress
  | VTrole
  | VTcurrency of currency
  | VTkey
[@@deriving show {with_path = false}]

type vset =
  | VSremoved
  | VSadded
  | VSstable
  | VSbefore
  | VSafter
  | VSfixed
[@@deriving show {with_path = false}]

type ptyp =
  | Tasset of lident
  | Tbuiltin of vtyp
  | Tcontainer of ptyp * container
  | Ttuple of ptyp list
[@@deriving show {with_path = false}]

type ltyp =
  | LTprog of ptyp
  | LTvset of vset * ltyp
[@@deriving show {with_path = false}]


type type_ = ptyp (* type of pterm *)
[@@deriving show {with_path = false}]

type ltype_ = ltyp (* type of lterm *)
[@@deriving show {with_path = false}]

(* operators and constants *)
type logical_operator =
  | And
  | Or
  | Imply
  | Equiv
[@@deriving show {with_path = false}]

type comparison_operator =
  | Equal
  | Nequal
  | Gt
  | Ge
  | Lt
  | Le
[@@deriving show {with_path = false}]

type assignment_operator =
  | ValueAssign
  | SimpleAssign
  | PlusAssign
  | MinusAssign
  | MultAssign
  | DivAssign
  | AndAssign
  | OrAssign
[@@deriving show {with_path = false}]

type arithmetic_operator =
  | Plus
  | Minus
  | Mult
  | Div
  | Modulo
[@@deriving show {with_path = false}]

type unary_arithmetic_operator =
  | Uplus
  | Uminus
[@@deriving show {with_path = false}]

type const =
  (* constant *)
  | Cstate
  | Cnow
  | Ctransferred
  | Ccaller
  | Cfail
  | Cbalance
  | Cconditions
  | Cactions
  | Cnone
  | Cany
  (* function *)
  | Cnth
  | Cclear
  | Cselect
  | Cremoveif
  | Csort
  | Ccount
  | Csum
  | Cmax
  | Cmin
  | Cenqueue
  | Cdequeue
  | Cpush
  | Cpop
  | Cadd
  | Caddifnotexist
  | Cremove
  | Cget
  | Ccontains
  | Cupdate
  | Cmem
  (* vset *)
  | Cbefore
  | Cafter
  | Cfixed
  | Cadded
  | Cremoved
[@@deriving show {with_path = false}]

type ('typ, 'node) struct_poly = {
  node : 'node;                   (* kind of object *)
  type_ : 'typ;                   (* type of object *)
  fvs : (ident * 'typ) list;      (* free variable *)
  label : ident option;           (* label (typically for instruction) *)
  loc : Location.t [@opaque];     (* location of object *)
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'qualid) qualid_poly = ('typ, (('id, 'qualid) qualid_node)) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'qualid) qualid_node =
  | Qident of 'id
  | Qdot of 'qualid * 'id
[@@deriving show {with_path = false}]

type ('id, 'typ) qualid_gen = ('id, 'typ, ('id, 'typ) qualid_gen) qualid_poly
[@@deriving show {with_path = false}]

type qualid = (lident, type_) qualid_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'rexpr) rexpr_poly = ('typ, ('id, 'typ, 'rexpr) rexpr_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'typ, 'rexpr) rexpr_node =
  | Rqualid of ('id, 'typ) qualid_gen
  | Ror of 'rexpr * 'rexpr
  | Raddress of 'id
[@@deriving show {with_path = false}]

type ('id, 'typ) rexpr_gen = ('id, 'typ, ('id, 'typ) rexpr_gen) rexpr_poly
[@@deriving show {with_path = false}]

type rexpr = (lident, type_) rexpr_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'sexpr) sexpr_poly = ('typ, ('id, 'sexpr) sexpr_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'sexpr) sexpr_node =
  | Sref of 'id
  | Sor of 'sexpr * 'sexpr
  | Sany
[@@deriving show {with_path = false}]

type ('id, 'typ) sexpr_gen = ('id, 'typ, ('id, 'typ) sexpr_gen) sexpr_poly
[@@deriving show {with_path = false}]

type sexpr = (lident, type_) sexpr_gen
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

(* basic variable *)
type 'typ bval_poly = ('typ, bval_node) struct_poly
[@@deriving show {with_path = false}]

and bval_node =
  | BVint          of Core.big_int
  | BVuint         of Core.big_int
  | BVbool         of bool
  | BVenum         of string
  | BVrational     of Core.big_int * Core.big_int
  | BVdate         of string (* TODO : find a date structure *)
  | BVstring       of string
  | BVcurrency     of currency * Core.big_int
  | BVaddress      of string
  | BVduration     of string
[@@deriving show {with_path = false}]

type 'typ bval_gen = 'typ bval_poly
[@@deriving show {with_path = false}]

type bval = type_ bval_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'term) decl_gen = {
  name    : 'id;
  typ     : 'typ option;
  default : 'term option;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ) signature = {
  name : 'id;
  args: 'typ list;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type quantifier =
  | Forall
  | Exists
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'pattern) pattern_poly = ('typ, ('id, 'typ, 'pattern) pattern_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'typ, 'pattern) pattern_node =
  | Mwild
  | Mvar of 'id
  | Mapp of ('id, 'typ) qualid_gen * 'pattern list
  | Mrec of (('id, 'typ) qualid_gen * 'pattern) list
  | Mtuple of 'pattern list
  | Mas of 'pattern * 'id * bool
  | Mor of 'pattern * 'pattern
  | Mscope of ('id, 'typ) qualid_gen * 'pattern
  | Mparen of 'pattern
  | Mghost of 'pattern
[@@deriving show {with_path = false}]

type ('id, 'typ) pattern_gen = ('id, 'typ, ('id, 'typ) pattern_gen) pattern_poly
[@@deriving show {with_path = false}]

type pattern = (lident, type_) pattern_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'term) lterm_poly = (ltype_, ('id, 'term) lterm_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'term) lterm_node =
  | Lquantifer of quantifier * 'id * ltype_ * 'term
  | Limply of 'term * 'term
  | Lletin of 'id * 'term * ltype_ * 'term
  | Lnot of 'term
  | Lapp of 'term * 'term list
  | Llogical of logical_operator * 'term * 'term
  | Lcomp of comparison_operator * 'term * 'term
  | Larith of arithmetic_operator * 'term * 'term
  | Luarith of unary_arithmetic_operator * 'term
  | Lvar of 'id
  | Larray of 'term list
  | Llit of ltype_ bval_gen
  | Ldot of 'term * 'id
  | Lconst of const
  | Ltuple of 'term list
[@@deriving show {with_path = false}]

type ('id) lterm_gen = ('id, ('id) lterm_gen) lterm_poly
[@@deriving show {with_path = false}]

type lterm = lident lterm_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'term) pterm_poly = ('typ, ('id, 'typ, 'term) pterm_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'typ, 'term) pterm_node  =
  | Papp of ('term * ('term) list)
  | Pmethod of ('term * 'id * ('term) list)
  | Plambda of 'id * 'typ option * bool * 'term
  | Plogical of logical_operator * 'term * 'term
  | Pnot of 'term
  | Pcomp of comparison_operator * 'term * 'term
  | Parith of arithmetic_operator * 'term * 'term
  | Puarith of unary_arithmetic_operator * 'term
  | Peffect of (assignment_operator * 'id * 'term) list
  | Precord of (('id, 'typ) qualid_gen * 'term) list
  | Pletin of 'id * 'term * 'typ option * 'term
  | Pvar of 'id
  | Parray of 'term list
  | Plit of 'typ bval_gen
  | Pdot of 'term * 'id
  | Pconst of const
  | Ptuple of 'term list
[@@deriving show {with_path = false}]

type ('id, 'typ) pterm_gen = ('id, 'typ, ('id, 'typ) pterm_gen) pterm_poly
[@@deriving show {with_path = false}]

type pterm = (lident, type_, pterm) pterm_poly
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'term, 'instr) instruction_poly = ('typ, ('id, 'typ, 'term, 'instr) instruction_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'typ, 'term, 'instr) instruction_node =
  | Iif of ('term * 'instr * 'instr option)                       (* condition * then_ * else_ *)
  | Ifor of ('id * 'term * 'instr)                                (* id * collection * body *)
  | Iseq of ('instr * 'instr)                                     (* lhs ; rhs*)
  | Imatchwith of 'term * (('id, 'typ) pattern_gen * 'instr) list (* match 'term with ('pattern * 'instr) list *)
  | Iassign of (assignment_operator * 'term * 'term)              (* $2 assignment_operator $3 *)
  | Irequire of (bool * 'term)                                    (* $1 ? require : failif *)
  | Itransfer of ('term * bool * ('id, 'typ) qualid_gen option)   (* value * back * dest *)
  | Ibreak
  | Iassert of 'term
  | Isimple of 'term
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) instruction_gen = ('id, 'typ, 'term, ('id, 'typ, 'term) instruction_gen) instruction_poly
[@@deriving show {with_path = false}]

and instruction = (lident, type_, pterm, instruction) instruction_poly


(* -------------------------------------------------------------------- *)

type ('id, 'term) label_term = {
  label : 'id option;
  term : 'term;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) variable = {
  decl         : ('id, 'typ, 'term) decl_gen; (* TODO *)
  constant     : bool;
  from         : ('id, 'typ) qualid_gen option;
  to_          : ('id, 'typ) qualid_gen option;
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id predicate = {
  name : 'id;
  args : ('id * ('id lterm_gen)) list;
  body : 'id lterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ) definition = {
  name : 'id;
  typ  : 'typ;
  var  : 'id;
  body : 'id lterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) verification = {
  predicates  : 'id predicate list;
  definitions : ('id, 'typ) definition list;
  axioms      : ('id, 'id lterm_gen) label_term list;
  theorems    : ('id, 'id lterm_gen) label_term list;
  variables   : ('id, 'typ, 'term) variable list;
  invariants  : ('id * ('id, 'id lterm_gen) label_term list) list;
  effect      : 'term option;
  specs       : ('id, 'id lterm_gen) label_term list;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) function_struct = {
  name         : 'id;
  args         : (('id, 'typ, 'typ bval_gen) decl_gen) list;
  body         : ('id, 'typ, 'term) instruction_gen;
  side         : bool; (* true if function contains a failwith call *)
  return       : 'typ;
  fvs          : (ident * 'typ) list [@opaque];
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type function_ = (lident, type_, pterm) function_struct
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term, 'instr) transition = {
  from : ('id, 'typ) sexpr_gen;
  on   : ('id * 'id) option;
  trs  : ('id * 'term option * 'instr option) list; (* to * condition * action*)
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term, 'instr) transaction_struct = {
  name            : 'id;
  args            : (('id, 'typ, 'typ bval_gen) decl_gen) list;
  calledby        : ('id, 'typ) rexpr_gen option;
  accept_transfer : bool;
  require         : ('id, 'term) label_term list option;
  transition      : ('id, 'typ, 'term, 'instr) transition option;
  verification    : ('id, 'typ, 'term) verification option;
  functions       : ('id, 'typ, 'term) function_struct list;
  effect          : ('id, 'typ, 'term) instruction_gen option;
  side            : bool;
  loc             : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type transaction = (lident, type_, pterm, instruction) transaction_struct
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) enum_item_struct = {
  name : 'id;
  initial : bool;
  verification : ('id, 'typ, 'term) verification option;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) enum_struct = {
  name : 'id; (* "_state" if it's coming from Dstates constructor *)
  items : ('id, 'typ, 'term) enum_item_struct list;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type enum = (lident, type_, pterm) enum_struct
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) asset_struct = {
  name    : 'id;
  fields  : ('id, 'typ, 'term) decl_gen list;
  key     : 'id option;
  sort    : 'id list;
  state   : 'id option;
  role    : bool;
  init    : 'term option;
  specs   : ('id, 'id lterm_gen) label_term list;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type asset = (lident, type_, pterm) asset_struct

type ('id, 'typ, 'term) contract = {
  name       : 'id;
  signatures : ('id, 'typ) signature list;
  init       : 'term option;
  loc        : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term, 'instr)  model_struct = {
  name          : 'id;
  variables     : ('id, 'typ, 'term) variable list;
  assets        : ('id, 'typ, 'term) asset_struct list;
  functions     : ('id, 'typ, 'term) function_struct list;
  transactions  : ('id, 'typ, 'term, 'instr) transaction_struct list;
  enums         : ('id, 'typ, 'term) enum_struct list;
  contracts     : ('id, 'typ, 'term) contract list;
  verifications : ('id, 'typ, 'term) verification list;
  loc           : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

and model = (lident, type_, pterm, instruction) model_struct




(* mk functions *)

let mk_struct_poly node typ = {
  node            = node;
  type_           = typ;
  fvs             = [];
  label           = None;
  loc             = Location.dummy;
}

let mk_label_term term = {
  label           = None;
  term            = term;
  loc             = Location.dummy;
}

let mk_variable decl = {
  decl            = decl;
  constant        = false;
  from            = None;
  to_             = None;
  loc             = Location.dummy;
}

let mk_predicate id body = {
  name            = id;
  args            = [];
  body            = body;
  loc             = Location.dummy;
}

let mk_definition id typ var body = {
  name            = id;
  typ             = typ;
  var             = var;
  body            = body;
  loc             = Location.dummy;
}

let mk_verification = {
  predicates      = [];
  definitions     = [];
  axioms          = [];
  theorems        = [];
  variables       = [];
  invariants      = [];
  effect          = None;
  specs           = [];
  loc             = Location.dummy;
}

let mk_function_struct id body ret = {
  name            = id;
  args            = [];
  body            = body;
  side            = false;
  return          = ret;
  fvs             = [];
  loc             = Location.dummy;
}

let mk_transition from = {
  from            = from;
  on              = None;
  trs             = [];
}

let mk_transaction_struct id = {
  name            = id;
  args            = [];
  calledby        = None;
  accept_transfer = false;
  require         = None;
  transition      = None;
  verification    = None;
  functions       = [];
  effect          = None;
  side            = false;
  loc             = Location.dummy;
}

let mk_enum_item id = {
  name            = id;
  initial         = false;
  verification    = None;
  loc             = Location.dummy;
}

let mk_enum id =   {
  name            = id;
  items           = [];
  loc             = Location.dummy;
}

let mk_asset id   = {
  name            = id;
  fields          = [];
  key             = None;
  sort            = [];
  state           = None;
  role            = false;
  init            = None;
  specs           = [];
  loc             = Location.dummy;
}

let mk_contract id = {
  name          = id;
  signatures    = [];
  init          = None;
  loc           = Location.dummy;
}

let mk_model id = {
  name          = id;
  variables     = [];
  assets        = [];
  functions     = [];
  transactions  = [];
  enums         = [];
  contracts     = [];
  verifications = [];
  loc           = Location.dummy;
}
