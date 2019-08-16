open Location
open Ident

type lident = ident loced

let pp_ident fmt i = Format.fprintf fmt "%s" i
let pp_lident fmt i = Format.fprintf fmt "%s" (unloc i)

type container =
  | Collection
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
  | VTrational
  | VTdate
  | VTduration
  | VTstring
  | VTaddress
  | VTrole
  | VTcurrency of currency
  | VTkey
[@@deriving show {with_path = false}]

type trtyp =
  | TRentry
  | TRaction (* add; remove; update *)
  | TRasset
  | TRfield
[@@deriving show {with_path = false}]

type ptyp =
  | Tasset of lident
  | Tenum of lident
  | Tcontract of lident
  | Tbuiltin of vtyp
  | Tcontainer of ptyp * container
  | Ttuple of ptyp list
  | Toption of ptyp
  | Tentry (* entry of external contract *)
  | Ttrace of trtyp
[@@deriving show {with_path = false}]

type type_ = ptyp (* type of pterm *)
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

type operator = [
  | `Logical of logical_operator
  | `Cmp     of comparison_operator
  | `Arith   of arithmetic_operator
  | `Unary   of unary_arithmetic_operator
  | `Assign  of assignment_operator
]
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
  | Canyaction
  (* function *)
  | Cisempty
  | Cget
  | Cadd
  | Caddnofail
  | Cremove
  | Cremovenofail
  | Cremoveif
  | Cupdate
  | Cupdatenofail (* if key exists -> update *)
  | Cclear
  | Ccontains
  | Cnth
  | Creverse
  | Cselect
  | Csort
  | Ccount
  | Csum
  | Cmax
  | Cmin
  | Csubset
  (* vset *)
  | Cbefore
  | Cunmoved
  | Cadded
  | Cremoved
  | Citerated
  | Ctoiterate
  (* predicates *)
  | Cmaybeperformedonlybyrole
  | Cmaybeperformedonlybyaction
  | Cmaybeperformedbyrole
  | Cmaybeperformedbyaction
[@@deriving show {with_path = false}]

type ('typ, 'node) struct_poly = {
  node : 'node;                   (* kind of object *)
  type_ : 'typ option;            (* type of object *)
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

type ('id, 'pattern) pattern_poly = ('id, ('id, 'pattern) pattern_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'pattern) pattern_node =
  | Mwild
  | Mconst of 'id
[@@deriving show {with_path = false}]

type ('id) pattern_gen = ('id, 'id pattern_gen) pattern_poly
[@@deriving show {with_path = false}]

type pattern = lident pattern_gen
[@@deriving show {with_path = false}]


type 'id call_kind =
  | Cid of 'id
  | Cconst of const
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) term_node  =
  | Pquantifer of quantifier * 'id * type_ * 'term
  | Pif of ('term * 'term * 'term)
  | Pmatchwith of 'term * ('id pattern_gen * 'term) list
  | Pcall of ('term option * 'id call_kind * (('id, 'typ, 'term) term_arg) list)
  | Plogical of logical_operator * 'term * 'term
  | Pnot of 'term
  | Pcomp of comparison_operator * 'term * 'term
  | Parith of arithmetic_operator * 'term * 'term
  | Puarith of unary_arithmetic_operator * 'term
  | Precord of 'term list
  | Pletin of 'id * 'term * 'typ option * 'term
  | Pvar of 'id
  | Parray of 'term list
  | Plit of 'typ bval_gen
  | Pdot of 'term * 'id
  | Pconst of const
  | Ptuple of 'term list
  | PsecurityActionRole of action_description * security_role list
  | PsecurityActionAction of action_description * security_action list
[@@deriving show {with_path = false}]

and ('id, 'typ, 'term) term_arg =
  | AExpr   of 'term
  | AFun    of 'id * 'typ * 'term
  | AEffect of ('id * operator * 'term) list
[@@deriving show {with_path = false}]

and action_description =
  | ADAny
  | ADOp  of string * lident

and security_role   = lident
and security_action = lident

(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'term) term_poly = ('typ, ('id, 'typ, 'term) term_node) struct_poly
[@@deriving show {with_path = false}]

type ('id, 'typ) term_gen = ('id, 'typ, ('id, 'typ) term_gen) term_poly
[@@deriving show {with_path = false}]

type pterm = (lident, type_) term_gen
[@@deriving show {with_path = false}]

type pterm_arg = (lident, ptyp, pterm) term_arg
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'term, 'instr) instruction_poly = {
  node : ('id, 'typ, 'term, 'instr) instruction_node;
  label: string option;
  subvars : ident list;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

and ('id, 'typ, 'term, 'instr) instruction_node =
  | Iif of ('term * 'instr * 'instr)                              (* condition * then_ * else_ *)
  | Ifor of ('id * 'term * 'instr)                                (* id * collection * body *)
  | Iletin of ('id * 'term * 'instr)                              (* id * init * body *)
  | Iseq of 'instr list                                           (* lhs ; rhs*)
  | Imatchwith of 'term * ('id pattern_gen * 'instr) list         (* match 'term with ('pattern * 'instr) list *)
  | Iassign of (assignment_operator * 'id * 'term)                (* $2 assignment_operator $3 *)
  | Irequire of (bool * 'term)                                    (* $1 ? require : failif *)
  | Itransfer of ('term * bool * ('id, 'typ) qualid_gen option)   (* value * back * dest *)
  | Ibreak
  | Iassert of 'term
  | Icall of ('term option * 'id call_kind * (('id, 'typ, 'term) term_arg) list)
  | Ireturn of 'term
  | Ilabel of 'id
[@@deriving show {with_path = false}]

and ('id, 'typ, 'term) instruction_gen = ('id, 'typ, 'term, ('id, 'typ, 'term) instruction_gen) instruction_poly

and instruction = (lident, ptyp, pterm, instruction) instruction_poly


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

type ('id, 'typ) predicate = {
  name : 'id;
  args : ('id * (('id, type_) term_gen)) list;
  body : ('id, type_) term_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ) definition = {
  name : 'id;
  typ  : 'typ;
  var  : 'id;
  body : ('id, type_) term_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ) invariant = {
  label: 'id;
  formulas: (('id, type_) term_gen) list;
}
[@@deriving show {with_path = false}]

type ('id, 'typ) specification = {
  name: 'id;
  formula: ('id, type_) term_gen;
  invariants: (('id, 'typ) invariant) list;
}
[@@deriving show {with_path = false}]

type ('id, 'typ) assert_ = {
  name: 'id;
  label: 'id;
  formula: ('id, type_) term_gen;
  invariants: ('id, 'typ) invariant list;
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) verification = {
  predicates  : ('id, 'typ) predicate list;
  definitions : ('id, 'typ) definition list;
  axioms      : ('id, ('id, type_) term_gen) label_term list;
  theorems    : ('id, ('id, type_) term_gen) label_term list;
  variables   : ('id, 'typ, 'term) variable list;
  invariants  : ('id * ('id, ('id, type_) term_gen) label_term list) list;
  effect      : 'term option;
  specs       : ('id, 'typ) specification list;
  asserts     : ('id, 'typ) assert_ list;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) function_struct = {
  name         : 'id;
  args         : (('id, 'typ, 'typ bval_gen) decl_gen) list;
  body         : ('id, 'typ, 'term) instruction_gen;
  side         : bool; (* true if function contains a failwith call *)
  verification : ('id, 'typ, 'term) verification option;
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
  invariants : ('id, ('id, type_) term_gen) label_term list;
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
  key     : 'id option;   (* TODO: option ? *)
  sort    : 'id list;
  state   : 'id option;
  role    : bool;
  init    : 'term option;
  specs   : ('id, ('id, type_) term_gen) label_term list;
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

(* vtyp -> ptyp *)
let vtbool       = Tbuiltin (VTbool      )
let vtint        = Tbuiltin (VTint       )
let vtrational   = Tbuiltin (VTrational  )
let vtdate       = Tbuiltin (VTdate      )
let vtduration   = Tbuiltin (VTduration  )
let vtstring     = Tbuiltin (VTstring    )
let vtaddress    = Tbuiltin (VTaddress   )
let vtrole       = Tbuiltin (VTrole      )
let vtcurrency c = Tbuiltin (VTcurrency c)
let vtkey        = Tbuiltin (VTkey       )

(* mk functions *)


let mk_sp ?label ?(loc = Location.dummy) ?type_ node =
  { node; type_; label; loc; }

let mk_instr ?label ?(loc = Location.dummy) ?(subvars=[]) node =
  { node; label; subvars; loc }

let mk_label_term ?label ?(loc = Location.dummy) term =
  { label; term; loc }

let mk_variable ?(constant = false) ?from ?to_ ?(loc = Location.dummy) decl =
  { decl; constant; from; to_; loc }

let mk_predicate ?(args = []) ?(loc = Location.dummy) name body =
  { name; args; body; loc }

let mk_definition ?(loc = Location.dummy) name typ var body =
  { name; typ; var; body; loc }

let mk_invariant ?(formulas = []) label =
  { label; formulas }

let mk_specification ?(invariants = []) name formula =
  { name; formula; invariants }

let mk_assert ?(invariants = []) name label formula =
  { name; label; formula; invariants }

let mk_verification ?(predicates = []) ?(definitions = []) ?(axioms = []) ?(theorems = []) ?(variables = []) ?(invariants = []) ?effect ?(specs = []) ?(asserts = []) ?(loc = Location.dummy) () =
  { predicates; definitions; axioms; theorems; variables; invariants; effect; specs; asserts; loc}

let mk_function_struct ?(args = []) ?verification ?(side = false) ?(fvs = []) ?(loc = Location.dummy) name body return =
  { name; args; body; verification; side; return; fvs; loc }

let mk_transition ?on ?(trs = []) from =
  { from; on; trs }

let mk_transaction_struct ?(args = []) ?calledby ?(accept_transfer = false) ?require ?transition ?verification ?(functions = []) ?effect ?(side = false) ?(loc = Location.dummy) name =
  { name; args; calledby; accept_transfer; require; transition; verification; functions; effect; side; loc }

let mk_enum_item ?(initial = false) ?(invariants = []) ?(loc = Location.dummy) name : ('id, 'typ, 'term) enum_item_struct =
  { name; initial; invariants; loc }

let mk_enum ?(items = []) ?(loc = Location.dummy) name =
  { name; items; loc }

let mk_decl ?typ ?default ?(loc = Location.dummy) name =
  { name; typ; default; loc }

let mk_asset ?(fields = []) ?key ?(sort = []) ?state ?(role = false) ?init ?(specs = []) ?(loc = Location.dummy) name   =
  { name; fields; key; sort; state; role; init; specs; loc }

let mk_contract ?(signatures = []) ?init ?(loc = Location.dummy) name =
  { name; signatures; init; loc }

let mk_model ?(variables = []) ?(assets = []) ?(functions = []) ?(transactions = []) ?(enums = []) ?(contracts = []) ?(verifications = []) ?(loc = Location.dummy) name =
  { name; variables; assets; functions; transactions; enums; contracts; verifications; loc }

let mk_id type_ id : qualid =
  { type_ = Some type_;
    loc   = loc id;
    node  = Qident id;
    label = None; }

let map_term_node (f : ('id, type_) term_gen -> ('id, type_) term_gen) = function
  | Pquantifer (q, i, t, e) -> Pquantifer (q, i, t, f e)
  | Pif (c, t, e)           -> Pif (f c, f t, f e)
  | Pmatchwith (e, l)       -> Pmatchwith (e, List.map (fun (p, e) -> (p, f e)) l)
  | Pcall (i, e, args)      ->
    Pcall (i, e, List.map (fun (arg : ('id, 'typ, 'term) term_arg) -> match arg with
        | AExpr e -> AExpr (f e)
        | AFun (x, xty, e) -> AFun (x, xty, f e)
        | AEffect l -> AEffect (List.map (fun (id, op, e) -> (id, op, f e)) l)) args)
  | Plogical (op, l, r)     -> Plogical (op, f l, f r)
  | Pnot e                  -> Pnot (f e)
  | Pcomp (c, l, r)         -> Pcomp (c, f l, f r)
  | Parith (op, l, r)       -> Parith (op, f l, f r)
  | Puarith (op, e)         -> Puarith (op, f e)
  | Precord l               -> Precord (List.map f l)
  | Pletin (i, a, t, b)     -> Pletin (i, f a, t, f b)
  | Pvar v                  -> Pvar v
  | Parray l                -> Parray (List.map f l)
  | Plit l                  -> Plit l
  | Pdot (e, i)             -> Pdot (f e, i)
  | Pconst c                -> Pconst c
  | Ptuple l                -> Ptuple (List.map f l)
  | PsecurityActionRole _   as e -> e
  | PsecurityActionAction _ as e -> e

let map_instr_node f = function
  | Iif (c, t, e)       -> Iif (c, f t, f e)
  | Ifor (i, c, b)      -> Ifor (i, c, f b)
  | Iletin (i, c, b)    -> Iletin (i, c, f b)
  | Iseq is             -> Iseq (List.map f is)
  | Imatchwith (a, ps)  -> Imatchwith (a, ps)
  | Iassign (op, l, r)  -> Iassign (op, l, r)
  | Irequire (b, x)     -> Irequire (b, x)
  | Itransfer x         -> Itransfer x
  | Ibreak              -> Ibreak
  | Iassert x           -> Iassert x
  | Icall (x, id, args) -> Icall (x, id, args)
  | Ireturn x           -> Ireturn x
  | Ilabel x            -> Ilabel x

let map_gen_poly g f (i : ('id, 'typ) struct_poly) : ('id, 'typ) struct_poly =
  {
    i with
    node = g f i.node
  }

let map_gen g f i =
  {
    i with
    node = g f i.node
  }

let map_term  f t = map_gen_poly map_term_node  f t
let map_instr f i = map_gen map_instr_node f i

let fold_term (f: 'a -> 't -> 'a) (accu : 'a) (term : ('id, type_) term_gen) =
  match term.node with
  | Pquantifer (_, _, _, e) -> f accu e
  | Pif (c, t, e)           -> f (f (f accu c) t) e
  | Pmatchwith (e, l)       -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Pcall (_, _, args)      -> List.fold_left (fun accu (arg : ('id, 'typ, 'term) term_arg) -> match arg with
      | AExpr e -> f accu e
      | AFun (_, _, e) -> f accu e
      | AEffect l -> List.fold_left (fun accu (_, _, e) -> f accu e) accu l ) accu args
  | Plogical (_, l, r)      -> f (f accu l) r
  | Pnot e                  -> f accu e
  | Pcomp (_, l, r)         -> f (f accu l) r
  | Parith (_, l, r)        -> f (f accu l) r
  | Puarith (_, e)          -> f accu e
  | Precord l               -> List.fold_left f accu l
  | Pletin (_, a, _, b)     -> f (f accu a) b
  | Pvar _                  -> accu
  | Parray l                -> List.fold_left f accu l
  | Plit _                  -> accu
  | Pdot (e, _)             -> f accu e
  | Pconst _                -> accu
  | Ptuple l                -> List.fold_left f accu l
  | PsecurityActionRole _   -> accu
  | PsecurityActionAction _ -> accu

let fold_instr f accu instr =
  match instr.node with
  | Iif (c, t, e)    -> f (f accu t) e
  | Ifor (i, c, b)   -> f accu b
  | Iletin (i, j, b) -> f accu b
  | Iseq is          -> List.fold_left f accu is
  | Imatchwith _     -> accu
  | Iassign _        -> accu
  | Irequire _       -> accu
  | Itransfer _      -> accu
  | Ibreak           -> accu
  | Iassert _        -> accu
  | Icall _          -> accu
  | Ireturn _        -> accu
  | Ilabel _         -> accu

let fold_instr_expr fi fe accu instr =
  match instr.node with
  | Iif (c, t, e)       -> fi (fi (fe accu c) t) e
  | Ifor (i, c, b)      -> fi (fe accu c) b
  | Iletin (i, j, b)    -> fi (fe accu j) b
  | Iseq is             -> List.fold_left fi accu is
  | Imatchwith (a, ps)  -> List.fold_left (fun accu (_, i) -> fi accu i) (fe accu a) ps
  | Iassign (_, _, e)   -> fe accu e
  | Irequire (_, x)     -> fe accu x
  | Itransfer (x, _, _) -> fe accu x
  | Ibreak              -> accu
  | Iassert x           -> fe accu x
  | Icall (x, id, args) -> fi accu instr
  | Ireturn x           -> fe accu x
  | Ilabel x            -> fi accu x

let fold_map_term g f (accu : 'a) (term : ('id, type_) term_gen) : 'term * 'a =
  match term.node with
  | Pquantifer (q, id, t, e) ->
    let ee, ea = f accu e in
    g (Pquantifer (q, id, t, ee)), ea

  | Pif (c, t, e) ->
    let ce, ca = f accu c in
    let ti, ta = f ca t in
    let ei, ea = f ta e in
    g (Pif (ce, ti, ei)), ea

  | Pmatchwith (e, l) ->
    let ee, ea = f accu e in
    let (pse, psa) =
      List.fold_left
        (fun (ps, accu) (p, i) ->
           let pa, accu = f accu i in
           [(p, i)] @ ps, accu) ([], ea) l
    in

    g (Pmatchwith (ee, l)), psa

  | Pcall (a, id, args) ->
    let ((argss, argsa) : 'c list * 'a) =
      List.fold_left
        (fun (pterms, accu) (x : ('id, 'typ, 'term) term_arg) ->
           let p, accu =
             match x with
             | AExpr a -> f accu a |> fun (x, acc) -> (Some (AExpr x), acc)
             | AFun (_, _, e) -> assert false
             | _ -> None, accu in
           let x = match p with | Some a -> a | None -> x in
           pterms @ [x], accu) ([], accu) args
    in
    g (Pcall (a, id, argss)), argsa

  | Plogical (op, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Plogical (op, le, re)), ra

  | Pnot e ->
    let ee, ea = f accu e in
    g (Pnot ee), ea

  | Pcomp (op, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Pcomp (op, le, re)), ra

  | Parith (op, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Parith (op, le, re)), ra

  | Puarith (op, e) ->
    let ee, ea = f accu e in
    g (Puarith (op, ee)), ea

  | Precord l ->
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) l in
    g (Precord lp), la

  | Pletin (id, i, t, o) ->
    let ie, ia = f accu i in
    let oe, oa = f ia o in
    g (Pletin (id, i, t, oe)), oa

  | Pvar id ->
    g (Pvar id), accu

  | Parray l ->
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) l in
    g (Parray lp), la

  | Plit l ->
    g (Plit l), accu

  | Pdot (e, id) ->
    let ee, ea = f accu e in
    g (Pdot (ee, id)), ea

  | Pconst c ->
    g (Pconst c), accu

  | Ptuple l ->
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) l in
    g (Ptuple lp), la

  | PsecurityActionRole _ as e ->
    g e, accu

  | PsecurityActionAction _ as e ->
    g e, accu

let fold_map_instr_term gi ge fi fe (accu : 'a) instr : 'instr * 'a =
  match instr.node with
  | Iif (c, t, e) ->
    let ce, ca = fe accu c in
    let ti, ta = fi ca t in
    let ei, ea = fi ta e in
    gi (Iif (ce, ti, ei)), ea

  | Ifor (i, c, b) ->
    let ce, ca = fe accu c in
    let bi, ba = fi ca b in
    gi (Ifor (i, ce, bi)), ba

  | Iletin (i, j, b) ->
    let je, ja = fe accu j in
    let bi, ba = fi ja b in
    gi (Iletin (i, je, bi)), ba

  | Iseq is ->
    let (isi, isa) : ('instr list * 'a) =
      List.fold_left
        (fun ((instrs, accu) : ('b list * 'c)) x ->
           let bi, accu = fi accu x in
           instrs @ [bi], accu) ([], accu) is
    in
    gi (Iseq isi), isa

  | Imatchwith (a, ps) ->
    let ae, aa = fe accu a in

    let (pse, psa) =
      List.fold_left
        (fun (ps, accu) (p, i) ->
           let pa, accu = fi accu i in
           [(p, i)] @ ps, accu) ([], aa) ps
    in

    gi (Imatchwith (ae, ps)), psa

  | Iassign (op, id, x) ->
    let xe, xa = fe accu x in
    gi (Iassign (op, id, xe)), xa

  | Irequire (b, x) ->
    let xe, xa = fe accu x in
    gi (Irequire (b, xe)), xa

  | Itransfer (x, b, q) ->
    let xe, xa = fe accu x in
    gi (Itransfer (xe, b, q)), xa

  | Ibreak ->
    gi (Ibreak), accu

  | Iassert x ->
    let xe, xa = fe accu x in
    gi (Iassert xe), xa

  | Icall (x, id, args) ->
    let xe, xa =
      match x with
      | Some x -> fe accu x |> (fun (a, b) -> (Some a, b))
      | None -> None, accu in

    let (argss, argsa) =
      List.fold_left
        (fun (pterms, accu) arg ->
           match arg with
           | AExpr x ->
             let p, accu = fe accu x in
             pterms @ [AExpr p], accu
           | _ ->
             pterms, accu
        ) ([], xa) args
    in
    gi (Icall (xe, id, argss)), argsa

  | Ireturn x ->
    let xe, xa = fe accu x in
    gi (Ireturn xe), xa

  | Ilabel x ->
    gi (Ilabel x), accu

(* -------------------------------------------------------------------- *)

module Utils : sig

  val get_asset                 : model -> lident -> asset
  val get_asset_field           : model -> (lident * lident ) -> (lident, ptyp, pterm) decl_gen
  val get_asset_key             : model -> lident -> (lident * vtyp)
  val get_container_asset_field : model -> (lident * lident ) -> container
  val get_named_field_list      : model -> lident -> pterm list -> (lident * pterm) list
  val get_field_list            : model -> lident -> lident list
  val get_enum_values           : model -> lident -> lident option
  val is_variable               : model -> lident -> bool
  val is_asset                  : model -> lident -> bool
  val is_enum_value             : model -> lident -> bool
  val get_var_type              : model -> lident -> type_

end = struct
  open Tools

  exception Anomaly of string

  type error_desc =
    | AssetNotFound of string
    | AssetFieldNotFound of string * string
    | AssetKeyTypeNotFound of string
    | ContainerNotFound of string * string
    | VariableNotFound
  [@@deriving show {with_path = false}]

  let emit_error (desc : error_desc) =
    let str = Format.asprintf "%a@." pp_error_desc desc in
    raise (Anomaly str)

  let get_asset_opt ast asset_name : asset option =
    let id = unloc asset_name in
    List.fold_left (fun accu (x : asset) -> if String.equal id (unloc x.name) then Some x else accu ) None ast.assets

  let get_asset ast asset_name : asset =
    let res = get_asset_opt ast asset_name in
    match res with
    | Some v -> v
    | _ -> emit_error (AssetNotFound (unloc asset_name))

  let get_asset_field ast (asset_name, field_name) =
    let asset = get_asset ast asset_name in
    let res = List.fold_left (fun accu (x : ('id, 'typ, 'term) decl_gen) -> if String.equal (unloc field_name) (unloc x.name) then Some x else accu) None asset.fields in
    match res with
    | Some v -> v
    | _ -> emit_error (AssetFieldNotFound (unloc asset_name, unloc field_name))

  let get_asset_key ast asset_name : (lident * vtyp) =
    let asset = get_asset ast asset_name in
    let key_id = Option.get asset.key in
    let key_field = get_asset_field ast (asset_name, key_id) in
    match key_field.typ with
    | Some (Tbuiltin v) -> (key_id, v)
    | _ -> emit_error (AssetKeyTypeNotFound (unloc asset_name))

  let get_container_asset_field ast (asset_name, field_name) =
    let field = get_asset_field ast (asset_name, field_name) in
    match field.typ with
    | Some Tcontainer (_, c) -> c
    | _ -> emit_error (ContainerNotFound (unloc asset_name, unloc field_name))

  let get_field_list ast asset_name =
    let asset = get_asset ast asset_name in
    List.map (fun (x : (lident, 'a, 'b) decl_gen) -> x.name) asset.fields

  let get_named_field_list ast asset_name list =
    let field_list = get_field_list ast asset_name in
    (* List.iter (fun x -> Format.eprintf "f1: %s@." (unloc x)) field_list;
       List.iter (fun x -> Format.eprintf "f2: %a@." pp_pterm x) list;
       Format.eprintf "lf1: %d@." (List.length field_list);
       Format.eprintf "lf2: %d@." (List.length list); *)
    List.map2 (fun x y -> x, y) field_list list

  let get_enum_opt ast ident =
    List.fold_left (fun accu (x : ('id, 'typ, 'term) enum_struct) ->
        if (Location.unloc x.name) = (Location.unloc ident)
        then Some x
        else accu
      ) None ast.enums

  let get_asset_opt ast ident =
    List.fold_left (fun accu (x : ('id, 'typ, 'term) asset_struct) ->
        if (Location.unloc x.name) = (Location.unloc ident)
        then Some x
        else accu
      ) None ast.assets

  let get_contract_opt ast ident =
    List.fold_left (fun accu (x : ('id, 'typ, 'term) contract) ->
        if (Location.unloc x.name) = (Location.unloc ident)
        then Some x
        else accu
      ) None ast.contracts

  let get_enum_values ast ident =
    List.fold_left (
      fun accu (x : ('id, 'typ, 'term) enum_struct) ->
        if List.fold_left (fun accu (x : ('id, 'typ, 'term) enum_item_struct) -> accu || (Location.unloc x.name) = (Location.unloc ident)) false x.items
        then (Some x.name)
        else accu
    ) None ast.enums

  let get_variable_opt ast ident : ('id, 'typ, 'term) variable option =
    List.fold_left (
      fun accu (x : ('id, 'typ, 'term) variable) ->
        if (String.equal (Location.unloc x.decl.name) (Location.unloc ident))
        then Some x
        else accu
    ) None ast.variables

  let is_variable ast ident =
    match get_variable_opt ast ident with
    | Some _ -> true
    | None   -> false

  let is_asset ast ident =
    match get_asset_opt ast ident with
    | Some _ -> true
    | None   -> false

  let is_enum_value ast ident =
    match get_enum_values ast ident with
    | Some _ -> true
    | None   -> false

  let get_var_type (ast : model) (ident : lident) : type_ =
    let var : type_ option =
      List.fold_left (
        fun accu (x : ('id, type_, 'term) variable) ->
          if (String.equal (Location.unloc x.decl.name) (Location.unloc ident))
          then x.decl.typ
          else accu
      ) None ast.variables in
    match var with
    | Some v -> v
    | None -> emit_error VariableNotFound
end

(* -------------------------------------------------------------------- *)

let create_fake_ast () =
  mk_model (dumloc "fake")
    ~variables:[
      mk_variable (mk_decl (dumloc "str")
                     ~typ:(Tbuiltin VTstring)
                     ~default:(mk_sp (Plit (mk_sp (BVstring "toto")))));
      mk_variable (mk_decl (dumloc "n")
                     ~typ:(Tbuiltin VTint)
                     ~default:(mk_sp (Plit (mk_sp (BVint Big_int.zero_big_int)))))
    ]

let create_test_shallow_ast () =
  mk_model (dumloc "test_shallow")
    ~assets:[
      mk_asset (dumloc "othermile")
        ~key:(dumloc "oid")
        ~fields:[mk_decl (dumloc "oid")
                   ~typ:(Tbuiltin VTint)];
      mk_asset (dumloc "mile")
        ~key:(dumloc "id")
        ~fields:[mk_decl (dumloc "id")
                   ~typ:(Tbuiltin VTint);
                 mk_decl (dumloc "othermiles")
                   ~typ:(Tcontainer (Tasset (dumloc "othermile"), Partition))];

      mk_asset (dumloc "owner")
        ~key:(dumloc "addr")
        ~fields:[mk_decl (dumloc "addr")
                   ~typ:(Tbuiltin VTaddress);
                 mk_decl (dumloc "miles1")
                   ~typ:(Tcontainer (Tasset (dumloc "mile"), Partition));
                 mk_decl (dumloc "miles2")
                   ~typ:(Tcontainer (Tasset (dumloc "othermile"), Partition))]

    ]
    ~transactions:[
      mk_transaction_struct (dumloc "add")
        ~args:[mk_decl (dumloc "ow")
                 ~typ:(Tasset (dumloc "owner"))
              ]
        ~effect:(
          mk_instr (Icall (Some (mk_sp (Pvar (dumloc "owner"))
                                   ~type_:(Tcontainer (Tasset (dumloc "owner"), Collection))
                                ),
                           Cconst Cadd,
                           [AExpr (mk_sp (Pvar (dumloc "ow"))
                                     ~type_:(Tasset (dumloc "owner")))]))
        );

      mk_transaction_struct (dumloc "add2")
        ~args:[mk_decl (dumloc "a")
                 ~typ:(Tbuiltin VTaddress);
               mk_decl (dumloc "l1")
                 ~typ:(Tcontainer (Tasset (dumloc "mile"), Collection));
               mk_decl (dumloc "l2")
                 ~typ:(Tcontainer (Tasset (dumloc "othermile"), Collection))
              ]
        ~effect:(
          mk_instr (Icall (Some (mk_sp (Pvar (dumloc "owner"))
                                   ~type_:(Tcontainer (Tasset (dumloc "owner"), Collection))
                                ),
                           Cconst Cadd,
                           [AExpr (mk_sp (Precord [mk_sp (Pvar (dumloc "a"))
                                                     ~type_:(Tbuiltin VTaddress);
                                                   mk_sp (Pvar (dumloc "l1"))
                                                     ~type_:(Tcontainer (Tasset (dumloc "mile"), Partition));
                                                   mk_sp (Pvar (dumloc "l2"))
                                                     ~type_:(Tcontainer (Tasset (dumloc "othermile"), Partition));
                                                  ])
                                     ~type_:(Tasset (dumloc "owner")))]))
        );

      mk_transaction_struct (dumloc "add3")
        ~args:[mk_decl (dumloc "a")
                 ~typ:(Tbuiltin VTaddress);
               mk_decl (dumloc "l1")
                 ~typ:(Tcontainer (Tasset (dumloc "mile"), Collection));
               mk_decl (dumloc "l2")
                 ~typ:(Tcontainer (Tasset (dumloc "othermile"), Collection))
              ]
        ~effect:(
          mk_instr (Iletin ((dumloc "o"),
                            (mk_sp (Precord [mk_sp (Pvar (dumloc "a"))
                                               ~type_:(Tbuiltin VTaddress);
                                             mk_sp (Pvar (dumloc "l1"))
                                               ~type_:(Tcontainer (Tasset (dumloc "mile"), Partition));
                                             mk_sp (Pvar (dumloc "l2"))
                                               ~type_:(Tcontainer (Tasset (dumloc "othermile"), Partition));
                                            ])
                               ~type_:(Tasset (dumloc "owner"))),
                            mk_instr (Icall (Some (mk_sp (Pvar (dumloc "owner"))
                                                     ~type_:(Tcontainer (Tasset (dumloc "owner"), Collection))
                                                  ),
                                             Cconst Cadd,
                                             [AExpr (mk_sp (Pvar (dumloc "o"))
                                                       ~type_:(Tasset (dumloc "owner")))])))));

      mk_transaction_struct (dumloc "add4")
        ~args:[mk_decl (dumloc "a")
                 ~typ:(Tbuiltin VTaddress);
               mk_decl (dumloc "o1")
                 ~typ:(Tbuiltin VTint);
               mk_decl (dumloc "o2")
                 ~typ:(Tbuiltin VTint);
               mk_decl (dumloc "o3")
                 ~typ:(Tbuiltin VTint);
               mk_decl (dumloc "o4")
                 ~typ:(Tbuiltin VTint)
              ]
        ~effect:(
          mk_instr (Iletin ((dumloc "o"),
                            (mk_sp (Precord [mk_sp (Pvar (dumloc "a"))
                                               ~type_:(Tbuiltin VTaddress);
                                             mk_sp (Parray [(mk_sp (Precord [mk_sp (Pvar (dumloc "o1"))
                                                                               ~type_:(Tbuiltin VTint);
                                                                             mk_sp (Parray [mk_sp (Precord [mk_sp (Pvar (dumloc "o2"))
                                                                                                              ~type_:(Tbuiltin VTint)
                                                                                                           ])
                                                                                              ~type_:(Tasset (dumloc "othermile"));
                                                                                            mk_sp (Precord [mk_sp (Pvar (dumloc "o3"))
                                                                                                              ~type_:(Tbuiltin VTint)
                                                                                                           ])
                                                                                              ~type_:(Tasset (dumloc "othermile"))
                                                                                           ])
                                                                               ~type_:(Tcontainer (Tasset (dumloc "othermile"), Partition))
                                                                            ])
                                                               ~type_:(Tasset (dumloc "mile")))])
                                               ~type_:(Tcontainer (Tasset (dumloc "mile"), Partition));
                                             mk_sp (Parray [(mk_sp (Precord [mk_sp (Pvar (dumloc "o4"))
                                                                               ~type_:(Tbuiltin VTint)
                                                                            ])
                                                               ~type_:(Tasset (dumloc "othermile")))])
                                               ~type_:(Tcontainer (Tasset (dumloc "othermile"), Partition));
                                            ])
                               ~type_:(Tasset (dumloc "owner"))),
                            mk_instr (Icall (Some (mk_sp (Pvar (dumloc "owner"))
                                                     ~type_:(Tcontainer (Tasset (dumloc "owner"), Collection))
                                                  ),
                                             Cconst Cadd,
                                             [AExpr (mk_sp (Pvar (dumloc "o"))
                                                       ~type_:(Tasset (dumloc "owner")))])))));

      mk_transaction_struct (dumloc "add5")
        ~args:[mk_decl (dumloc "a")
                 ~typ:(Tbuiltin VTaddress);
               mk_decl (dumloc "o1")
                 ~typ:(Tbuiltin VTint);
               mk_decl (dumloc "o2")
                 ~typ:(Tbuiltin VTint);
               mk_decl (dumloc "o3")
                 ~typ:(Tbuiltin VTint);
               mk_decl (dumloc "o4")
                 ~typ:(Tbuiltin VTint)
              ]
        ~effect:(
          mk_instr (Icall (Some (mk_sp (Pvar (dumloc "owner"))
                                   ~type_:(Tcontainer (Tasset (dumloc "owner"), Collection))
                                ),
                           Cconst Cadd,
                           [AExpr (mk_sp (Precord [mk_sp (Pvar (dumloc "a"))
                                                     ~type_:(Tbuiltin VTaddress);
                                                   mk_sp (Parray [(mk_sp (Precord [mk_sp (Pvar (dumloc "o1"))
                                                                                     ~type_:(Tbuiltin VTint);
                                                                                   mk_sp (Parray [mk_sp (Precord [mk_sp (Pvar (dumloc "o2"))
                                                                                                                    ~type_:(Tbuiltin VTint)
                                                                                                                 ])
                                                                                                    ~type_:(Tasset (dumloc "othermile"));
                                                                                                  mk_sp (Precord [mk_sp (Pvar (dumloc "o3"))
                                                                                                                    ~type_:(Tbuiltin VTint)
                                                                                                                 ])
                                                                                                    ~type_:(Tasset (dumloc "othermile"))
                                                                                                 ])
                                                                                     ~type_:(Tcontainer (Tasset (dumloc "othermile"), Partition))
                                                                                  ])
                                                                     ~type_:(Tasset (dumloc "mile")))])
                                                     ~type_:(Tcontainer (Tasset (dumloc "mile"), Partition));
                                                   mk_sp (Parray [(mk_sp (Precord [mk_sp (Pvar (dumloc "o4"))
                                                                                     ~type_:(Tbuiltin VTint)
                                                                                  ])
                                                                     ~type_:(Tasset (dumloc "othermile")))])
                                                     ~type_:(Tcontainer (Tasset (dumloc "othermile"), Partition));
                                                  ])
                                     ~type_:(Tasset (dumloc "owner")))])));


      mk_transaction_struct (dumloc "add6")
        ~args:[mk_decl (dumloc "a")
                 ~typ:(Tbuiltin VTaddress);
               mk_decl (dumloc "m")
                 ~typ:(Tasset (dumloc "mile"))
              ]
        ~effect:(
          mk_instr (Icall (None,
                           Cconst Cadd,
                           [
                             AExpr (
                               mk_sp (Pdot (
                                   mk_sp (Pcall (Some (mk_sp ~type_:(Tasset (dumloc "owner")) (Pvar (dumloc "owner"))),
                                                 Cconst Cget,
                                                 [AExpr (mk_sp (Pvar (dumloc "a"))
                                                           ~type_:(Tbuiltin VTaddress))]))
                                     ~type_:(Tasset (dumloc "owner")), dumloc "miles1"))
                                 ~type_:(Tcontainer (Tasset (dumloc "othermile"), Partition)));
                             AExpr (mk_sp (Pvar (dumloc "m"))
                                      ~type_:(Tasset (dumloc "mile")))
                           ])));

      mk_transaction_struct (dumloc "add7")
        ~args:[mk_decl (dumloc "a")
                 ~typ:(Tbuiltin VTaddress);
               mk_decl (dumloc "o1")
                 ~typ:(Tbuiltin VTint);
               mk_decl (dumloc "o2")
                 ~typ:(Tbuiltin VTint);
               mk_decl (dumloc "o3")
                 ~typ:(Tbuiltin VTint)
              ]
        ~effect:(
          mk_instr (Icall (None,
                           Cconst Cadd,
                           [
                             AExpr (
                               mk_sp (Pdot (
                                   mk_sp (Pcall (Some (mk_sp ~type_:(Tasset (dumloc "owner")) (Pvar (dumloc "owner"))),
                                                 Cconst Cget,
                                                 [AExpr (mk_sp (Pvar (dumloc "a"))
                                                           ~type_:(Tbuiltin VTaddress))]))
                                     ~type_:(Tasset (dumloc "owner")), dumloc "miles1"))
                                 ~type_:(Tcontainer (Tasset (dumloc "othermile"), Partition)));
                             AExpr (mk_sp (Precord [mk_sp (Pvar (dumloc "o1"))
                                                      ~type_:(Tbuiltin VTint);
                                                    mk_sp (Parray [mk_sp (Precord [mk_sp (Pvar (dumloc "o2"))
                                                                                     ~type_:(Tbuiltin VTint)
                                                                                  ])
                                                                     ~type_:(Tasset (dumloc "othermile"));
                                                                   mk_sp (Precord [mk_sp (Pvar (dumloc "o3"))
                                                                                     ~type_:(Tbuiltin VTint)
                                                                                  ])
                                                                     ~type_:(Tasset (dumloc "othermile"))
                                                                  ])
                                                      ~type_:(Tcontainer (Tasset (dumloc "othermile"), Partition))
                                                   ])
                                      ~type_:(Tasset (dumloc "mile")))
                           ])))
    ]



let create_miles_with_expiration_ast () =
  mk_model (dumloc "miles_with_expiration")
    ~variables:[
      mk_variable (mk_decl (dumloc "admin")
                     ~typ:(Tbuiltin VTaddress)
                     ~default:(mk_sp (Plit (mk_sp (BVaddress "tz1aazS5ms5cbGkb6FN1wvWmN7yrMTTcr6wB")))
                                 ~type_:(Tbuiltin VTrole)))
    ]
    ~assets:[
      mk_asset (dumloc "mile")
        ~key:(dumloc "id")
        ~sort:[(dumloc "expiration")]
        ~fields:[mk_decl (dumloc "id")
                   ~typ:(Tbuiltin VTstring);
                 mk_decl (dumloc "amount")
                   ~typ:(Tbuiltin VTint);
                 mk_decl (dumloc "expiration")
                   ~typ:(Tbuiltin VTdate); ]
        ~specs:[mk_label_term (mk_sp (Pcomp (Gt,
                                             (mk_sp (Pvar (dumloc "amount"))
                                                ~type_:((Tbuiltin VTint))
                                             ),
                                             (mk_sp (Plit (mk_sp (BVint Big_int.zero_big_int)))
                                                ~type_:((Tbuiltin VTint))
                                             )))
                                 ~type_:((Tbuiltin VTbool)))
                  ~label:(dumloc "m1")];
      mk_asset (dumloc "owner")
        ~key:(dumloc "addr")
        ~fields:[mk_decl (dumloc "addr")
                   ~typ:(Tbuiltin VTrole);
                 mk_decl (dumloc "miles")
                   ~typ:(Tcontainer (Tasset (dumloc "mile"), Partition))]
    ]

    ~transactions:[
      mk_transaction_struct (dumloc "add")
        ~args:[mk_decl (dumloc "ow")
                 ~typ:(Tbuiltin VTrole);
               mk_decl (dumloc "newmile")
                 ~typ:(Tasset (dumloc "mile"))
              ]
        ~calledby:(mk_sp (Rqualid (mk_sp (Qident (dumloc "admin"))
                                     ~type_:(Tbuiltin VTaddress)
                                  )))

        ~require:[mk_label_term (mk_sp (Pcomp (Gt,
                                               (mk_sp (Pdot (mk_sp (Pvar (dumloc "newmile"))
                                                               ~type_:(Tasset (dumloc "mile")),
                                                             (dumloc "amount")))
                                                  ~type_:(Tbuiltin VTint)),
                                               (mk_sp (Plit (mk_sp (BVint Big_int.zero_big_int)))
                                                  ~type_:(Tbuiltin VTint)
                                               )))
                                   ~type_:(Tbuiltin VTbool))
                    ~label:(dumloc "c1")]

        ~effect:(mk_instr (Iif (mk_sp (Pcall (Some (mk_sp ~type_:(Tcontainer (Tasset (dumloc "owner"), Partition)) (Pvar (dumloc "owner"))),
                                              Cconst Ccontains,
                                              [AExpr (mk_sp (Pvar (dumloc "ow"))
                                                        ~type_:(Tbuiltin VTaddress))]))
                                  ~type_:(Tbuiltin VTbool),
                                mk_instr (Icall (Some (mk_sp (Pdot ((mk_sp (Pcall (Some (mk_sp ~type_:(Tasset (dumloc "mile")) (Pvar (dumloc "owner"))),
                                                                                   Cconst Cget,
                                                                                   [AExpr (mk_sp (Pvar (dumloc "ow"))
                                                                                             ~type_:(Tbuiltin VTaddress))]))
                                                                       ~type_:(Tasset (dumloc "owner"))
                                                                    ),
                                                                    (dumloc "miles")))
                                                         ~type_:(Tcontainer (Tasset (dumloc "mile"), Partition))
                                                      ),
                                                 Cconst Cadd,
                                                 [AExpr (mk_sp (Pvar (dumloc "newmile"))
                                                           ~type_:(Tasset (dumloc "mile")))
                                                 ])),
                                mk_instr (Icall (Some (mk_sp (Pvar (dumloc "owner"))
                                                         ~type_:(Tcontainer (Tasset (dumloc "owner"), Collection))
                                                      ),
                                                 Cconst Cadd,
                                                 [AExpr (mk_sp (Precord [mk_sp (Pvar (dumloc "ow"))
                                                                           ~type_:(Tbuiltin VTaddress);
                                                                         mk_sp (Parray [mk_sp (Pvar (dumloc "newmile"))
                                                                                          ~type_:(Tasset (dumloc "mile"))])
                                                                           ~type_:(Tcontainer (Tasset (dumloc "mile"), Partition))
                                                                        ])
                                                           ~type_:(Tasset (dumloc "owner")))])))));

      mk_transaction_struct (dumloc "consume")
        ?verification: (Some (mk_verification ()
                                ~asserts:[
                                  mk_assert
                                    (dumloc "p1")
                                    (dumloc "p1")
                                    (mk_sp (Pcomp (Equal,
                                                   (mk_sp (Pvar (dumloc "remainder"))
                                                      ~type_:((Tbuiltin VTint))),
                                                   (mk_sp (Plit (mk_sp (BVint Big_int.zero_big_int)))
                                                      ~type_:((Tbuiltin VTint)))
                                                  ))
                                       ~type_:((Tbuiltin VTbool)))

                                    ~invariants:[
                                      mk_invariant (dumloc "loop")
                                        ~formulas:[
                                          (mk_sp (Plogical (And,
                                                            (mk_sp (Pcomp (Le,
                                                                           (mk_sp (Plit (mk_sp (BVint Big_int.zero_big_int)))
                                                                              ~type_:((Tbuiltin VTint))),
                                                                           (mk_sp (Pvar (dumloc "remainder"))
                                                                              ~type_:((Tbuiltin VTint)))
                                                                          ))
                                                               ~type_:((Tbuiltin VTbool))),
                                                            (mk_sp (Pcomp (Le,
                                                                           (mk_sp (Pvar (dumloc "remainder"))
                                                                              ~type_:((Tbuiltin VTint))),
                                                                           (mk_sp (Pcall (None,
                                                                                          Cconst Csum,
                                                                                          [
                                                                                            AExpr (mk_sp (Pvar (dumloc "to_iter"))
                                                                                                     ~type_:((Tasset (dumloc "mile"))));
                                                                                            AExpr (mk_sp (Pvar (dumloc "amount"))
                                                                                                     ~type_:((Tbuiltin VTint)))
                                                                                          ]))
                                                                              ~type_:((Tbuiltin VTint)))
                                                                          ))
                                                               ~type_:((Tbuiltin VTbool)))))
                                             ~type_:((Tbuiltin VTbool)))
                                        ]
                                    ]
                                ]
                                ~specs:[
                                  mk_specification (dumloc "p2")
                                    (mk_sp (Pcomp (Equal,
                                                   (mk_sp (Pcall (Some (mk_sp ~type_:((Tasset (dumloc "mile"))) (Pvar (dumloc "mile"))),
                                                                  Cconst Csum,
                                                                  [
                                                                    AExpr (mk_sp (Pvar (dumloc "amount"))
                                                                             ~type_:((Tbuiltin VTint)))
                                                                  ]))
                                                      ~type_:((Tbuiltin VTint))),
                                                   mk_sp (Parith (Minus,
                                                                  (mk_sp (Pcall (None,
                                                                                 Cconst Csum,
                                                                                 [
                                                                                   AExpr (mk_sp (Pcall (None,
                                                                                                        Cconst Cbefore,
                                                                                                        [
                                                                                                          AExpr (mk_sp (Pvar (dumloc "mile"))
                                                                                                                   ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection))))
                                                                                                        ]))
                                                                                            ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection))));
                                                                                   AExpr (mk_sp (Pvar (dumloc "amount"))
                                                                                            ~type_:((Tbuiltin VTint)))
                                                                                 ]))
                                                                     ~type_:((Tbuiltin VTint))),
                                                                  (mk_sp (Pvar (dumloc "quantity"))
                                                                     ~type_:((Tbuiltin VTint)))
                                                                 ))
                                                     ~type_:((Tbuiltin VTint))
                                                  ))
                                       ~type_:((Tbuiltin VTbool)))

                                    ~invariants:[
                                      mk_invariant (dumloc "loop")
                                        ~formulas:[
                                          (mk_sp (Pcall (None,
                                                         Cid (dumloc "subset"),
                                                         [
                                                           AExpr (mk_sp (Pcall (None,
                                                                                Cconst Cremoved,
                                                                                [
                                                                                  AExpr (mk_sp (Pvar (dumloc "miles"))
                                                                                           ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection))))
                                                                                ]))
                                                                    ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection))));
                                                           AExpr (mk_sp (Pcall (None,
                                                                                Cconst Cselect,
                                                                                [
                                                                                  AExpr (mk_sp (Pdot (mk_sp
                                                                                                        (Pvar (dumloc "o"))
                                                                                                        ~type_:((Tasset (dumloc "owner"))),
                                                                                                      dumloc "miles"))
                                                                                           ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection))))
                                                                                  ;
                                                                                  AExpr (mk_sp (Pcomp (Ge,
                                                                                                       mk_sp (Pdot (
                                                                                                           mk_sp (Pvar (dumloc "the"))
                                                                                                             ~type_:((Tasset (dumloc "mile"))),
                                                                                                           dumloc "expiration"
                                                                                                         ))
                                                                                                         ~type_:((Tbuiltin VTdate)),
                                                                                                       mk_sp (Pconst Cnow)
                                                                                                         ~type_:((Tbuiltin VTdate))
                                                                                                      ))
                                                                                           ~type_:((Tbuiltin VTbool))
                                                                                        )
                                                                                ]

                                                                               ))
                                                                    ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection))));
                                                         ]))
                                             ~type_:((Tbuiltin VTbool)))
                                        ]
                                    ];



                                  mk_specification (dumloc "p3")
                                    (mk_sp
                                       (Pquantifer (Forall,
                                                    dumloc "m",
                                                    Tasset (dumloc "mile"),
                                                    (mk_sp (Pcomp (Ge,
                                                                   (mk_sp (Pdot (mk_sp (Pvar (dumloc "m"))
                                                                                   ~type_:((Tasset (dumloc "mile"))),
                                                                                 (dumloc "expiration")))
                                                                      ~type_:((Tbuiltin VTdate))),
                                                                   (mk_sp (Pconst Cnow)
                                                                      ~type_:((Tbuiltin VTdate)))
                                                                  ))
                                                       ~type_:((Tbuiltin VTbool))
                                                    )))
                                       ~type_:((Tbuiltin VTbool)))

                                    ~invariants:[
                                      mk_invariant (dumloc "loop")
                                        ~formulas:[
                                          (mk_sp (Pcomp (Equal,
                                                         (mk_sp (Pcall (None,
                                                                        Cconst Csum,
                                                                        [
                                                                          AExpr (mk_sp (Pcall (None,
                                                                                               Cconst Cbefore,
                                                                                               [
                                                                                                 AExpr (mk_sp (Pvar (dumloc "mile"))
                                                                                                          ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection))))
                                                                                               ]))
                                                                                   ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection)))
                                                                                );
                                                                          AExpr (mk_sp (Pvar (dumloc "amount"))
                                                                                   ~type_:((Tbuiltin VTint)))
                                                                        ]))
                                                            ~type_:((Tbuiltin VTint))),

                                                         mk_sp (Parith (Minus,
                                                                        mk_sp (Parith (Plus,
                                                                                       (mk_sp (Pcall (None,
                                                                                                      Cconst Csum,
                                                                                                      [
                                                                                                        AExpr (mk_sp (Pvar (dumloc "mile"))
                                                                                                                 ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection))));
                                                                                                        AExpr (mk_sp (Pvar (dumloc "amount"))
                                                                                                                 ~type_:((Tbuiltin VTint)))
                                                                                                      ]))
                                                                                          ~type_:((Tbuiltin VTint))),
                                                                                       (mk_sp (Pvar (dumloc "quantity"))
                                                                                          ~type_:((Tbuiltin VTint)))
                                                                                      ))
                                                                          ~type_:((Tbuiltin VTint)),
                                                                        mk_sp (Pvar (dumloc "remainder"))
                                                                          ~type_:((Tbuiltin VTint))
                                                                       ))
                                                           ~type_:((Tbuiltin VTint))))

                                             ~type_:((Tbuiltin VTbool)))
                                        ]
                                    ];


                                  mk_specification (dumloc "p4")
                                    (mk_sp (Pcall (None,
                                                   Cid (dumloc "is_empty"),
                                                   [
                                                     AExpr (mk_sp (Pcall (None,
                                                                          Cid (dumloc "added"),
                                                                          [AExpr (mk_sp (Pvar (dumloc "mile"))
                                                                                    ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection))))
                                                                          ]))
                                                              ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection))))
                                                   ]))
                                       ~type_:((Tbuiltin VTbool)))

                                    ~invariants:[
                                      mk_invariant (dumloc "loop")
                                        ~formulas:[
                                          (mk_sp (Pcall (None,
                                                         Cid (dumloc "is_empty"),
                                                         [
                                                           AExpr (mk_sp (Pcall (None,
                                                                                Cid (dumloc "added"),
                                                                                [AExpr (mk_sp (Pvar (dumloc "mile"))
                                                                                          ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection))))
                                                                                ]))
                                                                    ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection))))
                                                         ]))
                                             ~type_:((Tbuiltin VTbool)))
                                        ]
                                    ]
                                ]
                             ))


        ~args:[mk_decl (dumloc "a")
                 ~typ:(Tbuiltin VTrole);
               mk_decl (dumloc "quantity")
                 ~typ:(Tbuiltin VTint)
              ]
        ~calledby:(mk_sp (Rqualid (mk_sp (Qident (dumloc "admin"))
                                     ~type_:(Tbuiltin VTaddress)
                                  )))

        ~effect:(
          mk_instr (Iletin (
              dumloc "ow",
              (mk_sp (Pcall (Some (mk_sp ~type_:(Tasset (dumloc "mile")) (Pvar (dumloc "owner"))),
                             Cconst Cget,
                             [AExpr (mk_sp (Pvar (dumloc "a"))
                                       ~type_:(Tbuiltin VTaddress))]))
                 ~type_:(Tasset (dumloc "owner"))
              ),
              mk_instr (Iletin (
                  dumloc "by_expiration",
                  (mk_sp (Pcall (None,
                                 Cconst Cselect,
                                 [
                                   AExpr (mk_sp (Pdot (mk_sp (Pvar (dumloc "ow"))
                                                         ~type_:(Tasset (dumloc "owner")),
                                                       dumloc "miles"))
                                            ~type_:(Tcontainer (Tasset (dumloc "mile"), Partition)));
                                   AExpr (mk_sp (Pcomp (Gt,
                                                        mk_sp (Pdot (
                                                            mk_sp (Pvar (dumloc "the"))
                                                              ~type_:((Tasset (dumloc "mile"))),
                                                            dumloc "expiration"
                                                          ))
                                                          ~type_:((Tbuiltin VTdate)),
                                                        mk_sp (Pconst Cnow)
                                                          ~type_:((Tbuiltin VTdate))
                                                       ))
                                            ~type_:((Tbuiltin VTbool))
                                         )
                                 ]))
                     ~type_:(Tcontainer (Tasset (dumloc "mile"), Collection))
                  ),
                  mk_instr (Iseq [
                      mk_instr (Irequire (true,
                                          (mk_sp (Pcomp (Ge,
                                                         (mk_sp (Pcall (None,
                                                                        Cconst Csum,
                                                                        [
                                                                          AExpr (mk_sp (Pvar (dumloc "by_expiration"))
                                                                                   ~type_:(Tcontainer (Tasset (dumloc "mile"), Collection)));
                                                                          AExpr (mk_sp (Pvar (dumloc "amount"))
                                                                                   ~type_:(Tbuiltin VTint))
                                                                        ]))
                                                            ~type_:(Tbuiltin VTint)),
                                                         (mk_sp (Pvar (dumloc "quantity"))
                                                            ~type_:(Tbuiltin VTint))
                                                        ))
                                             ~type_:(Tbuiltin VTbool)
                                          )
                                         ));
                      mk_instr (Iletin (
                          dumloc "remainder",
                          (mk_sp (Pvar (dumloc "quantity"))
                             ~type_:(Tbuiltin VTint)),
                          mk_instr
                            (Iseq [
                                mk_instr
                                  ~label:"loop"
                                  (Ifor (dumloc "m",
                                         mk_sp (Pvar (dumloc "by_expiration"))
                                           ~type_:(Tcontainer (Tasset (dumloc "mile"), Collection)),
                                         mk_instr (Iif (
                                             (mk_sp (Pcomp (Gt,
                                                            (mk_sp (Pvar (dumloc "remainder"))
                                                               ~type_:(Tbuiltin VTint)),
                                                            (mk_sp (Plit (mk_sp (BVint Big_int.zero_big_int)))
                                                               ~type_:(Tbuiltin VTint))
                                                           ))
                                                ~type_:(Tbuiltin VTbool)
                                             ),
                                             mk_instr (Iif (
                                                 (mk_sp (Pcomp (Gt,
                                                                (mk_sp (Pdot (
                                                                     mk_sp (Pvar (dumloc "m"))
                                                                       ~type_:(Tasset (dumloc "mile")),
                                                                     dumloc "amount"
                                                                   ))
                                                                    ~type_:(Tbuiltin VTint)),
                                                                (mk_sp (Pvar (dumloc "remainder"))
                                                                   ~type_:(Tbuiltin VTint))
                                                               ))
                                                    ~type_:(Tbuiltin VTbool)
                                                 ),
                                                 mk_instr (Iseq [
                                                     mk_instr (Iassign (ValueAssign,
                                                                        dumloc "remainder",
                                                                        (mk_sp (Plit (mk_sp (BVint Big_int.zero_big_int)))
                                                                           ~type_:(Tbuiltin VTint))));
                                                     mk_instr (Icall (Some (mk_sp (Pvar (dumloc "mile"))
                                                                              ~type_:(Tcontainer (Tasset (dumloc "mile"), Collection))),
                                                                      Cconst Cupdate,
                                                                      [
                                                                        AExpr (mk_sp (Pdot (
                                                                            mk_sp (Pvar (dumloc "m"))
                                                                              ~type_:(Tasset (dumloc "mile")),
                                                                            dumloc "id"
                                                                          ))
                                                                            ~type_:(Tbuiltin VTstring));
                                                                        AEffect [
                                                                          (dumloc "amount"),
                                                                          `Assign MinusAssign,
                                                                          mk_sp (Pvar (dumloc "remainder"))
                                                                            ~type_:(Tbuiltin VTint)
                                                                        ]
                                                                      ]
                                                                     )
                                                              )
                                                   ]),
                                                 mk_instr (Iif (
                                                     (mk_sp (Pcomp (Equal,
                                                                    (mk_sp (Pdot (
                                                                         mk_sp (Pvar (dumloc "m"))
                                                                           ~type_:(Tasset (dumloc "mile")),
                                                                         dumloc "amount"
                                                                       ))
                                                                        ~type_:(Tbuiltin VTint)),
                                                                    (mk_sp (Pvar (dumloc "remainder"))
                                                                       ~type_:(Tbuiltin VTint))
                                                                   ))
                                                        ~type_:(Tbuiltin VTbool)
                                                     ),
                                                     mk_instr (Iseq [
                                                         mk_instr (Iassign (ValueAssign,
                                                                            dumloc "remainder",
                                                                            (mk_sp (Plit (mk_sp (BVint Big_int.zero_big_int)))
                                                                               ~type_:(Tbuiltin VTint))));
                                                         mk_instr (Icall (Some (
                                                             (mk_sp (Pdot (
                                                                  mk_sp (Pvar (dumloc "ow"))
                                                                    ~type_:(Tasset (dumloc "owner")),
                                                                  dumloc "miles"
                                                                ))
                                                                 ~type_:(Tcontainer (Tasset (dumloc "mile"), Partition)))),
                                                                          Cconst Cremove,
                                                                          [
                                                                            AExpr (mk_sp (Pdot (
                                                                                mk_sp (Pvar (dumloc "m"))
                                                                                  ~type_:(Tasset (dumloc "mile")),
                                                                                dumloc "id"
                                                                              ))
                                                                                ~type_:(Tbuiltin VTstring))]
                                                           )
                                                           )
                                                       ]),
                                                     mk_instr (Iseq [
                                                         mk_instr (Iassign (MinusAssign,
                                                                            dumloc "remainder",
                                                                            (mk_sp (
                                                                                Pdot (
                                                                                  mk_sp (Pvar (dumloc "m"))
                                                                                    ~type_:(Tasset (dumloc "mile")),
                                                                                  dumloc "amount"))
                                                                                ~type_:(Tbuiltin VTint))));
                                                         mk_instr (Icall (Some (
                                                             (mk_sp (Pdot (
                                                                  mk_sp (Pvar (dumloc "ow"))
                                                                    ~type_:(Tasset (dumloc "owner")),
                                                                  dumloc "miles"
                                                                ))
                                                                 ~type_:(Tcontainer (Tasset (dumloc "mile"), Partition)))),
                                                                          Cconst Cremove,
                                                                          [
                                                                            AExpr (mk_sp (Pdot (
                                                                                mk_sp (Pvar (dumloc "m"))
                                                                                  ~type_:(Tasset (dumloc "mile")),
                                                                                dumloc "id"
                                                                              ))
                                                                                ~type_:(Tbuiltin VTstring))]
                                                           )
                                                           )
                                                       ]))))),
                                             mk_instr (Iseq []))
                                           )));
                                mk_instr
                                  (Ilabel (dumloc "p1"))
                              ])
                        ))]

                    ))))));

      mk_transaction_struct (dumloc "clear_expired")
        ~args:[]
        ~calledby:(mk_sp (Rqualid (mk_sp (Qident (dumloc "admin"))
                                     ~type_:(Tbuiltin VTaddress)
                                  )))


        ~effect:(mk_instr (Ifor (dumloc "o",
                                 mk_sp (Pvar (dumloc "owner"))
                                   ~type_:(Tcontainer (Tasset (dumloc "owner"), Collection)),
                                 mk_instr (Icall (Some (mk_sp (Pdot (mk_sp (Pvar (dumloc "o"))
                                                                       ~type_:(Tasset (dumloc "owner")),
                                                                     (dumloc "miles")))
                                                          ~type_:(Tcontainer (Tasset (dumloc "mile"), Collection))),
                                                  Cconst Cremoveif,
                                                  [
                                                    AExpr
                                                      (mk_sp (Pcomp (Lt,
                                                                     mk_sp (Pdot (mk_sp (Pvar (dumloc "the"))
                                                                                    ~type_:(Tasset (dumloc "owner")),
                                                                                  dumloc "expiration"))
                                                                       ~type_:(Tbuiltin VTdate),
                                                                     mk_sp (Pconst Cnow)
                                                                       ~type_:(Tbuiltin VTdate)))
                                                         ~type_:(Tbuiltin VTbool)
                                                      )
                                                  ]
                                                 )
                                          ))))

        ~verification:(mk_verification ()
                         ~specs:[
                           mk_specification (dumloc "s3")
                             (mk_sp (Pquantifer (Forall,
                                                 dumloc "m",
                                                 Tasset (dumloc "mile"),
                                                 (mk_sp (Pcomp (Lt,
                                                                (mk_sp (Pdot (mk_sp (Pvar (dumloc "m"))
                                                                                ~type_:((Tasset (dumloc "mile"))),
                                                                              (dumloc "expiration")))
                                                                   ~type_:((Tbuiltin VTdate))),
                                                                (mk_sp (Pconst Cnow)
                                                                   ~type_:((Tbuiltin VTdate)))
                                                               ))
                                                    ~type_:((Tbuiltin VTbool))
                                                 )))
                                ~type_:((Tbuiltin VTbool)))
                         ])
    ]

    ~verifications:[
      mk_verification ()
        ~specs:[
          mk_specification (dumloc "g1")
            (mk_sp (Pcall (None,
                           Cconst Cmaybeperformedonlybyrole,
                           [
                             AExpr (mk_sp (Pconst Canyaction)
                                      ~type_:(Ttrace TRentry));
                             AExpr (mk_sp (Pvar (dumloc "admin"))
                                      ~type_:((Tbuiltin VTaddress)))
                           ]))
               ~type_:((Tbuiltin VTbool)));
          mk_specification (dumloc "g2")
            (mk_sp (Pcall (None,
                           Cconst Cmaybeperformedonlybyaction,
                           [
                             AExpr (mk_sp (Pcall (
                                 None,
                                 Cid (dumloc "remove"),
                                 [
                                   AExpr (mk_sp (Pvar (dumloc "mile"))
                                            ~type_:((Tcontainer (Tasset (dumloc "mile"), Collection)))
                                         )]))
                                 ~type_:(Ttrace TRentry)
                               );
                             AExpr (mk_sp (Plogical (
                                 Or,
                                 mk_sp (Pvar (dumloc "consume"))
                                   ~type_:(Ttrace TRentry),
                                 mk_sp (Pvar (dumloc "clear_expired"))
                                   ~type_:(Ttrace TRentry))
                               )
                                 ~type_:(Ttrace TRentry))
                           ]))
               ~type_:((Tbuiltin VTbool)));
          mk_specification (dumloc "g3")
            (mk_sp (Pcall (None,
                           Cconst Cmaybeperformedonlybyaction,
                           [
                             AExpr (mk_sp (Pnot (mk_sp (Pcall (
                                 None,
                                 Cid (dumloc "add"),
                                 [
                                   AExpr (mk_sp (Pvar (dumloc "mile"))
                                            ~type_:(Ttrace TRaction)
                                         )
                                 ]))
                                 ~type_:(Ttrace TRaction)
                               ))
                                 ~type_:(Ttrace TRaction)
                               );
                             AExpr (mk_sp (Pvar (dumloc "consume"))
                                      ~type_:(Ttrace TRentry)
                                   )
                           ]))
               ~type_:((Tbuiltin VTbool)))
        ]
    ]
