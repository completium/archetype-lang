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
  | VTrational
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
  | Tenum of lident
  | Tcontract of lident
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
]
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
  (* vset *)
  | Cbefore
  | Cunmoved
  | Cadded
  | Cremoved
  | Citerated
  | Ctoiterate
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
  | Lquantifer of quantifier * 'id * ltype_ * 'term
  | Pif of ('term * 'term * 'term)
  | Pmatchwith of 'term * ('id pattern_gen * 'term) list
  | Pcall of ('id option * 'id call_kind * ('term term_arg) list)
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
[@@deriving show {with_path = false}]

and 'term term_arg =
  | AExpr   of 'term
  | AEffect of unit
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'term) lterm_poly = (ltype_, ('id, 'typ, 'term) term_node) struct_poly
[@@deriving show {with_path = false}]

type ('id, 'typ) lterm_gen = ('id, 'typ, ('id, 'typ) lterm_gen) lterm_poly
[@@deriving show {with_path = false}]

type lterm = (lident, type_) lterm_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'term) pterm_poly = ('typ, ('id, 'typ, 'term) term_node) struct_poly
[@@deriving show {with_path = false}]

type ('id, 'typ) pterm_gen = ('id, 'typ, ('id, 'typ) pterm_gen) pterm_poly
[@@deriving show {with_path = false}]

type pterm = (lident, type_, pterm) pterm_poly
[@@deriving show {with_path = false}]

type pterm_arg = pterm term_arg
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'term, 'instr) instruction_poly = ('typ, ('id, 'typ, 'term, 'instr) instruction_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'typ, 'term, 'instr) instruction_node =
  | Iif of ('term * 'instr * 'instr)                              (* condition * then_ * else_ *)
  | Ifor of ('id * 'term * 'instr)                                (* id * collection * body *)
  | Iletin of ('id * 'term * 'instr)                              (* id * init * body *)
  | Iseq of 'instr list                                           (* lhs ; rhs*)
  | Imatchwith of 'term * ('id pattern_gen * 'instr) list (* match 'term with ('pattern * 'instr) list *)
  | Iassign of (assignment_operator * 'id * 'term)                (* $2 assignment_operator $3 *)
  | Irequire of (bool * 'term)                                    (* $1 ? require : failif *)
  | Itransfer of ('term * bool * ('id, 'typ) qualid_gen option)   (* value * back * dest *)
  | Ibreak
  | Iassert of 'term
  | Icall of ('term option * 'id call_kind * ('term) list)
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) instruction_gen = ('id, 'typ, 'term, ('id, 'typ, 'term) instruction_gen) instruction_poly
[@@deriving show {with_path = false}]

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
  args : ('id * (('id, 'typ) lterm_gen)) list;
  body : ('id, 'typ) lterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ) definition = {
  name : 'id;
  typ  : 'typ;
  var  : 'id;
  body : ('id, 'typ) lterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) verification = {
  predicates  : ('id, 'typ) predicate list;
  definitions : ('id, 'typ) definition list;
  axioms      : ('id, ('id, 'typ) lterm_gen) label_term list;
  theorems    : ('id, ('id, 'typ) lterm_gen) label_term list;
  variables   : ('id, 'typ, 'term) variable list;
  invariants  : ('id * ('id, ('id, 'typ) lterm_gen) label_term list) list;
  effect      : 'term option;
  specs       : ('id, ('id, 'typ) lterm_gen) label_term list;
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
  specs   : ('id, ('id, 'typ) lterm_gen) label_term list;
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
let vtuint       = Tbuiltin (VTuint      )
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
  verification    = None;
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

let mk_id type_ id : qualid =
  { type_ = Some type_;
    loc   = loc id;
    node  = Qident id;
    label = None; }

let map_term_node f = function
  | Lquantifer (q, i, t, e) -> Lquantifer (q, i, t, f e)
  | Pif (c, t, e)           -> Pif (f c, f t, f e)
  | Pmatchwith (e, l)       -> Pmatchwith (e, List.map (fun (p, e) -> (p, f e)) l)
  | Pcall (i, e, args)      -> Pcall (i, e, List.map (fun arg -> match arg with | AExpr e -> AExpr (f e) | _ -> arg ) args)
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

let map_gen g f i =
  {
    i with
    node = g f i.node
  }

let map_term  f t = map_gen map_term_node  f t
let map_instr f i = map_gen map_instr_node f i

let fold_term f accu term =
  match term.node with
  | Lquantifer (_, _, _, e) -> f accu e
  | Pif (c, t, e)           -> f (f (f accu c) t) e
  | Pmatchwith (e, l)       -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Pcall (_, _, args)      -> List.fold_left (fun accu (arg : 'a term_arg) -> match arg with | AExpr e -> f accu e | _ -> accu ) accu args
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

let rec fold_map_term g f (accu : 'a) (term : 'term) : 'term * 'a =
  match term.node with
  | Lquantifer (q, id, t, e) ->
    let ee, ea = fold_map_term g f accu e in
    g (Lquantifer (q, id, t, ee)), ea

  | Pif (c, t, e) ->
    let ce, ca = fold_map_term g f accu c in
    let ti, ta = fold_map_term g f ca t in
    let ei, ea = fold_map_term g f ta e in
    g (Pif (ce, ti, ei)), ea

  | Pmatchwith (e, l) ->
    let ee, ea = fold_map_term g f accu e in
    let (pse, psa) =
      List.fold_left
        (fun (ps, accu) (p, i) ->
           let pa, accu = fold_map_term g f accu i in
           [(p, i)] @ ps, accu) ([], ea) l
    in

    g (Pmatchwith (ee, l)), psa

  | Pcall (a, id, args) ->
    let ((argss, argsa) : 'c list * 'a) =
      List.fold_left
        (fun (pterms, accu) (x : 'term term_arg) ->
           let p, accu =
             match x with
             | AExpr a -> fold_map_term g f accu a |> fun (x, acc) -> (Some (AExpr x), acc)
             | _ -> None, accu in
           let x = match p with | Some a -> a | None -> x in
           [x] @ pterms, accu) ([], accu) args
    in
    g (Pcall (a, id, argss)), argsa

  | Plogical (op, l, r) ->
    let le, la = fold_map_term g f accu l in
    let re, ra = fold_map_term g f la r in
    g (Plogical (op, le, re)), ra

  | Pnot e ->
    let ee, ea = fold_map_term g f accu e in
    g (Pnot ee), ea

  | Pcomp (op, l, r) ->
    let le, la = fold_map_term g f accu l in
    let re, ra = fold_map_term g f la r in
    g (Pcomp (op, le, re)), ra

  | Parith (op, l, r) ->
    let le, la = fold_map_term g f accu l in
    let re, ra = fold_map_term g f la r in
    g (Parith (op, le, re)), ra

  | Puarith (op, e) ->
    let ee, ea = fold_map_term g f accu e in
    g (Puarith (op, ee)), ea

  | Precord l ->
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = fold_map_term g f accu x in
           [p] @ pterms, accu) ([], accu) l in
    g (Precord lp), la

  | Pletin (id, i, t, o) ->
    let ie, ia = fold_map_term g f accu i in
    let oe, oa = fold_map_term g f ia o in
    g (Pletin (id, i, t, oe)), oa

  | Pvar id ->
    g (Pvar id), accu

  | Parray l ->
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = fold_map_term g f accu x in
           [p] @ pterms, accu) ([], accu) l in
    g (Parray lp), la

  | Plit l ->
    g (Plit l), accu

  | Pdot (e, id) ->
    let ee, ea = fold_map_term g f accu e in
    g (Pdot (ee, id)), ea

  | Pconst c ->
    g (Pconst c), accu

  | Ptuple l ->
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = fold_map_term g f accu x in
           [p] @ pterms, accu) ([], accu) l in
    g (Ptuple lp), la


let rec fold_map_instr_term gi ge fi fe (accu : 'a) instr : 'instr * 'a =
  match instr.node with
  | Iif (c, t, e) ->
    let ce, ca = fold_map_term (ge c) fe accu c in
    let ti, ta = fold_map_instr_term gi ge fi fe ca t in
    let ei, ea = fold_map_instr_term gi ge fi fe ta e in
    gi (Iif (ce, ti, ei)), ea

  | Ifor (i, c, b) ->
    let ce, ca = fold_map_term (ge c) fe accu c in
    let bi, ba = fold_map_instr_term gi ge fi fe ca b in
    gi (Ifor (i, ce, bi)), ba

  | Iletin (i, j, b) ->
    let je, ja = fold_map_term (ge j) fe accu j in
    let bi, ba = fold_map_instr_term gi ge fi fe ja b in
    gi (Iletin (i, je, bi)), ba

  | Iseq is ->
    let (isi, isa) : ('instr list * 'a) =
      List.fold_left
        (fun ((instrs, accu) : ('b list * 'c)) x ->
           let bi, accu = fold_map_instr_term gi ge fi fe accu x in
           [bi] @ instrs, accu) ([], accu) is
    in
    gi (Iseq isi), isa

  | Imatchwith (a, ps) ->
    let ae, aa = fold_map_term (ge a) fe accu a in

    let (pse, psa) =
      List.fold_left
        (fun (ps, accu) (p, i) ->
           let pa, accu = fold_map_instr_term gi ge fi fe accu i in
           [(p, i)] @ ps, accu) ([], aa) ps
    in

    gi (Imatchwith (ae, ps)), psa

  | Iassign (op, id, x) ->
    let xe, xa = fold_map_term (ge x) fe accu x in
    gi (Iassign (op, id, xe)), xa

  | Irequire (b, x) ->
    let xe, xa = fold_map_term (ge x) fe accu x in
    gi (Irequire (b, xe)), xa

  | Itransfer (x, b, q) ->
    let xe, xa = fold_map_term (ge x) fe accu x in
    gi (Itransfer (xe, b, q)), xa

  | Ibreak ->
    gi (Ibreak), accu

  | Iassert x ->
    let xe, xa = fold_map_term (ge x) fe accu x in
    gi (Iassert xe), xa

  | Icall (x, id, args) ->
    let xe, xa =
      match x with
      | Some x -> fold_map_term (ge x) fe accu x |> (fun (a, b) -> (Some a, b))
      | None -> None, accu in

    let (argss, argsa) =
      List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = fold_map_term (ge x) fe accu x in
           [p] @ pterms, accu) ([], xa) args
    in
    gi (Icall (xe, id, argss)), argsa

let mk_decl ?typ ?default ?(loc = Location.dummy) name =
  { name; typ; default; loc }

let create_fake_ast () =
  let ast = mk_model (dumloc "fake") in
  let decl1 = mk_decl (dumloc "str") ?typ:(Some (Tbuiltin VTstring)) ?default:(Some (mk_sp (Plit (mk_sp (BVstring "toto"))))) in
  let decl2 = mk_decl (dumloc "n") ?typ:(Some (Tbuiltin VTint)) ?default:(Some (mk_sp (Plit (mk_sp (BVint Big_int.zero_big_int))))) in
  let vars = [ mk_variable decl1; mk_variable decl2 ] in
  {
    ast with
    variables = vars;
  }

