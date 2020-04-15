open Location
open Ident
open Tools

type lident = ident loced

let pp_ident fmt i = Format.fprintf fmt "%s" i
let pp_lident fmt i = Format.fprintf fmt "%s" (unloc i)

type container =
  | Collection
  | Partition
  | View
[@@deriving show {with_path = false}]

type currency =
  | Tz
  | Mtz
  | Utz
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
  | VTcurrency
  | VTkey
  | VTbytes
[@@deriving show {with_path = false}]

type trtyp =
  | TRentry
  | TRaction (* add; remove; update *)
  | TRasset
  | TRfield
[@@deriving show {with_path = false}]

type ptyp =
  | Tnamed of int
  | Tasset of lident
  | Tenum of lident
  | Tcontract of lident
  | Tbuiltin of vtyp
  | Tcontainer of ptyp * container
  | Tlist of ptyp
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
  | DivRat
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
  | Csource
  | Cconditions
  | Cactions
  | Cnone
  | Cany
  | Canyaction
  | Cresult
  (* function *)
  | Cisempty
  | Cget
  | Cadd
  | Cremove
  | Cremoveif
  | Cclear
  | Cupdate
  | Caddupdate
  | Ccontains
  | Cnth
  | Cselect
  | Csort
  | Ccount
  | Csum
  | Cmax
  | Cmin
  | Csubsetof
  | Cslice
  | Cconcat
  | Clength
  | Cisnone
  | Cissome
  | Cgetopt
  (* list *)
  | Chead
  | Ctail
  | Cabs
  | Cprepend
  (* crypto *)
  | Cblake2b
  | Csha256
  | Csha512
  | Cchecksignature
  (* vset *)
  | Cbefore
  | Citerated
  | Ctoiterate
[@@deriving show {with_path = false}]

type ('node) struct_poly = {
  node : 'node;                   (* kind of object *)
  type_ : ptyp option;            (* type of object *)
  label : ident option;           (* label (typically for instruction) *)
  loc : Location.t [@opaque];     (* location of object *)
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type 'id qualid_gen = ('id qualid_node) struct_poly
[@@deriving show {with_path = false}]

and 'id qualid_node =
  | Qident of 'id
  | Qdot of 'id qualid_gen * 'id
[@@deriving show {with_path = false}]

type qualid = lident qualid_gen
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type 'id sexpr_gen = ('id sexpr_node) struct_poly
[@@deriving show {with_path = false}]

and 'id sexpr_node =
  | Sref of 'id
  | Sor of 'id sexpr_gen * 'id sexpr_gen
  | Sany
[@@deriving show {with_path = false}]

type sexpr = lident sexpr_gen
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

(* basic variable *)
type bval_gen = bval_node struct_poly
[@@deriving show {with_path = false}]

and bval_node =
  | BVint          of Core.big_int
  | BVuint         of Core.big_int
  | BVbool         of bool
  | BVenum         of string
  | BVrational     of Core.big_int * Core.big_int
  | BVdate         of Core.date
  | BVstring       of string
  | BVcurrency     of currency * Core.big_int
  | BVaddress      of string
  | BVduration     of Core.duration
  | BVbytes        of string
[@@deriving show {with_path = false}]

type bval = bval_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type 'id signature = {
  name : 'id;
  args: (lident * ptyp) list;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type quantifier =
  | Forall
  | Exists
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type 'id pattern_gen = ('id pattern_node) struct_poly
[@@deriving show {with_path = false}]

and 'id pattern_node =
  | Mwild
  | Mconst of 'id
[@@deriving show {with_path = false}]

type pattern = lident pattern_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type 'id call_kind =
  | Cid of 'id
  | Cconst of const
[@@deriving show {with_path = false}]

type var_temporality =
  | VTbefore
  | VTat of ident
  | VTnone
[@@deriving show {with_path = false}]

type vset =
  | Vadded
  | Vremoved
  | Vunmoved
  | Vnone
[@@deriving show {with_path = false}]

type 'id term_node  =
  | Pquantifer of quantifier * 'id * ('id term_gen option * type_) * 'id term_gen
  | Pif of ('id term_gen * 'id term_gen * 'id term_gen)
  | Pmatchwith of 'id term_gen * ('id pattern_gen * 'id term_gen) list
  | Pcall of ('id term_gen option * 'id call_kind * ('id term_arg) list)
  | Plogical of logical_operator * 'id term_gen * 'id term_gen
  | Pnot of 'id term_gen
  | Pmulticomp of 'id term_gen * (comparison_operator * 'id term_gen) list
  | Pcomp of comparison_operator * 'id term_gen * 'id term_gen
  | Parith of arithmetic_operator * 'id term_gen * 'id term_gen
  | Puarith of unary_arithmetic_operator * 'id term_gen
  | Precord of 'id term_gen list
  | Pletin of 'id * 'id term_gen * ptyp option * 'id term_gen * 'id term_gen option (* ident * init * type * body * otherwise *)
  | Pdeclvar of 'id * ptyp option * 'id term_gen
  | Pvar of var_temporality * vset * 'id
  | Parray of 'id term_gen list
  | Plit of bval
  | Pdot of 'id term_gen * 'id
  | Pconst of const
  | Ptuple of 'id term_gen list
  | Pnone
  | Psome of 'id term_gen
  | Pcast of ptyp * ptyp * 'id term_gen
[@@deriving show {with_path = false}]

and 'id term_arg =
  | AExpr    of 'id term_gen
  | AFun     of 'id * ptyp * ('id * ptyp * 'id term_gen) list * 'id term_gen
  | AEffect  of ('id * operator * 'id term_gen) list
  | ASorting of bool * 'id
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

and 'id term_poly = ('id term_node) struct_poly
[@@deriving show {with_path = false}]

and 'id term_gen = 'id term_poly
[@@deriving show {with_path = false}]

type pterm = lident term_gen
[@@deriving show {with_path = false}]

type pterm_arg = lident term_arg
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type 'id instruction_poly = {
  node : 'id instruction_node;
  label: string option;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

and 'id instruction_node =
  | Iif of ('id term_gen * 'id instruction_gen * 'id instruction_gen)               (* condition * then_ * else_ *)
  | Ifor of ('id * 'id term_gen * 'id instruction_gen)                              (* id * collection * body *)
  | Iiter of ('id * 'id term_gen* 'id term_gen * 'id instruction_gen)               (* id * bound_min * bound_max * body *)
  | Iletin of ('id * 'id term_gen * 'id instruction_gen)                            (* id * init * body *)
  | Ideclvar of 'id * 'id term_gen                                                  (* id * init *)
  | Iseq of 'id instruction_gen list                                                (* lhs ; rhs *)
  | Imatchwith of 'id term_gen * ('id pattern_gen * 'id instruction_gen) list       (* match term with ('pattern * 'id instruction_gen) list *)
  | Iassign of (ptyp * assignment_operator * 'id lvalue_gen * 'id term_gen)         (* $2 assignment_operator $3 *)
  | Irequire of (bool * 'id term_gen)                                               (* $1 ? require : failif *)
  | Itransfer of ('id term_gen * 'id term_gen * ('id * ('id term_gen) list) option) (* value * dest * call *)
  | Ibreak
  | Icall of ('id term_gen option * 'id call_kind * ('id term_arg) list)
  | Ireturn of 'id term_gen
  | Ilabel of 'id
  | Ifail of 'id term_gen
[@@deriving show {with_path = false}]

and 'id instruction_gen = 'id instruction_poly

and instruction = lident instruction_poly

and 'id lvalue_gen = [
  | `Var   of 'id
  | `Field of 'id term_gen * 'id
]

and lvalue = lident lvalue_gen

type 'id decl_gen = {
  name    : 'id;
  typ     : ptyp option;
  default : 'id term_gen option;
  shadow  : bool;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type 'id label_term = {
  label : 'id option;
  term : 'id term_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id variable = {
  decl         : 'id decl_gen; (* TODO *)
  constant     : bool;
  from         : 'id qualid_gen option;
  to_          : 'id qualid_gen option;
  invs         : 'id label_term list;
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id predicate = {
  name : 'id;
  args : ('id * type_) list;
  body : 'id term_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id definition = {
  name : 'id;
  typ  : type_;
  var  : 'id;
  body : 'id term_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id invariant = {
  label: 'id;
  formulas: 'id term_gen list;
}
[@@deriving show {with_path = false}]

type 'id postcondition = {
  name: 'id;
  formula: 'id term_gen;
  invariants: 'id invariant list;
  uses: 'id list;
}
[@@deriving show {with_path = false}]

type 'id assert_ = {
  name: 'id;
  label: 'id;
  formula: 'id term_gen;
  invariants: 'id invariant list;
  uses: 'id list;
}
[@@deriving show {with_path = false}]

type 'id specification = {
  predicates  : 'id predicate list;
  definitions : 'id definition list;
  lemmas      : 'id label_term list;
  theorems    : 'id label_term list;
  variables   : 'id variable list;
  invariants  : ('id * 'id label_term list) list;
  effect      : 'id instruction_gen option;
  specs       : 'id postcondition list;
  asserts     : 'id assert_ list;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type action_description =
  | ADAny
  | ADOp  of string * lident
[@@deriving show {with_path = false}]

type security_role   = lident
[@@deriving show {with_path = false}]

type security_action =
  | Sany
  | Sentry of lident list
[@@deriving show {with_path = false}]

type security_node =
  | SonlyByRole         of action_description * security_role list
  | SonlyInAction       of action_description * security_action
  | SonlyByRoleInAction of action_description * security_role list * security_action
  | SnotByRole          of action_description * security_role list
  | SnotInAction        of action_description * security_action
  | SnotByRoleInAction  of action_description * security_role list * security_action
  | StransferredBy      of action_description
  | StransferredTo      of action_description
  | SnoStorageFail      of security_action
[@@deriving show {with_path = false}]

type security_predicate = {
  s_node: security_node;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type security_item = {
  label       : lident;
  predicate   : security_predicate;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type security = {
  items       : security_item list;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id function_struct = {
  name          : 'id;
  args          : ('id decl_gen) list;
  body          : 'id instruction_gen;
  specification : 'id specification option;
  return        : ptyp;
  loc           : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type function_ = lident function_struct
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type 'id rexpr_gen = ('id rexpr_node) struct_poly
[@@deriving show {with_path = false}]

and 'id rexpr_node =
  | Rany
  | Rexpr of 'id term_gen
  | Ror of 'id rexpr_gen * 'id rexpr_gen
[@@deriving show {with_path = false}]

type rexpr = lident rexpr_gen
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type 'id transition = {
  from : 'id sexpr_gen;
  on   : ('id * ptyp * 'id * ptyp) option; (* key ident * key type * asset name * asset state type *)
  trs  : ('id * 'id term_gen option * 'id instruction_gen option) list; (* to * condition * action*)
}
[@@deriving show {with_path = false}]

type 'id transaction_struct = {
  name            : 'id;
  args            : ('id decl_gen) list;
  calledby        : 'id rexpr_gen option;
  accept_transfer : bool;
  require         : 'id label_term list option;
  failif          : 'id label_term list option;
  transition      : ('id transition) option;
  specification   : 'id specification option;
  functions       : 'id function_struct list;
  effect          : 'id instruction_gen option;
  loc             : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type transaction = lident transaction_struct
[@@deriving show {with_path = false}]

type 'id enum_item_struct = {
  name : 'id;
  initial : bool;
  invariants : 'id label_term list;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id  enum_kind =
  | EKenum of 'id
  | EKstate
[@@deriving show {with_path = false}]

type 'id enum_struct = {
  (* name : 'id; "_state" if it's coming from Dstates constructor *)
  kind: 'id enum_kind;
  items : ('id enum_item_struct) list;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type enum = lident enum_struct
[@@deriving show {with_path = false}]

type 'id asset_struct = {
  name    : 'id;
  fields  : 'id decl_gen list;
  key     : 'id option;   (* TODO: option ? *)
  sort    : 'id list;
  state   : 'id option;
  init    : 'id term_gen list list;
  specs   : 'id label_term list;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type asset = lident asset_struct

type 'id contract_struct = {
  name       : 'id;
  signatures : 'id signature list;
  init       : 'id term_gen option;
  loc        : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

and contract = lident contract_struct

type 'id decl_ =
  | Dvariable of 'id variable
  | Dasset    of 'id asset_struct
  | Denum     of 'id enum_struct
  | Dcontract of 'id contract_struct
[@@deriving show {with_path = false}]

type 'id fun_ =
  | Ffunction    of 'id function_struct
  | Ftransaction of 'id transaction_struct
[@@deriving show {with_path = false}]

type 'id model_struct = {
  name           : 'id;
  decls          : 'id decl_ list;
  funs           : 'id fun_ list;
  specifications : 'id specification list;
  securities     : security list;
  loc            : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

and model = lident model_struct

(* vtyp -> ptyp *)
let vtbool       = Tbuiltin (VTbool      )
let vtint        = Tbuiltin (VTint       )
let vtrational   = Tbuiltin (VTrational  )
let vtdate       = Tbuiltin (VTdate      )
let vtduration   = Tbuiltin (VTduration  )
let vtstring     = Tbuiltin (VTstring    )
let vtaddress    = Tbuiltin (VTaddress   )
let vtrole       = Tbuiltin (VTrole      )
let vtcurrency   = Tbuiltin (VTcurrency  )
let vtkey        = Tbuiltin (VTkey       )
let vtbytes      = Tbuiltin (VTbytes     )

(* mk functions *)


let mk_sp ?label ?(loc = Location.dummy) ?type_ node =
  { node; type_; label; loc; }

let mk_instr ?label ?(loc = Location.dummy) node =
  { node; label; loc }

let mk_label_term ?label ?(loc = Location.dummy) term =
  { label; term; loc }

let mk_variable ?(constant = false) ?from ?to_ ?(invs = []) ?(loc = Location.dummy) decl =
  { decl; constant; from; to_; invs; loc }

let mk_predicate ?(args = []) ?(loc = Location.dummy) name body =
  { name; args; body; loc }

let mk_definition ?(loc = Location.dummy) name typ var body =
  { name; typ; var; body; loc }

let mk_invariant ?(formulas = []) label =
  { label; formulas }

let mk_postcondition ?(invariants = []) ?(uses = []) name formula =
  { name; formula; invariants; uses }

let mk_assert ?(invariants = []) ?(uses = []) name label formula =
  { name; label; formula; invariants; uses }

let mk_specification ?(predicates = []) ?(definitions = []) ?(lemmas = []) ?(theorems = []) ?(variables = []) ?(invariants = []) ?effect ?(specs = []) ?(asserts = []) ?(loc = Location.dummy) () =
  { predicates; definitions; lemmas; theorems; variables; invariants; effect; specs; asserts; loc}

let mk_function_struct ?(args = []) ?specification ?(loc = Location.dummy) name body return =
  { name; args; body; specification; return; loc }

let mk_transition ?on ?(trs = []) from =
  { from; on; trs }

let mk_transaction_struct ?(args = []) ?calledby ?(accept_transfer = false) ?require ?failif ?transition ?specification ?(functions = []) ?effect ?(loc = Location.dummy) name =
  { name; args; calledby; accept_transfer; require; failif; transition; specification; functions; effect; loc }

let mk_enum_item ?(initial = false) ?(invariants = []) ?(loc = Location.dummy) name : 'id enum_item_struct =
  { name; initial; invariants; loc }

let mk_enum ?(items = []) ?(loc = Location.dummy) kind =
  { kind; items; loc }

let mk_decl ?typ ?default ?(shadow=false) ?(loc = Location.dummy) name =
  { name; typ; default; shadow; loc }

let mk_asset ?(fields = []) ?key ?(sort = []) ?state ?(init = []) ?(specs = []) ?(loc = Location.dummy) name   =
  { name; fields; key; sort; state; init; specs; loc }

let mk_contract ?(signatures = []) ?init ?(loc = Location.dummy) name =
  { name; signatures; init; loc }

let mk_model ?(decls = []) ?(funs = []) ?(specifications = []) ?(securities = []) ?(loc = Location.dummy) name =
  { name; decls; funs; specifications; securities; loc }

let mk_id type_ id : qualid =
  { type_ = Some type_;
    loc   = loc id;
    node  = Qident id;
    label = None; }

let map_term_node (f : 'id term_gen -> 'id term_gen) = function
  | Pquantifer (q, i, t, e) -> Pquantifer (q, i, t, f e)
  | Pif (c, t, e)           -> Pif (f c, f t, f e)
  | Pmatchwith (e, l)       -> Pmatchwith (e, List.map (fun (p, e) -> (p, f e)) l)
  | Pcall (i, e, args)      ->
    Pcall (i, e, List.map (fun (arg : 'id term_arg) -> match arg with
        | AExpr e -> AExpr (f e)
        | AFun (x, xty, l, e) -> AFun (x, xty, List.map (fun (x, y, z) -> (x, y, f z)) l, f e)
        | AEffect l -> AEffect (List.map (fun (id, op, e) -> (id, op, f e)) l)
        | ASorting (b, f) -> ASorting (b, f))
        args)
  | Plogical (op, l, r)     -> Plogical (op, f l, f r)
  | Pnot e                  -> Pnot (f e)
  | Pmulticomp (e, l)       -> Pmulticomp (f e, List.map (fun (op, e) -> (op, f e)) l)
  | Pcomp (c, l, r)         -> Pcomp (c, f l, f r)
  | Parith (op, l, r)       -> Parith (op, f l, f r)
  | Puarith (op, e)         -> Puarith (op, f e)
  | Precord l               -> Precord (List.map f l)
  | Pletin (i, a, t, b, o)     -> Pletin (i, f a, t, f b, Option.map f o)
  | Pdeclvar (i, t, v)      -> Pdeclvar (i, t, f v)
  | Pvar (b, vs, v)         -> Pvar (b, vs, v)
  | Parray l                -> Parray (List.map f l)
  | Plit l                  -> Plit l
  | Pdot (e, i)             -> Pdot (f e, i)
  | Pconst c                -> Pconst c
  | Ptuple l                -> Ptuple (List.map f l)
  | Pnone                   -> Pnone
  | Psome a                 -> Psome (f a)
  | Pcast (src, dst, v)     -> Pcast (src, dst, f v)

let map_instr_node f = function
  | Iif (c, t, e)       -> Iif (c, f t, f e)
  | Ifor (i, c, b)      -> Ifor (i, c, f b)
  | Iiter (i, a, b, c)  -> Iiter (i, a, b, f c)
  | Iletin (i, c, b)    -> Iletin (i, c, f b)
  | Ideclvar (i, v)     -> Ideclvar (i, v)
  | Iseq is             -> Iseq (List.map f is)
  | Imatchwith (a, ps)  -> Imatchwith (a, ps)
  | Iassign (t, op, l, r)-> Iassign (t, op, l, r)
  | Irequire (b, x)     -> Irequire (b, x)
  | Itransfer x         -> Itransfer x
  | Ibreak              -> Ibreak
  | Icall (x, id, args) -> Icall (x, id, args)
  | Ireturn x           -> Ireturn x
  | Ilabel x            -> Ilabel x
  | Ifail m             -> Ifail m

let map_gen_poly g f (i : 'id struct_poly) : 'id struct_poly =
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

let fold_term_arg (f : 'id term_gen -> 'a -> 'a) (accu : 'a) (arg : 'id term_arg) =
  match arg with
  | AExpr e -> f accu e
  | AFun (_, _, l, e) -> List.fold_left (fun accu (_, _, v) -> f accu v) (f accu e) l
  | AEffect l -> List.fold_left (fun accu (_, _, e) -> f accu e) accu l
  | ASorting _ -> accu

let fold_term (f: 'a -> 't -> 'a) (accu : 'a) (term : 'id term_gen) =
  match term.node with
  | Pquantifer (_, _, _, e) -> f accu e
  | Pif (c, t, e)           -> f (f (f accu c) t) e
  | Pmatchwith (e, l)       -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Pcall (_, _, args)      -> List.fold_left (fold_term_arg f) accu args
  | Plogical (_, l, r)      -> f (f accu l) r
  | Pnot e                  -> f accu e
  | Pmulticomp (e, l)       -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Pcomp (_, l, r)         -> f (f accu l) r
  | Parith (_, l, r)        -> f (f accu l) r
  | Puarith (_, e)          -> f accu e
  | Precord l               -> List.fold_left f accu l
  | Pletin (_, a, _, b, o)  -> let tmp = f (f accu a) b in Option.map_dfl (f tmp) tmp o
  | Pdeclvar (_, _, v)      -> f accu v
  | Pvar _                  -> accu
  | Parray l                -> List.fold_left f accu l
  | Plit _                  -> accu
  | Pdot (e, _)             -> f accu e
  | Pconst _                -> accu
  | Ptuple l                -> List.fold_left f accu l
  | Pnone                   -> accu
  | Psome a                 -> f accu a
  | Pcast (_, _, v)         -> f accu v

let fold_instr f accu instr =
  match instr.node with
  | Iif (_, t, e)      -> f (f accu t) e
  | Ifor (_, _, b)     -> f accu b
  | Iiter (_, _, _, c) -> f accu c
  | Iletin (_, _, b)   -> f accu b
  | Ideclvar (_, _)    -> accu
  | Iseq is            -> List.fold_left f accu is
  | Imatchwith _       -> accu
  | Iassign _          -> accu
  | Irequire _         -> accu
  | Itransfer _        -> accu
  | Ibreak             -> accu
  | Icall _            -> accu
  | Ireturn _          -> accu
  | Ilabel _           -> accu
  | Ifail _            -> accu

let fold_instr_expr fi fe accu instr =
  match instr.node with
  | Iif (c, t, e)       -> fi (fi (fe accu c) t) e
  | Ifor (_, c, b)      -> fi (fe accu c) b
  | Iiter (_, a, b, c)  -> fi (fe (fe accu a) b) c
  | Iletin (_, j, b)    -> fi (fe accu j) b
  | Ideclvar (_, v)     -> fe accu v
  | Iseq is             -> List.fold_left fi accu is
  | Imatchwith (a, ps)  -> List.fold_left (fun accu (_, i) -> fi accu i) (fe accu a) ps
  | Iassign (_, _, _, e)-> fe accu e
  | Irequire (_, x)     -> fe accu x
  | Itransfer (v, d, c) -> Option.map_dfl (fun (_, l) -> List.fold_left fe accu l) (fe (fe accu v) d) c
  | Ibreak              -> accu
  | Icall (x, _, args)  -> let accu = Option.map_dfl (fe accu) accu x in List.fold_left (fold_term_arg fe) accu args
  | Ireturn x           -> fe accu x
  | Ilabel x            -> fi accu x
  | Ifail m             -> fe accu m

let fold_map_term g f (accu : 'a) (term : 'id term_gen) : 'term * 'a =
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
           let ia, accu = f accu i in
           [(p, ia)] @ ps, accu) ([], ea) l
    in

    g (Pmatchwith (ee, pse)), psa

  | Pcall (a, id, args) ->
    let ((argss, argsa) : 'c list * 'a) =
      List.fold_left
        (fun (pterms, accu) (x : 'id term_arg) ->
           let p, accu =
             match x with
             | AExpr a -> f accu a |> fun (x, acc) -> (Some (AExpr x), acc)
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

  | Pmulticomp (e, l) ->
    let ee, ea = f accu e in
    let (le, la) =
      List.fold_left
        (fun (ps, accu) (p, i) ->
           let ia, accu = f accu i in
           [(p, ia)] @ ps, accu) ([], ea) l
    in

    g (Pmulticomp (ee, le)), la

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

  | Pletin (id, i, t, b, o) ->
    let ie, ia = f accu i in
    let be, ba = f ia b in
    let oe, oa =
      match o with
      | Some o -> f ba o |> (fun (x, y) -> (Some x, y))
      | None -> (None, ba) in
    g (Pletin (id, ie, t, be, oe)), oa

  | Pdeclvar (i, t, v) ->
    let ve, va = f accu v in
    g (Pdeclvar (i, t, ve)), va

  | Pvar (b, vs, v) ->
    g (Pvar (b, vs, v)), accu

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

  | Pnone ->
    g Pnone, accu

  | Psome a ->
    g (Psome a), accu

  | Pcast (src, dst, v) ->
    let ve, va = f accu v in
    g (Pcast (src, dst, ve)), va


let fold_map_instr_term gi _ge fi fe (accu : 'a) instr : 'id instruction_gen * 'a =
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

  | Iiter (i, a, b, c) ->
    let ae, aa = fe accu a in
    let be, ba = fe aa b in
    let ce, ca = fi ba c in
    gi (Iiter (i, ae, be, ce)), ca

  | Iletin (i, j, b) ->
    let je, ja = fe accu j in
    let bi, ba = fi ja b in
    gi (Iletin (i, je, bi)), ba

  | Ideclvar (i, v) ->
    let ve, va = fe accu v in
    gi (Ideclvar (i, ve)), va

  | Iseq is ->
    let (isi, isa) : ('id instruction_gen list * 'a) =
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
           let ia, accu = fi accu i in
           [(p, ia)] @ ps, accu) ([], aa) ps
    in

    gi (Imatchwith (ae, pse)), psa

  | Iassign (t, op, id, x) ->
    let xe, xa = fe accu x in
    gi (Iassign (t, op, id, xe)), xa

  | Irequire (b, x) ->
    let xe, xa = fe accu x in
    gi (Irequire (b, xe)), xa

  | Itransfer (v, d, c) ->
    let ve, va = fe accu v in
    let de, da = fe va d in
    (* FIXME handle c *)
    gi (Itransfer (ve, de, c)), da

  | Ibreak ->
    gi (Ibreak), accu

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

  | Ifail m ->
    let me, ma = fe accu m in
    gi (Ilabel me), ma
(* -------------------------------------------------------------------- *)

module Utils : sig

  val get_asset                 : model -> lident -> asset
  val get_asset_field           : model -> (lident * lident ) -> lident decl_gen
  val get_asset_key             : model -> lident -> (lident * vtyp)
  val get_container_asset_field : model -> (lident * lident ) -> container
  val get_named_field_list      : model -> lident -> pterm list -> (lident * pterm) list
  val get_field_list            : model -> lident -> lident list
  val get_enum_values           : model -> lident -> lident option
  val is_variable               : model -> lident -> bool
  val is_asset                  : model -> lident -> bool
  val is_enum_value             : model -> lident -> bool
  val get_var_type              : model -> lident -> type_
  val get_enum_name             : lident enum_struct -> lident
  val get_contract_sig_ids      : model -> ident -> ident -> lident list

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

  let get_variables ast = List.fold_right (fun (x : 'id decl_) accu -> match x with Dvariable x ->  x::accu | _ -> accu ) ast.decls []
  let get_assets ast    = List.fold_right (fun (x : 'id decl_) accu -> match x with Dasset x    ->  x::accu | _ -> accu ) ast.decls []
  let get_enums ast     = List.fold_right (fun (x : 'id decl_) accu -> match x with Denum x     ->  x::accu | _ -> accu ) ast.decls []
  let get_contracts ast = List.fold_right (fun (x : 'id decl_) accu -> match x with Dcontract x ->  x::accu | _ -> accu ) ast.decls []

  let get_asset_opt ast asset_name : asset option =
    let id = unloc asset_name in
    List.fold_left (fun accu (x : asset) -> if String.equal id (unloc x.name) then Some x else accu ) None (get_assets ast)

  let get_asset ast asset_name : asset =
    let res = get_asset_opt ast asset_name in
    match res with
    | Some v -> v
    | _ -> emit_error (AssetNotFound (unloc asset_name))

  let get_asset_field ast (asset_name, field_name) : 'id decl_gen =
    let asset = get_asset ast asset_name in
    let res = List.fold_left (fun accu (x : 'id decl_gen) -> if String.equal (unloc field_name) (unloc x.name) then Some x else accu) None asset.fields in
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
    List.map (fun (x : lident decl_gen) -> x.name) asset.fields

  let get_named_field_list ast asset_name list =
    let field_list = get_field_list ast asset_name in
    (* List.iter (fun x -> Format.eprintf "f1: %s@." (unloc x)) field_list;
       List.iter (fun x -> Format.eprintf "f2: %a@." pp_pterm x) list;
       Format.eprintf "lf1: %d@." (List.length field_list);
       Format.eprintf "lf2: %d@." (List.length list); *)
    List.map2 (fun x y -> x, y) field_list list

  let get_enum_name (e : 'id enum_struct) =
    match e.kind with
    | EKenum id -> id
    | EKstate -> dumloc "state"

  let get_enum_opt ast ident =
    List.fold_left (fun accu (x : 'id enum_struct) ->
        if (Location.unloc (get_enum_name x)) = (Location.unloc ident)
        then Some x
        else accu
      ) None (get_enums ast)

  let get_asset_opt ast ident =
    List.fold_left (fun accu (x : 'id asset_struct) ->
        if (Location.unloc x.name) = (Location.unloc ident)
        then Some x
        else accu
      ) None (get_assets ast)

  let get_contract_opt ast ident =
    List.fold_left (fun accu (x : 'id contract_struct) ->
        if (Location.unloc x.name) = (Location.unloc ident)
        then Some x
        else accu
      ) None (get_contracts ast)

  let get_enum_values ast ident =
    List.fold_left (
      fun accu (x : 'id enum_struct) ->
        if List.fold_left (fun accu (x : 'id enum_item_struct) -> accu || (Location.unloc x.name) = (Location.unloc ident)) false x.items
        then (Some (get_enum_name x))
        else accu
    ) None (get_enums ast)

  let get_variable_opt ast ident : 'id variable option =
    List.fold_left (
      fun accu (x : 'id variable) ->
        if (String.equal (Location.unloc x.decl.name) (Location.unloc ident))
        then Some x
        else accu
    ) None (get_variables ast)

  let is_variable ast ident : bool =
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
        fun accu (x : 'id variable) ->
          if (String.equal (Location.unloc x.decl.name) (Location.unloc ident))
          then x.decl.typ
          else accu
      ) None (get_variables ast) in
    match var with
    | Some v -> v
    | None -> emit_error VariableNotFound

  let get_contract_sig_ids (ast : model) (contract_id : ident) (fun_id : ident) : lident list =
    let get_signature signatures ident : ((lident * ptyp) list) option =
      List.fold_left (fun accu (x : 'id signature) ->
          if (Location.unloc x.name) = ident
          then Some (x.args)
          else accu
        ) None signatures
    in
    let contract = get_contract_opt ast (dumloc contract_id) in
    let contract = if (Option.is_none contract) then raise Not_found else Option.get contract in
    let signatures = get_signature contract.signatures fun_id in
    let signatures = if (Option.is_none signatures) then raise Not_found else Option.get signatures in
    List.map fst signatures
end
