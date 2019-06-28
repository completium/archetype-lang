open Ident

type lident = ident Location.loced
[@@deriving show {with_path = false}]

type currency =
  | Tez
  | Mutez
[@@deriving show {with_path = false}]

type container =
  | Collection
  | Partition
[@@deriving show {with_path = false}]

type btyp =
  | Bbool
  | Bint
  | Buint
  | Brational
  | Bdate
  | Bduration
  | Bstring
  | Baddress
  | Brole
  | Bcurrency of currency
  | Bkey
[@@deriving show {with_path = false}]

type vset =
  | VSremoved
  | VSadded
  | VSstable
  | VSbefore
  | VSafter
  | VSfixed
[@@deriving show {with_path = false}]

type trtyp =
  | TRentry
  | TRaction (* add; remove; update *)
  | TRasset
  | TRfield
[@@deriving show {with_path = false}]

type type_ =
  | Tasset of lident
  | Tenum of lident
  | Tcontract of lident
  | Tbuiltin of btyp
  | Tcontainer of type_ * container
  | Toption of type_
  | Ttuple of type_ list
  | Tunit
  | Tentry
  | Tprog of type_
  | Tvset of vset * type_
  | Ttrace of trtyp
[@@deriving show {with_path = false}]

type quantifier =
  | Forall
  | Exists
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
  (* predicates *)
  | Cmaybeperformedonlybyrole
  | Cmaybeperformedonlybyaction
  | Cmaybeperformedbyrole
  | Cmaybeperformedbyaction
[@@deriving show {with_path = false}]

type const_vset =
  | Cbefore
  | Cunmoved
  | Cadded
  | Cremoved
  | Citerated
  | Ctoiterate
[@@deriving show {with_path = false}]

type 'id storage_const_gen =
  | Get        of 'id
  | Add        of 'id
  | Remove     of 'id
  | Clear      of 'id
  | Update     of 'id
  | Sort       of 'id
  | Reverse    of 'id
  | Contains   of 'id
  | Nth        of 'id
  | Select     of 'id
  | Count      of 'id
  | Sum        of 'id * 'id
  | Min        of 'id * 'id
  | Max        of 'id * 'id
[@@deriving show {with_path = false}]

type storage_const = lident storage_const_gen
[@@deriving show {with_path = false}]

type 'id pattern_node =
  | Pwild
  | Pconst of 'id
[@@deriving show {with_path = false}]

type 'id pattern_gen = {
  node: 'id pattern_node;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type pattern = lident pattern_gen
[@@deriving show {with_path = false}]

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

type lit_value =
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

type ('id, 'qualid) qualid_node =
  | Qident of 'id
  | Qdot of 'qualid * 'id
[@@deriving show {with_path = false}]

type 'id qualid_gen = {
  node: ('id, 'id qualid_gen) qualid_node;
  type_: type_;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type qualid = lident qualid_gen
[@@deriving show {with_path = false}]

type 'id var_kind =
  | Vstorevar of 'id
  | Vstorecol of 'id (* asset_name *)
  | Vparam of 'id
  | Vlocal of 'id
  | Vconst of const
[@@deriving show {with_path = false}]

type 'id call_kind =
  | Alocal of 'id
  | Aconst of const
[@@deriving show {with_path = false}]

type ('id, 'term) mterm_node  =
  | Mquantifer   of quantifier * 'id * type_ * 'term
  | Mif          of ('term * 'term * 'term)
  | Mmatchwith   of 'term * ('id pattern_gen * 'term) list
  | Mapplocal    of ('id call_kind * ('id term_arg_gen) list)
  | Mappset      of const_vset * 'id mterm_gen
  | Mappexternal of ('id * 'id * 'id mterm_gen * ('id mterm_gen) list)
  | Mappget      of ('id mterm_gen * 'id mterm_gen)
  | Mappadd      of ('id mterm_gen * 'id mterm_gen)
  | Mappremove   of ('id mterm_gen * 'id mterm_gen)
  | Mappclear    of ('id mterm_gen)
  | Mappupdate
  | Mappreverse  of ('id mterm_gen)
  | Mappsort
  | Mappcontains of ('id mterm_gen * 'id mterm_gen)
  | Mappnth      of ('id mterm_gen * 'id mterm_gen)
  | Mappselect
  | Mappcount    of ('id mterm_gen)
  | Mappsum      of ('id * 'id mterm_gen)
  | Mappmin      of ('id * 'id mterm_gen)
  | Mappmax      of ('id * 'id mterm_gen)
  | Mlogical     of logical_operator * 'term * 'term
  | Mnot         of 'term
  | Mcomp        of comparison_operator * 'term * 'term
  | Marith       of arithmetic_operator * 'term * 'term
  | Muarith      of unary_arithmetic_operator * 'term
  | Mrecord      of 'term list
  | Mletin       of 'id * 'term * type_ option * 'term
  | Mvar         of 'id var_kind
  | Marray       of 'term list
  | Mlit         of lit_value
  | Mdot         of 'term * 'id
  | Mconst       of const
  | Mtuple       of 'term list
  | Mfor         of ('id * 'term * 'term)
  | Mseq         of 'term list
  | Massign      of (assignment_operator * 'id * 'id mterm_gen)
  | Mrequire     of (bool * 'id mterm_gen)
  | Mtransfer    of ('id mterm_gen * bool * 'id qualid_gen option)
  | Mbreak
  | Massert      of 'id mterm_gen
[@@deriving show {with_path = false}]

and 'id mterm_gen = {
  node: ('id, 'id mterm_gen) mterm_node;
  type_: type_;
  subvars: ident list;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

and mterm = lident mterm_gen
[@@deriving show {with_path = false}]

and 'id term_arg_gen =
  | AExpr   of 'id mterm_gen
  | AEffect of ('id * operator * 'id mterm_gen) list
[@@deriving show {with_path = false}]

and term_arg = lident term_arg_gen
[@@deriving show {with_path = false}]

type 'id label_term_gen = {
  label : 'id option;
  term : 'id mterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type label_term = lident label_term_gen
[@@deriving show {with_path = false}]

type 'id item_field_type =
  | FBasic            of btyp
  | FAssetKeys        of btyp * 'id
  | FAssetRecord      of btyp * 'id
  | FRecordCollection of 'id
  | FRecord           of 'id
  | FEnum             of 'id
  | FContainer        of container * 'id item_field_type
[@@deriving show {with_path = false}]

type 'id item_field_gen = {
  asset   : 'id option;
  name    : 'id;
  typ     : 'id item_field_type;
  ghost   : bool;
  default : 'id mterm_gen option; (* initial value *)
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type item_field = lident item_field_gen
[@@deriving show {with_path = false}]

type 'id storage_item_gen = {
  name        : 'id;
  fields      : 'id item_field_gen list;
  invariants  : lident label_term_gen list;
}
[@@deriving show {with_path = false}]

type storage_item = lident storage_item_gen

type 'id storage_gen = 'id storage_item_gen list
[@@deriving show {with_path = false}]

type storage = lident storage_gen
[@@deriving show {with_path = false}]

type 'id enum_item = {
  name: 'id;
  invariants : 'id label_term_gen list;
}
[@@deriving show {with_path = false}]

type 'id enum = {
  name: 'id;
  values: 'id enum_item list;
}
[@@deriving show {with_path = false}]

type 'id record_item_gen = {
  name: 'id;
  type_: type_;
  default: 'id mterm_gen option;
}
[@@deriving show {with_path = false}]

type record_item = lident record_item_gen
[@@deriving show {with_path = false}]

type 'id record_gen = {
  name: 'id;
  key: 'id option;
  values: 'id record_item_gen list;
}
[@@deriving show {with_path = false}]

type record = lident record_gen
[@@deriving show {with_path = false}]

type 'id contract_signature_gen = {
  name : 'id;
  args: type_ list;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type contract_signature = lident contract_signature_gen
[@@deriving show {with_path = false}]

type 'id contract_gen = {
  name       : 'id;
  signatures : 'id contract_signature_gen list;
  init       : 'id mterm_gen option;
  loc        : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type contract = lident contract_gen
[@@deriving show {with_path = false}]

type 'id function_ = {
  name: 'id;
}
[@@deriving show {with_path = false}]

type 'id entry = {
  name: 'id;
}
[@@deriving show {with_path = false}]

type 'id argument_gen = 'id * type_ * 'id mterm_gen option
[@@deriving show {with_path = false}]

type argument = lident argument_gen
[@@deriving show {with_path = false}]

type 'id function_struct_gen = {
  name: 'id;
  args: 'id argument_gen list;
  body: 'id mterm_gen;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type function_struct = lident function_struct_gen
[@@deriving show {with_path = false}]

type 'id function_node_gen =
  | Function           of 'id function_struct_gen * type_ (* fun * return type *)
  | Entry              of 'id function_struct_gen
[@@deriving show {with_path = false}]

type function_node = lident function_node_gen
[@@deriving show {with_path = false}]

type 'id signature_gen = {
  name: 'id;
  args: 'id argument_gen list;
  ret: type_ option;
}
[@@deriving show {with_path = false}]

type signature = lident signature_gen
[@@deriving show {with_path = false}]

type 'id variable_gen = {
  decl         : 'id argument_gen;
  constant     : bool;
  from         : 'id qualid_gen option;
  to_          : 'id qualid_gen option;
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type variable = lident variable_gen
[@@deriving show {with_path = false}]

type 'id predicate_gen = {
  name : 'id;
  args : ('id * ('id mterm_gen)) list;
  body : 'id mterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type predicate = lident predicate_gen
[@@deriving show {with_path = false}]

type 'id definition_gen = {
  name : 'id;
  typ  : type_;
  var  : 'id;
  body : 'id mterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type definition = lident definition_gen
[@@deriving show {with_path = false}]

type 'id invariant_gen = {
  label: 'id;
  formulas: 'id mterm_gen list;
}
[@@deriving show {with_path = false}]

type invariant = lident invariant_gen
[@@deriving show {with_path = false}]


type 'id specification_gen = {
  name: 'id;
  formula: 'id mterm_gen;
  invariants: ('id invariant_gen) list;
}
[@@deriving show {with_path = false}]

type specification = lident specification_gen
[@@deriving show {with_path = false}]


type 'id assert_gen = {
  name: 'id;
  label: 'id;
  formula: 'id mterm_gen;
  invariants: 'id invariant_gen list;
}
[@@deriving show {with_path = false}]

type assert_ = lident assert_gen
[@@deriving show {with_path = false}]

type 'id verification_gen = {
  predicates  : 'id predicate_gen list;
  definitions : 'id definition_gen list;
  axioms      : 'id label_term_gen list;
  theorems    : 'id label_term_gen list;
  variables   : 'id variable_gen list;
  invariants  : ('id * 'id label_term_gen list) list;
  effects     : 'id mterm_gen list;
  specs       : 'id specification_gen list;
  asserts     : 'id assert_gen list;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type verification = lident verification_gen
[@@deriving show {with_path = false}]

type 'id function__gen = {
  node:  'id function_node_gen;
  verif: 'id verification_gen option;
}
[@@deriving show {with_path = false}]

type function__ = lident function__gen
[@@deriving show {with_path = false}]

type 'id decl_node_gen =
  | Denum of 'id enum
  | Drecord of 'id record_gen
  | Dcontract of 'id contract_gen
[@@deriving show {with_path = false}]

type decl_node = lident decl_node_gen
[@@deriving show {with_path = false}]

type 'id api_item_gen =
  | APIStorage of 'id storage_const_gen
[@@deriving show {with_path = false}]

type api_item = lident api_item_gen
[@@deriving show {with_path = false}]

type 'id model_gen = {
  name:          lident;
  api_items:     'id api_item_gen list;
  decls:         'id decl_node_gen list;
  storage:       'id storage_gen;
  functions :    'id function__gen list;
  verification : 'id verification_gen;
}
[@@deriving show {with_path = false}]

type model = lident model_gen
[@@deriving show {with_path = false}]

let mk_qualid ?(loc = Location.dummy) node type_ : 'id qualid_gen =
  { node; type_; loc}

let mk_pattern ?(loc = Location.dummy) node : 'id pattern_gen =
  { node; loc}

let mk_mterm ?(subvars = []) ?(loc = Location.dummy) node type_ : 'id mterm_gen =
  { node; type_; subvars; loc}

let mk_label_term ?label ?(loc = Location.dummy) term : 'id label_term_gen =
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

let mk_verification ?(predicates = []) ?(definitions = []) ?(axioms = []) ?(theorems = []) ?(variables = []) ?(invariants = []) ?(effects = []) ?(specs = []) ?(asserts = []) ?(loc = Location.dummy) () =
  { predicates; definitions; axioms; theorems; variables; invariants; effects; specs; asserts; loc}

let mk_contract_signature ?(args=[]) ?(loc=Location.dummy) name : 'id contract_signature_gen =
  { name; args; loc }

let mk_contract ?(signatures=[]) ?init ?(loc=Location.dummy) name : 'id contract_gen =
  { name; signatures; init; loc }

let mk_enum ?(values = []) name : 'id enum =
  { name; values }

let mk_enum_item ?(invariants = []) name : 'id enum_item =
  { name; invariants }

let mk_record ?(values = []) ?key name : 'id record_gen =
  { name; key; values }

let mk_record_item ?default name type_ : 'id record_item_gen =
  { name; type_; default }

let mk_storage_item ?(fields = []) ?(invariants = []) name : 'id storage_item_gen =
  { name; fields; invariants }

let mk_item_field ?asset ?(ghost = false) ?default ?(loc = Location.dummy) name typ : 'id item_field_gen =
  { asset; name; typ; ghost; default; loc }

let mk_function_struct ?(args = []) ?(loc = Location.dummy) name body : function_struct =
  { name; args; body; loc }

let mk_function ?verif node : 'id function__gen =
  { node; verif }

let mk_signature ?(args = []) ?ret name : 'id signature_gen =
  { name; args; ret}

let mk_model ?(api_items = []) ?(decls = []) ?(functions = []) name storage verification : model =
  { name; api_items; storage; decls; functions; verification}


(* -------------------------------------------------------------------- *)

let map_term_node (f : 'id mterm_gen -> 'id mterm_gen) = function
  | Mquantifer (q, i, t, e) -> Mquantifer (q, i, t, f e)
  | Mif (c, t, e)           -> Mif (f c, f t, f e)
  | Mmatchwith (e, l)       -> Mmatchwith (e, List.map (fun (p, e) -> (p, f e)) l)
  | Mapplocal (e, args)          ->
    Mapplocal (e, List.map (fun (arg : 'id term_arg_gen) -> match arg with
        | AExpr e   -> AExpr (f e)
        | AEffect l -> AEffect (List.map (fun (id, op, e) -> (id, op, f e)) l)) args)
  | Mappset (c, e)          -> Mappset (c, f e)
  | Mappexternal (t, func, c, args) -> Mappexternal (t, func, f c, List.map f args)
  | Mappget (c, k)          -> Mappget (f c, f k)
  | Mappadd (c, i)          -> Mappadd (f c, f i)
  | Mappremove (c, i)       -> Mappremove (f c, f i)
  | Mappclear c             -> Mappclear (f c)
  | Mappupdate              -> Mappupdate
  | Mappreverse c           -> Mappreverse (f c)
  | Mappsort                -> Mappsort
  | Mappcontains (c, i)     -> Mappcontains (f c, f i)
  | Mappnth      (c, i)     -> Mappnth (f c, f i)
  | Mappselect              -> Mappselect
  | Mappcount c             -> Mappcount (f c)
  | Mappsum (fd, c)         -> Mappsum (fd, f c)
  | Mappmin (fd, c)         -> Mappmin (fd, f c)
  | Mappmax (fd, c)         -> Mappmax (fd, f c)
  | Mlogical (op, l, r)     -> Mlogical (op, f l, f r)
  | Mnot e                  -> Mnot (f e)
  | Mcomp (c, l, r)         -> Mcomp (c, f l, f r)
  | Marith (op, l, r)       -> Marith (op, f l, f r)
  | Muarith (op, e)         -> Muarith (op, f e)
  | Mrecord l               -> Mrecord (List.map f l)
  | Mletin (i, a, t, b)     -> Mletin (i, f a, t, f b)
  | Mvar v                  -> Mvar v
  | Marray l                -> Marray (List.map f l)
  | Mlit l                  -> Mlit l
  | Mdot (e, i)             -> Mdot (f e, i)
  | Mconst c                -> Mconst c
  | Mtuple l                -> Mtuple (List.map f l)
  | Mfor (i, c, b)          -> Mfor (i, f c, f b)
  | Mseq is                 -> Mseq (List.map f is)
  | Massign (op, l, r)      -> Massign (op, f l, f r)
  | Mrequire (b, x)         -> Mrequire (b, f x)
  | Mtransfer (x, b, q)     -> Mtransfer (f x, b, q)
  | Mbreak                  -> Mbreak
  | Massert x               -> Massert (f x)

let map_gen_mterm g f (i : 'id mterm_gen) : 'id mterm_gen =
  {
    i with
    node = g f i.node
  }
let map_term  f t = map_gen_mterm map_term_node  f t

let fold_term (f : 'a -> 't -> 'a) (accu : 'a) (term : 'id mterm_gen) =
  match term.node with
  | Mquantifer (_, _, _, e) -> f accu e
  | Mif (c, t, e)           -> f (f (f accu c) t) e
  | Mmatchwith (e, l)       -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Mapplocal (_, args)     -> List.fold_left (fun accu (arg : 'id term_arg_gen) -> match arg with
      | AExpr e -> f accu e
      | AEffect l -> List.fold_left (fun accu (_, _, e) -> f accu e) accu l ) accu args
  | Mappset (c, e)          -> f accu e
  | Mappexternal (t, func, c, args) -> List.fold_left f (f accu c) args
  | Mappget (c, k)          -> f (f accu k) c
  | Mappadd (c, i)          -> f (f accu c) i
  | Mappremove (c, i)       -> f (f accu c) i
  | Mappclear c             -> f accu c
  | Mappupdate              -> accu
  | Mappreverse c           -> f accu c
  | Mappsort                -> accu
  | Mappcontains (c, i)     -> f (f accu c) i
  | Mappnth      (c, i)     -> f (f accu c) i
  | Mappselect              -> accu
  | Mappcount c             -> f accu c
  | Mappsum (fd, c)         -> f accu c
  | Mappmin (fd, c)         -> f accu c
  | Mappmax (fd, c)         -> f accu c
  | Mlogical (_, l, r)      -> f (f accu l) r
  | Mnot e                  -> f accu e
  | Mcomp (_, l, r)         -> f (f accu l) r
  | Marith (_, l, r)        -> f (f accu l) r
  | Muarith (_, e)          -> f accu e
  | Mrecord l               -> List.fold_left f accu l
  | Mletin (_, a, _, b)     -> f (f accu a) b
  | Mvar _                  -> accu
  | Marray l                -> List.fold_left f accu l
  | Mlit _                  -> accu
  | Mdot (e, _)             -> f accu e
  | Mconst _                -> accu
  | Mtuple l                -> List.fold_left f accu l
  | Mfor (i, c, b)          -> f accu b
  | Mseq is                 -> List.fold_left f accu is
  | Massign (_, _, e)       -> f accu e
  | Mtransfer (x, _, _)     -> f accu x
  | Mrequire (_, x)         -> f accu x
  | Mbreak                  -> accu
  | Massert x               -> f accu x

let fold_map_term
    (g : ('id, 'term) mterm_node -> 'term)
    (f : 'a -> 'id mterm_gen -> 'id mterm_gen * 'a)
    (accu : 'a)
    (term : 'id mterm_gen) : 'term * 'a =
  match term.node with
  | Mquantifer (q, id, t, e) ->
    let ee, ea = f accu e in
    g (Mquantifer (q, id, t, ee)), ea

  | Mif (c, t, e) ->
    let ce, ca = f accu c in
    let ti, ta = f ca t in
    let ei, ea = f ta e in
    g (Mif (ce, ti, ei)), ea

  | Mmatchwith (e, l) ->
    let ee, ea = f accu e in
    let (pse, psa) =
      List.fold_left
        (fun (ps, accu) (p, i) ->
           let pa, accu = f accu i in
           [(p, i)] @ ps, accu) ([], ea) l
    in

    g (Mmatchwith (ee, l)), psa

  | Mapplocal (id, args) ->
    let ((argss, argsa) : 'c list * 'a) =
      List.fold_left
        (fun (pterms, accu) (x : 'id term_arg_gen) ->
           let p, accu =
             match x with
             | AExpr a -> f accu a |> fun (x, acc) -> (Some (AExpr x), acc)
             | _ -> None, accu in
           let x = match p with | Some a -> a | None -> x in
           pterms @ [x], accu) ([], accu) args
    in
    g (Mapplocal (id, argss)), argsa

  | Mappset (c, e) ->
    let ee, ea = f accu e in
    g (Mappset (c, ee)), ea

  | Mappexternal (t, func, c, args) ->
    let ce, ca = f accu c in
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], ca) args in
    g (Mappexternal (t, func, ce, lp)), la

  | Mappget (c, k) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    g (Mappget (ce, ke)), ka

  | Mappadd (c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mappadd (ce, ie)), ia

  | Mappremove (c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mappremove (ce, ie)), ia

  | Mappclear c ->
    let ce, ca = f accu c in
    g (Mappclear ce), ca

  | Mappupdate ->
    g Mappupdate, accu

  | Mappreverse c ->
    let ce, ca = f accu c in
    g (Mappreverse ce), ca

  | Mappsort ->
    g Mappsort, accu

  | Mappcontains (c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mappcontains (ce, ie)), ia

  | Mappnth (c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mappnth (ce, ie)), ia

  | Mappselect ->
    g Mappselect, accu

  | Mappcount c ->
    let ce, ca = f accu c in
    g (Mappcount ce), ca

  | Mappsum (fd, c) ->
    let ce, ca = f accu c in
    g (Mappsum (fd, ce)), ca

  | Mappmin (fd, c) ->
    let ce, ca = f accu c in
    g (Mappmin (fd, ce)), ca

  | Mappmax (fd, c) ->
    let ce, ca = f accu c in
    g (Mappmax (fd, ce)), ca

  | Mlogical (op, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mlogical (op, le, re)), ra

  | Mnot e ->
    let ee, ea = f accu e in
    g (Mnot ee), ea

  | Mcomp (op, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mcomp (op, le, re)), ra

  | Marith (op, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Marith (op, le, re)), ra

  | Muarith (op, e) ->
    let ee, ea = f accu e in
    g (Muarith (op, ee)), ea

  | Mrecord l ->
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) l in
    g (Mrecord lp), la

  | Mletin (id, i, t, o) ->
    let ie, ia = f accu i in
    let oe, oa = f ia o in
    g (Mletin (id, i, t, oe)), oa

  | Mvar id ->
    g (Mvar id), accu

  | Marray l ->
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) l in
    g (Marray lp), la

  | Mlit l ->
    g (Mlit l), accu

  | Mdot (e, id) ->
    let ee, ea = f accu e in
    g (Mdot (ee, id)), ea

  | Mconst c ->
    g (Mconst c), accu

  | Mtuple l ->
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) l in
    g (Mtuple lp), la

  | Mfor (i, c, b) ->
    let ce, ca = f accu c in
    let bi, ba = f ca b in
    g (Mfor (i, ce, bi)), ba

  | Mseq is ->
    let (isi, isa) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) is in
    g (Mseq isi), isa

  | Massign (op, id, x) ->
    let xe, xa = f accu x in
    g (Massign (op, id, xe)), xa

  | Mrequire (b, x) ->
    let xe, xa = f accu x in
    g (Mrequire (b, xe)), xa

  | Mtransfer (x, b, q) ->
    let xe, xa = f accu x in
    g (Mtransfer (xe, b, q)), xa

  | Mbreak ->
    g (Mbreak), accu

  | Massert x ->
    let xe, xa = f accu x in
    g (Massert xe), xa

(* -------------------------------------------------------------------- *)
module Utils : sig

  val function_name_from_function_node : function_node -> string
  val function_name_from_storage_node  : storage_const -> string
  val get_records                      : model -> record list
  val get_storage                      : model -> storage
  val get_record                       : model -> lident -> record
  val get_record_field                 : model -> (lident * lident) -> record_item
  val get_record_key                   : model -> lident -> (lident * btyp)
  val is_storage_attribute             : model -> lident -> bool
  val get_named_field_list             : model -> lident -> 'a list -> (lident * 'a) list
  val get_partitions                   : model -> (lident * record_item) list (* record id, record item *)
  val dest_partition                   : type_ -> lident
  val get_partition_record_key         : model -> lident -> lident -> (lident * lident * btyp)
  val get_entries                      : model -> (verification option * function_struct) list

end = struct

  open Tools
  open Location

  exception Anomaly of string

  type error_desc =
    | RecordNotFound of string
    | RecordFieldNotFound of string * string
    | RecordKeyTypeNotFound of string
    | NotaPartition
    | PartitionNotFound
  [@@deriving show {with_path = false}]

  let emit_error (desc : error_desc) =
    let str = Format.asprintf "%a@." pp_error_desc desc in
    raise (Anomaly str)

  let lident_to_string lident = Location.unloc lident

  let function_name_from_function_node = function
    | Function (fs, _)    -> lident_to_string fs.name
    | Entry     fs        -> lident_to_string fs.name

  let function_name_from_storage_node = function
    | Get       aid       -> "get_"      ^ lident_to_string aid
    | Add       aid       -> "add_"      ^ lident_to_string aid
    | Remove    aid       -> "remove_"   ^ lident_to_string aid
    | Clear     aid       -> "clear_"    ^ lident_to_string aid
    | Update    aid       -> "update_"   ^ lident_to_string aid
    | Sort      aid       -> "sort_"     ^ lident_to_string aid
    | Reverse   aid       -> "reverse_"  ^ lident_to_string aid
    | Contains  aid       -> "contains_" ^ lident_to_string aid
    | Select    aid       -> "select_"   ^ lident_to_string aid
    | Nth       aid       -> "nth_"      ^ lident_to_string aid
    | Count     aid       -> "count_"    ^ lident_to_string aid
    | Sum      (aid, fid) -> "sum_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
    | Min      (aid, fid) -> "min_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
    | Max      (aid, fid) -> "max_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid

  let is_record (d : decl_node) : bool =
    match d with
    | Drecord _ -> true
    | _          -> false

  let is_entry (f : function__) : bool =
    match f with
    | { node = Entry _; verif = _ } -> true
    | _                             -> false

  let get_entry (f : function__) : verification option * function_struct =
    match f with
    | { node = Entry s; verif = v } -> (v,s)
    | _                             -> assert false

  let get_entries m = List.filter is_entry m.functions |> List.map get_entry

  let dest_record  = function
    | Drecord r -> r
    | _ -> emit_error NotaPartition

  let get_records m = m.decls |> List.filter is_record |> List.map dest_record

  let get_record model record_name : record =
    let id = unloc record_name in
    let res = List.fold_left (fun accu (x : decl_node) ->
        match x with
        | Drecord r when String.equal (unloc record_name) (unloc r.name) -> Some r
        | _ -> accu
      ) None model.decls in
    match res with
    | Some v -> v
    | _ -> emit_error (RecordNotFound id)

  let get_partitions m : (lident * record_item) list=
    get_records m |> List.fold_left (fun acc (record : record) ->
        acc @ (List.fold_left (fun acc (ritem : record_item) ->
            match ritem.type_ with
            | Tcontainer (Tasset _, Partition) ->
              acc @ [record.name,ritem]
            | _ -> acc
          ) [] record.values)
      ) []

  let dest_partition = function
    | Tcontainer (Tasset p,Partition) -> p
    | _ -> assert false

  let get_record_field model (record_name, field_name) =
    let record = get_record model record_name in
    let res = List.fold_left (fun accu (x : record_item) ->
        if String.equal (unloc field_name) (unloc x.name) then
          Some x
        else accu) None record.values in
    match res with
    | Some v -> v
    | _ -> emit_error (RecordFieldNotFound (unloc record_name, unloc field_name))

  let get_record_key model record_name : (lident * btyp) =
    let record = get_record model record_name in
    let key_id = Option.get record.key in
    let key_field = get_record_field model (record_name, key_id) in
    match key_field.type_ with
    | Tbuiltin v -> (key_id, v)
    | _ -> emit_error (RecordKeyTypeNotFound (unloc record_name))

  (* returns : asset name, key name, key type *)
  let get_partition_record_key model record field : (lident * lident * btyp) =
    let partitions = get_partitions model in
    let rec rec_get = function
      | (r,(ri : record_item)) :: tl when compare r.pldesc record.pldesc = 0 &&
                                          compare ri.name.pldesc field.pldesc = 0 ->
        let pa  = dest_partition ri.type_ in
        let k,t = get_record_key model pa in
        (pa,k,t)
      | _ :: tl -> rec_get tl
      | _ -> emit_error (PartitionNotFound) in
    rec_get partitions


  let get_storage model =
    model.storage

  let is_storage_attribute model id =
    let s = get_storage model in
    let items = s in
    (List.fold_left (fun accu (x : storage_item) ->
         accu || String.equal (Location.unloc id) (Location.unloc x.name)) false items)

  let get_field_list model record_name =
    let record = get_record model record_name in
    List.map (fun (x : record_item) -> x.name) record.values

  let get_named_field_list ast asset_name list =
    let field_list = get_field_list ast asset_name in
    (* List.iter (fun x -> Format.eprintf "f1: %s@." (unloc x)) field_list;
       List.iter (fun x -> Format.eprintf "f2: %a@." pp_pterm x) list;
       Format.eprintf "lf1: %d@." (List.length field_list);
       Format.eprintf "lf2: %d@." (List.length list); *)
    List.map2 (fun x y -> x, y) field_list list
end
