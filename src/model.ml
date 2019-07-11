open Ident
open Tools

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

type 'id storage_const_gen =
  | Get              of 'id
  | Set              of 'id
  | Add              of 'id
  | Remove           of 'id
  | Clear            of 'id
  | Reverse          of 'id
  | UpdateAdd        of 'id * 'id
  | UpdateRemove     of 'id * 'id
  | UpdateClear      of 'id * 'id
  | UpdateReverse    of 'id * 'id
[@@deriving show {with_path = false}]

type storage_const = lident storage_const_gen
[@@deriving show {with_path = false}]

type 'id container_const_gen =
  | Add      of type_
  | Remove   of type_
  | Clear    of type_
  | Reverse  of type_
[@@deriving show {with_path = false}]

type container_const = lident container_const_gen
[@@deriving show {with_path = false}]

type 'id function_const_gen =
  | Select           of 'id
  | Sort             of 'id * 'id
  | Contains         of 'id
  | Nth              of 'id
  | Count            of 'id
  | Sum              of 'id * 'id
  | Min              of 'id * 'id
  | Max              of 'id * 'id
[@@deriving show {with_path = false}]

type function_const = lident function_const_gen
[@@deriving show {with_path = false}]

type builtin_const =
  | Min of type_
  | Max of type_
[@@deriving show {with_path = false}]

type 'id api_item_gen_node =
  | APIStorage   of 'id storage_const_gen
  | APIContainer of 'id container_const_gen
  | APIFunction  of 'id function_const_gen
  | APIBuiltin   of builtin_const
[@@deriving show {with_path = false}]

type api_item_node = lident api_item_gen_node
[@@deriving show {with_path = false}]

type 'id api_item_gen = {
  node: 'id api_item_gen_node;
  only_formula: bool;
}
[@@deriving show {with_path = false}]

type api_item = lident api_item_gen
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

type assignment_operator =
  | ValueAssign
  | PlusAssign
  | MinusAssign
  | MultAssign
  | DivAssign
  | AndAssign
  | OrAssign
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

type sort_kind =
  | SKasc
  | SKdesc
[@@deriving show {with_path = false}]

type ('id, 'term) mterm_node  =
  | Mif           of ('term * 'term * 'term)
  | Mmatchwith    of 'term * ('id pattern_gen * 'term) list
  | Mapp          of 'id * 'term list
  | Mexternal     of 'id * 'id * 'term * ('term) list
  | Mget          of 'term * 'term
  | Mset          of 'term * 'term * 'term
  | Maddasset     of ident * 'term * 'term
  | Maddfield     of ident * ident * 'term * 'term (* asset_name * field_name * asset instance * item *)
  | Maddlocal     of 'term * 'term
  | Mremoveasset  of ident * 'term * 'term
  | Mremovefield  of ident * ident * 'term * 'term
  | Mremovelocal  of 'term * 'term
  | Mclearasset   of ident * 'term
  | Mclearfield   of ident * ident * 'term
  | Mclearlocal   of 'term
  | Mreverseasset of ident * 'term
  | Mreversefield of ident * ident * 'term
  | Mreverselocal of 'term
  | Mselect       of ident * 'term * 'term
  | Msort         of ident * 'term * ident * sort_kind
  | Mcontains     of ident * 'term * 'term
  | Mnth          of ident * 'term * 'term
  | Mcount        of ident * 'term
  | Msum          of ident * 'id * 'term
  | Mmin          of ident * 'id * 'term
  | Mmax          of ident * 'id * 'term
  | Mmathmax      of 'term * 'term
  | Mmathmin      of 'term * 'term
  | Mfail         of 'term
  | Mand          of 'term * 'term
  | Mor           of 'term * 'term
  | Mimply        of 'term * 'term
  | Mequiv        of 'term * 'term
  | Mnot          of 'term
  | Mequal        of 'term * 'term
  | Mnequal       of 'term * 'term
  | Mgt           of 'term * 'term
  | Mge           of 'term * 'term
  | Mlt           of 'term * 'term
  | Mle           of 'term * 'term
  | Mplus         of 'term * 'term
  | Mminus        of 'term * 'term
  | Mmult         of 'term * 'term
  | Mdiv          of 'term * 'term
  | Mmodulo       of 'term * 'term
  | Muplus        of 'term
  | Muminus       of 'term
  | Mrecord       of 'term list
  | Mletin        of 'id * 'term * type_ option * 'term
  | Mvarstorevar  of 'id
  | Mvarstorecol  of 'id
  | Mvarenumval   of 'id
  | Mvarlocal     of 'id
  | Mvarfield     of 'id
  | Mvarthe
  | Mstate
  | Mnow
  | Mtransferred
  | Mcaller
  | Mbalance
  | Marray        of 'term list
  | Mint          of Core.big_int
  | Muint         of Core.big_int
  | Mbool         of bool
  | Menum         of string
  | Mrational     of Core.big_int * Core.big_int
  | Mdate         of string
  | Mstring       of string
  | Mcurrency     of Core.big_int * currency
  | Maddress      of string
  | Mduration     of string
  | Mdot          of 'term * 'id
  | Mtuple        of 'term list
  | Mfor          of ('id * 'term * 'term)
  | Mseq          of 'term list
  | Massign       of (assignment_operator * 'id * 'term)
  | Massignfield  of (assignment_operator * 'id * 'id * 'term)
  | Mrequire      of (bool * 'term)
  | Mtransfer     of ('term * bool * 'id qualid_gen option)
  | Mbreak
  | Massert       of 'term
  | Mreturn       of 'term
  (* quantifiers *)
  | Mforall       of 'id * type_ * 'term
  | Mexists       of 'id * type_ * 'term
  (* security predicates *)
  | Msetbefore    of 'term
  | Msetunmoved   of 'term
  | Msetadded     of 'term
  | Msetremoved   of 'term
  | Msetiterated  of 'term
  | Msettoiterate of 'term
  (* security predicates *)
  | MsecMayBePerformedOnlyByRole   of 'term * 'term
  | MsecMayBePerformedOnlyByAction of 'term * 'term
  | MsecMayBePerformedByRole       of 'term * 'term
  | MsecMayBePerformedByAction     of 'term * 'term
  | MsecTransferredBy              of 'term
  | MsecTransferredTo              of 'term
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

and mterm__node = (lident, mterm) mterm_node
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

type 'id model_gen = {
  name         : lident;
  api_items    : 'id api_item_gen list;
  decls        : 'id decl_node_gen list;
  storage      : 'id storage_gen;
  functions    : 'id function__gen list;
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
  { name; args; ret }

let mk_api_item ?(only_formula = false) node =
  { node; only_formula }

let mk_model ?(api_items = []) ?(decls = []) ?(functions = []) name storage verification : model =
  { name; api_items; storage; decls; functions; verification}


(* -------------------------------------------------------------------- *)

let map_term_node (ctx : 'c) (f : 'c -> 'id mterm_gen -> 'id mterm_gen) = function
  | Mif (c, t, e)                -> Mif (f ctx c, f ctx t, f ctx e)
  | Mmatchwith (e, l)            -> Mmatchwith (e, List.map (fun (p, e) -> (p, f ctx e)) l)
  | Mapp (e, args)               -> Mapp (e, List.map (f ctx) args)
  | Msetbefore    e              -> Msetbefore    (f ctx e)
  | Msetunmoved   e              -> Msetunmoved   (f ctx e)
  | Msetadded     e              -> Msetadded     (f ctx e)
  | Msetremoved   e              -> Msetremoved   (f ctx e)
  | Msetiterated  e              -> Msetiterated  (f ctx e)
  | Msettoiterate e              -> Msettoiterate (f ctx e)
  | Mexternal (t, func, c, args) -> Mexternal (t, func, f ctx c, List.map (f ctx) args)
  | Mget (c, k)                  -> Mget (f ctx c, f ctx k)
  | Mset (c, k, v)               -> Mset (f ctx c, f ctx k, f ctx v)
  | Maddasset (an, c, i)         -> Maddasset (an,f ctx c, f ctx i)
  | Maddfield (an, fn, c, i)     -> Maddfield (an, fn, f ctx c, f ctx i)
  | Maddlocal (c, i)             -> Maddlocal (f ctx c, f ctx i)
  | Mremoveasset (an, c, i)      -> Mremoveasset (an,f ctx c, f ctx i)
  | Mremovefield (an, fn, c, i)  -> Mremovefield (an, fn, f ctx c, f ctx i)
  | Mremovelocal (c, i)          -> Mremovelocal (f ctx c, f ctx i)
  | Mclearasset (an, i)          -> Mclearasset (an, f ctx i)
  | Mclearfield (an, fn, i)      -> Mclearfield (an, fn, f ctx i)
  | Mclearlocal (i)              -> Mclearlocal (f ctx i)
  | Mreverseasset (an, i)        -> Mreverseasset (an, f ctx i)
  | Mreversefield (an, fn, i)    -> Mreversefield (an, fn, f ctx i)
  | Mreverselocal (i)            -> Mreverselocal (f ctx i)
  | Mselect (an, c, p)           -> Mselect (an, f ctx c, f ctx p)
  | Msort (an, c, fn, k)         -> Msort (an, f ctx c, fn, k)
  | Mcontains (an, c, i)         -> Mcontains (an, f ctx c, f ctx i)
  | Mnth (an, c, i)              -> Mnth (an, f ctx c, f ctx i)
  | Mcount (an, c)               -> Mcount (an, f ctx c)
  | Msum (an, fd, c)             -> Msum (an, fd, f ctx c)
  | Mmin (an, fd, c)             -> Mmin (an, fd, f ctx c)
  | Mmax (an, fd, c)             -> Mmax (an, fd, f ctx c)
  | Mfail (msg)                  -> Mfail (f ctx msg)
  | Mmathmin (l, r)              -> Mmathmin (f ctx l, f ctx r)
  | Mmathmax (l, r)             -> Mmathmax (f ctx l, f ctx r)
  | Mand (l, r)                  -> Mand (f ctx l, f ctx r)
  | Mor (l, r)                   -> Mor (f ctx l, f ctx r)
  | Mimply (l, r)                -> Mimply (f ctx l, f ctx r)
  | Mequiv  (l, r)               -> Mequiv (f ctx l, f ctx r)
  | Mnot e                       -> Mnot (f ctx e)
  | Mequal (l, r)                -> Mequal (f ctx l, f ctx r)
  | Mnequal (l, r)               -> Mnequal (f ctx l, f ctx r)
  | Mgt (l, r)                   -> Mgt (f ctx l, f ctx r)
  | Mge (l, r)                   -> Mge (f ctx l, f ctx r)
  | Mlt (l, r)                   -> Mlt (f ctx l, f ctx r)
  | Mle (l, r)                   -> Mle (f ctx l, f ctx r)
  | Mplus (l, r)                 -> Mplus (f ctx l, f ctx r)
  | Mminus (l, r)                -> Mminus (f ctx l, f ctx r)
  | Mmult (l, r)                 -> Mmult (f ctx l, f ctx r)
  | Mdiv (l, r)                  -> Mdiv (f ctx l, f ctx r)
  | Mmodulo (l, r)               -> Mmodulo (f ctx l, f ctx r)
  | Muplus e                     -> Muplus (f ctx e)
  | Muminus e                    -> Muminus (f ctx e)
  | Mrecord l                    -> Mrecord (List.map (f ctx) l)
  | Mletin (i, a, t, b)          -> Mletin (i, f ctx a, t, f ctx b)
  | Mvarstorevar v               -> Mvarstorevar v
  | Mvarstorecol v               -> Mvarstorecol v
  | Mvarenumval v                -> Mvarenumval  v
  | Mvarfield v                  -> Mvarfield    v
  | Mvarlocal v                  -> Mvarlocal    v
  | Mvarthe                      -> Mvarthe
  | Mstate                       -> Mstate
  | Mnow                         -> Mnow
  | Mtransferred                 -> Mtransferred
  | Mcaller                      -> Mcaller
  | Mbalance                     -> Mbalance
  | Marray l                     -> Marray (List.map (f ctx) l)
  | Mint v                       -> Mint v
  | Muint v                      -> Muint v
  | Mbool v                      -> Mbool v
  | Menum v                      -> Menum v
  | Mrational (n, d)             -> Mrational (n, d)
  | Mdate v                      -> Mdate v
  | Mstring v                    -> Mstring v
  | Mcurrency (v, c)             -> Mcurrency (v, c)
  | Maddress v                   -> Maddress v
  | Mduration v                  -> Mduration v
  | Mdot (e, i)                  -> Mdot (f ctx e, i)
  | Mtuple l                     -> Mtuple (List.map (f ctx) l)
  | Mfor (i, c, b)               -> Mfor (i, f ctx c, f ctx b)
  | Mseq is                      -> Mseq (List.map (f ctx) is)
  | Massign (op, l, r)           -> Massign (op, l, f ctx r)
  | Massignfield (op, a, fi, r)  -> Massignfield (op, a, fi, f ctx r)
  | Mrequire (b, x)              -> Mrequire (b, f ctx x)
  | Mtransfer (x, b, q)          -> Mtransfer (f ctx x, b, q)
  | Mbreak                       -> Mbreak
  | Massert x                    -> Massert (f ctx x)
  | Mreturn x                    -> Mreturn (f ctx x)
  | Mforall (i, t, e)            -> Mforall (i, t, f ctx e)
  | Mexists (i, t, e)            -> Mexists (i, t, f ctx e)
  | MsecMayBePerformedOnlyByRole   (l, r) -> MsecMayBePerformedOnlyByRole   (f ctx l, f ctx r)
  | MsecMayBePerformedOnlyByAction (l, r) -> MsecMayBePerformedOnlyByAction (f ctx l, f ctx r)
  | MsecMayBePerformedByRole       (l, r) -> MsecMayBePerformedByRole       (f ctx l, f ctx r)
  | MsecMayBePerformedByAction     (l, r) -> MsecMayBePerformedByAction     (f ctx l, f ctx r)
  | MsecTransferredBy              a      -> MsecTransferredBy              (f ctx a)
  | MsecTransferredTo              a      -> MsecTransferredTo              (f ctx a)

let map_gen_mterm g f ctx (i : 'id mterm_gen) : 'id mterm_gen =
  {
    i with
    node = g f ctx i.node
  }
let map_term f ctx t =
  map_gen_mterm map_term_node f ctx t

let fold_term (f : 'a -> ('id mterm_gen) -> 'a) (accu : 'a) (term : 'id mterm_gen) : 'a =
  match term.node with
  | Mif (c, t, e)                         -> f (f (f accu c) t) e
  | Mmatchwith (e, l)                     -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Mapp (_, args)                        -> List.fold_left f accu args
  | Msetbefore    e                       -> f accu e
  | Msetunmoved   e                       -> f accu e
  | Msetadded     e                       -> f accu e
  | Msetremoved   e                       -> f accu e
  | Msetiterated  e                       -> f accu e
  | Msettoiterate e                       -> f accu e
  | Mexternal (t, func, c, args)          -> List.fold_left f (f accu c) args
  | Mget (c, k)                           -> f (f accu k) c
  | Mset (c, k, v)                        -> f (f (f accu v) k) c
  | Maddasset (an, c, i)                  -> f (f accu c) i
  | Maddfield (an, fn, c, i)              -> f (f accu c) i
  | Maddlocal (c, i)                      -> f (f accu c) i
  | Mremoveasset (an, c, i)               -> f (f accu c) i
  | Mremovefield (an, fn, c, i)           -> f (f accu c) i
  | Mremovelocal (c, i)                   -> f (f accu c) i
  | Mclearasset (an, c)                   -> f accu c
  | Mclearfield (an, fn, c)               -> f accu c
  | Mclearlocal (c)                       -> f accu c
  | Mreverseasset (an, c)                 -> f accu c
  | Mreversefield (an, fn, c)             -> f accu c
  | Mreverselocal (c)                     -> f accu c
  | Mselect (an, c, p)                    -> f (f accu c) p
  | Msort (an, c, p, _)                   -> f accu c
  | Mcontains (an, c, i)                  -> f (f accu c) i
  | Mnth      (an, c, i)                  -> f (f accu c) i
  | Mcount (an, c)                        -> f accu c
  | Msum (an, fd, c)                      -> f accu c
  | Mmin (an, fd, c)                      -> f accu c
  | Mmax (an, fd, c)                      -> f accu c
  | Mfail (msg)                           -> f accu msg
  | Mmathmax (l, r)                       -> f (f accu l) r
  | Mmathmin (l, r)                       -> f (f accu l) r
  | Mand (l, r)                           -> f (f accu l) r
  | Mor (l, r)                            -> f (f accu l) r
  | Mimply (l, r)                         -> f (f accu l) r
  | Mequiv  (l, r)                        -> f (f accu l) r
  | Mnot e                                -> f accu e
  | Mequal (l, r)                         -> f (f accu l) r
  | Mnequal (l, r)                        -> f (f accu l) r
  | Mgt (l, r)                            -> f (f accu l) r
  | Mge (l, r)                            -> f (f accu l) r
  | Mlt (l, r)                            -> f (f accu l) r
  | Mle (l, r)                            -> f (f accu l) r
  | Mplus (l, r)                          -> f (f accu l) r
  | Mminus (l, r)                         -> f (f accu l) r
  | Mmult (l, r)                          -> f (f accu l) r
  | Mdiv (l, r)                           -> f (f accu l) r
  | Mmodulo (l, r)                        -> f (f accu l) r
  | Muplus e                              -> f accu e
  | Muminus e                             -> f accu e
  | Mrecord l                             -> List.fold_left f accu l
  | Mletin (_, a, _, b)                   -> f (f accu a) b
  | Mvarstorevar _                        -> accu
  | Mvarstorecol _                        -> accu
  | Mvarenumval _                         -> accu
  | Mvarfield _                           -> accu
  | Mvarlocal _                           -> accu
  | Mvarthe                               -> accu
  | Marray l                              -> List.fold_left f accu l
  | Mint _                                -> accu
  | Muint _                               -> accu
  | Mbool _                               -> accu
  | Menum _                               -> accu
  | Mrational _                           -> accu
  | Mdate _                               -> accu
  | Mstring _                             -> accu
  | Mcurrency _                           -> accu
  | Maddress _                            -> accu
  | Mduration _                           -> accu
  | Mdot (e, _)                           -> f accu e
  | Mstate                                -> accu
  | Mnow                                  -> accu
  | Mtransferred                          -> accu
  | Mcaller                               -> accu
  | Mbalance                              -> accu
  | Mtuple l                              -> List.fold_left f accu l
  | Mfor (i, c, b)                        -> f accu b
  | Mseq is                               -> List.fold_left f accu is
  | Massign (_, _, e)                     -> f accu e
  | Massignfield (_, _, _, e)             -> f accu e
  | Mtransfer (x, _, _)                   -> f accu x
  | Mrequire (_, x)                       -> f accu x
  | Mbreak                                -> accu
  | Massert x                             -> f accu x
  | Mreturn x                             -> f accu x
  | Mforall (_, _, e)                     -> f accu e
  | Mexists (_, _, e)                     -> f accu e
  | MsecMayBePerformedOnlyByRole   (l, r) -> f (f accu l) r
  | MsecMayBePerformedOnlyByAction (l, r) -> f (f accu l) r
  | MsecMayBePerformedByRole       (l, r) -> f (f accu l) r
  | MsecMayBePerformedByAction     (l, r) -> f (f accu l) r
  | MsecTransferredBy              a      -> f accu a
  | MsecTransferredTo              a      -> f accu a

let fold_map_term
    (g : ('id, 'term) mterm_node -> 'term)
    (f : 'a -> 'id mterm_gen -> 'id mterm_gen * 'a)
    (accu : 'a)
    (term : 'id mterm_gen) : 'term * 'a =
  match term.node with
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

  | Mapp (id, args) ->
    let ((argss, argsa) : 'c list * 'a) =
      List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) args
    in
    g (Mapp (id, argss)), argsa

  | Msetbefore e ->
    let ee, ea = f accu e in
    g (Msetbefore ee), ea

  | Msetunmoved e ->
    let ee, ea = f accu e in
    g (Msetunmoved ee), ea

  | Msetadded e ->
    let ee, ea = f accu e in
    g (Msetadded ee), ea

  | Msetremoved e ->
    let ee, ea = f accu e in
    g (Msetremoved ee), ea

  | Msetiterated e ->
    let ee, ea = f accu e in
    g (Msetiterated ee), ea

  | Msettoiterate e ->
    let ee, ea = f accu e in
    g (Msettoiterate ee), ea

  | Mexternal (t, func, c, args) ->
    let ce, ca = f accu c in
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], ca) args in
    g (Mexternal (t, func, ce, lp)), la

  | Mget (c, k) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    g (Mget (ce, ke)), ka

  | Mset (c, k, v) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    let ve, va = f ka v in
    g (Mset (ce, ke, ve)), ka

  | Maddasset (an, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Maddasset (an, ce, ie)), ia

  | Maddfield (an, fn, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Maddfield (an, fn, ce, ie)), ia

  | Maddlocal (c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Maddlocal (ce, ie)), ia

  | Mremoveasset (an, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mremoveasset (an, ce, ie)), ia

  | Mremovefield (an, fn, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mremovefield (an, fn, ce, ie)), ia

  | Mremovelocal (c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mremovelocal (ce, ie)), ia

  | Mclearasset (an, i) ->
    let ie, ia = f accu i in
    g (Mclearasset (an, ie)), ia

  | Mclearfield (an, fn, i) ->
    let ie, ia = f accu i in
    g (Mclearfield (an, fn, ie)), ia

  | Mclearlocal i ->
    let ie, ia = f accu i in
    g (Mclearlocal (ie)), ia

  | Mreverseasset (an, i) ->
    let ie, ia = f accu i in
    g (Mreverseasset (an, ie)), ia

  | Mreversefield (an, fn, i) ->
    let ie, ia = f accu i in
    g (Mreversefield (an, fn, ie)), ia

  | Mreverselocal i ->
    let ie, ia = f accu i in
    g (Mreverselocal (ie)), ia

  | Mselect (an, c, p) ->
    let ce, ca = f accu c in
    let pe, pa = f ca p in
    g (Mselect (an, ce, pe)), pa

  | Msort (an, c, fi, k) ->
    let ce, ca = f accu c in
    g (Msort (an, ce, fi, k)), ca

  | Mcontains (an, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mcontains (an, ce, ie)), ia

  | Mnth (an, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mnth (an, ce, ie)), ia

  | Mcount (an, c) ->
    let ce, ca = f accu c in
    g (Mcount (an, ce)), ca

  | Msum (an, fd, c) ->
    let ce, ca = f accu c in
    g (Msum (an, fd, ce)), ca

  | Mmin (an, fd, c) ->
    let ce, ca = f accu c in
    g (Mmin (an, fd, ce)), ca

  | Mmax (an, fd, c) ->
    let ce, ca = f accu c in
    g (Mmax (an, fd, ce)), ca

  | Mfail (msg) ->
    let msge, msga = f accu msg in
    g (Mfail (msge)), msga

  | Mmathmax (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mmathmax (le, re)), ra

  | Mmathmin (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mmathmin (le, re)), ra

  | Mand (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mand (le, re)), ra

  | Mor (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mor (le, re)), ra

  | Mimply (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mimply (le, re)), ra

  | Mequiv  (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mequiv (le, re)), ra

  | Mnot e ->
    let ee, ea = f accu e in
    g (Mnot ee), ea

  | Mequal (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mequal (le, re)), ra

  | Mnequal (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mnequal (le, re)), ra

  | Mgt (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mgt (le, re)), ra

  | Mge (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mge (le, re)), ra

  | Mlt (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mlt (le, re)), ra

  | Mle (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mle (le, re)), ra

  | Mplus (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mplus (le, re)), ra

  | Mminus (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mminus (le, re)), ra

  | Mmult (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mmult (le, re)), ra

  | Mdiv (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mdiv (le, re)), ra

  | Mmodulo (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mmodulo (le, re)), ra

  | Muplus e ->
    let ee, ea = f accu e in
    g (Muplus ee), ea

  | Muminus e ->
    let ee, ea = f accu e in
    g (Muminus ee), ea

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

  | Mvarstorevar v ->
    g (Mvarstorevar v), accu

  | Mvarstorecol v ->
    g (Mvarstorecol v), accu

  | Mvarenumval v ->
    g (Mvarenumval v), accu

  | Mvarfield v ->
    g (Mvarfield v), accu

  | Mvarlocal v ->
    g (Mvarlocal v), accu

  | Mvarthe ->
    g Mvarthe, accu

  | Marray l ->
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) l in
    g (Marray lp), la

  | Mint v                   -> g (Mint v), accu
  | Muint v                  -> g (Muint v), accu
  | Mbool v                  -> g (Mbool v), accu
  | Menum v                  -> g (Menum v), accu
  | Mrational (n, d)         -> g (Mrational (n, d)), accu
  | Mdate v                  -> g (Mdate v), accu
  | Mstring v                -> g (Mstring v), accu
  | Mcurrency (v, c)         -> g (Mcurrency (v, c)), accu
  | Maddress v               -> g (Maddress v), accu
  | Mduration v              -> g (Mduration v), accu

  | Mdot (e, id) ->
    let ee, ea = f accu e in
    g (Mdot (ee, id)), ea

  | Mstate ->
    g Mstate, accu

  | Mnow ->
    g Mnow, accu

  | Mtransferred ->
    g Mtransferred, accu

  | Mcaller ->
    g Mcaller, accu

  | Mbalance ->
    g Mbalance, accu

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

  | Massignfield (op, a, fi, x) ->
    let xe, xa = f accu x in
    g (Massignfield (op, a, fi, xe)), xa

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

  | Mreturn x ->
    let xe, xa = f accu x in
    g (Mreturn xe), xa

  | Mforall (id, t, e) ->
    let ee, ea = f accu e in
    g (Mforall (id, t, ee)), ea

  | Mexists (id, t, e) ->
    let ee, ea = f accu e in
    g (Mexists (id, t, ee)), ea

  | MsecMayBePerformedOnlyByRole (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (MsecMayBePerformedOnlyByRole (le, re)), ra

  | MsecMayBePerformedOnlyByAction (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (MsecMayBePerformedOnlyByAction (le, re)), ra

  | MsecMayBePerformedByRole (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (MsecMayBePerformedByRole (le, re)), ra

  | MsecMayBePerformedByAction (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (MsecMayBePerformedByAction (le, re)), ra

  | MsecTransferredBy a ->
    let ee, ea = f accu a in
    g (MsecTransferredBy ee), ea

  | MsecTransferredTo a ->
    let ee, ea = f accu a in
    g (MsecTransferredTo ee), ea


let fold_model (f : 'a -> 'id mterm_gen -> 'a) (m : 'id model_gen) (accu : 'a) : 'a =
  let fold_left g l accu = List.fold_left (fun accu x -> g x accu) accu l in

  let fold_verification (f : 'a -> 'id mterm_gen -> 'a) (v : 'id verification_gen) (accu : 'a) : 'a = (
    let fold_label_term (f : 'a -> 'id mterm_gen -> 'a) (lt : 'id label_term_gen) (accu : 'a) : 'a =
      f accu lt.term
    in

    let fold_predicate (f : 'a -> 'id mterm_gen -> 'a) (p : 'id predicate_gen) (accu : 'a) : 'a =
      accu
      |> (fun x -> List.fold_left (fun accu x -> x |> snd |> f accu) x p.args)
      |> fun x -> f x p.body
    in

    let fold_definition (f : 'a -> 'id mterm_gen -> 'a) (d : 'id definition_gen) (accu : 'a) : 'a =
      f accu d.body
    in

    let fold_invariantt (f : 'a -> 'id mterm_gen -> 'a) (it : 'id * 'id label_term_gen list) (accu : 'a) : 'a =
      List.fold_left (fun accu x -> fold_label_term f x accu) accu (snd it)
    in

    let fold_invariant (f : 'a -> 'id mterm_gen -> 'a) (spec : 'id invariant_gen) (accu : 'a) : 'a =
      List.fold_left f accu spec.formulas
    in

    let fold_specification (f : 'a -> 'id mterm_gen -> 'a) (spec : 'id specification_gen) (accu : 'a) : 'a =
      accu
      |> (fun x -> f x spec.formula)
      |> (fun x -> List.fold_left (fun accu (x : 'id invariant_gen) -> fold_invariant f x accu) x spec.invariants)
    in

    let fold_variable (f : 'a -> 'id mterm_gen -> 'a) (spec : 'id variable_gen) (accu : 'a) : 'a =
      accu
    in

    let fold_assert (f : 'a -> 'id mterm_gen -> 'a) (assert_ : 'id assert_gen) (accu : 'a) : 'a =
      accu
      |> (fun x -> f accu assert_.formula)
      |> (fun x -> List.fold_left (fun accu (x : 'id invariant_gen) -> fold_invariant f x accu) x assert_.invariants)
    in

    accu
    |> fold_left (fold_predicate f) v.predicates
    |> fold_left (fold_definition f) v.definitions
    |> fold_left (fold_label_term f) v.axioms
    |> fold_left (fold_label_term f) v.theorems
    |> fold_left (fold_variable f) v.variables
    |> fold_left (fold_invariantt f) v.invariants
    |> (fun x -> List.fold_left (fun accu x -> f accu x) x v.effects)
    |> fold_left (fold_specification f) v.specs
    |> fold_left (fold_assert f) v.asserts
  ) in

  let fold_action (f : 'a -> 'id mterm_gen -> 'a) (a : 'id function__gen) (accu : 'a) : 'a = (
    let accu : 'a = (
      match a.node with
      | Function (fs, _)
      | Entry fs -> fold_term f accu fs.body
    ) in
    Option.map_dfl (fun (x : 'id verification_gen) -> fold_verification f x accu) accu a.verif
  ) in

  accu
  |> fold_left (fold_action f) m.functions
  |> fold_verification f m.verification

(* -------------------------------------------------------------------- *)
module Utils : sig

  val function_name_from_storage_const   : storage_const   -> string
  val function_name_from_container_const : container_const -> string
  val function_name_from_function_const  : function_const  -> string
  val get_records                        : model -> record list
  val get_storage                        : model -> storage
  val get_record                         : model -> lident -> record
  val get_record_field                   : model -> (lident * lident) -> record_item
  val get_record_key                     : model -> lident -> (lident * btyp)
  val is_storage_attribute               : model -> lident -> bool
  val get_named_field_list               : model -> lident -> 'a list -> (lident * 'a) list
  val get_partitions                     : model -> (lident * record_item) list (* record id, record item *)
  val dest_partition                     : type_ -> lident
  val get_partition_record_key           : model -> lident -> lident -> (lident * lident * btyp)
  val get_entries                        : model -> (verification option * function_struct) list
  val has_partition                      : model -> ident -> bool
  val get_record_partitions              : model -> ident -> record_item list
  val get_field_list                     : model -> lident -> lident list
  val get_field_pos                      : model -> lident -> lident -> int (* m, record, field *)
  val get_nth_record_val                 : int -> mterm -> mterm
  val dest_array                         : mterm -> mterm list
  val get_asset_type                     : mterm -> lident
  val is_local_assigned                  : lident -> mterm -> bool

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
    | NotanArray
    | NotaRecord
    | NotanAssetType
  [@@deriving show {with_path = false}]

  let emit_error (desc : error_desc) =
    let str = Format.asprintf "%a@." pp_error_desc desc in
    raise (Anomaly str)

  let lident_to_string lident = Location.unloc lident

  let function_name_from_function_node = function
    | Function (fs, _)    -> lident_to_string fs.name
    | Entry     fs        -> lident_to_string fs.name

  let function_name_from_storage_const = function
    | Get            aid       -> "get_"            ^ lident_to_string aid
    | Set            aid       -> "set_"            ^ lident_to_string aid
    | Add            aid       -> "add_"            ^ lident_to_string aid
    | Remove         aid       -> "remove_"         ^ lident_to_string aid
    | Clear          aid       -> "clear_"          ^ lident_to_string aid
    | Reverse        aid       -> "reverse_"        ^ lident_to_string aid
    | UpdateAdd     (aid, fid) -> "update_add_"     ^ lident_to_string aid ^ "_" ^ lident_to_string fid
    | UpdateRemove  (aid, fid) -> "update_remove_"  ^ lident_to_string aid ^ "_" ^ lident_to_string fid
    | UpdateClear   (aid, fid) -> "update_clear_"   ^ lident_to_string aid ^ "_" ^ lident_to_string fid
    | UpdateReverse (aid, fid) -> "update_reverse_" ^ lident_to_string aid ^ "_" ^ lident_to_string fid

  let function_name_from_container_const = function
    | Add            _ -> "add"
    | Remove         _ -> "remove"
    | Clear          _ -> "clear"
    | Reverse        _ -> "reverse"

  let function_name_from_function_const = function
    | Select    aid       -> "select_"   ^ lident_to_string aid
    | Sort     (aid, fid) -> "sort_"     ^ lident_to_string aid ^ "_" ^ lident_to_string fid
    | Contains  aid       -> "contains_" ^ lident_to_string aid
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

  let dest_array (t : mterm)  =
    match t.node with
    | Marray l -> l
    | _ -> emit_error NotanArray

  let get_nth_record_val pos (t : mterm) =
    match t.node with
    | Mrecord l -> List.nth l pos
    | _ -> emit_error NotaRecord

  let get_asset_type (t : mterm) : lident =
    match t.type_ with
    | Tasset n -> n
    | Tcontainer (Tasset n, _) -> n
    | _ -> emit_error NotanAssetType

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

  let has_partition m asset : bool =
    get_records m |> List.fold_left (fun acc (record : record) ->
        if compare asset record.name.pldesc = 0 then
          (List.fold_left (fun acc (ritem : record_item) ->
               match ritem.type_ with
               | Tcontainer (Tasset _, Partition) -> true
               | _ -> acc
             ) false record.values)
        else
          acc
      ) false


  let get_record_partitions m asset : record_item list =
    get_records m |> List.fold_left (fun acc (record : record) ->
        if compare asset record.name.pldesc = 0 then
          (List.fold_left (fun acc (ritem : record_item) ->
               match ritem.type_ with
               | Tcontainer (Tasset _, Partition) ->
                 acc @ [ritem]
               | _ -> acc
             ) [] record.values)
        else
          acc
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

  let get_field_pos model record field =
    let l = get_field_list model record in
    let rec rec_get_pos i = function
      | e :: tl when compare field.pldesc e.pldesc = 0 -> i
      | _ :: tl -> rec_get_pos (succ i) tl
      | [] -> assert false in
    rec_get_pos 0 l

  let get_named_field_list ast asset_name list =
    let field_list = get_field_list ast asset_name in
    (* List.iter (fun x -> Format.eprintf "f1: %s@." (unloc x)) field_list;
       List.iter (fun x -> Format.eprintf "f2: %a@." pp_pterm x) list;
       Format.eprintf "lf1: %d@." (List.length field_list);
       Format.eprintf "lf2: %d@." (List.length list); *)
    List.map2 (fun x y -> x, y) field_list list

  exception FoundAssign

  let is_local_assigned id (b : mterm) =
    let rec rec_search_assign _ (t : mterm) =
      match t.node with
      | Massign (_,i,_) when compare (unloc i) (unloc id)  = 0 -> raise FoundAssign
      | _ -> fold_term rec_search_assign false t in
    try rec_search_assign false b
    with _ -> true

end
