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
  | List
[@@deriving show {with_path = false}]

type btyp =
  | Bbool
  | Bint
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
  | Tassoc of btyp * type_
  | Tunit
  | Tstorage
  | Toperation
  | Tentry
  | Tprog of type_
  | Tvset of vset * type_
  | Ttrace of trtyp
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
  | Mif           of ('term * 'term * 'term option)
  | Mmatchwith    of 'term * ('id pattern_gen * 'term) list
  | Mapp          of 'id * 'term list
  | Mexternal     of 'id * 'id * 'term * ('term) list
  | Mget          of ident * 'term
  | Mset          of ident * 'term * 'term
  | Maddasset     of ident * 'term
  | Maddfield     of ident * ident * 'term * 'term (* asset_name * field_name * asset instance * item * shalow values*)
  | Maddlocal     of 'term * 'term
  | Mremoveasset  of ident * 'term
  | Mremovefield  of ident * ident * 'term * 'term
  | Mremovelocal  of 'term * 'term
  | Mclearasset   of ident
  | Mclearfield   of ident * ident * 'term
  | Mclearlocal   of 'term
  | Mreverseasset of ident
  | Mreversefield of ident * ident * 'term
  | Mreverselocal of 'term
  | Mselect       of ident * 'term * 'term
  | Msort         of ident * 'term * ident * sort_kind
  | Mcontains     of ident * 'term * 'term
  | Mmem          of ident * 'term * 'term
  | Msubset       of ident * 'term * 'term
  | Mnth          of ident * 'term * 'term
  | Mcount        of ident * 'term
  | Msum          of ident * 'id * 'term
  | Mmin          of ident * 'id * 'term
  | Mmax          of ident * 'id * 'term
  | Mmathmax      of 'term * 'term
  | Mmathmin      of 'term * 'term
  | Mfail         of 'id fail_type_gen
  | Mand          of 'term * 'term
  | Mor           of 'term * 'term
  | Mimply        of 'term * 'term
  | Mequiv        of 'term * 'term
  | Misempty      of ident * 'term
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
  | Mletin        of 'id list * 'term * type_ option * 'term
  | Mvarstorevar  of 'id
  | Mvarstorecol  of 'id
  | Mvarenumval   of 'id
  | Mvarlocal     of 'id
  | Mvarparam     of 'id
  | Mvarfield     of 'id
  | Mvarthe
  | Mstate
  | Mnow
  | Mtransferred
  | Mcaller
  | Mbalance
  | Mnone
  | Msome         of 'term
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
  | Mdotasset     of 'term * 'id
  | Mdotcontract  of 'term * 'id
  | Mtuple        of 'term list
  | Massoc        of 'term * 'term
  | Mfor          of ('id * 'term * 'term * ident option)
  | Mfold         of ('id * 'id list * 'term * 'term) (* ident list * collection * body *)
  | Mseq          of 'term list
  | Massign       of (assignment_operator * 'id * 'term)
  | Massignfield  of (assignment_operator * 'id * 'id * 'term)
  | Mtransfer     of ('term * bool * 'id qualid_gen option)
  | Mbreak
  | Massert       of 'term
  | Mreturn       of 'term
  | Mlabel        of 'id
  (* shallowing *)
  | Mshallow      of ident * 'term
  | Munshallow    of ident * 'term
  | Mlisttocoll   of ident * 'term
  (* *)
  | Mtokeys       of ident * 'term
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
  (* security arg *)
  | Manyaction
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

and 'id fail_type_gen =
  | Invalid of 'id mterm_gen
  | InvalidCaller
  | InvalidCondition of ident option
  | NoTransfer
  | InvalidState
[@@deriving show {with_path = false}]

and fail_type = lident fail_type_gen
[@@deriving show {with_path = false}]

and storage_const =
  | Get              of ident
  | Set              of ident
  | Add              of ident
  | Remove           of ident
  | Clear            of ident
  | Reverse          of ident
  | UpdateAdd        of ident * ident
  | UpdateRemove     of ident * ident
  | UpdateClear      of ident * ident
  | UpdateReverse    of ident * ident
  | ToKeys           of ident
[@@deriving show {with_path = false}]

and container_const =
  | AddItem          of type_
  | RemoveItem       of type_
  | ClearItem        of type_
  | ReverseItem      of type_
[@@deriving show {with_path = false}]

and function_const =
  | Select           of ident * mterm
  | Sort             of ident * ident
  | Contains         of ident
  | Nth              of ident
  | Count            of ident
  | Sum              of ident * ident
  | Min              of ident * ident
  | Max              of ident * ident
  | Shallow          of ident
  | Unshallow        of ident
  | Listtocoll       of ident
[@@deriving show {with_path = false}]

and builtin_const =
  | MinBuiltin of type_
  | MaxBuiltin of type_
[@@deriving show {with_path = false}]

and api_item_node =
  | APIStorage   of storage_const
  | APIContainer of container_const
  | APIFunction  of function_const
  | APIBuiltin   of builtin_const
[@@deriving show {with_path = false}]

and api_item = {
  node_item: api_item_node;
  only_formula: bool;
}
[@@deriving show {with_path = false}]

type info_var = {
  name: ident;
  type_ : type_;
  constant: bool;
  init: mterm option;
}
[@@deriving show {with_path = false}]

type info_enum = {
  name: ident;
  values: ident list;
}
[@@deriving show {with_path = false}]

type info_asset = {
  name: ident;
  key: ident;
  values: (ident * type_ * (mterm option)) list;
}
[@@deriving show {with_path = false}]

type info_contract = {
  name: ident;
  signatures: (ident * type_ list) list;
}
[@@deriving show {with_path = false}]

type info_item =
  | Ivar of info_var
  | Ienum of info_enum
  | Iasset of info_asset
  | Icontract of info_contract
[@@deriving show {with_path = false}]

type 'id label_term_gen = {
  label : 'id option;
  term : 'id mterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type label_term = lident label_term_gen
[@@deriving show {with_path = false}]

(* type 'id item_field_type =
   | FBasic            of btyp
   | FAssetKeys        of btyp * 'id
   | FAssetRecord      of btyp * 'id
   | FRecordCollection of 'id
   | FRecord           of 'id
   | FEnum             of 'id
   | FContainer        of container * 'id item_field_type
   [@@deriving show {with_path = false}] *)

type 'id storage_item_gen = {
  name        : 'id;
  asset       : 'id option;
  typ         : type_;
  ghost       : bool;
  default     : 'id mterm_gen; (* initial value *)
  invariants  : lident label_term_gen list;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type storage_item = lident storage_item_gen

type 'id storage_gen = 'id storage_item_gen list
[@@deriving show {with_path = false}]

type storage = lident storage_gen
[@@deriving show {with_path = false}]

type 'id enum_item_gen = {
  name: 'id;
  invariants : 'id label_term_gen list;
}
[@@deriving show {with_path = false}]

type enum_item = lident enum_item_gen
[@@deriving show {with_path = false}]

type 'id enum_gen = {
  name: 'id;
  values: 'id enum_item_gen list;
}
[@@deriving show {with_path = false}]

type enum = lident enum_gen
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
  args : ('id * type_) list;
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


type spec_mode =
  | Post
  | Assert
[@@deriving show {with_path = false}]

type 'id specification_gen = {
  name: 'id;
  mode: spec_mode;
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
  lemmas      : 'id label_term_gen list;
  theorems    : 'id label_term_gen list;
  variables   : 'id variable_gen list;
  invariants  : ('id * 'id label_term_gen list) list;
  effects     : 'id mterm_gen list;
  specs       : 'id specification_gen list;
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
  | Denum of 'id enum_gen
  | Drecord of 'id record_gen
  | Dcontract of 'id contract_gen
[@@deriving show {with_path = false}]

type decl_node = lident decl_node_gen
[@@deriving show {with_path = false}]

type 'id model_gen = {
  name         : lident;
  api_items    : api_item list;
  info         : info_item list;
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

let mk_specification ?(invariants = []) name mode formula =
  { name; mode; formula; invariants }

let mk_assert ?(invariants = []) name label formula =
  { name; label; formula; invariants }

let mk_verification ?(predicates = []) ?(definitions = []) ?(lemmas = []) ?(theorems = []) ?(variables = []) ?(invariants = []) ?(effects = []) ?(specs = []) ?(asserts = []) ?(loc = Location.dummy) () =
  { predicates; definitions; lemmas; theorems; variables; invariants; effects; specs; loc}

let mk_info_var ?(constant = false) ?init name type_ : info_var =
  { name; type_; constant; init}

let mk_info_enum ?(values = []) name : info_enum =
  { name; values }

let mk_info_asset ?(values = []) name key : info_asset =
  { name; key; values }

let mk_info_contract ?(signatures = []) name : info_contract =
  { name; signatures }

let mk_enum ?(values = []) name : 'id enum_gen =
  { name; values }

let mk_enum_item ?(invariants = []) name : 'id enum_item_gen =
  { name; invariants }

let mk_record ?(values = []) ?key name : 'id record_gen =
  { name; key; values }

let mk_record_item ?default name type_ : 'id record_item_gen =
  { name; type_; default }

let mk_contract_signature ?(args=[]) ?(loc=Location.dummy) name : 'id contract_signature_gen =
  { name; args; loc }

let mk_contract ?(signatures=[]) ?init ?(loc=Location.dummy) name : 'id contract_gen =
  { name; signatures; init; loc }

let mk_storage_item ?asset ?(ghost = false) ?(invariants = []) ?(loc = Location.dummy) name typ default : 'id storage_item_gen =
  { name; asset; typ; ghost; default; invariants; loc }

let mk_function_struct ?(args = []) ?(loc = Location.dummy) name body : function_struct =
  { name; args; body; loc }

let mk_function ?verif node : 'id function__gen =
  { node; verif }

let mk_signature ?(args = []) ?ret name : 'id signature_gen =
  { name; args; ret }

let mk_api_item ?(only_formula = false) node_item =
  { node_item; only_formula }

let mk_model ?(api_items = []) ?(info = []) ?(decls = []) ?(functions = []) ?(storage = []) ?(verification = mk_verification ())  storage name : model =
  { name; api_items; info; storage; decls; functions; verification}

(* -------------------------------------------------------------------- *)

let cmp_ident (i1 : ident) (i2 : ident) : bool = String.equal i1 i2
let cmp_lident (i1 : lident) (i2 : lident) : bool = cmp_ident (Location.unloc i1) (Location.unloc i2)
let cmp_bool (b1 : bool) (b2 : bool) : bool = b1 = b2
let cmp_assign_op (op1 : assignment_operator) (op2 : assignment_operator) : bool = op1 = op2
let cmp_currency (c1 : currency) (c2 : currency) : bool = c1 = c2
let cmp_container (c1 : container) (c2 : container) = c1 = c2
let cmp_btyp (b1 : btyp) (b2 : btyp) : bool = b1 = b2
let cmp_vset (v1 : vset) (v2 : vset) : bool = v1 = v2
let cmp_trtyp (t1 : trtyp) (t2 : trtyp) : bool = t1 = t2

let cmp_fail_type
    (cmp : 'term -> 'term -> bool)
    (ft1 : 'id fail_type_gen)
    (ft2 : 'id fail_type_gen) : bool =
  match ft1, ft2 with
  | Invalid mt1, Invalid mt2 -> cmp mt1 mt2
  | InvalidCaller, InvalidCaller -> true
  | InvalidCondition c1, InvalidCondition c2 -> Option.cmp cmp_ident c1 c2
  | _ -> false

let rec cmp_type
    (t1 : type_)
    (t2 : type_)
  : bool =
  match t1, t2 with
  | Tasset i1, Tasset i2                     -> cmp_lident i1 i2
  | Tenum i1, Tenum i2                       -> cmp_lident i1 i2
  | Tcontract i1, Tcontract i2               -> cmp_lident i1 i2
  | Tbuiltin b1, Tbuiltin b2                 -> cmp_btyp b1 b2
  | Tcontainer (t1, c1), Tcontainer (t2, c2) -> cmp_type t1 t2 && cmp_container c1 c2
  | Toption t1, Toption t2                   -> cmp_type t1 t2
  | Ttuple l1, Ttuple l2                     -> List.for_all2 cmp_type l1 l2
  | Tunit, Tunit                             -> true
  | Tentry, Tentry                           -> true
  | Tprog t1, Tprog t2                       -> cmp_type t1 t2
  | Tvset (v1, t1), Tvset (v2, t2)           -> cmp_vset v1 v2 && cmp_type t1 t2
  | Ttrace t1, Ttrace t2                     -> cmp_trtyp t1 t2
  | _ -> false

let cmp_pattern_node
    (cmpi  : 'id -> 'id -> bool)
    (p1    : 'id pattern_node)
    (p2    : 'id pattern_node)
  : bool =
  match p1, p2 with
  | Pconst c1, Pconst c2 -> cmpi c1 c2
  | Pwild, Pwild -> true
  | _ -> false

let cmp_pattern
    (p1 : 'id pattern_gen)
    (p2 : 'id pattern_gen)
  : bool =
  cmp_pattern_node cmp_lident p1.node p2.node

let cmp_qualid_node
    (cmp  : 'q -> 'q -> bool)
    (cmpi : 'id -> 'id -> bool)
    (p1   : ('id, 'q) qualid_node)
    (p2   : ('id, 'q) qualid_node)
  : bool =
  match p1, p2 with
  | Qident i1, Qident i2 -> cmpi i1 i2
  | Qdot (q1, i1), Qdot (q2, i2) -> cmp q1 q2 && cmpi i1 i2
  | _ -> false

let rec cmp_qualid
    (q1 : 'id qualid_gen)
    (q2 : 'id qualid_gen)
  : bool =
  cmp_qualid_node cmp_qualid cmp_lident q1.node q2.node

let cmp_mterm_node
    (cmp   : 'term -> 'term -> bool)
    (cmpi  : 'id -> 'id -> bool)
    (term1 : ('id, 'term) mterm_node)
    (term2 : ('id, 'term) mterm_node)
  : bool =
  try
    match term1, term2 with
    | Mif (c1, t1, e1), Mif (c2, t2, e2)                                               -> cmp c1 c2 && cmp t1 t2 && Option.cmp cmp e1 e2
    | Mmatchwith (e1, l1), Mmatchwith (e2, l2)                                         -> cmp e1 e2 && List.for_all2 (fun (p1, t1) (p2, t2) -> cmp_pattern p1 p2 && cmp t1 t2) l1 l2
    | Mapp (e1, args1), Mapp (e2, args2)                                               -> cmpi e1 e2 && List.for_all2 cmp args1 args2
    | Mexternal (t1, func1, c1, args1), Mexternal (t2, func2, c2, args2)               -> cmpi t1 t2 && cmpi func1 func2 && cmp c1 c2 && List.for_all2 cmp args1 args2
    | Mget (c1, k1), Mget (c2, k2)                                                     -> cmp_ident c1 c2 && cmp k1 k2
    | Mset (c1, k1, v1), Mset (c2, k2, v2)                                             -> cmp_ident c1 c2 && cmp k1 k2 && cmp v1 v2
    | Maddasset (an1, i1), Maddasset (an2, i2)                                         -> cmp_ident an1 an2 && cmp i1 i2
    | Maddfield (an1, fn1, c1, i1), Maddfield (an2, fn2, c2, i2)                       -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp c1 c2 && cmp i1 i2
    | Maddlocal (c1, i1), Maddlocal (c2, i2)                                           -> cmp c1 c2 && cmp i1 i2
    | Mremoveasset (an1, i1), Mremoveasset (an2, i2)                                   -> cmp_ident an1 an2 && cmp i1 i2
    | Mremovefield (an1, fn1, c1, i1), Mremovefield (an2, fn2, c2, i2)                 -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp c1 c2 && cmp i1 i2
    | Mremovelocal (c1, i1), Mremovelocal (c2, i2)                                     -> cmp c1 c2 && cmp i1 i2
    | Mclearasset (an1), Mclearasset (an2)                                             -> cmp_ident an1 an2
    | Mclearfield (an1, fn1, i1), Mclearfield (an2, fn2, i2)                           -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp i1 i2
    | Mclearlocal (i1), Mclearlocal (i2)                                               -> cmp i1 i2
    | Mreverseasset (an1), Mreverseasset (an2)                                         -> cmp_ident an1 an2
    | Mreversefield (an1, fn1, i1), Mreversefield (an2, fn2, i2)                       -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp i1 i2
    | Mreverselocal (i1), Mreverselocal (i2)                                           -> cmp i1 i2
    | Mselect (an1, c1, p1), Mselect (an2, c2, p2)                                     -> cmp_ident an1 an2 && cmp c1 c2 && cmp p1 p2
    | Msort (an1, c1, fn1, k1), Msort (an2, c2, fn2, k2)                               -> cmp_ident an1 an2 && cmp c1 c2 && cmp_ident fn1 fn2 && k1 = k2
    | Mcontains (an1, c1, i1), Mcontains (an2, c2, i2)                                 -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Mmem (an1, c1, i1), Mmem (an2, c2, i2)                                           -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Msubset (an1, c1, i1), Msubset (an2, c2, i2)                                     -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Mnth (an1, c1, i1), Mnth (an2, c2, i2)                                           -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Mcount (an1, c1), Mcount (an2, c2)                                               -> cmp_ident an1 an2 && cmp c1 c2
    | Msum (an1, fd1, c1), Msum (an2, fd2, c2)                                         -> cmp_ident an1 an2 && cmpi fd1 fd2 && cmp c1 c2
    | Mmin (an1, fd1, c1), Mmin (an2, fd2, c2)                                         -> cmp_ident an1 an2 && cmpi fd1 fd2 && cmp c1 c2
    | Mmax (an1, fd1, c1), Mmax (an2, fd2, c2)                                         -> cmp_ident an1 an2 && cmpi fd1 fd2 && cmp c1 c2
    | Mfail ft1, Mfail ft2                                                             -> cmp_fail_type cmp ft1 ft2
    | Mmathmin (l1, r1), Mmathmin (l2, r2)                                             -> cmp l1 l2 && cmp r1 r2
    | Mmathmax (l1, r1), Mmathmax (l2, r2)                                             -> cmp l1 l2 && cmp r1 r2
    | Mand (l1, r1), Mand (l2, r2)                                                     -> cmp l1 l2 && cmp r1 r2
    | Mor (l1, r1), Mor (l2, r2)                                                       -> cmp l1 l2 && cmp r1 r2
    | Mimply (l1, r1), Mimply (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Mequiv (l1, r1), Mequiv (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Misempty (l1, r1), Misempty (l2, r2)                                             -> cmp_ident l1 l2 && cmp r1 r2
    | Mnot e1, Mnot e2                                                                 -> cmp e1 e2
    | Mequal (l1, r1), Mequal (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Mnequal (l1, r1), Mnequal (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Mgt (l1, r1), Mgt (l2, r2)                                                       -> cmp l1 l2 && cmp r1 r2
    | Mge (l1, r1), Mge (l2, r2)                                                       -> cmp l1 l2 && cmp r1 r2
    | Mlt (l1, r1), Mlt (l2, r2)                                                       -> cmp l1 l2 && cmp r1 r2
    | Mle (l1, r1), Mle (l2, r2)                                                       -> cmp l1 l2 && cmp r1 r2
    | Mplus (l1, r1), Mplus (l2, r2)                                                   -> cmp l1 l2 && cmp r1 r2
    | Mminus (l1, r1), Mminus (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Mmult (l1, r1), Mmult (l2, r2)                                                   -> cmp l1 l2 && cmp r1 r2
    | Mdiv (l1, r1), Mdiv (l2, r2)                                                     -> cmp l1 l2 && cmp r1 r2
    | Mmodulo (l1, r1), Mmodulo (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Muplus e1, Muplus e2                                                             -> cmp e1 e2
    | Muminus e1, Muminus e2                                                           -> cmp e1 e2
    | Mrecord l1, Mrecord l2                                                           -> List.for_all2 cmp l1 l2
    | Mletin (i1, a1, t1, b1), Mletin (i2, a2, t2, b2)                                 -> List.for_all2 cmpi i1 i2 && cmp a1 a2 && Option.cmp cmp_type t1 t2 && cmp b1 b2
    | Mvarstorevar v1, Mvarstorevar v2                                                 -> cmpi v1 v2
    | Mvarstorecol v1, Mvarstorecol v2                                                 -> cmpi v1 v2
    | Mvarenumval v1, Mvarenumval v2                                                   -> cmpi v1 v2
    | Mvarfield v1, Mvarfield v2                                                       -> cmpi v1 v2
    | Mvarlocal v1, Mvarlocal v2                                                       -> cmpi v1 v2
    | Mvarparam v1, Mvarparam v2                                                       -> cmpi v1 v2
    | Mvarthe, Mvarthe                                                                 -> true
    | Mstate, Mstate                                                                   -> true
    | Mnow, Mnow                                                                       -> true
    | Mtransferred, Mtransferred                                                       -> true
    | Mcaller, Mcaller                                                                 -> true
    | Mbalance, Mbalance                                                               -> true
    | Marray l1, Marray l2                                                             -> List.for_all2 cmp l1 l2
    | Mint v1, Mint v2                                                                 -> Big_int.eq_big_int v1 v2
    | Muint v1, Muint v2                                                               -> Big_int.eq_big_int v1 v2
    | Mbool v1, Mbool v2                                                               -> cmp_bool v1 v2
    | Menum v1, Menum v2                                                               -> cmp_ident v1 v2
    | Mrational (n1, d1), Mrational (n2, d2)                                           -> Big_int.eq_big_int n1 n2 && Big_int.eq_big_int d1 d2
    | Mdate v1, Mdate v2                                                               -> cmp_ident v1 v2
    | Mstring v1, Mstring v2                                                           -> cmp_ident v1 v2
    | Mcurrency (v1, c1), Mcurrency (v2, c2)                                           -> Big_int.eq_big_int v1 v2 && cmp_currency c1 c2
    | Maddress v1, Maddress v2                                                         -> cmp_ident v1 v2
    | Mduration v1, Mduration v2                                                       -> cmp_ident v1 v2
    | Mdotasset (e1, i1), Mdotasset (e2, i2)                                           -> cmp e1 e2 && cmpi i1 i2
    | Mdotcontract (e1, i1), Mdotcontract (e2, i2)                                     -> cmp e1 e2 && cmpi i1 i2
    | Mtuple l1, Mtuple l2                                                             -> List.for_all2 cmp l1 l2
    | Mfor (i1, c1, b1, lbl1), Mfor (i2, c2, b2, lbl2)                                 -> cmpi i1 i2 && cmp c1 c2 && cmp b1 b2 && Option.cmp cmp_ident lbl1 lbl2
    | Mfold (i1, is1, c1, b1), Mfold (i2, is2, c2, b2)                                 -> cmpi i1 i2 && List.for_all2 cmpi is1 is2 && cmp c1 c2 && cmp b1 b2
    | Mseq is1, Mseq is2                                                               -> List.for_all2 cmp is1 is2
    | Massign (op1, l1, r1), Massign (op2, l2, r2)                                     -> cmp_assign_op op1 op2 && cmpi l1 l2 && cmp r1 r2
    | Massignfield (op1, a1, fi1, r1), Massignfield (op2, a2, fi2, r2)                 -> cmp_assign_op op1 op2 && cmpi a1 a2 && cmpi fi1 fi2 && cmp r1 r2
    | Mtransfer (x1, b1, q1), Mtransfer (x2, b2, q2)                                   -> cmp x1 x2 && cmp_bool b1 b2 && Option.cmp cmp_qualid q1 q2
    | Mbreak, Mbreak                                                                   -> true
    | Massert x1, Massert x2                                                           -> cmp x1 x2
    | Mreturn x1, Mreturn x2                                                           -> cmp x1 x2
    | Mforall (i1, t1, e1), Mforall (i2, t2, e2)                                       -> cmpi i1 i2 && cmp_type t1 t2 && cmp e1 e2
    | Mexists (i1, t1, e1), Mexists (i2, t2, e2)                                       -> cmpi i1 i2 && cmp_type t1 t2 && cmp e1 e2
    | Msetbefore e1, Msetbefore e2                                                     -> cmp e1 e2
    | Msetunmoved e1, Msetunmoved e2                                                   -> cmp e1 e2
    | Msetadded e1, Msetadded e2                                                       -> cmp e1 e2
    | Msetremoved e1, Msetremoved   e2                                                 -> cmp e1 e2
    | Msetiterated e1, Msetiterated  e2                                                -> cmp e1 e2
    | Msettoiterate e1, Msettoiterate e2                                               -> cmp e1 e2
    | MsecMayBePerformedOnlyByRole (l1, r1), MsecMayBePerformedOnlyByRole (l2, r2)     -> cmp l1 l2 && cmp r1 r2
    | MsecMayBePerformedOnlyByAction (l1, r1), MsecMayBePerformedOnlyByAction (l2, r2) -> cmp l1 l2 && cmp r1 r2
    | MsecMayBePerformedByRole (l1, r1), MsecMayBePerformedByRole (l2, r2)             -> cmp l1 l2 && cmp r1 r2
    | MsecMayBePerformedByAction (l1, r1), MsecMayBePerformedByAction (l2, r2)         -> cmp l1 l2 && cmp r1 r2
    | MsecTransferredBy a1, MsecTransferredBy a2                                       -> cmp a1 a2
    | MsecTransferredTo a1, MsecTransferredTo a2                                       -> cmp a1 a2
    | _ -> false
  with
    _ -> false

let rec cmp_mterm (term1 : mterm) (term2 : mterm) : bool =
  cmp_mterm_node cmp_mterm cmp_lident term1.node term2.node

let cmp_api_item_node (a1 : api_item_node) (a2 : api_item_node) : bool =
  let cmp_storage_const (s1 : storage_const) (s2 : storage_const) : bool =
    match s1, s2 with
    | Get an1, Get an2  -> cmp_ident an1 an2
    | Set an1 , Set an2 -> cmp_ident an1 an2
    | Add an1 , Add an2 -> cmp_ident an1 an2
    | Remove an1, Remove an2 -> cmp_ident an1 an2
    | Clear an1, Clear an2 -> cmp_ident an1 an2
    | Reverse an1, Reverse an2 -> cmp_ident an1 an2
    | UpdateAdd (an1, fn1), UpdateAdd (an2, fn2) -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | UpdateRemove (an1, fn1), UpdateRemove (an2, fn2) -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | UpdateClear (an1, fn1), UpdateClear (an2, fn2) -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | UpdateReverse (an1, fn1), UpdateReverse (an2, fn2) -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | ToKeys an1, ToKeys an2 -> cmp_ident an1 an2
    | _ -> false
  in
  let cmp_container_const (c1 : container_const) (c2 : container_const) : bool =
    match c1, c2 with
    | AddItem t1, AddItem t2         -> cmp_type t1 t2
    | RemoveItem t1, RemoveItem t2   -> cmp_type t1 t2
    | ClearItem t1, ClearItem t2     -> cmp_type t1 t2
    | ReverseItem t1, ReverseItem t2 -> cmp_type t1 t2
    | _ -> false
  in
  let cmp_function_const (f1 : function_const) (f2 : function_const) : bool =
    match f1, f2 with
    | Select (an1, p1), Select (an2, p2) -> cmp_ident an1 an2 && cmp_mterm p1 p2
    | Sort (an1 , fn1), Sort (an2 , fn2) -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | Contains an1, Contains an2         -> cmp_ident an1 an2
    | Nth an1, Nth an2                   -> cmp_ident an1 an2
    | Count an1, Count an2               -> cmp_ident an1 an2
    | Sum (an1, fn1), Sum (an2, fn2)     -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | Min (an1, fn1), Min (an2, fn2)     -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | Max (an1, fn1), Max (an2, fn2)     -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | Shallow an1, Shallow an2           -> cmp_ident an1 an2
    | Unshallow an1, Unshallow an2       -> cmp_ident an1 an2
    | _ -> false
  in
  let cmp_builtin_const (b1 : builtin_const) (b2 : builtin_const) : bool =
    match b1, b2 with
    | MinBuiltin t1, MinBuiltin t2 -> cmp_type t1 t2
    | MaxBuiltin t1, MaxBuiltin t2 -> cmp_type t1 t2
    | _ -> false
  in
  match a1, a2 with
  | APIStorage s1, APIStorage s2     -> cmp_storage_const s1 s2
  | APIContainer c1, APIContainer c2 -> cmp_container_const c1 c2
  | APIFunction f1, APIFunction f2   -> cmp_function_const f1 f2
  | APIBuiltin b1, APIBuiltin b2     -> cmp_builtin_const b1 b2
  | _ -> false

(* -------------------------------------------------------------------- *)

let map_term_node (f : 'id mterm_gen -> 'id mterm_gen) = function
  | Mif (c, t, e)                -> Mif (f c, f t, Option.map f e)
  | Mmatchwith (e, l)            -> Mmatchwith (e, List.map (fun (p, e) -> (p, f e)) l)
  | Mapp (e, args)               -> Mapp (e, List.map f args)
  | Msetbefore    e              -> Msetbefore    (f e)
  | Msetunmoved   e              -> Msetunmoved   (f e)
  | Msetadded     e              -> Msetadded     (f e)
  | Msetremoved   e              -> Msetremoved   (f e)
  | Msetiterated  e              -> Msetiterated  (f e)
  | Msettoiterate e              -> Msettoiterate (f e)
  | Mexternal (t, func, c, args) -> Mexternal (t, func, f c, List.map f args)
  | Mget (c, k)                  -> Mget (c, f k)
  | Mset (c, k, v)               -> Mset (c, f k, f v)
  | Maddasset (an, i)            -> Maddasset (an, f i)
  | Maddfield (an, fn, c, i)     -> Maddfield (an, fn, f c, f i)
  | Maddlocal (c, i)             -> Maddlocal (f c, f i)
  | Mremoveasset (an, i)         -> Mremoveasset (an, f i)
  | Mremovefield (an, fn, c, i)  -> Mremovefield (an, fn, f c, f i)
  | Mremovelocal (c, i)          -> Mremovelocal (f c, f i)
  | Mclearasset (an)             -> Mclearasset (an)
  | Mclearfield (an, fn, i)      -> Mclearfield (an, fn, f i)
  | Mclearlocal (i)              -> Mclearlocal (f i)
  | Mreverseasset (an)           -> Mreverseasset (an)
  | Mreversefield (an, fn, i)    -> Mreversefield (an, fn, f i)
  | Mreverselocal (i)            -> Mreverselocal (f i)
  | Mselect (an, c, p)           -> Mselect (an, f c, f p)
  | Msort (an, c, fn, k)         -> Msort (an, f c, fn, k)
  | Mcontains (an, c, i)         -> Mcontains (an, f c, f i)
  | Mmem (an, c, i)              -> Mmem (an, f c, f i)
  | Msubset (an, c, i)           -> Msubset (an, f c, f i)
  | Mnth (an, c, i)              -> Mnth (an, f c, f i)
  | Mcount (an, c)               -> Mcount (an, f c)
  | Msum (an, fd, c)             -> Msum (an, fd, f c)
  | Mmin (an, fd, c)             -> Mmin (an, fd, f c)
  | Mmax (an, fd, c)             -> Mmax (an, fd, f c)
  | Mfail (ft)                   -> Mfail (ft)
  | Mmathmin (l, r)              -> Mmathmin (f l, f r)
  | Mmathmax (l, r)              -> Mmathmax (f l, f r)
  | Mand (l, r)                  -> Mand (f l, f r)
  | Mor (l, r)                   -> Mor (f l, f r)
  | Mimply (l, r)                -> Mimply (f l, f r)
  | Mequiv  (l, r)               -> Mequiv (f l, f r)
  | Misempty (l, r)              -> Misempty (l, f r)
  | Mnot e                       -> Mnot (f e)
  | Mequal (l, r)                -> Mequal (f l, f r)
  | Mnequal (l, r)               -> Mnequal (f l, f r)
  | Mgt (l, r)                   -> Mgt (f l, f r)
  | Mge (l, r)                   -> Mge (f l, f r)
  | Mlt (l, r)                   -> Mlt (f l, f r)
  | Mle (l, r)                   -> Mle (f l, f r)
  | Mplus (l, r)                 -> Mplus (f l, f r)
  | Mminus (l, r)                -> Mminus (f l, f r)
  | Mmult (l, r)                 -> Mmult (f l, f r)
  | Mdiv (l, r)                  -> Mdiv (f l, f r)
  | Mmodulo (l, r)               -> Mmodulo (f l, f r)
  | Muplus e                     -> Muplus (f e)
  | Muminus e                    -> Muminus (f e)
  | Mrecord l                    -> Mrecord (List.map f l)
  | Mletin (i, a, t, b)          -> Mletin (i, f a, t, f b)
  | Mvarstorevar v               -> Mvarstorevar v
  | Mvarstorecol v               -> Mvarstorecol v
  | Mvarenumval v                -> Mvarenumval  v
  | Mvarfield v                  -> Mvarfield    v
  | Mvarlocal v                  -> Mvarlocal    v
  | Mvarparam v                  -> Mvarparam    v
  | Mvarthe                      -> Mvarthe
  | Mstate                       -> Mstate
  | Mnow                         -> Mnow
  | Mtransferred                 -> Mtransferred
  | Mcaller                      -> Mcaller
  | Mbalance                     -> Mbalance
  | Mnone                        -> Mnone
  | Msome v                      -> Msome (f v)
  | Marray l                     -> Marray (List.map f l)
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
  | Mdotasset (e, i)             -> Mdotasset (f e, i)
  | Mdotcontract (e, i)          -> Mdotcontract (f e, i)
  | Mtuple l                     -> Mtuple (List.map f l)
  | Massoc (k, v)                -> Massoc (f k, f v)
  | Mfor (i, c, b, lbl)          -> Mfor (i, f c, f b, lbl)
  | Mfold (i, is, c, b)          -> Mfold (i, is, f c, f b)
  | Mseq is                      -> Mseq (List.map f is)
  | Massign (op, l, r)           -> Massign (op, l, f r)
  | Massignfield (op, a, fi, r)  -> Massignfield (op, a, fi, f r)
  | Mtransfer (x, b, q)          -> Mtransfer (f x, b, q)
  | Mbreak                       -> Mbreak
  | Massert x                    -> Massert (f x)
  | Mreturn x                    -> Mreturn (f x)
  | Mlabel i                     -> Mlabel i
  | Mshallow (i, x)              -> Mshallow (i, f x)
  | Mlisttocoll (i, x)           -> Mlisttocoll (i, f x)
  | Munshallow (i, x)            -> Munshallow (i, f x)
  | Mtokeys (an, x)              -> Mtokeys (an, f x)
  | Mforall (i, t, e)            -> Mforall (i, t, f e)
  | Mexists (i, t, e)            -> Mexists (i, t, f e)
  | MsecMayBePerformedOnlyByRole   (l, r) -> MsecMayBePerformedOnlyByRole   (f l, f r)
  | MsecMayBePerformedOnlyByAction (l, r) -> MsecMayBePerformedOnlyByAction (f l, f r)
  | MsecMayBePerformedByRole       (l, r) -> MsecMayBePerformedByRole       (f l, f r)
  | MsecMayBePerformedByAction     (l, r) -> MsecMayBePerformedByAction     (f l, f r)
  | MsecTransferredBy              a      -> MsecTransferredBy              (f a)
  | MsecTransferredTo              a      -> MsecTransferredTo              (f a)
  | Manyaction                   -> Manyaction

let map_gen_mterm g f (i : 'id mterm_gen) : 'id mterm_gen =
  {
    i with
    node = g f i.node
  }

let map_mterm f t =
  map_gen_mterm map_term_node f t

type 'id ctx_model_gen = {
  formula: bool;
  fs : 'id function_struct_gen option;
  label: 'id option;
  spec_id : 'id option;
  invariant_id : 'id option;
}

type ctx_model = lident ctx_model_gen

let mk_ctx_model ?(formula = false) ?fs ?label ?spec_id ?invariant_id () : 'id ctx_model_gen =
  { formula; fs; label; spec_id; invariant_id }

let map_mterm_model_exec (f : ctx_model -> mterm -> mterm) (model : model) : model =
  let map_storage_item (ctx : ctx_model) (si : storage_item) : storage_item = (
    { si with
      default = f ctx si.default;
    }
  ) in
  let map_function_struct (ctx : ctx_model) (fs : function_struct) : function_struct =
    let ctx = { ctx with fs = Some fs } in
    let body = f ctx fs.body in
    { fs with body = body }
  in
  let map_function (ctx : ctx_model) (fun_ : function__) : function__ = (
    let node = match fun_.node with
      | Function (fs, ret) -> Function (map_function_struct ctx fs, ret)
      | Entry fs -> Entry (map_function_struct ctx fs)
    in
    { fun_ with node = node}
  ) in

  let ctx = mk_ctx_model () in
  let storage = List.map (map_storage_item ctx) model.storage in
  let functions = List.map (map_function ctx) model.functions in
  { model with
    functions = functions;
    storage = storage;
  }

let map_mterm_model_formula (f : ctx_model -> mterm -> mterm) (model : model) : model =
  let map_verification (ctx : ctx_model) (f : ctx_model -> mterm -> mterm) (v : verification) : verification = (
    let map_label_term (f : ctx_model -> mterm -> mterm) (lt : label_term) : label_term =
      let ctx = { ctx with label = lt.label } in
      { lt with
        term = f ctx lt.term }
    in

    let map_predicate (f : ctx_model -> mterm -> mterm) (p : predicate) : predicate =
      { p with
        args = List.map (fun (x, y) -> (x, y)) p.args;
        body = f ctx p.body;
      }
    in

    let map_definition (f : ctx_model -> mterm -> mterm) (d : definition) : definition =
      { d with
        body = f ctx d.body
      }
    in

    let map_invariantt (f : ctx_model -> mterm -> mterm) ((it_id, it_lt) : 'id * 'id label_term_gen list) : 'id * 'id label_term_gen list =
      (it_id, List.map (map_label_term f) it_lt)
    in

    let map_invariant (f : ctx_model -> mterm -> mterm) (spec : invariant) : invariant =
      let ctx = {ctx with invariant_id = Some spec.label } in
      { spec with
        formulas = List.map (f ctx) spec.formulas;
      }
    in

    let map_specification (f : ctx_model -> mterm -> mterm) (spec : specification) : specification =
      let ctx = { ctx with spec_id = Some spec.name} in
      { spec with
        formula = f ctx spec.formula;
        invariants = List.map (map_invariant f) spec.invariants;
      }
    in

    let map_variable (f : ctx_model -> mterm -> mterm) (spec : variable) : variable =
      spec
    in

    let ctx = { ctx with formula = true} in
    { v with
      predicates = List.map (map_predicate f) v.predicates;
      definitions = List.map (map_definition f) v.definitions;
      lemmas = List.map (map_label_term f) v.lemmas;
      theorems = List.map (map_label_term f) v.theorems;
      variables = List.map (map_variable f) v.variables;
      invariants = List.map (map_invariantt f) v.invariants;
      effects = List.map (f ctx) v.effects;
      specs = List.map (map_specification f) v.specs;
    }
  ) in

  let ctx : ctx_model = mk_ctx_model () in

  let map_function (f : ctx_model -> mterm -> mterm) (fun_ : function__) : function__ =
    let fs : function_struct =
      match fun_.node with
      | Function (fs, _) -> fs
      | Entry fs -> fs
    in
    let ctx = { ctx with fs = Some fs } in
    { fun_ with
      verif = Option.map (map_verification ctx f) fun_.verif;
    }
  in

  { model with
    functions = List.map (map_function f) model.functions;
    verification = map_verification ctx f model.verification
  }


let map_mterm_model (f : ctx_model -> mterm -> mterm) (model : model) : model =
  model
  |> map_mterm_model_exec f
  |> map_mterm_model_formula f

let fold_term (f : 'a -> ('id mterm_gen) -> 'a) (accu : 'a) (term : 'id mterm_gen) : 'a =
  match term.node with
  | Mif (c, t, e)                         ->
    begin
      let accu = f (f accu c) t in
      match e with
      | Some v -> f accu v
      | None -> accu
    end
  | Mmatchwith (e, l)                     -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Mapp (_, args)                        -> List.fold_left f accu args
  | Msetbefore    e                       -> f accu e
  | Msetunmoved   e                       -> f accu e
  | Msetadded     e                       -> f accu e
  | Msetremoved   e                       -> f accu e
  | Msetiterated  e                       -> f accu e
  | Msettoiterate e                       -> f accu e
  | Mexternal (t, func, c, args)          -> List.fold_left f (f accu c) args
  | Mget (_, k)                           -> f accu k
  | Mset (c, k, v)                        -> f (f accu v) k
  | Maddasset (an, i)                     -> f accu i
  | Maddfield (an, fn, c, i)              -> f (f accu c) i
  | Maddlocal (c, i)                      -> f (f accu c) i
  | Mremoveasset (an, i)                  -> f accu i
  | Mremovefield (an, fn, c, i)           -> f (f accu c) i
  | Mremovelocal (c, i)                   -> f (f accu c) i
  | Mclearasset (an)                      -> accu
  | Mclearfield (an, fn, c)               -> f accu c
  | Mclearlocal (c)                       -> f accu c
  | Mreverseasset (an)                    -> accu
  | Mreversefield (an, fn, c)             -> f accu c
  | Mreverselocal (c)                     -> f accu c
  | Mselect (an, c, p)                    -> f (f accu c) p
  | Msort (an, c, p, _)                   -> f accu c
  | Mcontains (an, c, i)                  -> f (f accu c) i
  | Mmem (an, c, i)                       -> f (f accu c) i
  | Msubset (an, c, i)                    -> f (f accu c) i
  | Mnth      (an, c, i)                  -> f (f accu c) i
  | Mcount (an, c)                        -> f accu c
  | Msum (an, fd, c)                      -> f accu c
  | Mmin (an, fd, c)                      -> f accu c
  | Mmax (an, fd, c)                      -> f accu c
  | Mfail (ft)                            -> accu
  | Mmathmax (l, r)                       -> f (f accu l) r
  | Mmathmin (l, r)                       -> f (f accu l) r
  | Mand (l, r)                           -> f (f accu l) r
  | Mor (l, r)                            -> f (f accu l) r
  | Mimply (l, r)                         -> f (f accu l) r
  | Mequiv  (l, r)                        -> f (f accu l) r
  | Misempty  (l, r)                      -> f accu r
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
  | Mvarparam _                           -> accu
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
  | Mdotasset (e, i)                      -> f accu e
  | Mdotcontract (e, i)                   -> f accu e
  | Mstate                                -> accu
  | Mnow                                  -> accu
  | Mtransferred                          -> accu
  | Mcaller                               -> accu
  | Mbalance                              -> accu
  | Mnone                                 -> accu
  | Msome v                               -> f accu v
  | Mtuple l                              -> List.fold_left f accu l
  | Massoc (k, v)                         -> f (f accu k) v
  | Mfor (i, c, b, lbl)                   -> f (f accu c) b
  | Mfold (i, is, c, b)                   -> f (f accu c) b
  | Mseq is                               -> List.fold_left f accu is
  | Massign (_, _, e)                     -> f accu e
  | Massignfield (_, _, _, e)             -> f accu e
  | Mtransfer (x, _, _)                   -> f accu x
  | Mbreak                                -> accu
  | Massert x                             -> f accu x
  | Mreturn x                             -> f accu x
  | Mlabel _                              -> accu
  | Mshallow (_, x)                       -> f accu x
  | Mlisttocoll (_, x)                    -> f accu x
  | Munshallow (_, x)                     -> f accu x
  | Mtokeys (_, x)                        -> f accu x
  | Mforall (_, _, e)                     -> f accu e
  | Mexists (_, _, e)                     -> f accu e
  | MsecMayBePerformedOnlyByRole   (l, r) -> f (f accu l) r
  | MsecMayBePerformedOnlyByAction (l, r) -> f (f accu l) r
  | MsecMayBePerformedByRole       (l, r) -> f (f accu l) r
  | MsecMayBePerformedByAction     (l, r) -> f (f accu l) r
  | MsecTransferredBy              a      -> f accu a
  | MsecTransferredTo              a      -> f accu a
  | Manyaction                            -> accu

let fold_map_term_list f acc l : 'term list * 'a =
  List.fold_left
    (fun (pterms, accu) x ->
       let p, accu = f accu x in
       pterms @ [p], accu) ([], acc) l

let fold_map_term
    (g : ('id, 'id mterm_gen) mterm_node -> 'id mterm_gen)
    (f : 'a -> 'id mterm_gen -> 'id mterm_gen * 'a)
    (accu : 'a)
    (term : 'id mterm_gen) : 'id mterm_gen * 'a =

  match term.node with
  | Mif (c, t, e) ->
    let ce, ca = f accu c in
    let ti, ta = f ca t in
    let ei, ea =
      match e with
      | Some v ->
        let a, b = f ta v in
        Some a, b
      | None -> None, ca
    in
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
    let ke, ka = f accu k in
    g (Mget (c, ke)), ka

  | Mset (c, k, v) ->
    let ke, ka = f accu k in
    let ve, va = f ka v in
    g (Mset (c, ke, ve)), va

  | Maddasset (an, i) ->
    let ie, ia = f accu i in
    g (Maddasset (an, ie)), ia

  | Maddfield (an, fn, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Maddfield (an, fn, ce, ie)), ia

  | Maddlocal (c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Maddlocal (ce, ie)), ia

  | Mremoveasset (an, i) ->
    let ie, ia = f accu i in
    g (Mremoveasset (an, ie)), ia

  | Mremovefield (an, fn, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mremovefield (an, fn, ce, ie)), ia

  | Mremovelocal (c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mremovelocal (ce, ie)), ia

  | Mclearasset (an) ->
    g (Mclearasset (an)), accu

  | Mclearfield (an, fn, i) ->
    let ie, ia = f accu i in
    g (Mclearfield (an, fn, ie)), ia

  | Mclearlocal i ->
    let ie, ia = f accu i in
    g (Mclearlocal (ie)), ia

  | Mreverseasset (an) ->
    g (Mreverseasset (an)), accu

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

  | Mmem (an, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mmem (an, ce, ie)), ia

  | Msubset (an, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Msubset (an, ce, ie)), ia

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

  | Mfail ft ->
    let fte, fta =
      match ft with
      | Invalid mt ->
        let mte, accu = f accu mt in
        Invalid mte, accu
      | _ -> ft, accu
    in
    g (Mfail fte), fta

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

  | Misempty  (l, r) ->
    let re, ra = f accu r in
    g (Misempty (l, re)), ra

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
    let le, la = fold_map_term_list f accu l in
    g (Mrecord le), la

  | Mletin (idd, i, t, o) ->
    let ie, ia = f accu i in
    let oe, oa = f ia o in
    g (Mletin (idd, i, t, oe)), oa

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

  | Mvarparam v ->
    g (Mvarparam v), accu

  | Mvarthe ->
    g Mvarthe, accu

  | Marray l ->
    let le, la = fold_map_term_list f accu l in
    g (Marray le), la

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

  | Mdotasset (e, i) ->
    let ee, ea = f accu e in
    g (Mdotasset (ee, i)), ea

  | Mdotcontract (e, i) ->
    let ee, ea = f accu e in
    g (Mdotcontract (ee, i)), ea

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

  | Mnone ->
    g Mnone, accu

  | Msome v ->
    let ve, va = f accu v in
    g (Msome ve), va

  | Mtuple l ->
    let le, la = fold_map_term_list f accu l in
    g (Mtuple le), la

  | Massoc (k, v) ->
    let ke, ka = f accu k in
    let ve, va = f ka v in
    g (Massoc (ke, ve)), va

  | Mfor (i, c, b, lbl) ->
    let ce, ca = f accu c in
    let bi, ba = f ca b in
    g (Mfor (i, ce, bi, lbl)), ba

  | Mfold (i, is, c, b) ->
    let ce, ca = f accu c in
    let bi, ba = f ca b in
    g (Mfold (i, is, ce, bi)), ba

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

  | Mlabel i ->
    g (Mlabel i), accu

  | Mshallow (i, x) ->
    let xe, xa = f accu x in
    g (Mshallow (i,xe)), xa

  | Mlisttocoll (i, x) ->
    let xe, xa = f accu x in
    g (Mlisttocoll (i,xe)), xa

  | Munshallow (i, x) ->
    let xe, xa = f accu x in
    g (Munshallow (i, xe)), xa

  | Mtokeys (an, x) ->
    let xe, xa = f accu x in
    g (Mtokeys (an, xe)), xa

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

  | Manyaction ->
    g (Manyaction), accu

let fold_left g l accu = List.fold_left (fun accu x -> g x accu) accu l

let fold_verification (ctx : 'id ctx_model_gen) (f : 'id ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (v : 'id verification_gen) (accu : 'a) : 'a =
  let fold_label_term (ctx : 'id ctx_model_gen) (f : 'id ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (lt : 'id label_term_gen) (accu : 'a) : 'a =
    let ctx = { ctx with label = lt.label } in
    f ctx accu lt.term
  in

  let fold_predicate (ctx : 'id ctx_model_gen) (f : 'id ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (p : 'id predicate_gen) (accu : 'a) : 'a =
    accu
    |> fun x -> f ctx x p.body
  in
  let fold_definition (ctx : 'id ctx_model_gen) (f : 'id ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (d : 'id definition_gen) (accu : 'a) : 'a =
    f ctx accu d.body
  in

  let fold_invariantt (ctx : 'id ctx_model_gen) (f : 'id ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (it : 'id * 'id label_term_gen list) (accu : 'a) : 'a =
    List.fold_left (fun accu x -> fold_label_term ctx f x accu) accu (snd it)
  in

  let fold_invariant (ctx : 'id ctx_model_gen) (f : 'id ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (spec : 'id invariant_gen) (accu : 'a) : 'a =
    let ctx = {ctx with invariant_id = Some spec.label } in
    List.fold_left (f ctx) accu spec.formulas
  in

  let fold_specification (ctx : 'id ctx_model_gen) (f : 'id ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (spec : 'id specification_gen) (accu : 'a) : 'a =
    let ctx = { ctx with spec_id = Some spec.name} in
    accu
    |> (fun x -> f ctx x spec.formula)
    |> (fun x -> List.fold_left (fun accu (x : 'id invariant_gen) -> fold_invariant ctx f x accu) x spec.invariants)
  in

  let fold_variable (ctx : 'id ctx_model_gen) (f : 'id ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (spec : 'id variable_gen) (accu : 'a) : 'a =
    accu
  in

  let ctx = { ctx with formula = true } in
  accu
  |> fold_left (fold_predicate ctx f) v.predicates
  |> fold_left (fold_definition ctx f) v.definitions
  |> fold_left (fold_label_term ctx f) v.lemmas
  |> fold_left (fold_label_term ctx f) v.theorems
  |> fold_left (fold_variable ctx f) v.variables
  |> fold_left (fold_invariantt ctx f) v.invariants
  |> (fun x -> List.fold_left (fun accu x -> f ctx accu x) x v.effects)
  |> fold_left (fold_specification ctx f) v.specs

let fold_model (f : 'id ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (m : 'id model_gen) (accu : 'a) : 'a =


  let fold_action (ctx : 'id ctx_model_gen) (f : 'id ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (a : 'id function__gen) (accu : 'a) : 'a = (
    let accu : 'a = (
      match a.node with
      | Function (fs, _)
      | Entry fs -> fold_term (f {ctx with fs = Some fs}) accu fs.body
    ) in
    Option.map_dfl (fun (x : 'id verification_gen) -> fold_verification ctx f x accu) accu a.verif
  ) in

  let ctx  : ctx_model = mk_ctx_model () in

  accu
  |> fold_left (fold_action ctx f) m.functions
  |> fold_verification ctx f m.verification

(* -------------------------------------------------------------------- *)

let merge_seq (mt1 : mterm) (mt2 : mterm) : mterm =
  match mt1.node, mt2.node with
  | Mseq l1, Mseq l2 -> mk_mterm (Mseq (l1 @ l2)) mt2.type_
  | _, Mseq l -> mk_mterm (Mseq ([mt1] @ l)) mt2.type_
  | Mseq l, _ -> mk_mterm (Mseq (l @ [mt2])) mt2.type_
  | _ -> mk_mterm (Mseq [mt1; mt2]) mt2.type_

(* -------------------------------------------------------------------- *)

module Utils : sig

  val function_name_from_storage_const   : storage_const   -> string
  val function_name_from_container_const : container_const -> string
  val function_name_from_function_const  : function_const  -> string
  val function_name_from_builtin_const   : builtin_const  -> string
  val get_assets                         : model -> info_asset list
  val get_records                        : model -> record list
  val get_storage                        : model -> storage
  val get_info_asset                     : model -> lident -> info_asset
  val get_asset_field                    : model -> (lident * ident) -> (ident * type_ * mterm option)
  val get_asset_key                      : model -> lident -> (ident * btyp)
  val get_field_container                : model -> ident -> ident -> (ident * container)
  val is_storage_attribute               : model -> lident -> bool
  val get_named_field_list               : model -> lident -> 'a list -> (ident * 'a) list
  val get_partitions                     : model -> (ident * ident * type_) list (* record id, record item *)
  val dest_partition                     : type_ -> lident
  val get_partition_asset_key            : model -> lident -> lident -> (ident * ident * btyp)
  val get_entries                        : model -> (verification option * function_struct) list
  val get_functions                      : model -> (verification option * function_struct* type_) list
  val has_partition                      : model -> ident -> bool
  val get_asset_partitions               : model -> ident -> (ident * type_ * mterm option) list
  val get_field_list                     : model -> lident -> ident list
  val get_field_pos                      : model -> lident -> lident -> int (* m, record, field *)
  val get_nth_record_val                 : int -> mterm -> mterm
  val dest_array                         : mterm -> mterm list
  val get_asset_type                     : mterm -> lident
  val is_local_assigned                  : lident -> mterm -> bool
  val get_function_args                  : function__ -> argument list
  val set_function_args                  : function__ -> argument list -> function__
  val map_function_terms                 : (mterm -> mterm) -> function__ -> function__
  val is_record                          : mterm -> bool
  val is_varlocal                        : mterm -> bool
  val dest_varlocal                      : mterm -> lident
  val is_container                       : type_ -> bool
  val get_key_pos                        : model -> lident -> int
  val get_loop_invariants                : model -> (lident * mterm) list -> ident -> (lident * mterm) list
  val get_formula                       : model -> mterm option -> ident -> mterm option
  val is_post                            : specification -> bool
  val get_sum_fields                     : model -> ident -> ident list
  val get_added_removed_sets             : model -> verification option -> ((lident, lident mterm_gen) mterm_node) list
  val get_storage_invariants             : model -> (ident * ident * mterm) list

end = struct

  open Tools
  open Location

  exception Anomaly of string

  type error_desc =
    | AssetNotFound of string
    | AssetFieldNotFound of string * string
    | AssetKeyTypeNotFound of string
    | NotaPartition
    | PartitionNotFound
    | NotanArray
    | NotaRecord of mterm
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
    | Get            aid       -> "get_"            ^ aid
    | Set            aid       -> "set_"            ^ aid
    | Add            aid       -> "add_"            ^ aid
    | Remove         aid       -> "remove_"         ^ aid
    | Clear          aid       -> "clear_"          ^ aid
    | Reverse        aid       -> "reverse_"        ^ aid
    | UpdateAdd     (aid, fid) -> "update_add_"     ^ aid ^ "_" ^ fid
    | UpdateRemove  (aid, fid) -> "update_remove_"  ^ aid ^ "_" ^ fid
    | UpdateClear   (aid, fid) -> "update_clear_"   ^ aid ^ "_" ^ fid
    | UpdateReverse (aid, fid) -> "update_reverse_" ^ aid ^ "_" ^ fid
    | ToKeys         aid       -> "to_keys_" ^ aid

  let function_name_from_container_const = function
    | AddItem            _ -> "add"
    | RemoveItem         _ -> "remove"
    | ClearItem          _ -> "clear"
    | ReverseItem        _ -> "reverse"

  let function_name_from_function_const = function
    | Select   (aid, _)    -> "select_"   ^ aid
    | Sort     (aid, fid)  -> "sort_"     ^ aid ^ "_" ^ fid
    | Contains  aid        -> "contains_" ^ aid
    | Nth       aid        -> "nth_"      ^ aid
    | Count     aid        -> "count_"    ^ aid
    | Sum      (aid, fid)  -> "sum_"      ^ aid ^ "_" ^ fid
    | Min      (aid, fid)  -> "min_"      ^ aid ^ "_" ^ fid
    | Max      (aid, fid)  -> "max_"      ^ aid ^ "_" ^ fid
    | Shallow  (aid)       -> "shallow_"  ^ aid
    | Unshallow (aid)      -> "unshallow" ^ aid
    | Listtocoll aid       -> "listtocoll_" ^ aid

  let function_name_from_builtin_const = function
    | MinBuiltin         _ -> "min"
    | MaxBuiltin         _ -> "max"

  let get_function_args (f : function__) : argument list =
    match f.node with
    | Function (s,_) -> s.args
    | Entry s        -> s.args

  let set_function_args (f : function__) (args : argument list) : function__ =
    match f.node with
    | Function (s,t) -> { node = Function ({ s with args = args },t); verif = f.verif }
    | Entry s        -> { node = Entry { s with args = args }; verif = f.verif }

  let is_asset (i : info_item) : bool =
    match i with
    | Iasset _ -> true
    | _        -> false

  let is_entry (f : function__) : bool =
    match f with
    | { node = Entry _; verif = _ } -> true
    | _                             -> false

  let is_function (f : function__) : bool =
    match f with
    | { node = Function _; verif = _ } -> true
    | _                                -> false

  let get_entry (f : function__) : verification option * function_struct =
    match f with
    | { node = Entry s; verif = v } -> (v,s)
    | _                             -> assert false

  let get_function (f : function__) : verification option * function_struct * type_ =
    match f with
    | { node = Function (s,t); verif = v } -> (v,s,t)
    | _                             -> assert false

  let get_entries m = List.filter is_entry m.functions |> List.map get_entry

  let get_functions m = List.filter is_function m.functions |> List.map get_function

  let dest_asset  = function
    | Iasset i -> i
    | _ -> emit_error NotaPartition

  let dest_array (t : mterm)  =
    match t.node with
    | Marray l -> l
    | _ -> emit_error NotanArray

  let get_nth_record_val pos (t : mterm) =
    match t.node with
    | Mrecord l -> List.nth l pos
    | _ -> emit_error (NotaRecord t)

  let get_asset_type (t : mterm) : lident =
    match t.type_ with
    | Tasset n -> n
    | Tcontainer (Tasset n, _) -> n
    | _ -> emit_error NotanAssetType

  let get_assets m = m.info |> List.filter is_asset |> List.map dest_asset

  let is_record (d : decl_node) : bool =
    match d with
    | Drecord _ -> true
    | _          -> false

  let dest_record  = function
    | Drecord r -> r
    | _ -> emit_error NotaPartition

  let get_records m = m.decls |> List.filter is_record |> List.map dest_record

  let get_info_asset model record_name : info_asset =
    let id = unloc record_name in
    let res = List.fold_left (fun accu (x : info_item) ->
        match x with
        | Iasset r when String.equal (unloc record_name) r.name -> Some r
        | _ -> accu
      ) None model.info in
    match res with
    | Some v -> v
    | _ -> emit_error (AssetNotFound id)

  let get_partitions m : (ident * ident * type_) list=
    get_assets m |> List.fold_left (fun acc (info : info_asset) ->
        acc @ (List.fold_left (fun acc (i,t,_) ->
            match t with
            | Tcontainer (Tasset _, Partition) ->
              acc @ [info.name,i,t]
            | _ -> acc
          ) [] info.values)
      ) []

  let has_partition m asset : bool =
    get_assets m |> List.fold_left (fun acc (info : info_asset) ->
        if compare asset info.name = 0 then
          (List.fold_left (fun acc (_,t,_) ->
               match t with
               | Tcontainer (Tasset _, Partition) -> true
               | _ -> acc
             ) false info.values)
        else
          acc
      ) false


  let get_asset_partitions m asset : (ident * type_ * (lident mterm_gen option)) list =
    get_assets m |> List.fold_left (fun acc (info : info_asset) ->
        if compare asset info.name = 0 then
          (List.fold_left (fun acc (i,t,d) ->
               match t with
               | Tcontainer (Tasset _, Partition) ->
                 acc @ [i,t,d]
               | _ -> acc
             ) [] info.values)
        else
          acc
      ) []

  let dest_partition = function
    | Tcontainer (Tasset p,Partition) -> p
    | _ -> assert false

  let get_asset_field model (record_name, field_name) =
    let asset = get_info_asset model record_name in
    let res = List.fold_left (fun accu (i,t,d : ident * type_ * (lident mterm_gen option)) ->
        if String.equal field_name i then
          Some (i,t,d)
        else accu) None asset.values in
    match res with
    | Some v -> v
    | _ -> emit_error (AssetFieldNotFound (unloc record_name, field_name))

  let get_asset_key model record_name : (ident * btyp) =
    let asset = get_info_asset model record_name in
    let key_id = asset.key in
    let (_,key_typ,_) = get_asset_field model (record_name, key_id) in
    match key_typ with
    | Tbuiltin v -> (key_id, v)
    | _ -> emit_error (AssetKeyTypeNotFound (unloc record_name))

  let get_field_container model asset_name field_name : ident * container =
    let (_,typ,_) = get_asset_field model (dumloc asset_name, field_name) in
    match typ with
    | Tcontainer (Tasset an, c) -> (unloc an, c)
    | _ -> assert false

  (* returns : asset name, key name, key type *)
  let get_partition_asset_key model record field : (ident * ident * btyp) =
    let partitions = get_partitions model in
    let rec rec_get = function
      | (r,i,t) :: tl when compare r record.pldesc = 0 &&
                           compare i field.pldesc = 0 ->
        let pa  = dest_partition t in
        let k,t = get_asset_key model pa in
        (unloc pa,k,t)
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

  let get_field_list (model : model) (record_name : lident) : ident list =
    let asset = get_info_asset model record_name in
    List.map (fun (i,_,_) -> i) asset.values

  let get_field_pos model record field =
    let l = get_field_list model record in
    let rec rec_get_pos i = function
      | e :: tl when compare field.pldesc e = 0 -> i
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


  let map_invariant_terms (m : mterm -> mterm) (i : invariant) : invariant = {
    i with
    formulas = List.map m i.formulas
  }

  let map_specification_terms (m : mterm -> mterm) (s : specification) : specification = {
    s with
    formula = m s.formula;
    invariants = List.map (map_invariant_terms m) s.invariants
  }

  let map_verification_terms (m : mterm -> mterm) (v : verification) : verification = {
    v with
    specs = List.map (map_specification_terms m) v.specs
  }


  let map_function_terms (m : mterm -> mterm) (f : function__) : function__ = {
    node = begin
      match f.node with
      | Function (s,r) -> Function ({
          s with body = m s.body;
        },r)
      | Entry s        -> Entry {
          s with body = m s.body;
        }
    end;
    verif = Option.map (map_verification_terms m) f.verif;
  }

  let is_record (t : mterm) =
    match t.node with
    | Mrecord _ -> true
    | _ -> false

  let is_varlocal (t : mterm) =
    match t.node with
    | Mvarlocal _ -> true
    | _ -> false

  let dest_varlocal (t : mterm) =
    match t.node with
    | Mvarlocal i -> i
    | _ -> assert false


  let is_container t =
    match t with
    | Tcontainer ((Tasset _),_) -> true
    | _ -> false


  let get_key_pos m n : int =
    get_assets m |> List.fold_left (fun acc (info : info_asset) ->
        if compare (unloc n) info.name = 0 then
          let (k,_) = get_asset_key m n in
          (List.fold_left (fun acc (i,_,_) ->
               if compare i k = 0 then
                 succ acc
               else
                 acc
             ) acc info.values)
        else
          acc
      ) (-1)

  (* i is the loop label *)
  let get_loop_invariants m acc (i : ident) : (lident * mterm) list =
    let internal_get (ctx : ctx_model) (acc : (lident * mterm) list) t =
      match ctx.invariant_id with
      | Some v when cmp_ident i (unloc v) ->
        begin
          match ctx.spec_id with
          | Some l -> acc @ [l,t]
          | _ -> acc
        end
      | _ -> acc in
    fold_model internal_get m acc

  let get_formula m acc (i : ident) : mterm option =
    let internal_get (ctx : ctx_model) (acc : mterm option) t =
      match acc, ctx.spec_id with
      | None, Some v when cmp_ident i (unloc v) -> Some t
      | _ -> acc in
    fold_model internal_get m acc

  let is_post (s : specification) =
    match s.mode with
    | Post -> true
    | _ -> false

  let get_sum_fields m a =
    List.fold_left (fun acc (ai : api_item) ->
        match ai.node_item with
        | APIFunction (Sum (asset,field)) when compare a asset = 0 ->
          acc @ [field]
        | _ -> acc
      ) [] m.api_items

  let get_added_removed_sets m v : ((lident,(lident mterm_gen)) mterm_node) list =
    let rec internal_fold_add_remove ctx acc (term : mterm) =
      match term.node with
      | Msetadded e -> acc @ [ Msetadded e ]
      | Msetremoved e -> acc @ [ Msetremoved e ]
      | _ -> fold_term (internal_fold_add_remove ctx) acc term in
    Tools.List.dedup (Option.map_dfl (fun (x : verification) ->
        fold_verification (mk_ctx_model ()) internal_fold_add_remove x []) [] v)

  (* returns asset name * invariant name * invariant term *)
  let get_storage_invariants (m : model) : (ident * ident * mterm) list =
    List.fold_left (fun acc (i : storage_item) ->
        let n = i.name |> unloc in
        List.fold_left (fun acc (lt : label_term) ->
            let inv_name = Tools.Option.fold (fun _ l -> unloc l) "" lt.label in
            let inv_term = lt.term in
            acc @ [n, inv_name, inv_term]
          ) acc i.invariants
      ) [] m.storage

end
