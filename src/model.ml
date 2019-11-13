open Ident
open Tools

type lident = ident Location.loced
[@@deriving show {with_path = false}]

type currency =
  | Tz
  | Mtz
  | Mutz
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
  | Bduration  | Bstring
  | Baddress
  | Brole
  | Bcurrency
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
  | Tstate
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

type comparison_operator =
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
  | Maddshallow   of ident * 'term list
  | Mexternal     of 'id * 'id * 'term * ('term) list
  | Mget          of ident * 'term
  | Mgetbefore    of ident * 'term
  | Mgetat        of ident * ident * 'term (* asset_name * label * value *)
  | Mset          of ident * ident list * 'term * 'term (*asset_name * field_name modified * ... *)
  | Maddasset     of ident * 'term
  | Maddfield     of ident * ident * 'term * 'term (* asset_name * field_name * asset instance * item * shalow values*)
  | Maddlocal     of 'term * 'term
  | Mremoveasset  of ident * 'term
  | Mremovefield  of ident * ident * 'term * 'term
  | Mremovelocal  of 'term * 'term
  | Mremoveif     of ident * 'term * 'term
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
  | Msubsetof     of ident * 'term * 'term
  | Mnth          of ident * 'term * 'term
  | Mcount        of ident * 'term
  | Msum          of ident * 'id * 'term
  | Mmin          of ident * 'id * 'term
  | Mmax          of ident * 'id * 'term
  | Mmathmax      of 'term * 'term
  | Mmathmin      of 'term * 'term
  | Mhead         of ident * 'term * 'term
  | Mtail         of ident * 'term * 'term
  | Mfail         of 'id fail_type_gen
  | Mand          of 'term * 'term
  | Mor           of 'term * 'term
  | Mimply        of 'term * 'term
  | Mequiv        of 'term * 'term
  | Misempty      of ident * 'term
  | Mnot          of 'term
  | Mmulticomp    of 'term * (comparison_operator * 'term) list
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
  | Masset       of 'term list
  | Mletin        of 'id list * 'term * type_ option * 'term * 'term option
  | Mdeclvar      of 'id list * type_ option * 'term
  | Mvarstorevar  of 'id
  | Mvarstorecol  of 'id
  | Mvarenumval   of 'id
  | Mvarlocal     of 'id
  | Mvarparam     of 'id
  | Mvarfield     of 'id
  | Mvarthe
  | Mvarstate
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
  | Mduration     of Core.duration
  | Mdotasset     of 'term * 'id
  | Mdotcontract  of 'term * 'id
  | Mtuple        of 'term list
  | Massoc        of 'term * 'term
  | Mfor          of ('id * 'term * 'term * ident option)
  | Miter          of ('id * 'term * 'term * 'term * ident option)
  | Mfold         of ('id * 'id list * 'term * 'term) (* ident list * collection * body *)
  | Mseq          of 'term list
  | Massign       of (assignment_operator * 'id * 'term)
  | Massignfield  of (assignment_operator * 'term * 'id * 'term)
  | Massignstate  of 'term
  | Mtransfer     of ('term * 'term) (* value * dest *)
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
  | Mforall       of 'id * type_ * 'term option * 'term
  | Mexists       of 'id * type_ * 'term option * 'term
  (* security predicates *)
  | Msetbefore    of 'term
  | Msetat        of ident * 'term
  | Msetunmoved   of 'term
  | Msetadded     of 'term
  | Msetremoved   of 'term
  | Msetiterated  of 'term
  | Msettoiterate of 'term
[@@deriving show {with_path = false}]

and 'id mterm_gen = {
  node: ('id, 'id mterm_gen) mterm_node;
  type_: type_;
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
  | Head             of ident
  | Tail             of ident
[@@deriving show {with_path = false}]

and builtin_const =
  | MinBuiltin of type_
  | MaxBuiltin of type_
[@@deriving show {with_path = false}]

and api_item_node =
  | APIStorage      of storage_const
  | APIContainer    of container_const
  | APIFunction     of function_const
  | APIBuiltin      of builtin_const
[@@deriving show {with_path = false}]

and api_item = {
  node_item: api_item_node;
  only_formula: bool;
}
[@@deriving show {with_path = false}]

and api_verif =
  | StorageInvariant of (ident * ident * mterm)

and action_description =
  | ADany
  | ADadd      of ident
  | ADremove   of ident
  | ADupdate   of ident
  | ADtransfer of ident
  | ADget      of ident
  | ADiterate  of ident
  | ADcall     of ident
[@@deriving show {with_path = false}]

and security_role   = lident
[@@deriving show {with_path = false}]

and security_action =
  | Sany
  | Sentry of lident list
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
  sort: ident list;
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

type model_type =
  | MTvar
  | MTconst
  | MTasset of ident
  | MTstate
  | MTenum of ident
[@@deriving show {with_path = false}]

type 'id storage_item_gen = {
  id          : 'id;
  model_type  : model_type;
  typ         : type_;
  ghost       : bool;
  default     : 'id mterm_gen; (* initial value *)
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

type 'id var_gen = {
  name: 'id;
  type_: type_;
  constant: bool;
  default: 'id mterm_gen option;
  invariants: 'id label_term_gen list;
  loc: Location.t;
}
[@@deriving show {with_path = false}]

type var = lident var_gen

type 'id enum_gen = {
  name: 'id;
  values: 'id enum_item_gen list;
}
[@@deriving show {with_path = false}]

type enum = lident enum_gen
[@@deriving show {with_path = false}]

type 'id asset_item_gen = {
  name: 'id;
  type_: type_;
  default: 'id mterm_gen option;
}
[@@deriving show {with_path = false}]

type asset_item = lident asset_item_gen
[@@deriving show {with_path = false}]

type 'id asset_gen = {
  name: 'id;
  key: 'id option;
  values: 'id asset_item_gen list;
  invariants  : lident label_term_gen list;
}
[@@deriving show {with_path = false}]

type asset = lident asset_gen
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

type source = Exo | Endo
[@@deriving show {with_path = false}]

type 'id function_struct_gen = {
  name: 'id;
  args: 'id argument_gen list;
  body: 'id mterm_gen;
  src : source;
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

type 'id postcondition_gen = {
  name: 'id;
  mode: spec_mode;
  formula: 'id mterm_gen;
  invariants: ('id invariant_gen) list;
  uses: 'id list;
}
[@@deriving show {with_path = false}]

type postcondition = lident postcondition_gen
[@@deriving show {with_path = false}]


type 'id assert_gen = {
  name: 'id;
  label: 'id;
  formula: 'id mterm_gen;
  invariants: 'id invariant_gen list;
  uses: 'id list;
}
[@@deriving show {with_path = false}]

type assert_ = lident assert_gen
[@@deriving show {with_path = false}]

type 'id specification_gen = {
  predicates     : 'id predicate_gen list;
  definitions    : 'id definition_gen list;
  lemmas         : 'id label_term_gen list;
  theorems       : 'id label_term_gen list;
  variables      : 'id variable_gen list;
  invariants     : ('id * 'id label_term_gen list) list;
  effects        : 'id mterm_gen list;
  postconditions : 'id postcondition_gen list;
  loc            : Location.t [@opaque];
}
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
  items : security_item list;
  loc   : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type specification = lident specification_gen
[@@deriving show {with_path = false}]

type 'id function__gen = {
  node:  'id function_node_gen;
  spec: 'id specification_gen option;
}
[@@deriving show {with_path = false}]

type function__ = lident function__gen
[@@deriving show {with_path = false}]

type 'id decl_node_gen =
  | Dvar of 'id var_gen
  | Denum of 'id enum_gen
  | Dasset of 'id asset_gen
  | Dcontract of 'id contract_gen
[@@deriving show {with_path = false}]

type decl_node = lident decl_node_gen
[@@deriving show {with_path = false}]

type 'id model_gen = {
  name          : lident;
  api_items     : api_item list;
  api_verif     : api_verif list;
  info          : info_item list;
  decls         : 'id decl_node_gen list;
  storage       : 'id storage_gen;
  functions     : 'id function__gen list;
  specification : 'id specification_gen;
  security      : security;
}
[@@deriving show {with_path = false}]

type property =
  | Ppostcondition of postcondition * ident option
  | PstorageInvariant of label_term
  | PsecurityPredicate of security_item
[@@deriving show {with_path = false}]

type model = lident model_gen
[@@deriving show {with_path = false}]

let mk_qualid ?(loc = Location.dummy) node type_ : 'id qualid_gen =
  { node; type_; loc}

let mk_pattern ?(loc = Location.dummy) node : 'id pattern_gen =
  { node; loc}

let mk_mterm ?(loc = Location.dummy) node type_ : 'id mterm_gen =
  { node; type_; loc}

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

let mk_postcondition ?(invariants = []) ?(uses = []) name mode formula =
  { name; mode; formula; invariants; uses }

let mk_assert ?(invariants = []) ?(uses = []) name label formula =
  { name; label; formula; invariants; uses }

let mk_specification ?(predicates = []) ?(definitions = []) ?(lemmas = []) ?(theorems = []) ?(variables = []) ?(invariants = []) ?(effects = []) ?(postconditions = []) ?(loc = Location.dummy) () =
  { predicates; definitions; lemmas; theorems; variables; invariants; effects; postconditions; loc}

let mk_security_predicate ?(loc = Location.dummy) s_node : security_predicate =
  { s_node; loc }

let mk_security_item ?(loc = Location.dummy) label predicate : security_item =
  { label; predicate; loc }

let mk_security ?(items = []) ?(loc = Location.dummy) () : security =
  { items; loc }

let mk_info_var ?(constant = false) ?init name type_ : info_var =
  { name; type_; constant; init}

let mk_info_enum ?(values = []) name : info_enum =
  { name; values }

let mk_info_asset ?(values = []) ?(sort = []) name key : info_asset =
  { name; key; sort; values }

let mk_info_contract ?(signatures = []) name : info_contract =
  { name; signatures }

let mk_var ?(constant=false) ?(invariants=[]) ?default ?(loc = Location.dummy) name type_ : 'id var_gen =
  { name; type_; default; constant; invariants; loc }

let mk_enum ?(values = []) name : 'id enum_gen =
  { name; values }

let mk_enum_item ?(invariants = []) name : 'id enum_item_gen =
  { name; invariants }

let mk_asset ?(values = []) ?(invariants = []) ?key name : 'id asset_gen =
  { name; key; values; invariants }

let mk_asset_item ?default name type_ : 'id asset_item_gen =
  { name; type_; default }

let mk_contract_signature ?(args=[]) ?(loc=Location.dummy) name : 'id contract_signature_gen =
  { name; args; loc }

let mk_contract ?(signatures=[]) ?init ?(loc=Location.dummy) name : 'id contract_gen =
  { name; signatures; init; loc }

let mk_storage_item ?(ghost = false) ?(loc = Location.dummy) id model_type typ default : 'id storage_item_gen =
  { id; model_type; typ; ghost; default; loc }

let mk_function_struct ?(args = []) ?(loc = Location.dummy) ?(src = Exo) name body : function_struct =
  { name; args; src; body; loc }

let mk_function ?spec node : 'id function__gen =
  { node; spec }

let mk_signature ?(args = []) ?ret name : 'id signature_gen =
  { name; args; ret }

let mk_api_item ?(only_formula = false) node_item =
  { node_item; only_formula }

let mk_model ?(api_items = []) ?(api_verif = []) ?(info = []) ?(decls = []) ?(functions = []) ?(storage = []) ?(specification = mk_specification ()) ?(security = mk_security ()) name : model =
  { name; api_items; api_verif; info; storage; decls; functions; specification; security }

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
let cmp_comparison_operator (op1 : comparison_operator) (op2 : comparison_operator) : bool = op1 = op2
let cmp_action_description (ad1 : action_description) (ad2 : action_description) : bool = ad1 = ad2
let cmp_security_role = cmp_lident
let cmp_security_action s1 s2 =
  match s1, s2 with
  | Sany, Sany -> true
  | Sentry e1, Sentry e2 -> List.for_all2 cmp_lident e1 e2
  | _ -> false

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
    | Maddshallow (e1, args1), Maddshallow (e2, args2)                                 -> cmp_ident e1 e2 && List.for_all2 cmp args1 args2
    | Mexternal (t1, func1, c1, args1), Mexternal (t2, func2, c2, args2)               -> cmpi t1 t2 && cmpi func1 func2 && cmp c1 c2 && List.for_all2 cmp args1 args2
    | Mget (c1, k1), Mget (c2, k2)                                                     -> cmp_ident c1 c2 && cmp k1 k2
    | Mgetbefore (c1, k1), Mgetbefore (c2, k2)                                         -> cmp_ident c1 c2 && cmp k1 k2
    | Mgetat (c1, d1, k1), Mgetat (c2, d2, k2)                                         -> cmp_ident c1 c2 && cmp_ident d1 d2 && cmp k1 k2
    | Mset (c1, l1, k1, v1), Mset (c2, l2, k2, v2)                                     -> cmp_ident c1 c2 && List.for_all2 cmp_ident l1 l2 && cmp k1 k2 && cmp v1 v2
    | Maddasset (an1, i1), Maddasset (an2, i2)                                         -> cmp_ident an1 an2 && cmp i1 i2
    | Maddfield (an1, fn1, c1, i1), Maddfield (an2, fn2, c2, i2)                       -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp c1 c2 && cmp i1 i2
    | Maddlocal (c1, i1), Maddlocal (c2, i2)                                           -> cmp c1 c2 && cmp i1 i2
    | Mremoveasset (an1, i1), Mremoveasset (an2, i2)                                   -> cmp_ident an1 an2 && cmp i1 i2
    | Mremovefield (an1, fn1, c1, i1), Mremovefield (an2, fn2, c2, i2)                 -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp c1 c2 && cmp i1 i2
    | Mremovelocal (c1, i1), Mremovelocal (c2, i2)                                     -> cmp c1 c2 && cmp i1 i2
    | Mclearasset (an1), Mclearasset (an2)                                             -> cmp_ident an1 an2
    | Mclearfield (an1, fn1, i1), Mclearfield (an2, fn2, i2)                           -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp i1 i2
    | Mremoveif (an1, fn1, i1), Mremoveif (an2, fn2, i2)                               -> cmp_ident an1 an2 && cmp fn1 fn2 && cmp i1 i2
    | Mclearlocal (i1), Mclearlocal (i2)                                               -> cmp i1 i2
    | Mreverseasset (an1), Mreverseasset (an2)                                         -> cmp_ident an1 an2
    | Mreversefield (an1, fn1, i1), Mreversefield (an2, fn2, i2)                       -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp i1 i2
    | Mreverselocal (i1), Mreverselocal (i2)                                           -> cmp i1 i2
    | Mselect (an1, c1, p1), Mselect (an2, c2, p2)                                     -> cmp_ident an1 an2 && cmp c1 c2 && cmp p1 p2
    | Msort (an1, c1, fn1, k1), Msort (an2, c2, fn2, k2)                               -> cmp_ident an1 an2 && cmp c1 c2 && cmp_ident fn1 fn2 && k1 = k2
    | Mcontains (an1, c1, i1), Mcontains (an2, c2, i2)                                 -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Mmem (an1, c1, i1), Mmem (an2, c2, i2)                                           -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Msubsetof (an1, c1, i1), Msubsetof (an2, c2, i2)                                 -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Mnth (an1, c1, i1), Mnth (an2, c2, i2)                                           -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Mcount (an1, c1), Mcount (an2, c2)                                               -> cmp_ident an1 an2 && cmp c1 c2
    | Msum (an1, fd1, c1), Msum (an2, fd2, c2)                                         -> cmp_ident an1 an2 && cmpi fd1 fd2 && cmp c1 c2
    | Mmin (an1, fd1, c1), Mmin (an2, fd2, c2)                                         -> cmp_ident an1 an2 && cmpi fd1 fd2 && cmp c1 c2
    | Mmax (an1, fd1, c1), Mmax (an2, fd2, c2)                                         -> cmp_ident an1 an2 && cmpi fd1 fd2 && cmp c1 c2
    | Mfail ft1, Mfail ft2                                                             -> cmp_fail_type cmp ft1 ft2
    | Mmathmin (l1, r1), Mmathmin (l2, r2)                                             -> cmp l1 l2 && cmp r1 r2
    | Mmathmax (l1, r1), Mmathmax (l2, r2)                                             -> cmp l1 l2 && cmp r1 r2
    | Mhead (an1, c1, i1), Mhead (an2, c2, i2)                                         -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Mtail (an1, c1, i1), Mtail (an2, c2, i2)                                         -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Mand (l1, r1), Mand (l2, r2)                                                     -> cmp l1 l2 && cmp r1 r2
    | Mor (l1, r1), Mor (l2, r2)                                                       -> cmp l1 l2 && cmp r1 r2
    | Mimply (l1, r1), Mimply (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Mequiv (l1, r1), Mequiv (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Misempty (l1, r1), Misempty (l2, r2)                                             -> cmp_ident l1 l2 && cmp r1 r2
    | Mnot e1, Mnot e2                                                                 -> cmp e1 e2
    | Mmulticomp (e1, l1), Mmulticomp (e2, l2)                                         -> cmp e1 e2 && List.for_all2 (fun (op1, t1) (op2, t2) -> cmp_comparison_operator op1 op2 && cmp t1 t2) l1 l2
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
    | Masset l1, Masset l2                                                           -> List.for_all2 cmp l1 l2
    | Mletin (i1, a1, t1, b1, o1), Mletin (i2, a2, t2, b2, o2)                         -> List.for_all2 cmpi i1 i2 && cmp a1 a2 && Option.cmp cmp_type t1 t2 && cmp b1 b2 && Option.cmp cmp o1 o2
    | Mdeclvar (i1, t1, v1), Mdeclvar (i2, t2, v2)                                     -> List.for_all2 cmpi i1 i2 && Option.cmp cmp_type t1 t2 && cmp v1 v2
    | Mvarstorevar v1, Mvarstorevar v2                                                 -> cmpi v1 v2
    | Mvarstorecol v1, Mvarstorecol v2                                                 -> cmpi v1 v2
    | Mvarenumval v1, Mvarenumval v2                                                   -> cmpi v1 v2
    | Mvarfield v1, Mvarfield v2                                                       -> cmpi v1 v2
    | Mvarlocal v1, Mvarlocal v2                                                       -> cmpi v1 v2
    | Mvarparam v1, Mvarparam v2                                                       -> cmpi v1 v2
    | Mvarthe, Mvarthe                                                                 -> true
    | Mvarstate, Mvarstate                                                             -> true
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
    | Mduration v1, Mduration v2                                                       -> Core.cmp_duration v1 v2
    | Mdotasset (e1, i1), Mdotasset (e2, i2)                                           -> cmp e1 e2 && cmpi i1 i2
    | Mdotcontract (e1, i1), Mdotcontract (e2, i2)                                     -> cmp e1 e2 && cmpi i1 i2
    | Mtuple l1, Mtuple l2                                                             -> List.for_all2 cmp l1 l2
    | Mfor (i1, c1, b1, lbl1), Mfor (i2, c2, b2, lbl2)                                 -> cmpi i1 i2 && cmp c1 c2 && cmp b1 b2 && Option.cmp cmp_ident lbl1 lbl2
    | Miter (i1, a1, b1, c1, lbl1), Miter (i2, a2, b2, c2, lbl2)                       -> cmpi i1 i2 && cmp a1 a2 && cmp b1 b2 && cmp c1 c2 && Option.cmp cmp_ident lbl1 lbl2
    | Mfold (i1, is1, c1, b1), Mfold (i2, is2, c2, b2)                                 -> cmpi i1 i2 && List.for_all2 cmpi is1 is2 && cmp c1 c2 && cmp b1 b2
    | Mseq is1, Mseq is2                                                               -> List.for_all2 cmp is1 is2
    | Massign (op1, l1, r1), Massign (op2, l2, r2)                                     -> cmp_assign_op op1 op2 && cmpi l1 l2 && cmp r1 r2
    | Massignfield (op1, a1, fi1, r1), Massignfield (op2, a2, fi2, r2)                 -> cmp_assign_op op1 op2 && cmp a1 a2 && cmpi fi1 fi2 && cmp r1 r2
    | Massignstate x1, Massignstate x2                                                 -> cmp x1 x2
    | Mtransfer (v1, d1), Mtransfer (v2, d2)                                           -> cmp v1 v2 && cmp d1 d2
    | Mbreak, Mbreak                                                                   -> true
    | Massert x1, Massert x2                                                           -> cmp x1 x2
    | Mreturn x1, Mreturn x2                                                           -> cmp x1 x2
    | Mforall (i1, t1, t2, e1), Mforall (i2, t3, t4, e2)                               -> cmpi i1 i2 && cmp_type t1 t3 && Option.cmp cmp t2 t4 && cmp e1 e2
    | Mexists (i1, t1, t2, e1), Mforall (i2, t3, t4, e2)                               -> cmpi i1 i2 && cmp_type t1 t3 && Option.cmp cmp t2 t4 && cmp e1 e2
    | Msetbefore e1, Msetbefore e2                                                     -> cmp e1 e2
    | Msetat (lbl1, e1), Msetat (lbl2, e2)                                             -> cmp_ident lbl1 lbl2 && cmp e1 e2
    | Msetunmoved e1, Msetunmoved e2                                                   -> cmp e1 e2
    | Msetadded e1, Msetadded e2                                                       -> cmp e1 e2
    | Msetremoved e1, Msetremoved   e2                                                 -> cmp e1 e2
    | Msetiterated e1, Msetiterated  e2                                                -> cmp e1 e2
    | Msettoiterate e1, Msettoiterate e2                                               -> cmp e1 e2
    | Mshallow (i1, x1), Mshallow (i2, x2)                                             -> cmp x1 x2 && cmp_ident i1 i2
    | Mlisttocoll (i1, x1), Mlisttocoll (i2, x2)                                       -> cmp x1 x2 && cmp_ident i1 i2
    | Munshallow (i1, x1), Munshallow (i2, x2)                                         -> cmp x1 x2 && cmp_ident i1 i2

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
  | APIStorage s1, APIStorage s2           -> cmp_storage_const s1 s2
  | APIContainer c1, APIContainer c2       -> cmp_container_const c1 c2
  | APIFunction f1, APIFunction f2         -> cmp_function_const f1 f2
  | APIBuiltin b1, APIBuiltin b2           -> cmp_builtin_const b1 b2
  | _ -> false

(* -------------------------------------------------------------------- *)

let cmp_api_verif (v1 : api_verif) (v2 : api_verif) : bool =
  match v1, v2 with
  | StorageInvariant (l1, an1, mt1), StorageInvariant (l2, an2, mt2) -> cmp_ident l1 l2 && cmp_ident an1 an2 && cmp_mterm mt1 mt2
(* | _ -> false *)

(* -------------------------------------------------------------------- *)

let map_term_node (f : 'id mterm_gen -> 'id mterm_gen) = function
  | Mif (c, t, e)                 -> Mif (f c, f t, Option.map f e)
  | Mmatchwith (e, l)             -> Mmatchwith (e, List.map (fun (p, e) -> (p, f e)) l)
  | Mapp (e, args)                -> Mapp (e, List.map f args)
  | Maddshallow (e, args)         -> Maddshallow (e, List.map f args)
  | Msetbefore    e               -> Msetbefore    (f e)
  | Msetat (lbl, e)               -> Msetat        (lbl, f e)
  | Msetunmoved   e               -> Msetunmoved   (f e)
  | Msetadded     e               -> Msetadded     (f e)
  | Msetremoved   e               -> Msetremoved   (f e)
  | Msetiterated  e               -> Msetiterated  (f e)
  | Msettoiterate e               -> Msettoiterate (f e)
  | Mexternal (t, func, c, args)  -> Mexternal (t, func, f c, List.map f args)
  | Mget (c, k)                   -> Mget (c, f k)
  | Mgetbefore (c, k)             -> Mgetbefore (c, f k)
  | Mgetat (c, d, k)              -> Mgetat (c, d, f k)
  | Mset (c, l, k, v)             -> Mset (c, l, f k, f v)
  | Maddasset (an, i)             -> Maddasset (an, f i)
  | Maddfield (an, fn, c, i)      -> Maddfield (an, fn, f c, f i)
  | Maddlocal (c, i)              -> Maddlocal (f c, f i)
  | Mremoveasset (an, i)          -> Mremoveasset (an, f i)
  | Mremovefield (an, fn, c, i)   -> Mremovefield (an, fn, f c, f i)
  | Mremovelocal (c, i)           -> Mremovelocal (f c, f i)
  | Mclearasset (an)              -> Mclearasset (an)
  | Mclearfield (an, fn, i)       -> Mclearfield (an, fn, f i)
  | Mremoveif (an, fn, i)         -> Mremoveif (an, f fn, f i)
  | Mclearlocal (i)               -> Mclearlocal (f i)
  | Mreverseasset (an)            -> Mreverseasset (an)
  | Mreversefield (an, fn, i)     -> Mreversefield (an, fn, f i)
  | Mreverselocal (i)             -> Mreverselocal (f i)
  | Mselect (an, c, p)            -> Mselect (an, f c, f p)
  | Msort (an, c, fn, k)          -> Msort (an, f c, fn, k)
  | Mcontains (an, c, i)          -> Mcontains (an, f c, f i)
  | Mmem (an, c, i)               -> Mmem (an, f c, f i)
  | Msubsetof (an, c, i)          -> Msubsetof (an, f c, f i)
  | Mnth (an, c, i)               -> Mnth (an, f c, f i)
  | Mcount (an, c)                -> Mcount (an, f c)
  | Msum (an, fd, c)              -> Msum (an, fd, f c)
  | Mmin (an, fd, c)              -> Mmin (an, fd, f c)
  | Mmax (an, fd, c)              -> Mmax (an, fd, f c)
  | Mfail (ft)                    -> Mfail (ft)
  | Mmathmin (l, r)               -> Mmathmin (f l, f r)
  | Mmathmax (l, r)               -> Mmathmax (f l, f r)
  | Mhead (an, c, i)              -> Mhead (an, f c, f i)
  | Mtail (an, c, i)              -> Mtail (an, f c, f i)
  | Mand (l, r)                   -> Mand (f l, f r)
  | Mor (l, r)                    -> Mor (f l, f r)
  | Mimply (l, r)                 -> Mimply (f l, f r)
  | Mequiv  (l, r)                -> Mequiv (f l, f r)
  | Misempty (l, r)               -> Misempty (l, f r)
  | Mnot e                        -> Mnot (f e)
  | Mmulticomp (e, l)             -> Mmulticomp (f e, List.map (fun (op, e) -> (op, f e)) l)
  | Mequal (l, r)                 -> Mequal (f l, f r)
  | Mnequal (l, r)                -> Mnequal (f l, f r)
  | Mgt (l, r)                    -> Mgt (f l, f r)
  | Mge (l, r)                    -> Mge (f l, f r)
  | Mlt (l, r)                    -> Mlt (f l, f r)
  | Mle (l, r)                    -> Mle (f l, f r)
  | Mplus (l, r)                  -> Mplus (f l, f r)
  | Mminus (l, r)                 -> Mminus (f l, f r)
  | Mmult (l, r)                  -> Mmult (f l, f r)
  | Mdiv (l, r)                   -> Mdiv (f l, f r)
  | Mmodulo (l, r)                -> Mmodulo (f l, f r)
  | Muplus e                      -> Muplus (f e)
  | Muminus e                     -> Muminus (f e)
  | Masset l                     -> Masset (List.map f l)
  | Mletin (i, a, t, b, o)        -> Mletin (i, f a, t, f b, Option.map f o)
  | Mdeclvar (i, t, v)            -> Mdeclvar (i, t, f v)
  | Mvarstorevar v                -> Mvarstorevar v
  | Mvarstorecol v                -> Mvarstorecol v
  | Mvarenumval v                 -> Mvarenumval  v
  | Mvarfield v                   -> Mvarfield    v
  | Mvarlocal v                   -> Mvarlocal    v
  | Mvarparam v                   -> Mvarparam    v
  | Mvarthe                       -> Mvarthe
  | Mvarstate                     -> Mvarstate
  | Mnow                          -> Mnow
  | Mtransferred                  -> Mtransferred
  | Mcaller                       -> Mcaller
  | Mbalance                      -> Mbalance
  | Mnone                         -> Mnone
  | Msome v                       -> Msome (f v)
  | Marray l                      -> Marray (List.map f l)
  | Mint v                        -> Mint v
  | Muint v                       -> Muint v
  | Mbool v                       -> Mbool v
  | Menum v                       -> Menum v
  | Mrational (n, d)              -> Mrational (n, d)
  | Mdate v                       -> Mdate v
  | Mstring v                     -> Mstring v
  | Mcurrency (v, c)              -> Mcurrency (v, c)
  | Maddress v                    -> Maddress v
  | Mduration v                   -> Mduration v
  | Mdotasset (e, i)              -> Mdotasset (f e, i)
  | Mdotcontract (e, i)           -> Mdotcontract (f e, i)
  | Mtuple l                      -> Mtuple (List.map f l)
  | Massoc (k, v)                 -> Massoc (f k, f v)
  | Mfor (i, c, b, lbl)           -> Mfor (i, f c, f b, lbl)
  | Miter (i, a, b, c, lbl)       -> Miter (i, f a, f b, f c, lbl)
  | Mfold (i, is, c, b)           -> Mfold (i, is, f c, f b)
  | Mseq is                       -> Mseq (List.map f is)
  | Massign (op, l, r)            -> Massign (op, l, f r)
  | Massignfield (op, a, fi, r)   -> Massignfield (op, a, fi, f r)
  | Massignstate x                -> Massignstate (f x)
  | Mtransfer (v, d)              -> Mtransfer (f v, f d)
  | Mbreak                        -> Mbreak
  | Massert x                     -> Massert (f x)
  | Mreturn x                     -> Mreturn (f x)
  | Mlabel i                      -> Mlabel i
  | Mshallow (i, x)               -> Mshallow (i, f x)
  | Mlisttocoll (i, x)            -> Mlisttocoll (i, f x)
  | Munshallow (i, x)             -> Munshallow (i, f x)
  | Mtokeys (an, x)               -> Mtokeys (an, f x)
  | Mforall (i, t, Some s, e)     -> Mforall (i, t, Some (f s), f e)
  | Mforall (i, t, None, e)       -> Mforall (i, t, None, f e)
  | Mexists (i, t, Some s, e)     -> Mexists (i, t, Some (f s), f e)
  | Mexists (i, t, None, e)       -> Mexists (i, t, None, f e)

let map_gen_mterm g f (i : 'id mterm_gen) : 'id mterm_gen =
  {
    i with
    node = g f i.node
  }

let map_mterm f t =
  map_gen_mterm map_term_node f t

type ('id, 't) ctx_model_gen = {
  formula: bool;
  fs : 'id function_struct_gen option;
  label: 'id option;
  spec_id : 'id option;
  invariant_id : 'id option;
  custom: 't;
}

type ctx_model = (lident, unit) ctx_model_gen

let mk_ctx_model ?(formula = false) ?fs ?label ?spec_id ?invariant_id custom : ('id, 't) ctx_model_gen =
  { formula; fs; label; spec_id; invariant_id; custom}

let map_mterm_model_exec custom (f : ('id, 't) ctx_model_gen -> mterm -> mterm) (model : model) : model =
  let map_storage_item (ctx : ('id, 't) ctx_model_gen) (si : storage_item) : storage_item = (
    { si with
      default = f ctx si.default;
    }
  ) in
  let map_function_struct (ctx : ('id, 't) ctx_model_gen) (fs : function_struct) : function_struct =
    let ctx = { ctx with fs = Some fs } in
    let body = f ctx fs.body in
    { fs with body = body }
  in
  let map_function (ctx : ('id, 't) ctx_model_gen) (fun_ : function__) : function__ = (
    let node = match fun_.node with
      | Function (fs, ret) -> Function (map_function_struct ctx fs, ret)
      | Entry fs -> Entry (map_function_struct ctx fs)
    in
    { fun_ with node = node}
  ) in

  let ctx = mk_ctx_model custom in
  let storage = List.map (map_storage_item ctx) model.storage in
  let functions = List.map (map_function ctx) model.functions in
  { model with
    functions = functions;
    storage = storage;
  }

let map_mterm_model_formula custom (f : ('id, 't) ctx_model_gen -> mterm -> mterm) (model : model) : model =
  let map_specification (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> mterm -> mterm) (v : specification) : specification = (
    let map_label_term (f : ('id, 't) ctx_model_gen -> mterm -> mterm) (lt : label_term) : label_term =
      let ctx = { ctx with label = lt.label } in
      { lt with
        term = f ctx lt.term }
    in

    let map_predicate (f : ('id, 't) ctx_model_gen -> mterm -> mterm) (p : predicate) : predicate =
      { p with
        args = List.map (fun (x, y) -> (x, y)) p.args;
        body = f ctx p.body;
      }
    in

    let map_definition (f : ('id, 't) ctx_model_gen -> mterm -> mterm) (d : definition) : definition =
      { d with
        body = f ctx d.body
      }
    in

    let map_invariantt (f : ('id, 't) ctx_model_gen -> mterm -> mterm) ((it_id, it_lt) : 'id * 'id label_term_gen list) : 'id * 'id label_term_gen list =
      (it_id, List.map (map_label_term f) it_lt)
    in

    let map_invariant (f : ('id, 't) ctx_model_gen -> mterm -> mterm) (spec : invariant) : invariant =
      let ctx = {ctx with invariant_id = Some spec.label } in
      { spec with
        formulas = List.map (f ctx) spec.formulas;
      }
    in

    let map_postcondition (f : ('id, 't) ctx_model_gen -> mterm -> mterm) (spec : postcondition) : postcondition =
      let ctx = { ctx with spec_id = Some spec.name} in
      { spec with
        formula = f ctx spec.formula;
        invariants = List.map (map_invariant f) spec.invariants;
      }
    in

    let map_variable (_f : ('id, 't) ctx_model_gen -> mterm -> mterm) (spec : variable) : variable =
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
      postconditions = List.map (map_postcondition f) v.postconditions;
    }
  ) in

  let ctx : ('id, 't) ctx_model_gen = mk_ctx_model custom in

  let map_function (f : ('id, 't) ctx_model_gen -> mterm -> mterm) (fun_ : function__) : function__ =
    let fs : function_struct =
      match fun_.node with
      | Function (fs, _) -> fs
      | Entry fs -> fs
    in
    let ctx = { ctx with fs = Some fs } in
    { fun_ with
      spec = Option.map (map_specification ctx f) fun_.spec;
    }
  in

  { model with
    functions = List.map (map_function f) model.functions;
    specification = map_specification ctx f model.specification
  }


let map_mterm_model_gen custom (f : ('id, 't) ctx_model_gen -> mterm -> mterm) (model : model) : model =
  model
  |> map_mterm_model_exec custom f
  |> map_mterm_model_formula custom f

let map_mterm_model (f : ('id, 't) ctx_model_gen -> mterm -> mterm) (model : model) : model =
  map_mterm_model_gen () f model

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
  | Maddshallow (_, args)                 -> List.fold_left f accu args
  | Msetbefore    e                       -> f accu e
  | Msetat   (_, e)                       -> f accu e
  | Msetunmoved   e                       -> f accu e
  | Msetadded     e                       -> f accu e
  | Msetremoved   e                       -> f accu e
  | Msetiterated  e                       -> f accu e
  | Msettoiterate e                       -> f accu e
  | Mexternal (_, _, c, args)             -> List.fold_left f (f accu c) args
  | Mget (_, k)                           -> f accu k
  | Mgetbefore (_, k)                     -> f accu k
  | Mgetat (_, _, k)                      -> f accu k
  | Mset (_, _, k, v)                     -> f (f accu v) k
  | Maddasset (_, i)                      -> f accu i
  | Maddfield (_, _, c, i)                -> f (f accu c) i
  | Maddlocal (c, i)                      -> f (f accu c) i
  | Mremoveasset (_, i)                   -> f accu i
  | Mremovefield (_, _, c, i)             -> f (f accu c) i
  | Mremovelocal (c, i)                   -> f (f accu c) i
  | Mclearasset _                         -> accu
  | Mclearfield (_, _, c)                 -> f accu c
  | Mremoveif (_, fn, c)                  -> f (f accu fn) c
  | Mclearlocal (c)                       -> f accu c
  | Mreverseasset _                       -> accu
  | Mreversefield (_, _, c)               -> f accu c
  | Mreverselocal (c)                     -> f accu c
  | Mselect (_, c, p)                     -> f (f accu c) p
  | Msort (_, c, _, _)                    -> f accu c
  | Mcontains (_, c, i)                   -> f (f accu c) i
  | Mmem (_, c, i)                        -> f (f accu c) i
  | Msubsetof (_, c, i)                   -> f (f accu c) i
  | Mnth (_, c, i)                        -> f (f accu c) i
  | Mcount (_, c)                         -> f accu c
  | Msum (_, _, c)                        -> f accu c
  | Mmin (_, _, c)                        -> f accu c
  | Mmax (_, _, c)                        -> f accu c
  | Mfail _                               -> accu
  | Mmathmax (l, r)                       -> f (f accu l) r
  | Mmathmin (l, r)                       -> f (f accu l) r
  | Mhead (_, c, i)                       -> f (f accu c) i
  | Mtail (_, c, i)                       -> f (f accu c) i
  | Mand (l, r)                           -> f (f accu l) r
  | Mor (l, r)                            -> f (f accu l) r
  | Mimply (l, r)                         -> f (f accu l) r
  | Mequiv  (l, r)                        -> f (f accu l) r
  | Misempty  (_, r)                      -> f accu r
  | Mnot e                                -> f accu e
  | Mmulticomp (e, l)                     -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
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
  | Masset l                             -> List.fold_left f accu l
  | Mletin (_, a, _, b, o)                -> let tmp = f (f accu a) b in Option.map_dfl (f tmp) tmp o
  | Mdeclvar (_, _, v)                    -> f accu v
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
  | Mdotasset (e, _)                      -> f accu e
  | Mdotcontract (e, _)                   -> f accu e
  | Mvarstate                             -> accu
  | Mnow                                  -> accu
  | Mtransferred                          -> accu
  | Mcaller                               -> accu
  | Mbalance                              -> accu
  | Mnone                                 -> accu
  | Msome v                               -> f accu v
  | Mtuple l                              -> List.fold_left f accu l
  | Massoc (k, v)                         -> f (f accu k) v
  | Mfor (_, c, b, _)                     -> f (f accu c) b
  | Miter (_, a, b, c, _)                 -> f (f (f accu a) b) c
  | Mfold (_, _, c, b)                    -> f (f accu c) b
  | Mseq is                               -> List.fold_left f accu is
  | Massign (_, _, e)                     -> f accu e
  | Massignfield (_, _, _, e)             -> f accu e
  | Massignstate x                        -> f accu x
  | Mtransfer (v, d)                      -> f (f accu v) d
  | Mbreak                                -> accu
  | Massert x                             -> f accu x
  | Mreturn x                             -> f accu x
  | Mlabel _                              -> accu
  | Mshallow (_, x)                       -> f accu x
  | Mlisttocoll (_, x)                    -> f accu x
  | Munshallow (_, x)                     -> f accu x
  | Mtokeys (_, x)                        -> f accu x
  | Mforall (_, _, Some s, e)             -> f (f accu s) e
  | Mforall (_, _, None, e)               -> f accu e
  | Mexists (_, _, Some s, e)             -> f (f accu s) e
  | Mexists (_, _, None, e)               -> f accu e

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
           let ia, accu = f accu i in
           (p, ia)::ps, accu) ([], ea) l
      |> (fun (x, y) -> (List.rev x, y))
    in

    g (Mmatchwith (ee, pse)), psa

  | Mapp (id, args) ->
    let ((argss, argsa) : 'c list * 'a) =
      List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) args
    in
    g (Mapp (id, argss)), argsa

  | Maddshallow (id, args) ->
    let ((argss, argsa) : 'c list * 'a) =
      List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) args
    in
    g (Maddshallow (id, argss)), argsa

  | Msetbefore e ->
    let ee, ea = f accu e in
    g (Msetbefore ee), ea

  | Msetat (lbl, e) ->
    let ee, ea = f accu e in
    g (Msetat (lbl, ee)), ea

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

  | Mgetbefore (c, k) ->
    let ke, ka = f accu k in
    g (Mgetbefore (c, ke)), ka

  | Mgetat (c, d, k) ->
    let ke, ka = f accu k in
    g (Mgetat (c, d, ke)), ka

  | Mset (c, l, k, v) ->
    let ke, ka = f accu k in
    let ve, va = f ka v in
    g (Mset (c, l, ke, ve)), va

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

  | Mremoveif (an, fn, i) ->
    let ie, ia = f accu i in
    let fe, fa = f ia fn in
    g (Mremoveif (an, fe, ie)), fa

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

  | Msubsetof (an, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Msubsetof (an, ce, ie)), ia

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

  | Mmathmin (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mmathmin (le, re)), ra

  | Mmathmax (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mmathmax (le, re)), ra

  | Mhead (an, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mhead (an, ce, ie)), ia

  | Mtail (an, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mtail (an, ce, ie)), ia

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

  | Mmulticomp (e, l) ->
    let ee, ea = f accu e in
    let (le, la) =
      List.fold_left
        (fun (ps, accu) (p, i) ->
           let ia, accu = f accu i in
           [(p, ia)] @ ps, accu) ([], ea) l
    in

    g (Mmulticomp (ee, le)), la

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

  | Masset l ->
    let le, la = fold_map_term_list f accu l in
    g (Masset le), la

  | Mletin (idd, i, t, b, o) ->
    let ie, ia = f accu i in
    let be, ba = f ia b in
    let oe, oa =
      match o with
      | Some o -> f ba o |> (fun (x, y) -> (Some x, y))
      | None -> (None, ba) in
    g (Mletin (idd, ie, t, be, oe)), oa

  | Mdeclvar (ids, t, v) ->
    let ve, va = f accu v in
    g (Mdeclvar (ids, t, ve)), va

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

  | Mvarstate ->
    g Mvarstate, accu

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

  | Miter (i, a, b, c, lbl) ->
    let ae, aa = f accu a in
    let be, ba = f aa b in
    let ce, ca = f ba c in
    g (Miter (i, ae, be, ce, lbl)), ca

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

  | Massignstate x ->
    let xe, xa = f accu x in
    g (Massignstate xe), xa

  | Mtransfer (v, d) ->
    let ve, va = f accu v in
    let de, da = f va d in
    g (Mtransfer (ve, de)), da

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

  | Mforall (id, t, Some s, e) ->
    let ee, ea = f accu e in
    let se, sa = f ea s in
    g (Mforall (id, t, Some se, ee)), sa

  | Mforall (id, t, None, e) ->
    let ee, ea = f accu e in
    g (Mforall (id, t, None, ee)), ea

  | Mexists (id, t, Some s, e) ->
    let ee, ea = f accu e in
    let se, sa = f ea s in
    g (Mexists (id, t, Some se, ee)), sa

  | Mexists (id, t, None, e) ->
    let ee, ea = f accu e in
    g (Mexists (id, t, None, ee)), ea

let fold_left g l accu = List.fold_left (fun accu x -> g x accu) accu l

let fold_specification (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (v : 'id specification_gen) (accu : 'a) : 'a =
  let fold_label_term (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (lt : 'id label_term_gen) (accu : 'a) : 'a =
    let ctx = { ctx with label = lt.label } in
    f ctx accu lt.term
  in

  let fold_predicate (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (p : 'id predicate_gen) (accu : 'a) : 'a =
    accu
    |> fun x -> f ctx x p.body
  in
  let fold_definition (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (d : 'id definition_gen) (accu : 'a) : 'a =
    f ctx accu d.body
  in

  let fold_invariantt (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (it : 'id * 'id label_term_gen list) (accu : 'a) : 'a =
    List.fold_left (fun accu x -> fold_label_term ctx f x accu) accu (snd it)
  in

  let fold_invariant (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (spec : 'id invariant_gen) (accu : 'a) : 'a =
    let ctx = {ctx with invariant_id = Some spec.label } in
    List.fold_left (f ctx) accu spec.formulas
  in

  let fold_postcondition (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (spec : 'id postcondition_gen) (accu : 'a) : 'a =
    let ctx = { ctx with spec_id = Some spec.name} in
    accu
    |> (fun x -> f ctx x spec.formula)
    |> (fun x -> List.fold_left (fun accu (x : 'id invariant_gen) -> fold_invariant ctx f x accu) x spec.invariants)
  in

  let fold_variable (_ctx : ('id, 't) ctx_model_gen) (_f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (_spec : 'id variable_gen) (accu : 'a) : 'a =
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
  |> fold_left (fold_postcondition ctx f) v.postconditions

let fold_model (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (m : 'id model_gen) (accu : 'a) : 'a =


  let fold_action (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (a : 'id function__gen) (accu : 'a) : 'a = (
    let accu : 'a = (
      match a.node with
      | Function (fs, _)
      | Entry fs -> fold_term (f {ctx with fs = Some fs}) accu fs.body
    ) in
    Option.map_dfl (fun (x : 'id specification_gen) -> fold_specification ctx f x accu) accu a.spec
  ) in

  let ctx : ctx_model = mk_ctx_model () in

  accu
  |> fold_left (fold_action ctx f) m.functions
  |> fold_specification ctx f m.specification

(* -------------------------------------------------------------------- *)

let merge_seq (mt1 : mterm) (mt2 : mterm) : mterm =
  match mt1.node, mt2.node with
  | Mseq l1, Mseq l2 -> mk_mterm (Mseq (l1 @ l2)) mt2.type_
  | _, Mseq l -> mk_mterm (Mseq ([mt1] @ l)) mt2.type_
  | Mseq l, _ -> mk_mterm (Mseq (l @ [mt2])) mt2.type_
  | _ -> mk_mterm (Mseq [mt1; mt2]) mt2.type_

let extract_list (mt : mterm) (e : mterm) =
  match mt with
  | { node = Mseq l; _} -> l @ [e]
  | _ -> [mt; e]

(* -------------------------------------------------------------------- *)

module Utils : sig

  val function_name_from_storage_const   : storage_const   -> string
  val function_name_from_container_const : container_const -> string
  val function_name_from_function_const  : function_const  -> string
  val function_name_from_builtin_const   : builtin_const  -> string
  val get_info_assets                    : model -> info_asset list
  val get_enums                          : model -> enum list
  val get_assets                         : model -> asset list
  val get_variables                      : model -> storage_item list
  val get_storage                        : model -> storage
  val get_info_asset                     : model -> lident -> info_asset
  val get_info_enum                      : model -> ident -> info_enum
  val get_asset_field                    : model -> (lident * ident) -> (ident * type_ * mterm option)
  val get_asset_key                      : model -> lident -> (ident * btyp)
  val get_field_container                : model -> ident -> ident -> (ident * container)
  val is_storage_attribute               : model -> lident -> bool
  val get_named_field_list               : model -> lident -> 'a list -> (ident * 'a) list
  val get_partitions                     : model -> (ident * ident * type_) list (* asset id, asset item *)
  val dest_partition                     : type_ -> lident
  val get_partition_asset_key            : model -> lident -> lident -> (ident * ident * btyp)
  val get_partition_assets               : model -> ident -> ident list
  val get_entries                        : model -> (specification option * function_struct) list
  val get_functions                      : model -> (specification option * function_struct* type_) list
  val has_partition                      : model -> ident -> bool
  val get_asset_partitions               : model -> ident -> (ident * type_ * mterm option) list
  val get_field_list                     : model -> lident -> ident list
  val get_field_pos                      : model -> lident -> lident -> int (* m, asset, field *)
  val get_nth_asset_val                 : int -> mterm -> mterm
  val dest_array                         : mterm -> mterm list
  val get_asset_type                     : mterm -> lident
  val is_local_assigned                  : lident -> mterm -> bool
  val get_function_args                  : function__ -> argument list
  val set_function_args                  : function__ -> argument list -> function__
  val map_function_terms                 : (mterm -> mterm) -> function__ -> function__
  val is_asset                          : mterm -> bool
  val is_varlocal                        : mterm -> bool
  val dest_varlocal                      : mterm -> lident
  val is_container                       : type_ -> bool
  val get_key_pos                        : model -> lident -> int
  val get_loop_invariants                : model -> (lident * mterm) list -> ident -> (lident * mterm) list
  val get_formula                        : model -> mterm option -> ident -> mterm option
  val is_post                            : postcondition -> bool
  val get_sum_fields                     : model -> ident -> ident list
  val get_added_removed_sets             : model -> specification option -> ((lident, lident mterm_gen) mterm_node) list
  val get_storage_invariants             : model -> ident option -> (ident * ident * mterm) list
  val is_field_storage                   : model -> ident -> bool
  val with_trace                         : model -> bool
  val get_callers                        : model -> ident -> ident list
  val no_fail                            : model -> ident -> lident option
  val type_to_asset                      : type_ -> lident
  val get_map_function                   : model -> (ident * ident list) list
  val retrieve_all_properties            : model -> (ident * property) list
  val retrieve_property                  : model -> ident -> property
  val get_default_value                  : model -> type_ -> mterm
  val with_transfer_for_mterm            : mterm -> bool
  val with_transfer                      : model -> bool

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
    | EnumNotFound of string
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
    | Select    (aid, _)   -> "select_"     ^ aid
    | Sort      (aid, fid) -> "sort_"       ^ aid ^ "_" ^ fid
    | Contains   aid       -> "contains_"   ^ aid
    | Nth        aid       -> "nth_"        ^ aid
    | Count      aid       -> "count_"      ^ aid
    | Sum       (aid, fid) -> "sum_"        ^ aid ^ "_" ^ fid
    | Min       (aid, fid) -> "min_"        ^ aid ^ "_" ^ fid
    | Max       (aid, fid) -> "max_"        ^ aid ^ "_" ^ fid
    | Shallow    aid       -> "shallow_"    ^ aid
    | Unshallow  aid       -> "unshallow"   ^ aid
    | Listtocoll aid       -> "listtocoll_" ^ aid
    | Head       aid       -> "head_"       ^ aid
    | Tail       aid       -> "tail_"       ^ aid

  let function_name_from_builtin_const = function
    | MinBuiltin         _ -> "min"
    | MaxBuiltin         _ -> "max"

  let get_function_args (f : function__) : argument list =
    match f.node with
    | Function (s,_) -> s.args
    | Entry s        -> s.args

  let set_function_args (f : function__) (args : argument list) : function__ =
    match f.node with
    | Function (s,t) -> { node = Function ({ s with args = args },t); spec = f.spec }
    | Entry s        -> { node = Entry { s with args = args }; spec = f.spec }

  let is_asset (i : info_item) : bool =
    match i with
    | Iasset _ -> true
    | _        -> false

  let is_entry (f : function__) : bool =
    match f with
    | { node = Entry _; spec = _ } -> true
    | _                             -> false

  let is_function (f : function__) : bool =
    match f with
    | { node = Function _; spec = _ } -> true
    | _                                -> false

  let get_entry (f : function__) : specification option * function_struct =
    match f with
    | { node = Entry s; spec = v } -> (v,s)
    | _                             -> assert false

  let get_function (f : function__) : specification option * function_struct * type_ =
    match f with
    | { node = Function (s,t); spec = v } -> (v,s,t)
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

  let get_nth_asset_val pos (t : mterm) =
    match t.node with
    | Masset l -> List.nth l pos
    | _ -> emit_error (NotaRecord t)

  let type_to_asset = function
    | Tasset n -> n
    | Tcontainer (Tasset n, _) -> n
    | _ -> emit_error NotanAssetType

  let get_asset_type (t : mterm) : lident = type_to_asset t.type_

  let get_info_assets m = m.info |> List.filter is_asset |> List.map dest_asset

  let is_asset (d : decl_node) : bool =
    match d with
    | Dasset _ -> true
    | _        -> false

  let dest_asset  = function
    | Dasset r -> r
    | _ -> emit_error NotaPartition


  let is_enum (d : decl_node) : bool =
    match d with
    | Denum _ -> true
    | _          -> false

  let dest_enum  = function
    | Denum e -> e
    | _ -> emit_error NotaPartition

  let get_enums m = m.decls |> List.filter is_enum |> List.map dest_enum

  let get_assets m = m.decls |> List.filter is_asset |> List.map dest_asset

  let is_variable (d : storage_item) : bool =
    match d.model_type with
    | MTconst | MTvar -> true
    | _    -> false

  let get_variables m = m.storage |> List.filter is_variable

  let get_info_asset model asset_name : info_asset =
    let id = unloc asset_name in
    let res = List.fold_left (fun accu (x : info_item) ->
        match x with
        | Iasset r when String.equal (unloc asset_name) r.name -> Some r
        | _ -> accu
      ) None model.info in
    match res with
    | Some v -> v
    | _ -> emit_error (AssetNotFound id)

  let get_info_enum (m : model) (id : ident) : info_enum =
    let res = List.fold_left (fun accu (x : info_item) ->
        match x with
        | Ienum r when String.equal id r.name -> Some r
        | _ -> accu
      ) None m.info in
    match res with
    | Some v -> v
    | _ -> emit_error (EnumNotFound id)

  (* let get_state_values (m : model) : ident list =
     [] *)

  let get_partitions m : (ident * ident * type_) list=
    get_info_assets m |> List.fold_left (fun acc (info : info_asset) ->
        acc @ (List.fold_left (fun acc (i,t,_) ->
            match t with
            | Tcontainer (Tasset _, Partition) ->
              acc @ [info.name,i,t]
            | _ -> acc
          ) [] info.values)
      ) []

  let has_partition m asset : bool =
    get_info_assets m |> List.fold_left (fun acc (info : info_asset) ->
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
    get_info_assets m |> List.fold_left (fun acc (info : info_asset) ->
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

  let get_asset_field model (asset_name, field_name) =
    let asset = get_info_asset model asset_name in
    let res = List.fold_left (fun accu (i,t,d : ident * type_ * (lident mterm_gen option)) ->
        if String.equal field_name i then
          Some (i,t,d)
        else accu) None asset.values in
    match res with
    | Some v -> v
    | _ -> emit_error (AssetFieldNotFound (unloc asset_name, field_name))

  let get_asset_key model asset_name : (ident * btyp) =
    let asset = get_info_asset model asset_name in
    let key_id = asset.key in
    let (_,key_typ,_) = get_asset_field model (asset_name, key_id) in
    match key_typ with
    | Tbuiltin v -> (key_id, v)
    | _ -> emit_error (AssetKeyTypeNotFound (unloc asset_name))

  let get_field_container model asset_name field_name : ident * container =
    let (_,typ,_) = get_asset_field model (dumloc asset_name, field_name) in
    match typ with
    | Tcontainer (Tasset an, c) -> (unloc an, c)
    | _ -> assert false

  let get_partition_assets model asset : ident list =
    get_partitions model
    |> List.filter (fun (a,_,_) -> compare asset a = 0)
    |> List.map (fun (_,_,t) -> type_to_asset t)
    |> List.map unloc

  (* returns : asset name, key name, key type *)
  let get_partition_asset_key model asset field : (ident * ident * btyp) =
    let partitions = get_partitions model in
    let rec rec_get = function
      | (r,i,t) :: _tl when compare r asset.pldesc = 0 &&
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
         accu || String.equal (Location.unloc id) (Location.unloc x.id)
       ) false items)

  let get_field_list (model : model) (asset_name : lident) : ident list =
    let asset = get_info_asset model asset_name in
    List.map (fun (i,_,_) -> i) asset.values

  let get_field_pos model asset field =
    let l = get_field_list model asset in
    let rec rec_get_pos i = function
      | e :: _tl when compare field.pldesc e = 0 -> i
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
    with FoundAssign -> true


  exception FoundTransfer

  let with_transfer_for_mterm_intern _ctx accu (mt : mterm) : bool =
    let rec aux accu (t : mterm) =
      match t.node with
      | Mtransfer _ -> raise FoundTransfer
      | _ -> fold_term aux accu t in
    aux accu mt

  let with_transfer_for_mterm (mt : mterm) : bool =
    try with_transfer_for_mterm_intern () false mt
    with FoundTransfer -> true

  let with_transfer (model : model) : bool =
    try fold_model with_transfer_for_mterm_intern model false
    with FoundTransfer -> true

  let map_invariant_terms (m : mterm -> mterm) (i : invariant) : invariant = {
    i with
    formulas = List.map m i.formulas
  }

  let map_postcondition_terms (m : mterm -> mterm) (s : postcondition) : postcondition = {
    s with
    formula = m s.formula;
    invariants = List.map (map_invariant_terms m) s.invariants
  }

  let map_specification_terms (m : mterm -> mterm) (v : specification) : specification = {
    v with
    postconditions = List.map (map_postcondition_terms m) v.postconditions
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
    spec = Option.map (map_specification_terms m) f.spec;
  }

  let is_asset (t : mterm) =
    match t.node with
    | Masset _ -> true
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
    get_info_assets m |> List.fold_left (fun acc (info : info_asset) ->
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

  let is_post (s : postcondition) =
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

  let get_added_removed_sets (_m : model) v : ((lident,(lident mterm_gen)) mterm_node) list =
    let rec internal_fold_add_remove ctx acc (term : mterm) =
      match term.node with
      | Msetadded e -> acc @ [ Msetadded e ]
      | Msetremoved e -> acc @ [ Msetremoved e ]
      | _ -> fold_term (internal_fold_add_remove ctx) acc term in
    Tools.List.dedup (Option.map_dfl (fun (x : specification) ->
        fold_specification (mk_ctx_model ()) internal_fold_add_remove x []) [] v)

  (* returns asset name * invariant name * invariant term *)
  let get_storage_invariants (m : model) (asset_name : ident option) : (ident * ident * mterm) list =
    match asset_name with
    | None -> []
    | Some asset_name ->
      try
        let assets : lident asset_gen list = get_assets m in
        let asset : lident asset_gen = List.find (fun (x : lident asset_gen) -> cmp_ident (unloc x.name) asset_name) assets in
        List.fold_left (fun acc (lt : label_term) ->
            let inv_name = Tools.Option.fold (fun _ l -> unloc l) "" lt.label in
            let inv_term = lt.term in
            acc @ [asset_name, inv_name, inv_term]
          ) [] asset.invariants
      with
      | Not_found -> []

  (* List.fold_left (fun acc (i : storage_item) ->
      let name = match i.id with
        | SIname name -> name
        | SIstate -> dumloc "state" in
      let n = name |> unloc in
      let do_fold =
        match asset with
        | Some a when compare n a = 0 -> true
        | Some _ -> false
        | _ -> true
      in
      if do_fold then
        List.fold_left (fun acc (lt : label_term) ->
            let inv_name = Tools.Option.fold (fun _ l -> unloc l) "" lt.label in
            let inv_term = lt.term in
            acc @ [n, inv_name, inv_term]
          ) acc i.invariants
      else acc
     ) [] m.storage *)

  let is_field_storage (m : model) (id : ident) : bool =
    let l : ident list = List.map (fun (x : storage_item) -> unloc x.id) m.storage in
    List.mem id l

  let with_trace (_m : model) : bool = true

  (* returns the list of entries calling the function named 'name' *)
  let get_callers (_m : model) (_name : ident) : ident list = [] (* TODO *)

  (* is there a no_fail predicate on an entry called fn ? *)
  let no_fail (m : model) (fn : ident) : lident option =
    List.fold_left (fun acc (p : security_item) ->
        match acc with
        | None ->
          begin
            match p.predicate.s_node with
            | SnoStorageFail Sany -> Some p.label
            | SnoStorageFail (Sentry l) ->
              if l |> List.map unloc |> List.mem fn then
                Some p.label
              else
                None
            | _ -> None
          end
        | _ -> acc
      ) None (m.security.items)

  let get_map_function (m : model) : (ident * ident list) list =
    let fun_ids : (ident * function_struct) list =
      List.map
        (fun (f : function__) ->
           match f.node with
           | Function (fs, _) -> unloc fs.name, fs
           | Entry fs-> unloc fs.name, fs)
        m.functions
    in
    let fun_id_list = List.map fst fun_ids in
    let rec extract_fun_id accu (mt : mterm) : ident list =
      let l = fold_term extract_fun_id accu mt in
      match mt.node with
      | Mapp (id, _args) when (List.exists (fun x -> (String.equal (unloc id) x)) fun_id_list) ->
        l @ [unloc id]
      | _ -> l
    in
    List.map (fun (name, fs : ident * function_struct) -> name, extract_fun_id [] fs.body) fun_ids

  let retrieve_all_properties (m : model) : (ident * property) list =
    let fold_decl = function
      | Dasset r -> List.map (fun (x : label_term) -> ((unloc |@ Option.get) x.label, PstorageInvariant x)) r.invariants
      | _ -> []
    in
    let fold_specification (fun_id : ident option) (sp : specification): (ident * property) list =
      []
      |> (@) (List.map (fun (pc : postcondition) -> (unloc pc.name, Ppostcondition (pc, fun_id))) sp.postconditions)
    in
    let fold_function (f : function__) : (ident * property) list =
      let name =
        match f.node with
        | Entry fs -> unloc fs.name
        | Function (fs, _) -> unloc fs.name
      in
      []
      |> (@) (Option.map_dfl (fold_specification (Some name)) [] f.spec)
    in
    []
    |> (@) (List.map fold_decl m.decls)
    |> (@) (List.map fold_function m.functions) |> List.flatten
    |> (@) (fold_specification None m.specification)
    |> (@) (List.map (fun (x : security_item) -> (unloc x.label, PsecurityPredicate x)) m.security.items)


  let retrieve_property (m : model) (id : ident) : property =
    let properties = retrieve_all_properties m in
    List.assoc id properties

  let rec get_default_value (m : model) (t : type_) =
    let aux = function
      | Tbuiltin Bbool       -> Mbool false
      | Tbuiltin Bint        -> Mint Big_int.zero_big_int
      | Tbuiltin Brational   -> Mrational (Big_int.zero_big_int, Big_int.zero_big_int)
      | Tbuiltin Bdate       -> Mdate "0"
      | Tbuiltin Bduration   -> Mduration (Core.mk_duration ())
      | Tbuiltin Bstring     -> Mstring ""
      | Tbuiltin Baddress    -> Maddress "tz1_default"
      | Tbuiltin Brole       -> Maddress "tz1_default"
      | Tbuiltin Bcurrency   -> Mcurrency (Big_int.zero_big_int, Tz)
      | Tbuiltin Bkey        -> Maddress "tz1_default"
      | Tasset asset_name    ->
        begin
          let a = get_info_asset m asset_name in
          let l : mterm list =
            List.map (
              fun (_, t, value : 'a * type_ * mterm option) ->
                match value with
                | Some v -> v
                | _ -> get_default_value m t
            ) a.values
          in
          Masset l
        end
      | Tcontainer _ -> Marray []
      | _ -> Mstring "FIXME"
    in
    let tt =
      match t with
      | Tcontainer (Tasset an, c) ->
        begin
          let _, t = get_asset_key m an in
          Tcontainer (Tbuiltin t, c)
        end
      | _ -> t
    in
    mk_mterm (aux t) tt
end
