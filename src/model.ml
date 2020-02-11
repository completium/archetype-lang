open Ident
open Tools

type lident = ident Location.loced
[@@deriving show {with_path = false}]

type currency =
  | Tz
  | Mtz
  | Utz
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
  | Btimestamp
  | Bstring
  | Baddress
  | Brole
  | Bcurrency
  | Bkey
  | Bbytes
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

type rat_arith_op =
  | Rplus
  | Rminus
  | Rmult
  | Rdiv
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
  (* lambda *)
  | Mletin          of 'id list * 'term * type_ option * 'term * 'term option
  | Mdeclvar        of 'id list * type_ option * 'term
  | Mapp            of 'id * 'term list
  (* assign *)
  | Massign         of (assignment_operator * type_ * 'id * 'term)
  | Massignvarstore of (assignment_operator * type_ * 'id * 'term)
  | Massignfield    of (assignment_operator * type_ * 'term * 'id * 'term)
  | Massignstate    of 'term
  (* control *)
  | Mif             of ('term * 'term * 'term option)
  | Mmatchwith      of 'term * ('id pattern_gen * 'term) list
  | Mfor            of ('id * 'term * 'term * ident option)
  | Miter           of ('id * 'term * 'term * 'term * ident option)
  | Mseq            of 'term list
  | Mreturn         of 'term
  | Mlabel          of 'id
  (* effect *)
  | Mfail           of 'id fail_type_gen
  | Mtransfer       of ('term * 'term) (* value * dest *)
  | Mexternal       of ident * 'id * 'term * ('id * 'term) list (* contract_id * id * field_address_id_val * args *)
  (* literals *)
  | Mint            of Core.big_int
  | Muint           of Core.big_int
  | Mbool           of bool
  | Menum           of string
  | Mrational       of Core.big_int * Core.big_int
  | Mstring         of string
  | Mcurrency       of Core.big_int * currency
  | Maddress        of string
  | Mdate           of Core.date
  | Mduration       of Core.duration
  | Mtimestamp      of Core.big_int
  | Mbytes          of string
  (* composite type constructors *)
  | Mnone
  | Msome           of 'term
  | Marray          of 'term list
  | Mtuple          of 'term list
  | Masset          of 'term list
  | Massoc          of 'term * 'term
  (* dot *)
  | Mdotasset       of 'term * 'id
  | Mdotcontract    of 'term * 'id
  (* comparison operators *)
  | Mequal          of 'term * 'term
  | Mnequal         of 'term * 'term
  | Mgt             of 'term * 'term
  | Mge             of 'term * 'term
  | Mlt             of 'term * 'term
  | Mle             of 'term * 'term
  | Mmulticomp      of 'term * (comparison_operator * 'term) list
  (* arithmetic operators *)
  | Mand            of 'term * 'term
  | Mor             of 'term * 'term
  | Mnot            of 'term
  | Mplus           of 'term * 'term
  | Mminus          of 'term * 'term
  | Mmult           of 'term * 'term
  | Mdiv            of 'term * 'term
  | Mmodulo         of 'term * 'term
  | Muplus          of 'term
  | Muminus         of 'term
  (* asset api effect *)
  | Maddasset       of ident * 'term
  | Maddfield       of ident * ident * 'term * 'term (* asset_name * field_name * asset instance * item * shalow values*)
  | Mremoveasset    of ident * 'term
  | Mremovefield    of ident * ident * 'term * 'term
  | Mclearasset     of ident
  | Mclearfield     of ident * ident
  | Mset            of ident * ident list * 'term * 'term (*asset_name * field_name modified * ... *)
  | Mupdate         of ident * 'term * ('id * assignment_operator * 'term) list
  | Mremoveif       of ident * 'term * 'term
  | Maddupdate      of ident * 'term * ('id * assignment_operator * 'term) list
  (* asset api expression *)
  | Mget            of ident * 'term
  | Mselect         of ident * 'term * 'term
  | Msort           of ident * 'term * ident * sort_kind
  | Mcontains       of ident * 'term * 'term
  | Mnth            of ident * 'term * 'term
  | Mcount          of ident * 'term
  | Msum            of ident * 'id * 'term
  | Mmin            of ident * 'id * 'term
  | Mmax            of ident * 'id * 'term
  | Mhead           of ident * 'term * 'term
  | Mtail           of ident * 'term * 'term
  (* list api effect *)
  | Mlistprepend    of 'term * 'term
  (* list api expression *)
  | Mlistcontains   of 'term * 'term
  | Mlistcount      of 'term
  | Mlistnth        of 'term * 'term
  (* builtin functions *)
  | Mfunmax         of 'term * 'term
  | Mfunmin         of 'term * 'term
  | Mfunabs         of 'term
  (* constants *)
  | Mvarstate
  | Mnow
  | Mtransferred
  | Mcaller
  | Mbalance
  | Msource
  (* variables *)
  | Mvarstorevar    of 'id
  | Mvarstorecol    of 'id
  | Mvarenumval     of 'id
  | Mvarlocal       of 'id
  | Mvarparam       of 'id
  | Mvarfield       of 'id
  | Mvarthe
  (* rational *)
  | Mdivrat         of 'term * 'term
  | Mrateq          of 'term * 'term
  | Mratcmp         of comparison_operator * 'term * 'term
  | Mratarith       of rat_arith_op * 'term * 'term
  | Mrattez         of 'term * 'term
  | Minttorat       of 'term
  (* functional *)
  | Mfold           of ('id * 'id list * 'term * 'term) (* ident list * collection * body *)
  (* imperative *)
  | Mbreak
  (* shallowing *)
  | Mshallow        of ident * 'term
  | Munshallow      of ident * 'term
  | Mlisttocoll     of ident * 'term
  | Maddshallow     of ident * 'term list
  (* collection keys *)
  | Mtokeys         of ident * 'term
  | Mcoltokeys      of ident
  (* quantifiers *)
  | Mforall         of 'id * type_ * 'term option * 'term
  | Mexists         of 'id * type_ * 'term option * 'term
  (* formula operators *)
  | Mimply          of 'term * 'term
  | Mequiv          of 'term * 'term
  (* formula expression*)
  | Mgetbefore      of ident * 'term
  | Mgetat          of ident * ident * 'term (* asset_name * label * value *)
  | Mgetfrommap     of ident * 'term * 'term
  | Mmem            of ident * 'term * 'term
  | Msubsetof       of ident * 'term * 'term
  | Misempty        of ident * 'term
  (* set api *)
  | Msetbefore      of 'term
  | Msetat          of ident * 'term
  | Msetunmoved     of 'term
  | Msetadded       of 'term
  | Msetremoved     of 'term
  | Msetiterated    of 'term
  | Msettoiterate   of 'term
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

and api_asset =
  | Get              of ident
  | Set              of ident
  | Add              of ident
  | Remove           of ident
  | UpdateAdd        of ident * ident
  | UpdateRemove     of ident * ident
  | ToKeys           of ident
  | ColToKeys        of ident
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

and api_list =
  | Lprepend         of type_
  | Lcontains        of type_
  | Lcount           of type_
  | Lnth             of type_
[@@deriving show {with_path = false}]

and api_builtin =
  | MinBuiltin of type_
  | MaxBuiltin of type_
[@@deriving show {with_path = false}]

and api_internal =
  | RatEq
  | RatCmp
  | RatArith
  | RatTez
[@@deriving show {with_path = false}]

and api_storage_node =
  | APIAsset      of api_asset
  | APIList       of api_list
  | APIBuiltin    of api_builtin
  | APIInternal   of api_internal
[@@deriving show {with_path = false}]

and api_storage = {
  node_item: api_storage_node;
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

type 'id label_term_gen = {
  label : 'id;
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
  const       : bool;
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
  original_type: type_;
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
  initial: 'id;
}
[@@deriving show {with_path = false}]

type enum = lident enum_gen
[@@deriving show {with_path = false}]

type 'id asset_item_gen = {
  name: 'id;
  type_: type_;
  original_type: type_;
  default: 'id mterm_gen option;
  shadow: bool;
}
[@@deriving show {with_path = false}]

type asset_item = lident asset_item_gen
[@@deriving show {with_path = false}]

type 'id asset_gen = {
  name: 'id;
  values: 'id asset_item_gen list;
  key: ident;
  sort: ident list;
  invariants  : lident label_term_gen list;
}
[@@deriving show {with_path = false}]

type asset = lident asset_gen
[@@deriving show {with_path = false}]

type 'id contract_signature_gen = {
  name : 'id;
  args: (lident * type_) list;
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
  api_items     : api_storage list;
  api_verif     : api_verif list;
  decls         : 'id decl_node_gen list;
  storage       : 'id storage_gen;
  functions     : 'id function__gen list;
  specification : 'id specification_gen;
  security      : security;
}
[@@deriving show {with_path = false}]

type property =
  | Ppostcondition of postcondition * ident option
  | PstorageInvariant of label_term * ident (* must be called asset invariant *)
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

let mk_label_term ?(loc = Location.dummy) term label : 'id label_term_gen =
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

let mk_var ?(constant=false) ?(invariants=[]) ?default ?(loc = Location.dummy) name type_ original_type : 'id var_gen =
  { name; type_; default; constant; invariants; original_type; loc }

let mk_enum ?(values = []) name initial : 'id enum_gen =
  { name; values; initial }

let mk_enum_item ?(invariants = []) name : 'id enum_item_gen =
  { name; invariants }

let mk_asset ?(values = []) ?(sort=[]) ?(invariants = []) name key : 'id asset_gen =
  { name; values; sort; key; invariants }

let mk_asset_item ?default ?(shadow=false) name type_ original_type : 'id asset_item_gen =
  { name; type_; original_type; default; shadow }

let mk_contract_signature ?(args=[]) ?(loc=Location.dummy) name : 'id contract_signature_gen =
  { name; args; loc }

let mk_contract ?(signatures=[]) ?init ?(loc=Location.dummy) name : 'id contract_gen =
  { name; signatures; init; loc }

let mk_storage_item ?(const=false) ?(ghost = false) ?(loc = Location.dummy) id model_type typ default : 'id storage_item_gen =
  { id; model_type; typ; const; ghost; default; loc }

let mk_function_struct ?(args = []) ?(loc = Location.dummy) ?(src = Exo) name body : function_struct =
  { name; args; src; body; loc }

let mk_function ?spec node : 'id function__gen =
  { node; spec }

let mk_signature ?(args = []) ?ret name : 'id signature_gen =
  { name; args; ret }

let mk_api_item ?(only_formula = false) node_item =
  { node_item; only_formula }

let mk_model ?(api_items = []) ?(api_verif = []) ?(decls = []) ?(functions = []) ?(storage = []) ?(specification = mk_specification ()) ?(security = mk_security ()) name : model =
  { name; api_items; api_verif; storage; decls; functions; specification; security }

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
let cmp_rat_arith_op (op1 : rat_arith_op) (op2 : rat_arith_op) : bool = op1 = op2
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
    (* lambda *)
    | Mletin (i1, a1, t1, b1, o1), Mletin (i2, a2, t2, b2, o2)                         -> List.for_all2 cmpi i1 i2 && cmp a1 a2 && Option.cmp cmp_type t1 t2 && cmp b1 b2 && Option.cmp cmp o1 o2
    | Mdeclvar (i1, t1, v1), Mdeclvar (i2, t2, v2)                                     -> List.for_all2 cmpi i1 i2 && Option.cmp cmp_type t1 t2 && cmp v1 v2
    | Mapp (e1, args1), Mapp (e2, args2)                                               -> cmpi e1 e2 && List.for_all2 cmp args1 args2
    (* assign *)
    | Massign (op1, t1, l1, r1), Massign (op2, t2, l2, r2)                             -> cmp_assign_op op1 op2 && cmp_type t1 t2 && cmpi l1 l2 && cmp r1 r2
    | Massignvarstore (op1, t1, l1, r1), Massignvarstore (op2, t2, l2, r2)             -> cmp_assign_op op1 op2 && cmp_type t1 t2 && cmpi l1 l2 && cmp r1 r2
    | Massignfield (op1, t1, a1, fi1, r1), Massignfield (op2, t2, a2, fi2, r2)         -> cmp_assign_op op1 op2 && cmp_type t1 t2 && cmp a1 a2 && cmpi fi1 fi2 && cmp r1 r2
    | Massignstate x1, Massignstate x2                                                 -> cmp x1 x2
    (* control *)
    | Mif (c1, t1, e1), Mif (c2, t2, e2)                                               -> cmp c1 c2 && cmp t1 t2 && Option.cmp cmp e1 e2
    | Mmatchwith (e1, l1), Mmatchwith (e2, l2)                                         -> cmp e1 e2 && List.for_all2 (fun (p1, t1) (p2, t2) -> cmp_pattern p1 p2 && cmp t1 t2) l1 l2
    | Mfor (i1, c1, b1, lbl1), Mfor (i2, c2, b2, lbl2)                                 -> cmpi i1 i2 && cmp c1 c2 && cmp b1 b2 && Option.cmp cmp_ident lbl1 lbl2
    | Miter (i1, a1, b1, c1, lbl1), Miter (i2, a2, b2, c2, lbl2)                       -> cmpi i1 i2 && cmp a1 a2 && cmp b1 b2 && cmp c1 c2 && Option.cmp cmp_ident lbl1 lbl2
    | Mseq is1, Mseq is2                                                               -> List.for_all2 cmp is1 is2
    | Mreturn x1, Mreturn x2                                                           -> cmp x1 x2
    | Mlabel i1, Mlabel i2                                                             -> cmpi i1 i2
    (* effect *)
    | Mfail ft1, Mfail ft2                                                             -> cmp_fail_type cmp ft1 ft2
    | Mtransfer (v1, d1), Mtransfer (v2, d2)                                           -> cmp v1 v2 && cmp d1 d2
    | Mexternal (t1, func1, c1, args1), Mexternal (t2, func2, c2, args2)               -> cmp_ident t1 t2 && cmpi func1 func2 && cmp c1 c2 && List.for_all2 (fun (id1, t1) (id2, t2) -> cmpi id1 id2 && cmp t1 t2) args1 args2
    (* literals *)
    | Mint v1, Mint v2                                                                 -> Big_int.eq_big_int v1 v2
    | Muint v1, Muint v2                                                               -> Big_int.eq_big_int v1 v2
    | Mbool v1, Mbool v2                                                               -> cmp_bool v1 v2
    | Menum v1, Menum v2                                                               -> cmp_ident v1 v2
    | Mrational (n1, d1), Mrational (n2, d2)                                           -> Big_int.eq_big_int n1 n2 && Big_int.eq_big_int d1 d2
    | Mstring v1, Mstring v2                                                           -> cmp_ident v1 v2
    | Mcurrency (v1, c1), Mcurrency (v2, c2)                                           -> Big_int.eq_big_int v1 v2 && cmp_currency c1 c2
    | Maddress v1, Maddress v2                                                         -> cmp_ident v1 v2
    | Mdate v1, Mdate v2                                                               -> Core.cmp_date v1 v2
    | Mduration v1, Mduration v2                                                       -> Core.cmp_duration v1 v2
    | Mtimestamp v1, Mtimestamp v2                                                     -> Big_int.eq_big_int v1 v2
    | Mbytes v1, Mbytes v2                                                             -> cmp_ident v1 v2
    (* composite type constructors *)
    | Mnone, Mnone                                                                     -> true
    | Msome v1, Msome v2                                                               -> cmp v1 v2
    | Marray l1, Marray l2                                                             -> List.for_all2 cmp l1 l2
    | Mtuple l1, Mtuple l2                                                             -> List.for_all2 cmp l1 l2
    | Masset l1, Masset l2                                                             -> List.for_all2 cmp l1 l2
    | Massoc (k1, v1), Massoc (k2, v2)                                                 -> cmp k1 k2 && cmp v1 v2
    (* dot *)
    | Mdotasset (e1, i1), Mdotasset (e2, i2)                                           -> cmp e1 e2 && cmpi i1 i2
    | Mdotcontract (e1, i1), Mdotcontract (e2, i2)                                     -> cmp e1 e2 && cmpi i1 i2
    (* comparison operators *)
    | Mequal (l1, r1), Mequal (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Mnequal (l1, r1), Mnequal (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Mgt (l1, r1), Mgt (l2, r2)                                                       -> cmp l1 l2 && cmp r1 r2
    | Mge (l1, r1), Mge (l2, r2)                                                       -> cmp l1 l2 && cmp r1 r2
    | Mlt (l1, r1), Mlt (l2, r2)                                                       -> cmp l1 l2 && cmp r1 r2
    | Mle (l1, r1), Mle (l2, r2)                                                       -> cmp l1 l2 && cmp r1 r2
    | Mmulticomp (e1, l1), Mmulticomp (e2, l2)                                         -> cmp e1 e2 && List.for_all2 (fun (op1, t1) (op2, t2) -> cmp_comparison_operator op1 op2 && cmp t1 t2) l1 l2
    (* arithmetic operators *)
    | Mand (l1, r1), Mand (l2, r2)                                                     -> cmp l1 l2 && cmp r1 r2
    | Mor (l1, r1), Mor (l2, r2)                                                       -> cmp l1 l2 && cmp r1 r2
    | Mnot e1, Mnot e2                                                                 -> cmp e1 e2
    | Mplus (l1, r1), Mplus (l2, r2)                                                   -> cmp l1 l2 && cmp r1 r2
    | Mminus (l1, r1), Mminus (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Mmult (l1, r1), Mmult (l2, r2)                                                   -> cmp l1 l2 && cmp r1 r2
    | Mdiv (l1, r1), Mdiv (l2, r2)                                                     -> cmp l1 l2 && cmp r1 r2
    | Mmodulo (l1, r1), Mmodulo (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Muplus e1, Muplus e2                                                             -> cmp e1 e2
    | Muminus e1, Muminus e2                                                           -> cmp e1 e2
    (* asset api effect *)
    | Maddasset (an1, i1), Maddasset (an2, i2)                                         -> cmp_ident an1 an2 && cmp i1 i2
    | Maddfield (an1, fn1, c1, i1), Maddfield (an2, fn2, c2, i2)                       -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp c1 c2 && cmp i1 i2
    | Mremoveasset (an1, i1), Mremoveasset (an2, i2)                                   -> cmp_ident an1 an2 && cmp i1 i2
    | Mremovefield (an1, fn1, c1, i1), Mremovefield (an2, fn2, c2, i2)                 -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp c1 c2 && cmp i1 i2
    | Mclearasset an1, Mclearasset an2                                                 -> cmp_ident an1 an2
    | Mclearfield (an1, fn1), Mclearfield (an2, fn2)                                   -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | Mset (c1, l1, k1, v1), Mset (c2, l2, k2, v2)                                     -> cmp_ident c1 c2 && List.for_all2 cmp_ident l1 l2 && cmp k1 k2 && cmp v1 v2
    | Mupdate (an1, k1, l1), Mupdate (an2, k2, l2)                                     -> cmp_ident an1 an2 && cmp k1 k2 && List.for_all2 (fun (id1, op1, v1) (id2, op2, v2) -> cmpi id1 id2 && cmp_assign_op op1 op2 && cmp v1 v2) l1 l2
    | Maddupdate (an1, k1, l1), Maddupdate (an2, k2, l2)                               -> cmp_ident an1 an2 && cmp k1 k2 && List.for_all2 (fun (id1, op1, v1) (id2, op2, v2) -> cmpi id1 id2 && cmp_assign_op op1 op2 && cmp v1 v2) l1 l2| Mremoveif (an1, fn1, i1), Mremoveif (an2, fn2, i2)                               -> cmp_ident an1 an2 && cmp fn1 fn2 && cmp i1 i2
    (* asset api expression *)
    | Mget (c1, k1), Mget (c2, k2)                                                     -> cmp_ident c1 c2 && cmp k1 k2
    | Mselect (an1, c1, p1), Mselect (an2, c2, p2)                                     -> cmp_ident an1 an2 && cmp c1 c2 && cmp p1 p2
    | Msort (an1, c1, fn1, k1), Msort (an2, c2, fn2, k2)                               -> cmp_ident an1 an2 && cmp c1 c2 && cmp_ident fn1 fn2 && k1 = k2
    | Mcontains (an1, c1, i1), Mcontains (an2, c2, i2)                                 -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Mnth (an1, c1, i1), Mnth (an2, c2, i2)                                           -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Mcount (an1, c1), Mcount (an2, c2)                                               -> cmp_ident an1 an2 && cmp c1 c2
    | Msum (an1, fd1, c1), Msum (an2, fd2, c2)                                         -> cmp_ident an1 an2 && cmpi fd1 fd2 && cmp c1 c2
    | Mmin (an1, fd1, c1), Mmin (an2, fd2, c2)                                         -> cmp_ident an1 an2 && cmpi fd1 fd2 && cmp c1 c2
    | Mmax (an1, fd1, c1), Mmax (an2, fd2, c2)                                         -> cmp_ident an1 an2 && cmpi fd1 fd2 && cmp c1 c2
    | Mhead (an1, c1, i1), Mhead (an2, c2, i2)                                         -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Mtail (an1, c1, i1), Mtail (an2, c2, i2)                                         -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    (* list api effect *)
    | Mlistprepend (c1, a1), Mlistprepend (c2, a2)                                     -> cmp c1 c2 && cmp a1 a2
    (* list api expression *)
    | Mlistcontains (c1, a1), Mlistcontains (c2, a2)                                   -> cmp c1 c2 && cmp a1 a2
    | Mlistcount c1, Mlistcount c2                                                     -> cmp c1 c2
    | Mlistnth (c1, a1), Mlistnth (c2, a2)                                             -> cmp c1 c2 && cmp a1 a2
    (* builtin functions *)
    | Mfunmin (l1, r1), Mfunmin (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Mfunmax (l1, r1), Mfunmax (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Mfunabs a1, Mfunabs a2                                                           -> cmp a1 a2
    (* constants *)
    | Mvarstate, Mvarstate                                                             -> true
    | Mnow, Mnow                                                                       -> true
    | Mtransferred, Mtransferred                                                       -> true
    | Mcaller, Mcaller                                                                 -> true
    | Mbalance, Mbalance                                                               -> true
    | Msource, Msource                                                                 -> true
    (* variables *)
    | Mvarstorevar v1, Mvarstorevar v2                                                 -> cmpi v1 v2
    | Mvarstorecol v1, Mvarstorecol v2                                                 -> cmpi v1 v2
    | Mvarenumval v1, Mvarenumval v2                                                   -> cmpi v1 v2
    | Mvarlocal v1, Mvarlocal v2                                                       -> cmpi v1 v2
    | Mvarparam v1, Mvarparam v2                                                       -> cmpi v1 v2
    | Mvarfield v1, Mvarfield v2                                                       -> cmpi v1 v2
    | Mvarthe, Mvarthe                                                                 -> true
    (* rational *)
    | Mdivrat (l1, r1), Mdivrat (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Mrateq (l1, r1), Mrateq (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Mratcmp (op1, l1, r1), Mratcmp (op2, l2, r2)                                     -> cmp_comparison_operator op1 op2 && cmp l1 l2 && cmp r1 r2
    | Mratarith (op1, l1, r1), Mratarith (op2, l2, r2)                                 -> cmp_rat_arith_op op1 op2 && cmp l1 l2 && cmp r1 r2
    | Mrattez (c1, t1), Mrattez (c2, t2)                                               -> cmp c1 c2 && cmp t1 t2
    | Minttorat e1, Minttorat e2                                                       -> cmp e1 e2
    (* functional *)
    | Mfold (i1, is1, c1, b1), Mfold (i2, is2, c2, b2)                                 -> cmpi i1 i2 && List.for_all2 cmpi is1 is2 && cmp c1 c2 && cmp b1 b2
    (* imperative *)
    | Mbreak, Mbreak                                                                   -> true
    (* shallowing *)
    | Mshallow (i1, x1), Mshallow (i2, x2)                                             -> cmp x1 x2 && cmp_ident i1 i2
    | Mlisttocoll (i1, x1), Mlisttocoll (i2, x2)                                       -> cmp x1 x2 && cmp_ident i1 i2
    | Munshallow (i1, x1), Munshallow (i2, x2)                                         -> cmp x1 x2 && cmp_ident i1 i2
    | Maddshallow (e1, args1), Maddshallow (e2, args2)                                 -> cmp_ident e1 e2 && List.for_all2 cmp args1 args2
    (* collection keys *)
    | Mtokeys (a1, x1), Mtokeys (a2, x2)                                               -> cmp_ident a1 a2 && cmp x1 x2
    | Mcoltokeys (a1), Mcoltokeys (a2)                                                 -> cmp_ident a1 a2
    (* quantifiers *)
    | Mforall (i1, t1, t2, e1), Mforall (i2, t3, t4, e2)                               -> cmpi i1 i2 && cmp_type t1 t3 && Option.cmp cmp t2 t4 && cmp e1 e2
    | Mexists (i1, t1, t2, e1), Mforall (i2, t3, t4, e2)                               -> cmpi i1 i2 && cmp_type t1 t3 && Option.cmp cmp t2 t4 && cmp e1 e2
    (* formula operators *)
    | Mimply (l1, r1), Mimply (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Mequiv (l1, r1), Mequiv (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    (* formula expression*)
    | Mgetbefore (c1, k1), Mgetbefore (c2, k2)                                         -> cmp_ident c1 c2 && cmp k1 k2
    | Mgetat (c1, d1, k1), Mgetat (c2, d2, k2)                                         -> cmp_ident c1 c2 && cmp_ident d1 d2 && cmp k1 k2
    | Mgetfrommap (an1, k1, c1), Mgetfrommap (an2, k2, c2)                             -> cmp_ident an1 an2 && cmp k1 k2 && cmp c1 c2
    | Mmem (an1, c1, i1), Mmem (an2, c2, i2)                                           -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Msubsetof (an1, c1, i1), Msubsetof (an2, c2, i2)                                 -> cmp_ident an1 an2 && cmp c1 c2 && cmp i1 i2
    | Misempty (l1, r1), Misempty (l2, r2)                                             -> cmp_ident l1 l2 && cmp r1 r2
    (* set api *)
    | Msetbefore e1, Msetbefore e2                                                     -> cmp e1 e2
    | Msetat (lbl1, e1), Msetat (lbl2, e2)                                             -> cmp_ident lbl1 lbl2 && cmp e1 e2
    | Msetunmoved e1, Msetunmoved e2                                                   -> cmp e1 e2
    | Msetadded e1, Msetadded e2                                                       -> cmp e1 e2
    | Msetremoved e1, Msetremoved   e2                                                 -> cmp e1 e2
    | Msetiterated e1, Msetiterated  e2                                                -> cmp e1 e2
    | Msettoiterate e1, Msettoiterate e2                                               -> cmp e1 e2
    (* *)
    | _ -> false
  with
    _ -> false

let rec cmp_mterm (term1 : mterm) (term2 : mterm) : bool =
  cmp_mterm_node cmp_mterm cmp_lident term1.node term2.node

let cmp_api_item_node (a1 : api_storage_node) (a2 : api_storage_node) : bool =
  let cmp_api_asset (s1 : api_asset) (s2 : api_asset) : bool =
    match s1, s2 with
    | Get an1, Get an2                                 -> cmp_ident an1 an2
    | Set an1 , Set an2                                -> cmp_ident an1 an2
    | Add an1 , Add an2                                -> cmp_ident an1 an2
    | Remove an1, Remove an2                           -> cmp_ident an1 an2
    | UpdateAdd (an1, fn1), UpdateAdd (an2, fn2)       -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | UpdateRemove (an1, fn1), UpdateRemove (an2, fn2) -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | ToKeys an1, ToKeys an2                           -> cmp_ident an1 an2
    | ColToKeys an1, ColToKeys an2                     -> cmp_ident an1 an2
    | Select (an1, p1), Select (an2, p2)               -> cmp_ident an1 an2 && cmp_mterm p1 p2
    | Sort (an1 , fn1), Sort (an2 , fn2)               -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | Contains an1, Contains an2                       -> cmp_ident an1 an2
    | Nth an1, Nth an2                                 -> cmp_ident an1 an2
    | Count an1, Count an2                             -> cmp_ident an1 an2
    | Sum (an1, fn1), Sum (an2, fn2)                   -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | Min (an1, fn1), Min (an2, fn2)                   -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | Max (an1, fn1), Max (an2, fn2)                   -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | Shallow an1, Shallow an2                         -> cmp_ident an1 an2
    | Unshallow an1, Unshallow an2                     -> cmp_ident an1 an2
    | Listtocoll an1, Listtocoll an2                   -> cmp_ident an1 an2
    | Head an1, Head an2                               -> cmp_ident an1 an2
    | Tail an1, Tail an2                               -> cmp_ident an1 an2
    | _ -> false
  in

  let cmp_api_list (c1 : api_list) (c2 : api_list) : bool =
    match c1, c2 with
    | Lprepend t1,  Lprepend t2  -> cmp_type t1 t2
    | Lcontains t1, Lcontains t2 -> cmp_type t1 t2
    | Lcount t1,    Lcount t2    -> cmp_type t1 t2
    | Lnth t1,      Lnth t2      -> cmp_type t1 t2
    | _ -> false
  in
  let cmp_api_builtin (b1 : api_builtin) (b2 : api_builtin) : bool =
    match b1, b2 with
    | MinBuiltin t1, MinBuiltin t2 -> cmp_type t1 t2
    | MaxBuiltin t1, MaxBuiltin t2 -> cmp_type t1 t2
    | _ -> false
  in
  let cmp_api_internal (i1 : api_internal) (i2 : api_internal) : bool =
    match i1, i2 with
    | RatEq,    RatEq    -> true
    | RatCmp,   RatCmp   -> true
    | RatArith, RatArith -> true
    | RatTez,   RatTez   -> true
    | _ -> false
  in
  match a1, a2 with
  | APIAsset s1,    APIAsset s2    -> cmp_api_asset s1 s2
  | APIList c1,     APIList c2     -> cmp_api_list c1 c2
  | APIBuiltin f1,  APIBuiltin f2  -> cmp_api_builtin f1 f2
  | APIInternal i1, APIInternal i2 -> cmp_api_internal i1 i2
  | _ -> false

(* -------------------------------------------------------------------- *)

let cmp_api_verif (v1 : api_verif) (v2 : api_verif) : bool =
  match v1, v2 with
  | StorageInvariant (l1, an1, mt1), StorageInvariant (l2, an2, mt2) -> cmp_ident l1 l2 && cmp_ident an1 an2 && cmp_mterm mt1 mt2
(* | _ -> false *)

(* -------------------------------------------------------------------- *)
let map_type (f : type_ -> type_) = function
  | Tasset id         -> Tasset id
  | Tenum id          -> Tenum id
  | Tstate            -> Tstate
  | Tcontract id      -> Tcontract id
  | Tbuiltin b        -> Tbuiltin b
  | Tcontainer (t, c) -> Tcontainer (f t, c)
  | Toption t         -> Toption (f t)
  | Ttuple l          -> Ttuple (List.map f l)
  | Tassoc (a, t)     -> Tassoc (a, f t)
  | Tunit             -> Tunit
  | Tstorage          -> Tstorage
  | Toperation        -> Toperation
  | Tentry            -> Tentry
  | Tprog t           -> Tprog (f t)
  | Tvset (v, t)      -> Tvset (v, t)
  | Ttrace t          -> Ttrace t

(* -------------------------------------------------------------------- *)

let map_term_node (f : 'id mterm_gen -> 'id mterm_gen) = function
  (* lambda *)
  | Mletin (i, a, t, b, o)         -> Mletin (i, f a, t, f b, Option.map f o)
  | Mdeclvar (i, t, v)             -> Mdeclvar (i, t, f v)
  | Mapp (e, args)                 -> Mapp (e, List.map f args)
  (* assign *)
  | Massign (op, t, l, r)          -> Massign (op, t, l, f r)
  | Massignvarstore (op, t, l, r)  -> Massignvarstore (op, t, l, f r)
  | Massignfield (op, t, a, fi, r) -> Massignfield (op, t, a, fi, f r)
  | Massignstate x                 -> Massignstate (f x)
  (* control *)
  | Mif (c, t, e)                  -> Mif (f c, f t, Option.map f e)
  | Mmatchwith (e, l)              -> Mmatchwith (e, List.map (fun (p, e) -> (p, f e)) l)
  | Mfor (i, c, b, lbl)            -> Mfor (i, f c, f b, lbl)
  | Miter (i, a, b, c, lbl)        -> Miter (i, f a, f b, f c, lbl)
  | Mseq is                        -> Mseq (List.map f is)
  | Mreturn x                      -> Mreturn (f x)
  | Mlabel i                       -> Mlabel i
  (* effect *)
  | Mfail v                        -> Mfail (match v with | Invalid v -> Invalid (f v) | _ -> v)
  | Mtransfer (v, d)               -> Mtransfer (f v, f d)
  | Mexternal (t, func, c, args)   -> Mexternal (t, func, f c, List.map (fun (id, t) -> (id, f t)) args)
  (* literals *)
  | Mint v                         -> Mint v
  | Muint v                        -> Muint v
  | Mbool v                        -> Mbool v
  | Menum v                        -> Menum v
  | Mrational (n, d)               -> Mrational (n, d)
  | Mstring v                      -> Mstring v
  | Mcurrency (v, c)               -> Mcurrency (v, c)
  | Maddress v                     -> Maddress v
  | Mdate v                        -> Mdate v
  | Mduration v                    -> Mduration v
  | Mtimestamp v                   -> Mtimestamp v
  | Mbytes v                       -> Mbytes v
  (* composite type constructors *)
  | Mnone                          -> Mnone
  | Msome v                        -> Msome (f v)
  | Marray l                       -> Marray (List.map f l)
  | Mtuple l                       -> Mtuple (List.map f l)
  | Masset l                       -> Masset (List.map f l)
  | Massoc (k, v)                  -> Massoc (f k, f v)
  (* dot *)
  | Mdotasset (e, i)               -> Mdotasset (f e, i)
  | Mdotcontract (e, i)            -> Mdotcontract (f e, i)
  (* comparison operators *)
  | Mequal (l, r)                  -> Mequal (f l, f r)
  | Mnequal (l, r)                 -> Mnequal (f l, f r)
  | Mgt (l, r)                     -> Mgt (f l, f r)
  | Mge (l, r)                     -> Mge (f l, f r)
  | Mlt (l, r)                     -> Mlt (f l, f r)
  | Mle (l, r)                     -> Mle (f l, f r)
  | Mmulticomp (e, l)              -> Mmulticomp (f e, List.map (fun (op, e) -> (op, f e)) l)
  (* arithmetic operators *)
  | Mand (l, r)                    -> Mand (f l, f r)
  | Mor (l, r)                     -> Mor (f l, f r)
  | Mnot e                         -> Mnot (f e)
  | Mplus (l, r)                   -> Mplus (f l, f r)
  | Mminus (l, r)                  -> Mminus (f l, f r)
  | Mmult (l, r)                   -> Mmult (f l, f r)
  | Mdiv (l, r)                    -> Mdiv (f l, f r)
  | Mmodulo (l, r)                 -> Mmodulo (f l, f r)
  | Muplus e                       -> Muplus (f e)
  | Muminus e                      -> Muminus (f e)
  (* asset api effect *)
  | Maddasset (an, i)              -> Maddasset (an, f i)
  | Maddfield (an, fn, c, i)       -> Maddfield (an, fn, f c, f i)
  | Mremoveasset (an, i)           -> Mremoveasset (an, f i)
  | Mremovefield (an, fn, c, i)    -> Mremovefield (an, fn, f c, f i)
  | Mclearasset (an)               -> Mclearasset (an)
  | Mclearfield (an, fn)           -> Mclearfield (an, fn)
  | Mset (c, l, k, v)              -> Mset (c, l, f k, f v)
  | Mupdate (an, k, l)             -> Mupdate (an, f k, List.map (fun (id, op, v) -> (id, op, f v)) l)
  | Mremoveif (an, fn, i)          -> Mremoveif (an, f fn, f i)
  | Maddupdate (an, k, l)          -> Maddupdate (an, f k, List.map (fun (id, op, v) -> (id, op, f v)) l)
  (* asset api expression *)
  | Mget (c, k)                    -> Mget (c, f k)
  | Mselect (an, c, p)             -> Mselect (an, f c, f p)
  | Msort (an, c, fn, k)           -> Msort (an, f c, fn, k)
  | Mcontains (an, c, i)           -> Mcontains (an, f c, f i)
  | Mnth (an, c, i)                -> Mnth (an, f c, f i)
  | Mcount (an, c)                 -> Mcount (an, f c)
  | Msum (an, fd, c)               -> Msum (an, fd, f c)
  | Mmin (an, fd, c)               -> Mmin (an, fd, f c)
  | Mmax (an, fd, c)               -> Mmax (an, fd, f c)
  | Mhead (an, c, i)               -> Mhead (an, f c, f i)
  | Mtail (an, c, i)               -> Mtail (an, f c, f i)
  (* list api effect *)
  | Mlistprepend (c, a)            -> Mlistprepend (f c, f a)
  (* list api expression *)
  | Mlistcontains (c, a)           -> Mlistcontains (f c, f a)
  | Mlistcount c                   -> Mlistcount (f c)
  | Mlistnth (c, a)                -> Mlistnth (f c, f a)
  (* builtin functions *)
  | Mfunmin (l, r)                 -> Mfunmin (f l, f r)
  | Mfunmax (l, r)                 -> Mfunmax (f l, f r)
  | Mfunabs a                      -> Mfunabs (f a)
  (* constants *)
  | Mvarstate                      -> Mvarstate
  | Mnow                           -> Mnow
  | Mtransferred                   -> Mtransferred
  | Mcaller                        -> Mcaller
  | Mbalance                       -> Mbalance
  | Msource                        -> Msource
  (* variables *)
  | Mvarstorevar v                 -> Mvarstorevar v
  | Mvarstorecol v                 -> Mvarstorecol v
  | Mvarenumval v                  -> Mvarenumval  v
  | Mvarlocal v                    -> Mvarlocal    v
  | Mvarparam v                    -> Mvarparam    v
  | Mvarfield v                    -> Mvarfield    v
  | Mvarthe                        -> Mvarthe
  (* rational *)
  | Mdivrat (l, r)                 -> Mdivrat (f l, f r)
  | Mrateq (l, r)                  -> Mrateq (f l, f r)
  | Mratcmp (op, l, r)             -> Mratcmp (op, f l, f r)
  | Mratarith (op, l, r)           -> Mratarith (op, f l, f r)
  | Mrattez (c, t)                 -> Mrattez (f c, f t)
  | Minttorat e                    -> Minttorat (f e)
  (* functional *)
  | Mfold (i, is, c, b)            -> Mfold (i, is, f c, f b)
  (* imperative *)
  | Mbreak                         -> Mbreak
  (* shallowing *)
  | Mshallow (i, x)                -> Mshallow (i, f x)
  | Munshallow (i, x)              -> Munshallow (i, f x)
  | Mlisttocoll (i, x)             -> Mlisttocoll (i, f x)
  | Maddshallow (e, args)          -> Maddshallow (e, List.map f args)
  (* collection keys *)
  | Mtokeys (an, x)                -> Mtokeys (an, f x)
  | Mcoltokeys an                  -> Mcoltokeys an
  (* quantifiers *)
  | Mforall (i, t, s, e)           -> Mforall (i, t, Option.map f s, f e)
  | Mexists (i, t, s, e)           -> Mexists (i, t, Option.map f s, f e)
  (* formula operators *)
  | Mimply (l, r)                  -> Mimply (f l, f r)
  | Mequiv  (l, r)                 -> Mequiv (f l, f r)
  (* formula expression*)
  | Mgetbefore (c, k)              -> Mgetbefore (c, f k)
  | Mgetat (c, d, k)               -> Mgetat (c, d, f k)
  | Mgetfrommap (an, k, c)         -> Mgetfrommap (an, f k, f c)
  | Mmem (an, c, i)                -> Mmem (an, f c, f i)
  | Msubsetof (an, c, i)           -> Msubsetof (an, f c, f i)
  | Misempty (l, r)                -> Misempty (l, f r)
  (* set api *)
  | Msetbefore    e                -> Msetbefore    (f e)
  | Msetat (lbl, e)                -> Msetat        (lbl, f e)
  | Msetunmoved   e                -> Msetunmoved   (f e)
  | Msetadded     e                -> Msetadded     (f e)
  | Msetremoved   e                -> Msetremoved   (f e)
  | Msetiterated  e                -> Msetiterated  (f e)
  | Msettoiterate e                -> Msettoiterate (f e)


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
      let ctx = { ctx with label = Some lt.label } in
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
  let opt f accu x = match x with | Some v -> f accu v | None -> accu in
  match term.node with
  (* lambda *)
  | Mletin (_, a, _, b, o)                -> let tmp = f (f accu a) b in Option.map_dfl (f tmp) tmp o
  | Mdeclvar (_, _, v)                    -> f accu v
  | Mapp (_, args)                        -> List.fold_left f accu args
  (* assign *)
  | Massign (_, _, _, e)                  -> f accu e
  | Massignvarstore (_, _, _, e)          -> f accu e
  | Massignfield (_, _, _, _, e)          -> f accu e
  | Massignstate x                        -> f accu x
  (* control *)
  | Mif (c, t, e)                         -> opt f (f (f accu c) t) e
  | Mmatchwith (e, l)                     -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Mfor (_, c, b, _)                     -> f (f accu c) b
  | Miter (_, a, b, c, _)                 -> f (f (f accu a) b) c
  | Mseq is                               -> List.fold_left f accu is
  | Mreturn x                             -> f accu x
  | Mlabel _                              -> accu
  (* effect *)
  | Mfail v                               -> (match v with | Invalid v -> f accu v | _ -> accu)
  | Mtransfer (v, d)                      -> f (f accu v) d
  | Mexternal (_, _, c, args)             -> List.fold_left (fun accu (_, t) -> f accu t) (f accu c) args
  (* literals *)
  | Mint _                                -> accu
  | Muint _                               -> accu
  | Mbool _                               -> accu
  | Menum _                               -> accu
  | Mrational _                           -> accu
  | Mstring _                             -> accu
  | Mcurrency _                           -> accu
  | Maddress _                            -> accu
  | Mdate _                               -> accu
  | Mduration _                           -> accu
  | Mtimestamp _                          -> accu
  | Mbytes _                              -> accu
  (* composite type constructors *)
  | Mnone                                 -> accu
  | Msome v                               -> f accu v
  | Marray l                              -> List.fold_left f accu l
  | Mtuple l                              -> List.fold_left f accu l
  | Masset l                              -> List.fold_left f accu l
  | Massoc (k, v)                         -> f (f accu k) v
  (* dot *)
  | Mdotasset (e, _)                      -> f accu e
  | Mdotcontract (e, _)                   -> f accu e
  (* comparison operators *)
  | Mequal (l, r)                         -> f (f accu l) r
  | Mnequal (l, r)                        -> f (f accu l) r
  | Mgt (l, r)                            -> f (f accu l) r
  | Mge (l, r)                            -> f (f accu l) r
  | Mlt (l, r)                            -> f (f accu l) r
  | Mle (l, r)                            -> f (f accu l) r
  | Mmulticomp (e, l)                     -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  (* arithmetic operators *)
  | Mand (l, r)                           -> f (f accu l) r
  | Mor (l, r)                            -> f (f accu l) r
  | Mnot e                                -> f accu e
  | Mplus (l, r)                          -> f (f accu l) r
  | Mminus (l, r)                         -> f (f accu l) r
  | Mmult (l, r)                          -> f (f accu l) r
  | Mdiv (l, r)                           -> f (f accu l) r
  | Mmodulo (l, r)                        -> f (f accu l) r
  | Muplus e                              -> f accu e
  | Muminus e                             -> f accu e
  (* asset api effect *)
  | Maddasset (_, i)                      -> f accu i
  | Maddfield (_, _, c, i)                -> f (f accu c) i
  | Mremoveasset (_, i)                   -> f accu i
  | Mremovefield (_, _, c, i)             -> f (f accu c) i
  | Mclearasset _                         -> accu
  | Mclearfield _                         -> accu
  | Mset (_, _, k, v)                     -> f (f accu v) k
  | Mupdate (_, k, l)                     -> List.fold_left (fun accu (_, _, v) -> f accu v) (f accu k) l
  | Mremoveif (_, fn, c)                  -> f (f accu fn) c
  | Maddupdate (_, k, l)                  -> List.fold_left (fun accu (_, _, v) -> f accu v) (f accu k) l
  (* asset api expression *)
  | Mget (_, k)                           -> f accu k
  | Mselect (_, c, p)                     -> f (f accu c) p
  | Msort (_, c, _, _)                    -> f accu c
  | Mcontains (_, c, i)                   -> f (f accu c) i
  | Mnth (_, c, i)                        -> f (f accu c) i
  | Mcount (_, c)                         -> f accu c
  | Msum (_, _, c)                        -> f accu c
  | Mmin (_, _, c)                        -> f accu c
  | Mmax (_, _, c)                        -> f accu c
  | Mhead (_, c, i)                       -> f (f accu c) i
  | Mtail (_, c, i)                       -> f (f accu c) i
  (* list api effect *)
  | Mlistprepend (c, a)                   -> f (f accu c) a
  (* list api expression *)
  | Mlistcontains (c, a)                  -> f (f accu c) a
  | Mlistcount c                          -> f accu c
  | Mlistnth (c, a)                       -> f (f accu c) a
  (* builtin functions *)
  | Mfunmax (l, r)                        -> f (f accu l) r
  | Mfunmin (l, r)                        -> f (f accu l) r
  | Mfunabs a                             -> f accu a
  (* constants *)
  | Mvarstate                             -> accu
  | Mnow                                  -> accu
  | Mtransferred                          -> accu
  | Mcaller                               -> accu
  | Mbalance                              -> accu
  | Msource                               -> accu
  (* variables *)
  | Mvarstorevar _                        -> accu
  | Mvarstorecol _                        -> accu
  | Mvarenumval _                         -> accu
  | Mvarlocal _                           -> accu
  | Mvarparam _                           -> accu
  | Mvarfield _                           -> accu
  | Mvarthe                               -> accu
  (* rational *)
  | Mdivrat (l, r)                        -> f (f accu l) r
  | Mrateq (l, r)                         -> f (f accu l) r
  | Mratcmp (_, l, r)                     -> f (f accu l) r
  | Mratarith (_, l, r)                   -> f (f accu l) r
  | Mrattez (c, t)                        -> f (f accu c) t
  | Minttorat e                           -> f accu e
  (* functional *)
  | Mfold (_, _, c, b)                    -> f (f accu c) b
  (* imperative *)
  | Mbreak                                -> accu
  (* shallowing *)
  | Mshallow (_, x)                       -> f accu x
  | Munshallow (_, x)                     -> f accu x
  | Mlisttocoll (_, x)                    -> f accu x
  | Maddshallow (_, args)                 -> List.fold_left f accu args
  (* collection keys *)
  | Mtokeys (_, x)                        -> f accu x
  | Mcoltokeys (_)                        -> accu
  (* quantifiers *)
  | Mforall (_, _, s, e)                  -> f (opt f accu s) e
  | Mexists (_, _, s, e)                  -> f (opt f accu s) e
  (* formula operators *)
  | Mimply (l, r)                         -> f (f accu l) r
  | Mequiv  (l, r)                        -> f (f accu l) r
  (* formula expression*)
  | Mgetbefore (_, k)                     -> f accu k
  | Mgetat (_, _, k)                      -> f accu k
  | Mgetfrommap (_, k, c)                 -> f (f accu k) c
  | Mmem (_, c, i)                        -> f (f accu c) i
  | Msubsetof (_, c, i)                   -> f (f accu c) i
  | Misempty  (_, r)                      -> f accu r
  (* set api *)
  | Msetbefore    e                       -> f accu e
  | Msetat   (_, e)                       -> f accu e
  | Msetunmoved   e                       -> f accu e
  | Msetadded     e                       -> f accu e
  | Msetremoved   e                       -> f accu e
  | Msetiterated  e                       -> f accu e
  | Msettoiterate e                       -> f accu e


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
  (* lambda *)

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

  | Mapp (id, args) ->
    let ((argss, argsa) : 'c list * 'a) =
      List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) args
    in
    g (Mapp (id, argss)), argsa


  (* assign *)

  | Massign (op, t, id, x) ->
    let xe, xa = f accu x in
    g (Massign (op, t, id, xe)), xa

  | Massignvarstore (op, t, id, x) ->
    let xe, xa = f accu x in
    g (Massignvarstore (op, t, id, xe)), xa

  | Massignfield (op, t, a, fi, x) ->
    let xe, xa = f accu x in
    g (Massignfield (op, t, a, fi, xe)), xa

  | Massignstate x ->
    let xe, xa = f accu x in
    g (Massignstate xe), xa


  (* control *)

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

  | Mfor (i, c, b, lbl) ->
    let ce, ca = f accu c in
    let bi, ba = f ca b in
    g (Mfor (i, ce, bi, lbl)), ba

  | Miter (i, a, b, c, lbl) ->
    let ae, aa = f accu a in
    let be, ba = f aa b in
    let ce, ca = f ba c in
    g (Miter (i, ae, be, ce, lbl)), ca

  | Mseq is ->
    let (isi, isa) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) is in
    g (Mseq isi), isa

  | Mreturn x ->
    let xe, xa = f accu x in
    g (Mreturn xe), xa

  | Mlabel i ->
    g (Mlabel i), accu


  (* effect *)

  | Mfail ft ->
    let fte, fta =
      match ft with
      | Invalid mt ->
        let mte, accu = f accu mt in
        Invalid mte, accu
      | _ -> ft, accu
    in
    g (Mfail fte), fta

  | Mtransfer (v, d) ->
    let ve, va = f accu v in
    let de, da = f va d in
    g (Mtransfer (ve, de)), da

  | Mexternal (t, func, c, args) ->
    let ce, ca = f accu c in
    let (lp, la) = List.fold_left
        (fun (pterms, accu) (id, x) ->
           let p, accu = f accu x in
           pterms @ [id, p], accu) ([], ca) args in
    g (Mexternal (t, func, ce, lp)), la


  (* literals *)

  | Mint v ->
    g (Mint v), accu

  | Muint v ->
    g (Muint v), accu

  | Mbool v ->
    g (Mbool v), accu

  | Menum v ->
    g (Menum v), accu

  | Mrational (n, d) ->
    g (Mrational (n, d)), accu

  | Mstring v ->
    g (Mstring v), accu

  | Mcurrency (v, c) ->
    g (Mcurrency (v, c)), accu

  | Maddress v ->
    g (Maddress v), accu

  | Mdate v ->
    g (Mdate v), accu

  | Mduration v ->
    g (Mduration v), accu

  | Mtimestamp v ->
    g (Mtimestamp v), accu

  | Mbytes v ->
    g (Mbytes v), accu


  (* composite type constructors *)

  | Mnone ->
    g Mnone, accu

  | Msome v ->
    let ve, va = f accu v in
    g (Msome ve), va

  | Marray l ->
    let le, la = fold_map_term_list f accu l in
    g (Marray le), la

  | Mtuple l ->
    let le, la = fold_map_term_list f accu l in
    g (Mtuple le), la

  | Masset l ->
    let le, la = fold_map_term_list f accu l in
    g (Masset le), la

  | Massoc (k, v) ->
    let ke, ka = f accu k in
    let ve, va = f ka v in
    g (Massoc (ke, ve)), va


  (* dot *)

  | Mdotasset (e, i) ->
    let ee, ea = f accu e in
    g (Mdotasset (ee, i)), ea

  | Mdotcontract (e, i) ->
    let ee, ea = f accu e in
    g (Mdotcontract (ee, i)), ea


  (* comparison operators *)

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

  | Mmulticomp (e, l) ->
    let ee, ea = f accu e in
    let (le, la) =
      List.fold_left
        (fun (ps, accu) (p, i) ->
           let ia, accu = f accu i in
           [(p, ia)] @ ps, accu) ([], ea) l
    in
    g (Mmulticomp (ee, le)), la


  (* arithmetic operators *)

  | Mand (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mand (le, re)), ra

  | Mor (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mor (le, re)), ra

  | Mnot e ->
    let ee, ea = f accu e in
    g (Mnot ee), ea

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


  (* asset api effect *)

  | Maddasset (an, i) ->
    let ie, ia = f accu i in
    g (Maddasset (an, ie)), ia

  | Maddfield (an, fn, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Maddfield (an, fn, ce, ie)), ia

  | Mremoveasset (an, i) ->
    let ie, ia = f accu i in
    g (Mremoveasset (an, ie)), ia

  | Mremovefield (an, fn, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mremovefield (an, fn, ce, ie)), ia

  | Mclearasset an ->
    g (Mclearasset an), accu

  | Mclearfield (an, fn) ->
    g (Mclearfield (an, fn)), accu

  | Mset (c, l, k, v) ->
    let ke, ka = f accu k in
    let ve, va = f ka v in
    g (Mset (c, l, ke, ve)), va

  | Mupdate (an, k, l) ->
    let ke, ka = f accu k in
    let le, la =
      List.fold_left
        (fun (ps, accu) (id, op, v) ->
           let va, accu = f accu v in
           (id, op, va)::ps, accu) ([], ka) l
      |> (fun (x, y) -> (List.rev x, y))
    in
    g (Mupdate (an, ke, le)), la

  | Mremoveif (an, fn, i) ->
    let ie, ia = f accu i in
    let fe, fa = f ia fn in
    g (Mremoveif (an, fe, ie)), fa

  | Maddupdate (an, k, l) ->
    let ke, ka = f accu k in
    let le, la =
      List.fold_left
        (fun (ps, accu) (id, op, v) ->
           let va, accu = f accu v in
           (id, op, va)::ps, accu) ([], ka) l
      |> (fun (x, y) -> (List.rev x, y))
    in
    g (Mupdate (an, ke, le)), la


  (* asset api expression *)

  | Mget (c, k) ->
    let ke, ka = f accu k in
    g (Mget (c, ke)), ka

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

  | Mhead (an, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mhead (an, ce, ie)), ia

  | Mtail (an, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mtail (an, ce, ie)), ia


  (* list api effect *)

  | Mlistprepend (c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Mlistprepend (ce, ae)), aa


  (* list api expression *)

  | Mlistcontains (c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Mlistcontains (ce, ae)), aa

  | Mlistcount c ->
    let ce, ca = f accu c in
    g (Mlistcount ce), ca

  | Mlistnth (c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Mlistnth (ce, ae)), aa


  (* builtin functions *)

  | Mfunmin (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mfunmin (le, re)), ra

  | Mfunmax (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mfunmax (le, re)), ra

  | Mfunabs a ->
    let ae, aa = f accu a in
    g (Mfunabs ae), aa


  (* constants *)

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

  | Msource ->
    g Msource, accu


  (* variables *)

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


  (* rational *)

  | Mdivrat (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mdivrat (le, re)), ra

  | Mrateq (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mrateq (le, re)), ra

  | Mratcmp (op, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mratcmp (op, le, re)), ra

  | Mratarith (op, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mratarith (op, le, re)), ra

  | Mrattez (c, t) ->
    let ce, ca = f accu c in
    let te, ta = f ca t in
    g (Mrattez (ce, te)), ta

  | Minttorat e ->
    let ee, ea = f accu e in
    g (Minttorat ee), ea


  (* functional *)

  | Mfold (i, is, c, b) ->
    let ce, ca = f accu c in
    let bi, ba = f ca b in
    g (Mfold (i, is, ce, bi)), ba


  (* imperative *)

  | Mbreak ->
    g (Mbreak), accu


  (* shallowing *)

  | Mshallow (i, x) ->
    let xe, xa = f accu x in
    g (Mshallow (i,xe)), xa

  | Munshallow (i, x) ->
    let xe, xa = f accu x in
    g (Munshallow (i, xe)), xa

  | Mlisttocoll (i, x) ->
    let xe, xa = f accu x in
    g (Mlisttocoll (i,xe)), xa

  | Maddshallow (id, args) ->
    let ((argss, argsa) : 'c list * 'a) =
      List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) args
    in
    g (Maddshallow (id, argss)), argsa


  (* collection keys *)

  | Mtokeys (an, x) ->
    let xe, xa = f accu x in
    g (Mtokeys (an, xe)), xa

  | Mcoltokeys (an) ->
    g (Mcoltokeys (an)), accu


  (* quantifiers *)

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


  (* formula operators *)

  | Mimply (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mimply (le, re)), ra

  | Mequiv  (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mequiv (le, re)), ra


  (* formula expression*)

  | Mgetbefore (c, k) ->
    let ke, ka = f accu k in
    g (Mgetbefore (c, ke)), ka

  | Mgetat (c, d, k) ->
    let ke, ka = f accu k in
    g (Mgetat (c, d, ke)), ka

  | Mgetfrommap (an, k, c) ->
    let ke, ka = f accu k in
    let ce, ca = f ka c in
    g (Mgetfrommap (an, ke, ce)), ca

  | Mmem (an, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Mmem (an, ce, ie)), ia

  | Msubsetof (an, c, i) ->
    let ce, ca = f accu c in
    let ie, ia = f ca i in
    g (Msubsetof (an, ce, ie)), ia

  | Misempty  (l, r) ->
    let re, ra = f accu r in
    g (Misempty (l, re)), ra


  (* set api *)

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


let fold_left g l accu = List.fold_left (fun accu x -> g x accu) accu l

let fold_specification (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (v : 'id specification_gen) (accu : 'a) : 'a =
  let fold_label_term (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (lt : 'id label_term_gen) (accu : 'a) : 'a =
    let ctx = { ctx with label = Some lt.label } in
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
      | Entry fs -> f {ctx with fs = Some fs} accu fs.body
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

  val function_name_from_api_asset       : api_asset    -> ident
  val function_name_from_api_list        : api_list     -> ident
  val function_name_from_api_builtin     : api_builtin  -> ident
  val function_name_from_api_internal    : api_internal -> ident
  val get_vars                           : model -> var list
  val get_enums                          : model -> enum list
  val get_assets                         : model -> asset list
  val get_var                            : model -> ident -> var
  val get_enum                           : model -> ident -> enum
  val get_asset                          : model -> ident -> asset
  val get_storage                        : model -> storage
  val get_asset_field                    : model -> (ident * ident) -> (ident * type_ * mterm option)
  val get_asset_key                      : model -> ident -> (ident * btyp)
  val get_field_container                : model -> ident -> ident -> (ident * container)
  val is_storage_attribute               : model -> ident -> bool
  val get_named_field_list               : model -> ident -> 'a list -> (ident * 'a) list
  val get_containers                     : model -> (ident * ident * type_) list (* asset id, asset item *)
  val get_partitions                     : model -> (ident * ident * type_) list (* asset id, asset item *)
  val dest_container                     : type_ -> ident
  val get_container_asset_key            : model -> ident -> ident -> (ident * ident * btyp)
  val get_container_assets               : model -> ident -> ident list
  val get_entries                        : model -> (specification option * function_struct) list
  val get_functions                      : model -> (specification option * function_struct* type_) list
  val has_container                      : model -> ident -> bool
  val get_asset_containers               : model -> ident -> (ident * type_ * mterm option) list
  val get_field_list                     : model -> ident -> ident list
  val get_field_pos                      : model -> ident -> ident -> int (* m, asset, field *)
  val get_nth_asset_val                  : int -> mterm -> mterm
  val dest_array                         : mterm -> mterm list
  val get_asset_type                     : mterm -> ident
  val is_local_assigned                  : ident -> mterm -> bool
  val get_function_args                  : function__ -> argument list
  val set_function_args                  : function__ -> argument list -> function__
  val map_function_terms                 : (mterm -> mterm) -> function__ -> function__
  val is_asset                           : mterm -> bool
  val is_varlocal                        : mterm -> bool
  val dest_varlocal                      : mterm -> ident
  val is_container                       : type_ -> bool
  val get_key_pos                        : model -> ident -> int
  val get_loop_invariants                : model -> (ident * mterm) list -> ident -> (ident * mterm) list
  val get_formula                        : model -> mterm option -> ident -> mterm option
  val is_post                            : postcondition -> bool
  val get_sum_fields                     : model -> ident -> ident list
  val get_added_removed_sets             : model -> specification option -> mterm__node list
  val get_storage_invariants             : model -> ident option -> (ident * ident * mterm) list
  val is_field_storage                   : model -> ident -> bool
  val with_trace                         : model -> bool
  val get_callers                        : model -> ident -> ident list
  val no_fail                            : model -> ident -> ident option
  val type_to_asset                      : type_ -> ident
  val get_map_function                   : model -> (ident * ident list) list
  val retrieve_all_properties            : model -> (ident * property) list
  val retrieve_property                  : model -> ident -> property
  val get_default_value                  : model -> type_ -> mterm
  val with_operations_for_mterm          : mterm -> bool
  val with_operations                    : model -> bool
  val get_source_for                     : model -> ctx_model -> mterm -> mterm option
  val eval                               : mterm -> (ident * mterm) list -> mterm
end = struct

  open Tools
  open Location

  exception Anomaly of string

  type error_desc =
    | AssetFieldNotFound of string * string
    | AssetKeyTypeNotFound of string
    | ContainerNotFound
    | NotanArray
    | NotaRecord of mterm
    | NotanAssetType
    | NotFound
  [@@deriving show {with_path = false}]

  let emit_error (desc : error_desc) =
    let str = Format.asprintf "%a@." pp_error_desc desc in
    raise (Anomaly str)

  let lident_to_string lident = Location.unloc lident

  let function_name_from_function_node = function
    | Function (fs, _)    -> lident_to_string fs.name
    | Entry     fs        -> lident_to_string fs.name

  let function_name_from_api_asset = function
    | Get            aid       -> "get_"            ^ aid
    | Set            aid       -> "set_"            ^ aid
    | Add            aid       -> "add_"            ^ aid
    | Remove         aid       -> "remove_"         ^ aid
    | UpdateAdd     (aid, fid) -> "update_add_"     ^ aid ^ "_" ^ fid
    | UpdateRemove  (aid, fid) -> "update_remove_"  ^ aid ^ "_" ^ fid
    | ToKeys         aid       -> "to_keys_"        ^ aid
    | ColToKeys      aid       -> "col_to_keys_"    ^ aid
    | Select        (aid, _)   -> "select_"         ^ aid
    | Sort          (aid, fid) -> "sort_"           ^ aid ^ "_" ^ fid
    | Contains       aid       -> "contains_"       ^ aid
    | Nth            aid       -> "nth_"            ^ aid
    | Count          aid       -> "count_"          ^ aid
    | Sum           (aid, fid) -> "sum_"            ^ aid ^ "_" ^ fid
    | Min           (aid, fid) -> "min_"            ^ aid ^ "_" ^ fid
    | Max           (aid, fid) -> "max_"            ^ aid ^ "_" ^ fid
    | Shallow        aid       -> "shallow_"        ^ aid
    | Unshallow      aid       -> "unshallow"       ^ aid
    | Listtocoll     aid       -> "listtocoll_"     ^ aid
    | Head           aid       -> "head_"           ^ aid
    | Tail           aid       -> "tail_"           ^ aid

  let function_name_from_api_list = function
    | Lprepend  _ -> "prepend"
    | Lcontains _ -> "contains"
    | Lcount    _ -> "count"
    | Lnth      _ -> "nth"

  let function_name_from_api_builtin = function
    | MinBuiltin         _ -> "min"
    | MaxBuiltin         _ -> "max"

  let function_name_from_api_internal = function
    | RatEq                -> "rat_eq"
    | RatCmp               -> "rat_cmp"
    | RatArith             -> "rat_arith"
    | RatTez               -> "rat_to_tez"

  let get_function_args (f : function__) : argument list =
    match f.node with
    | Function (s,_) -> s.args
    | Entry s        -> s.args

  let set_function_args (f : function__) (args : argument list) : function__ =
    match f.node with
    | Function (s,t) -> { node = Function ({ s with args = args },t); spec = f.spec }
    | Entry s        -> { node = Entry { s with args = args }; spec = f.spec }

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

  let dest_array (t : mterm)  =
    match t.node with
    | Marray l -> l
    | _ -> emit_error NotanArray

  let get_nth_asset_val pos (t : mterm) =
    match t.node with
    | Masset l -> List.nth l pos
    | _ -> emit_error (NotaRecord t)

  let type_to_asset = function
    | Tasset n -> n |> unloc
    | Tcontainer (Tasset n, _) -> n |> unloc
    | _ -> emit_error NotanAssetType

  let get_asset_type (t : mterm) : ident = type_to_asset t.type_

  let is_asset (d : decl_node) : bool =
    match d with
    | Dasset _ -> true
    | _        -> false

  let dest_asset  = function
    | Dasset r -> r
    | _ -> emit_error NotFound

  let is_enum (d : decl_node) : bool =
    match d with
    | Denum _ -> true
    | _          -> false

  let dest_enum  = function
    | Denum e -> e
    | _ -> emit_error NotFound

  let is_var (d : decl_node) : bool =
    match d with
    | Dvar _ -> true
    | _      -> false

  let dest_var  = function
    | Dvar v -> v
    | _ -> emit_error NotFound

  let get_vars m   = m.decls |> List.filter is_var   |> List.map dest_var
  let get_enums m  = m.decls |> List.filter is_enum  |> List.map dest_enum
  let get_assets m = m.decls |> List.filter is_asset |> List.map dest_asset

  let get_var   m id : var   = get_vars m   |> List.find (fun (x : var)   -> cmp_ident id (unloc x.name))
  let get_enum  m id : enum  = get_enums m  |> List.find (fun (x : enum)  -> cmp_ident id (unloc x.name))
  let get_asset m id : asset = get_assets m |> List.find (fun (x : asset) -> cmp_ident id (unloc x.name))

  (* let get_partitions m : (ident * ident * type_) list=
     get_info_assets m |> List.fold_left (fun acc (info : info_asset) ->
        acc @ (List.fold_left (fun acc (i,t,_) ->
            match t with
            | Tcontainer (Tasset _, Partition) ->
              acc @ [info.name,i,t]
            | _ -> acc
          ) [] info.values)
      ) [] *)

  let rec pp_type fmt t =
    match t with
    | Tasset an ->
      Format.fprintf fmt "%a" Printer_tools.pp_id an
    | Tstate ->
      Format.fprintf fmt "state"
    | Tenum en ->
      Format.fprintf fmt "%a" Printer_tools.pp_id en
    | Tcontract cn ->
      Format.fprintf fmt "%a" Printer_tools.pp_id cn
    | Tbuiltin b -> pp_btyp fmt b
    | Tcontainer (t, c) ->
      Format.fprintf fmt "%a %a"
        pp_type t
        pp_container c
    | Toption t ->
      Format.fprintf fmt "%a option"
        pp_type_ t
    | Ttuple ts ->
      Format.fprintf fmt "%a"
        (Printer_tools.pp_list " * " pp_type) ts
    | Tassoc (k, v) ->
      Format.fprintf fmt "(%a, %a) map"
        pp_btyp k
        pp_type v
    | Tunit ->
      Format.fprintf fmt "unit"
    | Tstorage ->
      Format.fprintf fmt "storage"
    | Toperation ->
      Format.fprintf fmt "operation"
    | Tentry ->
      Format.fprintf fmt "entry"
    | Tprog _
    | Tvset _
    | Ttrace _ -> Format.fprintf fmt "todo"

  let get_containers_internal f m : (ident * ident * type_) list =
    get_assets m |> List.fold_left (fun acc (asset : asset) ->
        acc @ (List.fold_left (fun acc (v : asset_item) ->
            let t : type_ = v.original_type in
            match t with
            | _ when f t ->
              acc @ [unloc asset.name, unloc v.name, t]
            | _ -> acc
          ) [] asset.values)
      ) []

  let get_containers m : (ident * ident * type_) list =
    get_containers_internal (function | Tcontainer (Tasset _, (Partition | Collection)) -> true | _ -> false ) m

  let get_partitions m : (ident * ident * type_) list =
    get_containers_internal (function | Tcontainer (Tasset _, Partition) -> true | _ -> false ) m

  let has_container (m : model) (asset : ident) : bool =
    try
      let asset = get_asset m asset in
      List.fold_left (fun acc v ->
          match v.type_ with
          | Tcontainer (Tasset _, (Partition | Collection)) -> true
          | _ -> acc
        ) false asset.values
    with
    | Not_found -> false

  let get_asset_containers (m : model) (asset : ident) : (ident * type_ * (lident mterm_gen option)) list =
    try
      let asset = get_asset m asset in
      List.fold_left (fun acc v ->
          match v.type_ with
          | Tcontainer (Tasset _, (Partition | Collection)) -> acc @ [unloc v.name, v.type_, v.default]
          | _ -> acc
        ) [] asset.values
    with
    | Not_found -> []

  let dest_container = function
    | Tcontainer (Tasset p,(Partition | Collection)) -> unloc p
    | _ -> assert false

  let get_asset_field (m : model) (asset_name, field_name : ident * ident) : ident * type_ * mterm option =
    try
      let asset = get_asset m asset_name in
      List.find (fun (x : asset_item) -> String.equal (unloc x.name) field_name) asset.values
      |> (fun (x : asset_item) -> unloc x.name, x.type_, x.default)
    with
    | Not_found -> emit_error (AssetFieldNotFound (asset_name, field_name))

  let get_asset_key (m : model) (asset_name : ident) : (ident * btyp) =
    try
      let asset = get_asset m asset_name in
      let key_id = asset.key in
      let (_,key_typ,_) = get_asset_field m (asset_name, key_id) in
      match key_typ with
      | Tbuiltin v -> (key_id, v)
      | _ -> raise Not_found
    with
    | Not_found -> emit_error (AssetKeyTypeNotFound (asset_name))

  let get_field_container model asset_name field_name : ident * container =
    let seek_original_type () : type_ =
      try
        let asset = get_asset model asset_name in
        List.find (fun (x : asset_item) -> String.equal (unloc x.name) field_name) asset.values
        |> (fun (x : asset_item) -> x.original_type)
      with
      | Not_found -> emit_error (AssetFieldNotFound (asset_name, field_name))
    in
    let ot = seek_original_type () in
    match ot with
    | Tcontainer (Tasset an, c) -> (unloc an, c)
    | _ -> assert false

  let get_container_assets model asset : ident list =
    get_containers model
    |> List.filter (fun (a,_,_) -> String.equal asset a)
    |> List.map (fun (_,_,t) -> type_to_asset t)

  (* returns : asset name, key name, key type *)
  let get_container_asset_key model asset field : (ident * ident * btyp) =
    let containers = get_containers model in
    let rec rec_get = function
      | (r,i,t) :: _tl when String.equal r asset &&
                            String.equal i field ->
        let pa  = dest_container t in
        let k,t = get_asset_key model pa in
        (pa,k,t)
      | _ :: tl -> rec_get tl
      | _ -> emit_error (ContainerNotFound) in
    rec_get containers

  let get_storage model =
    model.storage

  let is_storage_attribute model id =
    let s = get_storage model in
    let items = s in
    (List.fold_left (fun accu (x : storage_item) ->
         accu || String.equal id (Location.unloc x.id)
       ) false items)

  let get_field_list (model : model) (asset_name : ident) : ident list =
    try
      let asset = get_asset model asset_name in
      List.map (fun (x : asset_item) -> unloc x.name) asset.values
    with
    | Not_found -> []

  let get_field_pos model asset field =
    let l = get_field_list model asset in
    let rec rec_get_pos i = function
      | e :: _tl when String.equal field e -> i
      | _ :: tl -> rec_get_pos (succ i) tl
      | [] -> assert false in
    rec_get_pos 0 l

  let get_named_field_list ast asset_name list =
    let field_list = get_field_list ast asset_name in
    List.map2 (fun x y -> x, y) field_list list

  exception FoundAssign

  let is_local_assigned (id : ident) (b : mterm) =
    let rec rec_search_assign _ (t : mterm) =
      match t.node with
      | Massign (_, _, i,_) when String.equal (unloc i) id -> raise FoundAssign
      | _ -> fold_term rec_search_assign false t in
    try rec_search_assign false b
    with FoundAssign -> true


  exception FoundOperations

  let with_operations_for_mterm_intern _ctx accu (mt : mterm) : bool =
    let rec aux accu (t : mterm) =
      match t.node with
      | Mtransfer _ -> raise FoundOperations
      | Mexternal _ -> raise FoundOperations
      | _ -> fold_term aux accu t in
    aux accu mt

  let with_operations_for_mterm (mt : mterm) : bool =
    try with_operations_for_mterm_intern () false mt
    with FoundOperations -> true

  let with_operations (model : model) : bool =
    try fold_model with_operations_for_mterm_intern model false
    with FoundOperations -> true

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
    | Mvarlocal i -> unloc i
    | _ -> assert false

  let is_container t =
    match t with
    | Tcontainer ((Tasset _),_) -> true
    | _ -> false


  let get_key_pos (m : model) (n : ident) : int =
    get_assets m |> List.fold_left (fun acc (info : asset) ->
        if String.equal n (unloc info.name) then
          let (k,_) = get_asset_key m n in
          (List.fold_left (fun acc (i : asset_item) ->
               if String.equal (unloc i.name) k then
                 succ acc
               else
                 acc
             ) acc info.values)
        else
          acc
      ) (-1)

  (* i is the loop label *)
  let get_loop_invariants m (acc : (ident * mterm) list) (i : ident) : (ident * mterm) list =
    let internal_get (ctx : ctx_model) (acc : (ident * mterm) list) t =
      match ctx.invariant_id with
      | Some v when cmp_ident i (unloc v) ->
        begin
          match ctx.spec_id with
          | Some l -> acc @ [unloc l,t]
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
    List.fold_left (fun acc (ai : api_storage) ->
        match ai.node_item with
        | APIAsset (Sum (asset,field)) when String.equal a asset ->
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
    try
      let assets : lident asset_gen list = get_assets m in
      let assets : lident asset_gen list =
        begin
          match asset_name with
          | Some asset_name -> List.filter (fun (x : lident asset_gen) -> cmp_ident (unloc x.name) asset_name) assets
          | _ -> assets
        end
      in
      assets
      |>
      List.map (fun (asset : asset) ->
          List.map (fun (lt : label_term) ->
              let inv_name = Tools.Option.fold (fun _ l -> unloc l) "" (Some lt.label) in
              let inv_term = lt.term in
              [unloc asset.name, inv_name, inv_term]
            ) asset.invariants
        )
      |> List.flatten
      |> List.flatten
    with
    | Not_found -> []

  let is_field_storage (m : model) (id : ident) : bool =
    let l : ident list = List.map (fun (x : storage_item) -> unloc x.id) m.storage in
    List.mem id l

  let with_trace (_m : model) : bool = true

  (* returns the list of entries calling the function named 'name' *)
  let get_callers (_m : model) (_name : ident) : ident list = [] (* TODO *)

  (* is there a no_fail predicate on an entry called fn ? *)
  let no_fail (m : model) (fn : ident) : ident option =
    List.fold_left (fun acc (p : security_item) ->
        match acc with
        | None ->
          begin
            match p.predicate.s_node with
            | SnoStorageFail Sany -> Some (unloc p.label)
            | SnoStorageFail (Sentry l) ->
              if l |> List.map unloc |> List.mem fn then
                Some (unloc p.label)
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
      | Dasset r -> List.map (fun (x : label_term) -> (unloc x.label, PstorageInvariant (x, unloc r.name))) r.invariants
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
      | Tbuiltin Bdate       -> Mdate (Core.mk_date ())
      | Tbuiltin Bduration   -> Mduration (Core.mk_duration ())
      | Tbuiltin Btimestamp  -> Mtimestamp (Big_int.zero_big_int)
      | Tbuiltin Bstring     -> Mstring ""
      | Tbuiltin Baddress    -> Maddress "tz1_default"
      | Tbuiltin Brole       -> Maddress "tz1_default"
      | Tbuiltin Bcurrency   -> Mcurrency (Big_int.zero_big_int, Tz)
      | Tbuiltin Bkey        -> Maddress "tz1_default"
      | Tbuiltin Bbytes      -> Mbytes "0x0"
      | Tasset asset_name    ->
        begin
          let a = get_asset m (unloc asset_name) in
          let l : mterm list =
            List.map (
              fun (v : asset_item) ->
                match v.default with
                | Some v -> v
                | _ -> get_default_value m v.type_
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
          let _, t = get_asset_key m (unloc an) in
          Tcontainer (Tbuiltin t, c)
        end
      | _ -> t
    in
    mk_mterm (aux t) tt

  let get_source_for (_m : model) (_ctx : ctx_model) (c : mterm) : mterm option =
    match c.node with
    | Mvarparam an ->
      begin
        let l, an = deloc an in
        let idparam = mkloc l (an ^ "_values") in
        Some (mk_mterm (Mvarparam idparam) (Tassoc(Bint, Tasset (dumloc "myasset"))))
      end
    | _ -> None

  let eval (mt : mterm) (map_const_value : (ident * mterm) list) : mterm =
    let get_value (id : ident) : mterm = List.assoc id map_const_value in
    let is_const (id : ident) : bool = List.assoc_opt id map_const_value |> Option.is_some in
    let remove_const (mt : mterm) : mterm =
      let rec aux (mt : mterm) : mterm =
        match mt.node with
        | Mvarstorevar v
        | Mvarlocal v when is_const (unloc v) ->
          let dv = get_value (unloc v) in
          aux dv
        | _ -> map_mterm aux mt
      in
      aux mt
    in
    let eval_expr mt : mterm =
      let extract_int (i : mterm) : Big_int.big_int =
        match i.node with
        | Mint v -> v
        | _ -> assert false
      in

      let extract_rat (rat : mterm) : Big_int.big_int * Big_int.big_int =
        match rat.node with
        | Mrational (num, denom)
        | Mtuple [{node = Mint num; _}; {node = Mint denom; _}] -> (num, denom)
        | _ -> assert false
      in

      let extract_bool (b : mterm) : bool =
        match b.node with
        | Mbool v -> v
        | _ -> assert false
      in

      let arith op (a, b) : mterm =
        let a = extract_int a in
        let b = extract_int b in

        let res =
          match op with
          | `Plus -> Big_int.add_big_int a b
          | `Minus -> Big_int.sub_big_int a b
          | `Mult -> Big_int.mult_big_int a b
          | `Div -> Big_int.div_big_int a b
          | `Modulo -> Big_int.mod_big_int a b
          | _ -> assert false
        in
        mk_mterm (Mint res) (Tbuiltin Bint)
      in

      let rec aux (mt : mterm) : mterm =
        match mt.node with
        | Mplus   (a, b) -> arith `Plus  (a, b)
        | Mminus  (a, b) -> arith `Minus (a, b)
        | Mmult   (a, b) -> arith `Mult  (a, b)
        | Mdiv    (a, b) -> arith `Div   (a, b)
        | Mmodulo (a, b) -> arith `Modulo   (a, b)
        | Mnot     a     -> mk_mterm (Mbool (not (extract_bool a))) (Tbuiltin Bbool)
        | Mand    (a, b) -> mk_mterm (Mbool ((extract_bool a) && (extract_bool b))) (Tbuiltin Bbool)
        | Mor     (a, b) -> mk_mterm (Mbool ((extract_bool a) || (extract_bool b))) (Tbuiltin Bbool)
        | Mrateq  (a, b) ->
          begin
            let num1, denom1 = extract_rat a in
            let num2, denom2 = extract_rat b in
            let res = Big_int.eq_big_int (Big_int.mult_big_int num1 denom2) (Big_int.mult_big_int num2 denom1) in
            mk_mterm (Mbool res) (Tbuiltin Bbool)
          end
        (* | Mratcmp (op, _a, _b) ->
           begin
            (* let num1, denom1 = extract_rat a in
               let num2, denom2 = extract_rat b in *)
            let res =
              begin
                match op with
                | Gt
                | Ge
                | Lt
                | Le -> false (* TODO *)
              end
            in
            mk_mterm (Mbool res) (Tbuiltin Bbool)
           end *)
        | Mrattez (coef, c) ->
          begin
            let coef = aux coef in
            let c    = aux c    in
            match coef.node, c.node with
            | Mrational (num, denom), Mcurrency (v, cur)
            | Mtuple [{node = Mint num; _}; {node = Mint denom; _}], Mcurrency (v, cur) ->
              begin
                let res = Big_int.div_big_int (Big_int.mult_big_int num v) denom in
                mk_mterm (Mcurrency (res, cur)) (Tbuiltin Bcurrency)
              end
            | _ -> assert false
          end
        | _ -> map_mterm aux mt
      in
      aux mt
    in

    mt
    |> remove_const
    |> eval_expr


end
