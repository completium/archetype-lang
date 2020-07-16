open Ident
open Tools
open Location

type lident = ident Location.loced
[@@deriving show {with_path = false}]

type currency =
  | Tz
  | Mtz
  | Utz
[@@deriving show {with_path = false}]

type container =
  | Collection
  | Aggregate
  | Partition
  | View
[@@deriving show {with_path = false}]

type btyp =
  | Bunit
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
  | Bsignature
  | Bkey
  | Bkeyhash
  | Bbytes
  | Bnat
  | Bchainid
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
  | Tlist of type_
  | Toption of type_
  | Ttuple of type_ list
  | Tset of btyp
  | Tmap of btyp * type_
  | Trecord of lident
  | Tunit
  | Tstorage
  | Toperation
  | Tentry
  | Tentrysig of type_
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

type 'id for_ident_gen =
  | FIsimple of 'id
  | FIdouble of 'id * 'id
[@@deriving show {with_path = false}]

type for_ident = lident for_ident_gen
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

type ('id, 'term) assign_kind_gen =
  | Avar         of 'id
  | Avarstore    of 'id
  | Aasset       of 'id * 'id * 'term (* asset name * field name * key *)
  | Arecord      of 'id * 'id * 'term (* record name * field name * record *)
  | Astate
  | Aassetstate of ident * 'term     (* asset name * key *)
[@@deriving show {with_path = false}]

type 'term var_kind_gen =
  | Vassetstate of 'term
  | Vstorevar
  | Vstorecol
  | Venumval
  | Vlocal
  | Vparam
  | Vfield
  | Vstate
  | Vthe
[@@deriving show {with_path = false}]

type 'term container_kind_gen =
  | CKcoll
  | CKview  of 'term
  | CKfield of (ident * ident * 'term)
[@@deriving show {with_path = false}]

type 'term iter_container_kind_gen =
  | ICKcoll  of ident
  | ICKview  of 'term
  | ICKfield of (ident * ident * 'term)
  | ICKset   of 'term
  | ICKlist  of 'term
  | ICKmap   of 'term
[@@deriving show {with_path = false}]


type ('id, 'term) mterm_node  =
  (* lambda *)
  | Mletin            of 'id list * 'term * type_ option * 'term * 'term option
  | Mdeclvar          of 'id list * type_ option * 'term
  | Mapp              of 'id * 'term list
  (* assign *)
  | Massign           of (assignment_operator * ('id, 'term) assign_kind_gen * 'term) (* assignment kind value*)
  (* control *)
  | Mif               of ('term * 'term * 'term option)
  | Mmatchwith        of 'term * ('id pattern_gen * 'term) list
  | Mfor              of ('id for_ident_gen * 'term iter_container_kind_gen * 'term * ident option)
  | Miter             of ('id * 'term * 'term * 'term * ident option)
  | Mseq              of 'term list
  | Mreturn           of 'term
  | Mlabel            of 'id
  | Mmark             of 'id * 'term
  (* effect *)
  | Mfail             of 'id fail_type_gen
  | Mtransfer         of ('term * 'term)                                   (* value * dest *)
  | Mcallcontract     of 'term  * 'term * ident * 'id * ('id * 'term) list (* value * dest  * contract_id * id * args *)
  | Mcallentry        of 'term * 'id * 'term                               (* value * entry * arg *)
  | Mcallself         of 'term * 'id * 'term list                          (* value * entry * args *)
  (* entrypoint *)
  | Mentrycontract    of 'term * 'id   (* contract * ident *)
  | Mentrypoint       of 'term * 'term (* address * string *)
  | Mself             of 'id           (* entryname *)
  (* operation *)
  | Moperations
  | Mmkoperation      of 'term * 'term * 'term  (* value * address * args *)
  (* literals *)
  | Mint              of Core.big_int
  | Mnat              of Core.big_int
  | Mbool             of bool
  | Menum             of string
  | Mrational         of Core.big_int * Core.big_int
  | Mstring           of string
  | Mcurrency         of Core.big_int * currency
  | Maddress          of string
  | Mdate             of Core.date
  | Mduration         of Core.duration
  | Mtimestamp        of Core.big_int
  | Mbytes            of string
  | Munit
  (* control expression *)
  | Mexprif           of 'term * 'term * 'term
  | Mexprmatchwith    of 'term * ('id pattern_gen * 'term) list
  (* composite type constructors *)
  | Mnone
  | Msome             of 'term
  | Mtuple            of 'term list
  | Masset            of 'term list
  | Massets           of 'term list
  | Mlitset           of 'term list
  | Mlitlist          of 'term list
  | Mlitmap           of ('term * 'term) list
  | Mlitrecord        of (ident * 'term) list
  (* access *)
  | Mdot              of 'term * 'id
  | Mdotassetfield    of 'id * 'term * 'id
  | Mdotcontract      of 'term * 'id
  | Maccestuple       of 'term * Core.big_int
  (* comparison operators *)
  | Mequal            of type_ * 'term * 'term
  | Mnequal           of type_ * 'term * 'term
  | Mgt               of 'term * 'term
  | Mge               of 'term * 'term
  | Mlt               of 'term * 'term
  | Mle               of 'term * 'term
  | Mmulticomp        of 'term * (comparison_operator * 'term) list
  (* arithmetic operators *)
  | Mand              of 'term * 'term
  | Mor               of 'term * 'term
  | Mnot              of 'term
  | Mplus             of 'term * 'term
  | Mminus            of 'term * 'term
  | Mmult             of 'term * 'term
  | Mdivrat           of 'term * 'term
  | Mdiveuc           of 'term * 'term
  | Mmodulo           of 'term * 'term
  | Muplus            of 'term
  | Muminus           of 'term
  (* asset api effect *)
  | Maddasset         of ident * 'term
  | Maddfield         of ident * ident * 'term * 'term (* asset_name * field_name * asset instance * item *)
  | Mremoveasset      of ident * 'term
  | Mremovefield      of ident * ident * 'term * 'term
  | Mremoveall        of ident * ident * 'term
  | Mremoveif         of ident * 'term container_kind_gen * (ident * type_) list * 'term * 'term list (* asset_name, view, lambda (args, body, apply_args) *)
  | Mclear            of ident * 'term container_kind_gen
  | Mset              of ident * ident list * 'term * 'term (*asset_name * field_name modified * ... *)
  | Mupdate           of ident * 'term * ('id * assignment_operator * 'term) list
  | Maddupdate        of ident * 'term container_kind_gen * 'term * ('id * assignment_operator * 'term) list
  (* asset api expression *)
  | Mget              of ident * 'term container_kind_gen * 'term
  | Mselect           of ident * 'term container_kind_gen * (ident * type_) list * 'term * 'term list (* asset_name, view, lambda (args, body, apply_args) *)
  | Msort             of ident * 'term container_kind_gen * (ident * sort_kind) list
  | Mcontains         of ident * 'term container_kind_gen * 'term
  | Mnth              of ident * 'term container_kind_gen * 'term
  | Mcount            of ident * 'term container_kind_gen
  | Msum              of ident * 'term container_kind_gen * 'term
  | Mhead             of ident * 'term container_kind_gen * 'term
  | Mtail             of ident * 'term container_kind_gen * 'term
  (* utils *)
  | Mcast             of type_ * type_ * 'term
  | Mtupleaccess      of 'term * Core.big_int
  (* set api expression *)
  | Msetadd           of type_ * 'term * 'term
  | Msetremove        of type_ * 'term * 'term
  | Msetcontains      of type_ * 'term * 'term
  | Msetlength        of type_ * 'term
  (* list api expression *)
  | Mlistprepend      of type_ * 'term * 'term
  | Mlistcontains     of type_ * 'term * 'term
  | Mlistlength       of type_ * 'term
  | Mlistnth          of type_ * 'term * 'term
  (* map api expression *)
  | Mmapput           of type_ * type_ * 'term * 'term * 'term
  | Mmapremove        of type_ * type_ * 'term * 'term
  | Mmapget           of type_ * type_ * 'term * 'term
  | Mmapgetopt        of type_ * type_ * 'term * 'term
  | Mmapcontains      of type_ * type_ * 'term * 'term
  | Mmaplength        of type_ * type_ * 'term
  (* builtin functions *)
  | Mmin              of 'term * 'term
  | Mmax              of 'term * 'term
  | Mabs              of 'term
  | Mconcat           of 'term * 'term
  | Mslice            of 'term * 'term * 'term
  | Mlength           of 'term
  | Misnone           of 'term
  | Missome           of 'term
  | Mgetopt           of 'term
  | Mfloor            of 'term
  | Mceil             of 'term
  | Mpack             of 'term
  | Munpack           of  type_ * 'term
  (* crypto functions *)
  | Mblake2b          of 'term
  | Msha256           of 'term
  | Msha512           of 'term
  | Mhashkey          of 'term
  | Mchecksignature   of 'term * 'term * 'term
  (* constants *)
  | Mnow
  | Mtransferred
  | Mcaller
  | Mbalance
  | Msource
  | Mselfaddress
  | Mchainid
  (* variable *)
  | Mvar              of 'id * 'term var_kind_gen
  (* rational *)
  | Mrateq            of 'term * 'term
  | Mratcmp           of comparison_operator * 'term * 'term
  | Mratarith         of rat_arith_op * 'term * 'term
  | Mratuminus        of 'term
  | Mrattez           of 'term * 'term
  | Mdivtez           of 'term * 'term
  | Minttorat         of 'term
  | Mratdur           of 'term * 'term
  (* functional *)
  | Mfold             of ('id * 'id list * 'term * 'term) (* ident list * collection * body *)
  (* imperative *)
  | Mbreak
  (* quantifiers *)
  | Mforall           of 'id * type_ * 'term option * 'term
  | Mexists           of 'id * type_ * 'term option * 'term
  (* formula operators *)
  | Mimply            of 'term * 'term
  | Mequiv            of 'term * 'term
  (* formula asset collection *)
  | Msetbefore        of 'term
  | Msetat            of ident * 'term
  | Msetunmoved       of 'term
  | Msetadded         of 'term
  | Msetremoved       of 'term
  | Msetiterated      of 'term iter_container_kind_gen
  | Msettoiterate     of 'term iter_container_kind_gen
  (* formula asset collection methods *)
  | Mempty            of ident
  | Msingleton        of ident * 'term
  | Msubsetof         of ident * 'term container_kind_gen * 'term
  | Misempty          of ident * 'term
  | Munion            of ident * 'term * 'term
  | Minter            of ident * 'term * 'term
  | Mdiff             of ident * 'term * 'term
[@@deriving show {with_path = false}]

and assign_kind = (lident, mterm) assign_kind_gen

and var_kind = mterm var_kind_gen

and container_kind = mterm container_kind_gen

and iter_container_kind = mterm iter_container_kind_gen

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

and api_container_kind =
  | Coll
  | View
  | Field of ident * ident

and api_asset =
  | Get              of ident
  | Set              of ident
  | Add              of ident
  | Remove           of ident
  | Clear            of ident * api_container_kind
  | Update           of ident * (ident * assignment_operator * mterm) list
  | FieldAdd         of ident * ident
  | FieldRemove      of ident * ident
  | RemoveAll        of ident * ident
  | RemoveIf         of ident * api_container_kind * (ident * type_) list * mterm
  | Contains         of ident * api_container_kind
  | Nth              of ident * api_container_kind
  | Select           of ident * api_container_kind * (ident * type_) list * mterm
  | Sort             of ident * api_container_kind * (ident * sort_kind) list
  | Count            of ident * api_container_kind
  | Sum              of ident * api_container_kind * type_ * mterm
  | Head             of ident * api_container_kind
  | Tail             of ident * api_container_kind
[@@deriving show {with_path = false}]

and api_list =
  | Lprepend         of type_
  | Lcontains        of type_
  | Llength          of type_
  | Lnth             of type_
[@@deriving show {with_path = false}]

and api_builtin =
  | Bmin    of type_
  | Bmax    of type_
  | Babs    of type_
  | Bconcat of type_
  | Bslice  of type_
  | Blength of type_
  | Bisnone of type_
  | Bissome of type_
  | Bgetopt of type_
  | Bfloor
  | Bceil
[@@deriving show {with_path = false}]

and api_internal =
  | RatEq
  | RatCmp
  | RatArith
  | RatUminus
  | RatTez
  | DivTez
  | RatDur
[@@deriving show {with_path = false}]

and api_storage_node =
  | APIAsset      of api_asset
  | APIList       of api_list
  | APIBuiltin    of api_builtin
  | APIInternal   of api_internal
[@@deriving show {with_path = false}]

and api_loc =
  | OnlyFormula
  | OnlyExec
  | ExecFormula

and api_storage = {
  node_item: api_storage_node;
  api_loc: api_loc;
}
[@@deriving show {with_path = false}]

and api_verif =
  | StorageInvariant of (ident * ident * mterm)

and entry_description =
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

and security_entry =
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
  loc: Location.t [@opaque];
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
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type asset_item = lident asset_item_gen
[@@deriving show {with_path = false}]

type 'id asset_gen = {
  name: 'id;
  values: 'id asset_item_gen list;
  key: ident;
  sort: ident list;
  state: lident option;
  invariants  : lident label_term_gen list;
  init: 'id mterm_gen list;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type asset = lident asset_gen
[@@deriving show {with_path = false}]

type 'id record_field_gen = {
  name: 'id;
  type_: type_;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type record_field = lident record_field_gen
[@@deriving show {with_path = false}]

type 'id record_gen = {
  name: 'id;
  fields: 'id record_field_gen list;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type record = lident record_gen
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
  | SonlyByRole         of entry_description * security_role list
  | SonlyInEntry        of entry_description * security_entry
  | SonlyByRoleInEntry  of entry_description * security_role list * security_entry
  | SnotByRole          of entry_description * security_role list
  | SnotInEntry         of entry_description * security_entry
  | SnotByRoleInEntry   of entry_description * security_role list * security_entry
  | StransferredBy      of entry_description
  | StransferredTo      of entry_description
  | SnoStorageFail      of security_entry
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
  | Drecord of 'id record_gen
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
  loc           : Location.t [@opaque];
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

let mk_asset ?(values = []) ?(sort=[]) ?state ?(invariants = []) ?(init = []) ?(loc = Location.dummy) name key : 'id asset_gen =
  { name; values; sort; state; key; invariants; init; loc }

let mk_asset_item ?default ?(shadow=false) ?(loc = Location.dummy) name type_ original_type : 'id asset_item_gen =
  { name; type_; original_type; default; shadow; loc }

let mk_record ?(fields = []) ?(loc = Location.dummy) name : 'id record_gen =
  { name; fields; loc }

let mk_record_field ?(loc = Location.dummy) name type_ : 'id record_field_gen =
  { name; type_; loc }

let mk_contract_signature ?(args=[]) ?(loc=Location.dummy) name : 'id contract_signature_gen =
  { name; args; loc }

let mk_contract ?(signatures=[]) ?init ?(loc=Location.dummy) name : 'id contract_gen =
  { name; signatures; init; loc }

let mk_storage_item ?(const=false) ?(ghost = false) ?(loc = Location.dummy) id model_type typ default : 'id storage_item_gen =
  { id; model_type; typ; const; ghost; default; loc }

let mk_function_struct ?(args = []) ?(loc = Location.dummy) name body : function_struct =
  { name; args; body; loc }

let mk_function ?spec node : 'id function__gen =
  { node; spec }

let mk_signature ?(args = []) ?ret name : 'id signature_gen =
  { name; args; ret }

let mk_api_item node_item api_loc =
  { node_item; api_loc }

let mk_model ?(api_items = []) ?(api_verif = []) ?(decls = []) ?(functions = []) ?(storage = []) ?(specification = mk_specification ()) ?(security = mk_security ()) ?(loc = Location.dummy) name : model =
  { name; api_items; api_verif; storage; decls; functions; specification; security; loc }

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
let cmp_entry_description (ad1 : entry_description) (ad2 : entry_description) : bool = ad1 = ad2
let cmp_security_role = cmp_lident
let cmp_security_entry s1 s2 =
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
  | Tlist t1, Tlist t2                       -> cmp_type t1 t2
  | Toption t1, Toption t2                   -> cmp_type t1 t2
  | Ttuple l1, Ttuple l2                     -> List.for_all2 cmp_type l1 l2
  | Tunit, Tunit                             -> true
  | Tentry, Tentry                           -> true
  | Tentrysig t1, Tentrysig t2               -> cmp_type t1 t2
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

let cmp_for_ident
    (cmpi  : 'id -> 'id -> bool)
    (fi1 : 'id for_ident_gen)
    (fi2 : 'id for_ident_gen)
  : bool =
  match fi1, fi2 with
  | FIsimple i1, FIsimple i2 -> cmpi i1 i2
  | FIdouble (x1, y1), FIdouble (x2, y2) -> cmpi x1 x2 && cmpi y1 y2
  | _ -> false

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
  let cmp_assign_kind (lhs : assign_kind) (rhs : assign_kind) : bool =
    match lhs, rhs with
    | Avar id1, Avar id2                             -> cmpi id1 id2
    | Avarstore id1, Avarstore id2                   -> cmpi id1 id2
    | Aasset (an1, fn1, k1), Aasset (an2, fn2, k2)   -> cmpi an1 an2 && cmpi fn1 fn2 && cmp k1 k2
    | Arecord (rn1, fn1, r1), Arecord (rn2, fn2, r2) -> cmpi rn1 rn2 && cmpi fn1 fn2 && cmp r1 r2
    | Astate, Astate                                 -> true
    | Aassetstate (id1, v1), Aassetstate (id2, v2)   -> cmp_ident id1 id2 && cmp v1 v2
    | _ -> false
  in
  let cmp_var_kind (lhs : var_kind) (rhs : var_kind) : bool =
    match lhs, rhs with
    | Vassetstate v1, Vassetstate v2 -> cmp v1 v2
    | Vstorevar, Vstorevar
    | Vstorecol, Vstorecol
    | Venumval, Venumval
    | Vlocal, Vlocal
    | Vparam, Vparam
    | Vfield, Vfield
    | Vstate, Vstate
    | Vthe, Vthe -> true
    | _ -> false
  in
  let cmp_container_kind (lhs : container_kind) (rhs : container_kind) : bool =
    match lhs, rhs with
    | CKcoll, CKcoll -> true
    | CKview l, CKview r -> cmp l r
    | _ -> false
  in
  let cmp_iter_container_kind (lhs : iter_container_kind) (rhs : iter_container_kind) : bool =
    match lhs, rhs with
    | ICKcoll an1, ICKcoll an2 -> String.equal an1 an2
    | ICKview l, ICKview r -> cmp l r
    | ICKfield (an1, fn1, l1), ICKfield (an2, fn2, l2) -> String.equal an1 an2 && String.equal fn1 fn2 && cmp l1 l2
    | ICKset l,  ICKset r -> cmp l r
    | ICKlist l, ICKlist r -> cmp l r
    | ICKmap l, ICKmap r -> cmp l r
    | _ -> false
  in
  try
    match term1, term2 with
    (* lambda *)
    | Mletin (i1, a1, t1, b1, o1), Mletin (i2, a2, t2, b2, o2)                         -> List.for_all2 cmpi i1 i2 && cmp a1 a2 && Option.cmp cmp_type t1 t2 && cmp b1 b2 && Option.cmp cmp o1 o2
    | Mdeclvar (i1, t1, v1), Mdeclvar (i2, t2, v2)                                     -> List.for_all2 cmpi i1 i2 && Option.cmp cmp_type t1 t2 && cmp v1 v2
    | Mapp (e1, args1), Mapp (e2, args2)                                               -> cmpi e1 e2 && List.for_all2 cmp args1 args2
    (* assign *)
    | Massign (op1, k1, v1), Massign (op2, k2, v2)                                     -> cmp_assign_op op1 op2 && cmp_assign_kind k1 k2 && cmp v1 v2
    (* control *)
    | Mif (c1, t1, e1), Mif (c2, t2, e2)                                               -> cmp c1 c2 && cmp t1 t2 && Option.cmp cmp e1 e2
    | Mmatchwith (e1, l1), Mmatchwith (e2, l2)                                         -> cmp e1 e2 && List.for_all2 (fun (p1, t1) (p2, t2) -> cmp_pattern p1 p2 && cmp t1 t2) l1 l2
    | Mfor (i1, c1, b1, lbl1), Mfor (i2, c2, b2, lbl2)                                 -> cmp_for_ident cmpi i1 i2 && cmp_iter_container_kind c1 c2 && cmp b1 b2 && Option.cmp cmp_ident lbl1 lbl2
    | Miter (i1, a1, b1, c1, lbl1), Miter (i2, a2, b2, c2, lbl2)                       -> cmpi i1 i2 && cmp a1 a2 && cmp b1 b2 && cmp c1 c2 && Option.cmp cmp_ident lbl1 lbl2
    | Mseq is1, Mseq is2                                                               -> List.for_all2 cmp is1 is2
    | Mreturn x1, Mreturn x2                                                           -> cmp x1 x2
    | Mlabel i1, Mlabel i2                                                             -> cmpi i1 i2
    | Mmark (i1, x1), Mmark (i2, x2)                                                   -> cmpi i1 i2 && cmp x1 x2
    (* effect *)
    | Mfail ft1, Mfail ft2                                                             -> cmp_fail_type cmp ft1 ft2
    | Mtransfer (v1, d1), Mtransfer (v2, d2)                                           -> cmp v1 v2 && cmp d1 d2
    | Mcallcontract(v1, d1, t1, func1, args1), Mcallcontract(v2, d2, t2, func2, args2) -> cmp v1 v2 && cmp d1 d2 && cmp_ident t1 t2 && cmpi func1 func2 && List.for_all2 (fun (id1, t1) (id2, t2) -> cmpi id1 id2 && cmp t1 t2) args1 args2
    | Mcallentry (v1, e1, arg1), Mcallentry(v2, e2, arg2)                              -> cmp v1 v2 && cmpi e1 e2 && cmp arg1 arg2
    | Mcallself (v1, e1, args1), Mcallself(v2, e2, args2)                              -> cmp v1 v2 && cmpi e1 e2 && List.for_all2 cmp args1 args2
    (* entrypoint *)
    | Mentrycontract (c1, id1), Mentrycontract (c2, id2)                               -> cmp c1 c2 && cmpi id1 id2
    | Mentrypoint (a1, s1), Mentrypoint (a2, s2)                                       -> cmp a1 a2 && cmp s1 s2
    | Mself id1, Mself id2                                                             -> cmpi id1 id2
    (* operation *)
    | Moperations, Moperations                                                         -> true
    | Mmkoperation (v1, d1, a1), Mmkoperation (v2, d2, a2)                             -> cmp v1 v2 && cmp d1 d2 && cmp a1 a2
    (* literals *)
    | Mint v1, Mint v2                                                                 -> Big_int.eq_big_int v1 v2
    | Mnat v1, Mnat v2                                                                 -> Big_int.eq_big_int v1 v2
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
    | Munit, Munit                                                                     -> true
    (* control expression *)
    | Mexprif (c1, t1, e1), Mexprif (c2, t2, e2)                                       -> cmp c1 c2 && cmp t1 t2 && cmp e1 e2
    | Mexprmatchwith (e1, l1), Mexprmatchwith (e2, l2)                                 -> cmp e1 e2 && List.for_all2 (fun (p1, t1) (p2, t2) -> cmp_pattern p1 p2 && cmp t1 t2) l1 l2
    (* composite type constructors *)
    | Mnone, Mnone                                                                     -> true
    | Msome v1, Msome v2                                                               -> cmp v1 v2
    | Mtuple l1, Mtuple l2                                                             -> List.for_all2 cmp l1 l2
    | Masset l1, Masset l2                                                             -> List.for_all2 cmp l1 l2
    | Massets l1, Massets l2                                                           -> List.for_all2 cmp l1 l2
    | Mlitset l1, Mlitset l2                                                           -> List.for_all2 cmp l1 l2
    | Mlitlist l1, Mlitlist l2                                                         -> List.for_all2 cmp l1 l2
    | Mlitmap l1, Mlitmap l2                                                           -> List.for_all2 (fun (k1, v1) (k2, v2) -> (cmp k1 k2 && cmp v1 v2)) l1 l2
    | Mlitrecord l1, Mlitrecord l2                                                     -> List.for_all2 (fun (i1, v1) (i2, v2) -> (cmp_ident i1 i2 && cmp v1 v2)) l1 l2
    (* access *)
    | Mdot (e1, i1), Mdot (e2, i2)                                                     -> cmp e1 e2 && cmpi i1 i2
    | Mdotassetfield (an1, k1, fn1), Mdotassetfield (an2, k2, fn2)                     -> cmpi an1 an2 && cmp k1 k2 && cmpi fn1 fn2
    | Mdotcontract (e1, i1), Mdotcontract (e2, i2)                                     -> cmp e1 e2 && cmpi i1 i2
    | Maccestuple (e1, i1), Maccestuple (e2, i2)                                       -> cmp e1 e2 && Big_int.eq_big_int i1 i2
    (* comparison operators *)
    | Mequal (t1, l1, r1), Mequal (t2, l2, r2)                                         -> cmp_type t1 t2 && cmp l1 l2 && cmp r1 r2
    | Mnequal (t1, l1, r1), Mnequal (t2, l2, r2)                                       -> cmp_type t1 t2 && cmp l1 l2 && cmp r1 r2
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
    | Mdivrat (l1, r1), Mdivrat (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Mdiveuc (l1, r1), Mdiveuc (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Mmodulo (l1, r1), Mmodulo (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Muplus e1, Muplus e2                                                             -> cmp e1 e2
    | Muminus e1, Muminus e2                                                           -> cmp e1 e2
    (* asset api effect *)
    | Maddasset (an1, i1), Maddasset (an2, i2)                                         -> cmp_ident an1 an2 && cmp i1 i2
    | Maddfield (an1, fn1, c1, i1), Maddfield (an2, fn2, c2, i2)                       -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp c1 c2 && cmp i1 i2
    | Mremoveasset (an1, i1), Mremoveasset (an2, i2)                                   -> cmp_ident an1 an2 && cmp i1 i2
    | Mremovefield (an1, fn1, c1, i1), Mremovefield (an2, fn2, c2, i2)                 -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp c1 c2 && cmp i1 i2
    | Mremoveall (an1, fn1, a1), Mremoveall (an2, fn2, a2)                             -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp a1 a2
    | Mremoveif (an1, c1, la1, lb1, a1), Mremoveif (an2, c2, la2, lb2, a2)             -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && List.for_all2 (fun (i1, t1) (i2, t2) -> cmp_ident i1 i2 && cmp_type t1 t2) la1 la2 && cmp lb1 lb2 && List.for_all2 cmp a1 a2
    | Mclear (an1, v1), Mclear (an2, v2)                                               -> cmp_ident an1 an2 && cmp_container_kind v1 v2
    | Mset (c1, l1, k1, v1), Mset (c2, l2, k2, v2)                                     -> cmp_ident c1 c2 && List.for_all2 cmp_ident l1 l2 && cmp k1 k2 && cmp v1 v2
    | Mupdate (an1, k1, l1), Mupdate (an2, k2, l2)                                     -> cmp_ident an1 an2 && cmp k1 k2 && List.for_all2 (fun (id1, op1, v1) (id2, op2, v2) -> cmpi id1 id2 && cmp_assign_op op1 op2 && cmp v1 v2) l1 l2
    | Maddupdate (an1, c1, k1, l1), Maddupdate (an2, c2, k2, l2)                       -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp k1 k2 && List.for_all2 (fun (id1, op1, v1) (id2, op2, v2) -> cmpi id1 id2 && cmp_assign_op op1 op2 && cmp v1 v2) l1 l2
    (* asset api expression *)
    | Mget (an1, c1, k1), Mget (an2, c2, k2)                                           -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp k1 k2
    | Mselect (an1, c1, la1, lb1, a1), Mselect (an2, c2, la2, lb2, a2)                 -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && List.for_all2 (fun (i1, t1) (i2, t2) -> cmp_ident i1 i2 && cmp_type t1 t2) la1 la2 && cmp lb1 lb2 && List.for_all2 cmp a1 a2
    | Msort (an1, c1, l1), Msort (an2, c2, l2)                                         -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && List.for_all2 (fun (fn1, k1) (fn2, k2) -> cmp_ident fn1 fn2 && k1 = k2) l1 l2
    | Mcontains (an1, c1, i1), Mcontains (an2, c2, i2)                                 -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp i1 i2
    | Mnth (an1, c1, i1), Mnth (an2, c2, i2)                                           -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp i1 i2
    | Mcount (an1, c1), Mcount (an2, c2)                                               -> cmp_ident an1 an2 && cmp_container_kind c1 c2
    | Msum (an1, c1, p1), Msum (an2, c2, p2)                                           -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp p1 p2
    | Mhead (an1, c1, i1), Mhead (an2, c2, i2)                                         -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp i1 i2
    | Mtail (an1, c1, i1), Mtail (an2, c2, i2)                                         -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp i1 i2
    (* utils *)
    | Mcast (src1, dst1, v1), Mcast (src2, dst2, v2)                                   -> cmp_type src1 src2 && cmp_type dst1 dst2 && cmp v1 v2
    | Mtupleaccess (x1, k1), Mtupleaccess (x2, k2)                                     -> cmp x1 x2 && Big_int.eq_big_int k1 k2
    (* set api expression *)
    | Msetadd (t1, c1, a1), Msetadd (t2, c2, a2)                                       -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Msetremove (t1, c1, a1), Msetremove (t2, c2, a2)                                 -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Msetcontains (t1, c1, a1), Msetcontains (t2, c2, a2)                             -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Msetlength (t1, c1), Msetlength (t2, c2)                                         -> cmp_type t1 t2 && cmp c1 c2
    (* list api expression *)
    | Mlistprepend (t1, c1, a1), Mlistprepend (t2, c2, a2)                             -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Mlistcontains (t1, c1, a1), Mlistcontains (t2, c2, a2)                           -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Mlistlength (t1, c1), Mlistlength (t2, c2)                                       -> cmp_type t1 t2 && cmp c1 c2
    | Mlistnth (t1, c1, a1), Mlistnth (t2, c2, a2)                                     -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    (* map api expression *)
    | Mmapput (tk1, tv1, c1, k1, v1), Mmapput (tk2, tv2, c2, k2, v2)                   -> cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp c1 c2 && cmp k1 k2 && cmp v1 v2
    | Mmapremove (tk1, tv1, c1, k1), Mmapremove (tk2, tv2, c2, k2)                     -> cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp c1 c2 && cmp k1 k2
    | Mmapget (tk1, tv1, c1, k1), Mmapget (tk2, tv2, c2, k2)                           -> cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp c1 c2 && cmp k1 k2
    | Mmapgetopt (tk1, tv1, c1, k1), Mmapgetopt (tk2, tv2, c2, k2)                     -> cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp c1 c2 && cmp k1 k2
    | Mmapcontains (tk1, tv1, c1, k1), Mmapcontains (tk2, tv2, c2, k2)                 -> cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp c1 c2 && cmp k1 k2
    | Mmaplength (tk1, tv1, c1), Mmaplength (tk2, tv2, c2)                             -> cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp c1 c2
    (* builtin functions *)
    | Mmin (l1, r1), Mmin (l2, r2)                                                     -> cmp l1 l2 && cmp r1 r2
    | Mmax (l1, r1), Mmax (l2, r2)                                                     -> cmp l1 l2 && cmp r1 r2
    | Mabs a1, Mabs a2                                                                 -> cmp a1 a2
    | Mconcat (x1, y1), Mconcat (x2, y2)                                               -> cmp x1 x2 && cmp y1 y2
    | Mslice (x1, s1, e1), Mslice (x2, s2, e2)                                         -> cmp x1 x2 && cmp s1 s2 && cmp e1 e2
    | Mlength x1, Mlength x2                                                           -> cmp x1 x2
    | Misnone x1, Misnone x2                                                           -> cmp x1 x2
    | Missome x1, Missome x2                                                           -> cmp x1 x2
    | Mgetopt x1, Mgetopt x2                                                           -> cmp x1 x2
    | Mfloor x1, Mfloor x2                                                             -> cmp x1 x2
    | Mceil x1, Mceil x2                                                               -> cmp x1 x2
    | Mpack x1, Mpack x2                                                               -> cmp x1 x2
    | Munpack (t1, x1), Munpack (t2, x2)                                               -> cmp_type t1 t2 && cmp x1 x2
    (* crypto functions *)
    | Mblake2b x1, Mblake2b x2                                                         -> cmp x1 x2
    | Msha256  x1, Msha256  x2                                                         -> cmp x1 x2
    | Msha512  x1, Msha512  x2                                                         -> cmp x1 x2
    | Mhashkey x1, Mhashkey  x2                                                        -> cmp x1 x2
    | Mchecksignature (k1, s1, x1), Mchecksignature (k2, s2, x2)                       -> cmp k1 k2 && cmp s1 s2 && cmp x1 x2
    (* constants *)
    | Mnow, Mnow                                                                       -> true
    | Mtransferred, Mtransferred                                                       -> true
    | Mcaller, Mcaller                                                                 -> true
    | Mbalance, Mbalance                                                               -> true
    | Msource, Msource                                                                 -> true
    | Mselfaddress, Mselfaddress                                                       -> true
    | Mchainid, Mchainid                                                               -> true
    (* variable *)
    | Mvar (id1, k1), Mvar (id2, k2)                                                   -> cmpi id1 id2 && cmp_var_kind k1 k2
    (* rational *)
    | Mrateq (l1, r1), Mrateq (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Mratcmp (op1, l1, r1), Mratcmp (op2, l2, r2)                                     -> cmp_comparison_operator op1 op2 && cmp l1 l2 && cmp r1 r2
    | Mratarith (op1, l1, r1), Mratarith (op2, l2, r2)                                 -> cmp_rat_arith_op op1 op2 && cmp l1 l2 && cmp r1 r2
    | Mratuminus v1, Mratuminus v2                                                     -> cmp v1 v2
    | Mrattez (c1, t1), Mrattez (c2, t2)                                               -> cmp c1 c2 && cmp t1 t2
    | Mdivtez (c1, t1), Mdivtez (c2, t2)                                               -> cmp c1 c2 && cmp t1 t2
    | Minttorat e1, Minttorat e2                                                       -> cmp e1 e2
    | Mratdur (c1, t1), Mratdur (c2, t2)                                               -> cmp c1 c2 && cmp t1 t2
    (* functional *)
    | Mfold (i1, is1, c1, b1), Mfold (i2, is2, c2, b2)                                 -> cmpi i1 i2 && List.for_all2 cmpi is1 is2 && cmp c1 c2 && cmp b1 b2
    (* imperative *)
    | Mbreak, Mbreak                                                                   -> true
    (* quantifiers *)
    | Mforall (i1, t1, t2, e1), Mforall (i2, t3, t4, e2)                               -> cmpi i1 i2 && cmp_type t1 t3 && Option.cmp cmp t2 t4 && cmp e1 e2
    | Mexists (i1, t1, t2, e1), Mforall (i2, t3, t4, e2)                               -> cmpi i1 i2 && cmp_type t1 t3 && Option.cmp cmp t2 t4 && cmp e1 e2
    (* formula operators *)
    | Mimply (l1, r1), Mimply (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Mequiv (l1, r1), Mequiv (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    (* formula asset collection *)
    | Msetbefore e1, Msetbefore e2                                                     -> cmp e1 e2
    | Msetat (lbl1, e1), Msetat (lbl2, e2)                                             -> cmp_ident lbl1 lbl2 && cmp e1 e2
    | Msetunmoved e1, Msetunmoved e2                                                   -> cmp e1 e2
    | Msetadded e1, Msetadded e2                                                       -> cmp e1 e2
    | Msetremoved e1, Msetremoved   e2                                                 -> cmp e1 e2
    | Msetiterated e1, Msetiterated  e2                                                -> cmp_iter_container_kind e1 e2
    | Msettoiterate e1, Msettoiterate e2                                               -> cmp_iter_container_kind e1 e2
    (* formula asset collection methods *)
    | Mempty an1, Mempty an2                                                           -> cmp_ident an1 an2
    | Msingleton (an1, k1), Msingleton (an2, k2)                                       -> cmp_ident an1 an2 && cmp k1 k2
    | Msubsetof (an1, c1, i1), Msubsetof (an2, c2, i2)                                 -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp i1 i2
    | Misempty (an1, r1), Misempty (an2, r2)                                           -> cmp_ident an1 an2 && cmp r1 r2
    | Munion (an1, l1, r1), Munion (an2, l2, r2)                                       -> cmp_ident an1 an2 && cmp l1 l2 && cmp r1 r2
    | Minter (an1, l1, r1), Minter (an2, l2, r2)                                       -> cmp_ident an1 an2 && cmp l1 l2 && cmp r1 r2
    | Mdiff (an1, l1, r1), Mdiff (an2, l2, r2)                                         -> cmp_ident an1 an2 && cmp l1 l2 && cmp r1 r2
    (* *)
    | _ -> false
  with
    _ -> false

let rec cmp_mterm (term1 : mterm) (term2 : mterm) : bool =
  cmp_mterm_node cmp_mterm cmp_lident term1.node term2.node

let cmp_container_kind lhs rhs =
  match lhs, rhs with
  | Coll, Coll
  | View, View -> true
  | Field (an1, fn1), Field (an2, fn2) -> cmp_ident an1 an2 && cmp_ident fn1 fn2
  | _ -> false

let cmp_api_item_node (a1 : api_storage_node) (a2 : api_storage_node) : bool =
  let cmp_api_asset (s1 : api_asset) (s2 : api_asset) : bool =
    match s1, s2 with
    | Get an1, Get an2                                       -> cmp_ident an1 an2
    | Set an1 , Set an2                                      -> cmp_ident an1 an2
    | Add an1 , Add an2                                      -> cmp_ident an1 an2
    | Remove an1, Remove an2                                 -> cmp_ident an1 an2
    | Clear (an1, c1), Clear (an2, c2)                       -> cmp_ident an1 an2 && cmp_container_kind c1 c2
    | Update (an1, l1), Update (an2, l2)                     -> cmp_ident an1 an2 && List.for_all2 (fun (i1, op1, v1) (i2, op2, v2) -> cmp_ident i1 i2 && cmp_assign_op op1 op2 && cmp_mterm v1 v2) l1 l2
    | FieldAdd (an1, fn1), FieldAdd (an2, fn2)               -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | FieldRemove (an1, fn1), FieldRemove (an2, fn2)         -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | RemoveAll (an1, fn1), RemoveAll (an2, fn2)             -> cmp_ident an1 an2 && cmp_ident fn1 fn2
    | RemoveIf (an1, c1, l1, p1), RemoveIf (an2, c2, l2, p2) -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && List.for_all2 (fun (i1, t1) (i2, t2) -> cmp_ident i1 i2 && cmp_type t1 t2) l1 l2 && cmp_mterm p1 p2
    | Contains (an1, c1), Contains (an2, c2)                 -> cmp_ident an1 an2 && cmp_container_kind c1 c2
    | Nth (an1, c1), Nth (an2, c2)                           -> cmp_ident an1 an2 && cmp_container_kind c1 c2
    | Select (an1, c1, l1, p1), Select (an2, c2, l2, p2)     -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && List.for_all2 (fun (i1, t1) (i2, t2) -> cmp_ident i1 i2 && cmp_type t1 t2) l1 l2 && cmp_mterm p1 p2
    | Sort (an1, c1, l1), Sort (an2, c2, l2)                 -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && List.for_all2 (fun (fn1, k1) (fn2, k2) -> cmp_ident fn1 fn2 && k1 = k2) l1 l2
    | Count (an1, c1), Count (an2, c2)                       -> cmp_ident an1 an2 && cmp_container_kind c1 c2
    | Sum (an1, c1, t1, p1), Sum (an2, c2, t2, p2)           -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp_type t1 t2 && cmp_mterm p1 p2
    | Head (an1, c1), Head (an2, c2)                         -> cmp_ident an1 an2 && cmp_container_kind c1 c2
    | Tail (an1, c1), Tail (an2, c2)                         -> cmp_ident an1 an2 && cmp_container_kind c1 c2
    | _ -> false
  in

  let cmp_api_list (c1 : api_list) (c2 : api_list) : bool =
    match c1, c2 with
    | Lprepend  t1, Lprepend  t2 -> cmp_type t1 t2
    | Lcontains t1, Lcontains t2 -> cmp_type t1 t2
    | Llength   t1, Llength   t2 -> cmp_type t1 t2
    | Lnth      t1, Lnth      t2 -> cmp_type t1 t2
    | _ -> false
  in
  let cmp_api_builtin (b1 : api_builtin) (b2 : api_builtin) : bool =
    match b1, b2 with
    | Bmin    t1, Bmin    t2 -> cmp_type t1 t2
    | Bmax    t1, Bmax    t2 -> cmp_type t1 t2
    | Babs    t1, Babs    t2 -> cmp_type t1 t2
    | Bconcat t1, Bconcat t2 -> cmp_type t1 t2
    | Bslice  t1, Bslice  t2 -> cmp_type t1 t2
    | Blength t1, Blength t2 -> cmp_type t1 t2
    | Bisnone t1, Bisnone t2 -> cmp_type t1 t2
    | Bissome t1, Bissome t2 -> cmp_type t1 t2
    | Bgetopt t1, Bgetopt t2 -> cmp_type t1 t2
    | Bfloor    , Bfloor     -> true
    | Bceil     , Bceil      -> true
    | _ -> false
  in
  let cmp_api_internal (i1 : api_internal) (i2 : api_internal) : bool =
    match i1, i2 with
    | RatEq,     RatEq     -> true
    | RatCmp,    RatCmp    -> true
    | RatArith,  RatArith  -> true
    | RatUminus, RatUminus -> true
    | RatTez,    RatTez    -> true
    | DivTez,    DivTez    -> true
    | RatDur,    RatDur    -> true
    | _ -> false
  in
  match a1, a2 with
  | APIAsset s1,    APIAsset s2    -> cmp_api_asset s1 s2
  | APIList c1,     APIList c2     -> cmp_api_list c1 c2
  | APIBuiltin f1,  APIBuiltin f2  -> cmp_api_builtin f1 f2
  | APIInternal i1, APIInternal i2 -> cmp_api_internal i1 i2
  | _ -> false

let cmp_api_loc x1 x2 =
  match x1, x2 with
  | OnlyFormula, OnlyFormula
  | OnlyExec, OnlyExec
  | ExecFormula, ExecFormula -> true
  | _, _ -> false

let cmp_api_storage (c1 : api_storage) (c2 : api_storage) =
  cmp_api_item_node c1.node_item c2.node_item && cmp_api_loc c1.api_loc c2.api_loc

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
  | Tlist t           -> Tlist (f t)
  | Toption t         -> Toption (f t)
  | Ttuple l          -> Ttuple (List.map f l)
  | Tset k            -> Tset k
  | Tmap (k, v)       -> Tmap (k, f v)
  | Trecord id        -> Trecord id
  | Tunit             -> Tunit
  | Tstorage          -> Tstorage
  | Toperation        -> Toperation
  | Tentry            -> Tentry
  | Tentrysig t       -> Tentrysig (f t)
  | Tprog t           -> Tprog (f t)
  | Tvset (v, t)      -> Tvset (v, t)
  | Ttrace t          -> Ttrace t

(* -------------------------------------------------------------------- *)

let map_for_ident (g : 'id -> 'id) = function
  | FIsimple i             -> FIsimple (g i)
  | FIdouble (x, y)        -> FIdouble (g x, g y)

let map_assign_kind (fi : ident -> ident) (g : 'id -> 'id) f = function
  | Avar id             -> Avar (g id)
  | Avarstore id        -> Avarstore (g id)
  | Aasset (an, fn, k)  -> Aasset (g an, g fn, f k)
  | Arecord (rn, fn, r) -> Arecord (g rn, g fn, f r)
  | Astate              -> Astate
  | Aassetstate (id, v) -> Aassetstate (fi id, f v)

let map_var_kind f = function
  | Vassetstate mt -> Vassetstate (f mt)
  | Vstorevar -> Vstorevar
  | Vstorecol -> Vstorecol
  | Venumval -> Venumval
  | Vlocal -> Vlocal
  | Vparam -> Vparam
  | Vfield -> Vfield
  | Vstate -> Vstate
  | Vthe -> Vthe

let map_container_kind (fi : ident -> ident) f = function
  | CKcoll               -> CKcoll
  | CKview  mt           -> CKview  (f mt)
  | CKfield (an, fn, mt) -> CKfield (fi an, fi fn, f mt)

let map_iter_container_kind (fi : ident -> ident) f = function
  | ICKcoll  an           -> ICKcoll  (fi an)
  | ICKview  mt           -> ICKview  (f mt)
  | ICKfield (an, fn, mt) -> ICKfield (an, fn, f mt)
  | ICKset   mt           -> ICKset   (f mt)
  | ICKlist  mt           -> ICKlist  (f mt)
  | ICKmap   mt           -> ICKmap   (f mt)

let map_term_node_internal (fi : ident -> ident) (g : 'id -> 'id) (ft : type_ -> type_) (f : 'id mterm_gen -> 'id mterm_gen) = function
  (* lambda *)
  | Mletin (i, a, t, b, o)         -> Mletin (List.map g i, f a, Option.map ft t, f b, Option.map f o)
  | Mdeclvar (i, t, v)             -> Mdeclvar (List.map g i, Option.map ft t, f v)
  | Mapp (e, args)                 -> Mapp (g e, List.map f args)
  (* assign *)
  | Massign (op, k, v)             -> Massign (op, map_assign_kind fi g f k, f v)
  (* control *)
  | Mif (c, t, e)                  -> Mif (f c, f t, Option.map f e)
  | Mmatchwith (e, l)              -> Mmatchwith (f e, List.map (fun (p, e) -> (p, f e)) l)
  | Mfor (i, c, b, lbl)            -> Mfor (map_for_ident g i, map_iter_container_kind fi f c, f b, lbl)
  | Miter (i, a, b, c, lbl)        -> Miter (g i, f a, f b, f c, lbl)
  | Mseq is                        -> Mseq (List.map f is)
  | Mreturn x                      -> Mreturn (f x)
  | Mlabel i                       -> Mlabel (g i)
  | Mmark (i, x)                   -> Mmark (g i, f x)
  (* effect *)
  | Mfail v                        -> Mfail (match v with | Invalid v -> Invalid (f v) | _ -> v)
  | Mtransfer (v, d)               -> Mtransfer (f v, f d)
  | Mcallcontract (v, d, t, func, args) -> Mcallcontract(f v, f d, fi t, func, List.map (fun (id, t) -> (g id, f t)) args)
  | Mcallentry (v, e, arg)         -> Mcallentry(f v, g e, f arg)
  | Mcallself (v, e, args)         -> Mcallself(f v, g e, (List.map f args))
  (* entrypoint *)
  | Mentrycontract (c, id)         -> Mentrycontract (f c, g id)
  | Mentrypoint (a, s)             -> Mentrypoint (f a, f s)
  | Mself id                       -> Mself (g id)
  (* operation *)
  | Moperations                    -> Moperations
  | Mmkoperation (v, d, a)         -> Mmkoperation (f v, f d, f a)
  (* literals *)
  | Mint v                         -> Mint v
  | Mnat v                         -> Mnat v
  | Mbool v                        -> Mbool v
  | Menum v                        -> Menum (fi v)
  | Mrational (n, d)               -> Mrational (n, d)
  | Mstring v                      -> Mstring v
  | Mcurrency (v, c)               -> Mcurrency (v, c)
  | Maddress v                     -> Maddress v
  | Mdate v                        -> Mdate v
  | Mduration v                    -> Mduration v
  | Mtimestamp v                   -> Mtimestamp v
  | Mbytes v                       -> Mbytes v
  | Munit                          -> Munit
  (* control expression *)
  | Mexprif (c, t, e)              -> Mexprif (f c, f t, f e)
  | Mexprmatchwith (e, l)          -> Mexprmatchwith (f e, List.map (fun (p, e) -> (p, f e)) l)
  (* composite type constructors *)
  | Mnone                          -> Mnone
  | Msome v                        -> Msome (f v)
  | Mtuple l                       -> Mtuple (List.map f l)
  | Masset l                       -> Masset (List.map f l)
  | Massets l                      -> Massets (List.map f l)
  | Mlitset l                      -> Mlitset (List.map f l)
  | Mlitlist l                     -> Mlitlist (List.map f l)
  | Mlitmap l                      -> Mlitmap (List.map (pair_sigle_map f) l)
  | Mlitrecord l                   -> Mlitrecord (List.map ((fun (x, y) -> (x, f y))) l)
  (* access *)
  | Mdot (e, i)                    -> Mdot (f e, g i)
  | Mdotassetfield (an, k, fn)     -> Mdotassetfield (g an, f k, g fn)
  | Mdotcontract (e, i)            -> Mdotcontract (f e, g i)
  | Maccestuple (e, i)             -> Maccestuple (f e, i)
  (* comparison operators *)
  | Mequal (t, l, r)               -> Mequal (ft t, f l, f r)
  | Mnequal (t, l, r)              -> Mnequal (ft t, f l, f r)
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
  | Mdivrat (l, r)                 -> Mdivrat (f l, f r)
  | Mdiveuc (l, r)                 -> Mdiveuc (f l, f r)
  | Mmodulo (l, r)                 -> Mmodulo (f l, f r)
  | Muplus e                       -> Muplus (f e)
  | Muminus e                      -> Muminus (f e)
  (* asset api effect *)
  | Maddasset (an, i)              -> Maddasset (fi an, f i)
  | Maddfield (an, fn, c, i)       -> Maddfield (fi an, fi fn, f c, f i)
  | Mremoveasset (an, i)           -> Mremoveasset (fi an, f i)
  | Mremovefield (an, fn, c, i)    -> Mremovefield (fi an, fi fn, f c, f i)
  | Mremoveall (an, fn, a)         -> Mremoveall (fi an, fi fn, f a)
  | Mremoveif (an, c, la, lb, a)   -> Mremoveif (fi an, map_container_kind fi f c, List.map (fun (i, t) -> (fi i, ft t)) la, f lb, List.map f a)
  | Mclear (an, v)                 -> Mclear (fi an, map_container_kind fi f v)
  | Mset (an, l, k, v)             -> Mset (fi an, List.map fi l, f k, f v)
  | Mupdate (an, k, l)             -> Mupdate (fi an, f k, List.map (fun (id, op, v) -> (g id, op, f v)) l)
  | Maddupdate (an, c, k, l)       -> Maddupdate (fi an, map_container_kind fi f c, f k, List.map (fun (id, op, v) -> (g id, op, f v)) l)
  (* asset api expression *)
  | Mget (an, c, k)                -> Mget (fi an, map_container_kind fi f c, f k)
  | Mselect (an, c, la, lb, a)     -> Mselect (fi an, map_container_kind fi f c, List.map (fun (i, t) -> (fi i, ft t)) la, f lb, List.map f a)
  | Msort (an, c, l)               -> Msort (fi an, map_container_kind fi f c, l)
  | Mcontains (an, c, i)           -> Mcontains (fi an, map_container_kind fi f c, f i)
  | Mnth (an, c, i)                -> Mnth (fi an, map_container_kind fi f c, f i)
  | Mcount (an, c)                 -> Mcount (fi an, map_container_kind fi f c)
  | Msum (an, c, p)                -> Msum (fi an, map_container_kind fi f c, f p)
  | Mhead (an, c, i)               -> Mhead (fi an, map_container_kind fi f c, f i)
  | Mtail (an, c, i)               -> Mtail (fi an, map_container_kind fi f c, f i)
  (* utils *)
  | Mcast (src, dst, v)            -> Mcast (ft src, ft dst, f v)
  | Mtupleaccess (x, k)            -> Mtupleaccess (f x, k)
  (* set api expression *)
  | Msetadd (t, c, a)              -> Msetadd (ft t, f c, f a)
  | Msetremove (t, c, a)           -> Msetremove (ft t, f c, f a)
  | Msetcontains (t, c, a)         -> Msetcontains (ft t, f c, f a)
  | Msetlength (t, c)              -> Msetlength (ft t, f c)
  (* list api expression *)
  | Mlistprepend (t, c, a)         -> Mlistprepend (ft t, f c, f a)
  | Mlistcontains (t, c, a)        -> Mlistcontains (t, f c, f a)
  | Mlistlength(t, c)              -> Mlistlength(t, f c)
  | Mlistnth (t, c, a)             -> Mlistnth (t, f c, f a)
  (* map api expression *)
  | Mmapput (tk, tv, c, k, v)      -> Mmapput (ft tk, ft tv, f c, f k, f v)
  | Mmapremove (tk, tv, c, k)      -> Mmapremove (ft tk, ft tv, f c, f k)
  | Mmapget (tk, tv, c, k)         -> Mmapget (ft tk, ft tv, f c, f k)
  | Mmapgetopt (tk, tv, c, k)      -> Mmapgetopt (ft tk, ft tv, f c, f k)
  | Mmapcontains (tk, tv, c, k)    -> Mmapcontains (ft tk, ft tv, f c, f k)
  | Mmaplength (tk, tv, c)         -> Mmaplength (ft tk, ft tv, f c)
  (* builtin functions *)
  | Mmin (l, r)                    -> Mmin (f l, f r)
  | Mmax (l, r)                    -> Mmax (f l, f r)
  | Mabs a                         -> Mabs (f a)
  | Mconcat (x, y)                 -> Mconcat (f x, f y)
  | Mslice (x, s, e)               -> Mslice (f x, f s, f e)
  | Mlength x                      -> Mlength (f x)
  | Misnone x                      -> Misnone (f x)
  | Missome x                      -> Missome (f x)
  | Mgetopt x                      -> Mgetopt (f x)
  | Mfloor x                       -> Mfloor (f x)
  | Mceil x                        -> Mceil (f x)
  | Mpack x                        -> Mpack (f x)
  | Munpack (t, x)                 -> Munpack (ft t, f x)
  (* crypto functions *)
  | Mblake2b x                     -> Mblake2b (f x)
  | Msha256 x                      -> Msha256 (f x)
  | Msha512 x                      -> Msha512 (f x)
  | Mhashkey x                     -> Mhashkey (f x)
  | Mchecksignature (k, s, x)      -> Mchecksignature (f k, f s, f x)
  (* constants *)
  | Mnow                           -> Mnow
  | Mtransferred                   -> Mtransferred
  | Mcaller                        -> Mcaller
  | Mbalance                       -> Mbalance
  | Msource                        -> Msource
  | Mchainid                       -> Mchainid
  | Mselfaddress                   -> Mselfaddress
  (* variable *)
  | Mvar (id, k)                   -> Mvar (g id, map_var_kind f k)
  (* rational *)
  | Mrateq (l, r)                  -> Mrateq (f l, f r)
  | Mratcmp (op, l, r)             -> Mratcmp (op, f l, f r)
  | Mratarith (op, l, r)           -> Mratarith (op, f l, f r)
  | Mratuminus v                   -> Mratuminus (f v)
  | Mrattez (c, t)                 -> Mrattez (f c, f t)
  | Mdivtez (c, t)                 -> Mdivtez (f c, f t)
  | Minttorat e                    -> Minttorat (f e)
  | Mratdur (c, t)                 -> Mratdur (f c, f t)
  (* functional *)
  | Mfold (i, is, c, b)            -> Mfold (g i, List.map g is, f c, f b)
  (* imperative *)
  | Mbreak                         -> Mbreak
  (* quantifiers *)
  | Mforall (i, t, s, e)           -> Mforall (g i, ft t, Option.map f s, f e)
  | Mexists (i, t, s, e)           -> Mexists (g i, ft t, Option.map f s, f e)
  (* formula operators *)
  | Mimply (l, r)                  -> Mimply (f l, f r)
  | Mequiv  (l, r)                 -> Mequiv (f l, f r)
  (* formula asset collection *)
  | Msetbefore    e                -> Msetbefore    (f e)
  | Msetat (lbl, e)                -> Msetat        (fi lbl, f e)
  | Msetunmoved   e                -> Msetunmoved   (f e)
  | Msetadded     e                -> Msetadded     (f e)
  | Msetremoved   e                -> Msetremoved   (f e)
  | Msetiterated  e                -> Msetiterated  (map_iter_container_kind fi f e)
  | Msettoiterate e                -> Msettoiterate (map_iter_container_kind fi f e)
  (* formula asset collection methods *)
  | Mempty an                      -> Mempty     (fi an)
  | Msingleton (an, k)             -> Msingleton (fi an, f k)
  | Msubsetof (an, c, i)           -> Msubsetof  (fi an, map_container_kind fi f c, f i)
  | Misempty (an, r)               -> Misempty   (fi an, f r)
  | Munion (an, l, r)              -> Munion     (fi an, f l, f r)
  | Minter (an, l, r)              -> Minter     (fi an, f l, f r)
  | Mdiff (an, l, r)               -> Mdiff      (fi an, f l, f r)

let map_gen_mterm g f (i : 'id mterm_gen) : 'id mterm_gen =
  {
    i with
    node = g f i.node
  }

let map_term_node =
  let id x = x in
  map_term_node_internal id id id

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
)

let map_mterm_model_formula custom (f : ('id, 't) ctx_model_gen -> mterm -> mterm) (model : model) : model =
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

let fold_assign_kind f accu = function
  | Avar _              -> accu
  | Avarstore _         -> accu
  | Aasset  (_, _, mt)  -> f accu mt
  | Arecord (_, _, mt)  -> f accu mt
  | Astate              -> accu
  | Aassetstate (_, mt) -> f accu mt

let fold_var_kind f accu = function
  | Vassetstate mt -> f accu mt
  | Vstorevar
  | Vstorecol
  | Venumval
  | Vlocal
  | Vparam
  | Vfield
  | Vstate
  | Vthe -> accu

let fold_container_kind f accu = function
  | CKcoll          -> accu
  | CKview mt       -> f accu mt
  | CKfield (_, _, mt)      -> f accu mt

let fold_iter_container_kind f accu = function
  | ICKcoll  _          -> accu
  | ICKview  mt         -> f accu mt
  | ICKfield (_, _, mt) -> f accu mt
  | ICKset   mt         -> f accu mt
  | ICKlist  mt         -> f accu mt
  | ICKmap   mt         -> f accu mt

let fold_term (f : 'a -> ('id mterm_gen) -> 'a) (accu : 'a) (term : 'id mterm_gen) : 'a =
  let opt f accu x = match x with | Some v -> f accu v | None -> accu in
  match term.node with
  (* lambda *)
  | Mletin (_, a, _, b, o)                -> let tmp = f (f accu a) b in Option.map_dfl (f tmp) tmp o
  | Mdeclvar (_, _, v)                    -> f accu v
  | Mapp (_, args)                        -> List.fold_left f accu args
  (* assign *)
  | Massign (_, k, e)                     -> f (fold_assign_kind f accu k) e
  (* control *)
  | Mif (c, t, e)                         -> opt f (f (f accu c) t) e
  | Mmatchwith (e, l)                     -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Mfor (_, c, b, _)                     -> f (fold_iter_container_kind f accu c) b
  | Miter (_, a, b, c, _)                 -> f (f (f accu a) b) c
  | Mseq is                               -> List.fold_left f accu is
  | Mreturn x                             -> f accu x
  | Mlabel _                              -> accu
  | Mmark (_, x)                          -> f accu x
  (* effect *)
  | Mfail v                               -> (match v with | Invalid v -> f accu v | _ -> accu)
  | Mtransfer (v, d)                      -> f (f accu v) d
  | Mcallcontract (v, d, _, _, args)      -> List.fold_left (fun accu (_, t) -> f accu t) (f (f accu v) d) args
  | Mcallentry (v, _, arg)                -> f (f accu v) arg
  | Mcallself (v, _, args)                -> List.fold_left (fun accu x -> f accu x) (f accu v) args
  (* entrypoint *)
  | Mentrycontract (c, _)                 -> f accu c
  | Mentrypoint (a, s)                    -> f (f accu a) s
  | Mself _                               -> accu
  (* operation *)
  | Moperations                           -> accu
  | Mmkoperation (v, d, a)                -> f (f (f accu v) d) a
  (* literals *)
  | Mint _                                -> accu
  | Mnat _                                -> accu
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
  | Munit                                 -> accu
  (* control expression *)
  | Mexprif (c, t, e)                     -> f (f (f accu c) t) e
  | Mexprmatchwith (e, l)                 -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  (* composite type constructors *)
  | Mnone                                 -> accu
  | Msome v                               -> f accu v
  | Mtuple l                              -> List.fold_left f accu l
  | Masset l                              -> List.fold_left f accu l
  | Massets l                             -> List.fold_left f accu l
  | Mlitset l                             -> List.fold_left f accu l
  | Mlitlist l                            -> List.fold_left f accu l
  | Mlitmap l                             -> List.fold_left (fun accu (k, v) -> f (f accu k) v) accu l
  | Mlitrecord l                          -> List.fold_left (fun accu (_, v) -> f accu v) accu l
  (* access *)
  | Mdot (e, _)                           -> f accu e
  | Mdotassetfield (_, k, _)              -> f accu k
  | Mdotcontract (e, _)                   -> f accu e
  | Maccestuple (e, _)                    -> f accu e
  (* comparison operators *)
  | Mequal (_, l, r)                         -> f (f accu l) r
  | Mnequal (_, l, r)                        -> f (f accu l) r
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
  | Mdivrat (l, r)                        -> f (f accu l) r
  | Mdiveuc (l, r)                        -> f (f accu l) r
  | Mmodulo (l, r)                        -> f (f accu l) r
  | Muplus e                              -> f accu e
  | Muminus e                             -> f accu e
  (* asset api effect *)
  | Maddasset (_, i)                      -> f accu i
  | Maddfield (_, _, c, i)                -> f (f accu c) i
  | Mremoveasset (_, i)                   -> f accu i
  | Mremovefield (_, _, c, i)             -> f (f accu c) i
  | Mremoveall (_, _, a)                  -> f accu a
  | Mremoveif (_, c, _, lb, a)            -> List.fold_left (fun accu x -> f accu x) (f (fold_container_kind f accu c) lb) a
  | Mclear (_, v)                         -> fold_container_kind f accu v
  | Mset (_, _, k, v)                     -> f (f accu v) k
  | Mupdate (_, k, l)                     -> List.fold_left (fun accu (_, _, v) -> f accu v) (f accu k) l
  | Maddupdate (_, c, k, l)               -> List.fold_left (fun accu (_, _, v) -> f accu v) (f (fold_container_kind f accu c) k) l
  (* asset api expression *)
  | Mget (_, c, k)                        -> f (fold_container_kind f accu c) k
  | Mselect (_, c, _, lb, a)              -> List.fold_left (fun accu x -> f accu x) (f (fold_container_kind f accu c) lb) a
  | Msort (_, c,_)                        -> fold_container_kind f accu c
  | Mcontains (_, c, i)                   -> f (fold_container_kind f accu c) i
  | Mnth (_, c, i)                        -> f (fold_container_kind f accu c) i
  | Mcount (_, c)                         -> fold_container_kind f accu c
  | Msum (_, c, p)                        -> f (fold_container_kind f accu c) p
  | Mhead (_, c, i)                       -> f (fold_container_kind f accu c) i
  | Mtail (_, c, i)                       -> f (fold_container_kind f accu c) i
  (* utils *)
  | Mcast (_ , _, v)                      -> f accu v
  | Mtupleaccess (x, _)                   -> f accu x
  (* set api expression *)
  | Msetadd (_, c, a)                     -> f (f accu c) a
  | Msetremove (_, c, a)                  -> f (f accu c) a
  | Msetcontains (_, c, a)                -> f (f accu c) a
  | Msetlength (_, c)                     -> f accu c
  (* list api expression *)
  | Mlistprepend (_, c, a)                -> f (f accu c) a
  | Mlistcontains (_, c, a)               -> f (f accu c) a
  | Mlistlength (_, c)                    -> f accu c
  | Mlistnth (_, c, a)                    -> f (f accu c) a
  (* map api expression *)
  | Mmapput (_, _, c, k, v)               -> f (f (f accu c) k) v
  | Mmapremove (_, _, c, k)               -> f (f accu c) k
  | Mmapget (_, _, c, k)                  -> f (f accu c) k
  | Mmapgetopt (_, _, c, k)               -> f (f accu c) k
  | Mmapcontains (_, _, c, k)             -> f (f accu c) k
  | Mmaplength (_, _, c)                  -> f accu c
  (* builtin functions *)
  | Mmax (l, r)                           -> f (f accu l) r
  | Mmin (l, r)                           -> f (f accu l) r
  | Mabs a                                -> f accu a
  | Mconcat (x, y)                        -> f (f accu x) y
  | Mslice (x, s, e)                      -> f (f (f accu x) s) e
  | Mlength x                             -> f accu x
  | Misnone x                             -> f accu x
  | Missome x                             -> f accu x
  | Mgetopt x                             -> f accu x
  | Mfloor x                              -> f accu x
  | Mceil x                               -> f accu x
  | Mpack x                               -> f accu x
  | Munpack (_, x)                        -> f accu x
  (* crypto functions *)
  | Mblake2b x                            -> f accu x
  | Msha256  x                            -> f accu x
  | Msha512  x                            -> f accu x
  | Mhashkey  x                           -> f accu x
  | Mchecksignature (k, s, x)             -> f (f (f accu k) s) x
  (* constants *)
  | Mnow                                  -> accu
  | Mtransferred                          -> accu
  | Mcaller                               -> accu
  | Mbalance                              -> accu
  | Msource                               -> accu
  | Mselfaddress                          -> accu
  | Mchainid                              -> accu
  (* variable *)
  | Mvar (_, k)                           -> fold_var_kind f accu k
  (* rational *)
  | Mrateq (l, r)                         -> f (f accu l) r
  | Mratcmp (_, l, r)                     -> f (f accu l) r
  | Mratarith (_, l, r)                   -> f (f accu l) r
  | Mratuminus v                          -> f accu v
  | Mrattez (c, t)                        -> f (f accu c) t
  | Mdivtez (c, t)                        -> f (f accu c) t
  | Minttorat e                           -> f accu e
  | Mratdur (c, t)                        -> f (f accu c) t
  (* functional *)
  | Mfold (_, _, c, b)                    -> f (f accu c) b
  (* imperative *)
  | Mbreak                                -> accu
  (* quantifiers *)
  | Mforall (_, _, s, e)                  -> f (opt f accu s) e
  | Mexists (_, _, s, e)                  -> f (opt f accu s) e
  (* formula operators *)
  | Mimply (l, r)                         -> f (f accu l) r
  | Mequiv  (l, r)                        -> f (f accu l) r
  (* formula asset collection *)
  | Msetbefore    e                       -> f accu e
  | Msetat   (_, e)                       -> f accu e
  | Msetunmoved   e                       -> f accu e
  | Msetadded     e                       -> f accu e
  | Msetremoved   e                       -> f accu e
  | Msetiterated  e                       -> fold_iter_container_kind f accu e
  | Msettoiterate e                       -> fold_iter_container_kind f accu e
  (* formula asset collection methods *)
  | Mempty _                              -> accu
  | Msingleton (_, k)                     -> f accu k
  | Msubsetof (_, c, i)                   -> f (fold_container_kind f accu c) i
  | Misempty  (_, r)                      -> f accu r
  | Munion (_, l, r)                      -> f (f accu l) r
  | Minter (_, l, r)                      -> f (f accu l) r
  | Mdiff  (_, l, r)                      -> f (f accu l) r


let fold_map_term_list f acc l : 'term list * 'a =
  List.fold_left
    (fun (pterms, accu) x ->
       let p, accu = f accu x in
       pterms @ [p], accu) ([], acc) l

let fold_map_assign_kind f accu = function
  | Avar id             -> Avar id, accu
  | Avarstore id        -> Avarstore id, accu
  | Aasset (an, fn, k)  -> let ke, ka = f accu k in Aasset  (an, fn, ke), ka
  | Arecord (rn, fn, r) -> let re, ra = f accu r in Arecord (rn, fn, re), ra
  | Astate              -> Astate, accu
  | Aassetstate (id, v) -> let ve, va = f accu v in Aassetstate (id, ve), va

let fold_map_var_kind f accu = function
  | Vassetstate mt ->
    let mte, mta = f accu mt in
    Vassetstate mte, mta
  | Vstorevar -> Vstorevar, accu
  | Vstorecol -> Vstorecol, accu
  | Venumval  -> Venumval,  accu
  | Vlocal    -> Vlocal,    accu
  | Vparam    -> Vparam,    accu
  | Vfield    -> Vfield,    accu
  | Vstate    -> Vstate,    accu
  | Vthe      -> Vthe,      accu

let fold_map_container_kind f accu = function
  | CKcoll -> CKcoll, accu
  | CKview mt ->
    let mte, mta = f accu mt in
    CKview mte, mta
  | CKfield (an, fn, mt) ->
    let mte, mta = f accu mt in
    CKfield (an, fn, mte), mta

let fold_map_iter_container_kind f accu = function
  | ICKcoll an -> ICKcoll an, accu
  | ICKview mt ->
    let mte, mta = f accu mt in
    ICKview mte, mta
  | ICKfield (an, fn, mt) ->
    let mte, mta = f accu mt in
    ICKfield (an, fn, mte), mta
  | ICKset mt ->
    let mte, mta = f accu mt in
    ICKset mte, mta
  | ICKlist mt ->
    let mte, mta = f accu mt in
    ICKlist mte, mta
  | ICKmap mt ->
    let mte, mta = f accu mt in
    ICKmap mte, mta

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

  | Massign (op, k, v) ->
    let ke, ka = fold_map_assign_kind f accu k in
    let ve, va = f ka v in
    g (Massign (op, ke, ve)), va


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

  | Mfor (fi, c, b, lbl) ->
    let ce, ca = fold_map_iter_container_kind f accu c in
    let be, ba = f ca b in
    g (Mfor (fi, ce, be, lbl)), ba

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

  | Mmark (i, x) ->
    let xe, xa = f accu x in
    g (Mmark (i, xe)), xa


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

  | Mcallcontract(v, d, t, func, args) ->
    let ve, va = f accu v in
    let de, da = f va d in
    let (lp, la) = List.fold_left
        (fun (pterms, accu) (id, x) ->
           let p, accu = f accu x in
           pterms @ [id, p], accu) ([], da) args in
    g (Mcallcontract(ve, de, t, func, lp)), la

  | Mcallentry(v, e, arg) ->
    let ve, va = f accu v in
    let ae, aa = f va arg in
    g (Mcallentry(ve, e, ae)), aa

  | Mcallself(v, e, args) ->
    let ve, va = f accu v in
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], va) args in
    g (Mcallself(ve, e, lp)), la


  (* entrypoint *)

  | Mentrycontract (c, id) ->
    let ce, ca = f accu c in
    g (Mentrycontract (ce, id)), ca

  | Mentrypoint (a, s) ->
    let ae, aa = f accu a in
    let se, sa = f aa s in
    g (Mentrypoint (ae, se)), sa

  | Mself id ->
    g (Mself id), accu


  (* operation *)

  | Moperations ->
    g (Moperations), accu

  | Mmkoperation (v, d, a) ->
    let ve, va = f accu v in
    let de, da = f va d in
    let ae, aa = f da a in
    g (Mmkoperation (ve, de, ae)), aa


  (* literals *)

  | Mint v ->
    g (Mint v), accu

  | Mnat v ->
    g (Mnat v), accu

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

  | Munit ->
    g (Munit), accu


  (* control expression *)

  | Mexprif (c, t, e) ->
    let ce, ca = f accu c in
    let ti, ta = f ca t in
    let ei, ea = f ta e in
    g (Mexprif (ce, ti, ei)), ea

  | Mexprmatchwith (e, l) ->
    let ee, ea = f accu e in
    let (pse, psa) =
      List.fold_left
        (fun (ps, accu) (p, i) ->
           let ia, accu = f accu i in
           (p, ia)::ps, accu) ([], ea) l
      |> (fun (x, y) -> (List.rev x, y))
    in
    g (Mexprmatchwith (ee, pse)), psa


  (* composite type constructors *)

  | Mnone ->
    g Mnone, accu

  | Msome v ->
    let ve, va = f accu v in
    g (Msome ve), va

  | Mtuple l ->
    let le, la = fold_map_term_list f accu l in
    g (Mtuple le), la

  | Masset l ->
    let le, la = fold_map_term_list f accu l in
    g (Masset le), la

  | Massets l ->
    let le, la = fold_map_term_list f accu l in
    g (Massets le), la

  | Mlitset l ->
    let le, la = fold_map_term_list f accu l in
    g (Mlitset le), la

  | Mlitlist l ->
    let le, la = fold_map_term_list f accu l in
    g (Mlitlist le), la

  | Mlitmap l ->
    let le, la =
      List.fold_left
        (fun (pterms, accu) (k, v) ->
           let kn, accu = f accu k in
           let vn, accu = f accu v in
           pterms @ [kn, vn], accu) ([], accu) l
    in
    g (Mlitmap le), la

  | Mlitrecord l ->
    let le, la =
      List.fold_left
        (fun (pterms, accu) (i, v) ->
           let vn, accu = f accu v in
           pterms @ [i, vn], accu) ([], accu) l
    in
    g (Mlitrecord le), la

  (* dot *)

  | Mdot (e, i) ->
    let ee, ea = f accu e in
    g (Mdot (ee, i)), ea

  | Mdotassetfield (an, k, fn) ->
    let ke, ka = f accu k in
    g (Mdotassetfield (an, ke, fn)), ka

  | Mdotcontract (e, i) ->
    let ee, ea = f accu e in
    g (Mdotcontract (ee, i)), ea

  | Maccestuple (e, i) ->
    let ee, ea = f accu e in
    g (Maccestuple (ee, i)), ea


  (* comparison operators *)

  | Mequal (t, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mequal (t, le, re)), ra

  | Mnequal (t, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mnequal (t, le, re)), ra

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

  | Mdivrat (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mdivrat (le, re)), ra

  | Mdiveuc (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mdiveuc (le, re)), ra

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

  | Mremoveall (an, fn, a) ->
    let ae, aa = f accu a in
    g (Mremoveall (an, fn, ae)), aa

  | Mremoveif (an, c, la, lb, a) ->
    let ce, ca = fold_map_container_kind f accu c in
    let lbe, lba = f ca lb in
    let ae, aa =
      List.fold_left
        (fun (ae, accu) x ->
           let xa, accu = f accu x in
           xa::ae, accu) ([], lba) a
      |> (fun (x, y) -> (List.rev x, y))
    in
    g (Mremoveif (an, ce, la, lbe, ae)), aa

  | Mclear (an, v) ->
    let ve, va = fold_map_container_kind f accu v in
    g (Mclear (an, ve)), va

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

  | Maddupdate (an, c, k, l) ->
    let ce, ca = fold_map_container_kind f accu c in
    let ke, ka = f ca k in
    let le, la =
      List.fold_left
        (fun (ps, accu) (id, op, v) ->
           let va, accu = f accu v in
           (id, op, va)::ps, accu) ([], ka) l
      |> (fun (x, y) -> (List.rev x, y))
    in
    g (Maddupdate (an, ce, ke, le)), la


  (* asset api expression *)

  | Mget (an, c, k) ->
    let ce, ca = fold_map_container_kind f accu c in
    let ke, ka = f ca k in
    g (Mget (an, ce, ke)), ka

  | Mselect (an, c, la, lb, a) ->
    let ce, ca = fold_map_container_kind f accu c in
    let lbe, lba = f ca lb in
    let ae, aa =
      List.fold_left
        (fun (ae, accu) x ->
           let xa, accu = f accu x in
           xa::ae, accu) ([], lba) a
      |> (fun (x, y) -> (List.rev x, y))
    in
    g (Mselect (an, ce, la, lbe, ae)), aa

  | Msort (an, c, l) ->
    let ce, ca = fold_map_container_kind f accu c in
    g (Msort (an, ce, l)), ca

  | Mcontains (an, c, i) ->
    let ce, ca = fold_map_container_kind f accu c in
    let ie, ia = f ca i in
    g (Mcontains (an, ce, ie)), ia

  | Mnth (an, c, i) ->
    let ce, ca = fold_map_container_kind f accu c in
    let ie, ia = f ca i in
    g (Mnth (an, ce, ie)), ia

  | Mcount (an, c) ->
    let ce, ca = fold_map_container_kind f accu c in
    g (Mcount (an, ce)), ca

  | Msum (an, c, p) ->
    let ce, ca = fold_map_container_kind f accu c in
    let pe, pa = f ca p in
    g (Msum (an, ce, pe)), pa

  | Mhead (an, c, i) ->
    let ce, ca = fold_map_container_kind f accu c in
    let ie, ia = f ca i in
    g (Mhead (an, ce, ie)), ia

  | Mtail (an, c, i) ->
    let ce, ca = fold_map_container_kind f accu c in
    let ie, ia = f ca i in
    g (Mtail (an, ce, ie)), ia


  (* utils *)

  | Mcast (src, dst, v) ->
    let ve, va = f accu v in
    g (Mcast (src, dst, ve)), va

  | Mtupleaccess (x, k) ->
    let xe, xa = f accu x in
    g (Mtupleaccess (xe, k)), xa


  (* set api expression *)

  | Msetadd (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Msetadd (t, ce, ae)), aa

  | Msetremove (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Msetremove (t, ce, ae)), aa

  | Msetcontains (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Msetcontains (t, ce, ae)), aa

  | Msetlength (t, c) ->
    let ce, ca = f accu c in
    g (Msetlength (t, ce)), ca


  (* list api expression *)

  | Mlistprepend (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Mlistprepend (t, ce, ae)), aa

  | Mlistcontains (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Mlistcontains (t, ce, ae)), aa

  | Mlistlength (t, c) ->
    let ce, ca = f accu c in
    g (Mlistlength (t, ce)), ca

  | Mlistnth (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Mlistnth (t, ce, ae)), aa


  (* map api expression *)

  | Mmapput (tk, tv, c, k, v) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    let ve, va = f ka v in
    g (Mmapput (tk, tv, ce, ke, ve)), va

  | Mmapremove (tk, tv, c, k) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    g (Mmapremove (tk, tv, ce, ke)), ka

  | Mmapget (tk, tv, c, k) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    g (Mmapget (tk, tv, ce, ke)), ka

  | Mmapgetopt (tk, tv, c, k) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    g (Mmapgetopt (tk, tv, ce, ke)), ka

  | Mmapcontains (tk, tv, c, k) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    g (Mmapcontains (tk, tv, ce, ke)), ka

  | Mmaplength (tk, tv, c) ->
    let ce, ca = f accu c in
    g (Mmaplength (tk, tv, ce)), ca


  (* builtin functions *)

  | Mmin (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mmin (le, re)), ra

  | Mmax (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mmax (le, re)), ra

  | Mabs a ->
    let ae, aa = f accu a in
    g (Mabs ae), aa

  | Mconcat (x, y) ->
    let xe, xa = f accu x in
    let ye, ya = f xa y in
    g (Mconcat (xe, ye)), ya

  | Mslice (x, s, e) ->
    let xe, xa = f accu x in
    let se, sa = f xa s in
    let ee, ea = f sa e in
    g (Mslice (xe, se, ee)), ea

  | Mlength x ->
    let xe, xa = f accu x in
    g (Mlength xe), xa

  | Missome x ->
    let xe, xa = f accu x in
    g (Missome xe), xa

  | Misnone x ->
    let xe, xa = f accu x in
    g (Misnone xe), xa

  | Mgetopt x ->
    let xe, xa = f accu x in
    g (Mgetopt xe), xa

  | Mfloor x ->
    let xe, xa = f accu x in
    g (Mfloor xe), xa

  | Mceil x ->
    let xe, xa = f accu x in
    g (Mceil xe), xa

  | Mpack x ->
    let xe, xa = f accu x in
    g (Mpack xe), xa

  | Munpack (t, x) ->
    let xe, xa = f accu x in
    g (Munpack (t, xe)), xa

  (* crypto functions *)

  | Mblake2b x ->
    let xe, xa = f accu x in
    g (Mblake2b xe), xa

  | Msha256 x ->
    let xe, xa = f accu x in
    g (Msha256 xe), xa

  | Msha512 x ->
    let xe, xa = f accu x in
    g (Msha512 xe), xa

  | Mhashkey x ->
    let xe, xa = f accu x in
    g (Mhashkey xe), xa

  | Mchecksignature (k, s, x) ->
    let ke, ka = f accu k in
    let se, sa = f ka s in
    let xe, xa = f sa x in
    g (Mchecksignature (ke, se, xe)), xa


  (* constants *)

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

  | Mselfaddress ->
    g Mselfaddress, accu

  | Mchainid ->
    g Mchainid, accu


  (* variable *)

  | Mvar (id, k) ->
    let ke, ka = fold_map_var_kind f accu k in
    g (Mvar (id, ke)), ka


  (* rational *)

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

  | Mratuminus v ->
    let ve, va = f accu v in
    g (Mratuminus ve), va

  | Mrattez (c, t) ->
    let ce, ca = f accu c in
    let te, ta = f ca t in
    g (Mrattez (ce, te)), ta

  | Mdivtez (c, t) ->
    let ce, ca = f accu c in
    let te, ta = f ca t in
    g (Mdivtez (ce, te)), ta

  | Minttorat e ->
    let ee, ea = f accu e in
    g (Minttorat ee), ea

  | Mratdur (c, t) ->
    let ce, ca = f accu c in
    let te, ta = f ca t in
    g (Mratdur (ce, te)), ta


  (* functional *)

  | Mfold (i, is, c, b) ->
    let ce, ca = f accu c in
    let bi, ba = f ca b in
    g (Mfold (i, is, ce, bi)), ba


  (* imperative *)

  | Mbreak ->
    g (Mbreak), accu


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


  (* formula asset collection *)

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
    let ee, ea = fold_map_iter_container_kind f accu e in
    g (Msetiterated ee), ea

  | Msettoiterate e ->
    let ee, ea = fold_map_iter_container_kind f accu e in
    g (Msettoiterate ee), ea


  (* formula asset collection methods *)

  | Mempty an ->
    g (Mempty an), accu

  | Msingleton (an, k) ->
    let ke, ka = f accu k in
    g (Msingleton (an, ke)), ka

  | Msubsetof (an, c, i) ->
    let ce, ca = fold_map_container_kind f accu c in
    let ie, ia = f ca i in
    g (Msubsetof (an, ce, ie)), ia

  | Misempty  (l, r) ->
    let re, ra = f accu r in
    g (Misempty (l, re)), ra

  | Munion (an, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Munion (an, le, re)), ra

  | Minter (an, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Minter (an, le, re)), ra

  | Mdiff  (an, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mdiff (an, le, re)), ra

let fold_left g l accu = List.fold_left (fun accu x -> g x accu) accu l

let fold_label_term (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (lt : 'id label_term_gen) (accu : 'a) : 'a =
  let ctx = { ctx with label = Some lt.label } in
  f ctx accu lt.term

let fold_specification (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (v : 'id specification_gen) (accu : 'a) : 'a =

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
  let fold_mterm_option f x accu = match x with | Some v -> f accu v | _ -> accu in
  let fold_decl (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (d : 'id decl_node_gen) (accu : 'a) : 'a = (
    match d with
    | Dvar s ->
      accu
      |> fold_mterm_option (f ctx) s.default
      |> fold_left (fold_label_term { ctx with formula = true } f) s.invariants
    | Denum e ->
      accu
      |> (fun accu -> List.fold_left (fun accu (x : enum_item) -> fold_left (fold_label_term { ctx with formula = true } f) x.invariants accu) accu e.values)
    | Dasset a ->
      accu
      |> (fun accu -> List.fold_left (fun accu (x : asset_item) -> (fold_mterm_option (f ctx) x.default accu)) accu a.values)
      |> fold_left (fold_label_term { ctx with formula = true } f) a.invariants
    | _ -> accu
  ) in

  let fold_entry (ctx : ('id, 't) ctx_model_gen) (f : ('id, 't) ctx_model_gen -> 'a -> 'id mterm_gen -> 'a) (a : 'id function__gen) (accu : 'a) : 'a = (
    let accu : 'a = (
      match a.node with
      | Function (fs, _)
      | Entry fs -> f {ctx with fs = Some fs} accu fs.body
    ) in
    Option.map_dfl (fun (x : 'id specification_gen) -> fold_specification ctx f x accu) accu a.spec
  ) in

  let ctx : ctx_model = mk_ctx_model () in

  accu
  |> fold_left (fold_decl ctx f) m.decls
  |> fold_left (fold_entry ctx f) m.functions
  |> fold_specification ctx f m.specification

type kind_ident =
  | KIarchetype
  | KIdeclvarname
  | KIassetname
  | KIassetfield
  | KIassetstate
  | KIassetinit
  | KIrecordname
  | KIrecordfield
  | KIparamlambda
  | KIenumname
  | KIenumvalue
  | KIcontractname
  | KIcontractentry
  | KIstoragefield
  | KIentry
  | KIfunction
  | KIargument
  | KIlocalvar
  | KIlabel
  | KIpredicate
  | KIdefinition
  | KIdefinitionvar
  | KIinvariant
  | KIpostcondition
  | KIpostconditionuse
  | KIsecurityad
  | KIsecurityrole
  | KIsecurityentry
  | KImterm (* mterm *)

let replace_ident_model (f : kind_ident -> ident -> ident) (model : model) : model =
  let g k (id : lident) = {id with pldesc=(f k id.pldesc)} in
  let rec for_type (t : type_) : type_ =
    match t with
    | Tasset id         -> Tasset (g KIassetname id)
    | Tenum id          -> Tenum (g KIenumname id)
    | Tstate            -> t
    | Tcontract id      -> Tcontract (g KIcontractname id)
    | Tbuiltin _        -> t
    | Tcontainer (a, c) -> Tcontainer (for_type a, c)
    | Tlist a           -> Tlist (for_type a)
    | Toption a         -> Toption (for_type a)
    | Ttuple l          -> Ttuple (List.map for_type l)
    | Tset k            -> Tset k
    | Tmap (k, v)       -> Tmap (k, for_type v)
    | Trecord id        -> Trecord (g KIrecordname id)
    | Tunit             -> t
    | Tstorage          -> t
    | Toperation        -> t
    | Tentry            -> t
    | Tentrysig t       -> Tentrysig (for_type t)
    | Tprog a           -> Tprog (for_type a)
    | Tvset (v, a)      -> Tvset (v, for_type a)
    | Ttrace _          -> t
  in
  let rec for_mterm (mt : mterm) : mterm =
    let node : mterm__node = map_term_node_internal (f KImterm) (g KImterm) for_type for_mterm mt.node in
    mk_mterm node (for_type mt.type_)
  in
  let for_api_item (ai : api_storage) : api_storage =
    let for_node_item (asn : api_storage_node) : api_storage_node =
      let for_api_asset (aasset : api_asset) : api_asset =
        match aasset with
        | Get an                  -> Get (f KIassetname an)
        | Set an                  -> Set (f KIassetname an)
        | Add an                  -> Add (f KIassetname an)
        | Remove an               -> Remove (f KIassetname an)
        | Clear (an, ck)          -> Clear (f KIassetname an, ck)
        | Update (an, l)          -> Update (f KIassetname an, List.map (fun (id, op, v) -> (f KIparamlambda id, op, for_mterm v)) l)
        | FieldAdd (an, id)       -> FieldAdd (f KIassetname an, f KIassetfield id)
        | FieldRemove (an, id)    -> FieldRemove (f KIassetname an, f KIassetfield id)
        | RemoveAll (an, id)      -> RemoveAll (f KIassetname an, f KIassetfield id)
        | RemoveIf (an, ck, l, p) -> RemoveIf (f KIassetname an, ck, List.map (fun (id, t) -> f KIparamlambda id, t) l, for_mterm p)
        | Contains (an, ck)       -> Contains (f KIassetname an, ck)
        | Nth (an, ck)            -> Nth (f KIassetname an, ck)
        | Select (an, ck, l, p)   -> Select (f KIassetname an, ck, List.map (fun (id, t) -> f KIparamlambda id, t) l, for_mterm p)
        | Sort (an, ck, l)        -> Sort (an, ck, List.map (fun (id, k) -> f KIassetfield id, k) l)
        | Count (an, ck)          -> Count (f KIassetname an, ck)
        | Sum (an, ck, t, e)      -> Sum (f KIassetname an, ck, for_type t, for_mterm e)
        | Head (an, ck)           -> Head (f KIassetname an, ck)
        | Tail (an, ck)           -> Tail (f KIassetname an, ck)

      in
      let for_api_list (alist : api_list) : api_list =
        match alist with
        | Lprepend  t -> Lprepend  (for_type t)
        | Lcontains t -> Lcontains (for_type t)
        | Llength   t -> Llength   (for_type t)
        | Lnth      t -> Lnth      (for_type t)
      in
      let for_api_builtin (abuiltin : api_builtin) : api_builtin =
        match abuiltin with
        | Bmin t    -> Bmin    (for_type t)
        | Bmax t    -> Bmax    (for_type t)
        | Babs t    -> Babs    (for_type t)
        | Bconcat t -> Bconcat (for_type t)
        | Bslice  t -> Bslice  (for_type t)
        | Blength t -> Blength (for_type t)
        | Bisnone t -> Bisnone (for_type t)
        | Bissome t -> Bissome (for_type t)
        | Bgetopt t -> Bgetopt (for_type t)
        | Bfloor    -> Bfloor
        | Bceil     -> Bceil
      in
      let for_api_internal (ainternal : api_internal) : api_internal =
        match ainternal with
        | RatEq     -> RatEq
        | RatCmp    -> RatCmp
        | RatArith  -> RatArith
        | RatUminus -> RatUminus
        | RatTez    -> RatTez
        | DivTez    -> DivTez
        | RatDur    -> RatDur
      in
      match asn with
      | APIAsset    aasset    -> APIAsset    (for_api_asset aasset)
      | APIList     alist     -> APIList     (for_api_list alist)
      | APIBuiltin  abuiltin  -> APIBuiltin  (for_api_builtin abuiltin)
      | APIInternal ainternal -> APIInternal (for_api_internal ainternal)
    in
    {
      node_item    = for_node_item ai.node_item;
      api_loc      = ai.api_loc;
    }
  in
  let for_api_verif (apiv : api_verif) : api_verif =
    match apiv with
    | StorageInvariant (a, b, c) -> StorageInvariant (f KIassetname a, f KIassetfield b, for_mterm c)
  in
  let for_label_term (lt : label_term) : label_term =
    {
      label = g KIlabel lt.label;
      term  = for_mterm lt.term;
      loc   = lt.loc;
    }
  in
  let for_decl_node (d : decl_node) : decl_node =
    let for_var (v : var) : var =
      {
        name          = g KIdeclvarname v.name;
        type_         = for_type v.type_;
        original_type = for_type v.original_type;
        constant      = v.constant;
        default       = Option.map for_mterm v.default;
        invariants    = List.map for_label_term v.invariants;
        loc           = v.loc;
      }
    in
    let for_enum (e : enum) : enum =
      let for_enum_item (ei : enum_item) : enum_item =
        {
          name        = g KIenumvalue ei.name;
          invariants  = List.map for_label_term ei.invariants;
        }
      in
      {
        name          = g KIenumname e.name;
        values        = List.map for_enum_item e.values;
        initial       = g KIenumname e.initial;
      }
    in
    let for_asset (a : asset) : asset =
      let for_asset_item (ai : asset_item) : asset_item =
        {
          name          = g KIassetfield ai.name;
          type_         = for_type ai.type_;
          original_type = for_type ai.original_type;
          default       = Option.map for_mterm ai.default;
          shadow        = ai.shadow;
          loc           = ai.loc;
        }
      in
      {
        name          = g KIassetname a.name;
        values        = List.map for_asset_item a.values;
        key           = f KIassetfield a.key;
        sort          = List.map (f KIassetfield) a.sort;
        state         = Option.map (g KIassetstate) a.state;
        invariants    = List.map for_label_term a.invariants;
        init          = List.map for_mterm a.init;
        loc           = a.loc;
      }
    in
    let for_record (r : record) : record =
      let for_record_field (rf : record_field) =
        {
          name        = g KIrecordname rf.name;
          type_       = for_type rf.type_;
          loc         = rf.loc;
        }
      in
      {
        name          = g KIrecordname r.name;
        fields        = List.map for_record_field r.fields;
        loc           = r.loc;
      }
    in
    let for_contract (c : contract) : contract =
      let for_contract_signature (cs : contract_signature) : contract_signature = {
        name          = g KIcontractentry cs.name;
        args          = List.map (fun (x, y) -> g KIargument x, for_type y) cs.args;
        loc           = cs.loc;
      }
      in
      {
        name          = g KIcontractname c.name;
        signatures    = List.map for_contract_signature c.signatures;
        init          = Option.map for_mterm c.init;
        loc           = c.loc;
      }
    in
    match d with
    | Dvar v      -> Dvar      (for_var v)
    | Denum e     -> Denum     (for_enum e)
    | Dasset a    -> Dasset    (for_asset a)
    | Drecord r   -> Drecord   (for_record r)
    | Dcontract c -> Dcontract (for_contract c)
  in
  let for_storage_item (si : storage_item) : storage_item =
    let for_model_type (mt : model_type) : model_type =
      match mt with
      | MTvar      -> MTvar
      | MTconst    -> MTconst
      | MTasset id -> MTasset id
      | MTstate    -> MTstate
      | MTenum id  -> MTenum id
    in
    {
      id          = g KIstoragefield si.id;
      model_type  = for_model_type si.model_type;
      typ         = for_type si.typ;
      const       = si.const;
      ghost       = si.ghost;
      default     = for_mterm si.default;
      loc         = si.loc;
    }
  in
  let for_specification (spec : specification) : specification =
    let for_predicate (p : predicate) : predicate =
      {
        name = g KIpredicate p.name;
        args = List.map (fun (x, y) -> g KIargument x, for_type y) p.args;
        body = for_mterm p.body;
        loc  = p.loc;
      }
    in
    let for_definition (d : definition) : definition =
      {
        name = g KIdefinition d.name;
        typ  = for_type d.typ;
        var  = g KIdefinitionvar d.var;
        body = for_mterm d.body;
        loc  = d.loc;
      }
    in
    let for_variable (v : variable) : variable =
      let for_argument (arg : argument) : argument =
        let a, b, c = arg in
        g KIargument a, for_type b, Option.map for_mterm c
      in
      let rec for_qualid (q : qualid) : qualid =
        let for_qualid_node (qn : (lident, qualid) qualid_node) : (lident, qualid) qualid_node =
          match qn with
          | Qident id    -> Qident (g KImterm id)
          | Qdot (q, id) -> Qdot (for_qualid q, g KImterm id)
        in
        {
          node  = for_qualid_node q.node;
          type_ = for_type q.type_;
          loc   = q.loc;
        }
      in
      {
        decl         = for_argument v.decl;
        constant     = v.constant;
        from         = Option.map for_qualid v.from;
        to_          = Option.map for_qualid v.to_;
        loc          = v.loc;
      }
    in
    let for_invariant (i : invariant) : invariant =
      {
        label    = g KIlabel i.label;
        formulas = List.map for_mterm i.formulas;
      }
    in
    let for_postcondition (p : postcondition) : postcondition =
      {
        name       = g KIpostcondition p.name;
        mode       = p.mode;
        formula    = for_mterm p.formula;
        invariants = List.map for_invariant p.invariants;
        uses       = List.map (g KIpostconditionuse) p.uses;
      }
    in
    {
      predicates     = List.map for_predicate     spec.predicates;
      definitions    = List.map for_definition    spec.definitions;
      lemmas         = List.map for_label_term    spec.lemmas;
      theorems       = List.map for_label_term    spec.theorems;
      variables      = List.map for_variable      spec.variables;
      invariants     = List.map (fun (x, y) -> g KIinvariant x, List.map for_label_term y) spec.invariants;
      effects        = List.map for_mterm         spec.effects;
      postconditions = List.map for_postcondition spec.postconditions;
      loc            = spec.loc;
    }
  in
  let for_function__ (f__ : function__) : function__ =
    let for_function_node (fn : function_node) : function_node =
      let for_function_struct (fs : function_struct) : function_struct =
        let for_argument (arg : argument) : argument =
          let a, b, c = arg in
          g KIargument a, for_type b, Option.map for_mterm c
        in
        {
          name = g (match fn with | Function _ -> KIfunction | Entry _ -> KIentry) fs.name;
          args = List.map for_argument fs.args;
          body = for_mterm fs.body;
          loc  = fs.loc;
        }
      in
      match fn with
      | Function (fs, t) -> Function (for_function_struct fs, for_type t)
      | Entry fs         -> Entry (for_function_struct fs)
    in
    {
      node = for_function_node f__.node;
      spec = Option.map for_specification f__.spec;
    }
  in
  let for_security (s : security) : security =
    let for_security_item (si : security_item) : security_item =
      let for_security_predicate (sp : security_predicate) : security_predicate =
        let for_security_node (sn : security_node) : security_node =
          let for_entry_description (ad : entry_description) =
            match ad with
            | ADany         -> ADany
            | ADadd      id -> ADadd      (f KIsecurityad id)
            | ADremove   id -> ADremove   (f KIsecurityad id)
            | ADupdate   id -> ADupdate   (f KIsecurityad id)
            | ADtransfer id -> ADtransfer (f KIsecurityad id)
            | ADget      id -> ADget      (f KIsecurityad id)
            | ADiterate  id -> ADiterate  (f KIsecurityad id)
            | ADcall     id -> ADcall     (f KIsecurityad id)
          in
          let for_security_role (sr : security_role) : security_role = g KIsecurityrole sr in
          let for_security_entry (sa : security_entry) =
            match sa with
            | Sany     -> Sany
            | Sentry l -> Sentry (List.map (g KIsecurityentry) l)
          in
          match sn with
          | SonlyByRole         (ad, srl)     -> SonlyByRole         (for_entry_description ad, List.map for_security_role srl)
          | SonlyInEntry       (ad, sa)       -> SonlyInEntry       (for_entry_description ad, for_security_entry sa)
          | SonlyByRoleInEntry (ad, srl, sa)  -> SonlyByRoleInEntry (for_entry_description ad, List.map for_security_role srl, for_security_entry sa)
          | SnotByRole          (ad, srl)     -> SnotByRole          (for_entry_description ad, List.map for_security_role srl)
          | SnotInEntry        (ad, sa)       -> SnotInEntry        (for_entry_description ad, for_security_entry sa)
          | SnotByRoleInEntry  (ad, srl, sa)  -> SnotByRoleInEntry  (for_entry_description ad, List.map for_security_role srl, for_security_entry sa)
          | StransferredBy      (ad)          -> StransferredBy      (for_entry_description ad)
          | StransferredTo      (ad)          -> StransferredTo      (for_entry_description ad)
          | SnoStorageFail      sa            -> SnoStorageFail      (for_security_entry sa)
        in
        {
          s_node = for_security_node sp.s_node;
          loc    = sp.loc;
        }
      in
      {
        label     = g KIlabel si.label;
        predicate = for_security_predicate si.predicate;
        loc       = si.loc;
      }
    in
    {
      items = List.map for_security_item s.items;
      loc   = s.loc;
    }
  in
  {
    name          = g KIarchetype model.name;
    api_items     = List.map for_api_item  model.api_items;
    api_verif     = List.map for_api_verif model.api_verif;
    decls         = List.map for_decl_node model.decls;
    storage       = List.map for_storage_item model.storage;
    functions     = List.map for_function__ model.functions;
    specification = for_specification model.specification;
    security      = for_security model.security;
    loc           = model.loc;
  }

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

  val get_vars                           : model -> var list
  val get_enums                          : model -> enum list
  val get_assets                         : model -> asset list
  val get_var                            : model -> ident -> var
  val get_enum                           : model -> ident -> enum
  val get_enum_values                    : model -> ident -> ident list
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
  val get_sum_idxs                       : model -> ident -> int list
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
  val eval                               : (ident * mterm) list -> mterm -> mterm
  val get_select_idx                     : model -> ident -> mterm -> int
  val get_sum_idx                        : model -> ident -> mterm -> int
  val with_division                      : model -> bool
  val with_min_max                       : model -> bool
  val with_count                         : model -> ident -> bool
  val get_asset_collection               : ident -> mterm
  val is_asset_single_field              : model -> ident -> bool
  val get_labeled_value_from             : model -> ident -> mterm list -> (ident * mterm) list
  val add_api_storage_in_list            : api_storage list -> api_storage -> api_storage list
  val sort_api_storage                   : model -> bool -> api_storage list -> api_storage list
end = struct

  open Tools
  open Location

  exception Anomaly of string

  type error_desc =
    | AssetFieldNotFound of string * string
    | AssetKeyTypeNotFound of string
    | ContainerNotFound
    | NotaRecord of mterm
    | NotanAssetType
    | NotFound
    | CurrencyValueCannotBeNegative
  [@@deriving show {with_path = false}]

  let emit_error (desc : error_desc) =
    let str = Format.asprintf "%a@." pp_error_desc desc in
    raise (Anomaly str)

  let emit_error2 (lc, error : Location.t * error_desc) =
    let str : string = Format.asprintf "%a@." pp_error_desc error in
    let pos : Position.t list = [location_to_position lc] in
    Error.error_alert pos str (fun _ -> ())

  let lident_to_string lident = Location.unloc lident

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
    | _                            -> false

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
  let get_enum_values m id : ident list  = get_enums m
                                           |> List.find (fun (x : enum)  -> cmp_ident id (unloc x.name))
                                           |> fun e -> e.values
                                                       |> List.map (fun (v : enum_item) -> unloc (v.name))
  let get_asset m id : asset = get_assets m |> List.find (fun (x : asset) -> cmp_ident id (unloc x.name))

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
    get_containers_internal (function | Tcontainer (Tasset _, (Partition | Aggregate)) -> true | _ -> false ) m

  let get_partitions m : (ident * ident * type_) list =
    get_containers_internal (function | Tcontainer (Tasset _, Partition) -> true | _ -> false ) m

  let has_container (m : model) (asset : ident) : bool =
    try
      let asset = get_asset m asset in
      List.fold_left (fun acc (v : asset_item) ->
          match v.type_ with
          | Tcontainer (Tasset _, (Partition | Aggregate)) -> true
          | _ -> acc
        ) false asset.values
    with
    | Not_found -> false

  let get_asset_containers (m : model) (asset : ident) : (ident * type_ * (lident mterm_gen option)) list =
    try
      let asset = get_asset m asset in
      List.fold_left (fun acc (v : asset_item) ->
          match v.type_ with
          | Tcontainer (Tasset _, (Partition | Aggregate)) -> acc @ [unloc v.name, v.type_, v.default]
          | _ -> acc
        ) [] asset.values
    with
    | Not_found -> []

  let dest_container = function
    | Tcontainer (Tasset p,(Partition | Aggregate)) -> unloc p
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
      | Massign (_, Avar i,_) when String.equal (unloc i) id -> raise FoundAssign
      | _ -> fold_term rec_search_assign false t in
    try rec_search_assign false b
    with FoundAssign -> true


  exception FoundOperations

  let with_operations_for_mterm_intern _ctx accu (mt : mterm) : bool =
    let rec aux accu (t : mterm) =
      match t.node with
      | Mtransfer  _
      | Mcallcontract _
      | Mcallentry _
      | Mcallself _
      | Moperations
      | Mmkoperation _
        -> raise FoundOperations
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
    | Mvar (_, Vlocal) -> true
    | _ -> false

  let dest_varlocal (t : mterm) =
    match t.node with
    |  Mvar (i, Vlocal) -> unloc i
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
      | Tbuiltin Bkeyhash    -> Maddress "tz1_default"
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
      | Tcontainer _ -> Massets []
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
    | Mvar(an, Vparam) ->
      begin
        let l, an = deloc an in
        let idparam = mkloc l (an ^ "_values") in
        Some (mk_mterm (Mvar(idparam, Vparam) ) (Tmap(Bint, Tasset (dumloc "myasset"))))
      end
    | _ -> None

  let eval (map_const_value : (ident * mterm) list) (mt : mterm) : mterm =
    let get_value (id : ident) : mterm = List.assoc id map_const_value in
    let is_const (id : ident) : bool = List.assoc_opt id map_const_value |> Option.is_some in
    let remove_const (mt : mterm) : mterm =
      let rec aux (mt : mterm) : mterm =
        match mt.node with
        | Mvar(v, Vstorevar)
        | Mvar(v, Vlocal) when is_const (unloc v) ->
          let dv = get_value (unloc v) in
          aux dv
        | _ -> map_mterm aux mt
      in
      aux mt
    in

    let mk_rat a b =
      let a, b = Core.compute_irr_fract (a, b) in
      let num   = mk_mterm (Mint a) (Tbuiltin Bint) in
      let denom = mk_mterm (Mint b) (Tbuiltin Bint) in
      mk_mterm (Mtuple [num; denom]) (Ttuple [Tbuiltin Bint; Tbuiltin Bint])
    in

    let is_int (mt : mterm) =
      match mt.type_ with
      | Tbuiltin Bint -> true
      | _ -> false
    in

    let is_tez (mt : mterm) =
      match mt.type_ with
      | Tbuiltin Bcurrency -> true
      | _ -> false
    in

    let is_timestamp (mt : mterm) =
      match mt.type_ with
      | Tbuiltin Btimestamp -> true
      | _ -> false
    in


    let eval_expr mt :
      mterm =
      let rec aux (mt : mterm) : mterm =
        let extract_int (i : mterm) : Big_int.big_int =
          let i = aux i in
          match i.node with
          | Mint v -> v
          | _ -> assert false
        in

        let extract_bool (b : mterm) : bool =
          let b = aux b in
          match b.node with
          | Mbool v -> v
          | _ -> assert false
        in

        let extract_string (b : mterm) : string =
          let b = aux b in
          match b.node with
          | Mstring v -> v
          | _ -> assert false
        in

        let extract_rat (rat : mterm) : Big_int.big_int * Big_int.big_int =
          let rat = aux rat in
          match rat.node with
          | Mrational (num, denom)
          | Mtuple [{node = Mint num; _}; {node = Mint denom; _}] -> (num, denom)
          | _ -> assert false
        in

        let extract_tez (b : mterm) : Big_int.big_int =
          let b = aux b in
          match b.node with
          | Mcurrency (v, Utz) -> v
          | Mcurrency (v, Mtz) -> Big_int.mult_int_big_int 1000 v
          | Mcurrency (v, Tz)  -> Big_int.mult_int_big_int 1000000 v
          | _ -> assert false
        in

        let extract_timestamp (b : mterm) : Big_int.big_int =
          let b = aux b in
          match b.node with
          | Mtimestamp v -> v
          | _ -> assert false
        in

        let arith op (a, b) : mterm =
          let a = extract_int a in
          let b = extract_int b in

          let res =
            match op with
            | `Plus   -> Big_int.add_big_int a b
            | `Minus  -> Big_int.sub_big_int a b
            | `Mult   -> Big_int.mult_big_int a b
            | `Ediv   -> Big_int.div_big_int a b
            | `Modulo -> Big_int.mod_big_int a b
            | _ -> assert false
          in
          mk_mterm (Mint res) (Tbuiltin Bint)
        in

        match mt.node, mt.type_ with
        | Mplus   (a, b), Tbuiltin Bstring -> begin
            let a = extract_string a in
            let b = extract_string b in
            mk_mterm (Mstring (a ^ b)) (Tbuiltin Bstring)
          end
        | Mplus   (a, b), Tbuiltin Bcurrency when is_tez a && is_tez b -> begin
            let a = extract_tez a in
            let b = extract_tez b in
            mk_mterm (Mcurrency (Big_int.add_big_int a b, Utz)) (Tbuiltin Bcurrency)
          end
        | Mplus   (a, b), _ when is_timestamp a && is_int b -> begin
            let a = extract_timestamp a in
            let b = extract_int b in
            mk_mterm (Mtimestamp (Big_int.add_big_int a b)) (Tbuiltin Btimestamp)
          end
        | Mplus   (a, b), _ -> arith `Plus  (aux a, aux b)
        | Mminus  (a, b), Tbuiltin Bcurrency when is_tez a && is_tez b -> begin
            let a = extract_tez a in
            let b = extract_tez b in
            let res = Big_int.sub_big_int a b in
            if Big_int.sign_big_int res < 0 then emit_error2(mt.loc, CurrencyValueCannotBeNegative);
            mk_mterm (Mcurrency (res, Utz)) (Tbuiltin Bcurrency)
          end
        | Mminus  (a, b), _ when is_timestamp a && is_timestamp b -> begin
            let a = extract_timestamp a in
            let b = extract_timestamp b in
            let res = Big_int.sub_big_int a b in
            mk_mterm (Mint res) (Tbuiltin Bint)
          end
        | Mminus  (a, b), _ -> arith `Minus (aux a, aux b)
        | Mmult   (a, b), _ -> arith `Mult  (aux a, aux b)
        | Mdiveuc (a, b), _ -> arith `Ediv  (aux a, aux b)
        | Mmodulo (a, b), _ -> arith `Modulo   (aux a, aux b)
        | Mnot     a    , _ -> mk_mterm (Mbool (not (extract_bool (aux a)))) (Tbuiltin Bbool)
        | Mand    (a, b), _ -> mk_mterm (Mbool ((extract_bool (aux a)) && (extract_bool (aux b)))) (Tbuiltin Bbool)
        | Mor     (a, b), _ -> mk_mterm (Mbool ((extract_bool (aux a)) || (extract_bool (aux b)))) (Tbuiltin Bbool)
        | Mrateq  (a, b), _ -> begin
            let num1, denom1 = extract_rat (aux a) in
            let num2, denom2 = extract_rat (aux b) in
            let res = Big_int.eq_big_int (Big_int.mult_big_int num1 denom2) (Big_int.mult_big_int num2 denom1) in
            mk_mterm (Mbool res) (Tbuiltin Bbool)
          end
        | Mratarith (op, a, b), _ -> begin
            let num1, denom1 = extract_rat (aux a) in
            let num2, denom2 = extract_rat (aux b) in
            match op with
            | Rplus  -> mk_rat (Big_int.add_big_int (Big_int.mult_big_int num1 denom2) (Big_int.mult_big_int num2 denom1)) (Big_int.mult_big_int denom1 denom2)
            | Rminus -> mk_rat (Big_int.sub_big_int (Big_int.mult_big_int num1 denom2) (Big_int.mult_big_int num2 denom1)) (Big_int.mult_big_int denom1 denom2)
            | Rmult  -> mk_rat (Big_int.mult_big_int num1 num2) (Big_int.mult_big_int denom1 denom2)
            | Rdiv   -> mk_rat (Big_int.mult_big_int num1 denom2) (Big_int.mult_big_int num2 denom1)
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
        | Mrattez (coef, c), _ ->
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
            | _ -> begin
                Format.eprintf "%a@." pp_mterm mt;
                assert false
              end
          end
        | _ -> map_mterm aux mt
      in
      aux mt
    in

    mt
    |> remove_const
    |> eval_expr

  type searchfun =
    | SearchSelect
    | SearchSum

  let get_fun_idx typ (m : model) asset expr =
    let rec internal_get_fun_idx acc = function
      | (sc : api_storage) :: tl ->
        begin
          match typ, sc.node_item with
          | SearchSelect, APIAsset (Select (a, _, _, t)) -> continue_internal_get_fun_idx tl acc a t
          | SearchSum, APIAsset (Sum (a, _, _, t)) -> continue_internal_get_fun_idx tl acc a t
          | _ -> internal_get_fun_idx acc tl
        end
      | [] -> acc
    and continue_internal_get_fun_idx tl acc a t =
      if compare a asset = 0 then
        if cmp_mterm t expr then
          acc + 1
        else
          internal_get_fun_idx (acc + 1) tl
      else
        internal_get_fun_idx acc tl
    in
    internal_get_fun_idx 0 m.api_items

  let get_select_idx = get_fun_idx SearchSelect

  let get_sum_idx = get_fun_idx SearchSum

  let get_sum_idxs m a = (* TODO *)
    List.fold_left (fun acc (ai : api_storage) ->
        match ai.node_item with
        | APIAsset (Sum (asset, _, _type, formula)) when String.equal a asset ->
          acc @ [get_sum_idx m a formula]
        | _ -> acc
      ) [] m.api_items

  exception FoundDiv

  let with_div_for_mterm_intern _ctx accu (mt : mterm) : bool =
    let rec aux accu (t : mterm) =
      match t.node with
      | Mdivrat _ | Mdiveuc _ -> raise FoundDiv
      | Mmodulo _ -> raise FoundDiv
      | Massign (DivAssign,_,_) -> raise FoundDiv
      | _ -> fold_term aux accu t in
    aux accu mt

  let with_division (model : model) : bool =
    (try fold_model with_div_for_mterm_intern model false
     with FoundDiv -> true) || (
      List.fold_left (fun acc (ai : api_storage) ->
          match ai.node_item with
          | APIInternal RatTez ->
            acc || true
          | _ -> acc
        ) false model.api_items
    )

  exception FoundMinMax

  let with_minmax_for_mterm_intern _ctx accu (mt : mterm) : bool =
    let rec aux accu (t : mterm) =
      match t.node with
      | Mmax (_,_) -> raise FoundMinMax
      | Mmin (_,_) -> raise FoundMinMax
      | _ -> fold_term aux accu t in
    aux accu mt

  let with_min_max (model : model) : bool =
    (try fold_model with_minmax_for_mterm_intern model false
     with FoundMinMax -> true)

  let with_count m a =
    List.fold_left (fun acc (ai : api_storage) ->
        match ai.node_item with
        | APIAsset (Count (asset, _)) when String.equal a asset ->
          acc || true
        | _ -> acc
      ) false m.api_items

  let get_asset_collection (an : ident) : mterm =
    mk_mterm (Mvar (dumloc an, Vstorecol)) (Tcontainer (Tasset (dumloc an), Collection))

  let is_asset_single_field (model : model) (an : ident) : bool =
    get_asset model an |> fun x -> x.values |> List.filter (fun (x : asset_item) -> not x.shadow) |> List.length = 1

  let get_labeled_value_from (model : model) (an : ident) (values : mterm list) : (ident * mterm) list =
    let asset = get_asset model an in
    List.map2 (fun (x : asset_item) (y : mterm) -> unloc x.name, y) asset.values values

  let add_api_storage_in_list (l : api_storage list) (i :  api_storage) =
    let res, l = List.fold_left (fun (res, accu) (x : api_storage) ->
        if cmp_api_item_node x.node_item i.node_item
        then (true,
              { i with api_loc =
                         match x.api_loc, i.api_loc with
                         | _, ExecFormula
                         | ExecFormula, _
                         | OnlyExec, OnlyFormula
                         | OnlyFormula, OnlyExec -> ExecFormula
                         | _ -> i.api_loc
              }::accu)
        else (res, x::accu)) (false, []) l in
    if res then
      l
    else
      i::l

  let sort_api_storage (model : model) (verif : bool) (l : api_storage list) : api_storage list =
    l
    |> List.sort
      (fun (i1 : api_storage) (i2 : api_storage) ->
         let criteria_asset_name () : int =
           let default = "_" in
           let get_asset_name = function
             | APIAsset (Get           an)         -> an
             | APIAsset (Set           an)         -> an
             | APIAsset (Add           an)         -> an
             | APIAsset (Remove        an)         -> an
             | APIAsset (Clear        (an, _))     -> an
             | APIAsset (Update       (an, _))     -> an
             | APIAsset (FieldAdd    (an, _))      -> an
             | APIAsset (FieldRemove (an, _))      -> an
             | APIAsset (RemoveAll  (an, _))       -> an
             | APIAsset (RemoveIf   (an, _, _, _)) -> an
             | APIList _                           -> default
             | APIBuiltin _                        -> default
             | APIInternal _                       -> default
             | APIAsset (Contains   (an, _))       -> an
             | APIAsset (Nth        (an, _))       -> an
             | APIAsset (Select     (an, _, _, _)) -> an
             | APIAsset (Sort       (an, _, _))    -> an
             | APIAsset (Count      (an, _))       -> an
             | APIAsset (Sum        (an, _, _, _)) -> an
             | APIAsset (Head       (an, _))       -> an
             | APIAsset (Tail       (an, _))       -> an
           in
           let asset_list : ident list = List.fold_left (fun accu (x : decl_node) ->
               match x with
               | Dasset r -> accu @ [unloc r.name]
               | _ -> accu
             ) [] model.decls in
           let get_idx (i : api_storage) = List.index_of (fun x -> String.equal (get_asset_name i.node_item) x) asset_list in
           let idx1 = get_idx i1 in
           let idx2 = get_idx i2 in
           idx1 - idx2
         in

         let criteria_kind () : int =
           let get_kind = function
             | APIInternal (RatEq         ) ->  1
             | APIInternal (RatCmp        ) ->  2
             | APIInternal (RatArith      ) ->  3
             | APIInternal (RatUminus     ) ->  4
             | APIInternal (RatTez        ) ->  5
             | APIInternal (DivTez        ) ->  6
             | APIInternal (RatDur        ) ->  7
             | APIAsset   (Nth           _) -> if verif then 8 else 9
             | APIAsset   (Get           _) -> if verif then 9 else 8
             | APIAsset   (Set           _) -> 10
             | APIAsset   (Add           _) -> 11
             | APIAsset   (Remove        _) -> 12
             | APIAsset   (Clear         _) -> 13
             | APIAsset   (Update        _) -> 14
             | APIAsset   (FieldAdd      _) -> 15
             | APIAsset   (FieldRemove   _) -> 16
             | APIAsset   (RemoveAll     _) -> 17
             | APIAsset   (RemoveIf      _) -> 18
             | APIAsset   (Contains      _) -> 19
             | APIAsset   (Select        _) -> 20
             | APIAsset   (Sort          _) -> 21
             | APIAsset   (Count         _) -> 22
             | APIAsset   (Sum           _) -> 23
             | APIAsset   (Head          _) -> 24
             | APIAsset   (Tail          _) -> 25
             | APIList    (Lprepend      _) -> 26
             | APIList    (Lcontains     _) -> 27
             | APIList    (Llength       _) -> 28
             | APIList    (Lnth          _) -> 29
             | APIBuiltin (Bmin          _) -> 30
             | APIBuiltin (Bmax          _) -> 31
             | APIBuiltin (Babs          _) -> 32
             | APIBuiltin (Bconcat       _) -> 33
             | APIBuiltin (Bslice        _) -> 34
             | APIBuiltin (Blength       _) -> 35
             | APIBuiltin (Bisnone       _) -> 36
             | APIBuiltin (Bissome       _) -> 37
             | APIBuiltin (Bgetopt       _) -> 38
             | APIBuiltin (Bfloor         ) -> 39
             | APIBuiltin (Bceil          ) -> 40
           in
           let idx1 = get_kind i1.node_item in
           let idx2 = get_kind i2.node_item in
           idx1 - idx2
         in

         let c1 = criteria_asset_name () in
         if c1 = 0
         then criteria_kind ()
         else c1
      )
end
