open Ident
open Tools
open Location

type lident = ident Location.loced
[@@deriving show {with_path = false}]

type namespace = lident option
[@@deriving show {with_path = false}]

type path = namespace * lident
[@@deriving show {with_path = false}]

type mident = path
[@@deriving show {with_path = false}]

type container =
  | Collection
  | Aggregate
  | Partition
  | AssetContainer
  | AssetKey
  | AssetValue
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
  | Btez
  | Bsignature
  | Bkey
  | Bkeyhash
  | Bbytes
  | Bnat
  | Bchainid
  | Bbls12_381_fr
  | Bbls12_381_g1
  | Bbls12_381_g2
  | Bnever
  | Bchest
  | Bchest_key
[@@deriving show {with_path = false}]

type ntype =
  | Tasset of mident
  | Tenum of mident
  | Tstate
  | Tbuiltin of btyp
  | Tcontainer of type_ * container
  | Tlist of type_
  | Toption of type_
  | Ttuple of type_ list
  | Tset of type_
  | Tmap of type_ * type_
  | Tbig_map of type_ * type_
  | Titerable_big_map of type_ * type_
  | Tor of type_ * type_
  | Trecord of mident
  | Tevent of mident
  | Tlambda of type_ * type_
  | Tunit
  | Toperation
  | Tcontract of type_
  | Tticket of type_
  | Tsapling_state of int
  | Tsapling_transaction of int
[@@deriving show {with_path = false}]

and type_ = ntype * lident option
[@@deriving show {with_path = false}]

type pattern_node =
  | Pwild
  | Pconst of mident * lident list
[@@deriving show {with_path = false}]

type pattern = {
  node: pattern_node;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type for_ident =
  | FIsimple of mident
  | FIdouble of mident * mident
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

type sort_kind =
  | SKasc
  | SKdesc
[@@deriving show {with_path = false}]

type 'term assign_kind_gen =
  | Avar         of mident
  | Avarstore    of mident
  | Aasset       of mident * mident * 'term (* asset name * field name * key *)
  | Arecord      of 'term * mident * mident (* record * record name * field name *)
  | Atuple       of 'term * int * int (* tuple * index * length *)
  | Astate
  | Aoperations
[@@deriving show {with_path = false}]

type 'term var_kind_gen =
  | Vassetstate of 'term
  | Vstorevar
  | Vstorecol
  | Vlocal
  | Vparam
  | Vfield
  | Vstate
  | Vthe
  | Vparameter
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

type 'term transfer_kind_gen =
  | TKsimple    of 'term * 'term                                  (* dest *)
  | TKcall      of 'term * ident * type_ * 'term * 'term          (* entry_id * type_entry * dest * args *)
  | TKentry     of 'term * 'term * 'term                          (* entry * arg *)
  | TKgen       of 'term * ident * ident * type_ * 'term * 'term  (* entry_id * contract_id * addr * args *)
  | TKself      of 'term * ident * (ident * 'term) list           (* entry_id * args *)
  | TKoperation of 'term
[@@deriving show {with_path = false}]

type map_kind =
  | MKMap
  | MKBigMap
  | MKIterableBigMap
[@@deriving show {with_path = false}]

type michelson_struct = {
  ms_content: Michelson.obj_micheline
}
[@@deriving show {with_path = false}]

type 'term detach_kind_gen =
  | DK_option of type_ * ident
  | DK_map of type_ * ident * 'term
[@@deriving show {with_path = false}]

type 'term letin_value_gen =
  | LVsimple of 'term
  | LVreplace of mident * 'term detach_kind_gen * 'term
[@@deriving show {with_path = false}]

type 'term mterm_node  =
  (* lambda *)
  | Mletin            of mident list * 'term letin_value_gen * type_ option * 'term * 'term option
  | Mdeclvar          of mident list * type_ option * 'term * bool
  | Mdeclvaropt       of mident list * type_ option * 'term * 'term option * bool
  | Mapp              of mident * 'term list
  (* assign *)
  | Massign           of (assignment_operator * type_ * 'term assign_kind_gen * 'term) (* assignment kind value*)
  | Massignopt        of (assignment_operator * type_ * 'term assign_kind_gen * 'term * 'term) (* assignment kind value*)
  (* control *)
  | Mif               of ('term * 'term * 'term option)
  | Mmatchwith        of 'term * (pattern * 'term) list
  | Minstrmatchoption of 'term * mident * 'term * 'term
  | Minstrmatchor     of 'term * mident * 'term * mident * 'term
  | Minstrmatchlist   of 'term * mident * mident * 'term * 'term
  | Mfor              of (for_ident * 'term iter_container_kind_gen * 'term)
  | Miter             of (mident * 'term * 'term * 'term * bool (* true of id is nat else int *))
  | Mwhile            of ('term * 'term)
  | Mseq              of 'term list
  | Mreturn           of 'term
  (* effect *)
  | Mfail             of fail_type
  | Mfailsome         of 'term
  | Mtransfer         of 'term transfer_kind_gen
  | Memit             of mident * 'term
  | Mdetach           of mident * 'term detach_kind_gen * type_ * 'term
  (* entrypoint *)
  | Mgetentrypoint    of type_ * mident * 'term                 (* type * address * string *)
  | Mcallview         of type_ * 'term * mident * 'term         (* type * address * string * argument *)
  | Mimportcallview   of type_ * 'term * mident * 'term         (* type * address * string * argument *)
  | Mself             of mident                                 (* entryname *)
  (* operation *)
  | Moperations
  | Mmakeoperation    of 'term * 'term * 'term  (* value * address * args *)
  | Mmakeevent        of type_ * mident * 'term    (* type * id * arg *)
  | Mcreatecontract   of michelson_struct * 'term * 'term * 'term  (* value * option key_hash * address * init storage *)
  (* literals *)
  | Mint              of Core.big_int
  | Mnat              of Core.big_int
  | Mbool             of bool
  | Mrational         of Core.big_int * Core.big_int
  | Mstring           of string
  | Mmutez            of Core.big_int
  | Maddress          of string
  | Mdate             of Core.date
  | Mduration         of Core.duration
  | Mtimestamp        of Core.big_int
  | Mbytes            of string
  | Mchain_id         of string
  | Mkey              of string
  | Mkey_hash         of string
  | Msignature        of string
  | Mbls12_381_fr     of string
  | Mbls12_381_fr_n   of Core.big_int
  | Mbls12_381_g1     of string
  | Mbls12_381_g2     of string
  | Munit
  | MsaplingStateEmpty of int
  | MsaplingTransaction of int * string
  | Mchest            of string
  | Mchest_key        of string
  | Mtz_expr          of string
  (* control expression *)
  | Mexprif           of 'term * 'term * 'term
  | Mexprmatchwith    of 'term * (pattern * 'term) list
  | Mmatchoption      of 'term * mident * 'term * 'term
  | Mmatchor          of 'term * mident * 'term * mident * 'term
  | Mmatchlist        of 'term * mident * mident * 'term * 'term
  | Mternarybool      of 'term * 'term * 'term
  | Mternaryoption    of 'term * 'term * 'term
  | Mfold             of 'term * mident * 'term
  | Mmap              of 'term * mident * 'term
  | Mexeclambda       of 'term * 'term
  | Mapplylambda      of 'term * 'term
  (* composite type constructors *)
  | Mleft             of type_ * 'term
  | Mright            of type_ * 'term
  | Mnone
  | Msome             of 'term
  | Mtuple            of 'term list
  | Masset            of 'term list
  | Massets           of 'term list
  | Mlitset           of 'term list
  | Mlitlist          of 'term list
  | Mlitmap           of map_kind * ('term * 'term) list
  | Mlitrecord        of (ident * 'term) list
  | Mlitevent         of (ident * 'term) list
  | Mlambda           of type_ * mident * type_ * 'term
  (* access *)
  | Mdot              of 'term * mident
  | Mdotassetfield    of mident * 'term * mident
  | Mquestionoption   of 'term * mident
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
  | Mgreedyand        of 'term * 'term
  | Mgreedyor         of 'term * 'term
  | Mxor              of 'term * 'term
  | Mnot              of 'term
  | Mplus             of 'term * 'term
  | Mminus            of 'term * 'term
  | Mmult             of 'term * 'term
  | Mdivrat           of 'term * 'term
  | Mdiveuc           of 'term * 'term
  | Mmodulo           of 'term * 'term
  | Mdivmod           of 'term * 'term
  | Muminus           of 'term
  | MthreeWayCmp      of 'term * 'term
  | Mshiftleft        of 'term * 'term
  | Mshiftright       of 'term * 'term
  | Msubnat           of 'term * 'term
  | Msubmutez         of 'term * 'term
  (* asset api effect *)
  | Maddasset         of ident * 'term
  | Mputsingleasset   of ident * 'term
  | Mputasset         of ident * 'term * 'term
  | Maddfield         of ident * ident * 'term * 'term (* asset_name * field_name * asset instance * item *)
  | Mremoveasset      of ident * 'term
  | Mremovefield      of ident * ident * 'term * 'term
  | Mremoveall        of ident * 'term container_kind_gen
  | Mremoveif         of ident * 'term container_kind_gen * (ident * type_) list * 'term * 'term list (* asset_name, view, lambda (args, body, apply_args) *)
  | Mclear            of ident * 'term container_kind_gen
  | Mset              of ident * ident list * 'term * 'term (*asset_name * field_name modified * ... *)
  | Mupdate           of ident * 'term * (mident * assignment_operator * 'term) list
  | Mupdateall        of ident * 'term container_kind_gen * (mident * assignment_operator * 'term) list
  | Maddupdate        of ident * 'term container_kind_gen * 'term * (mident * assignment_operator * 'term) list
  | Mputremove        of ident * 'term container_kind_gen * 'term * 'term
  (* asset api expression *)
  | Mget              of ident * 'term container_kind_gen * 'term
  | Mgetsome          of ident * 'term container_kind_gen * 'term
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
  | Mrecupdate        of 'term * (ident * 'term) list
  | Mmakeasset        of ident * 'term * 'term
  | Mtocontainer      of ident
  | Mglobal_constant  of type_ * 'term
  (* set api expression *)
  | Msetadd           of type_ * 'term * 'term
  | Msetremove        of type_ * 'term * 'term
  | Msetupdate        of type_ * 'term * 'term * 'term
  | Msetcontains      of type_ * 'term * 'term
  | Msetlength        of type_ * 'term
  | Msetfold          of type_ * mident   * mident   * 'term * 'term * 'term
  (* set api instruction *)
  | Msetinstradd      of type_ * 'term assign_kind_gen * 'term
  | Msetinstrremove   of type_ * 'term assign_kind_gen * 'term
  (* list api expression *)
  | Mlistprepend      of type_ * 'term * 'term
  | Mlistlength       of type_ * 'term
  | Mlistcontains     of type_ * 'term * 'term
  | Mlistnth          of type_ * 'term * 'term
  | Mlisthead         of type_ * 'term * 'term
  | Mlisttail         of type_ * 'term * 'term
  | Mlistreverse      of type_ * 'term
  | Mlistconcat       of type_ * 'term * 'term
  | Mlistfold         of type_ * mident   * mident   * 'term * 'term * 'term
  (* list api instruction *)
  | Mlistinstrprepend of type_ * 'term assign_kind_gen * 'term
  | Mlistinstrconcat  of type_ * 'term assign_kind_gen * 'term
  (* map api expression *)
  | Mmapput           of map_kind * type_ * type_ * 'term * 'term * 'term
  | Mmapremove        of map_kind * type_ * type_ * 'term * 'term
  | Mmapupdate        of map_kind * type_ * type_ * 'term * 'term * 'term
  | Mmapget           of map_kind * type_ * type_ * 'term * 'term * ident option
  | Mmapgetopt        of map_kind * type_ * type_ * 'term * 'term
  | Mmapcontains      of map_kind * type_ * type_ * 'term * 'term
  | Mmaplength        of map_kind * type_ * type_ * 'term
  | Mmapfold          of map_kind * type_ * mident   * mident   * mident   * 'term * 'term * 'term
  (* map api instruction *)
  | Mmapinstrput      of map_kind * type_ * type_ * 'term assign_kind_gen * 'term * 'term
  | Mmapinstrremove   of map_kind * type_ * type_ * 'term assign_kind_gen * 'term
  | Mmapinstrupdate   of map_kind * type_ * type_ * 'term assign_kind_gen * 'term * 'term
  (* builtin functions *)
  | Mmin              of 'term * 'term
  | Mmax              of 'term * 'term
  | Mabs              of 'term
  | Mconcat           of 'term * 'term
  | Mconcatlist       of 'term
  | Mslice            of 'term * 'term * 'term
  | Mlength           of 'term
  | Misnone           of 'term
  | Missome           of 'term
  | Minttonat         of 'term
  | Mfloor            of 'term
  | Mceil             of 'term
  | Mnattostring      of 'term
  | Mbytestonat       of 'term
  | Mnattobytes       of 'term
  | Mbytestoint       of 'term
  | Minttobytes       of 'term
  | Mpack             of 'term
  | Munpack           of type_ * 'term
  | Msetdelegate      of 'term
  | Mkeyhashtocontract of 'term
  | Mcontracttoaddress of 'term
  | Maddresstocontract  of type_ * 'term
  | Mkeytoaddress      of 'term
  | Msimplify_rational of 'term
  | Mget_numerator     of 'term
  | Mget_denominator   of 'term
  | Misimplicitaddress of 'term
  (* crypto functions *)
  | Mblake2b          of 'term
  | Msha256           of 'term
  | Msha512           of 'term
  | Msha3             of 'term
  | Mkeccak           of 'term
  | Mkeytokeyhash     of 'term
  | Mchecksignature   of 'term * 'term * 'term
  (* voting *)
  | Mtotalvotingpower
  | Mvotingpower      of 'term
  (* ticket *)
  | Mcreateticket     of 'term * 'term
  | Mreadticket       of 'term
  | Msplitticket      of 'term * 'term * 'term
  | Mjointickets      of 'term * 'term
  (* sapling *)
  | Msapling_empty_state   of int
  | Msapling_verify_update of 'term * 'term
  (* bls curve *)
  | Mpairing_check of 'term
  (* timelock *)
  | Mopen_chest of 'term * 'term * 'term
  (* constants *)
  | Mnow
  | Mtransferred
  | Mcaller
  | Mbalance
  | Msource
  | Mselfaddress
  | Mselfchainid
  | Mmetadata
  | Mlevel
  | Mminblocktime
  (* variable *)
  | Mvar              of mident * 'term var_kind_gen
  | Menumval          of mident * 'term list * mident  (* value * args * ident of enum *)
  (* rational *)
  | Mrateq            of 'term * 'term
  | Mratcmp           of comparison_operator * 'term * 'term
  | Mratarith         of rat_arith_op * 'term * 'term
  | Mratuminus        of 'term
  | Mrattez           of 'term * 'term
  | Mnattoint         of 'term
  | Mnattorat         of 'term
  | Minttorat         of 'term
  | Mratdur           of 'term * 'term
  (* others *)
  | Minttodate        of 'term
  | Mmuteztonat       of 'term
[@@deriving show {with_path = false}]

and assign_kind = mterm assign_kind_gen

and var_kind = mterm var_kind_gen

and container_kind = mterm container_kind_gen

and iter_container_kind = mterm iter_container_kind_gen

and transfer_kind = mterm transfer_kind_gen

and mterm_gen = {
  node: mterm_gen mterm_node;
  type_: type_;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

and mterm = mterm_gen
[@@deriving show {with_path = false}]

and mterm__node = mterm mterm_node
[@@deriving show {with_path = false}]

and fail_type =
  | Invalid of mterm
  | InvalidCaller
  | InvalidSource
  | InvalidCondition of ident * mterm option
  | NotFound
  | AssetNotFound of ident
  | KeyExists of ident
  | KeyExistsOrNotFound of ident
  | DivByZero
  | NatNegAssign
  | NoTransfer
  | InvalidState
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
  | RemoveAll        of ident * api_container_kind
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
  | Lreverse         of type_
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
  | Boptget of type_
  | Bfloor
  | Bceil
  | Bnattostring
  | Bfail of type_
[@@deriving show {with_path = false}]

and api_internal =
  | RatEq
  | RatCmp
  | RatArith
  | RatUminus
  | RatTez
  | RatDur
[@@deriving show {with_path = false}]

and api_storage_node =
  | APIAsset      of api_asset
  | APIList       of api_list
  | APIBuiltin    of api_builtin
  | APIInternal   of api_internal
[@@deriving show {with_path = false}]

and api_storage = {
  node_item: api_storage_node;
}
[@@deriving show {with_path = false}]

and letin_value = mterm letin_value_gen
[@@deriving show {with_path = false}]

and detach_kind = mterm detach_kind_gen
[@@deriving show {with_path = false}]

type model_type =
  | MTvar
  | MTconst
  | MTasset of mident
  | MTstate
  | MTenum of ident
[@@deriving show {with_path = false}]

type storage_item = {
  id          : mident;
  model_type  : model_type;
  typ         : type_;
  const       : bool;
  ghost       : bool;
  default     : mterm; (* initial value *)
  namespace   : ident option;
  no_storage  : bool;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type storage = storage_item list
[@@deriving show {with_path = false}]

type enum_item = {
  name: mident;
  args: type_ list;
}
[@@deriving show {with_path = false}]

type variable_kind =
  | VKconstant
  | VKvariable
[@@deriving show {with_path = false}]

type var = {
  name: mident;
  type_: type_;
  original_type: type_;
  kind: variable_kind;
  default: mterm option;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type enum = {
  name: mident;
  values: enum_item list;
  initial: mident;
}
[@@deriving show {with_path = false}]

type asset_item = {
  name: mident;
  type_: type_;
  original_type: type_;
  default: mterm option;
  shadow: bool;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type asset = {
  name: mident;
  values: asset_item list;
  keys: ident list;
  sort: mident list;
  map_kind: map_kind;
  state: lident option;
  init: mterm list;
  no_storage: bool;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type position =
  | Ptuple of ident list
  | Pnode of position list
[@@deriving show {with_path = false}]

type record_field = {
  name: mident;
  type_: type_;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type record = {
  name: mident;
  fields: record_field list;
  pos: position;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type function_ = {
  name: mident;
}
[@@deriving show {with_path = false}]

type entry = {
  name: mident;
}
[@@deriving show {with_path = false}]

type argument = mident * type_ * mterm option
[@@deriving show {with_path = false}]

type function_struct = {
  name:  mident;
  args:  argument list;
  eargs: argument list;
  stovars: ident list;
  body:  mterm;
  loc :  Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type view_visibility =
  | VVonchain
  | VVoffchain
  | VVonoffchain
[@@deriving show {with_path = false}]

type function_node =
  | Function           of function_struct * type_ (* fun * return type *)
  | Getter             of function_struct * type_
  | View               of function_struct * type_ * view_visibility
  | Entry              of function_struct
[@@deriving show {with_path = false}]

type signature = {
  name: mident;
  args: argument list;
  ret: type_ option;
}
[@@deriving show {with_path = false}]

type function__ = {
  node:  function_node;
}
[@@deriving show {with_path = false}]

type decl_node =
  | Dvar    of var
  | Denum   of enum
  | Dasset  of asset
  | Drecord of record
  | Devent  of record
[@@deriving show {with_path = false}]

type parameter = {
  name    : mident;
  typ     : type_;
  default : mterm option;
  value   : mterm option;
  const   : bool;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type metadata_kind =
  | MKuri  of string loced
  | MKjson of string loced
[@@deriving show {with_path = false}]

type odel_asset = {
  name: mident;
  container_type: type_;
  key_type: type_;
  value_type: type_;
}
[@@deriving show {with_path = false}]

type odel_record = {
  name: mident;
  current_type: type_;
}
[@@deriving show {with_path = false}]

type odel_enum = {
  name: mident;
  current_type: type_;
}
[@@deriving show {with_path = false}]

type original_decl =
  | ODAsset of odel_asset
  | ODRecord of odel_record
  | ODEnum of odel_enum
[@@deriving show {with_path = false}]

type extra = {
  original_decls: original_decl list
}
[@@deriving show {with_path = false}]

type model = {
  name          : lident;
  parameters    : parameter list;
  metadata      : metadata_kind option;
  api_items     : api_storage list;
  decls         : decl_node list;
  storage       : storage;
  functions     : function__ list;
  extra         : extra;
  loc           : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

let mk_mident ?namespace id : mident =
  (namespace, id)

let unloc_mident (id : mident) : ident =
  unloc (snd id)

let loc_mident (id : mident) : Location.t =
  loc (snd id)

let deloc_mident (id : mident) : Location.t * ident =
  deloc (snd id)

let mk_pattern ?(loc = Location.dummy) node : pattern =
  { node; loc}

let mk_mterm ?(loc = Location.dummy) node type_ : mterm =
  { node; type_; loc}

let mk_var ?default ?(loc = Location.dummy) name type_ original_type kind : var =
  { name; type_; default; kind; original_type; loc }

let mk_enum ?(values = []) name initial : enum =
  { name; values; initial }

let mk_enum_item ?(args = []) name : enum_item =
  { name; args }

let mk_asset ?(values = []) ?(sort=[]) ?(map_kind = MKMap) ?state ?(keys = []) ?(init = []) ?(loc = Location.dummy) ?(no_storage = false) name : asset =
  { name; values; sort; map_kind; state; keys; init; no_storage; loc }

let mk_asset_item ?default ?(shadow=false) ?(loc = Location.dummy) name type_ original_type : asset_item =
  { name; type_; original_type; default; shadow; loc }

let mk_record ?(fields = []) ?(pos = Pnode []) ?(loc = Location.dummy) name : record =
  { name; fields; pos; loc }

let mk_record_field ?(loc = Location.dummy) name type_ : record_field =
  { name; type_; loc }

let mk_storage_item ?(const=false) ?(ghost = false) ?namespace ?(no_storage = false) ?(loc = Location.dummy) id model_type typ default : storage_item =
  { id; model_type; typ; const; ghost; default; namespace; no_storage; loc }

let mk_function_struct ?(args = []) ?(eargs = []) ?(stovars = []) ?(loc = Location.dummy) name body : function_struct =
  { name; args; eargs; stovars; body; loc }

let mk_function node : function__ =
  { node }

let mk_signature ?(args = []) ?ret name : signature =
  { name; args; ret }

let mk_api_item node_item =
  { node_item }

let mk_odel_asset name container_type key_type value_type : odel_asset =
  { name; container_type; key_type; value_type }

let mk_odel_record name current_type : odel_record =
  { name; current_type }

let mk_odel_enum name current_type : odel_enum =
  { name; current_type }

let mk_extra ?(original_decls = []) () : extra =
  { original_decls }

let mk_model ?(parameters = []) ?metadata ?(api_items = []) ?(decls = []) ?(functions = []) ?(storage = []) ?(extra = mk_extra ()) ?(loc = Location.dummy) name : model =
  { name; parameters; metadata; api_items; storage; decls; functions; extra; loc }

(* -------------------------------------------------------------------- *)

let gen_mident s (nm, id : mident) : ident =
  match nm with
  | Some nm -> (unloc nm) ^ s ^ (unloc id)
  | None -> unloc id

let normalize_mident = gen_mident "::"

let printable_mident = gen_mident "__"

let string_to_mident ?(nm : string option = None) id : mident = (Option.map dumloc nm, dumloc id)

(* -------------------------------------------------------------------- *)

let mktype ?annot n : type_ = n, annot
let get_ntype t : ntype = fst t
let get_atype t : lident option = snd t
let mkannot prefix (id : lident) : lident option = match unloc id with | "" -> None | v -> Some (mkloc (loc id) (prefix ^ v))
let mkfannot = mkannot "%"
let mkvannot = mkannot "@"

let tunit                  = mktype (Tunit)
let tbool                  = mktype (Tbuiltin Bbool)
let tnat                   = mktype (Tbuiltin Bnat)
let tint                   = mktype (Tbuiltin Bint)
let tstring                = mktype (Tbuiltin Bstring)
let tbytes                 = mktype (Tbuiltin Bbytes)
let ttez                   = mktype (Tbuiltin Btez)
let tduration              = mktype (Tbuiltin Bduration)
let tkey                   = mktype (Tbuiltin Bkey)
let tkeyhash               = mktype (Tbuiltin Bkeyhash)
let tdate                  = mktype (Tbuiltin Bdate)
let ttimestamp             = mktype (Tbuiltin Btimestamp)
let taddress               = mktype (Tbuiltin Baddress)
let tenum v                = mktype (Tenum v)
let tstate                 = mktype (Tstate)
let trecord rn             = mktype (Trecord rn)
let tevent e               = mktype (Tevent e)
let toption t              = mktype (Toption t)
let tset t                 = mktype (Tset t)
let tlist t                = mktype (Tlist t)
let tmap k v               = mktype (Tmap (k, v))
let tbig_map k v           = mktype (Tbig_map (k, v))
let titerable_big_map k v  = mktype (Titerable_big_map (k, v))
let tor l r                = mktype (Tor (l, r))
let tlambda a r            = mktype (Tlambda (a, r))
let ttuple l               = mktype (Ttuple l)
let trat                   = ttuple [tint; tnat]
let toperation             = mktype (Toperation)
let tsignature             = mktype (Tbuiltin Bsignature)
let tcontract t            = mktype (Tcontract t)
let tticket t              = mktype (Tticket t)
let tsapling_state       n = mktype (Tsapling_state n)
let tsapling_transaction n = mktype (Tsapling_transaction n)
let tchainid               = mktype (Tbuiltin Bchainid)
let tbls12_381_fr          = mktype (Tbuiltin Bbls12_381_fr)
let tbls12_381_g1          = mktype (Tbuiltin Bbls12_381_g1)
let tbls12_381_g2          = mktype (Tbuiltin Bbls12_381_g2)
let tnever                 = mktype (Tbuiltin Bnever)
let tchest                 = mktype (Tbuiltin Bchest)
let tchest_key             = mktype (Tbuiltin Bchest_key)
let tasset an              = mktype (Tasset an)
let tcollection an         = mktype (Tcontainer (tasset an, Collection))
let taggregate an          = mktype (Tcontainer (tasset an, Aggregate))
let tpartition an          = mktype (Tcontainer (tasset an, Partition))
let tassetcontainer an     = mktype (Tcontainer (tasset an, AssetContainer))
let tassetkey an           = mktype (Tcontainer (tasset an, AssetKey))
let tassetvalue an         = mktype (Tcontainer (tasset an, AssetValue))
let tview an               = mktype (Tcontainer (tasset an, View))
let toperations            = tlist toperation
let tmetadata              = tbig_map tstring tbytes

let mk_bool         x = mk_mterm (Mbool x) tbool
let mk_string       x = mk_mterm (Mstring x) tstring
let mk_bytes        x = mk_mterm (Mbytes x) tbytes
let mk_chain_id     x = mk_mterm (Mchain_id x) tchainid
let mk_key          x = mk_mterm (Mkey x) tkey
let mk_key_hash     x = mk_mterm (Mkey_hash x) tkeyhash
let mk_signature    x = mk_mterm (Msignature x) tsignature
let mk_bls12_381_fr x = mk_mterm (Mbls12_381_fr x) tbls12_381_fr
let mk_bls12_381_fr_n x = mk_mterm (Mbls12_381_fr_n x) tbls12_381_fr
let mk_bls12_381_g1 x = mk_mterm (Mbls12_381_g1 x) tbls12_381_g1
let mk_bls12_381_g2 x = mk_mterm (Mbls12_381_g2 x) tbls12_381_g2
let mk_bnat         x = mk_mterm (Mnat x) tnat
let mk_nat          x = mk_bnat  (Big_int.big_int_of_int x)
let mk_bint         x = mk_mterm (Mint x) tint
let mk_int          x = mk_bint  (Big_int.big_int_of_int x)
let mk_address      x = mk_mterm (Maddress x) taddress
let unit              = mk_mterm (Munit) tunit
let mk_sapling_state_empty n = mk_mterm (MsaplingStateEmpty n) (tsapling_state n)
let mk_sapling_transaction n x = mk_mterm (MsaplingTransaction (n, x)) (tsapling_transaction n)
let mk_chest     x = mk_mterm (Mchest x) tchest
let mk_chest_key x = mk_mterm (Mchest_key x) tchest_key
let mk_tz_expr   x = mk_mterm (Mtz_expr x) tunit
let mk_date      x = mk_mterm (Mdate x) tdate
let mk_duration  x = mk_mterm (Mduration x) tduration
let mk_pair (x : mterm list) = mk_mterm (Mtuple x) (ttuple (List.map (fun (x : mterm) -> x.type_) x))
(* let mk_left     x = mk_mterm (Mleft x) tdate
   let mk_right    x = mk_mterm (Mright x) tduration *)
let mtrue         = mk_bool true
let mfalse        = mk_bool false
let mnow          = mk_mterm Mnow         tdate
let mtransferred  = mk_mterm Mtransferred ttez
let mcaller       = mk_mterm Mcaller      taddress
let mbalance      = mk_mterm Mbalance     ttez
let msource       = mk_mterm Msource      taddress
let mselfaddress  = mk_mterm Mselfaddress taddress
let mselfchainid  = mk_mterm Mselfchainid tchainid
let mmetadata     = mk_mterm Mmetadata    (tmap tstring tbytes)
let mlevel        = mk_mterm Mlevel       tnat
let mminblocktime = mk_mterm Mminblocktime tnat

let mk_mvar id t = mk_mterm (Mvar(id, Vlocal)) t
let mk_pvar id t = mk_mterm (Mvar(id, Vparam)) t
let mk_svar id t = mk_mterm (Mvar(id, Vstorevar)) t
let mk_state_var _ = mk_mterm (Mvar(mk_mident (dumloc ""), Vstate)) ((Tenum (mk_mident (dumloc "state"))), None)

let mk_enum_value  ?(args=[]) id (e : mident) = mk_mterm (Menumval(id, args, e)) (mktype (Tenum e))
let mk_state_value id = mk_enum_value id (mk_mident (dumloc "state"))

let mk_btez v = mk_mterm (Mmutez v) ttez
let mk_tez  v = mk_btez (Big_int.big_int_of_int v)

let mk_tuple (l : mterm list) = mk_mterm (Mtuple l) (ttuple (List.map (fun (x : mterm) -> x.type_) l))

let mk_letin id v b = mk_mterm (Mletin([id], LVsimple v, Some v.type_, b, None)) b.type_

let mk_tupleaccess n (x : mterm) =
  match get_ntype x.type_ with
  | Ttuple lt ->
    let t = List.nth lt n in
    mk_mterm (Mtupleaccess (x, Big_int.big_int_of_int n)) t
  | v -> Format.eprintf "mk_tupleaccess type: %a@." pp_ntype v; assert false

let mk_min (lhs : mterm) (rhs : mterm) (t : type_) = mk_mterm (Mmin (lhs, rhs)) t

let mk_max (lhs : mterm) (rhs : mterm) (t : type_) = mk_mterm (Mmax (lhs, rhs)) t

let mk_abs (x : mterm) = mk_mterm (Mabs x) tnat

let mk_nat_to_int (x : mterm) = mk_mterm (Mnattoint x) tint

let mk_some x = mk_mterm (Msome x) (toption x.type_)

let mk_left t x = mk_mterm (Mleft (t, x)) (tor x.type_ t)

let mk_right t x = mk_mterm (Mright (t, x)) (tor t x.type_)

let mk_none t = mk_mterm (Mnone) (toption t)

let mk_pack v     = mk_mterm (Mpack v) tbytes
let mk_unpack t v = mk_mterm (Munpack (t, v)) t

let mk_blake2b  v = mk_mterm (Mblake2b v) tbytes
let mk_sha256   v = mk_mterm (Msha256  v) tbytes
let mk_sha512   v = mk_mterm (Msha512  v) tbytes
let mk_keytokeyhash v = mk_mterm (Mkeytokeyhash v) tkeyhash
let mk_checksignature a b c = mk_mterm (Mchecksignature (a, b, c)) tbool

let mk_brat n d  = mk_tuple [mk_bint n; mk_bnat d]
let mk_rat n d   = mk_tuple [mk_int n; mk_nat d]

let mk_muteztonat c = mk_mterm (Mmuteztonat c) tnat
let mk_nattoint c = mk_mterm (Mnattoint c) tint

let mk_metadata v = mk_mterm (Mlitmap(MKBigMap, v)) tmetadata

let fail x  = mk_mterm (Mfail (Invalid (mk_string x))) tunit
let failg x = mk_mterm (Mfail (Invalid (x))) tunit
let failc x = mk_mterm (Mfail x) tunit
let mnot x  = mk_mterm (Mnot x) tbool
let seq x   = mk_mterm (Mseq x) tunit
let skip    = seq []

let operations = mk_mterm Moperations (tlist toperation)

let mk_get_entrypoint (ty: type_) (entry_name: lident) (addr : mterm) : mterm =
  mk_mterm (Mgetentrypoint (ty, mk_mident entry_name, addr)) (toption (tcontract ty))
let mk_mkoperation a b c = mk_mterm (Mmakeoperation (a, b, c)) toperation
let mk_mkevent     a b c = mk_mterm (Mmakeevent (a, b, c)) toperation
let mk_transfer_op op = mk_mterm (Mtransfer (TKoperation op)) tunit

(* -------------------------------------------------------------------- *)

let fail_msg_ASSET_NOT_FOUND         = "ASSET_NOT_FOUND"
let fail_msg_DIV_BY_ZERO             = "DIV_BY_ZERO"
let fail_msg_INVALID_CALLER          = "INVALID_CALLER"
let fail_msg_INVALID_CONDITION       = "INVALID_CONDITION"
let fail_msg_INVALID_SOURCE          = "INVALID_SOURCE"
let fail_msg_INVALID_STATE           = "INVALID_STATE"
let fail_msg_KEY_EXISTS              = "KEY_EXISTS"
let fail_msg_KEY_EXISTS_OR_NOT_FOUND = "KEY_EXISTS_OR_NOT_FOUND"
let fail_msg_NAT_NEG_ASSIGN          = "NAT_NEG_ASSIGN"
let fail_msg_NO_TRANSFER             = "NO_TRANSFER"
let fail_msg_NOT_FOUND               = "NOT_FOUND"
let fail_msg_OPTION_IS_NONE          = "OPTION_IS_NONE"
let fail_msg_OUT_OF_BOUND            = "OUT_OF_BOUND"

let fail_msg_ENTRY_NOT_FOUND         = "ENTRY_NOT_FOUND"
let fail_msg_EMPTY_LIST              = "EMPTY_LIST"
let fail_msg_NOT_IMPLICIT_CONTRACT   = "NOT_IMPLICIT_CONTRACT"
let fail_msg_KEY_NOT_FOUND           = "KEY_NOT_FOUND"
let fail_msg_INVALID_EVENT_CONTRACT  = "INVALID_EVENT_CONTRACT"

(* -------------------------------------------------------------------- *)

let cmp_ident (i1 : ident) (i2 : ident) : bool = String.equal i1 i2
let cmp_big_int (n1 : Core.big_int) (n2 : Core.big_int) : bool = Big_int.compare_big_int n1 n2 = 0
let cmp_int (n1 : int) (n2 : int) : bool = n1 = n2
let cmp_lident (i1 : lident) (i2 : lident) : bool = cmp_ident (Location.unloc i1) (Location.unloc i2)
let cmp_namespace (n1 : namespace) (n2 : namespace) : bool = Option.cmp cmp_lident n1 n2
let cmp_path (p1 : path) (p2 : path) : bool = cmp_namespace (fst p1) (fst p2) && cmp_lident (snd p1) (snd p2)
let cmp_mident (i1 : mident) (i2 : mident) : bool = cmp_path i1 i2
let cmp_bool (b1 : bool) (b2 : bool) : bool = b1 = b2
let cmp_assign_op (op1 : assignment_operator) (op2 : assignment_operator) : bool = op1 = op2
let cmp_container (c1 : container) (c2 : container) = c1 = c2
let cmp_btyp (b1 : btyp) (b2 : btyp) : bool = b1 = b2
let cmp_comparison_operator (op1 : comparison_operator) (op2 : comparison_operator) : bool = op1 = op2
let cmp_rat_arith_op (op1 : rat_arith_op) (op2 : rat_arith_op) : bool = op1 = op2

let cmp_fail_type
    (cmp : 'term -> 'term -> bool)
    (ft1 : fail_type)
    (ft2 : fail_type) : bool =
  match ft1, ft2 with
  | Invalid mt1, Invalid mt2                         -> cmp mt1 mt2
  | InvalidCaller, InvalidCaller                     -> true
  | InvalidSource, InvalidSource                     -> true
  | InvalidCondition (c1, e1), InvalidCondition (c2, e2) -> cmp_ident c1 c2 && Option.cmp cmp e1 e2
  | NotFound, NotFound                               -> true
  | AssetNotFound an1, AssetNotFound an2             -> cmp_ident an1 an2
  | KeyExists an1, KeyExists an2                     -> cmp_ident an1 an2
  | KeyExistsOrNotFound an1, KeyExistsOrNotFound an2 -> cmp_ident an1 an2
  | DivByZero, DivByZero                             -> true
  | NatNegAssign, NatNegAssign                       -> true
  | NoTransfer, NoTransfer                           -> true
  | InvalidState, InvalidState                       -> true
  | _ -> false

let rec cmp_ntype
    (t1 : ntype)
    (t2 : ntype)
  : bool =
  match t1, t2 with
  | Tasset i1, Tasset i2                                   -> cmp_mident i1 i2
  | Tenum i1, Tenum i2                                     -> cmp_mident i1 i2
  | Tstate, Tstate                                         -> true
  | Tbuiltin b1, Tbuiltin b2                               -> cmp_btyp b1 b2
  | Tcontainer (t1, c1), Tcontainer (t2, c2)               -> cmp_type t1 t2 && cmp_container c1 c2
  | Tlist t1, Tlist t2                                     -> cmp_type t1 t2
  | Toption t1, Toption t2                                 -> cmp_type t1 t2
  | Ttuple l1, Ttuple l2                                   -> List.for_all2 cmp_type l1 l2
  | Tset b1, Tset b2                                       -> cmp_type b1 b2
  | Tmap (k1, v1), Tmap (k2, v2)                           -> cmp_type k1 k2 && cmp_type v1 v2
  | Tbig_map (k1, v1), Tbig_map (k2, v2)                   -> cmp_type k1 k2 && cmp_type v1 v2
  | Titerable_big_map (k1, v1), Titerable_big_map (k2, v2) -> cmp_type k1 k2 && cmp_type v1 v2
  | Tor (l1, r1), Tor (l2, r2)                             -> cmp_type l1 l2 && cmp_type r1 r2
  | Trecord i1, Trecord i2                                 -> cmp_mident i1 i2
  | Tevent e1, Tevent e2                                   -> cmp_mident e1 e2
  | Tlambda (a1, r1), Tlambda (a2, r2)                     -> cmp_type a1 a2 && cmp_type r1 r2
  | Tunit, Tunit                                           -> true
  | Toperation, Toperation                                 -> true
  | Tcontract t1, Tcontract t2                             -> cmp_type t1 t2
  | Tticket t1, Tticket t2                                 -> cmp_type t1 t2
  | Tsapling_state n1, Tsapling_state n2                   -> cmp_int n1 n2
  | Tsapling_transaction n1, Tsapling_transaction n2       -> cmp_int n1 n2
  | _ -> false

and cmp_type
    ?(with_annot=false)
    (t1 : type_)
    (t2 : type_)
  : bool =
  let b = cmp_ntype (get_ntype t1) (get_ntype t2) in
  if with_annot
  then b && (Option.cmp cmp_lident (get_atype t1) (get_atype t1))
  else b

let cmp_pattern_node
    (p1    : pattern_node)
    (p2    : pattern_node)
  : bool =
  match p1, p2 with
  | Pconst (c1, xs1), Pconst (c2, xs2) ->
    cmp_mident c1 c2
    && List.length xs1 = List.length xs2
    && List.for_all2 cmp_lident xs1 xs2
  | Pwild, Pwild -> true
  | _ -> false

let cmp_pattern
    (p1 : pattern)
    (p2 : pattern)
  : bool =
  cmp_pattern_node p1.node p2.node

let cmp_for_ident
    (cmpi  : 'id -> 'id -> bool)
    (fi1 : for_ident)
    (fi2 : for_ident)
  : bool =
  match fi1, fi2 with
  | FIsimple i1, FIsimple i2 -> cmpi i1 i2
  | FIdouble (x1, y1), FIdouble (x2, y2) -> cmpi x1 x2 && cmpi y1 y2
  | _ -> false

let cmp_mterm_node
    (cmp   : 'term -> 'term -> bool)
    (cmpi  : 'id -> 'id -> bool)
    (term1 : 'term mterm_node)
    (term2 : 'term mterm_node)
  : bool =
  let cmp_assign_kind (lhs : assign_kind) (rhs : assign_kind) : bool =
    match lhs, rhs with
    | Avar id1, Avar id2                               -> cmpi id1 id2
    | Avarstore id1, Avarstore id2                     -> cmpi id1 id2
    | Aasset (an1, fn1, k1), Aasset (an2, fn2, k2)     -> cmpi an1 an2 && cmpi fn1 fn2 && cmp k1 k2
    | Arecord (lv1, rn1, fn1), Arecord (lv2, rn2, fn2) -> cmp lv1 lv2 && cmpi rn1 rn2 && cmpi fn1 fn2
    | Atuple (lv1, n1, l1), Atuple (lv2, n2, l2)       -> cmp lv1 lv2 && cmp_int n1 n2 && cmp_int l1 l2
    | Astate, Astate                                   -> true
    | Aoperations, Aoperations                         -> true
    | _ -> false
  in
  let cmp_var_kind (lhs : var_kind) (rhs : var_kind) : bool =
    match lhs, rhs with
    | Vassetstate v1, Vassetstate v2 -> cmp v1 v2
    | Vstorevar, Vstorevar
    | Vstorecol, Vstorecol
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
    | CKfield (an1, fn1, mt1), CKfield (an2, fn2, mt2) -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp mt1 mt2
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
  let cmp_transfer_kind (lhs : transfer_kind) (rhs : transfer_kind) : bool =
    match lhs, rhs with
    | TKsimple (x1, d1), TKsimple (x2, d2)                     -> cmp x1 x2 && cmp d1 d2
    | TKcall (x1, i1, t1, d1, a1), TKcall (x2, i2, t2, d2, a2) -> cmp x1 x2 && cmp_ident i1 i2 && cmp_type t1 t2 && cmp d1 d2 && cmp a1 a2
    | TKentry (x1, e1, a1), TKentry (x2, e2, a2)               -> cmp x1 x2 && cmp e1 e2 && cmp a1 a2
    | TKgen (x1, en1, cn1, t1, ad1, a1), TKgen (x2, en2, cn2, t2, ad2, a2) -> cmp x1 x2 && cmp_ident en1 en2 && cmp_ident cn1 cn2 && cmp_type t1 t2 && cmp ad1 ad2 && cmp a1 a2
    | TKself (x1, i1, as1), TKself (x2, i2, as2)               -> cmp x1 x2 && cmp_ident i1 i2 && List.for_all2 (fun (id1, v1) (id2, v2) -> cmp_ident id1 id2 && cmp v1 v2) as1 as2
    | TKoperation x1, TKoperation x2                           -> cmp x1 x2
    | _ -> false
  in
  let cmp_map_kind (lhs : map_kind) (rhs : map_kind) : bool =
    match lhs, rhs with
    | _ -> false
  in
  let cmp_michelson_struct (lhs : michelson_struct) (rhs : michelson_struct) : bool =
    lhs = rhs
  in
  let cmp_letin_value (lhs : letin_value) (rhs : letin_value) : bool =
    match lhs, rhs with
    | LVsimple v1, LVsimple v2 -> cmp v1 v2
    | LVreplace (id1, k1, fa1), LVreplace (id2, k2, fa2) -> cmpi id1 id2 && k1 = k2 && cmp fa1 fa2
    | _, _ -> false
  in
  let cmp_detach_kind (lhs : detach_kind) (rhs : detach_kind) : bool =
    match lhs, rhs with
    | DK_option (ty1, id1), DK_option (ty2, id2) -> cmp_type ty1 ty2 && cmp_ident id1 id2
    | DK_map (ty1, id1, k1), DK_map (ty2, id2, k2) -> cmp_type ty1 ty2 && cmp_ident id1 id2 && cmp k1 k2
    | _, _ -> false
  in
  try
    match term1, term2 with
    (* lambda *)
    | Mletin (i1, a1, t1, b1, o1), Mletin (i2, a2, t2, b2, o2)                         -> List.for_all2 cmpi i1 i2 && cmp_letin_value a1 a2 && Option.cmp cmp_type t1 t2 && cmp b1 b2 && Option.cmp cmp o1 o2
    | Mdeclvar (i1, t1, v1, c1), Mdeclvar (i2, t2, v2, c2)                             -> List.for_all2 cmpi i1 i2 && Option.cmp cmp_type t1 t2 && cmp v1 v2 && cmp_bool c1 c2
    | Mdeclvaropt (i1, t1, v1, f1, c1), Mdeclvaropt (i2, t2, v2, f2, c2)               -> List.for_all2 cmpi i1 i2 && Option.cmp cmp_type t1 t2 && cmp v1 v2 && Option.cmp cmp f1 f2 && cmp_bool c1 c2
    | Mapp (e1, args1), Mapp (e2, args2)                                               -> cmpi e1 e2 && List.for_all2 cmp args1 args2
    (* assign *)
    | Massign (op1, t1, k1, v1), Massign (op2, t2, k2, v2)                             -> cmp_assign_op op1 op2 && cmp_type t1 t2 && cmp_assign_kind k1 k2 && cmp v1 v2
    | Massignopt (op1, t1, k1, v1, fa1), Massignopt (op2, t2, k2, v2, fa2)             -> cmp_assign_op op1 op2 && cmp_type t1 t2 && cmp_assign_kind k1 k2 && cmp v1 v2 && cmp fa1 fa2
    (* control *)
    | Mif (c1, t1, e1), Mif (c2, t2, e2)                                               -> cmp c1 c2 && cmp t1 t2 && Option.cmp cmp e1 e2
    | Mmatchwith (e1, l1), Mmatchwith (e2, l2)                                         -> cmp e1 e2 && List.for_all2 (fun (p1, t1) (p2, t2) -> cmp_pattern p1 p2 && cmp t1 t2) l1 l2
    | Minstrmatchoption (x1, i1, ve1, ne1), Minstrmatchoption (x2, i2, ve2, ne2)               -> cmp x1 x2 && cmpi i1 i2 && cmp ve1 ve2 && cmp ne1 ne2
    | Minstrmatchor (x1, lid1, le1, rid1, re1), Minstrmatchor (x2, lid2, le2, rid2, re2)       -> cmp x1 x2 && cmpi lid1 lid2 && cmp le1 le2 && cmpi rid1 rid2 && cmp re1 re2
    | Minstrmatchlist (x1, hid1, tid1, hte1, ee1), Minstrmatchlist (x2, hid2, tid2, hte2, ee2) -> cmp x1 x2 && cmpi hid1 hid2 && cmpi tid1 tid2 && cmp hte1 hte2 && cmp ee1 ee2
    | Mfor (i1, c1, b1), Mfor (i2, c2, b2)                                             -> cmp_for_ident cmpi i1 i2 && cmp_iter_container_kind c1 c2 && cmp b1 b2
    | Miter (i1, a1, b1, c1, strict1), Miter (i2, a2, b2, c2, strict2)                 -> cmpi i1 i2 && cmp a1 a2 && cmp b1 b2 && cmp c1 c2 && cmp_bool strict1 strict2
    | Mwhile (c1, b1), Mwhile (c2, b2)                                                 -> cmp c1 c2 && cmp b1 b2
    | Mseq is1, Mseq is2                                                               -> List.for_all2 cmp is1 is2
    | Mreturn x1, Mreturn x2                                                           -> cmp x1 x2
    (* effect *)
    | Mfail ft1, Mfail ft2                                                             -> cmp_fail_type cmp ft1 ft2
    | Mfailsome v1, Mfailsome v2                                                       -> cmp v1 v2
    | Mtransfer tr1, Mtransfer tr2                                                     -> cmp_transfer_kind tr1 tr2
    | Memit (e1, x1), Memit (e2, x2)                                                   -> cmpi e1 e2 && cmp x1 x2
    | Mdetach (id1, dk1, ty1, f1), Mdetach (id2, dk2, ty2, f2)                         -> cmpi id1 id2 && cmp_detach_kind dk1 dk2 && cmp_type ty1 ty2 && cmp f1 f2
    (* entrypoint *)
    | Mgetentrypoint (t1, a1, s1), Mgetentrypoint (t2, a2, s2)                         -> cmp_type t1 t2 && cmpi a1 a2 && cmp s1 s2
    | Mcallview (t1, a1, b1, c1), Mcallview (t2, a2, b2, c2)                           -> cmp_type t1 t2 && cmp a1 a2 && cmpi b1 b2 && cmp c1 c2
    | Mimportcallview (t1, a1, b1, c1), Mimportcallview (t2, a2, b2, c2)               -> cmp_type t1 t2 && cmp a1 a2 && cmpi b1 b2 && cmp c1 c2
    | Mself id1, Mself id2                                                             -> cmpi id1 id2
    (* operation *)
    | Moperations, Moperations                                                         -> true
    | Mmakeoperation (v1, d1, a1), Mmakeoperation (v2, d2, a2)                         -> cmp v1 v2 && cmp d1 d2 && cmp a1 a2
    | Mmakeevent (t1, id1, a1), Mmakeevent (t2, id2, a2)                               -> cmp_type t1 t2 && cmp_mident id1 id2 && cmp a1 a2
    | Mcreatecontract (ms1, d1, a1, si1), Mcreatecontract (ms2, d2, a2, si2)           -> cmp_michelson_struct ms1 ms2 && cmp d1 d2 && cmp a1 a2 && cmp si1 si2
    (* literals *)
    | Mint v1, Mint v2                                                                 -> Big_int.eq_big_int v1 v2
    | Mnat v1, Mnat v2                                                                 -> Big_int.eq_big_int v1 v2
    | Mbool v1, Mbool v2                                                               -> cmp_bool v1 v2
    | Mrational (n1, d1), Mrational (n2, d2)                                           -> Big_int.eq_big_int n1 n2 && Big_int.eq_big_int d1 d2
    | Mstring v1, Mstring v2                                                           -> cmp_ident v1 v2
    | Mmutez v1, Mmutez v2                                                             -> Big_int.eq_big_int v1 v2
    | Maddress v1, Maddress v2                                                         -> cmp_ident v1 v2
    | Mdate v1, Mdate v2                                                               -> Core.cmp_date v1 v2
    | Mduration v1, Mduration v2                                                       -> Core.cmp_duration v1 v2
    | Mtimestamp v1, Mtimestamp v2                                                     -> Big_int.eq_big_int v1 v2
    | Mbytes v1, Mbytes v2                                                             -> cmp_ident v1 v2
    | Mchain_id v1, Mchain_id v2                                                       -> cmp_ident v1 v2
    | Mkey v1, Mkey v2                                                                 -> cmp_ident v1 v2
    | Mkey_hash v1, Mkey_hash v2                                                       -> cmp_ident v1 v2
    | Msignature v1, Msignature v2                                                     -> cmp_ident v1 v2
    | Mbls12_381_fr v1, Mbls12_381_fr v2                                               -> cmp_ident v1 v2
    | Mbls12_381_fr_n v1, Mbls12_381_fr_n v2                                           -> Big_int.eq_big_int v1 v2
    | Mbls12_381_g1 v1, Mbls12_381_g1 v2                                               -> cmp_ident v1 v2
    | Mbls12_381_g2 v1, Mbls12_381_g2 v2                                               -> cmp_ident v1 v2
    | Munit, Munit                                                                     -> true
    | MsaplingStateEmpty n1, MsaplingStateEmpty n2                                     -> cmp_int n1 n2
    | MsaplingTransaction (n1, v1), MsaplingTransaction (n2, v2)                       -> cmp_int n1 n2 && cmp_ident v1 v2
    | Mchest v1, Mchest v2                                                             -> cmp_ident v1 v2
    | Mchest_key v1, Mchest_key v2                                                     -> cmp_ident v1 v2
    | Mtz_expr v1, Mtz_expr v2                                                         -> cmp_ident v1 v2
    (* control expression *)
    | Mexprif (c1, t1, e1), Mexprif (c2, t2, e2)                                       -> cmp c1 c2 && cmp t1 t2 && cmp e1 e2
    | Mexprmatchwith (e1, l1), Mexprmatchwith (e2, l2)                                 -> cmp e1 e2 && List.for_all2 (fun (p1, t1) (p2, t2) -> cmp_pattern p1 p2 && cmp t1 t2) l1 l2
    | Mmatchoption (x1, i1, ve1, ne1), Mmatchoption (x2, i2, ve2, ne2)                 -> cmp x1 x2 && cmpi i1 i2 && cmp ve1 ve2 && cmp ne1 ne2
    | Mmatchor (x1, lid1, le1, rid1, re1), Mmatchor (x2, lid2, le2, rid2, re2)         -> cmp x1 x2 && cmpi lid1 lid2 && cmp le1 le2 && cmpi rid1 rid2 && cmp re1 re2
    | Mmatchlist (x1, hid1, tid1, hte1, ee1), Mmatchlist (x2, hid2, tid2, hte2, ee2)   -> cmp x1 x2 && cmpi hid1 hid2 && cmpi tid1 tid2 && cmp hte1 hte2 && cmp ee1 ee2
    | Mternarybool (c1, a1, b1), Mternarybool (c2, a2, b2)                             -> cmp c1 c2 && cmp a1 a2 && cmp b1 b2
    | Mternaryoption (c1, a1, b1), Mternaryoption (c2, a2, b2)                         -> cmp c1 c2 && cmp a1 a2 && cmp b1 b2
    | Mfold (x1, i1, e1), Mfold (x2, i2, e2)                                           -> cmp x1 x2 && cmpi i1 i2 && cmp e1 e2
    | Mmap (x1, i1, e1), Mmap (x2, i2, e2)                                             -> cmp x1 x2 && cmpi i1 i2 && cmp e1 e2
    | Mexeclambda  (l1, a1), Mexeclambda  (l2, a2)                                     -> cmp l1 l2 && cmp a1 a2
    | Mapplylambda (l1, a1), Mapplylambda (l2, a2)                                     -> cmp l1 l2 && cmp a1 a2
    (* composite type constructors *)
    | Mleft (t1, x1), Mleft (t2, x2)                                                   -> cmp_type t1 t2 && cmp x1 x2
    | Mright (t1, x1), Mright (t2, x2)                                                 -> cmp_type t1 t2 && cmp x1 x2
    | Mnone, Mnone                                                                     -> true
    | Msome v1, Msome v2                                                               -> cmp v1 v2
    | Mtuple l1, Mtuple l2                                                             -> List.for_all2 cmp l1 l2
    | Masset l1, Masset l2                                                             -> List.for_all2 cmp l1 l2
    | Massets l1, Massets l2                                                           -> List.for_all2 cmp l1 l2
    | Mlitset l1, Mlitset l2                                                           -> List.for_all2 cmp l1 l2
    | Mlitlist l1, Mlitlist l2                                                         -> List.for_all2 cmp l1 l2
    | Mlitmap (b1, l1), Mlitmap (b2, l2)                                               -> cmp_map_kind b1 b2 && List.for_all2 (fun (k1, v1) (k2, v2) -> (cmp k1 k2 && cmp v1 v2)) l1 l2
    | Mlitrecord l1, Mlitrecord l2                                                     -> List.for_all2 (fun (i1, v1) (i2, v2) -> (cmp_ident i1 i2 && cmp v1 v2)) l1 l2
    | Mlitevent l1, Mlitevent l2                                                       -> List.for_all2 (fun (i1, v1) (i2, v2) -> (cmp_ident i1 i2 && cmp v1 v2)) l1 l2
    | Mlambda (rt1, id1, at1, e1), Mlambda (rt2, id2, at2, e2)                         -> cmp_type rt1 rt2 && cmpi id1 id2 && cmp_type at1 at2 && cmp e1 e2
    (* access *)
    | Mdot (e1, i1), Mdot (e2, i2)                                                     -> cmp e1 e2 && cmpi i1 i2
    | Mdotassetfield (an1, k1, fn1), Mdotassetfield (an2, k2, fn2)                     -> cmpi an1 an2 && cmp k1 k2 && cmpi fn1 fn2
    | Mquestionoption (a1, fn1), Mquestionoption (a2, fn2)                             -> cmp a1 a2 && cmpi fn1 fn2
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
    | Mgreedyand (l1, r1), Mgreedyand (l2, r2)                                         -> cmp l1 l2 && cmp r1 r2
    | Mgreedyor (l1, r1), Mgreedyor (l2, r2)                                           -> cmp l1 l2 && cmp r1 r2
    | Mxor (l1, r1), Mxor (l2, r2)                                                     -> cmp l1 l2 && cmp r1 r2
    | Mnot e1, Mnot e2                                                                 -> cmp e1 e2
    | Mplus (l1, r1), Mplus (l2, r2)                                                   -> cmp l1 l2 && cmp r1 r2
    | Mminus (l1, r1), Mminus (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Mmult (l1, r1), Mmult (l2, r2)                                                   -> cmp l1 l2 && cmp r1 r2
    | Mdivrat (l1, r1), Mdivrat (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Mdiveuc (l1, r1), Mdiveuc (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Mmodulo (l1, r1), Mmodulo (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Mdivmod (l1, r1), Mdivmod (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Muminus e1, Muminus e2                                                           -> cmp e1 e2
    | MthreeWayCmp (l1, r1), MthreeWayCmp (l2, r2)                                     -> cmp l1 l2 && cmp r1 r2
    | Mshiftleft (l1, r1), Mshiftleft (l2, r2)                                         -> cmp l1 l2 && cmp r1 r2
    | Mshiftright (l1, r1), Mshiftright (l2, r2)                                       -> cmp l1 l2 && cmp r1 r2
    | Msubnat (l1, r1), Msubnat (l2, r2)                                               -> cmp l1 l2 && cmp r1 r2
    | Msubmutez (l1, r1), Msubmutez (l2, r2)                                           -> cmp l1 l2 && cmp r1 r2
    (* asset api effect *)
    | Maddasset (an1, i1), Maddasset (an2, i2)                                         -> cmp_ident an1 an2 && cmp i1 i2
    | Mputsingleasset (an1, i1), Mputsingleasset (an2, i2)                             -> cmp_ident an1 an2 && cmp i1 i2
    | Mputasset (an1, k1, v1), Mputasset (an2, k2, v2)                                 -> cmp_ident an1 an2 && cmp k1 k2 && cmp v1 v2
    | Maddfield (an1, fn1, c1, i1), Maddfield (an2, fn2, c2, i2)                       -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp c1 c2 && cmp i1 i2
    | Mremoveasset (an1, i1), Mremoveasset (an2, i2)                                   -> cmp_ident an1 an2 && cmp i1 i2
    | Mremovefield (an1, fn1, c1, i1), Mremovefield (an2, fn2, c2, i2)                 -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp c1 c2 && cmp i1 i2
    | Mremoveall (an1, v1), Mremoveall (an2, v2)                                       -> cmp_ident an1 an2 && cmp_container_kind v1 v2
    | Mremoveif (an1, c1, la1, lb1, a1), Mremoveif (an2, c2, la2, lb2, a2)             -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && List.for_all2 (fun (i1, t1) (i2, t2) -> cmp_ident i1 i2 && cmp_type t1 t2) la1 la2 && cmp lb1 lb2 && List.for_all2 cmp a1 a2
    | Mclear (an1, v1), Mclear (an2, v2)                                               -> cmp_ident an1 an2 && cmp_container_kind v1 v2
    | Mset (c1, l1, k1, v1), Mset (c2, l2, k2, v2)                                     -> cmp_ident c1 c2 && List.for_all2 cmp_ident l1 l2 && cmp k1 k2 && cmp v1 v2
    | Mupdate (an1, k1, l1), Mupdate (an2, k2, l2)                                     -> cmp_ident an1 an2 && cmp k1 k2 && List.for_all2 (fun (id1, op1, v1) (id2, op2, v2) -> cmpi id1 id2 && cmp_assign_op op1 op2 && cmp v1 v2) l1 l2
    | Mupdateall (an1, c1, l1), Mupdateall (an2, c2, l2)                               -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && List.for_all2 (fun (id1, op1, v1) (id2, op2, v2) -> cmpi id1 id2 && cmp_assign_op op1 op2 && cmp v1 v2) l1 l2
    | Maddupdate (an1, c1, k1, l1), Maddupdate (an2, c2, k2, l2)                       -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp k1 k2 && List.for_all2 (fun (id1, op1, v1) (id2, op2, v2) -> cmpi id1 id2 && cmp_assign_op op1 op2 && cmp v1 v2) l1 l2
    | Mputremove (an1, c1, k1, v1), Mputremove (an2, c2, k2, v2)                       -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp k1 k2 && cmp v1 v2
    (* asset api expression *)
    | Mget (an1, c1, k1), Mget (an2, c2, k2)                                           -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp k1 k2
    | Mgetsome (an1, c1, k1), Mgetsome (an2, c2, k2)                                   -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp k1 k2
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
    | Mrecupdate (x1, l1), Mrecupdate (x2, l2)                                         -> cmp x1 x2 && List.for_all2 (fun (i1, v1) (i2, v2) -> cmp_ident i1 i2 && cmp v1 v2) l1 l2
    | Mmakeasset (an1, k1, v1), Mmakeasset (an2, k2, v2)                               -> cmp_ident an1 an2 && cmp k1 k2 && cmp v1 v2
    | Mtocontainer an1, Mtocontainer an2                                               -> cmp_ident an1 an2
    | Mglobal_constant (t1, v1), Mglobal_constant (t2, v2)                             -> cmp_type t1 t2 && cmp v1 v2
    (* set api expression *)
    | Msetadd (t1, c1, a1), Msetadd (t2, c2, a2)                                       -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Msetremove (t1, c1, a1), Msetremove (t2, c2, a2)                                 -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Msetupdate (t1, c1, b1, v1), Msetupdate (t2, c2, b2, v2)                         -> cmp_type t1 t2 && cmp c1 c2 && cmp b1 b2 && cmp v1 v2
    | Msetcontains (t1, c1, a1), Msetcontains (t2, c2, a2)                             -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Msetlength (t1, c1), Msetlength (t2, c2)                                         -> cmp_type t1 t2 && cmp c1 c2
    | Msetfold (t1, ix1, ia1, c1, a1, b1), Msetfold (t2, ix2, ia2, c2, a2, b2)         -> cmp_type t1 t2 && cmp_mident ix1 ix2 && cmp_mident ia1 ia2 && cmp c1 c2 && cmp a1 a2 && cmp b1 b2
    (* set api instruction *)
    | Msetinstradd (t1, ak1, a1),    Msetinstradd (t2, ak2, a2)                        -> cmp_type t1 t2 && cmp_assign_kind ak1 ak2 && cmp a1 a2
    | Msetinstrremove (t1, ak1, a1), Msetinstrremove (t2, ak2, a2)                     -> cmp_type t1 t2 && cmp_assign_kind ak1 ak2 && cmp a1 a2
    (* list api expression *)
    | Mlistprepend (t1, c1, a1), Mlistprepend (t2, c2, a2)                             -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Mlistlength (t1, c1), Mlistlength (t2, c2)                                       -> cmp_type t1 t2 && cmp c1 c2
    | Mlistcontains (t1, c1, a1), Mlistcontains (t2, c2, a2)                           -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Mlistnth (t1, c1, a1), Mlistnth (t2, c2, a2)                                     -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Mlisthead (t1, c1, a1), Mlisthead (t2, c2, a2)                                   -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Mlisttail (t1, c1, a1), Mlisttail (t2, c2, a2)                                   -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Mlistreverse (t1, l1), Mlistreverse (t2, l2)                                     -> cmp_type t1 t2 && cmp l1 l2
    | Mlistconcat (t1, l1, m1), Mlistconcat (t2, l2, m2)                               -> cmp_type t1 t2 && cmp l1 l2 && cmp m1 m2
    | Mlistfold (t1, ix1, ia1, c1, a1, b1), Mlistfold (t2, ix2, ia2, c2, a2, b2)       -> cmp_type t1 t2 && cmp_mident ix1 ix2 && cmp_mident ia1 ia2 && cmp c1 c2 && cmp a1 a2 && cmp b1 b2
    (* list api instruction *)
    | Mlistinstrprepend (t1, ak1, a1), Mlistinstrprepend (t2, ak2, a2)                 -> cmp_type t1 t2 && cmp_assign_kind ak1 ak2 && cmp a1 a2
    | Mlistinstrconcat (t1, ak1, a1),  Mlistinstrconcat (t2, ak2, a2)                  -> cmp_type t1 t2 && cmp_assign_kind ak1 ak2 && cmp a1 a2
    (* map api expression *)
    | Mmapput (mk1, tk1, tv1, c1, k1, v1), Mmapput (mk2, tk2, tv2, c2, k2, v2)                     -> cmp_map_kind mk1 mk2 && cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp c1 c2 && cmp k1 k2 && cmp v1 v2
    | Mmapremove (mk1, tk1, tv1, c1, k1), Mmapremove (mk2, tk2, tv2, c2, k2)                       -> cmp_map_kind mk1 mk2 && cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp c1 c2 && cmp k1 k2
    | Mmapupdate (mk1, tk1, tv1, c1, k1, v1), Mmapupdate (mk2, tk2, tv2, c2, k2, v2)               -> cmp_map_kind mk1 mk2 && cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp c1 c2 && cmp k1 k2 && cmp v1 v2
    | Mmapget (mk1, tk1, tv1, c1, k1, an1), Mmapget (mk2, tk2, tv2, c2, k2, an2)                   -> cmp_map_kind mk1 mk2 && cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp c1 c2 && cmp k1 k2 && Option.cmp cmp_ident an1 an2
    | Mmapgetopt (mk1, tk1, tv1, c1, k1), Mmapgetopt (mk2, tk2, tv2, c2, k2)                       -> cmp_map_kind mk1 mk2 && cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp c1 c2 && cmp k1 k2
    | Mmapcontains (mk1, tk1, tv1, c1, k1), Mmapcontains (mk2, tk2, tv2, c2, k2)                   -> cmp_map_kind mk1 mk2 && cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp c1 c2 && cmp k1 k2
    | Mmaplength (mk1, tk1, tv1, c1), Mmaplength (mk2, tk2, tv2, c2)                               -> cmp_map_kind mk1 mk2 && cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp c1 c2
    | Mmapfold (mk1, t1, ik1, iv1, ia1, c1, a1, b1), Mmapfold (mk2, t2, ik2, iv2, ia2, c2, a2, b2) -> cmp_map_kind mk1 mk2 && cmp_type t1 t2 && cmp_mident ik1 ik2 && cmp_mident iv1 iv2 && cmp_mident ia1 ia2 && cmp c1 c2 && cmp a1 a2 && cmp b1 b2
    (* map api instruction *)
    | Mmapinstrput (mk1, tk1, tv1, ak1, k1, v1), Mmapinstrput (mk2, tk2, tv2, ak2, k2, v2)         -> cmp_map_kind mk1 mk2 && cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp_assign_kind ak1 ak2 && cmp k1 k2 && cmp v1 v2
    | Mmapinstrremove (mk1, tk1, tv1, ak1, k1), Mmapinstrremove (mk2, tk2, tv2, ak2, k2)           -> cmp_map_kind mk1 mk2 && cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp_assign_kind ak1 ak2 && cmp k1 k2
    | Mmapinstrupdate (mk1, tk1, tv1, ak1, k1, v1), Mmapinstrupdate (mk2, tk2, tv2, ak2, k2, v2)   -> cmp_map_kind mk1 mk2 && cmp_type tk1 tk2 && cmp_type tv1 tv2 && cmp_assign_kind ak1 ak2 && cmp k1 k2 && cmp v1 v2
    (* builtin functions *)
    | Mmin (l1, r1), Mmin (l2, r2)                                                     -> cmp l1 l2 && cmp r1 r2
    | Mmax (l1, r1), Mmax (l2, r2)                                                     -> cmp l1 l2 && cmp r1 r2
    | Mabs a1, Mabs a2                                                                 -> cmp a1 a2
    | Mconcat (x1, y1), Mconcat (x2, y2)                                               -> cmp x1 x2 && cmp y1 y2
    | Mconcatlist (x1), Mconcatlist (x2)                                               -> cmp x1 x2
    | Mslice (x1, s1, e1), Mslice (x2, s2, e2)                                         -> cmp x1 x2 && cmp s1 s2 && cmp e1 e2
    | Mlength x1, Mlength x2                                                           -> cmp x1 x2
    | Misnone x1, Misnone x2                                                           -> cmp x1 x2
    | Missome x1, Missome x2                                                           -> cmp x1 x2
    | Minttonat x1, Minttonat x2                                                       -> cmp x1 x2
    | Mfloor x1, Mfloor x2                                                             -> cmp x1 x2
    | Mceil x1, Mceil x2                                                               -> cmp x1 x2
    | Mnattostring x1, Mnattostring x2                                                 -> cmp x1 x2
    | Mbytestonat x1, Mbytestonat x2                                                   -> cmp x1 x2
    | Mnattobytes x1, Mnattobytes x2                                                   -> cmp x1 x2
    | Mbytestoint x1, Mbytestoint x2                                                   -> cmp x1 x2
    | Minttobytes x1, Minttobytes x2                                                   -> cmp x1 x2
    | Mpack x1, Mpack x2                                                               -> cmp x1 x2
    | Munpack (t1, x1), Munpack (t2, x2)                                               -> cmp_type t1 t2 && cmp x1 x2
    | Msetdelegate x1, Msetdelegate x2                                                 -> cmp x1 x2
    | Mkeyhashtocontract x1, Mkeyhashtocontract x2                                     -> cmp x1 x2
    | Mcontracttoaddress x1, Mcontracttoaddress x2                                     -> cmp x1 x2
    | Maddresstocontract (t1, x1), Maddresstocontract (t2, x2)                         -> cmp_type t1 t2 && cmp x1 x2
    | Mkeytoaddress x1, Mkeytoaddress x2                                               -> cmp x1 x2
    | Msimplify_rational x1, Msimplify_rational x2                                     -> cmp x1 x2
    | Mget_numerator x1, Mget_numerator x2                                             -> cmp x1 x2
    | Mget_denominator x1, Mget_denominator x2                                         -> cmp x1 x2
    | Misimplicitaddress x1, Misimplicitaddress x2                                     -> cmp x1 x2
    (* crypto functions *)
    | Mblake2b x1, Mblake2b x2                                                         -> cmp x1 x2
    | Msha256  x1, Msha256  x2                                                         -> cmp x1 x2
    | Msha512  x1, Msha512  x2                                                         -> cmp x1 x2
    | Msha3    x1, Msha3    x2                                                         -> cmp x1 x2
    | Mkeccak  x1, Mkeccak  x2                                                         -> cmp x1 x2
    | Mkeytokeyhash x1, Mkeytokeyhash  x2                                              -> cmp x1 x2
    | Mchecksignature (k1, s1, x1), Mchecksignature (k2, s2, x2)                       -> cmp k1 k2 && cmp s1 s2 && cmp x1 x2
    (* voting *)
    | Mtotalvotingpower, Mtotalvotingpower                                             -> true
    | Mvotingpower x1, Mvotingpower x2                                                 -> cmp x1 x2
    (* ticket *)
    | Mcreateticket (x1, a1), Mcreateticket (x2, a2)                                   -> cmp x1 x2 && cmp a1 a2
    | Mreadticket x1, Mreadticket x2                                                   -> cmp x1 x2
    | Msplitticket (x1, a1, b1), Msplitticket (x2, a2, b2)                             -> cmp x1 x2 && cmp a1 a2 && cmp b1 b2
    | Mjointickets (x1, y1), Mjointickets (x2, y2)                                     -> cmp x1 x2 && cmp y1 y2
    (* sapling *)
    | Msapling_empty_state n1, Msapling_empty_state n2                                 -> cmp_int n1 n2
    | Msapling_verify_update (s1, t1), Msapling_verify_update (s2, t2)                 -> cmp s1 s2 && cmp t1 t2
    (* bls curve *)
    | Mpairing_check x1, Mpairing_check x2                                             -> cmp x1 x2
    (* timelock *)
    | Mopen_chest (x1, y1, z1), Mopen_chest (x2, y2, z2)                               -> cmp x1 x2 && cmp y1 y2 && cmp z1 z2
    (* constants *)
    | Mnow, Mnow                                                                       -> true
    | Mtransferred, Mtransferred                                                       -> true
    | Mcaller, Mcaller                                                                 -> true
    | Mbalance, Mbalance                                                               -> true
    | Msource, Msource                                                                 -> true
    | Mselfaddress, Mselfaddress                                                       -> true
    | Mselfchainid, Mselfchainid                                                       -> true
    | Mmetadata, Mmetadata                                                             -> true
    | Mlevel, Mlevel                                                                   -> true
    | Mminblocktime, Mminblocktime                                                     -> true
    (* variable *)
    | Mvar (id1, k1), Mvar (id2, k2)                                                   -> cmpi id1 id2 && cmp_var_kind k1 k2
    (* rational *)
    | Mrateq (l1, r1), Mrateq (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Mratcmp (op1, l1, r1), Mratcmp (op2, l2, r2)                                     -> cmp_comparison_operator op1 op2 && cmp l1 l2 && cmp r1 r2
    | Mratarith (op1, l1, r1), Mratarith (op2, l2, r2)                                 -> cmp_rat_arith_op op1 op2 && cmp l1 l2 && cmp r1 r2
    | Mratuminus v1, Mratuminus v2                                                     -> cmp v1 v2
    | Mrattez (c1, t1), Mrattez (c2, t2)                                               -> cmp c1 c2 && cmp t1 t2
    | Mnattoint e1, Mnattoint e2                                                       -> cmp e1 e2
    | Mnattorat e1, Mnattorat e2                                                       -> cmp e1 e2
    | Minttorat e1, Minttorat e2                                                       -> cmp e1 e2
    | Mratdur (c1, t1), Mratdur (c2, t2)                                               -> cmp c1 c2 && cmp t1 t2
    (* others *)
    | Minttodate v1, Minttodate v2                                                     -> cmp v1 v2
    | Mmuteztonat v1, Mmuteztonat v2                                                   -> cmp v1 v2
    | _ -> false
  with
    _ -> false

let rec cmp_mterm (term1 : mterm) (term2 : mterm) : bool =
  cmp_mterm_node cmp_mterm cmp_mident term1.node term2.node

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
    | RemoveAll (an1, c1), RemoveAll (an2, c2)               -> cmp_ident an1 an2 && cmp_container_kind c1 c2
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
    | Lreverse  t1, Lreverse  t2 -> cmp_type t1 t2
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
    | Boptget t1, Boptget t2 -> cmp_type t1 t2
    | Bfloor    , Bfloor     -> true
    | Bceil     , Bceil      -> true
    | Bnattostring, Bnattostring -> true
    | Bfail t1, Bfail t2 -> cmp_type t1 t2
    | _ -> false
  in
  let cmp_api_internal (i1 : api_internal) (i2 : api_internal) : bool =
    match i1, i2 with
    | RatEq,     RatEq     -> true
    | RatCmp,    RatCmp    -> true
    | RatArith,  RatArith  -> true
    | RatUminus, RatUminus -> true
    | RatTez,    RatTez    -> true
    | RatDur,    RatDur    -> true
    | _ -> false
  in
  match a1, a2 with
  | APIAsset s1,    APIAsset s2    -> cmp_api_asset s1 s2
  | APIList c1,     APIList c2     -> cmp_api_list c1 c2
  | APIBuiltin f1,  APIBuiltin f2  -> cmp_api_builtin f1 f2
  | APIInternal i1, APIInternal i2 -> cmp_api_internal i1 i2
  | _ -> false

let cmp_api_storage (c1 : api_storage) (c2 : api_storage) =
  cmp_api_item_node c1.node_item c2.node_item

(* -------------------------------------------------------------------- *)

let map_ptyp (f : type_ -> type_) (nt : ntype) : ntype =
  match nt with
  | Tasset id                -> Tasset id
  | Tenum id                 -> Tenum id
  | Tstate                   -> Tstate
  | Tbuiltin b               -> Tbuiltin b
  | Tcontainer (t, c)        -> Tcontainer (f t, c)
  | Tlist t                  -> Tlist (f t)
  | Toption t                -> Toption (f t)
  | Ttuple l                 -> Ttuple (List.map f l)
  | Tset k                   -> Tset (f k)
  | Tmap (k, v)              -> Tmap (f k, f v)
  | Tbig_map (k, v)          -> Tbig_map (f k, f v)
  | Titerable_big_map (k, v) -> Titerable_big_map (f k, f v)
  | Tor (l, r)               -> Tor (f l, f r)
  | Trecord id               -> Trecord id
  | Tevent id                -> Tevent id
  | Tlambda (a, r)           -> Tlambda (f a, f r)
  | Tunit                    -> Tunit
  | Toperation               -> Toperation
  | Tcontract t              -> Tcontract (f t)
  | Tticket t                -> Tticket (f t)
  | Tsapling_state n         -> Tsapling_state n
  | Tsapling_transaction n   -> Tsapling_transaction n

let map_type (f : type_ -> type_) (t : type_) : type_ =
  mktype ?annot:(get_atype t) (map_ptyp f (get_ntype t))

(* -------------------------------------------------------------------- *)
let fold_typ (f : 'a -> type_ -> 'a) (accu : 'a) (ty : type_) : 'a =
  match fst ty with
  | Tasset _                   -> accu
  | Tenum _                    -> accu
  | Tstate                     -> accu
  | Tbuiltin _                 -> accu
  | Tcontainer (t, _)          -> f accu t
  | Tlist t                    -> f accu t
  | Toption t                  -> f accu t
  | Ttuple l                   -> List.fold_left f accu l
  | Tset t                     -> f accu t
  | Tmap (kt, vt)              -> f (f accu kt) vt
  | Tbig_map (kt, vt)          -> f (f accu kt) vt
  | Titerable_big_map (kt, vt) -> f (f accu kt) vt
  | Tor (lt, rt)               -> f (f accu lt) rt
  | Trecord _                  -> accu
  | Tevent _                   -> accu
  | Tlambda (at, rt)           -> f (f accu at) rt
  | Tunit                      -> accu
  | Toperation                 -> accu
  | Tcontract t                -> f accu t
  | Tticket t                  -> f accu t
  | Tsapling_state _           -> accu
  | Tsapling_transaction _     -> accu

(* -------------------------------------------------------------------- *)

let map_for_ident (g : 'id -> 'id) = function
  | FIsimple i             -> FIsimple (g i)
  | FIdouble (x, y)        -> FIdouble (g x, g y)

let map_assign_kind (g : 'id -> 'id) f = function
  | Avar id              -> Avar (g id)
  | Avarstore id         -> Avarstore (g id)
  | Aasset (an, fn, k)   -> Aasset (g an, g fn, f k)
  | Arecord (lv, rn, fn) -> Arecord (f lv, g rn, g fn)
  | Atuple (lv, n, l)    -> Atuple (f lv, n, l)
  | Astate               -> Astate
  | Aoperations          -> Aoperations

let map_var_kind f = function
  | Vassetstate mt -> Vassetstate (f mt)
  | Vstorevar -> Vstorevar
  | Vstorecol -> Vstorecol
  | Vlocal -> Vlocal
  | Vparam -> Vparam
  | Vfield -> Vfield
  | Vstate -> Vstate
  | Vthe -> Vthe
  | Vparameter -> Vparameter

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

let map_transfer_kind (fi : ident -> ident) (ft : type_ -> type_) f = function
  | TKsimple (x, d)         -> TKsimple (f x, f d)
  | TKcall (x, id, t, d, a) -> TKcall (f x, fi id, ft t, f d, f a)
  | TKentry (x, e, a)       -> TKentry (f x, f e, f a)
  | TKgen (x, en, cn, t, ad, a)-> TKgen (f x, en, cn, t, f ad, f a)
  | TKself (x, id, args)    -> TKself (f x, fi id, List.map (fun (id, v) -> fi id, f v) args)
  | TKoperation x           -> TKoperation (f x)

let map_detach_kind (fi : ident -> ident) (ft : type_ -> type_) f = function
  | DK_option (ty, id)-> DK_option (ft ty, fi id)
  | DK_map (ty, id, k)-> DK_map (ft ty, fi id, f k)

let map_term_node_internal (fi : ident -> ident) (g : 'id -> 'id) (ft : type_ -> type_) (f : mterm -> mterm) = function
  (* lambda *)
  | Mletin (i, a, t, b, o)         -> Mletin (List.map g i, (match a with | LVsimple a -> LVsimple (f a) | LVreplace (id, k, fa) -> LVreplace (g id, k, f fa)), Option.map ft t, f b, Option.map f o)
  | Mdeclvar (i, t, v, c)          -> Mdeclvar (List.map g i, Option.map ft t, f v, c)
  | Mdeclvaropt (i, t, v, fa, c)   -> Mdeclvaropt (List.map g i, Option.map ft t, f v, Option.map f fa, c)
  | Mapp (e, args)                 -> Mapp (g e, List.map f args)
  (* assign *)
  | Massign (op, t, k, v)          -> Massign (op, ft t, map_assign_kind g f k, f v)
  | Massignopt (op, t, k, v, fa)   -> Massignopt (op, ft t, map_assign_kind g f k, f v, f fa)
  (* control *)
  | Mif (c, t, e)                  -> Mif (f c, f t, Option.map f e)
  | Mmatchwith (e, l)              -> Mmatchwith (f e, List.map (fun (p, e) -> (p, f e)) l)
  | Minstrmatchoption (x, i, ve, ne)       -> Minstrmatchoption (f x, g i, f ve, f ne)
  | Minstrmatchor (x, lid, le, rid, re)    -> Minstrmatchor     (f x, g lid, f le, g rid, f re)
  | Minstrmatchlist (x, hid, tid, hte, ee) -> Minstrmatchlist   (f x, g hid, g tid, f hte, f ee)
  | Mfor (i, c, b)                 -> Mfor (map_for_ident g i, map_iter_container_kind fi f c, f b)
  | Miter (i, a, b, c, strict)     -> Miter (g i, f a, f b, f c, strict)
  | Mwhile (c, b)                  -> Mwhile (f c, f b)
  | Mseq is                        -> Mseq (List.map f is)
  | Mreturn x                      -> Mreturn (f x)
  (* effect *)
  | Mfail v                        -> Mfail (match v with | Invalid v -> Invalid (f v) | _ -> v)
  | Mfailsome v                    -> Mfailsome (f v)
  | Mtransfer tr                   -> Mtransfer (map_transfer_kind fi ft f tr)
  | Memit (e, x)                   -> Memit (g e, f x)
  | Mdetach (id, dk, ty, fa)       -> Mdetach (g id, map_detach_kind fi ft f dk, ft ty, f fa)
  (* entrypoint *)
  | Mgetentrypoint (t, a, s)       -> Mgetentrypoint (ft t, g a, f s)
  | Mcallview (t, a, b, c)         -> Mcallview (ft t, f a, g b, f c)
  | Mimportcallview (t, a, b, c)   -> Mimportcallview (ft t, f a, g b, f c)
  | Mself id                       -> Mself (g id)
  (* operation *)
  | Moperations                    -> Moperations
  | Mmakeoperation (v, d, a)       -> Mmakeoperation (f v, f d, f a)
  | Mmakeevent (t, id, a)          -> Mmakeevent (ft t, g id, f a)
  | Mcreatecontract (ms, d, a, si) -> Mcreatecontract (ms, f d, f a, f si)
  (* literals *)
  | Mint v                         -> Mint v
  | Mnat v                         -> Mnat v
  | Mbool v                        -> Mbool v
  | Mrational (n, d)               -> Mrational (n, d)
  | Mstring v                      -> Mstring v
  | Mmutez v                       -> Mmutez v
  | Maddress v                     -> Maddress v
  | Mdate v                        -> Mdate v
  | Mduration v                    -> Mduration v
  | Mtimestamp v                   -> Mtimestamp v
  | Mbytes v                       -> Mbytes v
  | Mchain_id v                    -> Mchain_id v
  | Mkey v                         -> Mkey v
  | Mkey_hash v                    -> Mkey_hash v
  | Msignature v                   -> Msignature v
  | Mbls12_381_fr v                -> Mbls12_381_fr v
  | Mbls12_381_fr_n v              -> Mbls12_381_fr_n v
  | Mbls12_381_g1 v                -> Mbls12_381_g1 v
  | Mbls12_381_g2 v                -> Mbls12_381_g2 v
  | Munit                          -> Munit
  | MsaplingStateEmpty n           -> MsaplingStateEmpty n
  | MsaplingTransaction (n, v)     -> MsaplingTransaction (n, v)
  | Mchest v                       -> Mchest v
  | Mchest_key v                   -> Mchest_key v
  | Mtz_expr v                     -> Mtz_expr v
  (* control expression *)
  | Mexprif (c, t, e)              -> Mexprif        (f c, f t, f e)
  | Mexprmatchwith (e, l)          -> Mexprmatchwith (f e, List.map (fun (p, e) -> (p, f e)) l)
  | Mmatchoption (x, i, ve, ne)    -> Mmatchoption   (f x, g i, f ve, f ne)
  | Mmatchor (x, lid, le, rid, re) -> Mmatchor       (f x, g lid, f le, g rid, f re)
  | Mmatchlist (x, hid, tid, hte, ee) -> Mmatchlist  (f x, g hid, g tid, f hte, f ee)
  | Mternarybool (c, a, b)         -> Mternarybool   (f c, f a, f b)
  | Mternaryoption (c, a, b)       -> Mternaryoption (f c, f a, f b)
  | Mfold (x, i, e)                -> Mfold (f x, g i, f e)
  | Mmap (x, i, e)                 -> Mmap           (f x, g i, f e)
  | Mexeclambda  (l, a)            -> Mexeclambda    (f l, f a)
  | Mapplylambda (l, a)            -> Mapplylambda   (f l, f a)
  (* composite type constructors *)
  | Mleft (t, x)                   -> Mleft (ft t, f x)
  | Mright (t, x)                  -> Mright (ft t, f x)
  | Mnone                          -> Mnone
  | Msome v                        -> Msome (f v)
  | Mtuple l                       -> Mtuple (List.map f l)
  | Masset l                       -> Masset (List.map f l)
  | Massets l                      -> Massets (List.map f l)
  | Mlitset l                      -> Mlitset (List.map f l)
  | Mlitlist l                     -> Mlitlist (List.map f l)
  | Mlitmap (b, l)                 -> Mlitmap (b, List.map (pair_sigle_map f) l)
  | Mlitrecord l                   -> Mlitrecord (List.map ((fun (x, y) -> (x, f y))) l)
  | Mlitevent l                    -> Mlitevent (List.map ((fun (x, y) -> (x, f y))) l)
  | Mlambda (rt, id, at, e)        -> Mlambda (ft rt, g id, ft at, f e)
  (* access *)
  | Mdot (e, i)                    -> Mdot (f e, g i)
  | Mdotassetfield (an, k, fn)     -> Mdotassetfield (g an, f k, g fn)
  | Mquestionoption (a, fn)        -> Mquestionoption (f a, g fn)
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
  | Mgreedyand (l, r)              -> Mgreedyand (f l, f r)
  | Mgreedyor (l, r)               -> Mgreedyor (f l, f r)
  | Mxor (l, r)                    -> Mxor (f l, f r)
  | Mnot e                         -> Mnot (f e)
  | Mplus (l, r)                   -> Mplus (f l, f r)
  | Mminus (l, r)                  -> Mminus (f l, f r)
  | Mmult (l, r)                   -> Mmult (f l, f r)
  | Mdivrat (l, r)                 -> Mdivrat (f l, f r)
  | Mdiveuc (l, r)                 -> Mdiveuc (f l, f r)
  | Mmodulo (l, r)                 -> Mmodulo (f l, f r)
  | Mdivmod (l, r)                 -> Mdivmod (f l, f r)
  | Muminus e                      -> Muminus (f e)
  | MthreeWayCmp (l, r)            -> MthreeWayCmp (f l, f r)
  | Mshiftleft (l, r)              -> Mshiftleft (f l, f r)
  | Mshiftright (l, r)             -> Mshiftright (f l, f r)
  | Msubnat (l, r)                 -> Msubnat (f l, f r)
  | Msubmutez (l, r)               -> Msubmutez (f l, f r)
  (* asset api effect *)
  | Maddasset (an, i)              -> Maddasset (fi an, f i)
  | Mputsingleasset (an, i)        -> Mputsingleasset (fi an, f i)
  | Mputasset (an, k, v)           -> Mputasset (fi an, f k, f v)
  | Maddfield (an, fn, c, i)       -> Maddfield (fi an, fi fn, f c, f i)
  | Mremoveasset (an, i)           -> Mremoveasset (fi an, f i)
  | Mremovefield (an, fn, c, i)    -> Mremovefield (fi an, fi fn, f c, f i)
  | Mremoveall (an, v)             -> Mremoveall (fi an, map_container_kind fi f v)
  | Mremoveif (an, c, la, lb, a)   -> Mremoveif (fi an, map_container_kind fi f c, List.map (fun (i, t) -> (fi i, ft t)) la, f lb, List.map f a)
  | Mclear (an, v)                 -> Mclear (fi an, map_container_kind fi f v)
  | Mset (an, l, k, v)             -> Mset (fi an, List.map fi l, f k, f v)
  | Mupdate (an, k, l)             -> Mupdate (fi an, f k, List.map (fun (id, op, v) -> (g id, op, f v)) l)
  | Mupdateall (an, c, l)          -> Mupdateall (fi an, map_container_kind fi f c, List.map (fun (id, op, v) -> (g id, op, f v)) l)
  | Maddupdate (an, c, k, l)       -> Maddupdate (fi an, map_container_kind fi f c, f k, List.map (fun (id, op, v) -> (g id, op, f v)) l)
  | Mputremove (an, c, k, v)       -> Mputremove (fi an, map_container_kind fi f c, f k, f v)
  (* asset api expression *)
  | Mget (an, c, k)                -> Mget (fi an, map_container_kind fi f c, f k)
  | Mgetsome (an, c, k)            -> Mgetsome (fi an, map_container_kind fi f c, f k)
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
  | Mrecupdate (x, l)              -> Mrecupdate (f x, List.map (fun (i, v) -> i, f v) l)
  | Mmakeasset (an, k, v)          -> Mmakeasset (fi an, f k, f v)
  | Mtocontainer an                -> Mtocontainer an
  | Mglobal_constant (t, v)        -> Mglobal_constant (ft t, f v)
  (* set api expression *)
  | Msetadd (t, c, a)              -> Msetadd (ft t, f c, f a)
  | Msetremove (t, c, a)           -> Msetremove (ft t, f c, f a)
  | Msetupdate (t, c, b, v)        -> Msetupdate (ft t, f c, f b, f v)
  | Msetcontains (t, c, a)         -> Msetcontains (ft t, f c, f a)
  | Msetlength (t, c)              -> Msetlength (ft t, f c)
  | Msetfold (t, ix, ia, c, a, b)  -> Msetfold (ft t, g ix, g ia, f c, f a, f b)
  (* set api instruction *)
  | Msetinstradd (t, ak, a)        -> Msetinstradd (ft t, map_assign_kind g f ak, f a)
  | Msetinstrremove (t, ak, a)     -> Msetinstrremove (ft t, map_assign_kind g f ak, f a)
  (* list api expression *)
  | Mlistprepend (t, c, a)         -> Mlistprepend (ft t, f c, f a)
  | Mlistlength(t, c)              -> Mlistlength(ft t, f c)
  | Mlistcontains (t, c, a)        -> Mlistcontains (ft t, f c, f a)
  | Mlistnth (t, c, a)             -> Mlistnth (ft t, f c, f a)
  | Mlisthead (t, c, a)            -> Mlisthead (ft t, f c, f a)
  | Mlisttail (t, c, a)            -> Mlisttail (ft t, f c, f a)
  | Mlistreverse(t, l)             -> Mlistreverse(ft t, f l)
  | Mlistconcat(t, l, m)           -> Mlistconcat(ft t, f l, f m)
  | Mlistfold (t, ix, ia, c, a, b) -> Mlistfold (ft t, g ix, g ia, f c, f a, f b)
  (* list api instruction *)
  | Mlistinstrprepend (t, ak, a)   -> Mlistinstrprepend (ft t, map_assign_kind g f ak, f a)
  | Mlistinstrconcat (t, ak, a)    -> Mlistinstrconcat (ft t, map_assign_kind g f ak, f a)
  (* map api expression *)
  | Mmapput (mk, tk, tv, c, k, v)      -> Mmapput (mk, ft tk, ft tv, f c, f k, f v)
  | Mmapremove (mk, tk, tv, c, k)      -> Mmapremove (mk, ft tk, ft tv, f c, f k)
  | Mmapupdate (mk, tk, tv, c, k, v)   -> Mmapupdate (mk, ft tk, ft tv, f c, f k, f v)
  | Mmapget (mk, tk, tv, c, k, an)     -> Mmapget (mk, ft tk, ft tv, f c, f k, Option.map fi an)
  | Mmapgetopt (mk, tk, tv, c, k)      -> Mmapgetopt (mk, ft tk, ft tv, f c, f k)
  | Mmapcontains (mk, tk, tv, c, k)    -> Mmapcontains (mk, ft tk, ft tv, f c, f k)
  | Mmaplength (mk, tk, tv, c)         -> Mmaplength (mk, ft tk, ft tv, f c)
  | Mmapfold (mk, t, ik, iv, ia, c, a, b) -> Mmapfold (mk, ft t, g ik, g iv, g ia, f c, f a, f b)
  (* map api instruction *)
  | Mmapinstrput (mk, tk, tv, ak, k, v)    -> Mmapinstrput (mk, ft tk, ft tv, map_assign_kind g f ak, f k, f v)
  | Mmapinstrremove (mk, tk, tv, ak, k)    -> Mmapinstrremove (mk, ft tk, ft tv, map_assign_kind g f ak, f k)
  | Mmapinstrupdate (mk, tk, tv, ak, k, v) -> Mmapinstrupdate (mk, ft tk, ft tv, map_assign_kind g f ak, f k, f v)
  (* builtin functions *)
  | Mmin (l, r)                    -> Mmin (f l, f r)
  | Mmax (l, r)                    -> Mmax (f l, f r)
  | Mabs a                         -> Mabs (f a)
  | Mconcat (x, y)                 -> Mconcat (f x, f y)
  | Mconcatlist (x)                -> Mconcatlist (f x)
  | Mslice (x, s, e)               -> Mslice (f x, f s, f e)
  | Mlength x                      -> Mlength (f x)
  | Misnone x                      -> Misnone (f x)
  | Missome x                      -> Missome (f x)
  | Minttonat x                    -> Minttonat (f x)
  | Mfloor x                       -> Mfloor (f x)
  | Mceil x                        -> Mceil (f x)
  | Mnattostring x                 -> Mnattostring (f x)
  | Mbytestonat x                  -> Mbytestonat (f x)
  | Mnattobytes x                  -> Mnattobytes (f x)
  | Mbytestoint x                  -> Mbytestoint (f x)
  | Minttobytes x                  -> Minttobytes (f x)
  | Mpack x                        -> Mpack (f x)
  | Munpack (t, x)                 -> Munpack (ft t, f x)
  | Msetdelegate x                 -> Msetdelegate (f x)
  | Mkeyhashtocontract x           -> Mkeyhashtocontract (f x)
  | Mcontracttoaddress x           -> Mcontracttoaddress (f x)
  | Maddresstocontract (t, x)      -> Maddresstocontract (t, f x)
  | Mkeytoaddress x                -> Mkeytoaddress (f x)
  | Msimplify_rational x           -> Msimplify_rational (f x)
  | Mget_numerator x               -> Mget_numerator (f x)
  | Mget_denominator x             -> Mget_denominator (f x)
  | Misimplicitaddress x           -> Misimplicitaddress (f x)
  (* crypto functions *)
  | Mblake2b x                     -> Mblake2b (f x)
  | Msha256 x                      -> Msha256  (f x)
  | Msha512 x                      -> Msha512  (f x)
  | Msha3 x                        -> Msha3    (f x)
  | Mkeccak x                      -> Mkeccak  (f x)
  | Mkeytokeyhash x                -> Mkeytokeyhash (f x)
  | Mchecksignature (k, s, x)      -> Mchecksignature (f k, f s, f x)
  (* voting *)
  | Mtotalvotingpower              -> Mtotalvotingpower
  | Mvotingpower x                 -> Mvotingpower (f x)
  (* ticket *)
  | Mcreateticket (x, a)           -> Mcreateticket (f x, f a)
  | Mreadticket x                  -> Mreadticket (f x)
  | Msplitticket (x, a, b)         -> Msplitticket (f x, f a, f b)
  | Mjointickets (x, y)            -> Mjointickets (f x, f y)
  (* sapling *)
  | Msapling_empty_state    n      -> Msapling_empty_state   n
  | Msapling_verify_update (s, t)  -> Msapling_verify_update (f s, f t)
  (* bls curve *)
  | Mpairing_check x               -> Mpairing_check (f x)
  (* timelock *)
  | Mopen_chest (x, y, z)          -> Mopen_chest (f x, f y, f z)
  (* constants *)
  | Mnow                           -> Mnow
  | Mtransferred                   -> Mtransferred
  | Mcaller                        -> Mcaller
  | Mbalance                       -> Mbalance
  | Msource                        -> Msource
  | Mselfaddress                   -> Mselfaddress
  | Mselfchainid                   -> Mselfchainid
  | Mmetadata                      -> Mmetadata
  | Mlevel                         -> Mlevel
  | Mminblocktime                  -> Mminblocktime
  (* variable *)
  | Mvar (id, k)                   -> Mvar (g id, map_var_kind f k)
  | Menumval (id, args, e)         -> Menumval (g id, List.map f args, g e)
  (* rational *)
  | Mrateq (l, r)                  -> Mrateq (f l, f r)
  | Mratcmp (op, l, r)             -> Mratcmp (op, f l, f r)
  | Mratarith (op, l, r)           -> Mratarith (op, f l, f r)
  | Mratuminus v                   -> Mratuminus (f v)
  | Mrattez (c, t)                 -> Mrattez (f c, f t)
  | Mnattoint e                    -> Mnattoint (f e)
  | Mnattorat e                    -> Mnattorat (f e)
  | Minttorat e                    -> Minttorat (f e)
  | Mratdur (c, t)                 -> Mratdur (f c, f t)
  (* others *)
  | Minttodate v                   -> Minttodate (f v)
  | Mmuteztonat v                  -> Mmuteztonat (f v)

let map_mterm
    (f : mterm -> mterm)
    ?(fi : ident -> ident = id)
    ?(g  : 'id -> 'id = id)
    ?(ft : type_ -> type_ = id)
    (mt : mterm) : mterm =
  {
    mt with
    node  = map_term_node_internal fi g ft f mt.node;
    type_ = ft mt.type_;
  }

type 't ctx_model_gen = {
  formula: bool;
  fs : function_struct option;
  label: mident option;
  spec_id : mident option;
  invariant_id : mident option;
  custom: 't;
}

type ctx_model = unit ctx_model_gen

let mk_ctx_model ?(formula = false) ?fs ?label ?spec_id ?invariant_id custom : 't ctx_model_gen =
  { formula; fs; label; spec_id; invariant_id; custom}

let map_mterm_model_gen custom (f : 't ctx_model_gen -> mterm -> mterm) (model : model) : model =
  let map_storage_item (ctx : 't ctx_model_gen) (si : storage_item) : storage_item = (
    { si with
      default = f ctx si.default;
    }
  ) in
  let map_function_struct (ctx : 't ctx_model_gen) (fs : function_struct) : function_struct =
    let ctx = { ctx with fs = Some fs } in
    let body = f ctx fs.body in
    { fs with body = body }
  in
  let map_function (ctx : 't ctx_model_gen) (fun_ : function__) : function__ = (
    let node = match fun_.node with
      | Function (fs, ret)     -> Function (map_function_struct ctx fs, ret)
      | Getter   (fs, ret)     -> Getter   (map_function_struct ctx fs, ret)
      | View     (fs, ret, vv) -> View     (map_function_struct ctx fs, ret, vv)
      | Entry     fs           -> Entry    (map_function_struct ctx fs)
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

let map_mterm_model (f : 't ctx_model_gen -> mterm -> mterm) (model : model) : model =
  map_mterm_model_gen () f model

let fold_assign_kind f accu = function
  | Avar _              -> accu
  | Avarstore _         -> accu
  | Aasset  (_, _, mt)  -> f accu mt
  | Arecord (lv, _, _)  -> f accu lv
  | Atuple  (lv, _, _)  -> f accu lv
  | Astate              -> accu
  | Aoperations         -> accu

let fold_var_kind f accu = function
  | Vassetstate mt -> f accu mt
  | Vstorevar
  | Vstorecol
  | Vlocal
  | Vparam
  | Vfield
  | Vstate
  | Vthe
  | Vparameter -> accu

let fold_container_kind f accu = function
  | CKcoll             -> accu
  | CKview mt          -> f accu mt
  | CKfield (_, _, mt) -> f accu mt

let fold_iter_container_kind f accu = function
  | ICKcoll  _          -> accu
  | ICKview  mt         -> f accu mt
  | ICKfield (_, _, mt) -> f accu mt
  | ICKset   mt         -> f accu mt
  | ICKlist  mt         -> f accu mt
  | ICKmap   mt         -> f accu mt

let fold_transfer_kind f accu = function
  | TKsimple (x, d)        -> f (f accu x) d
  | TKcall (x, _, _, d, a) -> f (f (f accu x) d) a
  | TKentry (x, e, a)      -> f (f (f accu x) e) a
  | TKgen (x, _en, _cn, _t, ad, a)-> f (f (f accu x) ad) a
  | TKself (x, _, args)    -> List.fold_left f (f accu x) (List.map snd args)
  | TKoperation x          -> f accu x

let fold_detach_kind f accu = function
  | DK_option _ -> accu
  | DK_map (_, _, k) -> f accu k

let fold_term (f : 'a -> mterm -> 'a) (accu : 'a) (term : mterm) : 'a =
  let opt f accu x = match x with | Some v -> f accu v | None -> accu in
  match term.node with
  (* lambda *)
  | Mletin (_, a, _, b, o)                -> let tmp = f (match a with | LVsimple a -> f accu a | LVreplace (_, _, a) -> f accu a) b in Option.map_dfl (f tmp) tmp o
  | Mdeclvar (_, _, v, _)                 -> f accu v
  | Mdeclvaropt (_, _, v, fa, _)          -> let tmp = f accu v in Option.map_dfl (f tmp) tmp fa
  | Mapp (_, args)                        -> List.fold_left f accu args
  (* assign *)
  | Massign (_, _, k, e)                  -> f (fold_assign_kind f accu k) e
  | Massignopt (_, _, k, e, fa)           -> f (f (fold_assign_kind f accu k) e) fa
  (* control *)
  | Mif (c, t, e)                         -> opt f (f (f accu c) t) e
  | Mmatchwith (e, l)                     -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Minstrmatchoption (x, _, ve, ne)      -> f (f (f accu x) ve) ne
  | Minstrmatchor (x, _, le, _, re)       -> f (f (f accu x) le) re
  | Minstrmatchlist (x, _, _, hte, ee)    -> f (f (f accu x) hte) ee
  | Mfor (_, c, b)                        -> f (fold_iter_container_kind f accu c) b
  | Miter (_, a, b, c, _)                 -> f (f (f accu a) b) c
  | Mwhile (c, b)                         -> f (f accu c) b
  | Mseq is                               -> List.fold_left f accu is
  | Mreturn x                             -> f accu x
  (* effect *)
  | Mfail v                               -> (match v with | Invalid v -> f accu v | _ -> accu)
  | Mfailsome v                           -> f accu v
  | Mtransfer tr                          -> fold_transfer_kind f accu tr
  | Memit (_, x)                          -> f accu x
  | Mdetach (_, dk, _, x)                 -> f (fold_detach_kind f accu dk) x
  (* entrypoint *)
  | Mgetentrypoint (_, _, s)              -> f accu s
  | Mcallview (_, a, _, c)                -> f (f accu a) c
  | Mimportcallview (_, a, _, c)          -> f (f accu a) c
  | Mself _                               -> accu
  (* operation *)
  | Moperations                           -> accu
  | Mmakeoperation (v, d, a)              -> f (f (f accu v) d) a
  | Mmakeevent (_, _, a)                  -> f accu a
  | Mcreatecontract (_, d, a, si)         -> f (f (f accu d) a) si
  (* literals *)
  | Mint _                                -> accu
  | Mnat _                                -> accu
  | Mbool _                               -> accu
  | Mrational _                           -> accu
  | Mstring _                             -> accu
  | Mmutez _                              -> accu
  | Maddress _                            -> accu
  | Mdate _                               -> accu
  | Mduration _                           -> accu
  | Mtimestamp _                          -> accu
  | Mbytes _                              -> accu
  | Mchain_id _                           -> accu
  | Mkey _                                -> accu
  | Mkey_hash _                           -> accu
  | Msignature _                          -> accu
  | Mbls12_381_fr _                       -> accu
  | Mbls12_381_fr_n _                     -> accu
  | Mbls12_381_g1 _                       -> accu
  | Mbls12_381_g2 _                       -> accu
  | Munit                                 -> accu
  | MsaplingStateEmpty _                  -> accu
  | MsaplingTransaction _                 -> accu
  | Mchest _                              -> accu
  | Mchest_key _                          -> accu
  | Mtz_expr _                            -> accu
  (* control expression *)
  | Mexprif (c, t, e)                     -> f (f (f accu c) t) e
  | Mexprmatchwith (e, l)                 -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Mmatchoption (x, _, ve, ne)           -> f (f (f accu x) ve) ne
  | Mmatchor (x, _, le, _, re)            -> f (f (f accu x) le) re
  | Mmatchlist (x, _, _, hte, ee)         -> f (f (f accu x) hte) ee
  | Mternarybool (c, a, b)                -> f (f (f accu c) a) b
  | Mternaryoption (c, a, b)              -> f (f (f accu c) a) b
  | Mfold (x, _, e)                       -> f (f accu x) e
  | Mmap (x, _, e)                        -> f (f accu x) e
  | Mexeclambda  (l, a)                   -> f (f accu l) a
  | Mapplylambda (l, a)                   -> f (f accu l) a
  (* composite type constructors *)
  | Mleft (_, x)                          -> f accu x
  | Mright (_, x)                         -> f accu x
  | Mnone                                 -> accu
  | Msome v                               -> f accu v
  | Mtuple l                              -> List.fold_left f accu l
  | Masset l                              -> List.fold_left f accu l
  | Massets l                             -> List.fold_left f accu l
  | Mlitset l                             -> List.fold_left f accu l
  | Mlitlist l                            -> List.fold_left f accu l
  | Mlitmap (_, l)                        -> List.fold_left (fun accu (k, v) -> f (f accu k) v) accu l
  | Mlitrecord l                          -> List.fold_left (fun accu (_, v) -> f accu v) accu l
  | Mlitevent l                           -> List.fold_left (fun accu (_, v) -> f accu v) accu l
  | Mlambda (_, _, _, e)                  -> f accu e
  (* access *)
  | Mdot (e, _)                           -> f accu e
  | Mdotassetfield (_, k, _)              -> f accu k
  | Mquestionoption (a, _)                -> f accu a
  (* comparison operators *)
  | Mequal (_, l, r)                      -> f (f accu l) r
  | Mnequal (_, l, r)                     -> f (f accu l) r
  | Mgt (l, r)                            -> f (f accu l) r
  | Mge (l, r)                            -> f (f accu l) r
  | Mlt (l, r)                            -> f (f accu l) r
  | Mle (l, r)                            -> f (f accu l) r
  | Mmulticomp (e, l)                     -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  (* arithmetic operators *)
  | Mand (l, r)                           -> f (f accu l) r
  | Mor (l, r)                            -> f (f accu l) r
  | Mgreedyand (l, r)                     -> f (f accu l) r
  | Mgreedyor (l, r)                      -> f (f accu l) r
  | Mxor (l, r)                           -> f (f accu l) r
  | Mnot e                                -> f accu e
  | Mplus (l, r)                          -> f (f accu l) r
  | Mminus (l, r)                         -> f (f accu l) r
  | Mmult (l, r)                          -> f (f accu l) r
  | Mdivrat (l, r)                        -> f (f accu l) r
  | Mdiveuc (l, r)                        -> f (f accu l) r
  | Mmodulo (l, r)                        -> f (f accu l) r
  | Mdivmod (l, r)                        -> f (f accu l) r
  | Muminus e                             -> f accu e
  | MthreeWayCmp (l, r)                   -> f (f accu l) r
  | Mshiftleft (l, r)                     -> f (f accu l) r
  | Mshiftright (l, r)                    -> f (f accu l) r
  | Msubnat (l, r)                        -> f (f accu l) r
  | Msubmutez (l, r)                      -> f (f accu l) r
  (* asset api effect *)
  | Maddasset (_, i)                      -> f accu i
  | Mputsingleasset (_, i)                -> f accu i
  | Mputasset (_, k, v)                   -> f (f accu k) v
  | Maddfield (_, _, c, i)                -> f (f accu c) i
  | Mremoveasset (_, i)                   -> f accu i
  | Mremovefield (_, _, c, i)             -> f (f accu c) i
  | Mremoveall (_, v)                     -> fold_container_kind f accu v
  | Mremoveif (_, c, _, lb, a)            -> List.fold_left (fun accu x -> f accu x) (f (fold_container_kind f accu c) lb) a
  | Mclear (_, v)                         -> fold_container_kind f accu v
  | Mset (_, _, k, v)                     -> f (f accu v) k
  | Mupdate (_, k, l)                     -> List.fold_left (fun accu (_, _, v) -> f accu v) (f accu k) l
  | Mupdateall (_, c, l)                  -> List.fold_left (fun accu (_, _, v) -> f accu v) (fold_container_kind f accu c) l
  | Maddupdate (_, c, k, l)               -> List.fold_left (fun accu (_, _, v) -> f accu v) (f (fold_container_kind f accu c) k) l
  | Mputremove (_, _, k, v)               -> f (f accu v) k
  (* asset api expression *)
  | Mget (_, c, k)                        -> f (fold_container_kind f accu c) k
  | Mgetsome (_, c, k)                    -> f (fold_container_kind f accu c) k
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
  | Mrecupdate (x, l)                     -> List.fold_left (fun accu (_, v) -> f accu v) (f accu x) l
  | Mmakeasset (_, k, v)                  -> f (f accu k) v
  | Mtocontainer _                        -> accu
  | Mglobal_constant (_, v)               -> f accu v
  (* set api expression *)
  | Msetadd (_, c, a)                     -> f (f accu c) a
  | Msetremove (_, c, a)                  -> f (f accu c) a
  | Msetupdate (_, c, b, v)               -> f (f (f accu c) b) v
  | Msetcontains (_, c, a)                -> f (f accu c) a
  | Msetlength (_, c)                     -> f accu c
  | Msetfold (_, _, _, c, a, b)           -> f (f (f accu c) a) b
  (* set api instruction *)
  | Msetinstradd (_, _, a)                -> f accu a
  | Msetinstrremove (_, _, a)             -> f accu a
  (* list api expression *)
  | Mlistprepend (_, c, a)                -> f (f accu c) a
  | Mlistlength (_, c)                    -> f accu c
  | Mlistcontains (_, c, a)               -> f (f accu c) a
  | Mlistnth (_, c, a)                    -> f (f accu c) a
  | Mlisthead (_, c, a)                   -> f (f accu c) a
  | Mlisttail (_, c, a)                   -> f (f accu c) a
  | Mlistreverse (_, l)                   -> f accu l
  | Mlistconcat (_, l, m)                 -> f (f accu l) m
  | Mlistfold (_, _, _, c, a, b)          -> f (f (f accu c) a) b
  (* list api instruction *)
  | Mlistinstrprepend (_, _, a)           -> f accu a
  | Mlistinstrconcat (_, _, a)            -> f accu a
  (* map api expression *)
  | Mmapput (_, _, _, c, k, v)            -> f (f (f accu c) k) v
  | Mmapremove (_, _, _, c, k)            -> f (f accu c) k
  | Mmapupdate (_, _, _, c, k, v)         -> f (f (f accu c) k) v
  | Mmapget (_, _, _, c, k, _)            -> f (f accu c) k
  | Mmapgetopt (_, _, _, c, k)            -> f (f accu c) k
  | Mmapcontains (_, _, _, c, k)          -> f (f accu c) k
  | Mmaplength (_, _, _, c)               -> f accu c
  | Mmapfold (_, _, _, _, _, c, a, b)     -> f (f (f accu c) a) b
  (* map api instruction *)
  | Mmapinstrput (_, _, _, _, k, v)       -> f (f accu k) v
  | Mmapinstrremove (_, _, _, _, k)       -> f accu k
  | Mmapinstrupdate (_, _, _, _, k, v)    -> f (f accu k) v
  (* builtin functions *)
  | Mmax (l, r)                           -> f (f accu l) r
  | Mmin (l, r)                           -> f (f accu l) r
  | Mabs a                                -> f accu a
  | Mconcat (x, y)                        -> f (f accu x) y
  | Mconcatlist x                         -> f accu x
  | Mslice (x, s, e)                      -> f (f (f accu x) s) e
  | Mlength x                             -> f accu x
  | Misnone x                             -> f accu x
  | Missome x                             -> f accu x
  | Minttonat x                           -> f accu x
  | Mfloor x                              -> f accu x
  | Mceil x                               -> f accu x
  | Mnattostring x                        -> f accu x
  | Mbytestonat x                         -> f accu x
  | Mnattobytes x                         -> f accu x
  | Mbytestoint x                         -> f accu x
  | Minttobytes x                         -> f accu x
  | Mpack x                               -> f accu x
  | Munpack (_, x)                        -> f accu x
  | Msetdelegate x                        -> f accu x
  | Mkeyhashtocontract x                  -> f accu x
  | Mcontracttoaddress x                  -> f accu x
  | Maddresstocontract (_, x)             -> f accu x
  | Mkeytoaddress x                       -> f accu x
  | Msimplify_rational x                  -> f accu x
  | Mget_numerator x                      -> f accu x
  | Mget_denominator x                    -> f accu x
  | Misimplicitaddress x                  -> f accu x
  (* crypto functions *)
  | Mblake2b x                            -> f accu x
  | Msha256  x                            -> f accu x
  | Msha512  x                            -> f accu x
  | Msha3    x                            -> f accu x
  | Mkeccak  x                            -> f accu x
  | Mkeytokeyhash  x                      -> f accu x
  | Mchecksignature (k, s, x)             -> f (f (f accu k) s) x
  (* voting *)
  | Mtotalvotingpower                     -> accu
  | Mvotingpower x                        -> f accu x
  (* ticket *)
  | Mcreateticket (x, a)                  -> f (f accu x) a
  | Mreadticket x                         -> f accu x
  | Msplitticket (x, a, b)                -> f (f (f accu x) a) b
  | Mjointickets (x, y)                   -> f (f accu x) y
  (* sapling *)
  | Msapling_empty_state    _             -> accu
  | Msapling_verify_update (s, t)         -> f (f accu s) t
  (* bls curve *)
  | Mpairing_check x                      -> f accu x
  (* timelock *)
  | Mopen_chest (x, y, z)                 -> f (f (f accu x) y) z
  (* constants *)
  | Mnow                                  -> accu
  | Mtransferred                          -> accu
  | Mcaller                               -> accu
  | Mbalance                              -> accu
  | Msource                               -> accu
  | Mselfaddress                          -> accu
  | Mselfchainid                          -> accu
  | Mmetadata                             -> accu
  | Mlevel                                -> accu
  | Mminblocktime                         -> accu
  (* variable *)
  | Mvar (_, k)                           -> fold_var_kind f accu k
  | Menumval (_, args, _)                 -> List.fold_left f accu args
  (* rational *)
  | Mrateq (l, r)                         -> f (f accu l) r
  | Mratcmp (_, l, r)                     -> f (f accu l) r
  | Mratarith (_, l, r)                   -> f (f accu l) r
  | Mratuminus v                          -> f accu v
  | Mrattez (c, t)                        -> f (f accu c) t
  | Mnattoint e                           -> f accu e
  | Mnattorat e                           -> f accu e
  | Minttorat e                           -> f accu e
  | Mratdur (c, t)                        -> f (f accu c) t
  (* others *)
  | Minttodate v                          -> f accu v
  | Mmuteztonat v                         -> f accu v


let fold_map_term_list f acc l : 'term list * 'a =
  List.fold_left
    (fun (pterms, accu) x ->
       let p, accu = f accu x in
       pterms @ [p], accu) ([], acc) l

let fold_map_assign_kind f accu = function
  | Avar id              -> Avar id, accu
  | Avarstore id         -> Avarstore id, accu
  | Aasset (an, fn, k)   -> let ke, ka = f accu k in Aasset  (an, fn, ke), ka
  | Arecord (lv, rn, fn) -> let lve, lva = f accu lv in Arecord (lve, rn, fn), lva
  | Atuple (lv, n, l)    -> let lve, lva = f accu lv in Atuple (lve, n, l), lva
  | Astate               -> Astate, accu
  | Aoperations          -> Aoperations, accu

let fold_map_var_kind f accu = function
  | Vassetstate mt ->
    let mte, mta = f accu mt in
    Vassetstate mte, mta
  | Vstorevar -> Vstorevar, accu
  | Vstorecol -> Vstorecol, accu
  | Vlocal    -> Vlocal,    accu
  | Vparam    -> Vparam,    accu
  | Vfield    -> Vfield,    accu
  | Vstate    -> Vstate,    accu
  | Vthe      -> Vthe,      accu
  | Vparameter -> Vparameter, accu

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

let fold_map_transfer_kind f accu = function
  | TKsimple (x, d) ->
    let xe, xa = f accu x in
    let de, da = f xa d in
    TKsimple (xe, de), da
  | TKcall (x, id, t, d, a) ->
    let xe, xa = f accu x in
    let de, da = f xa d in
    let ae, aa = f da a in
    TKcall (xe, id, t, de, ae), aa
  | TKentry (x, e, a) ->
    let xe, xa = f accu x in
    let ee, ea = f xa e in
    let ae, aa = f ea a in
    TKentry (xe, ee, ae), aa
  | TKgen (x, en, cn, t, ad, a) ->
    let xe, xa = f accu x in
    let ade, ada = f xa ad in
    let ae, aa = f ada a in
    TKgen (xe, en, cn, t, ade, ae), aa
  | TKself (x, id, args)->
    let xe, xa = f accu x in
    let args, accu =
      List.fold_left (fun (args, accu) (id, a) ->
          let ae, aa = f accu a in
          (args @ [id, ae], aa)) ([], xa) args in
    TKself (xe, id, args), accu
  | TKoperation x ->
    let xe, xa = f accu x in
    TKoperation xe, xa

let fold_map_detach_kind f accu = function
  | DK_option (ty, id) -> DK_option (ty, id), accu
  | DK_map (ty, id, k) -> let ke, ka = f accu k in DK_map (ty, id, ke), ka

let fold_map_term
    (g : mterm mterm_node -> mterm)
    (f : 'a -> mterm -> mterm * 'a)
    (accu : 'a)
    (term : mterm) : mterm * 'a =

  match term.node with
  (* lambda *)

  | Mletin (idd, i, t, b, o) ->
    let ie, ia = (match i with | LVsimple a -> let e, a = f accu a in (LVsimple e, a) | LVreplace (id, k, a) -> let e, a = f accu a in (LVreplace (id, k, e), a)) in
    let be, ba = f ia b in
    let oe, oa =
      match o with
      | Some o -> f ba o |> (fun (x, y) -> (Some x, y))
      | None -> (None, ba) in
    g (Mletin (idd, ie, t, be, oe)), oa

  | Mdeclvar (ids, t, v, c) ->
    let ve, va = f accu v in
    g (Mdeclvar (ids, t, ve, c)), va

  | Mdeclvaropt (ids, t, v, fa, c) ->
    let ve, va = f accu v in
    let fae, faa =
      match fa with
      | Some o -> f va o |> (fun (x, y) -> (Some x, y))
      | None -> (None, va) in
    g (Mdeclvaropt (ids, t, ve, fae, c)), faa

  | Mapp (id, args) ->
    let ((argss, argsa) : 'c list * 'a) =
      List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) args
    in
    g (Mapp (id, argss)), argsa


  (* assign *)

  | Massign (op, t, k, v) ->
    let ke, ka = fold_map_assign_kind f accu k in
    let ve, va = f ka v in
    g (Massign (op, t, ke, ve)), va

  | Massignopt (op, t, k, v, fa) ->
    let ke, ka = fold_map_assign_kind f accu k in
    let ve, va = f ka v in
    let fae, faa = f va fa in
    g (Massignopt (op, t, ke, ve, fae)), faa

  (* control *)

  | Mif (c, t, e) ->
    let ce, ca = f accu c in
    let te, ta = f ca t in
    let ee, ea =
      match e with
      | Some v ->
        let a, b = f ta v in
        Some a, b
      | None -> None, ta
    in
    g (Mif (ce, te, ee)), ea

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

  | Minstrmatchoption (x, i, ve, ne) ->
    let xe, xa = f accu x in
    let vee, vea = f xa ve in
    let nee, nea = f vea ne in
    g (Minstrmatchoption (xe, i, vee, nee)), nea

  | Minstrmatchor (x, lid, le, rid, re) ->
    let xe, xa = f accu x in
    let lee, lea = f xa le in
    let ree, rea = f lea re in
    g (Minstrmatchor (xe, lid, lee, rid, ree)), rea

  | Minstrmatchlist (x, hid, tid, hte, ee) ->
    let xe, xa = f accu x in
    let htee, htea = f xa hte in
    let eee, eea = f htea ee in
    g (Minstrmatchlist (xe, hid, tid, htee, eee)), eea

  | Mfor (fi, c, b) ->
    let ce, ca = fold_map_iter_container_kind f accu c in
    let be, ba = f ca b in
    g (Mfor (fi, ce, be)), ba

  | Miter (i, a, b, c, strict) ->
    let ae, aa = f accu a in
    let be, ba = f aa b in
    let ce, ca = f ba c in
    g (Miter (i, ae, be, ce, strict)), ca

  | Mwhile (c, b) ->
    let ce, ca = f accu c in
    let be, ba = f ca b in
    g (Mwhile (ce, be)), ba

  | Mseq is ->
    let (isi, isa) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) is in
    g (Mseq isi), isa

  | Mreturn x ->
    let xe, xa = f accu x in
    g (Mreturn xe), xa

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

  | Mfailsome v ->
    let ve, va = f accu v in
    g (Mfailsome ve), va

  | Mtransfer tr ->
    let tre, tra = fold_map_transfer_kind f accu tr in
    g (Mtransfer tre), tra

  | Memit (e, x) ->
    let xe, xa = f accu x in
    g (Memit (e, xe)), xa

  | Mdetach (id, dk, ty, fa) ->
    let dke, dka = fold_map_detach_kind f accu dk in
    let fae, faa = f dka fa in
    g (Mdetach (id, dke, ty, fae)), faa


  (* entrypoint *)

  | Mgetentrypoint (t, a, s) ->
    let se, sa = f accu s in
    g (Mgetentrypoint (t, a, se)), sa

  | Mcallview (t, a, b, c) ->
    let ae, aa = f accu a in
    let ce, ca = f aa c in
    g (Mcallview (t, ae, b, ce)), ca

  | Mimportcallview (t, a, b, c) ->
    let ae, aa = f accu a in
    let ce, ca = f aa c in
    g (Mimportcallview (t, ae, b, ce)), ca

  | Mself id ->
    g (Mself id), accu


  (* operation *)

  | Moperations ->
    g (Moperations), accu

  | Mmakeoperation (v, d, a) ->
    let ve, va = f accu v in
    let de, da = f va d in
    let ae, aa = f da a in
    g (Mmakeoperation (ve, de, ae)), aa

  | Mmakeevent (t, id, a) ->
    let ae, aa = f accu a in
    g (Mmakeevent (t, id, ae)), aa

  | Mcreatecontract (ms, d, a, si) ->
    let de, da = f accu d in
    let ae, aa = f da a in
    let sie, sia = f aa si in
    g (Mcreatecontract (ms, de, ae, sie)), sia

  (* literals *)

  | Mint v ->
    g (Mint v), accu

  | Mnat v ->
    g (Mnat v), accu

  | Mbool v ->
    g (Mbool v), accu

  | Mrational (n, d) ->
    g (Mrational (n, d)), accu

  | Mstring v ->
    g (Mstring v), accu

  | Mmutez v ->
    g (Mmutez v), accu

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

  | Mchain_id v ->
    g (Mchain_id v), accu

  | Mkey v ->
    g (Mkey v), accu

  | Mkey_hash v ->
    g (Mkey_hash v), accu

  | Msignature v ->
    g (Msignature v), accu

  | Mbls12_381_fr v ->
    g (Mbls12_381_fr v), accu

  | Mbls12_381_fr_n v ->
    g (Mbls12_381_fr_n v), accu

  | Mbls12_381_g1 v ->
    g (Mbls12_381_g1 v), accu

  | Mbls12_381_g2 v ->
    g (Mbls12_381_g2 v), accu

  | Munit ->
    g (Munit), accu

  | MsaplingStateEmpty n ->
    g (MsaplingStateEmpty n), accu

  | MsaplingTransaction (n, v) ->
    g (MsaplingTransaction (n, v)), accu

  | Mchest v ->
    g (Mchest v), accu

  | Mchest_key v ->
    g (Mchest_key v), accu

  | Mtz_expr v ->
    g (Mtz_expr v), accu


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

  | Mmatchoption (x, i, ve, ne) ->
    let xe, xa = f accu x in
    let vee, vea = f xa ve in
    let nee, nea = f vea ne in
    g (Mmatchoption (xe, i, vee, nee)), nea

  | Mmatchor (x, lid, le, rid, re) ->
    let xe, xa = f accu x in
    let lee, lea = f xa le in
    let ree, rea = f lea re in
    g (Mmatchor (xe, lid, lee, rid, ree)), rea

  | Mmatchlist (x, hid, tid, hte, ee) ->
    let xe, xa = f accu x in
    let htee, htea = f xa hte in
    let eee, eea = f htea ee in
    g (Mmatchlist (xe, hid, tid, htee, eee)), eea

  | Mternarybool (c, a, b) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    let be, ba = f aa b in
    g (Mternarybool (ce, ae, be)), ba

  | Mternaryoption (c, a, b) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    let be, ba = f aa b in
    g (Mternaryoption (ce, ae, be)), ba

  | Mfold (x, i, e) ->
    let xe, xa = f accu x in
    let ee, ea = f xa e in
    g (Mfold (xe, i, ee)), ea

  | Mmap (x, i, e) ->
    let xe, xa = f accu x in
    let ee, ea = f xa e in
    g (Mmap (xe, i, ee)), ea

  | Mexeclambda  (l, a) ->
    let le, la = f accu l in
    let ae, aa = f la a in
    g (Mexeclambda (le, ae)), aa

  | Mapplylambda (l, a) ->
    let le, la = f accu l in
    let ae, aa = f la a in
    g (Mapplylambda (le, ae)), aa


  (* composite type constructors *)

  | Mleft (t, x) ->
    let xe, xa = f accu x in
    g (Mleft (t, xe)), xa

  | Mright (t, x) ->
    let xe, xa = f accu x in
    g (Mright (t, xe)), xa

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

  | Mlitmap (b, l) ->
    let le, la =
      List.fold_left
        (fun (pterms, accu) (k, v) ->
           let kn, accu = f accu k in
           let vn, accu = f accu v in
           pterms @ [kn, vn], accu) ([], accu) l
    in
    g (Mlitmap (b, le)), la

  | Mlitrecord l ->
    let le, la =
      List.fold_left
        (fun (pterms, accu) (i, v) ->
           let vn, accu = f accu v in
           pterms @ [i, vn], accu) ([], accu) l
    in
    g (Mlitrecord le), la

  | Mlitevent l ->
    let le, la =
      List.fold_left
        (fun (pterms, accu) (i, v) ->
           let vn, accu = f accu v in
           pterms @ [i, vn], accu) ([], accu) l
    in
    g (Mlitevent le), la

  | Mlambda (rt, id, at, e) ->
    let ee, ea = f accu e in
    g (Mlambda (rt, id, at, ee)), ea


  (* dot *)

  | Mdot (e, i) ->
    let ee, ea = f accu e in
    g (Mdot (ee, i)), ea

  | Mdotassetfield (an, k, fn) ->
    let ke, ka = f accu k in
    g (Mdotassetfield (an, ke, fn)), ka

  | Mquestionoption (a, fn) ->
    let ae, aa = f accu a in
    g (Mquestionoption (ae, fn)), aa


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

  | Mgreedyand (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mgreedyand (le, re)), ra

  | Mgreedyor (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mgreedyor (le, re)), ra

  | Mxor (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mxor (le, re)), ra

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

  | Mdivmod (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mdivmod (le, re)), ra

  | Muminus e ->
    let ee, ea = f accu e in
    g (Muminus ee), ea

  | MthreeWayCmp (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (MthreeWayCmp (le, re)), ra

  | Mshiftleft (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mshiftleft (le, re)), ra

  | Mshiftright (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mshiftright (le, re)), ra

  | Msubnat (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Msubnat (le, re)), ra

  | Msubmutez (l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Msubmutez (le, re)), ra


  (* asset api effect *)

  | Maddasset (an, i) ->
    let ie, ia = f accu i in
    g (Maddasset (an, ie)), ia

  | Mputsingleasset (an, i) ->
    let ie, ia = f accu i in
    g (Mputsingleasset (an, ie)), ia

  | Mputasset (an, k, v) ->
    let ke, ka = f accu k in
    let ve, va = f ka v in
    g (Mputasset (an, ke, ve)), va

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

  | Mremoveall (an, v) ->
    let ve, va = fold_map_container_kind f accu v in
    g (Mremoveall (an, ve)), va

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

  | Mupdateall (an, c, l) ->
    let ce, ca = fold_map_container_kind f accu c in
    let le, la =
      List.fold_left
        (fun (ps, accu) (id, op, v) ->
           let va, accu = f accu v in
           (id, op, va)::ps, accu) ([], ca) l
      |> (fun (x, y) -> (List.rev x, y))
    in
    g (Mupdateall (an, ce, le)), la

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

  | Mputremove (an, c, k, v) ->
    let ce, ca = fold_map_container_kind f accu c in
    let ke, ka = f ca k in
    let ve, va = f ka v in
    g (Mputremove (an, ce, ke, ve)), va

  (* asset api expression *)

  | Mget (an, c, k) ->
    let ce, ca = fold_map_container_kind f accu c in
    let ke, ka = f ca k in
    g (Mget (an, ce, ke)), ka

  | Mgetsome (an, c, k) ->
    let ce, ca = fold_map_container_kind f accu c in
    let ke, ka = f ca k in
    g (Mgetsome (an, ce, ke)), ka

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

  | Mrecupdate (x, l) ->
    let xe, xa = f accu x in
    let (le, la) =
      List.fold_left
        (fun (ls, accu) (i, v) ->
           let va, accu = f accu v in
           (i, va)::ls, accu) ([], xa) l
      |> (fun (x, y) -> (List.rev x, y))
    in
    g (Mrecupdate (xe, le)), la

  | Mmakeasset (an, c, i) ->
    let ke, ka = f accu c in
    let ve, va = f ka i in
    g (Mmakeasset (an, ke, ve)), va

  | Mtocontainer an ->
    g (Mtocontainer an), accu

  | Mglobal_constant (t, v) ->
    let ve, va = f accu v in
    g (Mglobal_constant (t, ve)), va

  (* set api expression *)

  | Msetadd (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Msetadd (t, ce, ae)), aa

  | Msetremove (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Msetremove (t, ce, ae)), aa

  | Msetupdate (t, c, b, v) ->
    let ce, ca = f accu c in
    let be, ba = f ca b in
    let ve, va = f ba v in
    g (Msetupdate (t, ce, be, ve)), va

  | Msetcontains (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Msetcontains (t, ce, ae)), aa

  | Msetlength (t, c) ->
    let ce, ca = f accu c in
    g (Msetlength (t, ce)), ca

  | Msetfold (t, ix, ia, c, a, b) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    let be, ba = f aa b in
    g (Msetfold (t, ix, ia, ce, ae, be)), ba


  (* set api instruction *)

  | Msetinstradd (t, ak, a) ->
    let ake, aka = fold_map_assign_kind f accu ak in
    let ae, aa = f aka a in
    g (Msetinstradd (t, ake, ae)), aa

  | Msetinstrremove (t, ak, a) ->
    let ake, aka = fold_map_assign_kind f accu ak in
    let ae, aa = f aka a in
    g (Msetinstrremove (t, ake, ae)), aa


  (* list api expression *)

  | Mlistprepend (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Mlistprepend (t, ce, ae)), aa

  | Mlistlength (t, c) ->
    let ce, ca = f accu c in
    g (Mlistlength (t, ce)), ca

  | Mlistcontains (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Mlistcontains (t, ce, ae)), aa

  | Mlistnth (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Mlistnth (t, ce, ae)), aa

  | Mlisthead (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Mlisthead (t, ce, ae)), aa

  | Mlisttail (t, c, a) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    g (Mlisttail (t, ce, ae)), aa

  | Mlistreverse (t, l) ->
    let le, la = f accu l in
    g (Mlistreverse (t, le)), la

  | Mlistconcat (t, l, m) ->
    let le, la = f accu l in
    let me, ma = f la m in
    g (Mlistconcat (t, le, me)), ma

  | Mlistfold (t, ix, ia, c, a, b) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    let be, ba = f aa b in
    g (Mlistfold (t, ix, ia, ce, ae, be)), ba


  (* list api instruction *)

  | Mlistinstrprepend (t, ak, a) ->
    let ake, aka = fold_map_assign_kind f accu ak in
    let ae, aa = f aka a in
    g (Mlistinstrprepend (t, ake, ae)), aa

  | Mlistinstrconcat (t, ak, a) ->
    let ake, aka = fold_map_assign_kind f accu ak in
    let ae, aa = f aka a in
    g (Mlistinstrconcat (t, ake, ae)), aa


  (* map api expression *)

  | Mmapput (mk, tk, tv, c, k, v) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    let ve, va = f ka v in
    g (Mmapput (mk, tk, tv, ce, ke, ve)), va

  | Mmapremove (mk, tk, tv, c, k) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    g (Mmapremove (mk, tk, tv, ce, ke)), ka

  | Mmapupdate (mk, tk, tv, c, k, v) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    let ve, va = f ka v in
    g (Mmapupdate (mk, tk, tv, ce, ke, ve)), va

  | Mmapget (mk, tk, tv, c, k, an) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    g (Mmapget (mk, tk, tv, ce, ke, an)), ka

  | Mmapgetopt (mk, tk, tv, c, k) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    g (Mmapgetopt (mk, tk, tv, ce, ke)), ka

  | Mmapcontains (mk, tk, tv, c, k) ->
    let ce, ca = f accu c in
    let ke, ka = f ca k in
    g (Mmapcontains (mk, tk, tv, ce, ke)), ka

  | Mmaplength (mk, tk, tv, c) ->
    let ce, ca = f accu c in
    g (Mmaplength (mk, tk, tv, ce)), ca

  | Mmapfold (mk, t, ik, iv, ia, c, a, b) ->
    let ce, ca = f accu c in
    let ae, aa = f ca a in
    let be, ba = f aa b in
    g (Mmapfold (mk, t, ik, iv, ia, ce, ae, be)), ba


  (* map api instruction *)

  | Mmapinstrput (mk, tk, tv, ak, k, v) ->
    let ake, aka = fold_map_assign_kind f accu ak in
    let ke, ka = f aka k in
    let ve, va = f ka v in
    g (Mmapinstrput (mk, tk, tv, ake, ke, ve)), va

  | Mmapinstrremove (mk, tk, tv, ak, k) ->
    let ake, aka = fold_map_assign_kind f accu ak in
    let ke, ka = f aka k in
    g (Mmapinstrremove (mk, tk, tv, ake, ke)), ka

  | Mmapinstrupdate (mk, tk, tv, ak, k, v) ->
    let ake, aka = fold_map_assign_kind f accu ak in
    let ke, ka = f aka k in
    let ve, va = f ka v in
    g (Mmapinstrupdate (mk, tk, tv, ake, ke, ve)), va


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

  | Mconcatlist x ->
    let xe, xa = f accu x in
    g (Mconcatlist xe), xa

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

  | Minttonat x ->
    let xe, xa = f accu x in
    g (Minttonat xe), xa

  | Misnone x ->
    let xe, xa = f accu x in
    g (Misnone xe), xa

  | Mfloor x ->
    let xe, xa = f accu x in
    g (Mfloor xe), xa

  | Mceil x ->
    let xe, xa = f accu x in
    g (Mceil xe), xa

  | Mnattostring x ->
    let xe, xa = f accu x in
    g (Mnattostring xe), xa

  | Mbytestonat x ->
    let xe, xa = f accu x in
    g (Mbytestonat xe), xa

  | Mnattobytes x ->
    let xe, xa = f accu x in
    g (Mnattobytes xe), xa

  | Mbytestoint x ->
    let xe, xa = f accu x in
    g (Mbytestoint xe), xa

  | Minttobytes x ->
    let xe, xa = f accu x in
    g (Minttobytes xe), xa

  | Mpack x ->
    let xe, xa = f accu x in
    g (Mpack xe), xa

  | Munpack (t, x) ->
    let xe, xa = f accu x in
    g (Munpack (t, xe)), xa

  | Msetdelegate x ->
    let xe, xa = f accu x in
    g (Msetdelegate xe), xa

  | Mkeyhashtocontract x ->
    let xe, xa = f accu x in
    g (Mkeyhashtocontract xe), xa

  | Mcontracttoaddress x ->
    let xe, xa = f accu x in
    g (Mcontracttoaddress xe), xa

  | Maddresstocontract (t, x) ->
    let xe, xa = f accu x in
    g (Maddresstocontract (t, xe)), xa

  | Mkeytoaddress x ->
    let xe, xa = f accu x in
    g (Mkeytoaddress xe), xa

  | Msimplify_rational x ->
    let xe, xa = f accu x in
    g (Msimplify_rational xe), xa

  | Mget_numerator x ->
    let xe, xa = f accu x in
    g (Mget_numerator xe), xa

  | Mget_denominator x ->
    let xe, xa = f accu x in
    g (Mget_denominator xe), xa

  | Misimplicitaddress x ->
    let xe, xa = f accu x in
    g (Misimplicitaddress xe), xa

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

  | Msha3 x ->
    let xe, xa = f accu x in
    g (Msha3 xe), xa

  | Mkeccak x ->
    let xe, xa = f accu x in
    g (Mkeccak xe), xa

  | Mkeytokeyhash x ->
    let xe, xa = f accu x in
    g (Mkeytokeyhash xe), xa

  | Mchecksignature (k, s, x) ->
    let ke, ka = f accu k in
    let se, sa = f ka s in
    let xe, xa = f sa x in
    g (Mchecksignature (ke, se, xe)), xa


  (* voting *)
  | Mtotalvotingpower ->
    g Mtotalvotingpower, accu

  | Mvotingpower x ->
    let xe, xa = f accu x in
    g (Mvotingpower xe), xa


  (* ticket *)

  | Mcreateticket (x, a) ->
    let xe, xa = f accu x in
    let ae, aa = f xa a in
    g (Mcreateticket (xe, ae)), aa

  | Mreadticket x ->
    let xe, xa = f accu x in
    g (Mreadticket xe), xa

  | Msplitticket (x, a, b) ->
    let xe, xa = f accu x in
    let ae, aa = f xa a in
    let be, ba = f aa b in
    g (Msplitticket (xe, ae, be)), ba

  | Mjointickets (x, y) ->
    let xe, xa = f accu x in
    let ye, ya = f xa y in
    g (Mjointickets (xe, ye)), ya


  (* sapling *)

  | Msapling_empty_state n ->
    g (Msapling_empty_state n), accu

  | Msapling_verify_update (s, t) ->
    let se, sa = f accu s in
    let te, ta = f sa t in
    g (Msapling_verify_update (se, te)), ta


  (* bls curve *)

  | Mpairing_check x ->
    let xe, xa = f accu x in
    g (Mpairing_check xe), xa


  (* bls curve *)

  | Mopen_chest (x, y, z) ->
    let xe, xa = f accu x in
    let ye, ya = f xa y in
    let ze, za = f ya z in
    g (Mopen_chest (xe, ye, ze)), za


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

  | Mselfchainid ->
    g Mselfchainid, accu

  | Mmetadata ->
    g Mmetadata, accu

  | Mlevel ->
    g Mlevel, accu

  | Mminblocktime ->
    g Mminblocktime, accu


  (* variable *)

  | Mvar (id, k) ->
    let ke, ka = fold_map_var_kind f accu k in
    g (Mvar (id, ke)), ka

  | Menumval (id, args, e) ->
    let ((argss, argsa) : 'c list * 'a) =
      List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) args
    in
    g (Menumval (id, argss, e)), argsa


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

  | Mnattoint e ->
    let ee, ea = f accu e in
    g (Mnattoint ee), ea

  | Mnattorat e ->
    let ee, ea = f accu e in
    g (Mnattorat ee), ea

  | Minttorat e ->
    let ee, ea = f accu e in
    g (Minttorat ee), ea

  | Mratdur (c, t) ->
    let ce, ca = f accu c in
    let te, ta = f ca t in
    g (Mratdur (ce, te)), ta


  (* others *)

  | Minttodate v ->
    let ve, va = f accu v in
    g (Minttodate ve), va

  | Mmuteztonat v ->
    let ve, va = f accu v in
    g (Mmuteztonat ve), va


let fold_left g l accu = List.fold_left (fun accu x -> g x accu) accu l
let fold_model (f : 't ctx_model_gen -> 'a -> mterm -> 'a) (m : model) (accu : 'a) : 'a =
  let fold_mterm_option f x accu = match x with | Some v -> f accu v | _ -> accu in
  let fold_decl (ctx : 't ctx_model_gen) (f : 't ctx_model_gen -> 'a -> mterm -> 'a) (d : decl_node) (accu : 'a) : 'a = (
    match d with
    | Dvar s ->
      accu
      |> fold_mterm_option (f ctx) s.default
    | Denum _e ->
      accu
    | Dasset a ->
      accu
      |> (fun accu -> List.fold_left (fun accu (x : asset_item) -> (fold_mterm_option (f ctx) x.default accu)) accu a.values)
    | _ -> accu
  ) in

  let fold_entry (ctx : 't ctx_model_gen) (f : 't ctx_model_gen -> 'a -> mterm -> 'a) (a : function__) (accu : 'a) : 'a = (
    match a.node with
    | Function (fs, _)
    | Getter (fs, _)
    | View (fs, _, _)
    | Entry fs -> f {ctx with fs = Some fs} accu fs.body
  ) in

  let ctx : ctx_model = mk_ctx_model () in

  accu
  |> fold_left (fold_decl ctx f) m.decls
  |> fold_left (fold_entry ctx f) m.functions

type kind_ident =
  | KIarchetype
  | KIparameter
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
  | KIgetter
  | KIview
  | KIargument
  | KIlocalvar
  | KIlabel
  | KIpredicate
  | KIdefinition
  | KIdefinitionvar
  | KIinvariant
  | KIpostcondition
  | KIpostconditionuse
  | KIfaillabel
  | KIfailfid
  | KIfailarg
  | KIsecurityad
  | KIsecurityrole
  | KIsecurityentry
  | KImterm (* mterm *)

let map_model (f : kind_ident -> ident -> ident) (for_type : type_ -> type_) (for_mterm : mterm -> mterm) (model : model) : model =
  let g k (id : mident) = (fst id, {(snd id) with pldesc=(f k (snd id).pldesc)}) in
  let h k (id : lident) = {id with pldesc=(f k id.pldesc)} in

  let for_parameter (p : parameter) : parameter =
    {
      name    = g KIparameter p.name;
      typ     = for_type p.typ;
      default = Option.map for_mterm p.default;
      value   = Option.map for_mterm p.value;
      const   = p.const;
      loc     = p.loc;
    }
  in

  let for_metadata (m : metadata_kind) : metadata_kind =
    match m with
    | MKuri  x -> MKuri  x
    | MKjson x -> MKjson x
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
        | RemoveAll (an, ck)      -> RemoveAll (f KIassetname an, ck)
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
        | Lreverse  t -> Lreverse  (for_type t)
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
        | Boptget t -> Boptget (for_type t)
        | Bfloor    -> Bfloor
        | Bceil     -> Bceil
        | Bnattostring -> Bnattostring
        | Bfail t -> Bfail (for_type t)
      in
      let for_api_internal (ainternal : api_internal) : api_internal =
        match ainternal with
        | RatEq     -> RatEq
        | RatCmp    -> RatCmp
        | RatArith  -> RatArith
        | RatUminus -> RatUminus
        | RatTez    -> RatTez
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
    }
  in
  let for_decl_node (d : decl_node) : decl_node =
    let for_var (v : var) : var =
      {
        name          = g KIdeclvarname v.name;
        type_         = for_type v.type_;
        original_type = for_type v.original_type;
        kind          = v.kind;
        default       = Option.map for_mterm v.default;
        loc           = v.loc;
      }
    in
    let for_enum (e : enum) : enum =
      let for_enum_item (ei : enum_item) : enum_item =
        {
          name        = g KIenumvalue ei.name;
          args        = List.map for_type ei.args;
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
        keys          = List.map (f KIassetfield) a.keys;
        sort          = List.map (g KIassetfield) a.sort;
        map_kind      = a.map_kind;
        state         = Option.map (h KIassetstate) a.state;
        init          = List.map for_mterm a.init;
        no_storage    = a.no_storage;
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
        pos           = r.pos;
        loc           = r.loc;
      }
    in
    match d with
    | Dvar v      -> Dvar      (for_var v)
    | Denum e     -> Denum     (for_enum e)
    | Dasset a    -> Dasset    (for_asset a)
    | Drecord r   -> Drecord   (for_record r)
    | Devent e    -> Devent    (for_record e)
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
      no_storage  = si.no_storage;
      namespace   = si.namespace;
      loc         = si.loc;
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
          name  = g (match fn with | Function _ -> KIfunction | Getter _ -> KIgetter | View _ -> KIview | Entry _ -> KIentry) fs.name;
          args  = List.map for_argument fs.args;
          eargs = List.map for_argument fs.eargs;
          stovars = fs.stovars;
          body  = for_mterm fs.body;
          loc   = fs.loc;
        }
      in
      match fn with
      | Function (fs, t)     -> Function (for_function_struct fs, for_type t)
      | Getter   (fs, t)     -> Getter   (for_function_struct fs, for_type t)
      | View     (fs, t, vv) -> View     (for_function_struct fs, for_type t, vv)
      | Entry     fs         -> Entry    (for_function_struct fs)
    in
    {
      node = for_function_node f__.node
    }
  in
  {
    name          = h KIarchetype model.name;
    parameters    = List.map for_parameter model.parameters;
    metadata      = Option.map for_metadata model.metadata;
    api_items     = List.map for_api_item  model.api_items;
    decls         = List.map for_decl_node model.decls;
    storage       = List.map for_storage_item model.storage;
    functions     = List.map for_function__ model.functions;
    extra         = model.extra;
    loc           = model.loc;
  }

let replace_ident_model (f : kind_ident -> ident -> ident) (model : model) : model =
  let h k (id : mident) = (fst id, {(snd id) with pldesc=(f k (snd id).pldesc)}) in
  (* let g k (id : lident) = {id with pldesc=(f k id.pldesc)} in *)
  let rec for_type (t : type_) : type_ =
    let for_ntype (nt : ntype) : ntype =
      match nt with
      | Tasset id                -> Tasset (h KIassetname id)
      | Tenum id                 -> Tenum (h KIenumname id)
      | Tstate                   -> nt
      | Tbuiltin _               -> nt
      | Tcontainer (a, c)        -> Tcontainer (for_type a, c)
      | Tlist a                  -> Tlist (for_type a)
      | Toption a                -> Toption (for_type a)
      | Ttuple l                 -> Ttuple (List.map for_type l)
      | Tset k                   -> Tset k
      | Tmap (k, v)              -> Tmap (k, for_type v)
      | Tbig_map (k, v)          -> Tbig_map (k, for_type v)
      | Titerable_big_map (k, v) -> Titerable_big_map (k, for_type v)
      | Tor (l, r)               -> Tor (for_type l, for_type r)
      | Trecord id               -> Trecord (h KIrecordname id)
      | Tevent id                -> Tevent (h KIrecordname id)
      | Tlambda (a, r)           -> Tlambda (for_type a, for_type r)
      | Tunit                    -> nt
      | Toperation               -> nt
      | Tcontract t              -> Tcontract (for_type t)
      | Tticket t                -> Tticket (for_type t)
      | Tsapling_state _         -> nt
      | Tsapling_transaction _   -> nt
    in
    mktype ?annot:(get_atype t) (for_ntype (get_ntype t))
  in
  let rec for_mterm (mt : mterm) : mterm =
    let node : mterm__node = map_term_node_internal (f KImterm) (h KImterm) for_type for_mterm mt.node in
    mk_mterm node (for_type mt.type_)
  in
  map_model f for_type for_mterm model

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

type effect = Eadded of ident | Eremoved of ident | Eupdated of ident
[@@deriving show {with_path = false}]
module Utils : sig

  val get_vars                           : model -> var list
  val get_enums                          : model -> enum list
  val get_assets                         : model -> asset list
  val get_records                        : model -> record list
  val get_var                            : model -> mident -> var
  val get_var_opt                        : model -> mident -> var option
  val get_enum                           : model -> mident -> enum
  val get_enum_opt                       : model -> ident -> enum option
  val get_enum_values                    : model -> ident -> ident list
  val get_asset                          : model -> mident -> asset
  val get_record                         : model -> mident -> record
  val get_event                          : model -> mident -> record
  val get_events                         : model -> record list
  val get_asset_field                    : model -> (mident * ident) -> (mident * type_ * mterm option)
  val get_asset_key                      : model -> mident -> (ident * type_)
  val get_asset_value                    : model -> mident -> type_
  val get_field_container                : model -> mident -> ident -> (mident * container)
  val get_partitions                     : model -> (ident * ident * type_) list (* asset id, asset item *)
  val get_container_asset_key            : model -> ident -> ident -> (mident * ident * type_)
  val is_asset                           : mterm -> bool
  val get_key_pos                        : model -> mident -> int
  val is_field_storage                   : model -> ident -> bool
  val type_to_asset                      : type_ -> ident
  val with_operations                    : model -> bool
  val cmp                                : mterm -> mterm -> int
  val eval                               : (ident * mterm) list -> mterm -> mterm
  val mk_rat                             : Core.big_int -> Core.big_int -> mterm
  val is_asset_single_field              : model -> mident -> bool
  val is_asset_map                       : model -> mident -> bool
  val get_labeled_value_from             : model -> mident -> mterm list -> (ident * mterm) list
  val add_api_storage_in_list            : api_storage list -> api_storage -> api_storage list
  val sort_api_storage                   : model -> bool -> api_storage list -> api_storage list
  val extract_key_value_from_masset      : model -> mterm -> mterm
  val is_not_string_nat_int              : type_ -> bool
  val get_asset_partitions               : model -> mident -> (ident * ident) list
  val get_fss                            : model -> function_struct list
  val get_fs                             : model -> ident -> function_struct
  val get_record_pos                     : model -> mident -> ident -> (int * int) list
  val is_partition                       : model -> mident -> ident -> bool

end = struct

  open Tools

  exception Anomaly of string

  type error_desc =
    | AssetFieldNotFound of string * string
    | AssetKeyTypeNotFound of string
    | AssetValueTypeNotFound of string
    | ContainerNotFound
    | NotanAssetType
    | NotFound
    | CurrencyValueCannotBeNegative
    | EmptyAssetKeys of string
    | SeveralAssetKeys of string
  [@@deriving show {with_path = false}]

  let emit_error (desc : error_desc) =
    let str = Format.asprintf "%a@." pp_error_desc desc in
    raise (Anomaly str)

  let emit_error2 (lc, error : Location.t * error_desc) =
    let str : string = Format.asprintf "%a@." pp_error_desc error in
    let pos : Position.t list = [location_to_position lc] in
    Error.error_alert pos str (fun _ -> ())

  let lident_to_string lident = Location.unloc lident

  let is_entry (f : function__) : bool =
    match f with
    | { node = Entry _ } -> true
    | _                            -> false

  let is_function (f : function__) : bool =
    match f with
    | { node = Function _ } -> true
    | _                                -> false

  let get_entry (f : function__) : function_struct =
    match f with
    | { node = Entry s } -> s
    | _                  -> assert false

  let type_to_asset t =
    match get_ntype t with
    | Tasset n -> n |> unloc_mident
    | Tcontainer ((Tasset n, _), _) -> n |> unloc_mident
    | _ -> emit_error NotanAssetType

  let is_record (d : decl_node) : bool =
    match d with
    | Drecord _ -> true
    | _         -> false

  let dest_record = function
    | Drecord r -> r
    | _  -> emit_error NotFound

  let is_event (d : decl_node) : bool =
    match d with
    | Devent _ -> true
    | _         -> false

  let dest_event = function
    | Devent r -> r
    | _  -> emit_error NotFound

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

  let get_vars    m = m.decls |> List.filter is_var    |> List.map dest_var
  let get_enums   m = m.decls |> List.filter is_enum   |> List.map dest_enum
  let get_assets  m = m.decls |> List.filter is_asset  |> List.map dest_asset
  let get_records m = m.decls |> List.filter is_record |> List.map dest_record
  let get_events  m = m.decls |> List.filter is_event  |> List.map dest_event

  let get_var      m id : var          = get_vars  m |> List.find     (fun (x : var)  -> cmp_mident id x.name)
  let get_var_opt  m id : var option   = get_vars  m |> List.find_opt (fun (x : var)  -> cmp_mident id x.name)
  let get_enum     m id : enum         = get_enums m |> List.find     (fun (x : enum) -> cmp_mident id x.name)
  let get_enum_opt m id : enum option  = get_enums m |> List.find_opt (fun (x : enum) -> cmp_ident id (unloc_mident x.name))
  let get_enum_values m id : ident list  = get_enums m
                                           |> List.find (fun (x : enum)  -> cmp_ident id (unloc_mident x.name))
                                           |> fun e -> e.values
                                                       |> List.map (fun (v : enum_item) -> unloc_mident (v.name))
  let get_asset  m id : asset = get_assets m |> List.find (fun (x : asset) -> cmp_mident id x.name)
  let get_record m id : record = get_records m |> List.find (fun (x : record) -> cmp_mident id x.name)
  let get_event  m id : record = get_events m |> List.find (fun (x : record) -> cmp_mident id x.name)

  let get_containers_internal f m : (ident * ident * type_) list =
    get_assets m |> List.fold_left (fun acc (asset : asset) ->
        acc @ (List.fold_left (fun acc (v : asset_item) ->
            let t : type_ = v.original_type in
            match t with
            | _ when f t ->
              acc @ [unloc_mident asset.name, unloc_mident v.name, t]
            | _ -> acc
          ) [] asset.values)
      ) []

  let get_containers m : (ident * ident * type_) list =
    get_containers_internal (fun x -> match get_ntype x with | Tcontainer ((Tasset _, _), (Partition | Aggregate)) -> true | _ -> false ) m

  let get_partitions m : (ident * ident * type_) list =
    get_containers_internal (fun x -> match get_ntype x with |  Tcontainer ((Tasset _, _), Partition) -> true | _ -> false ) m

  let dest_container t =
    match get_ntype t with
    | Tcontainer ((Tasset p, _),(Partition | Aggregate)) -> p
    | _ -> assert false

  let get_asset_field (m : model) (asset_name, field_name : mident * ident) : mident * type_ * mterm option =
    try
      let asset = get_asset m asset_name in
      List.find (fun (x : asset_item) -> String.equal (unloc_mident x.name) field_name) asset.values
      |> (fun (x : asset_item) -> x.name, x.type_, x.default)
    with
    | Not_found -> emit_error (AssetFieldNotFound (unloc_mident asset_name, field_name))

  let get_asset_keys (m : model) (asset_name : mident) : (ident * type_) list =
    try
      let asset = get_asset m asset_name in
      let key_ids = asset.keys in
      List.map (fun key_id -> key_id, (get_asset_field m (asset_name, key_id)|> fun (_, x, _) -> x)) key_ids
    with
    | Not_found -> emit_error (AssetKeyTypeNotFound (unloc_mident asset_name))

  let get_asset_values (m : model) (asset_name : mident) : (ident * type_) list =
    try
      let asset : asset = get_asset m asset_name in
      List.fold_right(fun (x : asset_item) accu ->
          if (List.exists(fun y -> String.equal y (unloc_mident x.name)) asset.keys)
          then accu
          else (unloc_mident x.name, x.type_)::accu)
        asset.values []
    with
    | Not_found -> emit_error (AssetValueTypeNotFound (unloc_mident asset_name))

  let get_asset_key (m : model) (asset_name : mident) : (ident * type_) =
    match get_asset_keys m asset_name with
    | []  -> emit_error (EmptyAssetKeys (unloc_mident asset_name))
    | [x] -> x
    | _ -> emit_error (SeveralAssetKeys (unloc_mident asset_name))

  let get_asset_value (m : model) (asset_name : mident) : type_ =
    match get_asset_keys m asset_name with
    | []  -> tunit
    | [x] -> snd x
    | l -> ttuple (List.map (fun x -> snd x) l)

  let get_field_container model asset_name field_name : mident * container =
    let seek_original_type () : type_ =
      try
        let asset = get_asset model asset_name in
        List.find (fun (x : asset_item) -> String.equal (unloc_mident x.name) field_name) asset.values
        |> (fun (x : asset_item) -> x.original_type)
      with
      | Not_found -> emit_error (AssetFieldNotFound (unloc_mident asset_name, field_name))
    in
    let ot = seek_original_type () in
    match get_ntype ot with
    | Tcontainer ((Tasset an, _), c) -> (an, c)
    | _ -> assert false

  (* returns : asset name, key name, key type *)
  let get_container_asset_key model asset field : (mident * ident * type_) =
    let containers = get_containers model in
    let rec rec_get = function
      | (r,i,t) :: _tl when String.equal r asset &&
                            String.equal i field ->
        let pa  = dest_container t in
        let k, t = get_asset_key model pa in
        (pa, k, t)
      | _ :: tl -> rec_get tl
      | _ -> emit_error (ContainerNotFound) in
    rec_get containers

  exception FoundOperations

  let with_operations_for_mterm_intern _ctx accu (mt : mterm) : bool =
    let rec aux accu (t : mterm) =
      match t.node with
      | Mtransfer _
      | Moperations
      | Mmakeoperation _
      | Massign (_, _, Aoperations,_)
      | Memit _
        -> raise FoundOperations
      | _ -> fold_term aux accu t in
    aux accu mt

  let with_operations (model : model) : bool =
    try fold_model with_operations_for_mterm_intern model false
    with FoundOperations -> true

  let is_asset (t : mterm) =
    match t.node with
    | Masset _ -> true
    | _ -> false

  let get_key_pos (m : model) (n : mident) : int =
    get_assets m |> List.fold_left (fun acc (info : asset) ->
        if cmp_mident n info.name then
          let (k,_) = get_asset_key m n in
          (List.fold_left (fun acc (i : asset_item) ->
               if String.equal (unloc_mident i.name) k then
                 succ acc
               else
                 acc
             ) acc info.values)
        else
          acc
      ) (-1)

  let is_field_storage (m : model) (id : ident) : bool =
    let l : ident list = List.map (fun (x : storage_item) -> unloc_mident x.id) m.storage in
    List.mem id l

  let mk_rat (n : Core.big_int) (d : Core.big_int) : mterm =
    let pos x = Big_int.sign_big_int x >= 0 in
    let abs x = Big_int.abs_big_int x in
    let neg x = Big_int.sub_big_int Big_int.zero_big_int x in
    let mk_int i = mk_bint i in
    let mk_nat i = if not (pos i) then assert false; mk_bnat i in
    let mk n d = mk_mterm (Mtuple [mk_int n ; mk_nat d]) trat in
    let x, y = Core.compute_irr_fract (n, d) in
    match pos x, pos y with
    | _ , true     -> mk x y
    | true, false  -> mk (neg x) (abs y)
    | false, false -> mk (abs x) (abs y)

  let rec cmp (lhs : mterm) (rhs : mterm) : int =
    match lhs.node, rhs.node with
    | Mbool      v1, Mbool      v2 -> Bool.compare v1 v2
    | Mnat       v1, Mnat       v2 -> Big_int.compare_big_int v1 v2
    | Mint       v1, Mint       v2 -> Big_int.compare_big_int v1 v2
    | Mstring    v1, Mstring    v2 -> String.compare v1 v2
    | Mmutez     v1, Mmutez     v2 -> Big_int.compare_big_int v1 v2
    | Maddress   v1, Maddress   v2 -> String.compare v1 v2
    | Mdate      v1, Mdate      v2 -> Big_int.compare_big_int (Core.date_to_timestamp v1) (Core.date_to_timestamp v2)
    | Mtimestamp v1, Mtimestamp v2 -> Big_int.compare_big_int v1 v2
    | Mbytes     v1, Mbytes     v2 -> String.compare v1 v2
    | Mchain_id  v1, Mchain_id  v2 -> String.compare v1 v2
    | Mkey       v1, Mkey       v2 -> String.compare v1 v2
    | Mkey_hash  v1, Mkey_hash  v2 -> String.compare v1 v2
    | Msignature v1, Msignature v2 -> String.compare v1 v2
    | Mbls12_381_fr v1, Mbls12_381_fr v2 -> String.compare v1 v2
    | Mbls12_381_fr_n v1, Mbls12_381_fr_n v2 -> Big_int.compare_big_int v1 v2
    | Mbls12_381_g1 v1, Mbls12_381_g1 v2 -> String.compare v1 v2
    | Mbls12_381_g2 v1, Mbls12_381_g2 v2 -> String.compare v1 v2
    | Mtuple l1, Mtuple l2 when List.length l1 = List.length l2 ->
      List.fold_left2 (fun accu x y ->
          match accu with
          | Some _ -> accu
          | None -> let r = cmp x y in if r = 0 then None else Some r
        ) None l1 l2 |> (Option.get_dfl 0)
    (* | Mlitrecord _ *)
    | Mcast (_, _, v1), _          -> cmp v1 rhs
    | _, Mcast (_, _, v2)          -> cmp lhs v2
    | _ -> Format.eprintf "lhs:%a@.rhs:%a@." pp_mterm lhs pp_mterm rhs; assert false

  let eval (map_const_value : (ident * mterm) list) (mt : mterm) : mterm =
    let get_value (id : ident) : mterm = List.assoc id map_const_value in
    let is_const (id : ident) : bool = List.assoc_opt id map_const_value |> Option.is_some in
    let remove_const (mt : mterm) : mterm =
      let rec aux (mt : mterm) : mterm =
        match mt.node with
        | Mvar(v, Vstorevar)
        | Mvar(v, Vlocal) when is_const (unloc_mident v) ->
          let dv = get_value (unloc_mident v) in
          aux dv
        | _ -> map_mterm aux mt
      in
      aux mt
    in

    let is_int (mt : mterm) =
      match get_ntype mt.type_ with
      | Tbuiltin Bint -> true
      | _ -> false
    in

    let is_tez (mt : mterm) =
      match get_ntype mt.type_ with
      | Tbuiltin Btez -> true
      | _ -> false
    in

    let is_timestamp (mt : mterm) =
      match get_ntype mt.type_ with
      | Tbuiltin Btimestamp -> true
      | _ -> false
    in

    let is_rat t =
      match get_ntype t with
      | Ttuple [(Tbuiltin Bint, _); (Tbuiltin Bnat, _)] -> true
      | _ -> false
    in

    let eval_expr mt : mterm =
      let rec aux (mt : mterm) : mterm =
        let rec extract_big_int (i : mterm) : Big_int.big_int =
          let i = aux i in
          match i.node with
          | Mnattoint x -> extract_big_int x
          | Mnat v
          | Mint v -> v
          | _ -> Format.eprintf "%a@." pp_mterm i; assert false
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

        let extract_bytes (b : mterm) : string =
          let b = aux b in
          match b.node with
          | Mbytes v -> v
          | _ -> assert false
        in

        let neg x = Big_int.sub_big_int Big_int.zero_big_int x in

        let rec extract_rat (rat : mterm) : Big_int.big_int * Big_int.big_int =
          let rat = aux rat in
          match rat.node with
          | Mnat n                 -> (n, Big_int.unit_big_int)
          | Mint n                 -> (n, Big_int.unit_big_int)
          | Mnattoint x            -> extract_rat x
          | Mnattorat x            -> extract_rat x
          | Minttorat x            -> extract_rat x
          | Mrational (num, denom) -> (num, denom)
          | Mtuple [num; denom]    -> (extract_big_int num, extract_big_int denom)
          | Muminus x              -> extract_rat x |> (fun (x, y) -> (neg x, y))
          | _ -> Format.printf "%a@." pp_mterm rat; assert false
        in

        let extract_tez (b : mterm) : Big_int.big_int =
          let b = aux b in
          match b.node with
          | Mmutez v -> v
          | _ -> assert false
        in

        let extract_timestamp (b : mterm) : Big_int.big_int =
          let b = aux b in
          match b.node with
          | Mtimestamp v -> v
          | _ -> assert false
        in

        let arith (t : type_) op (a, b) : mterm =
          let a = extract_big_int a in
          let b = extract_big_int b in

          let is_nat t = match get_ntype t with Tbuiltin Bnat -> true | _ -> false in
          let res, nat =
            match op with
            | `Plus   -> Big_int.add_big_int a b,  is_nat t
            | `Minus  -> Big_int.sub_big_int a b,  false
            | `Mult   -> Big_int.mult_big_int a b, is_nat t
            | `Ediv   -> Big_int.div_big_int a b,  is_nat t
            | `Modulo -> Big_int.mod_big_int a b,  true
            | _ -> assert false
          in
          match nat with
          | true  -> mk_mterm (Mnat res) tnat
          | false -> mk_mterm (Mint res) tint
        in

        let cmp_op op lhs rhs =
          let lhs = aux lhs in
          let rhs = aux rhs in

          let r =
            match lhs.node, rhs.node with
            | Mbool b1,     Mbool b2   -> Bool.compare b1 b2

            | Mnat n1,     Mnat n2
            | Mnat n1,     Mint n2
            | Mint n1,     Mnat n2
            | Mint n1,     Mint n2
            | Mmutez n1,   Mmutez n2
            | Mbls12_381_fr_n n1, Mbls12_381_fr_n n2
            | Mtimestamp n1, Mtimestamp n2 -> Big_int.compare_big_int n1 n2
            | Maddress s1,   Maddress s2
            | Mbytes s1,     Mbytes s2
            | Mchain_id s1,  Mchain_id s2
            | Mkey s1,       Mkey s2
            | Mkey_hash s1,  Mkey_hash s2
            | Msignature s1, Msignature s2
            | Mbls12_381_fr s1, Mbls12_381_fr s2
            | Mbls12_381_g1 s1, Mbls12_381_g1 s2
            | Mbls12_381_g2 s1, Mbls12_381_g2 s2
            | Mstring s1,   Mstring  s2 -> String.compare s1 s2

            | Mdate d1,     Mdate d2 -> Big_int.compare_big_int (Core.date_to_timestamp d1) (Core.date_to_timestamp d2)
            | Mduration d1, Mduration d2 -> Big_int.compare_big_int (Core.duration_to_timestamp d1) (Core.duration_to_timestamp d2)
            | _ -> assert false
          in
          let b =
            match op with
            | `Eq -> r = 0
            | `Nq -> r <> 0
            | `Gt -> r > 0
            | `Ge -> r >= 0
            | `Lt -> r < 0
            | `Le -> r <= 0
          in
          mk_bool b
        in

        match mt.node, mt.type_ with
        | Mnattoint x, _ -> let n = extract_big_int x in mk_bint n
        | Mnattorat x, _
        | Minttorat x, _ -> begin
            let n = extract_big_int x in
            mk_rat n Big_int.unit_big_int
          end
        | Muminus x, _ -> begin
            let n = extract_big_int x in
            mk_bint (Big_int.minus_big_int n)
          end
        | Mequal (_, lhs, rhs), _  -> cmp_op `Eq lhs rhs
        | Mnequal (_, lhs, rhs), _ -> cmp_op `Nq lhs rhs
        | Mgt (lhs, rhs), _        -> cmp_op `Gt lhs rhs
        | Mge (lhs, rhs), _        -> cmp_op `Ge lhs rhs
        | Mlt (lhs, rhs), _        -> cmp_op `Lt lhs rhs
        | Mle (lhs, rhs), _        -> cmp_op `Le lhs rhs
        | Mplus   (a, b), (Tbuiltin Bstring, _) -> begin
            let a = extract_string a in
            let b = extract_string b in
            mk_mterm (Mstring (a ^ b)) tstring
          end
        | Mplus   (a, b), (Tbuiltin Btez, _) when is_tez a && is_tez b -> begin
            let a = extract_tez a in
            let b = extract_tez b in
            mk_mterm (Mmutez (Big_int.add_big_int a b)) ttez
          end
        | Mplus   (a, b), _ when is_timestamp a && is_int b -> begin
            let a = extract_timestamp a in
            let b = extract_big_int b in
            mk_mterm (Mtimestamp (Big_int.add_big_int a b)) ttimestamp
          end
        | Mplus   (a, b), t -> arith t `Plus  (aux a, aux b)
        | Mminus  (a, b), (Tbuiltin Btez, _) when is_tez a && is_tez b -> begin
            let a = extract_tez a in
            let b = extract_tez b in
            let res = Big_int.sub_big_int a b in
            if Big_int.sign_big_int res < 0 then emit_error2(mt.loc, CurrencyValueCannotBeNegative);
            mk_mterm (Mmutez res) ttez
          end
        | Mminus  (a, b), _ when is_timestamp a && is_timestamp b -> begin
            let a = extract_timestamp a in
            let b = extract_timestamp b in
            let res = Big_int.sub_big_int a b in
            mk_mterm (Mint res) tint
          end
        | Mminus  (a, b), t -> arith t `Minus (aux a, aux b)
        | Mmult   (a, b), t -> arith t `Mult  (aux a, aux b)
        | Mdiveuc (a, b), t -> arith t `Ediv  (aux a, aux b)
        | Mmodulo (a, b), t -> arith t `Modulo   (aux a, aux b)
        | Mnot     a    , _ -> mk_mterm (Mbool (not (extract_bool (aux a)))) tbool
        | Mand    (a, b), _ -> mk_mterm (Mbool ((extract_bool (aux a)) && (extract_bool (aux b)))) tbool
        | Mor     (a, b), _ -> mk_mterm (Mbool ((extract_bool (aux a)) || (extract_bool (aux b)))) tbool
        | Mrateq  (a, b), _ -> begin
            let num1, denom1 = extract_rat (aux a) in
            let num2, denom2 = extract_rat (aux b) in
            let res = Big_int.eq_big_int (Big_int.mult_big_int num1 denom2) (Big_int.mult_big_int num2 denom1) in
            mk_bool res
          end

        | Mratcmp (op, a, b), _ -> begin
            let num1, denom1 = extract_rat (aux a) in
            let num2, denom2 = extract_rat (aux b) in
            let a = Big_int.mult_big_int num1 denom2 in
            let b = Big_int.mult_big_int num2 denom1 in
            let res =
              begin
                match op with
                | Gt -> Big_int.gt_big_int a b
                | Ge -> Big_int.ge_big_int a b
                | Lt -> Big_int.lt_big_int a b
                | Le -> Big_int.le_big_int a b
              end
            in
            mk_bool res
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

        | Mratuminus x, _ -> begin
            let num, denom = extract_rat (aux x) in
            mk_rat (neg num) denom
          end

        | Mrattez (coef, c), _ ->
          begin
            let coef = aux coef in
            let c    = aux c    in
            let f num denom v =
              let res = Big_int.div_big_int (Big_int.mult_big_int num v) denom in
              mk_mterm (Mmutez res) ttez
            in
            match coef.node, c.node with
            | Mrational (num, denom), Mmutez v -> f num denom v
            | Mtuple [num; denom], Mmutez v ->
              begin
                let num = extract_big_int num in
                let denom = extract_big_int denom in
                f num denom v
              end
            | _ -> begin
                Format.eprintf "%a@." pp_mterm mt;
                assert false
              end
          end

        | Mratdur (lhs, rhs), _ -> begin
            let lhs = aux lhs in
            let rhs = aux rhs in
            let f num denom v =
              let res = Big_int.div_big_int (Big_int.mult_big_int num v) denom in
              mk_bint res
            in
            match lhs.node, rhs.node with
            | Mrational (num, denom), Mint v -> f num denom v
            | Mtuple [num; denom], Mint v ->
              begin
                let num = extract_big_int num in
                let denom = extract_big_int denom in
                f num denom v
              end
            | _ -> begin
                Format.eprintf "%a@." pp_mterm mt;
                assert false
              end
          end

        | Mmin (a, b) , t when is_rat t -> begin
            let a = aux a in
            let b = aux b in
            let num1, denom1 = extract_rat a in
            let num2, denom2 = extract_rat b in
            let x = Big_int.mult_big_int num1 denom2 in
            let y = Big_int.mult_big_int num2 denom1 in
            if (Big_int.lt_big_int x y)
            then a
            else b
          end

        | Mmax (a, b) , t when is_rat t -> begin
            let a = aux a in
            let b = aux b in
            let num1, denom1 = extract_rat a in
            let num2, denom2 = extract_rat b in
            let x = Big_int.mult_big_int num1 denom2 in
            let y = Big_int.mult_big_int num2 denom1 in
            if (Big_int.gt_big_int x y)
            then a
            else b
          end

        | Mabs x        , t when is_rat t -> begin
            let num, denom = extract_rat (aux x) in
            mk_rat (Big_int.abs_big_int num) denom
          end

        | Mmin (a, b), _ -> begin
            let a = aux a in
            let b = aux b in
            if cmp a b < 0
            then a
            else b
          end

        | Mmax (a, b), _ -> begin
            let a = aux a in
            let b = aux b in
            if cmp a b > 0
            then a
            else b
          end

        | Mabs x, _ -> begin
            let n = extract_big_int x in
            mk_bnat (Big_int.abs_big_int n)
          end

        | Mfloor x, _ -> begin
            let num, denom = extract_rat (aux x) in
            let n = Big_int.div_big_int num denom in
            mk_bint n
          end

        | Mceil x, _ -> begin
            let num, denom = extract_rat (aux x) in
            let n, m = Big_int.quomod_big_int num denom in
            let n = Big_int.add_big_int n (if Big_int.eq_big_int m Big_int.zero_big_int then Big_int.zero_big_int else Big_int.unit_big_int) in
            mk_bint n
          end

        | Mconcat (x, y), t -> begin
            match get_ntype t with
            | Tbuiltin Bstring -> let x = extract_string x in let y = extract_string y in mk_string (x ^ y)
            | Tbuiltin Bbytes  -> let x = extract_bytes  x in let y = extract_bytes  y in mk_bytes (x ^ y)
            | _ -> assert false
          end

        | Mslice (s, a, b), t -> begin
            let a = extract_big_int a |> Big_int.int_of_big_int in
            let b = extract_big_int b |> Big_int.int_of_big_int in

            match get_ntype t with
            | Toption ((Tbuiltin Bstring, _)) -> let s = extract_string s in mk_string (String.sub s a b) |> mk_some
            | Toption ((Tbuiltin Bbytes , _)) -> let s = extract_bytes  s in mk_bytes  (String.sub s (2 * a) (2 * b)) |> mk_some
            | _ -> assert false
          end

        | Mlength x, _ -> begin
            match get_ntype x.type_ with
            | Tbuiltin Bstring -> let x = extract_string x in mk_nat (String.length x)
            | Tbuiltin Bbytes  -> let x = extract_bytes  x in mk_nat (String.length x)
            | _ -> assert false
          end

        | Mlambda _, _ -> mt

        | _ -> map_mterm aux mt
      in
      aux mt
    in

    mt
    |> remove_const
    |> eval_expr

  exception FoundMinMax

  let with_minmax_for_mterm_intern _ctx accu (mt : mterm) : bool =
    let rec aux accu (t : mterm) =
      match t.node with
      | Mmax (_,_) -> raise FoundMinMax
      | Mmin (_,_) -> raise FoundMinMax
      | _ -> fold_term aux accu t in
    aux accu mt

  let is_asset_single_field (model : model) (an : mident) : bool =
    get_asset model an |> fun x -> x.values |> List.filter (fun (x : asset_item) -> not x.shadow) |> List.length = 1

  let is_asset_map (model : model) (an : mident) : bool =
    get_asset model an |> fun x -> match x.map_kind with | MKMap -> true | _ -> false

  let get_labeled_value_from (model : model) (an : mident) (values : mterm list) : (ident * mterm) list =
    let asset = get_asset model an in
    List.map2 (fun (x : asset_item) (y : mterm) -> unloc_mident x.name, y) asset.values values

  let add_api_storage_in_list (l : api_storage list) (i :  api_storage) =
    let res, l = List.fold_left (fun (res, accu) (x : api_storage) ->
        if cmp_api_item_node x.node_item i.node_item
        then (true,
              i::accu)
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
             | APIAsset (Clear (_, Field (an, _))) -> an
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
               | Dasset r -> accu @ [unloc_mident r.name]
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
             | APIInternal (RatDur        ) ->  6
             | APIAsset   (Nth           _) -> if verif then 7 else 8
             | APIAsset   (Get           _) -> if verif then 8 else 7
             | APIAsset   (Set           _) -> 9
             | APIAsset   (Add           _) -> 10
             | APIAsset   (Remove        _) -> 11
             | APIAsset   (Update        _) -> 12
             | APIAsset   (FieldAdd      _) -> 13
             | APIAsset   (FieldRemove   _) -> 14
             | APIAsset   (RemoveAll     _) -> 15
             | APIAsset   (RemoveIf      _) -> 16
             | APIAsset   (Clear         _) -> 17
             | APIAsset   (Contains      _) -> 18
             | APIAsset   (Select        _) -> 19
             | APIAsset   (Sort          _) -> 20
             | APIAsset   (Count         _) -> 21
             | APIAsset   (Sum           _) -> 22
             | APIAsset   (Head          _) -> 23
             | APIAsset   (Tail          _) -> 24
             | APIList    (Lprepend      _) -> 25
             | APIList    (Lcontains     _) -> 26
             | APIList    (Llength       _) -> 27
             | APIList    (Lnth          _) -> 28
             | APIList    (Lreverse      _) -> 29
             | APIBuiltin (Bmin          _) -> 30
             | APIBuiltin (Bmax          _) -> 31
             | APIBuiltin (Babs          _) -> 32
             | APIBuiltin (Bconcat       _) -> 33
             | APIBuiltin (Bslice        _) -> 34
             | APIBuiltin (Blength       _) -> 35
             | APIBuiltin (Bisnone       _) -> 36
             | APIBuiltin (Bissome       _) -> 37
             | APIBuiltin (Boptget       _) -> 38
             | APIBuiltin (Bfloor         ) -> 39
             | APIBuiltin (Bceil          ) -> 40
             | APIBuiltin (Bnattostring   ) -> 41
             | APIBuiltin (Bfail         _) -> 42
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

  let add_type (l : type_ list) (x : type_) =
    if List.exists (cmp_type x) l
    then l
    else l @ [x]

  let extract_key_value_from_masset (model : model) (v : mterm) : mterm =
    match v with
    | {node = (Masset l); type_ = (Tasset an, _) } ->
      let asset : asset = get_asset model an in
      let asset_key = match asset.keys with [k] -> k | _ -> emit_error (SeveralAssetKeys (unloc_mident an)) in
      let assoc_fields = List.map2 (fun (ai : asset_item) (x : mterm) -> (unloc_mident ai.name, x)) asset.values l in
      List.find (fun (id, _) -> (String.equal asset_key id)) assoc_fields |> snd
    | _ -> raise Not_found

  let is_not_string_nat_int t = (match get_ntype t with | Tbuiltin (Bstring | Bnat | Bint) -> false | _ -> true)

  let get_function (model : model) (id : ident) : function_struct =
    model.functions
    |> List.map (fun x -> match x.node with | Function (fs, _) | Getter (fs, _) | View (fs, _, _) | Entry fs -> fs)
    |> List.find (fun (x : function_struct) -> String.equal (unloc_mident x.name) id)

  let get_asset_partitions (model : model) asset_name : (ident * ident) list =
    let asset = get_asset model asset_name in
    List.fold_left (fun accu (x : asset_item) ->
        match get_ntype x.original_type with
        | Tcontainer ((Tasset an, _), Partition) -> (unloc_mident x.name, unloc_mident an)::accu
        | _ -> accu
      ) [] asset.values

  let get_fss (model : model) : function_struct list =
    List.map (fun (x) -> match x.node with | Entry fs | Getter (fs, _) | View (fs, _, _) | Function (fs, _) -> fs) model.functions

  let get_fs (model : model) (id : ident) : function_struct =
    List.find (fun (x : function_struct) -> String.equal id (unloc_mident x.name)) (get_fss model)

  exception Found of (int * int) list

  let get_record_pos model rn fn =
    let r : record = get_record model rn in
    let fields_length = List.length r.fields in
    let fields_index  = List.index_of (fun (f : record_field) -> String.equal fn (unloc_mident f.name)) r.fields in
    if (fields_index == -1)
    then assert false;
    match r.pos with
    | Pnode [] -> [fields_index, fields_length]
    | _ -> begin

        let idx = ref fields_index in
        let rec aux accu p =
          match p with
          | Ptuple ids -> begin
              let l = List.length ids in
              if l <= !idx
              then idx := !idx - l
              else raise (Found (accu @ [!idx, l]))
            end
          | Pnode children -> begin
              let l = List.length children in
              List.iteri (fun i x -> aux (accu @ [i, l]) x) children
            end
        in

        try
          aux [] r.pos;
          assert false
        with
          Found res -> res
      end

  let is_partition model an fn : bool =
    try
      get_field_container model an fn |> (fun (_, x) -> match x with | Partition -> true | _ -> false)
    with
    | Not_found -> false

end
