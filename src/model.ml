open Ident
open Tools
open Location

type lident = ident Location.loced
[@@deriving show {with_path = false}]

type currency =
  (* | Tz
     | Mtz *)
  | Utz
[@@deriving show {with_path = false}]

type container =
  | Collection
  | Aggregate
  | Partition
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
  | Bcurrency
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

type ntype =
  | Tasset of lident
  | Tenum of lident
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
  | Trecord of lident
  | Tevent of lident
  | Tlambda of type_ * type_
  | Tunit
  | Tstorage
  | Toperation
  | Tcontract of type_
  | Tprog of type_
  | Tvset of vset * type_
  | Ttrace of trtyp
  | Tticket of type_
  | Tsapling_state of int
  | Tsapling_transaction of int
[@@deriving show {with_path = false}]

and type_ = ntype * lident option
[@@deriving show {with_path = false}]

type 'id pattern_node =
  | Pwild
  | Pconst of 'id * lident list
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

type sort_kind =
  | SKasc
  | SKdesc
[@@deriving show {with_path = false}]

type ('id, 'term) assign_kind_gen =
  | Avar         of 'id
  | Avarstore    of 'id
  | Aasset       of 'id * 'id * 'term (* asset name * field name * key *)
  | Arecord      of 'id * 'id * 'term (* record name * field name * record *)
  | Avartuple    of 'id * int         (* var name * index *)
  | Astate
  | Aassetstate of ident * 'term     (* asset name * key *)
  | Aoperations
[@@deriving show {with_path = false}]

type 'term var_kind_gen =
  | Vassetstate of 'term
  | Vstorevar
  | Vstorecol
  | Vdefinition
  | Vlocal
  | Vparam
  | Vfield
  | Vstate
  | Vthe
  | Vparameter
[@@deriving show {with_path = false}]

type temp =
  | Tbefore
  | Tat of ident
  | Tnone
[@@deriving show {with_path = false}]

type delta =
  | Dadded
  | Dremoved
  | Dunmoved
  | Dnone
[@@deriving show {with_path = false}]

type 'term container_kind_gen =
  | CKcoll  of temp * delta
  | CKview  of 'term
  | CKfield of (ident * ident * 'term * temp * delta)
  | CKdef   of ident
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
  | TKsimple    of 'term * 'term                         (* dest *)
  | TKcall      of 'term * ident * type_ * 'term * 'term (* entry_id * type_entry * dest * args *)
  | TKentry     of 'term * 'term * 'term                 (* entry * arg *)
  | TKself      of 'term * ident * (ident * 'term) list  (* entry_id * args *)
  | TKoperation of 'term
[@@deriving show {with_path = false}]

type map_kind =
  | MKMap
  | MKBigMap
  | MKIterableBigMap
[@@deriving show {with_path = false}]

type ('id, 'term) mterm_node  =
  (* lambda *)
  | Mletin            of 'id list * 'term * type_ option * 'term * 'term option
  | Mdeclvar          of 'id list * type_ option * 'term * bool
  | Mapp              of 'id * 'term list
  (* assign *)
  | Massign           of (assignment_operator * type_ * ('id, 'term) assign_kind_gen * 'term) (* assignment kind value*)
  (* control *)
  | Mif               of ('term * 'term * 'term option)
  | Mmatchwith        of 'term * ('id pattern_gen * 'term) list
  | Minstrmatchoption of 'term * 'id * 'term * 'term
  | Minstrmatchor     of 'term * 'id * 'term * 'id * 'term
  | Minstrmatchlist   of 'term * 'id * 'id * 'term * 'term
  | Mfor              of ('id for_ident_gen * 'term iter_container_kind_gen * 'term * ident option)
  | Miter             of ('id * 'term * 'term * 'term * ident option * bool (* true of id is nat else int *))
  | Mwhile            of ('term * 'term * ident option)
  | Mseq              of 'term list
  | Mreturn           of 'term
  | Mlabel            of 'id
  | Mmark             of 'id * 'term
  (* effect *)
  | Mfail             of 'id fail_type_gen
  | Mtransfer         of 'term transfer_kind_gen
  | Memit             of 'id * 'term
  (* entrypoint *)
  | Mentrypoint       of type_ * 'id * 'term * 'term option  (* type * address * string * require fail *)
  | Mcallview         of type_ * 'term * 'id * 'term         (* type * string * address * argument *)
  | Mself             of 'id                                 (* entryname *)
  (* operation *)
  | Moperations
  | Mmkoperation      of 'term * 'term * 'term  (* value * address * args *)
  (* literals *)
  | Mint              of Core.big_int
  | Mnat              of Core.big_int
  | Mbool             of bool
  | Mrational         of Core.big_int * Core.big_int
  | Mstring           of string
  | Mcurrency         of Core.big_int * currency
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
  (* control expression *)
  | Mexprif           of 'term * 'term * 'term
  | Mexprmatchwith    of 'term * ('id pattern_gen * 'term) list
  | Mmatchoption      of 'term * 'id * 'term * 'term
  | Mmatchor          of 'term * 'id * 'term * 'id * 'term
  | Mmatchlist        of 'term * 'id * 'id * 'term * 'term
  | Mternarybool      of 'term * 'term * 'term
  | Mternaryoption    of 'term * 'term * 'term
  | Mternaryasset     of ident * 'term * 'term * 'term
  | Mfold             of 'term * 'id * 'term
  | Mmap              of 'term * 'id * 'term
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
  | Mlambda           of type_ * 'id * type_ * 'term
  (* access *)
  | Mdot              of 'term * 'id
  | Mdotassetfield    of 'id * 'term * 'id
  | Mquestionoption   of 'term * 'id
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
  | Mremoveall        of ident * ident * 'term
  | Mremoveif         of ident * 'term container_kind_gen * (ident * type_) list * 'term * 'term list (* asset_name, view, lambda (args, body, apply_args) *)
  | Mclear            of ident * 'term container_kind_gen
  | Mset              of ident * ident list * 'term * 'term (*asset_name * field_name modified * ... *)
  | Mupdate           of ident * 'term * ('id * assignment_operator * 'term) list
  | Maddupdate        of ident * 'term container_kind_gen * 'term * ('id * assignment_operator * 'term) list
  (* asset api expression *)
  | Mget              of ident * 'term container_kind_gen * 'term
  | Mgetopt           of ident * 'term container_kind_gen * 'term
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
  (* set api expression *)
  | Msetadd           of type_ * 'term * 'term
  | Msetremove        of type_ * 'term * 'term
  | Msetcontains      of type_ * 'term * 'term
  | Msetlength        of type_ * 'term
  | Msetfold          of type_ * 'id   * 'id   * 'term * 'term * 'term
  (* set api instruction *)
  | Msetinstradd      of type_ * ('id, 'term) assign_kind_gen * 'term
  | Msetinstrremove   of type_ * ('id, 'term) assign_kind_gen * 'term
  (* list api expression *)
  | Mlistprepend      of type_ * 'term * 'term
  | Mlistlength       of type_ * 'term
  | Mlistcontains     of type_ * 'term * 'term
  | Mlistnth          of type_ * 'term * 'term
  | Mlistreverse      of type_ * 'term
  | Mlistconcat       of type_ * 'term * 'term
  | Mlistfold         of type_ * 'id   * 'id   * 'term * 'term * 'term
  (* list api instruction *)
  | Mlistinstrprepend of type_ * ('id, 'term) assign_kind_gen * 'term
  | Mlistinstrconcat  of type_ * ('id, 'term) assign_kind_gen * 'term
  (* map api expression *)
  | Mmapput           of map_kind * type_ * type_ * 'term * 'term * 'term
  | Mmapremove        of map_kind * type_ * type_ * 'term * 'term
  | Mmapupdate        of map_kind * type_ * type_ * 'term * 'term * 'term
  | Mmapget           of map_kind * type_ * type_ * 'term * 'term * ident option
  | Mmapgetopt        of map_kind * type_ * type_ * 'term * 'term
  | Mmapcontains      of map_kind * type_ * type_ * 'term * 'term
  | Mmaplength        of map_kind * type_ * type_ * 'term
  | Mmapfold          of map_kind * type_ * 'id   * 'id   * 'id   * 'term * 'term * 'term
  (* map api instruction *)
  | Mmapinstrput      of map_kind * type_ * type_ * ('id, 'term) assign_kind_gen * 'term * 'term
  | Mmapinstrremove   of map_kind * type_ * type_ * ('id, 'term) assign_kind_gen * 'term
  | Mmapinstrupdate   of map_kind * type_ * type_ * ('id, 'term) assign_kind_gen * 'term * 'term
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
  | Misnat            of 'term
  | Mtonat            of 'term
  | Moptget           of 'term
  | Mrequiresome      of 'term * 'term
  | Mfloor            of 'term
  | Mceil             of 'term
  | Mtostring         of type_ * 'term
  | Mpack             of 'term
  | Munpack           of type_ * 'term
  | Msetdelegate      of 'term
  | Mimplicitaccount  of 'term
  | Mcontractaddress  of 'term
  | Maddresscontract  of 'term
  | Mkeyaddress       of 'term
  (* crypto functions *)
  | Mblake2b          of 'term
  | Msha256           of 'term
  | Msha512           of 'term
  | Msha3             of 'term
  | Mkeccak           of 'term
  | Mhashkey          of 'term
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
  | Mchainid
  | Mmetadata
  | Mlevel
  (* variable *)
  | Mvar              of 'id * 'term var_kind_gen * temp * delta
  | Menumval          of 'id * 'term list * ident  (* value * args * ident of enum *)
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
  | Mdatefromtimestamp of 'term
  | Mmuteztonat        of 'term
  (* quantifiers *)
  | Mforall           of 'id * type_ * 'term option * 'term
  | Mexists           of 'id * type_ * 'term option * 'term
  (* formula operators *)
  | Mimply            of 'term * 'term
  | Mequiv            of 'term * 'term
  (* formula asset collection *)
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

and transfer_kind = mterm transfer_kind_gen

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
  | InvalidCondition of ident
  | NotFound
  | AssetNotFound of ident
  | KeyExists of ident
  | KeyExistsOrNotFound of ident
  | OutOfBound
  | DivByZero
  | NatAssign
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
  | Btostring of type_
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
[@@deriving show {with_path = false}]

type 'id storage_gen = 'id storage_item_gen list
[@@deriving show {with_path = false}]

type storage = lident storage_gen
[@@deriving show {with_path = false}]

type 'id enum_item_gen = {
  name: 'id;
  args: type_ list;
  invariants : 'id label_term_gen list;
}
[@@deriving show {with_path = false}]

type enum_item = lident enum_item_gen
[@@deriving show {with_path = false}]

type variable_kind =
  | VKconstant
  | VKvariable
[@@deriving show {with_path = false}]

type 'id var_gen = {
  name: 'id;
  type_: type_;
  original_type: type_;
  kind: variable_kind;
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
  keys: ident list;
  sort: 'id list;
  map_kind: map_kind;
  state: lident option;
  invariants  : lident label_term_gen list;
  init: 'id mterm_gen list;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type asset = lident asset_gen
[@@deriving show {with_path = false}]

type position =
  | Ptuple of ident list
  | Pnode of position list
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
  pos: position;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type record = lident record_gen
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
  name:  'id;
  args:  'id argument_gen list;
  eargs: 'id argument_gen list;
  stovars: ident list;
  body:  'id mterm_gen;
  loc :  Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type function_struct = lident function_struct_gen
[@@deriving show {with_path = false}]

type 'id function_node_gen =
  | Function           of 'id function_struct_gen * type_ (* fun * return type *)
  | Getter             of 'id function_struct_gen * type_
  | View               of 'id function_struct_gen * type_
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
  decl : 'id argument_gen;
  kind : variable_kind;
  loc  : Location.t [@opaque];
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

type 'id fail_gen = {
  label: 'id;
  fid: 'id option;
  arg: 'id;
  atype: type_;
  formula: 'id mterm_gen;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type fail = lident fail_gen
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
  fails          : 'id fail_gen list;
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
  | Devent of 'id record_gen
[@@deriving show {with_path = false}]

type decl_node = lident decl_node_gen
[@@deriving show {with_path = false}]

type 'id parameter_gen = {
  name    : 'id;
  typ     : type_;
  default : 'id mterm_gen option;
  value   : 'id mterm_gen option;
  const   : bool;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type parameter = lident parameter_gen
[@@deriving show {with_path = false}]

type metadata_kind =
  | MKuri  of string loced
  | MKjson of string loced
[@@deriving show {with_path = false}]

type 'id model_gen = {
  name          : lident;
  parameters    : 'id parameter_gen list;
  metadata      : metadata_kind option;
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

let mk_pattern ?(loc = Location.dummy) node : 'id pattern_gen =
  { node; loc}

let mk_mterm ?(loc = Location.dummy) node type_ : 'id mterm_gen =
  { node; type_; loc}

let mk_label_term ?(loc = Location.dummy) term label : 'id label_term_gen =
  { label; term; loc }

let mk_variable ?(loc = Location.dummy) decl kind =
  { decl; kind; loc }

let mk_predicate ?(args = []) ?(loc = Location.dummy) name body =
  { name; args; body; loc }

let mk_definition ?(loc = Location.dummy) name typ var body =
  { name; typ; var; body; loc }

let mk_invariant ?(formulas = []) label =
  { label; formulas }

let mk_fail ?(loc = Location.dummy) label fid arg atype formula =
  { label; fid; arg; atype; formula; loc }

let mk_postcondition ?(invariants = []) ?(uses = []) name mode formula =
  { name; mode; formula; invariants; uses }

let mk_assert ?(invariants = []) ?(uses = []) name label formula =
  { name; label; formula; invariants; uses }

let mk_specification ?(predicates = []) ?(definitions = []) ?(lemmas = []) ?(theorems = []) ?(fails = []) ?(variables = []) ?(invariants = []) ?(effects = []) ?(postconditions = []) ?(loc = Location.dummy) () =
  { predicates; definitions; lemmas; theorems; fails; variables; invariants; effects; postconditions; loc}

let mk_security_predicate ?(loc = Location.dummy) s_node : security_predicate =
  { s_node; loc }

let mk_security_item ?(loc = Location.dummy) label predicate : security_item =
  { label; predicate; loc }

let mk_security ?(items = []) ?(loc = Location.dummy) () : security =
  { items; loc }

let mk_var ?(invariants=[]) ?default ?(loc = Location.dummy) name type_ original_type kind : 'id var_gen =
  { name; type_; default; kind; invariants; original_type; loc }

let mk_enum ?(values = []) name initial : 'id enum_gen =
  { name; values; initial }

let mk_enum_item ?(args = []) ?(invariants = []) name : 'id enum_item_gen =
  { name; args; invariants }

let mk_asset ?(values = []) ?(sort=[]) ?(map_kind = MKMap) ?state ?(keys = []) ?(invariants = []) ?(init = []) ?(loc = Location.dummy) name : 'id asset_gen =
  { name; values; sort; map_kind; state; keys; invariants; init; loc }

let mk_asset_item ?default ?(shadow=false) ?(loc = Location.dummy) name type_ original_type : 'id asset_item_gen =
  { name; type_; original_type; default; shadow; loc }

let mk_record ?(fields = []) ?(pos = Pnode []) ?(loc = Location.dummy) name : 'id record_gen =
  { name; fields; pos; loc }

let mk_record_field ?(loc = Location.dummy) name type_ : 'id record_field_gen =
  { name; type_; loc }

let mk_storage_item ?(const=false) ?(ghost = false) ?(loc = Location.dummy) id model_type typ default : 'id storage_item_gen =
  { id; model_type; typ; const; ghost; default; loc }

let mk_function_struct ?(args = []) ?(eargs = []) ?(stovars = []) ?(loc = Location.dummy) name body : function_struct =
  { name; args; eargs; stovars; body; loc }

let mk_function ?spec node : 'id function__gen =
  { node; spec }

let mk_signature ?(args = []) ?ret name : 'id signature_gen =
  { name; args; ret }

let mk_api_item node_item api_loc =
  { node_item; api_loc }

let mk_model ?(parameters = []) ?metadata ?(api_items = []) ?(api_verif = []) ?(decls = []) ?(functions = []) ?(storage = []) ?(specification = mk_specification ()) ?(security = mk_security ()) ?(loc = Location.dummy) name : model =
  { name; parameters; metadata; api_items; api_verif; storage; decls; functions; specification; security; loc }

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
let ttez                   = mktype (Tbuiltin Bcurrency)
let tduration              = mktype (Tbuiltin Bduration)
let tkey                   = mktype (Tbuiltin Bkey)
let tkeyhash               = mktype (Tbuiltin Bkeyhash)
let tdate                  = mktype (Tbuiltin Bdate)
let ttimestamp             = mktype (Tbuiltin Btimestamp)
let taddress               = mktype (Tbuiltin Baddress)
let tenum v                = mktype (Tenum v)
let tstate                 = mktype (Tstate)
let tstorage               = mktype (Tstorage)
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
let mk_chest    x = mk_mterm (Mchest x) tchest
let mk_chest_key  x = mk_mterm (Mchest_key x) tchest_key
let mk_date     x = mk_mterm (Mdate x) tdate
let mk_duration x = mk_mterm (Mduration x) tduration
let mk_pair   x y = mk_mterm (Mtuple [x; y]) (ttuple [x.type_; y.type_])
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
let mchainid      = mk_mterm Mchainid     tchainid
let mmetadata     = mk_mterm Mmetadata    (tmap tstring tbytes)
let mlevel        = mk_mterm Mlevel       tnat

let mk_mvar id t = mk_mterm (Mvar(id, Vlocal, Tnone, Dnone )) t
let mk_pvar id t = mk_mterm (Mvar(id, Vparam, Tnone, Dnone )) t
let mk_svar id t = mk_mterm (Mvar(id, Vstorevar, Tnone, Dnone )) t
let mk_state_var _ = mk_mterm (Mvar(dumloc "", Vstate, Tnone, Dnone )) ((Tenum (dumloc "state")), None)

let mk_enum_value  ?(args=[]) id e = mk_mterm (Menumval(id, args, unloc e)) (mktype (Tenum e))
let mk_state_value id = mk_enum_value id (dumloc "state")

let mk_btez v = mk_mterm (Mcurrency (v, Utz)) ttez
let mk_tez  v = mk_btez (Big_int.big_int_of_int v)

let mk_tuple (l : mterm list) = mk_mterm (Mtuple l) (ttuple (List.map (fun (x : mterm) -> x.type_) l))

let mk_letin id v b = mk_mterm (Mletin([id], v, Some v.type_, b, None)) b.type_

let mk_tupleaccess n (x : mterm) =
  match get_ntype x.type_ with
  | Ttuple lt ->
    let t = List.nth lt n in
    mk_mterm (Mtupleaccess (x, Big_int.big_int_of_int n)) t
  | v -> Format.eprintf "mk_tupleaccess type: %a@." pp_ntype v; assert false

let mk_optget (x : mterm) =
  match get_ntype x.type_ with
  | Toption t -> mk_mterm (Moptget x) t
  | _ -> assert false

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
let mk_hashkey  v = mk_mterm (Mhashkey v) tkeyhash
let mk_checksignature a b c = mk_mterm (Mchecksignature (a, b, c)) tbool

let mk_brat n d  = mk_tuple [mk_bint n; mk_bnat d]
let mk_rat n d   = mk_tuple [mk_int n; mk_nat d]

let mk_metadata v = mk_mterm (Mlitmap(MKBigMap, v)) tmetadata

let fail x  = mk_mterm (Mfail (Invalid (mk_string x))) tunit
let failg x = mk_mterm (Mfail (Invalid (x))) tunit
let failc x = mk_mterm (Mfail x) tunit
let mnot x  = mk_mterm (Mnot x) tbool
let seq x   = mk_mterm (Mseq x) tunit
let skip    = seq []

let operations = mk_mterm Moperations (tlist toperation)

let mk_entrypoint (ty: type_) (entry_name: lident) (addr : mterm) (error_msg : mterm option) : mterm =
  mk_mterm (Mentrypoint (ty, entry_name, addr, error_msg)) (tcontract ty)
let mk_mkoperation a b c = mk_mterm (Mmkoperation (a, b, c)) tunit
let mk_transfer_op op = mk_mterm (Mtransfer (TKoperation op)) tunit

(* -------------------------------------------------------------------- *)

let cmp_ident (i1 : ident) (i2 : ident) : bool = String.equal i1 i2
let cmp_big_int (n1 : Core.big_int) (n2 : Core.big_int) : bool = Big_int.compare_big_int n1 n2 = 0
let cmp_int (n1 : int) (n2 : int) : bool = n1 = n2
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
  | Invalid mt1, Invalid mt2                         -> cmp mt1 mt2
  | InvalidCaller, InvalidCaller                     -> true
  | InvalidCondition c1, InvalidCondition c2         -> cmp_ident c1 c2
  | NotFound, NotFound                               -> true
  | AssetNotFound an1, AssetNotFound an2             -> cmp_ident an1 an2
  | KeyExists an1, KeyExists an2                     -> cmp_ident an1 an2
  | KeyExistsOrNotFound an1, KeyExistsOrNotFound an2 -> cmp_ident an1 an2
  | OutOfBound, OutOfBound                           -> true
  | DivByZero, DivByZero                             -> true
  | NatAssign, NatAssign                             -> true
  | NoTransfer, NoTransfer                           -> true
  | InvalidState, InvalidState                       -> true
  | _ -> false

let rec cmp_ntype
    (t1 : ntype)
    (t2 : ntype)
  : bool =
  match t1, t2 with
  | Tasset i1, Tasset i2                                   -> cmp_lident i1 i2
  | Tenum i1, Tenum i2                                     -> cmp_lident i1 i2
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
  | Trecord i1, Trecord i2                                 -> cmp_lident i1 i2
  | Tevent e1, Tevent e2                                   -> cmp_lident e1 e2
  | Tlambda (a1, r1), Tlambda (a2, r2)                     -> cmp_type a1 a2 && cmp_type r1 r2
  | Tunit, Tunit                                           -> true
  | Tstorage, Tstorage                                     -> true
  | Toperation, Toperation                                 -> true
  | Tcontract t1, Tcontract t2                             -> cmp_type t1 t2
  | Tprog t1, Tprog t2                                     -> cmp_type t1 t2
  | Tvset (v1, t1), Tvset (v2, t2)                         -> cmp_vset v1 v2 && cmp_type t1 t2
  | Ttrace t1, Ttrace t2                                   -> cmp_trtyp t1 t2
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
    (cmpi  : 'id -> 'id -> bool)
    (p1    : 'id pattern_node)
    (p2    : 'id pattern_node)
  : bool =
  match p1, p2 with
  | Pconst (c1, xs1), Pconst (c2, xs2) ->
    cmpi c1 c2
    && List.length xs1 = List.length xs2
    && List.for_all2 cmpi xs1 xs2
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
    | Avartuple (id1, n1), Avartuple (id2, n2)       -> cmpi id1 id2 && cmp_int n1 n2
    | Astate, Astate                                 -> true
    | Aassetstate (id1, v1), Aassetstate (id2, v2)   -> cmp_ident id1 id2 && cmp v1 v2
    | Aoperations, Aoperations                       -> true
    | _ -> false
  in
  let cmp_var_kind (lhs : var_kind) (rhs : var_kind) : bool =
    match lhs, rhs with
    | Vassetstate v1, Vassetstate v2 -> cmp v1 v2
    | Vstorevar, Vstorevar
    | Vstorecol, Vstorecol
    | Vdefinition, Vdefinition
    | Vlocal, Vlocal
    | Vparam, Vparam
    | Vfield, Vfield
    | Vstate, Vstate
    | Vthe, Vthe -> true
    | _ -> false
  in
  let cmp_temp (lhs : temp) (rhs : temp) : bool =
    match lhs, rhs with
    | Tbefore, Tbefore -> true
    | Tat i1, Tat i2   -> cmp_ident i1 i2
    | Tnone, Tnone     -> true
    | _ -> false
  in
  let cmp_delta (lhs : delta) (rhs : delta) : bool =
    match lhs, rhs with
    | Dadded, Dadded     -> true
    | Dremoved, Dremoved -> true
    | Dunmoved, Dunmoved -> true
    | Dnone, Dnone       -> true
    | _ -> false
  in
  let cmp_container_kind (lhs : container_kind) (rhs : container_kind) : bool =
    match lhs, rhs with
    | CKcoll (t1, d1), CKcoll (t2, d2) -> cmp_temp t1 t2 && cmp_delta d1 d2
    | CKview l, CKview r -> cmp l r
    | CKfield (an1, fn1, mt1, t1, d1), CKfield (an2, fn2, mt2, t2, d2) -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp mt1 mt2 && cmp_temp t1 t2 && cmp_delta d1 d2
    | CKdef v1, CKdef v2 -> cmp_ident v1 v2
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
    | TKself (x1, i1, as1), TKself (x2, i2, as2)               -> cmp x1 x2 && cmp_ident i1 i2 && List.for_all2 (fun (id1, v1) (id2, v2) -> cmp_ident id1 id2 && cmp v1 v2) as1 as2
    | TKoperation x1, TKoperation x2                           -> cmp x1 x2
    | _ -> false
  in
  let cmp_map_kind (lhs : map_kind) (rhs : map_kind) : bool =
    match lhs, rhs with
    | _ -> false
  in
  try
    match term1, term2 with
    (* lambda *)
    | Mletin (i1, a1, t1, b1, o1), Mletin (i2, a2, t2, b2, o2)                         -> List.for_all2 cmpi i1 i2 && cmp a1 a2 && Option.cmp cmp_type t1 t2 && cmp b1 b2 && Option.cmp cmp o1 o2
    | Mdeclvar (i1, t1, v1, c1), Mdeclvar (i2, t2, v2, c2)                             -> List.for_all2 cmpi i1 i2 && Option.cmp cmp_type t1 t2 && cmp v1 v2 && cmp_bool c1 c2
    | Mapp (e1, args1), Mapp (e2, args2)                                               -> cmpi e1 e2 && List.for_all2 cmp args1 args2
    (* assign *)
    | Massign (op1, t1, k1, v1), Massign (op2, t2, k2, v2)                             -> cmp_assign_op op1 op2 && cmp_type t1 t2 && cmp_assign_kind k1 k2 && cmp v1 v2
    (* control *)
    | Mif (c1, t1, e1), Mif (c2, t2, e2)                                               -> cmp c1 c2 && cmp t1 t2 && Option.cmp cmp e1 e2
    | Mmatchwith (e1, l1), Mmatchwith (e2, l2)                                         -> cmp e1 e2 && List.for_all2 (fun (p1, t1) (p2, t2) -> cmp_pattern p1 p2 && cmp t1 t2) l1 l2
    | Minstrmatchoption (x1, i1, ve1, ne1), Minstrmatchoption (x2, i2, ve2, ne2)               -> cmp x1 x2 && cmpi i1 i2 && cmp ve1 ve2 && cmp ne1 ne2
    | Minstrmatchor (x1, lid1, le1, rid1, re1), Minstrmatchor (x2, lid2, le2, rid2, re2)       -> cmp x1 x2 && cmpi lid1 lid2 && cmp le1 le2 && cmpi rid1 rid2 && cmp re1 re2
    | Minstrmatchlist (x1, hid1, tid1, hte1, ee1), Minstrmatchlist (x2, hid2, tid2, hte2, ee2) -> cmp x1 x2 && cmpi hid1 hid2 && cmpi tid1 tid2 && cmp hte1 hte2 && cmp ee1 ee2
    | Mfor (i1, c1, b1, lbl1), Mfor (i2, c2, b2, lbl2)                                 -> cmp_for_ident cmpi i1 i2 && cmp_iter_container_kind c1 c2 && cmp b1 b2 && Option.cmp cmp_ident lbl1 lbl2
    | Miter (i1, a1, b1, c1, lbl1, strict1), Miter (i2, a2, b2, c2, lbl2, strict2)     -> cmpi i1 i2 && cmp a1 a2 && cmp b1 b2 && cmp c1 c2 && Option.cmp cmp_ident lbl1 lbl2 && cmp_bool strict1 strict2
    | Mwhile (c1, b1, lbl1), Mwhile (c2, b2, lbl2)                                     -> cmp c1 c2 && cmp b1 b2 && Option.cmp cmp_ident lbl1 lbl2
    | Mseq is1, Mseq is2                                                               -> List.for_all2 cmp is1 is2
    | Mreturn x1, Mreturn x2                                                           -> cmp x1 x2
    | Mlabel i1, Mlabel i2                                                             -> cmpi i1 i2
    | Mmark (i1, x1), Mmark (i2, x2)                                                   -> cmpi i1 i2 && cmp x1 x2
    (* effect *)
    | Mfail ft1, Mfail ft2                                                             -> cmp_fail_type cmp ft1 ft2
    | Mtransfer tr1, Mtransfer tr2                                                     -> cmp_transfer_kind tr1 tr2
    | Memit (e1, x1), Memit (e2, x2)                                                   -> cmpi e1 e2 && cmp x1 x2
    (* entrypoint *)
    | Mentrypoint (t1, a1, s1, r1), Mentrypoint (t2, a2, s2, r2)                       -> cmp_type t1 t2 && cmpi a1 a2 && cmp s1 s2 && Option.cmp cmp r1 r2
    | Mcallview (t1, a1, b1, c1), Mcallview (t2, a2, b2, c2)                           -> cmp_type t1 t2 && cmp a1 a2 && cmpi b1 b2 && cmp c1 c2
    | Mself id1, Mself id2                                                             -> cmpi id1 id2
    (* operation *)
    | Moperations, Moperations                                                         -> true
    | Mmkoperation (v1, d1, a1), Mmkoperation (v2, d2, a2)                             -> cmp v1 v2 && cmp d1 d2 && cmp a1 a2
    (* literals *)
    | Mint v1, Mint v2                                                                 -> Big_int.eq_big_int v1 v2
    | Mnat v1, Mnat v2                                                                 -> Big_int.eq_big_int v1 v2
    | Mbool v1, Mbool v2                                                               -> cmp_bool v1 v2
    | Mrational (n1, d1), Mrational (n2, d2)                                           -> Big_int.eq_big_int n1 n2 && Big_int.eq_big_int d1 d2
    | Mstring v1, Mstring v2                                                           -> cmp_ident v1 v2
    | Mcurrency (v1, c1), Mcurrency (v2, c2)                                           -> Big_int.eq_big_int v1 v2 && cmp_currency c1 c2
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
    (* control expression *)
    | Mexprif (c1, t1, e1), Mexprif (c2, t2, e2)                                       -> cmp c1 c2 && cmp t1 t2 && cmp e1 e2
    | Mexprmatchwith (e1, l1), Mexprmatchwith (e2, l2)                                 -> cmp e1 e2 && List.for_all2 (fun (p1, t1) (p2, t2) -> cmp_pattern p1 p2 && cmp t1 t2) l1 l2
    | Mmatchoption (x1, i1, ve1, ne1), Mmatchoption (x2, i2, ve2, ne2)                 -> cmp x1 x2 && cmpi i1 i2 && cmp ve1 ve2 && cmp ne1 ne2
    | Mmatchor (x1, lid1, le1, rid1, re1), Mmatchor (x2, lid2, le2, rid2, re2)         -> cmp x1 x2 && cmpi lid1 lid2 && cmp le1 le2 && cmpi rid1 rid2 && cmp re1 re2
    | Mmatchlist (x1, hid1, tid1, hte1, ee1), Mmatchlist (x2, hid2, tid2, hte2, ee2)   -> cmp x1 x2 && cmpi hid1 hid2 && cmpi tid1 tid2 && cmp hte1 hte2 && cmp ee1 ee2
    | Mternarybool (c1, a1, b1), Mternarybool (c2, a2, b2)                             -> cmp c1 c2 && cmp a1 a2 && cmp b1 b2
    | Mternaryoption (c1, a1, b1), Mternaryoption (c2, a2, b2)                         -> cmp c1 c2 && cmp a1 a2 && cmp b1 b2
    | Mternaryasset (an1, k1, a1, b1), Mternaryasset (an2, k2, a2, b2)                 -> cmp_ident an1 an2 && cmp k1 k2 && cmp a1 a2 && cmp b1 b2
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
    | Mremoveall (an1, fn1, a1), Mremoveall (an2, fn2, a2)                             -> cmp_ident an1 an2 && cmp_ident fn1 fn2 && cmp a1 a2
    | Mremoveif (an1, c1, la1, lb1, a1), Mremoveif (an2, c2, la2, lb2, a2)             -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && List.for_all2 (fun (i1, t1) (i2, t2) -> cmp_ident i1 i2 && cmp_type t1 t2) la1 la2 && cmp lb1 lb2 && List.for_all2 cmp a1 a2
    | Mclear (an1, v1), Mclear (an2, v2)                                               -> cmp_ident an1 an2 && cmp_container_kind v1 v2
    | Mset (c1, l1, k1, v1), Mset (c2, l2, k2, v2)                                     -> cmp_ident c1 c2 && List.for_all2 cmp_ident l1 l2 && cmp k1 k2 && cmp v1 v2
    | Mupdate (an1, k1, l1), Mupdate (an2, k2, l2)                                     -> cmp_ident an1 an2 && cmp k1 k2 && List.for_all2 (fun (id1, op1, v1) (id2, op2, v2) -> cmpi id1 id2 && cmp_assign_op op1 op2 && cmp v1 v2) l1 l2
    | Maddupdate (an1, c1, k1, l1), Maddupdate (an2, c2, k2, l2)                       -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp k1 k2 && List.for_all2 (fun (id1, op1, v1) (id2, op2, v2) -> cmpi id1 id2 && cmp_assign_op op1 op2 && cmp v1 v2) l1 l2
    (* asset api expression *)
    | Mget (an1, c1, k1), Mget (an2, c2, k2)                                           -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp k1 k2
    | Mgetopt (an1, c1, k1), Mgetopt (an2, c2, k2)                                     -> cmp_ident an1 an2 && cmp_container_kind c1 c2 && cmp k1 k2
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
    (* set api expression *)
    | Msetadd (t1, c1, a1), Msetadd (t2, c2, a2)                                       -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Msetremove (t1, c1, a1), Msetremove (t2, c2, a2)                                 -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Msetcontains (t1, c1, a1), Msetcontains (t2, c2, a2)                             -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Msetlength (t1, c1), Msetlength (t2, c2)                                         -> cmp_type t1 t2 && cmp c1 c2
    | Msetfold (t1, ix1, ia1, c1, a1, b1), Msetfold (t2, ix2, ia2, c2, a2, b2)         -> cmp_type t1 t2 && cmp_lident ix1 ix2 && cmp_lident ia1 ia2 && cmp c1 c2 && cmp a1 a2 && cmp b1 b2
    (* set api instruction *)
    | Msetinstradd (t1, ak1, a1),    Msetinstradd (t2, ak2, a2)                        -> cmp_type t1 t2 && cmp_assign_kind ak1 ak2 && cmp a1 a2
    | Msetinstrremove (t1, ak1, a1), Msetinstrremove (t2, ak2, a2)                     -> cmp_type t1 t2 && cmp_assign_kind ak1 ak2 && cmp a1 a2
    (* list api expression *)
    | Mlistprepend (t1, c1, a1), Mlistprepend (t2, c2, a2)                             -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Mlistlength (t1, c1), Mlistlength (t2, c2)                                       -> cmp_type t1 t2 && cmp c1 c2
    | Mlistcontains (t1, c1, a1), Mlistcontains (t2, c2, a2)                           -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Mlistnth (t1, c1, a1), Mlistnth (t2, c2, a2)                                     -> cmp_type t1 t2 && cmp c1 c2 && cmp a1 a2
    | Mlistreverse (t1, l1), Mlistreverse (t2, l2)                                     -> cmp_type t1 t2 && cmp l1 l2
    | Mlistconcat (t1, l1, m1), Mlistconcat (t2, l2, m2)                               -> cmp_type t1 t2 && cmp l1 l2 && cmp m1 m2
    | Mlistfold (t1, ix1, ia1, c1, a1, b1), Mlistfold (t2, ix2, ia2, c2, a2, b2)       -> cmp_type t1 t2 && cmp_lident ix1 ix2 && cmp_lident ia1 ia2 && cmp c1 c2 && cmp a1 a2 && cmp b1 b2
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
    | Mmapfold (mk1, t1, ik1, iv1, ia1, c1, a1, b1), Mmapfold (mk2, t2, ik2, iv2, ia2, c2, a2, b2) -> cmp_map_kind mk1 mk2 && cmp_type t1 t2 && cmp_lident ik1 ik2 && cmp_lident iv1 iv2 && cmp_lident ia1 ia2 && cmp c1 c2 && cmp a1 a2 && cmp b1 b2
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
    | Misnat x1, Misnat x2                                                             -> cmp x1 x2
    | Mtonat x1, Mtonat x2                                                             -> cmp x1 x2
    | Moptget x1, Moptget x2                                                           -> cmp x1 x2
    | Mrequiresome (x1, y1), Mrequiresome (x2, y2)                                     -> cmp x1 x2 && cmp y1 y2
    | Mfloor x1, Mfloor x2                                                             -> cmp x1 x2
    | Mceil x1, Mceil x2                                                               -> cmp x1 x2
    | Mtostring(t1, x1), Mtostring (t2, x2)                                            -> cmp_type t1 t2 && cmp x1 x2
    | Mpack x1, Mpack x2                                                               -> cmp x1 x2
    | Munpack (t1, x1), Munpack (t2, x2)                                               -> cmp_type t1 t2 && cmp x1 x2
    | Msetdelegate x1, Msetdelegate x2                                                 -> cmp x1 x2
    | Mimplicitaccount x1, Mimplicitaccount x2                                         -> cmp x1 x2
    | Mcontractaddress x1, Mcontractaddress x2                                         -> cmp x1 x2
    | Maddresscontract x1, Maddresscontract x2                                         -> cmp x1 x2
    | Mkeyaddress x1, Mkeyaddress x2                                                   -> cmp x1 x2
    (* crypto functions *)
    | Mblake2b x1, Mblake2b x2                                                         -> cmp x1 x2
    | Msha256  x1, Msha256  x2                                                         -> cmp x1 x2
    | Msha512  x1, Msha512  x2                                                         -> cmp x1 x2
    | Msha3    x1, Msha3    x2                                                         -> cmp x1 x2
    | Mkeccak  x1, Mkeccak  x2                                                         -> cmp x1 x2
    | Mhashkey x1, Mhashkey  x2                                                        -> cmp x1 x2
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
    | Mchainid, Mchainid                                                               -> true
    | Mmetadata, Mmetadata                                                             -> true
    | Mlevel, Mlevel                                                                   -> true
    (* variable *)
    | Mvar (id1, k1, t1, d1), Mvar (id2, k2, t2, d2)                                   -> cmpi id1 id2 && cmp_var_kind k1 k2 && cmp_temp t1 t2 && cmp_delta d1 d2
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
    | Mdatefromtimestamp v1, Mdatefromtimestamp v2                                     -> cmp v1 v2
    | Mmuteztonat v1, Mmuteztonat v2                                                   -> cmp v1 v2
    (* quantifiers *)
    | Mforall (i1, t1, t2, e1), Mforall (i2, t3, t4, e2)                               -> cmpi i1 i2 && cmp_type t1 t3 && Option.cmp cmp t2 t4 && cmp e1 e2
    | Mexists (i1, t1, t2, e1), Mforall (i2, t3, t4, e2)                               -> cmpi i1 i2 && cmp_type t1 t3 && Option.cmp cmp t2 t4 && cmp e1 e2
    (* formula operators *)
    | Mimply (l1, r1), Mimply (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    | Mequiv (l1, r1), Mequiv (l2, r2)                                                 -> cmp l1 l2 && cmp r1 r2
    (* formula asset collection *)
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
    | Btostring t1, Btostring t2 -> cmp_type t1 t2
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
  | Tset k                   -> Tset k
  | Tmap (k, v)              -> Tmap (f k, f v)
  | Tbig_map (k, v)          -> Tbig_map (f k, f v)
  | Titerable_big_map (k, v) -> Titerable_big_map (f k, f v)
  | Tor (l, r)               -> Tor (f l, f r)
  | Trecord id               -> Trecord id
  | Tevent id                -> Tevent id
  | Tlambda (a, r)           -> Tlambda (f a, f r)
  | Tunit                    -> Tunit
  | Tstorage                 -> Tstorage
  | Toperation               -> Toperation
  | Tcontract t              -> Tcontract (f t)
  | Tticket t                -> Tticket (f t)
  | Tsapling_state n         -> Tsapling_state n
  | Tsapling_transaction n   -> Tsapling_transaction n
  | Tprog t                  -> Tprog (f t)
  | Tvset (v, t)             -> Tvset (v, f t)
  | Ttrace t                 -> Ttrace t

let map_type (f : type_ -> type_) (t : type_) : type_ =
  mktype ?annot:(get_atype t) (map_ptyp f (get_ntype t))

(* -------------------------------------------------------------------- *)

let map_for_ident (g : 'id -> 'id) = function
  | FIsimple i             -> FIsimple (g i)
  | FIdouble (x, y)        -> FIdouble (g x, g y)

let map_assign_kind (fi : ident -> ident) (g : 'id -> 'id) f = function
  | Avar id             -> Avar (g id)
  | Avarstore id        -> Avarstore (g id)
  | Aasset (an, fn, k)  -> Aasset (g an, g fn, f k)
  | Arecord (rn, fn, r) -> Arecord (g rn, g fn, f r)
  | Avartuple (id, n)   -> Avartuple (g id, n)
  | Astate              -> Astate
  | Aassetstate (id, v) -> Aassetstate (fi id, f v)
  | Aoperations         -> Aoperations

let map_var_kind f = function
  | Vassetstate mt -> Vassetstate (f mt)
  | Vstorevar -> Vstorevar
  | Vstorecol -> Vstorecol
  | Vdefinition -> Vdefinition
  | Vlocal -> Vlocal
  | Vparam -> Vparam
  | Vfield -> Vfield
  | Vstate -> Vstate
  | Vthe -> Vthe
  | Vparameter -> Vparameter

let map_temp (fi : ident -> ident) = function
  | Tbefore -> Tbefore
  | Tat i   -> Tat (fi i)
  | Tnone   -> Tnone

let map_delta = function
  | Dadded   -> Dadded
  | Dremoved -> Dremoved
  | Dunmoved -> Dunmoved
  | Dnone    -> Dnone

let map_container_kind (fi : ident -> ident) f = function
  | CKcoll (t, d)              -> CKcoll (map_temp fi t, map_delta d)
  | CKview  mt                 -> CKview  (f mt)
  | CKfield (an, fn, mt, t, d) -> CKfield (fi an, fi fn, f mt, map_temp fi t, map_delta d)
  | CKdef v                    -> CKdef (fi v)

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
  | TKself (x, id, args)    -> TKself (f x, fi id, List.map (fun (id, v) -> fi id, f v) args)
  | TKoperation x           -> TKoperation (f x)

let map_term_node_internal (fi : ident -> ident) (g : 'id -> 'id) (ft : type_ -> type_) (f : 'id mterm_gen -> 'id mterm_gen) = function
  (* lambda *)
  | Mletin (i, a, t, b, o)         -> Mletin (List.map g i, f a, Option.map ft t, f b, Option.map f o)
  | Mdeclvar (i, t, v, c)          -> Mdeclvar (List.map g i, Option.map ft t, f v, c)
  | Mapp (e, args)                 -> Mapp (g e, List.map f args)
  (* assign *)
  | Massign (op, t, k, v)          -> Massign (op, ft t, map_assign_kind fi g f k, f v)
  (* control *)
  | Mif (c, t, e)                  -> Mif (f c, f t, Option.map f e)
  | Mmatchwith (e, l)              -> Mmatchwith (f e, List.map (fun (p, e) -> (p, f e)) l)
  | Minstrmatchoption (x, i, ve, ne)       -> Minstrmatchoption (f x, g i, f ve, f ne)
  | Minstrmatchor (x, lid, le, rid, re)    -> Minstrmatchor     (f x, g lid, f le, g rid, f re)
  | Minstrmatchlist (x, hid, tid, hte, ee) -> Minstrmatchlist   (f x, g hid, g tid, f hte, f ee)
  | Mfor (i, c, b, lbl)            -> Mfor (map_for_ident g i, map_iter_container_kind fi f c, f b, lbl)
  | Miter (i, a, b, c, lbl, strict)-> Miter (g i, f a, f b, f c, lbl, strict)
  | Mwhile (c, b, lbl)             -> Mwhile (f c, f b, lbl)
  | Mseq is                        -> Mseq (List.map f is)
  | Mreturn x                      -> Mreturn (f x)
  | Mlabel i                       -> Mlabel (g i)
  | Mmark (i, x)                   -> Mmark (g i, f x)
  (* effect *)
  | Mfail v                        -> Mfail (match v with | Invalid v -> Invalid (f v) | _ -> v)
  | Mtransfer tr                   -> Mtransfer (map_transfer_kind fi ft f tr)
  | Memit (e, x)                   -> Memit (g e, f x)
  (* entrypoint *)
  | Mentrypoint (t, a, s, r)       -> Mentrypoint (ft t, g a, f s, Option.map f r)
  | Mcallview (t, a, b, c)         -> Mcallview (ft t, f a, g b, f c)
  | Mself id                       -> Mself (g id)
  (* operation *)
  | Moperations                    -> Moperations
  | Mmkoperation (v, d, a)         -> Mmkoperation (f v, f d, f a)
  (* literals *)
  | Mint v                         -> Mint v
  | Mnat v                         -> Mnat v
  | Mbool v                        -> Mbool v
  | Mrational (n, d)               -> Mrational (n, d)
  | Mstring v                      -> Mstring v
  | Mcurrency (v, c)               -> Mcurrency (v, c)
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
  (* control expression *)
  | Mexprif (c, t, e)              -> Mexprif        (f c, f t, f e)
  | Mexprmatchwith (e, l)          -> Mexprmatchwith (f e, List.map (fun (p, e) -> (p, f e)) l)
  | Mmatchoption (x, i, ve, ne)    -> Mmatchoption   (f x, g i, f ve, f ne)
  | Mmatchor (x, lid, le, rid, re) -> Mmatchor       (f x, g lid, f le, g rid, f re)
  | Mmatchlist (x, hid, tid, hte, ee) -> Mmatchlist  (f x, g hid, g tid, f hte, f ee)
  | Mternarybool (c, a, b)         -> Mternarybool   (f c, f a, f b)
  | Mternaryoption (c, a, b)       -> Mternaryoption (f c, f a, f b)
  | Mternaryasset (an, k, a, b)    -> Mternaryasset  (fi an, f k, f a, f b)
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
  | Mremoveall (an, fn, a)         -> Mremoveall (fi an, fi fn, f a)
  | Mremoveif (an, c, la, lb, a)   -> Mremoveif (fi an, map_container_kind fi f c, List.map (fun (i, t) -> (fi i, ft t)) la, f lb, List.map f a)
  | Mclear (an, v)                 -> Mclear (fi an, map_container_kind fi f v)
  | Mset (an, l, k, v)             -> Mset (fi an, List.map fi l, f k, f v)
  | Mupdate (an, k, l)             -> Mupdate (fi an, f k, List.map (fun (id, op, v) -> (g id, op, f v)) l)
  | Maddupdate (an, c, k, l)       -> Maddupdate (fi an, map_container_kind fi f c, f k, List.map (fun (id, op, v) -> (g id, op, f v)) l)
  (* asset api expression *)
  | Mget (an, c, k)                -> Mget (fi an, map_container_kind fi f c, f k)
  | Mgetopt (an, c, k)             -> Mgetopt (fi an, map_container_kind fi f c, f k)
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
  (* set api expression *)
  | Msetadd (t, c, a)              -> Msetadd (ft t, f c, f a)
  | Msetremove (t, c, a)           -> Msetremove (ft t, f c, f a)
  | Msetcontains (t, c, a)         -> Msetcontains (ft t, f c, f a)
  | Msetlength (t, c)              -> Msetlength (ft t, f c)
  | Msetfold (t, ix, ia, c, a, b)  -> Msetfold (ft t, g ix, g ia, f c, f a, f b)
  (* set api instruction *)
  | Msetinstradd (t, ak, a)        -> Msetinstradd (ft t, map_assign_kind fi g f ak, f a)
  | Msetinstrremove (t, ak, a)     -> Msetinstrremove (ft t, map_assign_kind fi g f ak, f a)
  (* list api expression *)
  | Mlistprepend (t, c, a)         -> Mlistprepend (ft t, f c, f a)
  | Mlistlength(t, c)              -> Mlistlength(ft t, f c)
  | Mlistcontains (t, c, a)        -> Mlistcontains (ft t, f c, f a)
  | Mlistnth (t, c, a)             -> Mlistnth (ft t, f c, f a)
  | Mlistreverse(t, l)             -> Mlistreverse(ft t, f l)
  | Mlistconcat(t, l, m)           -> Mlistconcat(ft t, f l, f m)
  | Mlistfold (t, ix, ia, c, a, b) -> Mlistfold (ft t, g ix, g ia, f c, f a, f b)
  (* list api instruction *)
  | Mlistinstrprepend (t, ak, a)   -> Mlistinstrprepend (ft t, map_assign_kind fi g f ak, f a)
  | Mlistinstrconcat (t, ak, a)    -> Mlistinstrconcat (ft t, map_assign_kind fi g f ak, f a)
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
  | Mmapinstrput (mk, tk, tv, ak, k, v)    -> Mmapinstrput (mk, ft tk, ft tv, map_assign_kind fi g f ak, f k, f v)
  | Mmapinstrremove (mk, tk, tv, ak, k)    -> Mmapinstrremove (mk, ft tk, ft tv, map_assign_kind fi g f ak, f k)
  | Mmapinstrupdate (mk, tk, tv, ak, k, v) -> Mmapinstrupdate (mk, ft tk, ft tv, map_assign_kind fi g f ak, f k, f v)
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
  | Misnat x                       -> Misnat (f x)
  | Mtonat x                       -> Mtonat (f x)
  | Moptget x                      -> Moptget (f x)
  | Mrequiresome (x, y)            -> Mrequiresome (f x, f y)
  | Mfloor x                       -> Mfloor (f x)
  | Mceil x                        -> Mceil (f x)
  | Mtostring (t, x)               -> Mtostring (ft t, f x)
  | Mpack x                        -> Mpack (f x)
  | Munpack (t, x)                 -> Munpack (ft t, f x)
  | Msetdelegate x                 -> Msetdelegate (f x)
  | Mimplicitaccount x             -> Mimplicitaccount (f x)
  | Mcontractaddress x             -> Mcontractaddress (f x)
  | Maddresscontract x             -> Maddresscontract (f x)
  | Mkeyaddress x                  -> Mkeyaddress (f x)
  (* crypto functions *)
  | Mblake2b x                     -> Mblake2b (f x)
  | Msha256 x                      -> Msha256  (f x)
  | Msha512 x                      -> Msha512  (f x)
  | Msha3 x                        -> Msha3    (f x)
  | Mkeccak x                      -> Mkeccak  (f x)
  | Mhashkey x                     -> Mhashkey (f x)
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
  | Mchainid                       -> Mchainid
  | Mmetadata                      -> Mmetadata
  | Mlevel                         -> Mlevel
  (* variable *)
  | Mvar (id, k, t, d)             -> Mvar (g id, map_var_kind f k, map_temp fi t, map_delta d)
  | Menumval (id, args, e)         -> Menumval (g id, List.map f args, fi e)
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
  | Mdatefromtimestamp v           -> Mdatefromtimestamp (f v)
  | Mmuteztonat v                  -> Mmuteztonat (f v)
  (* quantifiers *)
  | Mforall (i, t, s, e)           -> Mforall (g i, ft t, Option.map f s, f e)
  | Mexists (i, t, s, e)           -> Mexists (g i, ft t, Option.map f s, f e)
  (* formula operators *)
  | Mimply (l, r)                  -> Mimply (f l, f r)
  | Mequiv  (l, r)                 -> Mequiv (f l, f r)
  (* formula asset collection *)
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
      | Getter   (fs, ret) -> Getter   (map_function_struct ctx fs, ret)
      | View   (fs, ret)   -> View     (map_function_struct ctx fs, ret)
      | Entry     fs       -> Entry    (map_function_struct ctx fs)
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
      | Getter (fs, _) -> fs
      | View (fs, _) -> fs
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
  | Avartuple _         -> accu
  | Astate              -> accu
  | Aassetstate (_, mt) -> f accu mt
  | Aoperations         -> accu

let fold_var_kind f accu = function
  | Vassetstate mt -> f accu mt
  | Vstorevar
  | Vstorecol
  | Vdefinition
  | Vlocal
  | Vparam
  | Vfield
  | Vstate
  | Vthe
  | Vparameter -> accu

let fold_container_kind f accu = function
  | CKcoll _                 -> accu
  | CKview mt                -> f accu mt
  | CKfield (_, _, mt, _, _) -> f accu mt
  | CKdef _                  -> accu

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
  | TKself (x, _, args)    -> List.fold_left f (f accu x) (List.map snd args)
  | TKoperation x          -> f accu x

let fold_term (f : 'a -> ('id mterm_gen) -> 'a) (accu : 'a) (term : 'id mterm_gen) : 'a =
  let opt f accu x = match x with | Some v -> f accu v | None -> accu in
  match term.node with
  (* lambda *)
  | Mletin (_, a, _, b, o)                -> let tmp = f (f accu a) b in Option.map_dfl (f tmp) tmp o
  | Mdeclvar (_, _, v, _)                 -> f accu v
  | Mapp (_, args)                        -> List.fold_left f accu args
  (* assign *)
  | Massign (_, _, k, e)                  -> f (fold_assign_kind f accu k) e
  (* control *)
  | Mif (c, t, e)                         -> opt f (f (f accu c) t) e
  | Mmatchwith (e, l)                     -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Minstrmatchoption (x, _, ve, ne)      -> f (f (f accu x) ve) ne
  | Minstrmatchor (x, _, le, _, re)       -> f (f (f accu x) le) re
  | Minstrmatchlist (x, _, _, hte, ee)    -> f (f (f accu x) hte) ee
  | Mfor (_, c, b, _)                     -> f (fold_iter_container_kind f accu c) b
  | Miter (_, a, b, c, _, _)              -> f (f (f accu a) b) c
  | Mwhile (c, b, _)                      -> f (f accu c) b
  | Mseq is                               -> List.fold_left f accu is
  | Mreturn x                             -> f accu x
  | Mlabel _                              -> accu
  | Mmark (_, x)                          -> f accu x
  (* effect *)
  | Mfail v                               -> (match v with | Invalid v -> f accu v | _ -> accu)
  | Mtransfer tr                          -> fold_transfer_kind f accu tr
  | Memit (_, x)                          -> f accu x
  (* entrypoint *)
  | Mentrypoint (_, _, s, r)              -> let tmp = f accu s in Option.map_dfl (f tmp) tmp r
  | Mcallview (_, a, _, c)                -> f (f accu a) c
  | Mself _                               -> accu
  (* operation *)
  | Moperations                           -> accu
  | Mmkoperation (v, d, a)                -> f (f (f accu v) d) a
  (* literals *)
  | Mint _                                -> accu
  | Mnat _                                -> accu
  | Mbool _                               -> accu
  | Mrational _                           -> accu
  | Mstring _                             -> accu
  | Mcurrency _                           -> accu
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
  (* control expression *)
  | Mexprif (c, t, e)                     -> f (f (f accu c) t) e
  | Mexprmatchwith (e, l)                 -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Mmatchoption (x, _, ve, ne)           -> f (f (f accu x) ve) ne
  | Mmatchor (x, _, le, _, re)            -> f (f (f accu x) le) re
  | Mmatchlist (x, _, _, hte, ee)         -> f (f (f accu x) hte) ee
  | Mternarybool (c, a, b)                -> f (f (f accu c) a) b
  | Mternaryoption (c, a, b)              -> f (f (f accu c) a) b
  | Mternaryasset (_, k, a, b)            -> f (f (f accu k) a) b
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
  | Mremoveall (_, _, a)                  -> f accu a
  | Mremoveif (_, c, _, lb, a)            -> List.fold_left (fun accu x -> f accu x) (f (fold_container_kind f accu c) lb) a
  | Mclear (_, v)                         -> fold_container_kind f accu v
  | Mset (_, _, k, v)                     -> f (f accu v) k
  | Mupdate (_, k, l)                     -> List.fold_left (fun accu (_, _, v) -> f accu v) (f accu k) l
  | Maddupdate (_, c, k, l)               -> List.fold_left (fun accu (_, _, v) -> f accu v) (f (fold_container_kind f accu c) k) l
  (* asset api expression *)
  | Mget (_, c, k)                        -> f (fold_container_kind f accu c) k
  | Mgetopt (_, c, k)                     -> f (fold_container_kind f accu c) k
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
  (* set api expression *)
  | Msetadd (_, c, a)                     -> f (f accu c) a
  | Msetremove (_, c, a)                  -> f (f accu c) a
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
  | Misnat x                              -> f accu x
  | Mtonat x                              -> f accu x
  | Moptget x                             -> f accu x
  | Mrequiresome (x, y)                   -> f (f accu x) y
  | Mfloor x                              -> f accu x
  | Mceil x                               -> f accu x
  | Mtostring (_, x)                      -> f accu x
  | Mpack x                               -> f accu x
  | Munpack (_, x)                        -> f accu x
  | Msetdelegate x                        -> f accu x
  | Mimplicitaccount x                    -> f accu x
  | Mcontractaddress x                    -> f accu x
  | Maddresscontract x                    -> f accu x
  | Mkeyaddress x                         -> f accu x
  (* crypto functions *)
  | Mblake2b x                            -> f accu x
  | Msha256  x                            -> f accu x
  | Msha512  x                            -> f accu x
  | Msha3    x                            -> f accu x
  | Mkeccak  x                            -> f accu x
  | Mhashkey  x                           -> f accu x
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
  | Mchainid                              -> accu
  | Mmetadata                             -> accu
  | Mlevel                                -> accu
  (* variable *)
  | Mvar (_, k, _, _)                     -> fold_var_kind f accu k
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
  | Mdatefromtimestamp v                  -> f accu v
  | Mmuteztonat v                         -> f accu v
  (* quantifiers *)
  | Mforall (_, _, s, e)                  -> f (opt f accu s) e
  | Mexists (_, _, s, e)                  -> f (opt f accu s) e
  (* formula operators *)
  | Mimply (l, r)                         -> f (f accu l) r
  | Mequiv  (l, r)                        -> f (f accu l) r
  (* formula asset collection *)
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
  | Avartuple (id, n)   -> Avartuple (id, n), accu
  | Astate              -> Astate, accu
  | Aassetstate (id, v) -> let ve, va = f accu v in Aassetstate (id, ve), va
  | Aoperations         -> Aoperations, accu

let fold_map_var_kind f accu = function
  | Vassetstate mt ->
    let mte, mta = f accu mt in
    Vassetstate mte, mta
  | Vstorevar -> Vstorevar, accu
  | Vstorecol -> Vstorecol, accu
  | Vdefinition -> Vdefinition, accu
  | Vlocal    -> Vlocal,    accu
  | Vparam    -> Vparam,    accu
  | Vfield    -> Vfield,    accu
  | Vstate    -> Vstate,    accu
  | Vthe      -> Vthe,      accu
  | Vparameter -> Vparameter, accu

let fold_map_container_kind f accu = function
  | CKcoll (t, d) -> CKcoll (t, d), accu
  | CKview mt ->
    let mte, mta = f accu mt in
    CKview mte, mta
  | CKfield (an, fn, mt, t, d) ->
    let mte, mta = f accu mt in
    CKfield (an, fn, mte, t, d), mta
  | CKdef v -> CKdef v, accu

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

  | Mdeclvar (ids, t, v, c) ->
    let ve, va = f accu v in
    g (Mdeclvar (ids, t, ve, c)), va

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

  | Mfor (fi, c, b, lbl) ->
    let ce, ca = fold_map_iter_container_kind f accu c in
    let be, ba = f ca b in
    g (Mfor (fi, ce, be, lbl)), ba

  | Miter (i, a, b, c, lbl, strict) ->
    let ae, aa = f accu a in
    let be, ba = f aa b in
    let ce, ca = f ba c in
    g (Miter (i, ae, be, ce, lbl, strict)), ca

  | Mwhile (c, b, lbl) ->
    let ce, ca = f accu c in
    let be, ba = f ca b in
    g (Mwhile (ce, be, lbl)), ba

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

  | Mtransfer tr ->
    let tre, tra = fold_map_transfer_kind f accu tr in
    g (Mtransfer tre), tra

  | Memit (e, x) ->
    let xe, xa = f accu x in
    g (Memit (e, xe)), xa


  (* entrypoint *)

  | Mentrypoint (t, a, s, r) ->
    let se, sa = f accu s in
    let re, ra =
      match r with
      | Some r -> f sa r |> (fun (x, y) -> (Some x, y))
      | None -> (None, sa)
    in
    g (Mentrypoint (t, a, se, re)), ra

  | Mcallview (t, a, b, c) ->
    let ae, aa = f accu a in
    let ce, ca = f aa c in
    g (Mcallview (t, ae, b, ce)), ca

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

  | Mternaryasset (an, k, a, b) ->
    let ke, ka = f accu k in
    let ae, aa = f ka a in
    let be, ba = f aa b in
    g (Mternaryasset (an, ke, ae, be)), ba

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

  | Mgetopt (an, c, k) ->
    let ce, ca = fold_map_container_kind f accu c in
    let ke, ka = f ca k in
    g (Mgetopt (an, ce, ke)), ka

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

  | Misnat x ->
    let xe, xa = f accu x in
    g (Misnat xe), xa

  | Mtonat x ->
    let xe, xa = f accu x in
    g (Mtonat xe), xa

  | Misnone x ->
    let xe, xa = f accu x in
    g (Misnone xe), xa

  | Moptget x ->
    let xe, xa = f accu x in
    g (Moptget xe), xa

  | Mrequiresome (x, y) ->
    let xe, xa = f accu x in
    let ye, ya = f xa y in
    g (Mrequiresome (xe, ye)), ya

  | Mfloor x ->
    let xe, xa = f accu x in
    g (Mfloor xe), xa

  | Mceil x ->
    let xe, xa = f accu x in
    g (Mceil xe), xa

  | Mtostring (t, x) ->
    let xe, xa = f accu x in
    g (Mtostring (t, xe)), xa

  | Mpack x ->
    let xe, xa = f accu x in
    g (Mpack xe), xa

  | Munpack (t, x) ->
    let xe, xa = f accu x in
    g (Munpack (t, xe)), xa

  | Msetdelegate x ->
    let xe, xa = f accu x in
    g (Msetdelegate xe), xa

  | Mimplicitaccount x ->
    let xe, xa = f accu x in
    g (Mimplicitaccount xe), xa

  | Mcontractaddress x ->
    let xe, xa = f accu x in
    g (Mcontractaddress xe), xa

  | Maddresscontract x ->
    let xe, xa = f accu x in
    g (Maddresscontract xe), xa

  | Mkeyaddress x ->
    let xe, xa = f accu x in
    g (Mkeyaddress xe), xa

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

  | Mhashkey x ->
    let xe, xa = f accu x in
    g (Mhashkey xe), xa

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

  | Mchainid ->
    g Mchainid, accu

  | Mmetadata ->
    g Mmetadata, accu

  | Mlevel ->
    g Mlevel, accu


  (* variable *)

  | Mvar (id, k, t, d) ->
    let ke, ka = fold_map_var_kind f accu k in
    g (Mvar (id, ke, t, d)), ka

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

  | Mdatefromtimestamp v ->
    let ve, va = f accu v in
    g (Mdatefromtimestamp ve), va

  | Mmuteztonat v ->
    let ve, va = f accu v in
    g (Mmuteztonat ve), va


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
      | Getter (fs, _)
      | View (fs, _)
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
  let g k (id : lident) = {id with pldesc=(f k id.pldesc)} in

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
        | Btostring t -> Btostring (for_type t)
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
        kind          = v.kind;
        default       = Option.map for_mterm v.default;
        invariants    = List.map for_label_term v.invariants;
        loc           = v.loc;
      }
    in
    let for_enum (e : enum) : enum =
      let for_enum_item (ei : enum_item) : enum_item =
        {
          name        = g KIenumvalue ei.name;
          args        = List.map for_type ei.args;
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
        keys          = List.map (f KIassetfield) a.keys;
        sort          = List.map (g KIassetfield) a.sort;
        map_kind      = a.map_kind;
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
    let for_fail (f : fail) : fail =
      {
        label        = g KIfaillabel f.label;
        fid          = Option.map (g KIfailfid) f.fid;
        arg          = g KIfailarg   f.arg;
        atype        = for_type      f.atype;
        formula      = for_mterm     f.formula;
        loc          = f.loc;
      }
    in
    let for_variable (v : variable) : variable =
      let for_argument (arg : argument) : argument =
        let a, b, c = arg in
        g KIargument a, for_type b, Option.map for_mterm c
      in
      {
        decl = for_argument v.decl;
        kind = v.kind;
        loc  = v.loc;
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
      fails          = List.map for_fail          spec.fails;
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
          name  = g (match fn with | Function _ -> KIfunction | Getter _ -> KIgetter | View _ -> KIview | Entry _ -> KIentry) fs.name;
          args  = List.map for_argument fs.args;
          eargs = List.map for_argument fs.eargs;
          stovars = fs.stovars;
          body  = for_mterm fs.body;
          loc   = fs.loc;
        }
      in
      match fn with
      | Function (fs, t) -> Function (for_function_struct fs, for_type t)
      | Getter   (fs, t) -> Getter   (for_function_struct fs, for_type t)
      | View     (fs, t) -> View     (for_function_struct fs, for_type t)
      | Entry     fs     -> Entry    (for_function_struct fs)
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
          | SonlyInEntry       (ad, sa)       -> SonlyInEntry        (for_entry_description ad, for_security_entry sa)
          | SonlyByRoleInEntry (ad, srl, sa)  -> SonlyByRoleInEntry  (for_entry_description ad, List.map for_security_role srl, for_security_entry sa)
          | SnotByRole          (ad, srl)     -> SnotByRole          (for_entry_description ad, List.map for_security_role srl)
          | SnotInEntry        (ad, sa)       -> SnotInEntry         (for_entry_description ad, for_security_entry sa)
          | SnotByRoleInEntry  (ad, srl, sa)  -> SnotByRoleInEntry   (for_entry_description ad, List.map for_security_role srl, for_security_entry sa)
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
    parameters    = List.map for_parameter model.parameters;
    metadata      = Option.map for_metadata model.metadata;
    api_items     = List.map for_api_item  model.api_items;
    api_verif     = List.map for_api_verif model.api_verif;
    decls         = List.map for_decl_node model.decls;
    storage       = List.map for_storage_item model.storage;
    functions     = List.map for_function__ model.functions;
    specification = for_specification model.specification;
    security      = for_security model.security;
    loc           = model.loc;
  }

let replace_ident_model (f : kind_ident -> ident -> ident) (model : model) : model =
  let g k (id : lident) = {id with pldesc=(f k id.pldesc)} in
  let rec for_type (t : type_) : type_ =
    let for_ntype (nt : ntype) : ntype =
      match nt with
      | Tasset id                -> Tasset (g KIassetname id)
      | Tenum id                 -> Tenum (g KIenumname id)
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
      | Trecord id               -> Trecord (g KIrecordname id)
      | Tevent id                -> Tevent (g KIrecordname id)
      | Tlambda (a, r)           -> Tlambda (for_type a, for_type r)
      | Tunit                    -> nt
      | Tstorage                 -> nt
      | Toperation               -> nt
      | Tcontract t              -> Tcontract (for_type t)
      | Tticket t                -> Tticket (for_type t)
      | Tsapling_state _         -> nt
      | Tsapling_transaction _   -> nt
      | Tprog a                  -> Tprog (for_type a)
      | Tvset (v, a)             -> Tvset (v, for_type a)
      | Ttrace _                 -> nt
    in
    mktype ?annot:(get_atype t) (for_ntype (get_ntype t))
  in
  let rec for_mterm (mt : mterm) : mterm =
    let node : mterm__node = map_term_node_internal (f KImterm) (g KImterm) for_type for_mterm mt.node in
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
  val get_var                            : model -> ident -> var
  val get_enum                           : model -> ident -> enum
  val get_enum_opt                       : model -> ident -> enum option
  val get_enum_values                    : model -> ident -> ident list
  val get_asset                          : model -> ident -> asset
  val get_record                         : model -> ident -> record
  val get_event                          : model -> ident -> record
  val get_events                         : model -> record list
  val get_storage                        : model -> storage
  val get_asset_field                    : model -> (ident * ident) -> (ident * type_ * mterm option)
  val get_asset_key                      : model -> ident -> (ident * type_)
  val get_field_container                : model -> ident -> ident -> (ident * container)
  val is_storage_attribute               : model -> ident -> bool
  val get_named_field_list               : model -> ident -> 'a list -> (ident * 'a) list
  val get_containers                     : model -> (ident * ident * type_) list (* asset id, asset item *)
  val get_partitions                     : model -> (ident * ident * type_) list (* asset id, asset item *)
  val dest_container                     : type_ -> ident
  val get_container_asset_key            : model -> ident -> ident -> (ident * ident * type_)
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
  val get_storage_invariants             : model -> ident option -> (ident * ident * mterm) list
  val is_field_storage                   : model -> ident -> bool
  val with_trace                         : model -> bool
  val get_callers                        : model -> ident -> ident list
  val no_fail                            : model -> ident -> ident option
  val type_to_asset                      : type_ -> ident
  val get_map_function                   : model -> (ident * ident list) list
  val retrieve_all_properties            : model -> (ident * property) list
  val retrieve_property                  : model -> ident -> property
  val with_operations_for_mterm          : mterm -> bool
  val with_operations                    : model -> bool
  val get_source_for                     : model -> ctx_model -> mterm -> mterm option
  val cmp                                : mterm -> mterm -> int
  val eval                               : (ident * mterm) list -> mterm -> mterm
  val mk_rat                             : Core.big_int -> Core.big_int -> mterm
  val get_select_idx                     : model -> ident -> mterm -> int
  val get_sum_idx                        : model -> ident -> mterm -> int
  val get_removeif_idx                   : model -> ident -> mterm -> int
  val with_division                      : model -> bool
  val with_min_max                       : model -> bool
  val with_count                         : model -> ident -> bool
  val get_asset_collection               : ident -> mterm
  val is_asset_single_field              : model -> ident -> bool
  val is_asset_map                       : model -> ident -> bool
  val get_labeled_value_from             : model -> ident -> mterm list -> (ident * mterm) list
  val add_api_storage_in_list            : api_storage list -> api_storage -> api_storage list
  val sort_api_storage                   : model -> bool -> api_storage list -> api_storage list
  val get_all_set_types                  : model -> type_ list
  val get_all_list_types                 : model -> type_ list
  val get_all_map_types                  : model -> type_ list
  val get_all_fail_types                 : model -> type_ list
  val get_all_gen_mterm_type             : ('a list -> mterm -> 'a list) -> ('a list -> type_ -> 'a list) -> (decl_node -> 'a list -> 'a list) -> model -> 'a list
  val get_all_type_for_mterm             : ('a -> type_ -> 'a ) -> 'a -> mterm  -> 'a
  val extract_key_value_from_masset      : model -> mterm -> mterm
  val is_not_string_nat_int              : type_ -> bool
  val get_function                       : model -> ident -> function_struct
  val get_asset_partitions               : model -> ident -> (ident * ident) list
  val get_specifications                 : model -> specification list
  val get_specification                  : model -> ident -> specification option
  val get_fss                            : model -> function_struct list
  val get_fs                             : model -> ident -> function_struct
  val extract_assign_kind                : mterm -> assign_kind list
  val extract_asset_effect               : model -> mterm -> effect list
  val extract_var_idents                 : model -> mterm -> ident list
  val get_record_pos                     : model -> ident -> ident -> (int * int) list
  val is_partition                       : model -> ident -> ident -> bool

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

  let get_function_args (f : function__) : argument list =
    match f.node with
    | Function (s,_) -> s.args
    | Getter (s,_)   -> s.args
    | View (s,_)     -> s.args
    | Entry s        -> s.args

  let set_function_args (f : function__) (args : argument list) : function__ =
    match f.node with
    | Function (s, t) -> { node = Function ({ s with args = args },t); spec = f.spec }
    | Getter (s, t)   -> { node = Getter   ({ s with args = args },t); spec = f.spec }
    | View (s, t)     -> { node = View     ({ s with args = args },t); spec = f.spec }
    | Entry s         -> { node = Entry     { s with args = args };    spec = f.spec }

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

  let type_to_asset t =
    match get_ntype t with
    | Tasset n -> n |> unloc
    | Tcontainer ((Tasset n, _), _) -> n |> unloc
    | _ -> emit_error NotanAssetType

  let get_asset_type (t : mterm) : ident = type_to_asset t.type_

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

  let get_var   m id : var   = get_vars m   |> List.find (fun (x : var)   -> cmp_ident id (unloc x.name))
  let get_enum  m id : enum  = get_enums m  |> List.find (fun (x : enum)  -> cmp_ident id (unloc x.name))
  let get_enum_opt m id : enum option  = get_enums m |> List.find_opt (fun (x : enum)  -> cmp_ident id (unloc x.name))
  let get_enum_values m id : ident list  = get_enums m
                                           |> List.find (fun (x : enum)  -> cmp_ident id (unloc x.name))
                                           |> fun e -> e.values
                                                       |> List.map (fun (v : enum_item) -> unloc (v.name))
  let get_asset  m id : asset = get_assets m |> List.find (fun (x : asset) -> cmp_ident id (unloc x.name))
  let get_record m id : record = get_records m |> List.find (fun (x : record) -> cmp_ident id (unloc x.name))
  let get_event  m id : record = get_events m |> List.find (fun (x : record) -> cmp_ident id (unloc x.name))

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
    get_containers_internal (fun x -> match get_ntype x with | Tcontainer ((Tasset _, _), (Partition | Aggregate)) -> true | _ -> false ) m

  let get_partitions m : (ident * ident * type_) list =
    get_containers_internal (fun x -> match get_ntype x with |  Tcontainer ((Tasset _, _), Partition) -> true | _ -> false ) m

  let has_container (m : model) (asset : ident) : bool =
    try
      let asset = get_asset m asset in
      List.fold_left (fun acc (v : asset_item) ->
          match get_ntype v.type_ with
          | Tcontainer ((Tasset _, _), (Partition | Aggregate)) -> true
          | _ -> acc
        ) false asset.values
    with
    | Not_found -> false

  let get_asset_containers (m : model) (asset : ident) : (ident * type_ * (lident mterm_gen option)) list =
    try
      let asset = get_asset m asset in
      List.fold_left (fun acc (v : asset_item) ->
          match get_ntype v.type_ with
          | Tcontainer ((Tasset _, _), (Partition | Aggregate)) -> acc @ [unloc v.name, v.type_, v.default]
          | _ -> acc
        ) [] asset.values
    with
    | Not_found -> []

  let dest_container t =
    match get_ntype t with
    | Tcontainer ((Tasset p, _),(Partition | Aggregate)) -> unloc p
    | _ -> assert false

  let get_asset_field (m : model) (asset_name, field_name : ident * ident) : ident * type_ * mterm option =
    try
      let asset = get_asset m asset_name in
      List.find (fun (x : asset_item) -> String.equal (unloc x.name) field_name) asset.values
      |> (fun (x : asset_item) -> unloc x.name, x.type_, x.default)
    with
    | Not_found -> emit_error (AssetFieldNotFound (asset_name, field_name))

  let get_asset_keys (m : model) (asset_name : ident) : (ident * type_) list =
    try
      let asset = get_asset m asset_name in
      let key_ids = asset.keys in
      List.map (fun key_id -> key_id, (get_asset_field m (asset_name, key_id)|> fun (_, x, _) -> x)) key_ids
    with
    | Not_found -> emit_error (AssetKeyTypeNotFound (asset_name))

  let get_asset_key (m : model) (asset_name : ident) : (ident * type_) =
    match get_asset_keys m asset_name with
    | []  -> emit_error (EmptyAssetKeys (asset_name))
    | [x] -> x
    | _ -> emit_error (SeveralAssetKeys (asset_name))

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
    match get_ntype ot with
    | Tcontainer ((Tasset an, _), c) -> (unloc an, c)
    | _ -> assert false

  let get_container_assets model asset : ident list =
    get_containers model
    |> List.filter (fun (a,_,_) -> String.equal asset a)
    |> List.map (fun (_,_,t) -> type_to_asset t)

  (* returns : asset name, key name, key type *)
  let get_container_asset_key model asset field : (ident * ident * type_) =
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
      | Massign (_, _, Avar i,_) when String.equal (unloc i) id -> raise FoundAssign
      | Massign (_, _, Arecord (_, _, { node = (Mvar (i, _, _, _)) }), _) when String.equal (unloc i) id -> raise FoundAssign
      | _ -> fold_term rec_search_assign false t in
    try rec_search_assign false b
    with FoundAssign -> true


  exception FoundOperations

  let with_operations_for_mterm_intern _ctx accu (mt : mterm) : bool =
    let rec aux accu (t : mterm) =
      match t.node with
      | Mtransfer _
      | Moperations
      | Mmkoperation _
      | Massign (_, _, Aoperations,_)
      | Memit _
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
      | Getter (s,r) -> Getter ({
          s with body = m s.body;
        },r)
      | View (s,r) -> View ({
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
    | Mvar (_, Vlocal, _, _) -> true
    | _ -> false

  let dest_varlocal (t : mterm) =
    match t.node with
    |  Mvar (i, Vlocal, _, _) -> unloc i
    | _ -> assert false

  let is_container t =
    match get_ntype t with
    | Tcontainer ((Tasset _, _),_) -> true
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
           | Getter   (fs, _) -> unloc fs.name, fs
           | View   (fs, _)   -> unloc fs.name, fs
           | Entry     fs     -> unloc fs.name, fs)
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
        | Getter (fs, _) -> unloc fs.name
        | View (fs, _) -> unloc fs.name
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

  let get_source_for (_m : model) (_ctx : ctx_model) (c : mterm) : mterm option =
    match c.node with
    | Mvar(an, Vparam, t, d) ->
      begin
        let l, an = deloc an in
        let idparam = mkloc l (an ^ "_values") in
        Some (mk_mterm (Mvar(idparam, Vparam, t, d) ) (mktype (Tmap(tint, tasset (dumloc "myasset")))))
      end
    | _ -> None

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
    | Mbool      v1, Mbool v2      -> Bool.compare v1 v2
    | Mnat       v1, Mnat v2       -> Big_int.compare_big_int v1 v2
    | Mint       v1, Mint v2       -> Big_int.compare_big_int v1 v2
    | Mstring    v1, Mstring v2    -> String.compare v1 v2
    | Mcurrency  (v1, Utz), Mcurrency  (v2, Utz) -> Big_int.compare_big_int v1 v2
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
        | Mvar(v, Vstorevar, _, _)
        | Mvar(v, Vlocal, _, _) when is_const (unloc v) ->
          let dv = get_value (unloc v) in
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
      | Tbuiltin Bcurrency -> true
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
          | Mcurrency (v, Utz) -> v
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
            | Mcurrency (n1, Utz), Mcurrency (n2, Utz)
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
        | Mplus   (a, b), (Tbuiltin Bcurrency, _) when is_tez a && is_tez b -> begin
            let a = extract_tez a in
            let b = extract_tez b in
            mk_mterm (Mcurrency (Big_int.add_big_int a b, Utz)) ttez
          end
        | Mplus   (a, b), _ when is_timestamp a && is_int b -> begin
            let a = extract_timestamp a in
            let b = extract_big_int b in
            mk_mterm (Mtimestamp (Big_int.add_big_int a b)) ttimestamp
          end
        | Mplus   (a, b), t -> arith t `Plus  (aux a, aux b)
        | Mminus  (a, b), (Tbuiltin Bcurrency, _) when is_tez a && is_tez b -> begin
            let a = extract_tez a in
            let b = extract_tez b in
            let res = Big_int.sub_big_int a b in
            if Big_int.sign_big_int res < 0 then emit_error2(mt.loc, CurrencyValueCannotBeNegative);
            mk_mterm (Mcurrency (res, Utz)) ttez
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
            let f num denom v cur =
              let res = Big_int.div_big_int (Big_int.mult_big_int num v) denom in
              mk_mterm (Mcurrency (res, cur)) ttez
            in
            match coef.node, c.node with
            | Mrational (num, denom), Mcurrency (v, cur) -> f num denom v cur
            | Mtuple [num; denom], Mcurrency (v, cur) ->
              begin
                let num = extract_big_int num in
                let denom = extract_big_int denom in
                f num denom v cur
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

  type searchfun =
    | SearchSelect
    | SearchSum
    | SearchRemoveif

  let get_fun_idx typ (m : model) asset expr =
    let rec internal_get_fun_idx acc = function
      | (sc : api_storage) :: tl ->
        begin
          match typ, sc.node_item with
          | SearchSelect, APIAsset (Select (a, _, _, t)) -> continue_internal_get_fun_idx tl acc a t
          | SearchSum, APIAsset (Sum (a, _, _, t)) -> continue_internal_get_fun_idx tl acc a t
          | SearchRemoveif, APIAsset (RemoveIf (a, _, _, t)) -> continue_internal_get_fun_idx tl acc a t
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

  let get_removeif_idx = get_fun_idx SearchRemoveif

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
      | Massign (DivAssign,_,_,_) -> raise FoundDiv
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
    mk_mterm (Mvar (dumloc an, Vstorecol, Tnone, Dnone)) (mktype (Tcontainer (mktype (Tasset (dumloc an)), Collection)))

  let is_asset_single_field (model : model) (an : ident) : bool =
    get_asset model an |> fun x -> x.values |> List.filter (fun (x : asset_item) -> not x.shadow) |> List.length = 1

  let is_asset_map (model : model) (an : ident) : bool =
    get_asset model an |> fun x -> match x.map_kind with | MKMap -> true | _ -> false

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
             | APIBuiltin (Btostring     _) -> 41
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

  let get_all_gen_mterm_type for_mterm for_type for_decl (model : model) =
    let for_label_term accu (lt : label_term) = for_mterm accu lt.term in
    let for_decl_node accu (d : decl_node) =
      let for_var accu (v : var) =
        accu
        |> (fun accu -> for_type accu v.type_)
        |> (fun accu -> Option.map_dfl (for_mterm accu) accu v.default)
        |> (fun accu -> List.fold_left for_label_term accu v.invariants)
      in
      let for_enum accu (e : enum) =
        let for_enum_item accu (ei : enum_item) =
          List.fold_left for_label_term accu ei.invariants
        in
        List.fold_left for_enum_item accu e.values
      in
      let for_asset accu (a : asset) =
        let for_asset_item accu (ai : asset_item) =
          accu
          |> (fun accu -> for_type accu ai.type_)
          |> (fun accu -> Option.map_dfl (for_mterm accu) accu ai.default)
        in
        accu
        |> (fun accu -> List.fold_left for_asset_item accu a.values)
        |> (fun accu -> List.fold_left for_label_term accu a.invariants)
        |> (fun accu -> List.fold_left for_mterm accu a.init)
      in
      let for_record accu (r : record) =
        let for_record_field accu (rf : record_field) =
          for_type accu rf.type_
        in
        List.fold_left for_record_field accu r.fields;
      in
      match d with
      | Dvar v      -> for_var    accu v |> for_decl d
      | Denum e     -> for_enum   accu e |> for_decl d
      | Dasset a    -> for_asset  accu a |> for_decl d
      | Drecord r   -> for_record accu r |> for_decl d
      | Devent e    -> for_record accu e |> for_decl d
    in
    let for_storage_item accu (si : storage_item) =
      accu
      |> (fun accu -> for_type accu si.typ;)
      |> (fun accu -> for_mterm accu si.default;)
    in
    let for_specification accu (spec : specification) =
      let for_predicate accu (p : predicate) =
        accu
        |> (fun accu -> List.fold_left (fun accu (_, t) -> for_type accu t) accu p.args)
        |> (fun accu -> for_mterm accu p.body)
      in
      let for_definition accu (d : definition) =
        accu
        |> (fun accu -> for_type accu d.typ)
        |> (fun accu -> for_mterm accu d.body)
      in
      let for_variable accu (v : variable) =
        let for_argument accu (arg : argument) =
          let _, b, c = arg in
          accu
          |> (fun accu -> for_type accu b)
          |> (fun accu -> Option.map_dfl (for_mterm accu) accu c)
        in
        accu
        |> (fun accu -> for_argument accu v.decl)
      in
      let for_invariant accu (i : invariant) =
        List.fold_left for_mterm accu i.formulas
      in
      let for_postcondition accu (p : postcondition) =
        accu
        |> (fun accu -> for_mterm accu p.formula)
        |> (fun accu -> List.fold_left for_invariant accu p.invariants)
      in
      accu
      |> (fun accu -> List.fold_left for_predicate  accu spec.predicates)
      |> (fun accu -> List.fold_left for_definition accu spec.definitions)
      |> (fun accu -> List.fold_left for_label_term accu spec.lemmas)
      |> (fun accu -> List.fold_left for_label_term accu spec.theorems)
      |> (fun accu -> List.fold_left for_variable   accu spec.variables)
      |> (fun accu -> List.fold_left (fun accu (_, xs) -> List.fold_left for_label_term accu xs) accu spec.invariants)
      |> (fun accu -> List.fold_left for_mterm           accu spec.effects)
      |> (fun accu -> List.fold_left for_postcondition   accu spec.postconditions)
    in
    let for_function__ accu (f__ : function__) =
      let for_function_node accu (fn : function_node) =
        let for_function_struct accu (fs : function_struct) =
          let for_argument accu (arg : argument) =
            let _, b, c = arg in
            accu
            |> (fun accu -> for_type accu b)
            |> (fun accu -> Option.map_dfl (for_mterm accu) accu c)
          in
          accu
          |> (fun accu -> List.fold_left for_argument accu fs.args)
          |> (fun accu -> for_mterm accu fs.body)
        in
        let fs, t =
          match fn with
          | Function (fs, t) -> fs, Some t
          | Getter (fs, t) -> fs, Some t
          | View (fs, t) -> fs, Some t
          | Entry fs -> fs, None
        in
        accu
        |> (fun accu -> for_function_struct accu fs)
        |> (fun accu -> Option.map_dfl (for_type accu) accu t)

      in
      accu
      |> (fun accu -> for_function_node                       accu f__.node)
      |> (fun accu -> Option.map_dfl (for_specification accu) accu f__.spec)
    in
    []
    |> (fun accu -> List.fold_left for_decl_node    accu model.decls)
    |> (fun accu -> List.fold_left for_storage_item accu model.storage)
    |> (fun accu -> List.fold_left for_function__   accu model.functions)
    |> (fun accu -> for_specification               accu model.specification)

  let get_all_gen_mterm_type_internal for_mterm for_type (model : model) =
    get_all_gen_mterm_type for_mterm for_type (fun _ a -> a) model

  let get_all_type_for_mterm for_type (accu : 'a) (mt : mterm) : 'a =
    let rec aux accu (mt : mterm) =
      let accu = for_type accu mt.type_ in
      match mt.node with
      | Mletin (_, x, ot, b, o) -> begin
          accu
          |> (fun accu -> aux accu x)
          |> (fun accu -> Option.map_dfl (for_type accu) accu ot)
          |> (fun accu -> aux accu b)
          |> (fun accu -> Option.map_dfl (aux accu) accu o)
        end
      | Mdeclvar (_, Some t, v, _) -> begin
          accu
          |> (fun accu -> for_type accu t)
          |> (fun accu -> aux accu v)
        end
      | _ -> fold_term aux accu mt
    in
    aux accu mt

  let get_all_gen_type for_type (model : model) =
    get_all_gen_mterm_type_internal (get_all_type_for_mterm for_type) for_type model

  let add_type (l : type_ list) (x : type_) =
    if List.exists (cmp_type x) l
    then l
    else l @ [x]

  let get_all_set_types (model : model) : type_ list =
    let rec for_type accu t =
      match get_ntype t with
      | Tset _                   -> add_type accu t
      | Tlist   t                -> for_type accu t
      | Toption t                -> for_type accu t
      | Ttuple  ts               -> List.fold_left (for_type) accu ts
      | Tmap (_, t)              -> for_type accu t
      | Tbig_map (_, t)          -> for_type accu t
      | Titerable_big_map (_, t) -> for_type accu t
      | Tcontract t              -> for_type accu t
      | Tticket t                -> for_type accu t
      | Tprog t                  -> for_type accu t
      | Tvset (_, t)             -> for_type accu t
      | _ -> accu
    in
    get_all_gen_type for_type model

  let get_all_list_types (model : model) : type_ list =
    let rec for_type accu t =
      match get_ntype t with
      | Tlist   tv               -> add_type (for_type accu tv) t
      | Toption t                -> for_type accu t
      | Ttuple  ts               -> List.fold_left (for_type) accu ts
      | Tmap (_, t)              -> for_type accu t
      | Tbig_map (_, t)          -> for_type accu t
      | Titerable_big_map (_, t) -> for_type accu t
      | Tcontract t              -> for_type accu t
      | Tticket t                -> for_type accu t
      | Tprog t                  -> for_type accu t
      | Tvset (_, t)             -> for_type accu t
      | _ -> accu
    in
    get_all_gen_type for_type model

  let get_all_map_types (model : model) : type_ list =
    let rec for_type accu t =
      match get_ntype t with
      | Tlist     t               -> for_type accu t
      | Toption   t               -> for_type accu t
      | Ttuple    ts              -> List.fold_left (for_type) accu ts
      | Tmap      (_, tv)         -> add_type (for_type accu tv) t
      | Tbig_map  (_, tv)         -> add_type (for_type accu tv) t
      | Titerable_big_map (_, tv) -> add_type (for_type accu tv) t
      | Tcontract t               -> for_type accu t
      | Tticket t                 -> for_type accu t
      | Tprog     t               -> for_type accu t
      | Tvset     (_, t)          -> for_type accu t
      | _ -> accu
    in
    get_all_gen_type for_type model

  let get_all_fail_types (model : model) : type_ list =
    let for_type accu _ = accu in

    let for_mterm (accu : 'a) (mt : mterm) : 'a =
      let rec aux accu (mt : mterm) =
        let accu = for_type accu mt.type_ in
        match mt.node with
        | Mfail (Invalid e) -> let t = e.type_ in add_type accu t
        | _ -> fold_term aux accu mt
      in
      aux accu mt
    in
    get_all_gen_mterm_type_internal for_mterm for_type model

  let extract_key_value_from_masset (model : model) (v : mterm) : mterm =
    match v with
    | {node = (Masset l); type_ = (Tasset an, _) } ->
      let an = unloc an in
      let asset : asset = get_asset model an in
      let asset_key = match asset.keys with [k] -> k | _ -> emit_error (SeveralAssetKeys an) in
      let assoc_fields = List.map2 (fun (ai : asset_item) (x : mterm) -> (unloc ai.name, x)) asset.values l in
      List.find (fun (id, _) -> (String.equal asset_key id)) assoc_fields |> snd
    | _ -> raise Not_found

  let is_not_string_nat_int t = (match get_ntype t with | Tbuiltin (Bstring | Bnat | Bint) -> false | _ -> true)

  let get_function (model : model) (id : ident) : function_struct =
    model.functions
    |> List.map (fun x -> match x.node with | Function (fs, _) | Getter (fs, _) | View (fs, _) | Entry fs -> fs)
    |> List.find (fun (x : function_struct) -> String.equal (unloc x.name) id)

  let get_asset_partitions (model : model) asset_name : (ident * ident) list =
    let asset = get_asset model asset_name in
    List.fold_left (fun accu (x : asset_item) ->
        match get_ntype x.original_type with
        | Tcontainer ((Tasset an, _), Partition) -> (unloc x.name, unloc an)::accu
        | _ -> accu
      ) [] asset.values

  let get_specifications (model : model) =
    [ model.specification ] @
    (List.fold_left (fun acc (s,_) -> match s with Some v -> acc@[v] | None -> acc) [] (get_entries model)) @
    ((List.fold_left (fun acc (s,_,_) -> match s with Some v -> acc@[v] | None -> acc)) [] (get_functions model))

  let get_specification (model : model) (name : ident) =
    let rec get_entry_spec = function
      | (s, (f:function_struct))::_ when String.compare (unloc f.name) name = 0 -> s
      | _::tl -> get_entry_spec tl
      | [] -> None in
    let rec get_function_spec = function
      | (s, (f:function_struct),_)::_ when String.compare (unloc f.name) name = 0 -> s
      | _::tl -> get_function_spec tl
      | [] -> None in
    match get_entry_spec (get_entries model) with
    | Some s -> Some s
    | None -> get_function_spec (get_functions model)

  let get_fss (model : model) : function_struct list =
    List.map (fun (x) -> match x.node with | Entry fs | Getter (fs, _) | View (fs, _) | Function (fs, _) -> fs) model.functions

  let get_fs (model : model) (id : ident) : function_struct =
    List.find (fun (x : function_struct) -> String.equal id (unloc x.name)) (get_fss model)

  let extract_assign_kind (mt : mterm) : assign_kind list =
    let rec aux accu (t : mterm) =
      match t.node with
      | Massign (_, _, ak, _) -> ak::accu
      | _ -> fold_term aux accu t in
    aux [] mt

  let extract_asset_effect (model : model) (mt : mterm) : effect list =
    let only_partition accu an fn p =
      let aan, c = get_field_container model an fn in
      match c with
      | Partition -> begin match p with
          | `Added -> (Eadded aan)::accu
          | `Removed -> (Eremoved aan)::accu
        end
      | _ -> accu
    in
    let with_partition accu an fn m p =
      let accu = begin match m with
        | `Updated -> (Eupdated an)::accu
      end in
      only_partition accu an fn p
    in
    let all_partition accu an m p =
      let accu = begin match m with
        | `Updated -> (Eupdated an)::accu
        | `Removed -> (Eremoved an)::accu
        | `Added   -> (Eadded an)::accu
      end in
      let parts = get_asset_partitions model an in
      List.fold_left (fun accu (aan, afn) -> only_partition accu aan afn p) accu parts
    in
    let rec aux accu (t : mterm) =
      match t.node with
      | Maddasset (an, _)                                 -> (Eadded an)::accu
      | Mputsingleasset (an, _)                           -> (Eadded an)::accu
      | Mputasset (an, _, _)                              -> (Eadded an)::accu
      | Maddfield (an, fn, _, _)                          -> with_partition accu an fn `Updated `Added
      | Mremoveasset (an, _)                              -> (Eremoved an)::accu
      | Mremovefield (an, fn, _, _)                       -> with_partition accu an fn `Updated `Removed
      | Mremoveall (an, fn, _)                            -> with_partition accu an fn `Updated `Removed
      | Mremoveif (an, CKcoll _, _, _, _)                 -> all_partition accu an `Removed `Removed
      | Mremoveif (an, CKfield (_, fn, _, _, _), _, _, _) -> with_partition accu an fn `Updated `Removed
      | Mclear (an, CKcoll _)                             -> all_partition accu an `Removed `Removed
      | Mclear (an, CKview _)                             -> all_partition accu an `Removed `Removed
      | Mclear (an, CKfield (_, fn, _, _, _))             -> with_partition accu an fn `Updated `Removed
      | Mset (an, _, _, _)                                -> (Eupdated an)::accu
      | _ -> fold_term aux accu t in
    aux [] mt

  let extract_var_idents (model : model) (mt : mterm) : ident list =
    let add_expr_asset an ck accu =
      let aan =
        match ck with
        | CKcoll _
        | CKview _
        | CKdef _ -> an
        | CKfield (an, fn, _, _, _) -> get_field_container model an fn |> fst
      in
      aan::accu
    in

    let rec aux env accu (t : mterm) =
      match t.node with
      | Mletin (ids, a, _, b, o) ->
        let f = aux (env @ (List.map unloc ids)) in
        let tmp = f (f accu a) b in
        Option.map_dfl (f tmp) tmp o
      | Mforall (id, _, c, b) ->
        let f = aux (env @ [unloc id]) in
        f (Option.fold f accu c) b
      | Mvar (id, Vlocal, _, _)  when not (List.exists (String.equal (unloc id)) env) -> (unloc id)::accu
      | Mvar (id, Vstorevar, _, _) -> (unloc id)::accu
      | Mvar (id, Vstorecol, _, _) -> (unloc id)::accu
      | Mvar (_,  Vstate, _, _)    -> "state"::accu
      | Mnow                       -> "now"::accu
      | Mtransferred               -> "transferred"::accu
      | Mcaller                    -> "caller"::accu
      | Mbalance                   -> "balance"::accu
      | Msource                    -> "source"::accu
      | Mselfaddress               -> "selfaddress"::accu
      | Mchainid                   -> "chainid"::accu
      | Mmetadata                  -> "metadata"::accu
      | Mlevel                     -> "level"::accu

      | Mget (an, ck, _)
      | Mselect (an, ck, _, _, _)
      | Msort (an, ck,_)
      | Mcontains (an, ck, _)
      | Mnth (an, ck, _)
      | Mcount (an, ck)
      | Msum (an, ck, _)
      | Mhead (an, ck, _)
      | Mtail (an, ck, _)          -> add_expr_asset an ck accu

      | _ -> fold_term (aux env) accu t in
    aux [] [] mt
    |> Tools.List.dedup

  exception Found of (int * int) list

  let get_record_pos model rn fn =
    let r : record = get_record model rn in
    let fields_length = List.length r.fields in
    let fields_index  = List.index_of (fun (f : record_field) -> String.equal fn (unloc f.name)) r.fields in
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
