(* -------------------------------------------------------------------- *)
open Ident
open Location

(* -------------------------------------------------------------------- *)

let pp_lident fmt i = Format.fprintf fmt "%s" (unloc i)

type lident = ident loced
[@@deriving yojson, show {with_path = false}]

and id_scope =
  | SINone
  | SIParent
  | SIId of lident

(* -------------------------------------------------------------------- *)
and container =
  | Aggregate
  | Partition
  | AssetContainer
  | AssetKey
  | AssetValue
  | AssetView

and type_r =
  | Tref                 of (id_scope * lident)
  | Tcontainer           of type_t * container
  | Ttuple               of type_t list
  | Toption              of type_t
  | Tset                 of type_t
  | Tlist                of type_t
  | Tmap                 of type_t * type_t
  | Tbig_map             of type_t * type_t
  | Titerable_big_map    of type_t * type_t
  | Tor                  of type_t * type_t
  | Tlambda              of type_t * type_t
  | Tcontract            of type_t
  | Tticket              of type_t
  | Tsapling_state       of Core.big_int
  | Tsapling_transaction of Core.big_int

and type_t = type_r loced * lident option

(* -------------------------------------------------------------------- *)
and logical_operator =
  | And
  | Or
  | Xor

and comparison_operator =
  | Equal
  | Nequal
  | Gt
  | Ge
  | Lt
  | Le

and arithmetic_operator =
  | Plus
  | Minus
  | Mult
  | DivRat
  | DivEuc
  | Modulo
  | DivMod
  | ThreeWayCmp
  | ShiftLeft
  | ShiftRight

and unary_operator =
  | Uminus
  | Not

and assignment_operator =
  | ValueAssign
  | PlusAssign
  | MinusAssign
  | MultAssign
  | DivAssign
  | AndAssign
  | OrAssign

and operator =
  | Logical of logical_operator
  | Cmp     of comparison_operator
  | Arith   of arithmetic_operator
  | Unary   of unary_operator

type pattern_unloc =
  | Pwild
  | Pref of pname loced * lident list

and pname =
  | PIdent of ident
  | PCons
  | PNil
  | PSome
  | PNone
  | PLeft
  | PRight

and pattern = pattern_unloc loced

and for_ident_unloc = FIsimple of lident | FIdouble of lident * lident
and for_ident = for_ident_unloc loced

and transfer_t =
  | TTsimple    of expr * expr
  | TTcontract  of expr * expr * lident * type_t * expr
  | TTentry     of expr * lident * expr
  | TTentry2    of expr * lident * expr * lident * expr
  | TTself      of expr * lident * expr list
  | TToperation of expr

and micheline_t =
  | MIstring of string
  | MIint    of Core.big_int
  | MIbytes  of string
  | MIprim   of string * micheline_t list * string list
  | MIseq    of micheline_t list

and method_kind =
  | MKexpr of expr
  | MKself

and match_kind =
  | MKbasic
  | MKdetach

and var_decl_kind =
  | VDKbasic
  | VDKoption of expr option

and expr_unloc =
  | Eterm          of (id_scope * lident)
  | Eliteral       of literal
  | Earray         of id_scope * expr list
  | Erecord        of id_scope * record_item list
  | Etuple         of expr list
  | Edot           of expr * (id_scope * lident)
  | Equestiondot   of expr * (id_scope * lident)
  | Esqapp         of expr * expr
  | Emulticomp     of expr * (comparison_operator loced * expr) list
  | Eapp           of function_ * expr list
  | Eappt          of function_ * type_t list * expr list
  | Emethod        of method_kind * lident * expr list
  | Etransfer      of transfer_t
  | Edetach        of lident * expr * expr
  | Edorequire     of expr * expr
  | Edofailif      of expr * expr
  | Efail          of expr
  | Efailsome      of expr
  | Eassign        of assignment_operator * expr * expr
  | Eassignopt     of expr * expr * expr
  | Eif            of expr * expr * expr option
  | Efor           of for_ident * expr * expr
  | Eiter          of lident * expr option * expr * expr
  | Ewhile         of expr * expr
  | Eseq           of expr * expr
  | Eletin         of lident * type_t option * expr * expr * expr option
  | Evar           of lident list * type_t option * expr * var_decl_kind * bool (* const or not *)
  | Ematchwith     of expr * branch list * match_kind
  | Efold          of expr * lident * expr
  | Emap           of expr * lident * expr
  | Erecupdate     of expr * (lident * expr) list
  | Ereturn        of expr
  | Eoption        of option_
  | Eor            of or_
  | Elambda        of type_t option * lident * type_t option * expr
  | Elambda_michelson of type_t * type_t * micheline_t
  | Eentrypoint    of type_t * expr * expr * expr option
  | Ecallview      of type_t * expr * expr * expr
  | Eunpack        of type_t * expr
  | Eemit          of type_t * expr
  | Eself          of lident
  | Eternary       of expr * expr * expr
  | Eany
  | Enothing
  | Eunit
  | Etz_expr       of string
  | Emicheline     of micheline_t
  | Emicheline_expr of type_t * micheline_t * expr list
  | Einvalid

and branch = (pattern list * expr)

and scope =
  | Added
  | After
  | Before
  | Fixed
  | Removed
  | Stable

and option_ =
  | OSome of expr
  | ONone of type_t option

and or_ =
  | Oleft  of type_t option * type_t * expr
  | Oright of type_t * type_t option * expr

and function_ =
  | Fident of (id_scope * lident)
  | Foperator of operator loced

and literal =
  | Lint            of Core.big_int
  | Lnat            of Core.big_int
  | Ldecimal        of string
  | Ltz             of string
  | Lmtz            of string
  | Lutz            of string
  | Laddress        of string
  | Lstring         of string
  | Lbool           of bool
  | Lduration       of string
  | Ldate           of string
  | Lbytes          of string
  | Lpercent        of string
  | LnumberFr       of Core.big_int
  | LbytesFr        of string
  | LbytesG1        of string
  | LbytesG2        of string

and record_item = (assignment_operator * lident) option * expr

and expr = expr_unloc loced

and lident_typ = lident * type_t

(* -------------------------------------------------------------------- *)
and field_unloc =
  | Ffield of lident * type_t * expr option (** field *)

and field = field_unloc loced

and args = lident_typ list

and view_visibility =
  | VVonchain
  | VVoffchain
  | VVonoffchain
  | VVnone

and s_function = {
  name  : lident;
  args  : args;
  ret_t : type_t option;
  body  : expr;
  view  : bool;
  view_visibility: view_visibility;
}

and entry_properties = {
  accept_transfer : bool * expr option;
  sourcedby       : (expr * expr option) option;
  calledby        : (expr * expr option) option;
  state_is        : (lident * expr option) option;
  constants       : ((lident * expr * expr option) list) option;
  require         : ((lident * expr * expr option) list) option;
  failif          : ((lident * expr * expr option) list) option;
  functions       : (s_function loced) list;
}

and transition = (lident * expr option * expr option) list

and parameter = (lident * type_t * expr option * bool) loced

and parameters = parameter list loced option

and metadata =
  | Muri  of string loced
  | Mjson of string loced

(* -------------------------------------------------------------------- *)
and variable_kind =
  | VKvariable
  | VKconstant

and enum_kind =
  | EKenum of lident
  | EKstate

(* -------------------------------------------------------------------- *)
and declaration_unloc =
  | Darchetype     of lident * parameters * metadata option
  | Dimport        of lident option * lident
  | Dvariable      of variable_decl
  | Denum          of enum_kind * enum_decl
  | Dasset         of asset_decl
  | Drecord        of record_decl
  | Dentry         of entry_decl
  | Dgetter        of getter_decl
  | Dtransition    of transition_decl
  | Dnamespace     of namespace_decl
  | Dfunction      of s_function
  | Dtype          of lident * type_t
  | Devent         of record_decl
  | Dinvalid

and variable_decl =
  lident
  * type_t
  * expr option
  * variable_kind

and enum_decl =
  (lident * type_t list * enum_option list) list

and asset_decl =
  lident
  * field list
  * asset_option list
  * asset_post_option list
  * asset_operation option

and record_decl =
  lident
  * field list
  * expr option

and entry_decl =
  lident
  * args
  * entry_properties
  * expr option

and getter_decl = {
  name  : lident;
  args  : args;
  ret_t : type_t;
  entry_properties : entry_properties;
  body  : expr;
}

and transition_decl =
  lident
  * args
  * expr
  * entry_properties
  * transition

and namespace_decl =
  lident * declaration list

and map_kind =
  | MKMap
  | MKBigMap
  | MKIterableBigMap

and asset_option =
  | AOidentifiedby of lident list
  | AOsortedby of lident
  | AOtoMapKind of map_kind

and asset_post_option =
  | APOinit of expr list

and enum_option =
  | EOinitial

and declaration = declaration_unloc loced

and asset_operation_enum =
  | AOadd
  | AOremove
  | AOupdate

and asset_operation =
  | AssetOperation of asset_operation_enum list * expr option

(* -------------------------------------------------------------------- *)
and archetype_unloc = declaration list

and archetype = archetype_unloc loced
[@@deriving yojson, show {with_path = false},
            visitors { variety = "map"; ancestors = ["location_map"; "ident_map"] },
            visitors { variety = "iter"; ancestors = ["location_iter"; "ident_iter"] },
            visitors { variety = "reduce"; ancestors = ["location_reduce"; "ident_reduce"] },
            visitors { variety = "reduce2"; ancestors = ["location_reduce2"; "ident_reduce2"] }
    ]

(* -------------------------------------------------------------------- *)

(* types *)

let tref ?(loc=dummy) ?a vt : type_t = (mkloc loc (Tref (SINone, mkloc loc vt))), a

let tunit         = tref "unit"
let tstring       = tref "string"
let tnat          = tref "nat"
let tint          = tref "int"
let trational     = tref "rational"
let tbool         = tref "bool"
let trole         = tref "role"
let taddress      = tref "address"
let tdate         = tref "date"
let ttez          = tref "tez"
let tduration     = tref "duration"
let tsignature    = tref "signature"
let tkey          = tref "key"
let tkey_hash     = tref "key_hash"
let tbytes        = tref "bytes"
let tchain_id     = tref "chain_id"
let toperation    = tref "operation"
let toperation    = tref "operation"
let tbls12_381_fr = tref "bls12_381_fr"
let tbls12_381_g1 = tref "bls12_381_g1"
let tbls12_381_g2 = tref "bls12_381_g2"
let tnever        = tref "never"
let tchest        = tref "chest"
let tchest_key    = tref "chest_key"

let mk_tcontainer ?(loc=dummy) ?a t c : type_t =
  mkloc loc (Tcontainer (t, c)), a

let mk_ttuple ?(loc=dummy) ?a l : type_t =
  mkloc loc (Ttuple l), a

let mk_toption ?(loc=dummy) ?a t : type_t =
  mkloc loc (Toption t), a

let mk_tset ?(loc=dummy) ?a t : type_t =
  mkloc loc (Tset t), a

let mk_tlist ?(loc=dummy) ?a t : type_t =
  mkloc loc (Tlist t), a

let mk_tmap ?(loc=dummy) ?a k v : type_t =
  mkloc loc (Tmap (k, v)), a

let mk_tbig_map ?(loc=dummy) ?a k v : type_t =
  mkloc loc (Tbig_map (k, v)), a

let mk_titerable_big_map ?(loc=dummy) ?a k v : type_t =
  mkloc loc (Titerable_big_map (k, v)), a

let mk_tor ?(loc=dummy) ?a k v : type_t =
  mkloc loc (Tor (k, v)), a

let mk_tcontract ?(loc=dummy) ?a t : type_t =
  mkloc loc (Tcontract t), a

let mk_tticket ?(loc=dummy) ?a t : type_t =
  mkloc loc (Tticket t), a

let mk_sapling_state ?(loc=dummy) ?a n : type_t =
  mkloc loc (Tsapling_state n), a

let mk_sapling_transaction ?(loc=dummy) ?a n : type_t =
  mkloc loc (Tsapling_transaction n), a


(* expressions *)

let mk_eliteral ?(loc=dummy) l =
  mkloc loc (Eliteral l)

let ebint n = mk_eliteral (Lint n)
let eint  n = ebint (Big_int.big_int_of_int n)

let ebnat n = mk_eliteral (Lnat n)
let enat  n = ebnat (Big_int.big_int_of_int n)

let ebtz n = mk_eliteral (Ltz n)

let emtz n = mk_eliteral (Lmtz n)

let eutz n = mk_eliteral (Lutz n)

let epercent n = mk_eliteral (Lpercent n)

let etrue = mk_eliteral (Lbool true)
let efalse = mk_eliteral (Lbool false)

let eint      v = mk_eliteral (Lint      v)
let enat      v = mk_eliteral (Lnat      v)
let edecimal  v = mk_eliteral (Ldecimal  v)
let etz       v = mk_eliteral (Ltz       v)
let emtz      v = mk_eliteral (Lmtz      v)
let eutz      v = mk_eliteral (Lutz      v)
let eaddress  v = mk_eliteral (Laddress  v)
let estring   v = mk_eliteral (Lstring   v)
let ebool     v = mk_eliteral (Lbool     v)
let eduration v = mk_eliteral (Lduration v)
let edate     v = mk_eliteral (Ldate     v)
let ebytes    v = mk_eliteral (Lbytes    v)
let epercent  v = mk_eliteral (Lpercent  v)
let enumberFr v = mk_eliteral (LnumberFr v)
let ebytesFr  v = mk_eliteral (LbytesFr  v)
let ebytesG1  v = mk_eliteral (LbytesG1  v)
let ebytesG2  v = mk_eliteral (LbytesG2  v)

let eterm         ?(loc=dummy) ?(s=SINone) id     = mkloc loc (Eterm (s, id))
let earray        ?(loc=dummy) ?(s=SINone) l      = mkloc loc (Earray (s, l))
let erecord       ?(loc=dummy) ?(s=SINone) rl     = mkloc loc (Erecord (s, rl))
let etuple        ?(loc=dummy) l                  = mkloc loc (Etuple l)
let edot          ?(loc=dummy) e id               = mkloc loc (Edot (e, id))
let esqapp        ?(loc=dummy) e i                = mkloc loc (Esqapp (e, i))
let emulticomp    ?(loc=dummy) e l                = mkloc loc (Emulticomp(e, l))
let eapp          ?(loc=dummy) f e                = mkloc loc (Eapp(f, e))
let emethod       ?(loc=dummy) e id args          = mkloc loc (Emethod(e, id, args))
let etransfer     ?(loc=dummy) t                  = mkloc loc (Etransfer t)
let edorequire    ?(loc=dummy) e f                = mkloc loc (Edorequire(e, f))
let edofailif     ?(loc=dummy) e f                = mkloc loc (Edofailif (e, f))
let efail         ?(loc=dummy) e                  = mkloc loc (Efail e)
let eassign       ?(loc=dummy) op e v             = mkloc loc (Eassign(op, e, v))
let eassignopt    ?(loc=dummy) e v f              = mkloc loc (Eassignopt(e, v, f))
let eif           ?(loc=dummy) ?e c t             = mkloc loc (Eif(c, t, e))
let efor          ?(loc=dummy) id c b             = mkloc loc (Efor(id, c, b))
let eiter         ?(loc=dummy) ?min id max e      = mkloc loc (Eiter(id, min, max, e))
let ewhile        ?(loc=dummy) c b                = mkloc loc (Ewhile(c, b))
let eseq          ?(loc=dummy) e1 e2              = mkloc loc (Eseq(e1, e2))
let eletin        ?(loc=dummy) ?t ?o id v b       = mkloc loc (Eletin(id, t, v, b, o))
let evar          ?(loc=dummy) ?t id e k c        = mkloc loc (Evar(id, t, e, k, c))
let ematchwith    ?(loc=dummy) e l k              = mkloc loc (Ematchwith(e, l, k))
let erecupdate    ?(loc=dummy) e l                = mkloc loc (Erecupdate(e, l))
let ereturn       ?(loc=dummy) e                  = mkloc loc (Ereturn e)
let eoption       ?(loc=dummy) e                  = mkloc loc (Eoption e)
let eleft         ?(loc=dummy) t e                = mkloc loc (Eor (Oleft (None, t, e)))
let eright        ?(loc=dummy) t e                = mkloc loc (Eor (Oright (t, None, e)))
let eentrypoint   ?(loc=dummy) t e v b            = mkloc loc (Eentrypoint (t, e, v, b))
let eunpack       ?(loc=dummy) t e                = mkloc loc (Eunpack (t, e))
let eself         ?(loc=dummy) id                 = mkloc loc (Eself id)
let eany          ?(loc=dummy) _                  = mkloc loc (Eany)
let enothing      ?(loc=dummy) _                  = mkloc loc (Enothing)
let einvalid      ?(loc=dummy) _                  = mkloc loc (Einvalid)
let etz_expr      ?(loc=dummy) v                  = mkloc loc (Etz_expr v)
(* declarations utils *)

let mk_s_function name args ret_t body view view_visibility : s_function =
  {name; args; ret_t; body; view; view_visibility}

let mk_entry_properties ?(accept_transfer = (true, None)) ?sourcedby ?calledby ?state_is ?constants ?require ?failif ?(functions = []) _ : entry_properties =
  { accept_transfer; sourcedby; calledby; state_is; constants; require; failif; functions }

let mk_transition_item id eexto eexfrom : lident * expr option * expr option = id, eexto, eexfrom

let mk_variable_decl ?dv id t vk : variable_decl = id, t, dv, vk

let mk_enum_decl l : enum_decl = l

let mk_asset_decl ?(fs=[]) ?(aos=[]) ?(apos=[]) ?ao id : asset_decl = id, fs, aos, apos, ao

let mk_record_decl ?(fs=[]) ?pos id : record_decl = id, fs, pos

let mk_entry_decl ?(args=[]) ?body id ep : entry_decl = id, args, ep, body

let mk_transition_decl ?(args=[]) ?(trs=[]) id body ep : transition_decl = id, args, body, ep, trs

let mk_namespace_decl ?(ds=[]) id : namespace_decl = id, ds

let mk_asset_option_identifiedby ids = AOidentifiedby ids
let mk_asset_option_sortedby id      = AOsortedby id
let mk_asset_option_to_map_kind x    = AOtoMapKind x

let mk_asset_post_option_init l         = APOinit l

let mk_enum_option_initial _        = EOinitial

let mk_assetoperation aoes e : asset_operation = AssetOperation (aoes, e)



(* declarations *)

let mk_darchetype ?parameters ?metadata ?(loc=dummy) id =
  mkloc loc (Darchetype (id, parameters, metadata))

let mk_variable ?(loc=dummy) vd =
  mkloc loc (Dvariable vd)

let mk_enum ?(loc=dummy) ek ed =
  mkloc loc (Denum (ek, ed))

let mk_asset ?(loc=dummy) ad =
  mkloc loc (Dasset ad)

let mk_record ?(loc=dummy) rd =
  mkloc loc (Drecord rd)

let mk_event ?(loc=dummy) rd =
  mkloc loc (Devent rd)

let mk_entry ?(loc=dummy) ed =
  mkloc loc (Dentry ed)

let mk_transition ?(loc=dummy) td =
  mkloc loc (Dtransition td)

let mk_namespace ?(loc=dummy) nd =
  mkloc loc (Dnamespace nd)

let mk_function ?(loc=dummy) sf =
  mkloc loc (Dfunction sf)

let mk_dtype ?(loc=dummy) id t =
  mkloc loc (Dtype (id, t))

let mk_invalid ?(loc=dummy) () =
  mkloc loc Dinvalid

let mk_archetype ?(decls=[]) ?(loc=dummy) () = mkloc loc decls


(* -------------------------------------------------------------------- *)

let cst_now            = "now"
let cst_transferred    = "transferred"
let cst_caller         = "caller"
let cst_balance        = "balance"
let cst_source         = "source"
let cst_self_address   = "self_address"
let cst_self_chain_id  = "self_chain_id"
let cst_metadata       = "metadata"
let cst_level          = "level"
let cst_min_block_time = "min_block_time"


(* utils *)

let get_name = function
  | Darchetype  _                   -> "archetype"
  | Dimport (Some id, _)            -> unloc id
  | Dimport (None, _)               -> "_import"
  | Dvariable (id, _, _, _)         -> unloc id
  | Denum (EKenum id, _)            -> unloc id
  | Denum (EKstate, _)              -> "_state"
  | Dasset (id, _, _, _, _)         -> unloc id
  | Drecord (id, _, _)              -> unloc id
  | Devent  (id, _, _)              -> unloc id
  | Dentry (id, _, _, _)            -> unloc id
  | Dgetter {name; _}               -> unloc name
  | Dtransition (id, _, _, _, _) -> unloc id
  | Dnamespace (id, _)              -> unloc id
  | Dfunction fs                    -> unloc fs.name
  | Dtype  (id, _)                  -> unloc id
  | Dinvalid                        -> ""
