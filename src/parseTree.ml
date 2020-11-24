(* -------------------------------------------------------------------- *)
open Ident
open Location

(* -------------------------------------------------------------------- *)

let pp_lident fmt i = Format.fprintf fmt "%s" (unloc i)

type lident = ident loced
[@@deriving yojson, show {with_path = false}]

(* -------------------------------------------------------------------- *)
and container =
  | Aggregate
  | Partition
  | View

and type_r =
  | Tref       of lident
  | Tcontainer of type_t * container
  | Ttuple     of type_t list
  | Toption    of type_t
  | Tset       of type_t
  | Tlist      of type_t
  | Tmap       of type_t * type_t
  | Tbig_map   of type_t * type_t
  | Tor        of type_t * type_t
  | Tlambda    of type_t * type_t
  | Tcontract  of type_t
  | Tkeyof     of type_t

and type_t = type_r loced * lident option

(* -------------------------------------------------------------------- *)
and logical_operator =
  | And
  | Or
  | Xor
  | Imply
  | Equiv

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

and quantifier =
  | Forall
  | Exists

and operator =
  | Logical of logical_operator
  | Cmp     of comparison_operator
  | Arith   of arithmetic_operator
  | Unary   of unary_operator

type pattern_unloc =
  | Pwild
  | Pref of lident

and pattern = pattern_unloc loced

and var_label = VLBefore | VLIdent of lident

and var_vset  = VSAdded | VSUnmoved | VSRemoved

and for_ident_unloc = FIsimple of lident | FIdouble of lident * lident
and for_ident = for_ident_unloc loced

and transfer_t =
  | TTsimple    of expr * expr
  | TTcontract  of expr * expr * lident * type_t * expr
  | TTentry     of expr * lident * expr
  | TTself      of expr * lident * expr list
  | TToperation of expr

and expr_unloc =
  | Eterm          of (var_vset option * var_label option) * lident
  | Eliteral       of literal
  | Earray         of expr list
  | Erecord        of record_item list
  | Etuple         of expr list
  | Edot           of expr * lident
  | Esqapp         of expr * expr
  | Emulticomp     of expr * (comparison_operator loced * expr) list
  | Eapp           of function_ * expr list
  | Emethod        of expr * lident * expr list
  | Etransfer      of transfer_t
  | Edorequire     of expr * expr
  | Edofailif      of expr * expr
  | Efail          of expr
  | Eassign        of assignment_operator * expr * expr
  | Eif            of expr * expr * expr option
  | Efor           of lident option * for_ident * expr * expr
  | Eiter          of lident option * lident * expr option * expr * expr
  | Ewhile         of lident option * expr * expr
  | Eseq           of expr * expr
  | Eletin         of lident * type_t option * expr * expr * expr option
  | Evar           of lident * type_t option * expr
  | Ematchwith     of expr * branch list
  | Ematchoption   of expr * lident * expr * expr
  | Ematchor       of expr * lident * expr * lident * expr
  | Ematchlist     of expr * lident * lident * expr * expr
  | Eloopleft      of expr * lident * expr
  | Emap           of expr * lident * expr
  | Erecupdate     of expr * (lident * expr) list
  | Equantifier    of quantifier * lident * quantifier_kind * expr
  | Eassert        of lident
  | Elabel         of lident
  | Ereturn        of expr
  | Eoption        of option_
  | Eor            of or_
  | Elambda        of type_t option * lident * type_t option * expr
  | Eentrypoint    of type_t * expr * expr
  | Eunpack        of type_t * expr
  | Eself          of lident
  | Eany
  | Enothing
  | Einvalid

and branch = (pattern list * expr)

and scope =
  | Added
  | After
  | Before
  | Fixed
  | Removed
  | Stable

and quantifier_kind =
  | Qcollection of expr
  | Qtype of type_t

and option_ =
  | OSome of expr
  | ONone of type_t option

and or_ =
  | Oleft  of type_t option * type_t * expr
  | Oright of type_t * type_t option * expr

and function_ =
  | Fident of lident
  | Foperator of operator loced

and literal =
  | Lint      of Core.big_int
  | Lnat      of Core.big_int
  | Ldecimal  of string
  | Ltz       of string
  | Lmtz      of string
  | Lutz      of string
  | Laddress  of string
  | Lstring   of string
  | Lbool     of bool
  | Lduration of string
  | Ldate     of string
  | Lbytes    of string
  | Lpercent  of string

and record_item = (assignment_operator * lident) option * expr

and expr = expr_unloc loced

and lident_typ = lident * type_t * extension list option

and label_expr = (lident * expr) loced

and label_exprs = label_expr list

(* -------------------------------------------------------------------- *)
and extension_unloc =
  | Eextension of lident * expr list (** extension *)

and extension = extension_unloc loced

and exts = extension list option

(* -------------------------------------------------------------------- *)
and field_unloc =
  | Ffield of lident * type_t * expr option * exts   (** field *)

and field = field_unloc loced

and args = lident_typ list

and invariants = (lident * expr list) list

and specification_item_unloc =
  | Vpredicate     of lident * args * expr
  | Vdefinition    of lident * type_t * lident * expr
  | Vvariable      of lident * type_t * expr option
  | Veffect        of expr
  | Vassert        of (lident * expr * invariants * lident list)
  | Vfails         of (lident * lident * type_t * expr) list
  | Vpostcondition of (lident * expr * invariants * lident list * postkind option)

and postkind = PKPost | PKInv

and specification_item = specification_item_unloc loced

and specification_unloc = specification_item list * exts

and specification = specification_unloc loced

and security_arg_unloc =
  | Sident of lident
  | Sdot   of lident * lident
  | Slist  of security_arg list
  | Sapp   of lident * security_arg list
  | Sbut   of lident * security_arg
  | Sto    of lident * security_arg

and security_arg = security_arg_unloc loced

and security_item_unloc = lident * lident * security_arg list

and security_item = security_item_unloc loced

and security_unloc = security_item list * exts

and security = security_unloc loced

and s_function = {
  name  : lident;
  args  : args;
  ret_t : type_t option;
  spec  : specification option;
  body  : expr;
  getter: bool;
}

and entry_properties = {
  accept_transfer : bool;
  calledby        : (expr * exts) option;
  require         : ((lident * expr * expr option) list * exts) option;
  failif          : ((lident * expr * expr option) list * exts) option;
  spec_fun        : specification option;
  functions       : (s_function loced) list;
}

and transition = (lident * (expr * exts) option * (expr * exts) option) list

and parameter = (lident * type_t * expr option) loced

and parameters = parameter list loced option

(* -------------------------------------------------------------------- *)
and variable_kind =
  | VKvariable
  | VKconstant

and enum_kind =
  | EKenum of lident
  | EKstate

(* -------------------------------------------------------------------- *)
and declaration_unloc =
  | Darchetype     of lident * parameters * exts
  | Dvariable      of variable_decl
  | Denum          of enum_kind * enum_decl
  | Dasset         of asset_decl
  | Drecord        of record_decl
  | Dentry         of entry_decl
  | Dtransition    of transition_decl
  | Dextension     of extension_decl
  | Dnamespace     of namespace_decl
  | Dfunction      of s_function
  | Dspecification of specification
  | Dspecasset     of (lident * label_exprs)
  | Dspecfun       of specfun
  | Dspecvariable  of (lident * label_exprs)
  | Dsecurity      of security
  | Dtype          of lident * type_t
  | Dinvalid

and specfun_kind =
  | SKentry
  | SKfunction
  | SKgetter

and specfun = specfun_kind * lident * args * specification

and variable_decl =
  lident
  * type_t
  * expr option
  * variable_kind
  * label_exprs
  * exts

and enum_decl =
  (lident * enum_option list) list * exts

and asset_decl =
  lident
  * field list
  * field list (* shadow fields *)
  * asset_option list
  * asset_post_option list
  * asset_operation option
  * exts

and record_decl =
  lident
  * field list
  * expr option
  * exts

and entry_decl =
  lident
  * args
  * entry_properties
  * (expr * exts) option
  * exts

and transition_decl =
  lident
  * args
  * (lident * type_t) option
  * expr
  * entry_properties
  * transition
  * exts

and extension_decl =
  lident * expr list

and namespace_decl =
  lident * declaration list

and asset_option =
  | AOidentifiedby of lident list
  | AOsortedby of lident
  | AOtoBigMap

and asset_post_option =
  | APOstates of lident
  | APOconstraints of label_exprs
  | APOinit of expr list

and enum_option =
  | EOinitial
  | EOspecification of label_exprs

and declaration = declaration_unloc loced

and asset_operation_enum =
  | AOadd
  | AOremove
  | AOupdate

and asset_operation =
  | AssetOperation of asset_operation_enum list * expr option

(* -------------------------------------------------------------------- *)
and archetype_unloc =
  | Marchetype of declaration list
  | Mextension of lident * declaration list * declaration list

and archetype = archetype_unloc loced
[@@deriving yojson, show {with_path = false},
            visitors { variety = "map"; ancestors = ["location_map"; "ident_map"] },
            visitors { variety = "iter"; ancestors = ["location_iter"; "ident_iter"] },
            visitors { variety = "reduce"; ancestors = ["location_reduce"; "ident_reduce"] },
            visitors { variety = "reduce2"; ancestors = ["location_reduce2"; "ident_reduce2"] }
    ]

(* -------------------------------------------------------------------- *)

(* types *)

let tref ?(loc=dummy) ?a vt : type_t = (mkloc loc (Tref (mkloc loc vt))), a

let tunit      = tref "unit"
let tstring    = tref "string"
let tnat       = tref "nat"
let tint       = tref "int"
let trational  = tref "rational"
let tbool      = tref "bool"
let trole      = tref "role"
let taddress   = tref "address"
let tdate      = tref "date"
let ttez       = tref "tez"
let tduration  = tref "duration"
let tsignature = tref "signature"
let tkey       = tref "key"
let tkey_hash  = tref "key_hash"
let tbytes     = tref "bytes"
let tchain_id  = tref "chain_id"
let toperation = tref "operation"

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

let mk_tor ?(loc=dummy) ?a k v : type_t =
  mkloc loc (Tor (k, v)), a

let mk_tcontract ?(loc=dummy) ?a t : type_t =
  mkloc loc (Tcontract t), a

let mk_tkeyof ?(loc=dummy) ?a t : type_t =
  mkloc loc (Tkeyof t), a



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

let edecimal  v = mk_eliteral (Ldecimal v)
let eaddress  v = mk_eliteral (Laddress v)
let estring   v = mk_eliteral (Lstring v)
let eduration v = mk_eliteral (Lduration v)
let edate     v = mk_eliteral (Ldate v)
let ebytes    v = mk_eliteral (Lbytes v)

let eterm         ?(loc=dummy) ?temp ?delta id    = mkloc loc (Eterm ((delta, temp), id))
let earray        ?(loc=dummy) l                  = mkloc loc (Earray l)
let erecord       ?(loc=dummy) rl                 = mkloc loc (Erecord rl)
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
let eif           ?(loc=dummy) ?e c t             = mkloc loc (Eif(c, t, e))
let efor          ?(loc=dummy) ?lbl id c b        = mkloc loc (Efor(lbl, id, c, b))
let eiter         ?(loc=dummy) ?lbl ?min id max e = mkloc loc (Eiter(lbl, id, min, max, e))
let ewhile        ?(loc=dummy) ?lbl c b           = mkloc loc (Ewhile(lbl, c, b))
let eseq          ?(loc=dummy) e1 e2              = mkloc loc (Eseq(e1, e2))
let eletin        ?(loc=dummy) ?t ?o id v b       = mkloc loc (Eletin(id, t, v, b, o))
let evar          ?(loc=dummy) ?t id e            = mkloc loc (Evar(id, t, e))
let ematchwith    ?(loc=dummy) e l                = mkloc loc (Ematchwith(e, l))
let erecupdate    ?(loc=dummy) e l                = mkloc loc (Erecupdate(e, l))
let equantifier   ?(loc=dummy) q id qk e          = mkloc loc (Equantifier(q, id, qk, e))
let eassert       ?(loc=dummy) id                 = mkloc loc (Eassert id)
let elabel        ?(loc=dummy) id                 = mkloc loc (Elabel id)
let ereturn       ?(loc=dummy) e                  = mkloc loc (Ereturn e)
let eoption       ?(loc=dummy) e                  = mkloc loc (Eoption e)
let eentrypoint   ?(loc=dummy) t e v              = mkloc loc (Eentrypoint (t, e, v))
let eunpack       ?(loc=dummy) t e                = mkloc loc (Eunpack (t, e))
let eself         ?(loc=dummy) id                 = mkloc loc (Eself id)
let eany          ?(loc=dummy) _                  = mkloc loc (Eany)
let enothing      ?(loc=dummy) _                  = mkloc loc (Enothing)
let einvalid      ?(loc=dummy) _                  = mkloc loc (Einvalid)

(* declarations utils *)

let mk_s_function name args ret_t spec body getter : s_function =
  {name; args; ret_t; spec; body; getter}

let mk_entry_properties ?(accept_transfer = true) ?calledby ?require ?failif ?spec_fun ?(functions = []) _ : entry_properties =
  { accept_transfer; calledby; require; failif; spec_fun; functions }

let mk_transition_item id eexto eexts : lident * (expr * exts) option * (expr * exts) option = id, eexto, eexts

let mk_variable_decl ?dv ?(le=[]) ?exts id t vk : variable_decl = id, t, dv, vk, le, exts

let mk_enum_decl ?exts l : enum_decl = l, exts

let mk_asset_decl ?(fs=[]) ?(sfs=[]) ?(aos=[]) ?(apos=[]) ?ao ?exts id : asset_decl = id, fs, sfs, aos, apos, ao, exts

let mk_record_decl ?(fs=[]) ?pos ?exts id : record_decl = id, fs, pos, exts

let mk_entry_decl ?(args=[]) ?body ?exts id ep : entry_decl = id, args, ep, body, exts

let mk_transition_decl ?(args=[]) ?te ?(trs=[]) ?exts id body ep : transition_decl = id, args, te, body, ep, trs, exts

let mk_extension_decl ?(es=[]) id : extension_decl = id, es

let mk_namespace_decl ?(ds=[]) id : namespace_decl = id, ds

let mk_asset_option_identifiedby ids = AOidentifiedby ids
let mk_asset_option_sortedby id      = AOsortedby id
let mk_asset_option_to_big_map       = AOtoBigMap

let mk_asset_post_option_states id      = APOstates id
let mk_asset_post_option_constraints ls = APOconstraints ls
let mk_asset_post_option_init l         = APOinit l

let mk_enum_option_initial _        = EOinitial
let mk_enum_option_specification ls = EOspecification ls

let mk_assetoperation aoes e : asset_operation = AssetOperation (aoes, e)



(* declarations *)

let mk_darchetype ?parameters ?exts ?(loc=dummy) id =
  mkloc loc (Darchetype (id, parameters, exts))

let mk_variable ?(loc=dummy) vd =
  mkloc loc (Dvariable vd)

let mk_enum ?(loc=dummy) ek ed =
  mkloc loc (Denum (ek, ed))

let mk_asset ?(loc=dummy) ad =
  mkloc loc (Dasset ad)

let mk_record ?(loc=dummy) rd =
  mkloc loc (Drecord rd)

let mk_entry ?(loc=dummy) ed =
  mkloc loc (Dentry ed)

let mk_transition ?(loc=dummy) td =
  mkloc loc (Dtransition td)

let mk_extension ?(loc=dummy) ed =
  mkloc loc (Dextension ed)

let mk_namespace ?(loc=dummy) nd =
  mkloc loc (Dnamespace nd)

let mk_function ?(loc=dummy) sf =
  mkloc loc (Dfunction sf)

let mk_specification ?(loc=dummy) s =
  mkloc loc (Dspecification s)

let mk_specasset ?(loc=dummy) id ls =
  mkloc loc (Dspecasset (id, ls))

let mk_specfun ?(loc=dummy) sf id args s =
  mkloc loc (Dspecfun (sf, id, args, s))

let mk_specvariable ?(loc=dummy) id ls =
  mkloc loc (Dspecvariable (id, ls))

let mk_security ?(loc=dummy) s =
  mkloc loc (Dsecurity s)

let mk_dtype ?(loc=dummy) id t =
  mkloc loc (Dtype (id, t))


let mk_invalid ?(loc=dummy) () =
  mkloc loc Dinvalid



let mk_archetype ?(decls=[]) ?(loc=dummy) () =
  mkloc loc (Marchetype decls)

(* -------------------------------------------------------------------- *)


let is_keyword = function
  | "added"
  | "aggregate"
  | "and"
  | "any"
  | "archetype"
  | "assert"
  | "asset"
  | "as"
  | "at"
  | "before"
  | "begin"
  | "but"
  | "by"
  | "call"
  | "called"
  | "constant"
  | "contract"
  | "definition"
  | "div"
  | "do"
  | "dofailif"
  | "done"
  | "dorequire"
  | "effect"
  | "else"
  | "end"
  | "entry"
  | "entrypoint"
  | "enum"
  | "exists"
  | "extension"
  | "fail"
  | "failif"
  | "fails"
  | "false"
  | "for"
  | "forall"
  | "from"
  | "function"
  | "getter"
  | "identified"
  | "if"
  | "in"
  | "initial"
  | "initialized"
  | "invariant"
  | "iter"
  | "label"
  | "left"
  | "let"
  | "list"
  | "map"
  | "match_list"
  | "match_option"
  | "match_or"
  | "match_loop_left"
  | "match"
  | "namespace"
  | "none"
  | "not"
  | "on"
  | "option"
  | "or"
  | "otherwise"
  | "partition"
  | "pkey"
  | "postcondition"
  | "predicate"
  | "record"
  | "ref"
  | "removed"
  | "require"
  | "return"
  | "right"
  | "security"
  | "self"
  | "set"
  | "shadow"
  | "some"
  | "sorted"
  | "specification"
  | "states"
  | "then"
  | "to"
  | "transfer"
  | "transition"
  | "true"
  | "type"
  | "unmoved"
  | "unpack"
  | "use"
  | "var"
  | "variable"
  | "view"
  | "when"
  | "while"
  | "with"
  | "xor"
    -> true
  | _ -> false
