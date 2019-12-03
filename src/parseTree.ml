(* -------------------------------------------------------------------- *)
open Ident
open Location

(* -------------------------------------------------------------------- *)
type lident = ident loced
[@@deriving yojson]

let pp_lident fmt i = Format.fprintf fmt "%s" (unloc i)

(* -------------------------------------------------------------------- *)
type container =
  | Collection
  | Partition
[@@deriving yojson, show {with_path = false}]

type type_r =
  | Tref of lident
  | Tasset of lident
  | Tcontainer of type_t * container
  | Ttuple of type_t list
  | Toption of type_t
  | Tkeyof of type_t
[@@deriving yojson, show {with_path = false}]

and type_t = type_r loced
[@@deriving yojson, show {with_path = false}]

(* -------------------------------------------------------------------- *)
type logical_operator =
  | And
  | Or
  | Imply
  | Equiv
[@@deriving yojson, show {with_path = false}]

type comparison_operator =
  | Equal
  | Nequal
  | Gt
  | Ge
  | Lt
  | Le
[@@deriving yojson, show {with_path = false}]

type arithmetic_operator =
  | Plus
  | Minus
  | Mult
  | Div
  | Modulo
[@@deriving yojson, show {with_path = false}]

type unary_operator =
  | Uplus
  | Uminus
  | Not
[@@deriving yojson, show {with_path = false}]

type assignment_operator =
  | ValueAssign
  | PlusAssign
  | MinusAssign
  | MultAssign
  | DivAssign
  | AndAssign
  | OrAssign
[@@deriving yojson, show {with_path = false}]

type quantifier =
  | Forall
  | Exists
[@@deriving yojson, show {with_path = false}]

type operator = [
  | `Logical of logical_operator
  | `Cmp     of comparison_operator
  | `Arith   of arithmetic_operator
  | `Unary   of unary_operator
]
[@@deriving yojson, show {with_path = false}]

type pattern_unloc =
  | Pwild
  | Pref of lident
[@@deriving yojson, show {with_path = false}]

type pattern = pattern_unloc loced
[@@deriving yojson, show {with_path = false}]

type s_term = {
  before: bool;
  label: lident option;
}
[@@deriving yojson, show {with_path = false}]

type expr_unloc =
  | Eterm         of s_term * lident
  | Eliteral      of literal
  | Earray        of expr list
  | Erecord       of record_item list
  | Etuple        of expr list
  | Edot          of expr * lident
  | Emulticomp    of expr * (comparison_operator loced * expr) list
  | Eapp          of function_ * expr list
  | Emethod       of expr * lident * expr list
  | Etransfer     of expr * expr
  | Erequire      of expr
  | Efailif       of expr
  | Eassign       of assignment_operator * expr * expr
  | Eif           of expr * expr * expr option
  | Ebreak
  | Efor          of lident option * lident * expr * expr
  | Eiter         of lident option * lident * expr option * expr * expr
  | Eseq          of expr * expr
  | Eletin        of lident * type_t option * expr * expr * expr option
  | Evar          of lident * type_t option * expr
  | Ematchwith    of expr * branch list
  | Equantifier   of quantifier * lident * quantifier_kind * expr
  | Eassert       of lident
  | Elabel        of lident
  | Ereturn       of expr
  | Eoption       of option_
  | Einvalid
[@@deriving yojson, show {with_path = false}]

and branch = (pattern list * expr)

and scope = [
  | `Added
  | `After
  | `Before
  | `Fixed
  | `Removed
  | `Stable
]
[@@derive yojson, show {with_path = false}]

and quantifier_kind =
  | Qcollection of expr
  | Qtype of type_t
[@@deriving yojson, show {with_path = false}]

and option_ =
  | OSome of expr
  | ONone
[@@deriving yojson, show {with_path = false}]

and function_ =
  | Fident of lident
  | Foperator of operator loced
[@@deriving yojson, show {with_path = false}]

and literal =
  | Lnumber   of Core.big_int
  | Lrational of Core.big_int * Core.big_int
  | Ltz       of Core.big_int
  | Lmtz      of Core.big_int
  | Lutz      of Core.big_int
  | Laddress  of string
  | Lstring   of string
  | Lbool     of bool
  | Lduration of string
  | Ldate     of string
[@@deriving yojson, show {with_path = false}]

and record_item = (assignment_operator * lident) option * expr

and expr = expr_unloc loced
[@@deriving yojson, show {with_path = false}]

and lident_typ = lident * type_t * extension list option
[@@deriving yojson, show {with_path = false}]

and label_expr = (lident * expr) loced

and label_exprs = label_expr list

(* -------------------------------------------------------------------- *)
and extension_unloc =
  | Eextension of lident * expr option (** extension *)
[@@deriving yojson, show {with_path = false}]

and extension = extension_unloc loced
[@@deriving yojson, show {with_path = false}]

and exts = extension list option
[@@deriving yojson, show {with_path = false}]

(* -------------------------------------------------------------------- *)
type field_unloc =
  | Ffield of lident * type_t * expr option * exts   (** field *)
[@@deriving yojson, show {with_path = false}]

and field = field_unloc loced
[@@deriving yojson, show {with_path = false}]

type args = lident_typ list
[@@deriving yojson, show {with_path = false}]

type invariants = (lident * expr list) list
[@@deriving yojson, show {with_path = false}]

type specification_item_unloc =
  | Vpredicate of lident * args * expr
  | Vdefinition of lident * type_t * lident * expr
  | Vlemma of lident * expr
  | Vtheorem of lident * expr
  | Vvariable of lident * type_t * expr option
  | Veffect of expr
  | Vassert of (lident * expr * invariants * lident list)
  | Vpostcondition of (lident * expr * invariants * lident list)
[@@deriving yojson, show {with_path = false}]

type specification_item = specification_item_unloc loced
[@@deriving yojson, show {with_path = false}]

type specification_unloc = specification_item list * exts
[@@deriving yojson, show {with_path = false}]

type specification = specification_unloc loced
[@@deriving yojson, show {with_path = false}]

type security_arg_unloc =
  | Sident of lident
  | Sdot   of lident * lident
  | Slist of security_arg list
  | Sapp of lident * security_arg list
  | Sbut of lident * security_arg
  | Sto of lident * security_arg
[@@deriving yojson, show {with_path = false}]

and security_arg = security_arg_unloc loced
[@@deriving yojson, show {with_path = false}]

type security_item_unloc = lident * lident * security_arg list
[@@deriving yojson, show {with_path = false}]

type security_item = security_item_unloc loced
[@@deriving yojson, show {with_path = false}]

type security_unloc = security_item list * exts
[@@deriving yojson, show {with_path = false}]

and security = security_unloc loced
[@@deriving yojson, show {with_path = false}]

type s_function = {
  name  : lident;
  args  : args;
  ret_t : type_t option;
  spec : specification option;
  body  : expr;
}
[@@deriving yojson, show {with_path = false}]

type action_properties = {
  accept_transfer : bool;
  calledby        : (expr * exts) option;
  require         : (label_exprs * exts) option;
  failif          : (label_exprs * exts) option;
  spec            : specification option;
  functions       : (s_function loced) list;
}
[@@deriving yojson, show {with_path = false}]

type transition = (lident * (expr * exts) option * (expr * exts) option) list
[@@deriving yojson, show {with_path = false}]

(* -------------------------------------------------------------------- *)
type variable_kind =
  | VKvariable
  | VKconstant
[@@deriving yojson, show {with_path = false}]

type enum_kind =
  | EKenum of lident
  | EKstate
[@@deriving yojson, show {with_path = false}]

(* -------------------------------------------------------------------- *)
type declaration_unloc =
  | Darchetype     of lident * exts
  | Dvariable      of variable_decl
  | Denum          of enum_kind * enum_decl
  | Dasset         of asset_decl
  | Daction        of action_decl
  | Dtransition    of transition_decl
  | Dcontract      of contract_decl
  | Dextension     of extension_decl
  | Dnamespace     of namespace_decl
  | Dfunction      of s_function
  | Dspecification of specification
  | Dsecurity      of security
  | Dinvalid
[@@deriving yojson, show {with_path = false}]

and variable_decl =
  lident
  * type_t
  * expr option
  * value_option list option
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

and action_decl =
  lident
  * args
  * action_properties
  * (expr * exts) option
  * exts

and transition_decl =
  lident
  * args
  * (lident * type_t) option
  * expr
  * action_properties
  * transition
  * exts

and contract_decl =
  lident * signature list * exts

and extension_decl =
  lident * expr option

and namespace_decl =
  lident * declaration list

and value_option =
  | VOfrom of lident
  | VOto of lident
[@@deriving yojson, show {with_path = false}]

and asset_option =
  | AOidentifiedby of lident
  | AOsortedby of lident
[@@deriving yojson, show {with_path = false}]

and asset_post_option =
  | APOstates of lident
  | APOconstraints of label_exprs
  | APOinit of expr
[@@deriving yojson, show {with_path = false}]

and enum_option =
  | EOinitial
  | EOspecification of label_exprs
[@@deriving yojson, show {with_path = false}]

and signature =
  | Ssignature of lident * type_t list
[@@deriving yojson, show {with_path = false}]

and declaration = declaration_unloc loced
[@@deriving yojson, show {with_path = false}]

and asset_operation_enum =
  | AOadd
  | AOremove
  | AOupdate
[@@deriving yojson, show {with_path = false}]

and asset_operation =
  | AssetOperation of asset_operation_enum list * expr option
[@@deriving yojson, show {with_path = false}]

(* -------------------------------------------------------------------- *)
type archetype_unloc =
  | Marchetype of declaration list
  | Mextension of lident * declaration list * declaration list
[@@deriving yojson, show {with_path = false}]

and archetype = archetype_unloc loced
[@@deriving yojson, show {with_path = false}]

let mk_archetype ?(decls=[]) ?(loc=dummy) () =
  mkloc loc (Marchetype decls)

let mk_s_term ?(before=false) ?label () : s_term =
  { before = before; label = label }
