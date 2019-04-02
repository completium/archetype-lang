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
  | Queue
  | Stack
  | Set
  | Partition
[@@deriving yojson, show {with_path = false}]

type type_r =
  | Tref of lident
  | Tcontainer of type_t * container
  | Tvset of lident * type_t
  | Tapp of type_t * type_t
  | Ttuple of type_t list
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
  | SimpleAssign
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

type qualid =
  | Qident of lident
  | Qdot of qualid * lident
[@@deriving yojson, show {with_path = false}]

type name = lident option * lident
[@@deriving yojson, show {with_path = false}]

type pattern_unloc =
  | Pwild
  | Pref of lident
[@@deriving yojson, show {with_path = false}]

type pattern = pattern_unloc loced
[@@deriving yojson, show {with_path = false}]

type expr_r =
  | Eterm         of name
  | Eop           of operator
  | Eliteral      of literal
  | Earray        of lident option * expr list
  | Edot          of expr * lident
  | EassignFields of assignment_field list
  | Eapp          of expr * expr list
  | Etransfer     of expr * bool * qualid option
  | Eassign       of assignment_operator * expr * expr
  | Eif           of expr * expr * expr option
  | Ebreak
  | Efor          of lident * expr * expr * lident option
  | Eassert       of expr
  | Eseq          of expr * expr
  | Efun          of lident_typ list * expr
  | Eletin        of lident_typ * expr * expr
  | Ematchwith    of expr * (pattern list * expr) list
  | Equantifier   of quantifier * lident_typ * expr
[@@deriving yojson, show {with_path = false}]

and literal =
  | Lnumber   of Core.big_int
  | Lrational of Core.big_int * Core.big_int
  | Ltz       of Core.big_int
  | Laddress  of string
  | Lstring   of string
  | Lbool     of bool
  | Lduration of string
  | Ldate     of string
[@@deriving yojson, show {with_path = false}]

and assignment_field = assignment_operator * (lident option * lident) * expr

and expr = expr_r loced
[@@deriving yojson, show {with_path = false}]

and lident_typ = lident * type_t option * extension list option
[@@deriving yojson, show {with_path = false}]

(* -------------------------------------------------------------------- *)
and extension_r =
 | Eextension of lident * expr list option (** extension *)
[@@deriving yojson, show {with_path = false}]

and extension = extension_r loced
[@@deriving yojson, show {with_path = false}]

and exts = extension list option
[@@deriving yojson, show {with_path = false}]

(* -------------------------------------------------------------------- *)
type field_r =
  | Ffield of lident * type_t * expr option * extension list option   (** field *)
[@@deriving yojson, show {with_path = false}]

and field = field_r loced
[@@deriving yojson, show {with_path = false}]


type transition_r = (lident * expr option * expr option) list
[@@deriving yojson, show {with_path = false}]

type args = lident_typ list
[@@deriving yojson, show {with_path = false}]

type s_predicate = {
  name  : lident;
  args  : args;
  ret_t : type_t option;
  body  : expr;
}
[@@deriving yojson, show {with_path = false}]

(* -------------------------------------------------------------------- *)
type action_properties = {
  calledby : (expr * extension list option) option;
  condition : (named_item list * extension list option) option;
  functions : s_function list;
  specification : (s_variable * s_action * s_invariant * s_ensure * exts) option;
  invariants : (lident * named_item list * extension list option) list;
}
[@@deriving yojson, show {with_path = false}]

and named_item = lident option * expr
[@@deriving yojson, show {with_path = false}]

and s_variable = (lident * type_t * expr option) loced list option
[@@deriving yojson, show {with_path = false}]

and s_action = expr option
[@@deriving yojson, show {with_path = false}]

and s_invariant = named_item list option
[@@deriving yojson, show {with_path = false}]

and s_ensure = named_item list
[@@deriving yojson, show {with_path = false}]

and s_function = {
  name  : lident;
  args  : args;
  ret_t : type_t option;
  preds : s_predicate list;
  specs : named_item list;
  invs  : (lident * named_item list) list;
  body  : expr;
}
[@@deriving yojson, show {with_path = false}]

(* -------------------------------------------------------------------- *)
type declaration_r =
  | Duse           of lident                                              (** use *)
  | Dmodel         of lident * extension list option                      (** model *)
  | Dconstant      of lident * type_t * expr option * exts                (** constant *)
  | Dvariable      of lident * type_t * value_option list option * expr option * exts       (** variable *)
  | Denum          of lident * lident list                                           (** enum *)
  | Dstates        of lident option * (lident * state_option list option) list option * extension list option       (** states *)
  | Dasset         of lident * field list option * asset_option list option * asset_post_option list * asset_operation option * exts (** asset *)
  | Dobject        of lident * expr * exts                             (** object *)
  | Dkey           of lident * expr * exts                             (** key *)
  | Daction        of lident * args * action_properties * (expr * exts) option * exts
  | Dtransition    of lident * args * (lident * lident) option * expr * action_properties * transition_r * exts
  | Dextension     of lident * expr list option                        (** extension *)
  | Dnamespace     of lident * declaration list                        (** namespace *)
  | Dcontract      of lident * signature list * expr option * exts     (** contract *)
  | Dfunction      of s_function                                       (** function *)
  | Dspecification of named_item list * exts                           (** specification *)
[@@deriving yojson, show {with_path = false}]

and value_option =
  | VOfrom of qualid
  | VOto of qualid
[@@deriving yojson, show {with_path = false}]

and asset_option =
  | AOasrole
  | AOidentifiedby of lident
  | AOsortedby of lident
[@@deriving yojson, show {with_path = false}]

and asset_post_option =
  | APOstates of lident
  | APOconstraints of named_item list
  | APOinit of expr
[@@deriving yojson, show {with_path = false}]

and state_option =
  | SOinitial
  | SOspecification of named_item list
[@@deriving yojson, show {with_path = false}]

and signature =
  | Ssignature of lident * type_t list
[@@deriving yojson, show {with_path = false}]

and declaration = declaration_r loced
[@@deriving yojson, show {with_path = false}]

and asset_operation_enum =
  | AOadd
  | AOremove
  | AOupdate
[@@deriving yojson, show {with_path = false}]

and asset_operation =
  | AssetOperation of asset_operation_enum list * expr list option
[@@deriving yojson, show {with_path = false}]

(* -------------------------------------------------------------------- *)
type model_r =
  | Mmodel of declaration list
  | Mmodelextension of lident * declaration list * declaration list
[@@deriving yojson, show {with_path = false}]

and model = model_r loced
[@@deriving yojson, show {with_path = false}]
