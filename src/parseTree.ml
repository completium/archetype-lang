(* -------------------------------------------------------------------- *)
open Ident
open Location

(* -------------------------------------------------------------------- *)
type lident = ident loced

(* -------------------------------------------------------------------- *)
type container =
  | Collection
  | Queue
  | Stack
  | Set
  | Partition

type type_r =
  | Tref of lident
  | Tcontainer of type_t * container
  | Tvset of lident * type_t
  | Tapp of type_t * type_t
  | Ttuple of type_t list

and type_t = type_r loced

(* -------------------------------------------------------------------- *)
type logical_operator =
  | And
  | Or
  | Imply
  | Equiv

type comparison_operator =
  | Equal
  | Nequal
  | Gt
  | Ge
  | Lt
  | Le

type arithmetic_operator =
  | Plus
  | Minus
  | Mult
  | Div
  | Modulo

type unary_operator =
  | Uplus
  | Uminus
  | Not

type assignment_operator =
  | ValueAssign
  | SimpleAssign
  | PlusAssign
  | MinusAssign
  | MultAssign
  | DivAssign
  | AndAssign
  | OrAssign

type quantifier =
  | Forall
  | Exists

type operator = [
  | `Logical of logical_operator
  | `Cmp     of comparison_operator
  | `Arith   of arithmetic_operator
  | `Unary   of unary_operator
]

type name = lident option * lident

type expr_r =
  | Eterm         of name
  | Eop           of operator
  | Eliteral      of literal
  | Earray        of expr list
  | Edot          of expr * lident
  | EassignFields of assignment_field list
  | Eapp          of expr * expr list
  | Etransfer     of expr * bool * expr option
  | Eassign       of assignment_operator * expr * expr
  | Eif           of expr * expr * expr option
  | Ebreak
  | Efor          of lident * expr * expr * lident option
  | Eassert       of expr
  | Eseq          of expr * expr
  | Efun          of lident_typ list * expr
  | Eletin        of lident_typ * expr * expr
  | Equantifier   of quantifier * lident_typ * expr

and literal =
  | Lnumber   of Big_int.big_int
  | Lfloat    of float
  | Laddress  of string
  | Lstring   of string
  | Lbool     of bool
  | Lduration of string
  | Ldate     of string

and assignment_field =
  | AassignField of assignment_operator * (lident option * lident) * expr

and expr = expr_r loced
and lident_typ = lident * type_t option * extension list option

(* -------------------------------------------------------------------- *)
and extension_r =
 | Eextension of lident * expr list option (** extension *)

and extension = extension_r loced

and exts = extension list option

(* -------------------------------------------------------------------- *)
type field_r =
  | Ffield of lident * type_t * expr option * extension list option   (** field *)

and field = field_r loced

(* -------------------------------------------------------------------- *)
type transitem_r =
  | Tcalledby of expr * extension list option                                (** called by *)
  | Tcondition of named_item list * extension list option                    (** condition *)
  | Ttransition of expr * expr * expr option * extension list option         (** transition  *)
  | Tfunction of lident * args * type_t option * expr                        (** function *)
  | Tspecification of s_variable * s_action * s_invariant * s_ensure * exts  (** specification *)
  | Tinvariant of lident * named_item list * extension list option           (** invariant *)
  | Taction of expr * extension list option                                  (** action  *)

and transitem = transitem_r loced

and named_item = lident option * expr

and args = lident_typ list

and s_variable = (lident * type_t * expr option) list option
and s_action = expr option
and s_invariant = named_item list option
and s_ensure = named_item list


(* -------------------------------------------------------------------- *)
type declaration_r =
  | Duse           of lident                                              (** use *)
  | Dmodel         of lident                                              (** model *)
  | Dconstant      of lident * lident * expr option * exts                (** constant *)
  | Dvariable      of lident * lident * value_option list option * expr option * exts       (** variable *)
  | Drole          of lident * expr option * exts                                    (** role *)
  | Denum          of lident * lident list                                           (** enum *)
  | Dstates        of lident option * (lident * state_option list option) list       (** states *)
  | Dasset         of lident * field list option * expr list option * asset_option list option * expr option * asset_operation option (** asset *)
  | Dobject        of lident * expr * exts                             (** object *)
  | Dkey           of lident * expr * exts                             (** key *)
  | Dtransaction   of lident * args * transitem list * exts            (** transaction *)
  | Dextension     of lident * expr list option                        (** extension *)
  | Dnamespace     of lident * declaration list                        (** namespace *)
  | Dcontract      of lident * signature list * expr option * exts     (** contract *)
  | Dfunction      of lident * args * type_t option * expr             (** function *)
  | Dspecification of named_item list * exts                           (** specification *)

and value_option =
  | VOfrom of expr
  | VOto of expr

and asset_option =
  | AOasrole
  | AOidentifiedby of lident
  | AOsortedby of lident

and state_option =
  | SOinitial

and signature =
  | Ssignature of lident * type_t list

and declaration = declaration_r loced

and asset_operation_enum =
  | AOadd
  | AOremove
  | AOupdate

and asset_operation =
  | AssetOperation of asset_operation_enum list * expr list option

(* -------------------------------------------------------------------- *)
type model_r =
  | Mmodel of declaration list
  | Mmodelextension of lident * declaration list * declaration list

and model = model_r loced
