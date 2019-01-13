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
  | Subset
  | Partition

type type_r =
  | Tref of lident
  | Tcontainer of lident * container

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

type unary_operator =
  | Uplus
  | Uminus
  | Not

type assignment_operator =
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
  | Etransition   of expr
  | Eassign       of assignment_operator * expr * expr
  | Eif           of expr * expr * expr option
  | Ebreak
  | Efor          of lident * expr * expr
  | Eassert       of expr
  | Eseq          of expr * expr
  | Efun          of lident_typ list * expr
  | Eletin        of lident_typ * expr * expr
  | Equantifier   of quantifier * lident_typ * expr

and literal =
  | Lnumber of Big_int.big_int
  | Lfloat  of float
  | Lstring of string
  | Lbool   of bool

and assignment_field =
  | AassignField of assignment_operator * (lident option * lident) * expr

and expr = expr_r loced
and typ_ = expr
and lident_typ = lident * typ_ option

(* -------------------------------------------------------------------- *)
type extension_r =
 | Eextension of lident * expr list option (** extension *)

and extension = extension_r loced

(* -------------------------------------------------------------------- *)
type field_r =
  | Ffield of lident * type_t * expr option * extension list option   (** field *)

and field = field_r loced

(* -------------------------------------------------------------------- *)
type transitem_r =
  | Targs of field list * extension list option                      (** args *)
  | Tcalledby of expr * extension list option                        (** called by *)
  | Tensure of expr * extension list option                          (** ensure *)
  | Tcondition of expr * extension list option                       (** condition *)
  | Ttransferred of expr * extension list option                     (** transferred *)
  | Ttransition of expr * expr * expr option * extension list option (** transition  *)
  | Taction of expr * extension list option                          (** action  *)

and transitem = transitem_r loced

(* -------------------------------------------------------------------- *)
type declaration_r =
  | Duse         of lident                                              (** use *)
  | Dmodel       of lident                                              (** model *)
  | Dconstant    of lident * lident * extension list option             (** constant *)
  | Dvalue       of lident * lident * value_option list option * expr option * extension list option       (** value *)
  | Drole        of lident * expr option * extension list option                   (** role *)
  | Denum        of lident * lident list                                           (** enum *)
  | Dstates      of lident option * (lident * state_option list option) list       (** states *)
  | Dasset       of lident * field list option * expr list option * asset_option list option * expr option (** asset *)
  | Dassert      of expr                                                           (** assert *)
  | Dobject      of lident * expr * extension list option
  | Dkey         of lident * expr * extension list option
  | Dtransition  of lident * expr * expr * transitem list * extension list option  (** transition *)
  | Dtransaction of lident * transitem list * extension list option                (** transaction *)
  | Dextension   of lident * expr list option                                      (** extension *)
  | Dnamespace   of lident * declaration list                                      (** namespace *)

and value_option =
  | VOfrom of expr
  | VOto of expr

and asset_option =
  | AOasrole
  | AOidentifiedby of lident
  | AOsortedby of lident

and state_option =
  | SOinitial

and declaration = declaration_r loced

(* -------------------------------------------------------------------- *)
type model_r =
  | Mmodel of declaration list
  | Mmodelextension of lident * declaration list * declaration list

and model = model_r loced
