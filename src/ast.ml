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
  | Not

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

type assignment_operator =
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
  | `Arith   of arithmetic_operator
  | `Cmp     of comparison_operator
  | `Unary   of unary_operator
]

type name = lident option * lident

type expr_r =
  | Eterm         of name
  | Eop           of operator
  | Eapp          of expr * expr list
  | Eletin        of lident * expr * expr
  | Eseq          of expr * expr
  | Eliteral      of literal
  | Edot          of expr * lident
  | Efun          of (lident * typ_ option) list * expr
  | Earray        of expr list
  | Eassign       of expr * assignment_operator option * expr
  | Equantifier   of quantifier * (lident * typ_ option) * expr

and literal =
  | Lnumber of Big_int.big_int
  | Lfloat  of float
  | Lstring of string
  | Lbool   of bool

and expr = expr_r loced
and typ_ = expr

(* -------------------------------------------------------------------- *)
type extension_r =
 | Eextension of lident * expr list option (** extension *)

and extension = extension_r loced

(* -------------------------------------------------------------------- *)
type field_r =
  | Ffield of lident * type_t * expr option * extension list option   (** field *)

and field = field_r loced

(* -------------------------------------------------------------------- *)
type instr_r =
  | Iassign of assignment_operator * expr * expr
  | Iletin of lident * expr * code
  | Iif of expr * code * code option
  | Ifor of lident * expr * code
  | Itransfer of expr * bool * expr option
  | Itransition of expr
  | Iapp of expr
  | Iassert of expr
  | Ibreak
and code = instr list

and instr = instr_r loced

(* -------------------------------------------------------------------- *)
type transitem_r =
  | Targs of field list * extension list option                      (** args *)
  | Tcalledby of expr * extension list option                        (** called by *)
  | Tensure of expr * extension list option                          (** ensure *)
  | Tcondition of expr * extension list option                       (** condition *)
  | Ttransferred of expr * extension list option                     (** transferred *)
  | Ttransition of expr * expr * expr option * extension list option (** transition  *)
  | Taction of instr list * extension list option                    (** action  *)

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
  | Dasset       of lident * field list option * expr list option * asset_option list option * code option (** asset *)
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
