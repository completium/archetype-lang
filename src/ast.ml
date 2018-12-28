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

type assignment_operator =
  | Assign
  | PlusAssign
  | MinusAssign
  | MultAssign
  | DivAssign
  | AndAssign
  | OrAssign

type quantifier =
  | Forall
  | Exists

type expr_r =
  | Eterm of lident
  | Ecall of expr * expr list
  | Eliteral of literal
  | Edot of expr * expr
  | Elogical of logical_operator * expr * expr
  | Enot of expr
  | Ecomparison of comparison_operator * expr * expr
  | Earithmetic of arithmetic_operator * expr * expr
  | Earray of expr list
  | EassignFields of assignment_field list
  | Equantifier of quantifier * lident * expr * expr

and literal =
  | Lnumber of int
  | Lfloat  of float
  | Lstring of string

and assignment_field =
  | AassignField of assignment_operator * expr * expr

and expr = expr_r loced

(* -------------------------------------------------------------------- *)
type extension_r =
 | Eextension of lident * expr list option (** extension *)

and extension = extension_r loced

(* -------------------------------------------------------------------- *)
type field_r =
  | Ffield of lident * type_t * extension list option   (** field *)

and field = field_r loced

(* -------------------------------------------------------------------- *)
type instr_r =
  | Iassign of assignment_operator * expr * expr
  | Iletin of lident * expr * code
  | Iif of expr * code * code option
  | Ifor of lident * expr * code
  | Itransfer of expr * bool option * expr option
  | Itransition of expr
  | Icall of expr
  | Iassert of expr
and code = instr list

and instr = instr_r loced

(* -------------------------------------------------------------------- *)
type transitem_r =
  | Targs of field list                           (** args *)
  | Tcalledby of expr * extension list option     (** called by *)
  | Tensure of expr                               (** ensure *)
  | Tcondition of expr                            (** condition *)
  | Ttransferred of expr                          (** transferred *)
  | Ttransition of expr * expr * expr option      (** transition  *)
  | Taction of instr list                         (** action  *)

and transitem = transitem_r loced

(* -------------------------------------------------------------------- *)
type declaration_r =
  | Duse         of lident                                              (** use *)
  | Dmodel       of lident                                              (** model *)
  | Dconstant    of lident * lident * extension list option             (** constant *)
  | Dvalue       of lident * lident * value_option list option * expr option * extension list option             (** value *)
  | Drole        of lident * expr option * extension list option                   (** role *)
  | Denum        of lident * lident list                                           (** enum *)
  | Dstates      of lident option * (lident * state_option list option) list       (** states *)
  | Dasset       of lident * field list option * asset_option list option          (** asset *)
  | Dassert      of expr                                                           (** assert *)
  | Dobject      of lident * expr * extension list option
  | Dkey         of lident * expr * extension list option
  | Dtransition  of lident * expr * expr * transitem list * extension list option  (** transition *)
  | Dtransaction of lident * transitem list * extension list option                (** transaction *)
  | Dextension   of lident * expr list option                                      (** extension *)

and value_option =
  | VOfrom of expr
  | VOto of expr

and asset_option =
  | AOasrole
  | AOidentifiedby of lident

and state_option =
  | SOinitial

and declaration = declaration_r loced

(* -------------------------------------------------------------------- *)
type model =
  | Mmodel of declaration list
