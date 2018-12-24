(* -------------------------------------------------------------------- *)
open Ident
open Location

(* -------------------------------------------------------------------- *)
type lident = ident loced

(* -------------------------------------------------------------------- *)
type ident_r =
  | Idouble of lident * lident
  | Isimple of lident

and ident_t = ident_r loced

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
  | Eterm of ident_t * expr list option
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
  | Lstring of string

and assignment_field =
  | AassignField of assignment_operator * ident_t * expr

and expr = expr_r loced

(* -------------------------------------------------------------------- *)
type extension_r =
 | Eextension of lident * expr list option (** extension *)

and extension = extension_r loced

(* -------------------------------------------------------------------- *)
type field_r =
  | Ffield of lident * lident   (** field *)

and field = field_r loced

(* -------------------------------------------------------------------- *)
type instr_r =
  | Iassign of assignment_operator * expr * expr
  | Iletin of lident * expr * instr list
  | Iif of expr * instr list * instr list option
  | Ifor of lident * expr * instr list
  | Icall of expr
  | Iassert of expr

and instr = instr_r loced

(* -------------------------------------------------------------------- *)
type transitem_r =
  | Targs of field list                           (** args *)
  | Tcalledby of expr * extension list option     (** called by *)
  | Tensure of expr                               (** ensure *)
  | Tcondition of expr                            (** condition *)
  | Ttransition of lident * lident                (** transition  *)
  | Taction of instr list                         (** action  *)

and transitem = transitem_r loced

(* -------------------------------------------------------------------- *)
type declaration_r =
  | Duse         of lident                                              (** use *)
  | Dmodel       of lident                                              (** model *)
  | Dconstant    of lident * lident * extension list option             (** constant *)
  | Dvalue       of lident * lident * value_option list option * expr option * extension list option             (** value *)
  | Drole        of lident * expr option * extension list option        (** role *)
  | Denum        of lident * lident list                                (** enum *)
  | Dstates      of lident option * (lident * state_option list option) list (** states *)
  | Dasset       of lident * field list * asset_option list option      (** asset *)
  | Dassert      of expr                                                (** assert *)
  | Dtransition  of lident * lident * lident * transitem list * extension list option           (** transition *)
  | Dtransaction of lident * transitem list * extension list option                             (** transaction *)

and value_option =
  | VOfrom of lident
  | VOto of lident

and asset_option =
  | AOasrole
  | AOidentifiedby of lident

and state_option =
  | SOinitial

and declaration = declaration_r loced

(* -------------------------------------------------------------------- *)
type model =
  | Mmodel of declaration list
