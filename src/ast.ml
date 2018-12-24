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
  | Sassign of assignment_operator * expr * expr
  | Sif of expr * instr list * instr list option
  | Sfor of lident * expr * instr list
  | Scall of expr
  | Sassert of expr

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
  | Duse         of lident                                        (** use *)
  | Dmodel       of lident                                        (** model *)
  | Dconstant    of lident * lident * extension list option       (** constant *)
  | Dvalue       of lident * lident * extension list option       (** value *)
  | Drole        of lident * expr option * extension list option  (** role *)
  | Denum        of lident * lident list                          (** enum *)
  | Dstates      of lident option * lident list                   (** states *)
  | Dasset       of lident * field list                           (** asset *)
  | Dassert      of expr                                          (** assert *)
  | Dtransition  of lident * lident * lident * transitem list     (** transition *)
  | Dtransaction of lident * transitem list                       (** transaction *)

and declaration = declaration_r loced

(* -------------------------------------------------------------------- *)
type model =
  | Mmodel of declaration list
