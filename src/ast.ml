(* -------------------------------------------------------------------- *)
open Ident
open Location

(* -------------------------------------------------------------------- *)
type lident = ident loced

(* -------------------------------------------------------------------- *)
type field_r =
  | Tfield of lident * lident   (** field *)

and field = field_r loced

(* -------------------------------------------------------------------- *)
type declaration_r =
  | Tuse       of lident                      (** use *)
  | Tmodel     of lident                      (** model *)
  | Tconstant  of lident * lident             (** constant *)
  | Trole      of lident                      (** role *)
  | Tasset     of lident * field list         (** asset *)
  | Tenum      of lident * lident list        (** enum *)
  | Tstates    of lident option * lident list (** states *)
  | Textension of lident list                 (** extension *)

and declaration = declaration_r loced

(* -------------------------------------------------------------------- *)
type model =
  | Imodel of declaration list
