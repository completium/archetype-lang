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
type entity_r =
  | Tuse       of lident              (** use *)
  | Tmodel     of lident              (** model *)
  | Tconstant  of lident * lident     (** constant *)
  | Trole      of lident              (** role *)
  | Tasset     of lident * field list (** asset *)
  | Textension of lident list         (** extension *)

and entity = entity_r loced

(* -------------------------------------------------------------------- *)
type model =
  | Imodel of entity list
