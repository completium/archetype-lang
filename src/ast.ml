open Ident

type field =
  | Tfield of ident * ident        (** field *)

type entity =
  | Tuse of ident                  (** use *)
  | Tmodel of ident                (** model *)
  | Tconstant of ident * ident     (** constant *)
  | Trole of ident                 (** role *)
  | Tasset of ident * field list   (** asset *)
  | Textension of ident list       (** extension *)

type model =
  | Imodel of entity list
