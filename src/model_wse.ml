open Ident

module A = Ast
module M = Model

type type_   =
  | Tstorage
  | Toperations
  | Tbool
  | Tint
  | Tuint
  | Trational
  | Tdate
  | Tduration
  | Tstring
  | Taddress
  | Trole
  | Tcurrency of A.currency
  | Tkey
  | Trecord of ident
  | Tenum of ident
  | Ttuple of type_ list
  | Tcontainer of type_
  | Tcontract of ident
  | Tmap of type_ * type_
  | Tunit
[@@deriving show {with_path = false}]

type pattern =
  | Pwild
  | Pexpr of expr
  | Pconst of ident
[@@deriving show {with_path = false}]

and expr =
  | Eif        of (expr * expr * expr)
  | Ematchwith of expr * (pattern * expr) list
  | Ecall      of (expr * expr list)
  | Eand       of (expr * expr)
  | Eor        of (expr * expr)
  | Enot       of expr
  | Eequal     of (expr * expr)
  | Enequal    of (expr * expr)
  | Egt        of (expr * expr)
  | Ege        of (expr * expr)
  | Elt        of (expr * expr)
  | Ele        of (expr * expr)
  | Eplus      of (expr * expr)
  | Eminus     of (expr * expr)
  | Emult      of (expr * expr)
  | Ediv       of (expr * expr)
  | Emodulo    of (expr * expr)
  | Euplus     of expr
  | Euminus    of expr
  | Erecord    of (ident * expr) list
  | Eletin     of ((ident * type_) list * expr) list * expr
  | Evar       of ident
  | Earray     of expr list
  | Elitint    of Core.big_int
  | Elitbool   of bool
  | Elitstring of string
  | Elitmap    of type_ * type_
  | Elitraw    of string
  | Edot       of expr * ident
  | Etuple     of expr list
  | Efold      of (ident list * expr * expr)
[@@deriving show {with_path = false}]

type enum_struct = {
  name: ident;
  values: ident list;
}
[@@deriving show {with_path = false}]

type record_struct = {
  name: ident;
  values: (ident * type_ * expr) list;
}
[@@deriving show {with_path = false}]

type kind_function =
  | Function
  | Inline
  | Entry
[@@deriving show {with_path = false}]

type function_struct = {
  name: ident;
  kind: kind_function;
  ret:  type_;
  args: (ident * type_) list;
  body: expr;
}
[@@deriving show {with_path = false}]

type model = {
  name: ident;
  enums: enum_struct list;
  records: record_struct list;
  funs: function_struct list;
}
[@@deriving show {with_path = false}]


let mk_enum ?(values = []) name : enum_struct =
  { name; values }

let mk_record ?(values = []) name : record_struct =
  { name; values }

let mk_function ?(args = []) name kind ret body : function_struct =
  { name; kind; ret; args; body }

let mk_model ?(enums = []) ?(records = []) ?(funs = []) name : model =
  { name; enums; records; funs }