
type ident = string
[@@deriving show {with_path = false}]

type type_basic =
  | Tunit
  | Tbool
  | Tint
  | Tnat
  | Ttez
  | Tstring
  | Tbytes
  | Ttimestamp
  | Tkey
  | Tkey_hash
  | Tsignature
  | Toperation
  | Taddress
[@@deriving show {with_path = false}]

type type_ =
  | Tbasic of type_basic
  | Ttuple of type_ list
  | Tlist of type_
  | Tmap of type_ * type_
  | Tcontract of ident
  | Toption of type_
  | Tlocal of ident (* struct or type *)
[@@deriving show {with_path = false}]

type pattern =
  | Pid of ident
  | Pwild
[@@deriving show {with_path = false}]

type bin_operator =
  | And
  | Or
  | Equal
  | Nequal
  | Gt
  | Ge
  | Lt
  | Le
  | Plus
  | Minus
  | Mult
  | Div
  | Modulo
[@@deriving show {with_path = false}]

type unary_operator =
  | Not
  | Uminus
  | Uplus
[@@deriving show {with_path = false}]

type operator = [
  | `Bin of bin_operator
  | `Una of unary_operator
]
[@@deriving show {with_path = false}]

type literal =
  | Lint    of Core.big_int
  | Lbool   of bool
  | Lstring of string
  | Lmap    of type_ * type_
  | Lraw    of string
[@@deriving show {with_path = false}]

type expr =
  | Eletin of ((ident * type_) list * expr) list * expr
  | Etuple of expr list
  | Eif of (expr * expr * expr)
  | Ematchwith of expr * (pattern list * expr) list
  | Eapp of ident * expr list
  | Ebin of bin_operator * expr * expr
  | Eunary of unary_operator * expr
  | Erecord of ident option * (ident * expr) list
  | Evar of ident
  | Econtainer of expr list
  | Elit of literal
  | Edot of expr * ident
[@@deriving show {with_path = false}]

type type_struct = {
  name: ident;
  values: (ident * type_ option) list;
}
[@@deriving show {with_path = false}]

type struct_struct = {
  name: ident;
  fields: (ident * type_ * expr) list;
}
[@@deriving show {with_path = false}]

type fun_node =
  | Init
  | Entry
  | None
[@@deriving show {with_path = false}]

type fun_struct = {
  name: ident;
  node: fun_node;
  args: (ident * type_) list;
  ret: type_;
  body: expr;
}
[@@deriving show {with_path = false}]

type decl =
  | Dtype of type_struct
  | Dstruct of struct_struct
  | Dfun of fun_struct
[@@deriving show {with_path = false}]

type tree = {
  name: ident;
  decls: decl list;
}
[@@deriving show {with_path = false}]

let mk_type ?(values = []) name : type_struct =
  { name; values }

let mk_struct ?(fields = []) name : struct_struct =
  { name; fields }

let mk_fun name node args ret body : fun_struct =
  { name; node; args; ret; body }

let mk_tree ?(decls= []) name : tree =
  { name; decls }
