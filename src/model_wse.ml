open Location

module A = Ast
module M = Model

type lident  = A.lident
[@@deriving show {with_path = false}]

type type_   =
  | Tstorage
  | Toperations
  | Tbuiltin of A.vtyp
  | Trecord of lident
  | Tenum of lident
  | Ttuple of type_ list
  | Tcontainer of type_
[@@deriving show {with_path = false}]

type 'expr wse_expr =
  | Ifold of (lident list * 'expr * 'expr)
[@@deriving show {with_path = false}]

type 'expr expr_node = [
  | `Eexpr of (lident, type_, 'expr) A.term_node
  | `Ewse  of 'expr wse_expr
]
[@@deriving show {with_path = false}]

type expr    = (type_, expr expr_node) A.struct_poly
[@@deriving show {with_path = false}]

type pattern = A.pattern
[@@deriving show {with_path = false}]

let pp_lident fmt i = Format.fprintf fmt "%s" (unloc i)

type 'instr instruction_node =
  | Iletin of lident list * expr * 'instr
  | Ituple of lident list
[@@deriving show {with_path = false}]

type instruction = {
  node: instruction instruction_node;
  type_: type_ list;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type enum_struct = {
  name: lident;
  values: lident list;
}
[@@deriving show {with_path = false}]

type record_struct = {
  name: lident;
  values: (lident * type_) list;
  init: expr;
}
[@@deriving show {with_path = false}]

type kind_function =
  | Function
  | Entry
[@@deriving show {with_path = false}]

type function_struct = {
  name: lident;
  kind: kind_function;
  ret:  type_;
  args: (lident * type_) list;
  body: instruction;
}
[@@deriving show {with_path = false}]

type model = {
  name: lident;
  enums: enum_struct list;
  records: record_struct list;
  funs: function_struct list;
}
[@@deriving show {with_path = false}]




let mk_enum ?(values = []) name : enum_struct =
  { name; values }

let mk_record ?(values = []) name init : record_struct =
  { name; values; init }

let mk_function ?(args = []) name kind ret body : function_struct =
  { name; kind; ret; args; body }

let mk_model ?(enums = []) ?(records = []) ?(funs = []) name : model =
  { name; enums; records; funs }
