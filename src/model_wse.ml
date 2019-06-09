open Location

module A = Ast
module M = Model

type lident  = A.lident
[@@deriving show {with_path = false}]

type expr    = A.pterm
[@@deriving show {with_path = false}]

type type_   =
  | Tstorage
  | Toperations
  | Tbuiltin of A.ptyp
  | Trecord of lident
  | Tenum of lident
  | Ttuple of type_ list
  | Tcontainer of type_
[@@deriving show {with_path = false}]

type pattern = A.pattern
[@@deriving show {with_path = false}]

let pp_lident fmt i = Format.fprintf fmt "%s" (unloc i)

type type_instr =
  | OpsStorage
  | Storage
  | Ops
  | None
[@@deriving show {with_path = false}]

type letin_kind =
  | LKid of lident
  | LKtuple of lident list
[@@deriving show {with_path = false}]

type 'instr instruction_node =
  | Iif of (expr * 'instr * 'instr)
  | Imatchwith of expr * (pattern * 'instr) list
  | Ifold of (lident list * expr * 'instr)
  | Iletin of (letin_kind * expr * 'instr)
  | Iseq of 'instr list
  | Itransfer of (expr * string)
[@@deriving show {with_path = false}]

type instruction = {
  node: instruction instruction_node;
  type_: type_instr;
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
}
[@@deriving show {with_path = false}]

type kind_function =
  | Init
  | Function
  | Entry
[@@deriving show {with_path = false}]

type function_struct = {
  name: lident;
  kind: kind_function;
  ret: type_;
  args: (lident * type_) list;
  body: instruction;
}
[@@deriving show {with_path = false}]

type model = {
  name: lident;
  enums: enum_struct list;
  records: record_struct list;
  init: function_struct option;
  funs: function_struct list;
  entries: function_struct list;
}
[@@deriving show {with_path = false}]




let mk_enum ?(values = []) name : enum_struct =
  { name; values }

let mk_record ?(values = []) name : record_struct =
  { name; values }

let mk_function ?(args = []) name kind ret body : function_struct =
  { name; kind; ret; args; body }

let mk_model ?(enums = []) ?(records = []) ?init ?(funs = []) ?(entries = []) name : model =
  { name; enums; records; init; funs; entries }
