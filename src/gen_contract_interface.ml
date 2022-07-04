open Location
open Model

type type_kind =
  | Int
[@@deriving yojson, show {with_path = false}]

type type_ = {
  kind: type_kind;
  name: string;
  args: type_ list;
}
[@@deriving yojson, show {with_path = false}]

type expr_node =
  | String of string
[@@deriving yojson, show {with_path = false}]

type expression = {
  node: expr_node
}
[@@deriving yojson, show {with_path = false}]

type param = {
  name: string;
  type_: type_;
}
[@@deriving yojson, show {with_path = false}]

type decl_type = {
  kind: string
}
[@@deriving yojson, show {with_path = false}]

type decl_entrypoint = {
  name: string
}
[@@deriving yojson, show {with_path = false}]

type decl_getter = {
  name: string
}
[@@deriving yojson, show {with_path = false}]

type contract_interface = {
  name : string;
  parameters: param list;
  types: decl_type list;
  entrypoints: decl_entrypoint list;
  getters: decl_getter list;
  errors: expression list;
}
[@@deriving yojson, show {with_path = false}]

let mk_contract_interface name parameters types entrypoints getters errors : contract_interface =
  { name; parameters; types; entrypoints; getters; errors }

let model_to_contract_interface (model : model) : contract_interface =
  mk_contract_interface (unloc model.name) [] [] [] [] []

let model_to_contract_interface_json (model : model) : string =
  let ci = model_to_contract_interface model in
  Format.asprintf "%s\n" (Yojson.Safe.to_string (contract_interface_to_yojson ci))
