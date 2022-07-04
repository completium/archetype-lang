open Location

module M = Model

type type_ = {
  node: string;
  name: string option;
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

type parameter = {
  name: string;
  type_: type_  [@key "type"];
  const: bool;
  default: expression option;
}
[@@deriving yojson, show {with_path = false}]

type argument = {
  name: string;
  type_: type_  [@key "type"];
}
[@@deriving yojson, show {with_path = false}]

type decl_type = {
  kind: string
}
[@@deriving yojson, show {with_path = false}]

type decl_entrypoint = {
  name: string;
  args: argument list;
}
[@@deriving yojson, show {with_path = false}]

type decl_getter = {
  name: string;
  args: argument list;
  return: type_;
}
[@@deriving yojson, show {with_path = false}]

type contract_interface = {
  name : string;
  parameters: parameter list;
  types: decl_type list;
  entrypoints: decl_entrypoint list;
  getters: decl_getter list;
  errors: expression list;
}
[@@deriving yojson, show {with_path = false}]

let mk_type node name args : type_ =
  { node; name; args }

let mk_argument name type_ : argument =
  { name; type_ }

let mk_entrypoint name args : decl_entrypoint =
  { name; args }

let mk_getter name args return : decl_getter =
  { name; args; return }

let mk_parameter name type_ const default : parameter =
  { name; type_; const; default }

let mk_contract_interface name parameters types entrypoints getters errors : contract_interface =
  { name; parameters; types; entrypoints; getters; errors }

let rec for_type (t : M.type_) : type_ =
  match M.get_ntype t with
  | Tasset id                      -> mk_type "asset"              (Some (unloc id)) []
  | Tenum id                       -> mk_type "enum"               (Some (unloc id)) []
  | Tstate                         -> mk_type "state"               None             []
  | Tbuiltin Bunit                 -> mk_type "unit"                None             []
  | Tbuiltin Bbool                 -> mk_type "bool"                None             []
  | Tbuiltin Bint                  -> mk_type "int"                 None             []
  | Tbuiltin Brational             -> mk_type "rational"            None             []
  | Tbuiltin Bdate                 -> mk_type "date"                None             []
  | Tbuiltin Bduration             -> mk_type "duration"            None             []
  | Tbuiltin Btimestamp            -> mk_type "timestamp"           None             []
  | Tbuiltin Bstring               -> mk_type "string"              None             []
  | Tbuiltin Baddress              -> mk_type "address"             None             []
  | Tbuiltin Bcurrency             -> mk_type "currency"            None             []
  | Tbuiltin Bsignature            -> mk_type "signature"           None             []
  | Tbuiltin Bkey                  -> mk_type "key"                 None             []
  | Tbuiltin Bkeyhash              -> mk_type "keyHash"             None             []
  | Tbuiltin Bbytes                -> mk_type "bytes"               None             []
  | Tbuiltin Bnat                  -> mk_type "nat"                 None             []
  | Tbuiltin Bchainid              -> mk_type "chain_id"            None             []
  | Tbuiltin Bbls12_381_fr         -> mk_type "bls12_381_fr"        None             []
  | Tbuiltin Bbls12_381_g1         -> mk_type "bls12_381_g1"        None             []
  | Tbuiltin Bbls12_381_g2         -> mk_type "bls12_381_g2"        None             []
  | Tbuiltin Bnever                -> mk_type "never"               None             []
  | Tbuiltin Bchest                -> mk_type "chest"               None             []
  | Tbuiltin Bchest_key            -> mk_type "chest_key"           None             []
  | Tcontainer (t, Collection)     -> mk_type "collection"          None             [for_type t]
  | Tcontainer (t, Aggregate)      -> mk_type "aggregate"           None             [for_type t]
  | Tcontainer (t, Partition)      -> mk_type "partition"           None             [for_type t]
  | Tcontainer (t, AssetContainer) -> mk_type "asset_container"     None             [for_type t]
  | Tcontainer (t, AssetKey)       -> mk_type "asset_key"           None             [for_type t]
  | Tcontainer (t, AssetValue)     -> mk_type "asset_value"         None             [for_type t]
  | Tcontainer (t, View)           -> mk_type "asset_view"          None             [for_type t]
  | Tlist t                        -> mk_type "list"                None             [for_type t]
  | Toption t                      -> mk_type "option"              None             [for_type t]
  | Ttuple tl                      -> mk_type "tuple"               None             (List.map for_type tl)
  | Tset t                         -> mk_type "set"                 None             [for_type t]
  | Tmap (kt, vt)                  -> mk_type "map"                 None             [for_type kt; for_type vt]
  | Tbig_map (kt, vt)              -> mk_type "big_map"             None             [for_type kt; for_type vt]
  | Titerable_big_map (kt, vt)     -> mk_type "iterable_big_map"    None             [for_type kt; for_type vt]
  | Tor (lt, rt)                   -> mk_type "or"                  None             [for_type lt; for_type rt]
  | Trecord id                     -> mk_type "record"             (Some (unloc id)) []
  | Tevent id                      -> mk_type "event"              (Some (unloc id)) []
  | Tlambda (at, rt)               -> mk_type "lambda"              None             [for_type at; for_type rt]
  | Tunit                          -> mk_type "unit"                None             []
  | Toperation                     -> mk_type "operation"           None             []
  | Tcontract t                    -> mk_type "contract"            None             [for_type t]
  | Tticket t                      -> mk_type "ticket"              None             [for_type t]
  | Tsapling_state _n              -> mk_type "sapling_state"       None             []
  | Tsapling_transaction _n        -> mk_type "sapling_transaction" None             []
  | Tstorage                       -> assert false
  | Tprog _                        -> assert false
  | Tvset _                        -> assert false
  | Ttrace _                       -> assert false

let for_parameter (p : M.parameter) : parameter =
  mk_parameter (unloc p.name) (for_type p.typ) p.const None

let for_argument (a: M.argument) : argument =
  mk_argument (unloc (Tools.proj3_1 a)) (for_type (Tools.proj3_2 a))

let for_entrypoint (fs : M.function_struct) : decl_entrypoint =
  mk_entrypoint (unloc fs.name) (List.map for_argument fs.args)

let for_getter (fs, rt : M.function_struct * M.type_) : decl_getter =
  mk_getter (unloc fs.name) (List.map for_argument fs.args) (for_type rt)

let model_to_contract_interface (model : M.model) : contract_interface =
  let parameters = List.map for_parameter model.parameters in
  let types = [] in
  let entrypoints = List.map for_entrypoint (List.fold_right (fun (x : M.function__) accu -> match x.node with | Entry fs -> fs::accu | _ -> accu) model.functions [])  in
  let getters = List.map for_getter (List.fold_right (fun (x : M.function__) accu -> match x.node with | Getter (fs, r) -> (fs, r)::accu | _ -> accu) model.functions [])  in
  let errors = [] in
  mk_contract_interface (unloc model.name) parameters types entrypoints getters errors

let model_to_contract_interface_json (model : M.model) : string =
  let ci = model_to_contract_interface model in
  Format.asprintf "%s\n" (Yojson.Safe.to_string (contract_interface_to_yojson ci))
