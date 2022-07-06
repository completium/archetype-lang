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

type decl_field = {
  name: string;
  type_: type_  [@key "type"];
  is_key: bool;
}
[@@deriving yojson, show {with_path = false}]

type decl_asset = {
  name: string;
  container_kind: string;
  fields: decl_field list;
}
[@@deriving yojson, show {with_path = false}]

type decl_record = {
  name: string;
  fields: decl_field list;
}
[@@deriving yojson, show {with_path = false}]

type decl_constructor = {
  name: string;
  types: type_ list;
}
[@@deriving yojson, show {with_path = false}]
type decl_enum = {
  name: string;
  constructors: decl_constructor list;
}
[@@deriving yojson, show {with_path = false}]

type decl_event = {
  name: string;
  fields: decl_field list;
}
[@@deriving yojson, show {with_path = false}]

type decl_type = {
  assets:  decl_asset list;
  enums:   decl_enum list;
  records: decl_record list;
  events:  decl_event list;
}
[@@deriving yojson, show {with_path = false}]

type decl_storage = {
  name: string;
  type_: type_  [@key "type"];
  const: bool;
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
  types: decl_type;
  storage: decl_storage list;
  entrypoints: decl_entrypoint list;
  getters: decl_getter list;
  errors: expression list;
}
[@@deriving yojson, show {with_path = false}]

let mk_type node name args : type_ =
  { node; name; args }

let decl_type assets records enums events =
  { assets; records; enums; events }

let mk_decl_field name type_ is_key : decl_field =
  { name; type_; is_key }

let mk_decl_asset name container_kind fields : decl_asset =
  { name; container_kind; fields }

let mk_decl_record name fields : decl_record =
  { name; fields }

let mk_decl_constructor name types : decl_constructor =
  { name; types }

let mk_decl_enum name constructors : decl_enum =
  { name; constructors }

let mk_decl_event name fields : decl_event =
  { name; fields }

let mk_decl_type assets enums records events =
  { assets; enums; records; events }

let mk_storage name type_ const : decl_storage =
  { name; type_; const }

let mk_argument name type_ : argument =
  { name; type_ }

let mk_entrypoint name args : decl_entrypoint =
  { name; args }

let mk_getter name args return : decl_getter =
  { name; args; return }

let mk_parameter name type_ const default : parameter =
  { name; type_; const; default }

let mk_contract_interface name parameters types storage entrypoints getters errors : contract_interface =
  { name; parameters; types; storage; entrypoints; getters; errors }

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

let for_decl_type (d : M.decl_node) (assets, enums, records, events) =
  let for_asset_item (asset  : M.asset) (x : M.asset_item)     = mk_decl_field (unloc x.name) (for_type x.type_) (List.exists (String.equal (unloc x.name)) asset.keys) in
  let for_record_field (x : M.record_field) = mk_decl_field (unloc x.name) (for_type x.type_) false in
  let for_enum_item (x : M.enum_item)       = mk_decl_constructor (unloc x.name) (List.map for_type x.args) in
  let for_map_kind = function | M.MKMap -> "map" | M.MKBigMap -> "big_map" | M.MKIterableBigMap -> "iterable_big_map" in

  let for_asset  (asset  : M.asset)  : decl_asset  = mk_decl_asset  (unloc asset.name)  (for_map_kind asset.map_kind) (List.map (for_asset_item asset) asset.values) in
  let for_enum   (enum   : M.enum)   : decl_enum   = mk_decl_enum   (unloc enum.name)   (List.map for_enum_item enum.values) in
  let for_record (record : M.record) : decl_record = mk_decl_record (unloc record.name) (List.map for_record_field record.fields) in
  let for_event  (event  : M.record) : decl_event  = mk_decl_event  (unloc event.name)  (List.map for_record_field event.fields) in

  match d with
  | Dvar _       -> (assets, enums, records, events)
  | Denum enum   -> (assets, (for_enum enum)::enums, records, events)
  | Dasset asset -> ((for_asset asset)::assets, enums, records, events)
  | Drecord re   -> (assets, enums, (for_record re)::records, events)
  | Devent re    -> (assets, enums, records, (for_event re)::events)

let for_decl_type (ds : M.decl_node list) : decl_type =
  let assets, enums, records, events = List.fold_right for_decl_type ds ([], [], [], []) in
  mk_decl_type assets enums records events

let for_storage (d : M.decl_node) accu =
  let for_var (var : M.var) : decl_storage = mk_storage (unloc var.name) (for_type var.type_) (match var.kind with | VKconstant -> true | _ -> false) in
  let for_asset (asset : M.asset) : decl_storage = mk_storage (unloc asset.name) (mk_type "asset" (Some (unloc asset.name)) []) false in
  match d with
  | Dvar var     -> (for_var var)::accu
  | Denum _      -> accu
  | Dasset asset -> (for_asset asset)::accu
  | Drecord _    -> accu
  | Devent _     -> accu

let for_entrypoint (fs : M.function_struct) : decl_entrypoint =
  mk_entrypoint (unloc fs.name) (List.map for_argument fs.args)

let for_getter (fs, rt : M.function_struct * M.type_) : decl_getter =
  mk_getter (unloc fs.name) (List.map for_argument fs.args) (for_type rt)

let model_to_contract_interface (model : M.model) : contract_interface =
  let parameters = List.map for_parameter model.parameters in
  let types = for_decl_type model.decls in
  let storage = List.fold_right for_storage model.decls [] in
  let entrypoints = List.map for_entrypoint (List.fold_right (fun (x : M.function__) accu -> match x.node with | Entry fs -> fs::accu | _ -> accu) model.functions [])  in
  let getters = List.map for_getter (List.fold_right (fun (x : M.function__) accu -> match x.node with | Getter (fs, r) -> (fs, r)::accu | _ -> accu) model.functions [])  in
  let errors = [] in
  mk_contract_interface (unloc model.name) parameters types storage entrypoints getters errors

let model_to_contract_interface_json (model : M.model) : string =
  let ci = model_to_contract_interface model in
  Format.asprintf "%s\n" (Yojson.Safe.to_string (contract_interface_to_yojson ci))
