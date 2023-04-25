open Location

module M = Model
module T = Michelson

type micheline = {
  prim:     string option;
  int:      string option;
  bytes:    string option;
  string:   string option;
  args:     micheline list;
  annots:   string list;
  array:    micheline list;
  var_id:   string option;
  var_type: micheline option;
}
[@@deriving yojson, show {with_path = false}]

type type_ = {
  node: string;
  name: string option;
  int_value: int option;
  args: type_ list;
}
[@@deriving yojson, show {with_path = false}]

type parameter = {
  name: string;
  type_: type_  [@key "type"];
  const: bool;
  default: micheline option;
}
[@@deriving yojson, show {with_path = false}]

type argument = {
  name: string;
  type_: type_  [@key "type"];
}
[@@deriving yojson, show {with_path = false}]

type decl_asset_field = {
  name: string;
  type_: type_  [@key "type"];
  is_key: bool;
}
[@@deriving yojson, show {with_path = false}]

type decl_record_field = {
  name: string;
  type_: type_  [@key "type"];
}
[@@deriving yojson, show {with_path = false}]

type decl_asset = {
  name: string;
  container_kind: string;
  fields: decl_asset_field list;
  container_type_michelson: micheline;
  key_type_michelson: micheline;
  value_type_michelson: micheline;
}
[@@deriving yojson, show {with_path = false}]

type decl_record = {
  name: string;
  fields: decl_record_field list;
  type_michelson: micheline;
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
  type_michelson: micheline;
}
[@@deriving yojson, show {with_path = false}]

type decl_event = {
  name: string;
  fields: decl_record_field list;
  type_michelson: micheline;
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

type type_micheline = {
  value: micheline;
  is_storable: bool;
}
[@@deriving yojson, show {with_path = false}]

type decl_fun_ret = {
  name: string;
  args: argument list;
  return: type_;
  return_michelson: type_micheline;
}
[@@deriving yojson, show {with_path = false}]

type error_struct = {
  kind: string;
  args: string list;
  expr: micheline;
}
[@@deriving yojson, show {with_path = false}]

type contract_interface = {
  name : string;
  parameters: parameter list;
  types: decl_type;
  storage: decl_storage list;
  storage_type: type_micheline;
  entrypoints: decl_entrypoint list;
  getters: decl_fun_ret list;
  views: decl_fun_ret list;
  errors: error_struct list;
}
[@@deriving yojson, show {with_path = false}]

let mk_type node name int_value args : type_ =
  { node; name; int_value; args }

let decl_type assets records enums events =
  { assets; records; enums; events }

let mk_decl_asset_field name type_ is_key : decl_asset_field =
  { name; type_; is_key }

let mk_decl_record_field name type_ : decl_record_field =
  { name; type_ }

let mk_decl_asset name container_kind fields container_type_michelson key_type_michelson value_type_michelson : decl_asset =
  { name; container_kind; fields; container_type_michelson; key_type_michelson; value_type_michelson }

let mk_decl_record name fields type_michelson : decl_record =
  { name; fields; type_michelson }

let mk_decl_constructor name types : decl_constructor =
  { name; types }

let mk_decl_enum name constructors type_michelson : decl_enum =
  { name; constructors; type_michelson }

let mk_decl_event name fields type_michelson : decl_event =
  { name; fields; type_michelson }

let mk_decl_type assets enums records events  =
  { assets; enums; records; events }

let mk_storage name type_ const : decl_storage =
  { name; type_; const }

let mk_argument name type_ : argument =
  { name; type_ }

let mk_entrypoint name args : decl_entrypoint =
  { name; args }

let mk_type_micheline value is_storable : type_micheline =
  { value; is_storable }

let mk_decl_fun_ret name args return return_michelson : decl_fun_ret =
  { name; args; return; return_michelson }

let mk_parameter name type_ const default : parameter =
  { name; type_; const; default }

let mk_error_struct ?(args = []) kind expr : error_struct =
  { kind; args; expr }

let mk_contract_interface name parameters types storage storage_type entrypoints getters views errors : contract_interface =
  { name; parameters; types; storage; storage_type; entrypoints; getters; views; errors }

let rec for_type (t : M.type_) : type_ =
  match M.get_ntype t with
  | Tasset id                                   -> mk_type "asset"               (Some (M.printable_mident id)) None    []
  | Tenum id                                    -> mk_type "enum"                (Some (M.printable_mident id)) None    []
  | Tstate                                      -> mk_type "state"                None                      None    []
  | Tbuiltin Bunit                              -> mk_type "unit"                 None                      None    []
  | Tbuiltin Bbool                              -> mk_type "bool"                 None                      None    []
  | Tbuiltin Bint                               -> mk_type "int"                  None                      None    []
  | Tbuiltin Brational                          -> mk_type "rational"             None                      None    []
  | Tbuiltin Bdate                              -> mk_type "date"                 None                      None    []
  | Tbuiltin Bduration                          -> mk_type "duration"             None                      None    []
  | Tbuiltin Btimestamp                         -> mk_type "timestamp"            None                      None    []
  | Tbuiltin Bstring                            -> mk_type "string"               None                      None    []
  | Tbuiltin Baddress                           -> mk_type "address"              None                      None    []
  | Tbuiltin Btez                               -> mk_type "currency"             None                      None    []
  | Tbuiltin Bsignature                         -> mk_type "signature"            None                      None    []
  | Tbuiltin Bkey                               -> mk_type "key"                  None                      None    []
  | Tbuiltin Bkeyhash                           -> mk_type "key_hash"             None                      None    []
  | Tbuiltin Bbytes                             -> mk_type "bytes"                None                      None    []
  | Tbuiltin Bnat                               -> mk_type "nat"                  None                      None    []
  | Tbuiltin Bchainid                           -> mk_type "chain_id"             None                      None    []
  | Tbuiltin Bbls12_381_fr                      -> mk_type "bls12_381_fr"         None                      None    []
  | Tbuiltin Bbls12_381_g1                      -> mk_type "bls12_381_g1"         None                      None    []
  | Tbuiltin Bbls12_381_g2                      -> mk_type "bls12_381_g2"         None                      None    []
  | Tbuiltin Bnever                             -> mk_type "never"                None                      None    []
  | Tbuiltin Bchest                             -> mk_type "chest"                None                      None    []
  | Tbuiltin Bchest_key                         -> mk_type "chest_key"            None                      None    []
  | Tcontainer ((Tasset an, _), Collection)     -> mk_type "collection"          (Some (M.printable_mident an)) None    []
  | Tcontainer ((Tasset an, _), Aggregate)      -> mk_type "aggregate"           (Some (M.printable_mident an)) None    []
  | Tcontainer ((Tasset an, _), Partition)      -> mk_type "partition"           (Some (M.printable_mident an)) None    []
  | Tcontainer ((Tasset an, _), AssetContainer) -> mk_type "asset_container"     (Some (M.printable_mident an)) None    []
  | Tcontainer ((Tasset an, _), AssetKey)       -> mk_type "asset_key"           (Some (M.printable_mident an)) None    []
  | Tcontainer ((Tasset an, _), AssetValue)     -> mk_type "asset_value"         (Some (M.printable_mident an)) None    []
  | Tcontainer ((Tasset an, _), View)           -> mk_type "asset_view"          (Some (M.printable_mident an)) None    []
  | Tcontainer (t, Collection)                  -> mk_type "collection"           None                      None    [for_type t]
  | Tcontainer (t, Aggregate)                   -> mk_type "aggregate"            None                      None    [for_type t]
  | Tcontainer (t, Partition)                   -> mk_type "partition"            None                      None    [for_type t]
  | Tcontainer (t, AssetContainer)              -> mk_type "asset_container"      None                      None    [for_type t]
  | Tcontainer (t, AssetKey)                    -> mk_type "asset_key"            None                      None    [for_type t]
  | Tcontainer (t, AssetValue)                  -> mk_type "asset_value"          None                      None    [for_type t]
  | Tcontainer (t, View)                        -> mk_type "asset_view"           None                      None    [for_type t]
  | Tlist t                                     -> mk_type "list"                 None                      None    [for_type t]
  | Toption t                                   -> mk_type "option"               None                      None    [for_type t]
  | Ttuple tl                                   -> mk_type "tuple"                None                      None    (List.map for_type tl)
  | Tset t                                      -> mk_type "set"                  None                      None    [for_type t]
  | Tmap (kt, vt)                               -> mk_type "map"                  None                      None    [for_type kt; for_type vt]
  | Tbig_map (kt, vt)                           -> mk_type "big_map"              None                      None    [for_type kt; for_type vt]
  | Titerable_big_map (kt, vt)                  -> mk_type "iterable_big_map"     None                      None    [for_type kt; for_type vt]
  | Tor (lt, rt)                                -> mk_type "or"                   None                      None    [for_type lt; for_type rt]
  | Trecord id                                  -> mk_type "record"              (Some (M.printable_mident id)) None    []
  | Tevent id                                   -> mk_type "event"               (Some (M.printable_mident id)) None    []
  | Tlambda (at, rt)                            -> mk_type "lambda"               None                      None    [for_type at; for_type rt]
  | Tunit                                       -> mk_type "unit"                 None                      None    []
  | Toperation                                  -> mk_type "operation"            None                      None    []
  | Tcontract t                                 -> mk_type "contract"             None                      None    [for_type t]
  | Tticket t                                   -> mk_type "ticket"               None                      None    [for_type t]
  | Tsapling_state n                            -> mk_type "sapling_state"        None                     (Some n) []
  | Tsapling_transaction n                      -> mk_type "sapling_transaction"  None                     (Some n) []

type preprocess_obj = {
  params: M.parameter list;
  var_decls: M.decl_node list;
  size : int;
  with_state: bool;
}

let get_var_decls_size (model : M.model) : preprocess_obj =
  let params = model.parameters |> List.filter (fun (x : M.parameter) -> not x.const) in
  let var_decls = model.decls |> List.filter (function | M.Dvar var when (match var.kind with | M.VKconstant -> false | _ -> true) -> true | M.Dasset _ -> true | _ -> false) in
  let s_param = params |> List.length in
  let s_decls = var_decls |> List.length in
  let with_state = List.fold_left (fun accu x -> accu || (match x with | M.Denum ({name = (_, {pldesc = "state"})}) -> true | _ -> false) ) false model.decls in
  let size = s_param + s_decls + (if with_state then 1 else 0) in
  {params; var_decls; size; with_state}

let compute_path idx size : int list =
  if size = 1
  then []
  else [idx]
  (* begin
    let rec aux accu i =
      if i = 0
      then accu @ (if size - idx = 1 then [] else [0])
      else aux (1::accu) (i - 1)
    in
    aux [] idx
  end *)

let for_parameters (po : preprocess_obj) (ps : M.parameter list) : parameter list =
  let start = if po.with_state then 1 else 0 in
  let _, res =
  List.fold_left (
   fun ((i, accu) : int * parameter list) (p : M.parameter) -> begin
    let param = mk_parameter (M.unloc_mident p.name) (for_type p.typ) p.const None in
    (i + (if p.const then 0 else 1), param::accu)
   end) (start, []) ps
  in
  List.rev res

let for_argument (a: M.argument) : argument =
  mk_argument (M.unloc_mident (Tools.proj3_1 a)) (for_type (Tools.proj3_2 a))

let mk_prim p args annots = { prim = Some p; int = None;   bytes = None;  string = None;   args = args; annots = annots; array = []; var_id = None; var_type = None }
let mk_string v           = { prim = None;   int = None;   bytes = None;  string = Some v; args = [];   annots = [];     array = []; var_id = None; var_type = None }
let mk_bytes v            = { prim = None;   int = None;   bytes = Some v; string = None;  args = [];   annots = [];     array = []; var_id = None; var_type = None }
let mk_int v              = { prim = None;   int = Some v; bytes = None;   string = None;  args = [];   annots = [];     array = []; var_id = None; var_type = None }
let mk_array v            = { prim = None;   int = None;   bytes = None;   string = None;  args = [];   annots = [];     array = v;  var_id = None; var_type = None }
let mk_var _v             = { prim = None;   int = None;   bytes = None;   string = None;  args = [];   annots = [];     array = []; var_id = None; var_type = None }

let to_micheline (obj : T.obj_micheline) : micheline =
  let rec aux (obj : T.obj_micheline) =
    match obj with
    | Oprim   p -> mk_prim p.prim (List.map aux p.args) p.annots
    | Ostring v -> mk_string v
    | Obytes  v -> mk_bytes v
    | Oint    v -> mk_int v
    | Oarray  v -> mk_array (List.map aux v)
    | Ovar    v -> mk_var v
  in
  aux obj

let to_michelson_type (model : M.model) (type_michelson : M.type_) : micheline =
  let type_michelson = Gen_michelson.to_type model type_michelson in
  let obj = T.Utils.type_to_micheline type_michelson in
  to_micheline obj

let for_decl_type (model : M.model) (low_model : M.model) (d : M.decl_node) (assets, enums, records, events) =
  let for_asset_item (asset  : M.asset) (x : M.asset_item)     = mk_decl_asset_field (M.unloc_mident x.name) (for_type x.type_) (List.exists (String.equal (M.unloc_mident x.name)) asset.keys) in
  let for_record_field (x : M.record_field) = mk_decl_record_field (M.unloc_mident x.name) (for_type x.type_) in
  let for_enum_item (x : M.enum_item)       = mk_decl_constructor (M.unloc_mident x.name) (List.map for_type x.args) in
  let for_map_kind = function | M.MKMap -> "map" | M.MKBigMap -> "big_map" | M.MKIterableBigMap -> "iterable_big_map" in

  let ft x = to_michelson_type low_model x in
  let for_asset  (asset  : M.asset)  : decl_asset =
    let odasset : M.odel_asset = List.fold_left (fun accu x ->
        match x with
        | M.ODAsset x when M.cmp_mident x.name asset.name-> Some x
        | _-> accu) None low_model.extra.original_decls |> Option.get
    in
    let key_type =
      let an = asset.name in
      let f (t : M.type_) (annot : string) : M.type_ = M.mktype (fst t) ~annot:(dumloc annot) in
      match asset.keys with
      | []    -> assert false
      | [_]   -> ft odasset.key_type
      | ks    -> begin
          let kts = List.map (fun x -> let _, ty, _ = M.Utils.get_asset_field model (an, x) in f ty ("%" ^ x)) ks in
          match List.rev kts with
          | [] -> assert false
          | f::t ->
            let mk_pair x y = mk_prim "pair" [x; y] [] in
            List.fold_left (fun accu x -> mk_pair (ft x) accu) (ft f) t
        end
    in
    let container_type =
      let ct = odasset.container_type in
      match M.get_ntype ct with
      | Tset _ -> mk_prim "set" [key_type] []
      | Tmap (_, vt) -> mk_prim "map" [key_type; ft vt] []
      | Tbig_map (_, vt) -> mk_prim "big_map" [key_type; ft vt] []
      | _ -> ft ct
    in
    let value_type     = ft odasset.value_type in
    mk_decl_asset (M.printable_mident asset.name)  (for_map_kind asset.map_kind) (List.map (for_asset_item asset) asset.values) container_type key_type value_type
  in

  let for_enum (enum : M.enum) : decl_enum   =
    let odasset : M.odel_enum = List.fold_left (fun accu x ->
        match x with
        | M.ODEnum x when M.cmp_mident x.name enum.name-> Some x
        | _-> accu) None low_model.extra.original_decls |> Option.get
    in
    let michelson_type = ft odasset.current_type in
    mk_decl_enum (M.printable_mident enum.name) (List.map for_enum_item enum.values) michelson_type
  in

  let for_record (record : M.record) : decl_record =
    let type_michelson = to_michelson_type model (M.trecord record.name) in
    mk_decl_record (M.printable_mident record.name) (List.map for_record_field record.fields) type_michelson
  in

  let for_event  (event  : M.record) : decl_event =
    let type_michelson = to_michelson_type model (M.tevent event.name) in
    mk_decl_event  (M.printable_mident event.name)  (List.map for_record_field event.fields) type_michelson
  in

  match d with
  | Dvar _       -> (assets, enums, records, events)
  | Denum enum   -> (assets, (for_enum enum)::enums, records, events)
  | Dasset asset -> ((for_asset asset)::assets, enums, records, events)
  | Drecord re   -> (assets, enums, (for_record re)::records, events)
  | Devent re    -> (assets, enums, records, (for_event re)::events)

let for_decl_type (model : M.model) (low_model : M.model) (ds : M.decl_node list) : decl_type =
  let assets, enums, records, events = List.fold_right (for_decl_type model low_model) ds ([], [], [], []) in
  mk_decl_type assets enums records events

let for_decl_node (d : M.decl_node) accu : decl_storage list =
  let for_var (var : M.var) : decl_storage = mk_storage (M.unloc_mident var.name) (for_type var.type_) (match var.kind with | VKconstant -> true | _ -> false) in
  let for_asset (asset : M.asset) : decl_storage = mk_storage (M.unloc_mident asset.name) (mk_type "asset" (Some (M.unloc_mident asset.name)) None []) false in
  match d with
  | Dvar var     -> (for_var var)::accu
  | Denum _      -> accu
  | Dasset asset -> (for_asset asset)::accu
  | Drecord _    -> accu
  | Devent _     -> accu

let for_storage (_model : M.model) (po : preprocess_obj) =
  let start = List.length po.params + if po.with_state then 1 else 0 in
  let (_, res) : int * decl_storage list = List.fold_left
      (fun (i, accu) (x : M.decl_node) ->
         let accu : decl_storage list = for_decl_node x accu in
         (i + 1, accu)
      ) (start, []) po.var_decls in
  let res = List.rev res in
  if po.with_state
  then (mk_storage "_state" (mk_type "int" None None []) false)::res
  else res

let for_entrypoint (fs : M.function_struct) : decl_entrypoint =
  mk_entrypoint (M.unloc_mident fs.name) (List.map for_argument fs.args)

let tz_type_to_type_micheline ty =
  let value = to_micheline (T.Utils.type_to_micheline ty) in
  let is_storable = T.Utils.is_storable ty in
  let tm = mk_type_micheline value is_storable in
  tm

let for_decl_ret model (fs, rt : M.function_struct * M.type_) : decl_fun_ret =
  let ty = Gen_michelson.to_type model rt in
  let tm = tz_type_to_type_micheline ty in
  mk_decl_fun_ret (M.unloc_mident fs.name) (List.map for_argument fs.args) (for_type rt) tm

let for_getter = for_decl_ret
let for_view = for_decl_ret

let for_errors (model : M.model) : error_struct list =
  let mterm_to_micheline (mt : M.mterm) : micheline option =
    let f (mt : M.mterm) =
      match Gen_michelson.to_simple_data model mt with
      | Some v -> (v |> Michelson.Utils.data_to_micheline |> to_micheline |> Option.some)
      | None -> None
    in
    let seek_mterm_from_storevar id : M.mterm option =
      try
        let m : M.var = M.Utils.get_var model id in
        match m.kind with
        | VKconstant -> m.default
        | _ -> None
      with
      | _ -> None
    in

    match mt.node with
    | Mvar (id, Vstorevar) -> (match seek_mterm_from_storevar id with | Some v -> f v | None -> None)
    | _ -> f mt
  in
  let mk_pair a b = mk_prim "Pair" [mk_string a; mk_string b] [] in
  let rec aux (ctx : M.ctx_model) (accu : error_struct list) (mt : M.mterm) : error_struct list =
    match mt.node with
    | Mfail (fv) -> begin
        match fv with
        | Invalid v                -> (match mterm_to_micheline v with | Some v -> (mk_error_struct "Invalid" v)::accu | None -> accu)
        | InvalidCaller            -> (mk_error_struct "InvalidCaller"       (mk_string M.fail_msg_INVALID_CALLER))::accu
        | InvalidSource            -> (mk_error_struct "InvalidSource"       (mk_string M.fail_msg_INVALID_SOURCE))::accu
        | InvalidCondition (id, v) -> let f = mk_error_struct "InvalidCondition" ~args:[id] in (match v with | None -> (f (mk_pair M.fail_msg_INVALID_CONDITION id))::accu | Some v -> (match mterm_to_micheline v with | Some v -> (f v)::accu | None -> accu))
        | NotFound                 -> (mk_error_struct "NotFound"            (mk_string M.fail_msg_NOT_FOUND))::accu
        | AssetNotFound an         -> (mk_error_struct "AssetNotFound"       ~args:[an] (mk_pair M.fail_msg_ASSET_NOT_FOUND an))::accu
        | KeyExists an             -> (mk_error_struct "KeyExists"           ~args:[an] (mk_pair M.fail_msg_KEY_EXISTS an))::accu
        | KeyExistsOrNotFound an   -> (mk_error_struct "KeyExistsOrNotFound" ~args:[an] (mk_pair M.fail_msg_KEY_EXISTS_OR_NOT_FOUND an) )::accu
        | DivByZero                -> (mk_error_struct "DivByZero"           (mk_string M.fail_msg_DIV_BY_ZERO))::accu
        | NatNegAssign             -> (mk_error_struct "NatNegAssign"        (mk_string M.fail_msg_NAT_NEG_ASSIGN))::accu
        | NoTransfer               -> (mk_error_struct "NoTransfer"          (mk_string M.fail_msg_NO_TRANSFER))::accu
        | InvalidState             -> (mk_error_struct "InvalidState"        (mk_string M.fail_msg_INVALID_STATE))::accu
      end
    | Mdeclvaropt (_, _, _, fa, _) -> (match fa with Some v -> (match mterm_to_micheline v with | Some v -> (mk_error_struct "Invalid" v)::accu | None -> accu) | None -> (mk_error_struct "Invalid" (mk_string Model.fail_msg_OPTION_IS_NONE))::accu)
    | _ -> M.fold_term (aux ctx) accu mt
  in
  M.fold_model aux model []

let model_to_contract_interface (model : M.model) (low_model : M.model) (tz : T.michelson) : contract_interface =
  let po = get_var_decls_size model in
  let parameters = for_parameters po model.parameters in
  let types = for_decl_type model low_model model.decls in
  let storage = for_storage model po in
  let storage_type = tz_type_to_type_micheline tz.storage in
  let entrypoints = List.map for_entrypoint (List.fold_right (fun (x : M.function__) accu -> match x.node with | Entry fs -> fs::accu | _ -> accu) model.functions [])  in
  let getters = List.map (for_getter model) (List.fold_right (fun (x : M.function__) accu -> match x.node with | Getter (fs, r) -> (fs, r)::accu | _ -> accu) model.functions [])  in
  let views = List.map (for_view model) (List.fold_right (fun (x : M.function__) accu -> match x.node with | View (fs, r, (VVonchain | VVonoffchain)) -> (fs, r)::accu | _ -> accu) model.functions [])  in
  let errors = for_errors model in
  mk_contract_interface (unloc model.name) parameters types storage storage_type entrypoints getters views errors

let model_to_contract_interface_json (model : M.model) (low_model : M.model) (tz : T.michelson) : string =
  let ci = model_to_contract_interface model low_model tz in
  Format.asprintf "%s\n" (Yojson.Safe.to_string (contract_interface_to_yojson ci))

let rec tz_type_to_type_ (ty : T.type_) : type_=
  let f = tz_type_to_type_ in
  match ty.node with
  | Taddress               -> mk_type "address"              ty.annotation None []
  | Tbig_map (k, v)        -> mk_type "big_map"              ty.annotation None [f k; f v]
  | Tbool                  -> mk_type "bool"                 ty.annotation None []
  | Tbytes                 -> mk_type "bytes"                ty.annotation None []
  | Tchain_id              -> mk_type "chain_id"             ty.annotation None []
  | Tcontract t            -> mk_type "contract"             ty.annotation None [f t]
  | Tint                   -> mk_type "int"                  ty.annotation None []
  | Tkey                   -> mk_type "key"                  ty.annotation None []
  | Tkey_hash              -> mk_type "key_hash"             ty.annotation None []
  | Tlambda (a, r)         -> mk_type "lambda"               ty.annotation None [f a; f r]
  | Tlist t                -> mk_type "list"                 ty.annotation None [f t]
  | Tmap (k, v)            -> mk_type "map"                  ty.annotation None [f k; f v]
  | Tmutez                 -> mk_type "tez"                  ty.annotation None []
  | Tnat                   -> mk_type "nat"                  ty.annotation None []
  | Toperation             -> mk_type "operation"            ty.annotation None []
  | Toption t              -> mk_type "option"               ty.annotation None [f t]
  | Tor (l, r)             -> mk_type "or"                   ty.annotation None [f l; f r]
  | Tpair l                -> mk_type "tuple"                ty.annotation None (List.map f l)
  | Tset t                 -> mk_type "set"                  ty.annotation None [f t]
  | Tsignature             -> mk_type "signature"            ty.annotation None []
  | Tstring                -> mk_type "string"               ty.annotation None []
  | Ttimestamp             -> mk_type "date"                 ty.annotation None []
  | Tunit                  -> mk_type "unit"                 ty.annotation None []
  | Tticket t              -> mk_type "ticket"               ty.annotation None [f t]
  | Tsapling_transaction n -> mk_type "sapling_transaction"  ty.annotation (Some n) []
  | Tsapling_state n       -> mk_type "sapling_state"        ty.annotation (Some n) []
  | Tbls12_381_g1          -> mk_type "bls12_381_g1"         ty.annotation None []
  | Tbls12_381_g2          -> mk_type "bls12_381_g2"         ty.annotation None []
  | Tbls12_381_fr          -> mk_type "bls12_381_fr"         ty.annotation None []
  | Tnever                 -> mk_type "never"                ty.annotation None []
  | Tchest                 -> mk_type "chest"                ty.annotation None []
  | Tchest_key             -> mk_type "chest_key"            ty.annotation None []

let tz_type_to_args (ty : T.type_) : argument list=
  match ty with
  | {node = T.Tunit} -> []
  | ty -> [mk_argument "_" (tz_type_to_type_ ty) ]

let remove_percent str = if String.length str > 1 && String.get str 0 = '%' then String.sub str 1 (String.length str - 1) else str

let extract_storage (storage : T.type_) : decl_storage list =
  let split (obj : T.type_) : (string * T.type_) list =
    let rec aux (obj : T.type_) : (string * T.type_ ) list =
      match obj with
      | {annotation = Some a} -> [a, obj]
      | {node = T.Tpair l; _ } -> begin
          let l = List.map aux l |> List.flatten in
          if List.length l > 0
          then l
          else ["_", obj]
        end
      | _ -> []
    in

    match obj with
    | {node = T.Tpair _ } when List.length (aux obj) > 0 -> aux obj
    | {annotation = Some _} -> aux obj
    | _ -> ["%default", obj]
  in
  split storage
  |> List.map (fun (id, ty) -> mk_storage (remove_percent id) (tz_type_to_type_ ty) false)

let extract_entypoint (parameter : T.type_) : decl_entrypoint list =
  let split (obj : T.type_) : (string * T.type_) list =
    let rec aux (obj : T.type_) : (string * T.type_) list =
      match obj with
      | {node = T.Tor (p, r); _ } -> [aux p; aux r] |> List.flatten
      | {annotation = Some a} -> [a, obj]
      | _ -> []
    in

    match obj with
    | {node = T.Tor _ } -> aux obj
    | {annotation = Some _} -> aux obj
    | _ -> ["%default", obj]
  in
  split parameter
  |> List.map (fun (id, ty) -> mk_entrypoint (remove_percent id) (tz_type_to_args ty))

let tz_to_contract_interface (tz, env : T.michelson * Gen_decompile.env) : contract_interface =
  let parameters = [] in
  let types = mk_decl_type [] [] [] [] in
  let storage = extract_storage tz.storage in
  let entrypoints = extract_entypoint tz.parameter in
  let getters = [] in
  let views = List.map (fun (v : T.view_struct) ->
      let name = v.id in
      let args = tz_type_to_args v.param in
      let ret  = tz_type_to_type_ v.ret in
      let return_michelson = tz_type_to_type_micheline v.ret in
      mk_decl_fun_ret name args ret return_michelson
    ) tz.views in
  let errors = [] in
  let storage_type = tz_type_to_type_micheline tz.storage in
  mk_contract_interface env.name parameters types storage storage_type entrypoints getters views errors

let tz_to_contract_interface_json (tz, env : T.michelson * Gen_decompile.env) : string =
  let ci = tz_to_contract_interface (tz, env) in
  Format.asprintf "%s\n" (Yojson.Safe.to_string (contract_interface_to_yojson ci))
