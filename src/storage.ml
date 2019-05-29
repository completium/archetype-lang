open Ident
open Model

type field_ident = string
type asset_ident = string
type enum_ident = string
type enum_value_ident = string

type field =
  | Fid of string
  | Fall

type storage_policy =
  | Record
  | Flat
  | Hybrid of (asset_ident * field) list
[@@deriving show {with_path = false}]

type execution_mode =
  | WithSideEffect
  | WithoutSideEffect
[@@deriving show {with_path = false}]

type sorting_policy =
  | OnTheFly
  | OnChange
  | None
[@@deriving show {with_path = false}]

let storage_policy = ref Record
(*let storage_policy = ref Flat*)
let execution_mode = ref WithSideEffect
let sorting_policy = ref OnTheFly


type ('id) item_field_operation_type =
  | Get
  | Set
  | AddContainer of Model.container
  | RemoveContainer of Model.container
  | Addnofail
  | Remove
  | Removenofail
  | AddAsset     of 'id
  | RemoveAsset  of 'id
[@@deriving show {with_path = false}]

type ('id) item_field_operation = {
  name     : lident;
  typ      : 'id item_field_operation_type;
}
[@@deriving show {with_path = false}]

type ('id) item_field_type =
  | FBasic            of vtyp
  | FKeyCollection    of 'id * vtyp
  | FRecordMap        of asset_ident
  | FRecordCollection of 'id
  | FEnum             of 'id
  | FContainer        of Model.container * 'id item_field_type
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) item_field = {
  asset   : 'id option;
  name    : 'id;
  typ     : 'id item_field_type;
  ghost   : bool;
  default : 'term option; (* initial value *)
  ops     : 'id item_field_operation list;
  loc     : Location.t [@opaque]
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) storage_item = {
  name        : 'id;
  fields      : ('id, 'typ, 'term) item_field list;
  operations  : 'id item_field_operation list;
  invariants  : ('id, 'id lterm_gen) label_term list;
  init        : ((ident * 'term) list) list;
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) storage = ('id, 'typ, 'term) storage_item list;
[@@deriving show {with_path = false}]

type 'id enum = {
  name: enum_ident;
  values: enum_value_ident;
}

type ('id, 'typ) record_item = {
  name: enum_ident;
  type_: 'typ;
}
[@@deriving show {with_path = false}]

type 'id record = {
  name: enum_ident;
  values: record_item list;
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) type_node =
  | TNenum of 'id enum
  | TNrecord of ('id, 'typ) record
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) model = {
  tnodes: ('id, 'typ, 'term) type_node list;
  storage: ('id, 'typ, 'term) storage;
}
[@@deriving show {with_path = false}]
