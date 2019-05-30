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
  loc     : Location.t [@opaque]
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) storage_item = {
  name        : 'id;
  fields      : ('id, 'typ, 'term) item_field list;
  invariants  : ('id, 'id lterm_gen) label_term list;
  init        : ((ident * 'term) list) list;
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) storage = ('id, 'typ, 'term) storage_item list
[@@deriving show {with_path = false}]

type 'id enum = {
  name: enum_ident;
  values: enum_value_ident list;
}
[@@deriving show {with_path = false}]

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

type 'id function_ = {
  name: fun_ident;
}
[@@deriving show {with_path = false}]

type 'id entry = {
  name: entry_ident;
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) function_struct = {
  name: 'id;
  args: 'id * 'typ * 'term option;

}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) function_node =
  | Function           of ('id, 'typ, 'term) function_struct
  | Entry              of ('id, 'typ, 'term) function_struct
  | Get
  | Set
  | Make
  | AddAsset           of asset_ident
  | RemoveAsset        of asset_ident
  | UpdateAsset        of asset_ident
  | ContainsAsset      of asset_ident
  | SelectAsset        of asset_ident
  | CountAsset         of asset_ident
  | AddContainer       of asset_ident * field_ident * Model.container
  | RemoveContainer    of asset_ident * field_ident * Model.container
  | ContainsContainer  of asset_ident * field_ident * Model.container
  | SelectContainer    of asset_ident * field_ident * Model.container
  | CountContainer     of asset_ident * field_ident * Model.container
  | Other
[@@deriving show {with_path = false}]

type ('id) item_field_operation = {
  name     : lident;
  typ      : 'id item_field_operation_type;
}
[@@deriving show {with_path = false}]

type function_ = {
  node: function_node;
}

type ('id, 'typ, 'term) type_node =
  | TNenum of 'id enum
  | TNrecord of ('id, 'typ) record
  | TNstorage of ('id, 'typ, 'term) storage
  | TNfunction of function_
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) model = ('id, 'typ, 'term) type_node list;
[@@deriving show {with_path = false}]
