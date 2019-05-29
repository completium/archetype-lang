open Ident
open Model

type field_ident = string
type asset_ident = string
type enum_ident = string

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


type ('id) storage_field_operation_type =
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

type ('id) storage_field_operation = {
  name     : lident;
  typ      : 'id storage_field_operation_type;
}
[@@deriving show {with_path = false}]

type ('id) storage_field_type =
  | FBasic            of vtyp
  | FKeyCollection    of 'id * vtyp
  | FRecordMap        of asset_ident
  | FEnum             of 'id
  | FContainer        of Model.container * 'id storage_field_type
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) record_field = {
  asset   : 'id option;
  name    : 'id;
  typ     : 'id storage_field_type;
  ghost   : bool;
  default : 'term option; (* initial value *)
  ops     : 'id storage_field_operation list;
  loc     : Location.t [@opaque]
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) record = {
  name        : 'id;
  fields      : ('id, 'typ, 'term) record_field list;
  operations  : 'id storage_field_operation list;
  invariants  : ('id, 'id lterm_gen) label_term list;
  init        : ((ident * 'term) list) list;
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) storage = ('id, 'typ, 'term) record list;
[@@deriving show {with_path = false}]
