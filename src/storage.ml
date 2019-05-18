open Ident
open Model

type storage_policy =
  | Record
  | Flat
  | Hybrid of (string * string) list
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


type 'id storage_field_operation_type =
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

type 'id storage_field_operation = {
  name     : lident;
  typ      : 'id storage_field_operation_type;
}
[@@deriving show {with_path = false}]

type 'id storage_field_type =
  | FBasic            of vtyp
  | FKeyCollection    of 'id * vtyp
  | FRecordCollection of 'id
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

type ('id, 'typ, 'term)  model_with_storage = {
  name          : 'id;
  enums         : ('id, 'typ, 'term) enum_struct list;
  storage       : ('id, 'typ, 'term) record list;
  functions     : ('id, 'typ, 'term) function_struct list;
  transactions  : ('id, 'typ, 'term) transaction_struct list;
  verifications : ('id, 'typ, 'term) verification list;
  loc           : Location.t [@opaque];
}
[@@deriving show {with_path = false}]
