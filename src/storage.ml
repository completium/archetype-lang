open Ident
open Model

type storage_policy = Record | Flat
[@@deriving show {with_path = false}]

let storage_policy = ref Record
(*let storage_policy = ref Flat*)

type require =
  | Membership
[@@deriving show {with_path = false}]

type ensure =
  | Remove (*  ensures  { forall x:mile. mem x s.miles <-> (mem x (old s).miles /\ x <> m) } *)
  | Invariant
  | Sum
  | Min
  | Max
[@@deriving show {with_path = false}]

type storage_field_operation_type =
  | Get
  | Set
  | Add
  | Addnofail
  | Remove
  | Removenofail
  | Addasset     of lident
  | Removeasset  of lident
[@@deriving show {with_path = false}]

type storage_field_operation = {
  name     : lident;
  typ      : storage_field_operation_type;
  requires : require list;
  ensures  : ensure list;
}
[@@deriving show {with_path = false}]

type storage_field_type =
  | Fbasic  of vtyp
  | Frecord of ident
  | Fenum   of ident
  | Flist   of storage_field_type
  | Fset    of storage_field_type
  | Fmap    of vtyp * storage_field_type
  | Ftuple  of storage_field_type list
  | Flambda of storage_field_type * storage_field_type
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) record_field = {
    asset   : 'id option;
    name    : 'id;
    typ     : storage_field_type;
    ghost   : bool;
    default : 'term option; (* initial value *)
    ops     : storage_field_operation list;
    loc     : Location.t [@opaque]
  }
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) record = {
  fields      : ('id, 'typ, 'term) record_field list;
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
