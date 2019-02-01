open Model

type require =
  | Membership
  | UserDefined of lterm (* comes from asset invariant *)

type ensure =
  | Remove (*  ensures  { forall x:mile. mem x s.miles <-> (mem x (old s).miles /\ x <> m) } *)
  | Invariant
  | Sum
  | Min
  | Max

type storage_field_operation_type =
  | Get
  | Set
  | Add
  | Remove

type storage_field_operation = {
    typ      : storage_field_operation_type;
    requires : require list;
    ensures  : ensure list;
  }

type storage_field_type =
  | KeySet   of ident
  | ValueMap of vtyp (* field type *)
  | CollMap  of ident (* collection asset ident*)

type storage_field = {
    asset : ident;
    name  : ident;
    typ   : storage_field_type;
    ops   : storage_field_operation list;
  }

type storage = storage_field list

type transaction = {
    name         : ident;
    args         : arg list;
    requires     : require list;
    ensures      : ensure list;
    action       : pterm option;
}

type model_with_storage = {
    name : ident;
    storage : storage;
    transactions : transaction list;
  }

let model_to_modelws (m : model) : model_with_storage = {
    name = m.name;
    storage = [];
    transactions = [];
  }
