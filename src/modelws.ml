open Location
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
  | KeySet   of lident
  | ValueMap of vtyp (* field type *)
  | CollMap  of lident (* collection asset lident*)

type storage_field = {
    asset : lident;
    name  : lident;
    typ   : storage_field_type;
    ghost : bool;
    ops   : storage_field_operation list;
  }

type storage = {
    fields : storage_field list;
    invariants : lterm list;
  }

let empty_storage = { fields = []; invariants = [] }

type transaction_unloc = {
    name         : lident;
    args         : arg list;
    requires     : require list;
    spec         : specification option;
}

type transaction = transaction_unloc loced

type model_with_storage = {
    name : lident;
    storage : storage;
    transactions : transaction list;
  }

let model_to_modelws (m : model) : model_with_storage = {
    name = (unloc m).name;
    storage = empty_storage;
    transactions = [];
  }
