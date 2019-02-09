open Location
open Model

type require =
  | Membership

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
    value : pterm; (* initial value *)
    ops   : storage_field_operation list;
  }

type storage = {
    fields     : storage_field list;
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

let mk_storage_fields (asset : asset)  =
  let name = (unloc asset).name in
  List.fold_left (fun acc arg ->
      acc @ [mk_storage_field name arg]
    ) [] (unloc asset).args

let mk_storage m =
  let fields =
    List.fold_left (fun acc asset ->
        acc @ (mk_storage_fields asset)
      ) [] m.assets in
  { empty_storage with fields = fields }

let model_to_modelws (m : model) : model_with_storage = {
    name = (unloc m).name;
    storage = mk_storage (unloc m);
    transactions = [];
  }
