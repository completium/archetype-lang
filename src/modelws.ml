open Ident
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
  | Var      of vtyp
  | KeySet   of lident * vtyp
  | ValueMap of vtyp (* field type *)
  | CollMap  of lident * vtyp (* collection asset lident*)

type storage_field = {
    asset   : lident;
    name    : lident;
    typ     : storage_field_type;
    ghost   : bool;
    default : bval option; (* initial value *)
    ops     : storage_field_operation list;
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

(* type mapping exceptions : asset name, field name, type location *)
exception InvalidKeyType of lident * lident * Location.t
exception UnsupportedType of lident * lident * Location.t

(* asset field type to storage field type *)
let aft_to_sft aname fname iskey (typ : ptyp) =
  let loc = loc typ in
  let typ = unloc typ in
  match iskey, typ with
  | true, Tbuiltin typ -> KeySet (aname,typ)
  | true, _ -> raise (InvalidKeyType (aname,fname,loc))
  | false, Tbuiltin typ -> ValueMap typ
  (* what is the vtyp of the asset ? *)
  | false, Tasset _ -> ValueMap VTint
  | false, Tcontainer (ptyp,_) ->
     begin
     match unloc ptyp with
     (* what is the vtyp of the asset ? *)
     | Tasset _ -> CollMap (aname,VTint)
     | _ -> raise (UnsupportedType (aname,fname,loc))
     end
  | _ -> raise (UnsupportedType (aname,fname,loc))

exception NoFieldType of lident

let mk_storage_field name iskey (arg : decl) =
  let fname = (unloc arg).name in
  let typ =
    match (unloc arg).typ with
    | Some t -> t
    | None   -> raise (NoFieldType name)
  in
  let typ = aft_to_sft name fname iskey typ in
  [{
    asset   = name;
    name    = (unloc arg).name;
    typ     = typ;
    ghost   = false;
    default = (unloc arg).default;
    ops     = []
  }]

let mk_storage_fields (asset : asset)  =
  let asset = unloc asset in
  let name = asset.name in
  let key = asset.key in
  List.fold_left (fun acc arg ->
      let iskey = compare (unloc key) (get_decl_id arg) = 0 in
      acc @ (mk_storage_field name iskey arg)
    ) [] asset.args

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
