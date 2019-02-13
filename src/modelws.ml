open Location
open Model
open Modelinfo

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
  | Remove
[@@deriving show {with_path = false}]

type storage_field_operation = {
    typ      : storage_field_operation_type;
    requires : require list;
    ensures  : ensure list;
  }
[@@deriving show {with_path = false}]

type storage_field_type =
  | Enum     of lident
  | Var      of vtyp
  | KeySet   of lident * vtyp
  | ValueMap of lident * vtyp * vtyp (* aname, asset type, value type *)
  (* aname, asset type, asset name, value type *)
  | CollMap  of lident * vtyp * lident * vtyp
[@@deriving show {with_path = false}]

type storage_field_unloc = {
    asset   : lident option;
    name    : lident;
    typ     : storage_field_type;
    ghost   : bool;
    default : bval option; (* initial value *)
    ops     : storage_field_operation list;
  }
[@@deriving show {with_path = false}]

type storage_field = storage_field_unloc loced
[@@deriving show {with_path = false}]

type storage = {
    fields     : storage_field list;
    invariants : lterm list;
  }
[@@deriving show {with_path = false}]

let empty_storage = { fields = []; invariants = [] }

type transaction_unloc = {
    name         : lident;
    args         : arg list;
    requires     : require list;
    spec         : specification option;
}
[@@deriving show {with_path = false}]

type transaction = transaction_unloc loced
[@@deriving show {with_path = false}]

type enum_unloc = {
  name   : lident;
  values : lident list;
}
[@@deriving show {with_path = false}]

type enum = enum_unloc loced
[@@deriving show {with_path = false}]

type model_with_storage = {
  name         : lident;
  enums        : enum list;
  storage      : storage;
  transactions : transaction list;
}
[@@deriving show {with_path = false}]

(* asset field type to storage field type *)
let aft_to_sft info aname fname iskey (typ : ptyp) =
  let loc = loc typ in
  let typ = unloc typ in
  match iskey, typ with
  | true, Tbuiltin typ -> KeySet (aname,typ)
  | true, _ -> raise (InvalidKeyType (aname,fname,loc))
  | false, Tbuiltin typ -> ValueMap (aname, get_key_type aname info, typ)
  (* what is the vtyp of the asset ? *)
  | false, Tasset id -> ValueMap (aname, get_key_type id info,
                                  get_key_type id info)
  | false, Tcontainer (ptyp,_) ->
     begin
     match unloc ptyp with
     (* what is the vtyp of the asset ? *)
       | Tasset id -> CollMap (aname, (get_key_type aname info),
                               id, (get_key_type id info))
     | _ -> raise (UnsupportedType (aname,fname,loc))
     end
  | _ -> raise (UnsupportedType (aname,fname,loc))

let mk_asset_field aname fname =
  let loc = loc fname in
  let name = unloc aname in
  let field = unloc fname in
  { plloc = loc; pldesc = name^"_"^field }

let mk_storage_field info name iskey (arg : decl) =
  let fname = (unloc arg).name in
  let typ =
    match (unloc arg).typ with
    | Some t -> t
    | None   -> raise (NoFieldType name)
  in
  let typ = aft_to_sft info name fname iskey typ in
  [ mkloc (loc arg) {
    asset   = Some name;
    name    = mk_asset_field name (unloc arg).name;
    typ     = typ;
    ghost   = false;
    default = (unloc arg).default;
    ops     = []
  }]

let mk_storage_fields info (asset : asset)  =
  let asset = unloc asset in
  let name = asset.name in
  let key = asset.key in
  List.fold_left (fun acc arg ->
      let iskey = compare (unloc key) (get_decl_id arg) = 0 in
      acc @ (mk_storage_field info name iskey arg)
    ) [] asset.args

(* variable type to field type *)
let vt_to_ft var =
  match (unloc var.decl).typ with
  | Some t ->
    begin
      match unloc t with
      | Tbuiltin vt -> Var vt
      | _ -> raise (UnsupportedVartype (loc var.decl))
    end
  | None -> raise (VarNoType (loc var.decl))

let mk_variable (var : variable) =
  let l   = loc var in
  let var = unloc var in
  mkloc l {
  asset   = None;
  name    = (unloc var.decl).name;
  typ     = vt_to_ft var;
  ghost   = false;
  default = (unloc var.decl).default;
  ops     = []
}

(* maps *)
let mk_role_default (r : role) =
  match (unloc r).default with
  | Some (Raddress a) -> Some (mkloc (loc r) (BVaddress a))
  | _ -> None

let mk_role_var (role : role) =
  mkloc (loc role) {
  asset   = None;
  name    = (unloc role).name;
  typ     = Var VTaddress;
  ghost   = false;
  default = mk_role_default role;
  ops     = []
}

let mk_initial_state info (st : state) =
  let init = get_initial_state info st.name in
  mkloc (loc init) (BVenum (unloc init))

let mk_state_var info (st : state) = mkloc (st.loc) {
  asset   = None;
  name    = st.name;
  typ     = Enum st.name;
  ghost   = false;
  default = Some (mk_initial_state info st);
  ops     = []
}

let mk_storage info m =
  let fields = m.states
    |> List.fold_left (fun acc st -> acc @ [mk_state_var info st]) []
    |> fun x -> List.fold_left (fun acc r -> acc @ [mk_role_var r]) x m.roles
    |> fun x -> List.fold_left (fun acc var -> acc @ [mk_variable var]) x m.variables
    |> fun x ->
    List.fold_left (fun acc a ->
        acc @ (mk_storage_fields info a)
      ) x m.assets
  in
  { empty_storage with fields = fields }

let mk_enum (items : state_item list) = List.map (fun (it : state_item) -> it.name) items

let mk_enums _ m = m.states |> List.fold_left (fun acc (st : state) ->
    acc @ [
      mkloc (st.loc) { name   = st.name;
                       values = mk_enum st.items; }
    ]
  ) []

let model_to_modelws (info : info) (m : model) : model_with_storage = {
    name         = (unloc m).name;
    enums        = mk_enums info (unloc m);
    storage      = mk_storage info (unloc m);
    transactions = [];
  }
