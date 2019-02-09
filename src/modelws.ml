open Location
open Model

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
  | Var      of vtyp
  | KeySet   of lident * vtyp
  | ValueMap of vtyp (* field type *)
  | CollMap  of lident * vtyp (* collection asset lident*)
[@@deriving show {with_path = false}]

type storage_field = {
    asset   : lident option;
    name    : lident;
    typ     : storage_field_type;
    ghost   : bool;
    default : bval option; (* initial value *)
    ops     : storage_field_operation list;
  }
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

type model_with_storage = {
    name         : lident;
    storage      : storage;
    transactions : transaction list;
}
[@@deriving show {with_path = false}]

type info = {
  key_types : (string * vtyp) list; (* asset name, key type *)
}

(* type mapping exceptions : asset name, field name, type location *)
exception InvalidKeyType of lident * lident * Location.t
exception UnsupportedType of lident * lident * Location.t

let get_key_type fname key_types =
  let id = unloc fname in
  if List.mem_assoc id key_types
  then List.assoc id key_types
  else raise Not_found

(* asset field type to storage field type *)
let aft_to_sft info aname fname iskey (typ : ptyp) =
  let loc = loc typ in
  let typ = unloc typ in
  match iskey, typ with
  | true, Tbuiltin typ -> KeySet (aname,typ)
  | true, _ -> raise (InvalidKeyType (aname,fname,loc))
  | false, Tbuiltin typ -> ValueMap typ
  (* what is the vtyp of the asset ? *)
  | false, Tasset id -> ValueMap (get_key_type id info.key_types)
  | false, Tcontainer (ptyp,_) ->
     begin
     match unloc ptyp with
     (* what is the vtyp of the asset ? *)
     | Tasset id -> CollMap (aname, (get_key_type id info.key_types))
     | _ -> raise (UnsupportedType (aname,fname,loc))
     end
  | _ -> raise (UnsupportedType (aname,fname,loc))

exception NoFieldType of lident

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
  [{
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

exception VarNoType of Location.t
exception UnsupportedVartype of Location.t

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

let mk_variable (var : variable) = {
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
  | Some (Raddress a) -> Some (mkloc (loc r) (BVstring a))
  | _ -> None

let mk_role_var (role : role) = {
  asset   = None;
  name    = (unloc role).name;
  typ     = Var VTaddress;
  ghost   = false;
  default = mk_role_default role;
  ops     = []
}

let get_key_type (a : asset) =
  let assetid = get_asset_name a in
  let keyid = a |> unloc |> fun x -> x.key |> unloc in
  let rec rec_get_key = function
    | arg::tl ->
      if compare keyid (get_decl_id arg) = 0
      then
        let typ =
          match (unloc arg).typ with
          | Some t ->
            begin
              match unloc t with
              | Tbuiltin typ -> typ
              | _ -> raise (UnsupportedVartype (loc t))
            end
          | None   -> raise (NoFieldType (unloc arg).name)
        in (assetid, typ)
      else rec_get_key tl
    | [] -> raise Not_found in
  a |> unloc |> fun x -> x.args |> rec_get_key

let mk_storage m =
  let kt = m.assets |> List.fold_left (fun acc a -> acc @ [get_key_type a]) [] in
  let info = { key_types = kt } in
  let fields =
    m.roles
    |> List.fold_left (fun acc r -> acc @ [mk_role_var r]) []
    |> fun x -> List.fold_left (fun acc var -> acc @ [mk_variable var]) x m.variables
    |> fun x -> List.fold_left (fun acc a ->
                    acc @ (mk_storage_fields info a)) x m.assets
  in
  { empty_storage with fields = fields }

let model_to_modelws (m : model) : model_with_storage = {
    name = (unloc m).name;
    storage = mk_storage (unloc m);
    transactions = [];
  }
