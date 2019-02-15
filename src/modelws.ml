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
  | Addnofail
  | Remove
  | Removenofail
  | Addasset
  | Removeasset
[@@deriving show {with_path = false}]

type storage_field_operation = {
  name     : lident;
  typ      : storage_field_operation_type;
  actions  : storage_field_operation list;
  requires : require list;
  ensures  : ensure list;
}
[@@deriving show {with_path = false}]

type storage_field_type =
  | Enum     of lident
  | Var      of vtyp
  | KeySet   of lident * vtyp
  | ValueMap of lident * vtyp * vtyp (* aname, asset key type, value type *)
  (* aname, asset type, asset name, value type *)
  | CollMap  of lident * vtyp * lident * vtyp
[@@deriving show {with_path = false}]

type storage_field = {
    asset   : lident option;
    name    : lident;
    typ     : storage_field_type;
    ghost   : bool;
    default : bval option; (* initial value *)
    ops     : storage_field_operation list;
    loc     : Location.t [@opaque]
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

type enum_unloc = {
  name   : lident;
  values : lident list;
}
[@@deriving show {with_path = false}]

type enum = enum_unloc loced
[@@deriving show {with_path = false}]

type pterm = (lident,storage_field_type,pterm) poly_pterm loced
[@@deriving show {with_path = false}]

type function_ws = (lident,storage_field_type, pterm) gen_function
[@@deriving show {with_path = false}]

type model_with_storage = {
  name         : lident;
  enums        : enum list;
  storage      : storage;
  functions    : function_ws list;
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
  let fname = arg.name in
  let typ =
    match arg.typ with
    | Some t -> t
    | None   -> raise (NoFieldType name)
  in
  let typ = aft_to_sft info name fname iskey typ in [{
    asset   = Some name;
    name    = mk_asset_field name arg.name;
    typ     = typ;
    ghost   = false;
    default = arg.default;
    ops     = [];
    loc     = arg.loc
  }]

let mk_storage_fields info (asset : asset)  =
  let name = asset.name in
  let key = asset.key in
  List.fold_left (fun acc arg ->
      let iskey = compare (unloc key) (get_decl_id arg) = 0 in
      acc @ (mk_storage_field info name iskey arg)
    ) [] asset.args

(* variable type to field type *)
let vt_to_ft var =
  match var.decl.typ with
  | Some t ->
    begin
      match unloc t with
      | Tbuiltin vt -> Var vt
      | _ -> raise (UnsupportedVartype var.decl.loc)
    end
  | None -> raise (VarNoType var.decl.loc)

let mk_variable (var : variable) = {
  asset   = None;
  name    = var.decl.name;
  typ     = vt_to_ft var;
  ghost   = false;
  default = var.decl.default;
  ops     = [];
  loc     = var.loc;
}

(* maps *)
let mk_role_default (r : role) =
  match r.default with
  | Some (Raddress a) -> Some (mkloc (r.loc) (BVaddress a))
  | _ -> None

let mk_role_var (role : role) = {
  asset   = None;
  name    = role.name;
  typ     = Var VTaddress;
  ghost   = false;
  default = mk_role_default role;
  ops     = [];
  loc     = role.loc;
}

let mk_initial_state info (st : state) =
  let init = get_initial_state info st.name in
  mkloc (loc init) (BVenum (unloc init))

let mk_state_name n =
  mkloc (loc n) ((unloc n)^"_st")

let mk_state_var info (st : state) = {
  asset   = None;
  name    = mk_state_name st.name;
  typ     = Enum st.name;
  ghost   = false;
  default = Some (mk_initial_state info st);
  ops     = [];
  loc     = st.loc;
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

(* Field operations compilation *)

let mk_operation n a = {
  name = n;
  typ = a;
  actions = [];
  requires = [];
  ensures = [];
}

(* TODO *)
let compile_field_operation info _mws (f : storage_field) =
  match (is_key f.name info), f.typ with
  | false, Enum _ | false, Var _ | false, ValueMap _ ->
    List.map (mk_operation f.name) [Get;Set]
  | _ -> []

let compile_operations info mws =  {
  mws with
  storage = {
    mws.storage with
    fields =
      List.map (fun f -> {
            f with
            ops = compile_field_operation info mws f
          }
        ) mws.storage.fields
  }
}

(* this is a basic pterm without loc easier to use when building ml term *)
type basic_pterm = (string,storage_field_type,basic_pterm) poly_pterm

let lstr s = mkloc Location.dummy s

let rec loc_pterm (p : basic_pterm) : pterm =
  mkloc Location.dummy (
    match p with
    | Pif (c,t, Some e) -> Model.Pif (loc_pterm c, loc_pterm t, Some (loc_pterm e))
    | Pif (c,t, None) -> Model.Pif (loc_pterm c, loc_pterm t, None)
    | Pfor (id, c, b) -> Model.Pfor (lstr id, loc_pterm c, loc_pterm b)
    | Passign (a, f, t) -> Model.Passign (a, loc_pterm f, loc_pterm t)
    | Pfassign l -> Pfassign (List.map (fun (a, (i,j), v) ->
        (a, (Translate.map_option lstr i, lstr j), loc_pterm v)) l)
    | Ptransfer (f, b, i) -> Model.Ptransfer (loc_pterm f, b, i)
    | Ptransition -> Model.Ptransition
    | Pbreak -> Model.Pbreak
    | Pseq l -> Model.Pseq (List.map loc_pterm l)
    | Pnot e -> Model.Pnot (loc_pterm e)
    | Passert l -> Model.Passert l
    (* below is common entries with lterm *)
    | Prel i -> Model.Prel i
    | Pletin (i,v,t,b) -> Model.Pletin (lstr i,loc_pterm v, t, loc_pterm b)
    | Papp (f,a) -> Model.Papp (loc_pterm f, List.map loc_pterm a)
    | Plambda (i,t, b) -> Model.Plambda (lstr i, t, loc_pterm b)
    | Plogical (o,l,r) -> Model.Plogical (o, loc_pterm l, loc_pterm r)
    (* mutualize below with lterm ? *)
    | Pcomp (o,l,r) -> Model.Pcomp (o, loc_pterm l, loc_pterm r)
    | Parith (o,l,r) -> Model.Parith (o, loc_pterm l, loc_pterm r)
    | Puarith (u,e) -> Model.Puarith (u, loc_pterm e)
    | Pvar i -> Model.Pvar (lstr i)
    | Pfield i -> Model.Pfield (lstr i)
    | Passet i -> Model.Passet (lstr i)
    | Parray l -> Model.Parray (List.map loc_pterm l)
    | Plit v -> Model.Plit v
    | Pdot (l,r) -> Model.Pdot (loc_pterm l, loc_pterm r)
    | Pconst c -> Model.Pconst c
  )

let dummy_function = {
  name   = lstr "";
  args   = [];
  return = None;
  body   = loc_pterm Pbreak;
  loc    = Location.dummy;
}

let mk_arg (s,t) = { name = lstr s ; typ = t; default = None ; loc = Location.dummy }

let field_to_getset _info (f : storage_field) (op : storage_field_operation) =
  match f.asset, op.typ with
  | None, Get -> (* simply apply field name to argument "s" *)
    let n = unloc (f.name) in {
      dummy_function with
      name = lstr ("get_"^n);
      args = [mk_arg ("s",None)];
      body = loc_pterm (Papp (Pvar n,[Pvar "s"]))
    }
  | None, Set ->
    let n = unloc (f.name) in {
      dummy_function with
      name = lstr ("set_"^n);
      args = List.map mk_arg ["s",None; "v",None ];
      body = loc_pterm (Papp (Pvar "update_storage",[Pvar "s";
                                                     Papp (Pvar n, [Pvar "s"]);
                                                     Pvar "v"]));
    }
  (* simply apply field name to argument "s" *)
  (* Papp (Pvar (unloc (f.name)),[Pvar "s"])*)
  | _ -> dummy_function

let mk_getset_functions info (mws : model_with_storage) = {
  mws with
  functions = mws.functions @ (
      List.fold_left (
        fun acc f ->
          List.fold_left (
            fun acc op -> acc @ [field_to_getset info f op]
          ) acc f.ops
      ) [] mws.storage.fields
    )
}

let model_to_modelws (info : info) (m : model) : model_with_storage =
  {
    name         = (unloc m).name;
    enums        = mk_enums info (unloc m);
    storage      = mk_storage info (unloc m);
    functions    = [];
    transactions = [];
  }
  |> (compile_operations info)
  |> (mk_getset_functions info)
