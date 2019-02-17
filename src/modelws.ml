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
  requires : require list;
  ensures  : ensure list;
}
[@@deriving show {with_path = false}]

type storage_field_type =
  | Flocal of lident (* enum, state *)
  | Ftyp   of vtyp
  | Flist  of vtyp
  | Fset   of vtyp
  | Fmap   of vtyp * storage_field_type
  | Ftuple of storage_field_type list
[@@deriving show {with_path = false}]

type storage_field = {
    asset   : lident option;
    name    : lident;
    typ     : storage_field_type;
    ghost   : bool;
    default : pterm option; (* initial value *)
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

type pattern = (lident,storage_field_type,pattern) pattern_unloc loced
[@@deriving show {with_path = false}]

type pterm = (lident,storage_field_type,pattern,pterm) poly_pterm loced
[@@deriving show {with_path = false}]

type function_ws = (lident,storage_field_type,pattern,pterm) gen_function
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
(* This should be modulated/optimized according to asset usage in tx actions *)
let aft_to_sft info aname fname (typ : ptyp) =
  let loc = loc typ in
  let typ = unloc typ in
  match is_key fname info, typ with
  (* an asset key with a basic type *)
  | true , Tbuiltin vt         -> Flist vt
  (* an asset key with an extravagant type *)
  | true , _                   -> raise (InvalidKeyType (aname,fname,loc))
  (* an asset field with a basic type *)
  | false, Tbuiltin vt         -> Fmap (get_key_type aname info, Ftyp vt)
  (* an asset field with another(?) asset type TODO : check that asset are different
  *)
  | false, Tasset id           -> Fmap (get_key_type id info,
                                        Ftyp (get_key_type id info))
  (* an asset field which is a container of another asset *)
  | false, Tcontainer (ptyp,_) ->
     begin
     match unloc ptyp with
     (* what is the vtyp of the asset ? *)
       | Tasset id             -> Fmap (get_key_type aname info,
                                        Flist (get_key_type id info))
       | _                     -> raise (UnsupportedType (aname,fname,loc))
     end
  | _ -> raise (UnsupportedType (aname,fname,loc))

let mk_asset_field aname fname =
  let loc = loc fname in
  let name = unloc aname in
  let field = unloc fname in
  { plloc = loc; pldesc = name^"_"^field }

let mk_default_field (b : bval option) : Model.pterm option =
    Translate.map_option
    (fun x -> mkloc (loc x) (Plit x))
    b

let mk_storage_field info name (arg : decl) =
  let fname = arg.name in
  let typ =
    match arg.typ with
    | Some t -> t
    | None   -> raise (ExpectedVarType name)
  in
  let typ = aft_to_sft info name fname typ in [{
    asset   = Some name;
    name    = mk_asset_field name arg.name;
    typ     = typ;
    ghost   = false;
    default = mk_default_field arg.default;
    ops     = [];
    loc     = arg.loc
  }]

let mk_storage_fields info (asset : asset)  =
  let name = asset.name in
  List.fold_left (fun acc arg -> acc @ (mk_storage_field info name arg)) [] asset.args

(* variable type to field type *)
let vt_to_ft var =
  match var.decl.typ with
  | Some t ->
    begin
      match unloc t with
      | Tbuiltin vt -> Ftyp vt
      | _ -> raise (UnsupportedVartype var.decl.loc)
    end
  | None -> raise (VarNoType var.decl.loc)

let mk_variable (var : variable) =
  {
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
  | Some (Raddress a) -> Some (mkloc (r.loc) (Plit (mkloc (r.loc) (BVaddress a))))
  | _ -> None

let mk_role_var (role : role) = {
  asset   = None;
  name    = role.name;
  typ     = Ftyp VTaddress;
  ghost   = false;
  default = mk_role_default role;
  ops     = [];
  loc     = role.loc;
}

let mk_initial_state info (st : state) =
  let init = get_initial_state info st.name in
  mkloc (loc init) (Plit (mkloc (loc init) (BVenum (unloc init))))

let mk_state_name n =
  mkloc (loc n) ((unloc n)^"_st")

let mk_state_var info (st : state) = {
  asset   = None;
  name    = mk_state_name st.name;
  typ     = Flocal st.name;
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

let mk_operation n t = {
  name = n;
  typ = t;
  requires = [];
  ensures = [];
}

(* TODO *)
let compile_field_operation info _mws (f : storage_field) =
  match f.asset, f.typ with
  | None, Flocal _ | None, Ftyp _ | None, Fmap (_, Ftyp _) ->
    List.map (mk_operation f.name) [Get;Set]
  | Some aname, Flist _ ->
    if is_key aname info
    then
      List.map (mk_operation f.name) [Get;Add;Remove]
    else []
  | Some _, Fmap (_, Flist _) ->
    if is_partition f.name info
    then List.map (mk_operation f.name) [Get;Addasset;Removeasset]
    else []
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
type basic_pattern = (string,storage_field_type,basic_pattern) pattern_unloc
type basic_pterm = (string,storage_field_type,basic_pattern,basic_pterm) poly_pterm

let lstr s = mkloc Location.dummy s

let lit_to_pterm l = mkloc (loc l) (Plit l)

let rec mk_qualid q =
  match q with
  | Qident i -> Qident (lstr i)
  | Qdot (b, i) -> Qdot (mk_qualid b, lstr i)

let rec loc_qualid (q : string qualid) : lident qualid =
  match q with
  | Qident s -> Qident (lstr s)
  | Qdot (q, s) -> Qdot (loc_qualid q, lstr s)

let rec loc_pattern (p : basic_pattern) : pattern =
  mkloc Location.dummy (
    match p with
    | Mwild -> Mwild
    | Mvar s -> Mvar (lstr s)
    | Mapp (q, l) -> Mapp (loc_qualid q, List.map loc_pattern l)
    | Mrec l -> Mrec (List.map (fun (i, p) -> (loc_qualid i, loc_pattern p)) l)
    | Mtuple l -> Mtuple (List.map loc_pattern l)
    | Mas (p, o, g) -> Mas (loc_pattern p, lstr o, g)
    | Mor (lhs, rhs) -> Mor (loc_pattern lhs, loc_pattern rhs)
    | Mcast (p, t) -> Mcast (loc_pattern p, t)
    | Mscope (q, p) -> Mscope (loc_qualid q, loc_pattern p)
    | Mparen p -> Mparen (loc_pattern p)
    | Mghost p -> Mghost (loc_pattern p))

let rec loc_pterm (p : basic_pterm) : pterm =
  mkloc Location.dummy (
    match p with
    | Pif (c,t, Some e) -> Model.Pif (loc_pterm c, loc_pterm t, Some (loc_pterm e))
    | Pif (c,t, None) -> Model.Pif (loc_pterm c, loc_pterm t, None)
    | Pfor (id, c, b) -> Model.Pfor (lstr id, loc_pterm c, loc_pterm b)
    | Passign (a, f, t) -> Model.Passign (a, loc_pterm f, loc_pterm t)
    | Pfassign l -> Pfassign (List.map (fun (a, (i,j), v) ->
        (a, (Translate.map_option lstr i, lstr j), loc_pterm v)) l)
    | Ptransfer (f, b, q) -> Model.Ptransfer (loc_pterm f, b, Translate.map_option mk_qualid q)
    | Pbreak -> Model.Pbreak
    | Pseq (lhs, rhs) -> Model.Pseq (loc_pterm lhs, loc_pterm rhs)
    | Pnot e -> Model.Pnot (loc_pterm e)
    | Passert l -> Model.Passert l
    | Pmatchwith (e, l) -> Model.Pmatchwith (loc_pterm e, List.map (fun (p, e) -> (loc_pattern p, loc_pterm e)) l)
    | Precord l -> Model.Precord (List.map (fun (q, t) -> (loc_qualid q, loc_pterm t) ) l)
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
    | Parray l -> Model.Parray (List.map loc_pterm l)
    | Plit v -> Model.Plit v
    | Pdot (l,r) -> Model.Pdot (loc_pterm l, loc_pterm r)
    | Pconst c -> Model.Pconst c
    | Ptuple l -> Model.Ptuple (List.map loc_pterm l)
  )

let dummy_function = {
  name   = lstr "";
  args   = [];
  return = None;
  body   = loc_pterm Pbreak;
  loc    = Location.dummy;
}

let mk_arg (s,t) = { name = lstr s ; typ = t; default = None ; loc = Location.dummy }

let field_to_getset info (f : storage_field) (op : storage_field_operation) =
  match f.typ, f.asset, op.typ with
  | Ftyp _, None, Get | Flocal _, None, Get ->
    (* simply apply field name to argument "s" *)
    let n = unloc (f.name) in {
      dummy_function with
      name = lstr ("get_"^n);
      args = [mk_arg ("s",None)];
      body = loc_pterm (Papp (Pvar n,[Pvar "s"]))
    }
  | Ftyp _, None, Set | Flocal _, None, Set ->
    let n = unloc (f.name) in {
      dummy_function with
      name = lstr ("set_"^n);
      args = List.map mk_arg ["s",None; "v",None ];
      body = loc_pterm (Papp (Pvar "update_storage",[Pvar "s";
                                                     Papp (Pvar n, [Pvar "s"]);
                                                     Pvar "v"]));
    }
  | Flist vt, Some _, Get ->
    if is_key f.name info
    then
      let n = unloc (f.name) in {
        dummy_function with
        name = lstr ("get_"^n);
        args = List.map mk_arg ["p",Some (Ftuple [Flocal (lstr "storage"); Ftyp vt])];
        body = loc_pterm (
            Pletin ("s",Papp (Pvar "get_0_2",[Pvar "p"]),None,
            Pletin ("v",Papp (Pvar "get_1_2",[Pvar "p"]),None,
            Pmatchwith (Papp (Pdot (Pvar "List",Pvar "mem"),
                              [Ptuple[Pvar "v";
                                      Papp (Pvar n,[Pvar "s"])]]) ,[
               (Mapp (Qident "Some",[Mvar "k"]), Pvar "k");
               (Mapp (Qident "None",[]),  Papp (Pdot (Pvar "Current",Pvar "failwith"),
                                                [Papp (Pvar "not_found",[])]));
            ]
            ))
                   ));
      }
    else raise (Anomaly ("field_to_getset : "^(unloc (f.name))))
  | _ -> raise (Anomaly ("field_to_getset : "^(unloc (f.name))))

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
