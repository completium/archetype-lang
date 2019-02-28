open Location
open Model
open Modelinfo
open Tools

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
  | Flocal of lident (* enum, state, record *)
  | Ftyp   of vtyp
  | Flist  of storage_field_type
  | Fset   of storage_field_type
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

type transaction = {
  name         : lident;
  args         : ((storage_field_type, bval) gen_decl) list;
  requires     : require list;
  spec         : specification option;
  body         : pterm;
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type enum_unloc = {
  name   : lident;
  values : lident list;
}
[@@deriving show {with_path = false}]

type enum = enum_unloc loced
[@@deriving show {with_path = false}]

type record_field_type = {
  name    : lident;
  typ_    : storage_field_type;
  default : Model.bval option;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type record = {
  name   : lident;
  values : record_field_type list;
  loc    : Location.t [@opaque]
}
[@@deriving show {with_path = false}]

type pattern = (lident,storage_field_type,pattern) pattern_unloc loced
[@@deriving show {with_path = false}]

type pterm = (lident,storage_field_type,pattern,pterm) poly_pterm loced
[@@deriving show {with_path = false}]

type function_ws = (lident,storage_field_type,pattern,pterm) gen_function
[@@deriving show {with_path = false}]

type transaction_ws = (lident,storage_field_type,pattern,pterm) gen_transaction
[@@deriving show {with_path = false}]

type model_with_storage = {
  name         : lident;
  enums        : enum list;
  records      : record list;
  storage      : storage;
  functions    : function_ws list;
  transactions : transaction_ws list;
}
[@@deriving show {with_path = false}]

type asset_function =
  | Get of string
  | AddAsset of string
  | Addifnotexist of string
  | AddList of string * string

(* asset field type to storage field type *)
(* This should be modulated/optimized according to asset usage in tx actions *)
let aft_to_sft info aname iskey fname (typ : ptyp) =
  let loc = loc typ in
  let typ = unloc typ in
  match iskey, typ with
  (* an asset key with a basic type *)
  | true , Tbuiltin vt         -> Flist (Ftyp vt)
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
                                        Flist (Ftyp (get_key_type id info)))
       | _                     -> raise (UnsupportedType (aname,fname,loc))
     end
  | _ -> raise (UnsupportedType (aname,fname,loc))

let mk_asset_field aname fname =
  let loc = loc fname in
  let name = unloc aname in
  let field = unloc fname in
  { plloc = loc; pldesc = name^"_"^field }

let mk_asset_field_simple fname =
  let loc, name = deloc fname in
  { plloc = loc; pldesc = name^"_col" }

let mk_default_field (b : bval option) : Model.pterm option =
    map_option
    (fun x -> mkloc (loc x) (Plit x))
    b

let mk_storage_simple_asset info (asset : asset)  =
  let vtyp_key = Modelinfo.get_key_type asset.name info in
  let name = asset.name in
  [{
    asset   = Some name;
    name    = mk_asset_field_simple name;
    typ     = Fmap (vtyp_key, Flocal name);
    ghost   = false;
    default = None;
    ops     = [];
    loc     = asset.loc
  }]

let mk_storage_field info iskey name (arg : decl) =
  let fname = arg.name in
  let typ =
    match arg.typ with
    | Some t -> t
    | None   -> raise (ExpectedVarType name)
  in
  let typ = aft_to_sft info name iskey fname typ in [{
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
  List.fold_left (fun acc (arg : decl) ->
      let iskey = compare (unloc (arg.name)) (unloc (asset.key)) = 0 in
      acc @ (mk_storage_field info iskey name arg)
    ) [] asset.args

let mk_storage_asset info (asset : asset)  =
  let name = asset.name in
  let policy = Modelinfo.get_asset_policy name info in
  match policy with
  | Record -> mk_storage_simple_asset info asset
  | Flat -> mk_storage_fields info asset


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
        acc @ (mk_storage_asset info a)
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

(* Records building*)

let ptyp_to_vtyp = function
  | Tbuiltin b -> b
  | Tasset _a -> VTstring (* TODO *)
  | _ -> raise (Anomaly "to_storage_type")

let rec to_storage_type (_ptyp : ptyp option) : storage_field_type =
  match _ptyp with
  | Some v -> (
      match unloc v with
      | Tbuiltin b -> Ftyp b
      | Tasset a -> Flocal a
      | Tcontainer (t, c) ->
        (
          let t = to_storage_type (Some t) in
          match c with
           | Collection | Queue | Stack | Partition -> Flist t
           | Set | Subset -> Fset t)
      | Ttuple l -> Ftuple (List.map (fun x -> to_storage_type (Some x)) l)
      | _ -> raise (Anomaly "to_storage_type1")
    )
  | None -> raise (Anomaly "to_storage_type2")

let to_storage_decl (d : decl) : record_field_type = {
  name = d.name;
  typ_ = to_storage_type d.typ;
  default = d.default;
  loc = d.loc;
}

let mk_fields_record _info (a : asset) =
  let key_name = unloc a.key in
  List.fold_right (
    fun (i : decl) acc ->
      if (String.compare key_name (i.name |> unloc) <> 0)
      then (to_storage_decl i)::acc
      else acc) a.args []

let mk_record info (a : Model.asset) : record =
  {
    name = a.name;
    values = mk_fields_record info a;
    loc = a.loc;
  }

let mk_records info m =
  List.fold_right (fun (a : Model.asset) acc ->
      let policy = Modelinfo.get_asset_policy a.name info in
      match policy with
      | Record -> (mk_record info a)::acc
      | _ -> acc
    ) m.assets []

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
    (*print_endline ("asset : "^(unloc aname)^"; field : "^(unloc (f.name)));*)
    if is_key aname f.name info
    then
      List.map (mk_operation f.name) [Get;Add;Remove]
    else []
  | Some _, Fmap (_, Ftyp _) ->
    List.map (mk_operation f.name) [(*Get;Set*)]
  | Some a, Fmap (_, Flist _) ->
    let typ = unloc (get_asset_var_typ a (unloc f.name) info) in
    begin
      match typ with
      | Tcontainer (t, Partition) ->
        begin
          match unloc t with
          | Tasset ca -> List.map (mk_operation f.name) [Addasset ca; Removeasset ca]
          | _ -> []
        end
      | _ -> []
    end
  | Some _, Fmap (_, Flocal _) ->
    List.map (mk_operation f.name) [Get]
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

let gen_mapper_pterm p f =
  Model.poly_pterm_map
    p
    (fun x -> unloc x)
    (fun x y -> mkloc (Location.loc x) y)
    f
    id
    id
    id

let rec loc_pterm (p : basic_pterm) : pterm =
  Model.poly_pterm_map
    p
    (fun x -> x)
    (fun _x y -> mkloc (Location.dummy) y)
    loc_pterm
    lstr
    loc_qualid
    loc_pattern

let dummy_function = {
  name   = lstr "";
  args   = [];
  return = None;
  body   = loc_pterm Pbreak;
  loc    = Location.dummy;
}

let mk_arg (s,t) = { name = lstr s ; typ = t; default = None ; loc = Location.dummy }

let mk_letin params body =
  let rec mkl = function
    | (i,b,t) :: tl -> Pletin (i,b,t,mkl tl)
    | [] -> body in
  mkl params

let mk_typs (f : storage_field) a info =
  (get_asset_vars_typs a info) |>
  List.map (aft_to_sft info a false f.name) |>
  List.map (fun t ->
      match t with
      | Fmap (_, ft) -> ft
      | _ -> raise (UnsupportedVartype f.loc)
    )

(* k : key value
   p : the param (for ex. 'p2')
   v : is the field name
   note that :
   "s" is the storage
   "v" is the k value
*)
let mk_field_add p v = function
  | Flist _ ->
    (* List.add (p, s.v) *)
    let sv = Papp (Pvar v,[Pvar "s"]) in
    Papp (Pdot(Pvar "List",Pvar "add"),[Ptuple [Pvar p; sv]])
  | Fmap _ ->
    (* Map.add p0 p s.v *)
    let sv = Papp (Pvar v,[Pvar "s"]) in
    Papp (Pdot(Pvar "Map",Pvar "add"), [Pvar "k"; Pvar p; sv])
  | _ -> raise (Anomaly ("mk_field_add "^p^" "^v))

let mk_field_remove p v = function
  | Flist _ ->
    (* List.add (p, s.v) *)
    let sv = Papp (Pvar v,[Pvar "s"]) in
    Papp (Pdot(Pvar "List",Pvar "remove"),[Ptuple [Pvar p; sv]])
  | Fmap _ ->
    (* Map.add p0 p s.v *)
    let sv = Papp (Pvar v,[Pvar "s"]) in
    Papp (Pdot(Pvar "Map",Pvar "remove"), [Pvar p; sv])
  | _ -> raise (Anomaly ("mk_field_add "^p^" "^v))

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
  | Flist vt, Some a, Get ->
    if is_key a f.name info
    then
      let n = unloc (f.name) in {
        dummy_function with
        name = lstr ("get_"^n);
        args = List.map mk_arg ["p",Some (Ftuple [Flocal (lstr "storage"); vt])];
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
  | Flist vt, Some a, Add ->
    if is_key a f.name info
    then
      let typs = mk_typs f a info in
      let nb = string_of_int ((List.length typs)+2) in
      let params = List.mapi (fun i _ ->
          let si = string_of_int i in
          let sip2 = string_of_int (i+2) in
          "p"^si,Papp (Pvar ("get_"^sip2^"_"^nb),[Pvar "p"]),None
        ) typs in
      let adds = ["s",Papp (Pvar "update_storage",
                            [Pvar "s";
                             Papp (Pvar (unloc (f.name)), [Pvar "s"]);
                             mk_field_add "k" (unloc (f.name)) (Flist vt)
                            ]),None
      ] @ (List.mapi (fun i v ->
          let param = "p"^(string_of_int i) in
          let typ = (aft_to_sft info a false a) (get_asset_var_typ a v info) in
          "s",Papp (Pvar "update_storage",[Pvar "s";
                                           Papp (Pvar v, [Pvar "s"]);
                                           mk_field_add param v typ
                                          ]),None
        ) (get_asset_vars a info)) in
      let n = unloc (f.name) in {
        dummy_function with
        name = lstr ("add_"^n);
        args = List.map mk_arg ["p",
                                Some (Ftuple ([Flocal (lstr "storage"); vt]@typs))];
        body = loc_pterm (
            Pletin ("s",Papp (Pvar ("get_0_"^nb),[Pvar "p"]),None,
            Pletin ("k",Papp (Pvar ("get_1_"^nb),[Pvar "p"]),None,
            mk_letin params (Pmatchwith (Papp (Pdot (Pvar "List",Pvar "mem"),
                              [Ptuple[Pvar "k";
                                      Papp (Pvar n,[Pvar "s"])]]) ,[
                                           (Mapp (Qident "Some",[Mwild]),
                                            Papp (Pdot (Pvar "Current",Pvar "failwith"),
                                                  [Papp (Pvar "already_exists",[])]));
               (Mapp (Qident "None",[]), mk_letin adds (Pvar "s"));
            ]
            )))
                   ));
      }
    else raise (Anomaly ("field_to_getset : "^(unloc (f.name))))
  | Flist vt, Some a, Remove ->
    (*  let s = get p 0 in
        let k = get p 1 in
        let s = s.mile_id <- list_remove ((k, s.mile_id)) in
        let s = s.mile_amount <- Map.remove k (s.mile_amount) in
        let s = s.mile_expiration <- Map.remove k (s.mile_expiration) in s3
        end
    *)
    if is_key a f.name info
    then
      let nb = "2" in
      let rms = ["s",Papp (Pvar "update_storage",
                            [Pvar "s";
                             Papp (Pvar (unloc (f.name)), [Pvar "s"]);
                             mk_field_remove "k" (unloc (f.name)) (Flist vt)
                            ]),None
      ] @ (List.map (fun v ->
          let typ = (aft_to_sft info a false a) (get_asset_var_typ a v info) in
          "s",Papp (Pvar "update_storage",[Pvar "s";
                                           Papp (Pvar v, [Pvar "s"]);
                                           mk_field_remove "k" v typ
                                          ]),None
        ) (get_asset_vars a info)) in
      let n = unloc (f.name) in {
        dummy_function with
        name = lstr ("remove_"^n);
        args = List.map mk_arg ["p",
                                Some (Ftuple ([Flocal (lstr "storage"); vt]))];
        body = loc_pterm (
            Pletin ("s",Papp (Pvar ("get_0_"^nb),[Pvar "p"]),None,
            Pletin ("k",Papp (Pvar ("get_1_"^nb),[Pvar "p"]),None,
            (Pmatchwith (Papp (Pdot (Pvar "List",Pvar "mem"),
                               [Ptuple[Pvar "k";
                                       Papp (Pvar n,[Pvar "s"])]]) ,[
                           (Mapp (Qident "None",[]),
                            Papp (Pdot (Pvar "Current",Pvar "failwith"),
                                  [Papp (Pvar "not_found",[])]));
                           (Mapp (Qident "Some",[Mwild]), mk_letin rms (Pvar "s"));
            ]
            )))
                   ));
      }
    else raise (Anomaly ("field_to_getset : "^(unloc (f.name))))
  | Fmap (vtf, Flist vtt), Some _ (*a*), Addasset ca ->
    (* let add_asset (p : (storage, asset key, contained asset key, ...)) =
       let s  = get_0_n p in
       let k  = get_1_n p in
       let ak = get_2_n p in
       ...
       // check whether key is valid
       match Map.find k s.asset_field with
       | None -> Current.failwith "not found"
       | Some v ->
         // will fail if ak already present
         let s = add_asset (s, ak, ...) in
         let v = List.add ak v in
         let s = s.asset_field <- new_list in
         s
    *)
    let fn = unloc (f.name) in
    let add_asset_n = "add_"^(get_key_name ca info) in
    let typs = mk_typs f ca info in
    let nb = string_of_int ((List.length typs)+3) in
    let params = List.mapi (fun i _ ->
          let si = string_of_int i in
          let sip3 = string_of_int (i+3) in
          "p"^si,Papp (Pvar ("get_"^sip3^"_"^nb),[Pvar "p"]),None
      ) typs in
    let args = List.mapi (fun i _ -> Pvar ("p"^(string_of_int i))) typs in
    let mapfind = Papp (Pdot(Pvar "Map",Pvar "find"),[Pvar "k";
                                                      Papp (Pvar fn,[Pvar "s"])]) in
    { dummy_function with
      name = lstr ("add_"^(unloc (f.name))^"_"^(unloc ca));
      args = List.map mk_arg ["p", Some (Ftuple (
          [Flocal (lstr "storage"); Ftyp vtf; vtt]@typs))
        ];
      body = loc_pterm (
          Pletin ("s",Papp (Pvar ("get_0_"^nb),[Pvar "p"]),None,
          Pletin ("k",Papp (Pvar ("get_1_"^nb),[Pvar "p"]),None,
          Pletin ("a",Papp (Pvar ("get_2_"^nb),[Pvar "p"]),None,
          mk_letin params (Pmatchwith (mapfind,[
          (* None -> Current.failwith "not found "*)
          (Mapp (Qident "None",[]), Papp (Pdot (Pvar "Current",Pvar "failwith"),
                                           [Papp (Pvar "not_found",[])]));
          (* Some v -> ... *)
          (Mapp (Qident "Some",[Mvar "v"]),
           Pletin ("s",Papp (Pvar add_asset_n,[Ptuple ([Pvar "s";Pvar "a"]@args)]),None,
           Pletin ("v",
                   Papp (Pdot(Pvar "List",Pvar "add"),[Ptuple [Pvar "a";Pvar "v"]]),None,
           Pletin ("s",
                   Papp (Pvar "update_storage", [
                       Pvar "s";
                       Papp (Pvar fn,[Pvar "s"]);
                       Papp (Pdot(Pvar "Map",Pvar "add"),[Pvar "k";
                                                          Pvar "v";
                                                          Papp (Pvar fn,[Pvar "s"])])]
                     ), None,
           Pvar "s"
             ))))
                    ])
        )))));
    }
  | Fmap (vtf, Flist vtt), Some _ (*a*), Removeasset ca ->
    (* let remove_asset (p : (storage, asset key, contained asset key)) =
       let s  = get_0_n p in
       let k  = get_1_n p in
       let ak = get_2_n p in
       // check whether key is valid
       match Map.find k s.asset_field with
       | None -> Current.failwith "not found"
       | Some v ->
         // will fail if ak already present
         let s = remove_asset (s, ak, ...) in
         let v = List.remove ak v in
         let s = s.asset_field <- new_list in
         s
    *)
    let fn = unloc (f.name) in
    let rm_asset_n = "remove_"^(get_key_name ca info) in
    let nb = "3" in
    let mapfind = Papp (Pdot(Pvar "Map",Pvar "find"),[Pvar "k";
                                                      Papp (Pvar fn,[Pvar "s"])]) in
    { dummy_function with
      name = lstr ("remove_"^(unloc (f.name))^"_"^(unloc ca));
      args = List.map mk_arg ["p", Some (Ftuple (
          [Flocal (lstr "storage"); Ftyp vtf; vtt]))
        ];
      body = loc_pterm (
          Pletin ("s",Papp (Pvar ("get_0_"^nb),[Pvar "p"]),None,
          Pletin ("k",Papp (Pvar ("get_1_"^nb),[Pvar "p"]),None,
          Pletin ("a",Papp (Pvar ("get_2_"^nb),[Pvar "p"]),None,
          (Pmatchwith (mapfind,[
          (* None -> Current.failwith "not found "*)
          (Mapp (Qident "None",[]), Papp (Pdot (Pvar "Current",Pvar "failwith"),
                                           [Papp (Pvar "not_found",[])]));
          (* Some v -> ... *)
          (Mapp (Qident "Some",[Mvar "v"]),
           Pletin ("s",Papp (Pvar rm_asset_n,[Ptuple ([Pvar "s";Pvar "a"])]),None,
           Pletin ("v",
                   Papp (Pdot(Pvar "List",Pvar "remove"),[Ptuple [Pvar "a";Pvar "v"]]),None,
           Pletin ("s",
                   Papp (Pvar "update_storage", [
                       Pvar "s";
                       Papp (Pvar fn,[Pvar "s"]);
                       Papp (Pdot(Pvar "Map",Pvar "add"),[Pvar "k";
                                                          Pvar "v";
                                                          Papp (Pvar fn,[Pvar "s"])])]
                     ), None,
           Pvar "s"
             ))))
                    ])
        )))));
    }
  | Fmap (_vtf, Flocal name_asset), _, _ ->
    (* simply apply field name to argument "s" *)
    let key_type = get_key_type name_asset info in
    let _key_id = get_key_id name_asset info in
    let n = unloc name_asset in {
      dummy_function with
      name = lstr ("get_"^n);
      args = List.map mk_arg ["p",Some (Ftuple [Flocal (lstr "storage"); Ftyp key_type])];
      body = loc_pterm (
          Pletin ("s",Papp (Pvar "get_0_2",[Pvar "p"]),None,
          Pletin ("v",Papp (Pvar "get_1_2",[Pvar "p"]),None,
          Pmatchwith (Papp (Pdot (Pvar "Map",Pvar "find"),
          [Pvar "v"; Papp (Pvar (n ^ "_col"),[Pvar "s"])]) ,[
          (Mapp (Qident "Some",[Mvar "k"]), Pvar "k");
          (Mapp (Qident "None",[]),  Papp (Pdot (Pvar "Current",Pvar "failwith"),
                                     [Papp (Pvar "not_found",[])]))])
                 )))
    }

  | _ -> raise (Anomaly ("field_to_getset : "^(unloc (f.name))))

let mk_getset_functions info (mws : model_with_storage) = {
  mws with
  functions = mws.functions @ (
      List.fold_left (
        fun acc (f : storage_field) ->
          List.fold_left (
            fun acc op -> acc @ [field_to_getset info f op]
          ) acc f.ops
      ) [] mws.storage.fields
    )
}

let mk_transaction name args spec action loc : transaction_ws = {
  name         = name;
  args         = args;
  calledby     = None;
  condition    = None;
  transition   = None;
  spec         = spec;
  action       = action;
  loc          = loc;
  }

let mk_common_functions _info (mws : model_with_storage) = {
  mws with
  functions = mws.functions(* @ (
      mk_now_function()
                              )*)
}

let to_arg info (arg : (ptyp, bval) gen_decl) : (string * storage_field_type) list =
  let arg_name = unloc arg.name in
  let rec to_arg_rec prefix typ =
    match typ |> unloc with
    | Tasset lident ->
      let asset_args = List.fold_left (fun acc ((_s, i) : string * ptyp) ->
          (to_arg_rec ((unloc lident) ^ "_") i)@acc)
          [] (get_asset_vars_id_typs lident info) in
      [get_key_id lident info, Ftyp (get_key_type lident info)] @ asset_args
    | Tbuiltin vtb -> [(prefix ^ arg_name, Ftyp vtb)]
    | _ -> raise Tools.Unsupported_yet in
  let typ = Tools.get arg.typ in
  to_arg_rec "" typ

let compute_s_args info (t : Model.transaction) =
  let args = t.args in
  if List.length args = 0
  then ([], Some (Flocal (lstr "unit")))
  else (
    let ids, args = args
            |> List.map (fun i -> to_arg info i)
            |> List.flatten
            |> List.split in
    (ids, (Some (Ftuple args))))

let is_get_asset (e, args) =
  if List.length args <> 1 then false
  else
    match unloc e with
    | Pdot (a, b) -> (
        match unloc a, unloc b with
        | Pvar _, Pconst Cget -> true
        | _ -> false
      )
    | _ -> false

let extract_get_asset (e, args) =
  let asset_name =
    match unloc e with
    | Pdot (a, b) -> (
        match unloc a, unloc b with
        | Pvar a, Pconst Cget -> unloc a
        | _ -> raise (Anomaly("is_get_asset"))
      )
    | _ -> raise (Anomaly("is_get_asset")) in

  let arg = match args with
    | [a] -> a
    | _ -> raise (Anomaly("is_get_asset")) in
  (asset_name, arg)

let dest_get_asset (asset_name, arg) f =
  dumloc (Papp(dumloc (Pvar (dumloc ("get_" ^ asset_name))), [f arg]))

let replace_dot_expr a =
  let rec myreplace acc p =
    match unloc p with
    | Papp (e, args) when is_get_asset (e, args) ->
      (
        let asset_name, arg = extract_get_asset (e, args) in
        dest_get_asset (asset_name, arg) (myreplace acc)
      )
    | _ -> gen_mapper_pterm p (myreplace acc) in
  a |> myreplace []

let transaction_to_transaction_ws info (t : Model.transaction) : transaction_ws =
  let name = t.name in
  let ids, args = compute_s_args info t in
  let args = List.map mk_arg [("p", args);
                              ("s", Some (Flocal (lstr "storage")))] in
  let nb = ids |> List.length |> string_of_int in
  let action = Tools.get t.action in
  Format.eprintf "%a\n" Model.pp_pterm action;
  let action = replace_dot_expr action in
  Format.eprintf "--\n%a\n" Model.pp_pterm action;
  let act = loc_pterm (
      List.fold_right
        (fun x acc -> Pletin (x, Papp (Pvar ("get_0_"^nb),[Pvar "p"]),None,acc))
        ids
        (Ptuple[Pvar "empty_ops"; Pvar "s"])
    ) in
  mk_transaction name args None (Some act) dummy


let mk_transactions info (m : model_unloc)  mws = {
  mws with
  transactions = mws.transactions @ (
      List.fold_left (fun acc (t : Model.transaction) -> (transaction_to_transaction_ws info t)::acc)
        [] (m.transactions |> (fun l -> [List.nth l 0]))
    )
}

let model_to_modelws (info : info) (m : model) : model_with_storage =
  (*Format.printf "%a\n" Modelinfo.pp_info info;*)
  let m = unloc m in
  {
    name         = m.name;
    enums        = mk_enums info m;
    records      = mk_records info m;
    storage      = mk_storage info m;
    functions    = [];
    transactions = [];
  }
  (*|> (compile_operations info)*)
  (*  |> (mk_getset_functions info)*)
  |> (mk_transactions info m)
