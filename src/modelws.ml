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
  { plloc = loc; pldesc = name ^ "_" ^ field }

let mk_asset_field_simple fname =
  let loc, name = deloc fname in
  { plloc = loc; pldesc = name ^ "_col" }

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
  mkloc (loc n) ((unloc n) ^ "_st")

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

let rec to_storage_type (ptyp : ptyp option) : storage_field_type =
  match ptyp with
  | Some v -> (
      match unloc v with
      | Tbuiltin b -> Ftyp b
      | Tasset a -> Flocal a
      | Tcontainer (t, c) ->
        (
          let t = to_storage_type (Some t) in
          match c with
          | Partition -> Flist (Ftyp VTstring) (*TODO: retrieve key type when c is an asset *)
          | Collection | Queue | Stack -> Flist t
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
[@@deriving show {with_path = false}]

type basic_pterm = (string,storage_field_type,basic_pattern,basic_pterm) poly_pterm
[@@deriving show {with_path = false}]

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

let rec loc_pterm (p : basic_pterm) : pterm =
  Model.poly_pterm_map
    (fun x -> mkloc (Location.dummy) x)
    lstr
    id
    loc_pattern
    loc_pterm
    loc_qualid
    p

let rec unloc_qualid = function
  | Qident i -> Qident (unloc i)
  | Qdot (a, b) -> Qdot (unloc_qualid a, unloc b)

let rec unloc_pterm (p : Model.pterm) =
  p |> unloc |>
  Model.poly_pterm_map
    id
    unloc
    id
    id
    unloc_pterm
    unloc_qualid

let dummy_function = {
  name   = lstr "";
  args   = [];
  return = None;
  body   = loc_pterm Pbreak;
  side   = false;
  loc    = Location.dummy;
}

let dummy_transaction = {
  name         = lstr "";
  args         = [];
  calledby     = None;
  condition    = None;
  transition   = None;
  spec         = None;
  action       = None;
  side         = false;
  loc          = Location.dummy;
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
               (Mapp (Qident "None",[]), Papp (Pdot (Pvar "Current",Pvar "failwith"),
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

let flat_model_to_modelws (info : info) (m : model) : model_with_storage =
  let m = unloc m in
  {
    name         = m.name;
    enums        = mk_enums info m;
    records      = mk_records info m;
    storage      = mk_storage info m;
    functions    = [];
    transactions = [];
  }
  |> (compile_operations info)
  |> (mk_getset_functions info)




(* record policy process *)

type asset_function =
  | MkAsset of string
  | Get of string
  | AddAsset of string
  | Addifnotexist of string
  | AddList of string * string
[@@deriving show {with_path = false}]

let mk_fun_name = function
  | MkAsset s -> "mk_" ^ s
  | Get s -> "get_" ^ s
  | AddAsset s -> "add_asset_" ^ s
  | Addifnotexist s -> "addifnotexist_" ^ s
  | AddList (s, f) -> "add_list_" ^ s ^ "_" ^ f

let add_fun i l =
  if List.mem i l then
    l
  else
    i::l

let add_funs l1 l2 =
  List.fold_right (fun x acc -> add_fun x acc) l1 l2

let is_asset_const (e, args) const nb_args =
  if List.length args <> nb_args then false
  else
    match unloc e with
    | Pdot (a, b) -> (
        match unloc a, unloc b with
        | Pvar _, Pconst c when c = const-> true
        | _ -> false
      )
    | _ -> false

let is_asset_get           (e, args) = is_asset_const (e, args) Cget 1
let is_asset_add           (e, args) = is_asset_const (e, args) Cadd 1
let is_asset_addifnotexist (e, args) = is_asset_const (e, args) Caddifnotexist 2
let is_asset_removeif      (e, args) = is_asset_const (e, args) Cremoveif 1

let dest_asset_const_name = function
  | Pdot (a, b) -> (
      match unloc a, unloc b with
      | Pvar a, Pconst _ -> unloc a
      | _ -> raise (Anomaly("dest_asset_const_1"))
    )
  | _ -> raise (Anomaly("dest_asset_const_2"))

let dest_asset_get (e, args) =
  let asset_name = dest_asset_const_name (unloc e) in
  let arg = match args with
    | [a] -> a
    | _ -> raise (Anomaly("dest_asset_get")) in
  (asset_name, arg)

let dest_asset_add (e, args) =
  let asset_name = dest_asset_const_name (unloc e) in
  let arg = match args with
    | [a] -> a
    | _ -> raise (Anomaly("dest_asset_add")) in
  (asset_name, arg)

let dest_asset_addifnotexist (e, args) =
  let asset_name = dest_asset_const_name (unloc e) in
  let arg = match args with
    | [a; b] -> [a; b]
    | _ -> raise (Anomaly("dest_asset_addifnotexist")) in
  (asset_name, arg)

let rec gen_mapper_pterm f p =
  Model.poly_pterm_map
    (fun x -> mkloc (Location.loc p) x)
    id
    id
    id
    (gen_mapper_pterm f)
    id
    (unloc p)

let mk_fun_to_key () =
  {
    dummy_function with
    name = lstr "to_key";
    args = [
      { name = lstr "x" ; typ = None; default = None ; loc = Location.dummy }];
    body = loc_pterm (Papp(Pvar "get_0_2", [Pvar "x"]));
  }

let mk_mk_asset info name =
  let asset_args = get_asset_vars_id_typs (dumloc name) info in
  let asset_args : (string * storage_field_type) list = List.map (fun ((x, y) : (string * ptyp)) -> (x, to_storage_type (Some y))) asset_args in
  let args = List.map (fun ((x, y) : (string * storage_field_type)) -> mk_arg (x, Some y)) asset_args in
  let rec_items = List.map (fun ((x, _y) : (string * storage_field_type)) -> (Qident x, Pvar x)) asset_args in
  {
    dummy_function with
    name = lstr (mk_fun_name (MkAsset name));
    return = Some (Flocal (lstr name));
    args = args;
    body = loc_pterm (Precord rec_items);
  }

let pfailwith msg = Papp (Pdot (Pvar "Current", Pvar "failwith"), [Papp (Pvar msg, [])])

let pGen m s args = Papp (Pdot (Pvar m, Pvar s), args)

let pCurrent s = pGen "Current" s []
let pCurrentSender = pCurrent "sender"
let pCurrentBalance = pCurrent "balance"
let pCurrentTime = pCurrent "time_"

(*
let[@inline] get_owner (p: storage * address) : (address * owner) =
  let s = get p 0 in
  let v = get p 1 in
  begin match Map.find v (s.owner_col) with
    | Some k -> (v, k)
    | None -> Current.failwith ("not found")
  end
*)
let mk_get_asset info asset_name = {
  dummy_function with
  name = lstr (mk_fun_name (Get asset_name));
  args = [mk_arg ("p", Some (Ftuple ([Flocal (lstr "storage");
                                      Ftyp (get_key_type (dumloc asset_name) info) ])))];
  side = true;
  body =
    loc_pterm (
      Pletin ("s", Papp (Pvar "get_0_2", [Pvar "p"]), None,
        Pletin ("v", Papp (Pvar "get_1_2", [Pvar "p"]), None,
          Pmatchwith (Papp (Pdot (Pvar "Map", Pvar "find"),
            [Pvar "v";
             Papp (Pvar (asset_name ^ "_col"), [Pvar "s"])])
                     ,[
                       (Mapp (Qident "Some", [Mvar "k"]), Ptuple [Pvar "v"; Pvar "k"]);
                       (Mapp (Qident "None", []), pfailwith "not_found")
                     ]
            )))
    )
}

let extract_type_args_from_asset info asset_name : storage_field_type list =
  List.fold_left (fun acc (i : string * ptyp) -> acc @ [
      match i |> snd |> unloc with
      | Tbuiltin vtb -> Ftyp (vtb)
      | Tasset lident -> Ftyp (get_key_type lident info)
      | Tcontainer ({pldesc=Tbuiltin vtb;_}, _cont) -> Flist (Ftyp (vtb))
      | Tcontainer ({pldesc=Tasset lident;_}, _cont) -> Flist (Ftyp (get_key_type lident info))
      | _ -> raise (Anomaly "extract_type_args_from_asset")
    ]) [] (get_asset_vars_id_typs (dumloc asset_name) info)

let get_arg i nb = Papp (Pvar ("get_" ^ (string_of_int i) ^ "_" ^ (string_of_int nb)), [Pvar "p"])

type add_asset_gen_data = {
  asset_name : string;
  side : bool;
  asset_fun : asset_function;
  exist_expr : basic_pterm;
}

let get_mk_call_function asset_name arg nb idx_start n =
  let get_arg i = Papp (Pvar ("get_" ^ (string_of_int i) ^ "_" ^ (string_of_int nb)), [Pvar arg]) in
  Papp (Pvar (mk_fun_name (MkAsset asset_name)), Tools.int_fold (fun acc k -> acc @ [get_arg (idx_start + k)]) [] n)

(*let[@inline] add (p: storage * address * string list) : storage =
  let s = get p 0 in
  let owner_key = get p 1 in
  let owner_miles = get p 2 in
  match Map.find owner_key (s.owner_col) with
  | Some _ -> exist_expr
  | None -> s.owner_col <- Map.update owner_key (Some (mk_owner owner_miles)) s.owner_col*)
let mk_add_asset_gen info (data : add_asset_gen_data) =
  let asset_name = data.asset_name in
  let side = data.side in
  let exist_expr = data.exist_expr in
  let asset_fun = data.asset_fun in
  let args = extract_type_args_from_asset info asset_name in
  let nb = 2 + List.length args in
  let fun_mk = get_mk_call_function asset_name "p" nb 2 (nb - 2) in
  {
  dummy_function with
  name = lstr (mk_fun_name asset_fun);
  side = side;
  args = [mk_arg ("p", Some (Ftuple ([Flocal (lstr "storage");
                                      Ftyp (get_key_type (dumloc asset_name) info) ] @ args)))];
  return = Some (Flocal (lstr "storage"));
  body =
    loc_pterm (
      Pletin ("s", Papp (Pvar ("get_0_" ^ (string_of_int nb)), [Pvar "p"]), None,
        Pletin (asset_name ^ "_key", Papp (Pvar ("get_1_" ^ (string_of_int nb)), [Pvar "p"]), None,
          Pmatchwith (Papp (Pdot (Pvar "Map", Pvar "find"),
            [Pvar (asset_name ^ "_key");
             Papp (Pvar (asset_name ^ "_col"), [Pvar "s"])])
                     ,[
                       (Mapp (Qident "Some", [Mwild]), exist_expr);
                       (Mapp (Qident "None", []),

                        Papp (Pvar "update_storage",[Pvar "s";
                                                     Papp (Pvar (asset_name ^ "_col"), [Pvar "s"]);
                                                     Papp (Pdot (Pvar "Map", Pvar "update"),
                                                           [Pvar (asset_name ^ "_key");
                                                            Papp (Pvar "Some", [fun_mk]);
                                                            Papp (Pvar (asset_name ^ "_col"), [Pvar "s"])])
                       ]))
                     ]
          ))))
}

let mk_add_asset info asset_name = mk_add_asset_gen info {
    asset_name = asset_name;
    side       = true;
    asset_fun  = AddAsset asset_name;
    exist_expr = pfailwith "already_exists";
  }

let mk_addifnotexist info asset_name = mk_add_asset_gen info {
    asset_name = asset_name;
    side       = false;
    asset_fun  = Addifnotexist asset_name;
    exist_expr = Pvar "s";
  }

let mk_add_list info asset_name field_name =
  let asset_col = "mile" in
  let asset_col_key = asset_col ^ "_key" in
  let asset_name_key = asset_name ^ "_key" in
  let is_one_arg = true in
  {
    dummy_function with
    name = lstr (mk_fun_name (AddList (asset_name, field_name)));
    side = true;
    args = [mk_arg ("p", Some (Ftuple ([Flocal (lstr "storage");
                                        Ftyp (get_key_type (dumloc asset_name) info);
                                        Ftuple ([Ftyp VTstring; Flocal (lstr asset_col)]);
                                       ])))];
    body =
      loc_pterm (
      Pletin ("s",                 Papp (Pvar "get_0_3", [Pvar "p"]), None,
      Pletin (asset_name ^ "_key", Papp (Pvar "get_1_3", [Pvar "p"]), None,
      Pletin (asset_col_key,       Papp (Pvar "get_0_2", [Papp (Pvar "get_2_3", [Pvar "p"])]), None,
      Pletin (asset_col,           Papp (Pvar "get_1_2", [Papp (Pvar "get_2_3", [Pvar "p"])]), None,
      Pletin (asset_name,          Papp (Pvar "get_1_2", [Papp (Pvar (mk_fun_name (Get asset_name)), [Ptuple [Pvar "s"; Pvar asset_name_key]])]), None,
        Pseq(Pif(
               Papp (Pdot (Pvar "Map", Pvar "mem"), [Pvar asset_col_key; Papp (Pvar (asset_col ^ "_col"), [Pvar "s"])]),
               pfailwith "not_found", None),
             Pletin ("s", Papp (Pvar "update_storage",[Pvar "s";
                                                       Papp (Pvar (asset_col ^ "_col"), [Pvar "s"]);
                                                       Papp (Pdot (Pvar "Map", Pvar "update"),
                                                             [Pvar (asset_col ^ "_key");
                                                              Papp (Pvar "Some", [Pvar asset_col]);
                                                              Papp (Pvar (asset_col ^ "_col"), [Pvar "s"])])
                                                      ]), None,
                     Pletin ("newlist", Papp (Pdot (Pvar "List", Pvar "add"), [Ptuple [Pvar (asset_col_key);
                                                                                Papp (Pvar field_name, [Pvar asset_name])]]), None,
                             Pletin ("new" ^ asset_name,
                                     (if is_one_arg
                                      then (Papp (Pvar "update_simple", [Pvar (asset_name);
                                                                         Pvar "newlist"]))
                                  else (Papp (Pvar "update_storage", [Pvar (asset_name);
                                                           Papp (Pvar (field_name), [Pvar asset_name]);
                                                           Pvar "newlist"])))



                            , None,
                             Papp (Pvar "update_storage",[Pvar "s";
                                                          Papp (Pvar (asset_name ^ "_col"), [Pvar "s"]);
                                                          Papp (Pdot (Pvar "Map", Pvar "update"),
                                                                [Pvar (asset_name ^ "_key");
                                                                 Papp (Pvar "Some", [Pvar ("new" ^ asset_name)]);
                                                                 Papp (Pvar (asset_name ^ "_col"), [Pvar "s"])])
                                                         ])

                    ))))))))))
}

let generate_asset_functions info (l : asset_function list) : function_ws list =
  mk_fun_to_key() ::
  List.map (fun x ->
      match x with
      | MkAsset asset_name -> mk_mk_asset info asset_name
      | Get asset_name -> mk_get_asset info asset_name
      | AddAsset asset_name -> mk_add_asset info asset_name
      | Addifnotexist asset_name -> mk_addifnotexist info asset_name
      | AddList (asset_name, field_name) -> mk_add_list info asset_name field_name) l


type ret_typ =
  | Letin
  | Storage
  | Asset of string
  | Field of string * string  (*asset name, field name*)
  | Bool
  | Int
  | Nat
  | Timestamp
  | Time
  | String
  | Address
  | Tez
  | Const of Model.const
  | Id of string
  | Aaa of pterm
  | Operations
  | None

type process_data = {
  term : pterm;
  funs : asset_function list;
  ret  : ret_typ;
  side : bool;
}

let dummy_process_data = {
  term = dumloc Pbreak;
  funs = [];
  ret  = None;
  side = false;
}

type process_acc = {
  info : info;
  model : model_unloc;
  start : pterm option;
}

type process_ret = {
  funs : asset_function list;
  side : bool;
}

let rec cast_pattern_type (p : Model.pattern) : pattern =
  mkloc (Location.loc p)
    (Model.pattern_map
       id
       id
       (fun (x : ptyp) : storage_field_type -> to_storage_type (Some x))
       cast_pattern_type
       id
       (Location.unloc p))

let rec model_pterm_to_pterm (p : Model.pterm) : pterm =
  Model.poly_pterm_map
    (fun (x : (lident,storage_field_type,pattern,pterm) poly_pterm)-> mkloc (Location.loc p) x)
    id
    (fun (x : ptyp option) : storage_field_type option -> Some (to_storage_type x))
    cast_pattern_type
    model_pterm_to_pterm
    id
    (unloc p)

let get_storage_name (_acc : process_acc) = "s"

let mk_var str = dumloc (Pvar (dumloc str))

let is_asset_field _info (_asset_name, _field_name) = true (* TODO *)

let compute_const_field asset_name field_name _const pterm : asset_function list * ret_typ * 'a =
  let f = AddList (asset_name, field_name) in
  [f], Aaa (pterm), Pvar (dumloc (mk_fun_name f))


let extract_id_from_storage info id : ret_typ option =
  match Modelinfo.get_type_storage info id with
  | Some a -> Some (
      match a with
      | Some t -> (
          match t with
          | Tasset id -> Asset (unloc id)
          | Tbuiltin vt -> (
              match vt with
              | VTbool     -> Bool
              | VTint      -> Int
              | VTuint     -> Nat
              | VTdate     -> Timestamp
              | VTduration -> Time
              | VTstring   -> String
              | VTaddress  -> Address
              | VTcurrency _ -> Tez
            )
          | _ -> None
        )
      | None -> None
    )
  | None -> None

let retrieve_id_from_storage info id =
  let ret_storage = extract_id_from_storage info (unloc id) in
  (
    match ret_storage with
    | Some ret -> dumloc (Papp (dumloc (Pvar id), [loc_pterm (Pvar "s")])), ret
    | None -> dumloc (Pvar id), Id (unloc id))

let rec process_rec (acc : process_acc) (pterm : Model.pterm) : process_data =
  let loc, v = deloc pterm in
  match v with
  | Pseq (l, r) ->
    (
      let a = process_rec acc l in
      let b = process_rec acc r in
      let t = (
        match acc.start, a.ret, b.ret with
        | Some s, Operations, Operations ->
          dumloc (Pletin (dumloc "_ops", a.term, None,
                          dumloc (Pletin (dumloc "_ops", b.term, None, s))))
        | _ ->
          dumloc (Pletin (dumloc "s", a.term, None,
                          dumloc (Pletin (dumloc "s", b.term, None,
                                          loc_pterm (Ptuple[Pvar "_ops"; Pvar "s"])))))
      ) in
      {
        dummy_process_data with
        term = t;
        funs = add_funs a.funs b.funs;
        ret = Letin;
      }
    )
  | Papp (e, args) when is_asset_get (e, args)->
    (
      let asset_name, arg = dest_asset_get (e, args) in
      let storage_name = get_storage_name acc in
      let new_arg = process_rec acc arg in
      let f_arg = dumloc (Ptuple [mk_var storage_name; new_arg.term]) in
      {
        dummy_process_data with
        term = mkloc loc (Papp(dumloc (Pvar (dumloc (mk_fun_name (Get asset_name)))), [f_arg]));
        funs = new_arg.funs;
        ret = Asset asset_name;
      }
    )
  | Papp (e, args) when is_asset_add (e, args)->
    (
      let asset_name, arg = dest_asset_add (e, args) in
      let storage_name = get_storage_name acc in
      let arg = process_rec acc arg in
      let fields = get_asset_vars (dumloc asset_name) acc.info in
      let args : pterm list = (
        match unloc arg.term with
        | Pfassign l -> List.map (fun (_, _, z) ->
            match unloc z with
            | Pvar id when (String.equal (unloc id) "empty") -> mkloc (Location.loc z) (Pvar (dumloc "Nil"))
            | _ -> z) l
        | Pvar id ->
          begin
            let id = unloc id in
            [loc_pterm (Papp (Pvar "to_key", [Pvar id]))] @
            List.map (fun x -> loc_pterm (Papp (Pvar x, [Papp (Pvar "get_1_2", [Pvar id])]))) fields
          end
        | _ -> raise (Anomaly "process_rec")
        ) in
      let f_arg = dumloc (Ptuple ([mk_var storage_name] @ args)) in
      {
        dummy_process_data with
        term = mkloc loc (Papp (loc_pterm (Pvar (mk_fun_name (AddAsset asset_name))), [f_arg]));
        funs = add_fun (AddAsset asset_name) arg.funs;
        ret = Storage;
      }
    )
  | Papp (e, args) when is_asset_addifnotexist (e, args)->
    (
      let asset_name, arg = dest_asset_addifnotexist (e, args) in
      let storage_name = get_storage_name acc in
      let arg1 = process_rec acc (List.nth arg 0) in
      let arg2 = process_rec acc (List.nth arg 1) in
      let args : pterm list = (
        match unloc arg2.term with
        | Pfassign l -> List.map (fun (_, _, z) ->
            match unloc z with
            | Pvar id when (String.equal (unloc id) "empty") -> mkloc (Location.loc z) (Pvar (dumloc "Nil"))
            | _ -> z) l
        | _ -> raise (Anomaly "process_rec")
        ) in
      let f_arg = dumloc (Ptuple ([mk_var storage_name; arg1.term] @ args)) in
      {
        dummy_process_data with
        term = mkloc loc (Papp(dumloc (Pvar (dumloc (mk_fun_name (Addifnotexist asset_name)))), [f_arg]));
        funs = add_fun (Addifnotexist asset_name) (add_funs arg1.funs arg2.funs);
        ret = Storage
      }
    )
  | Papp (e, args) when is_asset_removeif (e, args)->
    (
      {
        dummy_process_data with
        term = mkloc loc (Pvar (dumloc "s"));
        funs = [];
        ret = Storage;
      }
    )
  | Papp (e, args) ->
    (
      let new_e = process_rec acc e in
      let new_args = List.map (process_rec acc) args in
      let a, b = new_args |> List.map (fun (x : process_data) -> (x.term, x.funs)) |> List.split in

      let nl, ret, p = (match new_e.ret with
          | Aaa arg ->
            begin
              let myarg = List.nth new_args 0 in
              [], None, Papp (new_e.term, [dumloc (Ptuple[dumloc (Pvar (dumloc "s")); arg; myarg.term])])
            end
          | _ -> [], None, Papp (new_e.term, a)) in {
        dummy_process_data with
        term = mkloc loc p;
        funs = add_funs (add_funs nl new_e.funs) (List.flatten b);
        ret = ret;
      }
    )
  | Pdot (l, r) ->
    (
      let a = process_rec acc l in
      let b = process_rec acc r in

      let nl, ret, p = (match a.ret, b.ret with
          | Field (asset_name, id), Const c -> compute_const_field asset_name id c a.term
          | Asset asset_name, Id id when is_asset_field acc.info (asset_name, id) -> [], Field (asset_name, id),
              Papp (dumloc (Pvar (dumloc "to_key")), [a.term])
          | _ -> [],None, Pdot (a.term, b.term)) in {
        dummy_process_data with
        term = mkloc loc p;
        funs = add_funs (add_funs nl a.funs) b.funs;
        ret = ret;
      }
    )
  | Pcomp (op, l, r) ->
    (
      let a = process_rec acc l in
      let b = process_rec acc r in

      let pt = (
        match op, a.ret, b.ret with

        (* Timestamp *)
        | Equal, Timestamp, Timestamp ->  dumloc (Papp (loc_pterm (Pdot (Pvar "Timestamp", Pvar "tim_eq")),  [a.term; b.term]))
        | Nequal, Timestamp, Timestamp -> dumloc (Papp (loc_pterm (Pdot (Pvar "Timestamp", Pvar "tim_neq")), [a.term; b.term]))
        | Gt, Timestamp, Timestamp ->     dumloc (Papp (loc_pterm (Pdot (Pvar "Timestamp", Pvar "tim_gt")),  [a.term; b.term]))
        | Ge, Timestamp, Timestamp ->     dumloc (Papp (loc_pterm (Pdot (Pvar "Timestamp", Pvar "tim_ge")),  [a.term; b.term]))
        | Lt, Timestamp, Timestamp ->     dumloc (Papp (loc_pterm (Pdot (Pvar "Timestamp", Pvar "tim_lt")),  [a.term; b.term]))
        | Le, Timestamp, Timestamp ->     dumloc (Papp (loc_pterm (Pdot (Pvar "Timestamp", Pvar "tim_le")),  [a.term; b.term]))

        (* Tez *)
        | Equal, Tez, Tez ->  dumloc (Papp (loc_pterm (Pdot (Pvar "Tez", Pvar "tez_eq")),  [a.term; b.term]))
        | Nequal, Tez, Tez -> dumloc (Papp (loc_pterm (Pdot (Pvar "Tez", Pvar "tez_neq")), [a.term; b.term]))
        | Gt, Tez, Tez ->     dumloc (Papp (loc_pterm (Pdot (Pvar "Tez", Pvar "tez_gt")),  [a.term; b.term]))
        | Ge, Tez, Tez ->     dumloc (Papp (loc_pterm (Pdot (Pvar "Tez", Pvar "tez_ge")),  [a.term; b.term]))
        | Lt, Tez, Tez ->     dumloc (Papp (loc_pterm (Pdot (Pvar "Tez", Pvar "tez_lt")),  [a.term; b.term]))
        | Le, Tez, Tez ->     dumloc (Papp (loc_pterm (Pdot (Pvar "Tez", Pvar "tez_le")),  [a.term; b.term]))
| _ -> mkloc loc (Pcomp (op, a.term, b.term))
      ) in
      {
        dummy_process_data with
        term = pt;
        funs = add_funs a.funs b.funs;
        ret = Bool;
      }
    )
  | Parith (op, l, r) ->
    (
      let a = process_rec acc l in
      let b = process_rec acc r in

      let pt,ret = (
        match op, a.ret, b.ret with
        | Plus, Tez, Tez -> dumloc (Papp (loc_pterm (Pdot (Pvar "Tez", Pvar "tez_plus")), [a.term; b.term])), Tez
        | _ -> mkloc loc (Parith (op, a.term, b.term)), None
      ) in
      {
        dummy_process_data with
        term = pt;
        funs = add_funs a.funs b.funs;
        ret = ret;
      }
    )
  | Plit x ->
    (
      let pt, ret = (
        match (unloc x) with
        | BVcurrency (_, a) -> dumloc (Papp (loc_pterm (Pdot (Pvar "Tez", Pvar "totez")), [dumloc (Plit (dumloc (BVint a)))])), Tez
        | _ -> model_pterm_to_pterm pterm, None
      ) in {
        dummy_process_data with
        term = pt;
        ret = ret;
      }
    )
  | Pvar id ->
    (
      let pt, ret = retrieve_id_from_storage acc.info id in
      {
        dummy_process_data with
        term = pt;
        funs = [];
        ret = ret;
      }
    )
  | Pconst c ->
    let t, ret =
      begin
        match c with
        | Cbalance -> loc_pterm (pCurrentBalance), Tez
        | Cnow     -> loc_pterm (pCurrentTime), Timestamp
        | _ -> model_pterm_to_pterm pterm, Const c
      end in
    {
      dummy_process_data with
      term = t;
      funs = [];
      ret = ret
    }
  | Ptransfer (amount, back, dest) ->
    (
      let a = process_rec acc amount in
      let q = (
        match dest with
        | Some q -> q
        | None -> (
            match unloc amount with
            | Pvar id ->
              begin
                let from, to_ =
                  List.fold_left (fun acc (x : variable) ->
                      if String.equal (unloc id) (unloc x.decl.name)
                      then (x.from, x.to_)
                      else acc) (None, None) acc.model.variables
                in
                let dest = if back then from else to_ in
                match dest with
                | Some q -> q
                | None -> raise (Anomaly "transfer process 1")
              end
            | _ -> raise (Anomaly "transfer process 2")
          )) in
      let d = (
        match q with
        | Qident id -> (
            let pt, _ret = retrieve_id_from_storage acc.info id in pt
          )
        | _ -> raise (Anomaly "transfer process 3")) in

      let t = dumloc (Papp (loc_pterm (Pdot (Pvar "List", Pvar "add")),
                            [dumloc (Ptuple [
                                 dumloc (Papp (loc_pterm (Pdot (Pvar "Contract", Pvar "transfer")),
                                               [d; a.term]));
                                 loc_pterm (Pvar "_ops")
                               ])])) in

    {
      dummy_process_data with
      term = t;
      funs = [];
      ret = Operations;
    }
    )
  | _ -> {
      dummy_process_data with
      term = model_pterm_to_pterm pterm;
      funs = [];
    }

let rec unloc_pattern (p : pattern) : basic_pattern =
  match unloc p with
  | Mwild -> Mwild
  | Mvar s -> Mvar (unloc s)
  | Mapp (q, l) -> Mapp (unloc_qualid q, List.map unloc_pattern l)
  | Mrec l -> Mrec (List.map (fun (i, p) -> (unloc_qualid i, unloc_pattern p)) l)
  | Mtuple l -> Mtuple (List.map unloc_pattern l)
  | Mas (p, o, g) -> Mas (unloc_pattern p, unloc o, g)
  | Mor (lhs, rhs) -> Mor (unloc_pattern lhs, unloc_pattern rhs)
  | Mcast (p, t) -> Mcast (unloc_pattern p, t)
  | Mscope (q, p) -> Mscope (unloc_qualid q, unloc_pattern p)
  | Mparen p -> Mparen (unloc_pattern p)
  | Mghost p -> Mghost (unloc_pattern p)

let rec pterm_to_basic_pterm (p : pterm) : basic_pterm =
  p |> unloc |>
  Model.poly_pterm_map
    id
    unloc
    id
    unloc_pattern
    pterm_to_basic_pterm
    unloc_qualid


type arg_typ =
  | ATSimple of storage_field_type
  | ATAsset of string * storage_field_type * storage_field_type list

type arg_ret = {
  id: string;
  typ: arg_typ
}

let compute_args info (t : Model.transaction) : (arg_ret list) =
  List.fold_left (
    fun acc (arg : (ptyp, bval) gen_decl) -> (
        let id = arg.name |> unloc in
        let typ = Tools.get arg.typ in
        match unloc typ with
        | Tbuiltin vtb -> acc @ [{id = id; typ = ATSimple (Ftyp vtb);}]
        | Tasset lident -> acc @ [{id = id; typ = ATAsset (
            unloc lident,
            Ftyp (get_key_type lident info),
            List.fold_left (fun acc (i : string * ptyp) -> acc @ [Ftyp (
                 match i |> snd |> unloc with
                 | Tbuiltin vtb -> vtb
                 | Tasset lident -> get_key_type lident info
                 | _ -> raise (Anomaly "compute_args_0")
               )])
               [] (get_asset_vars_id_typs lident info))
          }]
        | _ -> raise (Anomaly "compute_args")
      )
  ) [] t.args

let rec process_rexpr = function
  | Rqualid (Qident id) ->
    begin
      let id = unloc id in
      loc_pterm (Papp (Pdot (Pvar "Address", Pvar "add_eq"),
                       [pCurrentSender; Papp (Pdot (Pvar "Address", Pvar "key_to_addr"), [Papp (Pvar id, [Pvar "s"])])]))
    end
  | Ror (l, r) -> dumloc (Plogical (Or, process_rexpr l, process_rexpr r))
  | _ -> raise (Anomaly "process_rexpr")

let process_called_by (t : Model.transaction) pt =
  match t.calledby with
  | Some r -> dumloc (Pseq (dumloc (Pif (dumloc (Pnot (process_rexpr r)), loc_pterm (pfailwith "not_authorized_fun"), None)), pt))
  | _ -> pt

let process_args args0 pt =
  if List.length args0 = 0
  then pt
  else
    begin
      let nb = List.fold_left (fun acc x -> acc +
                                            (match x.typ with
                                             | ATSimple _ -> 1
                                             | ATAsset (_, _, l) -> 1 + List.length l)) 0 args0 in
      let action =
        (List.fold_right
           (fun (x : arg_ret) (i, acc) ->
              let get_arg i = Papp (Pvar ("get_" ^ (string_of_int i) ^ "_" ^ (string_of_int nb)), [Pvar "p"]) in
              let id = x.id in
              let n, body =
                begin
                  match x.typ with
                  | ATSimple _ -> i - 1, get_arg i
                  | ATAsset (id, _, l) ->
                    let length_l = List.length l in
                    (i - 1 - length_l),
                    Ptuple [get_arg (i - length_l);
                            Papp (Pvar (mk_fun_name (MkAsset id)), List.mapi (fun k _ -> get_arg (i - length_l + k + 1)) l)]
                end in
              n, dumloc (Pletin (dumloc id, loc_pterm body, None, acc)))
        ) args0 (nb - 1, pt) |> snd in
      action
    end

let process_action info (m : model_unloc) (t : Model.transaction) (act : Model.pterm option) (pt0 : pterm) =

  match act with
  | None ->
    begin
      let pt = pt0 in
      pt, { dummy_process_data with side = true;}
    end
  | Some action ->
    begin
      let dummy_pterm = dumloc (
          Pseq (loc_pterm (pfailwith "not_supported_yet"),
                pt0)) in

      let acc = {
        info = info;
        model = m;
        start = Some pt0;
      } in

      match (unloc m.name), (unloc t.name) with
      | "miles_with_expiration", "consume" ->
        dummy_pterm, { dummy_process_data with side = true;}
      | _ ->
        begin
          let s = process_rec acc action in
          let pt : pterm =
            begin
              match s.ret with
              | Storage -> dumloc (Pletin (dumloc "s", s.term, None, pt0))
              | Operations -> dumloc (Pletin (dumloc "_ops", s.term, None, pt0))
              | _ -> s.term
            end in
          pt, {dummy_process_data with side = true; funs = s.funs;}
        end
    end


let build_match_cond info (m : model_unloc) (cond : Model.pterm option) x : pterm =
  match cond with
  | None -> x
  | Some c ->
    (
      let acc = {
        info = info;
        model = m;
        start = None;
      } in
      let cond_process_data = process_rec acc c in
      let cond = cond_process_data.term in
      dumloc (Pseq (dumloc (Pif (dumloc (Pnot cond), loc_pterm (pfailwith "not_valid_condition"), None)), x))
    )

let build_match_act info (m : model_unloc) (t : Model.transaction) (action : Model.pterm option) (x : pterm) : pterm =
  let act, _ret = process_action info m t action x in act

let build_match_state _info (_m : model_unloc) (_t : Model.transaction) (from : string) ((to_, _cond, _action) : (lident * Model.pterm option * Model.pterm option))  : pterm =


  let to_ = unloc to_ in
  let act = dumloc (Pletin (dumloc "s", loc_pterm (Papp (Pvar "update_storage",[Pvar "s";
                                                    Papp (Pvar "_global_st", [Pvar "s"]);
                                                    Pvar to_])), None, loc_pterm (Ptuple [Pvar "_ops"; Pvar "s"])))
  |> build_match_act _info _m _t _action
  |> build_match_cond _info _m _cond
  in

  dumloc (Pmatchwith (
      loc_pterm (Papp (Pvar "_global_st", [Pvar "s"])),
      [
        (dumloc (Mapp (Qident (dumloc from), [])), act);
        (dumloc Mwild, loc_pterm (pfailwith "not_found"))
      ]
    ))

let process_state_machine (info : info) (m : model_unloc) (t : Model.transaction) ((pt, ret) : 'a * process_data) =
  match t.transition with
  | None -> (pt, ret)
  | Some (_id, from, l) ->
    begin
      let pt = dumloc (Pletin (dumloc "_ops", loc_pterm (Papp (Pvar ("get_0_2"), [Pvar "_sops"])) , None,
               dumloc (Pletin (dumloc "s",    loc_pterm (Papp (Pvar ("get_1_2"), [Pvar "_sops"])) , None, pt)))) in
      let from =
        begin
          match unloc from with
          | Sref lident -> unloc lident
          | _ -> raise (Anomaly "process_state_machine")
        end
      in
      let pt = List.fold_right (fun (x : (lident * Model.pterm option * Model.pterm option) ) acc ->
          let act = build_match_state info m t from x in
          dumloc (Pletin (dumloc "_sops", act, None, acc))) l pt in
      (pt, {ret with side = true})
    end

let process_add_ops pt =
  dumloc (Pletin (dumloc "_ops", loc_pterm (Pvar "empty_ops"), None, pt))

let transform_transaction (info : info) (m : model_unloc) (t : Model.transaction) : transaction_ws * asset_function list =
  let args0 = compute_args info t in
  let pt0 = loc_pterm (Ptuple[Pvar "_ops"; Pvar "s"]) in

  let args_p =
    if List.length args0 = 0
    then Some (Flocal (lstr "unit"))
    else (Some (Ftuple (List.fold_left
                          (fun acc (i : arg_ret) ->
                             acc @
                             (match i.typ with
                              | ATSimple x -> [x]
                              | ATAsset (_, k, l) -> k::l)
                          )
                          [] args0))) in

  let args = List.map mk_arg [("p", args_p);
                              ("s", Some (Flocal (lstr "storage")))] in

  let pt, ret =
    process_action info m t t.action pt0
    |> process_state_machine info m t in

  let action =
    pt
    |> process_called_by t
    |> process_args args0
    |> process_add_ops in

  {
    dummy_transaction with
    name         = t.name;
    args         = args;
    calledby     = None;
    condition    = None;
    transition   = None;
    spec         = None;
    action       = Some action;
    side         = ret.side;
    loc          = Location.dummy;
  }, ret.funs

let transform_transactions (info : info) (m : model_unloc) : (transaction_ws list * asset_function list) =
  List.fold_left (fun (trs, assfuns) (t : Model.transaction) ->
      let a, b = transform_transaction info m t in
      (trs @ [a], assfuns @ b))
    ([], []) m.transactions

let fun_trans (info : info) (m : model_unloc) (mws : model_with_storage) : model_with_storage =
  let (transactions, list) : (transaction_ws list * asset_function list) = transform_transactions info m in
  let list = List.flatten (List.map (fun (x : asset) ->
      let name = unloc x.name in
      [MkAsset name; Get name]) m.assets) @ list in
  let functions : function_ws list = generate_asset_functions info list in
  { mws with
    functions = functions @ mws.functions;
    transactions = transactions @ mws.transactions;
  }

let record_model_to_modelws (info : info) (m : model) : model_with_storage =
  let m_unloc = unloc m in
  {
    name         = m_unloc.name;
    enums        = mk_enums info m_unloc;
    records      = mk_records info m_unloc;
    storage      = mk_storage info m_unloc;
    functions    = [];
    transactions = [];
  }
  |> (fun_trans info m_unloc)

let model_to_modelws (info : info) (m : model) : model_with_storage =
  (match !Modelinfo.storage_policy with
  | Record -> record_model_to_modelws
  | Flat -> flat_model_to_modelws) info m
