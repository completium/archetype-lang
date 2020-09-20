
open Location
module M = Model
open Tools
open Mlwtree

(* ------------------------------------------------------------------- *)
type error_desc =
  | NotSupported of string
  | TODONotTranslated of string

let pp_error_desc fmt = function
  | NotSupported msg ->
    Format.fprintf fmt
      "Not supported: %s" msg
  | TODONotTranslated msg ->
    Format.fprintf fmt
      "Not translated: %s" msg

type error = Location.t * error_desc

let emit_error (lc, error) =
  let str : string = Format.asprintf "%a@." pp_error_desc error in
  let pos : Position.t list = [location_to_position lc] in
  Error.error_alert pos str (fun _ -> ())

let dl = with_dummy_loc

(* Constants -------------------------------------------------------------------*)

let gArchetypeDir   = "archetype"
let gArchetypeLib   = "Lib"
let gArchetypeField = "Field"
let gArchetypeView  = "View"
let gArchetypeColl  = "Collection"
let gArchetypeAgg   = "Aggregate"
let gArchetypeSum   = "Sum"
let gArchetypeSort  = "Sort"
let gArchetypeTrace = "Trace"
let gArchetypeSet   = "Set"
let gArchetypeList  = "List"

let gOperations     = "ops"

let gListAs         = "L"
let gFieldAs        = "F"
let gViewAs         = "V"

let mk_module_name id =
  if compare (String.get id 0) '_' = 0 then
    String.capitalize_ascii (String.sub id 1 ((String.length id) - 1))
  else String.capitalize_ascii id

let mk_id i          = "_" ^ i

let mk_ac_id a        = mk_id (a ^ "_assets")
let mk_ac_added_id a  = mk_id (a ^ "_assets_added")
let mk_ac_rmed_id a   = mk_id (a ^ "_assets_removed")
let mk_ac_unmvd_id a  = mk_id (a ^ "_assets_unmoved")

let mk_aggregate_id aggid = gArchetypeAgg ^ "_" ^ aggid

let gs                = "_s"
let gsinit            = "_s_init"
let gsarg             = "_s_arg"

let mk_ac_st s a      = Tdoti (s, mk_ac_id a)
let mk_ac_old_st s a  = Tdot (Told (Tvar s), Tvar (mk_ac_id a))
let mk_ac a           = mk_ac_st gs a
let mk_ac_old a       = mk_ac_old_st gs a

let mk_ac_added_st s a     = Tdoti (s, mk_ac_added_id a)
let mk_ac_old_added_st s a = Tdot (Told (Tvar s), Tvar (mk_ac_added_id a))
let mk_ac_added a          = mk_ac_added_st gs a
let mk_ac_old_added a      = mk_ac_old_added_st gs a

let mk_ac_rmed_st s a      = Tdoti (s, mk_ac_rmed_id a)
let mk_ac_old_rmed_st s a  = Tdot (Told (Tvar s), Tvar (mk_ac_rmed_id a))
let mk_ac_rmed a           = mk_ac_rmed_st gs a
let mk_ac_old_rmed a       = mk_ac_old_rmed_st gs a

let mk_field_id a     = gArchetypeField^ "_" ^ a
let mk_view_id a      = gArchetypeView ^ "_" ^ a

(* Use ---------------------------------------------------------------------------*)

let mk_use_list = Duse (false,["list";"List"],Some gListAs) |> loc_decl |> deloc

let mk_use = Duse (false,[gArchetypeDir;gArchetypeLib],None) |> loc_decl |> deloc

let mk_use_field = Duse (false,[gArchetypeDir;gArchetypeField],Some gFieldAs) |> loc_decl |> deloc

let mk_use_view = Duse (false,[gArchetypeDir;gArchetypeView],Some gViewAs) |> loc_decl |> deloc

let mk_use_module m = Duse (false,[deloc m],None) |> loc_decl |> deloc

let mk_use_euclidean_div m =
  if M.Utils.with_division m then
    [Duse (true,["int";"EuclideanDivision"],None)  |> loc_decl |> deloc]
  else []
let mk_use_min_max m =
  if M.Utils.with_min_max m then
    [Duse (true,["int";"MinMax"],None) |> loc_decl |> deloc]
  else []

(* ---------------------------------------------------------------------------- *)
let map_lident (i : M.lident) : loc_ident = {
  obj = i.pldesc;
  loc = i.plloc;
}

let map_btype = function
  | M.Bunit          -> Tyunit
  | M.Bbool          -> Tybool
  | M.Bint           -> Tyint
  | M.Brational      -> Tyrational
  | M.Bdate          -> Tydate
  | M.Bduration      -> Tyduration
  | M.Btimestamp     -> Tyint
  | M.Bstring        -> Tystring
  | M.Baddress       -> Tyaddr
  | M.Brole          -> Tyrole
  | M.Bcurrency      -> Tytez
  | M.Bsignature     -> Tysignature
  | M.Bkey           -> Tykey
  | M.Bkeyhash       -> Tykeyhash
  | M.Bbytes         -> Tybytes
  | M.Bnat           -> Tyuint
  | M.Bchainid       -> Tychainid

let get_type_idx t = List.index_of (M.cmp_type t)

let mk_map_name m t = "map"^(string_of_int (get_type_idx t (M.Utils.get_all_map_types m)))
let mk_set_name m t = "set"^(string_of_int (get_type_idx t (M.Utils.get_all_set_types m)))
let mk_list_name m t = "list"^(string_of_int (get_type_idx t (M.Utils.get_all_list_types m)))

let rec map_mtype m (t : M.type_) : loc_typ =
  dl (match t with
      | M.Tasset id                           -> Tyasset (map_lident id)
      | M.Tenum id                            -> Tyenum (map_lident id)
      | M.Tbuiltin v                          -> map_btype v
      | M.Tcontainer (Tasset id,M.Partition)  -> Typartition (dl (mk_field_id (unloc id)))
      | M.Tcontainer (Tasset id,M.Aggregate)  -> Tyaggregate (dl (mk_field_id (unloc id)))
      | M.Tcontainer (Tasset id,M.View)       -> Tyview (dl (mk_view_id (unloc id)))
      | M.Tcontainer (Tasset id,M.Collection) -> Tycoll (map_lident id)
      | M.Toption t                           -> Tyoption (map_mtype m t)
      | M.Ttuple l                            -> Tytuple (l |> List.map (map_mtype m))
      | M.Tunit                               -> Tyunit
      | M.Tstate                              -> Tystate
      | M.Tmap (_, _, _)                      -> Tycoll (dl (mk_map_name m t))
      | M.Tstorage                            -> Tystorage
      | M.Toperation                          -> Tyoperation (* TODO: replace by the right type *)
      | M.Tprog _                             -> Tyunit (* TODO: replace bmy the right type *)
      | M.Tvset _                             -> Tyunit (* TODO: replace by the right type *)
      | M.Ttrace _                            -> Tyunit (* TODO: replace by the right type *)
      | M.Tset t                              -> Tyset (dl (mk_set_name m (Tset t)))
      | M.Tlist t                             -> Tylist (map_mtype m t)
      | M.Tcontract _                         -> Tycontract
      | M.Trecord id                          -> Tyrecord (map_lident id)
      | _ -> print_endline (Format.asprintf "%a@." M.pp_type_ t); assert false)

let mk_list_name_from_mlwtype m t =
  let idx =
    M.Utils.get_all_list_types m
    |>  List.map (map_mtype m)
    |>  List.map unloc_type
    |> List.index_of (cmp_type (Tylist t)) in
  "List"^(string_of_int idx)

let rec mk_eq_type m e1 e2 = function
  | Tyunit -> Ttrue
  | Tybool -> Tor (Tpand (Tvar e1,Tvar e2),Tpand(Tnot (Tvar e1), Tnot (Tvar e2)))
  | Tyrational -> Tapp (Tvar "rat_eq",[Tvar e1; Tvar e2])
  | Tystring -> Teq (Tystring, Tvar e1, Tvar e2)
  | Tyaddr -> Teq (Tyaddr, Tvar e1, Tvar e2)
  | Tyrole -> Teq (Tyrole, Tvar e1, Tvar e2)
  | Tyasset a -> Tapp (Tvar ("eq_"^a),[Tvar e1; Tvar e2])
  | Typartition a -> Teqfield(a, Tvar e1, Tvar e2)
  | Tyaggregate a -> Teqfield(a, Tvar e1, Tvar e2)
  | Tyenum i -> Tapp (Tvar ("eq_"^i),[Tvar e1; Tvar e2])
  | Tyoperation -> Tapp (Tvar "_eq_operation",[Tvar e1; Tvar e2])
  | Tylist t -> Tapp (Tdoti (mk_list_name_from_mlwtype m t,"eq_list"),[Tvar e1; Tvar e2])
  | Tyoption t -> Tmatch (
      Ttuple [Tvar e1; Tvar e2], [
        Tpatt_tuple [Tpsome (e1^"v1"); Tpsome (e2^"v2")], mk_eq_type m (e1^"v1") (e2^"v2") t;
        Tpatt_tuple [Twild;Twild], Tfalse
      ])
  | Tytuple l ->
    let cmps = List.mapi (fun i t ->
        let e1i = e1^(string_of_int i) in
        let e2i = e2^(string_of_int i) in
        mk_eq_type m e1i e2i t
      ) l in
    let cmp = List.fold_left (fun acc cmp -> Tpand (acc,cmp)) (List.hd cmps) (List.tl cmps) in
    Tmatch (
      Ttuple [Tvar e1; Tvar e2], [
        Tpatt_tuple [
          Tpatt_tuple (List.mapi (fun i _ -> Tconst (e1^(string_of_int i)))l);
          Tpatt_tuple (List.mapi (fun i _ -> Tconst (e2^(string_of_int i)))l)
        ], Tif (cmp, Ttrue, Some Tfalse);
        Tpatt_tuple [Twild;Twild], Tfalse
      ])
  | _ -> Teq (Tyint, Tvar e1, Tvar e2)

let rec mk_le_type e1 e2 = function
  | Tyunit -> Ttrue
  | Tybool -> Tor ((Tnot (Tvar e1), (Tvar e2)))
  | Tyrational -> Tapp (Tvar "rat_cmp",[Tvar "OpCmpLe"; Tvar e1; Tvar e2])
  | Tystring -> Tle (Tystring, Tvar e1, Tvar e2)
  | Tyaddr -> Tle (Tyaddr, Tvar e1, Tvar e2)
  | Tyrole -> Tle (Tyrole, Tvar e1, Tvar e2)
  | Tytuple l ->
    let cmps = List.mapi (fun i t ->
        let e1i = e1^(string_of_int i) in
        let e2i = e2^(string_of_int i) in
        mk_le_type e1i e2i t
      ) l in
    let cmp = List.fold_left (fun acc cmp -> Tpand(cmp,acc)) (List.hd cmps) (List.tl cmps) in
    Tmatch (
      Ttuple [Tvar e1; Tvar e2], [
        Tpatt_tuple [
          Tpatt_tuple (List.mapi (fun i _ -> Tconst (e1^(string_of_int i)))l);
          Tpatt_tuple (List.mapi (fun i _ -> Tconst (e2^(string_of_int i)))l)
        ], Tif (cmp, Ttrue, Some Tfalse);
        Tpatt_tuple [Twild;Twild], Tfalse
      ])
  | _ -> Tle (Tyint, Tvar e1, Tvar e2)

(* Trace -------------------------------------------------------------------------*)
type change =
  | CAdd of ident
  | CRm of ident
  | CUpdate of ident
  | CTransfer of ident
  | CGet of ident
  | CIterate of ident
  | CCall of ident

type trace_id_type =
  | Asset
  | Entry
  | Field

let trace_value_type_to_string = function Asset -> "A" | Entry -> "E" | Field -> "F"

let mk_trace_id trtyp s = (trace_value_type_to_string trtyp) ^ (String.capitalize_ascii s)

let mk_change_term tr =
  match tr with
  | CAdd id -> Tapp (Tdoti("Tr",
                           "TrAdd_"),
                     [Tvar (mk_trace_id Asset id)])
  | CRm id -> Tapp (Tdoti("Tr",
                          "TrRm_"),
                    [Tvar (mk_trace_id Asset id)])
  | CUpdate id -> Tapp (Tdoti("Tr",
                              "TrUpdate_"),
                        [Tvar (mk_trace_id Field id)])
  | CGet id -> Tapp (Tdoti("Tr",
                           "TrGet_"),
                     [Tvar (mk_trace_id Asset id)])
  | _ -> assert false

let mk_trace tr =
  let gstr = Tdoti(gs,
                   mk_id "tr") in
  Tassign (gstr,
           Tcons (gListAs, mk_change_term tr,
                  gstr)
          ) |> loc_term

let mk_trace_asset m =
  let assets = M.Utils.get_assets m in
  if List.length assets > 0 then
    [ Denum ("_asset",
             assets
             |> List.map (fun (a : M.asset) -> mk_trace_id Asset (unloc a.name)))
      |> loc_decl]
  else []

let mk_trace_entry m =
  Denum ("_entry",
         M.Utils.get_entries m
         |> List.map (fun (_, (f : M.function_struct)) -> mk_trace_id Entry (unloc f.name)))
  |> loc_decl


let mk_trace_field m =
  Denum ("_field",
         (M.Utils.get_vars m
          |> List.map (fun (v : M.var) -> mk_trace_id Field (unloc v.name))) @
         (M.Utils.get_assets m
          |> List.map (fun (a : M.asset) ->
              List.map (fun (x : M.asset_item) -> mk_trace_id Field (unloc x.name)) a.values
            )
          |> List.flatten))
  |> loc_decl

let mk_trace_clone () =
  Dclone (
    [gArchetypeDir;gArchetypeTrace], "Tr", [
      Ctype ("_asset", Tyasset "_asset");
      Ctype ("_entry", Tyasset "_entry");
      Ctype ("_field", Tyasset "_field")
    ])
  |> loc_decl

let mk_trace_utils m =
  if M.Utils.with_trace m then
    (mk_trace_asset m) @ [
      mk_trace_entry m;
      mk_trace_field m;
      mk_trace_clone ()
    ] else []

(* Storage -----------------------------------------------------------------------*)

let mk_default_init = function
  | Drecord (n,fs) ->
    Dfun {
      name     = "mk_default_" ^ n;
      logic    = NoMod;
      args     = [];
      returns  = Tyasset n;
      raises   = [];
      fails    = [];
      variants = [];
      requires = [];
      ensures  = [];
      body     = Trecord (None, List.map (fun (f:field) ->
          f.name, f.init
        ) fs);
    }
  | _ -> assert false


let mk_collection_field asset to_id init = {
  name     = dl (to_id asset);
  typ      = loc_type (Tycoll asset);
  init     = begin match init with
    | Some l -> dl (Tmkcoll (dl asset, l))
    | None -> loc_term (Temptycoll asset);
  end;
  mutable_ = true;
}

let mk_const_fields m = [
  { name = mk_id gOperations   ; typ = Tylist Tyoperation ; init = Tnil gListAs; mutable_ = true; };
  { name = mk_id "balance"     ; typ = Tytez;      init = Tint Big_int.zero_big_int; mutable_ = true; };
  { name = mk_id "transferred" ; typ = Tytez;      init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = mk_id "caller"      ; typ = Tyaddr;     init = Tdefaultaddr; mutable_ = false; };
  { name = mk_id "source"      ; typ = Tyaddr;     init = Tdefaultaddr; mutable_ = false; };
  { name = mk_id "now"         ; typ = Tydate;     init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = mk_id "chainid"     ; typ = Tychainid;  init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = mk_id "selfaddress" ; typ = Tyaddr;     init = Tdefaultaddr; mutable_ = false; };
] @
  if M.Utils.with_trace m then
    [
      { name = mk_id "entry"     ; typ = Tyoption (Tyasset "_entry"); init = Tnone; mutable_ = false; };
      { name = mk_id "tr"        ; typ = Tyasset ("Tr._traces"); init = Tnil gListAs; mutable_ = true; }
    ]
  else []


let mk_sum_clone_id m a f = (String.capitalize_ascii a) ^"Sum" ^ (string_of_int (M.Utils.get_sum_idx m a f))
let mk_sum_clone_from_id asset id = (String.capitalize_ascii asset) ^"Sum" ^ (string_of_int id)
let mk_get_sum_value_id asset id = "get_" ^ asset ^ "_sum" ^ (string_of_int id)
let mk_get_sum_value_from_pos_id asset id = (mk_get_sum_value_id asset id)^"_from_pos"

let mk_sum a i v c = Tvsum ( mk_sum_clone_from_id a i, v, c)
let mk_sum_from_col a i c = Tcsum (mk_sum_clone_from_id a i, c)

let mk_sum_clone m asset key tkey formula =
  let cap_asset = String.capitalize_ascii asset in
  let id = M.Utils.get_sum_idx m asset formula in
  Dclone (
    [gArchetypeDir;gArchetypeSum],
    mk_sum_clone_from_id asset id, [
      Ctype ("collection", Tyasset (cap_asset ^ ".collection"));
      Ctype ("view", Tyasset ((String.capitalize_ascii (mk_view_id asset))^ ".view"));
      Ctype ("t", Tyasset asset);
      Ctype ("tk", tkey |> map_mtype m |> unloc_type);
      Cval ("field", mk_get_sum_value_id asset id);
      Cval ("view_to_list", cap_asset ^ ".view_to_list");
      Cval ("to_view", cap_asset ^ ".to_view");
      Cval ("empty", (String.capitalize_ascii (mk_view_id asset)) ^ ".empty");
      Cval ("contains", (String.capitalize_ascii (mk_view_id asset)) ^ ".contains");
      Cval ("nth", (String.capitalize_ascii (mk_view_id asset)) ^ ".nth");
      Cval ("head", (String.capitalize_ascii (mk_view_id asset)) ^ ".head");
      Cval ("tail", (String.capitalize_ascii (mk_view_id asset)) ^ ".tail");
      Cval ("card", (String.capitalize_ascii (mk_view_id asset)) ^ ".card");
      Cval ("add", cap_asset ^ ".add");
      Cval ("remove", cap_asset ^ ".remove");
      Cval ("set", cap_asset ^ ".set");
      Cval ("get", cap_asset ^ ".get");
      Cval ("keyt", key);
    ]
  )

(* asset is the asset name
   f is the partition field name
   kt is the key type
   pa is the partitionned asset name
   kpt is the partionned asset key type
*)
let mk_partition_axiom asset f _kt pa kpt : decl =
  Dtheorem (Axiom,
            asset ^ "_" ^ f ^ "_is_partition",
            Tforall ([["s"],Tystorage;["a"],Tyasset asset;["k"],kpt],
                     Timpl (Tmem (asset,
                                  Tvar("a"),
                                  mk_ac_st "s" asset),
                            Timpl (Tlmem (gListAs,
                                          Tvar "k",
                                          Tapp (Tvar f,
                                                [Tvar "a"])),
                                   Tccontains (pa,
                                               Tvar "k",
                                               mk_ac_st "s" pa)))))

(* Sort ----------------------------------------------------------------------*)

let sort_kind_to_string = function
  | M.SKasc -> "asc"
  | M.SKdesc -> "desc"

let mk_cmp_function_id asset fields = "cmp_" ^ asset ^ "_" ^
                                      (String.concat "_" (List.map (fun (f,k) ->
                                           f ^ "_" ^ (sort_kind_to_string k)) fields))

let rec mk_cmp_function_body m asset fields =
  match fields with
  | [field,kind] ->
    let a, b =
      begin match kind with
        | M.SKasc -> Tdoti("a", field),Tdoti("b", field)
        | M.SKdesc -> Tdoti("b", field),Tdoti("a", field)
      end in
    let (_,typ,_) = M.Utils.get_asset_field m (asset,field) in
    let t = map_mtype m typ |> unloc_type in
    Tle (t, a, b)
  | (field,kind)::tl ->
    let a, b =
      begin match kind with
        | M.SKasc -> Tdoti("a", field),Tdoti("b", field)
        | M.SKdesc -> Tdoti("b", field),Tdoti("a", field)
      end in
    Tif (
      Tlt (Tyint,a,b),
      Ttrue,
      Some (Tif (
          Teq (Tyint,a,b),
          mk_cmp_function_body m asset tl,
          Some (Tfalse)
        ))
    )
  | [] -> Ttrue

let mk_cmp_function m asset fields =
  let decl : (term, typ, ident) abstract_decl = Dfun {
      name     = mk_cmp_function_id asset fields;
      logic    = Logic;
      args     = ["a", Tyasset asset; "b", Tyasset asset];
      returns  = Tybool;
      raises   = [];
      fails    = [];
      variants = [];
      requires = [];
      ensures  = [];
      body     = mk_cmp_function_body m asset fields
    } in
  decl

let mk_sort_clone_id asset fields =
  (String.capitalize_ascii asset) ^ "Sort" ^
  (String.concat "" (List.map (fun (f,k) ->
       (String.capitalize_ascii f) ^ (String.capitalize_ascii (sort_kind_to_string k))) fields))

let mk_sort_clone _m asset fields =
  let cap_asset = String.capitalize_ascii asset in
  Dclone (
    [gArchetypeDir;gArchetypeSort],
    mk_sort_clone_id asset fields, [
      Ctype ("t", Tyasset asset);
      Ctype ("view", Tyasset ((mk_view_id asset)^".view"));
      Ctype ("collection", Tyasset (cap_asset ^ ".collection"));
      Cval ("cmp", mk_cmp_function_id asset fields);
      Cval ("view_to_list", cap_asset ^ ".view_to_list");
      Cval ("list_to_view", cap_asset ^ ".list_to_view")
    ])

(* filter --------------------------------------------------------------------*)

type filter = Select | Removeif

(* TODO : complete mapping *)
let rec mk_afun_test = function
  | Tdot (Tvar v,f) when compare v "the" = 0 -> Tdot (Tvar "a",f)
  | Tnow _ -> Tvar (mk_id "now")
  | Tcaller _ -> Tvar (mk_id "caller")
  | Tsender _ -> Tvar (mk_id "source")
  | _ as t -> map_abstract_term mk_afun_test id id t

let rec acc_has_id id = function
  | [] -> false
  | (_,i,_)::_ when compare id i = 0 -> true
  | (_,_,_)::tl -> acc_has_id id tl

(* TODO : complete mapping
   argument extraction is done on model's term because it is typed *)
let extract_args test =
  let rec internal_extract_args acc (term : M.mterm) =
    match term.M.node with
    | M.Mnow ->    if acc_has_id "now"    acc then acc else acc @ [term,mk_id "now", Tydate]
    | M.Mcaller -> if acc_has_id "caller" acc then acc else acc @ [term,mk_id "caller", Tyaddr]
    | M.Msource -> if acc_has_id "source" acc then acc else acc @ [term,mk_id "source", Tyaddr]
    | _ -> M.fold_term internal_extract_args acc term in
  internal_extract_args [] test

let mk_filter_name m asset test = function
  | Select   -> "select_" ^ asset ^ "_" ^ (string_of_int (M.Utils.get_select_idx m asset test))
  | Removeif -> "removeif_" ^ asset ^ "_" ^ (string_of_int (M.Utils.get_removeif_idx m asset test))

let mk_select_name m asset test = mk_filter_name m asset test Select
let mk_removeif_name m asset test = mk_filter_name m asset test Removeif

let mk_filter_predicate ftyp m asset test filter args =
  let args : (string * typ) list = List.map (fun (i,t) -> (i, (map_mtype m t |> unloc_type))) args in
  let name = mk_filter_name m asset test ftyp in
  let body = mk_afun_test filter in
  Dfun {
    name     = name;
    logic    = Logic;
    args     = args @ (extract_args test |> List.map (fun (_,a,b) -> a,b)) @ ["a", Tyasset asset];
    returns  = Tybool;
    raises   = [];
    fails    = [];
    variants = [];
    requires = [];
    ensures  = [{
        id = name ^ "_post";
        form = (Teq(Tyint,Tresult,body));
      }];
    body     = body;
  }

let mk_select_predicate = mk_filter_predicate Select
let mk_removeif_predicate = mk_filter_predicate Removeif

(* Definitions Utils --------------------------------------------------------- *)

let get_definition_body m id =
  let rec get_body = function
    | [] -> None
    | (def : M.definition) :: _ when compare (unloc def.name) id = 0 -> Some def.body
    | _ :: tl -> get_body tl in
  let rec get_spec = function
    | [] -> None
    | spec :: tl ->
      begin match get_body spec.M.definitions with
        | Some b -> Some b
        | None -> get_spec tl
      end in
  get_spec (M.Utils.get_specifications m)

let get_predicate_body m id =
  let rec get_body = function
    | [] -> None
    | (def : M.predicate) :: _ when compare (unloc def.name) id = 0 -> Some def.body
    | _ :: tl -> get_body tl in
  let rec get_spec = function
    | [] -> None
    | spec :: tl ->
      begin match get_body spec.M.predicates with
        | Some b -> Some b
        | None -> get_spec tl
      end in
  get_spec (M.Utils.get_specifications m)

let is_predicate m id =
  let rec search_pred = function
    | [] -> false
    | (def : M.predicate) :: _ when compare (unloc def.name) id = 0 -> true
    | _ :: tl -> search_pred tl in
  let rec search_spec = function
    | [] -> false
    | spec :: tl ->
      if search_pred spec.M.predicates then
        true
      else search_spec tl
  in
  search_spec (M.Utils.get_specifications m)

(* extracts entry's params for definitions *)
let extract_def_args m body =
  let rec internal_extract_def_args acc (term : M.mterm) =
    match term.M.node with
    | M.Mvar (id ,Vparam, _, _) ->
      if acc_has_id (unloc id) acc
      then acc
      else acc @ [Tvar (unloc id), unloc id, map_mtype m term.type_]
    | _ -> M.fold_term internal_extract_def_args acc term in
  internal_extract_def_args [] body

let get_def_params m id =
  match get_definition_body m id with
  | Some b -> extract_def_args m b |> List.map (fun (p,_,_) -> p)
  | None -> assert false

let get_pred_params m id =
  match get_predicate_body m id with
  | Some b -> extract_def_args m b |> List.map (fun (p,_,_) -> p)
  | None -> assert false

(* Utils --------------------------------------------------------------------- *)

let wdl (l : 'a list)  = List.map dl l
let unloc_decl = List.map unloc_decl
let loc_decl   = List.map loc_decl
let loc_field  = List.map loc_field
let deloc (l : 'a list) = List.map deloc l

let rec zip l1 l2 l3 l4 l5 l6 l7 l8 =
  match l1,l2,l3,l4,l5,l6,l7,l8 with
  | e1::tl1,e2::tl2,e3::tl3,e4::tl4,e5::tl5,e6::tl6,e7::tl7,e8::tl8 ->
    e1::e2::e3::e4::e5::e6::e7::e8::(zip tl1 tl2 tl3 tl4 tl5 tl6 tl7 tl8)
  | _ -> []

let cap s = mk_loc s.loc (String.capitalize_ascii s.obj)

(* Map type -------------------------------------------------------------------*)

let mk_eq_type_fun m id t = Dfun {
    name = "eq_" ^ id |> dl;
    logic = Logic;
    args = [
      dl "e1", t;
      dl "e2", t
    ];
    returns = Tybool |> dl;
    raises = [];
    fails = [];
    variants = [];
    requires = [];
    ensures = [];
    body = loc_term (mk_eq_type m "e1" "e2" (unloc_type t));
  }

let mk_le_type_fun _m id t = Dfun {
    name = "le_" ^ id |> dl;
    logic = Logic;
    args = [
      dl "e1", t;
      dl "e2", t
    ];
    returns = Tybool |> dl;
    raises = [];
    fails = [];
    variants = [];
    requires = [];
    ensures = [];
    body = loc_term (mk_le_type "e1" "e2" (unloc_type t));
  }

let mk_map_clone id k t =
  Dclone ([gArchetypeDir;gArchetypeColl] |> wdl,
          String.capitalize_ascii id |> dl, [
            Ctype (dl "t", t);
            Ctype (dl "tk", k);
            Cval  ("keyt" |> dl, "fst" |> dl);
            Cval  ("eqt" |> dl, "eq_" ^ id |> dl);
            Cval  ("lek" |> dl, "le_" ^ id |> dl);
          ]
         )

let mk_map_type m (t : M.type_) =
  match t with
  | Tmap (_, k, v) ->
    let map_name = mk_map_name m t in
    let t = M.Ttuple [k;v] in
    let typ = map_mtype m t in
    let key = map_mtype m k in [
      mk_eq_type_fun m map_name typ;
      mk_le_type_fun m map_name key;
      mk_map_clone map_name key typ
    ]
  | _ -> assert false

(* Set type -------------------------------------------------------------------*)

let mk_set_clone id t =
  Dclone ([gArchetypeDir;gArchetypeSet] |> wdl,
          String.capitalize_ascii id |> dl, [
            Ctype (dl "t", t);
            Cval  ("eqt" |> dl, "eq_" ^ id |> dl);
            Cval  ("le_t" |> dl, "le_" ^ id |> dl);
          ]
         )

let mk_set_type m (t : M.type_) =
  match t with
  | Tset et ->
    let set_name = mk_set_name m t in
    let et = map_mtype m et in [
      mk_eq_type_fun m set_name et;
      mk_le_type_fun m set_name et;
      mk_set_clone set_name et;
    ]
  | _ -> assert false

(* List type -------------------------------------------------------------------*)

let mk_list_clone id t =
  Dclone ([gArchetypeDir;gArchetypeList] |> wdl,
          String.capitalize_ascii id |> dl, [
            Ctype (dl "t", t);
            Cval  ("eqt" |> dl, "eq_" ^ id |> dl)
          ]
         )

let mk_list_type m (t : M.type_) =
  match t with
  | Tlist et ->
    let list_name = mk_list_name m t in
    let et = map_mtype m et in [
      mk_eq_type_fun m list_name et;
      mk_list_clone list_name et;
    ]
  | _ -> assert false

(* record type ----------------------------------------------------------------*)
let map_record_fields m =
  List.map (fun (f : M.record_field) -> {
        name     = map_lident f.name;
        typ      = map_mtype m f.type_;
        init     = loc_term Tnone;
        mutable_ = true;
      })

let mk_record m (r : M.record) : (loc_term, loc_typ, loc_ident) abstract_decl =
  Drecord (map_lident r.name, map_record_fields m r.fields)

(* Map model term -------------------------------------------------------------*)

let map_lidents = List.map map_lident

let rec type_to_init m (typ : loc_typ) : loc_term =
  mk_loc typ.loc (match typ.obj with
      | Tyasset i     -> Tapp (loc_term (Tvar ("mk_default_"^i.obj)),[])
      | Typartition i -> Temptyfield i
      | Tyaggregate i -> Temptyfield i
      | Tycoll i      -> Temptycoll i
      | Tylist _      -> Tnil (dl gListAs)
      | Tyview i      -> Temptyview i
      | Tymap i       -> Tvar (mk_loc typ.loc ("const (mk_default_" ^ i.obj ^ " ())"))
      | Tyenum i      -> Tvar (mk_loc typ.loc (unloc (M.Utils.get_enum m i.obj).initial))
      | Tytuple l     -> Ttuple (List.map (type_to_init m) l)
      | Tybool        -> Ttrue
      | Tystring      -> Temptystr
      | Tyrole        -> Tdefaultaddr
      | Tyaddr        -> Tdefaultaddr
      | _             -> Tint Big_int.zero_big_int)

let is_local_invariant _m an t =
  let rec internal_is_local acc (term : M.mterm) =
    match term.M.node with
    | M.Mforall (_i,M.Tasset a,_,_b) -> not (compare (a |> unloc) an = 0)
    | M.Msum (a,_,_) -> not (compare a an = 0)
    | M.Mselect (a, _, _, _, _) -> not (compare a an = 0)
    | _ -> M.fold_term internal_is_local acc term in
  internal_is_local true t

let adds_asset m an b =
  let rec internal_adds acc (term : M.mterm) =
    match term.M.node with
    | M.Maddasset (a,_) ->  compare a an = 0
    | M.Maddfield (a,f,_,_) ->
      let (pa,_,_) = M.Utils.get_container_asset_key m a f in
      compare pa an = 0
    | _ -> M.fold_term internal_adds acc term in
  internal_adds false b

let is_only_security (s : M.security_predicate) =
  match s.s_node with
  | M.SonlyByRole _ -> true
  | M.SonlyInEntry _ -> true
  | M.SonlyByRoleInEntry _ -> true
  | _ -> false

let map_action_to_change = function
  | M.ADadd i      -> CAdd i
  | M.ADremove i   -> CRm i
  | M.ADupdate i   -> CUpdate i
  | M.ADtransfer i -> CTransfer i
  | M.ADget i      -> CGet i
  | M.ADiterate i  -> CIterate i
  | M.ADcall i     -> CCall i
  | _ -> assert false

let map_security_pred loc (t : M.security_predicate) =
  let vars =
    ["tr";"caller";"entry"]
    |> List.map mk_id
    |> List.map (fun v ->
        match loc with
        | `Storage -> Tvar (v)
        | `Loop    -> Tdoti(gs,v)
      )
  in
  let tr = List.nth vars 0 in
  let caller = List.nth vars 1 in
  let entry = List.nth vars 2 in
  let mk_eq a b opt = Teq (Tyint,a,
                           if opt then
                             Tsome (Tvar b)
                           else
                             match loc with
                             | `Storage -> Tvar b
                             | `Loop    -> Tdoti(gs,b)
                          ) in
  let mk_performed_by t l opt =
    Tapp (Tvar "Tr.performed_by",
          [tr;
           List.fold_left (fun acc r ->
               Tor (acc,mk_eq t r opt)
             ) (mk_eq t (List.hd l) opt) (List.tl l) ])
  in
  let mk_changes_performed_by t a l opt =
    Tapp (Tvar "Tr.changes_performed_by",
          [tr;
           Tcons (gListAs, map_action_to_change a |> mk_change_term,Tnil gListAs);
           List.fold_left (fun acc r ->
               Tor (acc,mk_eq t r opt)
             ) (mk_eq t (List.hd l) opt) (List.tl l) ])
  in
  let mk_performed_by_2 t1 t2 l1 l2 =
    Tapp (Tvar "Tr.performed_by",
          [tr;
           Tand (
             List.fold_left (fun acc r ->
                 Tor (acc,mk_eq t1 r false)
               ) (mk_eq t1 (List.hd l1) false) (List.tl l2),
             List.fold_left (fun acc r ->
                 Tor (acc,mk_eq t2 r true)
               ) (mk_eq t2 (List.hd l2) true) (List.tl l2))])
  in
  let mk_changes_performed_by_2 t1 t2 a l1 l2 =
    Tapp (Tvar "Tr.performed_by",
          [tr;
           Tcons (gListAs, map_action_to_change a |> mk_change_term,Tnil gListAs);
           Tand (
             List.fold_left (fun acc r ->
                 Tor (acc,mk_eq t1 r false)
               ) (mk_eq t1 (List.hd l1) false) (List.tl l2),
             List.fold_left (fun acc r ->
                 Tor (acc,mk_eq t2 r true)
               ) (mk_eq t2 (List.hd l1) true) (List.tl l2))])
  in
  match t.M.s_node with
  | M.SonlyByRole (ADany,roles)     ->
    mk_performed_by caller (roles |> List.map unloc) false
  | M.SonlyInEntry (ADany,Sentry entries) ->
    mk_performed_by entry (entries |> List.map unloc |> List.map (mk_trace_id Entry)) true
  | M.SonlyByRole (a,roles)         ->
    mk_changes_performed_by caller a (roles |> List.map unloc) false
  | M.SonlyInEntry (a,Sentry entries)     ->
    mk_changes_performed_by entry
      a
      (entries |> List.map unloc |> List.map (mk_trace_id Entry))
      true
  | M.SonlyByRoleInEntry (ADany,roles,Sentry entries) ->
    mk_performed_by_2 caller entry
      (roles |> List.map unloc)
      (entries |> List.map unloc |> List.map (mk_trace_id Entry))
  | M.SonlyByRoleInEntry (a,roles,Sentry entries) ->
    mk_changes_performed_by_2 caller entry a
      (roles |> List.map unloc)
      (entries |> List.map unloc |> List.map (mk_trace_id Entry))
  | _ -> Tnottranslated

let mk_spec_invariant loc (sec : M.security_item) =
  if is_only_security sec.predicate then
    [
      {
        id = map_lident sec.label;
        form = map_security_pred loc sec.predicate |> loc_term;
      }
    ]
  else []

(* f --> f a *)
let mk_app_field (a : ident) (f : loc_ident) : loc_term * loc_term  =
  let arg   : term     = Tvar a in
  let loc_f : loc_term = mk_loc f.loc (Tvar f) in
  (loc_f,dl (Tapp (loc_f,[loc_term arg])))

let mk_invariant m n src inv : loc_term =
  let r        = M.Utils.get_asset m (unloc n) in
  let fields   = r.values |> List.map (fun (x : M.asset_item) -> (unloc x.name)) |> wdl in
  let asset    = map_lident n in
  let variable =
    match src with
    | `Preasset arg -> arg
    | _ -> "a" in
  let replacements = List.map (fun f -> mk_app_field variable f) fields in
  let replacing = List.fold_left (fun acc (t1,t2) -> loc_replace t1 t2 acc) inv replacements in
  match src with
  | `Preasset _ -> replacing
  | _ ->
    let mem_pred =
      match src with
      | `Storage -> Tmem ((unloc_ident asset),
                          Tvar variable,
                          Tvar (mk_ac_id asset.obj))
      | `Axiom   -> Tmem ((unloc_ident asset),
                          Tvar variable,
                          Tdoti ("s", mk_ac_id asset.obj))
      | `Axiom2   -> Tmem ((unloc_ident asset),
                           Tvar variable,
                           Tvar ("c"))
      | `Loop    -> Tmem ((unloc_ident asset),
                          Tvar variable,
                          mk_ac (unloc n))
      | `Prelist arg ->
        Tapp (Tvar ((String.capitalize_ascii (unloc n)) ^ ".internal_mem"),
              [Tvar variable; Tvar arg])
      | `Precoll arg -> Tmem ((unloc_ident asset),
                              Tvar variable,
                              Tvar arg)
      | _ -> Tnone
    in
    let prefix   =
      match src with
      | `Axiom ->
        Tforall ([["s"],Tystorage],
                 Tforall ([[variable],Tyasset (unloc_ident asset)],
                          Timpl (mem_pred,
                                 Ttobereplaced)))
      | `Axiom2 -> (* invariant is true for any sub collection of storage collection *)
        Tforall ([["s"],Tystorage],
                 Tforall ([["c"],Tycoll (unloc n)],
                          Timpl (
                            Tsubset (unloc n,
                                     Tvar "c",
                                     Tdoti ("s", mk_ac_id asset.obj)),
                            Tforall ([[variable],Tyasset (unloc_ident asset)],
                                     Timpl (mem_pred,
                                            Ttobereplaced)))))
      | _ ->
        Tforall ([[variable],Tyasset (unloc_ident asset)],
                 Timpl (mem_pred,
                        Ttobereplaced)) in
    loc_replace (dl Ttobereplaced) replacing (loc_term prefix)

let mk_storage_invariant m n (lbl : M.lident) (t : loc_term) = {
  id = map_lident lbl;
  form = mk_invariant m n `Storage t;
}

let mk_pre_coll m n arg inv : loc_term = mk_invariant m (dumloc n) (`Precoll arg) inv

let mk_pre_asset m n arg inv : loc_term = mk_invariant m (dumloc n) (`Preasset arg) inv

let mk_loop_invariant m n inv : loc_term = mk_invariant m (dumloc n) `Loop inv

let mk_axiom_invariant m n inv : loc_term = mk_invariant m (dumloc n) `Axiom inv

let mk_axiom2_invariant m n inv : loc_term = mk_invariant m (dumloc n) `Axiom2 inv

let mk_state_invariant _m _v (lbl : M.lident) (t : loc_term) = {
  id = map_lident lbl;
  form = Timpl (
      loc_term (Teq(Tyint,Tvar "state", Tvar (unloc _v))),
      t) |> dl
}

let mk_eq_enums m (r : M.asset) =
  List.fold_left (fun acc (item : M.asset_item) ->
      match item.type_ with
      | Tenum lid ->
        let id = unloc lid in
        if List.mem id acc then acc else acc @ [id]
      | _ -> acc
    ) [] r.values |>
  List.map (fun id ->
      Dfun  {
        name = "eq_" ^ id |> dl;
        logic = Logic;
        args = ["e1" |> dl, loc_type (Tyenum id);
                "e2" |> dl, loc_type (Tyenum id)];
        returns = Tybool |> dl;
        raises = [];
        fails = [];
        variants = [];
        requires = [];
        ensures = [];
        body = loc_term (Tmatch (
            Ttuple [Tvar "e1"; Tvar "e2"],
            List.fold_left (fun acc eval ->
                [ Tpatt_tuple [Tconst eval; Tconst eval], Ttrue ] @ acc
              ) [ Tpatt_tuple [Twild;Twild], Tfalse ] (M.Utils.get_enum_values m id)
          ));
      })

let mk_eq_key m (r : M.asset) =
  let asset = unloc r.name in
  let (_key, tkey) = M.Utils.get_asset_key m asset in
  let tkey = map_mtype m tkey in
  Dfun {
    name = "eq_"^asset^"_key" |> dl;
    logic = Logic;
    args = [
      "k1" |> dl, tkey;
      "k2" |> dl, tkey;
    ];
    returns = Tybool |> dl;
    raises = [];
    fails = [];
    variants = [];
    requires = [];
    ensures = [];
    body = loc_term (mk_eq_type m "k1" "k2" (unloc_type tkey));
  }

let mk_le_key m (r : M.asset) =
  let asset = unloc r.name in
  let (_key, tkey) = M.Utils.get_asset_key m asset in
  let tkey = map_mtype m tkey in
  Dfun {
    name = "le_"^asset^"_key" |> dl;
    logic = Logic;
    args = [
      "k1" |> dl, tkey;
      "k2" |> dl, tkey;
    ];
    returns = Tybool |> dl;
    raises = [];
    fails = [];
    variants = [];
    requires = [];
    ensures = [];
    body = loc_term (mk_le_type "k1" "k2" (unloc_type tkey));
  }

let mk_eq_asset m (r : M.asset) =
  let cmps = List.map (fun (item : M.asset_item) ->
      let id1 = "a1_"^(unloc item.name) in
      let id2 = "a2_"^(unloc item.name) in
      Tletin (false, id1, None, Tdoti("a1",unloc item.name),
              Tletin (false, id2, None, Tdoti ("a2",unloc item.name),
                      mk_eq_type m id1 id2 (unloc_type (map_mtype m item.type_))
                     )
             )
    ) r.values in
  Dfun  {
    name = "eq_" ^ (unloc r.name) |> dl;
    logic = Logic;
    args = ["a1" |> dl, Tyasset (map_lident r.name) |> dl;
            "a2" |> dl, Tyasset (map_lident r.name) |> dl];
    returns = Tybool |> dl;
    raises = [];
    fails = [];
    variants = [];
    requires = [];
    ensures = [];
    body = List.fold_left (fun acc cmp ->
        Tpand (acc,cmp)
      ) (List.hd cmps) (List.tl cmps) |> loc_term;
  }

let mk_enum _m (e : M.enum) : (loc_term,loc_typ,loc_ident) abstract_decl =
  Denum (map_lident e.name, List.map (fun (item : M.enum_item) -> map_lident item.name) e.values)

let get_fail_idx m t = succ (List.index_of (M.cmp_type t) (M.Utils.get_all_fail_types m))

let mk_exn m i t : (loc_term, loc_typ, ident with_loc) abstract_decl =
  let id = string_of_int (succ i) in
  Dexn (dl id,map_mtype m t)

let mk_field m (r : M.asset) =
  let asset = unloc r.name in
  let (_key, tkey) = M.Utils.get_asset_key m asset in
  let tkey = map_mtype m tkey in
  Dclone ([gArchetypeDir; gArchetypeField] |> wdl,
          String.capitalize_ascii (mk_field_id asset) |> dl,
          [Ctype ("tk" |> dl, tkey);
           Cval  ("eqk" |> dl, "eq_" ^ asset ^ "_key" |> dl);
           Cval  ("lek" |> dl, "le_" ^ asset ^ "_key" |> dl);
           Ctype ("view" |> dl, loc_type (Tyview (mk_view_id asset)));
           Cval  ("vmk" |> dl, (String.capitalize_ascii (mk_view_id asset))^".mk" |> dl);
           Cval  ("velts" |> dl, (String.capitalize_ascii (mk_view_id asset))^".elts" |> dl);
           Cval  ("vcontains" |> dl, (String.capitalize_ascii (mk_view_id asset))^".contains" |> dl)])

let mk_view m (r : M.asset) =
  let asset = unloc r.name in
  let (_key, tkey) = M.Utils.get_asset_key m asset in
  let tkey = map_mtype m tkey in
  Dclone ([gArchetypeDir; gArchetypeView] |> wdl,
          String.capitalize_ascii (mk_view_id asset) |> dl,
          [Ctype ("tk" |> dl, tkey);
           Cval  ("eqk" |> dl, "eq_" ^ asset ^ "_key" |> dl)])

let mk_coll m (r : M.asset) =
  let asset = unloc r.name in
  let (key, tkey) = M.Utils.get_asset_key m asset in
  let tkey = map_mtype m tkey in
  Dclone ([gArchetypeDir;gArchetypeColl] |> wdl,
          String.capitalize_ascii asset |> dl,
          [Ctype ("tk" |> dl, tkey);
           Cval  ("eqk" |> dl, "eq_" ^ asset ^ "_key" |> dl);
           Cval  ("lek" |> dl, "le_" ^ asset ^ "_key" |> dl);
           Ctype ("t" |> dl, Tyasset (dl asset) |> dl);
           Cval  ("keyt" |> dl, key |> dl);
           Cval  ("eqt" |> dl, "eq_" ^ asset |> dl);
           Ctype ("view" |> dl, loc_type (Tyview (mk_view_id asset)));
           Cval  ("vmk" |> dl, (String.capitalize_ascii (mk_view_id asset))^".mk" |> dl);
           Cval  ("velts" |> dl, (String.capitalize_ascii (mk_view_id asset))^".elts" |> dl);
           Cval  ("vcontains" |> dl, (String.capitalize_ascii (mk_view_id asset))^".contains" |> dl);
           Cval  ("vcard" |> dl, (String.capitalize_ascii (mk_view_id asset))^".card" |> dl);
           Ctype ("field" |> dl, loc_type (Tyasset ((mk_field_id asset)^".field")));
           Cval  ("felts" |> dl, (String.capitalize_ascii (mk_field_id asset))^".elts" |> dl);
           Cval  ("fcontains" |> dl, (String.capitalize_ascii (mk_field_id asset))^".contains" |> dl)
          ])

let mk_set_field_id fieldid = "set_" ^ fieldid

let mk_set_field _m asset fieldid oasset =
  let name = mk_set_field_id fieldid in
  Dfun {
    name = name |> dl;
    logic = Logic;
    args = [
      dl "f", loc_type (Tyaggregate (mk_field_id oasset));
      dl "a", loc_type (Tyasset asset)
    ];
    returns = loc_type (Tyasset asset);
    raises = [];
    fails = [];
    variants = [];
    requires = [];
    ensures = [(* {
                  id = dl (name ^ "_post") ;
                  form = loc_term (Teq(Tyint,Tresult,Tvar "s"));
                  } *)];
    body = dl (Trecord(Some (loc_term (Tvar "a")), [
        dl fieldid, loc_term (Tvar "f")
      ]))
  }

let mk_aggregates m (r : M.asset) =
  let asset = unloc r.name in
  let capasset = String.capitalize_ascii asset in
  let (_, tkey) = M.Utils.get_asset_key m asset in
  let tkey = map_mtype m tkey in
  let aggregates = M.Utils.get_asset_containers m asset in
  List.fold_left (fun acc (agg_id, field_type, _) ->
      let oasset = M.Utils.type_to_asset field_type in
      let (_,oasset_key_type) = M.Utils.get_asset_key m oasset in
      let agg_key_type = map_mtype m oasset_key_type in
      let clone = Dclone (
          [gArchetypeDir; gArchetypeAgg] |> wdl,
          String.capitalize_ascii (mk_aggregate_id agg_id) |> dl, [
            Ctype (dl "t", loc_type (Tyasset asset));
            Ctype (dl "tk", tkey);
            Ctype (dl "collection", loc_type (Tycoll capasset));
            Cval  (dl "elts", dl (capasset ^ "." ^ "elts"));
            Cval  (dl "get", dl (capasset ^ "." ^ "get"));
            Cval  (dl "set", dl (capasset ^ "." ^ "set"));
            Ctype (dl "field", loc_type (Tyaggregate (mk_field_id oasset)));
            Cval  (dl "setF", dl (mk_set_field_id agg_id));
            Cval  (dl "aggregate", dl agg_id);
            Ctype (dl "tkF", agg_key_type);
            Cval  (dl "containsF", dl ((mk_field_id oasset) ^ "." ^ "contains"));
            Cval  (dl "mkF", dl ((mk_field_id oasset) ^ "." ^ "mk"));
            Cval  (dl "eltsF", dl ((mk_field_id oasset) ^ "." ^ "elts"));
            Cval  (dl "addF", dl ((mk_field_id oasset) ^ "." ^ "add"));
            Cval  (dl "removeF", dl ((mk_field_id oasset) ^ "." ^ "remove"));
            Cval  (dl "emptyF", dl ((mk_field_id oasset) ^ "." ^ "empty"));
            Ctype (dl "tO", loc_type (Tyasset oasset));
            Ctype (dl "collectionO", loc_type (Tycoll oasset));
            Cval  (dl "getO", dl ((String.capitalize_ascii oasset) ^ "." ^ "get"));
          ]) in
      acc @ [mk_set_field m asset agg_id oasset; clone]
    ) [] aggregates

(* -------------------------------------------------------------------------- *)

let mk_partition_axioms (m : M.model) =
  M.Utils.get_containers m |> List.map (fun (n,i,_) ->
      let kt     = M.Utils.get_asset_key m n |> snd in
      let pa,_,pkt  = M.Utils.get_container_asset_key m n i in
      mk_partition_axiom n i kt pa (pkt |> map_mtype m |> unloc_type)
    ) |> loc_decl |> deloc

(* -------------------------------------------------------------------------- *)

let rec get_record id = function
  | Drecord (n,_) as r :: _tl when compare id n = 0 -> r
  | _ :: tl -> get_record id tl
  | [] -> assert false

let get_record_name = function
  | Drecord (n,_) -> n
  | _ -> assert false

(* variables loop invariants ------------------------------------------------ *)

let mk_lbl_before lbl =
  match lbl with
  | Some a -> "Before_" ^ a
  | None ->  "Before_loop"

let mk_inv_lbl lbl id =
  match lbl with
  | Some a-> id ^ "_invariant_" ^ a
  | None -> id ^ "_invariant"

let mk_storage_loop_inv lbl lblbef id =
  let iid = mk_inv_lbl lbl id in {
    id =  dl iid;
    form = loc_term (Teq (Tyint, Tapp (Tvar id,[Tvar gs]), Tapp (Tvar id, [Tat (lblbef,Tvar gs)])))
  }

let rec is_identical id = function
  | (M.Eadded i)::_ when String.compare i id = 0 -> false
  | (M.Eremoved i)::_ when String.compare i id = 0 -> false
  | (M.Eupdated i)::_ when String.compare i id = 0 -> false
  | _::tl -> is_identical id tl
  | [] -> true

let mk_vars_loop_invariants m entry lbl lblbef body =
  let assigned_vars = M.Utils.extract_assign_kind body |>
                      List.fold_left (fun acc ak ->
                          match ak with
                          | M.Avar id -> acc @ [unloc id]
                          | M.Avarstore id -> acc @ [unloc id]
                          | _ -> acc
                        ) []
  in
  let assigned_assets = M.Utils.extract_asset_effect m body in
  (* invariant_vars are the storage / local variables spec are about *)
  let get_specifications acc name =  begin
    match M.Utils.get_specification m name with
    | Some s -> acc @ List.map (fun (p : M.postcondition) -> p.formula) s.postconditions
    | None -> acc
  end in
  let invariant_vars =
    Option.fold get_specifications [] entry |>
    List.fold_left (fun acc t ->
        let l = M.Utils.extract_var_idents m t in
        acc @ l) [] |> Tools.List.dedup in
  (* scan storage fields : generate when in invariant_vars and not in assigned *)
  let storage_invs = List.fold_left (fun acc (item : M.storage_item) ->
      match item.model_type with
      | M.MTasset  id when (List.mem id invariant_vars) ->
        let acc = if is_identical id assigned_assets then
            acc @ [mk_storage_loop_inv lbl lblbef (mk_ac_id id)]
          else acc in
        let acc  = if not (List.mem (M.Eadded id) assigned_assets) then
            acc @ [mk_storage_loop_inv lbl lblbef (mk_ac_added_id id)]
          else acc in
        let acc = if not (List.mem (M.Eremoved id) assigned_assets) then
            acc @ [mk_storage_loop_inv lbl lblbef (mk_ac_rmed_id id)]
          else acc in
        acc
      | _ when (List.mem (unloc item.id) invariant_vars) && not (List.mem (unloc item.id) assigned_vars) ->
        acc @ [mk_storage_loop_inv lbl lblbef (unloc (item.id))]
      | _ -> acc
    ) [] (M.Utils.get_storage m) in
  let const_storage_invs = List.fold_left (fun acc id ->
      if List.mem id invariant_vars && not (List.mem id assigned_vars) then
        acc @ [mk_storage_loop_inv lbl lblbef ("_"^id)]
      else acc
    ) [] ["now"; "caller"; "balance"; "source"; "selfaddress"] in
  (* TODO : local variables (pass context) *)
  storage_invs @ const_storage_invs

(* -------------------------------------------------------------------------- *)

(* type logical_mod = Nomod | Added | Removed *)
type mode = Inv | Logic | Exec | Def

type logical_context = {
  lctx     : mode;
  entry_id : ident option;
  locals   : ident list;
  loop_id  : ident option;
}

let init_ctx = {
  lctx     = Exec;
  entry_id = None;
  locals   = [];
  loop_id  = None;
}

let add_local id ctx = { ctx with locals = id::ctx.locals }

let mk_trace_seq m t chs =
  if M.Utils.with_trace m then
    Tseq ([dl t] @ (List.map mk_trace chs))
  else t

let map_mpattern (p : M.lident M.pattern_node) =
  match p with
  | M.Pwild -> Twild
  | M.Pconst i -> Tconst (map_lident i)

let is_coll_field m f : bool =
  M.Utils.get_containers m |> List.map (fun (_,v,_) -> v) |> List.mem f

let is_exec_divergent = function
  | M.Mget _
  | M.Mnth _
    -> true
  | _ -> false

let get_tuple_size = function
  | M.Ttuple l -> List.length l
  | _ -> assert false

let cp_storage id = Tapp (Tvar "_cp_storage",[Tvar id])

let fail_if_neg_nat_value t left right op  =
  match t with
  | M.Tbuiltin  Bnat -> dl (
      Tif (dl (Tge(dl Tyint, left, right)), op,
           Some (loc_term (Tseq [Tassign (Tvar gs, cp_storage gsinit); Traise Enegassignnat])))
    )
  | _ -> op

let get_assign_value t left right = function
  | M.ValueAssign -> right
  | M.MinusAssign ->
    let op = dl (Tminus (dl Tyint, left, right)) in
    fail_if_neg_nat_value t left right op
  | M.PlusAssign -> dl (Tplus (dl Tyint, left, right))
  | M.MultAssign -> dl (Tmult (dl Tyint, left, right))
  | M.DivAssign -> dl (Tdiv (dl Tyint, left, right))
  | M.AndAssign -> dl (Tand (left, right))
  | M.OrAssign -> dl (Tor (left, right))

let is_partition m n f =
  match M.Utils.get_field_container m n f with
  | _,Partition -> true
  | _ -> false

let mk_get_force n k c = Tmatch (dl (Tget(n,k,c)),[
    Tpsome (dl "v"), loc_term (Tvar "v");
    Twild, loc_term (Tseq [Tassign(Tvar gs, cp_storage gsinit); Traise Enotfound])
  ])

let mk_match_get_some a k instr excn =
  Tmatch (dl (Tget (dl a, k, loc_term (mk_ac a))), [
      Tpignore, instr;
      Twild, loc_term (Tseq [Tassign (Tvar gs, cp_storage gsinit); Traise excn])
    ])

let mk_match_get_some_id id a k instr excn =
  Tmatch (dl (Tget (dl a, k, loc_term (mk_ac a))), [
      Tpsome id, instr;
      Twild, loc_term (Tseq [Tassign (Tvar gs, cp_storage gsinit); Traise excn])
    ])

let mk_match_get_some_id_nil id a k instr =
  Tmatch (dl (Tget (dl a, k, loc_term (mk_ac a))), [
      Tpsome id, instr;
      Twild, dl Tunit
    ])

let mk_match_get_none a k instr excn =
  Tmatch (dl (Tget (dl a, k, loc_term (mk_ac a))), [
      Tpignore, loc_term (Tseq [Tassign (Tvar gs, cp_storage gsinit); Traise excn]);
      Twild, instr
    ])

let mk_match matched id instr excn =
  Tmatch (matched, [
      Tpsome (dl id), instr;
      Twild, loc_term (Tseq [Tassign (Tvar gs, cp_storage gsinit); Traise excn])
    ])

let mk_storage_id ctx =
  match ctx.lctx with
  | Def -> gsarg
  | _ -> gs

let mk_coll_term n ctx (t,d) =
  let s = mk_storage_id ctx in
  match ctx.lctx, t, d with
  | Inv, _, M.Dnone         -> Tvar (mk_ac_id n)
  | Inv, _, M.Dadded        -> Tvar (mk_ac_added_id n)
  | Inv, _, M.Dremoved      -> Tvar (mk_ac_rmed_id n)
  | Inv, _, M.Dunmoved      -> Tvar (mk_ac_unmvd_id n)
  | _, M.Tnone, M.Dnone        -> mk_ac_st s n
  | _, M.Tnone, M.Dadded       -> mk_ac_added_st s n
  | _, M.Tnone, M.Dremoved     -> mk_ac_rmed_st s n
  | _, M.Tnone, M.Dunmoved     -> mk_ac_st s n (* TODO: temp * delta *)
  | _, M.Tbefore, M.Dnone      -> mk_ac_old_st s n
  | _, M.Tbefore, M.Dadded     -> mk_ac_old_added_st s n
  | _, M.Tbefore, M.Dremoved   -> mk_ac_old_rmed_st s n
  | _, M.Tbefore, M.Dunmoved   -> mk_ac_st s n (* TODO: temp * delta *)
  | _, M.Tat lbl, M.Dnone      -> Tat (lbl, mk_ac_st s n)
  | _, M.Tat lbl, M.Dadded     -> Tat (lbl, mk_ac_added_st s n)
  | _, M.Tat lbl, M.Dremoved   -> Tat (lbl, mk_ac_rmed_st s n)
  | _, M.Tat _lbl, M.Dunmoved  -> mk_ac_st s n (* TODO: temp * delta *)

let mk_loc_coll_term n ctx (t,d) = loc_term (mk_coll_term n ctx (t,d))
let mk_lc_term n ctx = mk_loc_coll_term n ctx (M.Tnone, M.Dnone)

let mk_temp_delta = function
  | M.CKcoll (t,d) -> (t,d)
  | _ -> M.Tnone,M.Dnone

let assign_operation a e l =
Tassign (
  loc_term (Tdoti(gs,"_ops")),
  dl (Tcons (dl gListAs,
    dl (Tapp( loc_term (Tvar "_mk_operation"),[a; e; l])),
    loc_term (Tdoti(gs,"_ops"))
)))

let rec map_mterm m ctx (mt : M.mterm) : loc_term =
  let error_internal desc = emit_error (mt.loc, desc); Tnottranslated in
  let error_not_translated (msg : string) = (* Tnottranslated in *) error_internal (TODONotTranslated msg) in
  let error_not_supported (msg : string) = error_internal (NotSupported msg) in
  let t =
    match mt.node with
    (* lambda *)

    | Mletin ([id], v, _, b, None) ->
      let ctx = add_local (unloc id) ctx in
      Tletin (M.Utils.is_local_assigned (unloc id) b, map_lident id, None, map_mterm m ctx v, map_mterm m ctx b)

    | Mletin ([id], { node = M.Mget (a, CKcoll (t,d), k); type_ = _ }, _, b, Some e) -> (* logical *)
      let ctx = ctx in
      Tmatch (Tget (loc_ident a,
                    map_mterm m ctx k,
                    mk_loc_coll_term a ctx (t,d)) |> dl,[
                Tpsome (map_lident id), map_mterm m ctx b;
                Twild, map_mterm m ctx e
              ])
    | Mletin ([id], { node = M.Mnth (n, CKview c,k); type_ = _ }, _, b, Some e) ->
      Tmatch (Tnth (dl (mk_view_id n),
                    map_mterm m ctx k,
                    map_mterm m ctx c) |> dl,[
                Tpsome (map_lident id),  map_mterm m ctx b;
                Twild, map_mterm m ctx e
              ])
    | Mletin ([id], { node = M.Mnth (n, CKcoll (t,d),k); type_ = _ }, _, b, Some e) ->
      Tmatch (
        Tnth (
          dl (mk_view_id n),
          map_mterm m ctx k,
          dl(Ttoview (dl n,mk_loc_coll_term n ctx (t,d))) ) |> dl,[
          Tpsome (map_lident id), map_mterm m ctx b;
          Twild, map_mterm m ctx e
        ])
    | Mletin ([id], { node = M.Moptget v; type_ = _ }, _, b, Some e) ->
      Tmatch (map_mterm m ctx v,[
          Tpsome (map_lident id), map_mterm m ctx b;
          Twild, map_mterm m ctx e
        ])

    | Mletin ([id], v, _, b, Some o) ->
      let ctx = ctx in
      Tmatch (map_mterm m ctx v,[
          Tpsome (map_lident id), map_mterm m ctx b;
          Twild, map_mterm m ctx o
        ])
    | Mletin (l, v, _, b, None) ->
      let ctx = List.fold_left (fun acc id -> add_local (unloc id) acc) ctx l in
      let id = "("^(l |> List.map unloc |> String.concat ",")^")" in
      Tletin (false, dl id , None, map_mterm m ctx v, map_mterm m ctx b)
    | Mletin              _ -> Tvar (dl "TODO letin")
    | Mdeclvar            _ -> error_not_supported "Mdeclvar"

    | Mapp (f, args) ->
      let args = args |> List.map (map_mterm m ctx) in
      if is_predicate m (unloc f) then
        let storage = loc_term (Tvar (mk_storage_id ctx)) in
        let params = get_pred_params m (unloc f) |> List.map loc_term in
        Tapp (mk_loc (map_lident f).loc (Tvar (map_lident f)), [storage] @ params @ args)
      else
        Tapp (mk_loc (map_lident f).loc (Tvar (map_lident f)), [loc_term (Tvar gsinit)] @ args)

    (* assign *)

    | Massign (ValueAssign, _, Avar id, v) ->
      Tassign (dl (Tvar (map_lident id)),map_mterm m ctx v)

    | Massign (MinusAssign, _, Avar id, v) ->
      Tassign (dl (Tvar (map_lident id)),
               dl (
                 Tminus (dl Tyint,
                         dl (Tvar (map_lident id)),
                         map_mterm m ctx v)))
    | Massign (ValueAssign, _, Aoperations, v) -> Tassign (loc_term (Tdoti(gs,mk_id gOperations)), map_mterm m ctx v)

    | Massign (_, _, Avar _, _) -> error_not_translated "Massign (_, _, Avar _, _)"

    | Massign (assignop, t, Avarstore id, v) ->
      let left = dl (Tdoti (dl gs,map_lident id)) in
      let right = map_mterm m ctx v in
      Tassign (left,get_assign_value t left right assignop)

    | Massign (assignop, t, Aasset (_id1, id2, k), v) ->
      let left = dl (Tdot (map_mterm m ctx (* id1 *) k, (* FIXME *)
                           dl (Tvar (map_lident id2)))) in
      let right = map_mterm m ctx v in
      Tassign (left,get_assign_value t left right assignop)

    | Massign (assignop, t, Arecord (_id1, id2, k), v) ->
      let left = dl (Tdot (map_mterm m ctx (* id1 *) k, (* FIXME *)
                           dl (Tvar (map_lident id2)))) in
      let right = map_mterm m ctx v in
      Tassign (left,get_assign_value t left right assignop)

    | Massign (_, _, Astate, v) -> Tassign (loc_term (Tdoti (gs, "state")), map_mterm m ctx v)

    | Massign (_, _, Aassetstate _, _) -> error_not_translated "Massign (_, _, Aassetstate _, _)"

    | Massign (_, _, Aoperations, _) -> error_not_translated "Massign (_, _, Aoperations, _)"


    (* control *)

    | Mif (c, t, Some { node=M.Mseq []; type_=_}) ->
      Tif (map_mterm m ctx c, map_mterm m ctx t, None)

    | Mif (c, t, e) ->
      Tif (map_mterm m ctx c, map_mterm m ctx t, Option.map (map_mterm m ctx) e)

    | Mmatchwith (t, l) ->
      Tmatch (map_mterm m ctx t, List.map (fun ((p : M.lident M.pattern_gen), e) ->
          (map_mpattern p.node, map_mterm m ctx e)
        ) l)

    | Mfor (_id, _c, _b, _lbl) -> error_not_supported "Mfor"
    | Miter (id, from, to_, body, lbl) -> (* ('id * 'term * 'term * 'term * ident option) *)
      let inv_ctx = { ctx with lctx = Logic } in
      Tmark (dl (mk_lbl_before lbl),
             dl (Tfor (map_lident id,
                       map_mterm m ctx from,
                       map_mterm m ctx to_,
                       mk_invariants m inv_ctx (Some id) lbl body,
                       map_mterm m ctx body
                      )))
    | Mwhile (test, body, lbl) ->
      let inv_ctx = { ctx with lctx = Logic } in
      Tmark (dl (mk_lbl_before lbl),
             dl (Twhile (map_mterm m ctx test,
                         mk_invariants m inv_ctx None lbl body,
                         map_mterm m ctx body
                        )))
    | Mseq [] -> Tunit
    | Mseq l -> Tseq (List.map (map_mterm m ctx) l)

    | Mreturn             v -> map_mterm m ctx v |> Mlwtree.deloc

    | Mlabel lbl ->
      begin
        match M.Utils.get_formula m None (unloc lbl) with
        | Some formula -> Tassert (Some (map_lident lbl),map_mterm m ctx formula)
        | _ -> assert false
      end
    | Mmark (lbl, x) -> Tmark (map_lident lbl, map_mterm m ctx x)

    (* effect *)

    | Mfail InvalidCaller        -> Traise Einvalidcaller
    | Mfail NoTransfer           -> Traise Enotransfer
    | Mfail (InvalidCondition _) -> Traise Einvalidcondition
    | Mfail InvalidState         -> Traise Einvalidstate
    | Mfail AssignNat -> Tseq [loc_term (Tassign (Tvar gs, cp_storage gsinit)); dl (Traise Enegassignnat)]
    | Mfail (Invalid v) ->
      let idx = get_fail_idx m v.type_ in
      Tseq [loc_term (Tassign (Tvar gs, cp_storage gsinit)); dl (Traise (Efail (idx,Some (map_mterm m ctx v))))]

    | Mtransfer (v, k) ->
      begin
        match k with
        | TKsimple d             ->
          let a = map_mterm m ctx v in
          let t = map_mterm m ctx d in
          Tseq[
          dl (Tassign (
            loc_term (Tdoti(gs,"_ops")),
            dl (Tcons (dl gListAs,
                    dl (Tapp(loc_term (Tvar "_mk_transfer"),[t;a])),
                    loc_term (Tdoti(gs,"_ops"))
                  ))));
          dl (Tassign (
            loc_term (Tdoti (gs,"_balance")),
            dl (Tminus (dl Tyint,
                    loc_term (Tdoti (gs,"_balance")),
                    a
                   ))
          ))
        ]
        | TKcall (id, _, d, _a)   ->
          let t = map_mterm m ctx v in
          let l = loc_term (Tnil gListAs) (*map_mterm m ctx a*) in
          let a = map_mterm m ctx d in
          let n = loc_term (Tstring id) in
          Tassign (
          loc_term (Tdoti(gs,"_ops")),
          dl (Tcons (dl gListAs,
                 dl (Tapp(loc_term (Tvar "_mk_call"),[a; t; n; l])),
                 loc_term (Tdoti(gs,"_ops"))
                )))
        | TKentry (e, _a)         ->
          assign_operation (map_mterm m ctx v) (map_mterm m ctx e) (loc_term (Tnil gListAs))(*(map_mterm m ctx a)*)
        | TKself (id, _a)        ->
          assign_operation
            (map_mterm m ctx v)
            (dl (Tapp (loc_term (Tvar "getopt"), [loc_term (Tentrypoint (id, Tselfaddress gs))])))
            (loc_term (Tnil gListAs))
      end

    (* entrypoint *)

    | Mentrypoint (_t, a, s) -> Tentrypoint (map_lident a, map_mterm m ctx s)
    | Mself id               -> Tapp (loc_term (Tvar "getopt"), [loc_term (Tentrypoint (unloc id, Tdefaultaddr))])


    (* operation *)

    | Moperations                 ->
      begin match ctx.lctx with
        | Inv -> Tvar (dl (mk_id gOperations))
        | _ -> Tdoti (dl gs, dl (mk_id gOperations))
      end
    | Mmkoperation (v, d, _a)   ->
      let a = map_mterm m ctx v in
      let e = map_mterm m ctx d in
      let l = loc_term (Tnil gListAs) in
      Tapp( loc_term (Tvar "_mk_operation"),[a; e; l])

    (* literals *)

    | Mint v -> Tint v
    | Mnat v -> Tint v
    | Mbool false -> Tfalse
    | Mbool true -> Ttrue
    | Menum               _ -> error_not_supported "Menum"
    | Mrational (l,r) -> Ttuple([ loc_term (Tint l); loc_term (Tint r)])
    | Mcurrency (i, Tz)   -> Tint (Big_int.mult_int_big_int 1000000 i)
    | Mcurrency (i, Mtz)  -> Tint (Big_int.mult_int_big_int 1000 i)
    | Mcurrency (i, Utz)  -> Tint i
    | Mstring v ->  (* Tint (Tools.sha v) *) Tstring v
    | Maddress v -> (* Tint (Tools.sha v) *) Tstring v
    | Mbytes v ->   (* Tint (Tools.sha v) *) Tstring v
    | Mdate s -> Tint (Core.date_to_timestamp s)
    | Mduration v -> Tint (Core.duration_to_timestamp v)
    | Mtimestamp v -> Tint v
    | Munit -> Tunit

    (* control expression *)

    | Mexprif (c, t, e) ->
      Tif (map_mterm m ctx c, map_mterm m ctx t, Some (map_mterm m ctx e))

    | Mexprmatchwith (t, l) ->
      Tmatch (map_mterm m ctx t, List.map (fun ((p : M.lident M.pattern_gen), e) ->
          (map_mpattern p.node, map_mterm m ctx e)
        ) l)

    | Mmatchsome _ -> error_not_supported "Mmatchsome"

    (* composite type constructors *)

    | Mnone -> Tnone
    | Msome v -> Tsome (map_mterm m ctx v)

    | Mtuple l              -> Ttuple (List.map (map_mterm m ctx) l)
    | Mtupleaccess (x, k) ->
      let card = begin match x.type_ with
        | Ttuple l -> List.length l
        | _ -> assert false
      end in Ttupleaccess (map_mterm m ctx x, (Big_int.int_of_big_int k)+1, card)
    | Mrecupdate (id, l) ->
      Trecord (Some (map_mterm m ctx id), List.map (fun (i,t) -> (dl i, map_mterm m ctx t)) l)
    | Masset l ->
      let asset = M.Utils.get_asset_type mt in
      let fns = M.Utils.get_field_list m asset |> wdl in
      Trecord (None,(List.combine fns (List.map (map_mterm m ctx) l)))

    | Massets l ->
      begin
        match mt.type_ with
        | Tcontainer (Tasset a,Collection) ->
          Tmkcoll (map_lident a, List.map (map_mterm m ctx) l)
        | Tcontainer (Tasset a,_) -> Temptyfield (dl (mk_field_id (unloc a)))
        | _ -> assert false
      end

    | Mlitset  l ->
      let set = mk_set_name m mt.type_ in
      if List.length l > 0 then
        Tmkcoll (dl set,
                 List.fold_left (fun acc v ->
                     acc @ [ map_mterm m ctx v]
                   ) ([] : loc_term list) l)
      else Temptycoll (dl set)
    | Mlitlist l ->
      List.fold_left(fun acc e ->
          dl (Tcons(dl gListAs, map_mterm m ctx e, acc))
        ) (loc_term (Tnil gListAs)) l |> Mlwtree.deloc
    | Mlitmap  l   ->
      let map = mk_map_name m mt.type_ in
      if List.length l > 0 then
        Tmkcoll (dl map,
                 List.fold_left (fun acc (k,v) ->
                     acc @ [dl (Ttuple [map_mterm m ctx k; map_mterm m ctx v])]
                   ) ([] : loc_term list) l)
      else Temptycoll (dl map)
    | Mlitrecord l -> Trecord (None, List.map (fun (n,v) -> (dl n, map_mterm m ctx v)) l)

    (* access *)

    | Mdot (e, i) -> Tdot (map_mterm m ctx e, mk_loc (loc i) (Tvar (map_lident i))) (* FIXME *)
    | Mdotassetfield (an, k, fn) ->
      Tdot(
        dl (Tapp (loc_term (Tvar ("get_"^(unloc an))),[map_mterm m ctx k])),
        loc_term (Tvar (unloc fn)))

    (* comparison operators *)

    | Mequal (t, l, r)  ->
      begin match t, ctx.lctx with
      | M.Tcontainer (Tasset id, Collection), (Logic | Inv) -> Teq (dl (Tycoll (map_lident id)), map_mterm m ctx l, map_mterm m ctx r)
      | _, (Logic | Inv) -> Teq (dl Tyint, map_mterm m ctx l, map_mterm m ctx r)
      | _           -> Teq (map_mtype m t, map_mterm m ctx l, map_mterm m ctx r)
      end
    | Mnequal (t, l, r) ->
      begin match t, ctx.lctx with
      | M.Tcontainer (Tasset id, Collection), (Logic | Inv) -> Tneq (dl (Tycoll (map_lident id)), map_mterm m ctx l, map_mterm m ctx r)
      | _, (Logic | Inv) -> Tneq (dl Tyint, map_mterm m ctx l, map_mterm m ctx r)
      | _           -> Tneq (map_mtype m t, map_mterm m ctx l, map_mterm m ctx r)
      end
    | Mgt (l, r) -> Tgt (map_mtype m l.type_, map_mterm m ctx l, map_mterm m ctx r)
    | Mge (l, r) -> Tge (map_mtype m l.type_, map_mterm m ctx l, map_mterm m ctx r)
    | Mlt (l, r) -> Tlt (map_mtype m l.type_, map_mterm m ctx l, map_mterm m ctx r)
    | Mle (l, r) -> Tle (map_mtype m l.type_, map_mterm m ctx l, map_mterm m ctx r)
    | Mmulticomp          _ -> error_not_translated "Mmulticomp"


    (* arithmetic operators *)

    | Mand (l, r) -> Tpand (map_mterm m ctx l, map_mterm m ctx r)
    | Mor (a, b) -> Tor (map_mterm m ctx a, map_mterm m ctx b)
    | Mxor (a, b) ->
      let t = map_mtype m (mt.type_) in
      Txor (t, map_mterm m ctx a, map_mterm m ctx b)
    | Mnot c -> Tnot (map_mterm m ctx c)
    | Mplus (l, r)  -> Tplus  (dl Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mminus (l, r) -> Tminus (dl Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mmult (l, r) -> Tmult (dl Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mdivrat _ -> error_not_translated "Mdivrat"
    | Mdiveuc (l, r) -> Tdiv (dl Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mmodulo (l, r) -> Tmod (dl Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Muminus v -> Tuminus (dl Tyint, map_mterm m ctx v)


    (* asset api effect *)

    | Maddasset (n, i) ->
      let key_value = mk_asset_key_value m ctx n i in
      let mk_add_assign coll =
        let add = dl (Tadd (dl n, map_mterm m ctx i, loc_term coll)) in
        dl (Tassign (loc_term coll, add)) in
      let assign = mk_add_assign (mk_ac n) in
      let assign_added = mk_add_assign (mk_ac_added n) in
      let assigns = dl (Tseq [assign; assign_added]) in
      mk_trace_seq m
        (mk_match_get_none n key_value assigns Ekeyexist)
        [CAdd n]

    | Maddfield (a, f, k, kb) ->
      let oasset, _, _ = M.Utils.get_container_asset_key m a f in
      let mk_add_id = loc_term (Tdoti (mk_aggregate_id f, "add")) in
      let v =
        if is_partition m a f then mk_asset_key_value m ctx a kb
        else map_mterm m ctx kb in
      let assign = dl (Tassign (loc_term (mk_ac a), dl (Tapp(mk_add_id,[
          map_mterm m ctx k;
          v;
          loc_term (mk_ac a)
        ])))) in
      let instr =
        if is_partition m a f then
          let mk_add_assign coll =
            let add = dl (Tadd (dl oasset, map_mterm m ctx kb, loc_term coll)) in
            dl (Tassign (loc_term coll, add)) in
          let assign = mk_add_assign (mk_ac oasset) in
          let assign_added = mk_add_assign (mk_ac_added oasset) in
          let assigns = dl (Tseq [assign; assign_added]) in
          dl (Tseq [assign; dl (mk_match_get_none oasset v assigns Ekeyexist)])
        else dl (mk_match_get_some oasset v assign Enotfound) in
      mk_trace_seq m
        (mk_match_get_some a (map_mterm m ctx k) instr Enotfound)
        ([CUpdate f] @ if is_partition m a f then [CAdd oasset] else [])

    | Mremoveasset (n, i) ->
      let partitions = M.Utils.get_asset_partitions m n in
      let remove = List.map (fun (f, oasset) ->
          let capoasset = String.capitalize_ascii oasset in
          let field = loc_term (Tdoti("_a", f)) in
          let remove = dl (Tapp (loc_term (Tdoti(capoasset,"removeif_in_field")), [field; loc_term (mk_ac oasset)])) in
          dl (Tassign (loc_term (mk_ac oasset), remove))
        ) partitions in
      let tr_rm_oassets = List.map (fun (f,_) ->
          let oasset, _, _ = M.Utils.get_container_asset_key m n f in
          CRm oasset) partitions in
      let remove =
        if List.length remove > 1 then
          dl (Tseq remove)
        else if compare (List.length remove) 1 = 0 then
          (List.hd remove)
        else dl Tnone in
      let remove_instr = dl (mk_match_get_some_id_nil (dl "_a") n (map_mterm m ctx i) remove) in
      let mk_assign coll = dl (Tassign (loc_term coll,dl (Tremove(dl n,map_mterm m ctx i,loc_term coll)))) in
      let mk_assign_add coll asset = dl (Tassign (loc_term coll,dl (Tadd(dl n,asset,loc_term coll)))) in
      let assign = mk_assign (mk_ac n) in
      let rm_instr = mk_assign_add (mk_ac_rmed n) (loc_term (Tvar "_a")) in
      let assign_rmed = dl (mk_match_get_some_id_nil (dl "_a") n (map_mterm m ctx i) rm_instr) in
      if List.length partitions > 0 then
        mk_trace_seq m
          (Tseq [remove_instr; assign; assign_rmed])
          ([CRm n] @ tr_rm_oassets)
      else
        mk_trace_seq m
          (Tseq [assign; assign_rmed])
          [CRm n]

    | Mremovefield (a, f, k, kb) ->
      let oasset, _, _ = M.Utils.get_container_asset_key m a f in
      let t, _, _ = M.Utils.get_container_asset_key m a f in
      let mk_rm_id = loc_term (Tdoti (mk_aggregate_id f, "remove")) in
      let assign = dl (Tassign (loc_term (mk_ac a), dl (Tapp(mk_rm_id,[
          map_mterm m ctx k;
          map_mterm m ctx kb;
          loc_term (mk_ac a)
        ])))) in
      let instr =
        if is_partition m a f then
          let mk_assign_rm coll =
            let rm = dl (Tremove (dl oasset, map_mterm m ctx kb, loc_term coll)) in
            dl (Tassign (loc_term coll, rm)) in
          let mk_assign_add coll asset =
            let rm = dl (Tadd (dl oasset, asset, loc_term coll)) in (* map_mterm m ctx kb *)
            dl (Tassign (loc_term coll, rm)) in
          let rm_assign = mk_assign_rm (mk_ac oasset) in
          let rm_instr = mk_assign_add (mk_ac_rmed oasset) (loc_term (Tvar "_a")) in
          let rm_assign_rmed = dl (mk_match_get_some_id_nil (dl "_a") oasset (map_mterm m ctx kb) rm_instr) in
          dl (Tseq [rm_assign; rm_assign_rmed; assign])
        else assign in
      mk_trace_seq m
        (mk_match_get_some a (map_mterm m ctx k) instr Enotfound)
        ([CUpdate f] @ if is_partition m a f then [CRm t] else [])

    | Mremoveall (a, f, v) ->
      let rm_field = dl (Tapp (loc_term (Tdoti (mk_aggregate_id f,"removeall")),[map_mterm m ctx v; loc_term (mk_ac a)])) in
      let assign_rm_field = dl (Tassign (loc_term (mk_ac a), rm_field)) in
      let oasset , _, _ = M.Utils.get_container_asset_key m a f in
      let instr =
        if is_partition m a f then
          let field = loc_term (Tdoti("_a", f)) in
          let capoasset = String.capitalize_ascii oasset in
          let rmif = dl (Tapp (loc_term (Tdoti(capoasset, "removeif_in_field")), [field; loc_term (mk_ac oasset)])) in
          let assign_rmif = dl (Tassign(loc_term (mk_ac oasset), rmif)) in
          mk_match_get_some_id (dl "_a") a (map_mterm m ctx v) (dl (Tseq [assign_rmif; assign_rm_field])) Enotfound
        else mk_match_get_some a (map_mterm m ctx v) assign_rm_field Enotfound in
      mk_trace_seq m instr ([CUpdate f] @ if is_partition m a f then [CRm oasset] else [])

    | Mremoveif (_a, (CKview _l), _la, _lb, _) -> assert false
    | Mremoveif (_a, CKdef _, _la, _lb, _) -> assert false

    | Mremoveif (a, CKfield (_, field, k, _, _), args, tbody, _a) ->
      let args = mk_filter_args m ctx args tbody in
      let oasset, _ = M.Utils.get_field_container m a field in
      let removeif_name = mk_removeif_name m oasset tbody in
      let removeif = dl (Tfremoveif (dl (
          mk_aggregate_id field),
                                     dl removeif_name, args,
                                     map_mterm m ctx k, mk_lc_term oasset ctx, mk_lc_term a ctx)) in
      let assign = dl (Tassign(mk_lc_term a ctx, removeif)) in
      if is_partition m a field then
        let removecoll = loc_term (Tpremoveif(oasset, removeif_name, args |> List.map unloc_term, Tdoti("_a",field), mk_ac oasset)) in
        let assign_rmcoll = dl (Tassign (loc_term (mk_ac oasset),removecoll)) in
        let instr = dl (Tseq[assign_rmcoll; assign]) in
        mk_trace_seq m (mk_match_get_some_id (dl "_a") a (map_mterm m ctx k) instr Enotfound) [CUpdate field; CRm oasset]
      else
        mk_trace_seq m (mk_match_get_some a (map_mterm m ctx k) assign Enotfound) [CUpdate field]

    | Mremoveif (a, CKcoll _, args, tbody, _a) ->

      let args = mk_filter_args m ctx args tbody in

      let partitions = M.Utils.get_asset_partitions m a in
      let remove = List.map (fun (f, oasset) ->
          let capoasset = String.capitalize_ascii oasset in
          let coll = Tselect(a, Tapp (Tvar (mk_removeif_name m a tbody), args |> List.map unloc_term), mk_ac a) in
          let field = loc_term (Tapp (Tdoti(mk_aggregate_id f,"union"),[coll])) in
          let remove = dl (Tapp (loc_term (Tdoti(capoasset,"removeif_in_field")), [field; loc_term (mk_ac oasset)])) in
          dl (Tassign (loc_term (mk_ac oasset), remove))
        ) partitions in
      let tr_rm_oassets = List.map (fun (f,_) ->
          let oasset, _, _ = M.Utils.get_container_asset_key m a f in
          CRm oasset) partitions in

      let removeif =
        dl (Tremoveif (dl a,
                       dl (mk_removeif_name m a tbody), args,
                       mk_lc_term a ctx)) in

      if List.length partitions > 0 then
        let assign = dl (Tassign (mk_lc_term a ctx, removeif)) in
        mk_trace_seq m (Tseq (remove @ [assign])) ([CRm a] @ tr_rm_oassets)
      else
        mk_trace_seq m (Tassign (mk_lc_term a ctx, removeif)) [CRm a]

    | Mclear (n, CKcoll _) ->
      let partitions = M.Utils.get_asset_partitions m n in
      let remove = List.map (fun (f, oasset) ->
          let capoasset = String.capitalize_ascii oasset in
          let field = loc_term (Tapp (Tdoti(mk_aggregate_id f,"union"),[mk_ac n])) in
          let remove = dl (Tapp (loc_term (Tdoti(capoasset,"removeif_in_field")), [field; loc_term (mk_ac oasset)])) in
          dl (Tassign (loc_term (mk_ac oasset), remove))
        ) partitions in
      let tr_rm_oassets = List.map (fun (f,_) ->
          let oasset, _, _ = M.Utils.get_container_asset_key m n f in
          CRm oasset) partitions in
      if List.length partitions > 0 then
        let assign = dl (Tassign(loc_term (Tdoti(gs,mk_ac_id n)), loc_term (Temptycoll n))) in
        mk_trace_seq m (Tseq (remove @ [assign])) ([CRm n] @ tr_rm_oassets)
      else
        mk_trace_seq m (Tassign(loc_term (Tdoti(gs,mk_ac_id n)), loc_term (Temptycoll n))) [CRm n]
    | Mclear (n, CKview v) ->
      let partitions = M.Utils.get_asset_partitions m n in
      let remove = List.map (fun (f, oasset) ->
          let capn = String.capitalize_ascii n in
          let capoasset = String.capitalize_ascii oasset in
          let viewvar = loc_term (Tvar "_view") in
          let field = dl (Tunionpred (dl (mk_aggregate_id f), dl (capn^".is_in_view"), [viewvar], loc_term (mk_ac n))) in
          let remove = dl (Tapp (loc_term (Tdoti(capoasset,"removeif_in_field")), [field; loc_term (mk_ac oasset)])) in
          dl (Tletin(false, dl "_view",None,map_mterm m ctx v, dl (Tassign (loc_term (mk_ac oasset), remove))))
        ) partitions in
      let tr_rm_oassets = List.map (fun (f,_) ->
          let oasset, _, _ = M.Utils.get_container_asset_key m n f in
          CRm oasset) partitions in
      let field = map_mterm m ctx v in
      let capasset = String.capitalize_ascii n in
      let clear = dl (Tapp (loc_term (Tdoti(capasset,"removeif_in_view")),[field; loc_term (mk_ac n)])) in
      let assign = Tassign (loc_term (mk_ac n), clear) in
      if List.length partitions > 0 then
        let instr = Tseq (remove @ [dl (assign)]) in
        mk_trace_seq m instr ([CRm n] @ tr_rm_oassets)
      else
        mk_trace_seq m assign [CRm n]
    | Mclear (_, CKdef _) -> assert false
    | Mclear (_n, CKfield (n, f, v, _, _)) ->
      let oasset,_ = M.Utils.get_field_container m n f in
      let asset = dl (mk_match_get_some_id (dl "_a") n (map_mterm m ctx v) (loc_term (Tvar "_a")) Enotfound) in
      let field = dl (Tdot(asset, loc_term (Tvar f))) in
      let capoasset = String.capitalize_ascii oasset in
      let clear = dl (Tapp (loc_term (Tdoti(capoasset,"removeif_in_field")),[field; loc_term (mk_ac oasset)])) in
      let assign = dl (Tassign (loc_term (mk_ac oasset), clear)) in
      let rm_field = dl (Tapp (loc_term (Tdoti (mk_aggregate_id f,"removeall")),[map_mterm m ctx v; loc_term (mk_ac n)])) in
      let assign_rm_field = dl (Tassign (loc_term (mk_ac n), rm_field)) in
      mk_trace_seq m (Tseq [assign; assign_rm_field]) [CRm oasset]
    | Mset (n, l, k, v) ->
      mk_trace_seq m
        (Tassign (loc_term (Tdoti(gs,mk_ac_id n)),dl (Tset(dl n,map_mterm m ctx k, map_mterm m ctx v,loc_term (mk_ac n)))))
        (List.map (fun f -> CUpdate f) l)

    | Mupdate             _ -> error_not_translated "Mupdate"
    | Maddupdate          _ -> error_not_translated "Maddupdate"
    | Maddforce           _ -> error_not_translated "Maddforce"

    | Mget (an, _c, k) ->
      begin match ctx.lctx with
        | Inv | Logic | Def -> Tget(dl an, map_mterm m ctx k,mk_lc_term an ctx)
        | _ -> mk_get_force (dl an) (map_mterm m ctx k) (mk_lc_term an ctx)
      end
    (* view api ------------------------------------------------------------- *)

    | Mselect (n, c, args, tbody, _a) ->
      let args = mk_filter_args m ctx args tbody in
      Tvselect (dl n, dl (mk_select_name m n tbody), args, mk_container_term m n ctx c, mk_lc_term n ctx)

    | Msort (n, c,l) -> Tvsort (dl (mk_sort_clone_id n l),mk_container_term m n ctx c,mk_lc_term n ctx)

    | Mcontains (n, c, r) -> Tvcontains (dl (mk_view_id n), map_mterm m ctx r, mk_container_term m n ctx c)

    | Mnth (n, c, k) ->
      let nth = Tnth (dl (mk_view_id n), map_mterm m ctx k, mk_container_term m n ctx c) in
      begin match ctx.lctx with
        | Logic | Inv | Def -> nth
        | _ ->  mk_match (dl nth) "_a" (loc_term (Tvar "_a")) Enotfound
      end

    | Mcount (n, c) -> Tcard (dl (mk_view_id n), mk_container_term m n ctx c)

    | Msum          (n, c, f) ->
      let cloneid = mk_sum_clone_id m n f in
      let col = mk_loc_coll_term n ctx (mk_temp_delta c) in
      Tvsum(dl cloneid , mk_container_term m n ctx c, col)

    | Mhead (n, c, v) ->
      Tvhead(dl (mk_view_id n), map_mterm m ctx v, mk_container_term m n ctx c)

    | Mtail (n, c, v) ->
      Tvtail(dl (mk_view_id n), map_mterm m ctx v, mk_container_term m n ctx c)

    (* utils ---------------------------------------------------------------- *)

    | Mcast (Tcontainer (Tasset a,Collection),Tcontainer (Tasset _, View), v) ->
      begin match v.node, ctx.lctx with
        | Mapp(f,_), _  when is_coll_field m (unloc f) ->
          map_mterm m ctx v |> Mlwtree.deloc
        | Mvar (f, Vlocal, _, _), _ when is_coll_field m (unloc f) ->
          map_mterm m ctx v |> Mlwtree.deloc
        (* | Mdotasset (_,f) when is_coll_field m (unloc f) -> *)
        | Mdotassetfield (_, _, f), _ when is_coll_field m (unloc f) ->
          map_mterm m ctx v |> Mlwtree.deloc
        | _ -> Ttoview (map_lident a,map_mterm m ctx v)
      end
    | Mcast (Tcontainer (Tasset a,View),Tlist _, v) -> Telts(dl (mk_view_id (unloc a)), map_mterm m ctx v)
    | Mcast (Tbuiltin Baddress, Tcontract _, v) -> Tapp (loc_term (Tvar "getopt"), [(dl (Tentrypoint (dl "", map_mterm m ctx v)))])
    | Mcast (Tmap _ as t, Tlist _, c) -> Telts (dl (mk_map_name m t), map_mterm m ctx c)
    | Mcast (Tset _ as t, Tlist _, c) -> Telts (dl (mk_set_name m t), map_mterm m ctx c)
    | Mcast (_, _, v)       -> map_mterm m ctx v |> Mlwtree.deloc


    (* set api expression *)
    | Msetadd (t, s, e)      -> Tadd (dl (mk_set_name m (Tset t)), map_mterm m ctx e, map_mterm m ctx s)
    | Msetremove (t, s, e)   -> Tremove (dl (mk_set_name m (Tset t)), map_mterm m ctx e, map_mterm m ctx s)
    | Msetcontains (t, s, e) -> Tcontains (dl (mk_set_name m (Tset t)), map_mterm m ctx e, map_mterm m ctx s)
    | Msetlength (t, s)      -> Tcard (dl (mk_set_name m (Tset t)), map_mterm m ctx s)
    | Msetfold _ -> error_not_translated "Mmapfold"

    (* list api expression *)

    | Mlistprepend (t, l, e)  -> Tprepend (dl (mk_list_name m (Tlist t)), map_mterm m ctx e, map_mterm m ctx l)
    | Mlistheadtail (_t, _l)  -> assert false
    | Mlistlength (t, l)      -> Tcard (dl (mk_list_name m (Tlist t)), map_mterm m ctx l)
    | Mlistcontains (t, l, e) -> Tcontains (dl (mk_list_name m (Tlist t)), map_mterm m ctx e, map_mterm m ctx l)
    | Mlistnth (t, n, l)      ->
      let nth = Tnth (dl (mk_list_name m (Tlist t)), map_mterm m ctx n, map_mterm m ctx l) in
      begin match ctx.lctx with
        | Logic | Inv | Def -> nth
        | _ -> mk_match (dl nth) "_a" (loc_term (Tvar "_a")) Enotfound
      end
    | Mlistreverse _ -> error_not_translated "Mlistreverse"
    | Mlistfold    _ -> error_not_translated "Mlistfold"

    (* map api expression *)

    | Mmapput (kt, vt, c, k, v)   ->
      Tadd (dl (mk_map_name m (M.Tmap (false, kt, vt))), dl (Ttuple [ map_mterm m ctx k; map_mterm m ctx v]), map_mterm m ctx c)
    | Mmapremove (kt, vt, c, k)   ->
      Tremove (dl (mk_map_name m (M.Tmap (false, kt, vt))),map_mterm m ctx k, map_mterm m ctx c)
    | Mmapget (kt, vt, c, k)      -> Tsnd(
        dl (mk_get_force (dl (mk_map_name m (M.Tmap (false, kt, vt)))) (map_mterm m ctx k) (map_mterm m ctx c)))
    | Mmapgetopt (kt, vt, c, k)   -> Tsndopt(
        dl (Tget (dl (mk_map_name m (M.Tmap (false, kt, vt))),map_mterm m ctx k, map_mterm m ctx c)))
    | Mmapcontains (kt, kv, c, k) ->
      Tcontains (dl (mk_map_name m (M.Tmap (false, kt, kv))),map_mterm m ctx k, map_mterm m ctx c)
    | Mmaplength (k, v, c)      ->
      let tmap = mk_map_name m (M.Tmap (false, k,v)) in Tcard (dl tmap,map_mterm m ctx c)
    | Mmapfold _ -> error_not_translated "Mmapfold"
    (* builtin functions *)
    | Mmax (l,r) ->
      begin match mt.type_ with
        | Ttuple _ -> Tapp (loc_term (Tvar "rat_max"),[map_mterm m ctx l; map_mterm m ctx r])
        | _ -> Tapp (loc_term (Tvar "max"),[map_mterm m ctx l; map_mterm m ctx r])
      end
    | Mmin (l,r) ->
      begin match mt.type_ with
        | Ttuple _ -> Tapp (loc_term (Tvar "rat_min"),[map_mterm m ctx l; map_mterm m ctx r])
        | _ -> Tapp (loc_term (Tvar "min"),[map_mterm m ctx l; map_mterm m ctx r])
      end
    | Mabs v ->
      begin match v.type_ with
        | M.Tbuiltin (M.Bint) -> Tapp (loc_term (Tvar "abs"),[map_mterm m ctx v])
        | M.Ttuple [M.Tbuiltin (M.Bint); M.Tbuiltin M.Bnat] ->
          Tapp (loc_term (Tvar "abs_rat"),[map_mterm m ctx v])
        | _ -> error_not_translated "Mfunabs"
      end

    | Mconcat (x, y) ->
      begin
        match mt.type_ with
        | Tbuiltin Bstring -> Tapp (loc_term (Tvar "str_concat"),[map_mterm m ctx x; map_mterm m ctx y])
        | Tbuiltin Bbytes -> Tapp (loc_term (Tvar "str_concat"),[map_mterm m ctx x; map_mterm m ctx y])
        | _ -> error_not_translated "Mconcat"
      end
    | Mslice  (s,i1,i2) ->
      begin match s.type_ with
        | Tbuiltin Bbytes -> Tapp (loc_term (Tvar "substring"),[map_mterm m ctx s; map_mterm m ctx i1; map_mterm m ctx i2])
        | _ -> Tapp (loc_term (Tvar "substring"),[map_mterm m ctx s; map_mterm m ctx i1; map_mterm m ctx i2])
      end
    | Mlength s ->
      begin match s.type_ with
        | Tbuiltin Bbytes -> Tapp (loc_term (Tvar "str_length"),[map_mterm m ctx s])
        | _ -> Tapp (loc_term (Tvar "str_length"),[map_mterm m ctx s])
      end
    | Misnone s -> Tapp (loc_term (Tvar "isnone"),[map_mterm m ctx s])
    | Missome s -> Tapp (loc_term (Tvar "issome"),[map_mterm m ctx s])
    | Moptget s -> Tapp (loc_term (Tvar "getopt"),[map_mterm m ctx s])
    | Mfloor  s -> Tapp (loc_term (Tvar "floor"),[map_mterm m ctx s])
    | Mceil   s -> Tapp (loc_term (Tvar "ceil"),[map_mterm m ctx s])
    | Mtostring (_, s) -> Tapp (loc_term (Tvar "from_int"),[map_mterm m ctx s])
    | Mpack   s -> Tapp (loc_term (Tvar "pack"),[map_mterm m ctx s])
    | Munpack (_, s) -> Tapp (loc_term (Tvar "unpack"),[map_mterm m ctx s])

    | Mblake2b x -> Tapp (loc_term (Tvar "blake2b"),[map_mterm m ctx x])
    | Msha256  x -> Tapp (loc_term (Tvar "sha256"),[map_mterm m ctx x])
    | Msha512  x -> Tapp (loc_term (Tvar "sha512"),[map_mterm m ctx x])
    | Mhashkey  x -> Tapp (loc_term (Tvar "hash_key"),[map_mterm m ctx x])
    | Mchecksignature (k,s,b) -> Tapp (loc_term (Tvar "check_signature"),[map_mterm m ctx k;map_mterm m ctx s;map_mterm m ctx b])


    (* constants *)

    | Mnow -> Tnow (dl gs)
    | Mtransferred -> Ttransferred (dl gs)
    | Mcaller -> Tcaller (dl gs)

    | Mbalance ->
      begin
        match ctx.lctx with
        | Inv -> loc_term (Tvar "_balance") |> Mlwtree.deloc
        | _ -> loc_term (Tdoti (gs, "_balance")) |> Mlwtree.deloc
      end

    | Msource               -> Tsender (dl gs)
    | Mselfaddress          -> Tdoti(dl gs, dl (mk_id "selfaddress"))
    | Mchainid              -> Tchainid (dl gs)
    | Mmetadata             -> assert false

    (* variables *)

    | Mvar(_, Vassetstate _, _, _) -> error_not_translated "Mvar(_, Vassetstate _)"

    | Mvar (v, Vstorevar, _, _) ->
      begin
        match ctx.lctx with
        | Inv -> Tvar (map_lident v)
        | Def -> Tdoti (dl gsarg, map_lident v)
        | _ -> Tdoti (dl gs, map_lident v)
      end

    | Mvar (n, Vstorecol, t, d) ->
      let coll = mk_loc_coll_term (unloc n) ctx (t,d) in
      coll |> Mlwtree.deloc

    | Mvar (v, Venumval, _, _) -> Tvar (map_lident v)
    | Mvar (v, Vdefinition, _, _) ->
      let params = get_def_params m (unloc v) |> List.map loc_term in
      Tapp (loc_term (Tvar (unloc v)), [loc_term (Tvar (mk_storage_id ctx))] @ params)
    | Mvar (v, Vlocal, _, _) -> Tvar (map_lident v)
    | Mvar (v, Vparam, _, _) -> Tvar (map_lident v)
    | Mvar (_, Vfield, _, _) -> error_not_translated "Mvar (_, Vfield)"
    | Mvar (_, Vthe, _, _)   -> error_not_translated "Mvar (_, Vthe)"
    | Mvar (_, Vstate, _, _) ->
      begin
        match ctx.lctx with
        | Inv -> loc_term (Tvar "state") |> Mlwtree.deloc
        | _ -> loc_term (Tdoti (gs, "state")) |> Mlwtree.deloc
      end



    (* rational ------------------------------------------------------------- *)

    | Mrateq (r,t) -> Tapp (loc_term (Tvar "rat_eq"),[map_mterm m ctx r; map_mterm m ctx t])
    | Mratcmp (cop,r,t) ->
      let cop_to_mterm = function
        | M.Ge -> Tvar "OpCmpGe"
        | M.Le -> Tvar "OpCmpLe"
        | M.Gt -> Tvar "OpCmpGt"
        | M.Lt -> Tvar "OpCmpLt" in
      Tapp (loc_term (Tvar "rat_cmp"),[loc_term (cop_to_mterm cop); map_mterm m ctx r; map_mterm m ctx t])
    | Mratarith (aop,r,t) ->
      let aop_to_mterm = function
        | M.Rplus -> Tvar "OpArithPlus"
        | M.Rminus -> Tvar "OpArithMinus"
        | M.Rmult -> Tvar "OpArithMult"
        | M.Rdiv -> Tvar "OpArithDiv" in
      Tapp (loc_term (Tvar "rat_arith"),[loc_term (aop_to_mterm aop); map_mterm m ctx r; map_mterm m ctx t])
    | Mratuminus v -> Tapp (loc_term (Tvar "rat_uminus"),[map_mterm m ctx v])
    | Mrattez (r,t) -> Tapp (loc_term (Tvar "rat_tez"),[map_mterm m ctx r; map_mterm m ctx t])
    | Mnattoint v -> map_mterm m ctx v |> Mlwtree.deloc
    | Mnattorat v -> Ttuple ([map_mterm m ctx v; loc_term (Tint (Big_int.big_int_of_int 1))])
    | Minttorat v -> Ttuple ([map_mterm m ctx v; loc_term (Tint (Big_int.big_int_of_int 1))])
    | Mratdur (r,t) -> Tapp (loc_term (Tvar "rat_dur"),[map_mterm m ctx r; map_mterm m ctx t])


    (* quantifiers ---------------------------------------------------------- *)

    | Mforall (i, t, None, b) ->
      let typ = map_mtype m t in
      Tforall (
        [[i |> map_lident],typ],
        map_mterm m ctx b)

    | Mforall (i, t, Some coll, b) ->
      let asset = M.Utils.get_asset_type (M.mk_mterm (M.Mbool false) t) in
      Tforall (
        [[i |> map_lident],loc_type (Tyasset asset)],
        dl (Timpl (dl (Tmem (dl asset,
                             loc_term (Tvar (unloc i)),
                             map_mterm m ctx coll)),
                   map_mterm m ctx b)))

    | Mexists (i, t, None, b) ->
      let typ = map_mtype m t in
      Texists (
        [[i |> map_lident],typ],
        map_mterm m ctx b)

    | Mexists (i, t, Some coll, b) ->
      let asset = M.Utils.get_asset_type (M.mk_mterm (M.Mbool false) t) in
      Texists (
        [[i |> map_lident],loc_type (Tyasset asset)],
        dl (Timpl (dl (Tmem (dl asset,
                             loc_term (Tvar (unloc i)),
                             map_mterm m ctx coll)),
                   map_mterm m ctx b)))


    (* formula operators *)

    | Mimply (a, b) -> Timpl  (map_mterm m ctx a, map_mterm m ctx b)
    | Mequiv (a, b) -> Tequiv (map_mterm m ctx a, map_mterm m ctx b)


    (* formula asset collection *)

    | Msetiterated  container ->
      let n = M.Utils.get_asset_type mt in
      let iter_id = Option.get (ctx.loop_id) in
      let arg = begin match container with
        | ICKview v  -> map_mterm m ctx v
        | ICKcoll n  -> dl (Ttoview (dl n, mk_lc_term n ctx))
        | ICKfield (_, _, c) -> dl (Ttoview (dl (mk_field_id n), map_mterm m ctx c))
        | ICKset  _  -> assert false
        | ICKlist _  -> assert false
        | ICKmap  _  -> assert false
      end in
      Tvhead (dl (mk_view_id n), loc_term (Tvar iter_id), arg)

    | Msettoiterate container ->
      let n = M.Utils.get_asset_type mt in
      let iter_id = Option.get (ctx.loop_id) in
      let arg = begin match container with
        | ICKview v  -> map_mterm m ctx v
        | ICKcoll n  -> dl (Ttoview (dl n, mk_lc_term n ctx))
        | ICKfield (_, _, c) -> dl (Ttoview (dl (mk_field_id n), map_mterm m ctx c))
        | ICKset  _  -> assert false
        | ICKlist _  -> assert false
        | ICKmap  _  -> assert false
      end in
      Tvtail (dl (mk_view_id n), loc_term (Tvar iter_id), arg)

    | Mempty an -> Temptycoll(dl (mk_view_id an))
    | Msingleton (an, k) -> Tsingl(dl an, map_mterm m ctx k)
    | Msubsetof (n, c, x) ->
      let arg = mk_container_term m n ctx c in
      Tsubset(dl (mk_view_id n), arg, map_mterm m ctx x)
    | Misempty (n, r) ->
      begin match r.type_ with
        | M.Tcontainer (_,View) -> Tvempty (dl (mk_view_id n), map_mterm m ctx r)
        | _ -> Tempty (dl n, map_mterm m ctx r)
      end
    | Munion     (an, l, r) -> Tunion(dl an, map_mterm m ctx l, map_mterm m ctx r)
    | Minter     (an, l, r) -> Tinter(dl an, map_mterm m ctx l, map_mterm m ctx r)
    | Mdiff      (an, l, r) -> Tdiff(dl an, map_mterm m ctx l, map_mterm m ctx r)
  in
  mk_loc mt.loc t
and mk_invariants (m : M.model) ctx id (lbl : ident option) lbody =
  let loop_invariants =
    Option.fold (M.Utils.get_loop_invariants m) [] lbl |>
    List.map (fun ((ilbl : ident),(i : M.mterm)) ->
        let iid =
          match lbl,ilbl with
          | Some a, b -> b ^ "_" ^ a
          | None, b -> b in
        let ctx = { ctx with loop_id = Option.map unloc id } in
        { id =  dl iid; form = map_mterm m ctx i }
      ) in
  let storage_loop_invariants = (* in storage invariants are strong :
                                         no need to repeat them in loop *)
    M.Utils.get_storage_invariants m None |>
    List.fold_left (fun acc (an,inn,t) ->
        if is_local_invariant m an t && not (adds_asset m an lbody)
        then
          let iid =
            match lbl with
            | Some a -> inn ^ "_" ^ a
            | _ -> inn in
          acc @ [{ id = dl iid;
                   form = mk_loop_invariant m an (map_mterm m ctx t) }]
        else acc
      ) ([] : (loc_term, loc_ident) abstract_formula list) in
  let security_loop_invariants =
    m.security.items
    |> List.filter (fun i -> is_only_security i.M.predicate)
    |> List.map (fun (sec : M.security_item) ->
        let id =
          match lbl with
          | Some a -> (unloc sec.label) ^ "_" ^ a
          | _ -> unloc sec.label in
        { id = dl id;
          form = map_security_pred `Loop sec.predicate |> loc_term; }
      )
  in
  let vars_loop_invariants = mk_vars_loop_invariants m ctx.entry_id lbl (mk_lbl_before lbl) lbody in
  loop_invariants          @
  storage_loop_invariants  @
  security_loop_invariants @
  vars_loop_invariants
and mk_filter_args m ctx args tbody =
  let globals =
    extract_args tbody |>
    List.map (fun (e, _, _) -> e) |>
    List.map (map_mterm m ctx) in
  let args = List.map (fun (i,_) -> loc_term (Tvar i)) args in
  args @ globals
and mk_asset_key_value m ctx a r = begin
  match r with
  | { M.node = (Masset _); type_ = _ } ->
    map_mterm m ctx (M.Utils.extract_key_value_from_masset m r)
  | _ ->
    let (k, _) = M.Utils.get_asset_key m a in
    dl (Tapp(loc_term (Tvar k),[map_mterm m ctx r]))
end
and mk_container_term m n ctx = function
  | M.CKview c  -> map_mterm m ctx c
  | M.CKfield (_, _, c, _, _) -> dl (Ttoview (dl (mk_field_id n), map_mterm m ctx c))
  | M.CKcoll (t,d) -> dl (Ttoview (dl n, mk_loc_coll_term n ctx (t,d)))
  | M.CKdef d ->
    let params = get_def_params m d in
    loc_term (Tapp(Tvar d, [Tvar (mk_storage_id ctx)] @ params))

(* Storage mapping -----------------------------------------------------------*)

let map_asset_values m (values : M.asset_item list) =
  let ctx = { init_ctx with lctx = Inv } in
  List.map (fun (value : M.asset_item) ->
      let typ_ = map_mtype m value.type_ in
      let init_value = type_to_init m typ_ in {
        name     = map_lident value.name;
        typ      = typ_;
        init     = Option.fold (fun _ -> map_mterm m ctx) init_value value.default;
        mutable_ = false;
      }
    ) values

let mk_asset m (r : M.asset) =
  Drecord (map_lident r.name, map_asset_values m r.values)

let map_init_mterm m ctx (t : M.mterm) =
  match t.node with
  | M.Mnow -> loc_term (Tint Big_int.zero_big_int)
  | _ -> map_mterm m ctx t

let mk_storage_items m =
  let ctx = { init_ctx with lctx = Inv } in
  List.fold_left (fun acc (item : M.storage_item) ->
      acc @
      match item.typ with
      | M.Tcontainer (Tasset id, Collection) ->
        let id = unloc id in [
          mk_collection_field id mk_ac_id (Some (match item.default.node with | Massets l -> List.map (map_mterm m ctx) l | _ -> assert false));
          mk_collection_field id mk_ac_added_id None;
          mk_collection_field id mk_ac_rmed_id None
        ]
      | M.Tcontainer (Tasset id, View) ->
        let id = unloc id in [
          mk_collection_field id mk_ac_id None;
          mk_collection_field id mk_ac_added_id None;
          mk_collection_field id mk_ac_rmed_id None
        ]
      | _ ->
        let typ_ = map_mtype m item.typ in [{
            name     = unloc item.id |> dl;
            typ      = typ_;
            init     = map_init_mterm m ctx item.default;
            mutable_ = true;
          }]
    ) []

let mk_asset_invariants m ctx =
  List.concat (List.map (fun (item : M.storage_item) ->
      let storage_id = item.id in
      let invs : M.label_term list =
        match item.model_type with
        | MTasset asset_name ->
          begin
            try
              let assets = M.Utils.get_assets m in
              let asset = List.find (fun (x : M.asset) -> cmp_ident (unloc x.name) asset_name) assets in
              asset.invariants
            with
            | Not_found -> assert false
          end
        | _ -> [] in
      List.map (fun (inv : M.label_term) -> mk_storage_invariant m storage_id inv.label (map_mterm m ctx inv.term)) invs
    ) m.storage)

let mk_contract_invariants m ctx =
  List.fold_left (fun acc (post : M.postcondition) ->
      acc @ [{
          id = map_lident post.name;
          form = map_mterm m ctx post.formula;
        }]
    ) [] m.specification.postconditions

let mk_variable_invariants m ctx =
  List.fold_left (fun acc decl ->
      match decl with
      | M.Dvar var ->
        acc @ (List.map (fun (inv : M.label_term) ->
            { id = map_lident inv.label; form = map_mterm m ctx inv.term}
          ) var.invariants)
      | _ -> acc
    ) [] m.decls

let mk_security_invariants (m : M.model) _ctx =
  List.fold_left (fun acc sec ->
      acc @ (mk_spec_invariant `Storage sec)
    ) [] m.security.items

let mk_state_invariants m ctx =
  List.fold_left (fun acc decl ->
      match decl with
      | M.Denum e ->
        List.fold_left (fun acc (value : M.enum_item) ->
            acc @ List.map (fun (inv : M.label_term) ->
                mk_state_invariant m value.name inv.label (map_mterm m ctx inv.term)
              ) value.invariants
          ) acc e.values
      | _ -> acc
    ) [] m.decls

let mk_storage m (l : M.storage) =
  let ctx = { init_ctx with lctx = Inv } in
  Dstorage {
    fields     = (mk_storage_items m l) @ (mk_const_fields m |> loc_field |> deloc);
    invariants =
      mk_asset_invariants m ctx    @
      mk_security_invariants m ctx @
      mk_state_invariants m ctx    @
      mk_contract_invariants m ctx @
      mk_variable_invariants m ctx
  }

let mk_cp_storage m (l : M.storage) =
  let arg = "_s_storage" in
  Dfun  {
    name = "_cp_storage" |> dl;
    logic = Logic;
    args = [arg |> dl, Tystorage |> dl];
    returns = Tystorage |> dl;
    raises = [];
    fails  = [];
    variants = [];
    requires = [];
    ensures = [{
        id = dl "cp_1";
        form = loc_term (Teq(Tyint,Tresult,Tvar arg));
      }];
    body = dl (Trecord (None, (List.map (fun (f : ('a, loc_typ, ident with_loc) abstract_field) ->
        f.name, dl (Tdoti(dl arg,f.name))
      ) (mk_storage_items m l)) @ (List.map (fun (f : (('a, 'b, ident) abstract_term, (ident, (ident, 'c) abstract_type) abstract_type, ident) abstract_field) ->
        dl f.name, dl (Tdoti (dl arg, dl f.name))
      ) (mk_const_fields m))))
  }

(* Verfication API -----------------------------------------------------------*)

let mk_axioms (m : M.model) : (loc_term, loc_typ, loc_ident) abstract_decl list =
  List.fold_left (fun acc apiv ->
      match apiv with
      | M.StorageInvariant (id,asset,formula) ->
        acc @ [
          Dtheorem (Axiom,
                    dl (asset ^ "_" ^ id ^ "_axiom"),
                    mk_axiom_invariant m asset (map_mterm m init_ctx formula));
          Dtheorem (Lemma,
                    dl (asset ^ "_" ^ id ^ "_axiom_2"),
                    mk_axiom2_invariant m asset (map_mterm m init_ctx formula))]

    ) [] m.api_verif

(* Storage API templates -----------------------------------------------------*)

let mk_api_precond m apid a src =
  M.Utils.get_storage_invariants m (Some a)
  |> List.fold_left (fun acc (_,lbl,t) ->
      if is_local_invariant m a t then
        acc @ [{
            id = "require_" ^ apid ^ "_" ^ lbl;
            form = unloc_term (mk_invariant m (dumloc a) src (map_mterm m init_ctx t))
          }]
      else acc
    ) []

let mk_key_found_cond old asset var =
  let coll = match old with
    | `Old -> mk_ac_old asset
    | `Curr -> mk_ac asset in
  Tneq(Tyint, Tget (asset, var, coll), Tnone)

let mk_not_found_cond old asset var =
  let coll =
    match old with
    | `Curr -> mk_ac asset
    | `Old -> mk_ac_old asset in
  Teq(Tyint, Tget(asset, var, coll), Tnone)

(* formula is in mlw tree *)
let mk_get_sum_value_from_pos asset id formula =
  Dfun {
    name = mk_get_sum_value_from_pos_id asset id;
    logic = Logic;
    args = ["v",Tyview asset; "c",Tycoll asset; "i",Tyint];
    returns = Tyint;
    raises = [];
    fails = [];
    variants = [];
    requires = [];
    ensures = [];
    body =
      let rec mk_body = function
        | Tdot (Tvar v,f) when compare v "the" = 0 ->
          Tmatch (
            Tcnth (asset,
                   Tvar "i",
                   Tvar "v"), [
              Tpsome "k",(Tmatch (Tget(asset,
                                       Tvar "k",
                                       Tvar "c"),[
                                    Tpsome "e", Tapp (f,[Tvar "e"]);
                                    Twild, Tint (Big_int.zero_big_int)
                                  ]));
              Twild,Tint (Big_int.zero_big_int)
            ]
          )
        | _ as t -> map_abstract_term mk_body Tools.id Tools.id t in
      mk_body formula
  }

let mk_get_sum_value asset id formula =
  Dfun {
    name = mk_get_sum_value_id asset id;
    logic = Logic;
    args = ["a",Tyasset asset];
    returns = Tyint;
    raises = [];
    fails = [];
    variants = [];
    requires = [];
    ensures = [];
    body =
      let rec mk_body = function
        | Tdot (Tvar v,f) when compare v "the" = 0 ->
          Tdot (Tvar "a",f)
        | _ as t -> map_abstract_term mk_body Tools.id Tools.id t in
      mk_body formula
  }

(* Storage API -------------------------------------------------------------- *)

let mk_storage_api_before_storage (m : M.model) _records =
  m.api_items |> List.fold_left (fun acc (sc : M.api_storage) ->
      match sc.node_item, sc.api_loc with
      | M.APIAsset (Sum (asset,_,_,formula)), _ when compare asset "todo" <> 0 ->
        let key, tkey      = M.Utils.get_asset_key m asset in
        let mlw_formula = map_mterm m init_ctx formula |> unloc_term in
        let id = M.Utils.get_sum_idx m asset formula in
        acc @ [ (*mk_get_sum_value_from_pos asset id mlw_formula;*)
          mk_get_sum_value asset id mlw_formula;
          mk_sum_clone m asset key tkey formula ]
      | M.APIAsset (Select (asset, _, args, test)), _ ->
        let mlw_test = map_mterm m init_ctx test in
        acc @ [ mk_select_predicate m asset test (mlw_test |> unloc_term) args ]
      | M.APIAsset (RemoveIf (asset, Field (_,field), args, test)), _ ->
        let mlw_test = map_mterm m init_ctx test in
        let oasset,_ = M.Utils.get_field_container m asset field in
        acc @ [ mk_removeif_predicate m oasset test (mlw_test |> unloc_term) args ]
      | M.APIAsset (RemoveIf (asset, Coll, args, test)), _ ->
        let mlw_test = map_mterm m init_ctx test in
        acc @ [ mk_removeif_predicate m asset test (mlw_test |> unloc_term) args ]
      | _ -> acc
    ) [] |> loc_decl |> deloc

let mk_storage_api (m : M.model) _records =
  m.api_items |> List.fold_left (fun acc (sc : M.api_storage) ->
      match sc.node_item, sc.api_loc with
      (*   | M.APIAsset (Nth (n, _)), _ ->
           acc @ [mk_nth_asset m n] *)
      | M.APIAsset (Sort (asset, _, field)), _ ->
        acc @ [ mk_cmp_function m asset field; mk_sort_clone m asset field]
      | M.APIBuiltin(Babs (M.Tbuiltin M.Bint)), _ ->
        acc @ [Duse (true,["int";"Abs"],None)]
      | _ -> acc
    ) [] |> loc_decl |> deloc

(* Entries --------------------------------------------------------------------*)

let fold_fails m ctx body : (loc_ident option * loc_term) list =
  let rec internal_fold_fails acc (term : M.mterm) =
    match term.M.node with
    | M.Mfail (Invalid v) ->
      let idx = get_fail_idx m v.type_ in
      let fails =
        Option.fold (fun acc (spec : M.specification) -> acc @ spec.fails) []
          (Option.fold (fun _ id -> M.Utils.get_specification m id) None ctx.entry_id) in
      (* retrieve fails with same arg type *)
      let formulas = List.fold_left (fun acc (fail : M.fail) ->
        let fidx = get_fail_idx m fail.atype in
        if compare idx fidx = 0 then
          acc @ [
            Some (map_lident fail.label),
            dl (Timpl (loc_term (Texn (Efail (idx, Some (Tvar (unloc fail.arg))))), map_mterm m ctx (fail.formula)))
          ]
        else acc
      ) [] fails in
      if compare (List.length formulas) 0 = 0 then
        acc @ [None, loc_term (Texn (Efail (idx, None)))]
      else acc @ formulas
    | _ ->  M.fold_term internal_fold_fails acc term in
  internal_fold_fails [] body

let fold_exns m body : term list =
  let rec internal_fold_exn acc (term : M.mterm) =
    match term.M.node with
    | M.Mget (_, _, k) -> internal_fold_exn (acc @ [Texn Enotfound]) k
    | M.Mmapget (_ , _, c, k) -> internal_fold_exn (internal_fold_exn (acc @ [Texn Enotfound]) k) c
    | M.Mnth (_, CKview c, k) -> internal_fold_exn (internal_fold_exn (acc @ [Texn Enotfound]) c) k
    | M.Mnth (_, CKcoll _, k) -> internal_fold_exn ((acc @ [Texn Enotfound])) k
    | M.Mset (_, _, k, v) -> internal_fold_exn (internal_fold_exn (acc @ [Texn Enotfound]) k) v
    | M.Maddasset (_, i) -> internal_fold_exn (acc @ [Texn Ekeyexist]) i
    | M.Maddfield (a, f, c, i) ->
      internal_fold_exn
        (internal_fold_exn (acc @ if (is_partition m a f) then [Texn Ekeyexist; Texn Enotfound]
                            else [Texn Enotfound ]) c) i
    | M.Mremovefield (_,_,k,v) -> internal_fold_exn
                                    (internal_fold_exn (acc @ [Texn Enotfound]) k) v
    | M.Mremoveall (_a,_f,v) -> internal_fold_exn (acc @ [Texn Enotfound]) v
    | M.Mremoveif (_, CKfield (_,_,k,_,_), _, _ ,_ ) -> internal_fold_exn (acc @ [Texn Enotfound]) k
    | M.Mclear (_a,CKfield (_,_,k,_,_)) -> internal_fold_exn (acc @ [Texn Enotfound]) k
    | M.Moptget _ -> acc @ [Texn Enotfound]
    | M.Mfail InvalidCaller -> acc @ [Texn Einvalidcaller]
    | M.Mfail NoTransfer -> acc @ [Texn Enotransfer]
    | M.Mfail (InvalidCondition _) -> acc @ [Texn Einvalidcondition]
    | M.Mfail InvalidState -> acc @ [Texn Einvalidstate]
    | M.Mfail AssignNat -> acc @ [Texn Enegassignnat]
    | M.Mlistnth _ -> acc @ [Texn Enotfound]
    | M.Mself _ -> acc @ [Texn Enotfound]
    | M.Mcast (Tbuiltin Baddress, Tcontract _, v) -> internal_fold_exn (acc @ [Texn Enotfound]) v
    | M.Mtransfer (v, TKself _) -> internal_fold_exn (acc @ [Texn Enotfound]) v
    | M.Mtransfer (v, _) -> internal_fold_exn acc v
    | M.Mapp (id, args) ->
      let fun_struct = M.Utils.get_function m (unloc id) in
      List.fold_left (fun acc arg ->
          internal_fold_exn acc arg) (internal_fold_exn acc fun_struct.body) args
    | _ -> M.fold_term internal_fold_exn acc term in
  Tools.List.dedup (internal_fold_exn [] body)

(* THEORY -------------------------------------------------------------------- *)

let mk_theory m =
  List.fold_left (fun acc (spec : M.specification) ->
      let ctx = { init_ctx with lctx = Def } in
      let defs = List.map (fun (def : M.definition) ->
          let t = map_mtype m (M.Tcontainer (def.typ,M.View)) in
          let asset = M.Utils.type_to_asset def.typ in
          let params = extract_def_args m def.body |> List.map (fun (_,id,typ) -> (dl id,typ)) in
          Dfun {
            name = map_lident def.name;
            logic = LogicOnly;
            args = [ dl gsarg, dl Tystorage ] @ params;
            returns = t;
            raises = [];
            fails = [];
            variants = [];
            requires = [];
            ensures = [];
            body =
              let coll = loc_term (mk_ac_st gsarg asset) in
              let select = dl (Tselect(
                  dl (M.Utils.type_to_asset def.typ),
                  dl (Tlambda ([map_lident def.var],map_mterm m ctx def.body)),
                  coll)) in
              dl (Ttoview (dl asset,select))
          }
        ) spec.definitions in
      let preds = List.map (fun (pred : M.predicate) ->
          let args = pred.args |> List.map (fun (i,t) -> map_lident i, map_mtype m t) in
          let params = extract_def_args m pred.body |> List.map (fun (_,id,typ) -> (dl id,typ)) in
          Dpred (map_lident pred.name, [dl gsarg, dl Tystorage] @ params @ args, map_mterm m ctx pred.body)
        ) spec.predicates in
      acc @ defs @ preds
    ) [] (M.Utils.get_specifications m)

(* ENTRIES & FUNCTIONS ------------------------------------------------------- *)

let is_fail (t : M.mterm) =
  match t.node with
  | M.Mfail _ -> true
  | _ -> false

let flatten_if_fail m ctx (t : M.mterm) : loc_term =
  let rec rec_flat acc (t : M.mterm) : loc_term list =
    match t.node with
    | M.Mif (c,th, Some e) when is_fail th ->
      rec_flat (acc@[mk_loc t.loc (Tif (map_mterm m ctx c, map_mterm m ctx th,None))]) e
    | _ -> acc @ [map_mterm m ctx t] in
  mk_loc t.loc (Tseq (rec_flat [] t))

let mk_ensures m acc (v : M.specification) =
  acc @ (List.map (fun (spec : M.postcondition) -> {
        id = spec.name |> map_lident;
        form = map_mterm m { init_ctx with lctx = Logic } spec.formula
      }) (v.postconditions |> List.filter M.Utils.is_post))

let mk_delta_requires m =
  M.Utils.get_assets m |>
  List.map (fun (a : M.asset) ->
      (* for each asset, generate preconditions that added and removed are empty *)
      let name = unloc a.name in
      [{
        id = dl ("require_" ^ name ^ "_added_isempty");
        form =
          Tempty (name, mk_ac_added name)
          |> loc_term
      }; {
         id = dl (name ^ "_removed_isempty");
         form =
           Tempty (name, mk_ac_rmed name)
           |> loc_term
       }]
    ) |> List.flatten

(* for each arg, retrieve corresponding type invariants, and convert it to precondition *)
let mk_preconds m args body : ((loc_term,loc_ident) abstract_formula) list =
  List.fold_left (fun acc (id,t,_) ->
      match t with
      | M.Tasset n ->
        let n = unloc n in
        M.Utils.get_storage_invariants m (Some n)
        |> List.fold_left (fun acc  (_, lbl, t) ->
            if is_local_invariant m n t && adds_asset m n body then
              (* TODO : should be more specific : the real question is
                 'is the element id added?' *)
              acc @ [{
                  id = dl lbl;
                  form = mk_pre_asset m n (unloc id) (map_mterm m init_ctx t)
                }]
            else
              acc
          ) []
      | M.Tcontainer (Tasset n, Collection)
      | M.Tcontainer (Tasset n, Aggregate)
      | M.Tcontainer (Tasset n, Partition) ->
        let n = unloc n in
        M.Utils.get_storage_invariants m (Some n)
        |> List.fold_left (fun acc  (_,lbl,t) ->
            if is_local_invariant m n t && adds_asset m n body then
              (* TODO similar to above : should be more specific : the real question is
                 'is the element of id added?' *)
              acc @ [{
                  id = dl lbl;
                  form = mk_pre_coll m n (unloc id) (map_mterm m init_ctx t)
                }]
            else
              acc
          ) []
      | _ -> acc
    ) [] args

let mk_entry_require m idents =
  if M.Utils.with_trace m && List.length idents > 0 then
    let mk_entry_eq id = Teq (Tyint,
                              Tdoti (gs,mk_id "entry"),
                              Tsome (Tvar (mk_trace_id Entry id))) in
    [
      {
        id = dl "entry_require";
        form =
          List.fold_left (fun acc id ->
              Tor (acc,mk_entry_eq id)
            ) (mk_entry_eq (List.hd idents)) (List.tl idents)
          |> loc_term
      };
      {
        id = dl "empty_ops";
        form = Teq (Tyint,
                    Tdoti (gs, mk_id "ops"),
                    Tnil gListAs)
               |> loc_term
      };
      {
        id = dl "empty_trace";
        form = Teq (Tyint,
                    Tdoti (gs, mk_id "tr"),
                    Tnil gListAs)
               |> loc_term
      }
    ]
  else []

let mk_functions m =
  M.Utils.get_functions m |> List.map (
    fun ((v : M.specification option),
         (s : M.function_struct),
         (t : M.type_)) ->
      let args = (List.map (fun (i, t, _) ->
          (map_lident i, map_mtype m t)
        ) s.args) in
      let ctx = { init_ctx with entry_id = Some (unloc s.name) } in
      Dfun {
        name     = map_lident s.name;
        logic    = NoMod;
        args     = [dl gsinit, loc_type Tystorage] @ args;
        returns  = map_mtype m t;
        raises   = fold_exns m s.body |> List.map loc_term;
        fails    = fold_fails m { ctx with lctx = Logic } s.body;
        variants = [];
        requires =
          (mk_entry_require m (M.Utils.get_callers m (unloc s.name))) @
          (* (mk_delta_requires m) @ *)
          (mk_preconds m s.args s.body);
        ensures  = Option.fold (mk_ensures m) [] v;
        body     = flatten_if_fail m ctx s.body;
      }
  )

let mk_entries m =
  M.Utils.get_entries m |> List.map (
    fun ((v : M.specification option),
         (s : M.function_struct)) ->
      let ctx = { init_ctx with entry_id = Some (unloc s.name) } in
      Dfun {
        name     = map_lident s.name;
        logic    = NoMod;
        args     = (List.map (fun (i,t,_) ->
            (map_lident i, map_mtype m t)
          ) s.args);
        returns  = dl Tyunit;
        raises   = fold_exns m s.body |> List.map loc_term;
        fails    = fold_fails m { ctx with lctx = Logic } s.body;
        variants = [];
        requires =
          (mk_entry_require m [unloc s.name]) @
          (mk_delta_requires m);
        ensures  = Option.fold (mk_ensures m) [] v;
        body     = flatten_if_fail m ctx s.body;
      }
  )

let rm_fail_exn = List.filter (fun e ->
    match unloc_term e with
    | Texn Enotfound
    | Texn Ekeyexist -> false
    | _ -> true)

let process_no_fail m (d : (loc_term, loc_typ, loc_ident) abstract_decl) =
  match d with
  | Dfun f ->
    begin
      match M.Utils.no_fail m (Mlwtree.deloc f.name) with
      | Some id ->
        Dfun { f with
               raises = rm_fail_exn f.raises;
               body   =
                 let body =
                   loc_term (Ttry (unloc_term f.body, [
                       Enotfound,Tassert (Some ("security_" ^ id),Tfalse);
                       Ekeyexist,Tassert (Some ("security_" ^ id),Tfalse)
                     ])) in
                 loc_term (
                   Tletin (false, gsinit, None, cp_storage gs, unloc_term body));
             }
      | _ -> (* *)
        Dfun { f with
               body   = loc_term (
                   Tletin (false, gsinit, None, cp_storage gs, unloc_term f.body));
             }
    end
  | _ -> d

(* ----------------------------------------------------------------------------*)

let to_whyml (m : M.model) : mlw_tree  =
  let assets           = M.Utils.get_assets m in
  let storage_module   = dl ((mk_module_name m.name.pldesc) ^ "_storage") in
  let uselib           = mk_use in
  let uselist          = mk_use_list in
  let uses             = [mk_use; mk_use_list; mk_use_field; mk_use_view] in
  let useEuclDiv       = mk_use_euclidean_div m in
  let useMinMax        = mk_use_min_max m in
  let traceutils       = mk_trace_utils m |> deloc in
  let enums            = M.Utils.get_enums m |> List.map (mk_enum m) in
  let exns             = M.Utils.get_all_fail_types m |> List.mapi (mk_exn m) in
  let records          = M.Utils.get_records m |> List.map (mk_record m) in
  let lists            = M.Utils.get_all_list_types m |> List.map (mk_list_type m) |> List.flatten in
  let maps             = M.Utils.get_all_map_types m |> List.map (mk_map_type m) |> List.flatten in
  let sets             = M.Utils.get_all_set_types m |> List.map (mk_set_type m) |> List.flatten in
  let mlwassets        = assets |> List.map (mk_asset m) |> wdl in
  let eq_enums        = assets |> List.map (mk_eq_enums m) |> List.flatten in
  let eq_keys          = assets |> List.map (mk_eq_key m) |> wdl in
  let le_keys          = assets |> List.map (mk_le_key m) |> wdl in
  let eq_assets        = assets |> List.map (mk_eq_asset m) |> wdl in
  let colls            = assets |> List.map (mk_coll m) |> wdl in
  let fields           = assets |> List.map (mk_field m) |> wdl in
  let views            = assets |> List.map (mk_view m) |> wdl in
  let aggregates       = assets |> List.map (mk_aggregates m) |> List.flatten in
  let init_records     = mlwassets |> unloc_decl |> List.map mk_default_init |> loc_decl in
  let mlwassets        = zip mlwassets eq_keys le_keys eq_assets init_records views fields colls |> deloc in
  let storage_api_bs   = mk_storage_api_before_storage m (records |> wdl) in
  let storage          = M.Utils.get_storage m |> mk_storage m in
  let cp_storage       = M.Utils.get_storage m |> mk_cp_storage m in
  let storageval       = Dval (true, dl gs, dl Tystorage) in
  let axioms           = mk_axioms m in
  let theory           = mk_theory m in
  let storage_api      = mk_storage_api m (mlwassets |> wdl) in
  let functions        = mk_functions m in
  let entries          = mk_entries m |> List.map (process_no_fail m) in
  let usestorage       = mk_use_module storage_module in
  let loct : loc_mlw_tree = [{
      name  = storage_module;
      decls = uses                   @
              useEuclDiv             @
              useMinMax              @
              traceutils             @
              exns                   @
              enums                  @
              records                @
              eq_enums               @
              lists                  @
              maps                   @
              sets                   @
              mlwassets              @
              aggregates             @
              storage_api_bs         @
              [storage;cp_storage;storageval]   @
              axioms                 @
              storage_api            @
              theory;
    };{
       name = dl (mk_module_name (unloc m.name));
       decls = [uselib;uselist;usestorage] @
               functions @
               entries;
     }] in unloc_tree loct
