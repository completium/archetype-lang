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

(* Constants -------------------------------------------------------------------*)

let gArchetypeDir   = "archetype"
let gArchetypeLib   = "Lib"
let gArchetypeColl  = "AssetCollection"
let gArchetypeSum   = "Sum"
let gArchetypeSort  = "Sort"
let gArchetypeList  = "KeyListUtils"
let gArchetypeTrace = "Trace"

let mk_id i          = "_" ^ i

let mk_ac_id a        = mk_id (a ^ "_assets")
let mk_ac_added_id a  = mk_id (a ^ "_assets_added")
let mk_ac_rmed_id a   = mk_id (a ^ "_assets_removed")

let gs                = "_s"

let mk_ac a           = Tdoti (gs, mk_ac_id a)
let mk_ac_old a       = Tdot (Told (Tvar gs), Tvar (mk_ac_id a))

let mk_ac_added a     = Tdoti (gs, mk_ac_added_id a)
let mk_ac_old_added a = Tdot (Told (Tvar gs), Tvar (mk_ac_added_id a))

let mk_ac_rmed a      = Tdoti (gs, mk_ac_rmed_id a)
let mk_ac_old_rmed a  = Tdot (Told (Tvar gs), Tvar (mk_ac_rmed_id a))

let mk_ac_sv s a      = Tdoti (s, mk_ac_id a)

(* Use ---------------------------------------------------------------------------*)

let mk_use = Duse [gArchetypeDir;gArchetypeLib] |> loc_decl |> deloc
let mk_use_list = Duse ["list";"List"] |> loc_decl |> deloc
let mk_use_module m = Duse [deloc m] |> loc_decl |> deloc
let mk_use_euclidean_div m =
  if M.Utils.with_division m then
    [Duse ["int";"EuclideanDivision"]  |> loc_decl |> deloc]
  else []

(* Trace -------------------------------------------------------------------------*)

type change =
  | CAdd of ident
  | CRm of ident
  | CUpdate of ident
  | CTransfer of ident
  | CGet of ident
  | CIterate of ident
  | CCall of ident

let mk_change_term tr =
  match tr with
  | CAdd id -> Tapp (Tdoti("Tr",
                           "TrAdd_"),
                     [Tvar (String.capitalize_ascii id)])
  | CRm id -> Tapp (Tdoti("Tr",
                          "TrRm_"),
                    [Tvar (String.capitalize_ascii id)])
  | CUpdate id -> Tapp (Tdoti("Tr",
                              "TrUpdate_"),
                        [Tvar (String.capitalize_ascii id)])
  | CGet id -> Tapp (Tdoti("Tr",
                           "TrGet_"),
                     [Tvar (String.capitalize_ascii id)])
  | _ -> assert false

let mk_trace tr =
  let gstr = Tdoti(gs,
                   mk_id "tr") in
  Tassign (gstr,
           Tcons (mk_change_term tr,
                  gstr)
          ) |> loc_term

let mk_trace_asset m =
  let assets = M.Utils.get_assets m in
  if List.length assets > 0 then
    [ Denum ("_asset",
             assets
             |> List.map (fun (a : M.asset) -> String.capitalize_ascii (unloc a.name)))
      |> loc_decl]
  else []

let mk_trace_entry m =
  Denum ("_entry",
         M.Utils.get_entries m
         |> List.map (fun (_, (f : M.function_struct)) -> String.capitalize_ascii (unloc f.name)))
  |> loc_decl

let mk_trace_field m =
  Denum ("_field",
         (M.Utils.get_vars m
          |> List.map (fun (v : M.var) -> String.capitalize_ascii (unloc v.name))) @
         (M.Utils.get_assets m
          |> List.map (fun (a : M.asset) ->
              List.map (fun (x : M.asset_item) -> String.capitalize_ascii (unloc x.name)) a.values
            )
          |> List.flatten))
  |> loc_decl

let mk_trace_clone () =
  Dclone ([gArchetypeDir;gArchetypeTrace], "Tr",
          [Ctype ("_asset","_asset");
           Ctype ("_entry","_entry");
           Ctype ("_field","_field")])
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
      variants = [];
      requires = [];
      ensures  = [];
      body     = Trecord (None, List.map (fun (f:field) ->
          f.name, f.init
        ) fs);
    }
  | _ -> assert false


let mk_collection_field asset to_id = {
  name     = with_dummy_loc (to_id asset);
  typ      = loc_type (Tycoll asset);
  init     = loc_term (Temptycoll asset);
  mutable_ = true;
}

let mk_const_fields m = [
  { name = mk_id "ops"   ; typ = Tyrecord "transfers" ; init = Tvar "Nil"; mutable_ = true; };
  { name = mk_id "balance" ;     typ = Tytez; init = Tint Big_int.zero_big_int; mutable_ = true; };
  { name = mk_id "transferred" ; typ = Tytez; init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = mk_id "caller"    ; typ = Tyaddr;  init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = mk_id "now"       ; typ = Tydate;  init = Tint Big_int.zero_big_int; mutable_ = false; }
] @
  if M.Utils.with_trace m then
    [
      { name = mk_id "entry"     ; typ = Tyoption (Tyasset "_entry"); init = Tnone; mutable_ = false; };
      { name = mk_id "tr"        ; typ = Tyasset ("Tr._traces"); init = Tnil; mutable_ = true; }
    ]
  else []

let mk_sum_clone_id m a f = (String.capitalize_ascii a) ^"Sum" ^ (string_of_int (M.Utils.get_sum_idx m a f))
let mk_sum_name m asset formula =  (mk_sum_clone_id m asset formula)^".sum"
let mk_sum_clone_from_id asset id = (String.capitalize_ascii asset) ^"Sum" ^ (string_of_int id)
let mk_sum_name_from_id asset id = (mk_sum_clone_from_id asset id)^".sum"
let mk_get_sum_value_id asset id = "get_" ^ asset ^ "_sum" ^ (string_of_int id)
let mk_get_sum_value_from_pos_id asset id = (mk_get_sum_value_id asset id)^"_from_pos"

let mk_sum_clone m asset key formula =
  let cap_asset = String.capitalize_ascii asset in
  let id = M.Utils.get_sum_idx m asset formula in
  Dclone ([gArchetypeDir;gArchetypeSum],
          mk_sum_clone_from_id asset id,
          [Ctype ("container",
                  cap_asset ^ ".collection");
           Ctype ("t",
                  asset);
           Cval ("f",
                 mk_get_sum_value_from_pos_id asset id);
           Cval ("field",
                 mk_get_sum_value_id asset id);
           Cval ("card",
                 cap_asset ^ ".card");
           Cfun ("union",
                 cap_asset ^ ".union");
           Cfun ("inter",
                 cap_asset ^ ".inter");
           Cfun ("diff",
                 cap_asset ^ ".diff");
           Cpred("is_empty",
                 cap_asset ^ ".is_empty");
           Cpred("subset",
                 cap_asset ^ ".subset");
           Cval ("singleton",
                 cap_asset ^ ".singleton");
           Cval ("witness",
                 cap_asset ^ ".witness");
           Cval ("keyf",
                 key);
          ])


(* n is the asset name
   f is the name of the key field
   ktyp is the key type
*)
let mk_keys_eq_axiom n f ktyp : decl =
  Dtheorem (Axiom,
            "eq_" ^ n ^ "_keys",
            Tforall ([["s"],Tystorage;["k"],ktyp],
                     Timpl (Tmem (n,
                                  Tvar "k",
                                  Tdoti ("s",n ^ "_keys")),
                            Teq (Tyint,
                                 Tapp (Tvar f,
                                       [Tget (n,
                                              Tdoti ("s",n ^ "_assets"),
                                              Tvar "k")]),
                                 Tvar "k"))))

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
                                  mk_ac_sv "s" asset),
                            Timpl (Tlmem (gArchetypeList,
                                          Tvar "k",
                                          Tapp (Tvar f,
                                                [Tvar "a"])),
                                   Tcontains (pa,
                                              Tvar "k",
                                              mk_ac_sv "s" pa)))))

(* Transfer ------------------------------------------------------------------*)

let mk_transfer () =
  let decl : (term, typ, ident) abstract_decl = Dfun {
      name     = "transfer";
      logic    = NoMod;
      args     = ["a", Tyint; "t", Tyaddr];
      returns  = Tyunit;
      raises   = [];
      variants = [];
      requires = [];
      ensures  = [
        { id   = "transfer_post_1";
          form = Teq(Tyint,
                     Tdoti(gs,"_balance"),
                     Tminus(Tyint,
                            Tdot(Told (Tvar gs),Tvar "_balance"),
                            Tvar "a"))
        }
      ];
      body     = Tseq[
          Tassign (
            Tdoti(gs,"_ops"),
            Tcons (
              Tapp(Tvar "mk_transfer",[Tvar "t";Tvar "a"]),
              Tdoti(gs,"_ops")
            ));
          Tassign (
            Tdoti (gs,"_balance"),
            Tminus (Tyint,
                    Tdoti (gs,"_balance"),
                    Tvar "a"
                   )
          )
        ]
    } in
  loc_decl decl |> deloc

(* Sort ----------------------------------------------------------------------*)

let sort_kind_to_string = function
| M.SKasc -> "asc"
| M.SKdesc -> "desc"

let mk_cmp_function_id asset fields = "cmp_" ^ asset ^ "_" ^
  (String.concat "_" (List.map (fun (f,k) ->
    f ^ "_" ^ (sort_kind_to_string k)) fields))

let rec mk_cmp_function_body fields =
  match fields with
  | [field,kind] ->
    let a, b =
    begin match kind with
    | M.SKasc -> Tdoti("a", field),Tdoti("b", field)
    | M.SKdesc -> Tdoti("b", field),Tdoti("a", field)
    end in
    Tle (Tyint, a, b)
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
        mk_cmp_function_body tl,
        Some (Tfalse)
      ))
    )
  | [] -> Ttrue

let mk_cmp_function _m asset fields =
  let decl : (term, typ, ident) abstract_decl = Dfun {
      name     = mk_cmp_function_id asset fields;
      logic    = Logic;
      args     = ["a", Tyasset asset; "b", Tyasset asset];
      returns  = Tybool;
      raises   = [];
      variants = [];
      requires = [];
      ensures  = [];
      body     = mk_cmp_function_body fields
    } in
  decl

let mk_sort_clone_id asset fields =
  (String.capitalize_ascii asset) ^ "Sort" ^
  (String.concat "" (List.map (fun (f,k) ->
    (String.capitalize_ascii f) ^ (String.capitalize_ascii (sort_kind_to_string k))) fields))

let mk_sort_clone _m asset fields =
  let cap_asset = String.capitalize_ascii asset in
  Dclone ([gArchetypeDir;gArchetypeSort],
          mk_sort_clone_id asset fields,
          [Ctype ("container",
                  cap_asset ^ ".collection");
           Ctype ("t",
                  asset);
           Cval ("cmp",
                 mk_cmp_function_id asset fields);
           Cval ("elts",
                 cap_asset ^ ".elts");
           Cval ("mk",
                 cap_asset ^ ".mk")
          ])

(* Select --------------------------------------------------------------------*)

(* TODO : complete mapping *)
let rec mk_select_test = function
  | Tdot (Tvar v,f) when compare v "the" = 0 -> Tdot (Tvar "a",f)
  | Tnow _ -> Tvar (mk_id "now")
  | Tcaller _ -> Tvar (mk_id "caller")
  | _ as t -> map_abstract_term mk_select_test id id t

(* internal select is a local defintion *)
let mk_select_body asset mlw_test : term =
  let capa = String.capitalize_ascii asset in
  Tletfun (
    {
      name     = "internal_select";
      logic    = Rec;
      args     = ["l",Tylist (Tyasset asset)];
      returns  = Tylist (Tyasset asset);
      raises   = [];
      variants = [Tvar "l"];
      requires = [];
      ensures  = [];
      body     = Tmlist (Tnil,"l","a","tl",
                         Tif(mk_select_test mlw_test,
                             Tcons (Tvar "a",Tapp (Tvar ("internal_select"),[Tvar "tl"])),
                             Some (Tapp (Tvar ("internal_select"),[Tvar "tl"])))
                        );
    },
    Tmkcoll (capa,
             Tapp (Tvar "internal_select",
                   [Tcontent (capa,
                              Tvar "c")]))
  )

(* TODO : complete mapping
   argument extraction is done on model's term because it is typed *)
let extract_args test =
  let rec internal_extract_args acc (term : M.mterm) =
    match term.M.node with
    | M.Mnow -> acc @ [term,mk_id "now", Tydate]
    | M.Mcaller -> acc @ [term,mk_id "caller", Tyaddr]
    | _ -> M.fold_term internal_extract_args acc term in
  internal_extract_args [] test

let mk_select_name m asset test = "select_" ^ asset ^ "_" ^ (string_of_int (M.Utils.get_select_idx m asset test))

let mk_select m asset test mlw_test only_formula =
  let id =  mk_select_name m asset test in
  let decl = Dfun {
      name     = id;
      logic    = if only_formula then Logic else NoMod;
      args     = (extract_args test |> List.map (fun (_,a,b) -> a,b)) @ ["c",Tycoll asset];
      returns  = Tycoll asset;
      raises   = [];
      variants = [];
      requires = [];
      ensures  = [
        { id   = id ^ "_post_1";
          form = Tforall (
              [["a"],Tyasset asset],
              Timpl (Tmem (asset,Tvar "a",Tresult),
                     mk_select_test mlw_test
                    )
            );
        };
        { id   = id ^ "_post_2";
          form = Tsubset (asset,
                          Tresult,
                          Tvar "c");
        }
      ];
      body     = mk_select_body asset mlw_test;
    } in
  decl

(* Utils ----------------------------------------------------------------------*)

let wdl (l : 'a list)  = List.map with_dummy_loc l
let unloc_decl = List.map unloc_decl
let loc_decl   = List.map loc_decl
let loc_field  = List.map loc_field
let deloc (l : 'a list) = List.map deloc l

let rec zip l1 l2 l3 l4 =
  match l1,l2,l3,l4 with
  | e1::tl1,e2::tl2,e3::tl3,e4::tl4 -> e1::e2::e3::e4::(zip tl1 tl2 tl3 tl4)
  | _ -> []

let cap s = mk_loc s.loc (String.capitalize_ascii s.obj)

(* Map model term -------------------------------------------------------------*)

let map_lident (i : M.lident) : loc_ident = {
  obj = i.pldesc;
  loc = i.plloc;
}

let map_lidents = List.map map_lident

let type_to_init (typ : loc_typ) : loc_term =
  mk_loc typ.loc (match typ.obj with
      | Typartition i -> Temptycoll i
      | Tycoll i      -> Temptycoll i
      | Tylist _      -> Tnil
      | Tymap i       -> Tvar (mk_loc typ.loc ("const (mk_default_" ^ i.obj ^ " ())"))
      | _             -> Tint Big_int.zero_big_int)

let map_btype = function
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
  | M.Bkey           -> Tykey
  | M.Bbytes         -> Tybytes

let rec map_mtype (t : M.type_) : loc_typ =
  with_dummy_loc (match t with
      | M.Tasset id                           -> Tyasset (map_lident id)
      | M.Tenum id                            -> Tyenum (map_lident id)
      | M.Tbuiltin v                          -> map_btype v
      | M.Tcontainer (Tasset id,M.Partition)  -> Typartition (map_lident id)
      | M.Tcontainer (Tasset id,M.Collection) -> Tycoll (map_lident id)
      | M.Tcontainer (t,M.Collection)         -> Tylist (map_mtype t).obj
      | M.Toption t                           -> Tyoption (map_mtype t).obj
      | M.Ttuple l                            -> Tytuple (l |> List.map map_mtype |> deloc)
      | M.Tunit                               -> Tyunit
      | M.Tstate                              -> Tystate
      | M.Tassoc (_, _)                       -> Tyunit (* TODO: replace by the right type *)
      | _ -> (Format.eprintf "type: %a@\n" Model.pp_type_ t; assert false))

let is_local_invariant _m an t =
  let rec internal_is_local acc (term : M.mterm) =
    match term.M.node with
    | M.Mforall (_i,M.Tasset a,_,_b) -> not (compare (a |> unloc) an = 0)
    | M.Msum (a,_,_) -> not (compare a an = 0)
    | M.Mmax (a,_,_) -> not (compare a an = 0)
    | M.Mmin (a,_,_) -> not (compare a an = 0)
    | M.Mselect (a,_,_) -> not (compare a an = 0)
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
  | M.SonlyInAction _ -> true
  | M.SonlyByRoleInAction _ -> true
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
           Tcons (map_action_to_change a |> mk_change_term,Tnil);
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
               ) (mk_eq t2 (List.hd l1) true) (List.tl l2))])
  in
  let mk_changes_performed_by_2 t1 t2 a l1 l2 =
    Tapp (Tvar "Tr.performed_by",
          [tr;
           Tcons (map_action_to_change a |> mk_change_term,Tnil);
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
  | M.SonlyInAction (ADany,Sentry entries) ->
    mk_performed_by entry (entries |> List.map unloc |> List.map String.capitalize_ascii) true
  | M.SonlyByRole (a,roles)         ->
    mk_changes_performed_by caller a (roles |> List.map unloc) false
  | M.SonlyInAction (a,Sentry entries)     ->
    mk_changes_performed_by entry
      a
      (entries |> List.map unloc |> List.map String.capitalize_ascii)
      true
  | M.SonlyByRoleInAction (ADany,roles,Sentry entries) ->
    mk_performed_by_2 caller entry
      (roles |> List.map unloc)
      (entries |> List.map unloc |> List.map String.capitalize_ascii)
  | M.SonlyByRoleInAction (a,roles,Sentry entries) ->
    mk_changes_performed_by_2 caller entry a
      (roles |> List.map unloc)
      (entries |> List.map unloc |> List.map String.capitalize_ascii)
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

(* prefixes with 'forall k_:key, mem k_ "asset"_keys ->  ...'
   replaces asset field "field" by '"field " (get "asset"_assets k_)'
   TODO : make sure there is no collision between "k_" and invariant vars

   m is the Model
   n is the asset name
   inv is the invariant to extend
*)

(* f --> f a *)
let mk_app_field (a : ident) (f : loc_ident) : loc_term * loc_term  =
  let arg   : term     = Tvar a in
  let loc_f : loc_term = mk_loc f.loc (Tvar f) in
  (loc_f,with_dummy_loc (Tapp (loc_f,[loc_term arg])))

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
    loc_replace (with_dummy_loc Ttobereplaced) replacing (loc_term prefix)

let mk_storage_invariant m n (lbl : M.lident) (t : loc_term) = {
  id = map_lident lbl;
  form = mk_invariant m n `Storage t;
}

let mk_pre_list m n arg inv : loc_term = mk_invariant m (dumloc n) (`Prelist arg) inv

let mk_pre_coll m n arg inv : loc_term = mk_invariant m (dumloc n) (`Precoll arg) inv

let mk_pre_asset m n arg inv : loc_term = mk_invariant m (dumloc n) (`Preasset arg) inv

let mk_loop_invariant m n inv : loc_term = mk_invariant m (dumloc n) `Loop inv

let mk_axiom_invariant m n inv : loc_term = mk_invariant m (dumloc n) `Axiom inv

let mk_axiom2_invariant m n inv : loc_term = mk_invariant m (dumloc n) `Axiom2 inv

let mk_state_invariant _m _v (lbl : M.lident) (t : loc_term) = {
  id = map_lident lbl;
  form = Timpl (
      loc_term (Teq(Tyint,Tvar "state", Tvar (unloc _v))),
      t) |> with_dummy_loc
}

let mk_eq_asset _m (r : M.asset) =
  let cmps = List.map (fun (item : M.asset_item) ->
      let f1 = Tdoti("a1",unloc item.name) in
      let f2 = Tdoti("a2",unloc item.name) in
      match item.type_ with
      | Tcontainer _ -> Tapp (Tvar "eq_keyl",[f1;f2])
      | _ ->            Teq (Tyint,f1,f2)
    ) r.values in
  Dfun  {
    name = "eq_" ^ (unloc r.name) |> with_dummy_loc;
    logic = Logic;
    args = ["a1" |> with_dummy_loc, Tyasset (map_lident r.name) |> with_dummy_loc;
            "a2" |> with_dummy_loc, Tyasset (map_lident r.name) |> with_dummy_loc];
    returns = Tybool |> with_dummy_loc;
    raises = [];
    variants = [];
    requires = [];
    ensures = [];
    body = List.fold_left (fun acc cmp ->
        Tpand (acc,cmp)
      ) (List.hd cmps) (List.tl cmps) |> loc_term;
  }

let mk_eq_extensionality _m (r : M.asset) : loc_decl =
  let asset = unloc r.name in
  Dtheorem (Lemma,
            asset ^ "_extensionality",
            Tforall ([["a1";"a2"],Tyasset asset],
                     Timpl (Tapp (Tvar ("eq_" ^ asset),
                                  [Tvar "a1";Tvar "a2"]),
                            Teq (Tyasset asset,Tvar "a1",Tvar "a2")
                           ))) |> Mlwtree.loc_decl

let map_enum _m (e : M.enum) : (loc_term,loc_typ,loc_ident) abstract_decl =
  Denum (map_lident e.name, List.map (fun (item : M.enum_item) -> map_lident item.name) e.values)

let record_to_clone m (r : M.asset) =
  let (key,_) = M.Utils.get_asset_key m (unloc r.name) in
  (* let sort =
    if List.length r.sort > 0 then
      List.hd r.sort
    else key in *) (* asset are sorted on key be default *)
  Dclone ([gArchetypeDir;gArchetypeColl] |> wdl,
          String.capitalize_ascii (unloc r.name) |> with_dummy_loc,
          [Ctype ("t" |> with_dummy_loc, (unloc r.name) |> with_dummy_loc);
           Cval  ("keyf" |> with_dummy_loc, key |> with_dummy_loc);
           (* Cval  ("sortf" |> with_dummy_loc, sort |> with_dummy_loc); *)
           Cval  ("eqf" |> with_dummy_loc, "eq_" ^ (unloc r.name) |> with_dummy_loc)])

let mk_partition_axioms (m : M.model) =
  M.Utils.get_containers m |> List.map (fun (n,i,_) ->
      let kt     = M.Utils.get_asset_key m n |> snd |> map_btype in
      let pa,_,pkt  = M.Utils.get_container_asset_key m n i in
      mk_partition_axiom n i kt pa (pkt |> map_btype)
    ) |> loc_decl |> deloc

let rec get_record id = function
  | Drecord (n,_) as r :: _tl when compare id n = 0 -> r
  | _ :: tl -> get_record id tl
  | [] -> assert false

let get_record_name = function
  | Drecord (n,_) -> n
  | _ -> assert false

let mk_var (i : ident) = Tvar i

let get_for_fun = function
  | M.Tcontainer (Tasset a,_) -> (fun (t1,t2) -> loc_term (Tnth  (unloc a,t1,t2))),
                                 (fun t ->       loc_term (Tcard (unloc a,t)))
  | _ -> assert false


type logical_mod = Nomod | Added | Removed
type lctx = Inv | Other

type logical_context = {
  lctx : lctx;
  old  : bool;
  lmod : logical_mod;
  localold : ident list;
}

let init_ctx = {
  lctx = Other;
  old = false;
  lmod = Nomod;
  localold = [];
}

let mk_trace_seq m t chs =
  if M.Utils.with_trace m then
    Tseq ([with_dummy_loc t] @ (List.map mk_trace chs))
  else t

let is_old (ctx : logical_context) (t : M.mterm) =
  match t.node with
  | M.Mdotasset ({ node = M.Mvarlocal id;  type_ = _},_) -> List.mem (unloc id) ctx.localold
  | _ -> false

let map_mpattern (p : M.lident M.pattern_node) =
  match p with
  | M.Pwild -> Twild
  | M.Pconst i -> Tconst (map_lident i)

let mk_ac_ctx a ctx =
  match ctx.lctx with
  | Inv -> loc_term (Tvar (mk_ac_id a))
  | _ ->  loc_term (mk_ac a)

let rec map_mterm m ctx (mt : M.mterm) : loc_term =
  let error_internal desc = emit_error (mt.loc, desc); Tnottranslated in
  let error_not_translated (msg : string) = (* Tnottranslated in *) error_internal (TODONotTranslated msg) in
  let error_not_supported (msg : string) = error_internal (NotSupported msg) in
  let t =
    match mt.node with
    (* lambda *)

    | Mletin ([id], v, _, b, None) ->
      Tletin (M.Utils.is_local_assigned (unloc id) b, map_lident id, None, map_mterm m ctx v, map_mterm m ctx b)

    | Mletin ([id], { node = M.Mapifget (a, {node = M.Msetbefore _; _}, k); type_ = _ }, _, b, Some e) -> (* logical *)
      let ctx = { ctx with (*old = true;*) localold = ctx.localold @ [unloc id] } in
      Tletin (M.Utils.is_local_assigned (unloc id) b,
              map_lident id,
              None,
              Tget (loc_ident a,
                    loc_term (mk_ac_old a),
                    map_mterm m ctx k) |> with_dummy_loc,
              Tif (Tnot (Teq (Tyint,
                              Tvar (unloc id),
                              Twitness a)) |> loc_term,
                   map_mterm m ctx b,
                   Some (map_mterm m ctx e)) |> with_dummy_loc)

    | Mletin ([id], { node = M.Mapifget (a,_, k); type_ = _ }, _, b, Some e) -> (* logical *)
      Tletin (M.Utils.is_local_assigned (unloc id) b,
              map_lident id,
              None,
              Tget (loc_ident a,
                    loc_term (mk_ac a),
                    map_mterm m ctx k) |> with_dummy_loc,
              Tif (Tnot (Teq (Tyint,
                              Tvar (unloc id),
                              Twitness a)) |> loc_term,
                   map_mterm m ctx b,
                   Some (map_mterm m ctx e)) |> with_dummy_loc)

    | Mletin              _ -> error_not_translated "Mletin"
    | Mdeclvar            _ -> error_not_translated "Mdeclvar"

    | Mapp (f, args) ->
      Tapp (mk_loc (map_lident f).loc (Tvar (map_lident f)), List.map (map_mterm m ctx) args)


    (* assign *)

    | Massign (ValueAssign, _, id, v) ->
      Tassign (with_dummy_loc (Tvar (map_lident id)),map_mterm m ctx v)

    | Massign (MinusAssign, _, id, v) ->
      Tassign (with_dummy_loc (Tvar (map_lident id)),
               with_dummy_loc (
                 Tminus (with_dummy_loc Tyint,
                         with_dummy_loc (Tvar (map_lident id)),
                         map_mterm m ctx v)))

    | Massign             _ -> error_not_translated "Massign"

    | Massignvarstore (assignop, _, id, v) ->
      let var = with_dummy_loc (Tdoti (with_dummy_loc gs,map_lident id)) in
      let value =
        begin
          match assignop with
          | ValueAssign -> map_mterm m ctx v
          | MinusAssign ->
            with_dummy_loc (
              Tminus (with_dummy_loc Tyint,
                      var,
                      map_mterm m ctx v))
          | PlusAssign ->
            with_dummy_loc (
              Tplus (with_dummy_loc Tyint,
                     var,
                     map_mterm m ctx v))
          | MultAssign ->
            with_dummy_loc (
              Tmult (with_dummy_loc Tyint,
                     var,
                     map_mterm m ctx v))
          | DivAssign ->
            with_dummy_loc (
              Tdiv (with_dummy_loc Tyint,
                    var,
                    map_mterm m ctx v))
          | AndAssign ->
            with_dummy_loc (
              Tand (var,
                    map_mterm m ctx v))
          | OrAssign ->
            with_dummy_loc (
              Tor (var,
                   map_mterm m ctx v))
        end in
      Tassign (var,value)
    | Massignfield (assignop, _, id1, id2, v) ->

      let id = with_dummy_loc (Tdot (map_mterm m ctx id1,
                                     with_dummy_loc (Tvar (map_lident id2)))) in
      let value =
        begin
          match assignop with
          | ValueAssign -> map_mterm m ctx v
          | MinusAssign -> with_dummy_loc (
              Tminus (with_dummy_loc Tyint,
                      id,
                      map_mterm m ctx v))
          | PlusAssign -> with_dummy_loc (
              Tplus (with_dummy_loc Tyint,
                     id,
                     map_mterm m ctx v))
          | MultAssign -> with_dummy_loc (
              Tmult (with_dummy_loc Tyint,
                     id,
                     map_mterm m ctx v))
          | DivAssign -> with_dummy_loc (
              Tdiv (with_dummy_loc Tyint,
                    id,
                    map_mterm m ctx v))
          | AndAssign -> with_dummy_loc (
              Tand (  id,
                      map_mterm m ctx v))
          | OrAssign -> with_dummy_loc (
              Tor (  id,
                     map_mterm m ctx v))
        end in
      Tassign (id,value)

    | Massignstate v -> Tassign (loc_term (Tdoti (gs, "state")), map_mterm m ctx v)

    | Massignassetstate _ -> error_not_translated "Massignassetstate"


    (* control *)

    | Mif (c, t, Some { node=M.Mseq []; type_=_}) ->
      Tif (map_mterm m ctx c, map_mterm m ctx t, None)

    | Mif (c, t, e) ->
      Tif (map_mterm m ctx c, map_mterm m ctx t, Option.map (map_mterm m ctx) e)

    | Mmatchwith (t, l) ->
      Tmatch (map_mterm m ctx t, List.map (fun ((p : M.lident M.pattern_gen), e) ->
          (map_mpattern p.node, map_mterm m ctx e)
        ) l)

    | Mfor (id, c, b, lbl) ->
      let (nth, card) = get_for_fun c.type_ in
      Tfor (with_dummy_loc "i",
            with_dummy_loc (
              Tminus (with_dummy_loc Tyunit,card (map_mterm m ctx c |> unloc_term),
                      (loc_term (Tint Big_int.unit_big_int)))
            ),
            mk_invariants m ctx lbl b,
            with_dummy_loc (
              Tletin (false,
                      map_lident id,
                      None,
                      nth (Tvar "i", map_mterm m ctx c |> unloc_term),
                      map_mterm m ctx b)))

    | Miter               _ -> error_not_translated "Miter"

    | Mseq l -> Tseq (List.map (map_mterm m ctx) l)

    | Mreturn             _ -> error_not_translated "Mreturn"

    | Mlabel lbl ->
      begin
        match M.Utils.get_formula m None (unloc lbl) with
        | Some formula -> Tassert (Some (map_lident lbl),map_mterm m ctx formula)
        | _ -> assert false
      end


    (* effect *)

    | Mfail InvalidCaller        -> Traise Einvalidcaller
    | Mfail NoTransfer           -> Traise Enotransfer
    | Mfail (InvalidCondition _) -> Traise Einvalidcondition
    | Mfail InvalidState         -> Traise Einvalidstate
    | Mfail               _ -> error_not_translated "Mfail"
    | Mtransfer (a, t) -> Ttransfer(map_mterm m ctx a, map_mterm m ctx t)
    | Mexternal           _ -> error_not_translated "Mexternal"


    (* literals *)

    | Mint v -> Tint v
    | Muint v -> Tint v
    | Mbool false -> Tfalse
    | Mbool true -> Ttrue
    | Menum               _ -> error_not_supported "Menum"
    | Mrational           _ -> error_not_translated "Mrational"
    | Mstring v -> Tint (sha v)
    | Mcurrency (i, Tz)   -> Tint (Big_int.mult_int_big_int 1000000 i)
    | Mcurrency (i, Mtz)  -> Tint (Big_int.mult_int_big_int 1000 i)
    | Mcurrency (i, Utz)  -> Tint i
    | Maddress v -> Tint (sha v)
    | Mdate s -> Tint (Core.date_to_timestamp s)
    | Mduration v -> Tint (Core.duration_to_timestamp v)
    | Mtimestamp v -> Tint v
    | Mbytes v -> Tbytes v


    (* control expression *)

    | Mexprif (c, t, e) ->
      Tif (map_mterm m ctx c, map_mterm m ctx t, Some (map_mterm m ctx e))

    | Mexprmatchwith (t, l) ->
      Tmatch (map_mterm m ctx t, List.map (fun ((p : M.lident M.pattern_gen), e) ->
          (map_mpattern p.node, map_mterm m ctx e)
        ) l)


    (* composite type constructors *)

    | Mnone                 -> error_not_translated "Mnone"
    | Msome               _ -> error_not_translated "Msome"

    | Marray l ->
      begin
        match mt.type_ with
        | Tcontainer (_,_) -> Tlist (l |> List.map (map_mterm m ctx))
        | _ -> assert false
      end

    | Mtuple              l -> Ttuple (List.map (map_mterm m ctx) l)

    | Masset l ->
      let asset = M.Utils.get_asset_type mt in
      let fns = M.Utils.get_field_list m asset |> wdl in
      Trecord (None,(List.combine fns (List.map (map_mterm m ctx) l)))

    | Massoc              _ -> error_not_translated "Massoc"


    (* dot *)

    | Mdotasset (e, i) -> Tdot (map_mterm m ctx e, mk_loc (loc i) (Tvar (map_lident i)))
    | Mdotcontract        _ -> error_not_translated "Mdotcontract"


    (* comparison operators *)

    | Mequal (l, r) -> Teq (with_dummy_loc Tyint,map_mterm m ctx l,map_mterm m ctx r)
    | Mnequal (l, r) -> Tneq (with_dummy_loc Tyint,map_mterm m ctx l,map_mterm m ctx r)
    | Mgt (l, r) -> Tgt (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mge (l, r) -> Tge (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mlt (l, r) -> Tlt (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mle (l, r) -> Tle (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mmulticomp          _ -> error_not_translated "Mmulticomp"


    (* arithmetic operators *)

    | Mand (l, r) -> Tand (map_mterm m ctx l, map_mterm m ctx r)
    | Mor (a, b) -> Tor (map_mterm m ctx a, map_mterm m ctx b)
    | Mnot c -> Tnot (map_mterm m ctx c)
    | Mplus (l, r)  -> Tplus  (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mminus (l, r) -> Tminus (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mmult (l, r) -> Tmult (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mdiv (l, r) -> Tdiv (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mmodulo (l, r) -> Tmod (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Muplus _ -> error_not_translated "Muplus"
    | Muminus v -> Tuminus (with_dummy_loc Tyint, map_mterm m ctx v)


    (* asset api effect *)

    | Maddasset (n, i) ->
      mk_trace_seq m
        (Tapp (loc_term (Tvar ("add_" ^ n)),[map_mterm m ctx i ]))
        [CAdd n]

    | Maddfield (a, f, c, i) ->
      let t, _, _ = M.Utils.get_container_asset_key m a f in
      mk_trace_seq m
        (Tapp (loc_term (Tvar ("add_" ^ a ^ "_" ^ f)),
               [map_mterm m ctx c; map_mterm m ctx i]))
        [CUpdate f; CAdd t]

    | Mremoveasset (n, a) ->
      mk_trace_seq m
        (Tletin (false,
                 with_dummy_loc ("_rm" ^ n),
                 None,
                 with_dummy_loc (Tapp (loc_term (Tvar ("get_" ^ n)),
                                       [map_mterm m ctx a])),
                 with_dummy_loc (Tapp (loc_term (Tvar ("remove_" ^ n)),
                                       [loc_term (Tvar ("_rm" ^ n))]))))
        [CRm n]

    | Mremovefield (a, f, k, v) ->
      let t,_,_ = M.Utils.get_container_asset_key m a f in
      let asset =
        match v.node with
        | M.Mdotasset (a,_) -> map_mterm m ctx a
        | _ -> with_dummy_loc (Tapp (loc_term (Tvar ("get_" ^ t)),
                                     [map_mterm m ctx v]))
      in
      mk_trace_seq m
        (Tletin (false,
                 with_dummy_loc ("_rm" ^ t),
                 None,
                 asset,
                 with_dummy_loc (Tapp (loc_term (Tvar ("remove_" ^ a ^ "_" ^ f)),
                                       [
                                         map_mterm m ctx k;
                                         loc_term (Tvar ("_rm" ^ t))
                                       ]
                                      ))))
        [CUpdate f; CRm t]

    | Mclearasset         _ -> error_not_translated "Mclearasset"
    | Mclearfield         _ -> error_not_translated "Mclearfield"

    | Mset (a, l, k, v) ->
      let asset =
        match k.node with
        | M.Mdotasset (a, _) -> map_mterm m ctx a
        | _ -> with_dummy_loc (Tapp (loc_term (Tvar ("get_" ^ a)),
                                     [map_mterm m ctx k]))
      in
      mk_trace_seq m
        (Tletin (false,
                 with_dummy_loc ("_old" ^ a),
                 None,
                 asset,
                 with_dummy_loc (Tapp (loc_term (Tvar ("set_" ^ a)),
                                       [
                                         loc_term (Tvar ("_old" ^ a));
                                         map_mterm m ctx v
                                       ]))))
        (List.map (fun f -> CUpdate f) l)

    | Mupdate             _ -> error_not_translated "Mupdate"
    | Mremoveif           _ -> error_not_translated "Mremoveif"
    | Maddupdate          _ -> error_not_translated "Maddupdate"


    (* asset api expression *)

    | Mget (n, k) -> Tapp (loc_term (Tvar ("get_" ^ n)),[map_mterm m ctx k])

    | Mselect (a, l, r) ->
      let args = extract_args r in
      let id = mk_select_name m a r in
      let argids = args |> List.map (fun (e, _, _) -> e) |> List.map (map_mterm m ctx) in
      Tapp (loc_term (Tvar id), argids @ [map_mterm m ctx l])

    | Msort               (a,c,l) ->
      let id = (mk_sort_clone_id a l) ^ ".sort" in
      Tapp (loc_term (Tvar id),[map_mterm m ctx c])

    | Mcontains (a, _, r) -> Tapp (loc_term (Tvar ("contains_" ^ a)), [map_mterm m ctx r])

    | Mnth                (n,c,k) -> Tapp (loc_term (Tvar ("nth_" ^ n)),[map_mterm m ctx c; map_mterm m ctx k])
    | Mcount              (a,t) -> Tcard (with_dummy_loc a, map_mterm m ctx t)

    | Msum          (a,_,f) ->
      let id = mk_sum_name m a f in
      Tapp (loc_term (Tvar id), [mk_ac_ctx a ctx])
    | Mmin                _ -> error_not_translated "Mmin"
    | Mmax                _ -> error_not_translated "Mmax"
    | Mhead               _ -> error_not_translated "Mhead"
    | Mtail               _ -> error_not_translated "Mtail"


    (* utils *)

    | Mgetfrommap         _ -> error_not_translated "Mgetfrommap"


    (* list api effect *)

    | Mlistprepend        _ -> error_not_translated "Mlistprepend"


    (* list api expression *)

    | Mlistcontains       _ -> error_not_translated "Mlistcontains"
    | Mlistcount          _ -> error_not_translated "Mlistcount"
    | Mlistnth            _ -> error_not_translated "Mlistnth"


    (* builtin functions *)

    | Mfunmax             _ -> error_not_translated "Mfunmax"
    | Mfunmin             _ -> error_not_translated "Mfunmin"
    | Mfunabs             _ -> error_not_translated "Mfunabs"


    (* constants *)

    | Mvarstate ->
      begin
        match ctx.lctx with
        | Inv -> loc_term (Tvar "state") |> Mlwtree.deloc
        | _ -> loc_term (Tdoti (gs, "state")) |> Mlwtree.deloc
      end

    | Mnow -> Tnow (with_dummy_loc gs)
    | Mtransferred -> Ttransferred (with_dummy_loc gs)
    | Mcaller -> Tcaller (with_dummy_loc gs)

    | Mbalance ->
      begin
        match ctx.lctx with
        | Inv -> loc_term (Tvar "_balance") |> Mlwtree.deloc
        | _ -> loc_term (Tdoti (gs, "_balance")) |> Mlwtree.deloc
      end

    | Msource               -> error_not_translated "Msource"


    (* variables *)

    | Mvarassetstate _ -> error_not_translated "Mvarassetstate"

    | Mvarstorevar v ->
      begin
        match ctx.lctx with
        | Inv -> Tvar (map_lident v)
        | _ -> Tdoti (with_dummy_loc gs, map_lident v)
      end

    | Mvarstorecol n ->
      let coll =
        match ctx.old, ctx.lmod with
        | false, Nomod   -> mk_ac (n |> unloc)
        | false, Added   -> mk_ac_added (n |> unloc)
        | false, Removed -> mk_ac_rmed (n |> unloc)
        | true, Nomod    -> mk_ac_old (n |> unloc)
        | true, Added    -> mk_ac_old_added (n |> unloc)
        | true, Removed  -> mk_ac_old_rmed (n |> unloc)
      in
      loc_term coll |> Mlwtree.deloc

    | Mvarenumval v -> Tvar (map_lident v)
    | Mvarlocal v -> Tvar (map_lident v)
    | Mvarparam v -> Tvar (map_lident v)
    | Mvarfield           _ -> error_not_translated "Mvarfield"
    | Mvarthe               -> error_not_translated "Mvarthe"


    (* rational *)

    | Mdivrat             _ -> error_not_translated "Mdivrat"
    | Mrateq              _ -> error_not_translated "Mrateq"
    | Mratcmp             _ -> error_not_translated "Mratcmp"
    | Mratarith           _ -> error_not_translated "Mratarith"
    | Mrattez             (r,t) -> Tapp (loc_term (Tvar "rat_tez"),[map_mterm m ctx r; map_mterm m ctx t])
    | Minttorat           _ -> error_not_translated "Minttorat"


    (* functional *)

    | Mfold               _ -> error_not_translated "Mfold"


    (* imperative *)

    | Mbreak                -> error_not_translated "break;"


    (* shallowing *)

    | Mshallow (a, e) -> Tapp (loc_term (Tvar ("shallow_" ^ a)), [map_mterm m ctx e])

    | Munshallow (a, e) ->
      let ctx =
        if is_old ctx e then
          { ctx with old = true }
        else ctx in
      Tapp (loc_term (Tvar ("unshallow_" ^ a)),
            [map_mterm m ctx (M.mk_mterm (Mvarstorecol (dumloc a))
                                (Tcontainer (Tasset (dumloc a),Collection)));
             map_mterm m ctx e])

    | Mlisttocoll (n, l) -> Tapp (loc_term (Tvar ("listtocoll_" ^ n)), [map_mterm m ctx l])

    | Maddshallow (n, l) ->
      let pa = M.Utils.get_container_assets m n |> List.map (fun a ->
          CAdd (String.capitalize_ascii a)) in
      mk_trace_seq m
        (Tapp (loc_term (Tvar ("add_shallow_" ^ n)),List.map (map_mterm m ctx) l))
        ([CAdd n] @ pa)


    (* collection keys *)

    | Mtokeys             _ -> error_not_translated "Mtokeys"
    | Mcoltokeys          _ -> error_not_translated "Mcoltokeys"


    (* quantifiers *)

    | Mforall (i, t, None, b) ->
      let asset = M.Utils.get_asset_type (M.mk_mterm (M.Mbool false) t) in
      Tforall (
        [[i |> map_lident],loc_type (Tyasset asset)],
        map_mterm m ctx b)

    | Mforall (i, t, Some coll, b) ->
      let asset = M.Utils.get_asset_type (M.mk_mterm (M.Mbool false) t) in
      Tforall (
        [[i |> map_lident],loc_type (Tyasset asset)],
        with_dummy_loc (Timpl (with_dummy_loc (Tmem (with_dummy_loc asset,
                                                     loc_term (Tvar (unloc i)),
                                                     map_mterm m ctx coll)),
                               map_mterm m ctx b)))

    | Mexists             _ -> error_not_translated "Mexists"


    (* formula operators *)

    | Mimply (a, b) -> Timpl (map_mterm m ctx a, map_mterm m ctx b)
    | Mequiv              _ -> error_not_translated "Mequiv"


    (* formula asset collection *)

    | Msetbefore c -> map_mterm m { ctx with old = true } c |> Mlwtree.deloc
    | Msetat              _ -> error_not_translated "Msetat"
    | Msetunmoved         _ -> error_not_translated "Msetunmoved"
    | Msetadded c ->  map_mterm m { ctx with lmod = Added } c |> Mlwtree.deloc
    | Msetremoved c -> map_mterm m { ctx with lmod = Removed } c |> Mlwtree.deloc
    | Msetiterated        _ -> error_not_translated "Msetiterated"
    | Msettoiterate c ->
      let n = M.Utils.get_asset_type mt |> with_dummy_loc in
      Ttoiter (n, with_dummy_loc "i", map_mterm m ctx c) (* TODO : should retrieve actual idx value *)


    (* formula asset collection methods *)

    | Mapifget (a, _c, k) -> Tapp (loc_term (Tvar ("get_" ^ a)),[map_mterm m ctx k])
    | Mapifsubsetof (n, l, r) -> Tsubset (with_dummy_loc n, map_mterm m ctx l, map_mterm m ctx r)
    | Mapifisempty (l, r) -> Tempty (with_dummy_loc l, map_mterm m ctx r)
    | Mapifselect (a, l, r) ->  let args = extract_args r in
      let id = mk_select_name m a r in
      let argids = args |> List.map (fun (e, _, _) -> e) |> List.map (map_mterm m ctx) in
      Tapp (loc_term (Tvar id), argids @ [map_mterm m ctx l])
    | Mapifsort      _ -> error_not_translated "Mapifsort"
    | Mapifcontains  (a, _, r) -> Tapp (loc_term (Tvar ("contains_" ^ a)), [map_mterm m ctx r])
    | Mapifnth       _ -> error_not_translated "Mapifnth"
    | Mapifcount     (a,t) ->  Tcard (with_dummy_loc a, map_mterm m ctx t)
    | Mapifsum       (a,_,f) ->
      let id = mk_sum_name m a f in
      Tapp (loc_term (Tvar id), [mk_ac_ctx a ctx])
    | Mapifmin       _ -> error_not_translated "Mapifmin"
    | Mapifmax       _ -> error_not_translated "Mapifmax"
    | Mapifhead      _ -> error_not_translated "Mapifhead"
    | Mapiftail      _ -> error_not_translated "Mapiftail"


  in
  mk_loc mt.loc t
and mk_invariants (m : M.model) ctx (lbl : ident option) lbody =
  let loop_invariants =
    Option.fold (M.Utils.get_loop_invariants m) [] lbl |>
    List.map (fun ((ilbl : ident),(i : M.mterm)) ->
        let iid =
          match lbl,ilbl with
          | Some a, b -> b ^ "_" ^ a
          | None, b -> b in
        { id =  with_dummy_loc iid; form = map_mterm m ctx i }
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
          acc @ [{ id = with_dummy_loc iid;
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
        { id = with_dummy_loc id;
          form = map_security_pred `Loop sec.predicate |> loc_term; }
      )
  in
  loop_invariants @ storage_loop_invariants @ security_loop_invariants

(* Storage mapping -----------------------------------------------------------*)

let map_record_values m (values : M.asset_item list) =
  List.map (fun (value : M.asset_item) ->
      let typ_ = map_mtype value.type_ in
      let init_value = type_to_init typ_ in {
        name     = map_lident value.name;
        typ      = typ_;
        init     = Option.fold (fun _ -> map_mterm m init_ctx) init_value value.default;
        mutable_ = false;
      }
    ) values

let map_record m (r : M.asset) =
  Drecord (map_lident r.name, map_record_values m r.values)

let map_storage_items m = List.fold_left (fun acc (item : M.storage_item) ->
    acc @
    match item.typ with
    | M.Tcontainer (Tasset id, Collection) ->
      let id = unloc id in [
        mk_collection_field id mk_ac_id;
        mk_collection_field id mk_ac_added_id;
        mk_collection_field id mk_ac_rmed_id
      ]
    | _ ->
      let typ_ = map_mtype item.typ in
      (* let init_value = type_to_init typ_ in *)
      [{
        name     = unloc item.id |> with_dummy_loc;
        typ      = typ_;
        init     = map_mterm m init_ctx item.default;
        mutable_ = true;
      }]
  ) []

let map_storage m (l : M.storage) =
  let ctx = { init_ctx with lctx = Inv } in
  Dstorage {
    fields     = (map_storage_items m l) @ (mk_const_fields m |> loc_field |> deloc);
    invariants = (** collect all invariants : *)
      (* -- asset invariants -- *)
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
          List.map (fun (inv : M.label_term) -> mk_storage_invariant m storage_id inv.label (map_mterm m ctx inv.term)) invs) l) @
      (* -- security predicates -- *)
      (List.fold_left (fun acc sec ->
           acc @ (mk_spec_invariant `Storage sec)) [] m.security.items) @
      (List.fold_left (fun acc decl ->
           match decl with
           | M.Denum e ->
             List.fold_left (fun acc (value : M.enum_item) ->
                 acc @ List.map (fun (inv : M.label_term) ->
                     mk_state_invariant m value.name inv.label (map_mterm m ctx inv.term)
                   ) value.invariants
               ) acc e.values
           | _ -> acc
         ) [] m.decls) @
      (* -- contract invariants -- *)
      (List.fold_left (fun acc (post : M.postcondition) ->
           acc @ [{
               id = map_lident post.name;
               form = map_mterm m ctx post.formula;
             }]
         ) [] m.specification.postconditions) @
      (* -- variable invariants -- *)
      (List.fold_left (fun acc decl ->
           match decl with
           | M.Dvar var ->
             acc @ (List.map (fun (inv : M.label_term) ->
                 { id = map_lident inv.label; form = map_mterm m ctx inv.term}
               ) var.invariants)
           | _ -> acc
         ) [] m.decls)
  }

(* Verfication API -----------------------------------------------------------*)

let mk_axioms (m : M.model) : (loc_term, loc_typ, loc_ident) abstract_decl list =
  List.fold_left (fun acc apiv ->
      match apiv with
      | M.StorageInvariant (id,asset,formula) ->
        acc @ [
          Dtheorem (Axiom,
                    with_dummy_loc (asset ^ "_" ^ id ^ "_axiom"),
                    mk_axiom_invariant m asset (map_mterm m init_ctx formula));
          Dtheorem (Lemma,
                    with_dummy_loc (asset ^ "_" ^ id ^ "_axiom_2"),
                    mk_axiom2_invariant m asset (map_mterm m init_ctx formula))]

    ) [] m.api_verif
(*let records = M.Utils.get_assets m |> List.map (fun (r : M.info_asset) -> dumloc (r.name)) in
  let keys    = records |> List.map (M.Utils.get_asset_key m) in
  List.map2 (fun r (k,kt) ->
    mk_keys_eq_axiom r.pldesc k (map_btype kt)
  ) records keys |> loc_decl |> deloc*)


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
  Tcontains (asset,
             var,
             match old with
             | `Old -> mk_ac_old asset
             | `Curr -> mk_ac asset)

let mk_not_found_cond old asset var = Tnot (mk_key_found_cond old asset var)

(* formula is in mlw tree *)
let mk_get_sum_value_from_pos asset id formula =
  Dfun {
    name = mk_get_sum_value_from_pos_id asset id;
    logic = Logic;
    args = ["c",Tycoll asset; "i",Tyint];
    returns = Tyint;
    raises = [];
    variants = [];
    requires = [];
    ensures = [];
    body =
      let rec mk_body = function
        | Tdot (Tvar v,f) when compare v "the" = 0 ->
          Tapp (f,
                [
                  Tnth (asset,
                        Tvar "i",
                        Tvar "c")
                ])
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


let mk_get_asset asset key ktyp = Dfun {
    name = "get_" ^ asset;
    logic = NoMod;
    args = ["k",ktyp];
    returns = Tyasset asset;
    raises = [ Timpl (Texn Enotfound,
                      mk_not_found_cond `Old asset (Tvar "k"))];
    variants = [];
    requires = [];
    ensures = [
      { id   = "get_" ^ asset ^ "_post_1";
        form = Tmem (asset,
                     Tresult,
                     mk_ac asset);
      };
      { id   = "get_" ^ asset ^ "_post_2";
        form = Teq(Tyint,
                   Tdot (Tresult,
                         Tvar key),
                   Tvar "k")
      }
    ];
    body = Tif (mk_not_found_cond `Curr asset (Tvar "k"),
                Traise Enotfound,
                Some (Tget (asset,
                            mk_ac asset,
                            Tvar "k")))
  }

let mk_nth_asset asset key ktyp = Dfun {
    name = "nth_" ^ asset;
    logic = NoMod;
    args = ["c",Tycoll asset;"k",ktyp];
    returns = Tyasset asset;
    raises = [ Timpl (Texn Enotfound,
                      mk_not_found_cond `Old asset (Tvar "k"))];
    variants = [];
    requires = [];
    ensures = [
      { id   = "nth_" ^ asset ^ "_post_1";
        form = Tmem (asset,
                     Tresult,
                     mk_ac asset);
      };
      { id   = "nth_" ^ asset ^ "_post_2";
        form = Teq(Tyint,
                   Tdot (Tresult,
                         Tvar key),
                   Tvar "k")
      }
    ];
    body = Tif (mk_not_found_cond `Curr asset (Tvar "k"),
                Traise Enotfound,
                Some (Tnth (asset,
                            Tvar "k",
                            Tvar "c")))
  }

let mk_set_sum_ensures m a =
  List.fold_left (fun acc idx ->
      acc @ [{
          id = "set_" ^ a ^ "_sum_post";
          form = Teq (Tyint,
                      Tapp (Tvar (mk_sum_name_from_id a idx),
                            [mk_ac_old a]),
                      Tplus (Tyint,
                             Tminus (Tyint,
                                     Tapp (Tvar (mk_sum_name_from_id a idx),
                                           [mk_ac a]),
                                     Tapp(Tvar (mk_get_sum_value_id a idx),
                                          [Tvar "new_asset"])),
                             Tapp(
                               Tvar (mk_get_sum_value_id a idx),
                               [Tvar "old_asset"])))
        }]) [] (M.Utils.get_sum_idxs m a)

let mk_set_count_ensures m a =
  if M.Utils.with_count m a then [{
      id = "set_" ^ a ^ "_count";
      form = Teq (Tyint,
                  Tcard (a,mk_ac a),
                  Tcard (a,mk_ac_old a)
                 );
    }]
  else []

let mk_set_ensures m n key fields =
  snd (List.fold_left (fun (i,acc) (f:field) ->
      if compare f.name key = 0 then
        (i,acc)
      else
        (succ i,acc@[{
             id   = "set_" ^ n ^ "_post" ^ (string_of_int i);
             form = Teq (Tyint,
                         Tapp (Tvar f.name,
                               [Tget (n,
                                      mk_ac n,
                                      Tdoti ("old_asset",
                                             key))]),
                         Tapp (Tvar f.name,
                               [Tvar ("new_asset")]))
           }])
    ) (1,[]) fields) @ (mk_set_sum_ensures m n) @ (mk_set_count_ensures m n)

let mk_set_asset_precond m apid a id = mk_api_precond m apid a (`Preasset id)

let mk_set_asset m key = function
  | Drecord (asset, fields) ->  Dfun {
      name = "set_" ^ asset;
      logic = NoMod;
      args = ["old_asset", Tyasset asset; "new_asset", Tyasset asset];
      returns = Tyunit;
      raises = [ Timpl (Texn Enotfound,
                        mk_not_found_cond `Old asset (Tdoti ("old_asset",key)))];
      variants = [];
      requires = mk_set_asset_precond m ("set_" ^ asset) asset "new_asset";
      ensures = mk_set_ensures m asset key fields;
      body = Tif (mk_not_found_cond `Curr asset (Tdoti ("old_asset",key)),
                  Traise Enotfound,
                  Some (
                    Tassign (mk_ac asset,
                             Tset (asset,
                                   mk_ac asset,
                                   Tdoti ("old_asset",key),
                                   Tvar ("new_asset")))
                  ))
    }
  | _ -> assert false

let mk_contains asset keyt = Dfun {
    name     = "contains_" ^ asset;
    logic    = NoMod;
    args     = ["k",keyt];
    returns  = Tybool;
    raises   = [];
    variants = [];
    requires = [];
    ensures  = [
      { id   = asset ^ "_contains_1";
        form = Teq(Tyint, Tvar "result", Tcontains (asset,
                                                    Tvar "k",
                                                    mk_ac asset))
      }];
    body     = Tcontains (asset,
                          Tvar "k",
                          mk_ac asset)
  }

let mk_unshallow asset keyt = Dfun {
    name     = "unshallow_" ^ asset;
    logic    = Logic;
    args     = ["c", Tycoll asset;"l",Tylist keyt];
    returns  = Tycoll asset;
    raises   = [];
    variants = [];
    requires = [];
    ensures  = [
      { id   = asset ^ "_unshallow_post_1";
        form = Tsubset (asset,
                        Tresult,
                        Tvar "c")
      }];
    body     = Tunshallow (asset,
                           Tvar "c",
                           Tvar "l")
  }

let mk_add_asset_precond m apid a id = mk_api_precond m apid a (`Preasset id)

let mk_listtocoll_precond m apid a id = mk_api_precond m apid a (`Prelist id)

let mk_listtocoll m asset = Dfun {
    name     = "listtocoll_" ^ asset;
    logic    = Logic;
    args     = ["l", Tylist (Tyasset asset)];
    returns  = Tycoll asset;
    raises   = [];
    variants = [];
    requires = mk_listtocoll_precond m ("listtocoll_" ^ asset) asset "l";
    ensures  = [
      (*      { id   = asset ^ "_unshallow_post_1";
              form = Tsubset (asset,
                              Tresult,
                              Tvar "c")
              }*)];
    body     = Tcoll (asset,
                      Tvar "l")
  }

(* basic getters *)

let gen_field_getter n _field = Dfun {
    name     = n;
    logic    = Logic;
    args     = [];
    returns  = Tyunit;
    raises   = [];
    variants = [];
    requires = [];
    ensures  = [];
    body     = Tnone;
  }

let gen_field_getters = function
  | Drecord (n,fs) ->
    List.map (gen_field_getter n) fs
  | _ -> assert false

let mk_add_sum_ensures m a e =
  List.fold_left (fun acc idx ->
      acc @ [{
          id = "add_" ^ a ^ "_sum_post";
          form = Teq (Tyint,
                      Tapp (Tvar (mk_sum_name_from_id a idx),
                            [mk_ac a]),
                      Tplus (Tyint,
                             Tapp (Tvar (mk_sum_name_from_id a idx),
                                   [mk_ac_old a]),
                             Tapp(
                               Tvar (mk_get_sum_value_id a idx),
                               [Tvar e])))
        }]) [] (M.Utils.get_sum_idxs m a)

let mk_add_count_ensures m a =
  if M.Utils.with_count m a then [{
      id = "add_" ^ a ^ "_count";
      form = Teq (Tyint,
                  Tcard (a, mk_ac a),
                  Tplus (Tyint,
                         Tcard (a, mk_ac_old a),
                         Tint (Big_int.big_int_of_int 1)
                        )
                 )
    }]
  else []

let mk_add_ensures m p a e =
  [
    { id   = p ^ "_post_1";
      form = Tmem (a,
                   Tvar e,
                   mk_ac a)
    };
    { id   = p ^ "_post_2";
      form = Teq (Tycoll a,
                  mk_ac a,
                  Tunion (a,
                          mk_ac_old a,
                          Tsingl (a,
                                  Tvar e)));
    };
    { id   = p ^ "_post_3";
      form = Teq (Tycoll a,
                  mk_ac_added a,
                  Tunion (a,
                          mk_ac_old_added a,
                          Tsingl (a,
                                  Tvar e)));
    };
    { id   = p ^ "_post_4";
      form = Tempty (a,
                     Tinter (a,
                             mk_ac_old_added a,
                             Tsingl (a,
                                     Tvar e)
                            ));
    };
  ] @ (mk_add_sum_ensures m a e) @ (mk_add_count_ensures m a)

let mk_add_asset m asset key : decl = Dfun {
    name     = "add_" ^ asset;
    logic    = NoMod;
    args     = ["new_asset", Tyasset asset];
    returns  = Tyunit;
    raises   = [ Timpl (Texn Ekeyexist,
                        mk_not_found_cond `Old asset (Tdoti ("new_asset", key)))];
    variants = [];
    requires = mk_add_asset_precond m ("add_" ^ asset) asset "new_asset";
    ensures  = mk_add_ensures m ("add_" ^ asset) asset "new_asset";
    body     = Tseq [
        Tif (mk_not_found_cond `Curr asset (Tdoti ("new_asset", key)),
             Traise Ekeyexist, (* then *)
             Some (Tseq [      (* else *)
                 Tassign (mk_ac asset,
                          Tadd (asset,
                                mk_ac asset,
                                Tvar "new_asset"));
                 Tassign (mk_ac_added asset,
                          Tadd (asset,
                                mk_ac_added asset,
                                Tvar "new_asset"))
               ]
               ))
      ];
  }

let mk_rm_sum_ensures m a e =
  List.fold_left (fun acc idx ->
      acc @ [{
          id = "remove_" ^ a ^ "_sum_post";
          form = Teq (Tyint,
                      Tapp (Tvar (mk_sum_name_from_id a idx),
                            [mk_ac a]),
                      Tminus (Tyint,
                              Tapp (Tvar (mk_sum_name_from_id a idx),
                                    [mk_ac_old a]),
                              Tapp(
                                Tvar (mk_get_sum_value_id a idx),
                                [Tvar e])))
        }]) [] (M.Utils.get_sum_idxs m a)

let mk_rm_count_ensures m a =
  if M.Utils.with_count m a then [{
      id = "rm_" ^ a ^ "_count";
      form = Teq (Tyint,
                  Tcard (a, mk_ac a),
                  Tminus (Tyint,
                          Tcard (a, mk_ac_old a),
                          Tint (Big_int.big_int_of_int 1)
                         )
                 )
    }]
  else []

let mk_rm_ensures m p a e =
  [
    { id   = p ^ "_post1";
      form = Tnot (Tmem (a,
                         Tvar (e),
                         mk_ac a))
    };
    { id   = p ^ "_post2";
      form = Teq (Tycoll a,
                  mk_ac a,
                  Tdiff (a,
                         mk_ac_old a,
                         Tsingl (a,
                                 Tvar e)))
    };
    { id   = p ^ "_post3";
      form = Teq (Tycoll a,
                  mk_ac_rmed a,
                  Tunion (a,
                          mk_ac_old_rmed a,
                          Tsingl (a,
                                  Tvar e)))
    }
  ] @ (mk_rm_sum_ensures m a e) @ (mk_rm_count_ensures m a)

let mk_rm_asset m asset key : decl =
  Dfun {
    name     = "remove_" ^ asset;
    logic    = NoMod;
    args     = ["a", Tyasset asset];
    returns  = Tyunit;
    raises   = [Timpl (Texn Enotfound,
                       mk_not_found_cond `Old asset (Tdoti ("a",key)))];
    variants = [];
    requires = [];
    ensures  = mk_rm_ensures m ("remove_" ^ asset) asset "a";
    body = Tif (mk_not_found_cond `Curr asset (Tdoti ("a",key)),
                Traise Enotfound,
                Some (
                  Tseq [
                    Tassign (mk_ac_rmed asset,
                             Tadd (asset,
                                   mk_ac_rmed asset,
                                   Tget(asset,
                                        mk_ac asset,
                                        Tdoti ("a",
                                               key))));
                    Tassign (mk_ac asset,
                             Tremove (asset,
                                      mk_ac asset,
                                      Tdoti ("a",
                                             key)))

                  ]));
  }

(* a      : asset name
   ak     : asset key field name
   pf      : partition field name
   adda    : added asset name
   addktyp : removed asset key type
*)
let mk_add_partition_field m a ak pf adda addak : decl =
  let addak_cond = mk_key_found_cond `Old adda (Tdoti ("new_asset", addak)) in
  let akey  = Tapp (Tvar ak,[Tvar "asset"]) in
  let addak = Tapp (Tvar addak,[Tvar "new_asset"]) in
  Dfun {
    name     = "add_" ^ a ^ "_" ^ pf;
    logic    = NoMod;
    args     = ["asset",Tyasset a; "new_asset",Tyasset adda];
    returns  = Tyunit;
    raises   = [Timpl (Texn Enotfound,
                       mk_not_found_cond `Old a (Tdoti ("asset",ak)));
                Timpl (Texn Ekeyexist,
                       addak_cond)];
    variants = [];
    requires = mk_add_asset_precond m ("add_" ^ a ^ "_" ^ pf) adda "new_asset";
    ensures  = mk_add_ensures m ("add_" ^ a ^ "_" ^ pf) adda "new_asset";
    body     =
      Tif (mk_not_found_cond `Curr a (Tdoti ("asset",ak)),
           Traise Enotfound,
           Some (Tseq [
               Tapp (Tvar ("add_" ^ adda),
                     [Tvar "new_asset"]);
               Tletin (false, a ^ "_" ^ pf,None,
                       Tapp (Tvar pf,[Tvar "asset"]),
                       Tletin (false,"new_" ^ a ^ "_" ^ pf,None,
                               Tcons (addak,Tvar (a ^ "_" ^ pf)),
                               Tletin (false,"new_asset",None,
                                       Trecord (Some (Tvar "asset"),
                                                [pf,Tvar ("new_" ^ a ^ "_" ^ pf)]),
                                       Tassign (mk_ac a,
                                                Tset (a,
                                                      mk_ac a,
                                                      akey,
                                                      Tvar ("new_asset")))
                                      )))]));
  }

(* n      : asset name
   ktyp   : asset key type
   f      : partition field name
   rmn    : removed asset name
   rmktyp : removed asset key type
*)
let mk_rm_partition_field m asset keyf f rmed_asset rmkey : decl = Dfun {
    name     = "remove_" ^ asset ^ "_" ^ f;
    logic    = NoMod;
    args     = ["asset",Tyasset asset; "rm_asset",Tyasset rmed_asset];
    returns  = Tyunit;
    raises   = [Timpl (Texn Enotfound,
                       mk_not_found_cond `Old asset (Tdoti ("asset",keyf))) ];
    variants = [];
    requires = [];
    ensures  = mk_rm_ensures m ("remove_" ^ asset ^ "_" ^ f) rmed_asset "rm_asset";
    body     =
      Tif (mk_not_found_cond `Curr asset (Tdoti ("asset",keyf)),
           Traise Enotfound,
           Some (
             Tletin (false,
                     asset ^ "_" ^ f,
                     None,
                     Tapp (Tvar f,
                           [Tvar ("asset")]),
                     Tletin (false,
                             "new_" ^ asset ^ "_" ^ f,
                             None,
                             Tlistremove (gArchetypeList,
                                          Tdoti ("rm_asset",
                                                 rmkey),
                                          Tvar (asset ^ "_" ^ f)),
                             Tletin (false,
                                     "new_" ^ asset ^ "_asset",
                                     None,
                                     Trecord (Some (Tvar ("asset")),
                                              [f,Tvar ("new_" ^ asset ^ "_" ^ f)]),
                                     Tseq [
                                       Tassign (mk_ac asset,
                                                Tset (asset,
                                                      mk_ac asset,
                                                      Tdoti ("asset",
                                                             keyf),
                                                      Tvar ("new_" ^ asset ^ "_asset")));
                                       Tapp (Tvar ("remove_" ^ rmed_asset),
                                             [Tvar ("rm_asset")])
                                     ]
                                    )))));
  }

let mk_storage_api_before_storage (m : M.model) _records =
  m.api_items |> List.fold_left (fun acc (sc : M.api_storage) ->
      match sc.node_item with
      | M.APIAsset (Sum (asset,_,formula)) when compare asset "todo" <> 0 ->
        let key      = M.Utils.get_asset_key m asset |> fst in
        let mlw_formula = map_mterm m init_ctx formula |> unloc_term in
        let id = M.Utils.get_sum_idx m asset formula in
        acc @ [ mk_get_sum_value_from_pos asset id mlw_formula;
                mk_get_sum_value asset id mlw_formula;
                mk_sum_clone m asset key formula ]
      | _ -> acc
    ) [] |> loc_decl |> deloc

let mk_rat_tez _m = Dfun {
    name     = "rat_tez";
    logic    = NoMod;
    args     = ["c", Tytuple [Tyint;Tyint]; "t", Tytez];
    returns  = Tytez;
    raises   = [Texn ENotAPair];
    variants = [];
    requires = [];
    ensures  = [];
    body = Tif (Tlt (Tyint,
                  Tmult(Tyint,
                        Tfst (Tvar "c"),
                        Tsnd (Tvar "c")),
                  Tint (Big_int.zero_big_int)
                ),
                Traise ENotAPair,
                Some (
                  Tdiv (Tyint,
                    Tmult (Tyint,
                      Tabs(Tfst (Tvar "c")),
                      Tvar "t"
                    ),
                    Tabs (Tsnd (Tvar "c"))
                  )
                ));
  }


let mk_storage_api (m : M.model) records =
  m.api_items |> List.fold_left (fun acc (sc : M.api_storage) ->
      match sc.node_item with
      | M.APIAsset (Get n) ->
        let k,kt = M.Utils.get_asset_key m n in
        acc @ [mk_get_asset n k (kt |> map_btype)]
       | M.APIAsset (Nth n) ->
        let k,kt = M.Utils.get_asset_key m n in
        acc @ [mk_nth_asset n k (kt |> map_btype)]
      | M.APIAsset (Add n) ->
        let k = M.Utils.get_asset_key m n |> fst in
        acc @ [mk_add_asset m n k]
      | M.APIAsset (Remove n) ->
        let kt = M.Utils.get_asset_key m n |> fst in
        acc @ [mk_rm_asset m n kt]
      | M.APIAsset (Set n) ->
        let record = get_record n (records |> unloc_decl) in
        let k      = M.Utils.get_asset_key m (get_record_name record) |> fst in
        acc @ [mk_set_asset m k record]
      | M.APIAsset (UpdateAdd (a,pf)) ->
        let k            = M.Utils.get_asset_key m a |> fst in
        let (pa,addak,_) = M.Utils.get_container_asset_key m a pf in
        acc @ [
          (*mk_add_asset           pa.pldesc addak.pldesc;*)
          mk_add_partition_field m a k pf pa addak
        ]
      | M.APIAsset (UpdateRemove (n,f)) ->
        let t         = M.Utils.get_asset_key m n |> fst in
        let (pa,pk,_) = M.Utils.get_container_asset_key m n f in
        acc @ [
          (*mk_rm_asset           pa.pldesc (pt |> map_btype);*)
          mk_rm_partition_field m n t f pa pk
        ]
      | M.APIAsset (Contains n) ->
        let t         =  M.Utils.get_asset_key m n |> snd |> map_btype in
        acc @ [ mk_contains n t ]
      | M.APIAsset (Select (asset,test)) ->
        let mlw_test = map_mterm m init_ctx test in
        acc @ [ mk_select m asset test (mlw_test |> unloc_term) sc.only_formula ]
      | M.APIAsset (Sort (asset,field)) ->
        acc @ [ mk_cmp_function m asset field; mk_sort_clone m asset field]
      (* TODO *)
      | M.APIAsset (Unshallow n) ->
        let t         =  M.Utils.get_asset_key m n |> snd |> map_btype in
        acc @ [ mk_unshallow n t ]
      | M.APIAsset (Listtocoll n) ->
        acc @ [ mk_listtocoll m n ]
      | M.APIInternal (RatTez) ->
        acc @ [mk_rat_tez m]
      | _ -> acc
    ) [] |> loc_decl |> deloc

(* Entries --------------------------------------------------------------------*)

let fold_exns body : term list =
  let rec internal_fold_exn acc (term : M.mterm) =
    match term.M.node with
    | M.Mget _ -> acc @ [Texn Enotfound]
    | M.Mnth _ -> acc @ [Texn Enotfound]
    | M.Maddasset _ -> acc @ [Texn Ekeyexist]
    | M.Maddfield _ -> acc @ [Texn Enotfound; Texn Ekeyexist]
    | M.Mfail InvalidCaller -> acc @ [Texn Einvalidcaller]
    | M.Mfail NoTransfer -> acc @ [Texn Enotransfer]
    | M.Mfail (InvalidCondition _) -> acc @ [Texn Einvalidcondition]
    | M.Mfail InvalidState -> acc @ [Texn Einvalidstate]
    | M.Mremoveasset _ -> acc @ [Texn Enotfound]
    | M.Mrattez _ -> acc @ [Texn ENotAPair]
    | _ -> M.fold_term internal_fold_exn acc term in
  Tools.List.dedup (internal_fold_exn [] body)

let is_fail (t : M.mterm) =
  match t.node with
  | M.Mfail _ -> true
  | _ -> false

let flatten_if_fail m (t : M.mterm) : loc_term =
  let rec rec_flat acc (t : M.mterm) : loc_term list =
    match t.node with
    | M.Mif (c,th, Some e) when is_fail th ->
      rec_flat (acc@[mk_loc t.loc (Tif (map_mterm m init_ctx c, map_mterm m init_ctx th,None))]) e
    | _ -> acc @ [map_mterm m init_ctx t] in
  mk_loc t.loc (Tseq (rec_flat [] t))

let mk_ensures m acc (v : M.specification) =
  acc @ (List.map (fun (spec : M.postcondition) -> {
        id = spec.name |> map_lident;
        form = map_mterm m init_ctx spec.formula
      }) (v.postconditions |> List.filter M.Utils.is_post))

let mk_require n i t = {
  id = with_dummy_loc (n ^ "_require_" ^ (string_of_int i));
  form = t
}

(* TODO : should plunge in called functions body *)
let mk_requires m n v =
  M.Utils.get_added_removed_sets m v
  |> List.map (fun t ->
      match t with
      | M.Msetadded e ->
        let a = M.Utils.get_asset_type e in
        loc_term (Tempty (a,mk_ac_added a))
      | M.Msetremoved e ->
        let a = M.Utils.get_asset_type e in
        loc_term (Tempty (a,mk_ac_rmed a))
      | _ -> assert false
    )
  |> Tools.List.dedup
  |> List.mapi (fun i t  -> mk_require n i t)

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
                  id = with_dummy_loc lbl;
                  form = mk_pre_asset m n (unloc id) (map_mterm m init_ctx t)
                }]
            else
              acc
          ) []
      | M.Tcontainer (Tasset n, Collection)
      | M.Tcontainer (Tasset n, Partition) ->
        let n = unloc n in
        M.Utils.get_storage_invariants m (Some n)
        |> List.fold_left (fun acc  (_,lbl,t) ->
            if is_local_invariant m n t && adds_asset m n body then
              (* TODO similar to above : should be more specific : the real question is
                 'is the element of id added?' *)
              acc @ [{
                  id = with_dummy_loc lbl;
                  form = mk_pre_coll m n (unloc id) (map_mterm m init_ctx t)
                }]
            else
              acc
          ) []
      | _ -> acc
    ) [] args

let is_src src (_,f,_) = f.M.src = src

let mk_entry_require m idents =
  if M.Utils.with_trace m && List.length idents > 0 then
    let mk_entry_eq id = Teq (Tyint,
                              Tdoti (gs,mk_id "entry"),
                              Tsome (Tvar (String.capitalize_ascii id))) in
    [
      {
        id = with_dummy_loc "entry_require";
        form =
          List.fold_left (fun acc id ->
              Tor (acc,mk_entry_eq id)
            ) (mk_entry_eq (List.hd idents)) (List.tl idents)
          |> loc_term
      };
      {
        id = with_dummy_loc "empty_trace";
        form = Teq (Tyint,
                    Tdoti (gs, mk_id "tr"),
                    Tnil)
               |> loc_term
      }
    ]
  else []

let add_raise_ctx args src m exn =
  match src with
  | M.Endo ->
    begin
      match exn with
      | Texn Ekeyexist ->
        if List.length args > 0 then
          let mk_ctx (id,t) =
            let id = Mlwtree.deloc id in
            match Mlwtree.deloc t with
            | Tyasset a ->
              let a = Mlwtree.deloc a in
              let key,_ = M.Utils.get_asset_key m a in
              mk_key_found_cond `Old a (Tdoti (a,key))
            | Typartition a | Tycoll a ->
              let a = Mlwtree.deloc a in
              let key,_ = M.Utils.get_asset_key m a in
              Texists ([["a"],Tyasset a],
                       Tand (mk_key_found_cond `Old a (Tdoti("a",key)),
                             Tcontains (a,
                                        Tdoti ("a",key),
                                        Tvar (id))))
            | _ -> exn
          in
          let ctx = List.fold_left (fun acc arg ->
              Tor (acc,mk_ctx arg)
            ) (mk_ctx (List.hd args)) (List.tl args) in
          Timpl (exn, ctx)
        else exn
      | _ -> exn
    end
  | _ -> exn

(* for now (src = `Endo) means the function is 'add_shallow_xxx' *)
let mk_functions src m =
  M.Utils.get_functions m |> List.filter (is_src src) |> List.map (
    fun ((v : M.specification option),
         (s : M.function_struct),
         (t : M.type_)) ->
      let args = (List.map (fun (i, t, _) ->
          (map_lident i, map_mtype t)
        ) s.args) in
      Dfun {
        name     = map_lident s.name;
        logic    = NoMod;
        args     = args;
        returns  = map_mtype t;
        raises   = fold_exns s.body |> List.map (add_raise_ctx args src m) |> List.map loc_term;
        variants = [];
        requires =
          (mk_entry_require m (M.Utils.get_callers m (unloc s.name))) @
          (mk_requires m (s.name |> unloc) v) @
          (mk_preconds m s.args s.body);
        ensures  = Option.fold (mk_ensures m) [] v;
        body     = flatten_if_fail m s.body;
      }
  )

let mk_endo_functions = mk_functions M.Endo
let mk_exo_functions = mk_functions M.Exo

let mk_entries m =
  M.Utils.get_entries m |> List.map (
    fun ((v : M.specification option),
         (s : M.function_struct)) ->
      Dfun {
        name     = map_lident s.name;
        logic    = NoMod;
        args     = (List.map (fun (i,t,_) ->
            (map_lident i, map_mtype t)
          ) s.args);
        returns  = with_dummy_loc Tyunit;
        raises   = fold_exns s.body |> List.map loc_term;
        variants = [];
        requires = (mk_entry_require m [unloc s.name]) @
                   (mk_requires m (unloc s.name) v);
        ensures  = Option.fold (mk_ensures m) [] v;
        body     = flatten_if_fail m s.body;
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
               body   = loc_term (Ttry (unloc_term f.body,
                                        [Enotfound,Tassert (Some ("security_" ^ id),Tfalse);
                                         Ekeyexist,Tassert (Some ("security_" ^ id),Tfalse)]));
             }
      | _ -> d
    end
  | _ -> d

(* ----------------------------------------------------------------------------*)

let to_whyml (m : M.model) : mlw_tree  =
  let storage_module   = with_dummy_loc (String.capitalize_ascii (m.name.pldesc ^ "_storage")) in
  let uselib           = mk_use in
  let uselist          = mk_use_list in
  let useEuclDiv       = mk_use_euclidean_div m in
  let traceutils       = mk_trace_utils m |> deloc in
  let enums            = M.Utils.get_enums m |> List.map (map_enum m) in
  let records          = M.Utils.get_assets m |> List.map (map_record m) |> wdl in
  let eq_assets        = M.Utils.get_assets m |> List.map (mk_eq_asset m) |> wdl in
  let eq_exten         = M.Utils.get_assets m |> List.map (mk_eq_extensionality m) |> deloc in
  let clones           = M.Utils.get_assets m  |> List.map (record_to_clone m) |> wdl in
  let init_records     = records |> unloc_decl |> List.map mk_default_init |> loc_decl in
  let records          = zip records eq_assets init_records clones |> deloc in
  let storage_api_bs   = mk_storage_api_before_storage m (records |> wdl) in
  let storage          = M.Utils.get_storage m |> map_storage m in
  let storageval       = Dval (with_dummy_loc gs, with_dummy_loc Tystorage) in
  let axioms           = mk_axioms m in
  (*let partition_axioms = mk_partition_axioms m in*)
  let transfer         = if M.Utils.with_operations m then [mk_transfer ()] else [] in
  let storage_api      = mk_storage_api m (records |> wdl) in
  let endo             = mk_endo_functions m in
  let functions        = mk_exo_functions m in
  let entries          = mk_entries m |> List.map (process_no_fail m) in
  let usestorage       = mk_use_module storage_module in
  let loct : loc_mlw_tree = [{
      name  = storage_module;
      decls = [uselib]               @
              useEuclDiv             @
              traceutils             @
              enums                  @
              records                @
              eq_exten               @
              storage_api_bs         @
              [storage;storageval]   @
              axioms                 @
              (*partition_axioms       @*)
              transfer               @
              storage_api            @
              endo;
    };{
       name = cap (map_lident m.name);
       decls = [uselib;uselist;usestorage] @
               functions @
               entries;
     }] in unloc_tree loct
