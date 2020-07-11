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
let gArchetypeField = "Field"
let gArchetypeView  = "View"
let gArchetypeColl  = "Collection"
let gArchetypeSum   = "Sum"
let gArchetypeSort  = "Sort"
let gListLib        = "L"
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

let mk_use_list = Duse (false,["list";"List"],Some "L") |> loc_decl |> deloc

let mk_use = Duse (false,[gArchetypeDir;gArchetypeLib],None) |> loc_decl |> deloc

let mk_use_field = Duse (false,[gArchetypeDir;gArchetypeField],Some "F") |> loc_decl |> deloc

let mk_use_view = Duse (false,[gArchetypeDir;gArchetypeView],Some "V") |> loc_decl |> deloc

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

let rec map_mtype (t : M.type_) : loc_typ =
  with_dummy_loc (match t with
      | M.Tasset id                           -> Tyasset (map_lident id)
      | M.Tenum id                            -> Tyenum (map_lident id)
      | M.Tbuiltin v                          -> map_btype v
      | M.Tcontainer (Tasset id,M.Partition)  -> Typartition (map_lident id)
      | M.Tcontainer (Tasset id,M.Aggregate)  -> Tyaggregate (map_lident id)
      | M.Tcontainer (Tasset id,M.View)       -> Tyview (map_lident id)
      | M.Tcontainer (Tasset id,M.Collection) -> Tycoll (map_lident id)
      | M.Tcontainer (t,M.Collection)         -> Tylist (map_mtype t).obj
      | M.Toption t                           -> Tyoption (map_mtype t).obj
      | M.Ttuple l                            -> Tytuple (l |> List.map map_mtype |> List.map deloc)
      | M.Tunit                               -> Tyunit
      | M.Tstate                              -> Tystate
      | M.Tmap (_, _)                         -> Tyunit (* TODO: replace by the right type *)
      | M.Tstorage                            -> Tystorage
      | M.Toperation                          -> Tyunit (* TODO: replace by the right type *)
      | M.Tentry                              -> Tyunit (* TODO: replace by the right type *)
      | M.Tprog _                             -> Tyunit (* TODO: replace by the right type *)
      | M.Tvset _                             -> Tyunit (* TODO: replace by the right type *)
      | M.Ttrace _                            -> Tyunit (* TODO: replace by the right type *)
      | M.Tcontract _                         -> Tyint
      | M.Tset t                              -> Tyset (map_mtype (Tbuiltin t)).obj
      | M.Tlist t                             -> Tylist (map_mtype t).obj
      | _ -> print_endline (Format.asprintf "%a@." M.pp_type_ t); assert false)

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
           Tcons (gListLib, mk_change_term tr,
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
  { name = mk_id "ops"         ; typ = Tyrecord "transfers" ; init = Tnil gListLib; mutable_ = true; };
  { name = mk_id "balance"     ; typ = Tytez;      init = Tint Big_int.zero_big_int; mutable_ = true; };
  { name = mk_id "transferred" ; typ = Tytez;      init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = mk_id "caller"      ; typ = Tyaddr;     init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = mk_id "source"      ; typ = Tyaddr;     init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = mk_id "now"         ; typ = Tydate;     init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = mk_id "chainid"     ; typ = Tychainid;  init = Tint Big_int.zero_big_int; mutable_ = false; }
] @
  if M.Utils.with_trace m then
    [
      { name = mk_id "entry"     ; typ = Tyoption (Tyasset "_entry"); init = Tnone; mutable_ = false; };
      { name = mk_id "tr"        ; typ = Tyasset ("Tr._traces"); init = Tnil gListLib; mutable_ = true; }
    ]
  else []


let mk_sum_clone_id m a f = (String.capitalize_ascii a) ^"Sum" ^ (string_of_int (M.Utils.get_sum_idx m a f))
let mk_sum_clone_from_id asset id = (String.capitalize_ascii asset) ^"Sum" ^ (string_of_int id)
let mk_get_sum_value_id asset id = "get_" ^ asset ^ "_sum" ^ (string_of_int id)
let mk_get_sum_value_from_pos_id asset id = (mk_get_sum_value_id asset id)^"_from_pos"

let mk_sum a i v c = Tvsum (mk_sum_clone_from_id a i, v, c)
let mk_sum_from_col a i c = Tcsum (mk_sum_clone_from_id a i, c)

let mk_sum_clone m asset _key formula =
  let cap_asset = String.capitalize_ascii asset in
  let id = M.Utils.get_sum_idx m asset formula in
  Dclone ([gArchetypeDir;gArchetypeSum],
          mk_sum_clone_from_id asset id,
          [Ctype ("collection",
                  cap_asset ^ ".collection");
           Ctype ("t",
                  asset);
           Ctype ("view",
                  cap_asset ^ ".view");
           Cval ("field",
                 mk_get_sum_value_id asset id);
           Cval ("get",
                 cap_asset ^ ".get");
           Cval ("velts",
                 cap_asset ^ ".velts");
           Cval ("vcard",
                 cap_asset ^ ".vcard");
           Cval ("vmk",
                 cap_asset ^ ".vmk")
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
                            Timpl (Tlmem (gListLib,
                                          Tvar "k",
                                          Tapp (Tvar f,
                                                [Tvar "a"])),
                                   Tccontains (pa,
                                               Tvar "k",
                                               mk_ac_sv "s" pa)))))

(* Transfer & contract call -------------------------------------------------*)

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
            Tcons ( gListLib,
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

let mk_call () =
  let decl : (term, typ, ident) abstract_decl = Dfun {
      name     = "call";
      logic    = NoMod;
      args     = ["t", Tyaddr];
      returns  = Tyunit;
      raises   = [];
      variants = [];
      requires = [];
      ensures  = [];
      body     =
        Tassign (
          Tdoti(gs,"_ops"),
          Tcons (gListLib,
                 Tapp(Tvar "mk_call",[Tvar "t"]),
                 Tdoti(gs,"_ops")
                ))
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
          [Ctype ("collection",
                  cap_asset ^ ".collection");
           Ctype ("t",
                  asset);
           Ctype ("view",
                  cap_asset ^ ".view");
           Cval ("cmp",
                 mk_cmp_function_id asset fields);
           Cval ("view_to_list",
                 cap_asset ^ ".view_to_list");
           Cval ("list_to_view",
                 cap_asset ^ ".list_to_view")
          ])

(* Select --------------------------------------------------------------------*)

(* TODO : complete mapping *)
let rec mk_afun_test = function
  | Tdot (Tvar v,f) when compare v "the" = 0 -> Tdot (Tvar "a",f)
  | Tnow _ -> Tvar (mk_id "now")
  | Tcaller _ -> Tvar (mk_id "caller")
  | Tsender _ -> Tvar (mk_id "source")
  | _ as t -> map_abstract_term mk_afun_test id id t

(* internal select is a local defintion *)
let mk_select_body forcoll asset mlw_test : term =
  (* let capa = String.capitalize_ascii asset in *)
  Tletfun (
    {
      name     = "internal_select";
      logic    = Rec;
      args     = ["l",Tylist Tyint];
      returns  = Tylist Tyint;
      raises   = [];
      variants = [Tvar "l"];
      requires = [];
      ensures  = [];
      body     =
        let some =
          Tmatch (Tget (asset,
                        Tvar "c",
                        Tvar "k"), [
                    Tpsome "a",
                    Tif(mk_afun_test mlw_test,
                        Tcons (gListLib, Tvar "k",Tapp (Tvar ("internal_select"),[Tvar "tl"])),
                        Some (Tapp (Tvar ("internal_select"),[Tvar "tl"])));
                    Twild, Tapp (Tvar ("internal_select"),[Tvar "tl"])
                  ]) in
        Tmlist (gListLib,Tnil gListLib,"l","k","tl",some);
    },

    Tmkview (asset,(Tapp (Tvar "internal_select",
                          [if forcoll then
                             Tapp(Tdoti(String.capitalize_ascii asset,"internal_list_to_view"),
                                  [Tcontent (asset,Tvar "c")])
                           else Tvcontent (asset,Tvar "v")])))
  )

(* TODO : complete mapping
   argument extraction is done on model's term because it is typed *)
let extract_args test =
  let rec internal_extract_args acc (term : M.mterm) =
    match term.M.node with
    | M.Mnow -> acc @ [term,mk_id "now", Tydate]
    | M.Mcaller -> acc @ [term,mk_id "caller", Tyaddr]
    | M.Msource -> acc @ [term,mk_id "source", Tyaddr]
    | _ -> M.fold_term internal_extract_args acc term in
  internal_extract_args [] test

let mk_select_name prefix m asset test = prefix^"select_" ^ asset ^ "_" ^ (string_of_int (M.Utils.get_select_idx m asset test))

let mk_vselect m asset test mlw_test only_formula args =
  let id =  mk_select_name "v" m asset test in
  let (key,_) = M.Utils.get_asset_key m asset in
  let args : (string * string abstract_type) list = List.map (fun (i,t) -> (i, (map_mtype t|> unloc_type))) args in
  let decl = Dfun {
      name     = id;
      logic    = if only_formula then Logic else NoMod;
      args     = (extract_args test |> List.map (fun (_,a,b) -> a,b)) @ args @ ["v",Tyview asset; "c", Tycoll asset];
      returns  = Tyview asset;
      raises   = [];
      variants = [];
      requires = [];
      ensures  = [
        { id   = id ^ "_post_1";
          form = Tforall (
              [["k"],Tykey],
              Timpl (Tvmem (asset,Tvar "k",Tresult),
                     Texists ([["a"],Tyasset asset],
                              Tand (
                                Teq (Tyint, Tdoti("a",key), Tvar "k"),
                                Tand (
                                  Tmem (asset, Tvar "a",Tvar "c"),
                                  mk_afun_test mlw_test
                                )
                              )
                             ))
            );
        };
        { id   = id ^ "_post_2";
          form = Tforall(
              [["k"],Tykey],
              Timpl (
                Tvmem(asset,Tvar "k",Tresult),
                Tvmem(asset,Tvar "k",Tvar "v")
              )
            );
        }
      ];
      body     = mk_select_body false asset mlw_test;
    } in
  decl

let mk_cselect m asset test mlw_test only_formula args =
  let id =  mk_select_name "c" m asset test in
  let (key,_) = M.Utils.get_asset_key m asset in
  let args = List.map (fun (i,t) -> (i, map_mtype t |> unloc_type)) args in
  let decl = Dfun {
      name     = id;
      logic    = if only_formula then Logic else NoMod;
      args     = (extract_args test |> List.map (fun (_,a,b) -> a,b)) @ args @ ["c", Tycoll asset];
      returns  = Tyview asset;
      raises   = [];
      variants = [];
      requires = [];
      ensures  = [
        { id   = id ^ "_post_1";
          form = Tforall (
              [["k"],Tykey],
              Timpl (Tvmem (asset,Tvar "k",Tresult),
                     Texists ([["a"],Tyasset asset],
                              Tand (
                                Teq (Tyint, Tdoti("a",key), Tvar "k"),
                                Tand (
                                  Tmem (asset, Tvar "a",Tvar "c"),
                                  mk_afun_test mlw_test
                                )
                              )
                             ))
            );
        };
        { id   = id ^ "_post_2";
          form = Tforall(
              [["k"],Tykey],
              Timpl (
                Tvmem(asset,Tvar "k",Tresult),
                Tccontains(asset,Tvar "k",Tvar "c")
              )
            );
        }
      ];
      body     = mk_select_body true asset mlw_test;
    } in
  decl

(* Removeif --------------------------------------------------------------------*)

(* internal removeif is a local defintion *)
let mk_removeif_body m forcoll asset mlw_test : term =
  (* let capa = String.capitalize_ascii asset in *)
  let (key,_) = M.Utils.get_asset_key m asset in
  Tletfun (
    {
      name     = "internal_removeif";
      logic    = Rec;
      args     = ["l",Tylist Tyint];
      returns  = Tyunit;
      raises   = [];
      variants = [Tvar "l"];
      requires = [];
      ensures  = [];
      body     =
        let some =
          Tmatch (Tget (asset,
                        Tvar "c",
                        Tvar "k"), [
                    Tpsome "a",
                    Tif(mk_afun_test mlw_test,
                        Tapp (Tvar ("remove_" ^ asset),[Tdoti ("a", key)])
                        (* Tremove (asset, mk_ac asset, Tvar ("a")) *),
                        Some (Tapp (Tvar ("internal_removeif"),[Tvar "tl"])));
                    Twild, Tapp (Tvar ("internal_removeif"),[Tvar "tl"])
                  ]) in
        Tmlist (gListLib,Tunit,"l","k","tl",some);
    },

    (Tapp (Tvar "internal_removeif",
           [if forcoll then
              Tapp(Tdoti(String.capitalize_ascii asset,"internal_list_to_view"),
                   [Tcontent (asset,Tvar "c")])
            else Tvcontent (asset,Tvar "v")]))
  )

let mk_removeif_name prefix m asset test = prefix^"removeif_" ^ asset ^ "_" ^ (string_of_int (M.Utils.get_select_idx m asset test))

let mk_vremoveif m asset fn test mlw_test only_formula args =
  let id =  mk_removeif_name "v" m asset test in
  let (_key,_) = M.Utils.get_asset_key m asset in
  let args : (string * string abstract_type) list = List.map (fun (i,t) -> (i, (map_mtype t|> unloc_type))) args in
  let decl = Dfun {
      name     = id;
      logic    = if only_formula then Logic else NoMod;
      args     = (extract_args test |> List.map (fun (_,a,b) -> a,b)) @ args @ ["k", Tykey; "c", Tycoll asset];
      returns  = Tyunit;
      raises   = [];
      variants = [];
      requires = [];
      ensures  = [];
      body     = begin
        (* mk_removeif_body m false asset mlw_test *)
        (* let (key, _) = M.Utils.get_asset_key m asset in *)
        let oan =
          M.Utils.get_asset_field m (asset, fn)
          |> (fun (_, x, _) -> x)
          |> M.Utils.dest_container
        in
        let (okey, _) = M.Utils.get_asset_key m oan in
        Tletfun (
          {
            name     = "internal_removeif";
            logic    = Rec;
            args     = ["l",Tylist Tyint];
            returns  = Tyunit;
            raises   = [];
            variants = [Tvar "l"];
            requires = [];
            ensures  = [];
            body     =
              let some =
                Tmatch (Tget (oan,
                              mk_ac oan,
                              Tvar "k"), [
                          Tpsome "a",
                          Tseq [Tif(mk_afun_test mlw_test, Tapp (Tvar ("remove_" ^ asset ^ "_" ^ fn),[Tvar "k"; Tdoti ("a", okey)]), None);
                                Tapp (Tvar ("internal_removeif"),[Tvar "tl"])];
                          Twild, Tapp (Tvar ("internal_removeif"),[Tvar "tl"])
                        ]) in
              Tmlist (gListLib,Tunit,"l","k","tl",some);
          },
          (Tmatch (Tget (asset, Tvar "c", Tvar "k"),
                   [ Tpsome "a",
                     Tapp (Tvar ("internal_removeif"), [Tapp(Tdoti(String.capitalize_ascii oan, "velts"),[Tdoti ("a", fn)])]);
                     Twild, Tunit
                   ]))
        )

      end;
    } in
  decl

let mk_cremoveif m asset test mlw_test only_formula args =
  let id =  mk_removeif_name "c" m asset test in
  let (_key,_) = M.Utils.get_asset_key m asset in
  let args = List.map (fun (i,t) -> (i, map_mtype t |> unloc_type)) args in
  let decl = Dfun {
      name     = id;
      logic    = if only_formula then Logic else NoMod;
      args     = (extract_args test |> List.map (fun (_,a,b) -> a,b)) @ args @ ["c", Tycoll asset];
      returns  = Tyunit;
      raises   = [];
      variants = [];
      requires = [];
      ensures  = [];
      body     = begin
        (* mk_removeif_body m true asset mlw_test *)
        let (key, _) = M.Utils.get_asset_key m asset in
        Tletfun (
          {
            name     = "internal_removeif";
            logic    = Rec;
            args     = ["l",Tylist Tyint];
            returns  = Tyunit;
            raises   = [];
            variants = [Tvar "l"];
            requires = [];
            ensures  = [];
            body     =
              let some =
                Tmatch (Tget (asset,
                              Tvar "c",
                              Tvar "k"), [
                          Tpsome "a",
                          Tif(mk_afun_test mlw_test,
                              Tapp (Tvar ("remove_" ^ asset),[Tdoti ("a", key)]),
                              Some (Tapp (Tvar ("internal_removeif"),[Tvar "tl"])));
                          Twild, Tapp (Tvar ("internal_removeif"),[Tvar "tl"])
                        ]) in
              Tmlist (gListLib,Tunit,"l","k","tl",some);
          },

          (Tapp (Tvar "internal_removeif",
                 [Tapp(Tdoti(String.capitalize_ascii asset,"internal_list_to_view"),
                       [Tcontent (asset, Tvar "c")])]))
        )
      end;
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

let map_lidents = List.map map_lident

let rec type_to_init m (typ : loc_typ) : loc_term =
  mk_loc typ.loc (match typ.obj with
      | Tyasset i     -> Tapp (loc_term (Tvar ("mk_default_"^i.obj)),[])
      | Typartition i -> Temptyfield i
      | Tyaggregate i -> Temptyfield i
      | Tycoll i      -> Temptycoll i
      | Tylist _      -> Tnil (with_dummy_loc gListLib)
      | Tyview i      -> Temptyview i
      | Tymap i       -> Tvar (mk_loc typ.loc ("const (mk_default_" ^ i.obj ^ " ())"))
      | Tyenum i      -> Tvar (mk_loc typ.loc (unloc (M.Utils.get_enum m i.obj).initial))
      | Tytuple l     -> Ttuple (List.map (type_to_init m) (List.map with_dummy_loc l))
      | Tybool        -> Ttrue
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
           Tcons (gListLib, map_action_to_change a |> mk_change_term,Tnil gListLib);
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
           Tcons (gListLib, map_action_to_change a |> mk_change_term,Tnil gListLib);
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

let mk_cmp_enums m (r : M.asset) =
  List.fold_left (fun acc (item : M.asset_item) ->
      match item.type_ with
      | Tenum lid ->
        let id = unloc lid in
        if List.mem id acc then acc else acc @ [id]
      | _ -> acc
    ) [] r.values |>
  List.map (fun id ->
      Dfun  {
        name = "cmp_" ^ id |> with_dummy_loc;
        logic = Logic;
        args = ["e1" |> with_dummy_loc, loc_type (Tyenum id);
                "e2" |> with_dummy_loc, loc_type (Tyenum id)];
        returns = Tybool |> with_dummy_loc;
        raises = [];
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

let mk_eq_asset _m (r : M.asset) =
  let cmps = List.map (fun (item : M.asset_item) ->
      let f1 = Tdoti("a1",unloc item.name) in
      let f2 = Tdoti("a2",unloc item.name) in
      match item.type_ with
      | Tasset a -> Tapp (Tvar ("eq_"^(unloc a)),[f1;f2])
      | Tcontainer (Tasset a, Collection) -> Teqfield(unloc a,f1,f2)
      | Tcontainer (Tasset a, Partition) -> Teqfield(unloc a,f1,f2)
      | Tcontainer (Tasset a, Aggregate) -> Teqfield(unloc a,f1,f2)
      | Tbuiltin Bbool -> (* a = b is (a && b) || (not a && not b) *)
        Tor (Tpand (f1,f2),Tpand(Tnot f1, Tnot f2))
      | Tbuiltin Brational ->
        Tpand (Teq (Tyint,Tfst f1,Tfst f2), Teq(Tyint,Tsnd f1, Tsnd f2))
      | Ttuple [Tbuiltin Bint;Tbuiltin Bint] ->
        Tpand (Teq (Tyint,Tfst f1,Tfst f2), Teq(Tyint,Tsnd f1, Tsnd f2))
      | Tenum lid -> Tapp (Tvar ("cmp_"^(unloc lid)),[f1;f2])
      | _ -> Teq (Tyint,f1,f2)
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
  Dclone ([gArchetypeDir;gArchetypeColl] |> wdl,
          String.capitalize_ascii (unloc r.name) |> with_dummy_loc,
          [Ctype ("t" |> with_dummy_loc, (unloc r.name) |> with_dummy_loc);
           Cval  ("keyt" |> with_dummy_loc, key |> with_dummy_loc);
           Cval  ("eqt" |> with_dummy_loc, "eq_" ^ (unloc r.name) |> with_dummy_loc)])

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
  | M.Tcontainer (Tasset a,_) ->
    let toview = (fun t -> Ttoview (unloc a, t)) in
    ((fun (t1,t2) -> loc_term (Tcnth  (unloc a,t1,t2))),
     (fun t ->       loc_term (Tvcard (unloc a, toview t))))
  | _ -> assert false

type logical_mod = Nomod | Added | Removed
type lctx = Inv | Other

type logical_context = {
  lctx : lctx;
  old  : bool;
  lmod : logical_mod;
  localold : ident list;
  loop_id : ident option;
}

let init_ctx = {
  lctx = Other;
  old = false;
  lmod = Nomod;
  localold = [];
  loop_id = None;
}

let mk_trace_seq m t chs =
  if M.Utils.with_trace m then
    Tseq ([with_dummy_loc t] @ (List.map mk_trace chs))
  else t

let is_old (ctx : logical_context) (t : M.mterm) =
  match t.node with
  (* | M.Mdotasset ({ node = M.Mvarlocal id;  type_ = _},_) -> List.mem (unloc id) ctx.localold *)
  | M.Mdotassetfield (an, _, _) -> List.mem (unloc an) ctx.localold
  | _ -> false

let map_mpattern (p : M.lident M.pattern_node) =
  match p with
  | M.Pwild -> Twild
  | M.Pconst i -> Tconst (map_lident i)

let mk_ac_ctx a ctx =
  match ctx.lctx with
  | Inv -> loc_term (Tvar (mk_ac_id a))
  | _ ->  loc_term (mk_ac a)

let is_coll_field m f : bool =
  M.Utils.get_containers m |> List.map (fun (_,v,_) -> v) |> List.mem f

let is_exec_divergent = function
  | M.Mget _
  | M.Mnth _
    -> true
  | _ -> false

let rec map_mterm m ctx (mt : M.mterm) : loc_term =
  let error_internal desc = emit_error (mt.loc, desc); Tnottranslated in
  let error_not_translated (msg : string) = (* Tnottranslated in *) error_internal (TODONotTranslated msg) in
  let error_not_supported (msg : string) = error_internal (NotSupported msg) in
  let t =
    match mt.node with
    (* lambda *)

    | Mletin ([id], v, _, b, None) ->
      Tletin (M.Utils.is_local_assigned (unloc id) b, map_lident id, None, map_mterm m ctx v, map_mterm m ctx b)

    (* | Mletin ([id], { node = M.Mget (a, {node = M.Msetbefore _; _}, k); type_ = _ }, _, b, Some e) -> (* logical *)
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
                   Some (map_mterm m ctx e)) |> with_dummy_loc) *)

    | Mletin ([id], { node = M.Mget (a, _, k); type_ = _ }, _, b, Some e) -> (* logical *)
      let ctx = ctx in
      Tmatch (Tget (loc_ident a,
                    mk_ac_ctx a ctx,
                    map_mterm m ctx k) |> with_dummy_loc,[
                Tpsome (map_lident id), map_mterm m ctx b;
                Twild, map_mterm m ctx e
              ])
    | Mletin ([id], { node = M.Mnth (n, CKview c,k); type_ = _ }, _, b, Some e) ->
      Tmatch (Tvnth (loc_ident n,
                     map_mterm m ctx k,
                     map_mterm m ctx c) |> with_dummy_loc,[
                Tpsome (map_lident id),  map_mterm m ctx b;
                Twild, map_mterm m ctx e
              ])
    | Mletin ([id], { node = M.Mnth (n, CKcoll,k); type_ = _ }, _, b, Some e) ->
      Tmatch (Tcnth (loc_ident n,
                     map_mterm m ctx k,
                     mk_ac_ctx n ctx) |> with_dummy_loc,[
                Tpsome (map_lident id), map_mterm m ctx b;
                Twild, map_mterm m ctx e
              ])

    | Mletin ([id], { node = M.Mgetopt v; type_ = _ }, _, b, Some e) ->
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

    | Mletin              _ -> error_not_translated "Mletin"
    | Mdeclvar            _ -> error_not_supported "Mdeclvar"

    | Mapp (f, args) ->
      Tapp (mk_loc (map_lident f).loc (Tvar (map_lident f)), List.map (map_mterm m ctx) args)


    (* assign *)

    | Massign (ValueAssign, Avar id, v) ->
      Tassign (with_dummy_loc (Tvar (map_lident id)),map_mterm m ctx v)

    | Massign (MinusAssign, Avar id, v) ->
      Tassign (with_dummy_loc (Tvar (map_lident id)),
               with_dummy_loc (
                 Tminus (with_dummy_loc Tyint,
                         with_dummy_loc (Tvar (map_lident id)),
                         map_mterm m ctx v)))

    | Massign (_, Avar _, _) -> error_not_translated "Massign (_, _, Avar _, _)"

    | Massign (assignop, Avarstore id, v) ->
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
    | Massign (assignop, Aasset (_id1, id2, k), v) ->

      let id = with_dummy_loc (Tdot (map_mterm m ctx (* id1 *) k, (* FIXME *)
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


    | Massign (_op, Arecord (_rn, _fn, _r), _v) -> error_not_translated "Massign (op, Arecord (_rn, fn, r), v)"

    | Massign (_, Astate, v) -> Tassign (loc_term (Tdoti (gs, "state")), map_mterm m ctx v)

    | Massign (_, Aassetstate _, _) -> error_not_translated "Massign (_, _, Aassetstate _, _)"


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
      Tfor (map_lident id,
            map_mterm m ctx from,
            map_mterm m ctx to_,
            mk_invariants m ctx id lbl body,
            map_mterm m ctx body
           )
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
    | Mfail (Invalid { node = M.Mstring msg; type_=_ }) -> Traise (Einvalid (Some msg))
    | Mfail (Invalid { node = M.Mvar (n, Vlocal); type_=_ }) -> Traise (Einvalid (Some (unloc n)))
    | Mfail               _ -> error_not_translated "Mfail"
    | Mtransfer (a, t) -> Ttransfer(map_mterm m ctx a, map_mterm m ctx t)
    | Mcallcontract (_, d, _, _, _) -> Tcall (map_mterm m ctx d)
    | Mcallentry (_, _e, _) -> error_not_translated "Mcallentry"
    | Mcallself (_, _e, _)  -> error_not_translated "Mcallself"


    (* entrypoint *)

    | Mentrycontract (_c, _id) -> error_not_translated "Mentrycontract"
    | Mentrypoint (_a, _s)     -> error_not_translated "Mentrypoint"
    | Mself _id                -> error_not_translated "Mself"


    (* literals *)

    | Mint v -> Tint v
    | Mnat v -> Tint v
    | Mbool false -> Tfalse
    | Mbool true -> Ttrue
    | Menum               _ -> error_not_supported "Menum"
    | Mrational (l,r) -> Ttuple([ loc_term (Tint l); loc_term (Tint r)])
    | Mstring v -> Tint (sha v)
    | Mcurrency (i, Tz)   -> Tint (Big_int.mult_int_big_int 1000000 i)
    | Mcurrency (i, Mtz)  -> Tint (Big_int.mult_int_big_int 1000 i)
    | Mcurrency (i, Utz)  -> Tint i
    | Maddress v -> Tint (sha v)
    | Mdate s -> Tint (Core.date_to_timestamp s)
    | Mduration v -> Tint (Core.duration_to_timestamp v)
    | Mtimestamp v -> Tint v
    | Mbytes v -> Tint (sha v)
    | Munit -> Tunit

    (* control expression *)

    | Mexprif (c, t, e) ->
      Tif (map_mterm m ctx c, map_mterm m ctx t, Some (map_mterm m ctx e))

    | Mexprmatchwith (t, l) ->
      Tmatch (map_mterm m ctx t, List.map (fun ((p : M.lident M.pattern_gen), e) ->
          (map_mpattern p.node, map_mterm m ctx e)
        ) l)


    (* composite type constructors *)

    | Mnone -> Tnone
    | Msome v -> Tsome (map_mterm m ctx v)

    | Mtuple l              -> Ttuple (List.map (map_mterm m ctx) l)
    | Mtupleaccess (_x, _k) -> error_not_translated "Mtupleaccess"

    | Masset l ->
      let asset = M.Utils.get_asset_type mt in
      let fns = M.Utils.get_field_list m asset |> wdl in
      Trecord (None,(List.combine fns (List.map (map_mterm m ctx) l)))

    | Massets _ ->
      begin
        match mt.type_ with
        | Tcontainer (Tasset a,_) -> (* Tlist (l |> List.map (map_mterm m ctx)) *) Temptyview (map_lident a)
        | _ -> assert false
      end

    | Mlitset  l ->
      List.fold_left(fun acc e ->
          with_dummy_loc (Tcons(with_dummy_loc gListLib, map_mterm m ctx e, acc))
        ) (loc_term (Tnil gListLib)) l |> Mlwtree.deloc
    | Mlitlist l ->
      List.fold_left(fun acc e ->
          with_dummy_loc (Tcons(with_dummy_loc gListLib, map_mterm m ctx e, acc))
        ) (loc_term (Tnil gListLib)) l |> Mlwtree.deloc
    | Mlitmap  _   -> error_not_translated "Mlitmap"
    | Mlitrecord _ -> error_not_translated "Mlitrecord"


    (* access *)

    | Mdot (e, i) -> Tdot (map_mterm m ctx e, mk_loc (loc i) (Tvar (map_lident i))) (* FIXME *)
    | Mdotassetfield (an, k, fn) ->
      Tdot(
        with_dummy_loc (Tapp (loc_term (Tvar ("get_"^(unloc an))),[map_mterm m ctx k])),
        loc_term (Tvar (unloc fn)))
    | Mdotcontract       _ -> error_not_translated "Mdotcontract"
    | Maccestuple        _ -> error_not_translated "Maccestuple"

    (* comparison operators *)

    | Mequal (l, r) -> Teq (map_mtype l.type_,map_mterm m ctx l,map_mterm m ctx r)
    | Mnequal (l, r) -> Tneq (map_mtype l.type_,map_mterm m ctx l,map_mterm m ctx r)
    | Mgt (l, r) -> Tgt (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mge (l, r) -> Tge (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mlt (l, r) -> Tlt (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mle (l, r) -> Tle (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mmulticomp          _ -> error_not_translated "Mmulticomp"


    (* arithmetic operators *)

    | Mand (l, r) -> Tpand (map_mterm m ctx l, map_mterm m ctx r)
    | Mor (a, b) -> Tor (map_mterm m ctx a, map_mterm m ctx b)
    | Mnot c -> Tnot (map_mterm m ctx c)
    | Mplus (l, r)  -> Tplus  (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mminus (l, r) -> Tminus (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mmult (l, r) -> Tmult (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
    | Mdivrat _ -> error_not_translated "Mdivrat"
    | Mdiveuc (l, r) -> Tdiv (with_dummy_loc Tyint, map_mterm m ctx l, map_mterm m ctx r)
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
        (Tapp (loc_term (Tvar ("remove_" ^ n)), [map_mterm m ctx a]))
        [CRm n]

    | Mremovefield (a, f, k, v) ->
      let t,_,_ = M.Utils.get_container_asset_key m a f in
      mk_trace_seq m
        (Tapp (loc_term (Tvar ("remove_" ^ a ^ "_" ^ f)),
               [
                 map_mterm m ctx k;
                 map_mterm m ctx v
               ]
              ))
        [CUpdate f; CRm t]

    | Mremoveall (a, f, v) -> Tapp (loc_term (Tvar ("removeall_" ^ a ^ "_" ^ f)),[map_mterm m ctx v])

    | Mremoveif (a, (CKview l | CKfield (_, _, l)), la, lb, _a) ->
      let args = extract_args lb in
      let id = mk_removeif_name "v" m a lb in
      let argids = args |> List.map (fun (e, _, _) -> e) |> List.map (map_mterm m ctx) in
      let args = List.map (fun (i,_) -> loc_term (Tvar i)) la in
      Tapp (loc_term (Tvar id), argids @ args @ [map_mterm m ctx l; mk_ac_ctx a ctx])

    | Mremoveif (a, CKcoll, la, lb, _a) ->
      let args = extract_args lb in
      let id = mk_removeif_name "c" m a lb in
      let argids = args |> List.map (fun (e, _, _) -> e) |> List.map (map_mterm m ctx) in
      let args = List.map (fun (i,_) -> loc_term (Tvar i)) la in
      Tapp (loc_term (Tvar id), argids @ args @ [mk_ac_ctx a ctx])

    | Mclear (n, CKcoll) -> Tapp (loc_term (Tvar ("clear_"^(n))),[])
    | Mclear (n, CKview v) -> Tapp (loc_term (Tvar ("clear_view_"^(n))),[map_mterm m ctx v])
    | Mclear (n, CKfield (_, _, v)) -> Tapp (loc_term (Tvar ("clear_view_"^(n))),[map_mterm m ctx v])

    | Mset (a, l, k, v) ->
      mk_trace_seq m
        (Tapp (loc_term (Tvar ("set_" ^ a)),
               [
                 map_mterm m ctx k;
                 map_mterm m ctx v
               ]))
        (List.map (fun f -> CUpdate f) l)

    | Mupdate             _ -> error_not_translated "Mupdate"
    | Maddupdate          _ -> error_not_translated "Maddupdate"


    (* asset api expression *)

    | Mget (an, _c, k) -> Tapp (loc_term (Tvar ("get_" ^ an)), [map_mterm m ctx k])

    | Mselect (a, (CKview l | CKfield (_, _, l)), la, lb, _a) ->
      let args = extract_args lb in
      let id = mk_select_name "v" m a lb in
      let argids = args |> List.map (fun (e, _, _) -> e) |> List.map (map_mterm m ctx) in
      let args = List.map (fun (i,_) -> loc_term (Tvar i)) la in
      Tapp (loc_term (Tvar id), argids @ args @ [map_mterm m ctx l; mk_ac_ctx a ctx])
    | Mselect (a, CKcoll, la, lb, _a) ->
      let args = extract_args lb in
      let id = mk_select_name "c" m a lb in
      let argids = args |> List.map (fun (e, _, _) -> e) |> List.map (map_mterm m ctx) in
      let args = List.map (fun (i,_) -> loc_term (Tvar i)) la in
      Tapp (loc_term (Tvar id), argids @ args @ [mk_ac_ctx a ctx])
    | Msort (a, (CKview c | CKfield (_, _, c)),l) -> Tvsort (with_dummy_loc (mk_sort_clone_id a l),map_mterm m ctx c,mk_ac_ctx a ctx)
    | Msort (a, CKcoll,l) -> Tcsort (with_dummy_loc (mk_sort_clone_id a l), mk_ac_ctx a ctx)
    | Mcontains (a, (CKview v | CKfield (_, _, v)), r) -> Tvcontains (with_dummy_loc a, map_mterm m ctx r, map_mterm m ctx v)
    | Mcontains (a, CKcoll, r) -> Tccontains (with_dummy_loc a, map_mterm m ctx r,  mk_ac_ctx a ctx )

    | Mnth (n, (CKview c | CKfield (_, _, c)),k) -> Tapp (loc_term (Tvar ("vnth_" ^ n)),[map_mterm m ctx c; map_mterm m ctx k])
    | Mnth (n, CKcoll,k) -> Tapp (loc_term (Tvar ("cnth_" ^ n)),[ mk_ac_ctx n ctx; map_mterm m ctx k])
    | Mcount (a, (CKview t | CKfield (_, _, t))) -> Tvcard (with_dummy_loc a, map_mterm m ctx t)
    | Mcount (a, CKcoll) -> Tccard (with_dummy_loc a, mk_ac_ctx a ctx)
    | Msum          (a, (CKview v | CKfield (_, _, v)),f) ->
      let cloneid = mk_sum_clone_id m a f in
      let col = mk_ac_ctx a ctx in
      Tvsum(with_dummy_loc cloneid,map_mterm m ctx v,col)
    | Msum (a, CKcoll,f) ->
      let cloneid = mk_sum_clone_id m a f in
      let col = mk_ac_ctx a ctx in
      Tcsum(with_dummy_loc cloneid,col)
    | Mhead (n, (CKview c | CKfield (_, _, c)), v) -> Tvhead(with_dummy_loc n, map_mterm m ctx v, map_mterm m ctx c)
    | Mhead (n, CKcoll, v) -> Tchead(with_dummy_loc n, map_mterm m ctx v, mk_ac_ctx n ctx)
    | Mtail  (n, (CKview c | CKfield (_, _, c)), v) -> Tvtail(with_dummy_loc n, map_mterm m ctx v, map_mterm m ctx c)
    | Mtail  (n, CKcoll, v) -> Tctail(with_dummy_loc n, map_mterm m ctx v, mk_ac_ctx n ctx)

    (* utils *)
    | Mcast (Tcontainer (Tasset a,Collection),Tcontainer (Tasset _, View), v) ->
      begin match v.node with
        | Mapp(f,_)  when is_coll_field m (unloc f) ->
          map_mterm m ctx v |> Mlwtree.deloc
        | Mvar (f, Vlocal) when is_coll_field m (unloc f) ->
          map_mterm m ctx v |> Mlwtree.deloc
        (* | Mdotasset (_,f) when is_coll_field m (unloc f) -> *)
        | Mdotassetfield (_, _, f) when is_coll_field m (unloc f) ->
          map_mterm m ctx v |> Mlwtree.deloc
        | _ -> Ttoview (map_lident a,map_mterm m ctx v)
      end
    | Mcast (_, _, v)       -> map_mterm m ctx v |> Mlwtree.deloc


    (* set api expression *)
    | Msetadd (_, s, e)      -> Tapp (loc_term (Tvar "set_add")      , [ map_mterm m ctx s; map_mterm m ctx e ])
    | Msetremove (_, s, e)   -> Tapp (loc_term (Tvar "set_remove")   , [ map_mterm m ctx s; map_mterm m ctx e ])
    | Msetcontains (_, s, e) -> Tapp (loc_term (Tvar "set_contains") , [ map_mterm m ctx s; map_mterm m ctx e ])
    | Msetlength (_, s)      -> Tapp (loc_term (Tvar "set_length")   , [ map_mterm m ctx s ])


    (* list api expression *)

    | Mlistprepend (_, l, e)  -> Tapp (loc_term (Tvar "lprepend"),[map_mterm m ctx l; map_mterm m ctx e])
    | Mlistcontains (_, l, e) -> Tapp (loc_term (Tvar "lcontains"),[map_mterm m ctx l; map_mterm m ctx e])
    | Mlistcount (_, l)       -> Tapp (loc_term (Tvar "lcard"),[map_mterm m ctx l])
    | Mlistnth (_, n, l)      -> Tapp (loc_term (Tvar "lnth"),[map_mterm m ctx l;map_mterm m ctx n])


    (* map api expression *)

    | Mmapput (_, _, c, k, v)   -> Tapp (loc_term (Tvar "map_put")      , [ map_mterm m ctx c; map_mterm m ctx k; map_mterm m ctx v ])
    | Mmapremove (_, _, c, k)   -> Tapp (loc_term (Tvar "map_remove")   , [ map_mterm m ctx c; map_mterm m ctx k ])
    | Mmapget (_, _, c, k)      -> Tapp (loc_term (Tvar "map_get")      , [ map_mterm m ctx c; map_mterm m ctx k ])
    | Mmapgetopt (_, _, c, k)   -> Tapp (loc_term (Tvar "map_getopt")   , [ map_mterm m ctx c; map_mterm m ctx k ])
    | Mmapcontains (_, _, c, k) -> Tapp (loc_term (Tvar "map_contains") , [ map_mterm m ctx c; map_mterm m ctx k ])
    | Mmaplength (_, _, c)      -> Tapp (loc_term (Tvar "map_length")   , [ map_mterm m ctx c ])


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
        | M.Ttuple [M.Tbuiltin (M.Bint); M.Tbuiltin M.Bint] ->
          Tapp (loc_term (Tvar "abs_rat"),[map_mterm m ctx v])
        | _ -> error_not_translated "Mfunabs"
      end

    | Mconcat (x, y) ->
      begin
        match mt.type_ with
        | Tbuiltin Bstring -> Tapp (loc_term (Tvar "str_concat"),[map_mterm m ctx x; map_mterm m ctx y])
        | Tbuiltin Bbytes -> Tapp (loc_term (Tvar "byt_concat"),[map_mterm m ctx x; map_mterm m ctx y])
        | _ -> error_not_translated "Mconcat"
      end
    | Mslice  (s,i1,i2) -> Tapp (loc_term (Tvar "substring"),[map_mterm m ctx s; map_mterm m ctx i1; map_mterm m ctx i2])
    | Mlength s -> Tapp (loc_term (Tvar "str_length"),[map_mterm m ctx s])
    | Misnone s -> Tapp (loc_term (Tvar "isnone"),[map_mterm m ctx s])
    | Missome s -> Tapp (loc_term (Tvar "issome"),[map_mterm m ctx s])
    | Mgetopt s -> Tapp (loc_term (Tvar "getopt"),[map_mterm m ctx s])
    | Mfloor  s -> Tapp (loc_term (Tvar "floor"),[map_mterm m ctx s])
    | Mceil   s -> Tapp (loc_term (Tvar "ceil"),[map_mterm m ctx s])
    | Mpack   s -> Tapp (loc_term (Tvar "pack"),[map_mterm m ctx s])
    | Munpack (_, s) -> Tapp (loc_term (Tvar "unpack"),[map_mterm m ctx s])

    | Mblake2b x -> Tapp (loc_term (Tvar "blake2b"),[map_mterm m ctx x])
    | Msha256  x -> Tapp (loc_term (Tvar "sha256"),[map_mterm m ctx x])
    | Msha512  x -> Tapp (loc_term (Tvar "sha512"),[map_mterm m ctx x])
    | Mhashkey  x -> Tapp (loc_term (Tvar "hash_key"),[map_mterm m ctx x])
    | Mchecksignature (k,s,b) -> Tapp (loc_term (Tvar "check_signature"),[map_mterm m ctx k;map_mterm m ctx s;map_mterm m ctx b])


    (* constants *)

    | Mnow -> Tnow (with_dummy_loc gs)
    | Mtransferred -> Ttransferred (with_dummy_loc gs)
    | Mcaller -> Tcaller (with_dummy_loc gs)

    | Mbalance ->
      begin
        match ctx.lctx with
        | Inv -> loc_term (Tvar "_balance") |> Mlwtree.deloc
        | _ -> loc_term (Tdoti (gs, "_balance")) |> Mlwtree.deloc
      end

    | Msource               -> Tsender (with_dummy_loc gs)
    | Mselfaddress          -> error_not_translated "selfaddress"
    | Mchainid              -> Tchainid (with_dummy_loc gs)

    (* variables *)

    | Mvar(_, Vassetstate _) -> error_not_translated "Mvar(_, Vassetstate _)"

    | Mvar (v, Vstorevar) ->
      begin
        match ctx.lctx with
        | Inv -> Tvar (map_lident v)
        | _ -> Tdoti (with_dummy_loc gs, map_lident v)
      end

    | Mvar (n, Vstorecol) ->
      let coll =
        match ctx.lctx, ctx.old, ctx.lmod with
        | Inv, _, _ -> Tvar (mk_ac_id (n |> unloc))
        | _, false, Nomod   -> mk_ac (n |> unloc)
        | _, false, Added   -> mk_ac_added (n |> unloc)
        | _, false, Removed -> mk_ac_rmed (n |> unloc)
        | _, true, Nomod    -> mk_ac_old (n |> unloc)
        | _, true, Added    -> mk_ac_old_added (n |> unloc)
        | _, true, Removed  -> mk_ac_old_rmed (n |> unloc)
      in
      loc_term coll |> Mlwtree.deloc

    | Mvar (v, Venumval) -> Tvar (map_lident v)
    | Mvar (v, Vlocal) -> Tvar (map_lident v)
    | Mvar (v, Vparam) -> Tvar (map_lident v)
    | Mvar (_, Vfield) -> error_not_translated "Mvar (_, Vfield)"
    | Mvar (_, Vthe)   -> error_not_translated "Mvar (_, Vthe)"
    | Mvar (_, Vstate) ->
      begin
        match ctx.lctx with
        | Inv -> loc_term (Tvar "state") |> Mlwtree.deloc
        | _ -> loc_term (Tdoti (gs, "state")) |> Mlwtree.deloc
      end



    (* rational *)

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
    | Mdivtez (r,t) -> Tapp (loc_term (Tvar "div_tez"),[map_mterm m ctx r; map_mterm m ctx t])
    | Minttorat v -> Ttuple ([map_mterm m ctx v; loc_term (Tint (Big_int.big_int_of_int 1))])
    | Mratdur (r,t) -> Tapp (loc_term (Tvar "rat_dur"),[map_mterm m ctx r; map_mterm m ctx t])

    (* functional *)

    | Mfold               _ -> error_not_translated "Mfold"


    (* imperative *)

    | Mbreak                -> error_not_translated "break;"


    (* quantifiers *)

    | Mforall (i, t, None, b) ->
      let typ = map_mtype t in
      Tforall (
        [[i |> map_lident],typ],
        map_mterm m ctx b)

    | Mforall (i, t, Some coll, b) ->
      let asset = M.Utils.get_asset_type (M.mk_mterm (M.Mbool false) t) in
      Tforall (
        [[i |> map_lident],loc_type (Tyasset asset)],
        with_dummy_loc (Timpl (with_dummy_loc (Tmem (with_dummy_loc asset,
                                                     loc_term (Tvar (unloc i)),
                                                     map_mterm m ctx coll)),
                               map_mterm m ctx b)))

    | Mexists (i, t, None, b) ->
      let typ = map_mtype t in
      Texists (
        [[i |> map_lident],typ],
        map_mterm m ctx b)

    | Mexists (i, t, Some coll, b) ->
      let asset = M.Utils.get_asset_type (M.mk_mterm (M.Mbool false) t) in
      Texists (
        [[i |> map_lident],loc_type (Tyasset asset)],
        with_dummy_loc (Timpl (with_dummy_loc (Tmem (with_dummy_loc asset,
                                                     loc_term (Tvar (unloc i)),
                                                     map_mterm m ctx coll)),
                               map_mterm m ctx b)))


    (* formula operators *)

    | Mimply (a, b) -> Timpl  (map_mterm m ctx a, map_mterm m ctx b)
    | Mequiv (a, b) -> Tequiv (map_mterm m ctx a, map_mterm m ctx b)


    (* formula asset collection *)

    (* | Msetbefore c -> map_mterm m { ctx with old = true } c |> Mlwtree.deloc *)
    | Msetbefore c -> Told (map_mterm m { ctx with old = true } c)
    | Msetat (label,t) -> Tat (with_dummy_loc label, map_mterm m ctx t)
    | Msetunmoved _ -> error_not_translated "Msetunmoved"
    | Msetadded c ->  map_mterm m { ctx with lmod = Added } c |> Mlwtree.deloc
    | Msetremoved c -> map_mterm m { ctx with lmod = Removed } c |> Mlwtree.deloc
    | Msetiterated  container ->
      let n = M.Utils.get_asset_type mt |> with_dummy_loc in
      let iter_id = Option.get (ctx.loop_id) in
      begin match container with
        | ICKview c  -> Tvhead (n,loc_term (Tvar iter_id),map_mterm m ctx c)
        | ICKcoll n  -> Tchead (with_dummy_loc n,loc_term (Tvar iter_id),mk_ac_ctx n ctx)
        | ICKfield (_, _, c) -> Tvhead (n,loc_term (Tvar iter_id),map_mterm m ctx c)
        | ICKset  _  -> error_not_translated "Msetiterated for set"
        | ICKlist _  -> error_not_translated "Msetiterated for list"
        | ICKmap  _  -> error_not_translated "Msetiterated for map"
      end
    | Msettoiterate container ->
      let n = M.Utils.get_asset_type mt |> with_dummy_loc in
      let iter_id = Option.get (ctx.loop_id) in
      begin match container with
        | ICKview c  -> Tvtail (n,loc_term (Tvar iter_id),map_mterm m ctx c)
        | ICKcoll n  -> Tctail (with_dummy_loc n,loc_term (Tvar iter_id),mk_ac_ctx n ctx)
        | ICKfield (_, _, c) -> Tvtail (n,loc_term (Tvar iter_id),map_mterm m ctx c)
        | ICKset  _  -> error_not_translated "Msettoiterate for set"
        | ICKlist _  -> error_not_translated "Msettoiterate for list"
        | ICKmap  _  -> error_not_translated "Msettoiterate for map"
      end
    (* formula asset collection methods *)

    (* | Mapifget (a, _c, k) -> Tapp (loc_term (Tvar ("get_" ^ a)),[map_mterm m ctx k])
       | Mapifpureget (a, k) -> Tfget(with_dummy_loc a, loc_term (mk_ac  a),map_mterm m ctx k) *)

    (* | Msubsetof (n, l, r) ->
       begin match l with
        | { node = Mcast(_,_,c); type_ = _ } -> Tsubset (with_dummy_loc n, map_mterm m ctx c, map_mterm m ctx r)
           | _ -> Tsubset (with_dummy_loc n, map_mterm m ctx l, map_mterm m ctx r)
       end *)

    | Msubsetof (an, c, x) -> begin
        let prefix, arg =
          match c with
          | CKcoll -> "c", mk_ac_ctx an ctx
          | CKview c
          | CKfield (_, _, c) -> "v", map_mterm m ctx c
        in
        Tapp (loc_term (Tdoti (String.capitalize_ascii an, prefix ^ "subset")), [arg; map_mterm m ctx x])
      end
    | Misempty (l, r) ->
      begin match r.type_ with
        | M.Tcontainer (_,View) -> Tvempty (with_dummy_loc l, map_mterm m ctx r)
        | _ -> Tempty (with_dummy_loc l, map_mterm m ctx r)
      end
      (* | Mapifselect (a, l, _, r, _) ->  let args = extract_args r in
         let id = mk_select_name "c" m a r in *)
      (* | Mapifselect (a, l, _, r, _) ->  let args = extract_args r in
         let id = mk_select_name m a r in
         let argids = args |> List.map (fun (e, _, _) -> e) |> List.map (map_mterm m ctx) in
         Tapp (loc_term (Tvar id), argids @ [map_mterm m ctx l; loc_term (mk_ac a)])
         | Mapifsort (a,c,l) -> Tvsort (with_dummy_loc (mk_sort_clone_id a l),map_mterm m ctx c,loc_term (mk_ac a))
         | Mapifcontains  (a, _, r) ->
         begin match ctx.lctx with
          | Inv ->
            Tccontains (with_dummy_loc a,
                       map_mterm m ctx r,
                       loc_term (Tvar (mk_ac_id a)))
          | _ ->
            Tccontains (with_dummy_loc a,
                       map_mterm m ctx r,
                       loc_term (mk_ac a))
         end
         | Mapifnth       _ -> error_not_translated "Mapifnth"
         | Mapifcount     (a,{ node=Mcast (_,_,c); type_=_ }) -> Tccard (with_dummy_loc a, map_mterm m ctx c)
         | Mapifcount     (a,t) -> Tvcard (with_dummy_loc a, map_mterm m ctx t)
         | Mapifsum       (a,v,f) ->
         let cloneid = mk_sum_clone_id m a f in
         Tvsum(with_dummy_loc cloneid,map_mterm m ctx v,mk_ac_ctx a ctx)
         | Mapifhead (n,c,v) -> Tchead (with_dummy_loc n, map_mterm m ctx v, map_mterm m ctx c)
         | Mapiftail (n,c,v) -> Tctail (with_dummy_loc n, map_mterm m ctx v, map_mterm m ctx c)
         Tsum(with_dummy_loc cloneid,map_mterm m ctx v,mk_ac_ctx a ctx)
         | Mapifhead (n,c,v) -> Thead (with_dummy_loc n, map_mterm m ctx v, map_mterm m ctx c)
         | Mapiftail (n,c,v) -> Ttail (with_dummy_loc n, map_mterm m ctx v, map_mterm m ctx c) *)
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
        let ctx = { ctx with loop_id = Some (unloc id) } in
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
      let init_value = type_to_init m typ_ in {
        name     = map_lident value.name;
        typ      = typ_;
        init     = Option.fold (fun _ -> map_mterm m init_ctx) init_value value.default;
        mutable_ = false;
      }
    ) values

let map_record m (r : M.asset) =
  Drecord (map_lident r.name, map_record_values m r.values)

let map_init_mterm m ctx (t : M.mterm) =
  match t.node with
  | M.Mnow -> loc_term (Tint Big_int.zero_big_int)
  | _ -> map_mterm m ctx t

let map_storage_items m = List.fold_left (fun acc (item : M.storage_item) ->
    acc @
    match item.typ with
    | M.Tcontainer (Tasset id, Collection) ->
      let id = unloc id in [
        mk_collection_field id mk_ac_id;
        mk_collection_field id mk_ac_added_id;
        mk_collection_field id mk_ac_rmed_id
      ]
    | M.Tcontainer (Tasset id, View) ->
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
        init     = map_init_mterm m init_ctx item.default;
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
  Tccontains (asset,
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
    args = ["v",Tyview asset; "c",Tycoll asset; "i",Tyint];
    returns = Tyint;
    raises = [];
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
                                       Tvar "c",
                                       Tvar "k"),[
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
    (* body = Tif (mk_not_found_cond `Curr asset (Tvar "k"),
                Traise Enotfound,
                Some (Tget (asset,
                            mk_ac asset,
                            Tvar "k"))); *)

    body = Tmatch (Tget (asset,
                         mk_ac asset,
                         Tvar "k"),[
                     Tpsome "e", Tvar "e";
                     Twild, Traise Enotfound])
  }

let mk_cnth_asset asset = Dfun {
    name = "cnth_" ^ asset;
    logic = NoMod;
    args = ["c",Tycoll asset;"i",Tyint];
    returns = Tykey;
    raises = [ Texn Enotfound ];
    variants = [];
    requires = [];
    ensures = [{ id   = "cnth_" ^ asset ^ "_post_1";
                 form = Tccontains (asset,
                                    Tresult,
                                    Tvar "c");
               }];
    body =
      Tmatch (Tcnth (asset,
                     Tvar "i",
                     Tvar "c"),[
                Tpsome "k", (Tvar "k"
                (* Tmatch (Tget (asset,
                              mk_ac asset,
                              Tvar "k"),[
                   Tpsome "v", Tvar "v";
                   Twild, Traise Enotfound
                   ]) *));
                Twild, Traise Enotfound
              ])
  }

let mk_vnth_asset asset = Dfun {
    name = "vnth_" ^ asset;
    logic = NoMod;
    args = ["v",Tyview asset;"i",Tyint];
    returns = Tykey;
    raises = [ Texn Enotfound ];
    variants = [];
    requires = [];
    ensures = [{ id   = "vnth_" ^ asset ^ "_post_1";
                 form = Tvmem (asset,
                               Tresult,
                               Tvar "v");
               }];
    body =
      Tmatch (Tvnth (asset,
                     Tvar "i",
                     Tvar "v"),[
                Tpsome "k", (Tvar "k"
                (* Tmatch (Tget (asset,
                              mk_ac asset,
                              Tvar "k"),[
                   Tpsome "v", Tvar "v";
                   Twild, Traise Enotfound
                   ]) *));
                Twild, Traise Enotfound
              ])
  }

let gen_exists_asset at asset_id asset a body =
  Texists ([[asset],Tyasset a],
           Tand (
             Teq(Tyint, Tget (a,begin match at with | `Curr -> mk_ac a | `Old -> mk_ac_old a end, Tvar asset_id),Tsome (Tvar asset)),
             body))

let exists_asset = gen_exists_asset `Curr "asset_id" "asset"

let exists_old_asset = gen_exists_asset `Old "asset_id" "old_asset"

let exists_rm_asset = gen_exists_asset `Curr "rm_asset_id" "rm_asset"

let mk_set_sum_ensures m a =
  List.fold_left (fun acc idx ->
      acc @ [{
          id = "set_" ^ a ^ "_sum_post";
          form = exists_asset a (Teq (Tyint,
                                      mk_sum_from_col a idx (mk_ac_old a),
                                      Tplus (Tyint,
                                             Tminus (Tyint,
                                                     mk_sum_from_col a idx (mk_ac a),
                                                     Tapp(Tvar (mk_get_sum_value_id a idx),
                                                          [Tvar "new_asset"])),
                                             Tapp(
                                               Tvar (mk_get_sum_value_id a idx),
                                               [Tvar "asset"]))))
        }]) [] (M.Utils.get_sum_idxs m a)

let mk_set_count_ensures m a =
  if M.Utils.with_count m a then [{
      id = "set_" ^ a ^ "_count";
      form = Teq (Tyint,
                  Tccard (a,mk_ac a),
                  Tccard (a,mk_ac_old a)
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
             form =
               let eq =
                 Teq (Tyint,
                      Tapp (Tvar f.name,
                            [Tvar "e"]),
                      Tapp (Tvar f.name,
                            [Tvar ("new_asset")])) in
               Tmatch (Tget (n,
                             mk_ac n,
                             Tvar "asset_id"),[
                         Tpsome "e",eq;
                         Twild, Tfalse
                       ])
           }])
    ) (1,[]) fields) @ (mk_set_sum_ensures m n) @ (mk_set_count_ensures m n)

let mk_set_asset_precond m apid a id = mk_api_precond m apid a (`Preasset id)

let mk_set_asset m key = function
  | Drecord (asset, fields) ->  Dfun {
      name = "set_" ^ asset;
      logic = NoMod;
      args = ["asset_id", Tykey; "new_asset", Tyasset asset];
      returns = Tyunit;
      raises = [ Timpl (Texn Enotfound,
                        mk_not_found_cond `Old asset (Tvar "asset_id"))];
      variants = [];
      requires = mk_set_asset_precond m ("set_" ^ asset) asset "new_asset";
      ensures = mk_set_ensures m asset key fields;
      body = Tif (mk_not_found_cond `Curr asset (Tvar "asset_id"),
                  Traise Enotfound,
                  Some (
                    Tassign (mk_ac asset,
                             Tset (asset,
                                   mk_ac asset,
                                   Tvar "asset_id",
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
        form = Teq(Tyint, Tvar "result", Tccontains (asset,
                                                     Tvar "k",
                                                     mk_ac asset))
      }];
    body     = Tccontains (asset,
                           Tvar "k",
                           mk_ac asset)
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
    ensures  = [];
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
                      mk_sum_from_col a idx (mk_ac a),
                      Tplus (Tyint,
                             mk_sum_from_col a idx (mk_ac_old a),
                             Tapp(
                               Tvar (mk_get_sum_value_id a idx),
                               [Tvar e])))
        }]) [] (M.Utils.get_sum_idxs m a)

let mk_add_count_ensures m a =
  if M.Utils.with_count m a then [{
      id = "add_" ^ a ^ "_count";
      form = Teq (Tyint,
                  Tccard (a, mk_ac a),
                  Tplus (Tyint,
                         Tccard (a, mk_ac_old a),
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

let mk_rm_sum_ensures m prefix a _e =
  List.fold_left (fun acc idx ->
      acc @ [{
          id = "remove_" ^ a ^ "_sum_post";
          form = prefix (Teq (Tyint,
                              mk_sum_from_col a idx (mk_ac a),
                              Tminus (Tyint,
                                      mk_sum_from_col a idx (mk_ac_old a),
                                      Tapp(
                                        Tvar (mk_get_sum_value_id a idx),
                                        [Tvar "asset"]))))
        }]) [] (M.Utils.get_sum_idxs m a)

let mk_clear_sum_ensures m a =
  List.fold_left (fun acc idx ->
      acc @ [{
          id = "clear_" ^ a ^ "_sum_post";
          form = Teq (Tyint,
                      mk_sum_from_col a idx (mk_ac a),
                      Tint (Big_int.zero_big_int))
        }]) [] (M.Utils.get_sum_idxs m a)

let mk_rm_count_ensures m a =
  if M.Utils.with_count m a then [{
      id = "rm_" ^ a ^ "_count";
      form = Teq (Tyint,
                  Tccard (a, mk_ac a),
                  Tminus (Tyint,
                          Tccard (a, mk_ac_old a),
                          Tint (Big_int.big_int_of_int 1)
                         )
                 )
    }]
  else []

let mk_clear_count_ensures m a =
  if M.Utils.with_count m a then [{
      id = "clear_" ^ a ^ "_count";
      form = Teq (Tyint,
                  Tccard (a, mk_ac a),
                  Tint (Big_int.zero_big_int)
                 )
    }]
  else []

let mk_rm_ensures m p a e =
  let mk_cond body =
    Timpl (
      Tccontains (a, Tvar e, mk_ac_old a),
      Texists (
        [["asset"],Tyasset a],
        Tand (
          Teq (Tyint,
               Tget (a, mk_ac_old a, Tvar e),
               Tsome (Tvar "asset")
              ),
          body
        )
      )
    ) in
  [
    { id   = p ^ "_post1";
      form = Tnot (Tccontains (a,
                               Tvar (e),
                               mk_ac a))
    };
    { id   = p ^ "_post2";
      form = mk_cond
          (Teq (Tycoll a,
                mk_ac a,
                Tdiff (a,
                       mk_ac_old a,
                       Tsingl (a,
                               Tvar "asset"))))
    };
    { id   = p ^ "_post3";
      form = mk_cond
          (Teq (Tycoll a,
                mk_ac_rmed a,
                Tunion (a,
                        mk_ac_old_rmed a,
                        Tsingl (a,
                                Tvar "asset"))))
    }
  ] @ (mk_rm_sum_ensures m mk_cond a e) @ (mk_rm_count_ensures m a)

let mk_clear_ensures m p a =
  [
    { id   = p ^ "_post1";
      form = Tempty (a, mk_ac a)
    };
  ] @ (mk_clear_sum_ensures m a) @ (mk_clear_count_ensures m a)

let mk_rm_asset m asset : decl =
  Dfun {
    name     = "remove_" ^ asset;
    logic    = NoMod;
    args     = ["a", Tykey];
    returns  = Tyunit;
    raises   = [];
    variants = [];
    requires = [];
    ensures  = mk_rm_ensures m ("remove_" ^ asset) asset "a";
    body =
      let get = Tget(asset,
                     mk_ac asset,
                     Tvar ("a")) in
      Tmatch (get,[
          Tpsome "e",
          Tseq [
            Tassign (mk_ac_rmed asset,
                     Tadd (asset,
                           mk_ac_rmed asset,
                           Tvar "e"));
            Tassign (mk_ac asset,
                     Tremove (asset,
                              mk_ac asset,
                              Tvar ("a")))
          ];
          Twild, Tunit
        ]);
  }

let mk_clear_coll m asset : decl = Dfun {
    name     = "clear_" ^ asset;
    logic    = NoMod;
    args     = [];
    returns  = Tyunit;
    raises   = [];
    variants = [];
    requires = [];
    ensures  = mk_clear_ensures m ("clear_" ^ asset) asset;
    body = Tassign (mk_ac asset, Temptycoll asset);
  }

let mk_clear_view _m asset : decl = Dfun {
    name     = "clear_view_" ^ asset;
    logic    = NoMod;
    args     = ["v", Tyview asset];
    returns  = Tyunit;
    raises   = [];
    variants = [];
    requires = [];
    ensures  = []; (* TODO *)
    body     =
      Tfor (
        "i",
        Tint Big_int.zero_big_int,
        Tvcard (
          asset,
          Tvar "v"),
        [], (* TODO : add invariant for storage api verification *)
        Tmatch (
          Tvnth (
            asset,
            Tvar "i",
            Tvar "v"
          ), [
            Tpsome "k", Tapp (
              Tvar ("remove_" ^ asset),
              [Tvar ("k")]);
            Twild, Tunit]
        )
      );
  }

(* let mk_clear_field_ensures m part p asset field _key =
   let collfield = Tapp (Tvar field, [Tvar ("a")]) in
   let oldassetcollfield = Ttocoll (asset,collfield,mk_ac_old asset) in
   let clear_field_ensures = [
    { id   = p ^ "_post1";
      form = Tvempty (asset, Tapp (Tvar field,[Tvar "a"]))
    }
   ] @ List.fold_left (fun acc idx ->
      acc @ [{
          id = p ^ "_sum_post";
          form = Teq (Tyint,
                      mk_sum asset idx collfield (mk_ac asset),
                      Tint (Big_int.zero_big_int))
        }]) [] (M.Utils.get_sum_idxs m asset) @
    (if M.Utils.with_count m asset then [{
         id = p ^ "_count";
         form = Teq (Tyint,
                     Tvcard (asset, collfield),
                     Tint (Big_int.zero_big_int)
                    )
       }]
     else [])
   in
   (* declare effect on base asset collections when partition *)
   let clear_field_part_ensures = [
    { id   = p ^ "_part_post1";
      form = Teq (Tyasset asset,
                  mk_ac asset,
                  Tdiff (asset ,
                         mk_ac_old asset,
                         oldassetcollfield))
    };
   ] @ List.fold_left (fun acc idx ->
      acc @ [{
          id = p ^ "_sum_post";
          form = Teq (Tyint,
                      mk_sum_from_col asset idx (mk_ac asset),
                      Tminus (Tyint,
                              mk_sum_from_col asset idx (mk_ac_old asset),
                              mk_sum_from_col asset idx (oldassetcollfield)))
        }]) [] (M.Utils.get_sum_idxs m asset) @
    (if M.Utils.with_count m asset then [{
         id = p ^ "_ccount";
         form = Teq (Tyint,
                     Tccard (asset, mk_ac asset),
                     Tminus (Tyint,
                             Tccard (asset, mk_ac_old asset),
                             Tccard (asset, oldassetcollfield)
                            )
                    )
       }]
     else [])
   in
   if part then
    clear_field_ensures @ clear_field_part_ensures
   else
    clear_field_ensures

   let mk_clear_field_coll _m _part asset field key clearedasset = Dfun {
    name     = "clear_" ^ asset ^ "_" ^ field;
    logic    = NoMod;
    args     = ["a", Tyasset asset];
    returns  = Tyunit;
    raises   = [];
    variants = [];
    requires = [];
    ensures  = mk_clear_field_ensures _m _part ("clear_"^asset^"_"^field) clearedasset field key;
    body =
      Tletin (
        false,
        "new_asset",
        None,
        Trecord (
          Some (Tvar ("a")),
          [field,Tnil gListLib]),
        Tassign (
          mk_ac asset,
          Tset (
            asset,
            mk_ac asset,
            Tdoti ("a",key),
            Tvar ("new_asset"))
        )
      );
   } *)

let mk_add_field_ensures m partition a _ak field prefix adda elem =
  let collfield = Tapp (Tvar field, [Tvar ("asset")]) in
  let key      = M.Utils.get_asset_key m adda |> fst in
  let assetcollfield = Ttocoll (adda,collfield,mk_ac adda) in
  let oldassetcollfield = Ttocoll (adda,collfield,mk_ac_old adda) in
  let add_field_ensures = [
    { id   = prefix ^ "_field_post1";
      form = exists_asset a
          (Tvmem (adda,
                  Tapp (Tvar key,[Tvar elem]),
                  collfield))

    };
  ] @ List.fold_left (fun acc idx ->
      acc @ [{
          id = "add_" ^ adda ^ "_field_sum_post";
          form = exists_asset a (Teq (Tyint,
                                      mk_sum adda idx collfield (mk_ac adda),
                                      Tplus (Tyint,
                                             mk_sum adda idx collfield (mk_ac_old adda),
                                             Tapp(
                                               Tvar (mk_get_sum_value_id adda idx),
                                               [Tvar elem]))))
        }]) [] (M.Utils.get_sum_idxs m adda) @
    (if M.Utils.with_count m adda then [{
         id = "add_" ^ adda ^ "_field_ccount";
         form = exists_asset a (Teq (Tyint,
                                     Tccard (adda, assetcollfield),
                                     Tplus (Tyint,
                                            Tccard (adda, oldassetcollfield),
                                            Tint (Big_int.big_int_of_int 1)
                                           )
                                    ))
       }]
     else [])
  in
  if partition then
    add_field_ensures @ (mk_add_ensures m prefix adda elem)
  else
    []

(* part    : is field a partition
   a       : asset name
   ak      : asset key field name
   pf      : partition field name
   adda    : added asset name
   addktyp : removed asset key type
*)
let mk_add_field m part a ak field adda addak : decl =
  (* let akey  = Tapp (Tvar ak,[Tvar "asset"]) in *)
  let addak = if part then Tapp (Tvar addak,[Tvar "new_asset"]) else Tvar "new_asset" in
  let test,exn =
    if part then
      (fun mode -> mk_key_found_cond mode a (Tvar ("asset_id"))), Ekeyexist
    else
      (fun mode -> mk_not_found_cond mode a (Tvar ("asset_id"))), Enotfound
  in
  Dfun {
    name     = "add_" ^ a ^ "_" ^ field;
    logic    = NoMod;
    args     = ["asset_id",Tykey; "new_asset", if part then Tyasset adda else Tykey];
    returns  = Tyunit;
    raises   = [Timpl (Texn exn,
                       test `Old);
               ];
    variants = [];
    requires = mk_add_asset_precond m ("add_" ^ a ^ "_" ^ field) adda "new_asset";
    ensures  = mk_add_field_ensures m part a ak field ("add_" ^ a ^ "_" ^ field) adda "new_asset";
    body     =
      Tmatch (Tget(a,
                   mk_ac a,
                   Tvar "asset_id"),[
                Tpsome "asset",
                (let addfield =
                   Tletin (false, a ^ "_" ^ field,None,
                           Tapp (Tvar field,[Tvar "asset"]),
                           Tletin (false,"new_" ^ a ^ "_" ^ field,None,
                                   Tvadd (adda, addak,Tvar (a ^ "_" ^ field)),
                                   Tletin (false,"new_asset",None,
                                           Trecord (Some (Tvar "asset"),
                                                    [field,Tvar ("new_" ^ a ^ "_" ^ field)]),
                                           Tassign (mk_ac a,
                                                    Tset (a,
                                                          mk_ac a,
                                                          Tvar "asset_id",
                                                          Tvar ("new_asset")))
                                          ))) in
                 if part then
                   Tseq [
                     Tapp (Tvar ("add_" ^ adda),
                           [Tvar "new_asset"]);
                     addfield ]
                 else
                   addfield)
                ;
                Twild, Traise exn
              ]);
  }

let mk_rm_field_ensures m part asset elem _ak field prefix rm_asset rm_elem =
  let collfield = Tapp (Tvar field, [Tvar ("asset")]) in
  let oldcollfield = Tapp (Tvar field, [Tvar "old_asset"]) in
  let add_conditions b =
    Timpl (
      mk_key_found_cond `Curr asset (Tvar elem),
      Timpl (
        mk_key_found_cond `Old rm_asset (Tvar rm_elem),
        b
      )
    ) in
  let rm_field_ensures = [
    { id   = prefix ^ "_field_post1";
      form = add_conditions (exists_asset asset (Tnot (Tvcontains (rm_asset,
                                                                   Tvar (rm_elem),
                                                                   collfield))))
    };
  ] @
    List.fold_left (fun acc idx ->
        acc @ [{
            id = "remove_" ^ rm_asset ^ "_field_sum_post";
            form = exists_asset asset (
                exists_rm_asset rm_asset (Teq (Tyint,
                                               mk_sum rm_asset idx collfield (mk_ac rm_asset),
                                               Tminus (Tyint,
                                                       mk_sum rm_asset idx collfield (mk_ac_old rm_asset),
                                                       Tapp(
                                                         Tvar (mk_get_sum_value_id rm_asset idx),
                                                         [Tvar "rm_asset"])))))
          }]) [] (M.Utils.get_sum_idxs m rm_asset) @
    (if M.Utils.with_count m rm_asset then [{
         id = "rm_" ^ rm_asset ^ "_field_ccount";
         form = exists_asset asset (
             exists_old_asset asset (Teq (Tyint,
                                          Tvcard (rm_asset, collfield),
                                          Tminus (Tyint,
                                                  Tvcard (rm_asset, oldcollfield),
                                                  Tint (Big_int.big_int_of_int 1)
                                                 )
                                         )))
       }]
     else [])
  in
  if part then
    (mk_rm_ensures m prefix rm_asset rm_elem)@rm_field_ensures
  else
    rm_field_ensures

(* part   : is rmed field a partititon ?
   asset  : asset name
   ktyp   : asset key type
   field  : field name
   rmn    : removed asset name
   rmktyp : removed asset key type
*)
let mk_rm_field m part asset keyf field rmed_asset _rmkey : decl =
  Dfun {
    name     = "remove_" ^ asset ^ "_" ^ field;
    logic    = NoMod;
    args     = ["asset_id",Tykey; "rm_asset_id",Tykey];
    returns  = Tyunit;
    raises   = [];
    variants = [];
    requires = [];
    ensures  = mk_rm_field_ensures m part asset "asset_id" keyf field ("remove_" ^ asset ^ "_" ^ field) rmed_asset "rm_asset_id";
    body     =
      Tmatch (Tget(asset, mk_ac asset, Tvar "asset_id"),[
          Tpsome "asset",
          Tletin (false,
                  asset ^ "_" ^ field,
                  None,
                  Tapp (Tvar field,
                        [Tvar ("asset")]),
                  Tletin (false,
                          "new_" ^ asset ^ "_" ^ field,
                          None,
                          Tvremove (rmed_asset,
                                    Tvar ("rm_asset_id"),
                                    Tvar (asset ^ "_" ^ field)),
                          Tletin (false,
                                  "new_" ^ asset ^ "_asset",
                                  None,
                                  Trecord (Some (Tvar ("asset")),
                                           [field,Tvar ("new_" ^ asset ^ "_" ^ field)]),
                                  let assign =
                                    Tassign (mk_ac asset,
                                             Tset (asset,
                                                   mk_ac asset,
                                                   Tvar ("asset_id"),
                                                   Tvar ("new_" ^ asset ^ "_asset"))) in
                                  if part then
                                    Tseq [
                                      assign;
                                      Tapp (Tvar ("remove_" ^ rmed_asset),
                                            [Tvar ("rm_asset_id")])
                                    ] else assign
                                 )))
          ;
          Twild, Tunit
        ])
    ;
  }

(* calls clear_view_asset if partition for now;
   NB : when container field are implemented with dedicated types
   and not view, this will change
*)
let mk_removeall _m part asset field rm_asset =
  Dfun {
    name     = "removeall_" ^ asset ^ "_" ^ field;
    logic    = NoMod;
    args     = ["asset_id",Tykey];
    returns  = Tyunit;
    raises   = [];
    variants = [];
    requires = [];
    ensures  = []; (* TODO *)
    body     =  Tmatch (
        Tget(asset, mk_ac asset, Tvar "asset_id"),[
          Tpsome "asset",
          Tletin (false,
                  "new_" ^ asset ^ "_asset",
                  None,
                  Trecord (Some (Tvar ("asset")),
                           [field, Temptyview rm_asset]),
                  let assign =
                    Tassign (mk_ac asset,
                             Tset (asset,
                                   mk_ac asset,
                                   Tvar ("asset_id"),
                                   Tvar ("new_" ^ asset ^ "_asset"))) in
                  if part then
                    Tseq [
                      assign;
                      Tapp (
                        Tvar ("clear_view_"^rm_asset),
                        [Tdoti ("asset",field)])
                    ] else assign
                 )
          ;
          Twild, Tunit
        ])
    ;
  }

let gnthgen = ref false
let gselectgen = ref false
let gremoveifgen = ref false
let gcleargen = ref false

let mk_storage_api_before_storage (m : M.model) _records =
  m.api_items |> List.fold_left (fun acc (sc : M.api_storage) ->
      match sc.node_item with
      | M.APIAsset (Sum (asset,_,_,formula)) when compare asset "todo" <> 0 ->
        let key      = M.Utils.get_asset_key m asset |> fst in
        let mlw_formula = map_mterm m init_ctx formula |> unloc_term in
        let id = M.Utils.get_sum_idx m asset formula in
        acc @ [ (*mk_get_sum_value_from_pos asset id mlw_formula;*)
          mk_get_sum_value asset id mlw_formula;
          mk_sum_clone m asset key formula ]
      | M.APIAsset (Select (asset, (View | Field _), args, test)) ->
        if not !gselectgen then (
          gselectgen := true;
          let mlw_test = map_mterm m init_ctx test in
          acc @ [ mk_vselect m asset test (mlw_test |> unloc_term) (match sc.api_loc with | OnlyFormula -> true | ExecFormula | OnlyExec -> false) args ])
        else acc
      | M.APIAsset (Select (asset, Coll, args, test)) ->
        let mlw_test = map_mterm m init_ctx test in
        acc @ [ mk_cselect m asset test (mlw_test |> unloc_term) (match sc.api_loc with | OnlyFormula -> true | ExecFormula | OnlyExec -> false) args ]
      | _ -> acc
    ) [] |> loc_decl |> deloc

let is_partition m n f =
  match M.Utils.get_field_container m n f with
  | _,Partition -> true
  | _ -> false

let mk_storage_api (m : M.model) records =
  m.api_items |> List.fold_left (fun acc (sc : M.api_storage) ->
      match sc.node_item with
      | M.APIAsset (Get n) ->
        let k,kt = M.Utils.get_asset_key m n in
        acc @ [mk_get_asset n k (kt |> map_btype)]
      | M.APIAsset (Nth (n, Coll)) ->
        acc @ [mk_cnth_asset n]
      | M.APIAsset (Nth (n, (View | Field _))) ->
        if not !gnthgen then (
          gnthgen := true;
          acc @ [mk_vnth_asset n])
        else acc
      | M.APIAsset (Add n) ->
        let k = M.Utils.get_asset_key m n |> fst in
        acc @ [mk_add_asset m n k]
      | M.APIAsset (Remove n) ->
        acc @ [mk_rm_asset m n]
      | M.APIAsset (Set n) ->
        let record = get_record n (records |> unloc_decl) in
        let k      = M.Utils.get_asset_key m (get_record_name record) |> fst in
        acc @ [mk_set_asset m k record]
      | M.APIAsset (FieldAdd (a,pf)) ->
        let k            = M.Utils.get_asset_key m a |> fst in
        let (pa,addak,_) = M.Utils.get_container_asset_key m a pf in
        acc @ [
          (*mk_add_asset           pa.pldesc addak.pldesc;*)
          mk_add_field m (is_partition m a pf) a k pf pa addak
        ]
      | M.APIAsset (FieldRemove (n,f)) ->
        let t         = M.Utils.get_asset_key m n |> fst in
        let (pa,pk,_) = M.Utils.get_container_asset_key m n f in
        acc @ [
          (*mk_rm_asset           pa.pldesc (pt |> map_btype);*)
          mk_rm_field m (is_partition m n f) n t f pa pk
        ]
      (* | M.APIAsset (Contains (n, _)) ->
         let t         =  M.Utils.get_asset_key m n |> snd |> map_btype in
         acc @ [ mk_contains n t ] *)
      | M.APIAsset (Sort (asset, _, field)) ->
        acc @ [ mk_cmp_function m asset field; mk_sort_clone m asset field]
      | M.APIAsset (Clear (n, Coll)) ->
        acc @ [mk_clear_coll m n]
      | M.APIAsset (Clear (n, (View | Field _))) ->
        if not !gcleargen then (
          gcleargen := true;
          acc @ [mk_rm_asset m n;mk_clear_view m n])
        else acc
      (*       | M.APIAsset (RemoveAll (n,f)) ->
               let (key,_) = M.Utils.get_asset_key m n in
               let (clearedasset,_,_) = M.Utils.get_container_asset_key m n f in
               acc @ [mk_clear_field_coll m (is_partition m n f) n f key clearedasset] *)
      | M.APIBuiltin(Babs (M.Tbuiltin M.Bint)) ->
        acc @ [Duse (true,["int";"Abs"],None)]
      | M.APIAsset (RemoveAll (a,f)) ->
        let (pa,_,_) = M.Utils.get_container_asset_key m a f in
        let ispart = is_partition m a f in
        acc @ (if ispart then [mk_clear_view m pa] else []) @ [mk_removeall m ispart a f pa]
      | M.APIAsset (RemoveIf (asset, Field (_, fn), args, test)) ->
        if not !gremoveifgen then (
          gremoveifgen := true;
          let mlw_test = map_mterm m init_ctx test in
          acc @ [ mk_vremoveif m asset fn test (mlw_test |> unloc_term) (match sc.api_loc with | OnlyFormula -> true | ExecFormula | OnlyExec -> false) args ])
        else acc
      | M.APIAsset (RemoveIf (asset, Coll, args, test)) ->
        let mlw_test = map_mterm m init_ctx test in
        acc @ [ mk_cremoveif m asset test (mlw_test |> unloc_term) (match sc.api_loc with | OnlyFormula -> true | ExecFormula | OnlyExec -> false) args ]
      | _ -> acc
    ) [] |> loc_decl |> deloc

(* Entries --------------------------------------------------------------------*)

let fold_exns m body : term list =
  let rec internal_fold_exn acc (term : M.mterm) =
    match term.M.node with
    | M.Mget (_, _, k) -> internal_fold_exn (acc @ [Texn Enotfound]) k
    | M.Mnth (_, CKview c, k) -> internal_fold_exn (internal_fold_exn (acc @ [Texn Enotfound]) c) k
    | M.Mnth (_, CKcoll, k) -> internal_fold_exn ((acc @ [Texn Enotfound])) k
    | M.Mset (_, _, k, v) -> internal_fold_exn (internal_fold_exn (acc @ [Texn Enotfound]) k) v
    | M.Maddasset (_, i) -> internal_fold_exn (acc @ [Texn Ekeyexist]) i
    | M.Maddfield (a, f, c, i) ->
      internal_fold_exn
        (internal_fold_exn (acc @ if (is_partition m a f) then [Texn Ekeyexist]
                            else [Texn Enotfound ]) c) i
    | M.Mgetopt _ -> acc @ [Texn Enotfound]
    | M.Mfail InvalidCaller -> acc @ [Texn Einvalidcaller]
    | M.Mfail NoTransfer -> acc @ [Texn Enotransfer]
    | M.Mfail (InvalidCondition _) -> acc @ [Texn Einvalidcondition]
    | M.Mfail InvalidState -> acc @ [Texn Einvalidstate]
    | M.Mfail (Invalid _) -> acc @ [Texn (Einvalid None)]
    | M.Mlistnth _ -> acc @ [Texn Enotfound]
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
      | M.Tcontainer (Tasset n, Aggregate)
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

let mk_entry_require m idents =
  if M.Utils.with_trace m && List.length idents > 0 then
    let mk_entry_eq id = Teq (Tyint,
                              Tdoti (gs,mk_id "entry"),
                              Tsome (Tvar (mk_trace_id Entry id))) in
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
                    Tnil gListLib)
               |> loc_term
      }
    ]
  else []

(* let add_raise_ctx args src m exn =
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
   | _ -> exn *)

(* for now (src = `Endo) means the function is 'add_shallow_xxx' *)
let mk_functions m =
  M.Utils.get_functions m |> List.map (
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
        raises   = fold_exns m s.body (* |> List.map (add_raise_ctx args src m) *) |> List.map loc_term;
        variants = [];
        requires =
          (mk_entry_require m (M.Utils.get_callers m (unloc s.name))) @
          (mk_requires m (s.name |> unloc) v) @
          (mk_preconds m s.args s.body);
        ensures  = Option.fold (mk_ensures m) [] v;
        body     = flatten_if_fail m s.body;
      }
  )

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
        raises   = fold_exns m s.body |> List.map loc_term;
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
  let uses             = [mk_use; mk_use_list; mk_use_field; mk_use_view] in
  let useEuclDiv       = mk_use_euclidean_div m in
  let useMinMax        = mk_use_min_max m in
  let traceutils       = mk_trace_utils m |> deloc in
  let enums            = M.Utils.get_enums m |> List.map (map_enum m) in
  let records          = M.Utils.get_assets m |> List.map (map_record m) |> wdl in
  let cmp_enums        = M.Utils.get_assets m |> List.map (mk_cmp_enums m) |> List.flatten in
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
  let transfer         = if M.Utils.with_operations m then [mk_transfer ();mk_call()] else [] in
  let storage_api      = mk_storage_api m (records |> wdl) in
  let functions        = mk_functions m in
  let entries          = mk_entries m |> List.map (process_no_fail m) in
  let usestorage       = mk_use_module storage_module in
  let loct : loc_mlw_tree = [{
      name  = storage_module;
      decls = uses                   @
              useEuclDiv             @
              useMinMax              @
              traceutils             @
              enums                  @
              cmp_enums              @
              records                @
              eq_exten               @
              storage_api_bs         @
              [storage;storageval]   @
              axioms                 @
              (*partition_axioms       @*)
              transfer               @
              storage_api;
    };{
       name = cap (map_lident m.name);
       decls = [uselib;uselist;usestorage] @
               functions @
               entries;
     }] in unloc_tree loct
