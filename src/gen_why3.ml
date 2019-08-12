open Location

module M = Model
open Tools
open Mlwtree

(* Constants -------------------------------------------------------------------*)

let gArchetypeDir   = "archetype2"
let gArchetypeLib   = "Lib"
let gArchetypeColl  = "AssetCollection"
let gArchetypeSum   = "Sum"
let gArchetypeList  = "IntListUtils"

let mkId s          = "_"^s

let mksacId s        = mkId (s^"_assets")
let mksaacId s       = mkId (s^"_assets_added")
let mksracId s       = mkId (s^"_assets_removed")

(* Utils -----------------------------------------------------------------------*)

let mk_use = Duse [gArchetypeDir;gArchetypeLib] |> loc_decl |> deloc
let mk_use_list = Duse ["list";"List"] |> loc_decl |> deloc
let mk_use_module m = Duse [deloc m]  |> loc_decl |> deloc

let mk_default_init = function
  | Drecord (n,fs) ->
    Dfun {
      name     = "mk_default_"^n;
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


let mk_collection_field id suffix = {
  name     = with_dummy_loc (mkId (id^suffix));
  typ      = loc_type (Tycoll id);
  init     = loc_term (Tdoti (String.capitalize_ascii id,"empty"));
  mutable_ = true;
}

let mk_const_fields with_trace = [
  { name = mkId "ops"   ; typ = Tyrecord "transfers" ; init = Tvar "Nil"; mutable_ = true; };
  { name = mkId "balance" ;     typ = Tytez; init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = mkId "transferred" ; typ = Tytez; init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = mkId "caller"    ; typ = Tyaddr;  init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = mkId "now"       ; typ = Tydate;  init = Tint Big_int.zero_big_int; mutable_ = false; };
] @
  if with_trace then
    [
      { name = mkId "tr"; typ = Tyrecord "Tr.log" ; init = Tvar "Nil"; mutable_ = true; };
      { name = mkId "ename"; typ = Tyoption (Tyenum "entry"); init = Tnone; mutable_ = true;}
    ]
  else []

let mk_trace_clone = Dclone (["archetype";"Trace"], "Tr",
                             [Ctype ("asset","asset");
                              Ctype ("entry","entry");
                              Ctype ("field","field")])

let mk_sum_clone m asset field = Dclone ([gArchetypeDir;gArchetypeSum],
                                         String.capitalize_ascii field,
                                         [Ctype ("container",
                                                 (String.capitalize_ascii asset)^".collection");
                                          Cval ("f",
                                                "get_"^field)])

let mk_get_field_from_pos asset field = Dfun {
    name = "get_"^field;
    logic = Logic;
    args = ["c",Tycoll asset; "i",Tyint];
    returns = Tyint;
    raises = [];
    variants = [];
    requires = [];
    ensures = [];
    body = Tapp (Tvar field,
                 [
                   Tnth (asset,
                         Tvar "i",
                         Tvar "c")
                 ])
  }

let mk_get_asset asset ktyp = Dfun {
    name = "get_"^asset;
    logic = NoMod;
    args = ["s",Tystorage; "k",ktyp];
    returns = Tyasset asset;
    raises = [ Enotfound ];
    variants = [];
    requires = [];
    ensures = [
      { id   = "get_"^asset^"_post";
        form = Tmem (asset,
                     Tresult,
                     Tdoti ("s",
                            mksacId asset));
      }
    ];
    body = Tif (Tnot (Tcontains (asset,
                                 Tvar "k",
                                 Tdoti ("s",
                                        mksacId asset))),
                Traise Enotfound,
                Some (Tget (asset,
                            Tdoti ("s",
                                   mksacId asset),
                            Tvar "k")))
  }

let rec get_asset_key_typ key (fields : field list) =
  match fields with
  | field :: tl when compare field.name key = 0 -> field.typ
  | _ :: tl -> get_asset_key_typ key tl
  | _ -> assert false

let mk_update_fields n key =
  List.fold_left (fun acc (f:field) ->
      if compare f.name key = 0 then
        acc
      else
        acc@[f.name,Tapp (Tvar f.name,
                          [Tvar ("new_asset")])]
    ) []

let mk_set_ensures n key fields =
  snd (List.fold_left (fun (i,acc) (f:field) ->
      if compare f.name key = 0 then
        (i,acc)
      else
        (succ i,acc@[{
             id   = "set_"^n^"_post"^(string_of_int i);
             form = Teq (Tyint,
                         Tapp (Tvar f.name,
                               [Tget (n,
                                      Tdoti ("s",mksacId n),
                                      Tvar "k")]),
                         Tapp (Tvar f.name,
                               [Tvar ("new_asset")]))
           }])
    ) (1,[]) fields)

let mk_set_asset key = function
  | Drecord (n,fields) ->  Dfun {
      name = "set_"^n;
      logic = NoMod;
      args = ["s",Tystorage; "k",get_asset_key_typ key fields; "new_asset", Tyasset n];
      returns = Tyunit;
      raises = [ Enotfound ];
      variants = [];
      requires = [];
      ensures = mk_set_ensures n key fields;
      body = Tif (Tnot (Tcontains (n,
                                   Tvar "k",
                                   Tdoti ("s",
                                          mkId (n^"_assets")))),
                  Traise Enotfound,
                  Some (
                    Tassign (Tdoti ("s",
                                    mkId (n^"_assets")),
                             Tset (n,
                                   Tdoti ("s",mkId(n^"_assets")),
                                   Tvar "k",Tvar ("new_asset")))
                  ))
    }
  | _ -> assert false

(* f --> f (get a_assets k) *)
let mk_app_field (a : loc_ident) (f : loc_ident) : loc_term * loc_term  =
  let arg   : term     = Tvar "a" in
  let loc_f : loc_term = mk_loc f.loc (Tvar f) in
  (loc_f,with_dummy_loc (Tapp (loc_f,[loc_term arg])))

(* n is the asset name
   f is the name of the key field
   ktyp is the key type
*)
let mk_keys_eq_axiom n f ktyp : decl =
  Daxiom ("eq_"^n^"_keys",
          Tforall ([["s"],Tystorage;["k"],ktyp],
                   Timpl (Tmem (n,
                                Tvar "k",
                                Tdoti ("s",n^"_keys")),
                          Teq (Tyint,
                               Tapp (Tvar f,
                                     [Tget (n,
                                            Tdoti ("s",n^"_assets"),
                                            Tvar "k")]),
                               Tvar "k"))))

(* n is the asset name
   f is the partition field name
   kt is the key type
   pa is the partitionned asset name
   kpt is the partionned asset key type
*)
let mk_partition_axiom n f kt pa kpt : decl =
  Daxiom (n^"_"^f^"_is_partition",
          Tforall ([["s"],Tystorage;["a"],Tyasset n;["k"],kpt],
                   Timpl (Tmem (n, Tvar("a"),Tdoti ("s",mksacId n)),
                          Timpl (Tlmem (gArchetypeList,
                                        Tvar "k",
                                        Tapp (Tvar f, [Tvar "a"])),
                                 Tcontains (pa, Tvar "k",Tdoti ("s",mksacId pa))))))

(* API storage templates -----------------------------------------------------*)

let get_select_id (m : M.model) asset test =
  let rec internal_get_select_id acc = function
    | (sc : M.api_item) :: tl ->
      begin
        match sc.node_item with
        | M.APIFunction (Select (a,t)) ->
          if compare a asset = 0 then
            if M.cmp_mterm t test then
              acc + 1
            else
              internal_get_select_id (acc + 1) tl
          else
            internal_get_select_id acc tl
        | _ -> internal_get_select_id acc tl
      end
    | [] -> acc in
  internal_get_select_id 0 m.api_items

(* TODO : complete mapping *)
let rec mk_select_test = function
  | Tdot (Tvar v,f) when compare v "the" = 0 -> Tdot (Tvar "a",f)
  | Tnow _ -> Tvar "_now"
  | _ as t -> map_abstract_term mk_select_test id id t

let mk_body asset mlw_test : term =
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
    Trecord (
      None,
      [capa^".content", Tapp (Tvar "internal_select",[Tdoti("c."^capa,"content")])]
    )
  )

(* argument extraction is done on model's term because it is typed *)
let extract_args test =
  let rec internal_extract_args acc (term : M.mterm) =
    match term.M.node with
    | M.Mnow -> acc @ [term,"_now", Tydate]
    | _ -> M.fold_term internal_extract_args acc term in
  internal_extract_args [] test

let mk_select_name m asset test = "select_"^asset^"_"^(string_of_int (get_select_id m asset test))

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
        { id   = id;
          form = Tforall (
              [["a"],Tyasset asset],
              Timpl (Tmem (asset,Tvar "a",Tresult),
                     mk_select_test mlw_test
                    )
            );
        }
      ];
      body     = mk_body asset mlw_test;
    } in
  decl


let mk_contains asset keyt = Dfun {
    name     = asset^"_contains";
    logic    = NoMod;
    args     = ["s",Tystorage; "k",keyt];
    returns  = Tybool;
    raises   = [];
    variants = [];
    requires = [];
    ensures  = [
      { id   = asset^"_contains_1";
        form = Teq(Tyint, Tvar "result", Tcontains (asset, Tvar "k",
                                                    Tdoti ("s",mkId(asset^"_assets"))))
      }];
    body     = Tcontains (asset, Tvar "k",
                          Tdoti ("s",mkId(asset^"_assets")))
  }

(* basic getters *)

let gen_field_getter n field = Dfun {
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

(* TODO : add postconditions *)
let mk_add_asset asset key : decl = Dfun {
    name     = "add_"^asset;
    logic    = NoMod;
    args     = ["s",Tystorage; "new_asset",Tyasset asset];
    returns  = Tyunit;
    raises   = [Ekeyexist];
    variants = [];
    requires = [];
    ensures  = [
      { id   = "add_"^asset^"_post_1";
        form = Tmem (asset, Tvar ("new_asset"),
                     Tdoti ("s",mkId(asset^"_assets")))
      };
      { id   = "add_"^asset^"_post_2";
        form = Teq (Tycoll asset,
                    Tdoti ("s",mkId(asset^"_assets")),
                    Tunion (asset,Tdot (Told (Tvar "s"),
                                  Tvar (mkId(asset^"_assets"))),
                            Tsingl (asset,Tvar "new_asset")));
      };
      { id   = "add_"^asset^"_post_3";
        form = Teq (Tycoll asset,
                    Tdoti ("s",mkId(asset^"_assets_added")),
                    Tunion (asset,Tdot (Told (Tvar "s"),
                                  Tvar (mkId(asset^"_assets_added"))),
                            Tsingl (asset,Tvar ("new_asset"))));
      };
      { id   = "add_"^asset^"_post_4";
        form = Tempty (asset,Tinter (asset,Tdot(Told (Tvar "s"),Tvar (mkId(asset^"_assets"))),
                               Tsingl (asset,Tvar ("new_asset"))
                              ));
      };

    ];
    body     = Tseq [
        Tif (Tmem (asset, Tvar ("new_asset"),
                   Tdoti ("s",mkId(asset^"_assets"))),
             Traise Ekeyexist, (* then *)
             Some (Tseq [      (* else *)
                 Tassign (Tdoti ("s",mkId(asset^"_assets")),
                          Tadd (asset,
                                Tdoti ("s",mkId(asset^"_assets")),
                                Tvar "new_asset"));
                 Tassign (Tdoti ("s",mkId(asset^"_assets_added")),
                          Tadd (asset,
                                Tdoti ("s",mkId(asset^"_assets_added")),
                                Tvar "new_asset"))
               ]
               ))
      ];
  }

let mk_rm_asset n ktyp : decl = Dfun {
    name     = "remove_"^n;
    logic    = NoMod;
    args     = ["s",Tystorage; "k",ktyp];
    returns  = Tyunit;
    raises   = [Enotfound];
    variants = [];
    requires = [];
    ensures  = [
      { id   = "remove_"^n^"_post1";
        form = Tnot (Tcontains (n, Tvar ("k"),Tdoti ("s",mkId(n^"_assets"))))
      };
      { id   = "remove_"^n^"_post2";
        form = Teq (Tycoll n,
                    Tdoti ("s",mkId (n^"_assets")),
                    Tdiff (n,Tdot(Told (Tvar "s"),
                                Tvar (mkId (n^"_assets"))),
                           Tsingl (n,Tget (n,Tdot (Told (Tvar "s"),Tvar (mkId(n^"_assets"))),Tvar "k"))))
      };
      { id   = "remove_"^n^"_post3";
        form = Teq (Tycoll n,
                    Tdoti ("s",mkId(n^"_assets_removed")),
                    Tunion (n,Tdot(Told (Tvar "s"),
                                 Tvar (mkId (n^"_assets_removed"))),
                            Tsingl (n,Tget (n,Tdot (Told (Tvar "s"),Tvar (mkId(n^"_assets_removed"))),Tvar "k"))))
      };
    ];
    body = Tif (Tnot (Tcontains (n, Tvar "k",
                            Tdoti ("s",mkId(n^"_assets")))),
                Traise Enotfound,
                Some (
                  Tseq [
                    Tassign (Tdoti("s",mkId (n^"_assets_removed")),
                             Tadd (n,
                                   Tdoti("s",
                                         mkId(n^"_assets_removed")),
                                   Tget(n,Tdoti ("s",mkId (n^"_assets")), Tvar "k")));
                    Tassign (Tdoti("s",mkId(n^"_assets")),
                             Tremove (n,
                                      Tdoti("s",
                                            mkId(n^"_assets")),
                                      Tvar "k"))

                  ]));
  }

(* a      : asset name
   ak     : asset key field name
   pf      : partition field name
   adda    : added asset name
   addktyp : removed asset key type
*)
let mk_add_partition_field a ak pf adda addak : decl =
  let akey  = Tapp (Tvar ak,[Tvar "asset"]) in
  let addak = Tapp (Tvar addak,[Tvar "new_asset"]) in
  Dfun {
    name     = "add_"^a^"_"^pf;
    logic    = NoMod;
    args     = ["s",Tystorage; "asset",Tyasset a; "new_asset",Tyasset adda];
    returns  = Tyunit;
    raises   = [Enotfound;Ekeyexist];
    variants = [];
    requires = [];
    ensures  = [
      { id   = "add_"^a^"_"^pf^"_post1";
        form = Tmem (adda, Tvar "new_asset",Tdoti ("s",mkId(adda^"_assets")))
      };
      { id   = "add_"^a^"_"^pf^"_post2";
        form = Teq (Tycoll adda,
                    Tdoti ("s",mkId(adda^"_assets")),
                    Tunion (adda,Tdot(Told (Tvar "s"),
                                 Tvar (mkId(adda^"_assets"))),
                            Tsingl (adda,Tvar "new_asset")))
      };
      { id   = "add_"^a^"_"^pf^"_post3";
        form = Teq (Tycoll adda,
                    Tdoti ("s",mkId(adda^"_assets_added")),
                    Tunion (adda,Tdot(Told (Tvar "s"),
                                 Tvar (mkId(adda^"_assets_added"))),
                            Tsingl (adda,Tvar "new_asset")))
      };
    ];
    body     =
      Tif (Tnot (Tmem (a,Tvar "asset",Tdoti ("s",mkId (a^"_assets")))), Traise Enotfound,
           Some (Tseq [
               Tapp (Tvar ("add_"^adda),[Tvar "s";Tvar "new_asset"]);
               Tletin (false, a^"_"^pf,None,
                       Tapp (Tvar pf,[Tvar "asset"]),
                       Tletin (false,"new_"^a^"_"^pf,None,
                               Tcons (addak,Tvar (a^"_"^pf)),
                               Tletin (false,"new_asset",None,
                                       Trecord (Some (Tvar "asset"),
                                                [pf,Tvar ("new_"^a^"_"^pf)]),
                                       Tassign (Tdoti ("s",mkId(a^"_assets")),
                                                Tset (a, Tdoti ("s",mkId (a^"_assets")),
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
let mk_rm_partition_field n ktyp f rmn rmktyp : decl = Dfun {
    name     = "remove_"^n^"_"^f;
    logic    = NoMod;
    args     = ["s",Tystorage; "k",ktyp; rmn^"_k",rmktyp];
    returns  = Tyunit;
    raises   = [Enotfound];
    variants = [];
    requires = [];
    ensures  = [
      { id   = "remove_"^n^"_"^f^"_post1";
        form = Tnot (Tcontains (rmn,Tvar (rmn^"_k"),Tdoti ("s",mkId(rmn^"_assets"))))
      };
      { id   = "remove_"^n^"_"^f^"_post2";
        form = Teq (Tycoll rmn,
                    Tdoti ("s",mkId (rmn^"_assets")),
                    Tdiff (rmn,Tdot(Told (Tvar "s"),
                                Tvar (mkId (rmn^"_assets"))),
                           Tsingl (rmn,Tget (rmn,Tdot(Told (Tvar "s"),Tvar (mkId (rmn^"_assets"))),
                                         Tvar (rmn^"_k")))))
      };
      { id   = "remove_"^n^"_"^f^"_post3";
        form = Teq (Tycoll rmn,
                    Tdoti ("s",mkId(rmn^"_assets_removed")),
                    Tunion (rmn,Tdot(Told (Tvar "s"),
                                 Tvar (mkId(rmn^"_assets_removed"))),
                            Tsingl (rmn,Tget (rmn,Tdot(Told (Tvar "s"),Tvar (mkId (rmn^"_assets"))),
                                         Tvar (rmn^"_k")))))
      };
    ];
    body     =
      Tif (Tnot (Tcontains (n,Tvar "k",Tdoti ("s",mkId(n^"_assets")))), Traise Enotfound,
           Some (
             Tletin (false,n^"_asset",None,
                     Tget (n,Tdoti ("s",mkId (n^"_assets")),Tvar "k"),
                     Tletin (false,n^"_"^f,None,
                             Tapp (Tvar f,[Tvar (n^"_asset")]),
                             Tletin (false,"new_"^n^"_"^f,None,
                                     Tlistremove (gArchetypeList,
                                              Tvar (n^"_"^f),Tvar (rmn^"_k")),
                                     Tletin (false,"new_"^n^"_asset",None,
                                             Trecord (Some (Tvar (n^"_asset")),
                                                      [f,Tvar ("new_"^n^"_"^f)]),
                                             Tseq [
                                               Tassign (Tdoti ("s",mkId(n^"_assets")),
                                                        Tset (n, Tdoti ("s",mkId(n^"_assets")),
                                                              Tvar "k",
                                                              Tvar ("new_"^n^"_asset")));
                                               Tapp (Tvar ("remove_"^rmn),
                                                     [Tvar "s";Tvar (rmn^"_k")])
                                             ]
                                            ))))));
  }

(* ----------------------------------------------------------------------------*)

let wdl (l : 'a list)  = List.map with_dummy_loc l
let unloc_decl = List.map unloc_decl
let loc_decl   = List.map loc_decl
let loc_field  = List.map loc_field
let deloc (l : 'a list) = List.map deloc l

let rec zip l1 l2 l3 =
  match l1,l2,l3 with
  | e1::tl1,e2::tl2,e3::tl3 -> e1::e2::e3::(zip tl1 tl2 tl3)
  | _ -> []

let cap s = mk_loc s.loc (String.capitalize_ascii s.obj)

(* ----------------------------------------------------------------------------*)

let map_lident (i : M.lident) : loc_ident = {
  obj = i.pldesc;
  loc = i.plloc;
}

let map_lidents = List.map map_lident

let type_to_init (typ : loc_typ) : loc_term =
  mk_loc typ.loc (match typ.obj with
      | Typartition i -> Tnil
      | Tycoll i      -> Tnil
      | Tymap i       -> Tvar (mk_loc typ.loc ("const (mk_default_"^i.obj^" ())"))
      | _             -> Tint Big_int.zero_big_int)

let map_btype = function
  | M.Bbool          -> Tybool
  | M.Bint           -> Tyint
  | M.Brational      -> Tyrational
  | M.Bdate          -> Tydate
  | M.Bduration      -> Tyduration
  | M.Bstring        -> Tystring
  | M.Baddress       -> Tyaddr
  | M.Brole          -> Tyrole
  | M.Bcurrency _    -> Tytez
  | M.Bkey           -> Tykey

let rec map_mtype (t : M.type_) : loc_typ =
  with_dummy_loc (match t with
      | M.Tasset id                           -> Tyasset (map_lident id)
      | M.Tenum id                            -> Tyenum (map_lident id)
      | M.Tbuiltin v                          -> map_btype v
      | M.Tcontainer (Tasset id,M.Partition)  -> Typartition (map_lident id)
      | M.Tcontainer (t,M.Collection)         -> Tylist (map_mtype t).obj
      | M.Toption t                           -> Tyoption (map_mtype t).obj
      | M.Ttuple l                            -> Tytuple (l |> List.map map_mtype |> deloc)
      | M.Tunit                               -> Tyunit
      | _ -> assert false)

let rec map_term (t : M.mterm) : loc_term = mk_loc t.loc (
    match t.node with
    | M.Maddress v  -> Tint (sha v)
    | M.Mint i      -> Tint i
    | M.Mvarlocal i -> Tvar (map_lident i)
    | M.Mgt (t1,t2) -> Tgt (with_dummy_loc Tyint,map_term t1,map_term t2)
    | _ ->
      let str = Format.asprintf "Not translated : %a@." M.pp_mterm t in
      print_endline str;
      Tnottranslated
  )

let map_record_term _ = map_term

let map_record_values (values : M.record_item list) =
  List.map (fun (value : M.record_item) ->
      let typ_ = map_mtype value.type_ in
      let init_value = type_to_init typ_ in {
        name     = map_lident value.name;
        typ      = typ_;
        init     = Option.fold map_record_term init_value value.default;
        mutable_ = false;
      }
    ) values

let map_storage_items = List.fold_left (fun acc (item : M.storage_item) ->
    acc @
    match item.typ with
    | M.Tcontainer (Tasset id, Collection) -> (* this is the way to detect assets ... *)
      let id = unloc id in [
        mk_collection_field id "_assets";
        mk_collection_field id "_assets_added";
        mk_collection_field id "_assets_removed"
      ]
    | _ ->
      let typ_ = map_mtype item.typ in
      let init_value = type_to_init typ_ in
      [{
        name     = item.name |> unloc |> mkId |> with_dummy_loc;
        typ      = typ_;
        init     = map_record_term init_value item.default;
        mutable_ = true;
      }]
  ) []

(* prefixes with 'forall k_:key, mem k_ "asset"_keys ->  ...'
   replaces asset field "field" by '"field " (get "asset"_assets k_)'
   TODO : make sure there is no collision between "k_" and invariant vars

   m is the Model
   n is the asset name
   inv is the invariant to extend
*)
let mk_extended_invariant m n inv : loc_term =
  let r        = M.Utils.get_record m n in
  let fields   = r.values |> List.map (fun (item : M.record_item) -> item.name) |> map_lidents in
  let asset    = map_lident n in
  let replacements = List.map (fun f -> mk_app_field asset f) fields in
  let replaced = List.fold_left (fun acc (t1,t2) -> loc_replace t1 t2 acc) inv replacements in
  let prefix   = Tforall ([["a"],Tyasset (unloc_ident asset)],
                          Timpl (Tmem ((unloc_ident asset), Tvar "a",
                                       Tvar (mkId(asset.obj^"_assets"))),
                                 Ttobereplaced)) in
  loc_replace (with_dummy_loc Ttobereplaced) replaced (loc_term prefix)

let map_extended_label_term m n (lt : M.label_term) = {
  id = Option.fold (fun _ x -> map_lident x)  (with_dummy_loc "") lt.label;
  form = mk_extended_invariant m n (map_term lt.term);
}

let map_record m (r : M.record) =
  Drecord (map_lident r.name, map_record_values r.values)

let record_to_clone m (r : M.record) =
  let (key,_) = M.Utils.get_record_key m r.name in
  Dclone ([gArchetypeDir;gArchetypeColl] |> wdl,
          String.capitalize_ascii (r.name |> unloc) |> with_dummy_loc,
          [Ctype ("t" |> with_dummy_loc, r.name |> unloc |> with_dummy_loc);
           Cval  ("keyf" |> with_dummy_loc, key |> unloc |> with_dummy_loc)])

let map_storage m (l : M.storage) =
  Dstorage {
    fields     = (map_storage_items l)@(mk_const_fields false |> loc_field |> deloc);
    invariants = List.concat (List.map (fun (item : M.storage_item) ->
        List.map (map_extended_label_term m item.name) item.invariants) l)
  }

let mk_axioms (m : M.model) =
  let records = M.Utils.get_records m |> List.map (fun (r : M.record) -> r.name) in
  let keys    = records |> List.map (M.Utils.get_record_key m) in
  List.map2 (fun r (k,kt) ->
      mk_keys_eq_axiom r.pldesc k.pldesc (map_btype kt)
    ) records keys |> loc_decl |> deloc

let mk_partition_axioms (m : M.model) =
  M.Utils.get_partitions m |> List.map (fun ((n : M.lident),(item : M.record_item)) ->
      let kt     = M.Utils.get_record_key m n |> snd |> map_btype in
      let pa,_,pkt  = M.Utils.get_partition_record_key m n item.name in
      mk_partition_axiom n.pldesc item.name.pldesc kt pa.pldesc (pkt |> map_btype)
    ) |> loc_decl |> deloc

let rec get_record id = function
  | Drecord (n,_) as r :: tl when compare id n = 0 -> r
  | _ :: tl -> get_record id tl
  | [] -> assert false

let get_record_name = function
  | Drecord (n,_) -> n
  | _ -> assert false

let mk_var (i : ident) = Tvar i

let get_for_fun = function
  | M.Tcontainer (Tasset a,M.Collection) -> (fun (t1,t2) -> loc_term (Tnth  (unloc a,t1,t2))),
                                            (fun t ->       loc_term (Tcard (unloc a,t)))
  | M.Tcontainer (Tasset a,M.Partition)  -> (fun (t1,t2) -> loc_term (Tlnth (gArchetypeList,t1,t2))),
                                            (fun t ->       loc_term (Tlcard (gArchetypeList,t)))
  | _ -> assert false

let rec map_mterm m (mt : M.mterm) : loc_term =
  let t =
    match mt.node with
    | M.Mif (c,t,Some { node=M.Mseq []; type_=_})  ->
      Tif (map_mterm m c, map_mterm m t, None)
    | M.Mif (c,t,e)  -> Tif (map_mterm m c, map_mterm m t, Option.map (map_mterm m) e)
    | M.Mnot c       -> Tnot (map_mterm m c)
    | M.Mfail _      -> Traise Enotfound (* TODO : Mfail should pass the type of exception ... *)
    | M.Mequal (l,r) -> Teq (with_dummy_loc Tyint,map_mterm m l,map_mterm m r)
    | M.Mcaller      -> Tcaller (with_dummy_loc "_s")
    | M.Mtransferred -> Ttransferred (with_dummy_loc "_s")
    | M.Mvarstorevar v  -> Tdoti (with_dummy_loc "_s", map_lident v)
    | M.Mcurrency (v,_) -> Tint v
    | M.Mgt (l, r)      -> Tgt (with_dummy_loc Tyint, map_mterm m l, map_mterm m r)
    | M.Mge (l, r)      -> Tge (with_dummy_loc Tyint, map_mterm m l, map_mterm m r)
    | M.Mlt (l, r)      -> Tlt (with_dummy_loc Tyint, map_mterm m l, map_mterm m r)
    | M.Mle (l, r)      -> Tle (with_dummy_loc Tyint, map_mterm m l, map_mterm m r)
    | M.Mvarlocal v     -> Tvar (map_lident v)
    | M.Mint v          -> Tint v
    | M.Mdotasset (e,i) -> Tdot (map_mterm m e, mk_loc (loc i) (Tvar (map_lident i)))
    | M.Mcontains (a,_,r) ->
      Tapp (loc_term (Tvar (a^"_contains")),
            [loc_term (Tvar "_s");map_mterm m r])
    | M.Maddfield (a,f,{ node = M.Mdotasset (c,_); type_= _ },i) ->
      Tapp (loc_term (Tvar ("add_"^a^"_"^f)),
            [loc_term (Tvar "_s"); map_mterm m c; map_mterm m i])
    | M.Mget (n,k) ->
      Tapp (loc_term (Tvar ("get_"^n)),[loc_term (Tvar "_s");map_mterm m k])
    | M.Maddasset (n,i) ->
      Tapp (loc_term (Tvar ("add_"^n)),[loc_term (Tvar "_s"); map_mterm m i ])
    | M.Mrecord l ->
      let asset = M.Utils.get_asset_type mt in
      let fns = M.Utils.get_field_list m asset |> List.map map_lident in
      Trecord (None,(List.combine fns (List.map (map_mterm m) l)))
    | M.Marray l ->
      Tapp (loc_term (Tvar "mkacol"),[with_dummy_loc (Tlist (l |> List.map (map_mterm m)))])
    | M.Mletin ([id],v,_,b) ->
      Tletin (M.Utils.is_local_assigned id b,map_lident id,None,map_mterm m v,map_mterm m b)
    | M.Mselect (a,l,r) ->
      let args = extract_args r in
      let id = mk_select_name m a r in
      let argids = args |> List.map (fun (e,_,_) -> e) |> List.map (map_mterm m) in
      Tapp (loc_term (Tvar id),argids @ [map_mterm m l])
    | M.Mnow -> Tnow (with_dummy_loc "_s")
    | M.Mseq l -> Tseq (List.map (map_mterm m) l)
    | M.Mfor (id,c,b) ->
      let (nth,card) = get_for_fun c.type_ in
      Tfor (with_dummy_loc "i",
            with_dummy_loc (
              Tminus (with_dummy_loc Tyunit,card (map_mterm m c |> unloc_term),
                      (loc_term (Tint Big_int.unit_big_int)))
            ),
            [],
            with_dummy_loc (
              Tletin (false,
                      map_lident id,
                      None,
                      with_dummy_loc (
                        Tapp (loc_term (Tvar (("get_"^(unloc (M.Utils.get_asset_type c))))),[
                            loc_term (Tvar "_s");
                            (nth (Tvar "i",map_mterm m c |> unloc_term))
                          ]
                          )),
                      map_mterm m b)))
    | M.Massign (ValueAssign,id,v) -> Tassign (with_dummy_loc (Tvar (map_lident id)),map_mterm m v)
    | M.Massign (MinusAssign,id,v) -> Tassign (with_dummy_loc (Tvar (map_lident id)),
                                               with_dummy_loc (
                                                 Tminus (with_dummy_loc Tyint,
                                                         with_dummy_loc (Tvar (map_lident id)),
                                                         map_mterm m v)))
    | M.Massignfield (ValueAssign,id1,id2,v) ->
      let id = with_dummy_loc (Tdoti (map_lident id1,map_lident id2)) in
      Tassign (id,map_mterm m v)
    | M.Massignfield (MinusAssign,id1,id2,v) ->
      let id = with_dummy_loc (Tdoti (map_lident id1,map_lident id2)) in
      Tassign (id,
               with_dummy_loc (
                 Tminus (with_dummy_loc Tyint,
                         id,
                         map_mterm m v)))
    | M.Mset (a,k,v) ->
      Tapp (loc_term (Tvar ("set_"^a)),
            [
              loc_term (Tvar "_s");
              map_mterm m k;
              map_mterm m v
            ])
    | M.Mremovefield (a,f,k,v) ->
      Tapp (loc_term (Tvar ("remove_"^a^"_"^f)),
            [
              loc_term (Tvar "_s");
              map_mterm m k;
              map_mterm m v
            ]
           )
    | M.Msum (a,l,v) ->
      Tapp (loc_term (Tvar ("sum_"^a)),[with_dummy_loc (Tvar (map_lident l)); map_mterm m v])
    | _ -> Tnone in
  mk_loc mt.loc t

let mk_storage_api (m : M.model) records =
  m.api_items |> List.fold_left (fun acc (sc : M.api_item) ->
      match sc.node_item with
      | M.APIStorage (Get n) ->
        let k = M.Utils.get_record_key m (dumloc n) |> snd |> map_btype in
        acc @ [mk_get_asset n k]
      | M.APIStorage (Add n) ->
        let k = M.Utils.get_record_key m (dumloc n) |> fst |> unloc in
        acc @ [mk_add_asset n k]
      | M.APIStorage (Remove n) ->
        let kt = M.Utils.get_record_key m (dumloc n) |> snd |> map_btype in
        acc @ [mk_rm_asset n kt]
      | M.APIStorage (Set n) ->
        let record = get_record n (records |> unloc_decl) in
        let k      = M.Utils.get_record_key m (get_record_name record |> dumloc) |> fst |> unloc in
        acc @ [mk_set_asset k record]
      | M.APIStorage (UpdateAdd (a,pf)) ->
        let k            = M.Utils.get_record_key m (dumloc a) |> fst |> unloc in
        let (pa,addak,_) = M.Utils.get_partition_record_key m (dumloc a) (dumloc pf) in
        acc @ [
          (*mk_add_asset           pa.pldesc addak.pldesc;*)
          mk_add_partition_field a k pf pa.pldesc addak.pldesc
        ]
      | M.APIStorage (UpdateRemove (n,f)) ->
        let t         = M.Utils.get_record_key m (dumloc n) |> snd |> map_btype in
        let (pa,_,pt) = M.Utils.get_partition_record_key m (dumloc n) (dumloc f) in
        acc @ [
          (*mk_rm_asset           pa.pldesc (pt |> map_btype);*)
          mk_rm_partition_field n t f pa.pldesc (pt |> map_btype)
        ]
      | M.APIFunction (Contains n) ->
        let t         =  M.Utils.get_record_key m (dumloc n) |> snd |> map_btype in
        acc @ [ mk_contains n t ]
      | M.APIFunction (Select (asset,test)) ->
        let mlw_test = map_mterm m test in
        acc @ [ mk_select m asset test (mlw_test |> unloc_term) sc.only_formula ]
      | M.APIFunction (Sum (asset,field)) when compare asset "todo" <> 0 ->
        acc @ [ mk_get_field_from_pos asset field;
                mk_sum_clone m asset field ]
      | _ -> acc
    ) [] |> loc_decl |> deloc

(* Entries --------------------------------------------------------------------*)

let fold_exns body : exn list =
  let rec internal_fold_exn acc (term : M.mterm) =
    match term.M.node with
    | M.Mget _ -> acc @ [Enotfound]
    | M.Mfor _ -> acc @ [Enotfound] (* mlw translation generates a get *)
    | M.Maddasset _ -> acc @ [Ekeyexist]
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
      rec_flat (acc@[mk_loc t.loc (Tif (map_mterm m c, map_mterm m th,None))]) e
    | _ -> acc @ [map_mterm m t] in
  mk_loc t.loc (Tseq (rec_flat [] t))

let mk_functions m =
  M.Utils.get_functions m |> List.map (
    fun ((_ : M.verification option),
         (s : M.function_struct),
         (t : M.type_)) ->
      Dfun {
        name     = map_lident s.name;
        logic    = NoMod;
        args     = ([with_dummy_loc "_s",with_dummy_loc Tystorage] @
                    (List.map (fun (i,t,_) ->
                         (map_lident i, map_mtype t)
                       ) s.args));
        returns  = map_mtype t;
        raises   = fold_exns s.body;
        variants = [];
        requires = [];        ensures  = [];
        body     = flatten_if_fail m s.body;
      }
  )

let mk_entries m =
  M.Utils.get_entries m |> List.map (
    fun ((_ : M.verification option),
         (s : M.function_struct)) ->
      Dfun {
        name     = map_lident s.name;
        logic    = NoMod;
        args     = ([with_dummy_loc "_s",with_dummy_loc Tystorage] @
                    (List.map (fun (i,t,_) ->
                         (map_lident i, map_mtype t)
                       ) s.args));
        returns  = with_dummy_loc Tyunit;
        raises   = fold_exns s.body;
        variants = [];
        requires = [];        ensures  = [];
        body     = flatten_if_fail m s.body;
      }
  )

(* ----------------------------------------------------------------------------*)

let to_whyml (m : M.model) : mlw_tree  =
  let storage_module   = with_dummy_loc (String.capitalize_ascii (m.name.pldesc^"_storage")) in
  let uselib           = mk_use in
  let uselist          = mk_use_list in
  let records          = M.Utils.get_records m |> List.map (map_record m) |> wdl in
  let clones           = M.Utils.get_records m  |> List.map (record_to_clone m) |> wdl in
  let init_records     = records |> unloc_decl |> List.map mk_default_init |> loc_decl in
  let records          = zip records clones init_records |> deloc in
  let storage          = M.Utils.get_storage m |> map_storage m in
  (*let axioms           = mk_axioms m in*)
  let partition_axioms = mk_partition_axioms m in
  let storage_api      = mk_storage_api m (records |> wdl) in
  let functions        = mk_functions m in
  let entries          = mk_entries m in
  let usestorage       = mk_use_module storage_module in
  let loct : loc_mlw_tree = [{
      name  = storage_module;
      decls = [uselib;uselist] @
              records          @
              [storage]        @
              (*axioms           @*)
              partition_axioms @
              storage_api;
    };{
       name = cap (map_lident m.name);
       decls = [uselib;uselist;usestorage] @
               functions @
               entries;
     }] in unloc_tree loct
