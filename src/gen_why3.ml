open Location

module M = Model
open Tools
open Mlwtree

(* Utils -----------------------------------------------------------------------*)

let mk_use : decl = Duse ["archetype";"Lib"]

let mk_default_val = function
  | Typartition _ -> Tvar "empty"
  | _ -> Tint 0

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

let mk_diff_set_fields asset = [
  { name = "added_"^asset  ; typ = Tycoll asset ; init = Tvar "empty"; mutable_ = true; };
  { name = "removed_"^asset; typ = Tycoll asset ; init = Tvar "empty"; mutable_ = true; }
]

let mk_asset_fields asset = [
  { name = asset^"_keys"   ; typ = Tycoll asset ; init = Tvar "empty"; mutable_ = true; };
  { name = asset^"_assets" ; typ = Tymap asset  ;
    init = Tvar ("const (mk_default_"^asset^" ())"); mutable_ = true; } ] @
  (mk_diff_set_fields asset)

let mk_const_fields with_trace = [
  { name = "ops_"   ; typ = Tyrecord "transfers" ; init = Tvar "Nil"; mutable_ = true; };
  { name = "get_balance_" ;     typ = Tytez; init = Tint 0; mutable_ = false; };
  { name = "get_transferred_" ; typ = Tytez; init = Tint 0; mutable_ = false; };
  { name = "get_caller_"    ; typ = Tyaddr;  init = Tint 0; mutable_ = false; };
  { name = "get_now_"       ; typ = Tydate;  init = Tint 0; mutable_ = false; };
] @
  if with_trace then
    [
      { name = "tr_"; typ = Tyrecord "Tr.log" ; init = Tvar "Nil"; mutable_ = true; };
      { name = "ename_"; typ = Tyoption (Tyenum "entry"); init = Tnone; mutable_ = true;}
    ]
  else []

let mk_trace_clone = Dclone (["archetype";"Trace"], "Tr",
                             [Ctype ("asset","asset");
                              Ctype ("entry","entry");
                              Ctype ("field","field")])

let mk_sum_clone field = Dclone (["archetype";"Sum"], String.capitalize_ascii field,
                                 [Ctype ("storage","storage_");
                                  Cval ("f","get_"^field)])

let mk_get_field asset ktyp field typ = Dfun {
    name = "get_"^field;
    logic = Logic;
    args = ["s",Tystorage; "k",ktyp];
    returns = typ;
    raises = [];
    variants = [];
    requires = [];
    ensures = [];
    body = Tapp (Tvar field,[Tget (Tdoti ("s",asset^"_assets"),Tvar "k")])
  }

let mk_get_asset asset ktyp = Dfun {
    name = "get_"^asset;
    logic = NoMod;
    args = ["s",Tystorage; "k",ktyp];
    returns = ktyp;
    raises = [ Enotfound ];
    variants = [];
    requires = [];
    ensures = [
      { id   = "get_"^asset^"_post";
        form = Tmem (Tvar "k",Tdoti ("s",asset^"_keys"));
      }
    ];
    body = Tif (Tnot (Tmem (Tvar "k",Tdoti ("s",asset^"_keys"))), Traise Enotfound,
                Some (Tvar "k"))
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
        acc@[f.name,Tapp (Tvar f.name,[Tvar ("new_"^n)])]
    ) []

let mk_update_ensures n key fields =
  snd (List.fold_left (fun (i,acc) (f:field) ->
      if compare f.name key = 0 then
        (i,acc)
      else
        (succ i,acc@[{
             id   = "update_"^n^"_post"^(string_of_int i);
             form = Teq (Tyint, Tapp (Tvar ("get_"^f.name),[Tvar "s";Tvar "k"]),
                         Tapp (Tvar f.name,[Tvar ("new_"^n)]))
           }])
    ) (1,[]) fields)

let mk_update_asset key = function
  | Drecord (n,fields) ->  Dfun {
      name = "update_"^n;
      logic = NoMod;
      args = ["s",Tystorage; "k",get_asset_key_typ key fields; "new_"^n, Tyasset n];
      returns = Tyunit;
      raises = [ Enotfound ];
      variants = [];
      requires = [];
      ensures = mk_update_ensures n key fields;
      body = Tif (Tnot (Tmem (Tvar "k",Tdoti ("s",n^"_keys"))), Traise Enotfound,
                  Some (
                    Tletin (false,"updated_"^n,None,
                            Trecord (Some (Tget (Tdoti ("s",n^"_assets"),Tvar "k")),
                                     mk_update_fields n key fields),
                            Tassign (Tdoti ("s",n^"_assets"),Tset (Tdoti ("s",n^"_assets"),
                                                                   Tvar "k",Tvar ("updated_"^n)))
                           )
                  ))
    }
  | _ -> assert false

(* Filter template -----------------------------------------------------------*)

let mk_filter n typ test : decl = Dfun {
    name = "filter_"^n;
    logic = Logic;
    args = ["s",Tystorage; "c",Tycoll "" ];
    returns = Tycoll "";
    raises = [];
    variants = [];
    requires = [];
    ensures = [
      { id   = "filter_"^n^"_post1";
        form = Tforall ([["k"],typ],Timpl (Tmem (Tvar "k",Tresult),test));
      };
      { id   = "filter_"^n^"_post2";
        form = Tsubset (Tresult,(Tvar "c"));
      }
    ];
    body = Tletfun ({
        name = "rec_filter";
        logic = Rec;
        args = ["l",Tylist Tyint]; (* TODO : should pass asset key type instead *)
        returns = Tylist Tyint;
        raises = [];
        variants = [Tvar "l"];
        requires = [];
        ensures = [
          { id   = "rec_filter_post1";
            form = Tforall ([["k"],typ],Timpl (Tlmem (Tvar "k",Tresult),test));
          };
          { id   = "rec_filter_post2";
            form = Tforall ([["k"],typ],Timpl (Tlmem (Tvar "k",Tresult),Tlmem (Tvar "k",Tvar "l")));
          }];
        body = Tmlist (Tnil,"l","k","tl",
                       Tif(test,Tcons (Tvar "k",Tapp (Tvar ("rec_filter"),[Tvar "tl"])),
                           Some (Tapp (Tvar ("rec_filter"),[Tvar "tl"])))
                      );
      }
                   ,Tapp (Tvar "mkacol",[Tapp (Tvar ("rec_filter"),[Tdoti("c","content")])]));
  }

(* API storage templates -----------------------------------------------------*)

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
let mk_add asset key : decl = Dfun {
    name     = "add_"^asset;
    logic    = NoMod;
    args     = ["s",Tystorage; "new_asset",Tyasset asset];
    returns  = Tyunit;
    raises   = [Ekeyexist];
    variants = [];
    requires = [];
    ensures  = [
      { id   = "add_"^asset^"_post_1";
        form = Tmem (Tdoti ("new_asset",key), Tdoti ("s",asset^"_keys"))
      };
      { id   = "add_"^asset^"_post_2";
        form = Teq (Tycoll asset,Tdoti ("s",asset^"_keys"),
                    Tunion (Tdot (Told (Tvar "s"), Tvar (asset^"_keys")),
                            Tsingl (Tdoti ("new_asset",key))));
      };
      { id   = "add_"^asset^"_post_3";
        form = Teq (Tycoll asset,Tdoti ("s","added_"^asset),
                    Tunion (Tdot (Told (Tvar "s"), Tvar ("added_"^asset)),
                            Tsingl (Tdoti ("new_asset",key))));
      };
      { id   = "add_"^asset^"_post_4";
        form = Tempty (Tinter (Tdot(Told (Tvar "s"),Tvar (asset^"_keys")),
                               Tsingl (Tdoti ("new_asset",key))
                              ));
      };

    ];
    body     = Tseq [
        Tif (Tmem (Tdoti ("new_asset",key), Tdoti ("s",asset^"_keys")),
             Traise Ekeyexist, (* then *)
             Some (Tseq [      (* else *)
                 Tassign (Tdoti ("s",asset^"_assets"),
                          Tset (Tdoti ("s",asset^"_assets"),Tdoti("new_asset",key),Tvar "new_asset"));
                 Tassign (Tdoti ("s",asset^"_keys"),
                          Tadd (Tdoti("new_asset",key),Tdoti ("s",asset^"_keys")));
                 Tassign (Tdoti ("s","added_"^asset),
                          Tadd (Tdoti("new_asset",key),Tdoti ("s","added_"^asset)))
               ]
               ))
      ];
  }

let mk_rm_asset n ktyp : decl = Dfun {
    name     = "remove_"^n;
    logic    = NoMod;
    args     = ["s",Tystorage; "k",ktyp];
    returns  = Tyunit;
    raises   = [Enotfound; Ekeyexist];
    variants = [];
    requires = [];
    ensures  = [
      { id   = "remove_"^n^"_post1";
        form = Tnot (Tmem (Tvar ("k"),Tdoti ("s",n^"_keys")))
      };
      { id   = "remove_"^n^"_post2";
        form = Teq (Tycoll n,Tdoti ("s",n^"_keys"), Tdiff (Tdot(Told (Tvar "s"),Tvar (n^"_keys")),Tsingl (Tvar "k")))
      };
      { id   = "remove_"^n^"_post3";
        form = Teq (Tycoll n,Tdoti ("s","removed_"^n), Tunion (Tdot(Told (Tvar "s"),Tvar ("removed_"^n)),Tsingl (Tvar "k")))
      };
    ];
    body = Tif (Tnot (Tmem (Tvar "k",Tdoti ("s",n^"_keys"))), Traise Enotfound,
                Some (
                  Tseq [
                    Tassign (Tdoti("s",n^"_keys"),Tremove (Tvar "k",Tdoti("s",n^"_keys")));
                    Tassign (Tdoti("s","removed_"^n),Tadd (Tvar "k",Tdoti("s","removed_"^n)))
                  ]));
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
    raises   = [Enotfound;Ekeyexist];
    variants = [];
    requires = [];
    ensures  = [
      { id   = "remove_"^n^"_"^f^"_post1";
        form = Tnot (Tmem (Tvar (rmn^"_k"),Tdoti ("s",rmn^"_keys")))
      };
      { id   = "remove_"^n^"_"^f^"_post2";
        form = Teq (Tycoll rmn,Tdoti ("s",rmn^"_keys"), Tdiff (Tdot(Told (Tvar "s"),Tvar (rmn^"_keys")),Tsingl (Tvar (rmn^"_k"))))
      };
      { id   = "remove_"^n^"_"^f^"_post3";
        form = Teq (Tycoll rmn,Tdoti ("s","removed_"^rmn), Tunion (Tdot(Told (Tvar "s"),Tvar ("removed_"^rmn)),Tsingl (Tvar (rmn^"_k"))))
      };
    ];
    body     = Tif (Tnot (Tmem (Tvar "k",Tdoti ("s",n^"_keys"))), Traise Enotfound,
                    Some (
                      Tletin (false,n^"_asset",None,Tget (Tdoti ("s",n^"_assets"),Tvar "k"),
                              Tletin (false,n^"_"^f,None,Tapp (Tvar f,[Tvar (n^"_asset")]),
                                      Tletin (false,"new_"^n^"_"^f,None,Tremove (Tvar (rmn^"_k"),Tvar (n^"_"^f)),
                                              Tletin (false,"new_"^n^"_asset",None,
                                                      Trecord (Some (Tvar (n^"_asset")),[f,Tvar ("new_"^n^"_"^f)]),
                                                      Tseq [
                                                        Tassign (Tdoti ("s",n^"_assets"),
                                                                 Tset (Tdoti ("s",n^"_assets"),Tvar "k",Tvar ("new_"^n^"_asset")));
                                                        Tapp (Tvar ("remove_"^rmn),[Tvar "s";Tvar (rmn^"_k")])
                                                      ]
                                                     ))))));
  }


(* test -----------------------------------------------------------------------*)

let mk_test_asset_enum = Denum ("asset",["Mile";"Owner"])
let mk_test_entry_enum = Denum ("entry",["Add";"Consume";"ClearExpired"])
let mk_test_field_enum = Denum ("field",["Amount";"Expiration";"Miles"])

let mk_test_mile : decl = Drecord ("mile", [
    { name = "id";         typ = Tystring; init = Tint 0; mutable_ = false; };
    { name = "amount";     typ = Tyint;    init = Tint 0; mutable_ = false; };
    { name = "expiration"; typ = Tydate;   init = Tint 0; mutable_ = false; };
  ])

let mk_test_owner : decl = Drecord ("owner", [
    { name = "addr" ; typ = Tyaddr; init = Tint 0; mutable_ = false; };
    { name = "miles"; typ = Typartition "mile"; init = Tvar "empty"; mutable_ = false; }
  ])

let mk_test_consume : decl = Dfun {
    name     = "consume";
    logic    = NoMod;
    args     = ["s",Tystorage; "ow",Tyaddr; "nbmiles",Tyint];
    returns  = Tytransfers;
    raises   = [Enotfound; Ekeyexist; Einvalidcaller; Einvalidcondition];
    variants = [];
    requires = [
      { id   = "r1";
        form = Teq (Tyoption (Tyenum "entry"),Tdoti ("s","ename_"),Tsome (Tenum "Consume"))
      };
      { id   = "r2";
        form = Tempty (Tdoti ("s","removed_mile"))
      };
      { id   = "r3";
        form = Tempty (Tdoti ("s","added_mile"))
      };
    ];
    ensures  = [];
    body     = Tseq [
        Tif (Tnot (Tmem (Tcaller "s", Tlist [Tdoti("s","admin")])),Traise Einvalidcaller, None);
        Tif (Tle (Tyint,Tvar "nbmiles",Tint 0),Traise Einvalidcondition, None);
        Tletin (false,"o",None,Tapp (Tvar "get_owner",[Tvar "s";Tvar "ow"]),
                Tletin (false,"miles",None,Tapp (Tvar "get_miles",[Tvar "s";Tvar "o"]),
                        Tletin (false,"l",None,Tapp (Tvar "filter_consume",[Tvar "s";Tvar "miles"]),
                                Tseq [
                                  Tif (Tnot (Tle (Tyint, Tvar "nbmiles", Tapp (Tdoti ("Amount","sum"),[Tvar "s";Tvar "l"]))),Traise Einvalidcondition,None);
                                  Tletin (true,"remainder",None,Tvar "nbmiles",
                                          Tseq [
                                            Ttry (
                                              Tfor ("i",Tminus (Tyint,Tcard (Tvar "l"),Tint 1),[
                                                  { id   = "loop_inv1";
                                                    form = Tdle (Tyint,Tint 0,Tvar "remainder",Tapp(Tdoti ("Amount","sum"),[Tvar "s";Ttail (Tvar "i",Tvar "l")]))
                                                  }
                                                ],
                                                    Tletin (false,"m",None,Tnth (Tvar "i",Tvar "l"),
                                                            Tif (Tgt (Tyint,Tapp (Tvar "get_amount",[Tvar "s";Tvar "m"]),Tvar "remainder"),
                                                                 Tletin (false,"new_mile",None,Trecord (Some (Tget (Tdoti ("s","mile_assets"),Tvar "m")),["amount",Tminus (Tyint,Tapp (Tvar "get_amount",[Tvar "s";Tvar "m"]),Tvar "remainder")]),
                                                                         Tseq [Tapp (Tvar "update_mile",[Tvar "s";Tvar "m";Tvar "new_mile"]);
                                                                               Tassign (Tvar "remainder",Tint 0);
                                                                               Traise Ebreak]),
                                                                 Some (Tif (Teq (Tyint,Tapp (Tvar "get_amount",[Tvar "s";Tvar "m"]),Tvar "remainder"),
                                                                            Tseq [Tassign (Tvar "remainder",Tint 0);
                                                                                  Tapp (Tvar "remove_owner_miles",[Tvar "s";Tvar "o";Tvar "m"]);
                                                                                  Traise Ebreak],
                                                                            Some (Tseq [
                                                                                Tassign (Tvar "remainder",Tminus (Tyint,Tvar "remainder",Tapp (Tvar "get_amount",[Tvar "s";Tvar "m"])));
                                                                                Tapp (Tvar "remove_owner_miles",[Tvar "s";Tvar "o";Tvar "m"])
                                                                              ]))))
                                                           )
                                                ),
                                              Ebreak,Tassert (Teq (Tyint,Tvar "remainder",Tint 0)));
                                            Tassert (Teq (Tyint,Tvar "remainder",Tint 0));
                                            Tvar "no_transfer"])
                                ])))
      ]
  }

let mk_test_storage : decl = Dstorage {
    fields = [
      { name = "admin" ; typ = Tyrole ; init = Tint 0; mutable_ = true; }
    ] @
      (mk_asset_fields "mile") @
      (mk_asset_fields "owner") @
      (mk_const_fields true)
    ;
    invariants = [
      { id   = "inv1";
        form = Tforall ([["k"],Tystring],
                        Timpl (Tmem (Tvar "k", Tvar "mile_keys"),
                               Tgt (Tyint, Tapp (Tvar "amount",
                                                 [Tget (Tvar "mile_assets",
                                                        Tvar "k")]), Tint 0)))
      };

    ];
  }

let mk_test_mlwtree : mlw_tree =   {
  name = "Miles_with_expiration_storage";
  decls = [
    mk_use;
    mk_test_asset_enum;
    mk_test_entry_enum;
    mk_test_field_enum;
    mk_trace_clone;
    mk_test_mile;
    mk_default_init mk_test_mile;
    mk_test_owner;
    mk_default_init mk_test_owner;
    mk_test_storage;
    mk_get_field "mile" Tystring "amount" Tyint;
    mk_get_field "mile" Tystring "expiration" Tydate;
    mk_get_field "owner" Tyaddr "miles" (Tycoll "");
    mk_update_asset "id" mk_test_mile;
    mk_get_asset "mile" Tystring;
    mk_get_asset "owner" Tystring;
    mk_sum_clone "amount";
    mk_filter "consume" (Tyint) (Tlt (Tyint,Tapp (Tvar "get_expiration",[Tvar "s";Tvar "k"]),Tnow "s"));
    mk_add "mile" "id";
    mk_add "owner" "addr";
    mk_rm_asset "mile" Tystring;
    mk_rm_partition_field "owner" Tyaddr "miles" "mile" Tystring;
    mk_test_consume;
  ];
}

(* ----------------------------------------------------------------------------*)

let wdl        = List.map with_dummy_loc
let unloc_decl = List.map unloc_decl
let loc_decl   = List.map loc_decl
let loc_field  = List.map loc_field
let deloc (l : 'a list) = List.map deloc l

let zip l1 l2 = List.concat (List.map2 (fun a b -> [a]@[b]) l1 l2)

let cap s = mk_loc s.loc (String.capitalize_ascii s.obj)

(* ----------------------------------------------------------------------------*)

let map_lident (i : M.lident) : loc_ident = {
  obj = i.pldesc;
  loc = i.plloc;
}

let type_to_init (typ : loc_typ) : loc_term =
  mk_loc typ.loc (match typ.obj with
      | Typartition i -> Tvar (mk_loc typ.loc "empty")
      | Tycoll i      -> Tvar (mk_loc typ.loc "empty")
      | Tymap i       -> Tvar (mk_loc typ.loc ("const (mk_default_"^i.obj^" ())"))
      | _             -> Tint 0)

let map_type (typ : M.type_) : loc_typ =
  let rec rec_map_type = function
    | M.Tasset i                 -> Tyasset (map_lident i)
    | M.Tenum i                  -> Tyenum (map_lident i)
    | M.Tcontract i              -> Tycontract (map_lident i)
    | M.Tbuiltin Bbool        -> Tybool
    | M.Tbuiltin Bint         -> Tyint
    | M.Tbuiltin Buint        -> Tyuint
    | M.Tbuiltin Brational    -> Tyrational
    | M.Tbuiltin Bdate        -> Tydate
    | M.Tbuiltin Bduration    -> Tyduration
    | M.Tbuiltin Bstring      -> Tystring
    | M.Tbuiltin Baddress     -> Tyaddr
    | M.Tbuiltin Brole        -> Tyrole
    | M.Tbuiltin (Bcurrency _)  -> Tytez
    | M.Tbuiltin Bkey           -> Tykey
    | M.Tcontainer (M.Tasset i, M.Partition) -> Typartition (map_lident i)
    | M.Tcontainer _             -> Typartition (with_dummy_loc "NOT TRANSLATED")
    | M.Ttuple l                 -> Tytuple (List.map rec_map_type l)
    | M.Tprog _
    | M.Tvset _
    | M.Ttrace _ -> (* TODO *) assert false
  in
  with_dummy_loc (rec_map_type typ)

let map_basic_type (typ : 'id M.item_field_type) : loc_typ =
  let rec_map_basic_type = function
    | M.FBasic Bbool        -> Tybool
    | M.FBasic Bint         -> Tyint
    | M.FBasic Buint        -> Tyuint
    | M.FBasic Brational    -> Tyrational
    | M.FBasic Bdate        -> Tydate
    | M.FBasic Bduration    -> Tyduration
    | M.FBasic Bstring      -> Tystring
    | M.FBasic Baddress     -> Tyaddr
    | M.FBasic Brole        -> Tyrole
    | M.FBasic Bcurrency _  -> Tytez
    | M.FBasic Bkey         -> Tykey
    | M.FAssetKeys (_,i)    -> Tycoll (map_lident i)
    | M.FAssetRecord (_,i)  -> Tymap (map_lident i)
    | M.FRecordCollection i -> Tymap (map_lident i) (* ? *)
    | M.FRecord i           -> Tyrecord (map_lident i)
    | M.FEnum i             -> Tyenum (map_lident i)
    | _ -> assert false in
  with_dummy_loc (rec_map_basic_type typ)

let map_bval (b : M.lit_value) : loc_term = mk_loc (Location.dummy) (
    match b with
    | M.BVaddress v -> Tint (sha v)
    | M.BVint i     -> Tint (Big_int.int_of_big_int i)
    | _ -> Tnottranslated
  )

let rec map_term (t : 'id M.mterm_gen) : loc_term = mk_loc t.loc (
    match t.node with
    | M.Mlit b -> map_bval b |> Mlwtree.deloc
    | M.Mvar i -> Tvar (map_lident i)
    | M.Mcomp (M.Gt,t1,t2) -> Tgt (with_dummy_loc Tyint,map_term t1,map_term t2)
    | _ -> Tnottranslated
  )

let map_record_term _ = map_term

let map_record_values (values : 'id M.record_item list) =
  List.map (fun (value : 'id M.record_item) ->
      let typ_ = map_type value.type_ in
      let init_value = type_to_init typ_ in {
        name     = map_lident value.name;
        typ      = typ_;
        init     = Option.fold map_record_term init_value value.default;
        mutable_ = false;
      }
    ) values

let map_storage_items = List.fold_left (fun acc (items : 'id M.storage_item) ->
    let extra_fields =
      if List.length items.fields > 1 (* this is the way to detect assets ... *)
      then (mk_diff_set_fields items.name.pldesc) |> loc_field |> deloc
      else []
    in
    (List.fold_left (fun acc (item : 'id M.item_field) ->
         let typ_ = map_basic_type item.typ in
         let init_value = type_to_init typ_ in
         acc @[{
             name     = map_lident item.name;
             typ      = typ_;
             init     = Option.fold map_record_term init_value item.default;
             mutable_ = true;
           }]
       ) acc items.fields) @ extra_fields
  ) []

let map_label_term (lt : M.lident M.label_term_gen) : (loc_term,loc_ident) abstract_formula = {
  id = Option.fold (fun _ x -> map_lident x)  (with_dummy_loc "") lt.label;
  form = map_term lt.term;
}

let map_decl (d : 'id M.decl_node) =
  match d with
  | M.TNrecord r -> Drecord (map_lident r.name, map_record_values r.values)
  | M.TNstorage l -> Dstorage {
      fields     = (map_storage_items l)@(mk_const_fields false |> loc_field |> deloc);
      invariants = List.concat (List.map (fun (item : 'id M.storage_item) ->
          List.map map_label_term item.invariants) l)
    }
  | _ -> assert false

let is_record (d : 'id M.decl_node) : bool =
  match d with
  | M.TNrecord _ -> true
  | _            -> false

let get_records = List.filter is_record

let is_storage (d : 'id M.decl_node) : bool =
  match d with
  | M.TNstorage _ -> true
  | _             -> false

let get_storage = List.filter is_storage

(* ----------------------------------------------------------------------------*)

let to_whyml (model : M.model) : mlw_tree  =
  let uselib       = mk_use |> Mlwtree.loc_decl |> Mlwtree.deloc in
  let records      = get_records model.decls |> List.map map_decl |> wdl in
  let init_records = records |> unloc_decl |> List.map mk_default_init |> loc_decl in
  let records      = zip records init_records |> deloc in
  let storage      = get_storage model.decls |> List.map map_decl in
  let loct : loc_mlw_tree = {
    name = cap (map_lident model.name);
    decls =  uselib :: (records @ storage);
  } in unloc_tree loct
