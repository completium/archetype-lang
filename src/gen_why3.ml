open Location

module M = Model
open Tools
open Mlwtree

(* Utils -----------------------------------------------------------------------*)

let mk_use = Duse ["archetype";"Lib"] |> loc_decl |> deloc
let mk_use_module m = Duse [deloc m]  |> loc_decl |> deloc

let mk_default_val = function
  | Typartition _ -> Tvar "empty"
  | _ -> Tint Big_int.zero_big_int

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
  { name = "get_balance_" ;     typ = Tytez; init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = "get_transferred_" ; typ = Tytez; init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = "get_caller_"    ; typ = Tyaddr;  init = Tint Big_int.zero_big_int; mutable_ = false; };
  { name = "get_now_"       ; typ = Tydate;  init = Tint Big_int.zero_big_int; mutable_ = false; };
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
        acc@[f.name,Tapp (Tvar f.name,[Tvar ("new_asset")])]
    ) []

let mk_update_ensures n key fields =
  snd (List.fold_left (fun (i,acc) (f:field) ->
      if compare f.name key = 0 then
        (i,acc)
      else
        (succ i,acc@[{
             id   = "update_"^n^"_post"^(string_of_int i);
             form = Teq (Tyint, Tapp (Tvar ("get_"^f.name),[Tvar "s";Tvar "k"]),
                         Tapp (Tvar f.name,[Tvar ("new_asset")]))
           }])
    ) (1,[]) fields)

let mk_update_asset key = function
  | Drecord (n,fields) ->  Dfun {
      name = "update_"^n;
      logic = NoMod;
      args = ["s",Tystorage; "k",get_asset_key_typ key fields; "new_asset", Tyasset n];
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

(* f --> f (get a_assets k) *)
let mk_app_field (a : loc_ident) (f : loc_ident) : loc_term * loc_term  =
  let arg   : term     = Tget (Tvar ((deloc a)^"_assets"), Tvar "k_") in
  let loc_f : loc_term = mk_loc f.loc (Tvar f) in
  (loc_f,with_dummy_loc (Tapp (loc_f,[loc_term arg])))

(* n is the asset name
   f is the name of the key field
   ktyp is the key type
*)
let mk_keys_eq_axiom n f ktyp : decl =
  Daxiom ("eq_"^n^"_keys",
          Tforall ([["s"],Tystorage;["k"],ktyp],
                   Timpl (Tmem (Tvar "k",Tdoti ("s",n^"_keys")),
                          Teq (Tyint,
                               Tapp (Tvar f,[Tget (Tdoti ("s",n^"_assets"),Tvar "k")]),
                               Tvar "k"))))

(* n is the asset name
   f is the partition field name
   kt is the key type
   pa is the partitionned asset name
   kpt is the partionned asset key type
*)
let mk_partition_axiom n f kt pa kpt : decl =
  Daxiom (n^"_"^f^"_is_partition",
          Tforall ([["s"],Tystorage;["k1"],kt;["k2"],kpt],
                   Timpl (Tmem (Tvar("k1"),Tdoti ("s",n^"_keys")),
                          Timpl (Tmem (Tvar "k2",
                                       Tapp (Tvar f,
                                             [Tget (Tdoti ("s",n^"_assets"),Tvar "k1")])),
                                 Tmem (Tvar "k2",Tdoti ("s",pa^"_keys"))))))

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
    body = Tletfun (
        {
          name = "rec_filter";
          logic = Rec;
          args = ["l",Tylist Tyint]; (* TODO : should pass asset key type instead *)
          returns = Tylist Tyint;
          raises = [];
          variants = [Tvar "l"];
          requires = [];
          ensures = [
            { id   = "rec_filter_post1";
              form = Tforall ([["k"],typ],
                              Timpl (Tlmem (Tvar "k",Tresult),
                                     test));
            };
            { id   = "rec_filter_post2";
              form = Tforall ([["k"],typ],
                              Timpl (Tlmem (Tvar "k",Tresult),
                                     Tlmem (Tvar "k",Tvar "l")));
            }];
          body = Tmlist (Tnil,"l","k","tl",
                         Tif(test,
                             Tcons (Tvar "k",Tapp (Tvar ("rec_filter"),[Tvar "tl"])),
                             Some (Tapp (Tvar ("rec_filter"),[Tvar "tl"])))
                        );
        },
        Tapp (Tvar "mkacol",
              [Tapp (Tvar ("rec_filter"),[Tdoti("c","content")])])
      );
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
        form = Tmem (Tdoti ("new_asset",key),
                     Tdoti ("s",asset^"_keys"))
      };
      { id   = "add_"^asset^"_post_2";
        form = Teq (Tycoll asset,
                    Tdoti ("s",asset^"_keys"),
                    Tunion (Tdot (Told (Tvar "s"),
                                  Tvar (asset^"_keys")),
                            Tsingl (Tdoti ("new_asset",key))));
      };
      { id   = "add_"^asset^"_post_3";
        form = Teq (Tycoll asset,
                    Tdoti ("s","added_"^asset),
                    Tunion (Tdot (Told (Tvar "s"),
                                  Tvar ("added_"^asset)),
                            Tsingl (Tdoti ("new_asset",key))));
      };
      { id   = "add_"^asset^"_post_4";
        form = Tempty (Tinter (Tdot(Told (Tvar "s"),Tvar (asset^"_keys")),
                               Tsingl (Tdoti ("new_asset",key))
                              ));
      };

    ];
    body     = Tseq [
        Tif (Tmem (Tdoti ("new_asset",key),
                   Tdoti ("s",asset^"_keys")),
             Traise Ekeyexist, (* then *)
             Some (Tseq [      (* else *)
                 Tassign (Tdoti ("s",asset^"_assets"),
                          Tset (Tdoti ("s",asset^"_assets"),
                                Tdoti("new_asset",key),
                                Tvar "new_asset"));
                 Tassign (Tdoti ("s",asset^"_keys"),
                          Tadd (Tdoti("new_asset",key),
                                Tdoti ("s",asset^"_keys")));
                 Tassign (Tdoti ("s","added_"^asset),
                          Tadd (Tdoti("new_asset",key),
                                Tdoti ("s","added_"^asset)))
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
        form = Teq (Tycoll n,
                    Tdoti ("s",n^"_keys"),
                    Tdiff (Tdot(Told (Tvar "s"),
                                Tvar (n^"_keys")),
                           Tsingl (Tvar "k")))
      };
      { id   = "remove_"^n^"_post3";
        form = Teq (Tycoll n,
                    Tdoti ("s","removed_"^n),
                    Tunion (Tdot(Told (Tvar "s"),
                                 Tvar ("removed_"^n)),
                            Tsingl (Tvar "k")))
      };
    ];
    body = Tif (Tnot (Tmem (Tvar "k",
                            Tdoti ("s",n^"_keys"))),
                Traise Enotfound,
                Some (
                  Tseq [
                    Tassign (Tdoti("s",n^"_keys"),
                             Tremove (Tvar "k",
                                      Tdoti("s",
                                            n^"_keys")));
                    Tassign (Tdoti("s","removed_"^n),
                             Tadd (Tvar "k",
                                   Tdoti("s",
                                         "removed_"^n)))
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
        form = Tmem (addak,Tdoti ("s",adda^"_keys"))
      };
      { id   = "add_"^a^"_"^pf^"_post2";
        form = Teq (Tycoll adda,
                    Tdoti ("s",adda^"_keys"),
                    Tunion (Tdot(Told (Tvar "s"),
                                 Tvar (adda^"_keys")),
                            Tsingl (addak)))
      };
      { id   = "add_"^a^"_"^pf^"_post3";
        form = Teq (Tycoll adda,
                    Tdoti ("s","added_"^adda),
                    Tunion (Tdot(Told (Tvar "s"),
                                 Tvar ("added_"^adda)),
                            Tsingl (addak)))
      };
    ];
    body     =
      Tif (Tnot (Tmem (akey,Tdoti ("s",a^"_keys"))), Traise Enotfound,
           Some (Tseq [
               Tapp (Tvar ("add_"^adda),[Tvar "s";Tvar "new_asset"]);
               Tletin (false, a^"_"^pf,None,
                       Tapp (Tvar pf,[Tvar "asset"]),
                       Tletin (false,"new_"^a^"_"^pf,None,
                               Tadd (addak,Tvar (a^"_"^pf)),
                               Tletin (false,"new_asset",None,
                                       Trecord (Some (Tvar "asset"),
                                                [pf,Tvar ("new_"^a^"_"^pf)]),
                                       Tassign (Tdoti ("s",a^"_assets"),
                                                Tset (Tdoti ("s",a^"_assets"),
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
    raises   = [Enotfound;Ekeyexist];
    variants = [];
    requires = [];
    ensures  = [
      { id   = "remove_"^n^"_"^f^"_post1";
        form = Tnot (Tmem (Tvar (rmn^"_k"),Tdoti ("s",rmn^"_keys")))
      };
      { id   = "remove_"^n^"_"^f^"_post2";
        form = Teq (Tycoll rmn,
                    Tdoti ("s",rmn^"_keys"),
                    Tdiff (Tdot(Told (Tvar "s"),
                                Tvar (rmn^"_keys")),
                           Tsingl (Tvar (rmn^"_k"))))
      };
      { id   = "remove_"^n^"_"^f^"_post3";
        form = Teq (Tycoll rmn,
                    Tdoti ("s","removed_"^rmn),
                    Tunion (Tdot(Told (Tvar "s"),
                                 Tvar ("removed_"^rmn)),
                            Tsingl (Tvar (rmn^"_k"))))
      };
    ];
    body     =
      Tif (Tnot (Tmem (Tvar "k",Tdoti ("s",n^"_keys"))), Traise Enotfound,
           Some (
             Tletin (false,n^"_asset",None,
                     Tget (Tdoti ("s",n^"_assets"),Tvar "k"),
                     Tletin (false,n^"_"^f,None,
                             Tapp (Tvar f,[Tvar (n^"_asset")]),
                             Tletin (false,"new_"^n^"_"^f,None,
                                     Tremove (Tvar (rmn^"_k"),
                                              Tvar (n^"_"^f)),
                                     Tletin (false,"new_"^n^"_asset",None,
                                             Trecord (Some (Tvar (n^"_asset")),
                                                      [f,Tvar ("new_"^n^"_"^f)]),
                                             Tseq [
                                               Tassign (Tdoti ("s",n^"_assets"),
                                                        Tset (Tdoti ("s",n^"_assets"),
                                                              Tvar "k",
                                                              Tvar ("new_"^n^"_asset")));
                                               Tapp (Tvar ("remove_"^rmn),
                                                     [Tvar "s";Tvar (rmn^"_k")])
                                             ]
                                            ))))));
  }


(* test -----------------------------------------------------------------------*)

let mk_test_asset_enum = Denum ("asset",["Mile";"Owner"])
let mk_test_entry_enum = Denum ("entry",["Add";"Consume";"ClearExpired"])
let mk_test_field_enum = Denum ("field",["Amount";"Expiration";"Miles"])

let mk_test_mile : decl = Drecord ("mile", [
    { name = "id";         typ = Tystring; init = Tint Big_int.zero_big_int; mutable_ = false; };
    { name = "amount";     typ = Tyint;    init = Tint Big_int.zero_big_int; mutable_ = false; };
    { name = "expiration"; typ = Tydate;   init = Tint Big_int.zero_big_int; mutable_ = false; };
  ])

let mk_test_owner : decl = Drecord ("owner", [
    { name = "addr" ; typ = Tyaddr; init = Tint Big_int.zero_big_int; mutable_ = false; };
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
        Tif (Tle (Tyint,Tvar "nbmiles",Tint Big_int.zero_big_int),Traise Einvalidcondition, None);
        Tletin (false,"o",None,Tapp (Tvar "get_owner",[Tvar "s";Tvar "ow"]),
                Tletin (false,"miles",None,Tapp (Tvar "get_miles",[Tvar "s";Tvar "o"]),
                        Tletin (false,"l",None,Tapp (Tvar "filter_consume",[Tvar "s";Tvar "miles"]),
                                Tseq [
                                  Tif (Tnot (Tle (Tyint, Tvar "nbmiles", Tapp (Tdoti ("Amount","sum"),[Tvar "s";Tvar "l"]))),Traise Einvalidcondition,None);
                                  Tletin (true,"remainder",None,Tvar "nbmiles",
                                          Tseq [
                                            Ttry (
                                              Tfor ("i",Tminus (Tyint,Tcard (Tvar "l"),Tint Big_int.unit_big_int),[
                                                  { id   = "loop_inv1";
                                                    form = Tdle (Tyint,Tint Big_int.zero_big_int,Tvar "remainder",Tapp(Tdoti ("Amount","sum"),[Tvar "s";Ttail (Tvar "i",Tvar "l")]))
                                                  }
                                                ],
                                                    Tletin (false,"m",None,Tnth (Tvar "i",Tvar "l"),
                                                            Tif (Tgt (Tyint,Tapp (Tvar "get_amount",[Tvar "s";Tvar "m"]),Tvar "remainder"),
                                                                 Tletin (false,"new_mile",None,Trecord (Some (Tget (Tdoti ("s","mile_assets"),Tvar "m")),["amount",Tminus (Tyint,Tapp (Tvar "get_amount",[Tvar "s";Tvar "m"]),Tvar "remainder")]),
                                                                         Tseq [Tapp (Tvar "update_mile",[Tvar "s";Tvar "m";Tvar "new_mile"]);
                                                                               Tassign (Tvar "remainder",Tint Big_int.zero_big_int);
                                                                               Traise Ebreak]),
                                                                 Some (Tif (Teq (Tyint,Tapp (Tvar "get_amount",[Tvar "s";Tvar "m"]),Tvar "remainder"),
                                                                            Tseq [Tassign (Tvar "remainder",Tint Big_int.zero_big_int);
                                                                                  Tapp (Tvar "remove_owner_miles",[Tvar "s";Tvar "o";Tvar "m"]);
                                                                                  Traise Ebreak],
                                                                            Some (Tseq [
                                                                                Tassign (Tvar "remainder",Tminus (Tyint,Tvar "remainder",Tapp (Tvar "get_amount",[Tvar "s";Tvar "m"])));
                                                                                Tapp (Tvar "remove_owner_miles",[Tvar "s";Tvar "o";Tvar "m"])
                                                                              ]))))
                                                           )
                                                ),
                                              Ebreak,Tassert (Teq (Tyint,Tvar "remainder",Tint Big_int.zero_big_int)));
                                            Tassert (Teq (Tyint,Tvar "remainder",Tint Big_int.zero_big_int));
                                            Tvar "no_transfer"])
                                ])))
      ]
  }

let mk_test_storage : decl = Dstorage {
    fields = [
      { name = "admin" ; typ = Tyrole ; init = Tint Big_int.zero_big_int; mutable_ = true; }
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
                                                        Tvar "k")]), Tint Big_int.zero_big_int)))
      };

    ];
  }

let mk_test_mlwtree : mlw_tree =   [{
    name = "Miles_with_expiration_storage";
    decls = [
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
      mk_add_asset "mile" "id";
      mk_add_asset "owner" "addr";
      mk_rm_asset "mile" Tystring;
      mk_rm_partition_field "owner" Tyaddr "miles" "mile" Tystring;
      mk_test_consume;
    ];
  }]

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

let map_lidents = List.map map_lident

let type_to_init (typ : loc_typ) : loc_term =
  mk_loc typ.loc (match typ.obj with
      | Typartition i -> Tvar (mk_loc typ.loc "empty")
      | Tycoll i      -> Tvar (mk_loc typ.loc "empty")
      | Tymap i       -> Tvar (mk_loc typ.loc ("const (mk_default_"^i.obj^" ())"))
      | _             -> Tint Big_int.zero_big_int)

let map_btype = function
  | M.Bbool          -> Tybool
  | M.Bint           -> Tyint
  | M.Buint          -> Tyuint
  | M.Brational      -> Tyrational
  | M.Bdate          -> Tydate
  | M.Bduration      -> Tyduration
  | M.Bstring        -> Tystring
  | M.Baddress       -> Tyaddr
  | M.Brole          -> Tyrole
  | M.Bcurrency _    -> Tytez
  | M.Bkey           -> Tykey

let rec map_type (typ : M.type_) : loc_typ =
  let rec rec_map_type = function
    | M.Tasset i                 -> Tyasset (map_lident i)
    | M.Tenum i                  -> Tyenum (map_lident i)
    | M.Tcontract i              -> Tycontract (map_lident i)
    | M.Tbuiltin vt              -> map_btype vt
    | M.Tcontainer (M.Tasset i,M.Partition) -> Typartition (map_lident i)
    | M.Tcontainer _             -> Typartition (with_dummy_loc "NOT TRANSLATED")
    | M.Toption ty               -> assert false
    | M.Ttuple l                 -> Tytuple (List.map rec_map_type l)
    | M.Tentry                   -> Typartition (with_dummy_loc "NOT TRANSLATED")
    | M.Tprog t                  -> Mlwtree.deloc (map_type t)
    | M.Tvset (_,t)              ->  Typartition (with_dummy_loc "NOT TRANSLATED")
    | M.Ttrace trtyp             -> Typartition (with_dummy_loc "NOT TRANSLATED")
    | M.Tunit                    -> Typartition (with_dummy_loc "NOT TRANSLATED")
  in
  with_dummy_loc (rec_map_type typ)

let map_basic_type (typ : 'id M.item_field_type) : loc_typ =
  let rec_map_basic_type = function
    | M.FBasic vt           -> map_btype vt
    | M.FAssetKeys (_,i)    -> Tycoll (map_lident i)
    | M.FAssetRecord (_,i)  -> Tymap (map_lident i)
    | M.FRecordCollection i -> Tymap (map_lident i) (* ? *)
    | M.FRecord i           -> Tyrecord (map_lident i)
    | M.FEnum i             -> Tyenum (map_lident i)
    | _ -> assert false in
  with_dummy_loc (rec_map_basic_type typ)

let rec map_term (t : M.mterm) : loc_term = mk_loc t.loc (
    match t.node with
    | M.Maddress v  -> Tint (sha v)
    | M.Mint i      -> Tint i
    | M.Mvarlocal i -> Tvar (map_lident i)
    | M.Mgt (t1,t2) -> Tgt (with_dummy_loc Tyint,map_term t1,map_term t2)
    | _ -> Tnottranslated
  )

let map_record_term _ = map_term

let map_record_values (values : M.record_item list) =
  List.map (fun (value : M.record_item) ->
      let typ_ = map_type value.type_ in
      let init_value = type_to_init typ_ in {
        name     = map_lident value.name;
        typ      = typ_;
        init     = Option.fold map_record_term init_value value.default;
        mutable_ = false;
      }
    ) values

let map_storage_items = List.fold_left (fun acc (items : M.storage_item) ->
    let extra_fields =
      if List.length items.fields > 1 (* this is the way to detect assets ... *)
      then (mk_diff_set_fields items.name.pldesc) |> loc_field |> deloc
      else []
    in
    (List.fold_left (fun acc (item : M.item_field) ->
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

(* prefixes with 'forall k_:key, mem k_ "asset"_keys ->  ...'
   replaces asset field "field" by '"field " (get "asset"_assets k_)'
   TODO : make sure there is no collision between "k_" and invariant vars

   m is the Model
   n is the asset name
   inv is the invariant to extend
*)
let mk_extended_invariant m n inv : loc_term =
  let r        = M.Utils.get_record m n in
  let ktyp     = M.Utils.get_record_key m n |> snd |> map_btype in
  let fields   = r.values |> List.map (fun (item : M.record_item) -> item.name) |> map_lidents in
  let asset    = map_lident n in
  let replacements = List.map (fun f -> mk_app_field asset f) fields in
  let replaced = List.fold_left (fun acc (t1,t2) -> loc_replace t1 t2 acc) inv replacements in
  let prefix   = Tforall ([["k_"],ktyp],
                          Timpl (Tmem (Tvar "k_", Tvar (asset.obj^"_keys")),
                                 Ttobereplaced)) in
  loc_replace (with_dummy_loc Ttobereplaced) replaced (loc_term prefix)

let map_extended_label_term m n (lt : M.label_term) = {
  id = Option.fold (fun _ x -> map_lident x)  (with_dummy_loc "") lt.label;
  form = mk_extended_invariant m n (map_term lt.term);
}

let map_record m (r : M.record) =
  Drecord (map_lident r.name, map_record_values r.values)

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

let mk_record_get_fields m (r : M.record) =
  let k,kt = M.Utils.get_record_key m r.name |> fun (x,y) -> (x, map_btype y) in
  List.fold_left (fun acc (item : M.record_item) ->
      if compare k.pldesc item.name.pldesc = 0 then
        acc
      else
        let ft = unloc_type (map_type item.type_) in
        acc @[mk_get_field r.name.pldesc kt item.name.pldesc ft]
    ) [] r.values

let mk_get_fields (m : M.model) =
  M.Utils.get_records m |> List.map (mk_record_get_fields m) |> List.concat |> loc_decl |> deloc

let rec get_record id = function
  | Drecord (n,_) as r :: tl when compare id n = 0 -> r
  | _ :: tl -> get_record id tl
  | [] -> assert false

let get_record_name = function
  | Drecord (n,_) -> n
  | _ -> assert false

let mk_storage_api (m : M.model) records =
  m.api_items |> List.fold_left (fun acc (sc : M.api_item) ->
      match sc.node with
      | M.APIStorage (Get n) ->
        let k = M.Utils.get_record_key m n |> snd |> map_btype in
        acc @ [mk_get_asset n.pldesc k]
      | M.APIStorage (Add n) ->
        let k = M.Utils.get_record_key m n |> fst |> unloc in
        acc @ [mk_add_asset n.pldesc k]
      | M.APIStorage (Set n) ->
        let record = get_record n.pldesc (records |> unloc_decl) in
        let k      = M.Utils.get_record_key m (get_record_name record |> dumloc) |> fst |> unloc in
        acc @ [mk_update_asset k record]
      | M.APIStorage (UpdateAdd (a,pf)) ->
        let k            = M.Utils.get_record_key m a |> fst |> unloc in
        let (pa,addak,_) = M.Utils.get_partition_record_key m a pf in
        acc @ [
          mk_add_asset           pa.pldesc addak.pldesc;
          mk_add_partition_field a.pldesc k pf.pldesc pa.pldesc addak.pldesc
        ]
      | M.APIStorage (UpdateRemove (n,f)) ->
        let t         = M.Utils.get_record_key m n |> snd |> map_btype in
        let (pa,_,pt) = M.Utils.get_partition_record_key m n f in
        acc @ [
          mk_rm_asset           pa.pldesc (pt |> map_btype);
          mk_rm_partition_field n.pldesc t f.pldesc pa.pldesc (pt |> map_btype)
        ]
      | _ -> acc
    ) [] |> loc_decl |> deloc

let mk_entries m =
  M.Utils.get_entries m |> List.map (fun ((_ : M.verification option),
                                          (s : M.function_struct)) ->
                                      Dfun {
                                        name     = map_lident s.name |> unloc_ident;
                                        logic    = NoMod;
                                        args     = [];
                                        returns  = Tytransfers;
                                        raises   = [];
                                        variants = [];
                                        requires = [];
                                        ensures  = [];
                                        body     = Tnone;
                                      }) |> loc_decl |> deloc

(* ----------------------------------------------------------------------------*)

let to_whyml (m : M.model) : mlw_tree  =
  let storage_module   = with_dummy_loc (String.capitalize_ascii (m.name.pldesc^"_storage")) in
  let uselib           = mk_use in
  let records          = M.Utils.get_records m |> List.map (map_record m) |> wdl in
  let init_records     = records |> unloc_decl |> List.map mk_default_init |> loc_decl in
  let records          = zip records init_records |> deloc in
  let storage          = M.Utils.get_storage m |> map_storage m in
  let axioms           = mk_axioms m in
  let partition_axioms = mk_partition_axioms m in
  let get_fields       = mk_get_fields m in
  let storage_api      = mk_storage_api m (records |> wdl) in
  let entries          = mk_entries m in
  let usestorage       = mk_use_module storage_module in
  let loct : loc_mlw_tree = [{
      name  = storage_module;
      decls = [uselib]         @
              records          @
              [storage]        @
              axioms           @
              partition_axioms @
              get_fields       @
              storage_api;
    };{
       name = cap (map_lident m.name);
       decls = [usestorage] @
               entries;
     }] in unloc_tree loct
