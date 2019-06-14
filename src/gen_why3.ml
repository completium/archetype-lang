open Location

module M = Model
open Mlwtree

(* Utils -----------------------------------------------------------------------*)

let mk_default_val = function
  | Typartition _ -> Tvar "empty"
  | _ -> Tint 0

let mk_default_init = function
  | Drecord (n,fs) ->
    Dfun {
      name     = "mk_default_"^n;
      logic    = false;
      args     = [];
      returns  = Tyasset n;
      raises   = [];
      requires = [];
      ensures  = [];
      body     = Trecord (None, List.map (fun (f:field) ->
          f.name, f.init
        ) fs);
    }
  | _ -> assert false

let mk_asset_fields asset = [
  { name = asset^"_keys"   ; typ = Tycoll asset ; init = Tvar "empty"; mutable_ = true; };
  { name = asset^"_assets" ; typ = Tymap asset  ;
    init = Tvar ("const (mk_default_"^asset^" ())"); mutable_ = true; };
  { name = "added_"^asset  ; typ = Tycoll asset ; init = Tvar "empty"; mutable_ = true; };
  { name = "removed_"^asset; typ = Tycoll asset ; init = Tvar "empty"; mutable_ = true; }
]

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
      { name = "ename"; typ = Tyoption (Tyenum "entry"); init = Tnone; mutable_ = true;}
    ]
    else []

let mk_trace_clone = Dclone (["archetype";"Trace"], "Tr",
                             [Ctype ("asset","asset");
                              Ctype ("entry","entry");
                              Ctype ("field","field")])

(* API storage templates -----------------------------------------------------*)

(* basic getters *)

let gen_field_getter n field = Dfun {
    name     = n;
    logic    = true;
    args     = [];
    returns  = Tyunit;
    raises   = [];
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
    logic    = false;
    args     = ["s",Tystorage; "new_asset",Tyasset asset];
    returns  = Tyunit;
    raises   = [Ekeyexist];
    requires = [];
    ensures  = [
      { id   = "add_"^asset^"_post_1";
        body = Tmem (Tdoti ("new_asset",key), Tdoti ("s",asset^"_keys"))
      };
      { id   = "add_"^asset^"_post_2";
        body = Teq (Tycoll asset,Tdoti ("s",asset^"_keys"),
                    Tunion (Tdot (Told (Tvar "s"), Tvar (asset^"_keys")),
                            Tsingl (Tdoti ("new_asset",key))));
      };
      { id   = "add_"^asset^"_post_3";
        body = Teq (Tycoll asset,Tdoti ("s","added_"^asset),
                    Tunion (Tdot (Told (Tvar "s"), Tvar ("added_"^asset)),
                            Tsingl (Tdoti ("new_asset",key))));
      };
      { id   = "add_"^asset^"_post_4";
        body = Tempty (Tinter (Tdot(Told (Tvar "s"),Tvar (asset^"_keys")),
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
    logic    = false;
    args     = ["s",Tystorage; "ow",Tyasset "owner"; "nbmiles",Tyint];
    returns  = Tytransfers;
    raises   = [Enotfound; Ekeyexist; Einvalidcaller; Einvalidcondition];
    requires = [
      { id   = "r1";
        body = Teq (Tyoption (Tyenum "entry"),Tdoti ("s","ename_"),Tsome (Tenum "Consume"))
      };
      { id   = "r2";
        body = Tempty (Tdoti ("s","removed_mile"))
      };
      { id   = "r3";
        body = Tempty (Tdoti ("s","added_mile"))
      };
    ];
    ensures  = [];
    body     = Tnone;
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
        body = Tforall ([["k"],Tystring],
                        Timpl (Tmem (Tvar "k", Tvar "mile_keys"),
                               Tgt (Tyint, Tapp (Tvar "amount",
                                                 [Tget (Tvar "mile_assets",
                                                        Tvar "k")]), Tint 0)))
      };

    ];
  }

(* ----------------------------------------------------------------------------*)

let mk_use : decl = Duse ["archetype";"Lib"]

let to_whyml (model : M.model) : mlw_tree  =
  let _name = unloc model.name in
  { name = "Miles_with_expiration_storage";
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
      mk_add "mile" "id";
      mk_add "owner" "addr";
      mk_test_consume;
    ];
  }
