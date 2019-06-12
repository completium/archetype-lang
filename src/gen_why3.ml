open Location

module M = Model
open Mlwtree

let mk_test_add asset key : decl = Dfun {
    name     = "add_"^asset;
    args     = ["s",Tystorage; "new_asset",Tyasset asset];
    returns  = Tyunit;
    raises   = [Ekeyexist];
    requires = [];
    ensures  = [
      { id   = "add_"^asset^"_post_1";
        body = Tmem (Tdoti ("new_asset","id"), Tdoti ("s",asset^"_keys"))
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

let to_whyml (model : M.model) : mlw_tree  =
  let name = unloc model.name in
  { name = name;
    decls = [
      mk_test_add "mile" "id";
      mk_test_add "owner" "addr"
    ];
  }
