open Location

module M = Model
open Mlwtree

let mk_test_add () : decl = Dfun {
    name     = "add_mile";
    args     = ["s",Tystorage; "new_asset",Tyasset "mile"];
    returns  = Tyunit;
    raises   = [Ekeyexist];
    requires = [];
    ensures  = [
      { id   = "add_mile_post_1";
        body = Tmem (Tdoti ("new_asset","id"), Tdoti ("s","mile_keys"))
      };
      { id   = "add_mile_post_2";
        body = Teq (Tycoll "mile",Tdoti ("s","mile_keys"),
                    Tunion (Tdot (Told (Tvar "s"), Tvar "mile_keys"),
                            Tsingl (Tdoti ("new_asset","id"))));
      };
      { id   = "add_mile_post_3";
        body = Teq (Tycoll "mile",Tdoti ("s","added_mile"),
                    Tunion (Tdot (Told (Tvar "s"), Tvar "added_mile"),
                            Tsingl (Tdoti ("new_asset","id"))));
      };
      { id   = "add_mile_post_4";
        body = Tempty (Tinter (Tdot(Told (Tvar "s"),Tvar "mile_keys"),
                               Tsingl (Tdoti ("new_asset","id"))
                              ));
      };

    ];
    body     = Tseq [
      Tif (Tmem (Tdoti ("new_asset","id"), Tdoti ("s","mile_keys")),
      Traise Ekeyexist, (* then *)
      Some (Tseq [      (* else *)
      Tassign (Tdoti ("s","mile_assets"),
               Tset (Tdoti ("s","mile_assets"),Tdoti("new_asset","id"),Tvar "new_mile"));
      Tassign (Tdoti ("s","mile_keys"),
               Tadd (Tdoti("new_asset","id"),Tdoti ("s","mile_keys")));
      Tassign (Tdoti ("s","added_mile"),
                Tadd (Tdoti("new_asset","id"),Tdoti ("s","added_mile")))
      ]
            ))
      ];
  }

let to_whyml (model : M.model) : mlw_tree  =
  let name = unloc model.name in
  { name = name;  decls = [mk_test_add ()]; }
