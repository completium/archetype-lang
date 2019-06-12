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
      { id   = "new_mile now belongs to mile collection";
        body = Tmem (Tdoti ("new_asset","id"), Tdoti ("s","mile_keys"))
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
