open Model

let mk_admin_role () = {
    name = "admin";
    default = None;
  }

let mk_mile_asset () = {
    name = "mile";
    args = [ { name = "id";
               typ  = Tbuiltin VTstring;
               default = None;
           };{ name = "amount";
               typ  = Tbuiltin VTuint;
               default = None;
           };{ name = "expiration";
               typ  = Tbuiltin VTstring;
               default = None;
             };];
    key = "id";
    sort = Some [ "expiration" ];
    role = false;
    init = None;
    preds =
      let zero = Plit (BVint Big_int.zero_big_int) in
      Some [Pcomparaison (Gt,(Pref (Rfield (Rasset "mile","amount"))),zero)];
  }

let mk_owner_asset () = {
    name = "owner";
    args = [ { name = "addr";
               typ  = Tbuiltin VTaddress;
               default = None;
           };{ name = "miles";
               typ  = Tcontainer (Tasset "mile",Partition);
               default = None;
           };];
    key = "addr";
    sort = None;
    role = true;
    init = None;
    preds = None;
  }

let mk_add_transaction = {
    name = "add";
    args = [ { name = "owner";
               typ  = Tbuiltin VTaddress;
               default = None;
           };{ name = "newmile";
               typ  = Tasset "mile";
               default = None;
           } ];
    calledby  = Some (Rrole "admin");
    condition = None;
    transferred = None;
    transition = None;
    ensures = None;
    action =
      let assign = Passign (SimpleAssign,Rfield (Rasset "owner","miles"),Pconst Cvoid) in
      let getowner = Papp (Pconst Cget,[Pref (Rasset "owner");Pref (Rvar "owner")]) in
      Some (Pseq [
       Papp (Pconst Caddifnotexist, [Pref (Rasset "owner"); assign]);
       Pletin ("lvar0",getowner, Tasset "owner",
               Papp (Pconst Cadd, [Pref (Rfield (Rvar ("lvar0"),"miles"));
                                   Pref (Rvar "newmile");
                                   Pliteral (BVstring "mile already exists")]))
      ]);
  }

let mk_consume_transaction () = {
    name = "consume";
    args = [ { name = "owner";
               typ  = Tbuiltin VTaddress;
               default = None;
           };{ name = "val";
               typ  = Tbuiltin VTint;
               default = None;
           } ];
    calledby  = Some (Rrole "admin");
    (*(owner.get owner).miles.when(mile.expiration > now).sum(amount) >= val;
      let o = get owner owner in
      let f = mile.expiration > now in
      let s = when o.miles f in
      sum s amount >= val
     *)
    condition =
      let getowner = Papp (Pconst Cget,[Pref (Rasset "owner");Pref (Rvar "owner")]) in
      let filter = Pcomp (Gt,Pref (Rfield (Rasset "mile","expiration")),
                                 Pconst Cnow) in
      let set = Papp (Pconst Cwhen, [ Pref (Rfield (Rvar "o","mile")); filter]) in
      let sum = Papp (Pconst Csum, [Pref (Pvar "s");Pref (Pfield (Rasset "mile","amount"))]) in
      Some [Pletin ("o",getowner, Tasset "owner",
            Pletin ("s", set, Tcontainer (Tasset "mile", Set),
            Pcomp (Ge,sum,Pref (Rvar "val"))))];
    transferred = None;
    transition = None;
    ensures =
      let exp = Lquantifer (Forall, "m", LTvset (VSremoved,LTprog (Tasset "mile")),
        Lapp (Lconst Cand,[
              Lapp (Lconst Cmem,[Lref (Rvar "m");Lref (Rfield (Rasset "owner","miles"))]);
              Llogical (Ge,Lref (Rfield(Rvar "m","expiration")),Lconst Cnow)])) in
      let getowner = Lapp (Lconst Cget,[Pref (Rasset "owner");Pref (Rvar "owner")]) in
      let sumamount = Lapp (Lconst Csum,[Lref (Rfield (Rvar "o","amount"))]) in
      let sumval = Larith (Plus,sumamount,Lref (Rvar "val")) in
      let rightamount =
        Lletin ("o",getowner, LTprog (Tasset "owner"),
                Lcomp (Lconst Equal,Lapp (Lconst Cbefore,[sumamount]),sumval)
          ) in
      Some [exp;rightamount];
    action =
      let zero = BVint Big_int.zero_big_int in
      let filter = Pcomp (Gt,Pref (Rfield (Rasset "mile","expiration")),
                                 Pconst Cnow) in
      let set = Papp (Pconst Cwhen, [ Pref (Rfield (Rvar "o","mile")); filter]) in
      let cond1 = Pcomp (Gt,Pref (Rfield (Rvar "m","amount")),Pref (Rvar "r")) in
      let then1 = Pseq [
                      Passign (MinusAssign, Rfield (Rvar "m","amount"),Pref (Rvar "r"));
                      Passign (SimpleAssign,Rvar "r",Plit zero);
                      Pbreak
                    ] in
      let cond2 = Pcomp (Eq,Pref (Rfield (Rvar "m","amount")),Pref (Rvar "r")) in
      let then2 = Pseq [
                      Papp (Pconst Cremove, Rfield (Rvar "o","miles"), Pref (Rvar "m"));
                      Passign (SimpleAssign,Rvar "r",Plit zero);
                      Pbreak
                    ]in
      let then3 = Pseq [
                      Passign (MinusAssign, Pref (Rvar "r"), Rfield (Rvar "m","amount"));
                      Papp (Pconst Cremove, Rfield (Rvar "o","miles"), Pref (Rvar "m"));
                      Pbreak
                    ] in
      let asert = Lcomp (Eq,Lref (Rvar "r"),Llit zero) in
      let p =
        Pletin ("r",Pref (Rvar "val"),Tbuiltin VTint,
        Pletin ("o",getowner,Tasset "owner",
        Pletin ("s",set,Tcontainer (Tasset "mile", Set),
        Pseq [
        Pfor ("i",Pref (Rvar "s"),
          Pif (cond1,
           then1,
          Some (Pif (cond2,
           then2,
          Some (
           then3
        )))));
        Passert (asert)
        ]))) in
        Some p;
  }

let mk_miles_model () = {
    name         = "miles_with_expiration";
    roles        = [ mk_admin_role () ];
    values       = [];
    assets       = [mk_mile_asset (); mk_owner_asset ()];
    transactions = [mk_add_transaction ()];
    stmachines   = [];
    enums        = [];
    preds        = [];
  }
