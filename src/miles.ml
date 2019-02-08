open Model

let mk_admin_role () = {
    name = "admin";
    default = None;
  }

let mk_mile_asset () = {
    name = "mile";
    args = [ { name = "id";
               typ  = Some (Tbuiltin VTstring);
               default = None;
           };{ name = "amount";
               typ  = Some (Tbuiltin VTuint);
               default = None;
           };{ name = "expiration";
               typ  = Some (Tbuiltin VTstring);
               default = None;
             };];
    key = "id";
    sort = Some [ "expiration" ];
    role = false;
    init = None;
    preds =
      let zero = Plit (BVint Big_int.zero_big_int) in
      Some [Pcomp (Gt,(Pref (Rfield (Rasset "mile","amount"))),zero)];
  }

let mk_owner_asset () = {
    name = "owner";
    args = [ { name = "addr";
               typ  = Some (Tbuiltin VTaddress);
               default = None;
           };{ name = "miles";
               typ  = Some (Tcontainer (Tasset "mile",Partition));
               default = None;
           };];
    key = "addr";
    sort = None;
    role = true;
    init = None;
    preds = None;
  }

let mk_add_transaction () : transaction = {
    name = "add";
    args = [ { name = "owner";
               typ  = Some (Tbuiltin VTaddress);
               default = None;
           };{ name = "newmile";
               typ  = Some (Tasset "mile");
               default = None;
           } ];
    calledby  = Some (Rrole "admin");
    condition = None;
    transferred = None;
    transition = None;
    spec = None;
    action =
      let assign = Passign (SimpleAssign,Rfield (Rasset "owner","miles"),Pconst Cvoid) in
      let getowner = Papp (Pconst Cget,[Pref (Rasset "owner");Pref (Rvar "owner")]) in
      Some (Pseq [
       Papp (Pconst Caddifnotexist, [Pref (Rasset "owner"); assign]);
       Pletin ("lvar0",getowner, Tasset "owner",
               Papp (Pconst Cadd, [Pref (Rfield (Rvar ("lvar0"),"miles"));
                                   Pref (Rvar "newmile");
                                   Plit (BVstring "mile already exists")]))
      ]);
  }

let mk_consume_transaction () : transaction = {
    name = "consume";
    args = [ { name = "owner";
               typ  = Some (Tbuiltin VTaddress);
               default = None;
           };{ name = "val";
               typ  = Some (Tbuiltin VTint);
               default = None;
           } ];
    calledby  = Some (Rrole "admin");
    (*(owner.get owner).miles.when(mile.expiration > now).sum(amount) >= val;
      let o = get owner owner in
      let f = mile.expiration > now in
      let s = when o.miles f in
      sum s amount >= val
     *)
    condition = (
      let getowner = Papp (Pconst Cget,[Pref (Rasset "owner");Pref (Rvar "owner")]) in
      let filter = Pcomp (Gt,Pref (Rfield (Rasset "mile","expiration")),
                                 Pconst Cnow) in
      let set = Papp (Pconst Cwhen, [ Pref (Rfield (Rvar "o","mile")); filter]) in
      let sum = Papp (Pconst Csum, [Pref (Rvar "s");Pref (Rfield (Rasset "mile","amount"))]) in
      let cond = Pcomp (Ge, sum, Pref (Rvar "val")) in
      Some (Pletin ("o", getowner, Tasset "owner",
            Pletin ("s", set, Tcontainer (Tasset "mile", Set),
            cond))));
    transferred = None;
    transition = None;
    spec = Some {
        variables = [];
        action    = None;
        invariants = [];
        ensures =
                let exp = Lquantifer (Forall, "m", LTvset (VSremoved,LTprog (Tasset "mile")),
        Llog (And,
              Lapp (Lconst Cmem,[Lref (Rvar "m");Lref (Rfield (Rasset "owner","miles"))]),
              Lcomp (Ge,Lref (Rfield(Rvar "m","expiration")),Lconst Cnow))) in
      let getowner = Lapp (Lconst Cget,[Lref (Rasset "owner");Lref (Rvar "owner")]) in
      let sumamount = Lapp (Lconst Csum,[Lref (Rfield (Rvar "o","amount"))]) in
      let sumval = Larith (Plus,sumamount,Lref (Rvar "val")) in
      let rightamount =
        Lletin ("o",getowner, Some (LTprog (Tasset "owner")),
                Lcomp (Equal,Lapp (Lconst Cbefore,[sumamount]),sumval)
          ) in
          [(None, exp);(None, rightamount)]
      };
    action = (
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
      let cond2 = Pcomp (Equal,Pref (Rfield (Rvar "m","amount")),Pref (Rvar "r")) in
      let then2 = Pseq [
                      Papp (Pconst Cremove, [Pref (Rfield (Rvar "o","miles")); Pref (Rvar "m")]);
                      Passign (SimpleAssign,Rvar "r",Plit zero);
                      Pbreak
                    ]in
      let then3 = Pseq [
                      Passign (MinusAssign, Rvar "r", Pref (Rfield (Rvar "m","amount")));
                      Papp (Pconst Cremove, [Pref (Rfield (Rvar "o","miles")); Pref (Rvar "m")]);
                      Pbreak
                    ] in
      let asert = Lcomp (Equal,Lref (Rvar "r"),Llit zero) in
      let getowner = Papp (Pconst Cget,[Pref (Rasset "owner");Pref (Rvar "owner")]) in
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
        Some p);
  }

let mk_miles_model () = {
    name          = "miles_with_expiration";
    roles         = [ mk_admin_role () ];
    variables     = [];
    assets        = [mk_mile_asset (); mk_owner_asset ()];
    functions     = [];
    transactions  = [mk_add_transaction ()];
    stmachines    = [];
    enums         = [];
    spec          = None;
  }
