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
      Some [Pcomp (Gt,(Pdot (Passet "mile", Pfield "amount")),zero)];
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
      let assign = Passign (SimpleAssign, Pdot (Passet "owner", Pfield "miles"),Pconst Cvoid) in
      let getowner = Papp (Pconst Cget,[Passet "owner"; Pvar "owner"]) in
      Some (Pseq [
       Papp (Pconst Caddifnotexist, [Passet "owner"; assign]);
       Pletin ("lvar0",getowner, Tasset "owner",
               Papp (Pconst Cadd, [Pdot (Pvar "lvar0", Pfield "miles");
                                   Pvar "newmile";
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
      let getowner = Papp (Pconst Cget,[Passet "owner";Pvar "owner"]) in
      let filter = Pcomp (Gt,Pdot (Passet "mile",Pfield "expiration"),
                                 Pconst Cnow) in
      let set = Papp (Pconst Cwhen, [ Pdot (Pvar "o", Pfield "miles"); filter]) in
      let sum = Papp (Pconst Csum, [Pvar "s";Pdot (Passet "mile", Pfield "amount")]) in
      let cond = Pcomp (Ge, sum, Pvar "val") in
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
              Lapp (Lconst Cmem,[Lvar "m";Ldot (Lasset "owner", Lfield "miles")]),
              Lcomp (Ge,Ldot (Lvar "m", Lfield "expiration"),Lconst Cnow))) in
      let getowner = Lapp (Lconst Cget,[Lasset "owner";Lfield "owner"]) in
      let sumamount = Lapp (Lconst Csum,[Ldot (Lvar "o", Lfield "amount")]) in
      let sumval = Larith (Plus,sumamount,Lvar "val") in
      let rightamount =
        Lletin ("o",getowner, Some (LTprog (Tasset "owner")),
                Lcomp (Equal,Lapp (Lconst Cbefore,[sumamount]),sumval)
          ) in
          [(None, exp);(None, rightamount)]
      };
    action = (
      let zero = BVint Big_int.zero_big_int in
      let filter = Pcomp (Gt,Pdot (Passet "mile", Pfield "expiration"),
                                 Pconst Cnow) in
      let set = Papp (Pconst Cwhen, [ Pdot (Pvar "o", Pfield "miles"); filter]) in
      let cond1 = Pcomp (Gt,Pdot (Pvar "m", Pfield "amount"), Pvar "r") in
      let then1 = Pseq [
                      Passign (MinusAssign, Pdot (Pvar "m", Pfield "amount"),Pvar "r");
                      Passign (SimpleAssign,Pvar "r",Plit zero);
                      Pbreak
                    ] in
      let cond2 = Pcomp (Equal,Pdot (Pvar "m", Pfield "amount"),Pvar "r") in
      let then2 = Pseq [
                      Papp (Pconst Cremove, [Pdot (Pvar "o", Pfield "miles"); Pvar "m"]);
                      Passign (SimpleAssign, Pvar "r",Plit zero);
                      Pbreak
                    ]in
      let then3 = Pseq [
                      Passign (MinusAssign, Pvar "r", Pdot (Pvar "m", Pfield "amount"));
                      Papp (Pconst Cremove, [Pdot (Pvar "o", Pfield "miles"); Pvar "m"]);
                      Pbreak
                    ] in
      let asert = Lcomp (Equal, Lvar "r", Llit zero) in
      let getowner = Papp (Pconst Cget,[Passet "owner";Pvar "owner"]) in
      let p =
        Pletin ("r", Pvar "val", Tbuiltin VTint,
        Pletin ("o", getowner, Tasset "owner",
        Pletin ("s", set, Tcontainer (Tasset "mile", Set),
        Pseq [
        Pfor ("i",Pvar "s",
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
