open Model
open Location

let mk_admin_role () = {
    name = mkdummy "admin";
    default = None;
  }

let mk_mile_asset () =
    let desc : asset_unloc = {
    name = mkdummy "mile";
    args = [  mkdummy { name = mkdummy "id";
               typ  = Some (mkdummy (Tbuiltin VTstring));
               default = None;
           }; mkdummy { name = mkdummy "amount";
               typ  = Some (mkdummy (Tbuiltin VTuint));
               default = None;
           }; mkdummy{ name = mkdummy "expiration";
               typ  = Some (mkdummy (Tbuiltin VTstring));
               default = None;
             };];
    key = mkdummy "id";
    sort = Some [ (mkdummy "expiration") ];
    role = false;
    init = None;
    preds =
      let zero = Plit (mkdummy (BVint 0(*Big_int.zero_big_int*))) in
      Some [
            (mkdummy (Pcomp (Gt, (mkdummy (Pdot ((mkdummy (Passet (mkdummy "mile"))), (mkdummy (Pfield (mkdummy "amount")))))), mkdummy zero)))];
  } in
  mkdummy desc

let mk_owner_asset () =
    let desc : asset_unloc = {
    name = mkdummy "owner";
    args = [ mkdummy { name = mkdummy "addr";
               typ  = Some (mkdummy (Tbuiltin VTaddress));
               default = None;
           }; mkdummy { name = mkdummy "miles";
               typ  = Some (mkdummy (Tcontainer (mkdummy (Tasset (mkdummy "mile")),Partition)));
               default = None;
           };];
    key = mkdummy "addr";
    sort = None;
    role = true;
    init = None;
    preds = None;
  } in mkdummy desc

let mk_add_transaction () : transaction =
let desc : transaction_unloc = {
    name = mkdummy "add";
    args = [ mkdummy { name = mkdummy "owner";
               typ  = Some (mkdummy (Tbuiltin VTaddress));
               default = None;
           };mkdummy { name = mkdummy "newmile";
               typ  = Some (mkdummy (Tasset (mkdummy "mile")));
               default = None;
           } ];
    calledby  = Some (Rrole (mkdummy "admin"));
    condition = None;
    transferred = None;
    transition = None;
    spec = None;
    action =
      let assign =
         mkdummy (Passign (
           SimpleAssign,
           mkdummy (Pdot (
             (mkdummy (Passet (mkdummy "owner"))),
             (mkdummy (Pfield (mkdummy "miles"))))),
           mkdummy (Pconst Cvoid))) in
      let getowner = mkdummy (Papp (mkdummy (Pconst Cget),
                             [mkdummy (Passet (mkdummy "owner"));
                              mkdummy (Pvar (mkdummy "owner"))])) in
      Some (mkdummy (Pseq [
       mkdummy (Papp (mkdummy (Pconst (Caddifnotexist)), [mkdummy (Passet (mkdummy "owner")); assign]));
       mkdummy (Pletin ((mkdummy "lvar0"), getowner, mkdummy (Tasset (mkdummy "owner")),
               mkdummy (Papp (mkdummy (Pconst Cadd),
                              [mkdummy (Pdot (mkdummy (Pvar (mkdummy "lvar0")), mkdummy (Pfield (mkdummy "miles"))));
                               mkdummy (Pvar (mkdummy "newmile"));
                               mkdummy (Plit (mkdummy (BVstring "mile already exists")))]))))
      ]));
  } in mkdummy desc

let mk_consume_transaction () : transaction =
    let desc : transaction_unloc = {
    name = mkdummy "consume";
    args = [ mkdummy { name = mkdummy "owner";
               typ  = Some (mkdummy (Tbuiltin VTaddress));
               default = None;
           }; mkdummy { name = mkdummy "val";
               typ  = Some (mkdummy (Tbuiltin VTint));
               default = None;
           } ];
    calledby  = Some (Rrole (mkdummy "admin"));
    (*(owner.get owner).miles.when(mile.expiration > now).sum(amount) >= val;
      let o = get owner owner in
      let f = mile.expiration > now in
      let s = when o.miles f in
      sum s amount >= val
     *)
    condition = (
      let getowner = mkdummy (Papp (mkdummy (Pconst Cget),
                                   [mkdummy (Passet (mkdummy "owner"));
                                    mkdummy (Pvar   (mkdummy "owner"))])) in
      let filter = mkdummy (Pcomp (Gt,
                          mkdummy (Pdot (mkdummy (Passet (mkdummy "mile")),
                                         mkdummy (Pfield (mkdummy "expiration")))),
                          mkdummy (Pconst Cnow))) in
      let set = mkdummy (Papp (mkdummy (Pconst Cwhen),
                               [ mkdummy (Pdot (mkdummy (Pvar (mkdummy "o")),
                                 mkdummy (Pfield (mkdummy "miles"))));
                                 filter])) in
      let sum = mkdummy (Papp (mkdummy (Pconst Csum),
                              [mkdummy (Pvar (mkdummy "s"));
                               mkdummy (Pdot (mkdummy (Passet (mkdummy "mile")),
                               mkdummy (Pfield (mkdummy "amount"))))])) in
      let cond = mkdummy (Pcomp (Ge, sum, mkdummy (Pvar (mkdummy "val")))) in
      Some (mkdummy (Pletin (mkdummy "o", getowner, (mkdummy (Tasset (mkdummy "owner"))),
            mkdummy (Pletin (mkdummy "s", set, mkdummy (Tcontainer (mkdummy (Tasset (mkdummy "mile")), Set)),
            cond))))));
    transferred = None;
    transition = None;
    spec = Some (mkdummy ({
        variables = [];
        action    = None;
        invariants = [];
        ensures =
          let exp = Lquantifer (Forall, mkdummy "m", mkdummy (LTvset (VSremoved,
                                                                      mkdummy (LTprog (mkdummy (Tasset (mkdummy "mile")))))),
        mkdummy (Llog (And,
              mkdummy (Lapp (mkdummy (Lconst Cmem),[mkdummy (Lvar (mkdummy "m"));
                                                    mkdummy (Ldot (mkdummy (Lasset (mkdummy ("owner"))),
                                                                   mkdummy (Lfield (mkdummy ("miles")))))])),
              mkdummy (Lcomp (Ge,
                              mkdummy (Ldot (mkdummy (Lvar (mkdummy "m")),
                                             mkdummy (Lfield (mkdummy "expiration")))),
                              mkdummy (Lconst Cnow)))))) in
      let getowner = mkdummy (Lapp (mkdummy (Lconst Cget),
                           [mkdummy (Lasset (mkdummy "owner"));
                            mkdummy (Lfield (mkdummy "owner"))])) in
      let sumamount = mkdummy (Lapp (mkdummy (Lconst Csum),
                            [mkdummy (Ldot (mkdummy (Lvar (mkdummy "o")),
                                            mkdummy (Lfield (mkdummy "amount"))))])) in
      let sumval = mkdummy (Larith (Plus, sumamount, mkdummy (Lvar (mkdummy "val")))) in
      let rightamount =
        Lletin (mkdummy "o", getowner, Some (mkdummy (LTprog (mkdummy (Tasset (mkdummy "owner"))))),
                mkdummy (Lcomp (Equal, mkdummy (Lapp (mkdummy (Lconst Cbefore),[sumamount])),sumval))
          ) in
          [(None, mkdummy exp);(None, mkdummy rightamount)]
      }));
    action = (
      let zero = BVint 0(*Big_int.zero_big_int*) in
      let filter = mkdummy (Pcomp (Gt, mkdummy (Pdot (mkdummy (Passet (mkdummy "mile")), mkdummy (Pfield (mkdummy "expiration")))),
                                 mkdummy (Pconst Cnow))) in
      let set = mkdummy (Papp (mkdummy (Pconst Cwhen), [ mkdummy (Pdot (mkdummy (Pvar (mkdummy "o")), mkdummy (Pfield (mkdummy "miles")))); filter])) in
      let cond1 = mkdummy (Pcomp (Gt,
                         mkdummy (Pdot (mkdummy (Pvar (mkdummy "m")),
                                       (mkdummy (Pfield (mkdummy "amount"))))),
                         mkdummy (Pvar (mkdummy "r")))) in
      let then1 = mkdummy (Pseq [
                      mkdummy (Passign (MinusAssign, mkdummy (Pdot (mkdummy (Pvar (mkdummy "m")),
                                                              mkdummy (Pfield (mkdummy "amount")))), mkdummy (Pvar (mkdummy "r"))));
                      mkdummy (Passign (SimpleAssign, mkdummy (Pvar (mkdummy "r")), mkdummy (Plit (mkdummy zero))));
                      mkdummy Pbreak
                    ]) in
      let cond2 = mkdummy (Pcomp (Equal,
                         mkdummy (Pdot (mkdummy (Pvar (mkdummy "m")),
                                        mkdummy (Pfield (mkdummy "amount")))),
                         mkdummy (Pvar (mkdummy "r")))) in
      let then2 = mkdummy (Pseq [
                      mkdummy (Papp (mkdummy (Pconst Cremove),
                                     [mkdummy (Pdot (mkdummy (Pvar (mkdummy "o")),
                                                     mkdummy (Pfield (mkdummy "miles"))));
                                      mkdummy (Pvar (mkdummy "m"))]));
                      mkdummy (Passign (SimpleAssign,
                                        mkdummy (Pvar (mkdummy "r")),
                                        mkdummy (Plit (mkdummy zero))));
                      mkdummy Pbreak
                    ]) in
      let then3 = mkdummy (Pseq [
                      mkdummy (Passign (MinusAssign,
                                        mkdummy (Pvar (mkdummy "r")),
                                        mkdummy (Pdot (mkdummy (Pvar (mkdummy "m")),
                                                       mkdummy (Pfield (mkdummy "amount"))))));
                      mkdummy (Papp (mkdummy (Pconst Cremove),
                                     [mkdummy (Pdot (mkdummy (Pvar (mkdummy "o")),
                                                     mkdummy (Pfield (mkdummy "miles"))));
                                     mkdummy (Pvar (mkdummy "m"))]));
                      mkdummy Pbreak
                    ]) in
      let asert = mkdummy (Lcomp (Equal, mkdummy (Lvar (mkdummy "r")), mkdummy (Llit (mkdummy zero)))) in
      let getowner = mkdummy (Papp (mkdummy (Pconst Cget),[mkdummy (Passet (mkdummy "owner")); mkdummy (Pvar (mkdummy "owner"))])) in
      let p =
        mkdummy (Pletin (mkdummy "r", mkdummy (Pvar (mkdummy "val")), mkdummy (Tbuiltin VTint),
        mkdummy (Pletin (mkdummy "o", getowner, mkdummy (Tasset (mkdummy "owner")),
        mkdummy (Pletin (mkdummy "s", set, mkdummy (Tcontainer (mkdummy (Tasset (mkdummy "mile")), Set)),
                                                   mkdummy (Pseq [

        mkdummy (Pfor (mkdummy "i", mkdummy (Pvar (mkdummy "s")),
          mkdummy (Pif (cond1,
           then1,
          Some (mkdummy (Pif (cond2,
           then2,
          Some (
           then3
        ))))))));
        mkdummy (Passert (asert))

        ]))))))) in
        Some p);
  } in mkdummy desc

let mk_miles_model () = mkdummy {
    name          = mkdummy "miles_with_expiration";
    roles         = [ mk_admin_role () ];
    variables     = [];
    assets        = [mk_mile_asset (); mk_owner_asset ()];
    functions     = [];
    transactions  = [mk_add_transaction ()];
    stmachines    = [];
    enums         = [];
    spec          = None;
  }
