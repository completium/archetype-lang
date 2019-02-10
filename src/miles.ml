open Model
open Location

let mk_admin_role () = dumloc {
    name = dumloc "admin";
    default = None;
  }

let mk_mile_asset () =
    let desc : asset_unloc = {
    name = dumloc "mile";
    args = [  dumloc { name = dumloc "id";
               typ  = Some (dumloc (Tbuiltin VTstring));
               default = None;
           }; dumloc { name = dumloc "amount";
               typ  = Some (dumloc (Tbuiltin VTuint));
               default = None;
           }; dumloc{ name = dumloc "expiration";
               typ  = Some (dumloc (Tbuiltin VTdate));
               default = None;
             };];
    key = dumloc "id";
    sort = Some [ (dumloc "expiration") ];
    role = false;
    init = None;
    preds =
      let zero = Plit (dumloc (BVint Big_int.zero_big_int)) in
      Some [
            (dumloc (Pcomp (Gt, (dumloc (Pdot ((dumloc (Passet (dumloc "mile"))), (dumloc (Pfield (dumloc "amount")))))), dumloc zero)))];
  } in
  dumloc desc

let mk_owner_asset () =
    let desc : asset_unloc = {
    name = dumloc "owner";
    args = [ dumloc { name = dumloc "addr";
               typ  = Some (dumloc (Tbuiltin VTaddress));
               default = None;
           }; dumloc { name = dumloc "miles";
               typ  = Some (dumloc (Tcontainer (dumloc (Tasset (dumloc "mile")),Partition)));
               default = None;
           };];
    key = dumloc "addr";
    sort = None;
    role = true;
    init = None;
    preds = None;
  } in dumloc desc

let mk_add_transaction () : transaction =
let desc : transaction_unloc = {
    name = dumloc "add";
    args = [ dumloc { name = dumloc "owner";
               typ  = Some (dumloc (Tbuiltin VTaddress));
               default = None;
           };dumloc { name = dumloc "newmile";
               typ  = Some (dumloc (Tasset (dumloc "mile")));
               default = None;
           } ];
    calledby  = Some (Rrole (dumloc "admin"));
    condition = None;
    transferred = None;
    transition = None;
    spec = None;
    action =
      let assign =
         dumloc (Passign (
           SimpleAssign,
           dumloc (Pdot (
             (dumloc (Passet (dumloc "owner"))),
             (dumloc (Pfield (dumloc "miles"))))),
           dumloc (Pconst Cnone))) in
      let getowner = dumloc (Papp (dumloc (Pconst Cget),
                             [dumloc (Passet (dumloc "owner"));
                              dumloc (Pvar (dumloc "owner"))])) in
      Some (dumloc (Pseq [
       dumloc (Papp (dumloc (Pconst (Caddifnotexist)), [dumloc (Passet (dumloc "owner")); assign]));
       dumloc (Pletin ((dumloc "lvar0"), getowner, Some (dumloc (Tasset (dumloc "owner"))),
               dumloc (Papp (dumloc (Pconst Cadd),
                              [dumloc (Pdot (dumloc (Pvar (dumloc "lvar0")), dumloc (Pfield (dumloc "miles"))));
                               dumloc (Pvar (dumloc "newmile"));
                               dumloc (Plit (dumloc (BVstring "mile already exists")))]))))
      ]));
  } in dumloc desc

let mk_consume_transaction () : transaction =
    let desc : transaction_unloc = {
    name = dumloc "consume";
    args = [ dumloc { name = dumloc "owner";
               typ  = Some (dumloc (Tbuiltin VTaddress));
               default = None;
           }; dumloc { name = dumloc "val";
               typ  = Some (dumloc (Tbuiltin VTint));
               default = None;
           } ];
    calledby  = Some (Rrole (dumloc "admin"));
    (*(owner.get owner).miles.when(mile.expiration > now).sum(amount) >= val;
      let o = get owner owner in
      let f = mile.expiration > now in
      let s = when o.miles f in
      sum s amount >= val
     *)
    condition = (
      let getowner = dumloc (Papp (dumloc (Pconst Cget),
                                   [dumloc (Passet (dumloc "owner"));
                                    dumloc (Pvar   (dumloc "owner"))])) in
      let filter = dumloc (Pcomp (Gt,
                          dumloc (Pdot (dumloc (Passet (dumloc "mile")),
                                         dumloc (Pfield (dumloc "expiration")))),
                          dumloc (Pconst Cnow))) in
      let set = dumloc (Papp (dumloc (Pconst Cwhen),
                               [ dumloc (Pdot (dumloc (Pvar (dumloc "o")),
                                 dumloc (Pfield (dumloc "miles"))));
                                 filter])) in
      let sum = dumloc (Papp (dumloc (Pconst Csum),
                              [dumloc (Pvar (dumloc "s"));
                               dumloc (Pdot (dumloc (Passet (dumloc "mile")),
                               dumloc (Pfield (dumloc "amount"))))])) in
      let cond = dumloc (Pcomp (Ge, sum, dumloc (Pvar (dumloc "val")))) in
      Some (dumloc (Pletin (dumloc "o", getowner, Some (dumloc (Tasset (dumloc "owner"))),
            dumloc (Pletin (dumloc "s", set, Some (dumloc (Tcontainer (dumloc (Tasset (dumloc "mile")), Set))),
            cond))))));
    transferred = None;
    transition = None;
    spec = Some (dumloc ({
        variables = [];
        action    = None;
        invariants = [];
        ensures =
          let exp = Lquantifer (Forall, dumloc "m", dumloc (LTvset (VSremoved,
                                                                      dumloc (LTprog (dumloc (Tasset (dumloc "mile")))))),
        dumloc (Llog (And,
              dumloc (Lapp (dumloc (Lconst Cmem),[dumloc (Lvar (dumloc "m"));
                                                    dumloc (Ldot (dumloc (Lasset (dumloc ("owner"))),
                                                                   dumloc (Lfield (dumloc ("miles")))))])),
              dumloc (Lcomp (Ge,
                              dumloc (Ldot (dumloc (Lvar (dumloc "m")),
                                             dumloc (Lfield (dumloc "expiration")))),
                              dumloc (Lconst Cnow)))))) in
      let getowner = dumloc (Lapp (dumloc (Lconst Cget),
                           [dumloc (Lasset (dumloc "owner"));
                            dumloc (Lfield (dumloc "owner"))])) in
      let sumamount = dumloc (Lapp (dumloc (Lconst Csum),
                            [dumloc (Ldot (dumloc (Lvar (dumloc "o")),
                                            dumloc (Lfield (dumloc "amount"))))])) in
      let sumval = dumloc (Larith (Plus, sumamount, dumloc (Lvar (dumloc "val")))) in
      let rightamount =
        Lletin (dumloc "o", getowner, Some (dumloc (LTprog (dumloc (Tasset (dumloc "owner"))))),
                dumloc (Lcomp (Equal, dumloc (Lapp (dumloc (Lconst Cbefore),[sumamount])),sumval))
          ) in
          [(None, dumloc exp);(None, dumloc rightamount)]
      }));
    action = (
      let zero = BVint Big_int.zero_big_int in
      let filter = dumloc (Pcomp (Gt, dumloc (Pdot (dumloc (Passet (dumloc "mile")), dumloc (Pfield (dumloc "expiration")))),
                                 dumloc (Pconst Cnow))) in
      let set = dumloc (Papp (dumloc (Pconst Cwhen), [ dumloc (Pdot (dumloc (Pvar (dumloc "o")), dumloc (Pfield (dumloc "miles")))); filter])) in
      let cond1 = dumloc (Pcomp (Gt,
                         dumloc (Pdot (dumloc (Pvar (dumloc "m")),
                                       (dumloc (Pfield (dumloc "amount"))))),
                         dumloc (Pvar (dumloc "r")))) in
      let then1 = dumloc (Pseq [
                      dumloc (Passign (MinusAssign, dumloc (Pdot (dumloc (Pvar (dumloc "m")),
                                                              dumloc (Pfield (dumloc "amount")))), dumloc (Pvar (dumloc "r"))));
                      dumloc (Passign (SimpleAssign, dumloc (Pvar (dumloc "r")), dumloc (Plit (dumloc zero))));
                      dumloc Pbreak
                    ]) in
      let cond2 = dumloc (Pcomp (Equal,
                         dumloc (Pdot (dumloc (Pvar (dumloc "m")),
                                        dumloc (Pfield (dumloc "amount")))),
                         dumloc (Pvar (dumloc "r")))) in
      let then2 = dumloc (Pseq [
                      dumloc (Papp (dumloc (Pconst Cremove),
                                     [dumloc (Pdot (dumloc (Pvar (dumloc "o")),
                                                     dumloc (Pfield (dumloc "miles"))));
                                      dumloc (Pvar (dumloc "m"))]));
                      dumloc (Passign (SimpleAssign,
                                        dumloc (Pvar (dumloc "r")),
                                        dumloc (Plit (dumloc zero))));
                      dumloc Pbreak
                    ]) in
      let then3 = dumloc (Pseq [
                      dumloc (Passign (MinusAssign,
                                        dumloc (Pvar (dumloc "r")),
                                        dumloc (Pdot (dumloc (Pvar (dumloc "m")),
                                                       dumloc (Pfield (dumloc "amount"))))));
                      dumloc (Papp (dumloc (Pconst Cremove),
                                     [dumloc (Pdot (dumloc (Pvar (dumloc "o")),
                                                     dumloc (Pfield (dumloc "miles"))));
                                     dumloc (Pvar (dumloc "m"))]));
                      dumloc Pbreak
                    ]) in
      let asert = dumloc (Lcomp (Equal, dumloc (Lvar (dumloc "r")), dumloc (Llit (dumloc zero)))) in
      let getowner = dumloc (Papp (dumloc (Pconst Cget),[dumloc (Passet (dumloc "owner")); dumloc (Pvar (dumloc "owner"))])) in
      let p =
        dumloc (Pletin (dumloc "r", dumloc (Pvar (dumloc "val")), Some (dumloc (Tbuiltin VTint)),
        dumloc (Pletin (dumloc "o", getowner, Some (dumloc (Tasset (dumloc "owner"))),
        dumloc (Pletin (dumloc "s", set, Some (dumloc (Tcontainer (dumloc (Tasset (dumloc "mile")), Set))),
                                                   dumloc (Pseq [

        dumloc (Pfor (dumloc "i", dumloc (Pvar (dumloc "s")),
          dumloc (Pif (cond1,
           then1,
          Some (dumloc (Pif (cond2,
           then2,
          Some (
           then3
        ))))))));
        dumloc (Passert (asert))

        ]))))))) in
        Some p);
  } in dumloc desc

let mk_miles_model () = dumloc {
    name          = dumloc "miles_with_expiration";
    roles         = [ mk_admin_role () ];
    variables     = [];
    assets        = [mk_mile_asset (); mk_owner_asset ()];
    functions     = [];
    transactions  = [mk_add_transaction ()];
    stmachines    = [];
    enums         = [];
    spec          = None;
  }
