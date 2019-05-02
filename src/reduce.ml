open Location
open Model

exception ReduceError of string * Location.t option

let fail str = dumloc (Papp (dumloc (Pconst Cfail), [dumloc (Plit (dumloc (BVstring str)))]))

let process_action model : model =
  let process_ap (tr : transaction) =
    let process_transition (tr : transaction) : transaction =
        match tr.transition with
        | None -> tr
        | Some transition ->
          let process_args (tr : transition) : ((ptyp, bval) gen_decl) list =
          match tr.on with
          | Some (id, id2) ->
              [{name = id;
                typ = Some (mkloc (Location.loc id2) (Tasset id2));
                default = None;
                loc = Location.merge (loc id) (loc id2)}]
          | None -> [] in
          let process_effect (tr : transition) =
            let state =
            match tr.on with
            | Some (_id, id2) -> dumloc (Pdot (dumloc (Pvar id2), dumloc (Pconst Cstate)))
            | _ -> dumloc (Pconst Cstate) in
            let code : pterm =
            (List.fold_right (fun (id, cond, effect) acc ->
              let tre =
                match tr.on with
                | Some (id, id_asset) -> dumloc (Papp (dumloc (Pdot (dumloc (Pvar id_asset),
                                                                     dumloc (Pconst Cupdate))), [
                                                                      (*TODO: insert key of asset*)
                                                                      dumloc (Precord [(Qident (dumloc "state"), dumloc (Pvar id))])
                                                                     ]))
                | None -> dumloc (Passign (ValueAssign, state, dumloc (Pvar id))) in
              let code =
              match effect with
              | Some e -> dumloc (Pseq (tre, e))
              | None -> tre in

              match cond with
              | Some c -> Some (dumloc (Pif (c, code, acc)))
              | None -> Some code
            ) tr.trs None)
            |> Tools.get
             in

            match (unloc tr.from) with
            | Sany -> Some code
            | _ ->
            begin
            let rec compute_patterns loc = function
            | Sref id -> [mkloc (Location.loc id) (Mapp (Qident id, []))]
            | Sor (a, b) -> [a; b] |> List.map (fun x -> compute_patterns loc (unloc x)) |> List.flatten
            | Sany -> raise (ReduceError ("any is not authorized in this expression", Some loc)) in
            let list_patterns = let l, f = deloc tr.from in compute_patterns l f in
            Some (dumloc (Pmatchwith (state,
              List.map (fun x -> (x, code)) list_patterns @
              [dumloc Mwild, fail "not_valid_state"]
            )))
            end

          in
          { tr with
            transition = None;
            args = tr.args @ (process_args transition);
            effect = process_effect transition;
          }
    in
    let process_calledby (tr : transaction) =
      let process_cb (cb : rexpr) : pterm =
        let rec process_rexpr rq =
        dumloc (* TODO: rq must be loced *) ( match rq with
          | Rqualid q ->
            begin
              let rec qualid_to_pterm (q : liqualid) : pterm =
              match q with
              | Qident i -> mkloc (loc i) (Pvar i)
              | Qdot (q, i) -> mkloc (Location.merge (loc i (* TODO: q must be loced *)) (loc i))
                       (Pdot (qualid_to_pterm q, mkloc (loc i) (Pvar i))) in
              Pcomp(Equal, dumloc (Pconst Ccaller), qualid_to_pterm q)
             end
          | Ror (l, r) ->
            Plogical (Or, process_rexpr l, process_rexpr r)
          | _ -> raise Modelinfo.TODO)
        in
        let condition = dumloc (Pnot (process_rexpr cb)) in
        dumloc (Pif (condition, fail "not_authorized_fun", None ))
      in
      begin
        match tr.calledby with
        | None -> tr
        | Some cb ->
          { tr with
            calledby = None;
            effect = match tr.effect with
              | Some e -> Some (dumloc (Pseq (process_cb cb, e)))
              | None -> Some (process_cb cb);
          }
      end in
    let process_conditions (tr : transaction) =
      let process_condition (x : label_pterm) : pterm =
        let msg =
         match x.label with
         | Some _label -> "check_condition_failed" (*"condition " ^ (unloc label) ^ " failed";*)
         | _ -> "condition failed" in
        mkloc x.loc (Pif (dumloc (Pnot x.term), fail msg, None))
      in
      match tr.condition with
      | None -> tr
      | Some conditions ->
        { tr with
          condition = None;
          effect = List.fold_right (fun x accu ->
              match accu with
              | Some e -> Some (dumloc (Pseq (process_condition x, e)))
              | None -> Some (process_condition x)
            ) conditions tr.effect;
        } in
    tr
    |> process_transition
    |> process_conditions
    |> process_calledby
    ;
  in
  let loc, m = deloc model in
  mkloc loc {
    m with
    transactions = List.map (fun x -> process_ap x) m.transactions;
  }

let sanity_check model : model =
  let _check_dv model : model =
    (model
     |> unloc
     |> (fun x -> x.variables)
     |> List.iter (fun v ->
         let d = v.decl in
         match d.typ, d.default with
         (*       | Some ({pldesc=Tbuiltin VTaddress; _}), None -> raise (Modelinfo.DefaultValueAssignment d.name)*)
         | _ -> ()));
    model in
  model
(*  |> _check_dv*)

let reduce_model (model : Model.model) =
  model
  |> process_action
  |> sanity_check
