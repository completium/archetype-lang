open Location
open Model

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
          let process_effect (_tr : transition) =
            Some (dumloc Pbreak)
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
        dumloc (Pif (condition, dumloc (Papp( dumloc (Pconst Cfail), [])), None ))
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
        mkloc x.loc (Pif (x.term, dumloc (Papp( dumloc (Pconst Cfail), [])), None ))
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
    |> process_calledby
    |> process_conditions;
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
