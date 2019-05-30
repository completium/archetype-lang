open Location
open Model

exception ReduceError of string * Location.t option
exception ErrorAcceptTransfer of string * Location.t * Location.t list
exception TODO

let type_unit : Model.type_ = Tbuiltin VTstring (* TODO: replace unit type *)

let fail str : instruction =
  let f = mk_struct_poly (Pconst Cfail) type_unit in
  let lit = mk_struct_poly (BVstring str) (Tbuiltin VTstring) in
  let arg = mk_struct_poly (Plit lit) (Tbuiltin VTstring) in
  let app = mk_struct_poly (Papp (f, [arg])) type_unit in
  mk_struct_poly (Isimple app) type_unit

(* mk_struct_poly (Plit (dumloc (BVstring str))) *)

(* let check_accept_transfer model =
   let m = unloc model in
   let mo f xs = match xs with | Some xs -> f xs | None -> [] in
   let rec process (e : pterm) : Location.t list = poly_pterm_fold (
      fun (accu : Location.t list) (x : pterm) ->
        let lo, v = deloc x in
        match v with
        | Pconst Ctransferred -> lo::accu
        | _ -> (process x) @ accu) [] (unloc e) in (* TODO: check call function too *)
   let _ = (List.fold_left (fun accu (tr : transaction) ->
      let l = List.flatten (Tools.map_option_neutral (List.map ((fun (x : label_pterm) -> process x.term))) [[]] tr.require) @ (mo process tr.effect) @ accu in
      match l with
      | [] -> accu
      | _ -> (let lo, v = deloc tr.name in
              if (tr.accept_transfer)
              then accu
              else raise (ErrorAcceptTransfer (v, lo, l)))) [] m.transactions) in
   model *)

(* let process_failif model : model =
   let rec process_pterm (pt : pterm) : pterm =
    let l, v = deloc pt in
    (
      let f x = poly_pterm_map_for_pterm (mkloc l) process_pterm x in
      match v with
      | Prequire (b, x) -> mkloc l (Pif ((if b then dumloc (Pnot x) else x), fail "required", None))
      | x -> f x
    ) in
   let l, v = deloc model in
   mkloc l {
    v with
    functions = List.map (fun (x : function_) -> { x with body = process_pterm (x.body) }) v.functions;
    transactions = List.map (fun (x : transaction) -> { x with effect = Tools.map_option process_pterm (x.effect) }) v.transactions;
   } *)

let mk_struct_with_loc node typ loc = let m = mk_struct_poly node typ in {m  with loc = loc; }

let process_action (model : model) : model =
  let process_ap (tr : transaction) : transaction =
    let process_calledby (tr : transaction) : transaction =
      let process_cb (cb : rexpr) : instruction =
        let rec process_rexpr (rq : rexpr) : pterm =
          match rq.node with
          | Rqualid q ->
            begin
              let rec qualid_to_pterm (q : qualid) : pterm =
                match q.node with
                | Qident i -> mk_struct_with_loc (Pvar i) q.type_ q.loc
                | Qdot (q, i) -> (
                    let qq = qualid_to_pterm q in
                    mk_struct_with_loc (Pdot (qq, i)) q.type_ (Location.merge qq.loc (loc i))
                  )
              in
              mk_struct_poly (Pcomp(Equal, mk_struct_poly (Pconst Ccaller) (Tbuiltin VTaddress), qualid_to_pterm q)) (Tbuiltin VTbool)
            end
          | Ror (l, r) ->
            mk_struct_poly (Plogical (Or, process_rexpr l, process_rexpr r)) (Tbuiltin VTbool)
          | Raddress a -> raise TODO (* TODO *) in
        let require : pterm = mk_struct_poly (Pnot (process_rexpr cb)) (Tbuiltin VTbool) in
        mk_struct_poly (Iif (require, fail "not_authorized_fun", None)) type_unit in
      begin
        match tr.calledby with
        | None -> tr
        | Some cb ->
          let instr : instruction = process_cb cb in
          { tr with
            calledby = None;
            effect = match tr.effect with
              | Some e -> Some (mk_struct_poly (Iseq (instr, e)) type_unit)
              | None -> Some instr;
          }
      end
    in

    (* let process_transition (tr : transaction) : transaction =
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
         in *)

    (* let process_requires (tr : transaction) =
       let process_require (x : label_pterm) : pterm =
        let msg =
          match x.label with
          | Some _label -> "check_require_failed" (*"require " ^ (unloc label) ^ " failed";*)
          | _ -> "require failed" in
        mkloc x.loc (Pif (dumloc (Pnot x.term), fail msg, None)) in
       match tr.require with
       | None -> tr
       | Some requires ->
        { tr with
          require = None;
          effect = List.fold_right (fun x accu ->
              match accu with
              | Some e -> Some (dumloc (Pseq (process_require x, e)))
              | None -> Some (process_require x)
            ) requires tr.effect;
        } in *)

    (* let process_accept_transfer (tr : transaction) =
       let at : pterm = dumloc (Pif (dumloc (Pnot (dumloc (Pcomp (Equal,
                                                                 (dumloc (Pconst Ctransferred)),
                                                                 (dumloc (Plit (dumloc (BVcurrency (Tez, Big_int.zero_big_int))))))))),
                                    fail "not_accept_transfer", None)) in
       if (not tr.accept_transfer)
       then { tr with
             effect =
               match tr.effect with
               | Some e -> Some (dumloc (Pseq (at, e)))
               | None -> Some (at)
           }
       else tr in *)

    tr
    (* |> process_transition
       |> process_requires
       |> process_accept_transfer *)
    |> process_calledby
    ;
  in {
    model with
    transactions = List.map (fun x -> process_ap x) model.transactions;
  }
(* |> process_failif *)

(* let sanity_check model : model =
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
   |> check_accept_transfer *)

let reduce_model (model : Model.model) =
  model
  |> process_action
(* |> sanity_check *)
