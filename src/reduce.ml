open Location
open Model

exception ReduceError of string * Location.t option
exception ErrorAcceptTransfer of string * Location.t * Location.t list
exception TODO


let map_instr_node f = function
  | Iif (c, t, e) -> Iif (c, f t, Tools.map_option f e)
  | Ifor (i, c, b) -> Ifor (i, c, f b)
  | Iseq (l, h) -> Iseq (f l, f h)
  | Imatchwith (a, ps) -> Imatchwith (a, ps)
  | Iassign (op, l, r) -> Iassign (op, l, r)
  | Irequire (b, x) -> Irequire (b, x)
  | Itransfer x -> Itransfer x
  | Ibreak -> Ibreak
  | Iassert x -> Iassert x
  | Isimple x -> Isimple x

let map_instr f i =
  {
    i with
    node = map_instr_node f i.node
  }

let type_unit     : Model.type_ = Tbuiltin VTstring (* TODO: replace unit type *)
let type_string   : Model.type_ = Tbuiltin VTstring
let type_bool     : Model.type_ = Tbuiltin VTbool
let type_currency : Model.type_ = Tbuiltin (VTcurrency Tez)

let mk_struct_with_loc node typ loc = let m = mk_struct_poly node typ in {m  with loc = loc; }

let mk_instr (node : (lident, type_, pterm, instruction) instruction_node) = mk_struct_poly node type_unit
let mk_instr_with_loc (node : (lident, type_, pterm, instruction) instruction_node) loc = mk_struct_with_loc node type_unit loc

let fail str : instruction =
  let f = mk_struct_poly (Pconst Cfail) type_unit in
  let lit = mk_struct_poly (BVstring str) (Tbuiltin VTstring) in
  let arg = mk_struct_poly (Plit lit) (Tbuiltin VTstring) in
  let app = mk_struct_poly (Papp (f, [arg])) type_unit in
  mk_instr (Isimple app)

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

let replace_instruction model : model =
  let process (instr : instruction) : instruction =
    let rec f (instr : instruction) : instruction =
      let l = instr.loc in
      (
        match instr.node with
        | Irequire (b, x) ->
          mk_instr_with_loc (Iif ((if b then mk_struct_poly (Pnot x) type_bool else x), fail "required", None)) l
        | _ -> map_instr f instr
      ) in
    map_instr f instr
  in {
    model with
    functions = List.map (fun (x : function_) -> { x with body = process (x.body) }) model.functions;
    transactions = List.map (fun (x : transaction) -> { x with effect = Tools.map_option process (x.effect) }) model.transactions;
  }

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
        mk_instr (Iif (require, fail "not_authorized_fun", None)) in
      begin
        match tr.calledby with
        | None -> tr
        | Some cb ->
          let instr : instruction = process_cb cb in
          { tr with
            calledby = None;
            effect = match tr.effect with
              | Some e -> Some (mk_instr (Iseq (instr, e)))
              | None -> Some instr;
          }
      end
    in

    let process_transition (tr : transaction) : transaction =
      match tr.transition with
      | None -> tr
      | Some transition ->
        let process_args tr =
          match tr.on with
          | Some (id, id2) ->
            [{name = id;
              typ = Some (Tasset id2);
              default = None;
              loc = Location.merge (loc id) (loc id2)}]
          | None -> [] in
        let process_effect (tr : (lident, type_, pterm, instruction) transition) : instruction option =
          let state : pterm =
            match tr.on with
            | Some (_id, id2) ->
              begin
                let a : pterm = mk_struct_poly (Pvar id2) type_unit in
                mk_struct_poly (Pdot (a, dumloc "state")) type_unit
              end
            | _ -> mk_struct_poly (Pconst Cstate) type_unit in
          let code : instruction =
            (List.fold_right (fun ((id, cond, effect) : (lident * pterm option * instruction option)) acc : instruction option ->
                 let tre : instruction =
                   match tr.on with
                   | Some (id, id_asset) ->
                     (
                       let asset : pterm = mk_struct_poly (Pvar id_asset) type_unit in
                       let update : lident = dumloc "update" in
                       let f : pterm = mk_struct_poly (Pdot (asset, update)) type_unit in

                       let q : qualid = mk_struct_poly (Qident (dumloc "state")) type_unit in
                       let aid : pterm = mk_struct_poly (Pvar id) type_unit in

                       let arg : pterm = mk_struct_poly (Precord [q, aid]) type_unit in
                       let args : pterm list = [arg] in

                       let a : (lident, type_, pterm) pterm_node = (Papp (f, args)) in
                       let app : pterm = mk_struct_poly a type_unit in
                       mk_instr (Isimple app)
                     )
                   | _ ->
                     mk_instr (Iassign (ValueAssign, state, mk_struct_with_loc (Pvar id) type_bool (Location.loc id))) in
                 let code : instruction =
                   match effect with
                   | Some e -> mk_instr (Iseq (tre, e))
                   | None -> tre in

                 match cond with
                 | Some c -> Some (mk_instr (Iif (c, code, acc)))
                 | None -> Some code
               ) tr.trs None)
            |> Tools.get
          in

          match transition.from.node with
          | Sany -> Some code
          | _ ->
            begin
              let rec compute_patterns (a : sexpr) : ((lident, type_) pattern_gen) list =
                match a.node with
                | Sref id -> [mk_struct_poly (Mapp (mk_struct_poly (Qident id) type_unit, [])) type_unit]
                | Sor (a, b) -> [a; b] |> List.map (fun x -> compute_patterns x) |> List.flatten
                | Sany -> raise (ReduceError ("any is not authorized in this expression", Some a.loc)) in
              let list_patterns : ((lident, type_) pattern_gen) list =
                compute_patterns tr.from in

              let pattern : pattern = mk_struct_poly Mwild type_unit in
              let instr : instruction = fail "not_valid_state" in
              Some (mk_instr (Imatchwith (state, List.map (fun x -> (x, code)) list_patterns @ [pattern, instr])))

            end
        in
        let args : ((lident, type_, type_ bval_gen) decl_gen) list = tr.args @ (process_args transition) in
        { tr with
          transition = None;
          args = args;
          effect = process_effect transition;
        }
    in

    let process_requires (tr : transaction) =
      let process_require (x : (lident, pterm) label_term) : instruction =
        let msg =
          match x.label with
          | Some label -> "require " ^ (unloc label) ^ " failed"
          | _ -> "require failed" in
        let cond : pterm = mk_struct_with_loc (Pnot x.term) type_unit x.loc in
        mk_instr_with_loc (Iif (cond, fail msg, None)) x.loc
      in
      match tr.require with
      | None -> tr
      | Some requires ->
        { tr with
          require = None;
          effect = List.fold_right (fun x accu ->
              let instr : instruction = process_require x in
              match accu with
              | Some e -> Some (mk_instr (Iseq (instr, e)))
              | None -> Some instr
            ) requires tr.effect;
        } in

    let process_accept_transfer (tr : transaction) =
      let lhs : pterm = mk_struct_poly (Pconst Ctransferred) type_currency in
      let basic_value : bval = mk_struct_poly (BVcurrency (Tez, Big_int.zero_big_int)) type_currency in
      let rhs : pterm = mk_struct_poly (Plit basic_value) type_currency in
      let eq : pterm = mk_struct_poly (Pcomp (Equal, lhs, rhs)) type_bool in
      let cond : pterm = mk_struct_poly (Pnot eq) type_bool in
      let at : instruction = mk_instr (Iif (cond, fail "not_accept_transfer", None)) in
      if (not tr.accept_transfer)
      then { tr with
             effect =
               match tr.effect with
               | Some e -> Some (mk_instr (Iseq (at, e)))
               | None -> Some at
           }
      else tr in

    tr
    |> process_transition
    |> process_requires
    |> process_accept_transfer
    |> process_calledby
    ;
  in {
    model with
    transactions = List.map (fun x -> process_ap x) model.transactions;
  }
    |> replace_instruction

let sanity_check model : model =
  (* let _check_dv model : model =
     (model
     |> unloc
     |> (fun x -> x.variables)
     |> List.iter (fun v ->
         let d = v.decl in
         match d.typ, d.default with
         (*       | Some ({pldesc=Tbuiltin VTaddress; _}), None -> raise (Modelinfo.DefaultValueAssignment d.name)*)
         | _ -> ()));
     model in *)
  model
(*  |> _check_dv*)
(* |> check_accept_transfer *)

let reduce_ast (model : Model.model) =
  model
  |> process_action
  |> sanity_check
