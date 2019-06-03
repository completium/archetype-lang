open Location
open Ast
open Tools

exception ReduceError of string * Location.t option
exception ErrorAcceptTransfer of string * Location.t * Location.t list
exception TODO

let map_instr_node f = function
  | Iif (c, t, e)       -> Iif (c, f t, f e)
  | Ifor (i, c, b)      -> Ifor (i, c, f b)
  | Iseq is             -> Iseq (List.map f is)
  | Imatchwith (a, ps)  -> Imatchwith (a, ps)
  | Iassign (op, l, r)  -> Iassign (op, l, r)
  | Irequire (b, x)     -> Irequire (b, x)
  | Itransfer x         -> Itransfer x
  | Ibreak              -> Ibreak
  | Iassert x           -> Iassert x
  | Icall (x, id, args) -> Icall (x, id, args)

let map_instr f i =
  {
    i with
    node = map_instr_node f i.node
  }

let type_string   : Model.type_ option = Some (vtstring)
let type_bool     : Model.type_ option = Some (vtbool)
let type_currency : Model.type_ option = Some (vtcurrency Tez)
let type_address  : Model.type_ option = Some (vtaddress)

let unit = mk_sp Ibreak

(* let mk_struct_with_loc node typ loc = let m = mk_struct_poly node typ in {m  with loc = loc; } *)

let fail str : instruction =
  let f = dumloc "fail" in (*mk_sp (Pconst Cfail) in*)
  let lit = mk_sp (BVstring str) ?type_:type_string in
  let arg = mk_sp (Plit lit) ?type_:type_string in
  (* let app = mk_struct_poly (Papp (f, [arg])) type_unit in *)
  mk_sp (Icall (None, f, [arg]))

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
          let m = mk_sp (Pnot x) ?type_:type_bool in
          mk_sp (Iif ((if b then m else x), fail "required", unit)) ?loc:(Some l)
        | _ -> map_instr f instr
      ) in
    map_instr f instr
  in {
    model with
    functions = List.map (fun (x : function_) -> { x with body = process (x.body) }) model.functions;
    transactions = List.map (fun (x : transaction) -> { x with effect = Option.map process (x.effect) }) model.transactions;
  }

let process_action (model : model) : model =
  let process_ap (tr : transaction) : transaction =
    let process_calledby (tr : transaction) : transaction =
      let process_cb (cb : rexpr) body : instruction =
        let rec process_rexpr (rq : rexpr) : pterm =
          match rq.node with
          | Rqualid q ->
            begin
              let rec qualid_to_pterm (q : qualid) : pterm =
                match q.node with
                | Qident i -> mk_sp (Pvar i) ?type_:q.type_ ?loc:(Some q.loc)
                | Qdot (q, i) -> (
                    let qq = qualid_to_pterm q in
                    mk_sp (Pdot (qq, i)) ?type_:q.type_ ?loc:(Some (Location.merge qq.loc (loc i)))
                  )
              in
              mk_sp (Pcomp(Equal, mk_sp (Pconst Ccaller) ?type_:type_address, qualid_to_pterm q)) ?type_:type_bool
            end
          | Ror (l, r) ->
            mk_sp (Plogical (Or, process_rexpr l, process_rexpr r)) ?type_:type_bool
          | Raddress a -> raise TODO (* TODO *) in
        let require : pterm = mk_sp (Pnot (process_rexpr cb)) ?type_:type_bool in
        mk_sp (Iif (require, fail "not_authorized_fun", body)) in
      begin
        let body = Option.get tr.effect in
        match tr.calledby with
        | None -> tr
        | Some cb ->
          let instr : instruction = process_cb cb body in
          { tr with
            calledby = None;
            effect = Some instr;
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
          let state : lident = dumloc "state" in
          let build_code (body : instruction) : instruction =
            (List.fold_right (fun ((id, cond, effect) : (lident * pterm option * instruction option)) acc : instruction ->
                 let tre : instruction =
                   match tr.on with
                   | Some (id, id_asset) ->
                     (
                       let asset : pterm = mk_sp (Pvar id_asset) in
                       let update : lident = dumloc "update" in

                       (* let q : qualid = mk_sp (Qident state) in
                          let aid : pterm = mk_sp (Pvar id) in *)

                       (* let arg : pterm = mk_sp (Precord [q; aid]) in *)
                       let args : pterm list = [] in (*TODO *)

                       mk_sp (Icall (Some asset, update, args))
                     )
                   | _ ->
                     let a : pterm = mk_sp (Pvar id) ?type_:type_bool ?loc:(Some (Location.loc id)) in
                     mk_sp (Iassign (ValueAssign, state, a)) in
                 let code : instruction =
                   match effect with
                   | Some e -> mk_sp (Iseq [tre; e])
                   | None -> tre in

                 match cond with
                 | Some c -> mk_sp (Iif (c, code, acc))
                 | None -> code
               ) tr.trs body)
          in
          let body : instruction = mk_sp (Iseq []) in
          let code : instruction  = build_code body in
          match transition.from.node with
          | Sany -> Some code
          | _ ->
            begin
              let rec compute_patterns (a : sexpr) : ((lident, type_) pattern_gen) list =
                match a.node with
                | Sref id -> [mk_sp (Mconst id)]
                | Sor (a, b) -> [a; b] |> List.map (fun x -> compute_patterns x) |> List.flatten
                | Sany -> raise (ReduceError ("any is not authorized in this expression", Some a.loc)) in
              let list_patterns : ((lident, type_) pattern_gen) list =
                compute_patterns tr.from in

              let pattern : pattern = mk_sp Mwild in
              let fail_instr : instruction = fail "not_valid_state" in

              let w = mk_sp (Pvar state) in
              Some (mk_sp (Imatchwith (w, List.map (fun x -> (x, code)) list_patterns @ [pattern, fail_instr])))

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
      let process_require (x : (lident, pterm) label_term) body : instruction =
        let msg =
          match x.label with
          | Some label -> "require " ^ (unloc label) ^ " failed"
          | _ -> "require failed" in
        let cond : pterm = mk_sp (Pnot x.term) ?loc:(Some x.loc) in
        mk_sp (Iif (cond, fail msg, body)) ?loc:(Some x.loc)
      in
      match tr.require with
      | None -> tr
      | Some requires ->
        let body = Option.get tr.effect in
        { tr with
          require = None;
          effect = Some (List.fold_right (fun (x : (lident, pterm) label_term) (accu : instruction) -> process_require x accu) requires body);
        } in

    let process_accept_transfer (tr : transaction) =
      let lhs : pterm = mk_sp (Pconst Ctransferred) ?type_:type_currency in
      let basic_value : bval = mk_sp (BVcurrency (Tez, Big_int.zero_big_int)) ?type_:type_currency in
      let rhs : pterm = mk_sp (Plit basic_value) ?type_:type_currency in
      let eq : pterm = mk_sp (Pcomp (Equal, lhs, rhs)) ?type_:type_bool in
      let cond : pterm = mk_sp (Pnot eq) ?type_:type_bool in
      let at body : instruction = mk_sp (Iif (cond, fail "not_accept_transfer", body)) in
      if (not tr.accept_transfer)
      then
        let body = Option.get tr.effect in
        { tr with
          effect = Some (at body);
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
(* |> _check_dv *)
(* |> check_accept_transfer *)

let reduce_ast (ast : Ast.model) =
  ast
  |> process_action
  |> sanity_check
