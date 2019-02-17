open Location
open Model
open ParseTree

exception ModelError of string * Location.t

type info = {
  assets : string list
}

let map_option f = function
  | Some x -> Some (f x)
  | None -> None

let map_list = function
  | Some l -> l
  | None -> []

let builtin_type str =
  match str with
  | "bool" -> Some VTbool
  | "int" -> Some VTint
  | "uint" -> Some VTuint
  | "date" -> Some VTdate
  | "duration" -> Some VTduration
  | "string" -> Some VTstring
  | "address" -> Some VTaddress
  | "tez" -> Some (VTcurrency (Tez, None))
  | "mtez" -> Some (VTcurrency (Mutez, None))
  | _ -> None

let to_const str =
  match str with
  | "state" -> Some Cstate
  | "now" -> Some Cnow
  | "transferred" -> Some Ctransferred
  | "caller" -> Some Ccaller
  | "fail" -> Some Cfail
  | "balance" -> Some Cbalance
  | "conditions" -> Some Cconditions
  | "actions" -> Some Cactions
  | "none" -> Some Cnone
  | "any" -> Some Cany
  (* function *)
  | "nth" -> Some Cnth
  | "clear" -> Some Cclear
  | "when" -> Some Cwhen
  | "removeif" -> Some Cremoveif
  | "sort" -> Some Csort
  | "count" -> Some Ccount
  | "sum" -> Some Csum
  | "max" -> Some Cmax
  | "min" -> Some Cmin
  | "enqueue" -> Some Cenqueue
  | "dequeue" -> Some Cdequeue
  | "push" -> Some Cpush
  | "pop" -> Some Cpop
  | "add" -> Some Cadd
  | "addifnotexist" -> Some Caddifnotexist
  | "remove" -> Some Cremove
  | "get" -> Some Cget
  | "contains" -> Some Ccontains
  | "update" -> Some Cupdate
  | "mem" -> Some Cmem
  (* vset *)
  | "before" -> Some Cbefore
  | "after" -> Some Cafter
  | "fixed" -> Some Cfixed
  | "added" -> Some Cadded
  | "removed" -> Some Cremoved
  | _ -> None

let container_to_container (c : ParseTree.container) : Model.container =
  match c with
  | Collection -> Collection
  | Queue -> Queue
  | Stack -> Stack
  | Set -> Set
  | Partition -> Partition


let to_logical_operator (op : ParseTree.logical_operator) : Model.logical_operator =
  match op with
  | And -> And
  | Or -> Or
  | Imply -> Imply
  | Equiv -> Equiv

let to_comparison_operator (op : ParseTree.comparison_operator) : Model.comparison_operator =
  match op with
  | Equal  -> Equal
  | Nequal -> Nequal
  | Gt     -> Gt
  | Ge     -> Ge
  | Lt     -> Lt
  | Le     -> Le

let to_arithmetic_operator (op : ParseTree.arithmetic_operator) : Model.arithmetic_operator =
  match op with
  | Plus   -> Plus
  | Minus  -> Minus
  | Mult   -> Mult
  | Div    -> Div
  | Modulo -> Modulo

let to_assignment_operator (op : ParseTree.assignment_operator) : Model.assignment_operator =
  match op with
  | ValueAssign -> ValueAssign
  | SimpleAssign -> SimpleAssign
  | PlusAssign -> PlusAssign
  | MinusAssign -> MinusAssign
  | MultAssign -> MultAssign
  | DivAssign -> DivAssign
  | AndAssign -> AndAssign
  | OrAssign -> OrAssign

let to_quantifier (op : ParseTree.quantifier) : Model.quantifier =
  match op with
  | Forall -> Forall
  | Exists -> Exists

let to_vset id =
  let loc, v = deloc id in
  match v with
  | "removed" -> VSremoved
  | "added" -> VSadded
  | "stable" -> VSstable
  | "before" -> VSbefore
  | "after" -> VSafter
  | "fixed" -> VSfixed
  | _ -> raise (ModelError (Format.sprintf "cannot convert %s to vset" v, loc))

let rec mk_ptyp e : ptyp =
  let loc, v = deloc e in
  mkloc loc
    (match v with
     | Tref v ->
       (let b = builtin_type (unloc v) in
        match b with
        | Some u -> Tbuiltin u
        | None -> Tasset v)
     | Tcontainer (t, container) -> Tcontainer ((mk_ptyp t), container_to_container container)
     | Tapp (f, v) -> Tapp (mk_ptyp f, mk_ptyp v)
     | Ttuple l -> Ttuple (List.map mk_ptyp l)
     | _ -> raise (ModelError ("mk_ptyp: unsupported type ", loc)))

let rec mk_ltyp e : ltyp=
  let loc, v = deloc e in
  mkloc loc
    (match v with
     | Tvset (vs, t) -> LTvset (to_vset vs, mk_ltyp t)
     | _ -> LTprog (mk_ptyp e))

let to_bval l =
  match l with
  | Lnumber b -> BVint b
  | Lfloat s -> BVfloat s
  | Laddress s -> BVaddress s
  | Lstring s -> BVstring s
  | Lbool b -> BVbool b
  | Lduration s -> BVduration s
  | Ldate s -> BVdate s
  | Ltz n -> BVcurrency (Tez, n)

let mk_lterm_id (id : lident) =
  let c = id |> unloc |> to_const in
  match c with
  | Some d -> Lconst d
  | None -> Lvar id

let rec mk_lterm (e : expr) : lterm =
  let loc, v = deloc e in
  mkloc loc (
    match v with
    | Eterm t -> (match t with
        | (Some _a, _e) -> Llit (mkloc loc (BVstring "TODO: Namespace"))
        | (None, e) -> mk_lterm_id e)

    | Eop _ -> raise (ModelError ("operation error", loc))
    | Eliteral l -> Llit (mkloc loc (to_bval l))
    | Earray l -> Larray (List.map mk_lterm l)
    | Edot (e, i) -> Ldot (mk_lterm e, mkloc (i |> Location.loc) (mk_lterm_id i))
    | EassignFields _l -> raise (ModelError ("assignment fields are not allowed in logical block", loc))
    | Eapp ({pldesc=Eop op; _}, [lhs; rhs]) ->
      (
        match op with
        | `Logical Imply -> Limply   (mk_lterm lhs, mk_lterm rhs)
        | `Logical o     -> Llogical (to_logical_operator o, mk_lterm lhs, mk_lterm rhs)
        | `Cmp o         -> Lcomp    (to_comparison_operator o, mk_lterm lhs, mk_lterm rhs)
        | `Arith o       -> Larith   (to_arithmetic_operator o, mk_lterm lhs, mk_lterm rhs)
        | _ -> raise (ModelError ("binary operation not valid", loc))
      )
    | Eapp ({pldesc=Eop op; _}, [e]) ->
      (
        match op with
        | `Unary Not -> Lnot (mk_lterm e)
        | `Unary Uplus -> Luarith (Uplus, mk_lterm e)
        | `Unary Uminus -> Luarith (Uminus, mk_lterm e)
        | _ -> raise (ModelError ("unary operation not valid", loc))
      )
    | Eapp (f, args) -> Lapp (mk_lterm f, List.map mk_lterm args)
    | Etransfer (_a, _, _dest) -> raise (ModelError ("\"transfer\" is not allowed in logical block", loc))
    | Eassign (_, _, _) -> raise (ModelError ("assignment is not allowed in logical block", loc))
    | Eif (_cond, _then_, _else_) -> raise (ModelError ("\"if\" is not allowed in logical block", loc))
    | Ebreak -> raise (ModelError ("\"break\" is not allowed in logical block", loc))
    | Efor (_, _, _, _) -> raise (ModelError ("\"for\" is not allowed in logical block", loc))
    | Eassert _ -> raise (ModelError ("\"assert\" is not allowed in logical block", loc))
    | Eseq (lhs, rhs) ->
      (let l = (let a = mk_lterm lhs in match a with | {pldesc = Lseq la; _} -> la | _ -> [a]) @
               (let a = mk_lterm rhs in match a with | {pldesc = Lseq la; _} -> la | _ -> [a]) in
       Lseq l)
    | Efun (args, body) ->
      (
        match List.rev args with
       | [] -> raise (ModelError ("no argument in lamda", loc))
       | (ia, it, _)::t ->
           List.fold_left (
           fun acc i ->
             let id, typ, _ = i in
             Llambda (id, map_option mk_ltyp typ, mkloc dummy acc)
           ) (Llambda (ia, map_option mk_ltyp it, mk_lterm body)) t)
    | Eletin ((i, typ, _), init, body) -> Lletin (i, mk_lterm init,
                                                  map_option mk_ltyp typ, mk_lterm body)
    | Ematchwith _ -> raise (ModelError ("match with is not allowed in logical block", loc))
    | Equantifier (q, (id, t, _), e) -> Lquantifer (to_quantifier q, id, map_option mk_ltyp t, mk_lterm e))

let rec mk_qualid (q : ParseTree.qualid) : lident Model.qualid =
  match q with
  | Qident i -> Qident i
  | Qdot (q, i) -> Qdot (mk_qualid q, i)

let mk_pterm_id id =
  let c = id |> unloc |> to_const in
  match c with
  | Some d -> Pconst d
  | None -> Pvar id

let mk_pattern pt : Model.pattern =
  let loc, v = deloc pt in
  mkloc loc (match v with
  | Pwild -> Mwild
  | Pref i -> Mvar i)

let rec mk_pterm (e : expr) : pterm =
  let loc, v = deloc e in
  mkloc loc  (
    match v with
    | Eterm t -> (match t with
        | (_, e) -> mk_pterm_id e)
    | Eop _ -> raise (ModelError ("operation error", loc))
    | Eliteral l -> Plit (mkloc loc (to_bval l))
    | Earray l -> Parray (List.map mk_pterm l)
    | Edot (e, i) -> Pdot (mk_pterm e, mkloc (i |> Location.loc) (mk_pterm_id i))
    | EassignFields l ->
      Pfassign (List.map
                  (fun i ->
                     let (op, (a, f), e) = i in
                     (to_assignment_operator op, (a, f), mk_pterm e)) l)
    | Eapp ({pldesc=Eop op; plloc=locop}, [lhs; rhs]) ->
      (
        match op with
        | `Logical Imply -> raise (ModelError ("Imply operator is not allowed in programming block", locop))
        | `Logical o -> Plogical (to_logical_operator o, mk_pterm lhs, mk_pterm rhs)
        | `Cmp o     -> Pcomp    (to_comparison_operator o, mk_pterm lhs, mk_pterm rhs)
        | `Arith o   -> Parith   (to_arithmetic_operator o, mk_pterm lhs, mk_pterm rhs)
        | _ -> raise (ModelError ("binary operation not valid", locop))
      )
    | Eapp ({pldesc=Eop op; _}, [e]) ->
      (
        match op with
        | `Unary Not -> Pnot (mk_pterm e)
        | `Unary Uplus -> Puarith (Uplus, mk_pterm e)
        | `Unary Uminus -> Puarith (Uminus, mk_pterm e)
        | _ -> raise (ModelError ("unary operation not valid", loc))
      )
    | Eapp (f, args) -> Papp (mk_pterm f, List.map mk_pterm args)
    | Etransfer (a, back, dest) -> Ptransfer (mk_pterm a, back, map_option mk_qualid dest)
    | Eassign (op, lhs, rhs) -> Passign (to_assignment_operator op, mk_pterm lhs, mk_pterm rhs)
    | Eif (cond, then_, else_) -> Pif (mk_pterm cond, mk_pterm then_, map_option mk_pterm else_)
    | Ebreak -> Pbreak
    | Efor (i, e, body, _name) -> Pfor (i, mk_pterm e, mk_pterm body)
    | Eassert e -> Passert (mk_lterm e)
    | Eseq (lhs, rhs) ->
      (let l = (let a = mk_pterm lhs in match a with | {pldesc = Pseq la; _} -> la | _ -> [a]) @
               (let a = mk_pterm rhs in match a with | {pldesc = Pseq la; _} -> la | _ -> [a]) in
       Pseq l)
    | Efun (args, body) ->
      (
        match List.rev args with
       | [] -> raise (ModelError ("no argument in lamda", loc))
       | (ia, it, _)::t ->
           List.fold_left (
           fun acc i ->
             let id, typ, _ = i in
             Plambda (id, map_option mk_ptyp typ, mkloc dummy acc)
           ) (Plambda (ia, map_option mk_ptyp it, mk_pterm body)) t)
    | Eletin ((i, typ, _), init, body) -> Pletin (i, mk_pterm init,
                                                  map_option mk_ptyp typ, mk_pterm body)
    | Ematchwith (e, l) ->
      let ll = List.fold_left
                    (fun acc (pts, e) -> (
                        (List.map (fun x -> (mk_pattern x, mk_pterm e)) pts)@acc
                        )) [] l in
      Pmatchwith (mk_pterm e, ll)
    | Equantifier _ -> raise (ModelError ("Quantifiers are not allowed in programming block", loc)))


let to_label_lterm (label, lterm) : label_lterm =
  {
    label = label;
    term = mk_lterm lterm;
    loc = dummy;
  }

let to_label_pterm (label, pterm) : label_pterm =
  {
    label = label;
    term = mk_pterm pterm;
    loc = dummy;
  }

let get_name_model (pt : ParseTree.model) : lident =
  let loc = loc pt in
  let ptu = Location.unloc pt in
  match ptu with
  | Mmodel decls ->
    (let res = List.fold_left (fun acc i -> (
           let decl_u = Location.unloc i in
           match decl_u with
           | Dmodel id -> (match acc with
               | None -> Some (unloc id)
               | _ -> raise (ModelError ("only one name can be set to model.", loc)))
           | _ -> acc)) None decls
     in
      match res with
      | Some id -> (dumloc id)
      | _ -> raise (ModelError ("no name for model found.", loc)))
  | _ -> raise (ModelError ("only ParseTree.model can be translated into Model.model.", loc))

let to_rexpr_dv_role e =
  let loc, v = deloc e in
  match v with
  | Eliteral l -> (
      match l with
      | Laddress a -> Raddress a
      | _ -> raise (ModelError ("only address is supported", loc)) )
  | Eterm (_, id) -> Rrole id
  (*  | Eapp ({pldesc = {pldesc = Eop}, args, _}) ->*)
  | _ -> raise (ModelError ("wrong type for ", loc))

let rec to_rexpr_calledby (e : ParseTree.expr) : rexpr =
  let loc, v = deloc e in
  match v with
  | Eterm (None, id) -> Rasset (dumloc "", id)
  | Edot (_, id) -> Rasset (id, id)
  | Eapp ({pldesc = Eop (`Logical Or); _}, args) ->
    ( let lhs = to_rexpr_calledby (List.nth args 0) in
      let rhs = to_rexpr_calledby (List.nth args 1) in
      Ror (lhs, rhs))
  | _ -> raise (ModelError ("type error: called by", loc))

let rec to_sexpr (e : expr) : Model.sexpr =
  let loc, v = deloc e in
  match v with
  | Eterm (None, id) -> mkloc loc (
      match id |> unloc |> to_const with
      | Some Cany -> Sany
      | _ -> Sref id)
  | Eapp ({pldesc = Eop (`Logical Or); _}, args) ->
    ( let lhs = to_sexpr (List.nth args 0) in
      let rhs = to_sexpr (List.nth args 1) in
      mkloc loc (Sor (lhs, rhs)))
  | _ -> raise (ModelError ("wrong type for ", loc))

let get_roles decls =
  List.fold_left ( fun acc i ->
      (let loc = loc i in
       let decl_u = Location.unloc i in
       match decl_u with
       | Drole (id, dv, _) ->
         {name = id; default = map_option to_rexpr_dv_role dv; loc = loc}::acc
       | _ -> acc)
    ) [] decls

let mk_bval e =
  let loc, v = deloc e in
  mkloc loc
    (match v with
     | Eliteral l -> to_bval l
     | _ -> raise (ModelError ("mk_bval: wrong type for ", loc)))

let mk_label_term mk item =
  let (name, v) = item in {
    label = name;
    term = v |> mk;
    loc = v |> loc;
  }

let mk_label_lterm =
  mk_label_term mk_lterm

let mk_decl loc ((id, typ, dv) : (lident * type_t option * expr option)) =
  {
    name = id;
    typ = map_option mk_ptyp typ;
    default = map_option mk_bval dv;
    loc = loc;
  }

let mk_decl_pterm loc ((id, typ, dv) : (lident * type_t option * expr option)) =
  {
    name = id;
    typ = map_option mk_ptyp typ;
    default = map_option mk_pterm dv;
    loc = loc;
  }

let mk_spec loc (vars : (lident * type_t * expr option) loced list option) action (invs : named_item list option) items = {
  variables = List.fold_left
      (fun acc i ->
         let loc, (id, typ, dv) = deloc i in
         ({
           decl = mk_decl_pterm loc (id, Some typ, dv);
           constant = false;
           loc = loc;
         })::acc) [] (vars |> map_list);
  action = map_option mk_pterm action;
  invariants = List.map mk_label_lterm (map_list invs);
  ensures = List.map mk_label_lterm items;
  loc = loc;
}

let mk_simple_spec loc items =
  mk_spec loc None None None items

let get_variables decls =
  List.fold_left ( fun acc i ->
      (let loc = loc i in
       let decl_u = Location.unloc i in
       match decl_u with
       | Dconstant (id, typ, dv, _) ->
         {decl = mk_decl_pterm loc (id, Some typ, dv); constant = true; loc = loc }::acc
       | Dvariable (id, typ, _, dv, _) ->
         {decl = mk_decl_pterm loc (id, Some typ, dv); constant = false; loc = loc }::acc
       | _ -> acc)
    ) [] decls

let get_asset_key opts =
  let default = Location.dumloc "_id" in
  match opts with
  | None -> default
  | Some opts ->
    (let id = (List.fold_left (fun acc i ->
         match i with
         | AOidentifiedby id -> Some id
         | _ -> acc) None opts) in
     match id with
     | Some i -> i
  | _ -> default)

let is_asset_role opts =
  match opts with
  | None -> false
  | Some opts -> List.fold_left (fun acc i ->
         match i with
         | AOasrole -> true
         | _ -> acc) false opts

let get_asset_fields fields =
    match fields with
      | None -> []
      | Some fields -> (List.fold_left (fun acc i ->
          let loc, v = deloc i in
          match v with
          | Ffield (id, typ, dv, _) -> mk_decl loc (id, Some typ, dv)::acc
        ) [] fields)

let get_assets decls =
  List.fold_left ( fun acc i ->
      (let loc, decl_u = Location.deloc i in
       match decl_u with
       | Dasset (id, fields, _cs, opts, init, _ops) ->
         ({
           name = id;
           args = get_asset_fields fields;
           key = get_asset_key opts;
           sort = None;
           role = is_asset_role opts;
           init = map_option mk_pterm init;
           preds = None;
           loc = loc;
         })::acc
       | _ -> acc)
    ) [] decls

let get_functions decls =
  List.fold_left ( fun acc i ->
      (let loc = loc i in
       let decl_u = Location.unloc i in
       match decl_u with
       | Dfunction (name, _args, typ, body) ->
         {name = name;
          args = [];
          return = map_option mk_ptyp typ;
          body = mk_pterm body;
          loc = loc;}::acc
       | _ -> acc)
    ) [] decls

let get_transaction_args (args : ParseTree.args)  =
  List.fold_left (fun acc (i : lident_typ) ->
      let name, typ, _ = i in
      mk_decl dummy (name, typ, None)::acc
    ) [] args

let get_transaction_calledby items : rexpr option =
  List.fold_left (fun acc i ->
      let loc, v = deloc i in
      match v with
      | Tcalledby (e, _) ->
        (match acc with
        | None -> Some (to_rexpr_calledby e)
        | _ -> raise (ModelError ("several called by found", loc)))
      | _ -> acc
    ) None items

let get_transaction_condition items : label_pterm list option =
  List.fold_left (fun acc i ->
      let loc, v = deloc i in
      match v with
      | Tcondition (items, _) ->
        (match acc with
         | None -> (Some (List.map (fun a -> let b, c = a in (to_label_pterm (b, c))) items))
         | _ -> raise (ModelError ("several condition found", loc)))
      | _ -> acc
    ) None items

let get_transaction_action items : pterm option =
  List.fold_left (fun acc i ->
      let loc, v = deloc i in
      match v with
      | Taction (e, _) ->
        (match acc with
         | None -> Some (mk_pterm e)
         | _ -> raise (ModelError ("several action found", loc)))
      | _ -> acc
    ) None items

let get_transaction_transition items =
  List.fold_left (fun acc i ->
      let loc, v = deloc i in
      match v with
      | Ttransition (from, to_, _id,  _) ->
        (match acc with
         | None -> Some (to_sexpr from, to_, None)
         | _ -> raise (ModelError ("several transition found", loc)))
      | _ -> acc
    ) None items

let get_transaction_specification items =
  List.fold_left (fun acc i ->
      let loc, v = deloc i in
      match v with
      | Tspecification (vars, action, invs, ensure, _) ->
        (match acc with
         | None -> Some (mk_spec loc vars action invs ensure)
         | _ -> raise (ModelError ("several specification found", loc)))
      | _ -> acc
    ) None items

let get_transactions decls =
  List.fold_left (fun acc i ->
      (let loc, v = deloc i in
       match v with
       | Dtransaction (name, args, items, _) ->
         {
           name = name;
           args = get_transaction_args args;
           calledby = get_transaction_calledby items;
           condition = get_transaction_condition items;
           transition = get_transaction_transition items;
           spec = get_transaction_specification items;
           action = get_transaction_action items;
           loc = loc;
         }::acc
       | _ -> acc)
    ) [] decls

let get_enums decls =
  List.fold_left ( fun acc i ->
      (let loc = loc i in
       let decl_u = Location.unloc i in
       match decl_u with
       | Denum (name, list) ->
         {name = name;
          vals = list;
          loc = loc;}::acc
       | _ -> acc)
    ) [] decls

let is_state_initial = function
  | None -> false
  | Some opts ->
  List.fold_left (fun acc i ->
      match i with
      | SOinitial -> true
      | _ -> acc
    ) false opts

let get_state_specifications (opts : state_option list) : Model.specification =
  let es = List.fold_left (fun acc i ->
          match i with
          | SOspecification xs -> List.map to_label_lterm xs @ acc
          | _ -> acc
        ) [] opts in
    {
      variables = [];
      action = None;
      invariants = [];
      ensures = es;
      loc = dummy;
    }

let get_states_items items =
  List.fold_left (fun acc i ->
      (let (name, opts) = i in
        {name = name;
         initial = is_state_initial opts;
         specs = map_option get_state_specifications opts;
         loc = Location.dummy;
       }::acc)) [] items

let get_states decls =
  List.fold_left (fun acc i ->
      (let loc, v = deloc i in
       match v with
       | Dstates (name, items) ->
         {name = (match name with | None -> dumloc "_global" | Some a -> a);
          items = get_states_items items;
          loc = loc;}::acc
       | _ -> acc)
    ) [] decls

let get_enums decls =
  List.fold_left ( fun acc i ->
      (let loc, decl_u = deloc i in
       match decl_u with
       | Denum (name, list) ->
         {name = name;
          vals = list;
          loc = loc;}::acc
       | _ -> acc)
    ) [] decls

let get_specs decls =
  List.fold_left ( fun acc i ->
      (let loc, decl_u = deloc i in
       match decl_u with
       | Dspecification (items, _) ->
         (mk_simple_spec loc items)::acc
       | _ -> acc)
    ) [] decls

let parseTree_to_model (pt : ParseTree.model) : Model.model =
  let ptu = Location.unloc pt in
  let decls = match ptu with
  | Mmodel decls -> decls
  | _ -> [] in

  mkloc (loc pt) {
    name          = get_name_model pt;
    roles         = get_roles decls;
    variables     = get_variables decls;
    assets        = get_assets decls;
    functions     = get_functions decls;
    transactions  = get_transactions decls;
    states        = get_states decls;
    enums         = get_enums decls;
    specs         = get_specs decls;
  }

let string_to_pterm (str : string) : Model.pterm =
  let lb = Lexing.from_string str in
  let pt = Parser.start_expr Lexer.token lb in
  mk_pterm pt
