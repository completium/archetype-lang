open Location
open Model
open ParseTree
open Tools

exception ModelError0 of string
exception ModelError  of string * Location.t
exception ModelError2 of string * Location.t * Location.t

type info = {
  assets : string list
}

let dummy_gen_decl = {
  name = dumloc "dummy";
  typ = None;
  default = None;
  loc = Location.dummy;
}

let dummy_variable : variable = {
  decl = dummy_gen_decl;
  constant = false;
  from = None;
  to_ = None;
  loc = Location.dummy;
}

let dummy_verif = {
  predicates  = [];
  definitions = [];
  axioms      = [];
  theorems    = [];
  variables   = [];
  invariants  = [];
  effect      = None;
  specs       = [];
  loc         = Location.dummy;
}

let builtin_type str =
  match str with
  | "bool" -> Some VTbool
  | "int" -> Some VTint
  | "uint" -> Some VTuint
  | "date" -> Some VTdate
  | "duration" -> Some VTduration
  | "string" -> Some VTstring
  | "address" -> Some VTaddress
  | "role" -> Some VTaddress
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
  | "select" -> Some Cselect
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
  | Queue      -> Queue
  | Stack      -> Stack
  | Set        -> Set
  | Partition  -> Partition

let to_logical_operator (op : ParseTree.logical_operator) : Model.logical_operator =
  match op with
  | And   -> And
  | Or    -> Or
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
  | "added"   -> VSadded
  | "stable"  -> VSstable
  | "before"  -> VSbefore
  | "after"   -> VSafter
  | "fixed"   -> VSfixed
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
     | _ -> raise (ModelError ("unsupported type: " ^ (show_type_r v), loc)))

let rec mk_ltyp e : ltyp=
  let loc, v = deloc e in
  mkloc loc
    (match v with
     | Tvset (vs, t) -> LTvset (to_vset vs, mk_ltyp t)
     | _ -> LTprog (mk_ptyp e))

let to_bval l =
  match l with
  | Lnumber b -> BVint b
  | Lrational (n, d) -> BVrational (n, d)
  | Ltz n -> BVcurrency (Tez, n)
  | Laddress s -> BVaddress s
  | Lstring s -> BVstring s
  | Lbool b -> BVbool b
  | Lduration s -> BVduration s
  | Ldate s -> BVdate s

let mk_lterm_id (id : lident) =
  let c = id |> unloc |> to_const in
  match c with
  | Some d -> Lconst d
  | None -> Lvar id

let compute_term = function
  | (Some a, e) -> mkloc (Location.merge (loc a) (loc e) ) ("_" ^ (unloc a) ^ "_" ^ (unloc e))
  | (None, e) -> e

let process_fun f ty c loc (args, body) =
  match List.rev args with
  | [] -> raise (ModelError ("no argument in lamda", loc))
  | (ia, it, _)::t ->
    List.fold_left (
      fun acc i ->
        let id, typ, _ = i in
        c (id, map_option ty typ, false, mkloc loc acc)
    ) (c (ia, map_option ty it, false, f body)) t

let rec mk_lterm (e : expr) : lterm =
  let loc, v = deloc e in
  mkloc loc (
    match v with
    | Eterm t -> mk_lterm_id (compute_term t)
    | Eop _ -> raise (ModelError ("operation error", loc))
    | Eliteral l -> Llit (mkloc loc (to_bval l))
    | Earray l -> Larray (None, List.map mk_lterm l)
    | Edot (e, i) -> Ldot (mk_lterm e, mkloc (i |> Location.loc) (mk_lterm_id i))
    | Erecord _ -> raise (ModelError ("assignment fields are not allowed in logical block", loc))
    | Etuple l -> Ltuple (List.map (fun x -> mk_lterm x) l)
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
    | Eassign (_, _, _) -> raise (ModelError ("assignments are not allowed in logical block", loc))
    | Eif (cond, then_, None) -> Limply (mk_lterm cond, mk_lterm then_)
    | Eif (cond, then_, Some else_) -> Llogical (And,
                                                 mkloc loc (Limply (mk_lterm cond, mk_lterm then_)),
                                                 mkloc loc (Limply (mkloc (Location.loc cond) (Lnot (mk_lterm cond)), mk_lterm else_)))
    | Ebreak -> raise (ModelError ("\"break\" is not allowed in logical block", loc))
    | Efor (_, _, _) -> raise (ModelError ("\"for\" is not allowed in logical block", loc))
    | Eassert _ -> raise (ModelError ("\"assert\" is not allowed in logical block", loc))
    | Eseq (lhs, rhs) -> Lseq (mk_lterm lhs, mk_lterm rhs)
    | Efun (args, body) -> process_fun mk_lterm mk_ltyp (fun (w, x, y, z) -> Llambda (w, x, y, z)) loc (args, body)
    | Eletin ((i, typ, _), init, body, _other) -> Lletin (i, mk_lterm init,
                                                  map_option mk_ltyp typ, mk_lterm body)
    | Ematchwith _ -> raise (ModelError ("match with is not allowed in logical block", loc))
    | Equantifier (q, (id, t, _), e) -> Lquantifer (to_quantifier q, id, map_option mk_ltyp t, mk_lterm e)
    | Elabel _ -> raise (ModelError ("labels are not allowed in logical block", loc)))

let rec mk_qualid (q : ParseTree.qualid) : liqualid =
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
    | Eterm t -> mk_pterm_id (compute_term t)
    | Eop _ -> raise (ModelError ("operation error", loc))
    | Eliteral l -> Plit (mkloc loc (to_bval l))
    | Earray l -> Parray (List.map mk_pterm l)
    | Edot (e, i) -> Pdot (mk_pterm e, mkloc (i |> Location.loc) (mk_pterm_id i))
    | Erecord l -> Pfassign (List.map
                               (fun i ->
                                  let (a, e) = i in
                                  let b = map_option (fun (op, id) ->
                                      (to_assignment_operator op, id)) a in
                                  (b, mk_pterm e)) l)
    | Etuple l -> Ptuple (List.map (fun x -> mk_pterm x) l)
    | Eapp ({pldesc=Eop op; plloc=locop}, [lhs; rhs]) ->
      (
        match op with
        | `Logical Imply -> raise (ModelError ("imply operator is not allowed in programming block", locop))
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
    | Efor (i, e, body) -> Pfor (i, mk_pterm e, mk_pterm body, None)
    | Eassert e -> Passert (mk_lterm e)
    | Eseq (lhs, rhs) -> Pseq (mk_pterm lhs, mk_pterm rhs)
    | Efun (args, body) -> process_fun mk_pterm mk_ptyp (fun (w, x, y, z) -> Plambda (w, x, y, z)) loc (args, body)
    | Eletin ((i, typ, _), init, body, _) -> Pletin (i, mk_pterm init, map_option mk_ptyp typ, mk_pterm body)
    | Ematchwith (e, l) ->
      let ll = List.fold_left
                    (fun acc (pts, e) -> (
                        (List.map (fun x -> (mk_pattern x, mk_pterm e)) pts)@acc
                        )) [] l in
      Pmatchwith (mk_pterm e, ll)
    | Equantifier _ -> raise (ModelError ("quantifiers are not allowed in programming block", loc))
    | Elabel (lbl, e) ->
      begin
        let p = mk_pterm e in
        match unloc p with
        | Pfor (i, a, body, _) -> Pfor (i, a, body, Some lbl)
        |  _ -> raise (ModelError ("labels are only allowed in for loop", loc))
      end)


let to_label_lterm x : label_lterm =
  let loc, (label, lterm) = deloc x in
  {
    label = label;
    term = mk_lterm lterm;
    loc = loc;
  }

let to_label_pterm x : label_pterm =
  let loc, (label, pterm) = deloc x in
  {
    label = label;
    term = mk_pterm pterm;
    loc = loc;
  }

(****************)


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

let mk_decl loc ((id, typ, dv) : (lident * type_t option * expr option)) =
  let mk_bval e =
    let loc, v = deloc e in
    mkloc loc
      (match v with
       | Eliteral l -> to_bval l
       | Eterm (None, id) -> BVenum (unloc id)
       | _ -> raise (ModelError ("mk_bval: wrong type for ", loc))) in
  {
    name = id;
    typ = map_option mk_ptyp typ;
    default = map_option mk_bval dv;
    loc = loc;
  }

let extract_args (args : ParseTree.args)  =
  List.fold_left (fun acc (i : lident_typ) ->
      let name, typ, _ = i in
      mk_decl dummy (name, typ, None)::acc
    ) [] (args |> List.rev)

let mk_label_lterm loc (l, e) : lterm label_term =
  {
    label = l;
    term  = mk_lterm e;
    loc   = loc;
  }

let map_label_lterm l : label_lterm list =
  List.map (fun x ->
      let loc, (l, e) = deloc x in
      mk_label_lterm loc (l, e)
    ) l




























(* model *)

(* name *)
let get_name_archetype decls : lident =
  let res = List.fold_left (fun acc i -> (
        match unloc i with
        | Darchetype _ -> (match acc with
            | None -> Some i
            | Some x -> raise (ModelError2 ("only one name can be set to archetype.", loc x, loc i)))
        | _ -> acc)) None decls
  in
  match res with
  | Some ({pldesc = Darchetype (id, _exts); plloc = _l }) -> id
  | _ -> raise (ModelError0 ("no name for archetype found."))

(* extraction of parse tree declarations *)
let extract_decls decls model =

  let mk_variable loc (id, typ, dv, opts, cst) =
    let mk_decl_pterm loc ((id, typ, dv) : (lident * type_t option * expr option)) =
      {
        name = id;
        typ = map_option mk_ptyp typ;
        default = map_option mk_pterm dv;
        loc = loc;
      } in
    let ret_from_to opts =
      match opts with
      | Some o ->
        (List.fold_left (fun (a, b) i ->
             match i with
             | VOfrom q -> (Some (mk_qualid q), b)
             | VOto q -> (a, Some (mk_qualid q))) (None, None) o)
      | _ -> (None, None) in
    let (from, to_) = ret_from_to opts in {
      decl = mk_decl_pterm loc (id, Some typ, dv);
      constant = cst;
      from = from;
      to_ = to_;
      loc = loc;
    } in

  let mk_asset loc (id, fields, opts, apo, _ops) =
    let get_asset_fields fields =
      (List.fold_right (fun i acc ->
           let loc, v = deloc i in
           match v with
           | Ffield (id, typ, dv, _) -> mk_decl loc (id, Some typ, dv)::acc
         ) fields []) in

    let extract_asset_opts opts m =
      List.fold_left (fun acc i ->
          match i with
          | AOasrole ->
            {
              acc with
              role = true;
            }
          | AOidentifiedby id ->
            {
              acc with
              key = id;
            }
          | AOsortedby id ->
            {
              acc with
              sort = id::acc.sort
            }) m opts in

    let extract_apo apos m =
      List.fold_left (fun acc i ->
          match i with
          | APOstates id ->
            {
              acc with
              state = Some id
            }
          | APOconstraints l ->
            {
              acc with
              specs = map_label_lterm l
            }
          | APOinit e ->
            {
              acc with
              init = Some (mk_pterm e)
            }
        ) m apos in

    {
      name  = id;
      args  = get_asset_fields fields;
      key   = dumloc "_id";
      sort  = [];
      state = None;
      role  = false;
      init  = None;
      specs = [];
      loc   = loc;
    }
    |> extract_asset_opts opts
    |> extract_apo apo
  in

  let mk_function loc f = {
    name = f.name;
    args = [];
    return = map_option mk_ptyp f.ret_t;
    body = mk_pterm f.body;
    side = false;
    loc = loc;
  } in

  let mk_action loc name args (props : action_properties) =
    let rec to_rexpr_calledby (e : ParseTree.expr) : rexpr =
      let loc, v = deloc e in
      match v with
      | Eterm (None, id)
      | Edot (_, id) -> Rqualid (Qident id)
      | Eapp ({pldesc = Eop (`Logical Or); _}, args) ->
        ( let lhs = to_rexpr_calledby (List.nth args 0) in
          let rhs = to_rexpr_calledby (List.nth args 1) in
          Ror (lhs, rhs))
      | _ -> raise (ModelError ("type error: called by", loc)) in

    {
      name = name;
      args = extract_args args;
      calledby  = map_option (fun (e, _) -> to_rexpr_calledby e) props.calledby;
      condition = map_option (fun (items, _) -> List.map (fun a -> to_label_pterm a) items) props.condition;
      transition = None;
      verification = None;
      effect = None;
      side = false;
      loc = loc;
    } in

  let mk_state loc (name, items) =
    let get_states_items items =
      let is_state_initial = function
        | None -> false
        | Some opts ->
          List.fold_left (fun acc i ->
              match i with
              | SOinitial -> true
              | _ -> acc
            ) false opts in
      let get_state_specifications (opts : state_option list) : Model.verification =
        let es = List.fold_left (fun acc i ->
            match i with
            | SOspecification xs -> (List.map to_label_lterm xs) @ acc
            | _ -> acc
          ) [] opts in
        {
          dummy_verif with
          specs = es;
        } in

      List.fold_left (fun acc i ->
          (let (name, opts) = i in
           {
             name = name;
             initial = is_state_initial opts;
             verification = map_option get_state_specifications opts;
             loc = Location.loc name;
           }::acc)) [] items in
    {
      name = (match name with | None -> dumloc "_global" | Some a -> a);
      items = get_states_items items;
      loc = loc;
    } in

  let mk_enum loc (name, list) = {
    name = name;
    vals = list;
    loc = loc;
  } in

  let mk_verification loc v =
    let mk_predicate loc (id, args, body) = {
      name = id;
      args = extract_args args;
      body = mk_lterm body;
      loc  = loc;
    } in
    let mk_definition loc (name, typ_, id, def) = {
      name = name;
      typ  = mk_ptyp typ_;
      id   = id;
      def  = mk_lterm def;
      loc  = loc;
    } in
	  List.fold_right (fun (x : verification_item) acc ->
        let loc, vitem = Location.deloc x in
        match vitem with
        | Vpredicate (i, args, body) ->
          {
            acc with
            predicates = (mk_predicate loc (i, args, body))::acc.predicates
          }
        | Vdefinition (name, typ_, id, def) ->
          {
            acc with
            definitions = (mk_definition loc (name, typ_, id, def))::acc.definitions
          }
        | Vaxiom (id, e) ->
          {
            acc with
            axioms = (mk_label_lterm loc (Some id, e))::acc.axioms
          }
        | Vtheorem (id, e) ->
          {
            acc with
            theorems = (mk_label_lterm loc (Some id, e))::acc.theorems
          }
        | Vvariable (id, typ, e) ->
          {
            acc with
            variables = (mk_variable loc (id, typ, e, None, false))::acc.variables
          }
        | Vinvariant (id, l) ->
          {
            acc with
            invariants = (id, map_label_lterm l)::acc.invariants
          }
        | Veffect e ->
          {
            acc with
            effect = Some (mk_pterm e)
          }
        | Vspecification l ->
          {
            acc with
            specs = (map_label_lterm l) @ acc.specs
          }
      ) (v |> unloc |> fst) { dummy_verif with loc = loc; }
  in

  List.fold_right ( fun i acc ->
      (let loc, decl_u = deloc i in
       match decl_u with
       | Dvariable (id, typ, dv, opts, cst, _) ->
         {
           acc with
           variables = (mk_variable loc (id, typ, dv, opts, cst))::acc.variables
         }
       | Denum (name, list, _) ->
         {
           acc with
           enums = (mk_enum loc (name, list))::acc.enums
         }
       | Dstates (name, Some items, _) ->
         {
           acc with
           states = (mk_state loc (name, items))::acc.states
         }
       | Dasset (id, fields, opts, apo, _ops, _) ->
         {
           acc with
           assets = (mk_asset loc (id, fields, opts, apo, _ops))::acc.assets
         }
       | Daction (name, args, props, action, _) ->
         {
           acc with
           transactions = {
             (mk_action loc name args props) with
             effect = Tools.map_option (fun x -> let a, _ = x in mk_pterm a) action
           }::acc.transactions
         }
       | Dtransition (name, args, _on, from, props, trs, _) ->
         {
           acc with
           transactions = {
             (mk_action loc name args props) with
             transition = Some (None, to_sexpr from, List.map (fun (to_, cond, action) -> (to_,
                                                                                           map_option mk_pterm cond,
                                                                                           map_option mk_pterm action)) trs)
           }::acc.transactions
         }
       | Dfunction f ->
         {
           acc with
           functions = (mk_function loc f)::acc.functions
         }
       | Dcontract _ -> acc
       | Dnamespace _ -> raise (ModelError ("namespace is not supported at this stage", loc))
       | Dverification v ->
         {
           acc with
           verifications = (mk_verification loc v)::acc.verifications
         }
       | _ -> acc)
    ) decls model



let sanity_check model : Model.model =
  let _check_dv model : Model.model =
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

let parseTree_to_model (pt : ParseTree.archetype) : Model.model =
  let decls = match unloc pt with
  | Marchetype decls -> decls
  | Mextension _ -> raise (ModelError ("extension cannot be translated into Model.model.", loc pt)) in

  {
    name          = get_name_archetype decls;
    variables     = [];
    assets        = [];
    functions     = [];
    transactions  = [];
    states        = [];
    enums         = [];
    verifications = [];
  }
  |> extract_decls decls
  |> mkloc (loc pt)
  |> sanity_check

let string_to_pterm (str : string) : Model.pterm =
  let lb = Lexing.from_string str in
  let pt = Parser.start_expr Lexer.token lb in
  mk_pterm pt
