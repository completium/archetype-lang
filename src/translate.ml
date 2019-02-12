open Location
open Model
open ParseTree

exception ModelError of string * Location.t

let map_option f = function
  | Some x -> Some (f x)
  | None -> None

let builtin_type str =
  match str with
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

let rec mk_ptyp e =
  let loc, v = deloc e in
  mkloc loc
    (match v with
     | Tref v -> (let b = builtin_type (unloc v) in
                  match b with
                  | Some u -> Tbuiltin u
                  | None -> Tasset v)
     | Tcontainer (t, container) -> Tcontainer ((mk_ptyp t), container_to_container container)
     | Tapp (f, v) -> Tapp (mk_ptyp f, mk_ptyp v)
     | Ttuple l -> Ttuple (List.map mk_ptyp l)
     | _ -> raise (ModelError ("mk_ptyp: unsupported type ", loc)))
(*   | Tvset (vs, t) -> *)

let to_bval l =
  match l with
  | Lnumber b -> BVint b
  | Lfloat s -> BVfloat s
  | Laddress s -> BVaddress s
  | Lstring s -> BVstring s
  | Lbool b -> BVbool b
  | Lduration s -> BVduration s
  | Ldate s -> BVdate s

let mk_lexpr e =
  let loc, v = deloc e in
  mkloc loc (match v with
      | Eliteral l -> Llit (mkloc loc (to_bval l))
      | _ -> Llit (mkloc loc (BVstring "TODO"))
    )

let rec mk_pexpr e =
  let loc, v = deloc e in
  mkloc loc (
    match v with
    | Eterm t -> (match t with
        | (Some a, e) -> Pdot (mkloc (Location.loc a) (Passet a), mkloc (Location.loc e) (Pfield e))
        | (None, e) -> (
            let c = to_const (unloc e) in
            match c with
            | Some d -> Pconst d
            | None -> Pvar e))

    | Eop _op -> Plit (mkloc loc (BVstring "TODO"))
    | Eliteral l -> Plit (mkloc loc (to_bval l))
    | Earray _l -> Plit (mkloc loc (BVstring "TODO"))
    | Edot (_e, _i) -> Plit (mkloc loc (BVstring "TODO"))
    | EassignFields _l -> Plit (mkloc loc (BVstring "TODO"))
    | Eapp (_f, _args) -> Plit (mkloc loc (BVstring "TODO"))
    | Etransfer (_e, _b, _a) -> Plit (mkloc loc (BVstring "TODO"))
    | Eassign (op, lhs, rhs) -> Passign (to_assignment_operator op, mk_pexpr lhs, mk_pexpr rhs)
    | Eif (cond, then_, else_) -> Pif (mk_pexpr cond, mk_pexpr then_, map_option mk_pexpr else_)
    | Ebreak -> Pbreak
    | Efor (i, e, body, _name) -> Pfor (i, mk_pexpr e, mk_pexpr body)
    | Eassert e -> Passert (mk_lexpr e)
    | Eseq (lhs, rhs) ->
      (let l = (let a = mk_pexpr lhs in match a with | {pldesc = Pseq la; _} -> la | _ -> [a]) @
               (let a = mk_pexpr rhs in match a with | {pldesc = Pseq la; _} -> la | _ -> [a]) in
       Pseq l)
    | Efun (_args, _body) -> Plit (mkloc loc (BVstring "TODO"))
    | Eletin ((i, typ, _), init, body) -> Pletin (i, mk_pexpr init, map_option mk_ptyp typ, mk_pexpr body)
    | Equantifier _ -> raise (ModelError ("mk_pexpr: quantifier is not allowed in pblock", loc)))

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

let to_rexpr e =
  let loc, v = deloc e in
  match v with
  | Eliteral l -> (
      match l with
      | Laddress a -> Raddress a
      | _ -> raise (ModelError ("only address is supported", loc)) )
  | Eterm (None, id) -> Rrole id
  | Eterm (Some a, id) -> Rasset (a, id)
  (*  | Eapp ({pldesc = {pldesc = Eop}, args, _}) ->*)
  | _ -> raise (ModelError ("wrong type for ", loc))

let get_roles decls =
  List.fold_left ( fun acc i ->
      (let loc = loc i in
       let decl_u = Location.unloc i in
       match decl_u with
       | Drole (id, dv, _) ->
         (mkloc loc {name = id; default = map_option to_rexpr dv})::acc
       | _ -> acc)
    ) [] decls

let mk_pterm e = (* TODO *)
  let loc = loc e in
  let v = unloc e in
  mkloc loc
    (match v with
     | Ebreak -> Pbreak
     | _ -> Pvar (dumloc "TODO") )

let mk_bval e =
  let loc, v = deloc e in
  mkloc loc
    (match v with
     | Eliteral l -> to_bval l
     | _ -> raise (ModelError ("mk_bval: wrong type for ", loc)))

let mk_decl loc ((id, typ, dv) : (lident * type_t * expr option)) =
  mkloc loc {
    name = id;
    typ = Some (mk_ptyp typ);
    default = map_option mk_bval dv;
  }

let get_variables decls =
  List.fold_left ( fun acc i ->
      (let loc = loc i in
       let decl_u = Location.unloc i in
       match decl_u with
       | Dconstant (id, typ, dv, _) ->
         mkloc loc {decl = mk_decl loc (id, typ, dv); constant = true }::acc
       | Dvariable (id, typ, _, dv, _) ->
         mkloc loc {decl = mk_decl loc (id, typ, dv); constant = false }::acc
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
          | Ffield (id, typ, dv, _) -> mk_decl loc (id, typ, dv)::acc
        ) [] fields)

let mk_asset loc ((id, fields, _cs, opts, init, _ops) : (lident * field list option * expr list option * asset_option list option * expr option * asset_operation option)) =
  mkloc loc {
    name = id;
    args = get_asset_fields fields;
    key = get_asset_key opts;
    sort = None;
    role = is_asset_role opts;
    init = map_option mk_pterm init;
    preds = None;
  }

let get_assets decls =
  List.fold_left ( fun acc i ->
      (let loc = loc i in
       let decl_u = Location.unloc i in
       match decl_u with
       | Dasset (id, fields, cs, opts, init, ops) -> mk_asset loc (id, fields, cs, opts, init, ops)::acc
       | _ -> acc)
    ) [] decls

let get_functions decls =
  List.fold_left ( fun acc i ->
      (let loc = loc i in
       let decl_u = Location.unloc i in
       match decl_u with
       | Dfunction (name, _args, typ, body) ->
         mkloc loc {name = name;
                    args = [];
                    return = map_option mk_ptyp typ;
                    body = mk_pexpr body }::acc
       | _ -> acc)
    ) [] decls

let get_enums decls =
  List.fold_left ( fun acc i ->
      (let loc = loc i in
       let decl_u = Location.unloc i in
       match decl_u with
       | Denum (name, list) ->
         mkloc loc {name = name;
                    vals = list }::acc
       | _ -> acc)
    ) [] decls

let get_states decls =
  List.fold_left ( fun acc i ->
      (let _loc = loc i in
       let decl_u = Location.unloc i in
       match decl_u with
(*       | Dstate (name, list) -> acc
         mkloc loc {name = name;
                    vals = list }::acc*)
       | _ -> acc)
    ) [] decls

let get_enums decls =
  List.fold_left ( fun acc i ->
      (let loc = loc i in
       let decl_u = Location.unloc i in
       match decl_u with
       | Denum (name, list) ->
         mkloc loc {name = name;
                    vals = list }::acc
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
    transactions  = [];
    stmachines    = [];
    states        = get_states decls;
    enums         = get_enums decls;
    spec          = None;
  }
