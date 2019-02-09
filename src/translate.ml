open Location
open Model
open ParseTree

exception ModelError of string * Location.t

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
  let loc = loc e in
  let value = unloc e in
  match value with
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
         (mkloc loc {name = id; default = BatOption.map to_rexpr dv})::acc
       | _ -> acc)
    ) [] decls

let mk_pterm e =
  let loc = loc e in
  let v = unloc e in
  mkloc loc
    (match v with
     | Ebreak -> Pbreak
     | _ -> Pbreak)

let mk_bval e =
  let loc = loc e in
  let v = unloc e in
  mkloc loc
    (match v with
     | Eliteral l -> (
         match l with
         | Lnumber b -> BVint b
         | Lfloat s -> BVfloat s
         | Laddress s -> BVaddress s
         | Lstring s -> BVstring s
         | Lbool b -> BVbool b
         | Lduration s -> BVduration s
         | Ldate s -> BVdate s)
      | _ -> raise (ModelError ("mk_bval: wrong type for ", loc)))


let lident_to_ptyp id = mkloc (loc id) (Tasset id) (* FIXME: complete with other type *)

let mk_decl loc ((id, typ, dv) : (lident * lident * expr option)) =
  mkloc loc {
    name = id;
    typ = Some (lident_to_ptyp typ);
    default = BatOption.map mk_bval dv;
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


let parseTree_to_model (pt : ParseTree.model) : Model.model =
  let ptu = Location.unloc pt in
  let decls = match ptu with
  | Mmodel decls -> decls
  | _ -> [] in

  mkloc (loc pt) {
    name          = get_name_model pt;
    roles         = get_roles decls;
    variables     = get_variables decls;
    assets        = [];
    functions     = [];
    transactions  = [];
    stmachines    = [];
    enums         = [];
    spec          = None;
  }
