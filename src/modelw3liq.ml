open Model
open Modelws
open Location

open Why3
open Ptree

let loc (a : 'a loced) : Loc.position =
  let l      = loc a in
  let fname  = l.loc_fname in
  let line   = (fst l.loc_start) in
  let startp = (snd l.loc_start) in
  let endp   = (snd l.loc_end)   in
  (fname, line, startp, endp)

let mk_ident s       = { id_str = unloc s; id_ats = []; id_loc = loc s }

let str_to_ident s l = { id_str = s; id_ats = []; id_loc = l }

(*let mk_expr e l = { expr_desc = unloc e; expr_loc = loc l }

let mk_term t l = { term_desc = unloc t; term_loc = loc l }

let mk_pat p l = { pat_desc = p; pat_loc = loc l }

let pat_var id = mk_pat (Pvar id)

let mk_var (id : lident) = mk_term (Tident (Qident (unloc id))) (loc id)

let param0 = [Loc.dummy_position, None, false, Some (PTtuple [])]

let param1 id ty = [Loc.dummy_position, Some id, false, Some ty]

let mk_tconst s =
  mk_term
    (Tconst
       Number.(ConstInt { ic_negative = false ;
                          ic_abs = int_literal_dec s }))

let mk_econst s =
  mk_expr
    (Econst
       Number.(ConstInt { ic_negative = false ;
                          ic_abs = int_literal_dec s }))

let mk_tapp f l = mk_term (Tidapp(f,l))

let mk_eapp f l = mk_expr (Eidapp(f,l))

let mk_evar x = mk_expr(Eident(Qident x))

*)

let mk_enum_vals = List.map (fun id -> loc id, mk_ident id, [])

let mk_enum_decl (enum : Modelws.enum) =
  Dtype [{
    td_loc = loc enum;
    td_ident = mk_ident (unloc enum).name;
    td_params = [];
    td_vis = Public;
    td_mut = true;
    td_inv = [];
    td_wit = [];
    td_def = TDalgebraic (mk_enum_vals (unloc enum).values);
  }]

let vtyp_to_ident vt =
  let s =
    match vt with
    | VTint                -> "int"
    | VTuint               -> "uint"
    | VTdate               -> "timestamp"
    | VTduration           -> "uint";
    | VTstring             -> "string";
    | VTaddress            -> "address";
    | VTcurrency (Tez,_)   -> "tez"
    | VTcurrency (Mutez,_) -> "tez" in
  str_to_ident s Loc.dummy_position

let rec mk_ptyp = function
  | Enum     lid        -> PTtyvar (mk_ident lid)
  | Var      vt         -> PTtyvar (vtyp_to_ident vt)
  | KeySet   (_,vt)     ->
    let listid =  str_to_ident "list" Loc.dummy_position in
    PTtyapp (Qident listid, [PTtyvar (vtyp_to_ident vt)])
  | ValueMap (_, vtf, vtt) ->
    let mapid  = str_to_ident "map" Loc.dummy_position in
    PTtyapp (Qident mapid, [PTtyvar (vtyp_to_ident vtf); PTtyvar (vtyp_to_ident vtt)])
  | CollMap  (_,vtf,_,vtt) ->
    let listid = str_to_ident "list" Loc.dummy_position in
    let mapid  = str_to_ident "map"  Loc.dummy_position in
    PTtyapp (Qident mapid, [PTtyvar (vtyp_to_ident vtf);
                            PTtyapp (Qident listid, [PTtyvar (vtyp_to_ident vtt)])])

let mk_storage_field (f : storage_field) : field = {
  f_loc     = loc f;
  f_ident   = mk_ident (unloc f).name;
  f_pty     = mk_ptyp (unloc f).typ;
  f_mutable = true;
  f_ghost   = (unloc f).ghost;
}

let mk_storage_decl (storage : storage) =
  Dtype [{
    td_loc = Loc.dummy_position;
    td_ident = { id_str = "storage"; id_ats = []; id_loc = Loc.dummy_position } ;
    td_params = [];
    td_vis = Public;
    td_mut = true;
    td_inv = [];
    td_wit = [];
    td_def = TDrecord (List.map mk_storage_field storage.fields);
  }]

(* returns a list of definition *)
let modelws_to_modelmlwliq (m : model_with_storage) =
  []
  |> fun x -> List.fold_left (fun acc e -> acc @ [mk_enum_decl e]) x m.enums
  |> fun x -> x @ [mk_storage_decl m.storage]
