open Model
open Modelws
open Location

open Why3
open Ptree

let loc (a : 'a Location.loced) : Loc.position =
  let l      = Location.loc a in
  let fname  = l.loc_fname in
  let line   = (fst l.loc_start) in
  let startp = (snd l.loc_start) in
  let endp   = (snd l.loc_end)   in
  Loc.user_position fname line startp endp

let mk_ident s       = { id_str = unloc s; id_ats = []; id_loc = loc s }

let mk_qid l =
  let rec aux l =
    match l with
      | [] -> assert false
      | [x] -> Qident(mk_ident x)
      | x::r -> Qdot(aux r,mk_ident x)
  in
  aux (List.rev l)

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

let lident_to_typ s = PTtyapp (mk_qid [s],[])

let vtyp_to_str = function
    | VTint                -> "int"
    | VTuint               -> "uint"
    | VTdate               -> "timestamp"
    | VTduration           -> "uint";
    | VTstring             -> "string";
    | VTaddress            -> "address";
    | VTcurrency (Tez,_)   -> "tez"
    | VTcurrency (Mutez,_) -> "tez"

let str_to_lident = mkloc Location.dummy

let str_to_ident s = { id_str = s; id_ats = []; id_loc = Loc.dummy_position }

let vtyp_to_ident vt = vt |> vtyp_to_str |> str_to_ident

let vtyp_to_mlwtyp vt = vt |> vtyp_to_str |> str_to_lident |> lident_to_typ

let mk_ptyp = function
  | Enum     lid        -> lident_to_typ lid
  | Var      vt         -> vt |> vtyp_to_mlwtyp
  | KeySet   (_,vt)     ->
    let listid =  str_to_ident "list" in
    PTtyapp (Qident listid, [vtyp_to_mlwtyp vt])
  | ValueMap (_, vtf, vtt) ->
    let mapid  = str_to_ident "map" in
    PTtyapp (Qident mapid, [vtyp_to_mlwtyp vtf; vtyp_to_mlwtyp vtt])
  | CollMap  (_,vtf,_,vtt) ->
    let listid = str_to_ident "list" in
    let mapid  = str_to_ident "map"  in
    PTtyapp (Qident mapid, [vtyp_to_mlwtyp vtf;
                            PTtyapp (Qident listid, [vtyp_to_mlwtyp vtt])])

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
let modelws_to_modelw3liq (m : model_with_storage) =
  []
  |> fun x -> List.fold_left (fun acc e -> acc @ [mk_enum_decl e]) x m.enums
  |> fun x -> x @ [mk_storage_decl m.storage]
