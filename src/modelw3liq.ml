open Model
open Modelinfo
open Modelws
open Location

open Why3
open Ptree

let empty_spec = {
    sp_pre = [];
    sp_post = [];
    sp_xpost = [];
    sp_reads = [];
    sp_writes = [];
    sp_alias = [];
    sp_variant = [];
    sp_checkrw = false;
    sp_diverge = false;
    sp_partial = false;
  }

let loc (a : 'a Location.loced) : Loc.position =
  let l      = Location.loc a in
  let fname  = l.loc_fname in
  let line   = (fst l.loc_start) in
  let startp = (snd l.loc_start) in
  let endp   = (snd l.loc_end)   in
  Loc.user_position fname line startp endp

let mk_expr e = { expr_desc = e; expr_loc = Loc.dummy_position }

let mk_ident s = { id_str = unloc s; id_ats = []; id_loc = loc s }

let mk_qid l =
  let rec aux l =
    match l with
      | [] -> assert false
      | [x] -> Qident(mk_ident x)
      | x::r -> Qdot(aux r,mk_ident x)
  in
  aux (List.rev l)

let mk_econst s =
  mk_expr
    (Econst
       Number.(ConstInt { ic_negative = false ;
                          ic_abs = int_literal_dec s }))

let str_to_eident (s : string list) = s
  |> List.map (fun x -> mkloc Location.dummy x)
  |> fun x -> Eident (mk_qid x)

let str_to_eidapp (s : string list) l = s
  |> List.map (fun x -> mkloc Location.dummy x)
  |> fun x -> Eidapp ((mk_qid x),l)

let mk_enum_vals = List.map (fun id -> loc id, mk_ident id, [])

let mk_evar x = mk_expr(Eident(mk_qid [x]))

let mk_enum_decl (enum : Modelws.enum) =
  Dtype [{
    td_loc = loc enum;
    td_ident = mk_ident (unloc enum).name;
    td_params = [];
    td_vis = Public;
    td_mut = false;
    td_inv = [];
    td_wit = [];
    td_def = TDalgebraic (mk_enum_vals (unloc enum).values);
  }]

let lident_to_typ s = PTtyapp (mk_qid [s],[])

let vtyp_to_str = function
  | VTbool               -> "bool"
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

(* storage init generation *)
let mk_init_args info fs : (lident * storage_field_type) list =
  List.fold_left (fun acc f ->
      let f = unloc f in
      match f.default with
      | None ->
        begin
          match  f.typ with
          | Enum id when (is_state info id) -> acc
          | KeySet _ -> acc
          | ValueMap _ -> acc
          | CollMap _ -> acc
          | _ as t -> acc @ [f.name, t]
        end
      | _ -> acc
    ) [] fs

type empty_container =
  | Emptylist     of vtyp
  | EmptyMap      of vtyp * vtyp
  | EmptySet      of vtyp
  | EmptyCollMap  of vtyp * vtyp

type initval =
  | Ienum   of lident
  | Ival    of bval
  | Iemptyc of empty_container
  | Iinput  of lident

let mk_init_val = function
  | VTbool       -> BVbool false
  | VTint        -> BVint Big_int.zero_big_int
  | VTuint       -> BVint Big_int.zero_big_int
  | VTdate       -> BVdate "1970-01-01T00:00:00Z"
  | VTduration   -> BVduration "0s"
  | VTstring     -> BVstring ""
  | VTaddress    -> BVaddress "@none"
  | VTcurrency _ -> BVcurrency "0"

let mk_init_fields info args fs : (lident * initval) list =
  List.fold_left (fun acc f ->
      let f = unloc f in
      match f.default with
      | Some bv -> acc @ [f.name, Ival bv]
      | None ->
         let init =
           if List.mem_assoc f.name args
           then Iinput f.name
           else (* not an input, no default value : depends on type *)
             match f.typ with
             | Enum id               -> Ienum (get_initial_state info id)
             | Var vt                -> Ival (mkloc Location.dummy (mk_init_val vt))
             | KeySet (_,vt)         -> Iemptyc (Emptylist vt)
             | ValueMap (_,vtf,vtt)  -> Iemptyc (EmptyMap (vtf,vtt))
             | CollMap (_,vtf,_,vtt) -> Iemptyc (EmptyCollMap (vtf,vtt))
         in
         acc @ [f.name, init]
    ) [] fs

let bval_to_expr = function
  | BVint      v -> mk_econst (Big_int.string_of_big_int v)
  | BVuint     v -> mk_econst (Big_int.string_of_big_int v)
  | BVbool     v -> mk_expr (if v then Etrue else Efalse)
  | BVenum     v -> mk_expr (str_to_eident [v])
  | BVfloat    v -> raise (CannotConvert v)
  | BVdate     v -> raise (CannotConvert v)
  | BVstring   _ -> raise (StringUnsupported)
  | BVcurrency v -> mk_econst v
  | BVaddress  v -> mk_econst v
  | BVduration v -> mk_econst v

let vtyp_to_acronym = function
  | VTbool       -> "bol"
  | VTint        -> "int"
  | VTuint       -> "unt"
  | VTdate       -> "tim"
  | VTduration   -> "dur"
  | VTstring     -> "str"
  | VTaddress    -> "add"
  | VTcurrency _ -> "tez"

let empty_cont_to_expr = function
  | Emptylist    _         -> mk_expr (str_to_eident ["Nil"])
  | EmptyMap     (vtf,vtt) ->
    let suffix = (vtyp_to_acronym vtf)^"_"^(vtyp_to_acronym vtt) in
    mk_expr (str_to_eidapp ["empty_map_"^suffix] [mk_expr (Etuple [])])
  | EmptySet     vt        ->
    let suffix = vtyp_to_acronym vt in
    mk_expr (str_to_eidapp ["empty_set_"^suffix] [mk_expr (Etuple [])])
  | EmptyCollMap (vtf,vtt) ->
    let suffix = (vtyp_to_acronym vtf)^"_"^(vtyp_to_acronym vtt) in
    mk_expr (str_to_eidapp ["empty_map_"^suffix^"l"] [mk_expr (Etuple [])])

let mk_init_body (fields : (lident * initval) list) : expr =
  let fields = fields |> List.map (fun (id,init) ->
      mk_qid [id], match init with
        | Ienum id   -> mk_expr (Eident (mk_qid [id]))
        | Ival bv    -> bval_to_expr (unloc bv)
        | Iemptyc ec -> empty_cont_to_expr ec
        | Iinput id  -> mk_evar id) in
  mk_expr (Erecord fields)

let mk_fun_decl id args body =
  let args = List.map (fun (id, sty) ->
      let id = mk_ident id in
      let ty = mk_ptyp sty in
      Loc.dummy_position, Some id, false, Some ty
    ) args in
  let f = Efun(args, None, Ity.MaskVisible, empty_spec, body) in
  Dlet(str_to_ident id, false, Expr.RKnone, mk_expr f)

let mk_init_storage info (s : storage) =
  let args = mk_init_args info s.fields in
  let fields = mk_init_fields info args s.fields in
  let body = mk_init_body fields in
  mk_fun_decl "init" args body

(* returns a list of definition *)
let modelws_to_modelw3liq (info : info) (m : model_with_storage) =
  Liq_printer.set_entries { Liq_printer.empty_entries with init = Some "init" };
  []
  |> fun x -> List.fold_left (fun acc e -> acc @ [mk_enum_decl e]) x m.enums
  |> fun x -> x @ [mk_storage_decl m.storage]
  |> fun x -> x @ [mk_init_storage info m.storage]
