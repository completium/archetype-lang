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

let loc2loc (l : Location.t) : Loc.position =
  let fname  = l.loc_fname in
  let line   = (fst l.loc_start) in
  let startp = (snd l.loc_start) in
  let endp   = (snd l.loc_end)   in
  Loc.user_position fname line startp endp

let loc (a : 'a loced) : Loc.position =
  let l = Location.loc a in
  loc2loc l

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
  | VTuint               -> "nat"
  | VTdate               -> "timestamp"
  | VTduration           -> "nat";
  | VTstring             -> "string";
  | VTaddress            -> "address";
  | VTcurrency (Tez,_)   -> "tez"
  | VTcurrency (Mutez,_) -> "tez"

let str_to_lident = mkloc Location.dummy

let str_to_ident s = { id_str = s; id_ats = []; id_loc = Loc.dummy_position }

let vtyp_to_ident vt = vt |> vtyp_to_str |> str_to_ident

let vtyp_to_mlwtyp vt = vt |> vtyp_to_str |> str_to_lident |> lident_to_typ

type field_storage =
  | Fenum of lident
  | Ftyp  of vtyp
  | Flist of vtyp
  | Fset  of vtyp
  | Fmap  of vtyp * field_storage

type field_storage_policy = {
  ftyps   : (lident * field_storage) list;
}

let get_storage_typ fname p = List.assoc fname p.ftyps

(* This should be modulated/optimized according to asset usage in tx actions *)
let sf_to_fs = function
  | Enum id               -> Fenum id
  | Var  vt               -> Ftyp vt
  | KeySet (_,vt)         -> Flist vt
  | ValueMap (_,vtf,vtt)  -> Fmap (vtf,Ftyp vtt)
  | CollMap (_,vtf,_,vtt) -> Fmap (vtf, Flist vtt)

let rec field_storage_to_ptyp = function
  | Fenum lid -> lident_to_typ lid
  | Ftyp vt   -> vt |> vtyp_to_mlwtyp
  | Flist vt  ->
    let listid =  str_to_ident "list" in
    PTtyapp (Qident listid, [vtyp_to_mlwtyp vt])
  | Fset vt   ->
    let listid =  str_to_ident "set" in
    PTtyapp (Qident listid, [vtyp_to_mlwtyp vt])
  | Fmap (vt,fs) ->
    let mapid  = str_to_ident "map"  in
    PTtyapp (Qident mapid, [vtyp_to_mlwtyp vt; field_storage_to_ptyp fs])

let mk_field_typ   (f : storage_field) = (f.name, sf_to_fs f.typ)

let mk_field_storage_policy storage = {
  ftyps   = List.map mk_field_typ storage.fields;
}

let mk_storage_field (p : field_storage_policy) (f : storage_field) : field = {
  f_loc     = loc2loc (f.loc);
  f_ident   = mk_ident f.name;
  f_pty     = field_storage_to_ptyp (List.assoc f.name p.ftyps);
  f_mutable = false;
  f_ghost   = f.ghost;
}

let mk_storage_decl (p : field_storage_policy) (storage : storage) =
  Dtype [{
    td_loc = Loc.dummy_position;
    td_ident = { id_str = "storage"; id_ats = []; id_loc = Loc.dummy_position } ;
    td_params = [];
    td_vis = Public;
    td_mut = false;
    td_inv = [];
    td_wit = [];
    td_def = TDrecord (List.map (mk_storage_field p) storage.fields);
  }]

(* storage init generation *)
let mk_init_args p info fs : (lident * field_storage) list =
  List.fold_left (fun acc f ->
      match f.default with
      | None ->
        begin
          match  f.typ with
          | Enum id when (is_state info id) -> acc
          | KeySet _ -> acc
          | ValueMap _ -> acc
          | CollMap _ -> acc
          | _ -> acc @ [f.name, List.assoc f.name p.ftyps]
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
  | VTbool           -> BVbool false
  | VTint            -> BVint Big_int.zero_big_int
  | VTuint           -> BVint Big_int.zero_big_int
  | VTdate           -> BVdate "1970-01-01T00:00:00Z"
  | VTduration       -> BVduration "0s"
  | VTstring         -> BVstring ""
  | VTaddress        -> BVaddress "@none"
  | VTcurrency (c,_) -> BVcurrency (c,Big_int.zero_big_int)

let mk_init_fields info args fs : (lident * initval) list =
  List.fold_left (fun acc f ->
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

let bval_to_expr info = function
  | BVint      v ->
    mk_expr (str_to_eidapp ["toint"] [mk_econst (Big_int.string_of_big_int v)])
  | BVuint     v ->
    mk_expr (str_to_eidapp ["tonat"] [mk_econst (Big_int.string_of_big_int v)])
  | BVbool     v -> mk_expr (if v then Etrue else Efalse)
  | BVenum     v -> mk_expr (str_to_eident [v])
  | BVfloat    v -> raise (CannotConvert v)
  | BVdate     v -> mk_expr (str_to_eident [get_dummy_for info v])
  | BVstring   _ -> raise (StringUnsupported)
  | BVcurrency (_,v) ->
    mk_expr (str_to_eidapp ["totez"] [mk_econst (Big_int.string_of_big_int v)])
  | BVaddress  v ->
    mk_expr (str_to_eidapp ["toaddress"] [mk_expr (str_to_eident [get_dummy_for info v])])
  | BVduration v -> mk_econst v

let vtyp_to_acronym = function
  | VTbool       -> "bol"
  | VTint        -> "int"
  | VTuint       -> "nat"
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

let mk_init_body info (fields : (lident * initval) list) : expr =
  let fields = fields |> List.map (fun (id,init) ->
      mk_qid [id], match init with
        | Ienum id   -> mk_expr (Eident (mk_qid [id]))
        | Ival bv    -> bval_to_expr info (unloc bv)
        | Iemptyc ec -> empty_cont_to_expr ec
        | Iinput id  -> mk_evar id) in
  mk_expr (Erecord fields)

let mk_fun_decl id args body =
  let args = List.map (fun (id, st) ->
      let id = mk_ident id in
      let ty = field_storage_to_ptyp st in
      Loc.dummy_position, Some id, false, Some ty
    ) args in
  let args =
    if compare (List.length args) 0 = 0
    then [Loc.dummy_position, None, false, Some (PTtuple [])]
    else args in
  let f = Efun(args, None, Ity.MaskVisible, empty_spec, body) in
  Dlet(str_to_ident id, false, Expr.RKnone, mk_expr f)

let mk_init_storage p info (s : storage) =
  let args = mk_init_args p info s.fields in
  let fields = mk_init_fields info args s.fields in
  let body = mk_init_body info fields in
  mk_fun_decl "init" args body

let vt_to_mk = function
  | VTaddress -> "mkaddress"
  | VTstring  -> "mkstring"
  | VTdate    -> "mktimestamp"
  | _ -> raise (NotFound "vt_to_ml")

let mk_dummy_decl (name,(_,vt)) =
  let body = mk_expr (str_to_eidapp [vt_to_mk vt] [mk_expr (Etuple [])]) in
  let f = Efun ([], None, Ity.MaskVisible, empty_spec, body) in
  Dlet(str_to_ident name, false, Expr.RKnone, mk_expr f)

(* generate functions *)
let field_type_to_mlwtyp (t:storage_field_type) = t |> sf_to_fs |> field_storage_to_ptyp

let is_var (p : Modelws.pterm) =
  match unloc p with
  | Pvar _ -> true
  | _ -> false

let dest_var (p : Modelws.pterm) : lident =
  match unloc p with
  | Pvar id -> id
  | _ -> raise (Anomaly "dest_var")

let is_lambda (p : Modelws.pterm) =
  match unloc p with
  | Plambda _ -> true
  | _ -> false

let dest_lambda (p : Modelws.pterm) =
  match unloc p with
  | Plambda (id,t,b) -> (id,t,b)
  | _ -> raise (Anomaly "dest_var")

let rec pterm_to_expr (p : Modelws.pterm) =  {
  expr_desc =
    begin
      match unloc p with
      | Pvar id -> Eident (mk_qid [id])
      | Papp (f, l) when is_var f ->
        let fid =  dest_var f in
        Eidapp (mk_qid [fid], List.map pterm_to_expr l)
      | Plambda (i,t,b) -> mk_efun [] (loc p) i t b
      | _ -> raise (Anomaly "pterm_to_expr")
    end;
  expr_loc = loc p
}
and mk_efun args l i t b =
  let t = Translate.map_option field_type_to_mlwtyp t in
  let args = args @ [l, Some (mk_ident i), false, t] in
  if is_lambda b
  then
    let (i,t,b) = dest_lambda b in
    mk_efun args (loc b) i t b
  else Efun (args, None, Ity.MaskVisible, empty_spec, pterm_to_expr b)

let rec mk_lambda (args : storage_field_type gen_decl list) body : Modelws.pterm =
  match args with
  | [a]   -> mkloc a.loc (Plambda (a.name, a.typ, body))
  | a::tl -> mkloc a.loc (Plambda (a.name, a.typ, mk_lambda tl body))
  | []    -> body

let mk_function_decl (f : function_ws) =
  let body = mk_lambda f.args f.body in
  Dlet (mk_ident f.name, false, Expr.RKnone, pterm_to_expr body)

(* returns a list of definition *)
let modelws_to_modelw3liq (info : info) (m : model_with_storage) =
  Liq_printer.set_entries
    { Liq_printer.empty_entries with
      init = Some "init";
      dummies = List.map (fun (n,(v,_)) -> (n,v)) info.dummy_vars;
    };
  let storage_policy = mk_field_storage_policy m.storage in
  []
  |> (fun x -> List.fold_left (fun acc d -> acc @ [mk_dummy_decl d]) x info.dummy_vars)
  |> (fun x -> List.fold_left (fun acc e -> acc @ [mk_enum_decl e]) x m.enums)
  |> (fun x -> x @ [mk_storage_decl storage_policy m.storage])
  |> (fun x -> x @ [mk_init_storage storage_policy info m.storage])
  |> (fun x -> List.fold_left (fun acc f -> acc @ [mk_function_decl f]) x m.functions)
