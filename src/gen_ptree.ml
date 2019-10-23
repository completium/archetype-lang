open Why3

let config : Whyconf.config = Whyconf.read_config None
let main : Whyconf.main = Whyconf.get_main config
let env : Env.env = Env.create_env (Whyconf.loadpath main)

module M = Mlwtree
module P = Ptree
module E = Expr

let unloc (x : 'a M.with_loc) = x.obj
let loc   (x : 'a M.with_loc) = (*x.loc*) Loc.dummy_position

let mk_ident_intern f g (x : 'a) =
  let str = f x in
  let l = g x in
  P.{ id_str = str; id_ats = []; id_loc = l }

let mk_ident_str (str : string) =
  mk_ident_intern (fun x -> x) (fun _ -> Loc.dummy_position) str

let mk_ident (s : M.loc_ident) =
  mk_ident_intern unloc loc s

let mk_qualid_intern f (l : 'a list) =
  let rec aux l =
    match l with
    | [] -> assert false
    | [x] -> P.Qident(f x)
    | x::r -> P.Qdot(aux r,f x)
  in
  aux (List.rev l)

let mk_qualid_str (l : string list) = mk_qualid_intern mk_ident_str l

let mk_qualid l = mk_qualid_intern mk_ident l

let use_import l =
  let qid_id_opt = (mk_qualid l, None) in
  P.Duseimport(Loc.dummy_position,false,[qid_id_opt])

let mk_expr e = P.{ expr_desc = e; expr_loc = Loc.dummy_position }

let mk_term t = P.{ term_desc = t; term_loc = Loc.dummy_position }

let mk_pat p = P.{ pat_desc = p; pat_loc = Loc.dummy_position }
let pat_var id = mk_pat (P.Pvar id)

let mk_var id = mk_term (P.Tident (Qident id))

let mk_type ?(td_loc=Loc.dummy_position) ?(td_params=[]) ?(td_vis=P.Public) ?(td_mut=false) ?(td_inv=[]) ?(td_wit=[]) td_ident td_def : P.type_decl =
  { td_loc; td_ident; td_params; td_vis; td_mut; td_inv; td_wit; td_def }

let param0 = [Loc.dummy_position, None, false, Some (P.PTtuple [])]
let param1 id ty = [Loc.dummy_position, Some id, false, Some ty]

let mk_const i =
  Number.(ConstInt { il_kind = ILitDec; il_int = BigInt.of_int i })

let mk_tconst i = mk_term (Tconst (mk_const i))

let mk_econst i = mk_expr (Econst (mk_const i))

let mk_tapp f l = mk_term (Tidapp(f,l))

let mk_eapp f l = mk_expr (Eidapp(f,l))

let mk_evar x = mk_expr(Eident(Qident x))

let mk_pattern ?(pat_loc=Loc.dummy_position) pat_desc : P.pattern =
  { pat_desc; pat_loc }

let mk_spec ?(sp_pre=[]) ?(sp_post=[]) ?(sp_xpost=[]) ?(sp_reads=[]) ?(sp_writes=[]) ?(sp_alias=[]) ?(sp_variant=[]) ?(sp_checkrw=false) ?(sp_diverge=false) ?(sp_partial=false) () : P.spec =
  { sp_pre; sp_post; sp_xpost; sp_reads; sp_writes; sp_alias; sp_variant; sp_checkrw; sp_diverge; sp_partial }

let mk_field ?(f_loc=Loc.dummy_position) ?(f_mutable=false) ?(f_ghost=false) f_ident f_pty : P.field =
  { f_loc; f_ident; f_pty; f_mutable; f_ghost}

let mk_binder ?(loc=Loc.dummy_position) ?ident ?(ghost=false) ?pty () : P.binder =
  (loc, ident, ghost, pty)

let to_type t =
  match unloc t with
  | M.Tyint          -> P.PTtyapp(mk_qualid_str ["int"], [])
  | M.Tyuint         -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tybool         -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tystring       -> P.PTtyapp(mk_qualid_str ["string"], [])
  | M.Tyrational     -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tyaddr         -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tyrole         -> P.PTtyapp(mk_qualid_str ["role"], [])
  | M.Tykey          -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tydate         -> P.PTtyapp(mk_qualid_str ["date"], [])
  | M.Tyduration     -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tytez          -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tystorage      -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tytransfers    -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tyunit         -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tycontract id  -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tyrecord id    -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tycoll id      -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tymap id       -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tyasset id     -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Typartition id -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tyenum id      -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tyoption t     -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tylist t       -> P.PTtyapp(mk_qualid_str [""], [])
  | M.Tytuple tl     -> P.PTtyapp(mk_qualid_str [""], [])

let to_term t =
  match unloc t with
  | _ -> P.Ttrue (* TODO *)

let to_expr e =
  match unloc e with
  | _ -> P.Etrue (* TODO *)

let to_field (x : (M.loc_term, M.loc_typ, M.loc_ident) M.abstract_field) : P.field =
  let ident : P.ident = mk_ident x.name in
  let t : P.pty = to_type x.typ in
  mk_field ident t

let extract_fun_args s =
  match s.args with
  | [] -> mk_binder () ~pty:(P.PTtuple[])
  | _ -> mk_binder () ~pty:(P.PTtuple[])


let to_ptree (mlwtree : M.loc_mlw_tree) : P.mlw_file =
  let to_module (m : (M.loc_term, M.loc_typ, M.loc_ident) M.abstract_module ) : P.ident * P.decl list =
    let id = m.name in
    let decls = List.fold_right (
        fun (x : (M.loc_term, M.loc_typ, M.loc_ident) M.abstract_decl) accu ->
          match x with
          | Duse ids -> (use_import ids)::accu
          | Dval (i, t) -> (
              let ident = mk_ident i in
              let ghost = false in
              let rs_kind = E.RKnone in
              let pattern : P.pattern = mk_pattern P.Pwild in
              let spec : P.spec = mk_spec () in
              let expr_desc = P.Eany([], rs_kind, Some (P.PTtyapp(mk_qualid [i], [])), pattern, Ity.MaskVisible, spec) in
              let expr = mk_expr expr_desc in
              P.Dlet(ident, ghost, rs_kind, expr)
            )::accu
          | Dclone (i, j, k) -> (
              let a = List.map (function
                  | M.Ctype (a, b) -> P.CStsym(mk_qualid [a], [], PTtyapp(mk_qualid [b], []))
                  | M.Cval  (a, b) -> P.CSvsym(mk_qualid [a], mk_qualid [b])
                  | M.Cfun  (a, b) -> P.CSfsym(mk_qualid [a], mk_qualid [b])
                  | M.Cpred (a, b) -> P.CSpsym(mk_qualid [a], mk_qualid [b])
                ) k in
              P.Dcloneimport (Loc.dummy_position, false, mk_qualid i, Some (mk_ident j), a)
            )::accu
          | Denum (id, values) -> (
              let vs = List.map (fun x -> (Loc.dummy_position, mk_ident x, [])) values in
              let type_def = (P.TDalgebraic vs) in
              P.Dtype [mk_type (mk_ident id) type_def ]
            )::accu
          | Drecord (i, l) -> (
              let fields : P.field list =
                List.map to_field l in
              P.Dtype [mk_type (mk_ident i) (P.TDrecord fields)]
            )::accu
          | Dstorage s -> (
              let fields = List.map to_field s.fields in
              let type_def = (P.TDrecord fields) in
              let wit : (P.qualid * P.expr) list =
                List.map (fun (x : (M.loc_term, M.loc_typ, M.loc_ident) M.abstract_field) -> (
                      let q = mk_qualid [x.name] in
                      let e = mk_expr (to_expr x.init) in
                      (q, e)))
                  s.fields in
              let invs : P.term list =
                List.map (fun (x : (M.loc_term, M.loc_ident) M.abstract_formula) ->
                    mk_term (to_term x.form))
                  s.invariants in
              P.Dtype [mk_type ~td_inv:invs ~td_wit:wit (mk_ident_str "_storage") type_def ]
            )::accu
          | Dtheorem (t, i, e) -> (
              let k =
                match t with
                | Axiom -> Decl.Paxiom
                | Lemma -> Decl.Plemma
                | Goal  -> Decl.Pgoal
              in
              let id = mk_ident i in
              let t : P.term = mk_term (to_term e) in
              P.Dprop(k, id, t)
            )::accu
          | Dfun s -> (
              let ident = mk_ident s.name in
              let ghost = false in
              let rs_kind = E.RKfunc in
              let binders : P.binder list = extract_fun_args s in
              let expr : P.expr = mk_expr (to_expr s.body) in
              let pattern : P.pattern = mk_pattern P.Pwild in
              let spec : P.spec = mk_spec () in
              let expr = mk_expr (P.Efun(binders, None, pattern, Ity.MaskVisible, spec, expr)) in
              P.Dlet(ident, ghost, rs_kind, expr)
            )::accu
      ) m.decls [] in
    (mk_ident id, decls)
  in
  Modules (List.map to_module mlwtree)
