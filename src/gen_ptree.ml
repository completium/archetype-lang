open Why3

let config : Whyconf.config = Whyconf.read_config None
let main : Whyconf.main = Whyconf.get_main config
let env : Env.env = Env.create_env (Whyconf.loadpath main)

module M = Mlwtree
module P = Ptree

let mk_ident s = P.{ id_str = s; id_ats = []; id_loc = Loc.dummy_position }

let mk_qualid l =
  let rec aux l =
    match l with
    | [] -> assert false
    | [x] -> P.Qident(mk_ident x)
    | x::r -> P.Qdot(aux r,mk_ident x)
  in
  aux (List.rev l)

let use_import l =
  let qid_id_opt = (mk_qualid l, None) in
  P.Duseimport(Loc.dummy_position,false,[qid_id_opt])

let mk_expr e = P.{ expr_desc = e; expr_loc = Loc.dummy_position }

let mk_term t = P.{ term_desc = t; term_loc = Loc.dummy_position }

let mk_pat p = P.{ pat_desc = p; pat_loc = Loc.dummy_position }
let pat_var id = mk_pat (P.Pvar id)

let mk_var id = mk_term (P.Tident (Qident id))

let param0 = [Loc.dummy_position, None, false, Some (P.PTtuple [])]
let param1 id ty = [Loc.dummy_position, Some id, false, Some ty]

let mk_const i =
  Number.(ConstInt { il_kind = ILitDec; il_int = BigInt.of_int i })

let mk_tconst i = mk_term (Tconst (mk_const i))

let mk_econst i = mk_expr (Econst (mk_const i))

let mk_tapp f l = mk_term (Tidapp(f,l))

let mk_eapp f l = mk_expr (Eidapp(f,l))

let mk_evar x = mk_expr(Eident(Qident x))

let to_ptree (mlwtree : M.mlw_tree) : P.mlw_file =
  (* mlwtree.decls *)
  Modules [(mk_ident "mytest", [])]
