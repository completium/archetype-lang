(* opening the Why3 library *)
open Why3
open Ptree
open Format
open Model
open Miles

(* adapt according to system ? *)
let cmlw_loadpath = "/home/dev/cml-lang/models/why3tests/"

(* helper function: [use th1 th2] insert the equivalent of a
   "use import th2" in theory th1 under construction *)
let use th1 th2 =
  let name = th2.Theory.th_name in
  Theory.close_scope
    (Theory.use_export (Theory.open_scope th1 name.Ident.id_string) th2)
    ~import:true

(* TODO : use of Ref FsetSum Array should be decided based on transactions' action *)
let add_use env theory =
  let theolibs = [ ["int"],   "Int";
                   ["ref"],   "Ref";
                   ["set"],   "Fset";
                   ["set"],   "FsetSum";
                   ["array"], "Array";
                   ["cml"],   "Types";
                   ["cml"],   "Contract";
                   ["cml"],   "Ngmap" ] in
  List.fold_left (fun theo (path,lib) ->
      let theolib = Env.read_theory env path lib in
      use theo theolib
    ) theory theolibs

let add_asset_ty_decl id key theo =
  let id = Ident.id_fresh id in
  let tys = Ty.create_tysymbol id [] (Ty.Alias (Ty.ty_app key [])) in
  let decl = Decl.create_ty_decl tys in
  Theory.add_decl theo decl

let get_key_type env =
  let types_theory = Env.read_theory env ["cml"] "Types" in
  Theory.ns_find_ts types_theory.Theory.th_export ["key"]

let add_asset_types env theory m =
  let key = get_key_type env in
  List.fold_left (fun theo id -> add_asset_ty_decl id key theo) theory (get_asset_names m)

let mk_loadpath main = (Whyconf.loadpath main) @ [cmlw_loadpath]

(*let add_storage_type _env theory m =
  let _fields = get_asset_fields m in
  let _tys =  Ty.create_tysymbol (Ident.id_fresh "storage") [] Ty.NoDef in
  let ty = (Ty.ty_var (Ty.create_tvsymbol (Ident.id_fresh "int"))) in
  let _constrs =
    List.map (fun (name,fname,_) ->

        (Term.create_fsymbol (Ident.id_fresh (name^"_"^fname)) [] ty,[])
      ) _fields in
  Theory.add_data_decl theory [_tys, [Term.fs_bool_true,[]; Term.fs_bool_false,[]]]
  let storage = Decl.create_data_decl [tys,constrs] in
  Theory.add_decl theory storage*)

let mk_theory (m : model) =
 let config : Whyconf.config = Whyconf.read_config None in
 let main : Whyconf.main = Whyconf.get_main config in
 let main = Whyconf.set_loadpath main (mk_loadpath main) in
 let env : Env.env = Env.create_env (Whyconf.loadpath main) in
 let theory = Theory.create_theory (Ident.id_fresh m.name) in
 let theory = add_use env theory in
 let theory = add_asset_types env theory m in
 (*let theory = add_storage_type env theory m in*)
 Theory.close_theory theory

let config : Whyconf.config = Whyconf.read_config None
let main : Whyconf.main = Whyconf.get_main config
let env : Env.env = Env.create_env (Whyconf.loadpath main)

let my_theory : Theory.theory_uc =
  Theory.create_theory (Ident.id_fresh "My_theory")

(* a ground propositional goal: true or false *)
let fmla_true : Term.term = Term.t_true
let fmla_false : Term.term = Term.t_false
let fmla1 : Term.term = Term.t_or fmla_true fmla_false

let prop_var_A : Term.lsymbol =
  Term.create_psymbol (Ident.id_fresh "A") []
let prop_var_B : Term.lsymbol =
  Term.create_psymbol (Ident.id_fresh "B") []
let atom_A : Term.term = Term.ps_app prop_var_A []
let atom_B : Term.term = Term.ps_app prop_var_B []
let fmla2 : Term.term =
  Term.t_implies (Term.t_and atom_A atom_B) atom_A

let two  : Term.term = Term.t_nat_const 2
let four : Term.term = Term.t_nat_const 4
let int_theory : Theory.theory = Env.read_theory env ["int"] "Int"
let plus_symbol : Term.lsymbol =
  Theory.ns_find_ls int_theory.Theory.th_export ["infix +"]
let two_plus_two : Term.term = Term.t_app_infer plus_symbol [two;two]
let fmla3 : Term.term = Term.t_equ two_plus_two four

let zero : Term.term = Term.t_nat_const 0
let mult_symbol : Term.lsymbol =
  Theory.ns_find_ls int_theory.Theory.th_export ["infix *"]
let ge_symbol : Term.lsymbol =
  Theory.ns_find_ls int_theory.Theory.th_export ["infix >="]
let var_x : Term.vsymbol =
  Term.create_vsymbol (Ident.id_fresh "x") Ty.ty_int
let x : Term.term = Term.t_var var_x
let x_times_x : Term.term = Term.t_app_infer mult_symbol [x;x]
let fmla4_aux : Term.term = Term.ps_app ge_symbol [x_times_x;zero]
let fmla4 : Term.term = Term.t_forall_close [var_x] [] fmla4_aux

let goal_id1 = Decl.create_prsymbol (Ident.id_fresh "goal1")
let goal_id2 = Decl.create_prsymbol (Ident.id_fresh "goal2")
let goal_id3 = Decl.create_prsymbol (Ident.id_fresh "goal3")
let goal_id4 = Decl.create_prsymbol (Ident.id_fresh "goal4")

let decl_goal1 : Decl.decl =
  Decl.create_prop_decl Decl.Pgoal goal_id1 fmla1
let my_theory : Theory.theory_uc = Theory.add_decl my_theory decl_goal1

let my_theory : Theory.theory_uc =
  Theory.add_param_decl my_theory prop_var_A
let my_theory : Theory.theory_uc =
  Theory.add_param_decl my_theory prop_var_B
let decl_goal2 : Decl.decl =
  Decl.create_prop_decl Decl.Pgoal goal_id2 fmla2
let my_theory : Theory.theory_uc = Theory.add_decl my_theory decl_goal2

(* helper function: [use th1 th2] insert the equivalent of a
   "use import th2" in theory th1 under construction *)
let use th1 th2 =
  let name = th2.Theory.th_name in
  Theory.close_scope
    (Theory.use_export (Theory.open_scope th1 name.Ident.id_string) th2)
    ~import:true

let int_theory : Theory.theory = Env.read_theory env ["int"] "Int"

let my_theory : Theory.theory_uc = use my_theory int_theory
let decl_goal3 : Decl.decl =
  Decl.create_prop_decl Decl.Pgoal goal_id3 fmla3
let my_theory : Theory.theory_uc = Theory.add_decl my_theory decl_goal3

let decl_goal4 : Decl.decl =
  Decl.create_prop_decl Decl.Pgoal goal_id4 fmla4
let my_theory : Theory.theory_uc = Theory.add_decl my_theory decl_goal4

let my_theory : Theory.theory = Theory.close_theory my_theory

let pr_theo theo = printf "@[my new theory is as follows:@\n@\n%a@]@."
              Pretty.print_theory theo

(* TYPING *)

let mk_ident s = { id_str = s; id_ats = []; id_loc = Loc.dummy_position }

let mk_expr e = { expr_desc = e; expr_loc = Loc.dummy_position }

let mk_term t = { term_desc = t; term_loc = Loc.dummy_position }

let mk_pat p = { pat_desc = p; pat_loc = Loc.dummy_position }
let pat_var id = mk_pat (Pvar id)

let mk_var id = mk_term (Tident (Qident id))

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

let mk_qid l =
  let rec aux l =
    match l with
      | [] -> assert false
      | [x] -> Qident(mk_ident x)
      | x::r -> Qdot(aux r,mk_ident x)
  in
  aux (List.rev l)

let use_import (f, m) =
  let m = mk_ident m in
  let loc = Loc.dummy_position in
  Typing.open_scope loc m;
  Typing.add_decl loc (Ptree.Duse (Qdot (Qident (mk_ident f), m)));
  Typing.close_scope loc ~import:true

let eq_symb = mk_qid [Ident.op_infix "="]
let int_type_id = mk_qid ["int"]
let int_type = PTtyapp(int_type_id,[])
let mul_int = mk_qid ["Int";Ident.op_infix "*"]

let d1 : Ptree.decl =
  let id_x = mk_ident "x" in
  let pre = mk_tapp eq_symb [mk_var id_x; mk_tconst "6"] in
  let result = mk_ident "result" in
  let post = mk_tapp eq_symb [mk_var result; mk_tconst "42"] in
  let spec = {
    sp_pre = [pre];
    sp_post = [Loc.dummy_position,[pat_var result,post]];
    sp_xpost = [];
    sp_reads = [];
    sp_writes = [];
    sp_alias = [];
    sp_variant = [];
    sp_checkrw = false;
    sp_diverge = false;
    sp_partial = false;
  }
  in
  let body = mk_eapp mul_int [mk_evar id_x; mk_econst "7"] in
  let f1 =
    Efun(param1 id_x int_type, None, Ity.MaskVisible, spec, body)
  in
  Dlet(mk_ident "f1",false,Expr.RKnone, mk_expr f1)

let test_typing () =
  let config : Whyconf.config = Whyconf.read_config None in
  let main : Whyconf.main = Whyconf.get_main config in
  let main = Whyconf.set_loadpath main (mk_loadpath main) in
  let env : Env.env = Env.create_env (Whyconf.loadpath main) in

  let () = Typing.open_file env [] (* empty pathname *) in
  let () = Typing.open_module (mk_ident "Miles_with_expiration") in
  let () = use_import ("int","Int") in
  let d1 : Ptree.decl =
    let id_x = mk_ident "x" in
    let pre = mk_tapp eq_symb [mk_var id_x; mk_tconst "6"] in
    let result = mk_ident "result" in
    let post = mk_tapp eq_symb [mk_var result; mk_tconst "42"] in
    let spec = {
        sp_pre = [pre];
        sp_post = [Loc.dummy_position,[pat_var result,post]];
        sp_xpost = [];
        sp_reads = [];
        sp_writes = [];
        sp_alias = [];
        sp_variant = [];
        sp_checkrw = false;
        sp_diverge = false;
        sp_partial = false;
      }
    in
    let body = mk_eapp mul_int [mk_evar id_x; mk_econst "7"] in
    let f1 =
      Efun(param1 id_x int_type, None, Ity.MaskVisible, spec, body)
    in
    Dlet(mk_ident "f1",false,Expr.RKnone, mk_expr f1) in
  (try Typing.add_decl Loc.dummy_position d1
  with e ->
    Format.printf "Exception raised during typing of d:@ %a@."
       Exn_printer.exn_printer e);
  let () = Typing.close_module Loc.dummy_position in
  let mods : Pmodule.pmodule Wstdlib.Mstr.t = Typing.close_file () in
  Wstdlib.Mstr.iter (fun _ m ->
      pr_theo m.Pmodule.mod_theory)
    mods

let _ =
  pr_theo my_theory;
  pr_theo (mk_theory (mk_miles_model ()));
  test_typing ()
