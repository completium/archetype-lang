(* opening the Why3 library *)
open Why3
open Format
open Model
open Miles

(* adapt according to system ? *)
let cmlw_loadpath = "/home/dev/contract-model-language/models/why3tests/"

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

let mk_theory (m : model) =
 let config : Whyconf.config = Whyconf.read_config None in
 let main : Whyconf.main = Whyconf.get_main config in
 let main = Whyconf.set_loadpath main (mk_loadpath main) in
 let env : Env.env = Env.create_env (Whyconf.loadpath main) in
 let theory = Theory.create_theory (Ident.id_fresh m.name) in
 let theory = add_use env theory in
 let theory = add_asset_types env theory m in
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

let _ =
  pr_theo my_theory;
  pr_theo (mk_theory (mk_miles_model ()))
