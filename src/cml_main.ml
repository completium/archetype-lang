(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2018   --   Inria - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)

open Why3
open CmlParseTree
open Pmodule
open Ptree
open Wstdlib

let debug = Debug.register_flag "cml"
  ~desc:"cml plugin debug flag"

let mk_id ~loc name =
  { id_str = name; id_ats = []; id_loc = loc }

let infix  ~loc s = Qident (mk_id ~loc (Ident.op_infix s))
let prefix ~loc s = Qident (mk_id ~loc (Ident.op_prefix s))
let get_op ~loc   = Qident (mk_id ~loc (Ident.op_get ""))
let set_op ~loc   = Qident (mk_id ~loc (Ident.op_set ""))

let mk_expr ~loc d =
  { expr_desc = d; expr_loc = loc }
let mk_term ~loc d =
  { term_desc = d; term_loc = loc }
let mk_pat ~loc d =
  { pat_desc = d; pat_loc = loc }
let mk_unit ~loc =
  mk_expr ~loc (Etuple [])
let mk_var ~loc id =
  mk_expr ~loc (Eident (Qident id))
let mk_tvar ~loc id =
  mk_term ~loc (Tident (Qident id))
let mk_ref ~loc e =
  mk_expr ~loc (Eidapp (Qident (mk_id ~loc "ref"), [e]))
let deref_id ~loc id =
  mk_expr ~loc (Eidapp (prefix ~loc "!", [mk_expr ~loc (Eident (Qident id))]))
let array_set ~loc a i v =
  mk_expr ~loc (Eidapp (set_op ~loc, [a; i; v]))
let constant ~loc i =
  mk_expr ~loc (Econst (Number.const_of_int i))
let constant_s ~loc s =
  mk_expr ~loc (Econst (Number.(ConstInt { ic_negative = false ; ic_abs = int_literal_dec s})))
let len ~loc =
  Qident (mk_id ~loc "len")
let break ~loc =
  Qident (mk_id ~loc "Break")
let break_handler ~loc =
  [break ~loc, None, mk_unit ~loc]
let return ~loc =
  Qident (mk_id ~loc "Return")
let return_handler ~loc =
  let x = mk_id ~loc "x" in
  [return ~loc, Some (mk_pat ~loc (Pvar x)), mk_var ~loc x]
let array_make ~loc n v =
  mk_expr ~loc (Eidapp (Qdot (Qident (mk_id ~loc "Array"), mk_id ~loc "make"),
                        [n; v]))

let empty_spec = {
  sp_pre     = [];
  sp_post    = [];
  sp_xpost   = [];
  sp_reads   = [];
  sp_writes  = [];
  sp_alias   = [];
  sp_variant = [];
  sp_checkrw = false;
  sp_diverge = false;
  sp_partial = false;
}

type env = {
  vars: ident Mstr.t;
  for_index: int;
}

let empty_env =
  { vars = Mstr.empty;
    for_index = 0; }

(*let add_var env id =
  { env with vars = Mstr.add id.id_str id env.vars }

let for_vars ~loc env =
  let i = env.for_index in
  let env = { env with for_index = i + 1 } in
  let i = string_of_int env.for_index in
  mk_id ~loc ("for index " ^ i ), mk_id ~loc ("for list " ^ i), env

(* dereference all variables from the environment *)
let deref env t =
  let deref _ ({id_loc=loc} as id) t =
    let tid = mk_term ~loc (Tident (Qident id)) in
    let tid = mk_term ~loc (Tidapp (prefix ~loc "!", [tid])) in
    mk_term ~loc:t.term_loc (Tlet (id, tid, t)) in
  Mstr.fold deref env.vars t

let rec has_stmt p = function
  | Dstmt s -> p s || begin match s.stmt_desc with
    | Sbreak  | Sreturn _ | Sassign _ | Slabel _
    | Seval _ | Sset _ | Sassert _ | Swhile _ -> false
    | Sif (_, bl1, bl2) -> has_stmtl p bl1 || has_stmtl p bl2
    | Sfor (_, _, _, bl) -> has_stmtl p bl end
  | _ -> false
and has_stmtl p bl = List.exists (has_stmt p) bl

let has_breakl = has_stmtl (fun s -> s.stmt_desc = Sbreak)
let has_returnl = has_stmtl (function { stmt_desc = Sreturn _ } -> true | _ -> false)

let rec expr_has_call id e = match e.Py_ast.expr_desc with
  | Enone | Ebool _ | Eint _ | Estring _ | Py_ast.Eident _ -> false
  | Emake (e1, e2) | Eget (e1, e2) | Ebinop (_, e1, e2) ->
    expr_has_call id e1 || expr_has_call id e2
  | Eunop (_, e1) -> expr_has_call id e1
  | Ecall (f, el) -> id.id_str = f.id_str || List.exists (expr_has_call id) el
  | Elist el -> List.exists (expr_has_call id) el

let rec stmt_has_call id s = match s.stmt_desc with
  | Sbreak | Slabel _ | Sassert _ -> false
  | Sreturn e | Sassign (_, e) | Seval e -> expr_has_call id e
  | Sset (e1, e2, e3) ->
    expr_has_call id e1 || expr_has_call id e2 || expr_has_call id e3
  | Sif (e, s1, s2) -> expr_has_call id e || block_has_call id s1 || block_has_call id s2
  | Sfor (_, e, _, s) | Swhile (e, _, _, s) -> expr_has_call id e || block_has_call id s
and block_has_call id = has_stmtl (stmt_has_call id)

let rec expr env {Py_ast.expr_loc = loc; Py_ast.expr_desc = d } = match d with
  | Py_ast.Enone ->
    mk_unit ~loc
  | Py_ast.Ebool b ->
    mk_expr ~loc (if b then Etrue else Efalse)
  | Py_ast.Eint s ->
    constant_s ~loc s
  | Py_ast.Estring _s ->
    mk_unit ~loc (*FIXME*)
  | Py_ast.Eident id ->
    if not (Mstr.mem id.id_str env.vars) then
      Loc.errorm ~loc "unbound variable %s" id.id_str;
    deref_id ~loc id
  | Py_ast.Ebinop (op, e1, e2) ->
    let e1 = expr env e1 in
    let e2 = expr env e2 in
    mk_expr ~loc (match op with
      | Py_ast.Band -> Eand (e1, e2)
      | Py_ast.Bor  -> Eor  (e1, e2)
      | Py_ast.Badd -> Eidapp (infix ~loc "+", [e1; e2])
      | Py_ast.Bsub -> Eidapp (infix ~loc "-", [e1; e2])
      | Py_ast.Bmul -> Eidapp (infix ~loc "*", [e1; e2])
      | Py_ast.Bdiv -> Eidapp (infix ~loc "//", [e1; e2])
      | Py_ast.Bmod -> Eidapp (infix ~loc "%", [e1; e2])
      | Py_ast.Beq  -> Eidapp (infix ~loc "=", [e1; e2])
      | Py_ast.Bneq -> Eidapp (infix ~loc "<>", [e1; e2])
      | Py_ast.Blt  -> Eidapp (infix ~loc "<", [e1; e2])
      | Py_ast.Ble  -> Eidapp (infix ~loc "<=", [e1; e2])
      | Py_ast.Bgt  -> Eidapp (infix ~loc ">", [e1; e2])
      | Py_ast.Bge  -> Eidapp (infix ~loc ">=", [e1; e2])
    )
  | Py_ast.Eunop (Py_ast.Uneg, e) ->
    mk_expr ~loc (Eidapp (prefix ~loc "-", [expr env e]))
  | Py_ast.Eunop (Py_ast.Unot, e) ->
    mk_expr ~loc (Eidapp (Qident (mk_id ~loc "not"), [expr env e]))
  | Py_ast.Ecall ({id_str="print"}, el) ->
    let eval res e =
      mk_expr ~loc (Elet (mk_id ~loc "_", false, Expr.RKnone, expr env e, res)) in
    List.fold_left eval (mk_unit ~loc) el
  | Py_ast.Ecall (id, el) ->
    mk_expr ~loc (Eidapp (Qident id, List.map (expr env) el))
  | Py_ast.Emake (e1, e2) -> (* [e1]*e2 *)
    array_make ~loc (expr env e2) (expr env e1)
  | Py_ast.Elist [] ->
    array_make ~loc (constant ~loc 0) (constant ~loc 0)
  | Py_ast.Elist el ->
    let n = List.length el in
    let n = constant ~loc n in
    let id = mk_id ~loc "new array" in
    mk_expr ~loc (Elet (id, false, Expr.RKnone, array_make ~loc n (constant ~loc 0),
    let i = ref (-1) in
    let init seq e =
      incr i; let i = constant ~loc !i in
      let assign = array_set ~loc (mk_var ~loc id) i (expr env e) in
      mk_expr ~loc (Esequence (assign, seq)) in
    List.fold_left init (mk_var ~loc id) el))
  | Py_ast.Eget (e1, e2) ->
    mk_expr ~loc (Eidapp (get_op ~loc, [expr env e1; expr env e2]))
 *)
let no_params ~loc = [loc, None, false, Some (PTtuple [])]
(*
let rec stmt env ({Py_ast.stmt_loc = loc; Py_ast.stmt_desc = d } as s) =
  match d with
  | Py_ast.Seval e ->
    expr env e
  | Py_ast.Sif (e, s1, s2) ->
    mk_expr ~loc (Eif (expr env e, block env ~loc s1, block env ~loc s2))
  | Py_ast.Sreturn e ->
    mk_expr ~loc (Eraise (return ~loc, Some (expr env e)))
  | Py_ast.Sassign (id, e) ->
    let e = expr env e in
    if Mstr.mem id.id_str env.vars then
      let x = let loc = id.id_loc in mk_expr ~loc (Eident (Qident id)) in
      mk_expr ~loc (Einfix (x, mk_id ~loc (Ident.op_infix ":="), e))
    else
      block env ~loc [Dstmt s]
  | Py_ast.Sset (e1, e2, e3) ->
    array_set ~loc (expr env e1) (expr env e2) (expr env e3)
  | Py_ast.Sassert (k, t) ->
    mk_expr ~loc (Eassert (k, deref env t))
  | Py_ast.Swhile (e, inv, var, s) ->
    let inv = List.map (deref env) inv in
    let var = List.map (fun (t, o) -> deref env t, o) var in
    let loop = mk_expr ~loc
      (Ewhile (expr env e, inv, var, block env ~loc s)) in
    if has_breakl s then mk_expr ~loc (Ematch (loop, [], break_handler ~loc))
    else loop
  | Py_ast.Sbreak ->
    mk_expr ~loc (Eraise (break ~loc, None))
  | Py_ast.Slabel _ ->
    mk_unit ~loc (* ignore lonely marks *)
  (* make a special case for
       for id in range(e1, e2):
    *)
  | Py_ast.Sfor (id, {Py_ast.expr_desc=Ecall ({id_str="range"}, [e1;e2])},
                 inv, body) ->
    let inv = List.map (deref env) inv in
    let e_to = expr env e2 in
    let ub = mk_expr ~loc (Eidapp (infix ~loc "-", [e_to; constant ~loc 1])) in
    mk_expr ~loc (Efor (id, expr env e1, Expr.To, ub, inv,
    mk_expr ~loc (Elet (id, false, Expr.RKnone, mk_ref ~loc (mk_var ~loc id),
    let env = add_var env id in
    block ~loc env body))))
  (* otherwise, translate
       for id in e:
         #@ invariant inv
         body
    to
       let l = e in
       for i = 0 to len(l)-1 do
         invariant { let id = l[i] in I }
         let id = ref l[i] in
         body
       done
    *)
  | Py_ast.Sfor (id, e, inv, body) ->
    let i, l, env = for_vars ~loc env in
    let e = expr env e in
    mk_expr ~loc (Elet (l, false, Expr.RKnone, e, (* evaluate e only once *)
    let lb = constant ~loc 0 in
    let lenl = mk_expr ~loc (Eidapp (len ~loc, [mk_var ~loc l])) in
    let ub = mk_expr ~loc (Eidapp (infix ~loc "-", [lenl; constant ~loc 1])) in
    let invariant inv =
      let loc = inv.term_loc in
      let li = mk_term ~loc
        (Tidapp (get_op ~loc, [mk_tvar ~loc l; mk_tvar ~loc i])) in
      mk_term ~loc (Tlet (id, li, deref env inv)) in
    mk_expr ~loc (Efor (i, lb, Expr.To, ub, List.map invariant inv,
    let li = mk_expr ~loc
      (Eidapp (get_op ~loc, [mk_var ~loc l; mk_var ~loc i])) in
    mk_expr ~loc (Elet (id, false, Expr.RKnone, mk_ref ~loc li,
    let env = add_var env id in
    block ~loc env body
    ))))))

and block env ~loc = function
  | [] ->
    mk_unit ~loc
  | Dstmt { stmt_loc = loc; stmt_desc = Slabel id } :: sl ->
    mk_expr ~loc (Elabel (id, block env ~loc sl))
  | Dstmt { Py_ast.stmt_loc = loc; stmt_desc = Py_ast.Sassign (id, e) } :: sl
    when not (Mstr.mem id.id_str env.vars) ->
    let e = expr env e in (* check e *before* adding id to environment *)
    let env = add_var env id in
    mk_expr ~loc (Elet (id, false, Expr.RKnone, mk_ref ~loc e, block env ~loc sl))
  | Dstmt ({ Py_ast.stmt_loc = loc } as s) :: sl ->
    let s = stmt env s in
    if sl = [] then s else mk_expr ~loc (Esequence (s, block env ~loc sl))
  | Ddef (id, idl, sp, bl) :: sl ->
    (* f(x1,...,xn): body ==>
      let f x1 ... xn =
        let x1 = ref x1 in ... let xn = ref xn in
        try body with Return x -> x *)
    let env' = List.fold_left add_var empty_env idl in
    let body = block env' ~loc:id.id_loc bl in
    let body = if not (has_returnl bl) then body else
      let loc = id.id_loc in
      mk_expr ~loc (Ematch (body, [], return_handler ~loc)) in
    let local bl id =
      let loc = id.id_loc in
      let ref = mk_ref ~loc (mk_var ~loc id) in
      mk_expr ~loc (Elet (id, false, Expr.RKnone, ref, bl)) in
    let body = List.fold_left local body idl in
    let param id = id.id_loc, Some id, false, None in
    let params = if idl = [] then no_params ~loc else List.map param idl in
    let s = block env ~loc sl in
    let e = if block_has_call id bl then
      Erec ([id, false, Expr.RKnone, params, None, Ity.MaskVisible, sp, body], s)
    else
      let e = Efun (params, None, Ity.MaskVisible, sp, body) in
      Elet (id, false, Expr.RKnone, mk_expr ~loc e, s) in
    mk_expr ~loc e
  | (Dimport _ | Py_ast.Dlogic _) :: sl ->
    block env ~loc sl

let fresh_type_var =
  let r = ref 0 in
  fun loc -> incr r;
    PTtyvar { id_str = "a" ^ string_of_int !r; id_loc = loc; id_ats = [] }

let logic_param id =
  id.id_loc, Some id, false, fresh_type_var id.id_loc

let logic = function
  | Py_ast.Dlogic (func, id, idl) ->
    let d = { ld_loc = id.id_loc;
              ld_ident = id;
              ld_params = List.map logic_param idl;
              ld_type = if func then Some (fresh_type_var id.id_loc) else None;
              ld_def = None } in
    Typing.add_decl id.id_loc (Dlogic [d])
  | _ -> ()
 *)
let translate ~loc dl =
  (*  List.iter logic dl;*)
  (*  let bl = block empty_env ~loc dl in*)
  let bl = dl in
  let fd = Efun (no_params ~loc, None, Ity.MaskVisible, empty_spec, bl) in
  let main = Dlet (mk_id ~loc "main", false, Expr.RKnone, mk_expr ~loc fd) in
  Typing.add_decl loc main

let read_channel env path file c =
  let f = CmlIo.parse_model ~name:file c in
  Debug.dprintf debug "%s parsed successfully.@." file;
  let file = Filename.basename file in
  let file = Filename.chop_extension file in
  let name = Strings.capitalize file in
  Debug.dprintf debug "building module %s.@." name;
  Typing.open_file env path;
  let loc = Loc.user_position file 0 0 0 in
  Typing.open_module (mk_id ~loc name);
(*  let use_import (f, m) =
    let m = mk_id ~loc m in
    Typing.open_scope loc m;
    Typing.add_decl loc (Ptree.Duse (Qdot (Qident (mk_id ~loc f), m)));
    Typing.close_scope loc ~import:true in
  List.iter use_import
    ["int", "Int"; "ref", "Refint"; "python", "Python"];*)
  (*  translate ~loc f;*)
  Typing.close_module loc;
  let mm = Typing.close_file () in
  if path = [] && Debug.test_flag debug then begin
    let add_m _ m modm = Ident.Mid.add m.mod_theory.Theory.th_name m modm in
    let print_m _ m = Pmodule.print_module Format.err_formatter m in
    Ident.Mid.iter print_m (Mstr.fold add_m mm Ident.Mid.empty)
  end;
  mm

let () =
  Env.register_format mlw_language "cml" ["cml"] read_channel
    ~desc:"cml format"
