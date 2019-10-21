(********************************************************************)
(*                                                                  *)
(*  The Why3 Verification Platform   /   The Why3 Development Team  *)
(*  Copyright 2010-2019   --   Inria - CNRS - Paris-Sud University  *)
(*                                                                  *)
(*  This software is distributed under the terms of the GNU Lesser  *)
(*  General Public License version 2.1, with the special exception  *)
(*  on linking described in file LICENSE.                           *)
(*                                                                  *)
(********************************************************************)
(* [%% import "config.mlh" ] *)

open Why3
open Ptree

(* printing *)

open Format

type printing_kind = | Ast | Mlw

module type Ast_or_mlw = sig
  val pp_kind : printing_kind
  val pp_loc  : bool
end

module Output ( M : Ast_or_mlw) = struct

  let usanitize = Ident.(sanitizer char_to_ualpha char_to_alnumus)
  let lsanitize = Ident.(sanitizer char_to_lalpha char_to_alnumus)
  (* lprinter: lowercase symbols
     uprinter: uppercase symbols *)
  let lprinter, uprinter =
    Ident.create_ident_printer Pretty.why3_keywords ~sanitizer:lsanitize,
    Ident.create_ident_printer Pretty.why3_keywords ~sanitizer:usanitize

  let pp_kind = M.pp_kind
  let pp_loc = M.pp_loc

  let ast_or_mlw (ast_fmt : ('a, formatter, unit) format) (mlw_fmt : ('a, formatter, unit) format) =
    match pp_kind with
    | Ast -> ast_fmt
    | Mlw -> mlw_fmt

  (* Pp n-uple missing functions *)

  let print_3uple pp_1 pp_2 pp_3 fmt (x1,x2,x3) =
    fprintf fmt (ast_or_mlw "(%a,%a,%a)" "(%a,%a,%a)") pp_1 x1 pp_2 x2 pp_3 x3

  (* Pp for external types *)

  let print_attribute fmt a =
    match pp_kind with
    | Ast -> fprintf fmt "attr(attr_tag:%d,attr_str:%s)" a.Ident.attr_tag a.Ident.attr_string
    | Mlw -> fprintf fmt "[@@%s]" a.Ident.attr_string

  let print_loc fmt l =
    let (fname,lnum,cnum1,cnum2) = Loc.get l in
    match pp_kind with
    | Ast ->
      (
        if pp_loc then
          fprintf fmt "loc(fname:%s,line:%d,column:%d-%d)" fname lnum cnum1 cnum2
        else
          fprintf fmt ""
      )
    | Mlw -> fprintf fmt "\"%s\" %d %d %d" fname lnum cnum1 cnum2

  let print_dbinop fmt db =
    Dterm.(
      match db with
      | DTand -> fprintf fmt (ast_or_mlw "DTand" "/\\")
      | DTand_asym -> fprintf fmt (ast_or_mlw "DTand_sym" "&&")
      | DTor -> fprintf fmt (ast_or_mlw "DTor" "\\/")
      | DTor_asym -> fprintf fmt (ast_or_mlw "DTor_asym" "||")
      | DTimplies -> fprintf fmt (ast_or_mlw "DTimplies" "->")
      | DTiff -> fprintf fmt (ast_or_mlw "DTiff" "<->")
      | DTby -> fprintf fmt (ast_or_mlw "DTby" "by")
      | DTso -> fprintf fmt (ast_or_mlw "DTso" "so")
    )

  let print_dquant fmt q =
    Dterm.(
      match q with
      | DTforall -> fprintf fmt (ast_or_mlw "DTforall" "forall")
      | DTexists -> fprintf fmt (ast_or_mlw "DTexists" "exists")
      | DTlambda -> fprintf fmt (ast_or_mlw "DTlambda" "lambda")
    )

  let print_rs_kind fmt rsk =
    Expr.(
      match rsk with
      | RKnone -> fprintf fmt (ast_or_mlw "RKnone" "RKnone")
      | RKlocal -> fprintf fmt (ast_or_mlw "RKlocal" "RKlocal")
      | RKfunc -> fprintf fmt (ast_or_mlw "RKfunc" "RKfunc")
      | RKpred -> fprintf fmt (ast_or_mlw "RKpred" "RKpred")
      | RKlemma -> fprintf fmt (ast_or_mlw "RKlemma" "RKlemma")
    )

  let rec print_mask fmt m =
    Ity.(
      match m with
      | MaskVisible -> fprintf fmt (ast_or_mlw "MaskVisible" "MaskVisible")
      | MaskTuple mask_list -> fprintf fmt (ast_or_mlw "MaskTuple(%a)" "MaskTuple(%a)") (Pp.print_list Pp.comma print_mask) mask_list
      | MaskGhost -> fprintf fmt (ast_or_mlw "MaskGhost" "MaskGhost")
    )

  let print_for_direction fmt fd =
    Expr.(
      match fd with
      | To -> fprintf fmt (ast_or_mlw "To" "To")
      | DownTo -> fprintf fmt (ast_or_mlw "DownTo" "DownTo")
    )

  let print_assertion_kind fmt ak =
    Expr.(
      match ak with
      | Assert -> fprintf fmt (ast_or_mlw "Assert" "assert")
      | Assume -> fprintf fmt (ast_or_mlw "Assume" "Assume")
      | Check -> fprintf fmt (ast_or_mlw "Check" "Check")
    )

  let print_big_int fmt bi =
    fprintf fmt (ast_or_mlw "%s" "%s") (BigInt.to_string bi)

  let print_ind_sign fmt is =
    Decl.(
      match is with
      | Ind -> fprintf fmt (ast_or_mlw "Ind" "Ind")
      | Coind -> fprintf fmt (ast_or_mlw "Coind" "Coind")
    )

  let print_prop_kind fmt pk =
    Decl.(
      match pk with
      | Plemma -> fprintf fmt (ast_or_mlw "Plemma" "lemma")
      | Paxiom -> fprintf fmt (ast_or_mlw "Paxiom" "axiom")
      | Pgoal -> fprintf fmt (ast_or_mlw "Pgoal" "goal")
    )

  let print_ident_str fmt (i : Ptree.ident) =
    Ident.(match sn_decode i.id_str with
        | SNinfix  s -> fprintf fmt (ast_or_mlw "SNinfix(%s)" "(%s)") s
        | SNtight  s -> fprintf fmt (ast_or_mlw "SNtight(%s)" "(%s)") s
        | SNprefix s -> fprintf fmt (ast_or_mlw "SNprefix(%s)" "(%s)") s
        | SNget    s -> fprintf fmt (ast_or_mlw "SNget(%s)" "(%s)") s
        | SNset    s -> fprintf fmt (ast_or_mlw "SNset(%s)" "(%s)") s
        | SNupdate s -> fprintf fmt (ast_or_mlw "SNupdate(%s)" "(%s)") s
        | SNcut    s -> fprintf fmt (ast_or_mlw "SNcut(%s)" "(%s)") s
        | SNlcut   s -> fprintf fmt (ast_or_mlw "SNlcut(%s)" "(%s)") s
        | SNrcut   s -> fprintf fmt (ast_or_mlw "SNrcut(%s)" "(%s)") s
        | SNword   s -> fprintf fmt (ast_or_mlw "SNword(%s)" "%s") s
      )

  (*s Parse trees. *)

  let print_attr fmt a =
    match a with
    | ATstr attribute -> fprintf fmt (ast_or_mlw "ATstr(%a)" "%a") print_attribute attribute
    | ATpos position -> fprintf fmt (ast_or_mlw "ATpos(%a)" "[#%a]") print_loc position

  let print_ident fmt i =
    match pp_kind with
    | Ast -> fprintf fmt "ident(id_str:%s,id_ats:%a,id_loc(%a))" i.id_str (Pp.print_list Pp.comma print_attr) i.id_ats print_loc i.id_loc
    | Mlw -> fprintf fmt "@[%a@ [#%a] %a@]" print_ident_str i print_loc i.id_loc
               (Pp.print_list Pp.space print_attr) i.id_ats

  (*s Types *)

  let rec print_qualid fmt q =
    match q with
    | Qident ident -> fprintf fmt (ast_or_mlw "Qident(%a)" "%a") print_ident_str ident
    | Qdot(qualid,ident) -> fprintf fmt (ast_or_mlw "Qdot(%a,%a)" "%a.%a") print_qualid qualid print_ident_str ident

  let rec print_pty fmt pt =
    match pt with
    | PTtyvar ident -> fprintf fmt (ast_or_mlw "PTtyvar(%a)" "'%a") print_ident_str ident
    | PTtyapp(qualid,[]) -> fprintf fmt (ast_or_mlw "PTtyapp(%a,[])" "%a") print_qualid qualid
    | PTtyapp(qualid,pty_hd::pty_tl) -> fprintf fmt (ast_or_mlw "PTtyapp(%a,%a)" "%a %a") print_qualid qualid (Pp.print_list Pp.space print_pty) (pty_hd::pty_tl)
    | PTtuple [] -> fprintf fmt (ast_or_mlw "PTtuple([])" "unit")
    | PTtuple(pty_hd::pty_tl) -> fprintf fmt (ast_or_mlw "PTtuple(%a)" "PTtuple(%a)") (Pp.print_list Pp.comma print_pty) (pty_hd::pty_tl)
    | PTref   pty_list -> fprintf fmt (ast_or_mlw "PTref(%a)" "PTref(%a)") (Pp.print_list Pp.comma print_pty) pty_list
    | PTarrow(pty1,pty2) -> fprintf fmt (ast_or_mlw "PTarrow(%a,%a)" "PTarrow(%a,%a)") print_pty pty1 print_pty pty2
    | PTscope(qualid,pty) -> fprintf fmt (ast_or_mlw "PTscope(%a,%a)" "PTscope(%a,%a)") print_qualid qualid print_pty pty
    | PTparen pty -> fprintf fmt (ast_or_mlw "PTparen(%a)" "PTparen(%a)") print_pty pty
    | PTpure  pty -> fprintf fmt (ast_or_mlw "PTpure(%a)" "PTpure(%a)") print_pty pty

  (*s Patterns *)

  let print_ghost fmt g =
    match pp_kind with
    | Ast -> fprintf fmt "ghost(%B)" g
    | Mlw -> if g then fprintf fmt "ghost "

  let rec print_pattern fmt p =
    match pp_kind with
    | Ast -> fprintf fmt "pattern(pat_desc:(%a),pat_loc(%a))" print_pat_desc p.pat_desc print_loc p.pat_loc
    | Mlw -> fprintf fmt "%a" print_pat_desc p.pat_desc

  and print_pat_desc fmt pd =
    match pd with
    | Pwild -> fprintf fmt (ast_or_mlw "Pwild" "Pwild")
    | Pvar ident -> fprintf fmt (ast_or_mlw "Pvar(%a)" "%a") print_ident_str ident
    | Papp(qualid,pattern_list) -> fprintf fmt (ast_or_mlw "Papp(%a,%a)" "Papp(%a,%a)") print_qualid qualid (Pp.print_list Pp.comma print_pattern) pattern_list
    | Prec qualid_pattern_list -> fprintf fmt (ast_or_mlw "Prec(%a)" "Prec(%a)") (Pp.print_list Pp.comma (Pp.print_pair print_qualid print_pattern)) qualid_pattern_list
    | Ptuple pattern_list -> fprintf fmt (ast_or_mlw "Ptuple(%a)" "Ptuple(%a)") (Pp.print_list Pp.comma print_pattern) pattern_list
    | Pas(pattern,ident,ghost) -> fprintf fmt (ast_or_mlw "Pas(%a,%a,%a)" "Pas(%a,%a,%a)") print_pattern pattern print_ident_str ident print_ghost ghost
    | Por(pattern1,pattern2) -> fprintf fmt (ast_or_mlw "Por(%a,%a)" "Por(%a,%a)") print_pattern pattern1 print_pattern pattern2
    | Pcast(pattern,pty) -> fprintf fmt (ast_or_mlw "Pcast(%a,%a)" "Pcast(%a,%a)") print_pattern pattern print_pty pty
    | Pscope(qualid,pattern) -> fprintf fmt (ast_or_mlw "Pscope(%a,%a)" "Pscope(%a,%a)") print_qualid qualid print_pattern pattern
    | Pparen pattern -> fprintf fmt (ast_or_mlw "Pparen(%a)" "Pparen(%a)") print_pattern pattern
    | Pghost pattern -> fprintf fmt (ast_or_mlw "Pghost(%a)" "Pghost(%a)") print_pattern pattern

  (*s Logical terms and formulas *)

  let print_binder ?(paren=false) fmt (position,ident,ghost,pty) =
    match pp_kind with
    | Ast -> fprintf fmt "binder(%a,%a,%a,%a)" print_loc position (Pp.print_option print_ident) ident print_ghost ghost (Pp.print_option print_pty) pty
    | Mlw -> match (ident,pty) with
      | Some id, Some ty ->
        if paren then
          fprintf fmt "(%a:%a)" print_ident id print_pty ty
        else
          fprintf fmt "%a:%a" print_ident id print_pty ty
      | None,Some ty ->
        if paren then
          fprintf fmt "(_:%a)" print_pty ty
        else
          fprintf fmt "%a" print_pty ty
      | _,None ->
        fprintf fmt "<_,None:binder>"

  let print_param fmt (position,ident,ghost,pty) =
    match pp_kind with
    | Ast -> fprintf fmt "param(%a,%a,%a,%a)" print_loc position (Pp.print_option print_ident_str) ident print_ghost ghost print_pty pty
    | Mlw -> fprintf fmt "@[%a (%a : %a)@]" print_ghost ghost (Pp.print_option print_ident_str) ident print_pty pty

  let rec print_term fmt t =
    match pp_kind with
    | Ast -> fprintf fmt "term(term_desc:%a,term_loc:%a)" print_term_desc t.term_desc print_loc t.term_loc
    | Mlw -> fprintf fmt "%a" print_term_desc t.term_desc

  and print_term_desc fmt td =
    match td with
    | Ttrue -> fprintf fmt (ast_or_mlw "Ttrue" "true")
    | Tfalse -> fprintf fmt (ast_or_mlw "Tfalse" "false")
    | Tconst constant -> fprintf fmt (ast_or_mlw "Tconst(%a)" "%a") Number.print_constant constant
    | Tident qualid -> fprintf fmt (ast_or_mlw "Tident(%a)" "%a") print_qualid qualid
    | Tasref qualid -> fprintf fmt (ast_or_mlw "Tasref(%a)" "Tasref(%a)") print_qualid qualid
    | Tidapp(qualid,term_list) -> fprintf fmt (ast_or_mlw "Tidapp(%a,%a)" "@[(%a@ %a)@]") print_qualid qualid (Pp.print_list Pp.space print_term) term_list
    | Tapply(term1,term2) -> fprintf fmt (ast_or_mlw "Tapply(%a,%a)" "@[(%a@ %a)@]") print_term term1 print_term term2
    | Tinfix(term1,ident,term2) ->
      (
        match pp_kind with
        | Ast -> fprintf fmt "Tinfix(%a,%a,%a)" print_term term1 print_ident_str ident print_term term2
        | Mlw -> fprintf fmt "@[%a(%a,%a)@]" print_ident_str ident print_term term1 print_term term2
      )
    | Tinnfix(term1,ident,term2) -> fprintf fmt (ast_or_mlw "Tinnfix(%a,%a,%a)" "Tinnfix(%a,%a,%a)") print_term term1 print_ident_str ident print_term term2
    | Tbinop(term1,dbinop,term2) -> fprintf fmt (ast_or_mlw "Tbinop(%a,%a,%a)" "@[(%a %a@ %a)@]") print_term term1 print_dbinop dbinop print_term term2
    | Tbinnop(term1,dbinop,term2) -> fprintf fmt (ast_or_mlw "Tbinnop(%a,%a,%a)" "Tbinnop(%a,%a,%a)") print_term term1 print_dbinop dbinop print_term term2
    | Tnot term -> fprintf fmt (ast_or_mlw "Tnot(%a)" "Tnot(%a)") print_term term
    | Tif(term_if,term_then,term_else) -> fprintf fmt (ast_or_mlw "Tif(%a,%a,%a)" "Tif(%a,%a,%a)") print_term term_if print_term term_then print_term term_else
    | Tquant(dquant,binder_list,[],term) -> fprintf fmt (ast_or_mlw "Tquant(%a,%a,[],%a)" "@[%a %a.@ %a@]") print_dquant dquant (Pp.print_list Pp.comma (print_binder ~paren:false)) binder_list print_term term
    | Tquant(dquant,binder_list,term_list_hd::term_list_tl,term) -> fprintf fmt (ast_or_mlw "Tquant(%a,%a,%a,%a)" "Tquant(%a,%a,%a,%a)") print_dquant dquant (Pp.print_list Pp.comma (print_binder ~paren:false)) binder_list (Pp.print_list Pp.comma (Pp.print_list Pp.comma print_term)) (term_list_hd::term_list_tl) print_term term
    | Tattr(attr,term) -> fprintf fmt (ast_or_mlw "Tattr(%a,%a)" "Tattr(%a,%a)") print_attr attr print_term term
    | Tlet(ident,term1,term2) -> fprintf fmt (ast_or_mlw "Tlet(%a,%a,%a)" "Tlet(%a,%a,%a)") print_ident_str ident print_term term1 print_term term2
    | Tcase(term,pattern_term_list) -> fprintf fmt (ast_or_mlw "Tcase(%a,%a)" "Tcase(%a,%a)") print_term term (Pp.print_list Pp.comma (Pp.print_pair print_pattern print_term)) pattern_term_list
    | Tcast(term,pty) -> fprintf fmt (ast_or_mlw "Tcast(%a,%a)" "Tcast(%a,%a)") print_term term print_pty pty
    | Ttuple term_list -> fprintf fmt (ast_or_mlw "Ttuple(%a)" "Ttuple(%a)") (Pp.print_list Pp.comma print_term) term_list
    | Trecord qualid_term_list -> fprintf fmt (ast_or_mlw "Trecord(%a)" "Trecord(%a)") (Pp.print_list Pp.comma (Pp.print_pair print_qualid print_term)) qualid_term_list
    | Tupdate(term,qualid_term_list) -> fprintf fmt (ast_or_mlw "Tupdate(%a,%a)" "Tupdate(%a,%a)") print_term term (Pp.print_list Pp.comma (Pp.print_pair print_qualid print_term)) qualid_term_list
    | Tscope(qualid,term) -> fprintf fmt (ast_or_mlw "Tscope(%a,%a)" "Tscope(%a,%a)") print_qualid qualid print_term term
    | Tat(term,ident) -> fprintf fmt (ast_or_mlw "Tat(%a,%a)" "Tat(%a,%a)") print_term term print_ident_str ident

  (*s Program expressions *)

  let print_invariant fmt i =
    fprintf fmt (ast_or_mlw "invariant(%a)" "invariant(%a)") (Pp.print_list Pp.comma print_term) i

  let print_variant fmt v =
    fprintf fmt (ast_or_mlw "variant(%a)" "variant(%a)") (Pp.print_list Pp.comma (Pp.print_pair print_term (Pp.print_option print_qualid))) v

  let print_pre fmt p =
    fprintf fmt (ast_or_mlw "pre(%a)" "@[requires { %a }@]") print_term p

  let print_post_case fmt (pat,t) =
    fprintf fmt "@[returns { %a ->@ %a }@]" print_pattern pat print_term t

  let print_post fmt (location,post_cases) =
    match pp_kind with
    | Ast -> fprintf fmt "post(%a)" (Pp.print_pair print_loc (Pp.print_list Pp.comma (Pp.print_pair print_pattern print_term))) (location,post_cases)
    | Mlw -> fprintf fmt "@[%a@]" (Pp.print_list Pp.newline print_post_case) post_cases

  let print_xpost fmt xp =
    fprintf fmt (ast_or_mlw "xpost(%a)" "xpost(%a)") (Pp.print_pair print_loc (Pp.print_list Pp.comma (Pp.print_pair print_qualid (Pp.print_option (Pp.print_pair print_pattern print_term))))) xp

  let print_spec fmt s =
    match pp_kind with
    | Ast -> fprintf fmt "spec(sp_pre:%a,sp_post:%a,sp_xpost:%a,sp_reads:%a,sp_writes:%a,sp_alias:%a,sp_variant:%a,sp_checkrw:%B,sp_diverge:%B,sp_partial:%B)" (Pp.print_list Pp.comma print_pre) s.sp_pre (Pp.print_list Pp.comma print_post) s.sp_post (Pp.print_list Pp.comma print_xpost) s.sp_xpost (Pp.print_list Pp.comma print_qualid) s.sp_reads (Pp.print_list Pp.comma print_term) s.sp_writes (Pp.print_list Pp.comma (Pp.print_pair print_term print_term)) s.sp_alias print_variant s.sp_variant s.sp_checkrw s.sp_diverge s.sp_partial
    | Mlw -> fprintf fmt "@[%a@\n%a@]"
               (Pp.print_list Pp.newline print_pre) s.sp_pre
               (Pp.print_list Pp.newline print_post) s.sp_post

  let rec print_expr fmt e =
    match pp_kind with
    | Ast -> fprintf fmt "expr(expr_desc:%a,expr_loc:%a)" print_expr_desc e.expr_desc print_loc e.expr_loc
    | Mlw -> fprintf fmt "%a" print_expr_desc e.expr_desc

  and print_expr_desc fmt ed =
    match ed with
    | Eref -> fprintf fmt (ast_or_mlw "Eref" "Eref")
    | Etrue -> fprintf fmt (ast_or_mlw "Etrue" "Etrue")
    | Efalse -> fprintf fmt (ast_or_mlw "Efalse" "Efalse")
    | Econst constant -> fprintf fmt (ast_or_mlw "Econst(%a)" "%a") Number.print_constant constant
    (* lambda-calculus *)
    | Eident qualid -> fprintf fmt (ast_or_mlw "Eident(%a)" "%a") print_qualid qualid
    | Easref qualid -> fprintf fmt (ast_or_mlw "Easref(%a)" "Easref(%a)") print_qualid qualid
    | Eidapp(qualid,expr_list) -> fprintf fmt (ast_or_mlw "Eidapp(%a,%a)" "@[(%a@ %a)@]") print_qualid qualid (Pp.print_list Pp.space print_expr) expr_list
    | Eapply(expr1,expr2) -> fprintf fmt (ast_or_mlw "Eapply(%a,%a)" "Eapply(%a,%a)") print_expr expr1 print_expr expr2
    | Einfix(expr1,ident,expr2) -> fprintf fmt (ast_or_mlw "Einfix(%a,%a,%a)" "Einfix(%a,%a,%a)") print_expr expr1 print_ident_str ident print_expr expr2
    | Einnfix(expr1,ident,expr2) -> fprintf fmt (ast_or_mlw "Einnfix(%a,%a,%a)" "Einnfix(%a,%a,%a)") print_expr expr1 print_ident_str ident print_expr expr2
    | Elet(ident,ghost,rs_kind,expr1,expr2) ->
      (
        match pp_kind with
        | Ast -> fprintf fmt "Elet(%a,%a,%a,%a,%a)" print_ident_str ident print_ghost ghost print_rs_kind rs_kind print_expr expr1 print_expr expr2
        | Mlw -> fprintf fmt "@[<hv 0>@[let %a%a =@ %a in@]@ @[%a@]@]" print_ghost ghost print_ident_str ident print_expr expr1 print_expr expr2
      )
    | Erec(fundef_list,expr) -> fprintf fmt (ast_or_mlw "Erec(%a,%a)" "Erec(%a,%a)") (Pp.print_list Pp.comma print_fundef) fundef_list print_expr expr
    | Efun(binder_list,pty_option,pattern,mask,spec,expr) -> fprintf fmt (ast_or_mlw "Efun(%a,%a,%a,%a,%a)" "Efun(%a,%a,%a,%a,%a)") (Pp.print_list Pp.comma (print_binder ~paren:false)) binder_list (Pp.print_option print_pty) pty_option print_mask mask print_spec spec print_expr expr
    | Eany(param_list,rs_kind,pty_option,pattern,mask,spec) -> fprintf fmt (ast_or_mlw "Eany(%a,%a,%a,%a,%a)" "Eany(%a,%a,%a,%a,%a)") (Pp.print_list Pp.comma print_param) param_list print_rs_kind rs_kind (Pp.print_option print_pty) pty_option print_mask mask print_spec spec
    | Etuple(expr_list) -> fprintf fmt (ast_or_mlw "Etuple(%a)" "(%a)") (Pp.print_list Pp.comma print_expr) expr_list
    | Erecord qualid_expr_list -> fprintf fmt (ast_or_mlw "Erecord(%a)" "Erecord(%a)") (Pp.print_list Pp.comma (Pp.print_pair print_qualid print_expr)) qualid_expr_list
    | Eupdate(expr,qualid_expr_list) -> fprintf fmt (ast_or_mlw "Eupdate(%a,%a)" "Eupdate(%a,%a)") print_expr expr (Pp.print_list Pp.comma (Pp.print_pair print_qualid print_expr)) qualid_expr_list
    | Eassign expr_qualid_option_expr_list -> fprintf fmt (ast_or_mlw "Eassign(%a)" "Eassign(%a)") (Pp.print_list Pp.comma (print_3uple print_expr (Pp.print_option print_qualid) print_expr)) expr_qualid_option_expr_list
    (* control *)
    | Esequence(expr1,expr2) -> fprintf fmt (ast_or_mlw "Esequence(%a,%a)" "@[%a;@ %a@]") print_expr expr1 print_expr expr2
    | Eif(expr_if,expr_then,expr_else) -> fprintf fmt (ast_or_mlw "Eif(%a,%a,%a)" "@[<hv 2>(if %a@ then %a@ else %a)@]") print_expr expr_if print_expr expr_then print_expr expr_else
    | Ewhile(expr1,invariant,variant,expr2) -> fprintf fmt (ast_or_mlw "Ewhile(%a,%a,%a,%a)" "Ewhile(%a,%a,%a,%a)") print_expr expr1 print_invariant invariant print_variant variant print_expr expr2
    | Eand(expr1,expr2) -> fprintf fmt (ast_or_mlw "Eand(%a,%a)" "Eand(%a,%a)") print_expr expr1 print_expr expr2
    | Eor(expr1,expr2) -> fprintf fmt (ast_or_mlw "Eor(%a,%a)" "Eor(%a,%a)") print_expr expr1 print_expr expr2
    | Enot expr -> fprintf fmt (ast_or_mlw "Enot(%a)" "Enot(%a)") print_expr expr
    | Ematch(expr,reg_branch_list,exn_branch_list) -> fprintf fmt (ast_or_mlw "Ematch(%a,%a,%a)" "Ematch(%a,%a,%a)") print_expr expr (Pp.print_list Pp.comma print_reg_branch) reg_branch_list (Pp.print_list Pp.comma print_exn_branch) exn_branch_list
    | Eabsurd -> fprintf fmt (ast_or_mlw "Eabsurd" "Eabsurd")
    | Epure term -> fprintf fmt (ast_or_mlw "Epure(%a)" "Epure(%a)") print_term term
    | Eidpur qualid -> fprintf fmt (ast_or_mlw "Eidpur(%a)" "Eidpur(%a)") print_qualid qualid
    | Eraise(qualid,expr_option) -> fprintf fmt (ast_or_mlw "Eraise(%a,%a)" "Eraise(%a,%a)") print_qualid qualid (Pp.print_option print_expr) expr_option
    | Eexn(ident,pty,mask,expr) -> fprintf fmt (ast_or_mlw "Eexn(%a,%a,%a,%a)" "Eexn(%a,%a,%a,%a)") print_ident_str ident print_pty pty print_mask mask print_expr expr
    | Eoptexn(ident,mask,expr) -> fprintf fmt (ast_or_mlw "Eoptexn(%a,%a,%a)" "Eoptexn(%a,%a,%a)") print_ident_str ident print_mask mask print_expr expr
    | Efor(ident,expr1,for_direction,expr2,invariant,expr3) -> fprintf fmt (ast_or_mlw "Efor(%a,%a,%a,%a,%a,%a)" "Efor(%a,%a,%a,%a,%a,%a)") print_ident_str ident print_expr expr1 print_for_direction for_direction print_expr expr2 print_invariant invariant print_expr expr3
    (* annotations *)
    | Eassert(assertion_kind,term) ->
      (
        match pp_kind with
        | Ast -> fprintf fmt "Eassert(%a,%a)" print_assertion_kind assertion_kind print_term term
        | Mlw -> fprintf fmt "%a {%a}" print_assertion_kind assertion_kind print_term term
      )
    | Escope(qualid,expr) -> fprintf fmt (ast_or_mlw "Escope(%a,%a)" "Escope(%a,%a)") print_qualid qualid print_expr expr
    | Elabel(ident,expr) -> fprintf fmt (ast_or_mlw "Elabel(%a,%a)" "Elabel(%a,%a)") print_ident_str ident print_expr expr
    | Ecast(expr,pty) -> fprintf fmt (ast_or_mlw "Ecast(%a,%a)" "%a : %a") print_expr expr print_pty pty
    | Eghost expr -> fprintf fmt (ast_or_mlw "Eghost(%a)" "Eghost(%a)") print_expr expr
    | Eattr(attr,expr) -> fprintf fmt (ast_or_mlw "Eattre(%a,%a)" "Eattre(%a,%a)") print_attr attr print_expr expr

  and print_reg_branch fmt (pattern,expr) =
    fprintf fmt (ast_or_mlw "reg_branch(%a,%a)" "reg_branch(%a,%a)") print_pattern pattern print_expr expr

  and print_exn_branch fmt (qualid,pattern_option,expr) =
    fprintf fmt (ast_or_mlw "exn_branch(%a,%a,%a)" "exn_branch(%a,%a,%a)") print_qualid qualid (Pp.print_option print_pattern) pattern_option print_expr expr

  and print_fundef fmt (ident,ghost,rs_kind,binder_list,pty_option,pattern,mask,spec,expr) =
    match pp_kind with
    | Ast -> fprintf fmt "fundef(%a,%a,%a,%a,%a,%a,%a,%a)" print_ident_str ident print_ghost ghost print_rs_kind rs_kind (Pp.print_list Pp.comma (print_binder ~paren:false)) binder_list (Pp.print_option print_pty) pty_option print_mask mask print_spec spec print_expr expr
    | Mlw -> fprintf fmt "%a%a @[%a@]: %a@\n%a@\n=@ %a" print_ghost ghost print_ident_str ident (Pp.print_list Pp.space (print_binder ~paren:true)) binder_list (Pp.print_option print_pty) pty_option print_spec spec print_expr expr

  let print_field fmt f =
    fprintf fmt (ast_or_mlw "field(f_loc:%a,f_ident:%a,f_pty:%a,f_mutable:%B,f_ghost:%B)" "field(f_loc:%a,f_ident:%a,f_pty:%a,f_mutable:%B,f_ghost:%B)") print_loc f.f_loc print_ident_str f.f_ident print_pty f.f_pty f.f_mutable f.f_ghost

  let print_type_def fmt tdef =
    match tdef with
    | TDalias    pty -> fprintf fmt (ast_or_mlw "TDalias(%a)" "TDalias(%a)") print_pty pty
    | TDalgebraic list_of_position_ident_param_list -> fprintf fmt (ast_or_mlw "TDalgebraic(%a)" "TDalgebraic(%a)") (Pp.print_list Pp.comma (print_3uple print_loc print_ident_str (Pp.print_list Pp.comma print_param))) list_of_position_ident_param_list
    | TDrecord   field_list -> fprintf fmt (ast_or_mlw "TDrecord(%a)" "TDrecord(%a)") (Pp.print_list Pp.comma print_field) field_list
    | TDrange    (bigint_t1,bigint_t2) -> fprintf fmt (ast_or_mlw "TDrange[%a-%a]" "TDrange[%a-%a]") print_big_int bigint_t1 print_big_int bigint_t2
    | TDfloat    (i1,i2) -> fprintf fmt (ast_or_mlw "TDfloat[%d,%d]" "TDfloat[%d,%d]") i1 i2

  let print_visibility fmt v =
    match v with
    | Public -> fprintf fmt (ast_or_mlw "Public" "Public")
    | Private -> fprintf fmt (ast_or_mlw "Private" "Private")
    | Abstract -> fprintf fmt (ast_or_mlw "Abstract" "Abstract")

  let print_type_decl fmt td =
    fprintf fmt (ast_or_mlw "type_decl(td_loc:%a,td_ident:%a,td_params:%a,td_vis:%a,td_mut:%B,td_inv:%a,td_wit:%a,td_def:%a)" "type_decl(td_loc:%a,td_ident:%a,td_params:%a,td_vis:%a,td_mut:%B,td_inv:%a,td_wit:%a,td_def:%a)") print_loc td.td_loc print_ident_str td.td_ident (Pp.print_list Pp.comma print_ident_str) td.td_params print_visibility td.td_vis td.td_mut print_invariant td.td_inv (Pp.print_list Pp.comma (Pp.print_pair print_qualid print_expr)) td.td_wit print_type_def td.td_def

  let print_logic_decl fmt ld =
    match pp_kind with
    | Ast -> fprintf fmt "logic_decl(ld_loc:%a,ld_ident:%a,ld_params:%a,ld_type:%a,ld_def:%a)" print_loc ld.ld_loc print_ident_str ld.ld_ident (Pp.print_list Pp.comma print_param) ld.ld_params (Pp.print_option print_pty) ld.ld_type (Pp.print_option print_term) ld.ld_def
    | Mlw -> match ld.ld_type with
      | Some ld_type -> fprintf fmt "@[<h 0>function %a %a : %a = %a @]" print_ident_str ld.ld_ident (Pp.print_list Pp.space print_param) ld.ld_params print_pty ld_type  (Pp.print_option print_term) ld.ld_def
      | None -> fprintf fmt "@[<h 0>predicate %a %a = %a @]" print_ident_str ld.ld_ident (Pp.print_list Pp.space print_param) ld.ld_params (Pp.print_option print_term) ld.ld_def

  let print_ind_decl fmt id =
    fprintf fmt (ast_or_mlw "ind_decl(in_loc:%a,in_ident:%a,in_params:%a,in_def:%a)" "ind_decl(in_loc:%a,in_ident:%a,in_params:%a,in_def:%a)") print_loc id.in_loc print_ident_str id.in_ident (Pp.print_list Pp.comma print_param) id.in_params (Pp.print_list Pp.comma (print_3uple print_loc print_ident_str print_term)) id.in_def

  let print_metarg fmt m =
    match m with
    | Mty  pty -> fprintf fmt (ast_or_mlw "Mty(%a)" "Mty(%a)") print_pty pty
    | Mfs  qualid -> fprintf fmt (ast_or_mlw "Mfs(%a)" "Mfs(%a)") print_qualid qualid
    | Mps  qualid -> fprintf fmt (ast_or_mlw "Mps(%a)" "Mps(%a)") print_qualid qualid
    | Max  qualid -> fprintf fmt (ast_or_mlw "Max(%a)" "Max(%a)") print_qualid qualid
    | Mlm  qualid -> fprintf fmt (ast_or_mlw "Mlm(%a)" "Mlm(%a)") print_qualid qualid
    | Mgl  qualid -> fprintf fmt (ast_or_mlw "Mgl(%a)" "Mgl(%a)") print_qualid qualid
    | Mval qualid [@if WHY3VERSION >= 130] -> fprintf fmt (ast_or_mlw "Mval(%a)" "Mval(%a)") print_qualid qualid
    | Mstr str -> fprintf fmt (ast_or_mlw "Mstr(%s)" "Mstr(%s)") str
    | Mint i -> fprintf fmt (ast_or_mlw "Mint(%d)" "Mint(%d)") i

  let print_clone_subst fmt cs =
    match cs with
    | CStsym(qualid,ident_list,pty) -> fprintf fmt (ast_or_mlw "CStsym(%a,%a,%a)" "CStsym(%a,%a,%a)") print_qualid qualid (Pp.print_list Pp.space print_ident_str) ident_list print_pty pty
    | CSfsym(qualid1,qualid2) -> fprintf fmt (ast_or_mlw "CSfsym(%a,%a)" "CSfsym(%a,%a)") print_qualid qualid1 print_qualid qualid2
    | CSpsym(qualid1,qualid2) -> fprintf fmt (ast_or_mlw "CSpsym(%a,%a)" "CSpsym(%a,%a)") print_qualid qualid1 print_qualid qualid2
    | CSvsym(qualid1,qualid2) -> fprintf fmt (ast_or_mlw "CSvsym(%a,%a)" "CSvsym(%a,%a)") print_qualid qualid1 print_qualid qualid2
    | CSxsym(qualid1,qualid2) -> fprintf fmt (ast_or_mlw "CSxsym(%a,%a)" "CSxsym(%a,%a)") print_qualid qualid1 print_qualid qualid2
    | CSprop  prop_kind -> fprintf fmt (ast_or_mlw "CSprop(%a)" "CSprop(%a)") print_prop_kind prop_kind
    | CSaxiom qualid -> fprintf fmt (ast_or_mlw "CSaxiom(%a)" "CSaxiom(%a)") print_qualid qualid
    | CSlemma qualid -> fprintf fmt (ast_or_mlw "CSlemma(%a)" "CSlemma(%a)") print_qualid qualid
    | CSgoal  qualid -> fprintf fmt (ast_or_mlw "CSgoal(%a)" "CSgoal(%a)") print_qualid qualid

  let print_import fmt import =
    match pp_kind with
    | Ast -> fprintf fmt "import(%B)" import
    | Mlw -> if import then fprintf fmt "import"

  let [@warning "-39"] rec print_decl fmt d =
    match d with
    | Dtype type_decl_list -> fprintf fmt (ast_or_mlw "Dtype(%a)" "Dtype(%a)") (Pp.print_list Pp.comma print_type_decl) type_decl_list
    | Dlogic logic_decl_list -> fprintf fmt (ast_or_mlw "Dlogic(%a)" "@[<h 0>%a@]") (Pp.print_list Pp.comma print_logic_decl) logic_decl_list
    | Dind(ind_sign,ind_decl_list) -> fprintf fmt (ast_or_mlw "Dind(%a,%a)" "Dind(%a,%a)") print_ind_sign ind_sign (Pp.print_list Pp.comma print_ind_decl) ind_decl_list
    | Dprop(prop_kind,ident,term) -> fprintf fmt (ast_or_mlw "Dprop(%a,%a,%a)" "@[<hov 2>%a %a:@ %a@]") print_prop_kind prop_kind print_ident_str ident print_term term
    | Dlet(ident,ghost,rs_kind,expr) ->
      (
        match pp_kind with
        | Ast -> fprintf fmt "Dlet(%a,%a,%a,%a)" print_ident_str ident print_ghost ghost print_rs_kind rs_kind print_expr expr
        | Mlw -> fprintf fmt "@[let@ %a@ %a@ =%a@]" print_ident_str ident print_ghost ghost print_expr expr
      )
    | Drec [] -> assert false
    | Drec (fundef_list_hd::fundef_list_tl) ->
      (
        match pp_kind with
        | Ast -> fprintf fmt "Drec(%a)" (Pp.print_list Pp.comma print_fundef) (fundef_list_hd::fundef_list_tl)
        | Mlw -> fprintf fmt "@[<v 0>let rec %a%a@]"
                   print_fundef fundef_list_hd
                   (Pp.print_list_pre (Pp.constant_formatted "@\nwith ") print_fundef) fundef_list_tl
      )
    | Dexn(ident,pty,mask)-> fprintf fmt (ast_or_mlw "Dexn(%a,%a,%a)" "Dexn(%a,%a,%a)") print_ident_str ident print_pty pty print_mask mask
    | Dmeta(ident,metarg_list) -> fprintf fmt (ast_or_mlw "Dmeta(%a,%a)" "Dmeta(%a,%a)") print_ident_str ident (Pp.print_list Pp.comma print_metarg) metarg_list
    (* | Dclone(qualid,clone_subst_list) [@if WHY3VERSION <= 120] -> fprintf fmt (ast_or_mlw "Dclone(%a,%a)" "Dclone(%a,%a)") print_qualid qualid (Pp.print_list Pp.comma print_clone_subst) clone_subst_list
       | Duse qualid [@if WHY3VERSION <= 120] -> fprintf fmt (ast_or_mlw "Duse(%a)" "@[<hov 2>use %a@]") print_qualid qualid *)
    | Dcloneexport(qualid,clone_subst_list) [@if WHY3VERSION >= 130] -> fprintf fmt (ast_or_mlw "Dcloneexport(%a,%a)" "Dcloneexport(%a,%a)") print_qualid qualid (Pp.print_list Pp.comma print_clone_subst) clone_subst_list
    | Duseexport qualid [@if WHY3VERSION >= 130] -> fprintf fmt (ast_or_mlw "Duseexport(%a)" "@[<hov 2>use export %a@]") print_qualid qualid
    | Dcloneimport(loc,import,qualid,id_opt,clone_subst_l) [@if WHY3VERSION >= 130] ->
      (
        match pp_kind with
        | Ast -> fprintf fmt "Dcloneimport(%a,%a,%a,%a,%a)" print_loc loc print_import import print_qualid qualid (Pp.print_option print_ident) id_opt (Pp.print_list Pp.comma print_clone_subst) clone_subst_l
        | Mlw -> fprintf fmt "Dcloneimport(%a,%a,%a,%a,%a)" print_loc loc print_import import print_qualid qualid (Pp.print_option print_ident) id_opt (Pp.print_list Pp.comma print_clone_subst) clone_subst_l
      )
    | Duseimport(loc,import,qualid_id_opt_l) [@if WHY3VERSION >= 130] ->
      (
        match pp_kind with
        | Ast -> fprintf fmt "Duseimport(%a,%a,%a)" print_loc loc print_import import (Pp.print_list Pp.comma (Pp.print_pair print_qualid (Pp.print_option print_ident))) qualid_id_opt_l
        | Mlw -> fprintf fmt "@[use %a [#%a] %a@]" print_import import print_loc loc (Pp.print_list Pp.comma ((Pp.print_pair_delim Pp.nothing Pp.nothing Pp.nothing) print_qualid (Pp.print_option print_ident))) qualid_id_opt_l
      )
    | Dimport(qualid) [@if WHY3VERSION >= 130] -> fprintf fmt (ast_or_mlw "Dimport(%a)" "@[<hov 2>import %a@]") print_qualid qualid
    | Dscope(loc,import,id,decls) [@if WHY3VERSION >= 130] ->
      (
        match pp_kind with
        | Ast -> fprintf fmt "Dscope(%a,%a,%a,%a)" print_loc loc print_import import print_ident id (Pp.print_list Pp.newline print_decl) decls
        | Mlw -> fprintf fmt "@[scope %a@\n@[<v 2>  %a@]@\nend@]" print_ident id (Pp.print_list Pp.newline print_decl) decls
      )

  (*s Modules *)
  let print_module fmt (ident,decl_list) =
    match pp_kind with
    | Ast -> fprintf fmt "module(%a,%a)" print_ident_str ident (Pp.print_list Pp.comma print_decl) decl_list
    | Mlw ->
      let id = Ident.string_unique uprinter (usanitize ident.id_str) in
      fprintf fmt "@[<v 0>module %s [@@%s]@\n@[<v 2>  %a@]@\nend@]" id ident.id_str (Pp.print_list Pp.newline print_decl) decl_list

  let print_modules fmt modules_list =
    Ident.forget_all lprinter;
    Ident.forget_all uprinter;
    fprintf fmt "%a" (Pp.print_list Pp.newline print_module) modules_list

  (* [%% if WHY3VERSION >= 130 ] *)
  let print_mlw_file fmt mlw_file =
    match mlw_file with
    | Modules modules_list -> fprintf fmt "%a" print_modules modules_list
    | Decls decls -> fprintf fmt "@[%a@]" (Pp.print_list Pp.newline print_decl) decls
    (* [%% endif ] *)
end

module Output_ast = Output(struct let pp_kind = Ast and pp_loc = false end)
module Output_mlw = Output(struct let pp_kind = Mlw and pp_loc = false  end)

let print (fname, channel : string * in_channel) k =
  let lb = Lexing.from_channel channel in
  Loc.set_file fname lb;
  let mlw_file = Lexer.parse_mlw_file lb in
  let f =
    match k with
    | `Ast -> Output_ast.print_mlw_file
    | `Mlw -> Output_mlw.print_mlw_file
  in
  Format.printf "%a@." f mlw_file
