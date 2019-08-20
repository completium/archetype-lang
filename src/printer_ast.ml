open Tools
open Ast
open Printer_tools

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_currency fmt = function
  | Tez   -> Format.fprintf fmt "tz"
  | Mutez -> Format.fprintf fmt "mtz"

let pp_vtyp fmt = function
  | VTbool       -> Format.fprintf fmt "bool"
  | VTint        -> Format.fprintf fmt "int"
  | VTrational   -> Format.fprintf fmt "rational"
  | VTdate       -> Format.fprintf fmt "date"
  | VTduration   -> Format.fprintf fmt "duration"
  | VTstring     -> Format.fprintf fmt "string"
  | VTaddress    -> Format.fprintf fmt "address"
  | VTrole       -> Format.fprintf fmt "role"
  | VTcurrency c -> pp_currency fmt c
  | VTkey        -> Format.fprintf fmt "key"

let pp_container fmt = function
  | Collection -> Format.fprintf fmt "collection"
  | Partition  -> Format.fprintf fmt "partition"
  | Subset     -> Format.fprintf fmt "subset"

let rec pp_ptyp fmt (t : ptyp) =
  match t with
  | Tasset an ->
    Format.fprintf fmt "%a" pp_id an
  | Tenum en ->
    Format.fprintf fmt "%a" pp_id en
  | Tcontract cn ->
    Format.fprintf fmt "%a" pp_id cn
  | Tbuiltin b -> pp_vtyp fmt b
  | Tcontainer (t, c) ->
    Format.fprintf fmt "%a %a"
      pp_ptyp t
      pp_container c
  | Toption t ->
    Format.fprintf fmt "%a option"
      pp_type_ t
  | Ttuple ts ->
    Format.fprintf fmt "%a"
      (pp_list " * " pp_type_) ts
  | Tentry ->
    Format.fprintf fmt "entry"
  | Ttrace t ->
    Format.fprintf fmt "%a"
      pp_trtyp t

let pp_bval fmt (bval : bval) =
  let pp_node fmt = function
    | BVint v           -> pp_big_int fmt v
    | BVuint v          -> pp_big_int fmt v
    | BVbool v          -> pp_str fmt (if v then "true" else "false")
    | BVenum v          -> pp_str fmt v
    | BVrational (n, d) -> Format.fprintf fmt "(%a div %a)" pp_big_int n pp_big_int d
    | BVdate v          -> pp_str fmt v
    | BVstring s        -> pp_str fmt s
    | BVcurrency (c, v) -> Format.fprintf fmt "%a %a" pp_big_int v pp_currency c
    | BVaddress v       -> pp_str fmt v
    | BVduration v      -> pp_str fmt v
  in
  pp_struct_poly pp_node fmt bval

let pp_logical_operator fmt = function
  | And   -> pp_str fmt "and"
  | Or    -> pp_str fmt "or"
  | Imply -> pp_str fmt "->"
  | Equiv -> pp_str fmt "<->"

let pp_comparison_operator fmt = function
  | Equal  -> pp_str fmt "="
  | Nequal -> pp_str fmt "<>"
  | Gt     -> pp_str fmt ">"
  | Ge     -> pp_str fmt ">="
  | Lt     -> pp_str fmt "<"
  | Le     -> pp_str fmt "<="

let pp_arithmetic_operator fmt = function
  | Plus   -> pp_str fmt "+"
  | Minus  -> pp_str fmt "-"
  | Mult   -> pp_str fmt "*"
  | Div    -> pp_str fmt "/"
  | Modulo -> pp_str fmt "%"

let pp_unary_arithmetic_operator fmt = function
  | Uplus  -> pp_str fmt "+"
  | Uminus -> pp_str fmt "-"

let pp_assignment_operator fmt = function
  | ValueAssign -> pp_str fmt ":="
  | PlusAssign  -> pp_str fmt "+="
  | MinusAssign -> pp_str fmt "-="
  | MultAssign  -> pp_str fmt "*="
  | DivAssign   -> pp_str fmt "/="
  | AndAssign   -> pp_str fmt "&="
  | OrAssign    -> pp_str fmt "|="

let pp_operator fmt = function
  | `Logical op -> pp_logical_operator fmt op
  | `Cmp     op -> pp_comparison_operator fmt op
  | `Arith   op -> pp_arithmetic_operator fmt op
  | `Unary   op -> pp_unary_arithmetic_operator fmt op
  | `Assign  op -> pp_assignment_operator fmt op

let pp_struct_poly pp_node fmt (s : 'a struct_poly) =
  if !Options.opt_typed then
    Format.fprintf fmt "(%a : %a)"
      pp_node s.node
      (pp_option pp_ptyp) s.type_
  else
    pp_node fmt s.node

let rec pp_qualid fmt (q : qualid) =
  let pp_node fmt = function
    | Qident i ->
      pp_id fmt i
    | Qdot (q, i) ->
      Format.fprintf fmt "%a.%a"
        pp_qualid q
        pp_id i
  in
  pp_struct_poly pp_node fmt q

let pp_quantifier fmt = function
  | Forall -> pp_str fmt "forall"
  | Exists -> pp_str fmt "exists"

let pp_pattern fmt (p : pattern) =
  let pp_node fmt = function
    | Mconst c -> pp_id fmt c
    | Mwild    -> pp_str fmt "_"
  in
  pp_struct_poly pp_node fmt p

let to_const = function
  | Cstate        -> "state"
  | Cnow          -> "now"
  | Ctransferred  -> "transferred"
  | Ccaller       -> "caller"
  | Cfail         -> "fail"
  | Cbalance      -> "balance"
  | Cconditions   -> "conditions"
  | Cactions      -> "actions"
  | Cnone         -> "none"
  | Cany          -> "any"
  | Canyaction    -> "anyaction"
  | Cisempty      -> "is_empty"
  | Cget          -> "get"
  | Cadd          -> "add"
  | Caddnofail    -> "addnofail"
  | Cremove       -> "remove"
  | Cremovenofail -> "removenofail"
  | Cremoveif     -> "removeif"
  | Cupdate       -> "update"
  | Cupdatenofail -> "updatenofail"
  | Cclear        -> "clear"
  | Ccontains     -> "contains"
  | Cnth          -> "nth"
  | Creverse      -> "reverse"
  | Cselect       -> "select"
  | Csort         -> "sort"
  | Ccount        -> "count"
  | Csum          -> "sum"
  | Cmax          -> "max"
  | Cmin          -> "min"
  | Csubset       -> "subset"
  | Cbefore       -> "before"
  | Cunmoved      -> "unmoved"
  | Cadded        -> "added"
  | Cremoved      -> "removed"
  | Citerated     -> "iterated"
  | Ctoiterate    -> "toiterate"
  | Cmaybeperformedonlybyrole   -> "may be performed only by role"
  | Cmaybeperformedonlybyaction -> "may be performed only by action"
  | Cmaybeperformedbyrole       -> "may be performed by role"
  | Cmaybeperformedbyaction     -> "may be performed by action"

let pp_call_kind fmt = function
  | Cid id -> pp_id fmt id
  | Cconst c -> pp_str fmt (to_const c)

let rec pp_pterm fmt (pterm : pterm) =
  let pp_node fmt = function
    | Pquantifer (q, i, t, b) ->
      Format.fprintf fmt "%a (%a : %a), %a"
        pp_quantifier q
        pp_id i
        pp_ptyp t
        pp_pterm b

    | Pif (c, t, e) ->
      Format.fprintf fmt "if %a@\nthen @[%a@]@\nelse @[%a@]"
        pp_pterm c
        pp_pterm t
        pp_pterm e

    | Pmatchwith (m, ps) ->
      Format.fprintf fmt "match %a with@\n  @[%a@]@\n"
        pp_pterm m
        (pp_list "@\n" (fun fmt (p, i) ->
             Format.fprintf fmt "| %a -> %a"
               pp_pattern p
               pp_pterm i)) ps

    | Pcall (meth, kind, args) ->
      Format.fprintf fmt "%a%a (%a)"
        (pp_option (pp_postfix "." pp_pterm)) meth
        pp_call_kind kind
        (pp_list ", " pp_term_arg) args

    | Plogical (op, lhs, rhs) ->
      Format.fprintf fmt "%a %a %a"
        pp_pterm lhs
        pp_logical_operator op
        pp_pterm rhs

    | Pnot pt ->
      Format.fprintf fmt "not %a"
        pp_pterm pt

    | Pcomp (op, lhs, rhs) ->
      Format.fprintf fmt "%a %a %a"
        pp_pterm lhs
        pp_comparison_operator op
        pp_pterm rhs

    | Parith (op, lhs, rhs) ->
      Format.fprintf fmt "%a %a %a"
        pp_pterm lhs
        pp_arithmetic_operator op
        pp_pterm rhs

    | Puarith (op, p) ->
      Format.fprintf fmt "%a%a"
        pp_unary_arithmetic_operator op
        pp_pterm p

    | Precord l ->
      Format.fprintf fmt "{%a}"
        (pp_list "; " pp_pterm) l

    | Pletin (id, init, t, body) ->
      Format.fprintf fmt "let %a%a= %a in@\n%a"
        pp_id id
        (pp_option (pp_prefix " : " pp_ptyp)) t
        pp_pterm init
        pp_pterm body

    | Pvar id ->
      pp_id fmt id

    | Parray l ->
      Format.fprintf fmt "[%a]"
        (pp_list "; " pp_pterm) l

    | Plit v ->
      pp_bval fmt v

    | Pdot (e, i) ->
      Format.fprintf fmt "%a.%a"
        pp_pterm e
        pp_id i

    | Pconst c ->
      pp_str fmt (to_const c)

    | Ptuple l ->
      Format.fprintf fmt "(%a)"
        (pp_list ", " pp_pterm) l

    | PsecurityActionRole (desc, s) ->
      Format.fprintf fmt "security action role %a %a"
        pp_action_description desc
        (pp_list " " pp_security_role) s

    | PsecurityActionAction (desc, s) ->
      Format.fprintf fmt "security action action %a %a"
        pp_action_description desc
        (pp_list " " pp_security_role) s
  in
  pp_struct_poly pp_node fmt pterm

and pp_term_arg fmt = function
  | AExpr pt -> pp_pterm fmt pt
  | AFun (id, t, pt) ->
    Format.fprintf fmt "(%a : %a) -> %a"
      pp_id id
      pp_ptyp t
      pp_pterm pt

  | AEffect l ->
    (pp_list " " (fun fmt (id, op, pt) ->
         Format.fprintf fmt "%a %a %a"
           pp_id id
           pp_operator op
           pp_pterm pt)) fmt l

let pp_instruction_poly pp fmt i =
  pp fmt i.node

let rec pp_instruction fmt (i : instruction) =
  let pp_node fmt = function
    | Iif (c, t, e) ->
      Format.fprintf fmt "if %a@\nthen @[%a@]@\nelse @[%a@]"
        pp_pterm c
        pp_instruction t
        pp_instruction e

    | Ifor (id, c, body) ->
      Format.fprintf fmt "for (%a in %a)@\n  @[%a@]"
        pp_id id
        pp_pterm c
        pp_instruction body

    | Iletin (id, init, body) ->
      Format.fprintf fmt "let %a = %a in@\n%a"
        pp_id id
        pp_pterm init
        pp_instruction body

    | Iseq l ->
      if List.is_empty l
      then pp_str fmt "()"
      else (pp_list ";@\n" pp_instruction) fmt l

    | Imatchwith (m, ps) ->
      Format.fprintf fmt "match %a with@\n  @[%a@]@\n"
        pp_pterm m
        (pp_list "@\n" (fun fmt (p, i) ->
             Format.fprintf fmt "| %a -> %a"
               pp_pattern p
               pp_instruction i)) ps

    | Iassign (op, id, value) ->
      Format.fprintf fmt "%a %a %a"
        pp_id id
        pp_assignment_operator op
        pp_pterm value

    | Irequire (k, pt) ->
      Format.fprintf fmt "%a (%a)"
        pp_str (if k then "require" else "failwith")
        pp_pterm pt

    | Itransfer (value, back, dest) ->
      Format.fprintf fmt "transfer %a"
        (pp_do_if back pp_str) "back"

    | Ibreak ->
      pp_str fmt "break"

    | Iassert pt ->
      Format.fprintf fmt "assert %a"
        pp_pterm pt

    | Icall (meth, kind, args) ->
      Format.fprintf fmt "%a%a (%a)"
        (pp_option (pp_postfix "." pp_pterm)) meth
        pp_call_kind kind
        (pp_list ", " pp_term_arg) args

    | Ireturn pt ->
      Format.fprintf fmt "return %a"
        pp_pterm pt

    | Ilabel id ->
      Format.fprintf fmt "label %a"
        pp_id id
  in
  pp_instruction_poly pp_node fmt i

let pp_label_term fmt (lt : lident label_term) =
  Format.fprintf fmt "%a%a"
    (pp_option (pp_postfix " : " pp_id)) lt.label
    pp_pterm lt.term

let pp_predicate fmt (p : lident predicate) =
  Format.fprintf fmt "predicate %a (%a) =@\n  @[%a@]"
    pp_id p.name
    (pp_list ", " (fun fmt (id, typ) -> Format.fprintf fmt "%a : %a" pp_id id pp_ptyp typ)) p.args
    pp_pterm p.body

let pp_variable_verif fmt (v : lident variable) =
  let decl = v.decl in
  Format.fprintf fmt "variable %a%a%a"
    pp_id decl.name
    (pp_option (pp_prefix " " pp_ptyp)) decl.typ
    (pp_option (pp_prefix " := " pp_pterm)) decl.default

let pp_definitions fmt (d : lident definition) =
  Format.fprintf fmt "definition %a =@\n  @[{ %a : %a | %a }@]"
    pp_id d.name
    pp_id d.var
    pp_ptyp d.typ
    pp_pterm d.body

let pp_invariant fmt (i : lident invariant) =
  Format.fprintf fmt "invariant of %a@\n  @[%a@]"
    pp_id i.label
    (pp_list "@\n" pp_pterm) i.formulas

let pp_specification fmt (s : lident specification) : unit =
  Format.fprintf fmt "specification %a@\n  @[%a]@\n%a@\n"
    pp_id s.name
    pp_pterm s.formula
    (pp_do_if (not (List.is_empty s.invariants))
       (fun fmt l ->
          Format.fprintf fmt "invariants@\n  @[%a@]"
            (pp_list "@\n" pp_invariant) l)) s.invariants

let pp_assert fmt (s : lident assert_) : unit =
  Format.fprintf fmt "assert %a on %a @\n  @[%a]@\n%a@\n"
    pp_id s.name
    pp_id s.label
    pp_pterm s.formula
    (pp_no_empty_list pp_invariant) s.invariants

let pp_verification fmt (v : lident verification) =
  Format.fprintf fmt "%a%a%a%a%a%a%a%a%a@\n"
    (pp_no_empty_list pp_predicate) v.predicates
    (pp_no_empty_list pp_definitions) v.definitions
    (pp_no_empty_list (fun fmt -> Format.fprintf fmt "axioms:@\n  @[%a@]@\n" pp_label_term)) v.axioms
    (pp_no_empty_list (fun fmt -> Format.fprintf fmt "theorems:@\n  @[%a@]@\n" pp_label_term)) v.theorems
    (pp_no_empty_list pp_variable_verif) v.variables
    (pp_no_empty_list (fun fmt (id, l : lident * lident label_term list) ->
         Format.fprintf fmt "invariants:@\n  @[%a@]@\n"
           (pp_list "@\n" (fun fmt (lt : lident label_term) ->
                Format.fprintf fmt "%a : %a"
                  pp_id id
                  pp_label_term lt
              )) l)) v.invariants
    (pp_option (fun fmt -> Format.fprintf fmt "effect:@\n  @[%a@]@\n" pp_pterm)) v.effect
    (pp_no_empty_list pp_specification) v.specs
    (pp_no_empty_list pp_assert) v.asserts

let pp_variable fmt (v : lident variable) =
  Format.fprintf fmt "%a %a%a%a%a"
    pp_ptyp (Option.get v.decl.typ)
    pp_id v.decl.name
    (pp_option (pp_prefix " from " pp_qualid)) v.from
    (pp_option (pp_prefix " to " pp_qualid)) v.to_
    (pp_option (pp_prefix " := " pp_pterm)) v.decl.default

let pp_field fmt (f : lident decl_gen) =
  Format.fprintf fmt "%a : %a%a"
    pp_id f.name
    (pp_option pp_ptyp) f.typ
    (pp_option (pp_prefix " := " pp_pterm)) f.default

let pp_asset fmt (a : lident asset_struct) =
  Format.fprintf fmt "asset %a%a%a = {@\n  @[%a@]@\n}%a%a%a@\n"
    pp_id a.name
    (pp_option (pp_prefix " identified by " pp_id)) a.key
    (pp_do_if (not (List.is_empty a.sort)) (pp_prefix " sorted by " (pp_list ", " pp_id))) a.sort
    (pp_list "@\n" pp_field) a.fields
    (pp_option (pp_prefix " with states " pp_id)) a.state
    (pp_option (pp_prefix " initialized by " pp_pterm)) a.init
    (pp_do_if (not (List.is_empty a.specs)) (
        fun fmt ->
          Format.fprintf fmt " with {@[%a@]}"
            (pp_list ";@\n" pp_label_term))) a.specs

let pp_enum_item fmt (ei : lident enum_item_struct) =
  Format.fprintf fmt "| %a%a%a"
    pp_id ei.name
    (pp_do_if ei.initial pp_str) " initial"
    (pp_do_if (not (List.is_empty ei.invariants)) (
        fun fmt ->
          Format.fprintf fmt " with {@[%a@]}"
            (pp_list ";@\n" pp_label_term))) ei.invariants

let pp_enum fmt (e : lident enum_struct) =
  Format.fprintf fmt "enum %a =@\n  @[%a@]@\n"
    pp_id e.name
    (pp_list "@\n" pp_enum_item) e.items

let pp_signature fmt (s : lident signature) =
  Format.fprintf fmt "%a: %a"
    pp_id s.name
    (fun fmt x ->
       if List.is_empty x
       then pp_str fmt "()"
       else (pp_list ", " pp_ptyp) fmt x) s.args

let pp_contract fmt (c : lident contract) =
  Format.fprintf fmt "contract %a =@\n  @[%a@]@\n"
    pp_id c.name
    (pp_list "@\n" pp_signature) c.signatures

let rec pp_rexpr fmt (r : rexpr) =
  let pp_node fmt = function
    | Rqualid q -> pp_qualid fmt q
    | Ror (lhs, rhs) ->
      Format.fprintf fmt "%a or %a"
        pp_rexpr lhs
        pp_rexpr rhs
    | Raddress a -> pp_id fmt a
  in
  pp_struct_poly pp_node fmt r

let rec pp_sexpr fmt (s : sexpr) =
  let pp_node fmt = function
    | Sref id -> pp_id fmt id
    | Sor (lhs, rhs) ->
      Format.fprintf fmt "%a or %a"
        pp_sexpr lhs
        pp_sexpr rhs
    | Sany -> pp_str fmt "any"
  in
  pp_struct_poly pp_node fmt s

let pp_transition fmt t =
  Format.fprintf fmt "transition from %a%a =@\n  @[%a@]\n"
    pp_sexpr t.from
    (pp_option (pp_prefix " on " (fun fmt (x, y) -> Format.fprintf fmt "%a.%a" pp_id x pp_id y))) t.on
    (pp_list "@\n" (fun fmt (to_, cond, action) ->
         Format.fprintf fmt "to %a%a@\n%a@\n"
           pp_id to_
           (pp_option (fun fmt x -> (Format.fprintf fmt " when %a" pp_pterm x))) cond
           (pp_option (fun fmt x -> (Format.fprintf fmt "with effect:@\n  @[%a@]" pp_instruction x))) action
       )) t.trs

let pp_function fmt (f : function_) =
  Format.fprintf fmt "function %a (%a) : %a =@\n  @[%a%a@]@\n"
    pp_id f.name
    (pp_list ", " (fun fmt (x : lident decl_gen) ->
         Format.fprintf fmt "%a : %a"
           pp_id x.name
           pp_ptyp (Option.get x.typ)
       )) f.args
    pp_ptyp f.return
    (pp_option pp_verification) f.verification
    pp_instruction f.body

let pp_transaction fmt (t : transaction) =
  Format.fprintf fmt "transaction %a (%a) =@\n  @[%a%a%a%a%a%a%a@]\n"
    pp_id t.name
    (pp_list ", " (fun fmt (x : lident decl_gen) ->
         Format.fprintf fmt "%a : %a"
           pp_id x.name
           pp_ptyp (Option.get x.typ)
       )) t.args
    (pp_option (fun fmt -> Format.fprintf fmt "called by: %a@\n" pp_rexpr)) t.calledby
    (pp_do_if t.accept_transfer (fun fmt _ -> Format.fprintf fmt "accept transfer@\n")) ()
    (pp_option (pp_list "@\n " (fun fmt -> Format.fprintf fmt "requires: %a@\n" pp_label_term))) t.require
    (pp_option (fun fmt x -> Format.fprintf fmt "transition:@\n  @[%a@]@\n" pp_transition x)) t.transition
    (pp_option pp_verification) t.verification
    (pp_list "@\n" pp_function) t.functions
    (pp_option (fun fmt x -> Format.fprintf fmt "effect:@\n  @[%a@]@\n" pp_instruction x)) t.effect


let pp_ast fmt (ast : model) =
  Format.fprintf fmt "%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      %a\
                      %a\
                      %a\
                      @."
    pp_id ast.name
    (pp_list "@\n" pp_variable) ast.variables
    (pp_list "@\n" pp_asset) ast.assets
    (pp_list "@\n" pp_enum) ast.enums
    (pp_list "@\n" pp_contract) ast.contracts
    (pp_no_empty_list pp_function) ast.functions
    (pp_no_empty_list pp_transaction) ast.transactions
    (pp_no_empty_list pp_verification) ast.verifications

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
