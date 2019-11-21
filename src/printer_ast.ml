open Tools
open Ast
open Printer_tools

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_with_paren pp fmt =
  if (!Options.opt_all_parenthesis)
  then Format.fprintf fmt "(@[%a@])" pp
  else pp fmt

let pp_no_paren pp fmt = pp fmt

let pp_currency fmt = function
  | Tz   -> Format.fprintf fmt "tz"
  | Mtz  -> Format.fprintf fmt "mtz"
  | Mutz -> Format.fprintf fmt "mutz"

let pp_vtyp fmt = function
  | VTbool       -> Format.fprintf fmt "bool"
  | VTint        -> Format.fprintf fmt "int"
  | VTrational   -> Format.fprintf fmt "rational"
  | VTdate       -> Format.fprintf fmt "date"
  | VTduration   -> Format.fprintf fmt "duration"
  | VTstring     -> Format.fprintf fmt "string"
  | VTaddress    -> Format.fprintf fmt "address"
  | VTrole       -> Format.fprintf fmt "role"
  | VTcurrency   -> Format.fprintf fmt "tez"
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
      (pp_list " * " pp_ptyp) ts
  | Tentry ->
    Format.fprintf fmt "entry"
  | Ttrace t ->
    Format.fprintf fmt "%a"
      pp_trtyp t

let pp_struct_poly pp_node fmt (s : 'a struct_poly) =
  if !Options.opt_typed then
    Format.fprintf fmt "(%a : %a)"
      pp_node s.node
      (pp_option pp_ptyp) s.type_
  else
    pp_node fmt s.node

let pp_bval fmt (bval : bval) =
  let pp_node fmt = function
    | BVint v           -> pp_big_int fmt v
    | BVuint v          -> pp_big_int fmt v
    | BVbool v          -> pp_str fmt (if v then "true" else "false")
    | BVenum v          -> pp_str fmt v
    | BVrational (n, d) -> Format.fprintf fmt "(%a div %a)" pp_big_int n pp_big_int d
    | BVdate v          -> Core.pp_date fmt v
    | BVstring s        -> pp_str fmt s
    | BVcurrency (c, v) -> Format.fprintf fmt "%a%a" pp_big_int v pp_currency c
    | BVaddress v       -> Format.fprintf fmt "@@%a" pp_str v
    | BVduration v      -> Core.pp_duration_for_printer fmt v
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
  | Cisempty      -> "isempty"
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
  | Csubsetof     -> "subsetof"
  | Chead         -> "head"
  | Ctail         -> "tail"
  | Cbefore       -> "before"
  | Cunmoved      -> "unmoved"
  | Cadded        -> "added"
  | Cremoved      -> "removed"
  | Citerated     -> "iterated"
  | Ctoiterate    -> "toiterate"

let pp_call_kind fmt = function
  | Cid id -> pp_id fmt id
  | Cconst c -> pp_str fmt (to_const c)

let pp_security_role = pp_lident

let pp_action_description fmt = function
  | ADAny -> pp_str fmt "anyaction"
  | ADOp (a, b) -> Format.fprintf fmt "%s (%a)" a pp_id b

let rec pp_pterm fmt (pterm : pterm) =
  let pp_node fmt = function
    | Pquantifer (q, i, (a, t), b) ->
      let pp fmt (q, i, (_a, t), b) =
        Format.fprintf fmt "%a (%a : %a), %a"
          pp_quantifier q
          pp_id i
          pp_ptyp t
          pp_pterm b
      in
      (pp_with_paren pp) fmt (q, i, (a, t), b)

    | Pif (c, t, e) ->
      let pp fmt (c, t, e) =
        Format.fprintf fmt "if %a@\nthen @[%a@]@\nelse @[%a@]"
          pp_pterm c
          pp_pterm t
          pp_pterm e
      in
      (pp_with_paren pp) fmt (c, t, e)

    | Pmatchwith (m, ps) ->
      let pp fmt (m, ps) =
        Format.fprintf fmt "match %a with@\n  @[%a@]@\n"
          pp_pterm m
          (pp_list "@\n" (fun fmt (p, i) ->
               Format.fprintf fmt "| %a -> %a"
                 pp_pattern p
                 pp_pterm i)) ps
      in
      (pp_with_paren pp) fmt (m, ps)

    | Pcall (meth, kind, args) ->
      let pp fmt (meth, kind, args) =
        Format.fprintf fmt "%a%a(%a)"
          (pp_option (pp_postfix "." pp_pterm)) meth
          pp_call_kind kind
          (pp_list ", " pp_term_arg) args
      in
      (pp_with_paren pp) fmt (meth, kind, args)

    | Plogical (op, lhs, rhs) ->
      let pp fmt (op, lhs, rhs) =
        Format.fprintf fmt "%a %a %a"
          pp_pterm lhs
          pp_logical_operator op
          pp_pterm rhs
      in
      (pp_with_paren pp) fmt (op, lhs, rhs)

    | Pnot pt ->
      let pp fmt pt =
        Format.fprintf fmt "not (%a)"
          pp_pterm pt
      in
      (pp_with_paren pp) fmt pt

    | Pmulticomp (e, l) ->
      let pp fmt (e, l) =
        let pp_item fmt (op, e) =
          Format.fprintf fmt "%a %a"
            pp_comparison_operator op
            pp_pterm e
        in
        Format.fprintf fmt "%a %a"
          pp_pterm e
          (pp_list " " pp_item) l
      in
      (pp_with_paren pp) fmt (e, l)

    | Pcomp (op, lhs, rhs) ->
      let pp fmt (op, lhs, rhs) =
        Format.fprintf fmt "%a %a %a"
          pp_pterm lhs
          pp_comparison_operator op
          pp_pterm rhs
      in
      (pp_with_paren pp) fmt (op, lhs, rhs)

    | Parith (op, lhs, rhs) ->
      let pp fmt (op, lhs, rhs) =
        Format.fprintf fmt "%a %a %a"
          pp_pterm lhs
          pp_arithmetic_operator op
          pp_pterm rhs
      in
      (pp_with_paren pp) fmt (op, lhs, rhs)

    | Puarith (op, p) ->
      let pp fmt (op, p) =
        Format.fprintf fmt "%a%a"
          pp_unary_arithmetic_operator op
          pp_pterm p
      in
      (pp_with_paren pp) fmt (op, p)

    | Precord l ->
      let pp fmt l =
        Format.fprintf fmt "{%a}"
          (pp_list "; " pp_pterm) l
      in
      (pp_no_paren pp) fmt l

    | Pletin (id, init, t, body, otherwise) ->
      let pp fmt (id, init, t, body) =
        Format.fprintf fmt "let %a%a= %a in@\n%a%a"
          pp_id id
          (pp_option (pp_prefix " : " pp_ptyp)) t
          pp_pterm init
          pp_pterm body
          (pp_option (fun fmt -> Format.fprintf fmt " otherwise %a" pp_pterm)) otherwise
      in
      (pp_with_paren pp) fmt (id, init, t, body)

    | Pdeclvar (i, t, v) ->
      let pp fmt (i, t, v) =
        Format.fprintf fmt "var %a%a = %a"
          pp_id i
          (pp_option (pp_prefix " : " pp_ptyp)) t
          pp_pterm v
      in
      (pp_with_paren pp) fmt (i, t, v)

    | Pvar (vt, id) ->
      let pp fmt (vt, id) =
        match vt with
        | VTbefore -> Format.fprintf fmt "before.%a" pp_id id
        | VTat lbl -> Format.fprintf fmt "at(%s).%a" lbl pp_id id
        | VTnone   -> pp_id fmt id
      in
      (pp_no_paren pp) fmt (vt, id)

    | Parray l ->
      let pp fmt l =
        Format.fprintf fmt "[%a]"
          (pp_list "; " pp_pterm) l
      in
      (pp_no_paren pp) fmt l

    | Plit v ->
      let pp fmt v =
        pp_bval fmt v
      in
      (pp_no_paren pp) fmt v

    | Pdot (e, i) ->
      let pp fmt (e, i) =
        Format.fprintf fmt "%a.%a"
          pp_pterm e
          pp_id i
      in
      (pp_with_paren pp) fmt (e, i)

    | Pconst c ->
      let pp fmt c =
        pp_str fmt (to_const c)
      in
      (pp_no_paren pp) fmt c

    | Ptuple l ->
      let pp fmt l =
        Format.fprintf fmt "(%a)"
          (pp_list ", " pp_pterm) l
      in
      (pp_no_paren pp) fmt l
  in
  pp_struct_poly pp_node fmt pterm

and pp_term_arg fmt = function
  | AExpr pt -> pp_pterm fmt pt
  | AFun (id, t, pt) ->
    if !Options.opt_typed
    then
      Format.fprintf fmt "(%a : %a) -> %a"
        pp_id id
        pp_ptyp t
        pp_pterm pt
    else
      pp_pterm fmt pt

  | AEffect l ->
    Format.fprintf fmt "{ %a }"
      (pp_list "; " (fun fmt (id, op, pt) ->
           Format.fprintf fmt "%a %a %a"
             pp_id id
             pp_operator op
             pp_pterm pt)) l

  | ASorting (_b, f) ->
    pp_id fmt f

let pp_instruction_poly pp fmt i =
  pp fmt i.node

let rec pp_instruction fmt (i : instruction) =
  let pp_node fmt = function
    | Iif (c, t, {node = Iseq []; _}) ->
      let pp fmt (c, t) =
        Format.fprintf fmt "if %a@\nthen @[%a@]"
          pp_pterm c
          pp_instruction t
      in
      (pp_with_paren pp) fmt (c, t)
    | Iif (c, t, e) ->
      let pp fmt (c, t, e) =
        Format.fprintf fmt "if %a@\nthen @[%a@]@\nelse @[%a@]"
          pp_pterm c
          pp_instruction t
          pp_instruction e
      in
      (pp_with_paren pp) fmt (c, t, e)

    | Ifor (id, c, body) ->
      let pp fmt (id, c, body) =
        Format.fprintf fmt "for %a%a in %a do@\n  @[%a@]@\ndone"
          (fun fmt x -> match x with Some v -> (Format.fprintf fmt ": %a " pp_str v) | _ -> ()) i.label
          pp_id id
          pp_pterm c
          pp_instruction body
      in
      (pp_with_paren pp) fmt (id, c, body)

    | Iiter (id, a, b, body) ->
      let pp fmt (id, a, b, body) =
        Format.fprintf fmt "iter %a%a from %a to %a do@\n  @[%a@]@\ndone"
          (fun fmt x -> match x with Some v -> (Format.fprintf fmt ": %a " pp_str v) | _ -> ()) i.label
          pp_id id
          pp_pterm a
          pp_pterm b
          pp_instruction body
      in
      (pp_with_paren pp) fmt (id, a, b, body)

    | Iletin (id, init, body) ->
      let pp fmt (id, init, body) =
        Format.fprintf fmt "let %a%a = %a in@\n%a"
          pp_id id
          (pp_option (pp_prefix " : " pp_ptyp)) init.type_
          pp_pterm init
          pp_instruction body
      in
      (pp_with_paren pp) fmt (id, init, body)

    | Ideclvar (i, v) ->
      let pp fmt (i, v) =
        Format.fprintf fmt "var %a%a = %a"
          pp_id i
          (pp_option (pp_prefix " : " pp_ptyp)) v.type_
          pp_pterm v
      in
      (pp_with_paren pp) fmt (i, v)

    | Iseq l ->
      let pp fmt l =
        if List.is_empty l
        then pp_str fmt "()"
        else pp_paren (pp_list ";@\n" pp_instruction) fmt l
      in
      (pp_with_paren pp) fmt l

    | Imatchwith (m, ps) ->
      let pp fmt (m, ps) =
        Format.fprintf fmt "match %a with@\n  @[%a@]@\n"
          pp_pterm m
          (pp_list "@\n" (fun fmt (p, i) ->
               Format.fprintf fmt "| %a -> %a"
                 pp_pattern p
                 pp_instruction i)) ps
      in
      (pp_with_paren pp) fmt (m, ps)

    | Iassign (op, `Var id, value) ->
      let pp fmt (op, id, value) =
        Format.fprintf fmt "%a %a %a"
          pp_id id
          pp_assignment_operator op
          pp_pterm value
      in
      (pp_with_paren pp) fmt (op, id, value)

    | Iassign (op, `Field (nm, id), value) ->
      let pp fmt (op, id, value) =
        Format.fprintf fmt "%a.%a %a %a"
          pp_pterm nm pp_id id
          pp_assignment_operator op
          pp_pterm value
      in
      (pp_with_paren pp) fmt (op, id, value)

    | Irequire (k, pt) ->
      let pp fmt (k, pt) =
        Format.fprintf fmt "%a (%a)"
          pp_str (if k then "require" else "failwith")
          pp_pterm pt
      in
      (pp_with_paren pp) fmt (k, pt)

    | Itransfer (value, dest) ->
      let pp fmt (value, dest) =
        Format.fprintf fmt "transfer %a to %a"
          pp_pterm value
          pp_pterm dest
      in
      (pp_with_paren pp) fmt (value, dest)

    | Ibreak ->
      let pp fmt () =
        pp_str fmt "break"
      in
      (pp_with_paren pp) fmt ()

    | Iassert pt ->
      let pp fmt pt =
        Format.fprintf fmt "assert %a"
          pp_pterm pt
      in
      (pp_with_paren pp) fmt pt

    | Icall (meth, kind, args) ->
      let pp fmt (meth, kind, args) =
        Format.fprintf fmt "%a%a(%a)"
          (pp_option (pp_postfix "." pp_pterm)) meth
          pp_call_kind kind
          (pp_list ", " pp_term_arg) args
      in
      (pp_with_paren pp) fmt (meth, kind, args)

    | Ireturn pt ->
      let pp fmt pt =
        Format.fprintf fmt "return %a"
          pp_pterm pt
      in
      (pp_with_paren pp) fmt pt

    | Ilabel id ->
      let pp fmt id =
        Format.fprintf fmt "assert %a" (** TODO: must be label *)
          pp_id id
      in
      (pp_with_paren pp) fmt id
  in
  pp_instruction_poly pp_node fmt i

let pp_label_term fmt (lt : lident label_term) =
  Format.fprintf fmt "%a%a"
    (pp_option (pp_postfix " : " pp_id)) lt.label
    pp_pterm lt.term

let pp_specification fmt (v : lident specification) =
  let empty = List.is_empty v.predicates
              && List.is_empty v.definitions
              && List.is_empty v.lemmas
              && List.is_empty v.theorems
              && List.is_empty v.variables
              && List.is_empty v.invariants
              && List.is_empty v.asserts
              && List.is_empty v.specs
  in
  let pp_predicate fmt (p : lident predicate) =
    Format.fprintf fmt "predicate %a (%a) =@\n  @[%a@]"
      pp_id p.name
      (pp_list ", " (fun fmt (id, typ) -> Format.fprintf fmt "%a : %a" pp_id id pp_ptyp typ)) p.args
      pp_pterm p.body
  in
  let pp_definitions fmt (d : lident definition) =
    Format.fprintf fmt "definition %a =@\n  @[{ %a : %a | %a }@]"
      pp_id d.name
      pp_id d.var
      pp_ptyp d.typ
      pp_pterm d.body
  in
  let pp_variable_spec fmt (v : lident variable) =
    let decl = v.decl in
    Format.fprintf fmt "variable %a%a%a"
      pp_id decl.name
      (pp_option (pp_prefix " " pp_ptyp)) decl.typ
      (pp_option (pp_prefix " := " pp_pterm)) decl.default
  in
  let pp_invariant fmt (i : lident invariant) =
    Format.fprintf fmt "invariant for %a {@\n  @[%a@]@\n}"
      pp_id i.label
      (pp_list ";@\n" pp_pterm) i.formulas
  in
  let pp_invariants fmt is =
    (pp_do_if (match is with | [] -> false | _ -> true) (fun fmt -> Format.fprintf fmt "@\n  @[%a@]" (pp_list "@\n" pp_invariant))) fmt is
  in
  let pp_use fmt u =
    (pp_do_if (match u with | [] -> false | _ -> true) (fun fmt -> Format.fprintf fmt "@\n  @[use: %a;@]" (pp_list "@ " pp_id))) fmt u
  in
  let pp_assert fmt (s : lident assert_) : unit =
    Format.fprintf fmt "assert %a {@\n  @[%a@]%a%a@\n}"
      pp_id s.name
      pp_pterm s.formula
      pp_invariants s.invariants
      pp_use s.uses
  in
  let pp_postcondition fmt (s : lident postcondition) : unit =
    Format.fprintf fmt "postcondition %a {@\n  @[%a@]%a%a@\n}"
      pp_id s.name
      pp_pterm s.formula
      pp_invariants s.invariants
      pp_use s.uses
  in
  if empty
  then ()
  else
    Format.fprintf fmt "specification {@\n  @[%a%a%a%a%a%a%a%a%a@]@\n}@\n"
      (pp_no_empty_list2 pp_predicate) v.predicates
      (pp_no_empty_list2 pp_definitions) v.definitions
      (pp_no_empty_list2 (fun fmt -> Format.fprintf fmt "axioms:@\n  @[%a@]@\n" pp_label_term)) v.lemmas
      (pp_no_empty_list2 (fun fmt -> Format.fprintf fmt "theorems:@\n  @[%a@]@\n" pp_label_term)) v.theorems
      (pp_no_empty_list2 pp_variable_spec) v.variables
      (pp_no_empty_list2 (fun fmt (id, l : lident * lident label_term list) ->
           Format.fprintf fmt "invariants:@\n  @[%a@]@\n"
             (pp_list "@\n" (fun fmt (lt : lident label_term) ->
                  Format.fprintf fmt "%a : %a"
                    pp_id id
                    pp_label_term lt
                )) l)) v.invariants
      (pp_option (fun fmt -> Format.fprintf fmt "effect {@\n  @[%a@]}@\n" pp_instruction)) v.effect
      (pp_no_empty_list2 pp_assert) v.asserts
      (pp_no_empty_list2 pp_postcondition) v.specs

let pp_security fmt (s : security) =
  let pp_security_action fmt (a : security_action)=
    match a with
    | Sany -> Format.fprintf fmt "any"
    | Sentry l ->
      if List.length l = 1
      then pp_id fmt (List.nth l 0)
      else Format.fprintf fmt "[%a]" (pp_list " or " pp_id) l
  in
  let pp_security_role = pp_id in
  let pp_security_roles fmt l =
    if List.length l = 1
    then pp_id fmt (List.nth l 0)
    else Format.fprintf fmt "[%a]" (pp_list " or " pp_security_role) l
  in
  let pp_security_predicate fmt (sp : security_predicate) =
    match sp.s_node with
    | SonlyByRole (ad, roles) ->
      Format.fprintf fmt "only_by_role (%a, %a)"
        pp_action_description ad
        pp_security_roles roles

    | SonlyInAction (ad, action) ->
      Format.fprintf fmt "only_in_action (%a, %a)"
        pp_action_description ad
        pp_security_action action

    | SonlyByRoleInAction (ad, roles, action) ->
      Format.fprintf fmt "only_by_role_in_action (%a, %a, %a)"
        pp_action_description ad
        pp_security_roles roles
        pp_security_action action

    | SnotByRole (ad, roles) ->
      Format.fprintf fmt "not_by_role (%a, %a)"
        pp_action_description ad
        pp_security_roles roles

    | SnotInAction (ad, action) ->
      Format.fprintf fmt "not_in_action (%a, %a)"
        pp_action_description ad
        pp_security_action action

    | SnotByRoleInAction (ad, roles, action) ->
      Format.fprintf fmt "not_by_role_in_action (%a, %a, %a)"
        pp_action_description ad
        pp_security_roles roles
        pp_security_action action

    | StransferredBy ad ->
      Format.fprintf fmt "transferred_by (%a)"
        pp_action_description ad

    | StransferredTo ad ->
      Format.fprintf fmt "transferred_to (%a)"
        pp_action_description ad

    | SnoStorageFail action ->
      Format.fprintf fmt "no_storage_fail (%a)"
        pp_security_action action
  in

  let pp_security_item fmt (si : security_item) =
    Format.fprintf fmt "%a : %a;"
      pp_id si.label
      pp_security_predicate si.predicate
  in
  let empty = List.is_empty s.items
  in
  if empty
  then ()
  else
    Format.fprintf fmt "security {@\n  @[%a@]@\n}@\n"
      (pp_no_empty_list pp_security_item) s.items

let pp_variable fmt (v : lident variable) =
  Format.fprintf fmt "%s %a %a%a%a%a@\n"
    (if v.constant then "constant" else "variable")
    pp_id v.decl.name
    pp_ptyp (Option.get v.decl.typ)
    (pp_option (pp_prefix " from " pp_qualid)) v.from
    (pp_option (pp_prefix " to " pp_qualid)) v.to_
    (pp_option (pp_prefix " = " pp_pterm)) v.decl.default

let pp_field fmt (f : lident decl_gen) =
  Format.fprintf fmt "%a : %a%a;"
    pp_id f.name
    (pp_option pp_ptyp) f.typ
    (pp_option (pp_prefix " := " pp_pterm)) f.default

let pp_asset fmt (a : lident asset_struct) =
  Format.fprintf fmt "asset %a%a%a {@\n  @[%a@]@\n}%a%a%a@\n"
    pp_id a.name
    (pp_option (pp_prefix " identified by " pp_id)) a.key
    (pp_do_if (not (List.is_empty a.sort)) (pp_prefix " sorted by " (pp_list ", " pp_id))) a.sort
    (pp_list "@\n" pp_field) a.fields
    (pp_option (pp_prefix " with states " pp_id)) a.state
    (pp_option (pp_prefix " initialized by " pp_pterm)) a.init
    (pp_do_if (not (List.is_empty a.specs)) (
        fun fmt ->
          Format.fprintf fmt " with {@\n  @[%a@]@\n}"
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
  Format.fprintf fmt "%a =@\n  @[%a@]@\n"
    (fun fmt e ->
       match e.kind with
       | EKenum id -> Format.fprintf fmt "enum %a" pp_id id
       | EKstate -> pp_str fmt "states"
    ) e
    (pp_list "@\n" pp_enum_item) e.items

let pp_signature fmt (s : lident signature) =
  Format.fprintf fmt "%a: %a"
    pp_id s.name
    (fun fmt x ->
       if List.is_empty x
       then pp_str fmt "()"
       else (pp_list ", " pp_ptyp) fmt x) s.args

let pp_contract fmt (c : contract) =
  Format.fprintf fmt "contract %a =@\n  @[%a@]@\n"
    pp_id c.name
    (pp_list "@\n" pp_signature) c.signatures

let rec pp_rexpr fmt (r : rexpr) =
  let pp_node fmt = function
    | Rany -> pp_str fmt "any"
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

let pp_function fmt (f : function_) =
  Format.fprintf fmt "function %a (%a) : %a =@\n  @[%a%a@]@\n"
    pp_id f.name
    (pp_list ", " (fun fmt (x : lident decl_gen) ->
         Format.fprintf fmt "%a : %a"
           pp_id x.name
           pp_ptyp (Option.get x.typ)
       )) f.args
    pp_ptyp f.return
    (pp_option pp_specification) f.specification
    pp_instruction f.body

let pp_transaction_action fmt (t : transaction) =
  Format.fprintf fmt "action %a %a {@\n  @[%a%a%a%a%a%a%a@]@\n}@\n"
    pp_id t.name
    (pp_list " " (fun fmt (x : lident decl_gen) ->
         Format.fprintf fmt "(%a : %a)"
           pp_id x.name
           pp_ptyp (Option.get x.typ)
       )) t.args
    (pp_option pp_specification) t.specification
    (pp_do_if t.accept_transfer (fun fmt _ -> Format.fprintf fmt "accept transfer@\n")) ()
    (pp_option (fun fmt -> Format.fprintf fmt "called by %a@\n" pp_rexpr)) t.calledby
    (pp_option (pp_list "@\n " (fun fmt -> Format.fprintf fmt "require {@\n  @[%a@]@\n}@\n" pp_label_term))) t.require
    (pp_option (pp_list "@\n " (fun fmt -> Format.fprintf fmt "failif {@\n  @[%a@]@\n}@\n" pp_label_term))) t.failif
    (pp_list "@\n" pp_function) t.functions
    (pp_option (fun fmt x -> Format.fprintf fmt "effect {@\n  @[%a@]@\n}@\n" pp_instruction x)) t.effect

let rec pp_sexpr fmt (sexpr : sexpr) =
  match sexpr.node with
  | Sref id -> pp_id fmt id
  | Sor (lhs, rhs) -> Format.fprintf fmt "%a or %a" pp_sexpr lhs pp_sexpr rhs
  | Sany -> pp_str fmt "any"

let pp_transaction_transition fmt (t : transaction) (tr : lident transition) =
  Format.fprintf fmt "transition %a %a from %a%a {@\n  @[%a%a%a%a%a%a@]@\n}@\n"
    pp_id t.name
    (pp_list " " (fun fmt (x : lident decl_gen) ->
         Format.fprintf fmt "(%a : %a)"
           pp_id x.name
           pp_ptyp (Option.get x.typ)
       )) t.args
    pp_sexpr tr.from
    (pp_option (pp_prefix " on " (fun fmt (x, y) -> Format.fprintf fmt "%a.%a" pp_id x pp_id y))) tr.on
    (pp_option pp_specification) t.specification
    (pp_option (fun fmt -> Format.fprintf fmt "called by %a@\n" pp_rexpr)) t.calledby
    (pp_do_if t.accept_transfer (fun fmt _ -> Format.fprintf fmt "accept transfer@\n")) ()
    (pp_option (pp_list "@\n " (fun fmt -> Format.fprintf fmt "require {@\n  @[%a@]@\n}@\n" pp_label_term))) t.require
    (pp_list "@\n" pp_function) t.functions
    (pp_list "@\n" (fun fmt (to_, cond, action) ->
         Format.fprintf fmt "to %a%a@\n%a@\n"
           pp_id to_
           (pp_option (fun fmt x -> (Format.fprintf fmt " when %a" pp_pterm x))) cond
           (pp_option (fun fmt x -> (Format.fprintf fmt "with effect {@\n  @[%a@]}@\n" pp_instruction x))) action
       )) tr.trs

let pp_transaction fmt (t : transaction) =
  match t.transition with
  | Some tr -> pp_transaction_transition fmt t tr
  | None -> pp_transaction_action fmt t

let pp_ast fmt (ast : model) =
  Format.fprintf fmt "archetype %a@\n@\n\
                      %a%a%a%a%a%a%a%a@."
    pp_id ast.name
    (pp_no_empty_list2 pp_variable) ast.variables
    (pp_no_empty_list2 pp_asset) ast.assets
    (pp_no_empty_list2 pp_enum) ast.enums
    (pp_no_empty_list2 pp_contract) ast.contracts
    (pp_no_empty_list2 pp_function) ast.functions
    (pp_no_empty_list2 pp_transaction) ast.transactions
    (pp_no_empty_list2 pp_specification) ast.specifications
    (pp_no_empty_list2 pp_security) ast.securities

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
