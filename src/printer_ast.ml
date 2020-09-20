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
  | Utz  -> Format.fprintf fmt "utz"

let pp_vtyp fmt = function
  | VTunit       -> Format.fprintf fmt "unit"
  | VTbool       -> Format.fprintf fmt "bool"
  | VTnat        -> Format.fprintf fmt "nat"
  | VTint        -> Format.fprintf fmt "int"
  | VTrational   -> Format.fprintf fmt "rational"
  | VTdate       -> Format.fprintf fmt "date"
  | VTduration   -> Format.fprintf fmt "duration"
  | VTstring     -> Format.fprintf fmt "string"
  | VTaddress    -> Format.fprintf fmt "address"
  | VTrole       -> Format.fprintf fmt "role"
  | VTcurrency   -> Format.fprintf fmt "tez"
  | VTsignature  -> Format.fprintf fmt "signature"
  | VTkey        -> Format.fprintf fmt "key"
  | VTkeyhash    -> Format.fprintf fmt "key_hash"
  | VTbytes      -> Format.fprintf fmt "bytes"
  | VTchainid    -> Format.fprintf fmt "chain_id"

let pp_container fmt = function
  | Collection -> Format.fprintf fmt "collection"
  | Aggregate  -> Format.fprintf fmt "aggregate"
  | Partition  -> Format.fprintf fmt "partition"
  | View       -> Format.fprintf fmt "view"

let rec pp_ptyp fmt (t : ptyp) =
  match t with
  | Tnamed i ->
    Format.fprintf fmt "#%d" i
  | Tasset an ->
    Format.fprintf fmt "%a" pp_id an
  | Trecord i ->
    Format.fprintf fmt "%a" pp_id i
  | Tenum en ->
    Format.fprintf fmt "%a" pp_id en
  | Tbuiltin b -> pp_vtyp fmt b
  | Tcontainer (t, c) ->
    Format.fprintf fmt "%a %a"
      pp_ptyp t
      pp_container c
  | Tset t ->
    Format.fprintf fmt "%a set"
      pp_ptyp t
  | Tlist t ->
    Format.fprintf fmt "%a list"
      pp_ptyp t
  | Tmap (k, v) ->
    Format.fprintf fmt "(%a * %a) map"
      pp_ptyp k
      pp_ptyp v
  | Toption t ->
    Format.fprintf fmt "%a option"
      pp_ptyp t
  | Ttuple ts ->
    Format.fprintf fmt "%a"
      (pp_list " * " pp_ptyp) ts
  | Toperation ->
    Format.fprintf fmt "operation"
  | Tcontract et ->
    Format.fprintf fmt "contract<%a>" pp_ptyp et
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
    | BVint v           -> Format.fprintf fmt "%ai" pp_big_int v
    | BVnat v           -> pp_big_int fmt v
    | BVbool v          -> pp_str fmt (if v then "true" else "false")
    | BVenum v          -> pp_str fmt v
    | BVrational (n, d) -> Format.fprintf fmt "(%a / %a)" pp_big_int n pp_big_int d
    | BVdate v          -> Core.pp_date fmt v
    | BVstring s        -> pp_str fmt s
    | BVcurrency (c, v) -> Format.fprintf fmt "%a%a" pp_big_int v pp_currency c
    | BVaddress v       -> Format.fprintf fmt "@@%a" pp_str v
    | BVduration v      -> Core.pp_duration_for_printer fmt v
    | BVbytes s         -> Format.fprintf fmt "0x%a" pp_str s
    | BVunit            -> Format.fprintf fmt "()"
  in
  pp_struct_poly pp_node fmt bval

let pp_logical_operator fmt = function
  | And   -> pp_str fmt "and"
  | Or    -> pp_str fmt "or"
  | Xor   -> pp_str fmt "xor"
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
  | DivEuc -> pp_str fmt "div"
  | DivRat -> pp_str fmt "/"
  | Modulo -> pp_str fmt "%"

let pp_unary_arithmetic_operator fmt = function
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
  (* constant *)
  | Cstate          -> "state"
  | Cnow            -> "now"
  | Ctransferred    -> "transferred"
  | Ccaller         -> "caller"
  | Cfail           -> "fail"
  | Cbalance        -> "balance"
  | Csource         -> "source"
  | Cselfaddress    -> "selfaddress"
  | Cconditions     -> "conditions"
  | Centries        -> "entries"
  | Cnone           -> "none"
  | Cany            -> "any"
  | Canyentry       -> "anyentry"
  | Cresult         -> "result"
  | Cchainid        -> "chain_id"
  | Coperations     -> "operations"
  | Cmetadata       -> "metadata"
  (* function *)
  | Cadd            -> "add"
  | Caddupdate      -> "addupdate"
  | Cceil           -> "ceil"
  | Cclear          -> "clear"
  | Cconcat         -> "concat"
  | Ccontains       -> "contains"
  | Ccount          -> "count"
  | Cfloor          -> "floor"
  | Cget            -> "get"
  | Cgetopt         -> "getopt"
  | Cisnone         -> "isnone"
  | Cissome         -> "issome"
  | Clength         -> "length"
  | Cmax            -> "max"
  | Cmin            -> "min"
  | Cnth            -> "nth"
  | Cpack           -> "pack"
  | Cremove         -> "remove"
  | Cremoveall      -> "removeall"
  | Cremoveif       -> "removeif"
  | Cselect         -> "select"
  | Cslice          -> "slice"
  | Csort           -> "sort"
  | Csum            -> "sum"
  | Cunpack         -> "unpack"
  | Cupdate         -> "update"
  | Cmkoperation    -> "mkoperation"
  | Ctostring       -> "to_string"
  (* set *)
  | Csadd           -> "set_add"
  | Csremove        -> "set_remove"
  | Cscontains      -> "set_contains"
  | Cslength        -> "set_length"
  (* list *)
  | Chead           -> "head"
  | Ctail           -> "tail"
  | Cabs            -> "abs"
  | Cprepend        -> "prepend"
  | Cheadtail       -> "head_tail"
  | Creverse        -> "reverse"
  (* map *)
  | Cmput           -> "put"
  | Cmremove        -> "remove"
  | Cmget           -> "get"
  | Cmgetopt        -> "getopt"
  | Cmcontains      -> "contains"
  | Cmlength        -> "length"
  (* crypto *)
  | Cblake2b        -> "blake2b"
  | Csha256         -> "sha256"
  | Csha512         -> "sha512"
  | Cchecksignature -> "check_signature"
  | Chashkey        -> "hash_key"
  (* vset *)
  | Cbefore         -> "before"
  | Citerated       -> "iterated"
  | Ctoiterate      -> "toiterate"
  (* formula *)
  | Cempty          -> "empty"
  | Cisempty        -> "isempty"
  | Csingleton      -> "singleton"
  | Csubsetof       -> "subsetof"
  | Cunion          -> "union"
  | Cinter          -> "inter"
  | Cdiff           -> "diff"

let pp_call_kind fmt = function
  | Cid id -> pp_id fmt id
  | Cconst c -> pp_str fmt (to_const c)

let pp_security_role = pp_lident

let pp_entry_description fmt = function
  | ADAny -> pp_str fmt "anyentry"
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

    | Precupdate (e, l) ->
      let pp fmt (e, l) =
        Format.fprintf fmt "{%a with %a}"
          pp_pterm e
          (pp_list "; " (fun fmt (id, v) -> Format.fprintf fmt "%a = %a" pp_id id pp_pterm v)) l
      in
      (pp_no_paren pp) fmt (e, l)

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

    | Pvar (vt, vs, id) ->
      let pp fmt (vt, vs, id) =
        let vs_val = match vs with
          | Vadded -> "added."
          | Vremoved -> "removed."
          | Vunmoved -> "unmoved."
          | Vnone -> ""
        in
        let vt_val =
          match vt with
          | VTbefore -> "before."
          | VTat lbl -> Format.asprintf "at(%s)." lbl
          | VTnone   -> ""
        in
        Format.fprintf fmt "%s%s%a" vs_val vt_val pp_id id
      in
      (pp_no_paren pp) fmt (vt, vs, id)

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

    | Pdot ({ node = Pcall (Some { node = Pvar (VTnone, Vnone, an) },
                            Cconst Cget, [AExpr k]) }, fn)
      ->
      let pp fmt (an, k, fn) =
        Format.fprintf fmt "%a[%a].%a"
          pp_id an
          pp_pterm k
          pp_id fn
      in (pp_with_paren pp) fmt (an, k, fn)

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

    | Ptupleaccess (x, k) ->
      let pp fmt (x, k) =
        Format.fprintf fmt "%a[%a]"
          pp_pterm x
          pp_big_int k
      in
      (pp_no_paren pp) fmt (x, k)

    | Pnone -> pp_str fmt "none"

    | Psome a ->
      let pp fmt a =
        Format.fprintf fmt "some(%a)"
          pp_pterm a
      in
      (pp_no_paren pp) fmt a

    | Pcast (src, dst, a) ->
      let pp fmt (src, dst, a) =
        Format.fprintf fmt "cast_%a_%a(%a)"
          pp_ptyp src
          pp_ptyp dst
          pp_pterm a
      in
      (pp_no_paren pp) fmt (src, dst, a)

    | Pself id ->
      let pp fmt id =
        Format.fprintf fmt "self.%a"
          pp_id id
      in
      (pp_no_paren pp) fmt id

    | Pentrypoint (t, a, b) ->
      let pp fmt (t, a, b) =
        Format.fprintf fmt "entrypoint<%a>(%a, %a)"
          pp_ptyp  t
          pp_id a
          pp_pterm b
      in
      (pp_no_paren pp) fmt (t, a, b)
  in
  pp_struct_poly pp_node fmt pterm

and pp_term_arg fmt = function
  | AExpr pt -> pp_pterm fmt pt
  | AFun (id, t, l, pt) ->
    if !Options.opt_typed
    then
      Format.fprintf fmt "(%a : %a)%a -> %a"
        pp_id id
        pp_ptyp t
        (pp_list "" (fun fmt (x, y, z) -> Format.fprintf fmt " (%a : %a = %a)" pp_id x pp_ptyp y pp_pterm z)) l
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

  | ASorting (b, f) ->
    let k = if b then "asc" else "desc" in
    Format.fprintf fmt "%s(%a)"
      k
      pp_id f

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

    | Ifor (fid, c, body) ->
      let pp fmt (fid, c, body) =
        Format.fprintf fmt "for %a%a in %a do@\n  @[%a@]@\ndone"
          (fun fmt x -> match x with Some v -> (Format.fprintf fmt ": %a " pp_str v) | _ -> ()) i.label
          (fun fmt fid ->
             match fid with
             | FIsimple i -> pp_id fmt i
             | FIdouble (x, y) -> Format.fprintf fmt "(%a, %a)" pp_id x pp_id y) fid
          pp_pterm c
          pp_instruction body
      in
      (pp_with_paren pp) fmt (fid, c, body)

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

    | Iwhile (cond, body) ->
      let pp fmt (cond, body) =
        Format.fprintf fmt "while %a block{@\n  @[%a@]@\n}"
          pp_pterm cond
          pp_instruction body
      in
      (pp_with_paren pp) fmt (cond, body)

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

    | Iassign (op, _, `Var id, value) ->
      let pp fmt (op, id, value) =
        Format.fprintf fmt "%a %a %a"
          pp_id id
          pp_assignment_operator op
          pp_pterm value
      in
      (pp_with_paren pp) fmt (op, id, value)

    | Iassign (op, _, `Field (an, k, fn), value) ->
      let pp fmt (op, an, k, fn, value) =
        Format.fprintf fmt "%a[%a].%a %a %a"
          pp_id an
          pp_pterm k
          pp_id fn
          pp_assignment_operator op
          pp_pterm value
      in
      (pp_with_paren pp) fmt (op, an, k, fn, value)

    | Irequire (k, pt, f) ->
      let pp fmt (k, pt, f) =
        Format.fprintf fmt "%a (%a, %a)"
          pp_str (if k then "dorequire" else "dofailif")
          pp_pterm pt
          pp_pterm f
      in
      (pp_with_paren pp) fmt (k, pt, f)

    | Itransfer (value, tr) ->
      let pp fmt (value, tr) =
        Format.fprintf fmt "transfer %a%a"
          pp_pterm value
          (fun fmt -> function
             | TTsimple dst                 -> Format.fprintf fmt " to %a" pp_pterm dst
             | TTcontract (dst, id, t, arg) -> Format.fprintf fmt " to %a call %a<%a>(%a)" pp_pterm dst pp_id id pp_ptyp t pp_pterm arg
             | TTentry (e, arg)             -> Format.fprintf fmt " to entry %a(%a)" pp_pterm e pp_pterm arg
             | TTself (id, args)            -> Format.fprintf fmt " to entry self.%a(%a)" pp_id id (pp_list "," (fun fmt (id, v) ->  Format.fprintf fmt "%a = %a" pp_id id pp_pterm v)) args
          ) tr
      in
      (pp_with_paren pp) fmt (value, tr)

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

    | Ifail pt ->
      let pp fmt pt =
        Format.fprintf fmt "fail (%a)"
          pp_pterm pt
      in
      (pp_with_paren pp) fmt pt
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
              && List.is_empty v.fails
              && List.is_empty v.theorems
              && List.is_empty v.variables
              && List.is_empty v.invariants
              && Option.is_none v.effect
              && List.is_empty v.specs
              && List.is_empty v.asserts
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
  let pp_fail fmt (f : lident fail) =
    Format.fprintf fmt "%a with (%a : %a):@\n  @[%a@];@\n"
      pp_id f.label
      pp_id f.arg
      pp_ptyp f.atype
      pp_pterm f.formula
  in
  let pp_fails fmt l = if List.is_empty l then () else Format.fprintf fmt "fails {@\n  @[%a@]@\n}" (pp_list "@\n" pp_fail) l in
  let pp_variable_spec fmt (v : lident variable) =
    let decl = v.decl in
    Format.fprintf fmt "variable %a%a%a"
      pp_id decl.name
      (pp_option (pp_prefix " : " pp_ptyp)) decl.typ
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
    Format.fprintf fmt "specification {@\n  @[%a%a%a%a%a%a%a%a%a%a@]@\n}@\n"
      (pp_no_empty_list2 pp_predicate) v.predicates
      (pp_no_empty_list2 pp_definitions) v.definitions
      pp_fails v.fails
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
      (pp_option (fun fmt -> Format.fprintf fmt "shadow effect {@\n  @[%a@]@\n}@\n" pp_instruction)) v.effect
      (pp_no_empty_list2 pp_assert) v.asserts
      (pp_no_empty_list2 pp_postcondition) v.specs

let pp_security fmt (s : security) =
  let pp_security_entry fmt (a : security_entry)=
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
        pp_entry_description ad
        pp_security_roles roles

    | SonlyInEntry (ad, entry) ->
      Format.fprintf fmt "only_in_entry (%a, %a)"
        pp_entry_description ad
        pp_security_entry entry

    | SonlyByRoleInEntry (ad, roles, entry) ->
      Format.fprintf fmt "only_by_role_in_entry (%a, %a, %a)"
        pp_entry_description ad
        pp_security_roles roles
        pp_security_entry entry

    | SnotByRole (ad, roles) ->
      Format.fprintf fmt "not_by_role (%a, %a)"
        pp_entry_description ad
        pp_security_roles roles

    | SnotInEntry (ad, entry) ->
      Format.fprintf fmt "not_in_entry (%a, %a)"
        pp_entry_description ad
        pp_security_entry entry

    | SnotByRoleInEntry (ad, roles, entry) ->
      Format.fprintf fmt "not_by_role_in_entry (%a, %a, %a)"
        pp_entry_description ad
        pp_security_roles roles
        pp_security_entry entry

    | StransferredBy ad ->
      Format.fprintf fmt "transferred_by (%a)"
        pp_entry_description ad

    | StransferredTo ad ->
      Format.fprintf fmt "transferred_to (%a)"
        pp_entry_description ad

    | SnoStorageFail entry ->
      Format.fprintf fmt "no_storage_fail (%a)"
        pp_security_entry entry
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
  Format.fprintf fmt "%s %a : %a%a%a@\n"
    (if v.constant then "constant" else "variable")
    pp_id v.decl.name
    pp_ptyp (Option.get v.decl.typ)
    (pp_option (pp_prefix " = " pp_pterm)) v.decl.default
    (fun fmt l ->
       if List.is_empty l
       then ()
       else Format.fprintf fmt " { @[%a@] }" (pp_list ";@\n" pp_label_term) l
    ) v.invs

let pp_field fmt (f : lident decl_gen) =
  Format.fprintf fmt "%a : %a%a;"
    pp_id f.name
    (pp_option pp_ptyp) f.typ
    (pp_option (pp_prefix " := " pp_pterm)) f.default

let pp_asset fmt (a : lident asset_struct) =
  let fields = List.filter (fun f -> not f.shadow) a.fields in
  let shadow_fields = List.filter (fun f -> f.shadow) a.fields in
  Format.fprintf fmt "asset %a%a%a%a {@\n  @[%a@]@\n}%a%a%a%a@\n"
    pp_id a.name
    (pp_prefix " identified by " (pp_list " " pp_id)) a.keys
    (pp_do_if (not (List.is_empty a.sort)) (pp_prefix " sorted by " (pp_list ", " pp_id))) a.sort
    (pp_do_if (a.big_map) (fun fmt _ -> pp_str fmt " to big_map")) ()
    (pp_list "@\n" pp_field) fields
    (pp_do_if (not (List.is_empty shadow_fields)) (
        fun fmt fields ->
          Format.fprintf fmt " shadow {@\n  @[%a@]@\n} "
            (pp_list "@\n" pp_field) fields)) shadow_fields
    (pp_option (pp_prefix " with states " pp_id)) a.state
    (pp_do_if (not (List.is_empty a.init)) (
        let pp1 fmt init1 =
          Format.fprintf fmt "  {%a};"
            (pp_list "; " pp_pterm) init1
        in
        fun fmt init ->
          Format.fprintf fmt " initialized by {@\n%a@\n} "
            (pp_list "@\n" pp1) init)) a.init
    (pp_do_if (not (List.is_empty a.specs)) (
        fun fmt ->
          Format.fprintf fmt " with {@\n  @[%a@]@\n}"
            (pp_list ";@\n" pp_label_term))) a.specs

let pp_record fmt (r : record) =
  Format.fprintf fmt "record %a {@\n  @[%a@]@\n}@\n"
    pp_id r.name
    (pp_list "@\n" pp_field) r.fields

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

let rec pp_rexpr fmt (r : rexpr) =
  let pp_node fmt = function
    | Rany -> pp_str fmt "any"
    | Rexpr e -> pp_pterm fmt e
    | Ror (lhs, rhs) ->
      Format.fprintf fmt "%a or %a"
        pp_rexpr lhs
        pp_rexpr rhs
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

let pp_fun_ident_typ fmt (arg : lident decl_gen) =
  Format.fprintf fmt "%a : %a"
    pp_id arg.name
    pp_ptyp (Option.get arg.typ)

let pp_fun_args fmt args =
  Format.fprintf fmt " (%a)"
    (pp_list " " pp_fun_ident_typ) args

let pp_function fmt (f : function_) =
  Format.fprintf fmt "%s %a%a : %a =@\n  @[%a%a@]@\n"
    (match f.kind with | FKfunction -> "function" | FKgetter -> "getter")
    pp_id f.name
    pp_fun_args f.args
    pp_ptyp f.return
    (pp_option pp_specification) f.specification
    pp_instruction f.body

let pp_transaction_entry fmt (t : transaction) =
  Format.fprintf fmt "entry %a%a {@\n  @[%a%a%a%a%a%a%a@]@\n}@\n"
    pp_id t.name
    pp_fun_args t.args
    (pp_option pp_specification) t.specification
    (pp_do_if (not t.accept_transfer) (fun fmt _ -> Format.fprintf fmt "refuse transfer@\n")) ()
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
  Format.fprintf fmt "transition %a%a%a {@\n  @[%a%a%a%a%a%a%a@]@\n}@\n"
    pp_id t.name
    pp_fun_args t.args
    (pp_option (pp_prefix " on " (fun fmt (k, _, an, _) -> Format.fprintf fmt "(%a : pkey of %a)" pp_id k pp_id an))) tr.on
    (pp_option pp_specification) t.specification
    (pp_option (fun fmt -> Format.fprintf fmt "called by %a@\n" pp_rexpr)) t.calledby
    (pp_do_if t.accept_transfer (fun fmt _ -> Format.fprintf fmt "accept transfer@\n")) ()
    (pp_option (pp_list "@\n " (fun fmt -> Format.fprintf fmt "require {@\n  @[%a@]@\n}@\n" pp_label_term))) t.require
    (pp_list "@\n" pp_function) t.functions
    (fun fmt from -> Format.fprintf fmt " from %a@\n"
        pp_sexpr from
    ) tr.from
    (pp_list "@\n" (fun fmt (to_, cond, entry) ->
         Format.fprintf fmt "to %a%a@\n%a@\n"
           pp_id to_
           (pp_option (fun fmt x -> (Format.fprintf fmt " when %a" pp_pterm x))) cond
           (pp_option (fun fmt x -> (Format.fprintf fmt "with effect {@\n  @[%a@]}@\n" pp_instruction x))) entry
       )) tr.trs

let pp_transaction fmt (t : transaction) =
  match t.transition with
  | Some tr -> pp_transaction_transition fmt t tr
  | None -> pp_transaction_entry fmt t

let pp_decl_ fmt = function
  | Dvariable v -> pp_variable fmt v
  | Dasset    a -> pp_asset fmt a
  | Drecord   r -> pp_record fmt r
  | Denum     e -> pp_enum fmt e

let pp_fun_ fmt = function
  | Ffunction f    -> pp_function fmt f
  | Ftransaction t -> pp_transaction fmt t

let pp_ast fmt (ast : ast) =
  Format.fprintf fmt "archetype %a@\n@\n@." pp_id ast.name;
  (pp_no_empty_list2 pp_decl_) fmt ast.decls;
  (pp_no_empty_list2 pp_fun_) fmt ast.funs;
  (pp_no_empty_list2 pp_specification) fmt ast.specifications;
  (pp_no_empty_list2 pp_security) fmt ast.securities;
  Format.fprintf fmt "@."

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_ast (x : ast) = string_of__of_pp pp_ast x
