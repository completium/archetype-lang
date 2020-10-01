(* -------------------------------------------------------------------------- *)
open Core
open Location
open ParseTree
open Printer_tools

let e_equal_greater =  (10,  NonAssoc) (* =>  *)
let e_in            =  (10,  NonAssoc) (* in  *)
let e_to            =  (10,  NonAssoc) (* to  *)
let e_other         =  (12,  Right)    (* otherwise *)
let e_then          =  (14,  Right)    (* then *)
let e_else          =  (16,  Right)    (* else *)
let e_comma         =  (20,  Left)     (* ,   *)
let e_semi_colon    =  (20,  Left)     (* ;   *)
let e_colon         =  (25,  NonAssoc) (* :   *)
let e_assign_simple =  (30,  NonAssoc) (* :=  *)
let e_assign_plus   =  (30,  NonAssoc) (* +=  *)
let e_assign_minus  =  (30,  NonAssoc) (* -=  *)
let e_assign_mult   =  (30,  NonAssoc) (* *=  *)
let e_assign_div    =  (30,  NonAssoc) (* /=  *)
let e_assign_and    =  (30,  NonAssoc) (* &=  *)
let e_assign_or     =  (30,  NonAssoc) (* |=  *)
let e_opspec1       =  (35,  NonAssoc) (* op spec 1  *)
let e_opspec2       =  (35,  NonAssoc) (* op spec 2  *)
let e_opspec3       =  (35,  NonAssoc) (* op spec 3  *)
let e_opspec4       =  (35,  NonAssoc) (* op spec 4  *)
let e_imply         =  (40,  Right)    (* ->  *)
let e_equiv         =  (50,  NonAssoc) (* <-> *)
let e_and           =  (60,  Left)     (* and *)
let e_or            =  (70,  Left)     (* or  *)
let e_xor           =  (70,  Left)     (* xor *)
let e_equal         =  (80,  NonAssoc) (* =   *)
let e_nequal        =  (80,  NonAssoc) (* <>  *)
let e_gt            =  (90,  Left)     (* >   *)
let e_ge            =  (90,  Left)     (* >=  *)
let e_lt            =  (90,  Left)     (* <   *)
let e_le            =  (90,  Left)     (* <=  *)
let e_plus          =  (100, Left)     (* +   *)
let e_minus         =  (100, Left)     (* -   *)
let e_mult          =  (110, Left)     (* *   *)
let e_divrat        =  (110, Left)     (* /   *)
let e_modulo        =  (110, Left)     (* %   *)
let e_diveuc        =  (115, Left)     (* div *)
let e_not           =  (115, Right)    (* not *)
let e_dot           =  (120, Right)    (* .   *)
let e_coloncolon    =  (130, NonAssoc) (* ::  *)
let e_app           =  (140, NonAssoc) (* f ()  *)
let e_for           =  (140, NonAssoc) (* for in .  *)


let e_tuple         =  (50,  NonAssoc) (* *  *)

let e_default       =  (0, NonAssoc) (* ?  *)
let e_simple        =  (150, NonAssoc) (* ?  *)

let get_prec_from_operator (op : operator) =
  match op with
  | Logical And     -> e_and
  | Logical Or      -> e_or
  | Logical Xor     -> e_xor
  | Logical Imply   -> e_imply
  | Logical Equiv   -> e_equiv
  | Cmp Equal       -> e_equal
  | Cmp Nequal      -> e_nequal
  | Cmp Gt          -> e_gt
  | Cmp Ge          -> e_ge
  | Cmp Lt          -> e_lt
  | Cmp Le          -> e_le
  | Arith Plus      -> e_plus
  | Arith Minus     -> e_minus
  | Arith Mult      -> e_mult
  | Arith DivRat    -> e_divrat
  | Arith DivEuc    -> e_diveuc
  | Arith Modulo    -> e_modulo
  | Unary Uminus    -> e_minus
  | Unary Not       -> e_not

let get_prec_from_assignment_operator (op : assignment_operator) =
  match op with
  | ValueAssign  -> e_assign_simple
  | PlusAssign   -> e_assign_plus
  | MinusAssign  -> e_assign_minus
  | MultAssign   -> e_assign_mult
  | DivAssign    -> e_assign_div
  | AndAssign    -> e_assign_and
  | OrAssign     -> e_assign_or

(* -------------------------------------------------------------------------- *)
let container_to_str c =
  match c with
  | Aggregate  -> "aggregate"
  | Partition  -> "partition"
  | View       -> "view"

let pp_id fmt (id : lident) =
  let id = unloc id in
  Format.fprintf fmt "%a%s" (fun fmt _ -> if is_keyword id then pp_str fmt "%" else ()) () id

let pp_container fmt c =
  Format.fprintf fmt "%s" (container_to_str c)

let rec pp_type outer pos fmt e =
  let pp_type_default = pp_type e_default PNone in

  match unloc e with
  | Tref x ->
    Format.fprintf fmt
      "%a"
      pp_id x

  | Tcontainer (x, y) ->
    Format.fprintf fmt
      "%a<%a>"
      pp_container y
      pp_type_default x

  | Ttuple l ->

    let pp fmt l =
      Format.fprintf fmt
        "(%a)"
        (pp_list " * " (pp_type e_tuple PInfix)) l
    in
    (maybe_paren outer e_tuple pos pp) fmt l

  | Toption x ->
    Format.fprintf fmt
      "option<%a>"
      pp_type_default x

  | Tset x ->
    Format.fprintf fmt
      "set<%a>"
      pp_type_default x

  | Tlist x ->
    Format.fprintf fmt
      "list<%a>"
      pp_type_default x

  | Tmap (k, v) ->
    Format.fprintf fmt
      "map<%a, %a>"
      pp_type_default k
      pp_type_default v

  | Tcontract t ->
    Format.fprintf fmt
      "contract<%a>"
      pp_type_default t

  | Tkeyof t ->
    Format.fprintf fmt
      "pkey<%a>"
      pp_type_default t

let pp_type fmt e = pp_type e_default PNone fmt e


(* -------------------------------------------------------------------------- *)
let logical_operator_to_str op =
  match op with
  | And   -> "and"
  | Or    -> "or"
  | Xor   -> "xor"
  | Imply -> "->"
  | Equiv -> "<->"

let comparison_operator_to_str op =
  match op with
  | Equal  -> "="
  | Nequal -> "<>"
  | Gt     -> ">"
  | Ge     -> ">="
  | Lt     -> "<"
  | Le     -> "<="

let arithmetic_operator_to_str op =
  match op with
  | Plus   -> "+"
  | Minus  -> "-"
  | Mult   -> "*"
  | DivRat -> "/"
  | DivEuc -> "div"
  | Modulo -> "%"

let unary_operator_to_str op =
  match op with
  | Uminus  -> "-"
  | Not     -> "not"

let operator_to_str op =
  match op with
  | Logical o -> logical_operator_to_str o
  | Cmp o     -> comparison_operator_to_str o
  | Arith o   -> arithmetic_operator_to_str o
  | Unary o   -> unary_operator_to_str o

let pp_operator fmt op =
  Format.fprintf fmt "%s" (operator_to_str op)

let assignment_operator_extra_to_str = function
  | PlusAssign   -> "+="
  | MinusAssign  -> "-="
  | MultAssign   -> "*="
  | DivAssign    -> "/="
  | AndAssign    -> "&="
  | OrAssign     -> "|="
  | _            -> raise (Anomaly "assignment_operator")

let assignment_operator_record_to_str op =
  match op with
  | ValueAssign  -> "="
  | _ -> assignment_operator_extra_to_str op

let assignment_operator_expr_to_str op =
  match op with
  | ValueAssign  -> ":="
  | _ -> assignment_operator_extra_to_str op

let pp_assignment_operator_record fmt op =
  Format.fprintf fmt "%s" (assignment_operator_record_to_str op)

let pp_assignment_operator_expr fmt op =
  Format.fprintf fmt "%s" (assignment_operator_expr_to_str op)

let quantifier_to_str op =
  match op with
  | Forall -> "forall"
  | Exists -> "exists"

let pp_quantifier fmt op =
  Format.fprintf fmt "%s" (quantifier_to_str op)

let pp_pattern fmt p =
  match unloc p with
  | Pwild ->  Format.fprintf fmt "| _"
  | Pref i ->  Format.fprintf fmt "| %a" pp_id i

let string_of_scope (s : scope) =
  match s with
  | Added   -> "added"
  | After   -> "after"
  | Before  -> "before"
  | Fixed   -> "fixed"
  | Removed -> "removed"
  | Stable  -> "stable"

let rec pp_expr outer pos fmt a =
  let e = unloc a in
  match e with
  | Eterm ((vset, lbl), id) ->
    let pp_label fmt lbl =
      let s =
        match lbl with
        | VLBefore   -> "before"
        | VLIdent  x -> Format.asprintf "at(%a)" pp_id x
      in Format.fprintf fmt "%s." s in

    let pp_vset fmt vset =
      let s =
        match vset with
        | VSAdded   -> "added"
        | VSUnmoved -> "unmoved"
        | VSRemoved -> "removed"
      in Format.fprintf fmt "%s." s in

    Format.fprintf fmt "%a%a%a"
      (pp_option pp_vset ) vset
      (pp_option pp_label) lbl
      pp_id id

  | Eliteral x ->

    let pp fmt x =
      Format.fprintf fmt "%a"
        pp_literal x
    in
    pp fmt x

  | Earray values ->

    let pp fmt values =
      Format.fprintf fmt "[%a]"
        (pp_list "; " (pp_expr e_simple PInfix)) values
    in
    (maybe_paren outer e_default pos pp) fmt values


  | Edot (lhs, rhs) ->

    let pp fmt (lhs, rhs) =
      Format.fprintf fmt "%a.%a"
        pp_simple_expr lhs
        pp_id rhs
    in
    (maybe_paren outer e_dot pos pp) fmt (lhs, rhs)

  | Esqapp (i, e) ->

    let pp fmt (i, e) =
      Format.fprintf fmt "%a[%a]"
        pp_simple_expr i
        pp_simple_expr e
    in
    (maybe_paren outer e_default pos pp) fmt (i, e)

  | Emulticomp (e, l) ->
    let pp fmt (e, l) =
      let pp_item fmt (op, e) =
        Format.fprintf fmt "%a %a"
          pp_str (comparison_operator_to_str (unloc op))
          pp_simple_expr e
      in
      Format.fprintf fmt "%a %a"
        pp_simple_expr e
        (pp_list " " pp_item) l
    in
    (maybe_paren outer e_default pos pp) fmt (e, l)

  | Erecord l ->

    let pp fmt l = pp_record_expr_internal fmt l in
    (maybe_paren outer e_simple pos pp) fmt l


  | Etuple l ->

    let pp fmt l =
      Format.fprintf fmt "(%a)"
        (pp_list ",@ " pp_simple_expr) l
    in
    (maybe_paren outer e_comma pos pp) fmt l


  | Eapp (Foperator {pldesc = op; _}, [a; b]) ->

    let pp fmt (op, a, b) =
      let prec = get_prec_from_operator op in
      Format.fprintf fmt "%a %a %a"
        (pp_expr prec PLeft) a
        pp_operator op
        (pp_expr prec PRight) b
    in
    (maybe_paren outer (get_prec_from_operator op) pos pp) fmt (op, a, b)

  | Eapp (Foperator {pldesc = op; _}, [a]) ->

    let pp fmt (op, a) =
      let prec = get_prec_from_operator op in
      Format.fprintf fmt "%a %a"
        pp_operator op
        (pp_expr prec PRight) a
    in
    (maybe_paren outer (get_prec_from_operator op) pos pp) fmt (op, a)

  | Eapp (Foperator _, _) -> raise (Anomaly "Eapp")

  | Eapp (Fident id, args) ->

    let pp fmt (id, args) =
      Format.fprintf fmt "%a%a"
        pp_id id
        (fun fmt args ->
           match args with
           | [] -> Format.fprintf fmt "()"
           | _ -> Format.fprintf fmt " (%a)" (pp_list ", " pp_simple_expr) args) args
    in
    (maybe_paren outer e_app pos pp) fmt (id, args)

  | Emethod (e, id, args) ->

    let pp fmt (e, id, args) =
      Format.fprintf fmt "%a.%a%a"
        pp_simple_expr e
        pp_id id
        (fun fmt args ->
           match args with
           | [] -> Format.fprintf fmt "()"
           | _ -> Format.fprintf fmt " (%a)" (pp_list ", " pp_simple_expr) args) args
    in
    (maybe_paren outer e_app pos pp) fmt (e, id, args)

  | Etransfer (x, tr) ->
    let pp fmt (x, tr) =
      Format.fprintf fmt "transfer %a%a"
        pp_simple_expr x
        (fun fmt -> function
           | TTsimple dst                 -> Format.fprintf fmt " to %a" pp_simple_expr dst
           | TTcontract (dst, id, t, arg) -> Format.fprintf fmt " to %a call %a<%a>(%a)" pp_simple_expr dst pp_id id pp_type t pp_simple_expr arg
           | TTentry (id, arg)            -> Format.fprintf fmt " to entry %a(%a)" pp_id id pp_simple_expr arg
           | TTself (id, args)            -> Format.fprintf fmt " to entry self.%a(%a)" pp_id id (pp_list "," pp_simple_expr) args
        ) tr
    in
    (maybe_paren outer e_default pos pp) fmt (x, tr)

  | Edorequire (x, y) ->

    let pp fmt (x, y) =
      Format.fprintf fmt "dorequire (%a, %a)"
        pp_simple_expr x
        pp_simple_expr y
    in
    (maybe_paren outer e_default pos pp) fmt (x, y)

  | Edofailif (x, y) ->

    let pp fmt (x, y) =
      Format.fprintf fmt "dofailif (%a, %a)"
        pp_simple_expr x
        pp_simple_expr y
    in
    (maybe_paren outer e_default pos pp) fmt (x, y)

  | Efail x ->

    let pp fmt x =
      Format.fprintf fmt "fail (%a)"
        pp_simple_expr x
    in
    (maybe_paren outer e_default pos pp) fmt x

  | Ereturn x ->

    let pp fmt x =
      Format.fprintf fmt "return %a"
        pp_simple_expr x
    in
    (maybe_paren outer e_default pos pp) fmt x

  | Eoption x ->

    let pp fmt x =
      let pp_option_ fmt x =
        match x with
        | OSome x -> Format.fprintf fmt "some(%a)" pp_simple_expr x
        | ONone -> Format.fprintf fmt "none"
      in
      Format.fprintf fmt "%a"
        pp_option_ x
    in
    (maybe_paren outer e_default pos pp) fmt x

  | Eassign (op, lhs, rhs) ->

    let prec = get_prec_from_assignment_operator op in
    let pp fmt (op, lhs, rhs) =
      Format.fprintf fmt "%a %a %a"
        (pp_expr prec PLeft) lhs
        pp_assignment_operator_expr op
        (pp_expr prec PRight) rhs
    in
    (maybe_paren outer prec pos pp) fmt (op, lhs, rhs)


  | Eif (cond, then_, else_) ->

    let pp fmt (cond, then_, else_) =
      Format.fprintf fmt "@[if %a@ then (%a)@ %a @]"
        (pp_expr e_default PNone) cond
        (pp_expr e_default PNone) then_
        pp_else else_
    in
    (maybe_paren outer e_default pos pp) fmt (cond, then_, else_)


  | Ematchwith (x, xs) ->

    let pp fmt (x, xs) =
      Format.fprintf fmt "match %a with@\n%a@\nend"
        (pp_expr e_default PNone) x
        (pp_list "@\n" (fun fmt (pts, e) ->
             Format.fprintf fmt "%a -> %a"
               (pp_list " " pp_pattern) pts
               (pp_expr e_imply PRight) e)) xs
    in
    (maybe_paren outer e_default pos pp) fmt (x, xs)

  | Erecupdate (e, l) ->

    let pp fmt (e, l) =
      Format.fprintf fmt "{%a with %a}"
        (pp_expr e_default PNone) e
        (pp_list "; " (fun fmt (id, v) ->
             Format.fprintf fmt "%a = (%a)"
               pp_id id
               (pp_expr e_equal PRight) v)) l
    in
    (maybe_paren outer e_default pos pp) fmt (e, l)

  | Efor (lbl, fid, expr, body) ->

    let pp fmt (lbl, fid, expr, body) =
      Format.fprintf fmt "for %a%a in %a do@\n  @[%a@]@\ndone"
        (pp_option (fun fmt -> Format.fprintf fmt ": %a " pp_id)) lbl
        (fun fmt fid ->
           match unloc fid with
           | FIsimple i -> pp_id fmt i
           | FIdouble (x, y) -> Format.fprintf fmt "(%a, %a)" pp_id x pp_id y) fid
        (pp_expr e_default PNone) expr
        (pp_expr e_for PNone) body
    in
    (maybe_paren outer e_default pos pp) fmt (lbl, fid, expr, body)

  | Eiter (lbl, id, a, b, body) ->

    let pp fmt (lbl, id, a, b, body) =
      Format.fprintf fmt "iter %a%a %ato %a do@\n  @[%a@]@\ndone"
        (pp_option (fun fmt -> Format.fprintf fmt ": %a " pp_id)) lbl
        pp_id id
        (pp_option (fun fmt -> Format.fprintf fmt "from %a " (pp_expr e_default PNone))) a
        (pp_expr e_default PNone) b
        (pp_expr e_for PNone) body
    in
    (maybe_paren outer e_default pos pp) fmt (lbl, id, a, b, body)

  | Ewhile (lbl, cond, body) ->

    let pp fmt (lbl, cond, body) =
      Format.fprintf fmt "while %a%a do@\n  @[%a@]@\ndone"
        (pp_option (fun fmt -> Format.fprintf fmt ": %a " pp_id)) lbl
        (pp_expr e_default PNone) cond
        (pp_expr e_for PNone) body
    in
    (maybe_paren outer e_default pos pp) fmt (lbl, cond, body)

  | Eseq (x, y) ->

    let pp fmt (x, y) =
      Format.fprintf fmt "%a;@\n%a"
        (pp_expr e_semi_colon PLeft) x
        (pp_expr e_semi_colon PRight) y
    in
    (maybe_paren outer e_semi_colon pos pp) fmt (x, y)

  | Eletin (id, t, e, body, other) ->
    let f =
      match t with
      | Some ({pldesc= Ttuple _; _}) -> pp_paren
      | _ -> pp_neutral
    in
    let pp fmt (id, t, e, body, other) =
      Format.fprintf fmt "@[@[<hv 0>let%a %a%a =@;<1 2>%a@;<1 0>in@]@ %a%a@]" (*"let %a = %a in %a"*)
        (pp_option (fun fmt _ -> Format.fprintf fmt " some")) other
        pp_id id
        (pp_option (pp_prefix " : " (f pp_type))) t
        (pp_expr e_in PLeft) e
        (pp_expr e_in PRight) body
        (pp_option (fun fmt e ->
             Format.fprintf fmt "@\notherwise %a"
               (pp_expr e_other PInfix) e)) other
    in
    (maybe_paren outer e_default pos pp) fmt (id, t, e, body, other)

  | Evar (id, t, e) ->

    let pp fmt (id, t, e) =
      let f =
        match t with
        | Some ({pldesc= Ttuple _; _}) -> pp_paren
        | _ -> pp_neutral
      in
      Format.fprintf fmt "var %a%a = %a"
        pp_id id
        (pp_option (pp_prefix " : " (f pp_type))) t
        (pp_expr e_in PLeft) e
    in
    (maybe_paren outer e_default pos pp) fmt (id, t, e)

  | Equantifier (q, id, t, body) ->

    let pp fmt (q, id, t, body) =
      let pp_quantifier_kind fmt t =
        match t with
        | Qcollection e ->
          Format.fprintf fmt "in %a" (pp_expr e_simple PNone) e
        | Qtype t_ ->
          Format.fprintf fmt ": %a" pp_type t_
      in
      Format.fprintf fmt "%a %a %a, %a"
        pp_quantifier q
        pp_id id
        pp_quantifier_kind t
        (pp_expr e_comma PRight) body
    in
    (maybe_paren outer e_default pos pp) fmt (q, id, t, body)

  | Eassert i ->

    let pp fmt i =
      Format.fprintf fmt "assert %a"
        pp_id i
    in
    (maybe_paren outer e_colon pos pp) fmt i

  | Elabel i ->

    let pp fmt i =
      Format.fprintf fmt "label %a"
        pp_id i
    in
    (maybe_paren outer e_colon pos pp) fmt i

  | Eunpack (t, arg) ->

    let pp fmt (t, arg) =
      Format.fprintf fmt "unpack<%a>(%a)"
        pp_type t
        (pp_expr e_default PNone) arg
    in
    (maybe_paren outer e_colon pos pp) fmt (t, arg)

  | Eentrypoint (t, a, b) ->
    let pp fmt (t, a, b) =
      Format.fprintf fmt "entrypoint<%a>(%a, %a)"
        pp_type t
        (pp_expr e_default PNone) a
        (pp_expr e_default PNone) b
    in
    (maybe_paren outer e_colon pos pp) fmt (t, a, b)

  | Eself x -> Format.fprintf fmt "(self.%a)" pp_id x

  | Eany -> Format.fprintf fmt "any"

  | Enothing -> Format.fprintf fmt "()"

  | Einvalid -> Format.fprintf fmt "(* invalid expr *)"


and pp_else fmt (e : expr option) =
  match e with
  | None -> ()
  | Some x -> Format.fprintf fmt " else (%a)" (pp_expr e_else PRight) x

and pp_literal fmt lit =
  match lit with
  | Lint      n -> Format.fprintf fmt "%si" (Big_int.string_of_big_int n)
  | Lnat      n -> Format.fprintf fmt "%s" (Big_int.string_of_big_int n)
  | Ldecimal  n -> Format.fprintf fmt "%s" n
  (* | Lrational (d, n) -> Format.fprintf fmt "%s div %s"
                          (Big_int.string_of_big_int d)
                          (Big_int.string_of_big_int n) *)
  | Ltz       n -> Format.fprintf fmt "%stz" (Big_int.string_of_big_int n)
  | Lmtz      n -> Format.fprintf fmt "%smtz" (Big_int.string_of_big_int n)
  | Lutz      n -> Format.fprintf fmt "%sutz" (Big_int.string_of_big_int n)
  | Laddress  a -> Format.fprintf fmt "@%s" a
  | Lstring   s -> Format.fprintf fmt "\"%s\"" s
  | Lbool     b -> Format.fprintf fmt "%s" (if b then "true" else "false")
  | Lduration d -> Format.fprintf fmt "%s" d
  | Ldate     d -> Format.fprintf fmt "%s" d
  | Lbytes    s -> Format.fprintf fmt "0x%s" s
  | Lpercent  n -> Format.fprintf fmt "%s%%" (Big_int.string_of_big_int n)

and pp_ident_ident fmt a =
  match a with
  | (x, y) ->
    Format.fprintf fmt "%a%a"
      (pp_option (pp_postfix "." pp_id)) x
      pp_id y

and pp_ident_typ fmt a =
  match a with
  | (x, y, exts) ->
    Format.fprintf fmt "%a%a%a"
      pp_id x
      pp_extensions exts
      (pp_prefix " : " pp_type) y

and pp_ident_quant fmt a =
  match a with
  | (x, y, exts) ->
    Format.fprintf fmt "%a%a%a"
      pp_id x
      pp_extensions exts
      (pp_prefix " in " pp_type) y

and pp_fun_ident_typ fmt (arg : lident_typ) =
  match arg with
  | (x, y, exts) ->
    Format.fprintf fmt "%a%a : %a"
      pp_id x
      pp_extensions exts
      pp_type y

and pp_fun_args fmt args =
  Format.fprintf fmt " (%a)"
    (pp_list ", " pp_fun_ident_typ) args

and pp_record_expr_internal fmt l =
  Format.fprintf fmt "{%a}"
    (pp_list ";@ " (
        fun fmt (o, e) ->
          Format.fprintf fmt "%a%a"
            (pp_option (fun fmt (op, id) ->
                 Format.fprintf fmt "%a %a "
                   pp_id id
                   pp_assignment_operator_record op
               )) o
            pp_simple_expr e
      )) l

(* -------------------------------------------------------------------------- *)
and pp_field fmt { pldesc = f; _ } =
  match f with
  | Ffield (id, typ, dv, exts) ->
    Format.fprintf fmt "%a%a : %a%a"
      pp_id id
      pp_extensions exts
      pp_type typ
      (pp_option (pp_prefix " = " (pp_expr e_equal PRight))) dv

(* -------------------------------------------------------------------------- *)
and pp_extension fmt { pldesc = e; _ } =
  match e with
  | Eextension (id, args) ->
    Format.fprintf fmt "[%%%%%a%a%%%%]"
      pp_id id
      pp_ext_args args

and pp_extensions x = (pp_option (pp_list " " pp_extension)) x

and pp_simple_expr fmt e = (pp_expr e_simple PNone) fmt e

and pp_ext_args fmt l =
  match l with
  | [] -> ()
  | _ -> Format.fprintf fmt "(%a)" (pp_list ", " pp_simple_expr) l

(* -------------------------------------------------------------------------- *)
let pp_to fmt ((to_, when_, effect) : (lident * expr option * expr option)) =
  Format.fprintf fmt " to %a@\n%a%a"
    pp_id to_
    (pp_option (pp_enclose " when {" "}@\n" (pp_expr e_default PNone))) when_
    (pp_option (pp_enclose " with effect {" "}@\n" (pp_expr e_default PNone))) effect

let pp_specification_variable fmt (sv : (lident * type_t * expr option) loced) =
  match sv with
  | {pldesc = (id, typ, dv); _} ->
    Format.fprintf fmt "variable %a : %a%a"
      pp_id id
      pp_type typ
      (pp_option (pp_prefix " = " (pp_expr e_equal PRight))) dv

(* -------------------------------------------------------------------------- *)

let pp_asset_option fmt opt =
  match opt with
  | AOidentifiedby ids -> Format.fprintf fmt "identified by %a" (pp_list " " pp_id) ids
  | AOsortedby id      -> Format.fprintf fmt "sorted by %a" pp_id id
  | AOto id            -> Format.fprintf fmt "to %a" pp_id id

let operation_enum_to_str e =
  match e with
  | AOadd    -> "@add"
  | AOremove -> "@remove"
  | AOupdate -> "@update"

let pp_asset_operation_enum fmt e =
  Format.fprintf fmt "%s" (operation_enum_to_str e)

let pp_asset_operation fmt (e : asset_operation) =
  match e with
  | AssetOperation (x, y) -> Format.fprintf fmt "[%a%a]"
                               (pp_list " " pp_asset_operation_enum) x
                               (pp_option (pp_prefix " " pp_simple_expr)) y

let pp_label_expr fmt (le : label_expr) =
  let (lbl, e) = unloc le in
  Format.fprintf fmt "%a%a"
    (pp_postfix " : " pp_id) lbl
    (pp_expr e_colon PRight) e

let pp_label_exprs xs = (pp_list ";@\n" pp_label_expr) xs

let pp_enum_option fmt = function
  | EOinitial ->
    Format.fprintf fmt "initial"

  | EOspecification xs ->
    Format.fprintf fmt "with { %a }"
      pp_label_exprs xs

let pp_ident_state fmt item =
  match item with
  | (id, opts) ->
    Format.fprintf fmt "%a%a"
      pp_id id
      (pp_prefix " " (pp_list " " pp_enum_option)) opts

let pp_asset_post_option fmt (apo : asset_post_option) =
  match apo with
  | APOstates i ->
    Format.fprintf fmt " with states %a@\n"
      pp_id i
  | APOconstraints cs ->
    Format.fprintf fmt " with {@\n  @[%a@]@\n}"
      pp_label_exprs cs
  | APOinit l ->
    Format.fprintf fmt " initialized by {@\n  @[%a@]@\n}"
      (pp_list ";@\n"
         (fun fmt x ->
            match unloc x with
              Erecord l -> pp_record_expr_internal fmt l
            | _ -> assert false)) l

let map_option f x =
  match x with
  | Some y -> f y
  | None -> ()

let pp_invariant fmt (lbl, is) =
  Format.fprintf fmt "invariant for %a {@\n  @[%a@]@\n}"
    pp_id lbl
    (pp_list ";@\n" (pp_expr e_default PNone)) is

let pp_invariants fmt is =
  (pp_do_if (match is with | [] -> false | _ -> true) (fun fmt -> Format.fprintf fmt "@\n  @[%a@]" (pp_list "@\n" pp_invariant))) fmt is

let pp_use fmt u =
  (pp_do_if (match u with | [] -> false | _ -> true) (fun fmt -> Format.fprintf fmt "@\n  @[use: %a;@]" (pp_list "@ " pp_id))) fmt u

let pp_pc_ci fmt (s, id, f, is, u) =
  match s, is, u with
  | "", [], [] -> Format.fprintf fmt "%a: %a@\n" pp_id id (pp_expr e_default PNone) f
  | _  -> begin
      Format.fprintf fmt "%a %a {@\n  @[%a@]%a%a@\n}"
        pp_str s
        pp_id id
        (pp_expr e_default PNone) f
        pp_invariants is
        pp_use u
    end

let pp_postcondition fmt (id, f, is, u) =
  pp_pc_ci fmt ("postcondition", id, f, is, u)

let pp_contractinvariant fmt (id, f, is, u) =
  pp_pc_ci fmt ("contract invariant", id, f, is, u)

let pp_assert fmt (id, f, is, u) =
  Format.fprintf fmt "assert %a {@\n  @[%a@]%a%a@\n}"
    pp_id id
    (pp_expr e_default PNone) f
    pp_invariants is
    pp_use u

let pp_specification_item fmt = function
  | Vpredicate (id, args, body) ->
    Format.fprintf fmt "predicate %a %a {@\n  @[%a@]@\n}"
      pp_id id
      pp_fun_args args
      (pp_expr e_default PNone) body

  | Vdefinition (id, typ, var, body) ->
    Format.fprintf fmt "definition %a {@\n  @[%a : %a | %a@]@\n}"
      pp_id id
      pp_id var
      pp_type typ
      (pp_expr e_default PNone) body

  (* fails {
     x with (s : string) :
      true;
     } *)
  | Vfails l ->
    Format.fprintf fmt "fails {@\n  @[%a@]@\n}"
      (pp_list "" (fun fmt (lbl, arg, t, f) ->
           Format.fprintf fmt "%a with (%a : %a):@\n  %a;"
             pp_id lbl
             pp_id arg
             pp_type t
             (pp_expr e_default PNone) f
         )) l

  | Vvariable (id, typ, dv) ->
    Format.fprintf fmt "variable %a : %a%a"
      pp_id id
      pp_type typ
      (pp_option (fun fmt x -> Format.fprintf fmt " = %a" (pp_expr e_equal PRight) x)) dv

  | Veffect e ->
    Format.fprintf fmt "shadow effect {@\n  @[%a@]@\n}"
      (pp_expr e_default PNone) e

  | Vassert (id, f, is, u) -> pp_assert fmt (id, f, is, u)

  | Vpostcondition (id, f, xs, u, Some PKPost) ->
    pp_postcondition fmt (id, f, xs, u)

  | Vpostcondition (id, f, xs, u, Some PKInv) ->
    pp_contractinvariant fmt (id, f, xs, u)

  | Vpostcondition (id, f, xs, u, None) ->
    pp_pc_ci fmt ("", id, f, xs, u)

let pp_specification_items = pp_list "@\n@\n" pp_specification_item

let pp_function fmt (f : s_function) =
  Format.fprintf fmt "%s %a %a%a %a@\n"
    (if f.getter then "getter" else "function")
    pp_id f.name
    pp_fun_args f.args
    (pp_option (pp_prefix " : " pp_type)) f.ret_t
    (pp_if (match f.spec with | Some _ -> true | None -> false)
         (fun fmt (f : s_function) ->
            Format.fprintf fmt "{@\n%a@\neffect@\n{%a}}"
              (pp_option (
                  fun fmt (x : specification) ->
                    let (items, exts) = unloc x in
                    let items = List.map unloc items in
                    Format.fprintf fmt "specification%a {@\n  @[%a@]@\n}"
                      pp_extensions exts
                      pp_specification_items items
                )) f.spec
              (pp_expr e_default PNone) f.body)
         (fun fmt (f : s_function) ->
            Format.fprintf fmt "{@\n%a@\n}" (pp_expr e_equal PRight) f.body)) f

let pp_spec fmt (items, exts) =
  let is_simple_spec items =
    List.for_all (fun x ->
        match x with
        | Vpostcondition (_, _, _, _, None) -> true
        | _ -> false
      ) items
  in
  let items = items |> List.map (fun x -> x |> unloc) in
  match items with
  | l when is_simple_spec l ->
    begin
      Format.fprintf fmt "specification%a {@\n  @[%a@]@\n}@\n"
        pp_extensions exts
        (pp_list "@\n" (
            fun fmt x ->
              match x with
              | Vpostcondition (id, f, _, u, None) ->
                Format.fprintf fmt "%a: %a%a;@\n"
                  pp_id id
                  (pp_expr e_default PNone) f
                  pp_use u
              | _ -> assert false
          )) items
    end
  | _ ->
    begin
      Format.fprintf fmt "specification%a {@\n  @[%a@]@\n}@\n"
        pp_extensions exts
        pp_specification_items items
    end

let rec pp_security_arg fmt arg =
  let arg = unloc arg in
  match arg with
  | Sident id -> pp_id fmt id
  | Sdot (a, b) ->
    Format.fprintf fmt "%a.%a"
      pp_id a
      pp_id b
  | Slist l ->
    Format.fprintf fmt "[%a]"
      (pp_list " or " pp_security_arg) l
  | Sapp (id, args) ->
    Format.fprintf fmt "%a (%a)"
      pp_id id
      (pp_list ",@ " pp_security_arg) args
  | Sbut (id, arg) ->
    Format.fprintf fmt "%a but %a"
      pp_id id
      pp_security_arg arg
  | Sto (id, arg) ->
    Format.fprintf fmt "%a to %a"
      pp_id id
      pp_security_arg arg

let pp_security fmt (items, exts) =
  let pp_security_item fmt (s : security_item) =
    let (label, id, args) = unloc s in
    Format.fprintf fmt "%a : %a (%a)"
      pp_id label
      pp_id id
      (pp_list ", " pp_security_arg) args
  in
  let pp_security_items =
    pp_list ";@\n" pp_security_item
  in
  Format.fprintf fmt "security%a {@\n  @[%a@]@\n}@\n"
    pp_extensions exts
    pp_security_items items

let pp_entry_properties fmt (props : entry_properties) =
  map_option (
    fun v ->
      let items, exts = v |> unloc in
      pp_spec fmt (items, exts)
  ) props.spec_fun;
  if (not props.accept_transfer)
  then Format.fprintf fmt "refuse transfer@\n";
  map_option (fun (e, exts) ->
      Format.fprintf fmt "called by%a %a@\n"
        pp_extensions exts
        (pp_expr e_default PNone) e) props.calledby;
  let pp_rf s1 s2 fmt (l, exts) =
    Format.fprintf fmt "%s%a {@\n  @[%a@]@\n}@\n"
      s1
      pp_extensions exts
      (pp_list ";@\n" (fun fmt (id, e, f) ->
           Format.fprintf fmt "%a%a: %a"
             pp_id id
             (pp_option (fun fmt x -> Format.fprintf fmt " %s %a" s2 (pp_expr e_default PNone) x)) f
             (pp_expr e_default PNone) e
         )) l
  in
  pp_option (pp_rf "require" "otherwise") fmt props.require;
  pp_option (pp_rf "failif" "with") fmt props.failif;
  (pp_list "@\n" pp_function) fmt (List.map unloc props.functions)

let pp_transition fmt (to_, conditions, effect) =
  Format.fprintf fmt "to %a%a%a@\n"
    pp_id to_
    (pp_option (
        fun fmt (e, exts) ->
          Format.fprintf fmt " when%a { %a }"
            pp_extensions exts
            (pp_expr e_default PNone) e)) conditions
    (pp_option (fun fmt (e, exts) ->
         Format.fprintf fmt "@\nwith effect%a {@\n  @[%a@]@\n}"
           pp_extensions exts
           pp_simple_expr e)) effect

let rec pp_declaration fmt { pldesc = e; _ } =
  let is_empty_entry_properties_opt (ap : entry_properties) (a : 'a option) =
    match ap.calledby, ap.require, ap.functions, ap.spec_fun, a with
    | None, None, [], None, None -> true
    | _ -> false in
  match e with
  | Darchetype (id, exts) ->
    Format.fprintf fmt "archetype%a %a"
      pp_extensions exts
      pp_id id

  | Dvariable (id, typ, dv, kind, invs, exts) ->
    Format.fprintf fmt "%a%a %a : %a%a%a"
      pp_str (match kind with | VKvariable -> "variable" | VKconstant -> "constant")
      pp_extensions exts
      pp_id id
      pp_type typ
      (pp_option (pp_prefix " = " (pp_expr e_equal PRight))) dv
      (pp_do_if (List.length invs > 0) (fun fmt x -> Format.fprintf fmt "@\nwith {@\n  @[%a@]@\n}" pp_label_exprs x)) invs

  | Denum (id, (ids, exts)) ->
    Format.fprintf fmt "%a%a"
      (fun fmt id -> (
           match id with
           | EKstate -> Format.fprintf fmt "states%a" pp_extensions exts
           | EKenum id -> Format.fprintf fmt "enum%a %a" pp_extensions exts pp_id id
         )) id
      (fun fmt ids -> (
           match ids with
           | [] -> ()
           | l -> Format.fprintf fmt " =@\n  @[%a@]"
                    (pp_list "@\n" (pp_prefix "| " pp_ident_state)) l
         )) ids

  | Dasset (id, fields, shadow_fields, opts, apo, ops, exts) ->
    Format.fprintf fmt "asset%a%a %a%a%a%a%a@\n"
      pp_extensions exts
      (pp_option pp_asset_operation) ops
      pp_id id
      (pp_prefix " " (pp_list " @," pp_asset_option)) opts
      (pp_do_if (List.length fields > 0) ((fun fmt -> Format.fprintf fmt " {@\n  @[%a@]@\n}" (pp_list ";@\n" pp_field)))) fields
      (pp_do_if (List.length shadow_fields > 0) ((fun fmt -> Format.fprintf fmt "shadow {@\n  @[%a@]@\n}" (pp_list ";@\n" pp_field)))) shadow_fields
      (pp_list " " pp_asset_post_option) apo

  | Drecord (id, fields, exts) ->
    Format.fprintf fmt "record%a %a %a@\n"
      pp_extensions exts
      pp_id id
      (pp_do_if (List.length fields > 0) ((fun fmt -> Format.fprintf fmt " {@\n  @[%a@]@\n}" (pp_list ";@\n" pp_field)))) fields

  | Dentry (id, args, props, code, exts) ->
    Format.fprintf fmt "entry%a %a%a%a"
      pp_extensions exts
      pp_id id
      pp_fun_args args
      (pp_do_if (not (is_empty_entry_properties_opt props code))
         (fun fmt x ->
            let pr, cod = x in
            Format.fprintf fmt " {@\n  @[%a%a@]@\n}"
              pp_entry_properties pr
              (pp_option (fun fmt (code, exts) ->
                   Format.fprintf fmt "effect%a {@\n  @[%a@]@\n}@\n"
                     pp_extensions exts
                     (pp_expr e_default PNone) code
                 )) cod)) (props, code)

  | Dtransition (id, args, on, from, props, trs, exts) ->
    Format.fprintf fmt "transition%a %a%a%a%a"
      pp_extensions exts
      pp_id id
      pp_fun_args args
      (pp_option (fun fmt (a, b) ->
           Format.fprintf fmt " on (%a : %a)"
             pp_id a
             pp_type b
         )) on
      (fun fmt (pr, ts) ->
         Format.fprintf fmt " {@\n  @[%a%a%a@]@\n}"
           (pp_do_if (not (is_empty_entry_properties_opt props None)) pp_entry_properties) pr
           (fun fmt from -> Format.fprintf fmt "from %a@\n" pp_simple_expr from) from
           (pp_list "@\n" pp_transition) ts) (props, trs)

  | Dextension (id, args) ->
    Format.fprintf fmt "%%%a%a"
      pp_id id
      pp_ext_args args

  | Dnamespace (id, ds) ->
    Format.fprintf fmt "namespace %a {@\n  @[%a@]@\n}"
      pp_id id
      (pp_list "\n" pp_declaration) ds

  | Dfunction f ->
    Format.fprintf fmt "%a"
      pp_function f

  | Dspecification v ->
    let items, exts = v |> unloc in
    pp_spec fmt (items, exts)

  | Dspecasset (id, l) ->
    Format.fprintf fmt "specification asset %a {@\n  @[%a@]@\n}"
      pp_id id
      (pp_list "@\n" (fun fmt x -> let (id, x) = unloc x in Format.fprintf fmt "%a: %a;" pp_id id pp_simple_expr x)) l

  | Dspecfun (sfk, id, args, s) ->
    Format.fprintf fmt "specification %s %a%a {@\n  @[%a@]@\n}"
      (match sfk with | SKentry -> "entry"  | SKgetter -> "getter"  | SKfunction -> "function")
      pp_id id
      pp_fun_args args
      pp_specification_items (s |> unloc |> fst |> List.map unloc)

  | Dspecvariable (id, l) ->
    Format.fprintf fmt "specification variable %a {@\n  @[%a@]@\n}"
      pp_id id
      (pp_list "@\n" (fun fmt x -> let (id, x) = unloc x in Format.fprintf fmt "%a: %a;" pp_id id pp_simple_expr x)) l

  | Dsecurity v ->
    let items, exts = v |> unloc in
    pp_security fmt (items, exts)

  | Dinvalid ->
    Format.fprintf fmt "(* invalid declaration *)"

(* -------------------------------------------------------------------------- *)
let pp_archetype fmt { pldesc = m; _ } =
  match m with
  | Marchetype es ->
    Format.fprintf fmt "%a@\n" (pp_list "@\n@\n" pp_declaration) es
  | Mextension (id, ds, es) ->
    Format.fprintf fmt "archetype extension %a (@\n  @[%a@]@\n) {@\n  @[%a@]@\n}@\n"
      pp_id id
      (pp_list "@,\n" pp_declaration) ds
      (pp_list "@,\n" pp_declaration) es


(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a" pp x

(* -------------------------------------------------------------------------- *)
let type_to_str        = string_of__of_pp pp_type
let declaration_to_str = string_of__of_pp pp_declaration
let archetype_to_str   = string_of__of_pp pp_archetype
