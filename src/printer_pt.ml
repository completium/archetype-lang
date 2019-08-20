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
let e_equal         =  (80,  NonAssoc) (* =   *)
let e_nequal        =  (80,  NonAssoc) (* <>  *)
let e_gt            =  (90,  Left)     (* >   *)
let e_ge            =  (90,  Left)     (* >=  *)
let e_lt            =  (90,  Left)     (* <   *)
let e_le            =  (90,  Left)     (* <=  *)
let e_plus          =  (100, Left)     (* +   *)
let e_minus         =  (100, Left)     (* -   *)
let e_mult          =  (110, Left)     (* *   *)
let e_div           =  (110, Left)     (* /   *)
let e_modulo        =  (110, Left)     (* %   *)
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
  | `Logical And     -> e_and
  | `Logical Or      -> e_or
  | `Logical Imply   -> e_imply
  | `Logical Equiv   -> e_equiv
  | `Cmp Equal       -> e_equal
  | `Cmp Nequal      -> e_nequal
  | `Cmp Gt          -> e_gt
  | `Cmp Ge          -> e_ge
  | `Cmp Lt          -> e_lt
  | `Cmp Le          -> e_le
  | `Arith Plus      -> e_plus
  | `Arith Minus     -> e_minus
  | `Arith Mult      -> e_mult
  | `Arith Div       -> e_div
  | `Arith Modulo    -> e_modulo
  | `Unary Uplus     -> e_plus
  | `Unary Uminus    -> e_minus
  | `Unary Not       -> e_not

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
  | Collection -> "collection"
  | Partition  -> "partition"

let pp_container fmt c =
  Format.fprintf fmt "%s" (container_to_str c)

let rec pp_type outer pos fmt e =
  let pp_type_default = pp_type e_default PNone in

  match unloc e with
  | Tref x ->
    Format.fprintf fmt
      "%a"
      pp_id x

  | Tasset x ->
    Format.fprintf fmt
      "%a record"
      pp_id x

  | Tcontainer (x, y) ->
    Format.fprintf fmt
      "%a %a"
      pp_type_default x
      pp_container y

  | Ttuple l ->

    let pp fmt l =
      Format.fprintf fmt
        "%a"
        (pp_list " * " (pp_type e_tuple PInfix)) l
    in
    (maybe_paren outer e_tuple pos pp) fmt l

  | Toption x ->
    Format.fprintf fmt
      "%a option"
      pp_type_default x



let pp_type fmt e = pp_type e_default PNone fmt e


(* -------------------------------------------------------------------------- *)
let logical_operator_to_str op =
  match op with
  | And   -> "and"
  | Or    -> "or"
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
  | Div    -> "/"
  | Modulo -> "%"

let unary_operator_to_str op =
  match op with
  | Uplus   -> "+"
  | Uminus  -> "-"
  | Not     -> "not"

let operator_to_str op =
  match op with
  | `Logical o -> logical_operator_to_str o
  | `Cmp o     -> comparison_operator_to_str o
  | `Arith o   -> arithmetic_operator_to_str o
  | `Unary o   -> unary_operator_to_str o

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

let rec pp_qualid fmt (q : ParseTree.qualid) =
  match q with
  | Qident i -> Format.fprintf fmt "%a" pp_id i
  | Qdot (q, i) -> Format.fprintf fmt "%a.%a"
                     pp_qualid q
                     pp_id i

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
  | `Added   -> "added"
  | `After   -> "after"
  | `Before  -> "before"
  | `Fixed   -> "fixed"
  | `Removed -> "removed"
  | `Stable  -> "stable"

let rec pp_expr outer pos fmt a =
  let e = unloc a in
  match e with
  | Eterm (l, e, id) ->

    let pp fmt (l, e, id) =
      let f fmt (e, id) = Format.fprintf fmt "%a%a"
          (pp_option (pp_postfix "::" pp_id)) e
          pp_id id in
      begin
        match l with
        | Some l -> Format.fprintf fmt "(%a at %a)"
                      f (e, id)
                      pp_id l
        | None -> f fmt (e, id)
      end
    in
    pp fmt (l, e, id)

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

    let pp fmt l =
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
    in
    (maybe_paren outer e_simple pos pp) fmt l


  | Etuple l ->

    let pp fmt l =
      Format.fprintf fmt "%a"
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
           | _ -> Format.fprintf fmt " %a" (pp_list " " pp_simple_expr) args) args
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
           | _ -> Format.fprintf fmt " %a" (pp_list " " pp_simple_expr) args) args
    in
    (maybe_paren outer e_app pos pp) fmt (e, id, args)

  | Etransfer (x, back, to_value) ->

    let pp fmt (x, back, to_value) =
      Format.fprintf fmt "transfer%s %a%a"
        (if back then " back" else "")
        pp_simple_expr x
        (pp_option (pp_prefix " to " pp_id)) to_value
    in
    (maybe_paren outer e_default pos pp) fmt (x, back, to_value)

  | Erequire x ->

    let pp fmt x =
      Format.fprintf fmt "require %a"
        pp_simple_expr x
    in
    (maybe_paren outer e_default pos pp) fmt x

  | Efailif x ->

    let pp fmt x =
      Format.fprintf fmt "failif %a"
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
        | OSome x -> Format.fprintf fmt "Some %a" pp_simple_expr x
        | ONone -> Format.fprintf fmt "None"
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


  | Ebreak ->

    let pp fmt =
      Format.fprintf fmt "break"
    in
    pp fmt


  | Efor (None, id, expr, body) ->

    let pp fmt (id, expr, body) =
      Format.fprintf fmt "for (%a in %a) (@\n@[<v 2>  %a@]@\n)"
        pp_id id
        (pp_expr e_default PNone) expr
        (pp_expr e_for PNone) body
    in
    (maybe_paren outer e_default pos pp) fmt (id, expr, body)

  | Efor (Some lbl, id, expr, body) ->
    pp_expr outer pos fmt
      (mkloc (loc a) (Elabel (lbl, mkloc (loc a) (Efor (None, id, expr, body)))))

  | Eassert e ->

    let pp fmt e =
      Format.fprintf fmt "assert (%a)"
        (pp_expr e_default PNone) e
    in
    (maybe_paren outer e_default pos pp) fmt e


  | Eseq (x, y) ->

    let pp fmt (x, y) =
      Format.fprintf fmt "%a;@\n%a"
        (pp_expr e_semi_colon PLeft) x
        (pp_expr e_semi_colon PRight) y
    in
    (maybe_paren outer e_semi_colon pos pp) fmt (x, y)

  | Eletin (id, t, e, body, other) ->

    let pp fmt (id, t, e, body, other) =
      Format.fprintf fmt "@[@[<hv 0>let %a%a =@;<1 2>%a@;<1 0>in@]@ %a%a@]" (*"let %a = %a in %a"*)
        pp_id id
        (pp_option (pp_prefix " : " pp_type)) t
        (pp_expr e_in PLeft) e
        (pp_expr e_in PRight) body
        (pp_option (fun fmt e ->
             Format.fprintf fmt "@\notherwise %a"
               (pp_expr e_other PInfix) e)) other
    in
    (maybe_paren outer e_default pos pp) fmt (id, t, e, body, other)


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

  | Elabel (i, x) ->

    let pp fmt (i, x) =
      Format.fprintf fmt "%a : %a"
        pp_id i
        (pp_expr e_colon PRight) x
    in
    (maybe_paren outer e_colon pos pp) fmt (i, x)

  | Eilabel i ->

    let pp fmt i =
      Format.fprintf fmt "label %a"
        pp_id i
    in
    (maybe_paren outer e_colon pos pp) fmt i

  | Esecurity x ->

    let pp fmt s =
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
          Format.fprintf fmt "(%a %a)"
            pp_id id
            (pp_list "@ " pp_security_arg) args
        | Sbut (id, arg) ->
          Format.fprintf fmt "(%a but %a)"
            pp_id id
            pp_security_arg arg
        | Sto (id, arg) ->
          Format.fprintf fmt "(%a to %a)"
            pp_id id
            pp_security_arg arg
      in
      let pp_security_pred fmt arg =
        let s = unloc s in
        match s with
        | SMayBePerformedOnlyByRole (lhs, rhs) ->
          Format.fprintf fmt "%a may be performed only by role %a"
            pp_security_arg lhs
            pp_security_arg rhs

        | SMayBePerformedOnlyByAction (lhs, rhs) ->
          Format.fprintf fmt "%a may be performed only by action %a"
            pp_security_arg lhs
            pp_security_arg rhs

        | SMayBePerformedByRole (lhs, rhs) ->
          Format.fprintf fmt "%a may be performed by role %a"
            pp_security_arg lhs
            pp_security_arg rhs

        | SMayBePerformedByAction (lhs, rhs) ->
          Format.fprintf fmt "%a may be performed by action %a"
            pp_security_arg lhs
            pp_security_arg rhs

        | STransferredBy arg ->
          Format.fprintf fmt "transferred by %a"
            pp_security_arg arg

        | STransferredTo arg ->
          Format.fprintf fmt "transferred to %a"
            pp_security_arg arg
      in
      Format.fprintf fmt "[_[ %a ]_]"
        pp_security_pred s
    in
    (maybe_paren outer e_default pos pp) fmt x

  | Einvalid -> Format.fprintf fmt "(* invalid expr *)"


and pp_else fmt (e : expr option) =
  match e with
  | None -> ()
  | Some x -> Format.fprintf fmt " else (%a)" (pp_expr e_else PRight) x

and pp_literal fmt lit =
  match lit with
  | Lnumber   n -> Format.fprintf fmt "%s" (Big_int.string_of_big_int n)
  | Lrational (d, n) -> Format.fprintf fmt "%s div %s"
                          (Big_int.string_of_big_int d)
                          (Big_int.string_of_big_int n)
  | Ltz       n -> Format.fprintf fmt "%stz" (Big_int.string_of_big_int n)
  | Laddress  a -> Format.fprintf fmt "@%s" a
  | Lstring   s -> Format.fprintf fmt "\"%s\"" s
  | Lbool     b -> Format.fprintf fmt "%s" (if b then "true" else "false")
  | Lduration d -> Format.fprintf fmt "%s" d
  | Ldate     d -> Format.fprintf fmt "%s" d

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
    Format.fprintf fmt "(%a%a : %a)"
      pp_id x
      pp_extensions exts
      pp_type y

and pp_fun_args fmt args =
  match args with
  | [] -> Format.fprintf fmt ""
  | _ -> Format.fprintf fmt " %a" (pp_list " " pp_fun_ident_typ) args

(* -------------------------------------------------------------------------- *)
and pp_field fmt { pldesc = f; _ } =
  match f with
  | Ffield (id, typ, dv, exts) ->
    Format.fprintf fmt "%a%a : %a%a;"
      pp_id id
      pp_extensions exts
      pp_type typ
      (pp_option (pp_prefix " = " (pp_expr e_equal PRight))) dv

(* -------------------------------------------------------------------------- *)
and pp_extension fmt { pldesc = e; _ } =
  match e with
  | Eextension (id, args) ->
    Format.fprintf fmt "[%%%a%a%%]"
      pp_id id
      (pp_option (pp_prefix " " (pp_list " " pp_simple_expr))) args

and pp_extensions x = (pp_option (pp_list " " pp_extension)) x

and pp_simple_expr fmt e = (pp_expr e_simple PNone) fmt e


(* -------------------------------------------------------------------------- *)
let pp_to fmt ((to_, when_, effect) : (lident * expr option * expr option)) =
  Format.fprintf fmt " to %a@\n%a%a"
    pp_id to_
    (pp_option (pp_enclose " when {" "}@\n" (pp_expr e_default PNone))) when_
    (pp_option (pp_enclose " with effect {" "}@\n" (pp_expr e_default PNone))) effect

let pp_specification_variable fmt (sv : (lident * type_t * expr option) loced) =
  match sv with
  | {pldesc = (id, typ, dv); _} ->
    Format.fprintf fmt "variable %a %a%a"
      pp_id id
      pp_type typ
      (pp_option (pp_prefix " = " (pp_expr e_equal PRight))) dv

(* -------------------------------------------------------------------------- *)
let pp_value_option fmt opt =
  match opt with
  | VOfrom e -> Format.fprintf fmt "from %a" pp_id e
  | VOto   e -> Format.fprintf fmt "to %a"   pp_id e

let pp_asset_option fmt opt =
  match opt with
  | AOidentifiedby id -> Format.fprintf fmt "identified by %a" pp_id id
  | AOsortedby id  -> Format.fprintf fmt "sorted by %a" pp_id id

let pp_signature fmt s =
  match s with
  | Ssignature (id, xs) ->
    Format.fprintf fmt "action %a%a"
      pp_id id
      (pp_do_if (List.length xs > 0) (pp_prefix " : " (pp_list ", " pp_type))) xs

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
                               (pp_option (pp_prefix " " (pp_list " " pp_simple_expr))) y

let pp_label_expr fmt (le : label_expr) =
  let (lbl, e) = unloc le in
  Format.fprintf fmt "%a%a"
    (pp_option (pp_postfix " : " pp_id)) lbl
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
    Format.fprintf fmt " with states %a"
      pp_id i
  | APOconstraints cs ->
    Format.fprintf fmt " with {@[<v 0>%a@]}"
      (pp_list ";@\n" pp_label_expr) cs
  | APOinit e ->
    Format.fprintf fmt " initialized by %a"
      pp_simple_expr e

let map_option f x =
  match x with
  | Some y -> f y
  | None -> ()

let pp_invariants fmt (lbl, is) =
  Format.fprintf fmt "invariants for %a {@\n@[<v 2>  %a@]@\n}"
    pp_id lbl
    (pp_list ";@\n" (pp_expr e_default PNone)) is

let pp_specification fmt (id, f, is) =
  Format.fprintf fmt "specification %a = {@\n@[<v 2>  %a@]@\n@[<v 2>  %a@]@\n}"
    pp_id id
    (pp_expr e_default PNone) f
    (pp_list "@\n" pp_invariants) is

let pp_assert fmt (id, lbl, f, is) =
  Format.fprintf fmt "assert %a at %a = {@\n@[<v 2>  %a@]@\n@[<v 2>  %a@]@\n}"
    pp_id id
    pp_id lbl
    (pp_expr e_default PNone) f
    (pp_list "@\n" pp_invariants) is

let pp_verification_item fmt = function
  | Vpredicate (id, args, body) ->
    Format.fprintf fmt "predicate %a %a = {@\n@[<v 2>  %a@\n}"
      pp_id id
      pp_fun_args args
      (pp_expr e_default PNone) body

  | Vdefinition (id, typ, var, body) ->
    Format.fprintf fmt "definition %a =@\n@[<v 2>  { %a : %a | %a }@]"
      pp_id id
      pp_id var
      pp_type typ
      (pp_expr e_default PNone) body

  | Vlemma (id, body) ->
    Format.fprintf fmt "lemma %a = {@\n@[<v 2>  %a@]@\n}"
      pp_id id
      (pp_expr e_default PNone) body

  | Vtheorem (id, body) ->
    Format.fprintf fmt "theorem %a = {@\n@[<v 2>  %a@]@\n}"
      pp_id id
      (pp_expr e_default PNone) body

  | Vvariable (id, typ, dv) ->
    Format.fprintf fmt "variable %a %a%a"
      pp_id id
      pp_type typ
      (pp_option (fun fmt x -> Format.fprintf fmt " = %a" (pp_expr e_equal PRight) x)) dv

  | Veffect e ->
    Format.fprintf fmt "effect {@\n@[<v 2>  %a@]@\n}"
      (pp_expr e_default PNone) e

  | Vassert (id, lbl, f, is) -> pp_assert fmt (id, lbl, f, is)

  | Vspecification (id, f, xs) -> pp_specification fmt (id, f, xs)

let pp_verification_items = pp_list "@\n@\n" pp_verification_item

let pp_function fmt (f : s_function) =
  Format.fprintf fmt "function %a %a%a %a@\n"
    pp_id f.name
    pp_fun_args f.args
    (pp_option (pp_prefix " : " pp_type)) f.ret_t
    (pp_if (match f.verif with | Some _ -> true | None -> false)
         (fun fmt (f : s_function) ->
            Format.fprintf fmt "= {@\n%a@\neffect@\n{%a}}"
              (pp_option (
                  fun fmt (x : verification) ->
                    let (items, exts) = unloc x in
                    let items = List.map unloc items in
                    Format.fprintf fmt "verification%a {@\n@[<v 2>  %a@]@\n}"
                      pp_extensions exts
                      pp_verification_items items
                )) f.verif
              (pp_expr e_default PNone) f.body)
         (fun fmt (f : s_function) ->
            Format.fprintf fmt "= {@\n%a@\n}" (pp_expr e_equal PRight) f.body)) f

let pp_verif fmt (items, exts) =
  let items = items |> List.map (fun x -> x |> unloc) in
  match items with
  (* | l when List.fold_left (fun accu x -> match x with | Vassert _ | Vspecification _ -> accu | _ -> false) true l ->
     begin
      Format.fprintf fmt "%a@\n" pp_verification_items items
     end *)
  | _ ->
    begin
      Format.fprintf fmt "verification%a {@\n@[<v 2>  %a@]@\n}@\n"
        pp_extensions exts
        pp_verification_items items
    end

let pp_action_properties fmt (props : action_properties) =
  map_option (
    fun v ->
      let items, exts = v |> unloc in
      pp_verif fmt (items, exts)
  ) props.verif;
  map_option (fun (e, exts) ->
      Format.fprintf fmt "called by%a %a@\n"
        pp_extensions exts
        (pp_expr e_default PNone) e) props.calledby;
  if (props.accept_transfer)
  then Format.fprintf fmt "accept transfer@\n";
  map_option (fun (cs, exts) ->
      Format.fprintf fmt "require%a{@\n@[<v 2>  %a@]}@\n"
        pp_extensions exts
        pp_label_exprs cs) props.require;
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
         Format.fprintf fmt "@\nwith effect%a {@\n@[<v 2>  %a@]}"
           pp_extensions exts
           pp_simple_expr e)) effect

let rec pp_declaration fmt { pldesc = e; _ } =
  let is_empty_action_properties_opt (ap : action_properties) (a : 'a option) =
    match ap.calledby, ap.require, ap.functions, ap.verif, a with
    | None, None, [], None, None -> true
    | _ -> false in
  match e with
  | Darchetype (id, exts) ->
    Format.fprintf fmt "archetype%a %a"
      pp_extensions exts
      pp_id id

  | Dvariable (id, typ, dv, opts, kind, exts) ->
    Format.fprintf fmt "%a%a %a %a%a%a"
      pp_str (match kind with | VKvariable -> "variable" | VKconstant -> "constant")
      pp_extensions exts
      pp_id id
      pp_type typ
      (pp_option (pp_prefix " " (pp_list " " pp_value_option))) opts
      (pp_option (pp_prefix " = " (pp_expr e_equal PRight))) dv

  | Dinstance (id, t, dv, exts) ->
    Format.fprintf fmt "instance%a %a of %a = %a"
      pp_extensions exts
      pp_id id
      pp_id t
      (pp_expr e_equal PRight) dv

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
           | l -> Format.fprintf fmt " =@\n@[<v 2>@]%a"
                    (pp_list "@\n" (pp_prefix "| " pp_ident_state)) l
         )) ids

  | Dasset (id, fields, opts, apo, ops, exts) ->
    Format.fprintf fmt "asset%a%a %a%a%a%a"
      pp_extensions exts
      (pp_option pp_asset_operation) ops
      pp_id id
      (pp_prefix " " (pp_list " @," pp_asset_option)) opts
      (pp_do_if (List.length fields > 0) ((fun fmt -> Format.fprintf fmt " = {@\n@[<v 2>%a@]}@\n" (pp_list "@\n" pp_field)))) fields
      (pp_list "@\n" pp_asset_post_option) apo

  | Daction (id, args, props, code, exts) ->
    Format.fprintf fmt "action%a %a%a%a"
      pp_extensions exts
      pp_id id
      pp_fun_args args
      (pp_do_if (not (is_empty_action_properties_opt props code))
         (fun fmt x ->
            let pr, cod = x in
            Format.fprintf fmt " = {@\n@[<v 2>%a%a@]@\n}"
              pp_action_properties pr
              (pp_option (fun fmt (code, exts) ->
                   Format.fprintf fmt "effect%a {@\n@[<v 2>  %a@]}@\n"
                     pp_extensions exts
                     (pp_expr e_default PNone) code
                 )) cod)) (props, code)

  | Dtransition (id, args, on, from, props, trs, exts) ->
    Format.fprintf fmt "transition%a %a%a%a from %a%a"
      pp_extensions exts
      pp_id id
      pp_fun_args args
      (pp_option (fun fmt (a, b) ->
           Format.fprintf fmt " on %a : %a"
             pp_id a
             pp_id b
         )) on
      pp_simple_expr from
      (fun fmt (pr, ts) ->
         Format.fprintf fmt " = {@\n@[<v 2>  %a%a@]@\n}"
           (pp_do_if (not (is_empty_action_properties_opt props None)) pp_action_properties) pr
           (pp_list "@\n" pp_transition) ts) (props, trs)

  | Dextension (id, args) ->
    Format.fprintf fmt "%%%a%a"
      pp_id id
      (pp_option (pp_prefix " " (pp_list " " pp_simple_expr))) args

  | Dnamespace (id, ds) ->
    Format.fprintf fmt "namespace %a {@\n@[<v 2>  %a@]@\n}"
      pp_id id
      (pp_list "\n" pp_declaration) ds

  | Dcontract (id, xs, exts) ->
    Format.fprintf fmt "contract%a %a = {@\n@[<v 2>  %a@]@\n}"
      pp_extensions exts
      pp_id id
      (pp_list "@\n" pp_signature) xs

  | Dfunction f ->
    Format.fprintf fmt "%a"
      pp_function f

  | Dverification v ->
    let items, exts = v |> unloc in
    pp_verif fmt (items, exts)

  | Dinvalid ->
    Format.fprintf fmt "(* invalid declaration *)"

(* -------------------------------------------------------------------------- *)
let pp_archetype fmt { pldesc = m; _ } =
  match m with
  | Marchetype es ->
    Format.fprintf fmt "%a@\n" (pp_list "@\n@\n" pp_declaration) es
  | Mextension (id, ds, es) ->
    Format.fprintf fmt "archetype extension %a (@\n@[<v 2>  %a@]@\n) = {@\n@[<v 2>  %a@]@\n}@\n"
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
