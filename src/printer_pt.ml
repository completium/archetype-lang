(* -------------------------------------------------------------------------- *)
open Core
open Location
open ParseTree
open Printer_tools

(* -------------------------------------------------------------------- *)

let is_keyword = Lexer.keywords_ |> List.map fst |> fun x y -> List.mem y x

(* -------------------------------------------------------------------- *)

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
let e_or            =  (60,  Left)     (* or  *)
let e_xor           =  (60,  Left)     (* xor *)
let e_and           =  (70,  Left)     (* and *)
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
let e_divmod        =  (110, Left)     (* /%  *)
let e_three_way_cmp =  (112, Left)     (* <=> *)
let e_left_shift    =  (115, Left)     (* <<  *)
let e_right_shift   =  (115, Left)     (* >>  *)
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
  | Arith DivMod    -> e_divmod
  | Arith ThreeWayCmp -> e_three_way_cmp
  | Arith ShiftLeft -> e_left_shift
  | Arith ShiftRight-> e_right_shift
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
  | Aggregate      -> "aggregate"
  | Partition      -> "partition"
  | AssetContainer -> "asset_container"
  | AssetValue     -> "asset_value"
  | AssetView      -> "asset_view"
  | AssetKey       -> "asset_key"

let is_percent_prefix str = (String.length str >= 1 && String.equal "_" (String.sub str 0 1)) || is_keyword str

let string_of_id (id : Ident.ident) : string =
  if is_percent_prefix id then "%" ^ id else id

let pp_id fmt (id : lident) =
  Format.fprintf fmt "%s" (string_of_id (unloc id))

let pp_container fmt c =
  Format.fprintf fmt "%s" (container_to_str c)

let pp_id_scope fmt = function
  | SINone -> ()
  | SIParent -> Format.fprintf fmt "::"
  | SIId x -> Format.fprintf fmt "%a::" pp_id x

let pp_lid fmt lid =
  Format.fprintf fmt "%a%a"
    pp_id_scope (fst lid)
    pp_id (snd lid)

let rec pp_type fmt (e, a) =
  let f fmt x = pp_type fmt x in

  let pp_e fmt e =
    match unloc e with
    | Tref (s, x) ->
      Format.fprintf fmt
        "%a%a"
        pp_id_scope s
        pp_id x

    | Tcontainer (x, y) ->
      Format.fprintf fmt
        "%a<%a>"
        pp_container y
        f x

    | Ttuple l -> (pp_paren (pp_list " * " f)) fmt l

    | Toption x ->
      Format.fprintf fmt
        "option<%a>"
        f x

    | Tset x ->
      Format.fprintf fmt
        "set<%a>"
        f x

    | Tlist x ->
      Format.fprintf fmt
        "list<%a>"
        f x

    | Tmap (k, v) ->
      Format.fprintf fmt
        "map<%a, %a>"
        f k
        f v

    | Tbig_map (k, v) ->
      Format.fprintf fmt
        "big_map<%a, %a>"
        f k
        f v

    | Titerable_big_map (k, v) ->
      Format.fprintf fmt
        "iterable_big_map<%a, %a>"
        f k
        f v

    | Tor (k, v) ->
      Format.fprintf fmt
        "or<%a, %a>"
        f k
        f v

    | Tlambda (a, r) ->
      Format.fprintf fmt
        "lambda<%a, %a>"
        f a
        f r

    | Tcontract t ->
      Format.fprintf fmt
        "contract<%a>"
        f t

    | Tticket t ->
      Format.fprintf fmt
        "ticket<%a>"
        f t

    | Tsapling_state n       -> Format.fprintf fmt "sapling_state(%s)"       (Big_int.string_of_big_int n)
    | Tsapling_transaction n -> Format.fprintf fmt "sapling_transaction(%s)" (Big_int.string_of_big_int n)

  in

  match a with
  | None -> pp_e fmt e
  | Some a -> Format.fprintf fmt "(%a %%%a)" pp_e e pp_id a

(* -------------------------------------------------------------------------- *)
let logical_operator_to_str op =
  match op with
  | And   -> "and"
  | Or    -> "or"
  | Xor   -> "xor"

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
  | DivMod -> "/%"
  | ThreeWayCmp  -> "<=>"
  | ShiftLeft  -> "<<|"
  | ShiftRight -> "|>>"

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
  | ValueAssign  -> ":="
  | _ -> assignment_operator_extra_to_str op

let assignment_operator_expr_to_str op =
  match op with
  | ValueAssign  -> ":="
  | _ -> assignment_operator_extra_to_str op

let pp_assignment_operator_record fmt op =
  Format.fprintf fmt "%s" (assignment_operator_record_to_str op)

let pp_assignment_operator_expr fmt op =
  Format.fprintf fmt "%s" (assignment_operator_expr_to_str op)

let pp_pname fmt op =
  let x =
    match op with
    | PIdent x -> string_of_id x
    | PCons    -> "$cons"
    | PNil     -> "$nil"
    | PSome    -> "$some"
    | PNone    -> "$none"
    | PLeft    -> "$left"
    | PRight   -> "$right"
  in Format.fprintf fmt "%s" x

let pp_pattern fmt p =
  match unloc p with
  | Pwild -> Format.fprintf fmt "| _"

  | Pref ({ pldesc = PSome }, [x]) ->
    Format.fprintf fmt "| some %a" pp_id x

  | Pref ({ pldesc = PNone }, []) ->
    Format.fprintf fmt "| none"

  | Pref ({ pldesc = PCons }, [x; xs]) ->
    Format.fprintf fmt "| %a :: %a" pp_id x pp_id xs

  | Pref ({ pldesc = PNil }, []) ->
    Format.fprintf fmt "| []"

  | Pref ({ pldesc = PLeft }, [x]) ->
    Format.fprintf fmt "| left %a" pp_id x

  | Pref ({ pldesc = PRight }, [x]) ->
    Format.fprintf fmt "| right %a" pp_id x

  | Pref (i, [] ) ->  Format.fprintf fmt "| %a" pp_pname (unloc i)
  | Pref (i, [x]) ->  Format.fprintf fmt "| %a %a" pp_pname (unloc i) pp_id x
  | Pref (i, xs ) ->  Format.fprintf fmt "| %a (%a)" pp_pname (unloc i) (pp_list ", " pp_id) xs

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
  | Eterm (scope, id) ->
    Format.fprintf fmt "%a%a"
      pp_id_scope scope
      pp_id id

  | Eliteral x ->

    let pp fmt x =
      Format.fprintf fmt "%a"
        pp_literal x
    in
    pp fmt x

  | Earray (scope, values) ->

    let pp fmt (scope, values) =
      Format.fprintf fmt "%a[%a]"
        pp_id_scope scope
        (pp_list "; " (pp_expr e_simple PInfix)) values
    in
    (maybe_paren outer e_default pos pp) fmt (scope, values)


  | Edot (lhs, (scope, rhs)) ->

    let pp fmt (lhs, (scope, rhs)) =
      Format.fprintf fmt "%a.%a%a"
        pp_simple_expr lhs
        pp_id_scope scope
        pp_id rhs
    in
    (maybe_paren outer e_dot pos pp) fmt (lhs, (scope, rhs))

  | Equestiondot (lhs, (scope, rhs)) ->

    let pp fmt (lhs, (scope, rhs)) =
      Format.fprintf fmt "%a?.%a%a"
        pp_simple_expr lhs
        pp_id_scope scope
        pp_id rhs
    in
    (maybe_paren outer e_dot pos pp) fmt (lhs, (scope, rhs))

  | Eternary (c, x, y) ->

    let pp fmt (c, x, y) =
      Format.fprintf fmt "(%a ? %a : %a)"
        pp_simple_expr c
        pp_simple_expr x
        pp_simple_expr y
    in
    pp fmt (c, x, y)

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

  | Erecord (scope, l) ->

    let pp fmt l = pp_record_expr_internal fmt (scope, l) in
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
        pp_lid id
        (fun fmt args ->
           match args with
           | [] -> Format.fprintf fmt "()"
           | _ -> Format.fprintf fmt " (%a)" (pp_list ", " pp_simple_expr) args) args
    in
    (maybe_paren outer e_app pos pp) fmt (id, args)

  | Eappt (Foperator _, _, _) -> assert false

  | Eappt (Fident id, ts, args) -> begin
      let pp fmt (id, args) =
        Format.fprintf fmt "%a%a%a"
          pp_lid id
          (fun fmt ts ->
             match ts with
             | [] -> Format.fprintf fmt ""
             | _  -> Format.fprintf fmt "<%a>" (pp_list ", " pp_type) ts) ts
          (fun fmt args ->
             match args with
             | [] -> Format.fprintf fmt "()"
             | _ -> Format.fprintf fmt " (%a)" (pp_list ", " pp_simple_expr) args) args
      in
      (maybe_paren outer e_app pos pp) fmt (id, args)
    end

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

  | Etransfer tr ->
    let pp fmt = function
      | TTsimple (x, dst)               -> Format.fprintf fmt "transfer %a to %a" pp_simple_expr x pp_simple_expr dst
      | TTcontract (x, dst, id, t, arg) -> Format.fprintf fmt "transfer %a to %a call %a<%a>(%a)" pp_simple_expr x pp_simple_expr dst pp_id id pp_type t pp_simple_expr arg
      | TTentry (x, id, arg)            -> Format.fprintf fmt "transfer %a to entry %a(%a)" pp_simple_expr x pp_id id pp_simple_expr arg
      | TTentry2 (x, ida, arga, id, arg)-> Format.fprintf fmt "transfer %a to entry entry %a(%a).%a(%a)" pp_simple_expr x pp_id ida pp_simple_expr arga pp_id id pp_simple_expr arg
      | TTself (x, id, args)            -> Format.fprintf fmt "transfer %a to entry self.%a(%a)" pp_simple_expr x pp_id id (pp_list "," pp_simple_expr) args
      | TToperation x                   -> Format.fprintf fmt "transfer %a" pp_simple_expr x
    in
    (maybe_paren outer e_default pos pp) fmt tr

  | Edetach (id, x, f) ->
    Format.fprintf fmt "detach %a as %a : %a" pp_simple_expr x pp_id id pp_simple_expr f

  | Edorequire (x, y) ->

    let pp fmt (x, y) =
      Format.fprintf fmt "do_require (%a, %a)"
        pp_simple_expr x
        pp_simple_expr y
    in
    (maybe_paren outer e_default pos pp) fmt (x, y)

  | Edofailif (x, y) ->

    let pp fmt (x, y) =
      Format.fprintf fmt "do_fail_if (%a, %a)"
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

  | Efailsome x ->

    let pp fmt x =
      Format.fprintf fmt "fail_some (%a)"
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
        | ONone None -> Format.fprintf fmt "none"
        | ONone (Some t) -> Format.fprintf fmt "none<%a>" pp_type t
      in
      pp_option_ fmt x
    in
    (maybe_paren outer e_default pos pp) fmt x

  | Eor x ->

    let pp fmt x =
      let pp_or_ fmt x =
        let pp_ot fmt = function
          | None -> Format.fprintf fmt "_"
          | Some t -> pp_type fmt t
        in
        match x with
        | Oleft  (ot, t, x) -> Format.fprintf fmt "left<%a, %a>(%a)"  pp_ot ot pp_type t pp_simple_expr x
        | Oright (t, ot, x) -> Format.fprintf fmt "right<%a, %a>(%a)" pp_type t pp_ot ot pp_simple_expr x
      in
      pp_or_ fmt x
    in
    (maybe_paren outer e_default pos pp) fmt x

  | Elambda (rt, id, at, e) ->

    let pp fmt (rt, id, at, e) =
      Format.fprintf fmt "lambda%a(%a -> @[%a@])"
        (pp_option (fun fmt -> Format.fprintf fmt "<%a>" pp_type)) rt
        (fun fmt (id, at) ->
           match at with
           | Some at -> Format.fprintf fmt "(%a : %a)" pp_id id pp_type at
           | None -> pp_id fmt id) (id, at)
        pp_simple_expr e

    in
    (maybe_paren outer e_default pos pp) fmt (rt, id, at, e)

  | Eassign (op, lhs, rhs) ->

    let prec = get_prec_from_assignment_operator op in
    let pp fmt (op, lhs, rhs) =
      Format.fprintf fmt "%a %a %a"
        (pp_expr prec PLeft) lhs
        pp_assignment_operator_expr op
        (pp_expr prec PRight) rhs
    in
    (maybe_paren outer prec pos pp) fmt (op, lhs, rhs)

  | Eassignopt (lhs, rhs, fa) ->
    let prec = get_prec_from_assignment_operator ValueAssign in
    let pp fmt (lhs, rhs, fa) =
      Format.fprintf fmt "%a ?:= %a : %a"
        (pp_expr prec PLeft) lhs
        (pp_expr prec PRight) rhs
        (pp_expr prec PRight) fa
    in
    (maybe_paren outer prec pos pp) fmt (lhs, rhs, fa)

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

  | Efold (x, id, e) ->
    let pp fmt (x, id, e) =
      Format.fprintf fmt "fold (%a, %a -> (@[%a@]))@\n"
        (pp_expr e_default PNone) x
        pp_id id
        (pp_expr e_default PNone) e
    in
    (maybe_paren outer e_default pos pp) fmt (x, id, e)

  | Emap (x, id, e) ->
    let pp fmt (x, id, e) =
      Format.fprintf fmt "map (%a, %a -> (@[%a@]))"
        (pp_expr e_default PNone) x
        pp_id id
        (pp_expr e_default PNone) e
    in
    (maybe_paren outer e_default pos pp) fmt (x, id, e)

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

  | Efor (fid, expr, body) ->

    let pp fmt (fid, expr, body) =
      Format.fprintf fmt "for %a in %a do@\n  @[%a@]@\ndone"
        (fun fmt fid ->
           match unloc fid with
           | FIsimple i -> pp_id fmt i
           | FIdouble (x, y) -> Format.fprintf fmt "(%a, %a)" pp_id x pp_id y) fid
        (pp_expr e_default PNone) expr
        (pp_expr e_for PNone) body
    in
    (maybe_paren outer e_default pos pp) fmt (fid, expr, body)

  | Eiter (id, a, b, body) ->

    let pp fmt (id, a, b, body) =
      Format.fprintf fmt "iter %a %ato %a do@\n  @[%a@]@\ndone"
        pp_id id
        (pp_option (fun fmt -> Format.fprintf fmt "from %a " (pp_expr e_default PNone))) a
        (pp_expr e_default PNone) b
        (pp_expr e_for PNone) body
    in
    (maybe_paren outer e_default pos pp) fmt (id, a, b, body)

  | Ewhile (cond, body) ->

    let pp fmt (cond, body) =
      Format.fprintf fmt "while %a do@\n  @[%a@]@\ndone"
        (pp_expr e_default PNone) cond
        (pp_expr e_for PNone) body
    in
    (maybe_paren outer e_default pos pp) fmt (cond, body)

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
      | Some ({pldesc= Ttuple _; _}, _) -> pp_paren
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

  | Evar (id, t, e, c) ->

    let pp fmt (id, t, e) =
      let f =
        match t with
        | Some ({pldesc= Ttuple _; _}, _) -> pp_paren
        | _ -> pp_neutral
      in
      Format.fprintf fmt "%s %a%a = %a"
        (if c then "const" else "var")
        pp_id id
        (pp_option (pp_prefix " : " (f pp_type))) t
        (pp_expr e_in PLeft) e
    in
    (maybe_paren outer e_default pos pp) fmt (id, t, e)

  | Evaropt (id, t, e, f, c) ->

    let pp fmt (id, t, e, fa) =
      let f =
        match t with
        | Some ({pldesc= Ttuple _; _}, _) -> pp_paren
        | _ -> pp_neutral
      in
      Format.fprintf fmt "%s %a%a ?= %a%a"
        (if c then "const" else "var")
        pp_id id
        (pp_option (pp_prefix " : " (f pp_type))) t
        (pp_expr e_in PLeft) e
        (pp_option (fun fmt x -> Format.fprintf fmt " : %a" (pp_expr e_in PLeft) x)) fa
    in
    (maybe_paren outer e_default pos pp) fmt (id, t, e, f)

  | Eunpack (t, arg) ->
    let pp fmt (t, arg) =
      Format.fprintf fmt "unpack<%a>(%a)"
        pp_type t
        (pp_expr e_default PNone) arg
    in
    (maybe_paren outer e_colon pos pp) fmt (t, arg)

  | Eemit (t, arg) ->
    let pp fmt (t, arg) =
      Format.fprintf fmt "emit<%a>(%a)"
        pp_type t
        (pp_expr e_default PNone) arg
    in
    (maybe_paren outer e_colon pos pp) fmt (t, arg)

  | Eentrypoint (t, a, b, c) ->
    let pp fmt (t, a, b, c) =
      Format.fprintf fmt "%s<%a>(%a, %a%a)"
        (if Option.is_some c then "require_entrypoint" else "get_entrypoint")
        pp_type t
        (pp_expr e_default PNone) a
        (pp_expr e_default PNone) b
        (pp_option (fun fmt x -> Format.fprintf fmt ", %a" (pp_expr e_default PNone) x)) c
    in
    (maybe_paren outer e_colon pos pp) fmt (t, a, b, c)

  | Ecallview (t, a, b, c) ->
    let pp fmt (t, a, b, c) =
      Format.fprintf fmt "call_view<%a>(%a, %a, %a)"
        pp_type t
        (pp_expr e_default PNone) a
        (pp_expr e_default PNone) b
        (pp_expr e_default PNone) c
    in
    (maybe_paren outer e_colon pos pp) fmt (t, a, b, c)

  | Eself x -> Format.fprintf fmt "(self.%a)" pp_id x

  | Eany -> Format.fprintf fmt "any"

  | Enothing -> Format.fprintf fmt "()"

  | Eunit -> Format.fprintf fmt "Unit"

  | Etz_expr s -> Format.pp_print_string fmt s

  | Einvalid -> Format.fprintf fmt "(* invalid expr *)"

  | Emicheline micheline -> begin
      let printable_micheline : Micheline_printer.node = Micheline_tools.pt_to_micheline micheline in
      Format.fprintf fmt "michelson @[%a@]" Micheline_printer.print_expr printable_micheline
    end

  | Elambda_michelson (it, rt, body) -> begin
      let printable_micheline : Micheline_printer.node = Micheline_tools.pt_to_micheline body in
      Format.fprintf fmt "lambda_michelson<%a, %a>(@[%a@])"
        pp_type it
        pp_type rt
        Micheline_printer.print_expr printable_micheline
    end

and pp_else fmt (e : expr option) =
  match e with
  | None -> ()
  | Some x -> Format.fprintf fmt " else (%a)" (pp_expr e_else PRight) x

and pp_literal fmt lit =
  match lit with
  | Lint                  n -> Format.fprintf fmt "%si" (Big_int.string_of_big_int n)
  | Lnat                  n -> Format.fprintf fmt "%s" (Big_int.string_of_big_int n)
  | Ldecimal              n -> Format.fprintf fmt "%s" n
  (* | Lrational (d, n) -> Format.fprintf fmt "%s div %s"
                          (Big_int.string_of_big_int d)
                          (Big_int.string_of_big_int n) *)
  | Ltz                   n -> Format.fprintf fmt "%stz"  n
  | Lmtz                  n -> Format.fprintf fmt "%smtz" n
  | Lutz                  n -> Format.fprintf fmt "%sutz" n
  | Laddress              a -> Format.fprintf fmt "@%s" a
  | Lstring               s -> Format.fprintf fmt "\"%s\"" s
  | Lbool                 b -> Format.fprintf fmt "%s" (if b then "true" else "false")
  | Lduration             d -> Format.fprintf fmt "%s" d
  | Ldate                 d -> Format.fprintf fmt "%s" d
  | Lbytes                s -> Format.fprintf fmt "0x%s" s
  | Lpercent              n -> Format.fprintf fmt "%s%%" n

and pp_ident_ident fmt a =
  match a with
  | (x, y) ->
    Format.fprintf fmt "%a%a"
      (pp_option (pp_postfix "." pp_id)) x
      pp_id y

and pp_ident_typ fmt a =
  match a with
  | (x, y) ->
    Format.fprintf fmt "%a%a"
      pp_id x
      (pp_prefix " : " pp_type) y

and pp_ident_quant fmt a =
  match a with
  | (x, y) ->
    Format.fprintf fmt "%a%a"
      pp_id x
      (pp_prefix " in " pp_type) y

and pp_fun_ident_typ fmt (arg : lident_typ) =
  match arg with
  | (x, y) ->
    Format.fprintf fmt "%a : %a"
      pp_id x
      pp_type y

and pp_fun_args fmt args =
  Format.fprintf fmt " (%a)"
    (pp_list ", " pp_fun_ident_typ) args

and pp_record_expr_internal fmt (scope, l) =
  Format.fprintf fmt "%a{%a}"
    pp_id_scope scope
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
  | Ffield (id, typ, dv) ->
    Format.fprintf fmt "%a : %a%a"
      pp_id id
      pp_type typ
      (pp_option (pp_prefix " = " (pp_expr e_equal PRight))) dv

(* -------------------------------------------------------------------------- *)
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
    Format.fprintf fmt "variable %a : %a%a"
      pp_id id
      pp_type typ
      (pp_option (pp_prefix " = " (pp_expr e_equal PRight))) dv

(* -------------------------------------------------------------------------- *)

let pp_map_kind fmt x =
  match x with
  | MKMap            -> Format.fprintf fmt "map"
  | MKBigMap         -> Format.fprintf fmt "big_map"
  | MKIterableBigMap -> Format.fprintf fmt "iterable_big_map"

let pp_asset_option fmt opt =
  match opt with
  | AOidentifiedby ids -> Format.fprintf fmt "identified by %a" (pp_list " " pp_id) ids
  | AOsortedby id      -> Format.fprintf fmt "sorted by %a" pp_id id
  | AOtoMapKind x      -> Format.fprintf fmt "to %a" pp_map_kind x

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

let pp_enum_option fmt = function
  | EOinitial ->
    Format.fprintf fmt "initial"

let pp_ident_state fmt item =
  match item with
  | (id, lt, opts) ->
    Format.fprintf fmt "%a%a%a"
      pp_id id
      (fun fmt l ->
         if List.length l = 0
         then ()
         else (Format.fprintf fmt " <%a>" (pp_list " * " pp_type) l)
      ) lt
      (pp_prefix " " (pp_list " " pp_enum_option)) opts

let pp_asset_post_option fmt (apo : asset_post_option) =
  match apo with
  | APOinit l ->
    Format.fprintf fmt " initialized by {@\n  @[%a@]@\n}"
      (pp_list ";@\n"
         (fun fmt x ->
            match unloc x with
              Erecord (scope, l) -> pp_record_expr_internal fmt (scope, l)
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

let pp_function fmt (f : s_function) =
  Format.fprintf fmt "%s%s %a %a%a {@\n%a@\n}@\n"
    (match f.view_visibility with | VVonoffchain -> "onchain offchain "  | VVonchain -> "onchain "  | VVoffchain -> "offchain "  | VVnone -> "")
    (if f.getter then "getter" else if f.view then "view" else "function")
    pp_id f.name
    pp_fun_args f.args
    (pp_option (pp_prefix " : " pp_type)) f.ret_t
    (pp_expr e_equal PRight) f.body

let pp_entry_properties fmt (props : entry_properties) =
  if (not (fst props.accept_transfer))
  then begin
    Format.fprintf fmt "no transfer%a@\n"
      (pp_option (fun fmt o -> Format.fprintf fmt " otherwise %a" (pp_expr e_default PNone) o )) (snd props.accept_transfer)
  end;
  map_option (fun (e, o) ->
      Format.fprintf fmt "sourced by %a%a@\n"
        (pp_expr e_default PNone) e
        (pp_option (fun fmt o -> Format.fprintf fmt " otherwise %a" (pp_expr e_default PNone) o )) o
    ) props.sourcedby;
  map_option (fun (e, o) ->
      Format.fprintf fmt "called by %a%a@\n"
        (pp_expr e_default PNone) e
        (pp_option (fun fmt o -> Format.fprintf fmt " otherwise %a" (pp_expr e_default PNone) o )) o
    ) props.calledby;
  map_option (fun (x, o) ->
      Format.fprintf fmt "state is %a%a@\n"
        pp_id x
        (pp_option (fun fmt o -> Format.fprintf fmt " otherwise %a" (pp_expr e_default PNone) o )) o
    ) props.state_is;
  let pp_rf s1 s2 fmt l =
    Format.fprintf fmt "%s {@\n  @[%a@]@\n}@\n"
      s1
      (pp_list ";@\n" (fun fmt (id, e, f) ->
           Format.fprintf fmt "%a: %a%a"
             pp_id id
             (pp_expr e_default PNone) e
             (pp_option (fun fmt x -> Format.fprintf fmt " %s %a" s2 (pp_expr e_default PNone) x)) f
         )) l
  in
  let pp_cf fmt l =
    Format.fprintf fmt "constant {@\n  @[%a@]@\n}@\n"
      (pp_list ";@\n" (fun fmt (id, e, f) ->
           Format.fprintf fmt "%a %s %a%a"
             pp_id id
             (if Option.is_some f then "?is" else "is")
             (pp_expr e_default PNone) e
             (pp_option (fun fmt x -> Format.fprintf fmt " otherwise %a" (pp_expr e_default PNone) x)) f
         )) l
  in
  pp_option pp_cf fmt props.constants;
  pp_option (pp_rf "require" "otherwise") fmt props.require;
  pp_option (pp_rf "fail if" "with") fmt props.failif;
  (pp_list "@\n" pp_function) fmt (List.map unloc props.functions)

let pp_transition fmt (to_, conditions, effect) =
  Format.fprintf fmt "to %a%a%a@\n"
    pp_id to_
    (pp_option (
        fun fmt e ->
          Format.fprintf fmt " when { %a }"
            (pp_expr e_default PNone) e)) conditions
    (pp_option (fun fmt e ->
         Format.fprintf fmt "@\nwith effect {@\n  @[%a@]@\n}"
           pp_simple_expr e)) effect

let pp_parameter fmt (id, ty, dv, c) =
  Format.fprintf fmt "%a%a : %a%a"
    (pp_do_if c (fun fmt _ -> pp_str fmt "const ")) ()
    pp_id id
    pp_type ty
    (pp_option (fun fmt x -> Format.fprintf fmt " = %a" pp_simple_expr x)) dv

let pp_parameters fmt = function
  | None -> ()
  | Some xs -> Format.fprintf fmt "(%a)" (pp_list ", " (fun fmt x -> pp_parameter fmt (unloc x))) (unloc xs)

let pp_metadata fmt (m : metadata) =
  match m with
  | Muri  v -> Format.fprintf fmt "\"%s\"" (unloc v)
  | Mjson v -> Format.fprintf fmt "`%s`" (unloc v)

let rec pp_declaration fmt { pldesc = e; _ } =
  let is_empty_entry_properties_opt (ap : entry_properties) (a : 'a option) =
    match ap.sourcedby, ap.calledby, ap.require, ap.functions, a with
    | None, None, None, [], None -> true
    | _ -> false in
  match e with
  | Darchetype (id, ps, m) ->
    Format.fprintf fmt "archetype %a%a%a"
      pp_id id
      pp_parameters ps
      (pp_option (fun fmt x -> Format.fprintf fmt "@\nwith metadata %a" pp_metadata x)) m

  | Dimport (Some id, path) ->
    Format.fprintf fmt "import %a from \"%a\""
      pp_id id
      pp_id path

  | Dimport (None, path) ->
    Format.fprintf fmt "import \"%a\""
      pp_id path

  | Dvariable (id, typ, dv, kind) ->
    Format.fprintf fmt "%a %a : %a%a"
      pp_str (match kind with | VKvariable -> "variable" | VKconstant -> "constant")
      pp_id id
      pp_type typ
      (pp_option (pp_prefix " = " (pp_expr e_equal PRight))) dv

  | Denum (id, ids) ->
    Format.fprintf fmt "%a%a"
      (fun fmt id -> (
           match id with
           | EKstate -> Format.fprintf fmt "states"
           | EKenum id -> Format.fprintf fmt "enum %a" pp_id id
         )) id
      (fun fmt ids -> (
           match ids with
           | [] -> ()
           | l -> Format.fprintf fmt " =@\n  @[%a@]"
                    (pp_list "@\n" (pp_prefix "| " pp_ident_state)) l
         )) ids

  | Dasset (id, fields, opts, apo, ops) ->
    Format.fprintf fmt "asset%a %a%a%a%a@\n"
      (pp_option pp_asset_operation) ops
      pp_id id
      (pp_prefix " " (pp_list " @," pp_asset_option)) opts
      (pp_do_if (List.length fields > 0) ((fun fmt -> Format.fprintf fmt " {@\n  @[%a@]@\n}" (pp_list ";@\n" pp_field)))) fields
      (pp_list " " pp_asset_post_option) apo

  | Drecord (id, fields, pos) ->
    Format.fprintf fmt "record %a %a%a@\n"
      pp_id id
      (pp_do_if (List.length fields > 0) ((fun fmt -> Format.fprintf fmt " {@\n  @[%a@]@\n}" (pp_list ";@\n" pp_field)))) fields
      (pp_option (fun fmt x -> Format.fprintf fmt " as (%a)" (pp_expr e_default PNone) x)) pos

  | Devent (id, fields, pos) ->
    Format.fprintf fmt "event %a %a%a@\n"
      pp_id id
      (pp_do_if (List.length fields > 0) ((fun fmt -> Format.fprintf fmt " {@\n  @[%a@]@\n}" (pp_list ";@\n" pp_field)))) fields
      (pp_option (fun fmt x -> Format.fprintf fmt " as (%a)" (pp_expr e_default PNone) x)) pos

  | Dentry (id, args, props, code) ->
    Format.fprintf fmt "entry %a%a%a"
      pp_id id
      pp_fun_args args
      (pp_do_if (not (is_empty_entry_properties_opt props code))
         (fun fmt x ->
            let pr, cod = x in
            Format.fprintf fmt " {@\n  @[%a%a@]@\n}"
              pp_entry_properties pr
              (pp_option (fun fmt code ->
                   Format.fprintf fmt "effect {@\n  @[%a@]@\n}@\n"
                     (pp_expr e_default PNone) code
                 )) cod)) (props, code)

  | Dtransition (id, args, from, props, trs) ->
    Format.fprintf fmt "transition %a%a%a"
      pp_id id
      pp_fun_args args
      (fun fmt (pr, ts) ->
         Format.fprintf fmt " {@\n  @[%a%a%a@]@\n}"
           (pp_do_if (not (is_empty_entry_properties_opt props None)) pp_entry_properties) pr
           (fun fmt from -> Format.fprintf fmt "from %a@\n" pp_simple_expr from) from
           (pp_list "@\n" pp_transition) ts) (props, trs)

  | Dnamespace (id, ds) ->
    Format.fprintf fmt "namespace %a {@\n  @[%a@]@\n}"
      pp_id id
      (pp_list "\n" pp_declaration) ds

  | Dfunction f ->
    Format.fprintf fmt "%a"
      pp_function f

  | Dtype (id, t) ->
    Format.fprintf fmt "type %a = %a"
      pp_id id
      pp_type t

  | Dinvalid ->
    Format.fprintf fmt "(* invalid declaration *)"

(* -------------------------------------------------------------------------- *)
let pp_archetype fmt { pldesc = es; _ } =
  Format.fprintf fmt "%a@\n" (pp_list "@\n@\n" pp_declaration) es

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a" pp x

(* -------------------------------------------------------------------------- *)
let type_to_str        = string_of__of_pp pp_type
let declaration_to_str = string_of__of_pp pp_declaration
let archetype_to_str   = string_of__of_pp pp_archetype
