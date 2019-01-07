(* -------------------------------------------------------------------- *)
open Core
open Location
open Ast

(* -------------------------------------------------------------------- *)
let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp

(* -------------------------------------------------------------------- *)
let pp_id fmt (id : lident) =
  Format.fprintf fmt "%s" (unloc id)

(* -------------------------------------------------------------------- *)
let pp_option pp fmt x =
  match x with None -> () | Some x -> pp fmt x

(* -------------------------------------------------------------------- *)
let pp_enclose pre post pp fmt x =
  Format.fprintf fmt "%(%)%a%(%)" pre pp x post

let pp_prefix pre pp fmt x =
  pp_enclose pre "" pp fmt x

let pp_postfix post pp fmt x =
  pp_enclose "" post pp fmt x

let pp_paren pp fmt x =
  pp_enclose "(" ")" pp fmt x

(* -------------------------------------------------------------------- *)
let container_to_str c =
match c with
  | Collection -> "collection"
  | Queue -> "queue"
  | Stack -> "stack"
  | Set -> "set"
  | Subset -> "subset"
  | Partition -> "partition"

let pp_container fmt c =
 Format.fprintf fmt "%s" (container_to_str c)

let pp_type fmt { pldesc = e } =
  match e with
  | Tref x -> Format.fprintf fmt "%a" pp_id x
  | Tcontainer (x, y) -> Format.fprintf fmt "%a %a" pp_id x pp_container y


(* -------------------------------------------------------------------- *)
let logical_operator_to_str op =
match op with
  | And   -> "and"
  | Or    -> "or"
  | Imply -> "->"
  | Equiv -> "<->"

let pp_logical_operator fmt op =
 Format.fprintf fmt "%s" (logical_operator_to_str op)

let comparison_operator_to_str op =
match op with
  | Equal  -> "="
  | Nequal -> "<>"
  | Gt     -> ">"
  | Ge     -> ">="
  | Lt     -> "<"
  | Le     -> "<="

let pp_comparison_operator fmt op =
 Format.fprintf fmt "%s" (comparison_operator_to_str op)

let arithmetic_operator_to_str op =
match op with
  | Plus   -> "+"
  | Minus  -> "-"
  | Mult   -> "*"
  | Div    -> "/"

let pp_arithmetic_operator fmt op =
 Format.fprintf fmt "%s" (arithmetic_operator_to_str op)

let unary_operator_to_str op =
match op with
  | Uplus   -> "+"
  | Uminus  -> "-"

let pp_unary_operator fmt op =
 Format.fprintf fmt "%s" (unary_operator_to_str op)

let assignment_operator_to_str op =
match op with
  | Assign      -> ":="
  | PlusAssign  -> "+="
  | MinusAssign -> "-="
  | MultAssign  -> "*="
  | DivAssign   -> "/="
  | AndAssign   -> "&="
  | OrAssign    -> "|="

let pp_assignment_operator fmt op =
 Format.fprintf fmt "%s" (assignment_operator_to_str op)

let quantifier_to_str op =
match op with
  | Forall -> "forall"
  | Exists -> "exists"

let pp_quantifier fmt op =
 Format.fprintf fmt "%s" (quantifier_to_str op)

let rec pp_expr fmt { pldesc = e } =
  match e with
  | Eterm id ->
      Format.fprintf fmt "%a"
        pp_id id

  | Ecall (e, args) ->
      Format.fprintf fmt "%a %a"
        pp_expr e
        (pp_list "@," pp_expr) args

  | Eliteral x ->
      Format.fprintf fmt "%a"
        pp_literal x

  | Enamespace (id, x) ->
      Format.fprintf fmt "%a::%a"
        pp_id id
        pp_expr x

  | Edot (lhs, rhs) ->
      Format.fprintf fmt "%a.%a"
        pp_expr lhs pp_expr rhs

  | Efun (ids, x) ->
      Format.fprintf fmt "fun %a => %a"
        (pp_list " " pp_id) ids
        pp_expr x

  | Elogical (op, lhs, rhs) ->
      Format.fprintf fmt "%a %a %a"
        pp_expr lhs
        pp_logical_operator op
        pp_expr rhs

  | Enot e ->
      Format.fprintf fmt "not %a"
        pp_expr e

  | Ecomparison (op, lhs, rhs) ->
      Format.fprintf fmt "%a %a %a"
        pp_expr lhs
        pp_comparison_operator op
        pp_expr rhs

  | Earithmetic (op, lhs, rhs) ->
      Format.fprintf fmt "%a %a %a"
        pp_expr lhs
        pp_arithmetic_operator op
        pp_expr rhs

  | Eunary (op, e) ->
      Format.fprintf fmt "%a %a"
       pp_unary_operator op
       pp_expr e

  | Earray values ->
      Format.fprintf fmt "[%a]"
        (pp_list "@," pp_expr) values

  | EassignFields l ->
      Format.fprintf fmt "{%a}"
        (pp_list " " pp_assignment_field) l

  | Equantifier (q, id, t, body) ->
      Format.fprintf fmt "%a %a : %a, %a"
        pp_quantifier q
        pp_id id
        pp_expr t
        pp_expr body

  | Eletin (id, e, body) ->
      Format.fprintf fmt "let %a = %a in %a"
        pp_id id
        pp_expr e
        pp_expr body

and pp_literal fmt lit =
  match lit with
  | Lnumber n -> Format.fprintf fmt "%d" n
  | Lfloat  f -> Format.fprintf fmt "%f" f
  | Lstring s -> Format.fprintf fmt "\"%s\"" s
  | Lbool   b -> Format.fprintf fmt "%s" (if b then "true" else "false")

and pp_assignment_field fmt f =
  match f with
  | AassignField (op, id, e) ->
      Format.fprintf fmt "%a %a %a"
        pp_expr id pp_assignment_operator op pp_expr e

(* -------------------------------------------------------------------- *)
let pp_extension fmt { pldesc = e } =
  match e with
  | Eextension (id, args) ->
        Format.fprintf fmt "[%%%a%a]"
        pp_id id
        (pp_option (pp_prefix " " (pp_list " " pp_expr))) args


(* -------------------------------------------------------------------- *)
let pp_field fmt { pldesc = f } =
  match f with
  | Ffield (id, typ, _) -> Format.fprintf fmt "%a : %a;" pp_id id pp_type typ


(* -------------------------------------------------------------------- *)
let rec pp_instr fmt { pldesc = s } =
  match s with
  | Iassign (op, lhs, rhs) ->
      Format.fprintf fmt "%a %a %a"
        pp_expr lhs
        pp_assignment_operator op
        pp_expr rhs

  | Iletin (id, e, body) ->
      Format.fprintf fmt "let %a = %a in %a"
        pp_id id
        pp_expr e
        (pp_list "@," pp_instr) body

  | Iif (cond, _then, _else) ->
      Format.fprintf fmt "if %a then {%a}%a"
        pp_expr cond
        (pp_list "@," pp_instr) _then
        (pp_option (pp_enclose " else {" "}" (pp_list "@," pp_instr))) _else

  | Ifor (id, expr, body) ->
      Format.fprintf fmt "for (%a in %a) {%a}"
        pp_id id pp_expr expr (pp_list "@," pp_instr) body

  | Itransfer (x, back, to_value) ->
      Format.fprintf fmt "transfer%s %a%a"
        (if back then " back" else "")
        pp_expr x
        (pp_option (pp_prefix " to " pp_expr)) to_value

  | Itransition x ->
      Format.fprintf fmt "transition to %a"
        pp_expr x

  | Icall e ->
      Format.fprintf fmt "%a"
        pp_expr e

  | Iassert e ->
      Format.fprintf fmt "assert (%a)"
        pp_expr e

  | Ibreak ->
      Format.fprintf fmt "break"


(* -------------------------------------------------------------------- *)
let pp_transitem fmt { pldesc = t } =
  match t with
  | Targs (fields, exts) ->
      Format.fprintf fmt "args%a = {@[<v 2>]@,%a@]}\n"
        (pp_option (pp_list " " pp_extension)) exts
        (pp_list "@," pp_field) fields

  | Tcalledby (e, exts) ->
      Format.fprintf fmt "called by%a %a;\n"
        (pp_option (pp_list " " pp_extension)) exts
        pp_expr e

  | Tensure (e, exts) ->
      Format.fprintf fmt "ensure%a: %a;\n"
        (pp_option (pp_list " " pp_extension)) exts
        pp_expr e

  | Tcondition (e, exts) ->
      Format.fprintf fmt "condition%a: %a;\n"
        (pp_option (pp_list " " pp_extension)) exts
        pp_expr e

  | Ttransferred (e, exts) ->
      Format.fprintf fmt "transferred%a: %a;\n"
        (pp_option (pp_list " " pp_extension)) exts
        pp_expr e

  | Ttransition (from, _to, id, exts) ->
      Format.fprintf fmt "transition%a%a from %a to %a;"
        (pp_option (pp_list " " pp_extension)) exts
        (pp_option (pp_prefix " " pp_expr)) id
        pp_expr from
        pp_expr _to

  | Taction (instrs, exts) ->
      Format.fprintf fmt "action%a:\n %a"
        (pp_option (pp_list " " pp_extension)) exts
        (pp_list "@," pp_instr) instrs

(* -------------------------------------------------------------------- *)
let state_option_to_str opt =
match opt with
  | SOinitial  -> "initial"

let pp_state_option fmt opt =
 Format.fprintf fmt "%s" (state_option_to_str opt)

let pp_ident_state_option fmt item =
  match item with
  | (id, opts) ->
      Format.fprintf fmt "%a%a"
      pp_id id
      (pp_option (pp_prefix " " (pp_list " " pp_state_option))) opts

let pp_value_option fmt opt =
match opt with
  | VOfrom e -> Format.fprintf fmt "from %a" pp_expr e
  | VOto   e -> Format.fprintf fmt "to %a"   pp_expr e

let pp_asset_option fmt opt =
match opt with
  | AOasrole -> Format.fprintf fmt "as role"
  | AOidentifiedby id -> Format.fprintf fmt "identified by %a" pp_id id
  | AOsortedby id  -> Format.fprintf fmt "sorted by %a" pp_id id

let rec pp_declaration fmt { pldesc = e } =
  match e with
  | Duse id ->
      Format.fprintf fmt "use %a\n" pp_id id

  | Dmodel id ->
      Format.fprintf fmt "model %a\n" pp_id id

  | Dconstant (id, typ, exts) ->
      Format.fprintf fmt "constant%a %a %a\n"
          (pp_option (pp_list " " pp_extension)) exts
          pp_id id
          pp_id typ

  | Dvalue (id, typ, opts, dv, exts) ->
      Format.fprintf fmt "value%a %a %a%a%a\n"
          (pp_option (pp_list " " pp_extension)) exts
          pp_id id
          pp_id typ
          (pp_option (pp_prefix " " (pp_list " " pp_value_option))) opts
          (pp_option (pp_prefix " := " pp_expr)) dv

  | Drole (id, dv, exts) ->
      Format.fprintf fmt "role%a %a%a\n"
          (pp_option (pp_list " " pp_extension)) exts
           pp_id id
          (pp_option (pp_prefix " := " pp_expr)) dv

  | Denum (id, ids) ->
      Format.fprintf fmt "enum %a =\n@[<v 2>%a@]"
        pp_id id
        (pp_list "\n" (pp_prefix "| " pp_id)) ids

  | Dstates (id, ids) ->
      Format.fprintf fmt "states%a\n@[<v 2>%a@]"
        (pp_option (pp_enclose " " " =" pp_id)) id
        (pp_list "\n" (pp_prefix "| " pp_ident_state_option)) ids

  | Dasset (id, fields, cs, opts, init) ->
      Format.fprintf fmt "asset %a%a = {@[<v 2>]@,%a}%a%a\n"
        pp_id id
        (pp_option (pp_prefix " " (pp_list " @," pp_asset_option))) opts
        (pp_option (pp_list "@," pp_field)) fields
        (pp_option (pp_enclose " with {" "}" (pp_list " @," pp_expr))) cs
        (pp_option (pp_enclose " initialized by {" "}" (pp_list "@," pp_instr))) init

  | Dassert e ->
      Format.fprintf fmt "assert (%a)\n"
        pp_expr e

  | Dobject (id, e, exts) ->
      Format.fprintf fmt "object%a %a %a\n"
        (pp_option (pp_list " " pp_extension)) exts
        pp_id id
        pp_expr e

  | Dkey (id, e, exts) ->
      Format.fprintf fmt "key%a %a of %a\n"
        (pp_option (pp_list " " pp_extension)) exts
        pp_id id
        pp_expr e

  | Dtransition (id, from, _to, items, exts) ->
      Format.fprintf fmt "transition%a %a from %a to %a={%a}\n"
        (pp_option (pp_list " " pp_extension)) exts
        pp_id id
        pp_expr from
        pp_expr _to
        (pp_list "@," pp_transitem) items

  | Dtransaction (id, items, exts) ->
      Format.fprintf fmt "transaction%a %a = {@[<v 2>]@,%a@}\n"
        (pp_option (pp_list " " pp_extension)) exts
        pp_id id
        (pp_list "@," pp_transitem) items

  | Dextension (id, args) ->
      Format.fprintf fmt "%%%a%a\n"
        pp_id id
        (pp_option (pp_prefix " " (pp_list " " pp_expr))) args

  | Dnamespace (id, ds) ->
      Format.fprintf fmt "namespace %a { %a }\n"
         pp_id id
        (pp_list "@[<v 2>]@,\n" pp_declaration) ds


(* -------------------------------------------------------------------- *)
let pp_model fmt { pldesc = m } =
  match m with
| Mmodel es ->
  Format.fprintf fmt "%a" (pp_list "@,\n" pp_declaration) es
| Mmodelextension (id, ds, es) ->
  Format.fprintf fmt "model extension %a (%a) = {%a}"
     pp_id id
    (pp_list "@,\n" pp_declaration) ds
    (pp_list "@,\n" pp_declaration) es


(* -------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a" pp x

(* -------------------------------------------------------------------- *)
let type_to_str  = string_of__of_pp pp_type
let expr_to_str  = string_of__of_pp pp_expr
let extension_to_str = string_of__of_pp pp_extension
let field_to_str  = string_of__of_pp pp_field
let instr_to_str = string_of__of_pp pp_instr
let transitem_to_str = string_of__of_pp pp_transitem
let declaration_to_str = string_of__of_pp pp_declaration
let model_to_str  = string_of__of_pp pp_model
