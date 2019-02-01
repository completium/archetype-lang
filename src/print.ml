(* -------------------------------------------------------------------------- *)
open Core
open Location
open ParseTree

(* -------------------------------------------------------------------------- *)
let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp

(* -------------------------------------------------------------------------- *)
let pp_id fmt (id : lident) =
  Format.fprintf fmt "%s" (unloc id)

(* -------------------------------------------------------------------------- *)
let pp_option pp fmt x =
  match x with None -> () | Some x -> pp fmt x

let pp_enclose pre post pp fmt x =
  Format.fprintf fmt "%(%)%a%(%)" pre pp x post

let pp_prefix pre pp fmt x =
  pp_enclose pre "" pp fmt x

let pp_postfix post pp fmt x =
  pp_enclose "" post pp fmt x

let pp_paren pp fmt x =
  pp_enclose "(" ")" pp fmt x

let pp_if c pp1 pp2 fmt x =
  match c with
  | true  -> Format.fprintf fmt "%a" pp1 x
  | false -> Format.fprintf fmt "%a" pp2 x

let pp_maybe c tx pp fmt x =
  pp_if c (tx pp) pp fmt x

let pp_maybe_paren c pp =
  pp_maybe c pp_paren pp

(* -------------------------------------------------------------------------- *)
type assoc  = Left | Right | NonAssoc
type pos    = Left | Right | Infix | None

let precedences_ =
[
  "=>",  (10,  NonAssoc);
  ",",   (20,  Left);

  ":=",  (30,  NonAssoc);
  "+=",  (30,  NonAssoc);
  "-=",  (30,  NonAssoc);
  "*=",  (30,  NonAssoc);
  "/=",  (30,  NonAssoc);
  "&=",  (30,  NonAssoc);
  "|=",  (30,  NonAssoc);

  "->",  (40,  Right);
  "<->", (50,  NonAssoc);

  "and", (60,  Left);
  "or",  (70,  Left);

  "=",   (80,  NonAssoc);
  "<>",  (80,  NonAssoc);

  ">",   (90,  NonAssoc);
  ">=",  (90,  NonAssoc);
  "<",   (90,  NonAssoc);
  "<=",  (90,  NonAssoc);

  "+",   (100, NonAssoc);
  "-",   (100, NonAssoc);

  "*",   (110, NonAssoc);
  "/",   (110, NonAssoc);

  ".",   (120, Right);

  "::",  (130, NonAssoc);
]

let precedences = Hashtbl.create 0

let () =
List.iter (fun (k, v) -> Hashtbl.add precedences k v) precedences_

let get_precedence name =
try let res = Hashtbl.find precedences name in Some res with Not_found -> None

let maybe_paren outer inner pp =
let c =
match (outer, inner) with
| (_, (_, NonAssoc)) -> true
| _ -> false
in pp_maybe_paren c pp


(* -------------------------------------------------------------------------- *)
let container_to_str c =
match c with
  | Collection -> "collection"
  | Queue      -> "queue"
  | Stack      -> "stack"
  | Set        -> "set"
  | Partition  -> "partition"

let pp_container fmt c =
 Format.fprintf fmt "%s" (container_to_str c)

let rec pp_type fmt { pldesc = e } =
  match e with
  | Tref x ->
      Format.fprintf fmt
        "%a"
         pp_id x

  | Tcontainer (x, y) ->
      Format.fprintf fmt
        "%a %a"
           pp_type x
           pp_container y

  | Tvset (x, y) ->
      Format.fprintf fmt
        "%a %a"
           pp_id x
           pp_type y

  | Tapp (x, y) ->
      Format.fprintf fmt
        "%a -> %a"
           pp_type x
           pp_type y

  | Ttuple l ->
      Format.fprintf fmt
        "(%a)"
           (pp_list " * " pp_type) l


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

let assignment_operator_to_str op =
match op with
  | SimpleAssign -> ":="
  | PlusAssign   -> "+="
  | MinusAssign  -> "-="
  | MultAssign   -> "*="
  | DivAssign    -> "/="
  | AndAssign    -> "&="
  | OrAssign     -> "|="

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
  | Eterm (e, id) ->
      Format.fprintf fmt "%a%a"
        (pp_option (pp_postfix "::" pp_id)) e
         pp_id id
  | Eop op ->
      Format.fprintf fmt "%a"
         pp_operator op

  | Eliteral x ->
      Format.fprintf fmt "%a"
        pp_literal x

  | Earray values ->
      Format.fprintf fmt "[%a]"
        (pp_list ", " pp_expr) values

  | Edot (lhs, rhs) ->
      Format.fprintf fmt "%a.%a"
        pp_expr lhs
        pp_id rhs

  | EassignFields l ->
      Format.fprintf fmt "{%a}"
        (pp_list " " pp_assignment_field) l

  | Eapp ({pldesc = Eop op}, [a; b]) ->
      let _prec = get_precedence (operator_to_str op) in
      Format.fprintf fmt "%a %a %a"
        pp_expr a
        pp_operator op
        pp_expr b

  | Eapp (e, args) ->
      Format.fprintf fmt "%a%a"
        pp_expr e
        pp_args args

  | Etransfer (x, back, to_value) ->
      Format.fprintf fmt "transfer%s %a%a"
        (if back then " back" else "")
        pp_expr x
        (pp_option (pp_prefix " to " pp_expr)) to_value

  | Etransition x ->
      Format.fprintf fmt "transition to %a"
        pp_expr x

  | Eassign (op, lhs, rhs) ->
      Format.fprintf fmt "%a %a %a"
        pp_expr lhs
        pp_assignment_operator op
        pp_expr rhs

  | Eif (cond, then_, else_) ->
      Format.fprintf fmt "if %a then (%a)%a"
        pp_expr cond
        pp_expr then_
        (pp_option (pp_prefix " else " pp_expr)) else_

  | Ebreak ->
      Format.fprintf fmt "break"

  | Efor (id, expr, body, invariants) ->
      Format.fprintf fmt "for (%a in %a)%a (%a)"
        pp_id id
        (pp_option (pp_list "\n" (pp_enclose " invariant (" ")" pp_expr))) invariants
        pp_expr expr
        pp_expr body

  | Eassert e ->
      Format.fprintf fmt "assert (%a)"
        pp_expr e

  | Eseq (x, y) ->
      Format.fprintf fmt "%a, %a"
        pp_expr x
        pp_expr y

  | Efun (id_ts, x) ->
      Format.fprintf fmt "fun %a => %a"
        (pp_list " " pp_ident_typ) id_ts
        pp_expr x

  | Eletin (id_t, e, body) ->
      Format.fprintf fmt "let %a = %a in %a"
        pp_ident_typ id_t
        pp_expr e
        pp_expr body

  | Equantifier (q, id_t, body) ->
      Format.fprintf fmt "%a %a, %a"
        pp_quantifier q
        pp_ident_typ id_t
        pp_expr body


and pp_literal fmt lit =
  match lit with
  | Lnumber   n -> Format.fprintf fmt "%s" (Big_int.string_of_big_int n)
  | Lfloat    f -> Format.fprintf fmt "%f" f
  | Laddress  a -> Format.fprintf fmt "@%s" a
  | Lstring   s -> Format.fprintf fmt "\"%s\"" s
  | Lbool     b -> Format.fprintf fmt "%s" (if b then "true" else "false")
  | Lduration d -> Format.fprintf fmt "%s" d
  | Ldate     d -> Format.fprintf fmt "%s" d

and pp_assignment_field fmt f =
  match f with
  | AassignField (op, id, e) ->
      Format.fprintf fmt "%a %a %a;"
        pp_ident_ident id
        pp_assignment_operator op
        pp_expr e

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
  (pp_option (pp_prefix " : " pp_type)) y

and pp_args fmt args =
match args with
| [] -> Format.fprintf fmt "()"
| _ -> Format.fprintf fmt " %a" (pp_list " " pp_expr) args

(* -------------------------------------------------------------------------- *)
and pp_field fmt { pldesc = f } =
  match f with
  | Ffield (id, typ, dv, exts) ->
      Format.fprintf fmt "%a%a : %a%a;"
        pp_id id
        (pp_option (pp_list " " pp_extension)) exts
        pp_type typ
        (pp_option (pp_prefix " := " pp_expr)) dv

(* -------------------------------------------------------------------------- *)
and pp_extension fmt { pldesc = e } =
  match e with
  | Eextension (id, args) ->
        Format.fprintf fmt "[%%%a%a]"
        pp_id id
        (pp_option (pp_prefix " " (pp_list " " pp_expr))) args

(* -------------------------------------------------------------------------- *)
let pp_transitem fmt { pldesc = t } =
  match t with
  | Targs (fields, exts) ->
      Format.fprintf fmt "args%a = {%a}\n"
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

  | Ttransition (from, _to, id, exts) ->
      Format.fprintf fmt "transition%a%a from %a to %a;"
        (pp_option (pp_list " " pp_extension)) exts
        (pp_option (pp_prefix " " pp_expr)) id
        pp_expr from
        pp_expr _to

  | Taction (e, exts) ->
      Format.fprintf fmt "action%a:\n %a;"
        (pp_option (pp_list " " pp_extension)) exts
        pp_expr e

(* -------------------------------------------------------------------------- *)
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

let pp_signature fmt s =
match s with
  | Ssignature (id, xs) ->
      Format.fprintf fmt "transaction %a : %a;\n"
        pp_id id
        (pp_list " " pp_type) xs


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
      Format.fprintf fmt "enum %a =\n@[<v 2>@]%a"
        pp_id id
        (pp_list "\n" (pp_prefix "| " pp_id)) ids

  | Dstates (id, ids) ->
      Format.fprintf fmt "states%a\n@[<v 2>@]%a"
        (pp_option (pp_enclose " " " =" pp_id)) id
        (pp_list "\n" (pp_prefix "| " pp_ident_state_option)) ids

  | Dasset (id, fields, cs, opts, init) ->
      Format.fprintf fmt "asset %a%a%a%a%a\n"
        pp_id id
        (pp_option (pp_prefix " " (pp_list " @," pp_asset_option))) opts
        (pp_option (pp_enclose " = { " " }" (pp_list "@," pp_field))) fields
        (pp_option (pp_list " @," (pp_enclose " with { " " }" pp_expr))) cs
        (pp_option (pp_enclose " initialized by { " " }" pp_expr)) init

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
      Format.fprintf fmt "transaction%a %a = {%a}\n"
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
        (pp_list "\n" pp_declaration) ds

  | Dcontract (id, xs, dv, exts) ->
      Format.fprintf fmt "contract%a %a = { %a } %a\n"
          (pp_option (pp_list " " pp_extension)) exts
           pp_id id
           (pp_list " " pp_signature) xs
          (pp_option (pp_prefix " := " pp_expr)) dv


(* -------------------------------------------------------------------------- *)
let pp_model fmt { pldesc = m } =
  match m with
| Mmodel es ->
  Format.fprintf fmt "%a" (pp_list "@,\n" pp_declaration) es
| Mmodelextension (id, ds, es) ->
  Format.fprintf fmt "model extension %a (%a) = {%a}"
     pp_id id
    (pp_list "@,\n" pp_declaration) ds
    (pp_list "@,\n" pp_declaration) es


(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a" pp x

(* -------------------------------------------------------------------------- *)
let type_to_str  = string_of__of_pp pp_type
let expr_to_str  = string_of__of_pp pp_expr
let extension_to_str = string_of__of_pp pp_extension
let field_to_str  = string_of__of_pp pp_field
let transitem_to_str = string_of__of_pp pp_transitem
let declaration_to_str = string_of__of_pp pp_declaration
let model_to_str  = string_of__of_pp pp_model
