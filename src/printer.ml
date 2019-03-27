(* -------------------------------------------------------------------------- *)
open Core
open Location
open ParseTree

let pp_str fmt str =
  Format.fprintf fmt "%s" str

(* -------------------------------------------------------------------------- *)
let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp

(* -------------------------------------------------------------------------- *)
let pp_id fmt (id : lident) =
  Format.fprintf fmt "%s" (unloc id)

let pp_name fmt = function
  | (None, id) -> Format.fprintf fmt "%s" (unloc id)
  | (Some a, id) -> Format.fprintf fmt "%s.%s" (unloc a) (unloc id)

(* -------------------------------------------------------------------------- *)
let is_none x =
  match x with None -> true | _ -> false

let pp_option pp fmt x =
  match x with None -> () | Some x -> pp fmt x

let pp_option2 pp1 pp2 fmt x =
  match x with None -> pp1 fmt x | Some x -> pp2 fmt x

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

let rec pp_type fmt { pldesc = e; _ } =
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
  | ValueAssign  -> "="
  | SimpleAssign -> ":="
  | PlusAssign   -> "+="
  | MinusAssign  -> "-="
  | MultAssign   -> "*="
  | DivAssign    -> "/="
  | AndAssign    -> "&="
  | OrAssign     -> "|="

let pp_assignment_operator fmt op =
 Format.fprintf fmt "%s" (assignment_operator_to_str op)

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

let rec pp_expr fmt { pldesc = e; _ } =
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
        (pp_list ";@ " pp_assignment_field) l

  | Eapp ({pldesc = Eop op; _}, [a; b]) ->
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
        (pp_option (pp_prefix " to " pp_qualid)) to_value

  | Eassign (op, lhs, rhs) ->
      Format.fprintf fmt "%a %a %a"
        pp_expr lhs
        pp_assignment_operator op
        pp_expr rhs

  | Eif (cond, then_, else_) ->
      Format.fprintf fmt "@[if %a@ then (%a)@ %a @]"
        pp_expr cond
        pp_expr then_
        pp_else else_

  | Ematchwith (x, xs) ->
      Format.fprintf fmt "match %a with@\n%a@\nend"
        pp_expr x
        (pp_list "@\n" pp_branch) xs

  | Ebreak ->
      Format.fprintf fmt "break"

  | Efor (id, expr, body, label) ->
      Format.fprintf fmt "%afor (%a in %a) (@\n@[<v 2>  %a@]@\n)"
        (pp_option (pp_postfix " : " pp_id)) label
        pp_id id
        pp_expr expr
        pp_expr body

  | Eassert e ->
      Format.fprintf fmt "assert (%a)"
        pp_expr e

  | Eseq (x, y) ->
      Format.fprintf fmt "%a;@\n%a"
        pp_expr x
        pp_expr y

  | Efun (id_ts, x) ->
      Format.fprintf fmt "fun %a => %a"
        (pp_list " " pp_ident_typ) id_ts
        pp_expr x

  | Eletin (id_t, e, body) ->
        Format.fprintf fmt "@[@[<hv 0>let %a =@;<1 2>%a@;<1 0>in@]@ %a@]" (*"let %a = %a in %a"*)
        pp_ident_typ id_t
        pp_expr e
        pp_expr body

  | Equantifier (q, id_t, body) ->
      Format.fprintf fmt "%a %a, %a"
        pp_quantifier q
        pp_ident_typ id_t
        pp_expr body

and pp_else fmt (e : expr option) =
  match e with
| None -> ()
| Some x -> Format.fprintf fmt " else (%a)" pp_expr x

and pp_literal fmt lit =
  match lit with
  | Lnumber   n -> Format.fprintf fmt "%s" (Big_int.string_of_big_int n)
  | Lfloat    f -> Format.fprintf fmt "%s" f
  | Ltz       n -> Format.fprintf fmt "%stz" (Big_int.string_of_big_int n)
  | Laddress  a -> Format.fprintf fmt "@%s" a
  | Lstring   s -> Format.fprintf fmt "\"%s\"" s
  | Lbool     b -> Format.fprintf fmt "%s" (if b then "true" else "false")
  | Lduration d -> Format.fprintf fmt "%s" d
  | Ldate     d -> Format.fprintf fmt "%s" d

and pp_assignment_field fmt (op, id, e) =
  Format.fprintf fmt "%a %a %a"
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
| (x, y, exts) ->
  Format.fprintf fmt "%a%a%a"
  pp_id x
  (pp_option (pp_list " " pp_extension)) exts
  (pp_option (pp_prefix " : " pp_type)) y

and pp_args fmt args =
match args with
| [] -> Format.fprintf fmt "()"
| _ -> Format.fprintf fmt " %a" (pp_list " " pp_expr) args

and pp_branch fmt (pts, e) =
  Format.fprintf fmt "%a -> %a"
    (pp_list " " pp_pattern) pts
    pp_expr e

and pp_fun_ident_typ fmt (arg : lident_typ) =
match arg with
| (x, None, exts) -> Format.fprintf fmt "%a%a" pp_id x (pp_option (pp_list " " pp_extension)) exts
| (x, Some y, exts) ->
    Format.fprintf fmt "(%a%a : %a)"
    pp_id x
    (pp_option (pp_list " " pp_extension)) exts
    pp_type y

and pp_fun_args fmt args =
match args with
| [] -> Format.fprintf fmt ""
| _ -> Format.fprintf fmt " %a" (pp_list " " pp_fun_ident_typ) args

(* -------------------------------------------------------------------------- *)
and pp_field fmt { pldesc = f; _ } =
  match f with
  | Ffield (id, typ, dv, exts) ->
      Format.fprintf fmt "%a%a : %a%a"
        pp_id id
        (pp_option (pp_list " " pp_extension)) exts
        pp_type typ
        (pp_option (pp_prefix " := " pp_expr)) dv

(* -------------------------------------------------------------------------- *)
and pp_extension fmt { pldesc = e; _ } =
  match e with
  | Eextension (id, args) ->
        Format.fprintf fmt "[%%%a%a]"
        pp_id id
        (pp_option (pp_prefix " " (pp_list " " pp_expr))) args

(* -------------------------------------------------------------------------- *)
let pp_named_item fmt (s : named_item) =
match s with
| (None, y)   ->
    Format.fprintf fmt "%a" pp_expr y

| (Some x, y) ->
    Format.fprintf fmt "%a : %a"
    pp_id x
    pp_expr y

let pp_to fmt ((to_, when_, effect) : (lident * expr option * expr option)) =
  Format.fprintf fmt "to %a@\n%a%a"
    pp_id to_
    (pp_option (pp_enclose "by condition " "@\n" pp_expr)) when_
    (pp_option (pp_enclose "with effect (" ")@\n" pp_expr)) effect

let pp_specification_variable fmt (sv : (lident * type_t * expr option) loced) =
match sv with
| {pldesc = (id, typ, dv); _} ->
    Format.fprintf fmt "variable %a %a%a"
        pp_id id
        pp_type typ
        (pp_option (pp_prefix " = " pp_expr)) dv

(*
let pp_transitem fmt { pldesc = t; _ } =
  match t with
  | Tcalledby (e, exts) ->
      Format.fprintf fmt "called by%a %a"
        (pp_option (pp_list " " pp_extension)) exts
        pp_expr e

  | Tcondition (xs, exts) ->
      Format.fprintf fmt "condition%a@\n@[<v 2>  %a@]@\n"
       (pp_option (pp_list " " pp_extension)) exts
       (pp_list ";@\n" pp_named_item) xs

  | Tfunction (id, args, r, b) ->
      Format.fprintf fmt "function %a %a%a = %a"
        pp_id id
        pp_fun_args args
        (pp_option (pp_prefix " : " pp_type)) r
        pp_expr b

  | Tspecification (None, None, None, se, exts) ->
      Format.fprintf fmt "specification%a@\n@[<v 2>  %a@]@\n"
       (pp_option (pp_list " " pp_extension)) exts
       (pp_list "; " pp_named_item) se

   | Tspecification (sv, sa, si, se, exts) ->
      Format.fprintf fmt "specification%a@\n@[<v 2>  %a@]@\n@[<v 2>  %a@]@\n@[<v 2>  %a@]@\n@[<v 2>  %a@]"
       (pp_option (pp_list " " pp_extension)) exts
       (pp_option (pp_list "@\n" pp_specification_variable)) sv
       (pp_option (pp_prefix "effect@\n  " pp_expr)) sa
       (pp_option (pp_prefix "invariant@\n  " (pp_list ";@\n  " pp_named_item))) si
       (pp_prefix "ensure@\n  " (pp_list ";@\n  " pp_named_item)) se

  | Tinvariant (id, xs, exts) ->
      Format.fprintf fmt "invariant%a %a@\n@[<v 2>  %a@]@\n"
       (pp_option (pp_list " " pp_extension)) exts
       pp_id id
       (pp_list "; " pp_named_item) xs
*)

(* -------------------------------------------------------------------------- *)
let pp_state_option fmt = function
  | SOinitial  -> Format.fprintf fmt "initial"
  | SOspecification xs -> Format.fprintf fmt "with {%a}" (pp_list ";@\n" pp_named_item) xs

let pp_ident_state_option fmt item =
  match item with
  | (id, opts) ->
      Format.fprintf fmt "%a%a"
      pp_id id
      (pp_option (pp_prefix " " (pp_list " " pp_state_option))) opts

let pp_value_option fmt opt =
match opt with
  | VOfrom e -> Format.fprintf fmt "from %a" pp_qualid e
  | VOto   e -> Format.fprintf fmt "to %a"   pp_qualid e

let pp_asset_option fmt opt =
match opt with
  | AOasrole -> Format.fprintf fmt "as role"
  | AOidentifiedby id -> Format.fprintf fmt "identified by %a" pp_id id
  | AOsortedby id  -> Format.fprintf fmt "sorted by %a" pp_id id

let pp_signature fmt s =
match s with
  | Ssignature (id, xs) ->
      Format.fprintf fmt "action %a : %a"
        pp_id id
        (pp_list " " pp_type) xs

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
(pp_option (pp_prefix " " (pp_list " " pp_expr))) y

let pp_asset_post_option fmt (apo : asset_post_option) =
  match apo with
  | APOstates i ->
    Format.fprintf fmt " with states %a"
      pp_id i
  | APOconstraints cs ->
    Format.fprintf fmt " with {@\n @[<v 2>  %a@] } "
      (pp_list ";@\n  " pp_named_item) cs
  | APOinit init ->
    Format.fprintf fmt " initialized by {@\n @[<v 2>  %a@] }"
      (pp_list "@\n" (pp_enclose "{" "}" (pp_list "; " pp_expr))) init

let map_option f x =
  match x with
  | Some y -> f y
  | None -> ()

let pp_action_properties fmt (props : action_properties) =
  map_option (fun (e, exts) ->
      Format.fprintf fmt "called by%a %a@\n"
        (pp_option (pp_list " " pp_extension)) exts
        pp_expr e) props.calledby

let pp_effect fmt (code, _) =
  Format.fprintf fmt "effect@\n@[<v 2>  %a@]@\n"
    (*(pp_list " " pp_extension) exts*)
    pp_expr code

let rec pp_declaration fmt { pldesc = e; _ } =
  match e with
  | Duse id ->
    Format.fprintf fmt "use %a" pp_id id

  | Dmodel (id, exts) ->
    Format.fprintf fmt "model%a %a"
      (pp_option (pp_list " " pp_extension)) exts
      pp_id id

  | Dconstant (id, typ, dv, exts) ->
      Format.fprintf fmt "constant%a %a %a%a"
          (pp_option (pp_list " " pp_extension)) exts
          pp_id id
          pp_type typ
          (pp_option (pp_prefix " = " pp_expr)) dv

  | Dvariable (id, typ, opts, dv, exts) ->
      Format.fprintf fmt "variable%a %a %a%a%a"
          (pp_option (pp_list " " pp_extension)) exts
          pp_id id
          pp_type typ
          (pp_option (pp_prefix " " (pp_list " " pp_value_option))) opts
          (pp_option (pp_prefix " = " pp_expr)) dv

  | Denum (id, ids) ->
      Format.fprintf fmt "enum %a =\n@[<v 2>@]%a"
        pp_id id
        (pp_list "\n" (pp_prefix "| " pp_id)) ids

  | Dstates (id, ids, exts) ->
      Format.fprintf fmt "states%a%a =@\n@[<v 2>@]%a"
        (pp_option (pp_list " " pp_extension)) exts
        (pp_option (pp_prefix " " pp_id)) id
        (pp_option (pp_list "\n" (pp_prefix "| " pp_ident_state_option))) ids

  | Dasset (id, fields, opts, apo, ops, exts) ->
      Format.fprintf fmt "asset%a%a %a%a%a%a"
        (pp_option (pp_list " " pp_extension)) exts
        (pp_option pp_asset_operation) ops
        pp_id id
        (pp_option (pp_prefix " " (pp_list " @," pp_asset_option))) opts
        (pp_option ((fun fmt -> Format.fprintf fmt " = {@\n @[<v 2>%a@] }@\n" (pp_list ";@\n" pp_field)))) fields
        (pp_list "@\n" pp_asset_post_option) apo

  | Dobject (id, e, exts) ->
      Format.fprintf fmt "object%a %a %a"
        (pp_option (pp_list " " pp_extension)) exts
        pp_id id
        pp_expr e

  | Dkey (id, e, exts) ->
      Format.fprintf fmt "key%a %a of %a"
        (pp_option (pp_list " " pp_extension)) exts
        pp_id id
        pp_expr e

  | Daction (id, args, props, code, exts) ->
      Format.fprintf fmt "action%a %a%a = {@\n@[<v 2>  %a%a@]@\n}"
        (pp_option (pp_list "@," pp_extension)) exts
        pp_id id
        pp_fun_args args
        pp_action_properties props
        (pp_option pp_effect) code

  | Dtransition (id, args, _on, _from, props, _trs, exts) ->
      Format.fprintf fmt "transition%a %a%a = {@\n@[<v 2>  %a@]@\n}"
        (pp_option (pp_list "@," pp_extension)) exts
        pp_id id
        pp_fun_args args
        pp_action_properties props

(*  | Ttransition (id, from, lto, exts) ->
      Format.fprintf fmt "transition%a%a from %a@\n%a"
        (pp_option (pp_prefix " " pp_qualid)) id
        (pp_option (pp_list " " pp_extension)) exts
        pp_expr from
        (pp_list "@\n" pp_to) lto*)



  | Dextension (id, args) ->
      Format.fprintf fmt "%%%a%a"
        pp_id id
        (pp_option (pp_prefix " " (pp_list " " pp_expr))) args

  | Dnamespace (id, ds) ->
      Format.fprintf fmt "namespace %a {@\n@[<v 2>  %a@]@\n}"
         pp_id id
        (pp_list "\n" pp_declaration) ds

  | Dcontract (id, xs, dv, exts) ->
      Format.fprintf fmt "contract%a %a = {@\n@[<v 2>  %a@]@\n}%a"
          (pp_option (pp_list " " pp_extension)) exts
           pp_id id
           (pp_list ";@\n" pp_signature) xs
          (pp_option (pp_prefix " = " pp_expr)) dv

  | Dfunction (id, args, r, b) ->
      Format.fprintf fmt "function %a %a%a = %a"
        pp_id id
        pp_fun_args args
        (pp_option (pp_prefix " : " pp_type)) r
        pp_expr b

  | Dspecification (xs, exts) ->
      Format.fprintf fmt "specification%a {@\n@[<v 2>  %a@]@\n}"
        (pp_option (pp_list " " pp_extension)) exts
        (pp_list ";@\n" pp_named_item) xs

(* -------------------------------------------------------------------------- *)
let pp_model fmt { pldesc = m; _ } =
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
let declaration_to_str = string_of__of_pp pp_declaration
let model_to_str  = string_of__of_pp pp_model
