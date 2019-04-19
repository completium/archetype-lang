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

let pp_do_if c pp fmt x =
  match c with
  | true  -> pp fmt x
  | _ -> ()

let pp_if c pp_true pp_false fmt x =
  match c with
  | true  -> pp_true fmt x
  | false -> pp_false fmt x

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
  ";",   (20,  Left);

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

let rec pp_type fmt e =
  match unloc e with
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

let rec pp_expr fmt a =
  let e = unloc a in
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
        (pp_list "; " pp_expr) values

  | Edot (lhs, rhs) ->
      Format.fprintf fmt "%a.%a"
        pp_expr lhs
        pp_id rhs

  | Erecord l ->
      Format.fprintf fmt "{%a}"
        (pp_list ";@ " pp_record_field) l

  | Etuple l ->
      Format.fprintf fmt "%a"
        (pp_list ",@ " pp_expr) l

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

  | Efor (id, expr, body) ->
      Format.fprintf fmt "for (%a in %a) (@\n@[<v 2>  %a@]@\n)"
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

  | Eletin (id_t, e, body, other) ->
        Format.fprintf fmt "@[@[<hv 0>let %a =@;<1 2>%a@;<1 0>in@]@ %a%a@]" (*"let %a = %a in %a"*)
        pp_ident_typ id_t
        pp_expr e
        pp_expr body
        (pp_option (fun fmt e ->
            Format.fprintf fmt "@\notherwise %a"
              pp_expr e)) other

  | Equantifier (q, id_t, body) ->
      Format.fprintf fmt "%a %a, %a"
        pp_quantifier q
        pp_ident_typ id_t
        pp_expr body

  | Elabel (i, x) ->
      Format.fprintf fmt "%a : %a"
        pp_id i
        pp_expr x

and pp_else fmt (e : expr option) =
  match e with
| None -> ()
| Some x -> Format.fprintf fmt " else (%a)" pp_expr x

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

and pp_record_field fmt (o, e) =
  Format.fprintf fmt "%a%a"
    (pp_option (fun fmt (op, id) ->
         Format.fprintf fmt "%a %a "
           pp_id id
           pp_assignment_operator op
       )) o
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
  pp_extensions exts
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
| (x, None, exts) -> Format.fprintf fmt "%a%a" pp_id x pp_extensions exts
| (x, Some y, exts) ->
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
      Format.fprintf fmt "%a%a : %a%a"
        pp_id id
        pp_extensions exts
        pp_type typ
        (pp_option (pp_prefix " = " pp_expr)) dv

(* -------------------------------------------------------------------------- *)
and pp_extension fmt { pldesc = e; _ } =
  match e with
  | Eextension (id, args) ->
        Format.fprintf fmt "[%%%a%a]"
        pp_id id
        (pp_option (pp_prefix " " (pp_list " " pp_expr))) args

and pp_extensions x = (pp_option (pp_list " " pp_extension)) x

(* -------------------------------------------------------------------------- *)
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

(* -------------------------------------------------------------------------- *)
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
(pp_option (pp_prefix " " (pp_list " " pp_expr))) y

let pp_label_expr fmt (le : label_expr) =
  let (lbl, e) = unloc le in
  Format.fprintf fmt "%a%a"
    (pp_option (pp_postfix " : " pp_id)) lbl
    pp_expr e

let pp_label_exprs xs = (pp_list ";@\n" pp_label_expr) xs

let pp_state_option fmt = function
  | SOinitial ->
    Format.fprintf fmt "initial"

  | SOspecification xs ->
    Format.fprintf fmt "with { %a }"
      pp_label_exprs xs

let pp_ident_state_option fmt item =
  match item with
  | (id, opts) ->
      Format.fprintf fmt "%a%a"
      pp_id id
      (pp_option (pp_prefix " " (pp_list " " pp_state_option))) opts

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
      pp_expr e

let map_option f x =
  match x with
  | Some y -> f y
  | None -> ()

let pp_specification fmt xs =
  Format.fprintf fmt "specification@\n@[<v 2>  %a@]"
    pp_label_exprs xs

let pp_verification_item fmt = function
  | Vpredicate (id, args, body) ->
    Format.fprintf fmt "predicate %a %a =@\n@{<v 2>  %a@}"
      pp_id id
      pp_fun_args args
      pp_expr body

  | Vdefinition (id, typ, var, body) ->
    Format.fprintf fmt "definition %a =@\n@[<v 2>  { %a : %a | %a }@]"
      pp_id id
      pp_id var
      pp_type typ
      pp_expr body

  | Vaxiom (id, body) ->
    Format.fprintf fmt "axiom %a =@\n@[<v 2>  %a@]"
      pp_id id
      pp_expr body

  | Vtheorem (id, body) ->
    Format.fprintf fmt "theorem %a =@\n@[<v 2>  %a@]"
      pp_id id
      pp_expr body

  | Vvariable (id, typ, dv) ->
    Format.fprintf fmt "variable %a %a%a"
      pp_id id
      pp_type typ
      (pp_option (fun fmt x -> Format.fprintf fmt " = %a" pp_expr x)) dv

  | Vinvariant (id, cs) ->
    Format.fprintf fmt "invariant %a@\n@[<v 2>  %a@]"
      pp_id id
      pp_label_exprs cs

  | Veffect e ->
    Format.fprintf fmt "effect@\n@[<v 2>  %a@]"
      pp_expr e

  | Vspecification xs -> pp_specification fmt xs

let pp_verification_items = pp_list "@\n@\n" pp_verification_item

let pp_function fmt (f : s_function) =
  Format.fprintf fmt "function %a %a%a %a"
    pp_id f.name
    pp_fun_args f.args
    (pp_option (pp_prefix " : " pp_type)) f.ret_t
    (pp_if (match f.verif with | Some _ -> true | None -> false)
         (fun fmt (f : s_function) ->
            Format.fprintf fmt "= {@\nspecification@\n%a@\neffect@\n%a}"
              (pp_option pp_verification) f.verif
              pp_expr f.body)
         (fun fmt (f : s_function) ->
            Format.fprintf fmt "=@\n%a" pp_expr f.body)) f

let pp_action_properties fmt (props : action_properties) =
  map_option (
    fun v ->
      let items, exts = v |> unloc in
      let items = items |> List.map (fun x -> x |> unloc) in
      match items with
      | [Vspecification v] -> pp_specification fmt v
      | _ ->
        begin
          Format.fprintf fmt "verification%a {@\n@[<v 2>  %a@]@\n}@\n"
            pp_extensions exts
            pp_verification_items items
        end) props.verif;
  map_option (fun (e, exts) ->
      Format.fprintf fmt "called by%a %a@\n"
        pp_extensions exts
        pp_expr e) props.calledby;
  map_option (fun (cs, exts) ->
      Format.fprintf fmt "condition%a@\n@[<v 2>  %a@]@\n"
        pp_extensions exts
        pp_label_exprs cs) props.condition;
  (pp_list "@\n" pp_function) fmt (List.map unloc props.functions)

let pp_transition fmt (to_, conditions, effect) =
  Format.fprintf fmt "to %a%a%a@\n"
    pp_id to_
    (pp_option (pp_prefix "@\nwhen " pp_expr)) conditions
    (pp_option (fun fmt x ->
         Format.fprintf fmt "@\nwith effect@\n@[<v 2>  %a@]"
           pp_expr x)) effect


let rec pp_declaration fmt { pldesc = e; _ } =
  let is_empty_action_properties_opt (ap : action_properties) (a : 'a option) =
    match ap.calledby, ap.condition, ap.functions, ap.verif, a with
    | None, None, [], None, None -> true
    | _ -> false in
  match e with
  | Darchetype (id, exts) ->
    Format.fprintf fmt "archetype%a %a"
      pp_extensions exts
      pp_id id

  | Dvariable (id, typ, dv, opts, cst, exts) ->
     Format.fprintf fmt "%a%a %a %a%a%a"
          pp_str (if cst then "constant" else "variable")
          pp_extensions exts
          pp_id id
          pp_type typ
          (pp_option (pp_prefix " " (pp_list " " pp_value_option))) opts
          (pp_option (pp_prefix " = " pp_expr)) dv

  | Denum (id, ids, exts) ->
      Format.fprintf fmt "enum%a %a =\n@[<v 2>@]%a"
        pp_extensions exts
        pp_id id
        (pp_list "\n" (pp_prefix "| " pp_id)) ids

  | Dstates (id, ids, exts) ->
      Format.fprintf fmt "states%a%a%a"
        pp_extensions exts
        (pp_option (pp_prefix " " pp_id)) id
        (pp_do_if (match ids with | Some l when List.length l > 0 -> true | _ -> false) (
            fun fmt x->
              Format.fprintf fmt " =@\n@[<v 2>@]%a"
                (pp_option (pp_list "\n" (pp_prefix "| " pp_ident_state_option))) x)) ids

  | Dasset (id, fields, opts, apo, ops, exts) ->
      Format.fprintf fmt "asset%a%a %a%a%a%a"
        pp_extensions exts
        (pp_option pp_asset_operation) ops
        pp_id id
        (pp_prefix " " (pp_list " @," pp_asset_option)) opts
        (pp_do_if (List.length fields > 0) ((fun fmt -> Format.fprintf fmt " = {@\n@[<v 2>%a@]}@\n" (pp_list ";@\n" pp_field)))) fields
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
                     Format.fprintf fmt "effect%a@\n@[<v 2>  %a@]@\n"
                       pp_extensions exts
                       pp_expr code
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
        pp_expr from
        (fun fmt (pr, ts) ->
           Format.fprintf fmt " = {@\n@[<v 2>  %a%a@]@\n}"
             (pp_do_if (not (is_empty_action_properties_opt props None)) pp_action_properties) pr
             (pp_list "@\n" pp_transition) ts) (props, trs)

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
      pp_extensions exts
      pp_id id
      (pp_list "@\n" pp_signature) xs
      (pp_option (pp_prefix " = " pp_expr)) dv

  | Dfunction f ->
    Format.fprintf fmt "%a"
      pp_function f

  | Dverification v ->
    let items, exts = v |> unloc in
    let items = items |> List.map (fun x -> x |> unloc) in
    Format.fprintf fmt "verification%a {@\n@[<v 2>  %a@]@\n}"
      pp_extensions exts
      pp_verification_items items

(* -------------------------------------------------------------------------- *)
let pp_archetype fmt { pldesc = m; _ } =
  match m with
  | Marchetype es ->
    Format.fprintf fmt "%a@\n" (pp_list "@,\n" pp_declaration) es
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
let expr_to_str        = string_of__of_pp pp_expr
let declaration_to_str = string_of__of_pp pp_declaration
let archetype_to_str   = string_of__of_pp pp_archetype
