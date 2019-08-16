(* open Location *)
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

let rec pp_qualid fmt (q : qualid) =
  match q.node with
  | Qdot (q, i) ->
    Format.fprintf fmt "%a.%a"
      pp_qualid q
      pp_id i
  | Qident i -> pp_id fmt i

let pp_bval fmt (bval : bval) =
  match bval.node with
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

let rec pp_pterm fmt (pterm : pterm) =
  match pterm.node with
  (* | Pquantifer of quantifier * 'id * type_ * 'term *)
  | Pif (c, t, e) ->
    Format.fprintf fmt "if %a@\nthen @[<v 2>%a@]@\nelse @[<v 2>%a@]"
      pp_pterm c
      pp_pterm t
      pp_pterm e

  (* | Pmatchwith of 'term * ('id pattern_gen * 'term) list *)
  (* | Pcall of ('term option * 'id call_kind * (('id, 'typ, 'term) term_arg) list) *)
  (* | Plogical of logical_operator * 'term * 'term *)
  (* | Pnot of 'term *)
  (* | Pcomp of comparison_operator * 'term * 'term *)
  (* | Parith of arithmetic_operator * 'term * 'term *)
  (* | Puarith of unary_arithmetic_operator * 'term *)
  (* | Precord of 'term list *)
  (* | Pletin of 'id * 'term * 'typ option * 'term *)
  | Pvar id ->
    pp_id fmt id

  | Parray l ->
    Format.fprintf fmt "[%a]"
      (pp_list "; " pp_pterm) l

  | Plit v ->
    pp_bval fmt v

  | Pdot (e, i) ->
    Format.fprintf fmt "%a (%a)"
      pp_id i
      pp_pterm e

  (* | Pconst of const *)
  | Ptuple l ->
    Format.fprintf fmt "(%a)"
      (pp_list ", " pp_pterm) l

  (* | PsecurityActionRole of action_description * security_role list *)
  (* | PsecurityActionAction of action_description * security_action list *)
  | _ -> Format.fprintf fmt "pterm:TODO"

let pp_instruction fmt (i : instruction) =
  match i.node with
  (* | Iif of ('term * 'instr * 'instr)                              condition * then_ * else_ *)
  (* | Ifor of ('id * 'term * 'instr)                                id * collection * body *)
  (* | Iletin of ('id * 'term * 'instr)                              id * init * body *)
  (* | Iseq of 'instr list                                           lhs ; rhs *)
  (* | Imatchwith of 'term * ('id pattern_gen * 'instr) list         match 'term with ('pattern * 'instr) list *)
  (* | Iassign of (assignment_operator * 'id * 'term)                $2 assignment_operator $3 *)
  (* | Irequire of (bool * 'term)                                    $1 ? require : failif *)
  (* | Itransfer of ('term * bool * ('id, 'typ) qualid_gen option)   value * back * dest *)
  (* | Ibreak *)
  (* | Iassert of 'term *)
  (* | Icall of ('term option * 'id call_kind * (('id, 'typ, 'term) term_arg) list) *)
  (* | Ireturn of 'term *)
  (* | Ilabel of 'id *)
  | _ -> Format.fprintf fmt "instruction:TODO"

let pp_label_term fmt (lt : (lident, (lident, type_) term_gen) label_term) =
  Format.fprintf fmt "%a%a"
    (pp_option (pp_postfix " : " pp_id)) lt.label
    pp_pterm lt.term

let pp_variable fmt (v : (lident, ptyp, pterm) variable) =
  Format.fprintf fmt "%a %a%a%a%a"
    pp_ptyp (Option.get v.decl.typ)
    pp_id v.decl.name
    (pp_option (pp_prefix " from " pp_qualid)) v.from
    (pp_option (pp_prefix " to " pp_qualid)) v.to_
    (pp_option (pp_prefix " := " pp_pterm)) v.decl.default

let pp_field fmt (f : (lident, ptyp, pterm) decl_gen) =
  Format.fprintf fmt "%a : %a%a"
    pp_id f.name
    (pp_option pp_ptyp) f.typ
    (pp_option (pp_prefix " := " pp_pterm)) f.default

let pp_asset fmt (a : (lident, ptyp, pterm) asset_struct) =
  Format.fprintf fmt "asset %a%a%a = {@\n  @[%a@]@\n}%a%a%a@\n"
    pp_id a.name
    (pp_option (pp_prefix " identified by " pp_id)) a.key
    (pp_do_if (not (List.is_empty a.sort)) (pp_prefix " sorted by " (pp_list ", " pp_id))) a.sort
    (pp_list "@\n" pp_field) a.fields
    (pp_option (pp_prefix " with states " pp_id)) a.state
    (pp_option (pp_prefix " initialized by " pp_pterm)) a.init
    (pp_do_if (not (List.is_empty a.specs)) (
        fun fmt ->
          Format.fprintf fmt " with {@[<v 0>%a@]}"
            (pp_list ";@\n" pp_label_term))) a.specs

let pp_enum_item fmt (ei : (lident, ptyp, pterm) enum_item_struct) =
  Format.fprintf fmt "| %a%a%a"
    pp_id ei.name
    (pp_do_if ei.initial pp_str) " initial"
    (pp_do_if (not (List.is_empty ei.invariants)) (
        fun fmt ->
          Format.fprintf fmt " with {@[<v 0>%a@]}"
            (pp_list ";@\n" pp_label_term))) ei.invariants

let pp_enum fmt (e : (lident, ptyp, pterm) enum_struct) =
  Format.fprintf fmt "enum %a =@\n  @[%a@]@\n"
    pp_id e.name
    (pp_list "@\n" pp_enum_item) e.items

let pp_ast fmt (ast : model) =
  Format.fprintf fmt "%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @."
    pp_id ast.name
    (pp_list "@\n" pp_variable) ast.variables
    (pp_list "@\n" pp_asset) ast.assets
    (pp_list "@\n" pp_enum) ast.enums
(* ast.functions      *)
(* ast.transactions   *)
(* ast.contracts      *)
(* ast.verifications  *)

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
