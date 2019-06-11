open Mltree

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_id = pp_str

(* -------------------------------------------------------------------------- *)
let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp

let pp_option pp fmt (x : 'a option) =
  match x with None -> () | Some x -> pp fmt x

let pp_enclose pre post pp fmt x =
  Format.fprintf fmt "%(%)%a%(%)" pre pp x post

let pp_prefix pre pp fmt x =
  pp_enclose pre "" pp fmt x

let pp_postfix post pp fmt x =
  pp_enclose "" post pp fmt x

(* -------------------------------------------------------------------------- *)

let type_basic_to_string = function
  | Tunit -> "unit"
  | Tbool -> "bool"
  | Tint -> "int"
  | Tnat -> "nat"
  | Ttez -> "tez"
  | Tstring -> "string"
  | Tbytes -> "bytes"
  | Ttimestamp -> "timestamp"
  | Tkey -> "key"
  | Tkey_hash -> "key_hash"
  | Tsignature -> "signature"
  | Toperation -> "operation"
  | Taddress -> "address"

let rec pp_type fmt = function
  | Tbasic b ->
    Format.fprintf fmt "%s"
      (type_basic_to_string b)

  | Ttuple l ->
    let pp fmt l =
      Format.fprintf fmt "%a"
        (pp_list " * " pp_type) l
    in
    pp fmt l

  | Tlist t ->
    let pp fmt t =
      Format.fprintf fmt "%a list"
        pp_type t
    in
    pp fmt t

  | Tmap (k, v) ->
    let pp fmt (k, v) =
      Format.fprintf fmt "(%a, %a) map"
        pp_type k
        pp_type v
    in
    pp fmt (k, v)

  | Tcontract ->
    Format.fprintf fmt "contract"

  | Toption t ->
    let pp fmt t =
      Format.fprintf fmt "%a option"
        pp_type t
    in
    pp fmt t

  | Tlocal str ->
    Format.fprintf fmt "%s" str

let pp_literal fmt = function
  | Lint    n -> Format.fprintf fmt "%s" (Big_int.string_of_big_int n)
  | Lbool   b -> Format.fprintf fmt "%s" (if b then "true" else "false")
  | Lstring s -> Format.fprintf fmt "\"%s\"" s
  | Lraw    s -> Format.fprintf fmt "%s" s

let binop_to_string = function
  | And    -> "&&"
  | Or     -> "||"
  | Equal  -> "="
  | Nequal -> "<>"
  | Gt     -> ">"
  | Ge     -> ">="
  | Lt     -> "<"
  | Le     -> "<="
  | Plus   -> "+"
  | Minus  -> "-"
  | Mult   -> "*"
  | Div    -> "/"
  | Modulo -> "%"

let unaop_to_string = function
  | Not    -> "not"
  | Uminus -> "-"
  | Uplus  -> "+"

let pp_pattern fmt = function
  | Pid id ->
    Format.fprintf fmt "| %a"
      pp_id id
  | Pwild ->
    Format.fprintf fmt "| _"

let rec pp_expr fmt = function
  | Eletin (l, b) ->
    let pp_letin_item fmt (l, e) =
      Format.fprintf fmt "let %a : %a = %a in@\n"
        (pp_list "@\n" pp_id) (List.map fst l)
        pp_type (Ttuple (List.map snd l))
        pp_expr e
    in
    Format.fprintf fmt "%a%a"
      (pp_list "@\n" pp_letin_item) l
      pp_expr b

  | Eif (cond, then_, else_) ->
    let pp fmt (cond, then_, else_) =
      Format.fprintf fmt "@[if %a@ then (%a)@ else (%a)@ @]"
        pp_expr cond
        pp_expr then_
        pp_expr else_
    in
    pp fmt (cond, then_, else_)

  | Ematchwith (e, l) ->
    let pp fmt (e, l) =
      Format.fprintf fmt "match %a with@\n%a@\n"
        pp_expr e
        (pp_list "@\n" (fun fmt (pts, e) ->
             Format.fprintf fmt "%a -> %a"
               (pp_list " " pp_pattern) pts
               pp_expr e)) l
    in
    pp fmt (e, l)

  | Eapp (id, args) ->

    let pp fmt (id, args) =
      Format.fprintf fmt "%a %a"
        pp_id id
        (pp_list " " pp_expr) args
    in
    pp fmt (id, args)

  | Ebin (op, l, r) ->
    let pp fmt (op, l, r) =
      Format.fprintf fmt "%a %s %a"
        pp_expr l
        (binop_to_string op)
        pp_expr r
    in
    pp fmt (op, l, r)

  | Eunary (op, e) ->

    let pp fmt (op, e) =
      Format.fprintf fmt "%s%a"
        (unaop_to_string op)
        pp_expr e
    in

    pp fmt (op, e)

  | Erecord l ->
    let pp fmt l =
      Format.fprintf fmt "{ %a }"
        (pp_list "; " pp_expr) l
    in
    pp fmt l

  | Etuple l ->

    let pp fmt l =
      Format.fprintf fmt "(%a)"
        (pp_list ", " pp_expr) l
    in
    pp fmt l

  | Evar s ->
    Format.fprintf fmt "%s" s

  | Econtainer l ->
    let pp fmt l =
      Format.fprintf fmt "[%a]"
        (pp_list "; " pp_expr) l
    in
    pp fmt l

  | Elit l ->
    pp_literal fmt l

  | Edot (e, id) ->

    let pp fmt (e, id) =
      Format.fprintf fmt "%a.%a"
        pp_expr e
        pp_id id
    in
    pp fmt (e, id)

let pp_struct_type fmt (s : type_struct) =
  let pp_item fmt ((id, t) : (ident * type_ option)) =
    Format.fprintf fmt "| %a%a"
      pp_id id
      (pp_option (pp_prefix " of " pp_type)) t
  in
  Format.fprintf fmt "type %a =@\n@[<v 2>  %a@]@."
    pp_id s.name
    (pp_list "@\n" pp_item) s.values

let pp_sstruct fmt (s : struct_struct) =
  let pp_field fmt ((id, t) : (ident * type_)) =
    Format.fprintf fmt "%a: %a;"
      pp_id id
      pp_type t
  in
  Format.fprintf fmt "type %a = {@\n@[<v 2>  %a@]@\n}@."
    pp_id s.name
    (pp_list "@\n" pp_field) s.fields

let pp_fun fmt (s : fun_struct) =
  let pp_fun_node fmt = function
    | Init  -> Format.fprintf fmt "%%init"
    | Entry -> Format.fprintf fmt "%%entry"
    | None  -> Format.fprintf fmt ""
  in
  let pp_arg fmt (id, t) =
    match t with
    | Tbasic Tunit -> Format.fprintf fmt "()"
    | _ ->
      Format.fprintf fmt "(%a : %a)"
        pp_id id
        pp_type t
  in
  Format.fprintf fmt "let%a %a %a =@\n %a@."
    pp_fun_node s.node
    pp_id s.name
    (pp_list " " pp_arg) s.args
    pp_expr s.body

let pp_decl fmt = function
  | Dtype s   -> pp_struct_type fmt s
  | Dstruct s -> pp_sstruct fmt s
  | Dfun s    -> pp_fun fmt s

let pp_tree fmt tree =
  Format.fprintf fmt "%a@."
    (pp_list "@\n" pp_decl) tree.decls

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_tree (x : tree) = string_of__of_pp pp_tree x
