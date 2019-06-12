open Mlwtree

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_id = pp_str


let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_id = pp_str

(* -------------------------------------------------------------------------- *)

let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp

let pp_enclose pre post pp fmt x =
  Format.fprintf fmt "%(%)%a%(%)" pre pp x post

(* -------------------------------------------------------------------------- *)

type assoc  = Left | Right | NonAssoc
type pos    = PLeft | PRight | PInfix | PNone

let e_in            =  (10,  NonAssoc) (* in  *)
let e_to            =  (10,  NonAssoc) (* to  *)
let e_arrow         =  (12,  NonAssoc) (* ->  *)
let e_match         =  (14,  Right)    (* match *)
let e_if            =  (14,  Right)    (* if  *)
let e_then          =  (14,  Right)    (* then *)
let e_else          =  (16,  Right)    (* else *)
let e_comma         =  (20,  Left)     (* ,   *)
let e_semi_colon    =  (20,  Left)     (* ;   *)
let e_colon         =  (25,  NonAssoc) (* :   *)
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
let e_app           =  (140, NonAssoc) (* f ()  *)
let e_for           =  (140, NonAssoc) (* for in .  *)

let e_default       =  (0, NonAssoc)   (* ?  *)
let e_simple        =   (200, NonAssoc)   (* ?  *)
let e_top           =  (-1,NonAssoc)

(* -------------------------------------------------------------------------- *)

let pp_if c pp_true pp_false fmt x =
  match c with
  | true  -> pp_true fmt x
  | false -> pp_false fmt x

let pp_maybe c tx pp fmt x =
  pp_if c (tx pp) pp fmt x

let pp_paren pp fmt x =
  pp_enclose "(" ")" pp fmt x

let pp_maybe_paren c pp =
  pp_maybe c pp_paren pp

let maybe_paren outer inner pos pp =
  let c =
    match (outer, inner, pos) with
    | ((o, Right), (i, Right), PLeft) when o >= i -> true
    | ((o, Right), (i, NonAssoc), _)  when o >= i -> true
    | ((o, Right), (i, Left), _)      when o >= i -> true
    | ((o, Left),  (i, Left), _)      when o >= i -> true
    | ((o, NonAssoc), (i, _), _)      when o >= i -> true
    | _ -> false
  in pp_maybe_paren c pp

let needs_paren = function
  | Tdoti _ -> false
  | Tdot _  -> false
  | Tvar _  -> false
  | _ -> true

let pp_with_paren pp fmt x =
  pp_maybe (needs_paren x) pp_paren pp fmt x

(* -------------------------------------------------------------------------- *)

let pp_type fmt typ =
  let typ_str =
    match typ with
    | Tyint         -> "int"
    | Tystring      -> "string"
    | Tydate        -> "date"
    | Tyaddr        -> "address"
    | Tyrole        -> "role"
    | Tytez         -> "tez"
    | Tystorage     -> "storage_"
    | Tyunit        -> "unit"
    | Tyrecord i    -> i
    | Tyasset  i    -> i
    | Tycoll i      -> "acol"
    | Tymap i       -> "map "^i
    | Typartition _ -> "acol" in

  pp_str fmt typ_str

(* -------------------------------------------------------------------------- *)

let pp_exn fmt e =
  let e_str =
    match e with
    | Ekeyexist -> "KeyExist"
    | Enotfound -> "NotFound" in
  pp_str fmt e_str

(* -------------------------------------------------------------------------- *)

let pp_univ_decl fmt (i,t) =
  Format.fprintf fmt "%a : %a"
    (pp_list " " pp_str) i
    pp_type t

(* -------------------------------------------------------------------------- *)

let rec pp_term outer pos fmt = function
  | Tseq l         -> Format.fprintf fmt "%a" (pp_list ";@\n" (pp_term outer pos)) l
  | Tif (i,t, None)    ->
    let pp fmt (cond, then_) =
      Format.fprintf fmt "@[if %a@ then %a@ @]"
        (pp_term e_if PRight) cond
        (pp_term e_then PRight) then_
    in
    (maybe_paren outer e_default pos pp) fmt (i, t)
  | Tif (i,t, Some e)    ->
    let pp fmt (cond, then_, else_) =
      Format.fprintf fmt "@[if %a then @\n  @[%a @]@\nelse @\n  @[%a @]@]"
        (pp_term e_if PRight) cond
        (pp_term e_then PRight) then_
        (pp_term e_else PRight) else_
    in
    (maybe_paren outer e_default pos pp) fmt (i, t, e)
  | Traise e -> Format.fprintf fmt "raise %a" pp_exn e
  | Tmem (e1,e2) ->
    Format.fprintf fmt "mem %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Tvar i -> pp_str fmt i
  | Tdoti (i1,i2) -> Format.fprintf fmt "%a.%a" pp_str i1 pp_str i2
  | Tdot (e1,e2) ->
    Format.fprintf fmt "%a.%a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_term outer pos) e2
  | Tassign (e1,e2) ->
    Format.fprintf fmt "%a <- %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_term outer pos) e2
  | Tadd (e1,e2) ->
    Format.fprintf fmt "add %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Tset (e1,e2,e3) ->
    Format.fprintf fmt "set %a %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
      (pp_with_paren (pp_term outer pos)) e3
  | Teq (Tycoll _, e1, e2) ->
    Format.fprintf fmt "%a == %a" (pp_term outer pos) e1 (pp_term outer pos) e2
  | Tunion (e1, e2) ->
    Format.fprintf fmt "union %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Tinter (e1, e2) ->
    Format.fprintf fmt "inter %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Told e ->
    Format.fprintf fmt "old %a"
      (pp_with_paren (pp_term outer pos)) e
  | Tsingl e ->
    Format.fprintf fmt "singl %a"
      (pp_with_paren (pp_term outer pos)) e
  | Tempty e ->
    Format.fprintf fmt "is_empty %a"
      (pp_with_paren (pp_term outer pos)) e
  | Tint i -> pp_str fmt (string_of_int i)
  | Tforall (ud,b) ->
    Format.fprintf fmt "@[forall %a.@\n%a@]"
      (pp_list ", " pp_univ_decl) ud
      (pp_term e_default PRight) b
  | Timpl (e1,e2) ->
    Format.fprintf fmt "@[%a ->@\n%a @]"
      (pp_term e_default PRight) e1
      (pp_term e_default PRight) e2
  | Tgt (_,e1,e2) ->
    Format.fprintf fmt "%a > %a"
      (pp_term e_default PRight) e1
      (pp_term e_default PRight) e2
  | Tapp (f,a) ->
    Format.fprintf fmt "%a %a"
      (pp_with_paren (pp_term outer pos)) f
      (pp_list " " (pp_with_paren (pp_term outer pos))) a
  | Tget (e1,e2) ->
    Format.fprintf fmt "get %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | _ -> pp_str fmt "NOT IMPLEMENTED"

(* -------------------------------------------------------------------------- *)

let pp_raise fmt raises =
  if List.length raises = 0
  then pp_str fmt ""
  else
    Format.fprintf fmt "raises { %a }@\n"
      (pp_list ", " pp_exn) raises


let pp_formula fmt f =
  Format.fprintf fmt "@[ [@expl:%a]@\n %a @]"
    pp_str f.id
    (pp_term e_default PRight) f.body

let pp_ensures fmt ensures =
  if List.length ensures = 0
  then pp_str fmt ""
  else
    Format.fprintf fmt "ensures {@\n %a @\n}@\n"
      (pp_list "@\n}@\nensures {@\n" pp_formula) ensures

let pp_fun fmt (s : fun_struct) =
  let pp_arg fmt (id, t) =
    Format.fprintf fmt "(%a : %a)"
      pp_id id
      pp_type t in
  Format.fprintf fmt "let %a %a @\n%a%a=@[  %a@]@."
    pp_id s.name
    (pp_list " " pp_arg) s.args
    pp_raise s.raises
    pp_ensures s.ensures
    (pp_term e_top PRight) s.body

(* -------------------------------------------------------------------------- *)

let pp_mutable fmt m =
  let m_str = if m then "mutable" else "" in
  pp_str fmt m_str

let pp_field fmt (f : field) =
  Format.fprintf fmt "%a %a : %a"
    pp_mutable f.mutable_
    pp_str f.name
    pp_type f.typ

(* -------------------------------------------------------------------------- *)

let pp_init fmt ((f : field),i) =
  Format.fprintf fmt "%a = %a"
    pp_str f.name
    (pp_term e_default PRight) i

let pp_invariants fmt invs =
  if List.length invs = 0
  then pp_str fmt ""
  else
    Format.fprintf fmt " invariant {@\n %a @\n}@\n"
      (pp_list "@\n}@\n invariant {@\n " pp_formula) invs

let pp_storage fmt (s : storage_struct) =
  Format.fprintf fmt "type storage_ = {@\n  @[%a @]@\n}%aby {@\n  @[%a @]@\n}"
    (pp_list ";@\n" pp_field) (fst (List.split s.fields))
    pp_invariants s.invariants
    (pp_list ";@\n" pp_init) s.fields

(* -------------------------------------------------------------------------- *)

let pp_decl fmt = function
  | Duse _     -> Format.fprintf fmt "TODO: use"
  | Dclone _   -> Format.fprintf fmt "TODO: clone"
  | Drecord _  -> Format.fprintf fmt "TODO: record"
  | Dstorage s -> pp_storage fmt s
  | Daxiom _   -> Format.fprintf fmt "TODO: axiom"
  | Dfun s     -> pp_fun fmt s

let pp_mlw_tree fmt (tree : mlw_tree) =
  Format.fprintf fmt "%a"
    (pp_list "@\n@\n" pp_decl) tree.decls

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a" pp x

let show_mlw_tree x = string_of__of_pp pp_mlw_tree x
