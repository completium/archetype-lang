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
  | Tdoti _  -> false
  | Tdot _   -> false
  | Tvar _   -> false
  | Tenum _  -> false
  | Tnil     -> false
  | Tint _   -> false
  | Tnone    -> false
  | Tletin _ -> false
  | Tif _    -> false
  | Ttry _   -> false
  | Tresult  -> false
  | _ -> true

let pp_with_paren pp fmt x =
  pp_maybe (needs_paren x) pp_paren pp fmt x

let needs_paren = function
  | Tseq _ -> true
  | _      -> false

let pp_if_with_paren pp fmt x =
  pp_maybe (needs_paren x) pp_paren pp fmt x

(* -------------------------------------------------------------------------- *)

let pp_logic fmt m =
  let mod_str =
    match m with
    | Logic         -> " function"
    | Rec           -> " rec"
    | NoMod         -> "" in
  pp_str fmt mod_str

(* -------------------------------------------------------------------------- *)

let pp_type fmt typ =
  let rec typ_str t =
    match t with
    | Tyint         -> "int"
    | Tystring      -> "string"
    | Tydate        -> "date"
    | Tyaddr        -> "address"
    | Tyrole        -> "role"
    | Tytez         -> "tez"
    | Tystorage     -> "storage_"
    | Tyunit        -> "unit"
    | Tytransfers   -> "transfers"
    | Tycoll i      -> "acol"
    | Typartition _ -> "acol"
    | Tymap i       -> "map "^i
    | Tyrecord i    -> i
    | Tyasset i     -> i
    | Tyenum i      -> i
    | Tyoption tt   -> "option "^(typ_str tt)
    | Tylist tt     -> "list "^(typ_str tt)
    | Tybool        -> "bool"
    | Tyuint        -> "uint"
    | Tycontract i  -> i
    | Tyrational    -> "rational"
    | Tyduration    -> "duration"
    | Tykey         -> "key"
    | Tytuple l     -> "("^(String.concat ", " (List.map typ_str l))^")"
  in
  pp_str fmt (typ_str typ)

(* -------------------------------------------------------------------------- *)

let pp_exn fmt e =
  let e_str =
    match e with
    | Ekeyexist         -> "KeyExist"
    | Enotfound         -> "NotFound"
    | Einvalidcaller    -> "InvalidCaller"
    | Einvalidcondition -> "InvalidCondition"
    | Ebreak            -> "Break" in
  pp_str fmt e_str

(* -------------------------------------------------------------------------- *)

let pp_univ_decl fmt (i,t) =
  Format.fprintf fmt "%a : %a"
    (pp_list " " pp_str) i
    pp_type t

(* -------------------------------------------------------------------------- *)

let pp_ref fmt = function
  | true -> pp_str fmt " ref"
  | false -> pp_str fmt ""

let pp_lettyp fmt = function
  | Some t -> Format.fprintf fmt " : %a" pp_typ t
  | None -> pp_str fmt ""

(* -------------------------------------------------------------------------- *)

let pp_raise fmt raises =
  if List.length raises = 0
  then pp_str fmt ""
  else
    Format.fprintf fmt "raises { %a }@\n"
      (pp_list ", " pp_exn) raises

let pp_arg fmt (id, t) =
  Format.fprintf fmt "(%a : %a)"
    pp_id id
    pp_type t

let pp_args fmt l =
  if List.length l = 0
  then pp_str fmt "()"
  else Format.fprintf fmt "%a" (pp_list " " pp_arg) l

(* -------------------------------------------------------------------------- *)

let rec pp_term outer pos fmt = function
  | Tseq l         -> Format.fprintf fmt "@[%a@]" (pp_list ";@\n" (pp_term outer pos)) l
  | Tif (i,t, None)    ->
    Format.fprintf fmt "@[if %a@ then %a@]"
      (pp_term e_if PRight) t
      (pp_if_with_paren (pp_term e_then PRight)) t
  | Tif (i,t, Some e)    ->
    Format.fprintf fmt "@[if %a then @\n  @[%a @]@\nelse @\n  @[%a @]@]"
      (pp_term e_if PRight) i
      (pp_if_with_paren (pp_term e_then PRight)) t
      (pp_if_with_paren (pp_term e_else PRight)) e
  | Traise e -> Format.fprintf fmt "raise %a" pp_exn e
  | Tmem (e1,e2) ->
    Format.fprintf fmt "mem %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Tlmem (e1,e2) ->
    Format.fprintf fmt "lmem %a %a"
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
  | Teq (_, e1, e2) ->
    Format.fprintf fmt "%a = %a" (pp_term outer pos) e1 (pp_term outer pos) e2
  | Tunion (e1, e2) ->
    Format.fprintf fmt "union %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Tinter (e1, e2) ->
    Format.fprintf fmt "inter %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Tdiff (e1, e2) ->
    Format.fprintf fmt "diff %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Told e ->
    Format.fprintf fmt "old %a"
      (pp_with_paren (pp_term outer pos)) e
  | Tsingl e ->
    Format.fprintf fmt "singleton %a"
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
  | Tapp (f,[]) ->
    Format.fprintf fmt "%a ()"
      (pp_with_paren (pp_term outer pos)) f
  | Tapp (f,a) ->
    Format.fprintf fmt "%a %a"
      (pp_with_paren (pp_term outer pos)) f
      (pp_list " " (pp_with_paren (pp_term outer pos))) a
  | Tget (e1,e2) ->
    Format.fprintf fmt "get %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Trecord (None,l) ->
    Format.fprintf fmt "{@\n  @[%a@]@\n}"
      (pp_list ";@\n" pp_recfield) l
  | Trecord (Some e,l) ->
    Format.fprintf fmt "{ %a with@\n  @[%a@]@\n}"
      (pp_with_paren (pp_term outer pos)) e
      (pp_list ";@\n" pp_recfield) l
  | Tnone -> pp_str fmt "None"
  | Tenum i -> pp_str fmt i
  | Tsome e -> Format.fprintf fmt "Some %a" (pp_with_paren (pp_term e_default PRight)) e
  | Tnot e -> Format.fprintf fmt "not %a" (pp_with_paren (pp_term outer pos)) e
  | Tlist l -> pp_tlist outer pos fmt l
  | Tnil -> pp_str fmt "Nil"
  | Tcaller i -> Format.fprintf fmt "get_caller_ %a" pp_str i
  | Tle (_,e1,e2) ->
    Format.fprintf fmt "%a <= %a"
      (pp_term outer pos) e1
      (pp_term outer pos) e2
  | Tlt (_,e1,e2) ->
    Format.fprintf fmt "%a < %a"
      (pp_term outer pos) e1
      (pp_term outer pos) e2
  | Tletin (r,i,t,b,e) ->
    Format.fprintf fmt "let%a %a%a = %a in@\n%a"
      pp_ref r
      pp_str i
      pp_lettyp t
      (pp_term outer pos) b
      (pp_if_with_paren (pp_term outer pos)) e
  | Tletfun (s,e) ->
    Format.fprintf fmt "@[%a@] in@\n%a"
      pp_fun s
      (pp_term outer pos) e
  | Tfor (i,s,l,b) ->
    Format.fprintf fmt "for %a = 0 to %a do@\n%a@\n  @[%a@]@\ndone"
      pp_str i
      (pp_term outer pos) s
      pp_invariants l
      (pp_term outer pos) b
  | Ttry (b,e,c) ->
    Format.fprintf fmt "try@\n  @[%a@]@\nwith %a ->@\n  @[%a@]@\nend"
      (pp_term outer pos) b
      pp_exn e
      (pp_term outer pos) c
  | Tassert e ->
    Format.fprintf fmt "assert { %a }"
      (pp_term outer pos) e
  | Tcard e ->
    Format.fprintf fmt "card %a" (pp_with_paren (pp_term outer pos)) e
  | Tminus (_,e1,e2) ->
    Format.fprintf fmt "%a - %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Tnth (e1,e2) ->
    Format.fprintf fmt "nth %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Tdle (_,e1,e2,e3) ->
    Format.fprintf fmt "%a <= %a <= %a"
      (pp_term outer pos) e1
      (pp_term outer pos) e2
      (pp_term outer pos) e3
  | Tresult -> pp_str fmt "result"
  | Tsubset (e1,e2) ->
    Format.fprintf fmt "subset %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Ttail (e1,e2) ->
    Format.fprintf fmt "tail %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Tnow i -> Format.fprintf fmt "get_now_ %a" pp_str i
  | Tmlist (e1,i1,i2,i3,e2) ->
    Format.fprintf fmt "@[match %a with@\n| Nil -> %a@\n| Cons %a %a -> @\n  @[%a@]@\nend@]"
      pp_str i1
      (pp_term outer pos) e1
      pp_str i2
      pp_str i3
      (pp_term outer pos) e2
  | Tcons (e1,e2) ->
    Format.fprintf fmt "Cons %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Tremove (e1,e2) ->
    Format.fprintf fmt "remove %a %a"
      (pp_with_paren (pp_term outer pos)) e1
      (pp_with_paren (pp_term outer pos)) e2
  | Tnottranslated -> pp_str fmt "NOT TRANSLATED"
  | _ -> pp_str fmt "NOT IMPLEMENTED"
and pp_recfield fmt (n,t) =
  Format.fprintf fmt "%a = %a"
    pp_str n
    (pp_term e_default PRight) t
and pp_tlist outer pos fmt = function
  | []    -> pp_str fmt "Nil"
  | e::tl -> Format.fprintf fmt "Cons %a %a"
               (pp_with_paren (pp_term outer pos)) e
               (pp_tlist outer pos) tl
and pp_formula fmt f =
  Format.fprintf fmt "@[ [@expl:%a]@\n %a @]"
    pp_str f.id
    (pp_term e_default PRight) f.form
and pp_invariants fmt invs =
  if List.length invs = 0
  then pp_str fmt ""
  else
    Format.fprintf fmt "invariant {@\n %a @\n} "
      (pp_list "@\n}@\n invariant {@\n " pp_formula) invs
and pp_ensures fmt ensures =
  if List.length ensures = 0
  then pp_str fmt ""
  else
    Format.fprintf fmt "ensures {@\n %a @\n}@\n"
      (pp_list "@\n}@\nensures {@\n " pp_formula) ensures
and pp_requires fmt requires =
  if List.length requires = 0
  then pp_str fmt ""
  else
    Format.fprintf fmt "requires {@\n %a @\n}@\n"
      (pp_list "@\n}@\nrequires {@\n" pp_formula) requires
and pp_variants fmt variants =
  if List.length variants = 0
  then pp_str fmt ""
  else
    Format.fprintf fmt "variant { %a }@\n"
      (pp_list ", " (pp_term e_default PRight)) variants
and pp_fun fmt (s : fun_struct) =
  Format.fprintf fmt "let%a %a %a : %a@\n%a%a%a%a=  @[%a@]"
    pp_logic s.logic
    pp_id s.name
    pp_args s.args
    pp_type s.returns
    pp_variants s.variants
    pp_raise s.raises
    pp_requires s.requires
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

let pp_record fmt (i,fl) =
  Format.fprintf fmt "type %a = {@\n  @[%a@] @\n}"
    pp_str i
    (pp_list ";@\n" pp_field) fl

(* -------------------------------------------------------------------------- *)

let pp_init fmt (f : field) =
  Format.fprintf fmt "%a = %a"
    pp_str f.name
    (pp_term e_default PRight) f.init

let pp_storage fmt (s : storage_struct) =
  Format.fprintf fmt "type storage_ = {@\n  @[%a @]@\n} %aby {@\n  @[%a @]@\n}"
    (pp_list ";@\n" pp_field) s.fields
    pp_invariants s.invariants
    (pp_list ";@\n" pp_init) s.fields

(* -------------------------------------------------------------------------- *)

let pp_enum fmt (i,l) =
  Format.fprintf fmt "type %a =@\n @[| %a@]"
    pp_str i
    (pp_list "@\n| " pp_str) l

(* -------------------------------------------------------------------------- *)

let pp_qualid fmt q = Format.fprintf fmt "%a" (pp_list "." pp_str) q

(* -------------------------------------------------------------------------- *)

let pp_clone_subst fmt = function
  | Ctype (i,j) -> Format.fprintf fmt "type %a = %a" pp_str i pp_str j
  | Cval (i,j)  -> Format.fprintf fmt "val %a = %a" pp_str i pp_str j

let pp_clone fmt (i,j,l) =
  Format.fprintf fmt "clone %a as %a with @[%a@]"
    pp_qualid i
    pp_str j
    (pp_list ",@\n" pp_clone_subst) l

(* -------------------------------------------------------------------------- *)

let pp_decl fmt = function
  | Duse l         -> Format.fprintf fmt "use %a" pp_qualid l
  | Dclone (i,j,l) -> pp_clone fmt (i,j,l)
  | Denum (i,l)    -> pp_enum fmt (i,l)
  | Drecord (i,l)  -> pp_record fmt (i,l)
  | Dstorage s     -> pp_storage fmt s
  | Daxiom _       -> Format.fprintf fmt "TODO: axiom"
  | Dfun s         -> pp_fun fmt s

let pp_mlw_tree fmt (tree : mlw_tree) =
  Format.fprintf fmt "module %a@\n  @[%a@]@\nend"
    pp_str tree.name
    (pp_list "@\n@\n" pp_decl) tree.decls

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a" pp x

let show_mlw_tree x = string_of__of_pp pp_mlw_tree x
