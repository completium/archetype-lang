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

(* -------------------------------------------------------------------------- *)

let pp_type fmt typ =
  let typ_str =
    match typ with
    | Tyint      -> "int"
    | Tystring   -> "string"
    | Tyaddr     -> "address"
    | Tystorage  -> "storage_"
    | Tyunit     -> "unit"
    | Tyrecord i -> i
    | Tyasset  i -> i in
  pp_str fmt typ_str

(* -------------------------------------------------------------------------- *)

let pp_exn fmt e =
  let e_str =
    match e with
    | Ekeyexist -> "KeyExist"
    | Enotfound -> "NotFound" in
  pp_str fmt e_str

(* -------------------------------------------------------------------------- *)

let rec pp_term outer pos fmt = function
  | Tseq l         -> Format.fprintf fmt "%a" (pp_list "@\n" (pp_term outer pos)) l
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
  | _ -> pp_str fmt "NOT IMPLEMENTED"

(* -------------------------------------------------------------------------- *)

let pp_raise fmt raises =
  if List.length raises = 0
  then pp_str fmt ""
  else
    Format.fprintf fmt "raises { %a }@\n"
      (pp_list ", " pp_exn) raises

let pp_fun fmt (s : fun_struct) =
  let pp_arg fmt (id, t) =
    Format.fprintf fmt "(%a : %a)"
      pp_id id
      pp_type t in
  Format.fprintf fmt "let %a %a @\n%a=@[  %a@]@."
    pp_id s.name
    (pp_list " " pp_arg) s.args
    pp_raise s.raises
    (pp_term e_top PRight) s.body

(* -------------------------------------------------------------------------- *)

let pp_decl fmt = function
  | Duse _     -> Format.fprintf fmt "TODO: use"
  | Dclone _   -> Format.fprintf fmt "TODO: clone"
  | Drecord _  -> Format.fprintf fmt "TODO: record"
  | Dstorage _ -> Format.fprintf fmt "TODO: storage"
  | Daxiom _   -> Format.fprintf fmt "TODO: axiom"
  | Dfun s     -> pp_fun fmt s

let pp_mlw_tree fmt (tree : mlw_tree) =
  Format.fprintf fmt "%a"
    (pp_list "@\n" pp_decl) tree.decls

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a" pp x

let show_mlw_tree x = string_of__of_pp pp_mlw_tree x
