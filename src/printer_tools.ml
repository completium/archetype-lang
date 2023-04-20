open Core
open Location
open Ident
open Tools

exception Anomaly of string

type lident = ident loced
type mident = lident option * lident

let pp_void _fmt _ = ()

let pp_neutral pp fmt x =
  pp fmt x

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_big_int fmt bi =
  Format.fprintf fmt "%s" (Big_int.string_of_big_int bi)

let pp_newline fmt _ = Format.fprintf fmt "@\n"

let pp_nl pp fmt x = Format.fprintf fmt "%a@\n" pp x

(* -------------------------------------------------------------------------- *)
let pp_list sep pp =
  Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt "%(%)" sep)
    pp

let pp_no_empty_list_with_sep sep pp fmt l =
  if List.is_empty l
  then ()
  else (pp_list sep pp) fmt l

let pp_no_empty_list pp fmt l =
  pp_no_empty_list_with_sep "@\n" pp fmt l

let pp_no_empty_list2 pp fmt l =
  if List.is_empty l
  then ()
  else Format.fprintf fmt "%a@\n" (pp_no_empty_list pp) l

(* -------------------------------------------------------------------------- *)
let pp_ident fmt str = Format.fprintf fmt "%s" str

let pp_id fmt (id : lident) =
  Format.fprintf fmt "%s" (unloc id)

let pp_mid fmt (id : mident) =
  let pp_nm fmt = function
    | None -> ()
    | Some nm -> Format.fprintf fmt "%a::" pp_id nm
  in
  Format.fprintf fmt "%a%s" pp_nm (fst id) (unloc (snd id))

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

let pp_some pp fmt x =
  match x with
  | Some x -> pp fmt x
  | None -> ()

(* -------------------------------------------------------------------------- *)
type assoc  = Left | Right | NonAssoc
type pos    = PLeft | PRight | PInfix | PNone


let maybe_paren outer inner pos pp =
  let c =
    match (outer, inner, pos) with
    | ((o, Right), (i, Right), PLeft) when o >= i -> true
    | ((o, Right), (i, NonAssoc), _) when o >= i -> true
    | ((o, Right), (i, Left), _) when o >= i -> true
    | ((o, Left), (i, Left), _) when o >= i -> true
    | ((o, NonAssoc), (i, _), _) when o >= i -> true
    | _ -> false
  in pp_maybe_paren c pp

(* -------------------------------------------------------------------------- *)

let pp_version fmt _ = pp_str fmt Options.version

let pp_bin fmt _ = Format.fprintf fmt "archetype %a" pp_version ()
