open Location
open Ident
open Tools

type lident = ident loced

let pp_lident fmt i = Format.fprintf fmt "%s" (unloc i)

type 'id qualid =
  | Qident of 'id
  | Qdot of 'id qualid * 'id
[@@deriving show {with_path = false}]

type liqualid = lident qualid
[@@deriving show {with_path = false}]

type rexpr =
  | Rqualid of liqualid
  | Ror of rexpr * rexpr
  | Raddress of string
[@@deriving show {with_path = false}]

type role = {
  name    : lident;
  default : rexpr option;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type sexpr_unloc =
  | Sref of lident
  | Sor of sexpr * sexpr
  | Sany
[@@deriving show {with_path = false}]

and sexpr = sexpr_unloc loced
[@@deriving show {with_path = false}]

type currency =
  | Tez
  | Mutez
[@@deriving show {with_path = false}]

type transfer = {
  from : role;
  tto  : role;
}
[@@deriving show {with_path = false}]

type container =
  | Collection
  | Queue
  | Stack
  | Set
  | Subset
  | Partition
[@@deriving show {with_path = false}]

type vtyp =
  | VTbool
  | VTint
  | VTuint
  | VTdate
  | VTduration
  | VTstring
  | VTaddress
  | VTrole
  | VTcurrency of currency * transfer option
  | VTobject
  | VTkey
[@@deriving show {with_path = false}]

type vset =
  | VSremoved
  | VSadded
  | VSstable
  | VSbefore
  | VSafter
  | VSfixed
[@@deriving show {with_path = false}]

type ptyp_unloc =
  | Tasset of lident
  | Tbuiltin of vtyp
  | Tcontainer of ptyp * container
  | Tapp of ptyp * ptyp
  | Ttuple of ptyp list
[@@deriving show {with_path = false}]

and ptyp = ptyp_unloc loced
[@@deriving show {with_path = false}]

type ltyp_unloc =
  | LTprog of ptyp
  | LTvset of vset * ltyp
[@@deriving show {with_path = false}]

and ltyp = ltyp_unloc loced
[@@deriving show {with_path = false}]

(* basic variable *)
type bval_unloc =
  | BVint          of Core.big_int
  | BVuint         of Core.big_int
  | BVbool         of bool
  | BVenum         of string
  | BVrational     of Core.big_int * Core.big_int
  | BVdate         of string (* todo : plus Bat.date *)
  | BVstring       of string
  | BVcurrency     of currency * Core.big_int
  | BVaddress      of string
  | BVduration     of string
[@@deriving show {with_path = false}]

and bval = bval_unloc loced

type ('typ,'term) gen_decl = {
  name    : lident;
  typ     : 'typ option;
  default : 'term option;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type decl = (ptyp, bval) gen_decl
[@@deriving show {with_path = false}]

type logical_operator =
  | And
  | Or
  | Imply
  | Equiv
[@@deriving show {with_path = false}]

type comparison_operator =
  | Equal
  | Nequal
  | Gt
  | Ge
  | Lt
  | Le
[@@deriving show {with_path = false}]

type assignment_operator =
  | ValueAssign
  | SimpleAssign
  | PlusAssign
  | MinusAssign
  | MultAssign
  | DivAssign
  | AndAssign
  | OrAssign
[@@deriving show {with_path = false}]

type arithmetic_operator =
  | Plus
  | Minus
  | Mult
  | Div
  | Modulo
[@@deriving show {with_path = false}]

type unary_arithmetic_operator =
  | Uplus
  | Uminus
[@@deriving show {with_path = false}]

type const =
  (* constant *)
  | Cstate
  | Cnow
  | Ctransferred
  | Ccaller
  | Cfail
  | Cbalance
  | Cconditions
  | Cactions
  | Cnone
  | Cany
  (* function *)
  | Cnth
  | Cclear
  | Cselect
  | Cremoveif
  | Csort
  | Ccount
  | Csum
  | Cmax
  | Cmin
  | Cenqueue
  | Cdequeue
  | Cpush
  | Cpop
  | Cadd
  | Caddifnotexist
  | Cremove
  | Cget
  | Ccontains
  | Cupdate
  | Cmem
  (* vset *)
  | Cbefore
  | Cafter
  | Cfixed
  | Cadded
  | Cremoved
[@@deriving show {with_path = false}]

type signature = {
  name : lident;
  args: ptyp list;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type quantifier =
  | Forall
  | Exists
[@@deriving show {with_path = false}]

type ('id,'typ,'pattern) pattern_unloc =
  | Mwild
  | Mvar of 'id
  | Mapp of 'id qualid * 'pattern list
  | Mrec of ('id qualid * 'pattern) list
  | Mtuple of 'pattern list
  | Mas of 'pattern * 'id * bool
  | Mor of 'pattern * 'pattern
  | Mcast of 'pattern * 'typ
  | Mscope of 'id qualid * 'pattern
  | Mparen of 'pattern
  | Mghost of 'pattern
[@@deriving show {with_path = false}]

type pattern = (lident,ptyp,pattern) pattern_unloc loced
[@@deriving show {with_path = false}]

type lterm_unloc =
  | Lquantifer of quantifier * lident * ltyp option * lterm
  | Limply of lterm * lterm
  (* below is common entries with lterm *)
  | Lrel of int
  | Lletin of lident * lterm * ltyp option * lterm
  | Lseq of lterm * lterm
  | Lnot of lterm
  | Lapp of lterm * lterm list
  | Llambda of lident * ltyp option * bool * lterm
  | Llogical of logical_operator * lterm * lterm
  (* mutualize below with pterm ? *)
  | Lcomp of comparison_operator * lterm * lterm
  | Larith of arithmetic_operator * lterm * lterm
  | Luarith of unary_arithmetic_operator * lterm
  | Lvar of lident
  | Lfield of lident
  | Lasset of lident
  | Larray of lident option * lterm list
  | Llit of bval
  | Ldot of lterm * lterm
  | Lconst of const
  | Ltuple of lterm list
  | Lrequire of bool * lterm
[@@deriving show {with_path = false}]

and lterm = lterm_unloc loced

type ('id,'typ,'pattern,'term) poly_pterm  =
  (* program specific *)
  | Pif of 'term * 'term * ('term) option
  | Pfor of 'id * 'term * 'term * 'id option
  | Passign of assignment_operator * 'term * 'term
  | Pfassign of ((assignment_operator * 'id) option * 'term) list
  | Ptransfer of 'term * bool * 'id qualid option
  | Pbreak
  | Pseq of 'term * 'term
  | Pnot of 'term
  | Passert of lterm
  | Pmatchwith of 'term * ('pattern * 'term) list
  | Precord of ('id qualid * 'term) list
  (* below is common entries with lterm *)
  | Prel of int
  | Pletin of 'id * 'term * 'typ option * 'term
  | Papp of 'term * ('term) list
  | Plambda of 'id * 'typ option * bool * 'term
  | Plogical of logical_operator * 'term * 'term
  (* mutualize below with lterm ? *)
  | Pcomp of comparison_operator * 'term * 'term
  | Parith of arithmetic_operator * 'term * 'term
  | Puarith of unary_arithmetic_operator * 'term
  | Pvar of 'id
  | Parray of 'term list
  | Plit of bval
  | Pdot of 'term * 'term
  | Pconst of const
  | Ptuple of 'term list
  | Prequire of bool * 'term
[@@deriving show {with_path = false}]

type pterm = ((lident,ptyp,pattern,pterm) poly_pterm) loced
[@@deriving show {with_path = false}]

type cond = pterm
[@@deriving show {with_path = false}]

type arg = decl
[@@deriving show {with_path = false}]

type 'a label_term = {
  label : lident option;
  term : 'a;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type label_pterm = pterm label_term
[@@deriving show {with_path = false}]

type label_lterm = lterm label_term
[@@deriving show {with_path = false}]

type variable = {
  decl         : (ptyp, pterm) gen_decl;
  constant     : bool;
  from         : liqualid option;
  to_          : liqualid option;
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type predicate = {
  name : lident;
  args : arg list;
  body : lterm;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type definition = {
  name : lident;
  typ  : ptyp;
  id   : lident;
  def  : lterm;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type verification = {
  predicates  : predicate list;
  definitions : definition list;
  axioms      : label_lterm list;
  theorems    : label_lterm list;
  variables   : variable list;
  invariants  : (lident * label_lterm list) list;
  effect      : pterm option;
  specs       : label_lterm list;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type name = lident option * lident

type ('id,'typ,'pattern,'term) gen_function = {
  name         : lident;
  args         : (('typ, bval) gen_decl) list;
  return       : 'typ option;
  body         : ('id,'typ,'pattern,'term) poly_pterm loced;
  side         : bool;
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type function_ = (lident,ptyp,pattern,pterm) gen_function
[@@deriving show {with_path = false}]

type transition = {
  from : sexpr;
  on   : (lident * lident) option;
  trs  : (lident * pterm option * pterm option) list; (* to * condition * action*)
}
[@@deriving show {with_path = false}]

type ('id,'typ,'pattern,'term) gen_transaction = {
  name         : lident;
  args         : (('typ, bval) gen_decl) list;
  calledby     : rexpr option;
  condition    : label_pterm list option;
  transition   : transition option;
  functions    : function_ list;
  verification : verification option;
  effect       : ('id,'typ,'pattern,'term) poly_pterm loced option;
  side         : bool;
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type transaction = (lident, ptyp, pattern, pterm) gen_transaction
[@@deriving show {with_path = false}]

type state_item = {
  name : lident;
  initial : bool;
  verification : verification option;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type state = {
  name : lident;
  items : state_item list;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type asset = {
  name  : lident;
  args  : arg list;
  key   : lident;
  sort  : lident list;
  state : lident option;
  role  : bool;
  init  : pterm option;
  specs : label_lterm list;
  loc   : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type enum = {
  name : lident;
  vals : lident list;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id,'typ) gen_predicate = {
  name         : lident;
  args         : (('typ, bval) gen_decl) list;
  return       : 'typ option;
  body         : lterm;
}
[@@deriving show {with_path = false}]

type contract = {
  name       : lident;
  signatures : signature list;
  init       : pterm option;
  loc        : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type model_unloc = {
  name          : lident;
  variables     : variable list;
  assets        : asset list;
  functions     : function_ list;
  transactions  : transaction list;
  states        : state list;
  enums         : enum list;
  contracts     : contract list;
  verifications : verification list;
}
[@@deriving show {with_path = false}]

and model = model_unloc loced

let get_decl_id (a : decl) = a |> fun x -> x.name |> unloc

let get_asset_name (a : asset) = a |> fun x -> x.name |> unloc

let get_asset_names m = (unloc m).assets |> List.map get_asset_name

(* returns a list of triplet : asset name, field name, field type *)
let get_asset_fields m =
  List.fold_left (fun acc (a : asset) ->
      let id = a.name in
      List.fold_left (fun acc (arg : decl) ->
          acc @ [id, arg.name, arg.typ]
        ) acc a.args
    ) [] m.assets

(* generic mapper for poly_pterm type
   f  : function called on each constructor
   fi : function called on ident argument into constructor
   ft : function called on typ argument into constructor
   fr : function called on pattern argument into constructor
   fp : function called on pterm argument into constructor
   fq : function called on qualid argument into constructor
*)
let poly_pterm_map f fi ft fr fp fq = function
  | Pif (c, t, e) -> f (Pif (fp c, fp t, map_option fp e))
  | Pfor (id, c, b, lbl) -> f (Pfor (fi id, fp c, fp b, map_option fi lbl))
  | Passign (a, e, t) -> f (Passign (a, fp e, fp t))
  | Pfassign l -> f (Pfassign (List.map (fun (a, v) ->
      let b = map_option (fun (x, y) -> (x, fi y)) a in
      (b, fp v)) l))
  | Ptransfer (e, b, q) -> f (Ptransfer (fp e, b, map_option fq q))
  | Pbreak -> f (Pbreak)
  | Pseq (lhs, rhs) -> f (Pseq (fp lhs, fp rhs))
  | Pnot e -> f (Pnot (fp e))
  | Passert l -> f (Passert l)
  | Pmatchwith (e, l) -> f (Pmatchwith (fp e, List.map (fun (p, e) -> (fr p, fp e)) l))
  | Precord l -> f (Precord (List.map (fun (q, t) -> (fq q, fp t) ) l))
  | Prel i -> f (Prel i)
  | Pletin (i, v, t, b) -> f (Pletin (fi i, fp v, ft t, fp b))
  | Papp (e, a) -> f (Papp (fp e, List.map fp a))
  | Plambda (i, t, s, b) -> f (Plambda (fi i, ft t, s, fp b))
  | Plogical (o, l, r) -> f (Plogical (o, fp l, fp r))
  | Pcomp (o, l, r) -> f (Pcomp (o, fp l, fp r))
  | Parith (o, l, r) -> f (Parith (o, fp l, fp r))
  | Puarith (u, e) -> f (Puarith (u, fp e))
  | Pvar i -> f (Pvar (fi i))
  | Parray l -> f (Parray (List.map fp l))
  | Plit v -> f (Plit v)
  | Pdot (l, r) -> f (Pdot (fp l, fp r))
  | Pconst c -> f (Pconst c)
  | Ptuple l -> f (Ptuple (List.map fp l))
  | Prequire (b, x) -> f (Prequire (b, fp x))

(* generic mapper for poly_pterm type
   f   : function called on each constructor
   acc : accumulator
*)
let poly_pterm_fold f acc = function
  | Pif (c, t, Some e) -> f (f (f acc c) t) e
  | Pif (c, t, None) -> f (f acc c) t
  | Pfor (_id, c, b, _lbl) -> f (f acc c) b
  | Passign (_a, e, t) -> f (f acc e) t
  | Pfassign l -> List.fold_left (fun acc (_, v) -> f acc v) acc l
  | Ptransfer (e, _b, _q) -> f acc e
  | Pseq (lhs, rhs) -> f (f acc lhs) rhs
  | Pnot e -> f acc e
  | Pmatchwith (e, l) -> List.fold_left (fun acc (_p, e) -> f acc e) (f acc e) l
  | Precord l -> List.fold_left (fun acc (_q, t) -> f acc t) acc l
  | Pletin (_i, _v, _t, b) -> f acc b
  | Papp (e, a) -> List.fold_left f (f acc e) a
  | Plambda (_i, _t, _s, b) -> f acc b
  | Plogical (_o, l, r) -> f (f acc l) r
  | Pcomp (_o, l, r) -> f (f acc l) r
  | Parith (_o, l, r) -> f (f acc l) r
  | Puarith (_u, e) -> f acc e
  | Parray l -> List.fold_left f acc l
  | Pdot (l, r) -> f (f acc l) r
  | Ptuple l -> List.fold_left f acc l
  | Prequire (_b, x) -> (f acc x)
  | _ -> acc

let pattern_map f fi ft fr fq = function
  | Mwild -> f Mwild
  | Mvar s -> f (Mvar (fi s))
  | Mapp (q, l) -> f (Mapp (fq q, List.map fr l))
  | Mrec l -> Mrec (List.map (fun (i, p) -> (fq i, fr p)) l)
  | Mtuple l -> Mtuple (List.map fr l)
  | Mas (p, o, g) -> Mas (fr p, fi o, g)
  | Mor (lhs, rhs) -> Mor (fr lhs, fr rhs)
  | Mcast (p, t) -> Mcast (fr p, ft t)
  | Mscope (q, p) -> Mscope (fq q, fr p)
  | Mparen p -> Mparen (fr p)
  | Mghost p -> Mghost (fr p)

let assignment_operator_to_str = function
  | ValueAssign -> "value"
  | SimpleAssign -> "simple"
  | PlusAssign -> "plus"
  | MinusAssign -> "minus"
  | MultAssign -> "mult"
  | DivAssign -> "div"
  | AndAssign -> "and"
  | OrAssign -> "or"

let mk_init_val = function
  | VTbool           -> BVbool false
  | VTint            -> BVint Big_int.zero_big_int
  | VTuint           -> BVint Big_int.zero_big_int
  | VTdate           -> BVdate "1970-01-01T00:00:00Z"
  | VTduration       -> BVduration "0s"
  | VTstring         -> BVstring ""
  | VTaddress        -> BVaddress "@none"
  | VTrole           -> BVaddress "@none"
  | VTcurrency (c,_) -> BVcurrency (c,Big_int.zero_big_int)
  | VTkey            -> BVstring ""
  | VTobject         -> BVstring ""


type basic_pattern = (string, ptyp, basic_pattern) pattern_unloc
[@@deriving show {with_path = false}]

type basic_pterm = (string, ptyp, basic_pattern, basic_pterm) poly_pterm
[@@deriving show {with_path = false}]

let lstr s = mkloc Location.dummy s

let rec loc_qualid (q : string qualid) : lident qualid =
  match q with
  | Qident s -> Qident (lstr s)
  | Qdot (q, s) -> Qdot (loc_qualid q, lstr s)

let rec loc_pattern (p : basic_pattern) : pattern =
  mkloc Location.dummy (
    match p with
    | Mwild -> Mwild
    | Mvar s -> Mvar (lstr s)
    | Mapp (q, l) -> Mapp (loc_qualid q, List.map loc_pattern l)
    | Mrec l -> Mrec (List.map (fun (i, p) -> (loc_qualid i, loc_pattern p)) l)
    | Mtuple l -> Mtuple (List.map loc_pattern l)
    | Mas (p, o, g) -> Mas (loc_pattern p, lstr o, g)
    | Mor (lhs, rhs) -> Mor (loc_pattern lhs, loc_pattern rhs)
    | Mcast (p, t) -> Mcast (loc_pattern p, t)
    | Mscope (q, p) -> Mscope (loc_qualid q, loc_pattern p)
    | Mparen p -> Mparen (loc_pattern p)
    | Mghost p -> Mghost (loc_pattern p))

let rec loc_pterm (p : basic_pterm) : pterm =
  poly_pterm_map
    (fun x -> mkloc (Location.dummy) x)
    lstr
    id
    loc_pattern
    loc_pterm
    loc_qualid
    p
