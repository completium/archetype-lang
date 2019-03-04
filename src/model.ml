open Location
open Ident
open Tools

type lident = ident loced
[@@deriving show {with_path = false}]

type rexpr =
  | Ror of rexpr * rexpr
  | Rrole of lident
  | Raddress of string
  | Rasset of lident * lident
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
  | VTcurrency of currency * transfer option
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
  | BVfloat        of string
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
  | Cwhen
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
  name : const;
  arrity: int;
  return: ptyp;
  args: ptyp list;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type quantifier =
  | Forall
  | Exists
[@@deriving show {with_path = false}]

type 'id qualid =
  | Qident of 'id
  | Qdot of 'id qualid * 'id
[@@deriving show {with_path = false}]

type liqualid = lident qualid
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
  | Llambda of lident * ltyp option * lterm
  | Llogical of logical_operator * lterm * lterm
  (* mutualize below with pterm ? *)
  | Lcomp of comparison_operator * lterm * lterm
  | Larith of arithmetic_operator * lterm * lterm
  | Luarith of unary_arithmetic_operator * lterm
  | Lvar of lident
  | Lfield of lident
  | Lasset of lident
  | Larray of lterm list
  | Llit of bval
  | Ldot of lterm * lterm
  | Lconst of const
[@@deriving show {with_path = false}]

and lterm = lterm_unloc loced

type ('id,'typ,'pattern,'term) poly_pterm  =
  (* program specific *)
  | Pif of 'term * 'term * ('term) option
  | Pfor of 'id * 'term * 'term
  | Passign of assignment_operator * 'term * 'term
  | Pfassign of (assignment_operator * ('id option * 'id) * 'term) list
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
  | Plambda of 'id * 'typ option * 'term
  | Plogical of logical_operator * 'term * 'term
  (* mutualize below with lterm ? *)
  | Pcomp of comparison_operator * 'term * 'term
  | Parith of arithmetic_operator * 'term * 'term
  | Puarith of unary_arithmetic_operator * 'term
  | Pvar of 'id
  | Parray of ('term) list
  | Plit of bval
  | Pdot of 'term * 'term
  | Pconst of const
  | Ptuple of ('term) list
[@@deriving show {with_path = false}]

type pterm = ((lident,ptyp,pattern,pterm) poly_pterm) loced
[@@deriving show {with_path = false}]

type cond = pterm
[@@deriving show {with_path = false}]

type variable = {
  decl         : (ptyp, pterm) gen_decl;
  constant     : bool;
  loc : Location.t [@opaque];
}
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

type specification = {
  variables  : variable list;
  action     : pterm option;
  invariants : label_lterm list;
  ensures    : label_lterm list;
  loc        : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type name = lident option * lident

type ('id,'typ,'pattern,'term) gen_transaction = {
  name         : lident;
  args         : (('typ, bval) gen_decl) list;
  calledby     : rexpr option;
  condition    : label_pterm list option;
  transition   : (liqualid option * sexpr * (lident * pterm option * pterm option) list) option;
                 (*            id *  from * (    to *    condition *       action)) *)
  spec         : specification option;
  action       : ('id,'typ,'pattern,'term) poly_pterm loced option;
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type transaction = (lident,ptyp,pattern,pterm) gen_transaction
[@@deriving show {with_path = false}]

type state_item = {
  name : lident;
  initial : bool;
  specs : specification option;
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
  sort  : lident list option;
  role  : bool;
  init  : pterm option;
  preds : pterm list option;
  loc   : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type enum = {
  name : lident;
  vals : lident list;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id,'typ,'pattern,'term) gen_function = {
  name         : lident;
  args         : (('typ, bval) gen_decl) list;
  return       : 'typ option;
  body         : ('id,'typ,'pattern,'term) poly_pterm loced;
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type function_ = (lident,ptyp,pattern,pterm) gen_function
[@@deriving show {with_path = false}]

type model_unloc = {
  name         : lident;
  roles        : role list;
  variables    : variable list;
  assets       : asset list;
  functions    : function_ list;
  transactions : transaction list;
  states       : state list;
  enums        : enum list;
  specs        : specification list;
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
    | Pfor (id, c, b) -> f (Pfor (fi id, fp c, fp b))
    | Passign (a, e, t) -> f (Passign (a, fp e, fp t))
    | Pfassign l -> f (Pfassign (List.map (fun (a, (i, j), v) ->
        (a, (map_option fi i, fi j), fp v)) l))
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
    | Plambda (i, t, b) -> f (Plambda (fi i, ft t, fp b))
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

(* generic mapper for poly_pterm type
  f   : function called on each constructor
  acc : accumulator
*)
let poly_pterm_fold f acc = function
    | Pif (c, t, Some e) -> f (f (f acc c) t) e
    | Pif (c, t, None) -> f (f acc c) t
    | Pfor (_id, c, b) -> f (f acc c) b
    | Passign (_a, e, t) -> f (f acc e) t
    | Pfassign l -> List.fold_left (fun acc (_a, (_i, _j), v) -> f acc v) acc l
    | Ptransfer (e, _b, _q) -> f acc e
    | Pseq (lhs, rhs) -> f (f acc lhs) rhs
    | Pnot e -> f acc e
    | Pmatchwith (e, l) -> List.fold_left (fun acc (_p, e) -> f acc e) (f acc e) l
    | Precord l -> List.fold_left (fun acc (_q, t) -> f acc t) acc l
    | Pletin (_i, _v, _t, b) -> f acc b
    | Papp (e, a) -> List.fold_left f (f acc e) a
    | Plambda (_i, _t, b) -> f acc b
    | Plogical (_o, l, r) -> f (f acc l) r
    | Pcomp (_o, l, r) -> f (f acc l) r
    | Parith (_o, l, r) -> f (f acc l) r
    | Puarith (_u, e) -> f acc e
    | Parray l -> List.fold_left f acc l
    | Pdot (l, r) -> f (f acc l) r
    | Ptuple l -> List.fold_left f acc l
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
