open Location
open Ident

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
  | Pletin of 'id * ('term) * ('typ option) * ('term)
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

type transaction = {
  name         : lident;
  args         : arg list;
  calledby     : rexpr option;
  condition    : label_pterm list option;
  transition   : (sexpr * lident * (lident * lident) option) option; (* from * to * fied *)
  spec         : specification option;
  action       : pterm option;
  loc          : Location.t [@opaque];
}
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
