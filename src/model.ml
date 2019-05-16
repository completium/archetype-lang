open Location
open Ident
(* open Tools *)

(* type ('id, 'typ) vtyp =
  | VTbool
  | VTint
  | VTuint
  | VTdate
  | VTduration
  | VTstring
  | VTaddress
  | VTrole
  | VTcurrency of currency * ('id, 'typ) transfer option
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

type ptyp =
  | Tasset of lident
  | Tbuiltin of vtyp
  | Tcontainer of ptyp * container
  | Ttuple of ptyp list
[@@deriving show {with_path = false}]

type ltyp =
  | LTprog of ptyp
  | LTvset of vset * ltyp
[@@deriving show {with_path = false}] *)


type ttype_ = (* type of pterm *)
| Ttype1
| Ttype2
[@@deriving show {with_path = false}]

type ltype_ = (* type of lterm *)
| Ltype1
| Ltype2
[@@deriving show {with_path = false}]

(* operators and constants *)
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


type lident = ident loced

let pp_lident fmt i = Format.fprintf fmt "%s" (unloc i)

type ('typ, 'node) struct_poly = {
  node : 'node;
  type_ : 'typ;
  fvs : (string * 'typ) list;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'qualid) qualid_poly = ('typ, (('id, 'qualid) qualid_node)) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'qualid) qualid_node =
  | Qident of 'id
  | Qdot of 'qualid * 'id
[@@deriving show {with_path = false}]

type ('id, 'typ) qualid_gen = ('id, 'typ, ('id, 'typ) qualid_gen) qualid_poly
[@@deriving show {with_path = false}]

type qualid = (lident, ttype_) qualid_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'rexpr) rexpr_poly = ('typ, ('id, 'typ, 'rexpr) rexpr_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'typ, 'rexpr) rexpr_node =
  | Rqualid of ('id, 'typ) qualid_gen
  | Ror of 'rexpr * 'rexpr
  | Raddress of 'id
[@@deriving show {with_path = false}]

type ('id, 'typ) rexpr_gen = ('id, 'typ, ('id, 'typ) rexpr_gen) rexpr_poly
[@@deriving show {with_path = false}]

type rexpr = (lident, ttype_) rexpr_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ) role = {
  name    : 'id;
  default : ('id, 'typ) rexpr_gen option;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'sexpr) sexpr_poly = ('typ, ('id, 'sexpr) sexpr_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'sexpr) sexpr_node =
  | Sref of 'id
  | Sor of 'sexpr * 'sexpr
  | Sany
[@@deriving show {with_path = false}]

type ('id, 'typ) sexpr_gen = ('id, 'typ, ('id, 'typ) sexpr_gen) sexpr_poly
[@@deriving show {with_path = false}]

type sexpr = (lident, ttype_) sexpr_gen
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type currency =
  | Tez
  | Mutez
[@@deriving show {with_path = false}]

type ('id, 'typ) transfer = {
  from : ('id, 'typ) role;
  tto  : ('id, 'typ) role;
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


(* -------------------------------------------------------------------- *)

(* basic variable *)
type 'typ bval_poly = ('typ, bval_node) struct_poly
[@@deriving show {with_path = false}]

and bval_node =
  | BVint          of Core.big_int
  | BVuint         of Core.big_int
  | BVbool         of bool
  | BVenum         of string
  | BVrational     of Core.big_int * Core.big_int
  | BVdate         of string (* TODO : find a date structure *)
  | BVstring       of string
  | BVcurrency     of currency * Core.big_int
  | BVaddress      of string
  | BVduration     of string
[@@deriving show {with_path = false}]

type 'typ bval_gen = 'typ bval_poly
[@@deriving show {with_path = false}]

type bval = ttype_ bval_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'term) decl_gen = {
  name    : 'id;
  typ     : 'typ option;
  default : 'term option;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ) signature = {
  name : 'id;
  args: 'typ list;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type quantifier =
  | Forall
  | Exists
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'pattern) pattern_poly = ('typ, ('id, 'typ, 'pattern) pattern_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'typ, 'pattern) pattern_node =
  | Mwild
  | Mvar of 'id
  | Mapp of ('id, 'typ) qualid_gen * 'pattern list
  | Mrec of (('id, 'typ) qualid_gen * 'pattern) list
  | Mtuple of 'pattern list
  | Mas of 'pattern * 'id * bool
  | Mor of 'pattern * 'pattern
  | Mscope of ('id, 'typ) qualid_gen * 'pattern
  | Mparen of 'pattern
  | Mghost of 'pattern
[@@deriving show {with_path = false}]

type ('id, 'typ) pattern_gen = ('id, 'typ, ('id, 'typ) pattern_gen) pattern_poly
[@@deriving show {with_path = false}]

type pattern = (lident, ttype_) pattern_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'term) lterm_poly = (ltype_, ('id, 'term) lterm_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'term) lterm_node =
  | Lquantifer of quantifier * 'id * ltype_ * 'term
  | Limply of 'term * 'term
  | Lletin of 'id * 'term * ltype_ * 'term
  | Lnot of 'term
  | Lapp of 'term * 'term list
  | Llogical of logical_operator * 'term * 'term
  | Lcomp of comparison_operator * 'term * 'term
  | Larith of arithmetic_operator * 'term * 'term
  | Luarith of unary_arithmetic_operator * 'term
  | Lvar of 'id
  | Larray of 'term list
  | Llit of ltype_ bval_gen
  | Ldot of 'term * 'id
  | Lconst of const
  | Ltuple of 'term list
[@@deriving show {with_path = false}]

type ('id) lterm_gen = ('id, ('id) lterm_gen) lterm_poly
[@@deriving show {with_path = false}]

type lterm = lident lterm_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'term) pterm_poly = ('typ, ('id, 'typ, 'term) pterm_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'typ, 'term) pterm_node  =
  | Papp of ('term * ('term) list)
  | Pmethod of ('term * 'id * ('term) list)
  | Plambda of 'id * 'typ option * bool * 'term
  | Plogical of logical_operator * 'term * 'term
  | Pnot of 'term
  | Pcomp of comparison_operator * 'term * 'term
  | Parith of arithmetic_operator * 'term * 'term
  | Puarith of unary_arithmetic_operator * 'term
  | Peffect of (assignment_operator * 'id * 'term) list
  | Precord of (('id, 'typ) qualid_gen * 'term) list
  | Pletin of 'id * 'term * 'typ option * 'term
  | Pvar of 'id
  | Parray of 'term list
  | Plit of 'typ bval_gen
  | Pdot of 'term * 'id
  | Pconst of const
  | Ptuple of 'term list
[@@deriving show {with_path = false}]

type ('id, 'typ) pterm_gen = ('id, 'typ, ('id, 'typ) pterm_gen) pterm_poly
[@@deriving show {with_path = false}]

type pterm = (lident, ttype_, pterm) pterm_poly
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type ('id, 'typ, 'term, 'instr) instruction_poly = ('typ, ('id, 'typ, 'term, 'instr) instruction_node) struct_poly
[@@deriving show {with_path = false}]

and ('id, 'typ, 'term, 'instr) instruction_node =
  | Iif of ('term * 'instr * 'instr option)                      (* condition * then_ * else_ *)
  | Ifor of ('id * 'term * 'term * 'id option)                   (* id * collection * body * label *)
  | Iseq of ('instr * 'instr)                                    (* lhs ; rhs*)
  | Imatchwith of 'term * (('id, 'typ) pattern_gen * 'term) list (* match 'term with ('pattern * 'term) list *)
  | Iassign of (assignment_operator * 'term * 'term)             (* $2 assignment_operator $3 *)
  | Irequire of (bool * 'term)                                   (* $1 ? require : failwith *)
  | Itransfer of ('term * bool * ('id, 'typ) qualid_gen option)  (* value * back * dest *)
  | Ibreak
  | Iassert of 'term
  | Isimple of 'term
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) instruction_gen = ('id, 'typ, 'term, ('id, 'typ, 'term) instruction_gen) instruction_poly
[@@deriving show {with_path = false}]

and instruction = (lident, pattern, pterm, instruction) instruction_poly


(* -------------------------------------------------------------------- *)

type ('id, 'term) label_term = {
  label : 'id option;
  term : 'term;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) variable = {
  decl         : ('id, 'typ, 'term) decl_gen; (* TODO *)
  constant     : bool;
  from         : ('id, 'typ) qualid_gen option;
  to_          : ('id, 'typ) qualid_gen option;
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id predicate = {
  name : 'id;
  args : ('id * ('id lterm_gen)) list;
  body : 'id lterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ) definition = {
  name : 'id;
  typ  : 'typ;
  id   : 'id;
  def  : 'id lterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) verification = {
  predicates  : 'id predicate list;
  definitions : ('id, 'typ) definition list;
  axioms      : ('id, 'id lterm_gen) label_term list;
  theorems    : ('id, 'id lterm_gen) label_term list;
  variables   : ('id, 'typ, 'term) variable list;
  invariants  : ('id * ('id, 'id lterm_gen) label_term list) list;
  effect      : 'term option;
  specs       : ('id, 'id lterm_gen) label_term list;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) function_struct = {
  name         : 'id;
  args         : (('id, 'typ, 'typ bval_gen) decl_gen) list;
  body         : ('id, 'typ, 'term) instruction_gen;
  side         : bool; (* true if function contains a failwith call *)
  return       : 'typ;
  fvs          : (ident * 'typ) list [@opaque];
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type function_ = (lident, ttype_, pterm) function_struct
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) transition = {
  from : ('id, 'typ) sexpr_gen;
  on   : ('id * 'id) option;
  trs  : ('id * 'term option * 'term option) list; (* to * condition * action*)
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) transaction_struct = {
  name            : 'id;
  args            : (('id, 'typ, 'typ bval_gen) decl_gen) list;
  calledby        : ('id, 'typ) rexpr_gen option;
  accept_transfer : bool;
  require         : ('id, 'term) label_term list option;
  transition      : ('id, 'typ, 'term) transition option;
  functions       : ('id, 'typ, 'term) function_struct list;
  verification    : ('id, 'typ, 'term) verification option;
  effect          : ('id, 'typ, 'term) instruction_gen option;
  side            : bool;
  loc             : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type transaction = (lident, ttype_, pterm) transaction_struct
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) enum_item_struct = {
  name : 'id;
  initial : bool;
  verification : ('id, 'typ, 'term) verification option;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) enum_struct = {
  name : 'id; (* "_state" if it's coming from Dstates constructor *)
  items : ('id, 'typ, 'term) enum_item_struct list;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type enum = (lident, ttype_, pterm) enum_struct
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) asset_struct = {
  name  : 'id;
  args  : ('id, 'typ, 'term) decl_gen list;
  key   : 'id;
  sort  : 'id list;
  state : 'id option;
  role  : bool;
  init  : 'term option;
  specs : ('id, 'id lterm_gen) label_term list;
  loc   : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type asset = (lident, ttype_, pterm) asset_struct

type ('id, 'typ) predicate_struct = {
  name         : 'id;
  args         : (('id, 'typ, 'typ bval_gen) decl_gen) list;
  return       : 'typ option;
  body         : 'id lterm_gen;
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term) contract = {
  name       : 'id;
  signatures : ('id, 'typ) signature list;
  init       : 'term option;
  loc        : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type ('id, 'typ, 'term)  model_struct = {
  name          : 'id;
  variables     : ('id, 'typ, 'term) variable list;
  assets        : ('id, 'typ, 'term) asset_struct list;
  functions     : ('id, 'typ, 'term) function_struct list;
  transactions  : ('id, 'typ, 'term) transaction_struct list;
  enums         : ('id, 'typ, 'term) enum_struct list;
  contracts     : ('id, 'typ, 'term) contract list;
  verifications : ('id, 'typ, 'term) verification list;
  loc           : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

and model = (lident, ttype_, pterm) model_struct



(* TOOLS *)

(* mk *)

let mk_model id = {
  name          = id;
  variables     = [];
  assets        = [];
  functions     = [];
  transactions  = [];
  enums         = [];
  contracts     = [];
  verifications = [];
  loc           = Location.dummy;
}

(*
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

let poly_pterm_map_for_pterm f g = poly_pterm_map f id id id g id

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
*)
