open Location
open Ident

(*
type 'a loced = {
  pldesc : 'a;
}
[@@deriving show]

let unloc  x = x.pldesc

let mkloc (x : 'a) : 'a loced =
  { pldesc = x; }

let mkdummy (x : 'a) : 'a loced =
  mkloc x
*)

type lident = ident loced
[@@deriving show]

type role = {
    name         : lident;
    default      : rexpr option;
  }
[@@deriving show]

and rexpr =
  | Ror of rexpr * rexpr
  | Rrole of lident
[@@deriving show]

type currency =
  | Tez
  | Mutez
[@@deriving show]

type transfer = {
    from : role;
    tto  : role;
}
[@@deriving show]

type container =
  | Collection
  | Queue
  | Stack
  | Set
  | Subset
  | Partition
[@@deriving show]

type vtyp =
  | VTint
  | VTuint
  | VTdate
  | VTstring
  | VTaddress
  | VTcurrency of currency * transfer option
[@@deriving show]

type vset =
  | VSremoved
  | VSadded
  | VSstable
  | VSbefore
  | VSafter
[@@deriving show]

type ptyp_unloc =
  | Tasset of lident
  | Tbuiltin of vtyp
  | Tcontainer of ptyp * container
  | Tapp of ptyp * ptyp
  | Tnuplet of ptyp list
[@@deriving show]

and ptyp = ptyp_unloc loced
[@@deriving show]

type ltyp_unloc =
  | LTprog of ptyp
  | LTvset of vset * ltyp
[@@deriving show]

and ltyp = ltyp_unloc loced
[@@deriving show]

(* basic variable *)
type bval_unloc =
(*  | BVint          of Big_int.big_int [@printer fun fmt -> fprintf fmt "%s" Big_int.string_of_big_int]
    | BVuint         of Big_int.big_int*)
  | BVint          of int
  | BVuint         of int
  | BVfloat        of float
  | BVdate         of string (* todo : plus Bat.date *)
  | BVstring       of string
  | BVcurrency     of float
[@@deriving show]

and bval = bval_unloc loced

type decl_unloc = {
    name         : lident;
    typ          : ptyp option;
    default      : bval option;
}
[@@deriving show]

and decl = decl_unloc loced

type variable = {
    decl         : decl;
    constant     : bool;
}
[@@deriving show]

type arg = decl
[@@deriving show]

type logical_operator =
  | And
  | Or
  | Imply
  | Equiv
[@@deriving show]

type comparison_operator =
  | Equal
  | Nequal
  | Gt
  | Ge
  | Lt
  | Le
[@@deriving show]

type assignment_operator =
  | SimpleAssign
  | PlusAssign
  | MinusAssign
  | MultAssign
  | DivAssign
  | AndAssign
  | OrAssign
[@@deriving show]

type arithmetic_operator =
  | Plus
  | Minus
  | Mult
  | Div
[@@deriving show]

type const =
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
  | Cvoid
  | Cnow
  | Cmem
  | Cbefore
[@@deriving show]

type signature_unloc = {
  name : const;
  arrity: int;
  return: ptyp;
  args: ptyp list;
}
[@@deriving show]

and signature = signature_unloc loced

type quantifier =
  | Forall
  | Exists
[@@deriving show]

type lterm_unloc =
  | Lquantifer of quantifier * lident * ltyp * lterm
  | Limply of lterm * lterm
  (* below is common entries with lterm *)
  | Lrel of int
  | Lletin of lident * lterm * ltyp option * lterm
  | Lapp of lterm * lterm list
  | Llambda of lident * ltyp option * lterm
  | Llog of logical_operator * lterm * lterm
  (* mutualize below with pterm ? *)
  | Lcomp of comparison_operator * lterm * lterm
  | Larith of arithmetic_operator * lterm * lterm
  | Lvar of lident
  | Lfield of lident
  | Lasset of lident
  | Llit of bval
  | Ldot of lterm * lterm
  | Lconst of const
[@@deriving show]

and lterm = lterm_unloc loced

type pterm_unloc  =
  (* program specific *)
  | Pif of pterm * pterm * pterm option
  | Pfor of lident * pterm * pterm
  | Passign of assignment_operator * pterm * pterm
  | Ptransfer
  | Ptransition
  | Pbreak
  | Pseq of pterm list
  | Pnot of pterm
  | Passert of lterm
  (* below is common entries with lterm *)
  | Prel of int
  | Pletin of lident * pterm * ptyp * pterm
  | Papp of pterm * pterm list
  | Plambda of lident * ptyp * pterm
  | Plogical of logical_operator * pterm * pterm
  (* mutualize below with lterm ? *)
  | Pcomp of comparison_operator * pterm * pterm
  | Parith of arithmetic_operator * pterm * pterm
  | Pvar of lident
  | Pfield of lident
  | Passet of lident
  | Plit of bval
  | Pdot of pterm * pterm
  | Pconst of const
[@@deriving show]

and pterm = pterm_unloc loced
[@@deriving show]

type cond = pterm
[@@deriving show]

type label_lterm = (lident option * lterm)
[@@deriving show]

type specification_unloc = {
    variables  : variable list;
    action     : pterm option;
    invariants : label_lterm list;
    ensures    : label_lterm list;
  }
[@@deriving show]

and specification = specification_unloc loced
[@@deriving show]

type transaction_unloc = {
    name         : lident;
    args         : arg list;
    calledby     : rexpr option;
    condition    : cond option;
    transferred  : cond option;
    transition   : (lident * lident * lident) option; (* state * state from * state to *)
    spec         : specification option;
    action       : pterm option;
}
[@@deriving show]

and transaction = transaction_unloc loced
[@@deriving show]

type state = lident
[@@deriving show]

type stmachine = {
    name         : lident;
    states       : state list;
    initial      : state;
    transitions  : lident list; (* transaction name list *)
}
[@@deriving show]

type asset_unloc = {
    name         : lident;
    args         : arg list;
    key          : lident;
    sort         : lident list option;
    role         : bool;
    init         : pterm option;
    preds        : pterm list option;
}
[@@deriving show]

and asset = asset_unloc loced

type enum_unloc = {
  name : lident;
  vals : lident list;
}
[@@deriving show]

and enum = enum_unloc loced

type function_unloc = {
    name         : lident;
    args         : arg list;
    return       : ptyp option;
    body         : pterm;
  }
[@@deriving show]

and function_ = function_unloc loced

type model_unloc = {
    name         : lident;
    roles        : role list;
    variables    : variable list;
    assets       : asset list;
    functions    : function_ list;
    transactions : transaction list;
    stmachines   : stmachine list;
    enums        : enum list;
    spec         : specification option;
}
[@@deriving show]

and model = model_unloc loced

let get_asset_names m =
List.map (fun (a : asset) -> (unloc (unloc a).name)) (unloc m).assets

(* returns a list of triplet : asset name, field name, field type *)
let get_asset_fields m =
  List.fold_left (fun acc (a : asset) ->
      let id = (unloc a).name in
      List.fold_left (fun acc (arg : decl) ->
          acc @ [id, (unloc arg).name, (unloc arg).typ]
        ) acc (unloc a).args
    ) [] m.assets
