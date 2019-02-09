open Location
open Ident

type lident = ident loced
[@@deriving show {with_path = false}]

type role = {
    name         : lident;
    default      : rexpr option;
  }
[@@deriving show {with_path = false}]

and rexpr =
  | Ror of rexpr * rexpr
  | Rrole of lident
  | Raddress of string
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
  | VTint
  | VTuint
  | VTdate
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
[@@deriving show {with_path = false}]

type ptyp_unloc =
  | Tasset of lident
  | Tbuiltin of vtyp
  | Tcontainer of ptyp * container
  | Tapp of ptyp * ptyp
  | Tnuplet of ptyp list
[@@deriving show {with_path = false}]

and ptyp = ptyp_unloc loced
[@@deriving show {with_path = false}]

type ltyp_unloc =
  | LTprog of ptyp
  | LTvset of vset * ltyp
[@@deriving show {with_path = false}]

and ltyp = ltyp_unloc loced
[@@deriving show {with_path = false}]

let pp_big_int fmt n = Format.fprintf fmt "%s" (Big_int.string_of_big_int n)
type big_int = Big_int.big_int

(* basic variable *)
type bval_unloc =
  | BVint          of big_int
  | BVuint         of big_int
  | BVfloat        of float
  | BVdate         of string (* todo : plus Bat.date *)
  | BVstring       of string
  | BVcurrency     of float
[@@deriving show {with_path = false}]

and bval = bval_unloc loced

type decl_unloc = {
    name         : lident;
    typ          : ptyp option;
    default      : bval option;
}
[@@deriving show {with_path = false}]

and decl = decl_unloc loced

type variable = {
    decl         : decl;
    constant     : bool;
}
[@@deriving show {with_path = false}]

type arg = decl
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
[@@deriving show {with_path = false}]

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
[@@deriving show {with_path = false}]

type signature_unloc = {
  name : const;
  arrity: int;
  return: ptyp;
  args: ptyp list;
}
[@@deriving show {with_path = false}]

and signature = signature_unloc loced

type quantifier =
  | Forall
  | Exists
[@@deriving show {with_path = false}]

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
[@@deriving show {with_path = false}]

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
[@@deriving show {with_path = false}]

and pterm = pterm_unloc loced
[@@deriving show {with_path = false}]

type cond = pterm
[@@deriving show {with_path = false}]

type label_lterm = (lident option * lterm)
[@@deriving show {with_path = false}]

type specification_unloc = {
    variables  : variable list;
    action     : pterm option;
    invariants : label_lterm list;
    ensures    : label_lterm list;
  }
[@@deriving show {with_path = false}]

and specification = specification_unloc loced
[@@deriving show {with_path = false}]

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
[@@deriving show {with_path = false}]

and transaction = transaction_unloc loced
[@@deriving show {with_path = false}]

type state = lident
[@@deriving show {with_path = false}]

type stmachine = {
    name         : lident;
    states       : state list;
    initial      : state;
    transitions  : lident list; (* transaction name list *)
}
[@@deriving show {with_path = false}]

type asset_unloc = {
    name         : lident;
    args         : arg list;
    key          : lident;
    sort         : lident list option;
    role         : bool;
    init         : pterm option;
    preds        : pterm list option;
}
[@@deriving show {with_path = false}]

and asset = asset_unloc loced

type enum_unloc = {
  name : lident;
  vals : lident list;
}
[@@deriving show {with_path = false}]

and enum = enum_unloc loced

type function_unloc = {
    name         : lident;
    args         : arg list;
    return       : ptyp option;
    body         : pterm;
  }
[@@deriving show {with_path = false}]

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
[@@deriving show {with_path = false}]

and model = model_unloc loced

let get_decl_id (a : decl)     = a |> unloc |> fun x -> x.name |> unloc

let get_asset_name (a : asset) = a |> unloc |> fun x -> x.name |> unloc

let get_asset_names m = (unloc m).assets |> List.map get_asset_name

(* returns a list of triplet : asset name, field name, field type *)
let get_asset_fields m =
  List.fold_left (fun acc (a : asset) ->
      let id = (unloc a).name in
      List.fold_left (fun acc (arg : decl) ->
          acc @ [id, (unloc arg).name, (unloc arg).typ]
        ) acc (unloc a).args
    ) [] m.assets
