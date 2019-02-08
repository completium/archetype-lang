open Location

type ident = string
type lident = ident loced

type role = {
    name         : lident;
    default      : rexpr option;
  }
and rexpr =
  | Ror of rexpr * rexpr
  | Rrole of lident

type currency =
  | Tez
  | Mutez

type transfer = {
    from : role;
    tto  : role;
}

type container =
  | Collection
  | Queue
  | Stack
  | Set
  | Subset
  | Partition

type vtyp =
  | VTint
  | VTuint
  | VTdate
  | VTstring
  | VTaddress
  | VTcurrency of currency * transfer option

type vset =
  | VSremoved
  | VSadded
  | VSstable
  | VSbefore
  | VSafter

type ptyp_unloc =
  | Tasset of lident
  | Tbuiltin of vtyp
  | Tcontainer of ptyp * container
  | Tapp of ptyp * ptyp
  | Tnuplet of ptyp list

and ptyp = ptyp_unloc loced

type ltyp_unloc =
  | LTprog of ptyp
  | LTvset of vset * ltyp

and ltyp = ltyp_unloc loced

(* basic variable *)
type bval_unloc =
  | BVint          of Big_int.big_int
  | BVuint         of Big_int.big_int
  | BVfloat        of float
  | BVdate         of string (* todo : plus Bat.date *)
  | BVstring       of string
  | BVcurrency     of float

and bval = bval_unloc loced

type decl_unloc = {
    name         : lident;
    typ          : ptyp option;
    default      : bval option;
}

and decl = decl_unloc loced

type variable = {
    decl         : decl;
    constant     : bool;
}

type arg = decl

type logical_operator =
  | And
  | Or
  | Imply
  | Equiv

type comparison_operator =
  | Equal
  | Nequal
  | Gt
  | Ge
  | Lt
  | Le

type assignment_operator =
  | SimpleAssign
  | PlusAssign
  | MinusAssign
  | MultAssign
  | DivAssign
  | AndAssign
  | OrAssign

type arithmetic_operator =
  | Plus
  | Minus
  | Mult
  | Div

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

type signature_unloc = {
  name : const;
  arrity: int;
  return: ptyp;
  args: ptyp list;
}

and signature = signature_unloc loced

type quantifier =
  | Forall
  | Exists

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

and pterm = pterm_unloc loced

type cond = pterm

type label_lterm = (lident option * lterm)

type specification_unloc = {
    variables  : variable list;
    action     : pterm option;
    invariants : label_lterm list;
    ensures    : label_lterm list;
  }

and specification = specification_unloc loced

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

and transaction = transaction_unloc loced

type state = lident

type stmachine = {
    name         : lident;
    states       : state list;
    initial      : state;
    transitions  : lident list; (* transaction name list *)
}

type asset_unloc = {
    name         : lident;
    args         : arg list;
    key          : lident;
    sort         : lident list option;
    role         : bool;
    init         : pterm option;
    preds        : pterm list option;
}

and asset = asset_unloc loced

type enum_unloc = {
  name : lident;
  vals : lident list;
}

and enum = enum_unloc loced

type function_unloc = {
    name         : lident;
    args         : arg list;
    return       : ptyp option;
    body         : pterm;
  }

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
