
type ident = string

type role = {
    name         : ident;
    default      : rexpr option;
  }
and rexpr =
  | Ror of rexpr * rexpr
  | Rrole of ident

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
  | VTcurrency    of currency * transfer option

type ptyp =
  | Tasset of ident
  | Tbuiltin of vtyp
  | Tcontainer of ptyp * container
  | Tapp of ptyp * ptyp
  | Tnuplet of ptyp list

type vset =
  | VSremoved
  | VSadded
  | VSstable
  | VSbefore
  | VSafter

type ltyp =
  | LTprog of ptyp
  | LTvset of vset * ltyp

(* basic variable *)
type bval =
  | BVint          of Big_int.big_int
  | BVuint         of Big_int.big_int
  | BVfloat        of float
  | BVdate         of string (* todo : plus Bat.date *)
  | BVstring       of string
  | BVcurrency     of float

type decl = {
    name         : ident;
    typ          : ptyp option;
    default      : bval option;
}

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

type signature = {
  name : const;
  arrity: int;
  return: ptyp;
  args: ptyp list;
}

type quantifier =
  | Forall
  | Exists

type lterm =
  | Lquantifer of quantifier * ident * ltyp * lterm
  | Limply of lterm * lterm
  (* below is common entries with lterm *)
  | Lrel of int
  | Lletin of ident * lterm * ltyp option * lterm
  | Lapp of lterm * lterm list
  | Llambda of ident * ltyp option * lterm
  | Llog of logical_operator * lterm * lterm
  (* mutualize below with pterm ? *)
  | Lcomp of comparison_operator * lterm * lterm
  | Larith of arithmetic_operator * lterm * lterm
  | Lvar of ident
  | Lfield of ident
  | Lasset of ident
  | Llit of bval
  | Ldot of lterm * lterm
  | Lconst of const

type pterm =
  (* program specific *)
  | Pif of pterm * pterm * pterm option
  | Pfor of ident * pterm * pterm
  | Passign of assignment_operator * pterm * pterm
  | Ptransfer
  | Ptransition
  | Pbreak
  | Pseq of pterm list
  | Pnot of pterm
  | Passert of lterm
  (* below is common entries with lterm *)
  | Prel of int
  | Pletin of ident * pterm * ptyp * pterm
  | Papp of pterm * pterm list
  | Plambda of ident * ptyp * pterm
  | Plogical of logical_operator * pterm * pterm
  (* mutualize below with lterm ? *)
  | Pcomp of comparison_operator * pterm * pterm
  | Parith of arithmetic_operator * pterm * pterm
  | Pvar of ident
  | Pfield of ident
  | Passet of ident
  | Plit of bval
  | Pdot of pterm * pterm
  | Pconst of const

type cond = pterm

type label_lterm = (ident option * lterm)

type specification = {
    variables  : variable list;
    action     : pterm option;
    invariants : label_lterm list;
    ensures    : label_lterm list;
  }

type transaction = {
    name         : ident;
    args         : arg list;
    calledby     : rexpr option;
    condition    : cond option;
    transferred  : cond option;
    transition   : (ident * ident * ident) option; (* state * state from * state to *)
    spec         : specification option;
    action       : pterm option;
}

type state = ident

type stmachine = {
    name         : ident;
    states       : state list;
    initial      : state;
    transitions  : ident list; (* transaction name list *)
}

type asset = {
    name         : ident;
    args         : arg list;
    key          : ident;
    sort         : ident list option;
    role         : bool;
    init         : pterm option;
    preds        : pterm list option;
}

type enum = {
    name         : ident;
    vals         : ident list;
}

type function_ = {
    name         : ident;
    args         : arg list;
    return       : ptyp option;
    body         : pterm;
  }

type model = {
    name         : ident;
    roles        : role list;
    variables    : variable list;
    assets       : asset list;
    functions    : function_ list;
    transactions : transaction list;
    stmachines   : stmachine list;
    enums        : enum list;
    spec         : specification option;
}

let get_asset_names m = List.map (fun (a : asset) -> a.name) m.assets

(* returns a list of triplet : asset name, field name, field type *)
let get_asset_fields m =
  List.fold_left (fun acc (a : asset) ->
      let id = a.name in
      List.fold_left (fun acc (arg : decl) ->
          acc @ [id, arg.name, arg.typ]
        ) acc a.args
    ) [] m.assets
