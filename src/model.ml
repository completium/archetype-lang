
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
  | VTfloat
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

(* basic value *)
type bval =
  | BVint          of Big_int.big_int
  | BVuint         of Big_int.big_int
  | BVfloat        of float
  | BVdate         of string (* todo : plus Bat.date *)
  | BVstring       of string
  | BVcurrency     of float

type decl = {
    name         : ident;
    typ          : ptyp;
    default      : bval option;
}

type value = {
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

type reference =
  | Rvar of ident
  | Rfield of reference * ident
  | Rasset of ident

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
  | Lrel of ident
  (* below is common entries with lterm *)
  | Lletin of ident * lterm * ltyp * lterm
  | Lapp of lterm * lterm list
  | Llambda of ident * ltyp * lterm
  | Llogical of logical_operator * lterm * lterm
  (* mutualize below with pterm ? *)
  | Lcomp of comparison_operator * lterm * lterm
  | Larith of arithmetic_operator * lterm * lterm
  | Lref of reference
  | Llit of bval
  | Lconst of const

type pterm =
  (* program specific *)
  | Pif of pterm * pterm * pterm option
  | Pfor of ident * pterm * pterm
  | Passign of assignment_operator * reference * pterm
  | Ptransfer
  | Ptransition
  | Pbreak
  | Pseq of pterm list
  | Pnot of pterm
  | Passert of lterm
  (* below is common entries with lterm *)
  | Pletin of ident * pterm * ptyp * pterm
  | Papp of pterm * pterm list
  | Plambda of ident * ptyp * pterm
  | Plogical of logical_operator * pterm * pterm
  (* mutualize below with lterm ? *)
  | Pcomp of comparison_operator * pterm * pterm
  | Parith of arithmetic_operator * pterm * pterm
  | Pref of reference
  | Plit of bval
  | Pconst of const

type cond = pterm

type transaction = {
    name         : ident;
    args         : arg list;
    calledby     : rexpr option;
    condition    : cond option;
    transferred  : cond option;
    transition   : (ident * ident * ident) option; (* state * state from * state to *)
    ensures      : lterm list option;
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

type model = {
    name         : ident;
    roles        : role list;
    values       : value list;
    assets       : asset list;
    transactions : transaction list;
    stmachines   : stmachine list;
    enums        : enum list;
    preds        : lterm list;
}
