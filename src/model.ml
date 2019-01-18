
type ident = string

type role = {
    name         : ident;
    default      : rexpr option;
} and rexpr =
  | Ror of rexpr * rexpr
  | Rrole of role

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
  | BVcurrency     of float

type decl = {
    name         : ident;
    typ          : vtyp;
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

type collection = int

type reference =
  | Rvar of ident
  | Rfield of ident * ident

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
  | Cremove
  | Cget
  | Ccontains
  | Cupdate

type signature = {
  name : const;
  arrity: int;
  return: ptyp;
  args: ptyp list;
}

type pterm =
  | Pletin of ident * ptyp * pterm
  | PIf of pterm * pterm * pterm option
  | PFor of ident * collection * pterm
  | PAssign of assignment_operator * reference * pterm
  | PTransfer
  | PTransition
  | Pbreak
  | PSeq of pterm list
  | Plogical of logical_operator * pterm * pterm
  | Pnot of pterm
  | Pcomparaison of comparison_operator * pterm * pterm
  | Parithmetic of arithmetic_operator * pterm * pterm
  | Pref of reference
  | Pliteral of bval
  | Papp of pterm * pterm list
  | PLambda of ident * ptyp * pterm
  | Pconst of signature

type cond = pterm

type quantifier =
  | Forall
  | Exists

type lterm =
  | Lletin of ident * ltyp * lterm
  | Lquantifer of quantifier * ident * ltyp * lterm
  | Limply of lterm * lterm
  | Lapp of lterm * lterm list
  | Llambda of ident * ltyp * lterm
  | Lconst of ident
  | Lrel of ident
  | Lliteral of bval


type transaction = {
    name         : ident;
    args         : arg list;
    calledby     : rexpr;
    condition    : cond;
    transferred  : cond;
    transition   : ident * ident * ident; (* state * state from * state to *)
    action       : pterm;
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
    init         : pterm;
    preds        : lterm list
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
