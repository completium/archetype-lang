
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

type typ_ =
  | Tbuiltin of vtyp
  | Tasset of ident
  | Tcontainer of typ_ * container
  | Tapp of typ_ * typ_
  | Tnuplet of typ_ list

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
| Pvar of ident
| Pfield of ident * ident

type prog =
  | Pletin of ident * typ_ * calc
  | PIf of cond * prog * prog option
  | PFor of ident * collection * prog
  | PAssign of assignment_operator * reference * calc
  | PTransfer
  | PTransition
  | PSeq of prog list
and cond =
  | Plogical of logical_operator * cond * cond
  | Pnot of cond
  | Pcomparaison of comparison_operator * cond * cond
  | Pcalc of calc
and calc =
  | Parithmetic of arithmetic_operator
  | Pref of reference
  | Pliteral of bval

type quantifier =
  | Forall
  | Exists

type term =
  | Tletin of ident * typ_ * term
  | Tquantifer of quantifier * ident * typ_ * term
  | Timply of term * term
  | Tapp of term * term list
  | Tlambda of ident * typ_ * term
  | Tvar of ident
  | Tliteral of bval


type transaction = {
    name         : ident;
    args         : arg list;
    calledby     : rexpr;
    condition    : cond;
    transferred  : calc;
    transition   : ident * ident * ident; (* state * state from * state to *)
    action       : prog;
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
    init         : prog;
    preds        : term list
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
    preds        : term list;
}
