
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

type vtyp =
  | VTint
  | VTuint
  | VTfloat
  | VTdate
  | VTcurrency    of currency * transfer option

(* basic value *)
type bval =
  | BVint          of int
  | BVuint         of int
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

type prog =
  | Ifthen
and cond = int
and calc = int

type pred = int

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
    preds        : pred list
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
    preds        : pred list;
}
