open Location
open Ident

type lident = ident loced

let pp_ident fmt i = Format.fprintf fmt "%s" i
let pp_lident fmt i = Format.fprintf fmt "%s" (unloc i)

type container =
  | Collection
  | Aggregate
  | Partition
  | View
[@@deriving show {with_path = false}]

type currency =
  | Tz
  | Mtz
  | Utz
[@@deriving show {with_path = false}]

type vtyp =
  | VTunit
  | VTbool
  | VTnat
  | VTint
  | VTrational
  | VTdate
  | VTduration
  | VTstring
  | VTaddress
  | VTrole
  | VTcurrency
  | VTkey
  | VTkeyhash
  | VTsignature
  | VTbytes
  | VTchainid
[@@deriving show {with_path = false}]

type trtyp =
  | TRentry
  | TRaction (* add; remove; update *)
  | TRasset
  | TRfield
[@@deriving show {with_path = false}]

type ptyp =
  | Tnamed of int
  | Tasset of lident
  | Trecord of lident
  | Tenum of lident
  | Tbuiltin of vtyp
  | Tcontainer of ptyp * container
  | Tset of ptyp
  | Tlist of ptyp
  | Tmap of ptyp * ptyp
  | Ttuple of ptyp list
  | Toption of ptyp
  | Toperation
  | Tcontract of ptyp
  | Ttrace of trtyp
[@@deriving show {with_path = false}]

type type_ = ptyp (* type of pterm *)
[@@deriving show {with_path = false}]

(* operators and constants *)
type logical_operator =
  | And
  | Or
  | Xor
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
  | DivRat
  | DivEuc
  | Modulo
[@@deriving show {with_path = false}]

type unary_arithmetic_operator =
  | Uminus
[@@deriving show {with_path = false}]

type operator = [
  | `Logical of logical_operator
  | `Cmp     of comparison_operator
  | `Arith   of arithmetic_operator
  | `Unary   of unary_arithmetic_operator
  | `Assign  of assignment_operator
]
[@@deriving show {with_path = false}]

type const =
  (* constant *)
  | Cstate
  | Cnow
  | Ctransferred
  | Ccaller
  | Cfail
  | Cbalance
  | Csource
  | Cselfaddress
  | Cconditions
  | Centries
  | Cnone
  | Cany
  | Canyentry
  | Cresult
  | Cchainid
  | Coperations
  | Cmetadata
  (* function *)
  | Cadd
  | Caddupdate
  | Cceil
  | Cclear
  | Cconcat
  | Ccontains
  | Ccount
  | Cfloor
  | Cget
  | Cgetopt
  | Cisnone
  | Cissome
  | Clength
  | Cmax
  | Cmin
  | Cnth
  | Cpack
  | Cremove
  | Cremoveall
  | Cremoveif
  | Cselect
  | Cslice
  | Csort
  | Csum
  | Cunpack
  | Cupdate
  | Cmkoperation
  | Ctostring
  (* set *)
  | Csadd
  | Csremove
  | Cscontains
  | Cslength
  (* list *)
  | Chead
  | Ctail
  | Cabs
  | Cprepend
  | Cheadtail
  | Creverse
  (* map *)
  | Cmput
  | Cmremove
  | Cmget
  | Cmgetopt
  | Cmcontains
  | Cmlength
  (* crypto *)
  | Cblake2b
  | Csha256
  | Csha512
  | Cchecksignature
  | Chashkey
  (* vset *)
  | Cbefore
  | Citerated
  | Ctoiterate
  (* formula *)
  | Cempty
  | Cisempty
  | Csingleton
  | Csubsetof
  | Cunion
  | Cinter
  | Cdiff
[@@deriving show {with_path = false}]

type ('node) struct_poly = {
  node : 'node;                   (* kind of object *)
  type_ : ptyp option;            (* type of object *)
  label : ident option;           (* label (typically for instruction) *)
  loc : Location.t [@opaque];     (* location of object *)
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type 'id qualid_gen = ('id qualid_node) struct_poly
[@@deriving show {with_path = false}]

and 'id qualid_node =
  | Qident of 'id
  | Qdot of 'id qualid_gen * 'id
[@@deriving show {with_path = false}]

type qualid = lident qualid_gen
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type 'id sexpr_gen = ('id sexpr_node) struct_poly
[@@deriving show {with_path = false}]

and 'id sexpr_node =
  | Sref of 'id
  | Sor of 'id sexpr_gen * 'id sexpr_gen
  | Sany
[@@deriving show {with_path = false}]

type sexpr = lident sexpr_gen
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

(* basic variable *)
type bval_gen = bval_node struct_poly
[@@deriving show {with_path = false}]

and bval_node =
  | BVint          of Core.big_int
  | BVnat          of Core.big_int
  | BVbool         of bool
  | BVenum         of string
  | BVrational     of Core.big_int * Core.big_int
  | BVdate         of Core.date
  | BVstring       of string
  | BVcurrency     of currency * Core.big_int
  | BVaddress      of string
  | BVduration     of Core.duration
  | BVbytes        of string
  | BVunit
[@@deriving show {with_path = false}]

type bval = bval_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type quantifier =
  | Forall
  | Exists
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type 'id pattern_gen = ('id pattern_node) struct_poly
[@@deriving show {with_path = false}]

and 'id pattern_node =
  | Mwild
  | Mconst of 'id
[@@deriving show {with_path = false}]

type pattern = lident pattern_gen
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type 'id call_kind =
  | Cid of 'id
  | Cconst of const
[@@deriving show {with_path = false}]

type var_temporality =
  | VTbefore
  | VTat of ident
  | VTnone
[@@deriving show {with_path = false}]

type vset =
  | Vadded
  | Vremoved
  | Vunmoved
  | Vnone
[@@deriving show {with_path = false}]

type 'id term_node  =
  | Pquantifer of quantifier * 'id * ('id term_gen option * type_) * 'id term_gen
  | Pif of ('id term_gen * 'id term_gen * 'id term_gen)
  | Pmatchwith of 'id term_gen * ('id pattern_gen * 'id term_gen) list
  | Pcall of ('id term_gen option * 'id call_kind * ('id term_arg) list)
  | Plogical of logical_operator * 'id term_gen * 'id term_gen
  | Pnot of 'id term_gen
  | Pmulticomp of 'id term_gen * (comparison_operator * 'id term_gen) list
  | Pcomp of comparison_operator * 'id term_gen * 'id term_gen
  | Parith of arithmetic_operator * 'id term_gen * 'id term_gen
  | Puarith of unary_arithmetic_operator * 'id term_gen
  | Precord of 'id term_gen list
  | Precupdate of 'id term_gen * ('id * 'id term_gen) list
  | Pletin of 'id * 'id term_gen * ptyp option * 'id term_gen * 'id term_gen option (* ident * init * type * body * otherwise *)
  | Pdeclvar of 'id * ptyp option * 'id term_gen
  | Pvar of var_temporality * vset * 'id
  | Parray of 'id term_gen list
  | Plit of bval
  | Pdot of 'id term_gen * 'id
  | Pconst of const
  | Ptuple of 'id term_gen list
  | Ptupleaccess of 'id term_gen * Core.big_int
  | Pnone
  | Psome of 'id term_gen
  | Pcast of ptyp * ptyp * 'id term_gen
  | Pself of 'id
  | Pentrypoint of ptyp * 'id * 'id term_gen
[@@deriving show {with_path = false}]

and 'id term_arg =
  | AExpr    of 'id term_gen
  | AFun     of 'id * ptyp * ('id * ptyp * 'id term_gen) list * 'id term_gen
  | AEffect  of ('id * operator * 'id term_gen) list
  | ASorting of bool * 'id
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

and 'id term_poly = ('id term_node) struct_poly
[@@deriving show {with_path = false}]

and 'id term_gen = 'id term_poly
[@@deriving show {with_path = false}]

type pterm = lident term_gen
[@@deriving show {with_path = false}]

type pterm_arg = lident term_arg
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type 'id instruction_poly = {
  node : 'id instruction_node;
  label: string option;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

and 'id transfer_t =
  | TTsimple   of 'id term_gen
  | TTcontract of 'id term_gen * 'id * type_ * 'id term_gen
  | TTentry    of 'id term_gen * 'id term_gen
  | TTself     of 'id * ('id * 'id term_gen) list

and 'id instruction_node =
  | Iif of ('id term_gen * 'id instruction_gen * 'id instruction_gen)               (* condition * then_ * else_ *)
  | Ifor of ('id for_ident * 'id term_gen * 'id instruction_gen)                    (* id * collection * body *)
  | Iiter of ('id * 'id term_gen* 'id term_gen * 'id instruction_gen)               (* id * bound_min * bound_max * body *)
  | Iwhile of ('id term_gen * 'id instruction_gen)                                  (* condition * body *)
  | Iletin of ('id * 'id term_gen * 'id instruction_gen)                            (* id * init * body *)
  | Ideclvar of 'id * 'id term_gen                                                  (* id * init *)
  | Iseq of 'id instruction_gen list                                                (* lhs ; rhs *)
  | Imatchwith of 'id term_gen * ('id pattern_gen * 'id instruction_gen) list       (* match term with ('pattern * 'id instruction_gen) list *)
  | Iassign of (assignment_operator * ptyp * 'id lvalue_gen * 'id term_gen)         (* $2 assignment_operator $3 *)
  | Irequire of (bool * 'id term_gen * 'id term_gen)                                               (* $1 ? require : failif *)
  | Itransfer of ('id term_gen * 'id transfer_t)
  | Icall of ('id term_gen option * 'id call_kind * ('id term_arg) list)
  | Ireturn of 'id term_gen
  | Ilabel of 'id
  | Ifail of 'id term_gen
[@@deriving show {with_path = false}]

and 'id for_ident = FIsimple of 'id | FIdouble of 'id * 'id

and 'id instruction_gen = 'id instruction_poly

and instruction = lident instruction_poly

and 'id lvalue_gen = [
  | `Var   of 'id
  | `Field of 'id * 'id term_gen * 'id
]

and lvalue = lident lvalue_gen

type 'id decl_gen = {
  name    : 'id;
  typ     : ptyp option;
  default : 'id term_gen option;
  shadow  : bool;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type 'id label_term = {
  label : 'id option;
  term : 'id term_gen;
  error: 'id term_gen option;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id variable = {
  decl         : 'id decl_gen; (* TODO *)
  constant     : bool;
  invs         : 'id label_term list;
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id predicate = {
  name : 'id;
  args : ('id * type_) list;
  body : 'id term_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id definition = {
  name : 'id;
  typ  : type_;
  var  : 'id;
  body : 'id term_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id fail = {
  label: 'id;
  arg: 'id;
  atype: type_;
  formula: 'id term_gen;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id invariant = {
  label: 'id;
  formulas: 'id term_gen list;
}
[@@deriving show {with_path = false}]

type 'id postcondition = {
  name: 'id;
  formula: 'id term_gen;
  invariants: 'id invariant list;
  uses: 'id list;
}
[@@deriving show {with_path = false}]

type 'id assert_ = {
  name: 'id;
  label: 'id;
  formula: 'id term_gen;
  invariants: 'id invariant list;
  uses: 'id list;
}
[@@deriving show {with_path = false}]

type 'id specification = {
  predicates  : 'id predicate list;
  definitions : 'id definition list;
  fails       : 'id fail list;
  lemmas      : 'id label_term list;
  theorems    : 'id label_term list;
  variables   : 'id variable list;
  invariants  : ('id * 'id label_term list) list;
  effect      : 'id instruction_gen option;
  specs       : 'id postcondition list;
  asserts     : 'id assert_ list;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type entry_description =
  | ADAny
  | ADOp  of string * lident
[@@deriving show {with_path = false}]

type security_role   = lident
[@@deriving show {with_path = false}]

type security_entry =
  | Sany
  | Sentry of lident list
[@@deriving show {with_path = false}]

type security_node =
  | SonlyByRole         of entry_description * security_role list
  | SonlyInEntry        of entry_description * security_entry
  | SonlyByRoleInEntry  of entry_description * security_role list * security_entry
  | SnotByRole          of entry_description * security_role list
  | SnotInEntry         of entry_description * security_entry
  | SnotByRoleInEntry   of entry_description * security_role list * security_entry
  | StransferredBy      of entry_description
  | StransferredTo      of entry_description
  | SnoStorageFail      of security_entry
[@@deriving show {with_path = false}]

type security_predicate = {
  s_node: security_node;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type security_item = {
  label       : lident;
  predicate   : security_predicate;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type security = {
  items       : security_item list;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type fun_kind =
  | FKfunction
  | FKgetter
[@@deriving show {with_path = false}]

type 'id function_struct = {
  name          : 'id;
  kind          : fun_kind;
  args          : ('id decl_gen) list;
  body          : 'id instruction_gen;
  specification : 'id specification option;
  return        : ptyp;
  loc           : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type function_ = lident function_struct
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type 'id rexpr_gen = ('id rexpr_node) struct_poly
[@@deriving show {with_path = false}]

and 'id rexpr_node =
  | Rany
  | Rexpr of 'id term_gen
  | Ror of 'id rexpr_gen * 'id rexpr_gen
[@@deriving show {with_path = false}]

type rexpr = lident rexpr_gen
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type 'id transition = {
  from : 'id sexpr_gen;
  on   : ('id * ptyp * 'id * ptyp) option; (* key ident * key type * asset name * asset state type *)
  trs  : ('id * 'id term_gen option * 'id instruction_gen option) list; (* to * condition * entry*)
}
[@@deriving show {with_path = false}]

type 'id transaction_struct = {
  name            : 'id;
  args            : ('id decl_gen) list;
  calledby        : 'id rexpr_gen option;
  accept_transfer : bool;
  require         : 'id label_term list option;
  failif          : 'id label_term list option;
  transition      : ('id transition) option;
  specification   : 'id specification option;
  functions       : 'id function_struct list;
  effect          : 'id instruction_gen option;
  loc             : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type transaction = lident transaction_struct
[@@deriving show {with_path = false}]

type 'id enum_item_struct = {
  name : 'id;
  initial : bool;
  invariants : 'id label_term list;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id  enum_kind =
  | EKenum of 'id
  | EKstate
[@@deriving show {with_path = false}]

type 'id enum_struct = {
  (* name : 'id; "_state" if it's coming from Dstates constructor *)
  kind: 'id enum_kind;
  items : ('id enum_item_struct) list;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type enum = lident enum_struct
[@@deriving show {with_path = false}]

type 'id asset_struct = {
  name    : 'id;
  fields  : 'id decl_gen list;
  keys    : 'id list;   (* TODO: option ? *)
  sort    : 'id list;
  big_map : bool;
  state   : 'id option;
  init    : 'id term_gen list list;
  specs   : 'id label_term list;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type asset = lident asset_struct

type 'id record_struct = {
  name    : 'id;
  fields  : 'id decl_gen list;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type record = lident record_struct

type 'id decl_ =
  | Dvariable of 'id variable
  | Dasset    of 'id asset_struct
  | Drecord   of 'id record_struct
  | Denum     of 'id enum_struct
[@@deriving show {with_path = false}]

type 'id fun_ =
  | Ffunction    of 'id function_struct
  | Ftransaction of 'id transaction_struct
[@@deriving show {with_path = false}]

type 'id ast_struct = {
  name           : 'id;
  decls          : 'id decl_ list;
  funs           : 'id fun_ list;
  specifications : 'id specification list;
  securities     : security list;
  loc            : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

and ast = lident ast_struct

(* vtyp -> ptyp *)
let vtunit       = Tbuiltin (VTunit      )
let vtbool       = Tbuiltin (VTbool      )
let vtnat        = Tbuiltin (VTnat       )
let vtint        = Tbuiltin (VTint       )
let vtrational   = Tbuiltin (VTrational  )
let vtdate       = Tbuiltin (VTdate      )
let vtduration   = Tbuiltin (VTduration  )
let vtstring     = Tbuiltin (VTstring    )
let vtaddress    = Tbuiltin (VTaddress   )
let vtrole       = Tbuiltin (VTrole      )
let vtcurrency   = Tbuiltin (VTcurrency  )
let vtsignature  = Tbuiltin (VTsignature )
let vtkey        = Tbuiltin (VTkey       )
let vtkeyhash    = Tbuiltin (VTkeyhash   )
let vtbytes      = Tbuiltin (VTbytes     )
let vtchainid    = Tbuiltin (VTchainid   )

(* mk functions *)


let mk_sp ?label ?(loc = Location.dummy) ?type_ node =
  { node; type_; label; loc; }

let mk_instr ?label ?(loc = Location.dummy) node =
  { node; label; loc }

let mk_label_term ?label ?error ?(loc = Location.dummy) term =
  { label; term; error; loc }

let mk_variable ?(constant = false) ?(invs = []) ?(loc = Location.dummy) decl =
  { decl; constant; invs; loc }

let mk_predicate ?(args = []) ?(loc = Location.dummy) name body =
  { name; args; body; loc }

let mk_definition ?(loc = Location.dummy) name typ var body =
  { name; typ; var; body; loc }

let mk_fail ?(loc = Location.dummy) label arg atype formula =
  { label; arg; atype; formula; loc }

let mk_invariant ?(formulas = []) label =
  { label; formulas }

let mk_postcondition ?(invariants = []) ?(uses = []) name formula =
  { name; formula; invariants; uses }

let mk_assert ?(invariants = []) ?(uses = []) name label formula =
  { name; label; formula; invariants; uses }

let mk_specification ?(predicates = []) ?(definitions = []) ?(fails = []) ?(lemmas = []) ?(theorems = []) ?(variables = []) ?(invariants = []) ?effect ?(specs = []) ?(asserts = []) ?(loc = Location.dummy) () =
  { predicates; definitions; fails; lemmas; theorems; variables; invariants; effect; specs; asserts; loc}

let mk_function_struct ?(args = []) ?specification ?(loc = Location.dummy) name kind body return =
  { name; kind; args; body; specification; return; loc }

let mk_transition ?on ?(trs = []) from =
  { from; on; trs }

let mk_transaction_struct ?(args = []) ?calledby ?(accept_transfer = false) ?require ?failif ?transition ?specification ?(functions = []) ?effect ?(loc = Location.dummy) name =
  { name; args; calledby; accept_transfer; require; failif; transition; specification; functions; effect; loc }

let mk_enum_item ?(initial = false) ?(invariants = []) ?(loc = Location.dummy) name : 'id enum_item_struct =
  { name; initial; invariants; loc }

let mk_enum ?(items = []) ?(loc = Location.dummy) kind =
  { kind; items; loc }

let mk_decl ?typ ?default ?(shadow=false) ?(loc = Location.dummy) name =
  { name; typ; default; shadow; loc }

let mk_asset ?(fields = []) ?(keys = []) ?(sort = []) ?(big_map = false) ?state ?(init = []) ?(specs = []) ?(loc = Location.dummy) name   =
  { name; fields; keys; sort; big_map; state; init; specs; loc }

let mk_model ?(decls = []) ?(funs = []) ?(specifications = []) ?(securities = []) ?(loc = Location.dummy) name =
  { name; decls; funs; specifications; securities; loc }

let mk_id type_ id : qualid =
  { type_ = Some type_;
    loc   = loc id;
    node  = Qident id;
    label = None; }

module Utils : sig

  val get_asset                 : ast -> lident -> asset
  val get_asset_field           : ast -> (lident * lident ) -> lident decl_gen
  val get_asset_key             : ast -> lident -> (lident * vtyp)
  val get_container_asset_field : ast -> (lident * lident ) -> container
  val get_named_field_list      : ast -> lident -> pterm list -> (lident * pterm) list
  val get_field_list            : ast -> lident -> lident list
  val get_enum_values           : ast -> lident -> lident option
  val is_variable               : ast -> lident -> bool
  val is_asset                  : ast -> lident -> bool
  val is_enum_value             : ast -> lident -> bool
  val is_definition             : ast -> lident -> bool
  val get_var_type              : ast -> lident -> type_
  val get_enum_name             : lident enum_struct -> lident

end = struct
  open Tools

  exception Anomaly of string

  type error_desc =
    | AssetNotFound of string
    | AssetFieldNotFound of string * string
    | AssetKeyTypeNotFound of string
    | ContainerNotFound of string * string
    | VariableNotFound
  [@@deriving show {with_path = false}]

  let emit_error (desc : error_desc) =
    let str = Format.asprintf "%a@." pp_error_desc desc in
    raise (Anomaly str)

  let get_variables ast = List.fold_right (fun (x : 'id decl_) accu -> match x with Dvariable x ->  x::accu | _ -> accu ) ast.decls []
  let get_assets ast    = List.fold_right (fun (x : 'id decl_) accu -> match x with Dasset x    ->  x::accu | _ -> accu ) ast.decls []
  let get_enums ast     = List.fold_right (fun (x : 'id decl_) accu -> match x with Denum x     ->  x::accu | _ -> accu ) ast.decls []

  let get_definitions (ast : ast) =
    let for_spec s = s.definitions in
    let for_spec_accu accu s =
      accu @ for_spec s
    in
    let for_spec_accu_opt accu s =
      Option.map_dfl (fun x -> for_spec_accu accu x) accu s
    in
    []
    |> (fun acc -> List.fold_left (fun accu (fu : 'id fun_) ->
        let s =
          match fu with
          | Ffunction fs -> fs.specification
          | Ftransaction ts -> ts.specification
        in
        for_spec_accu_opt accu s) acc ast.funs)
    |> fun acc -> List.fold_left (fun accu s -> for_spec_accu accu s) acc ast.specifications

  let get_asset_opt ast asset_name : asset option =
    let id = unloc asset_name in
    List.fold_left (fun accu (x : asset) -> if String.equal id (unloc x.name) then Some x else accu ) None (get_assets ast)

  let get_asset ast asset_name : asset =
    let res = get_asset_opt ast asset_name in
    match res with
    | Some v -> v
    | _ -> emit_error (AssetNotFound (unloc asset_name))

  let get_asset_field ast (asset_name, field_name) : 'id decl_gen =
    let asset = get_asset ast asset_name in
    let res = List.fold_left (fun accu (x : 'id decl_gen) -> if String.equal (unloc field_name) (unloc x.name) then Some x else accu) None asset.fields in
    match res with
    | Some v -> v
    | _ -> emit_error (AssetFieldNotFound (unloc asset_name, unloc field_name))

  let get_asset_key ast asset_name : (lident * vtyp) =
    let asset = get_asset ast asset_name in
    let key_id = match asset.keys with [k] -> k | _ -> assert false in (* TODO *)
    let key_field = get_asset_field ast (asset_name, key_id) in
    match key_field.typ with
    | Some (Tbuiltin v) -> (key_id, v)
    | _ -> emit_error (AssetKeyTypeNotFound (unloc asset_name))

  let get_container_asset_field ast (asset_name, field_name) =
    let field = get_asset_field ast (asset_name, field_name) in
    match field.typ with
    | Some Tcontainer (_, c) -> c
    | _ -> emit_error (ContainerNotFound (unloc asset_name, unloc field_name))

  let get_field_list ast asset_name =
    let asset = get_asset ast asset_name in
    List.map (fun (x : lident decl_gen) -> x.name) asset.fields

  let get_named_field_list ast asset_name list =
    let field_list = get_field_list ast asset_name in
    (* List.iter (fun x -> Format.eprintf "f1: %s@." (unloc x)) field_list;
       List.iter (fun x -> Format.eprintf "f2: %a@." pp_pterm x) list;
       Format.eprintf "lf1: %d@." (List.length field_list);
       Format.eprintf "lf2: %d@." (List.length list); *)
    List.map2 (fun x y -> x, y) field_list list

  let get_enum_name (e : 'id enum_struct) =
    match e.kind with
    | EKenum id -> id
    | EKstate -> dumloc "state"

  let get_enum_opt ast ident =
    List.fold_left (fun accu (x : 'id enum_struct) ->
        if (Location.unloc (get_enum_name x)) = (Location.unloc ident)
        then Some x
        else accu
      ) None (get_enums ast)

  let get_asset_opt ast ident =
    List.fold_left (fun accu (x : 'id asset_struct) ->
        if (Location.unloc x.name) = (Location.unloc ident)
        then Some x
        else accu
      ) None (get_assets ast)

  let get_enum_values ast ident =
    List.fold_left (
      fun accu (x : 'id enum_struct) ->
        if List.fold_left (fun accu (x : 'id enum_item_struct) -> accu || (Location.unloc x.name) = (Location.unloc ident)) false x.items
        then (Some (get_enum_name x))
        else accu
    ) None (get_enums ast)

  let get_definition ast ident =
    List.fold_left (fun accu (x : 'id definition) ->
        if (Location.unloc x.name) = (Location.unloc ident)
        then Some x
        else accu
      ) None (get_definitions ast)

  let get_variable_opt ast ident : 'id variable option =
    List.fold_left (
      fun accu (x : 'id variable) ->
        if (String.equal (Location.unloc x.decl.name) (Location.unloc ident))
        then Some x
        else accu
    ) None (get_variables ast)

  let is_variable ast ident : bool =
    match get_variable_opt ast ident with
    | Some _ -> true
    | None   -> false

  let is_asset ast ident =
    match get_asset_opt ast ident with
    | Some _ -> true
    | None   -> false

  let is_definition ast ident =
    match get_definition ast ident with
    | Some _ -> true
    | None   -> false

  let is_enum_value ast ident =
    match get_enum_values ast ident with
    | Some _ -> true
    | None   -> false

  let get_var_type (ast : ast) (ident : lident) : type_ =
    let var : type_ option =
      List.fold_left (
        fun accu (x : 'id variable) ->
          if (String.equal (Location.unloc x.decl.name) (Location.unloc ident))
          then x.decl.typ
          else accu
      ) None (get_variables ast) in
    match var with
    | Some v -> v
    | None -> emit_error VariableNotFound

end
