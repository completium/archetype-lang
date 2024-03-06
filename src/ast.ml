open Location
open Ident

type lident = ident loced
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)
type namespace = lident
[@@deriving show {with_path = false}]

type longident = namespace * lident
[@@deriving show {with_path = false}]

let unloc_longident ((nm, id) : longident) =
  (unloc nm, unloc id)

let cmp_longident ((nm1, id1) : longident) ((nm2, id2) : longident) =
  String.equal (unloc nm1) (unloc nm2) && String.equal (unloc id1) (unloc id2)

let pp_ident fmt i = Format.fprintf fmt "%s" i
let pp_lident fmt i = Format.fprintf fmt "%s" (unloc i)

type container =
  | Collection
  | Aggregate
  | Partition
  | AssetContainer
  | AssetKey
  | AssetValue
  | AssetView
[@@deriving show {with_path = false}]

type currency =
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
  | VTcurrency
  | VTkey
  | VTkeyhash
  | VTsignature
  | VTbytes
  | VTchainid
  | VTbls12_381_fr
  | VTbls12_381_g1
  | VTbls12_381_g2
  | VTnever
  | VTchest
  | VTchest_key
[@@deriving show {with_path = false}]

type ptyp =
  | Tnamed of int
  | Tasset of longident
  | Trecord of longident
  | Tevent of longident
  | Tenum of longident
  | Tbuiltin of vtyp
  | Tcontainer of type_ * container
  | Tset of type_
  | Tlist of type_
  | Tmap of type_ * type_
  | Tbig_map of type_ * type_
  | Titerable_big_map of type_ * type_
  | Tor of type_ * type_
  | Tlambda of type_ * type_
  | Ttuple of type_ list
  | Toption of type_
  | Toperation
  | Tcontract of type_
  | Tticket of type_
  | Tsapling_state       of int
  | Tsapling_transaction of int
[@@deriving show {with_path = false}]

and type_ = ptyp (* * lident option *) (* type of pterm *)
[@@deriving show {with_path = false}]

(* operators and constants *)
type logical_operator =
  | And
  | Or
  | Xor
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
  | DivMod
  | ThreeWayCmp
  | ShiftLeft
  | ShiftRight
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
  | Cselfchainid
  | Coperations
  | Cmetadata
  | Clevel
  (* function *)
  | Cadd
  | Cput
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
  | Cinttonat
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
  | Cupdateall
  | Cmakeoperation
  | Cmakeevent
  | Cmakesandboxexecoperation
  | Cnattostring
  | Cbytestonat
  | Cnattobytes
  | Cbytestoint
  | Cinttobytes
  | Cexec
  | Capply
  | Cinttodate
  | CmutezToNat
  | Csetdelegate
  | Ckeyhashtocontract
  | Csubnat
  | Csubmutez
  | Cgreedyand
  | Cgreedyor
  | CmakeAsset
  | CtoContainer
  | CputRemove
  | CgetEntrypoint
  | CrequireEntrypoint
  | CcallView
  | CimportCallView
  | CselfCallView
  | Csimplifyrational
  | Cgetnumerator
  | Cgetdenominator
  | Cglobalconstant
  | Cexphorner
  (* set *)
  | Csadd
  | Csremove
  | Csupdate
  | Cscontains
  | Cslength
  (* list *)
  | Chead
  | Ctail
  | Cabs
  | Cprepend
  | Creverse
  (* map *)
  | Cmput
  | Cmremove
  | Cmupdate
  | Cmget
  | Cmgetopt
  | Cmcontains
  | Cmlength
  (* crypto *)
  | Cblake2b
  | Csha256
  | Csha512
  | Csha3
  | Ckeccak
  | Cchecksignature
  | Ckeytokeyhash
  | Ccontracttoaddress
  | Caddresstocontract
  | Ckeytoaddress
  | Cisimplicitaddress
  (* voting *)
  | Ctotalvotingpower
  | Cvotingpower
  (* blockchain *)
  | Cminblocktime
  (* ticket *)
  | Ccreateticket
  | Creadticket
  | Csplitticket
  | Cjointickets
  (* sapling *)
  | Csapling_empty_state
  | Csapling_verify_update
  (* bls *)
  | Cpairing_check
  (* timelock *)
  | Copen_chest
  (* event *)
  | Cemit
  (* instrs *)
  | Csandbox_exec
[@@deriving show {with_path = false}]

type ('node) struct_poly = {
  node : 'node;                   (* kind of object *)
  type_ : type_ option;           (* type of object *)
  label : ident option;           (* label (typically for instruction) *)
  loc : Location.t [@opaque];     (* location of object *)
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type qualid = qualid_node struct_poly
[@@deriving show {with_path = false}]

and qualid_node =
  | Qident of lident
  | Qdot of qualid * lident
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type sexpr = sexpr_node struct_poly
[@@deriving show {with_path = false}]

and sexpr_node =
  | Sref of lident
  | Sor of sexpr * sexpr
  | Sany
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

(* basic variable *)
type bval = bval_node struct_poly

and bval_node =
  | BVint          of Core.big_int
  | BVnat          of Core.big_int
  | BVbool         of bool
  | BVrational     of Core.big_int * Core.big_int
  | BVdate         of Core.date
  | BVstring       of string
  | BVcurrency     of currency * Core.big_int
  | BVaddress      of string
  | BVduration     of Core.duration
  | BVbytes        of string
  | BVunit
  | BVbls12_381_num_fr of Core.big_int
  | BVbls12_381_byt_fr of string
  | BVbls12_381_g1 of string
  | BVbls12_381_g2 of string
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type pattern = pattern_node struct_poly
[@@deriving show {with_path = false}]

and pattern_node =
  | Mwild
  | Mconst of lident * lident list
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type call_kind =
  | Cid of longident
  | Cconst of const
[@@deriving show {with_path = false}]

type michelson_struct = {
  ms_content: Michelson.obj_micheline
}
[@@deriving show {with_path = false}]

type pterm = pterm_node struct_poly

and pterm_node  =
  | Pif of (pterm * pterm * pterm)
  | Pmatchwith of pterm * (pattern * pterm) list
  | Pmatchoption of pterm * lident list * pterm * pterm
  | Pmatchor     of pterm * lident list * pterm * lident list * pterm
  | Pmatchlist   of pterm * lident * lident * pterm * pterm
  | Pfold        of pterm * lident * pterm
  | Pmap         of pterm * lident * pterm
  | Pcall of (pterm option * call_kind * type_ list * pterm_arg list)
  | Plogical of logical_operator * pterm * pterm
  | Pnot of pterm
  | Pmulticomp of pterm * (comparison_operator * pterm) list
  | Pcomp of comparison_operator * pterm * pterm
  | Parith of arithmetic_operator * pterm * pterm
  | Puarith of unary_arithmetic_operator * pterm
  | Precord of pterm list
  | Precupdate of pterm * (lident * pterm) list
  | Pletin of lident * pterm * type_ option * pterm * pterm option (* ident * init * type * body * otherwise *)
  | Pdeclvar of lident * type_ option * pterm * bool
  | Pvar of longident
  | Parray of pterm list
  | Plit of bval
  | Pdot of pterm * lident
  | Pquestiondot of pterm * lident
  | Pconst of const
  | Ptuple of pterm list
  | Ptupleaccess of pterm * Core.big_int
  | Pnone
  | Psome of pterm
  | Pleft of type_ * pterm
  | Pright of type_ * pterm
  | Plambda of type_ * lident * type_ * pterm
  | Plambda_michelson of type_ * type_ * Michelson.obj_micheline
  | Pcast of type_ * type_ * pterm
  | Pself of lident
  | Pternary of pterm * pterm * pterm
  | Pcreatecontract of pterm * pterm * create_contract_type
  | Ptz_expr of string
  | Pmicheline_expr of type_ * Michelson.obj_micheline * pterm list
  | Pfailexpr of pterm
[@@deriving show {with_path = false}]

and pterm_arg =
  | AExpr    of pterm
  | AFun     of lident * type_ * (lident * type_ * pterm) list * pterm
  | AEffect  of (lident * operator * pterm) list
  | ASorting of bool * lident
  | AIdent   of lident
[@@deriving show {with_path = false}]

and create_contract_type =
  | CCTz  of michelson_struct * pterm
  | CCArl of ident * (ident * pterm) list
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

(* -------------------------------------------------------------------- *)

type instruction = {
  node : instruction_node;
  label: string option;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

and transfer_t =
  | TTsimple    of pterm * pterm
  | TTcontract  of pterm * pterm * lident * type_ * pterm
  | TTentry     of pterm * pterm * pterm
  | TTgen       of pterm * ident * ident * type_ * pterm * pterm
  | TTself      of pterm * lident * (lident * pterm) list
  | TToperation of pterm

and detach_kind =
  | DK_option of type_ * ident
  | DK_map of type_ * ident * pterm

and var_decl_kind =
  | VDKbasic
  | VDKoption of pterm option

and instruction_node =
  | Iif of (pterm * instruction * instruction)                               (* condition * then_ * else_ *)
  | Ifor of (for_ident * pterm * instruction)                                (* id * collection * body *)
  | Iiter of (lident * pterm * pterm * instruction)                          (* id * bound_min * bound_max * body *)
  | Iwhile of (pterm * instruction)                                          (* condition * body *)
  | Iletin of (lident * pterm * instruction)                                 (* id * init * body *)
  | Ideclvar of (lident * type_) list * pterm * var_decl_kind *  bool        (* (id * type_) list * init * var_decl_kind * constant *)
  | Iseq of instruction list                                                 (* lhs ; rhs *)
  | Imatchwith   of pterm * (pattern * instruction) list                     (* match term with ('pattern * instruction) list *)
  | Imatchoption of pterm * lident list * instruction * instruction
  | Imatchor     of pterm * lident list * instruction * lident list * instruction
  | Imatchlist   of pterm * lident * lident * instruction * instruction
  | Imatchdetach of detach_kind * lident * instruction * instruction
  | Iassign of (assignment_operator * type_ * lvalue * pterm * pterm option) (* $2 assignment_operator $3 [ : $4]*)
  | Irequire of (bool * pterm * pterm)                                       (* $1 ? require : failif *)
  | Itransfer of transfer_t
  | Iemit of longident * pterm
  | Icall of (pterm option * call_kind * pterm_arg list)
  | Ireturn of pterm
  | Ifail of pterm
  | Ifailsome of pterm
  | Idetach of lident * detach_kind * type_ * pterm
  | Imicheline of Michelson.obj_micheline
[@@deriving show {with_path = false}]

and for_ident = FIsimple of lident | FIdouble of lident * lident

and lvalue = [
  | `Var   of lident
  | `Field of longident * pterm * lident
  | `Asset of longident * pterm * lident
  | `Tuple of pterm * int * int
]

type 'a decl_gen = {
  name    : 'a;
  typ     : type_ option;
  default : pterm option;
  shadow  : bool;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type label_term = {
  label : lident option;
  term : pterm;
  error: pterm option;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type variable_kind =
  | VKconstant
  | VKvariable
[@@deriving show {with_path = false}]

type variable = {
  decl : longident decl_gen; (* TODO *)
  kind : variable_kind;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type parameter = {
  name    : lident;
  typ     : type_;
  default : pterm option;
  value   : pterm option;
  const   : bool;
  loc     : Location.t [@opaque];
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

type view_visibility =
  | VVonchain
  | VVonoffchain
  | VVoffchain
[@@deriving show {with_path = false}]

type fun_kind =
  | FKfunction
  | FKview of view_visibility
[@@deriving show {with_path = false}]


type returned_fun_type =
  | Typed of ptyp
  | Void
[@@deriving show {with_path = false}]

type function_ = {
  name        : longident;
  kind        : fun_kind;
  args        : lident decl_gen list;
  body        : instruction;
  return      : returned_fun_type;
  side_effect : bool;
  storage_usage : bool;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]


(* -------------------------------------------------------------------- *)

type rexpr = rexpr_node struct_poly
[@@deriving show {with_path = false}]

and rexpr_node =
  | Rany
  | Rasset of longident
  | Rexpr of pterm
  | Ror of rexpr * rexpr
[@@deriving show {with_path = false}]

(* -------------------------------------------------------------------- *)

type transition = {
  from : sexpr;
  trs  : (lident * pterm option * instruction option) list; (* to * condition * entry*)
}
[@@deriving show {with_path = false}]

type tr_kind =
  | Entry
  | Getter of ptyp
[@@deriving show {with_path = false}]

type transaction = {
  kind            : tr_kind;
  name            : lident;
  args            : lident decl_gen list;
  sourcedby       : (rexpr * pterm option) loced option;
  calledby        : (rexpr * pterm option) loced option;
  state_is        : (lident * pterm option) loced option;
  accept_transfer : bool * pterm option;
  constants       : label_term list option;
  require         : label_term list option;
  failif          : label_term list option;
  transition      : transition option;
  functions       : function_ list;
  effect          : instruction option;
  loc             : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type enum_item_struct = {
  name : lident;
  initial : bool;
  invariants : label_term list;
  args: ptyp list;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type enum_kind =
  | EKenum of longident
  | EKstate of namespace
[@@deriving show {with_path = false}]

type enum = {
  (* name : 'id; "_state" if it's coming from Dstates constructor *)
  kind: enum_kind;
  items : enum_item_struct list;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type map_kind =
  | MKMap
  | MKBigMap
  | MKIterableBigMap
[@@deriving show {with_path = false}]

type init_asset =
  | IAliteral of pterm list list
  | IAident of lident
[@@deriving show {with_path = false}]

type asset = {
  name     : longident;
  fields   : lident decl_gen list;
  keys     : lident list;   (* TODO: option ? *)
  sort     : lident list;
  map_kind : map_kind;
  init     : init_asset;
  specs    : label_term list;
  loc      : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type position =
  | Pleaf of lident
  | Pnode of position list
[@@deriving show {with_path = false}]

type record = {
  name    : longident;
  fields  : lident decl_gen list;
  pos     : position;
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type decl_ =
  | Dvariable of variable
  | Dasset    of asset
  | Drecord   of record
  | Denum     of enum
  | Devent    of record
[@@deriving show {with_path = false}]

type fun_ =
  | Ffunction    of function_
  | Ftransaction of transaction
[@@deriving show {with_path = false}]

type metadata_kind =
  | MKuri  of string loced
  | MKjson of string loced
[@@deriving show {with_path = false}]

type import_kind_node =
  | INMichelson of michelson_struct
  | INArchetype
[@@deriving show {with_path = false}]

type ast = {
  name           : lident;
  parameters     : parameter list;
  metadata       : metadata_kind option;
  decls          : decl_ list;
  funs           : fun_ list;
  loc            : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

(* vtyp -> type_ *)
let vtaddress      = Tbuiltin (VTaddress      )
let vtbls12_381_fr = Tbuiltin (VTbls12_381_fr )
let vtbls12_381_g1 = Tbuiltin (VTbls12_381_g1 )
let vtbls12_381_g2 = Tbuiltin (VTbls12_381_g2 )
let vtbool         = Tbuiltin (VTbool         )
let vtbytes        = Tbuiltin (VTbytes        )
let vtchainid      = Tbuiltin (VTchainid      )
let vtcurrency     = Tbuiltin (VTcurrency     )
let vtdate         = Tbuiltin (VTdate         )
let vtduration     = Tbuiltin (VTduration     )
let vtint          = Tbuiltin (VTint          )
let vtkey          = Tbuiltin (VTkey          )
let vtkeyhash      = Tbuiltin (VTkeyhash      )
let vtnat          = Tbuiltin (VTnat          )
let vtrational     = Tbuiltin (VTrational     )
let vtsignature    = Tbuiltin (VTsignature    )
let vtstring       = Tbuiltin (VTstring       )
let vtunit         = Tbuiltin (VTunit         )
let vtnever        = Tbuiltin (VTnever        )
let vtchest        = Tbuiltin (VTchest        )
let vtchest_key    = Tbuiltin (VTchest_key    )

let vts = [
  vtaddress      ;
  vtbls12_381_fr ;
  vtbls12_381_g1 ;
  vtbls12_381_g2 ;
  vtbool         ;
  vtbytes        ;
  vtchainid      ;
  vtcurrency     ;
  vtdate         ;
  vtduration     ;
  vtint          ;
  vtkey          ;
  vtkeyhash      ;
  vtnat          ;
  vtnever        ;
  vtrational     ;
  vtsignature    ;
  vtstring       ;
  vtunit         ;
]
(* mk functions *)

let mk_sp ?label ?(loc = Location.dummy) ?type_ node =
  { node; type_; label; loc; }

let mk_instr ?label ?(loc = Location.dummy) node =
  { node; label; loc }

let mk_label_term ?label ?error ?(loc = Location.dummy) term =
  { label; term; error; loc }

let mk_variable ?(loc = Location.dummy) decl kind =
  { decl; kind; loc }

let mk_function_struct ?(args = []) ?(loc = Location.dummy) name kind body return side_effect storage_usage =
  { name; kind; args; body; return; side_effect; storage_usage; loc }

let mk_transition ?(trs = []) from =
  { from; trs }

let mk_transaction_struct ?(args = []) ?sourcedby ?calledby ?state_is ?(accept_transfer = (false, None)) ?constants ?require ?failif ?transition ?(functions = []) ?effect ?(loc = Location.dummy) kind name =
  { kind; name; args; sourcedby; calledby; state_is; accept_transfer; constants; require; failif; transition; functions; effect; loc }

let mk_enum_item ?(initial = false) ?(args = []) ?(invariants = []) ?(loc = Location.dummy) name : enum_item_struct =
  { name; initial; args; invariants; loc }

let mk_enum ?(items = []) ?(loc = Location.dummy) kind =
  { kind; items; loc }

let mk_decl ?typ ?default ?(shadow=false) ?(loc = Location.dummy) name =
  { name; typ; default; shadow; loc }

let mk_asset ?(fields = []) ?(keys = []) ?(sort = []) ?(map_kind = MKMap) ?(init = (IAliteral [])) ?(specs = []) ?(loc = Location.dummy) name   =
  { name; fields; keys; sort; map_kind; init; specs; loc }

let mk_model ?(parameters = []) ?metadata ?(decls = []) ?(funs = []) ?(loc = Location.dummy) name =
  { name; parameters; metadata; decls; funs; loc }

let mk_id type_ id : qualid =
  { type_ = Some type_;
    loc   = loc id;
    node  = Qident id;
    label = None; }

let map_ptyp ft t =
  match t with
  | Tnamed n                   -> Tnamed n
  | Tasset i                   -> Tasset i
  | Trecord u                  -> Trecord u
  | Tevent i                   -> Tevent i
  | Tenum i                    -> Tenum i
  | Tbuiltin vtyp              -> Tbuiltin vtyp
  | Tcontainer (t, container)  -> Tcontainer (ft t, container)
  | Tset t                     -> Tset (ft t)
  | Tlist t                    -> Tlist (ft t)
  | Tmap (kt, vt)              -> Tmap (ft kt, ft vt)
  | Tbig_map (kt, vt)          -> Tbig_map (ft kt, ft vt)
  | Titerable_big_map (kt, vt) -> Titerable_big_map (ft kt, ft vt)
  | Tor (lt, rt)               -> Tor (ft lt, ft rt)
  | Tlambda (it, rt)           -> Tlambda (ft it, ft rt)
  | Ttuple lt                  -> Ttuple (List.map ft lt)
  | Toption t                  -> Toption (ft t)
  | Toperation                 -> Toperation
  | Tcontract t                -> Tcontract (ft t)
  | Tticket t                  -> Tticket (ft t)
  | Tsapling_state n           -> Tsapling_state n
  | Tsapling_transaction n     -> Tsapling_transaction n


let vunit = mk_sp ~type_:vtunit BVunit

module Utils : sig

  val get_asset                 : ast -> lident -> asset
  val get_asset_field           : ast -> (lident * lident ) -> lident decl_gen
  val get_asset_key             : ast -> lident -> (lident * vtyp)
  val get_container_asset_field : ast -> (lident * lident ) -> container
  val get_named_field_list      : ast -> lident -> pterm list -> (lident * pterm) list
  val get_field_list            : ast -> lident -> lident list
  val is_variable               : ast -> longident -> bool
  val is_asset                  : ast -> lident -> bool
  val is_parameter              : ast -> lident -> bool
  val get_var_type              : ast -> longident -> type_
  val is_literal                : pterm -> bool

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

  let get_variables ast = List.fold_right (fun (x : decl_) accu -> match x with Dvariable x ->  x::accu | _ -> accu ) ast.decls []
  let get_assets ast    = List.fold_right (fun (x : decl_) accu -> match x with Dasset x    ->  x::accu | _ -> accu ) ast.decls []
  let get_enums ast     = List.fold_right (fun (x : decl_) accu -> match x with Denum x     ->  x::accu | _ -> accu ) ast.decls []

  let get_asset_opt ast asset_name : asset option =
    let id = unloc asset_name in
    List.fold_left (fun accu (x : asset) -> if String.equal id (snd (unloc_longident x.name)) then Some x else accu ) None (get_assets ast)

  let get_asset ast asset_name : asset =
    let res = get_asset_opt ast asset_name in
    match res with
    | Some v -> v
    | _ -> emit_error (AssetNotFound (unloc asset_name))

  let get_asset_field ast (asset_name, field_name) : lident decl_gen =
    let asset = get_asset ast asset_name in
    let res = List.fold_left (fun accu (x : lident decl_gen) -> if String.equal (unloc field_name) (unloc x.name) then Some x else accu) None asset.fields in
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

  let get_asset_opt ast ident =
    List.fold_left (fun accu (x : asset) ->
        if (Location.unloc (snd x.name)) = (Location.unloc ident)
        then Some x
        else accu
      ) None (get_assets ast)

  let get_variable_opt ast (ident : longident) : variable option =
    List.fold_left (
      fun accu (x : variable) ->
        if cmp_longident x.decl.name ident
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

  let is_parameter ast ident =
    List.exists (fun (x : parameter) -> String.equal (unloc ident) (unloc x.name)) ast.parameters

  let get_var_type (ast : ast) (ident : longident) : type_ =
    let var : type_ option =
      List.fold_left (
        fun accu (x : variable) ->
          if cmp_longident x.decl.name ident
          then x.decl.typ
          else accu
      ) None (get_variables ast) in
    match var with
    | Some v -> v
    | None -> emit_error VariableNotFound

  let rec is_literal (p : pterm) : bool =
    match p.node with
    | Pif _ -> false
    | Pmatchwith _ -> false
    | Pmatchoption _ -> false
    | Pmatchor _ -> false
    | Pmatchlist _ -> false
    | Pfold _ -> false
    | Pmap _ -> false
    | Pcall _ -> false
    | Plogical _ -> false
    | Pnot p -> is_literal p
    | Pmulticomp _ -> false
    | Pcomp _ -> false
    | Parith _ -> false
    | Puarith _ -> false
    | Precord ps -> List.for_all is_literal ps
    | Precupdate _ -> false
    | Pletin _ -> false
    | Pdeclvar _ -> false
    | Pvar _ -> false
    | Parray ps -> List.for_all is_literal ps
    | Plit _ -> true
    | Pdot _ -> false
    | Pquestiondot _ -> false
    | Pconst _ -> false
    | Ptuple ps -> List.for_all is_literal ps
    | Ptupleaccess _ -> false
    | Pnone -> true
    | Psome p -> is_literal p
    | Pleft (_, p) -> is_literal p
    | Pright (_, p) -> is_literal p
    | Plambda _ -> false
    | Plambda_michelson _ -> true
    | Pcast _ -> false
    | Pself _ -> false
    | Pternary _ -> false
    | Pcreatecontract _ -> false
    | Ptz_expr _ -> true
    | Pfailexpr _ -> false
    | Pmicheline_expr _ -> false

end
