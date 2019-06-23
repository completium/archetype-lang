open Ident

type lident = ident Location.loced
[@@deriving show {with_path = false}]

type currency =
  | Tez
  | Mutez
[@@deriving show {with_path = false}]

type container =
  | Collection
  | Partition
[@@deriving show {with_path = false}]

type btyp =
  | Bbool
  | Bint
  | Buint
  | Brational
  | Bdate
  | Bduration
  | Bstring
  | Baddress
  | Brole
  | Bcurrency of currency
  | Bkey
[@@deriving show {with_path = false}]

type vset =
  | VSremoved
  | VSadded
  | VSstable
  | VSbefore
  | VSafter
  | VSfixed
[@@deriving show {with_path = false}]

type trtyp =
  | TRentry
  | TRaction (* add; remove; update *)
  | TRasset
  | TRfield
[@@deriving show {with_path = false}]

type type_ =
  | Tasset of lident
  | Tenum of lident
  | Tcontract of lident
  | Tbuiltin of btyp
  | Tcontainer of type_ * container
  | Ttuple of type_ list
  | Tprog of type_
  | Tvset of vset * type_
  | Ttrace of trtyp
[@@deriving show {with_path = false}]

type quantifier =
  | Forall
  | Exists
[@@deriving show {with_path = false}]

type const =
  (* constant *)
  | Cstate
  | Cnow
  | Ctransferred
  | Ccaller
  | Cfail
  | Cbalance
  | Cconditions
  | Cactions
  | Cnone
  | Cany
  | Canyaction
  (* function *)
  | Cget
  | Cadd
  | Caddnofail
  | Cremove
  | Cremovenofail
  | Cremoveif
  | Cupdate
  | Cupdatenofail (* if key exists -> update *)
  | Cclear
  | Ccontains
  | Cnth
  | Creverse
  | Cselect
  | Csort
  | Ccount
  | Csum
  | Cmax
  | Cmin
  (* vset *)
  | Cbefore
  | Cunmoved
  | Cadded
  | Cremoved
  | Citerated
  | Ctoiterate
  (* predicates *)
  | Cmaybeperformedonlybyrole
  | Cmaybeperformedonlybyaction
  | Cmaybeperformedbyrole
  | Cmaybeperformedbyaction
[@@deriving show {with_path = false}]

type 'id storage_const_gen =
  | Get                of 'id
  | AddAsset           of 'id
  | RemoveAsset        of 'id
  | ClearAsset         of 'id
  | UpdateAsset        of 'id
  | ContainsAsset      of 'id
  | NthAsset           of 'id
  | SelectAsset        of 'id
  | SortAsset          of 'id
  | ReverseAsset       of 'id
  | CountAsset         of 'id
  | SumAsset           of 'id
  | MinAsset           of 'id
  | MaxAsset           of 'id
  | AddContainer       of 'id * 'id
  | RemoveContainer    of 'id * 'id
  | ClearContainer     of 'id * 'id
  | ContainsContainer  of 'id * 'id
  | NthContainer       of 'id * 'id
  | SelectContainer    of 'id * 'id
  | SortContainer      of 'id * 'id
  | ReverseContainer   of 'id * 'id
  | CountContainer     of 'id * 'id
  | SumContainer       of 'id * 'id
  | MinContainer       of 'id * 'id
  | MaxContainer       of 'id * 'id
[@@deriving show {with_path = false}]

type storage_const = lident storage_const_gen
[@@deriving show {with_path = false}]

type 'id call_kind =
  | Cid of 'id
  | Cconst of const
  | Cstorage of 'id storage_const_gen
[@@deriving show {with_path = false}]

type 'id pattern_node =
  | Pwild
  | Pconst of 'id
[@@deriving show {with_path = false}]

type 'id pattern_gen = {
  node: 'id pattern_node;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type pattern = lident pattern_gen
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
  | Div
  | Modulo
[@@deriving show {with_path = false}]

type unary_arithmetic_operator =
  | Uplus
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

type lit_value =
  | BVint          of Core.big_int
  | BVuint         of Core.big_int
  | BVbool         of bool
  | BVenum         of string
  | BVrational     of Core.big_int * Core.big_int
  | BVdate         of string (* TODO : find a date structure *)
  | BVstring       of string
  | BVcurrency     of currency * Core.big_int
  | BVaddress      of string
  | BVduration     of string
[@@deriving show {with_path = false}]

type ('id, 'term) mterm_node  =
  | Mquantifer of quantifier * 'id * type_ * 'term
  | Mif of ('term * 'term * 'term)
  | Mmatchwith of 'term * ('id pattern_gen * 'term) list
  | Mcall of ('id option * 'id call_kind * ('id term_arg_gen) list)
  | Mlogical of logical_operator * 'term * 'term
  | Mnot of 'term
  | Mcomp of comparison_operator * 'term * 'term
  | Marith of arithmetic_operator * 'term * 'term
  | Muarith of unary_arithmetic_operator * 'term
  | Mrecord of 'term list
  | Mletin of 'id * 'term * type_ option * 'term
  | Mvar of 'id
  | Marray of 'term list
  | Mlit of lit_value
  | Mdot of 'term * 'id
  | Mconst of const
  | Mtuple of 'term list
[@@deriving show {with_path = false}]

and 'id mterm_gen = {
  node: ('id, 'id mterm_gen) mterm_node;
  type_: type_;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

and mterm = lident mterm_gen
[@@deriving show {with_path = false}]

and 'id term_arg_gen =
  | AExpr   of 'id mterm_gen
  | AEffect of ('id * operator * 'id mterm_gen) list
[@@deriving show {with_path = false}]

and term_arg = lident term_arg_gen
[@@deriving show {with_path = false}]

type 'id label_term_gen = {
  label : 'id option;
  term : 'id mterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type label_term = lident label_term_gen
[@@deriving show {with_path = false}]

type 'id item_field_type =
  | FBasic            of btyp
  | FAssetKeys        of btyp * 'id
  | FAssetRecord      of btyp * 'id
  | FRecordCollection of 'id
  | FRecord           of 'id
  | FEnum             of 'id
  | FContainer        of container * 'id item_field_type
[@@deriving show {with_path = false}]

type 'id item_field_gen = {
  asset   : 'id option;
  name    : 'id;
  typ     : 'id item_field_type;
  ghost   : bool;
  default : 'id mterm_gen option; (* initial value *)
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type item_field = lident item_field_gen
[@@deriving show {with_path = false}]

type 'id storage_item_gen = {
  name        : 'id;
  fields      : 'id item_field_gen list;
  invariants  : lident label_term_gen list;
  init        : ((ident * mterm) list) list;
}
[@@deriving show {with_path = false}]

type storage_item = lident storage_item_gen

type 'id storage_gen = 'id storage_item_gen list
[@@deriving show {with_path = false}]

type storage = lident storage_gen
[@@deriving show {with_path = false}]

type 'id enum_item = {
  name: 'id;
  invariants : 'id label_term_gen list;
}
[@@deriving show {with_path = false}]

type 'id enum = {
  name: 'id;
  values: 'id enum_item list;
}
[@@deriving show {with_path = false}]

type 'id record_item_gen = {
  name: 'id;
  type_: type_;
  default: 'id mterm_gen option;
}
[@@deriving show {with_path = false}]

type record_item = lident record_item_gen
[@@deriving show {with_path = false}]

type 'id record_gen = {
  name: 'id;
  key: 'id option;
  values: 'id record_item_gen list;
}
[@@deriving show {with_path = false}]

type record = lident record_gen
[@@deriving show {with_path = false}]

type 'id contract_signature_gen = {
  name : 'id;
  args: type_ list;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type contract_signature = lident contract_signature_gen
[@@deriving show {with_path = false}]

type 'id contract_gen = {
  name       : 'id;
  signatures : 'id contract_signature_gen list;
  init       : 'id mterm_gen option;
  loc        : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type contract = lident contract_gen
[@@deriving show {with_path = false}]

type 'id function_ = {
  name: 'id;
}
[@@deriving show {with_path = false}]

type 'id entry = {
  name: 'id;
}
[@@deriving show {with_path = false}]

type 'id argument_gen = 'id * type_ * 'id mterm_gen option
[@@deriving show {with_path = false}]

type argument = lident argument_gen
[@@deriving show {with_path = false}]

type ('id, 'qualid) qualid_node =
  | Qident of 'id
  | Qdot of 'qualid * 'id
[@@deriving show {with_path = false}]

type 'id qualid_gen = {
  node: ('id, 'id qualid_gen) qualid_node;
  type_: type_;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type qualid = lident qualid_gen
[@@deriving show {with_path = false}]

type ('id, 'instr) instruction_node =
  | Iif of ('id mterm_gen * 'instr * 'instr)                              (* condition * then_ * else_ *)
  | Ifor of ('id * 'id mterm_gen * 'instr)                                (* id * collection * body *)
  | Iletin of ('id * 'id mterm_gen * 'instr)                              (* id * init * body *)
  | Iseq of 'instr list                                                   (* lhs ; rhs*)
  | Imatchwith of 'id mterm_gen * ('id pattern_gen * 'instr) list         (* match 'term with ('pattern * 'instr) list *)
  | Iassign of (assignment_operator * 'id * 'id mterm_gen)                (* $2 assignment_operator $3 *)
  | Irequire of (bool * 'id mterm_gen)                                    (* $1 ? require : failif *)
  | Itransfer of ('id mterm_gen * bool * 'id qualid_gen option)           (* value * back * dest *)
  | Ibreak
  | Iassert of 'id mterm_gen
  | Icall of ('id mterm_gen option * 'id call_kind * ('id term_arg_gen) list)
[@@deriving show {with_path = false}]

and 'id instruction_gen = {
  node:    ('id, 'id instruction_gen) instruction_node;
  subvars: ident list;
  loc:     Location.t [@opaque];
}
[@@deriving show {with_path = false}]

and instruction = lident instruction_gen
[@@deriving show {with_path = false}]

type 'id function_struct_gen = {
  name: 'id;
  args: 'id argument_gen list;
  body: 'id instruction_gen;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type function_struct = lident function_struct_gen
[@@deriving show {with_path = false}]

type 'id function_node_gen =
  | Function           of 'id function_struct_gen * type_ (* fun * return type *)
  | Entry              of 'id function_struct_gen
  | Storage            of storage_const
  | Other
[@@deriving show {with_path = false}]

type function_node = lident function_node_gen
[@@deriving show {with_path = false}]

type 'id signature_gen = {
  name: 'id;
  args: 'id argument_gen list;
  ret: type_ option;
}
[@@deriving show {with_path = false}]

type signature = lident signature_gen
[@@deriving show {with_path = false}]

type 'id variable_gen = {
  decl         : 'id argument_gen;
  constant     : bool;
  from         : 'id qualid_gen option;
  to_          : 'id qualid_gen option;
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type variable = lident variable_gen
[@@deriving show {with_path = false}]

type 'id predicate_gen = {
  name : 'id;
  args : ('id * ('id mterm_gen)) list;
  body : 'id mterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type predicate = lident predicate_gen
[@@deriving show {with_path = false}]

type 'id definition_gen = {
  name : 'id;
  typ  : type_;
  var  : 'id;
  body : 'id mterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type definition = lident definition_gen
[@@deriving show {with_path = false}]

type 'id invariant_gen = {
  label: 'id;
  formulas: 'id mterm_gen list;
}
[@@deriving show {with_path = false}]

type invariant = lident invariant_gen
[@@deriving show {with_path = false}]


type 'id specification_gen = {
  name: 'id;
  formula: 'id mterm_gen;
  invariants: ('id invariant_gen) list;
}
[@@deriving show {with_path = false}]

type specification = lident specification_gen
[@@deriving show {with_path = false}]


type 'id assert_gen = {
  name: 'id;
  label: 'id;
  formula: 'id mterm_gen;
  invariants: 'id invariant_gen list;
}
[@@deriving show {with_path = false}]

type assert_ = lident assert_gen
[@@deriving show {with_path = false}]

type 'id verification_gen = {
  predicates  : 'id predicate_gen list;
  definitions : 'id definition_gen list;
  axioms      : 'id label_term_gen list;
  theorems    : 'id label_term_gen list;
  variables   : 'id variable_gen list;
  invariants  : ('id * 'id label_term_gen list) list;
  effect      : 'id mterm_gen option;
  specs       : 'id specification_gen list;
  asserts     : 'id assert_gen list;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type verification = lident verification_gen
[@@deriving show {with_path = false}]

type 'id function__gen = {
  node: 'id function_node_gen;
  verif  : 'id verification_gen option;
}
[@@deriving show {with_path = false}]

type function__ = lident function__gen
[@@deriving show {with_path = false}]

type 'id decl_node_gen =
  | TNenum of 'id enum
  | TNrecord of 'id record_gen
  | TNcontract of 'id contract_gen
  | TNstorage of 'id storage_gen
  | TNfunction of 'id function__gen
  | TNverification of 'id verification_gen
[@@deriving show {with_path = false}]

type decl_node = lident decl_node_gen
[@@deriving show {with_path = false}]

type 'id model_gen = {
  name: lident;
  decls: 'id decl_node_gen list;
}
[@@deriving show {with_path = false}]

type model = lident model_gen
[@@deriving show {with_path = false}]

let lident_to_string lident = Location.unloc lident

let function_name_from_function_node = function
  | Function          (fs, _)    -> lident_to_string fs.name
  | Entry              fs        -> lident_to_string fs.name
  | Storage (Get                aid      ) -> "get_"      ^ lident_to_string aid
  | Storage (AddAsset           aid      ) -> "add_"      ^ lident_to_string aid
  | Storage (RemoveAsset        aid      ) -> "remove_"   ^ lident_to_string aid
  | Storage (ClearAsset         aid      ) -> "clear_"    ^ lident_to_string aid
  | Storage (UpdateAsset        aid      ) -> "update_"   ^ lident_to_string aid
  | Storage (ContainsAsset      aid      ) -> "contains_" ^ lident_to_string aid
  | Storage (NthAsset           aid      ) -> "nth_"      ^ lident_to_string aid
  | Storage (SelectAsset        aid      ) -> "select_"   ^ lident_to_string aid
  | Storage (SortAsset          aid      ) -> "sort_"     ^ lident_to_string aid
  | Storage (ReverseAsset       aid      ) -> "reverse_"  ^ lident_to_string aid
  | Storage (CountAsset         aid      ) -> "count_"    ^ lident_to_string aid
  | Storage (SumAsset           aid      ) -> "sum_"      ^ lident_to_string aid
  | Storage (MinAsset           aid      ) -> "min_"      ^ lident_to_string aid
  | Storage (MaxAsset           aid      ) -> "max_"      ^ lident_to_string aid
  | Storage (AddContainer      (aid, fid)) -> "add_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | Storage (RemoveContainer   (aid, fid)) -> "remove_"   ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | Storage (ClearContainer    (aid, fid)) -> "clear_"    ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | Storage (ContainsContainer (aid, fid)) -> "contains_" ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | Storage (NthContainer      (aid, fid)) -> "nth_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | Storage (SelectContainer   (aid, fid)) -> "select_"   ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | Storage (SortContainer     (aid, fid)) -> "sort_"     ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | Storage (ReverseContainer  (aid, fid)) -> "reverse_"  ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | Storage (CountContainer    (aid, fid)) -> "count_"    ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | Storage (SumContainer      (aid, fid)) -> "sum_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | Storage (MinContainer      (aid, fid)) -> "min_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | Storage (MaxContainer      (aid, fid)) -> "max_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | Other -> assert false

let mk_qualid ?(loc = Location.dummy) node type_ : 'id qualid_gen =
  { node; type_; loc}

let mk_pattern ?(loc = Location.dummy) node : 'id pattern_gen =
  { node; loc}

let mk_mterm ?(loc = Location.dummy) node type_ : 'id mterm_gen =
  { node; type_; loc}

let mk_label_term ?label ?(loc = Location.dummy) term : 'id label_term_gen =
  { label; term; loc }

let mk_instruction ?(loc = Location.dummy) ?(subvars=[]) node : 'id instruction_gen =
  { node; subvars; loc}

let mk_variable ?(constant = false) ?from ?to_ ?(loc = Location.dummy) decl =
  { decl; constant; from; to_; loc }

let mk_predicate ?(args = []) ?(loc = Location.dummy) name body =
  { name; args; body; loc }

let mk_definition ?(loc = Location.dummy) name typ var body =
  { name; typ; var; body; loc }

let mk_invariant ?(formulas = []) label =
  { label; formulas }

let mk_specification ?(invariants = []) name formula =
  { name; formula; invariants }

let mk_assert ?(invariants = []) name label formula =
  { name; label; formula; invariants }

let mk_verification ?(predicates = []) ?(definitions = []) ?(axioms = []) ?(theorems = []) ?(variables = []) ?(invariants = []) ?effect ?(specs = []) ?(asserts = []) ?(loc = Location.dummy) () =
  { predicates; definitions; axioms; theorems; variables; invariants; effect; specs; asserts; loc}

let mk_contract_signature ?(args=[]) ?(loc=Location.dummy) name : 'id contract_signature_gen =
  { name; args; loc }

let mk_contract ?(signatures=[]) ?init ?(loc=Location.dummy) name : 'id contract_gen =
  { name; signatures; init; loc }

let mk_enum ?(values = []) name : 'id enum =
  { name; values }

let mk_enum_item ?(invariants = []) name : 'id enum_item =
  { name; invariants }

let mk_record ?(values = []) ?key name : 'id record_gen =
  { name; key; values }

let mk_record_item ?default name type_ : 'id record_item_gen =
  { name; type_; default }

let mk_storage_item ?(fields = []) ?(invariants = []) ?(init = []) name : 'id storage_item_gen =
  { name; fields; invariants; init }

let mk_item_field ?asset ?(ghost = false) ?default ?(loc = Location.dummy) name typ : 'id item_field_gen =
  { asset; name; typ; ghost; default; loc }

let mk_function_struct ?(args = []) ?(loc = Location.dummy) name body : function_struct =
  { name; args; body; loc }

let mk_function ?verif node : 'id function__gen =
  { node; verif }

let mk_signature ?(args = []) ?ret name : 'id signature_gen =
  { name; args; ret}

let mk_model ?(decls = []) name : model =
  { name; decls}


(* -------------------------------------------------------------------- *)

let map_term_node (f : 'id mterm_gen -> 'id mterm_gen) = function
  | Mquantifer (q, i, t, e) -> Mquantifer (q, i, t, f e)
  | Mif (c, t, e)           -> Mif (f c, f t, f e)
  | Mmatchwith (e, l)       -> Mmatchwith (e, List.map (fun (p, e) -> (p, f e)) l)
  | Mcall (i, e, args)      ->
    Mcall (i, e, List.map (fun (arg : 'id term_arg_gen) -> match arg with
        | AExpr e   -> AExpr (f e)
        | AEffect l -> AEffect (List.map (fun (id, op, e) -> (id, op, f e)) l)) args)
  | Mlogical (op, l, r)     -> Mlogical (op, f l, f r)
  | Mnot e                  -> Mnot (f e)
  | Mcomp (c, l, r)         -> Mcomp (c, f l, f r)
  | Marith (op, l, r)       -> Marith (op, f l, f r)
  | Muarith (op, e)         -> Muarith (op, f e)
  | Mrecord l               -> Mrecord (List.map f l)
  | Mletin (i, a, t, b)     -> Mletin (i, f a, t, f b)
  | Mvar v                  -> Mvar v
  | Marray l                -> Marray (List.map f l)
  | Mlit l                  -> Mlit l
  | Mdot (e, i)             -> Mdot (f e, i)
  | Mconst c                -> Mconst c
  | Mtuple l                -> Mtuple (List.map f l)

let map_instr_node f = function
  | Iif (c, t, e)       -> Iif (c, f t, f e)
  | Ifor (i, c, b)      -> Ifor (i, c, f b)
  | Iletin (i, c, b)    -> Iletin (i, c, f b)
  | Iseq is             -> Iseq (List.map f is)
  | Imatchwith (a, ps)  -> Imatchwith (a, ps)
  | Iassign (op, l, r)  -> Iassign (op, l, r)
  | Irequire (b, x)     -> Irequire (b, x)
  | Itransfer x         -> Itransfer x
  | Ibreak              -> Ibreak
  | Iassert x           -> Iassert x
  | Icall (x, id, args) -> Icall (x, id, args)

let map_gen_mterm g f (i : 'id mterm_gen) : 'id mterm_gen =
  {
    i with
    node = g f i.node
  }

let map_gen g f (i : 'id instruction_gen) : 'id instruction_gen =
  {
    i with
    node = g f i.node
  }

let map_term  f t = map_gen_mterm map_term_node  f t
let map_instr f i = map_gen map_instr_node f i

let fold_term (f : 'a -> 't -> 'a) (accu : 'a) (term : 'id mterm_gen) =
  match term.node with
  | Mquantifer (_, _, _, e) -> f accu e
  | Mif (c, t, e)           -> f (f (f accu c) t) e
  | Mmatchwith (e, l)       -> List.fold_left (fun accu (_, a) -> f accu a) (f accu e) l
  | Mcall (_, _, args)      -> List.fold_left (fun accu (arg : 'id term_arg_gen) -> match arg with
      | AExpr e -> f accu e
      | AEffect l -> List.fold_left (fun accu (_, _, e) -> f accu e) accu l ) accu args
  | Mlogical (_, l, r)      -> f (f accu l) r
  | Mnot e                  -> f accu e
  | Mcomp (_, l, r)         -> f (f accu l) r
  | Marith (_, l, r)        -> f (f accu l) r
  | Muarith (_, e)          -> f accu e
  | Mrecord l               -> List.fold_left f accu l
  | Mletin (_, a, _, b)     -> f (f accu a) b
  | Mvar _                  -> accu
  | Marray l                -> List.fold_left f accu l
  | Mlit _                  -> accu
  | Mdot (e, _)             -> f accu e
  | Mconst _                -> accu
  | Mtuple l                -> List.fold_left f accu l

let fold_instr f accu (instr : 'id instruction_gen) =
  match instr.node with
  | Iif (c, t, e)    -> f (f accu t) e
  | Ifor (i, c, b)   -> f accu b
  | Iletin (i, j, b) -> f accu b
  | Iseq is          -> List.fold_left f accu is
  | Imatchwith _     -> accu
  | Iassign _        -> accu
  | Irequire _       -> accu
  | Itransfer _      -> accu
  | Ibreak           -> accu
  | Iassert _        -> accu
  | Icall _          -> accu

let fold_instr_expr fi fe accu (instr : 'id instruction_gen) =
  match instr.node with
  | Iif (c, t, e)       -> fi (fi (fe accu c) t) e
  | Ifor (i, c, b)      -> fi (fe accu c) b
  | Iletin (i, j, b)    -> fi (fe accu j) b
  | Iseq is             -> List.fold_left fi accu is
  | Imatchwith (a, ps)  -> List.fold_left (fun accu (_, i) -> fi accu i) (fe accu a) ps
  | Iassign (_, _, e)   -> fe accu e
  | Irequire (_, x)     -> fe accu x
  | Itransfer (x, _, _) -> fe accu x
  | Ibreak              -> accu
  | Iassert x           -> fe accu x
  | Icall (x, id, args) -> fi accu instr

let fold_map_term g f (accu : 'a) (term : 'id mterm_gen) : 'term * 'a =
  match term.node with
  | Mquantifer (q, id, t, e) ->
    let ee, ea = f accu e in
    g (Mquantifer (q, id, t, ee)), ea

  | Mif (c, t, e) ->
    let ce, ca = f accu c in
    let ti, ta = f ca t in
    let ei, ea = f ta e in
    g (Mif (ce, ti, ei)), ea

  | Mmatchwith (e, l) ->
    let ee, ea = f accu e in
    let (pse, psa) =
      List.fold_left
        (fun (ps, accu) (p, i) ->
           let pa, accu = f accu i in
           [(p, i)] @ ps, accu) ([], ea) l
    in

    g (Mmatchwith (ee, l)), psa

  | Mcall (a, id, args) ->
    let ((argss, argsa) : 'c list * 'a) =
      List.fold_left
        (fun (pterms, accu) (x : 'id term_arg_gen) ->
           let p, accu =
             match x with
             | AExpr a -> f accu a |> fun (x, acc) -> (Some (AExpr x), acc)
             | _ -> None, accu in
           let x = match p with | Some a -> a | None -> x in
           pterms @ [x], accu) ([], accu) args
    in
    g (Mcall (a, id, argss)), argsa

  | Mlogical (op, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mlogical (op, le, re)), ra

  | Mnot e ->
    let ee, ea = f accu e in
    g (Mnot ee), ea

  | Mcomp (op, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Mcomp (op, le, re)), ra

  | Marith (op, l, r) ->
    let le, la = f accu l in
    let re, ra = f la r in
    g (Marith (op, le, re)), ra

  | Muarith (op, e) ->
    let ee, ea = f accu e in
    g (Muarith (op, ee)), ea

  | Mrecord l ->
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) l in
    g (Mrecord lp), la

  | Mletin (id, i, t, o) ->
    let ie, ia = f accu i in
    let oe, oa = f ia o in
    g (Mletin (id, i, t, oe)), oa

  | Mvar id ->
    g (Mvar id), accu

  | Marray l ->
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) l in
    g (Marray lp), la

  | Mlit l ->
    g (Mlit l), accu

  | Mdot (e, id) ->
    let ee, ea = f accu e in
    g (Mdot (ee, id)), ea

  | Mconst c ->
    g (Mconst c), accu

  | Mtuple l ->
    let (lp, la) = List.fold_left
        (fun (pterms, accu) x ->
           let p, accu = f accu x in
           pterms @ [p], accu) ([], accu) l in
    g (Mtuple lp), la


let fold_map_instr_term gi ge fi fe (accu : 'a) (instr : 'id instruction_gen) : 'instr * 'a =
  match instr.node with
  | Iif (c, t, e) ->
    let ce, ca = fe accu c in
    let ti, ta = fi ca t in
    let ei, ea = fi ta e in
    gi (Iif (ce, ti, ei)), ea

  | Ifor (i, c, b) ->
    let ce, ca = fe accu c in
    let bi, ba = fi ca b in
    gi (Ifor (i, ce, bi)), ba

  | Iletin (i, j, b) ->
    let je, ja = fe accu j in
    let bi, ba = fi ja b in
    gi (Iletin (i, je, bi)), ba

  | Iseq is ->
    let (isi, isa) : ('instr list * 'a) =
      List.fold_left
        (fun ((instrs, accu) : ('b list * 'c)) x ->
           let bi, accu = fi accu x in
           instrs @ [bi], accu) ([], accu) is
    in
    gi (Iseq isi), isa

  | Imatchwith (a, ps) ->
    let ae, aa = fe accu a in

    let (pse, psa) =
      List.fold_left
        (fun (ps, accu) (p, i) ->
           let pa, accu = fi accu i in
           [(p, i)] @ ps, accu) ([], aa) ps
    in

    gi (Imatchwith (ae, ps)), psa

  | Iassign (op, id, x) ->
    let xe, xa = fe accu x in
    gi (Iassign (op, id, xe)), xa

  | Irequire (b, x) ->
    let xe, xa = fe accu x in
    gi (Irequire (b, xe)), xa

  | Itransfer (x, b, q) ->
    let xe, xa = fe accu x in
    gi (Itransfer (xe, b, q)), xa

  | Ibreak ->
    gi (Ibreak), accu

  | Iassert x ->
    let xe, xa = fe accu x in
    gi (Iassert xe), xa

  | Icall (x, id, args) ->
    let xe, xa =
      match x with
      | Some x -> fe accu x |> (fun (a, b) -> (Some a, b))
      | None -> None, accu in

    let (argss, argsa) =
      List.fold_left
        (fun (pterms, accu) arg ->
           match arg with
           | AExpr x ->
             let p, accu = fe accu x in
             pterms @ [AExpr p], accu
           | _ ->
             pterms, accu
        ) ([], xa) args
    in
    gi (Icall (xe, id, argss)), argsa


(* -------------------------------------------------------------------- *)
module Utils : sig

  val get_records                : model -> record list
  val get_storage                : model -> storage
  val get_record                 : model -> lident -> record
  val get_record_field           : model -> (lident * lident) -> record_item
  val get_record_key             : model -> lident -> (lident * btyp)
  val is_storage_attribute       : model -> lident -> bool
  val get_named_field_list       : model -> lident -> 'a list -> (lident * 'a) list
  val get_partitions             : model -> (lident * record_item) list (* record id, record item *)
  val dest_partition             : type_ -> lident
  val get_storage_api            : model -> storage_const list
  val get_partition_record_key   : model -> lident -> lident -> (lident * lident * btyp)
  val get_entries                : model -> (verification option * function_struct) list

end = struct

  open Tools
  open Location

  exception Anomaly of string

  type error_desc =
    | RecordNotFound of string
    | RecordFieldNotFound of string * string
    | RecordKeyTypeNotFound of string
    | StorageNotFound
    | NotaPartition
    | PartitionNotFound
  [@@deriving show {with_path = false}]

  let emit_error (desc : error_desc) =
    let str = Format.asprintf "%a@." pp_error_desc desc in
    raise (Anomaly str)

  let is_storage_const (d : decl_node) : bool =
    match d with
    | TNfunction { node = Storage _; verif = _ } -> true
    | _ -> false

  let dest_storage_const (d : decl_node) : storage_const =
    match d with
    | TNfunction { node = Storage sc; verif = _ } -> sc
    | _ -> assert false

  let get_storage_api m  =
    m.decls |> List.filter is_storage_const |> List.map dest_storage_const

  let is_record (d : decl_node) : bool =
    match d with
    | TNrecord _ -> true
    | _          -> false

  let is_entry (d : decl_node) : bool =
    match d with
    | TNfunction { node = Entry _; verif = _ } -> true
    | _                                        -> false

  let get_entry (d : decl_node) : verification option * function_struct =
    match d with
    | TNfunction { node = Entry s; verif = v } -> (v,s)
    | _                                        -> assert false

  let get_entries m = List.filter is_entry m.decls |> List.map get_entry

  let dest_record  = function
    | TNrecord r -> r
    | _ -> emit_error NotaPartition

  let get_records m = m.decls |> List.filter is_record |> List.map dest_record

  let get_record model record_name : record =
    let id = unloc record_name in
    let res = List.fold_left (fun accu (x : decl_node) ->
        match x with
        | TNrecord r when String.equal (unloc record_name) (unloc r.name) -> Some r
        | _ -> accu
      ) None model.decls in
    match res with
    | Some v -> v
    | _ -> emit_error (RecordNotFound id)

  let get_partitions m : (lident * record_item) list=
    get_records m |> List.fold_left (fun acc (record : record) ->
        acc @ (List.fold_left (fun acc (ritem : record_item) ->
            match ritem.type_ with
            | Tcontainer (Tasset _, Partition) ->
              acc @ [record.name,ritem]
            | _ -> acc
          ) [] record.values)
      ) []

  let dest_partition = function
    | Tcontainer (Tasset p,Partition) -> p
    | _ -> assert false

  let get_record_field model (record_name, field_name) =
    let record = get_record model record_name in
    let res = List.fold_left (fun accu (x : record_item) ->
        if String.equal (unloc field_name) (unloc x.name) then
          Some x
        else accu) None record.values in
    match res with
    | Some v -> v
    | _ -> emit_error (RecordFieldNotFound (unloc record_name, unloc field_name))

  let get_record_key model record_name : (lident * btyp) =
    let record = get_record model record_name in
    let key_id = Option.get record.key in
    let key_field = get_record_field model (record_name, key_id) in
    match key_field.type_ with
    | Tbuiltin v -> (key_id, v)
    | _ -> emit_error (RecordKeyTypeNotFound (unloc record_name))

  (* returns : asset name, key name, key type *)
  let get_partition_record_key model record field : (lident * lident * btyp) =
    let partitions = get_partitions model in
    let rec rec_get = function
      | (r,(ri : record_item)) :: tl when compare r.pldesc record.pldesc = 0 &&
                                          compare ri.name.pldesc field.pldesc = 0 ->
        let pa  = dest_partition ri.type_ in
        let k,t = get_record_key model pa in
        (pa,k,t)
      | _ :: tl -> rec_get tl
      | _ -> emit_error (PartitionNotFound) in
    rec_get partitions

  let get_storage_opt model =
    List.fold_left (fun accu (x : decl_node) ->
        match x with
        | TNstorage s -> Some s
        | _ -> accu
      ) None model.decls

  let get_storage model =
    let res = get_storage_opt model in
    match res with
    | Some e -> e
    | _ -> emit_error StorageNotFound

  let is_storage_attribute model id =
    let s = get_storage_opt model in
    match s with
    | Some items ->
      (List.fold_left (fun accu (x : storage_item) ->
           accu || String.equal (Location.unloc id) (Location.unloc x.name)) false items)
    | None -> false

  let get_field_list model record_name =
    let record = get_record model record_name in
    List.map (fun (x : record_item) -> x.name) record.values

  let get_named_field_list ast asset_name list =
    let field_list = get_field_list ast asset_name in
    (* List.iter (fun x -> Format.eprintf "f1: %s@." (unloc x)) field_list;
       List.iter (fun x -> Format.eprintf "f2: %a@." pp_pterm x) list;
       Format.eprintf "lf1: %d@." (List.length field_list);
       Format.eprintf "lf2: %d@." (List.length list); *)
    List.map2 (fun x y -> x, y) field_list list
end
