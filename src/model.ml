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

type 'id call_kind =
  | Cid of 'id
  | Cconst of const
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
  | Mcall of ('id option * 'id call_kind * ('id term_arg) list)
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

and 'id term_arg =
  | AExpr   of 'id mterm_gen
  | AEffect of ('id * operator * 'id mterm_gen) list
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

type 'id item_field = {
  asset   : 'id option;
  name    : 'id;
  typ     : 'id item_field_type;
  ghost   : bool;
  default : 'id mterm_gen option; (* initial value *)
  loc     : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id storage_item_gen = {
  name        : 'id;
  fields      : 'id item_field list;
  invariants  : lident label_term_gen list;
  init        : ((ident * mterm) list) list;
}
[@@deriving show {with_path = false}]

type storage_item = lident storage_item_gen

type 'id storage = 'id storage_item_gen list
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

type 'id record = {
  name: 'id;
  key: 'id option;
  values: 'id record_item_gen list;
}
[@@deriving show {with_path = false}]

type 'id contract_signature = {
  name : 'id;
  args: type_ list;
  loc: Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id contract_gen = {
  name       : 'id;
  signatures : 'id contract_signature list;
  init       : 'id mterm_gen option;
  loc        : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type contract = lident contract_gen

type 'id function_ = {
  name: 'id;
}
[@@deriving show {with_path = false}]

type 'id entry = {
  name: 'id;
}
[@@deriving show {with_path = false}]

type 'id argument = 'id * type_ * 'id mterm_gen option
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
  | Icall of ('id mterm_gen option * 'id call_kind * ('id term_arg) list)
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
  args: 'id argument list;
  body: 'id instruction_gen;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type function_struct = lident function_struct_gen
[@@deriving show {with_path = false}]

type 'id function_node =
  | Function           of 'id function_struct_gen * type_ (* fun * return type *)
  | Entry              of 'id function_struct_gen
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
  | Other
[@@deriving show {with_path = false}]

type 'id signature = {
  name: 'id;
  args: 'id argument list;
  ret: type_ option;
}
[@@deriving show {with_path = false}]

type 'id variable = {
  decl         : 'id argument;
  constant     : bool;
  from         : 'id qualid_gen option;
  to_          : 'id qualid_gen option;
  loc          : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id predicate = {
  name : 'id;
  args : ('id * ('id mterm_gen)) list;
  body : 'id mterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id definition = {
  name : 'id;
  typ  : type_;
  var  : 'id;
  body : 'id mterm_gen;
  loc  : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id invariant = {
  label: 'id;
  formulas: 'id mterm_gen list;
}
[@@deriving show {with_path = false}]

type 'id specification = {
  name: 'id;
  formula: 'id mterm_gen;
  invariants: ('id invariant) list;
}
[@@deriving show {with_path = false}]

type 'id assert_ = {
  name: 'id;
  label: 'id;
  formula: 'id mterm_gen;
  invariants: 'id invariant list;
}
[@@deriving show {with_path = false}]

type 'id verification = {
  predicates  : 'id predicate list;
  definitions : 'id definition list;
  axioms      : 'id label_term_gen list;
  theorems    : 'id label_term_gen list;
  variables   : 'id variable list;
  invariants  : ('id * 'id label_term_gen list) list;
  effect      : 'id mterm_gen option;
  specs       : 'id specification list;
  asserts     : 'id assert_ list;
  loc         : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type 'id function__ = {
  node: 'id function_node;
  verif  : 'id verification option;
}
[@@deriving show {with_path = false}]

type 'id decl_node =
  | TNenum of 'id enum
  | TNrecord of 'id record
  | TNcontract of 'id contract_gen
  | TNstorage of 'id storage
  | TNfunction of 'id function__
[@@deriving show {with_path = false}]

type 'id model_gen = {
  name: lident;
  decls: 'id decl_node list;
}
[@@deriving show {with_path = false}]

type model = lident model_gen
[@@deriving show {with_path = false}]

let lident_to_string lident = Location.unloc lident

let function_name_from_function_node = function
  | Function          (fs, _)    -> lident_to_string fs.name
  | Entry              fs        -> lident_to_string fs.name
  | Get                aid       -> "get_"      ^ lident_to_string aid
  | AddAsset           aid       -> "add_"      ^ lident_to_string aid
  | RemoveAsset        aid       -> "remove_"   ^ lident_to_string aid
  | ClearAsset         aid       -> "clear_"    ^ lident_to_string aid
  | UpdateAsset        aid       -> "update_"   ^ lident_to_string aid
  | ContainsAsset      aid       -> "contains_" ^ lident_to_string aid
  | NthAsset           aid       -> "nth_"      ^ lident_to_string aid
  | SelectAsset        aid       -> "select_"   ^ lident_to_string aid
  | SortAsset          aid       -> "sort_"     ^ lident_to_string aid
  | ReverseAsset       aid       -> "reverse_"  ^ lident_to_string aid
  | CountAsset         aid       -> "count_"    ^ lident_to_string aid
  | SumAsset           aid       -> "sum_"      ^ lident_to_string aid
  | MinAsset           aid       -> "min_"      ^ lident_to_string aid
  | MaxAsset           aid       -> "max_"      ^ lident_to_string aid
  | AddContainer      (aid, fid) -> "add_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | RemoveContainer   (aid, fid) -> "remove_"   ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | ClearContainer    (aid, fid) -> "clear_"    ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | ContainsContainer (aid, fid) -> "contains_" ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | NthContainer      (aid, fid) -> "nth_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | SelectContainer   (aid, fid) -> "select_"   ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | SortContainer     (aid, fid) -> "sort_"     ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | ReverseContainer  (aid, fid) -> "reverse_"  ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | CountContainer    (aid, fid) -> "count_"    ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | SumContainer      (aid, fid) -> "sum_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | MinContainer      (aid, fid) -> "min_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | MaxContainer      (aid, fid) -> "max_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
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

let mk_contract_signature ?(args=[]) ?(loc=Location.dummy) name : 'id contract_signature =
  { name; args; loc }

let mk_contract ?(signatures=[]) ?init ?(loc=Location.dummy) name : 'id contract_gen =
  { name; signatures; init; loc }

let mk_enum ?(values = []) name : 'id enum =
  { name; values }

let mk_enum_item ?(invariants = []) name : 'id enum_item =
  { name; invariants }

let mk_record ?(values = []) ?key name : 'id record =
  { name; key; values }

let mk_record_item ?default name type_ : 'id record_item_gen =
  { name; type_; default }

let mk_storage_item ?(fields = []) ?(invariants = []) ?(init = []) name : 'id storage_item_gen =
  { name; fields; invariants; init }

let mk_item_field ?asset ?(ghost = false) ?default ?(loc = Location.dummy) name typ : 'id item_field =
  { asset; name; typ; ghost; default; loc }

let mk_function_struct ?(args = []) ?(loc = Location.dummy) name body : function_struct =
  { name; args; body; loc }

let mk_function ?verif node : 'id function__ =
  { node; verif }

let mk_signature ?(args = []) ?ret name : 'id signature =
  { name; args; ret}

let mk_model ?(decls = []) name : model =
  { name; decls}


(* -------------------------------------------------------------------- *)

module Utils : sig

  val get_record           : model -> lident -> lident record
  val get_record_field     : model -> (lident * lident) -> record_item
  val get_record_key       : model -> lident -> (lident * btyp)
  val is_storage_attribute : model -> lident -> bool
  val get_named_field_list : model -> lident -> 'a list -> (lident * 'a) list

end = struct

  open Tools
  open Location

  exception Anomaly of string

  type error_desc =
    | RecordNotFound of string
    | RecordFieldNotFound of string * string
    | RecordKeyTypeNotFound of string
    | StorageNotFound
  [@@deriving show {with_path = false}]

  let emit_error (desc : error_desc) =
    let str = Format.asprintf "%a@." pp_error_desc desc in
    raise (Anomaly str)

  let get_record model record_name : lident record =
    let id = unloc record_name in
    let res = List.fold_left (fun accu (x : lident decl_node) ->
        match x with
        | TNrecord r when String.equal (unloc record_name) (unloc r.name) -> Some r
        | _ -> accu
      ) None model.decls in
    match res with
    | Some v -> v
    | _ -> emit_error (RecordNotFound id)

  let get_record_field model (record_name, field_name) =
    let record = get_record model record_name in
    let res = List.fold_left (fun accu (x : record_item) -> if String.equal (unloc field_name) (unloc x.name) then Some x else accu) None record.values in
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

  let get_storage model =
    List.fold_left (fun accu (x : lident decl_node) ->
        match x with
        | TNstorage s -> Some s
        | _ -> accu
      ) None model.decls

  let get_storage_strict model =
    let res = get_storage model in
    match res with
    | Some e -> e
    | _ -> emit_error StorageNotFound

  let is_storage_attribute model id =
    let s = get_storage model in
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
