open Ident

module A = Ast


type lident = ident Location.loced
[@@deriving show {with_path = false}]

type type_ = A.ptyp
[@@deriving show {with_path = false}]

type term = A.pterm
[@@deriving show {with_path = false}]

type field_ident = lident
[@@deriving show {with_path = false}]

type argument_ident = lident
[@@deriving show {with_path = false}]

type fun_ident = lident
[@@deriving show {with_path = false}]

type asset_ident = lident
[@@deriving show {with_path = false}]

type enum_ident = lident
[@@deriving show {with_path = false}]

type record_ident = lident
[@@deriving show {with_path = false}]

type contract_ident = lident
[@@deriving show {with_path = false}]

type contract_function_ident = lident
[@@deriving show {with_path = false}]

type enum_value_ident = lident
[@@deriving show {with_path = false}]

type item_field_type =
  | FBasic            of A.vtyp
  | FAssetKeys        of A.vtyp * asset_ident
  | FAssetRecord      of A.vtyp * asset_ident
  | FRecordCollection of asset_ident
  | FRecord           of asset_ident
  | FEnum             of enum_ident
  | FContainer        of Ast.container * item_field_type
[@@deriving show {with_path = false}]

type item_field = {
  asset   : asset_ident option;
  name    : field_ident;
  typ     : item_field_type;
  ghost   : bool;
  default : A.pterm option; (* initial value *)
  loc     : Location.t [@opaque]
}
[@@deriving show {with_path = false}]

type storage_item = {
  name        : field_ident;
  fields      : item_field list;
  invariants  : (lident, (lident, type_) A.lterm_gen) A.label_term list;
  init        : ((ident * A.pterm) list) list;
}
[@@deriving show {with_path = false}]

type storage = storage_item list
[@@deriving show {with_path = false}]

type enum_item = {
  name: enum_value_ident;
  invariants : (lident, (lident, type_) A.lterm_gen) A.label_term list;
}
[@@deriving show {with_path = false}]

type enum = {
  name: enum_ident;
  values: enum_item list;
}
[@@deriving show {with_path = false}]

type record_item = {
  name: record_ident;
  type_: type_;
  default: A.pterm option;
}
[@@deriving show {with_path = false}]

type record = {
  name: record_ident;
  key: record_ident option;
  values: record_item list;
}
[@@deriving show {with_path = false}]

type contract = (lident, type_, A.pterm) Ast.contract
[@@deriving show {with_path = false}]

type 'id function_ = {
  name: fun_ident;
}
[@@deriving show {with_path = false}]

type 'id entry = {
  name: fun_ident;
}
[@@deriving show {with_path = false}]

type argument = argument_ident * type_ * A.pterm option
[@@deriving show {with_path = false}]

type function_struct = {
  name: fun_ident;
  args: argument list;
  body: A.instruction;
  loc : Location.t [@opaque];
}
[@@deriving show {with_path = false}]

type function_node =
  | Function           of function_struct * type_ (* fun * return type *)
  | Entry              of function_struct
  | Get                of asset_ident
  | AddAsset           of asset_ident
  | RemoveAsset        of asset_ident
  | ClearAsset         of asset_ident
  | UpdateAsset        of asset_ident
  | ContainsAsset      of asset_ident
  | NthAsset           of asset_ident
  | SelectAsset        of asset_ident
  | SortAsset          of asset_ident
  | ReverseAsset       of asset_ident
  | CountAsset         of asset_ident
  | SumAsset           of asset_ident
  | MinAsset           of asset_ident
  | MaxAsset           of asset_ident
  | AddContainer       of asset_ident * field_ident * A.container
  | RemoveContainer    of asset_ident * field_ident * A.container
  | ClearContainer     of asset_ident * field_ident * A.container
  | ContainsContainer  of asset_ident * field_ident * A.container
  | NthContainer       of asset_ident * field_ident * A.container
  | SelectContainer    of asset_ident * field_ident * A.container
  | SortContainer      of asset_ident * field_ident * A.container
  | ReverseContainer   of asset_ident * field_ident * A.container
  | CountContainer     of asset_ident * field_ident * A.container
  | SumContainer       of asset_ident * field_ident * A.container
  | MinContainer       of asset_ident * field_ident * A.container
  | MaxContainer       of asset_ident * field_ident * A.container
  | Other
[@@deriving show {with_path = false}]

type signature = {
  name: fun_ident;
  args: argument list;
  ret: type_ option;
}
[@@deriving show {with_path = false}]

type function__ = {
  node: function_node;
  verif  : (lident, type_, A.pterm) A.verification option;
}
[@@deriving show {with_path = false}]

type decl_node =
  | TNenum of enum
  | TNrecord of record
  | TNcontract of contract
  | TNstorage of storage
  | TNfunction of function__
[@@deriving show {with_path = false}]

type model = {
  name: lident;
  decls: decl_node list;
}
[@@deriving show {with_path = false}]

let lident_to_string lident = Location.unloc lident

let function_name_from_function_node = function
  | Function           (fs, _)      -> lident_to_string fs.name
  | Entry              fs           -> lident_to_string fs.name
  | Get                aid          -> "get_"      ^ lident_to_string aid
  | AddAsset           aid          -> "add_"      ^ lident_to_string aid
  | RemoveAsset        aid          -> "remove_"   ^ lident_to_string aid
  | ClearAsset         aid          -> "clear_"    ^ lident_to_string aid
  | UpdateAsset        aid          -> "update_"   ^ lident_to_string aid
  | ContainsAsset      aid          -> "contains_" ^ lident_to_string aid
  | NthAsset           aid          -> "nth_"      ^ lident_to_string aid
  | SelectAsset        aid          -> "select_"   ^ lident_to_string aid
  | SortAsset          aid          -> "sort_"     ^ lident_to_string aid
  | ReverseAsset       aid          -> "reverse_"  ^ lident_to_string aid
  | CountAsset         aid          -> "count_"    ^ lident_to_string aid
  | SumAsset           aid          -> "sum_"      ^ lident_to_string aid
  | MinAsset           aid          -> "min_"      ^ lident_to_string aid
  | MaxAsset           aid          -> "max_"      ^ lident_to_string aid
  | AddContainer      (aid, fid, _) -> "add_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | RemoveContainer   (aid, fid, _) -> "remove_"   ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | ClearContainer    (aid, fid, _) -> "clear_"    ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | ContainsContainer (aid, fid, _) -> "contains_" ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | NthContainer      (aid, fid, _) -> "nth_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | SelectContainer   (aid, fid, _) -> "select_"   ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | SortContainer     (aid, fid, _) -> "sort_"     ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | ReverseContainer  (aid, fid, _) -> "reverse_"  ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | CountContainer    (aid, fid, _) -> "count_"    ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | SumContainer      (aid, fid, _) -> "sum_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | MinContainer      (aid, fid, _) -> "min_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | MaxContainer      (aid, fid, _) -> "max_"      ^ lident_to_string aid ^ "_" ^ lident_to_string fid
  | Other -> assert false

let mk_enum ?(values = []) name : enum =
  { name; values }

let mk_enum_item ?(invariants = []) name : enum_item =
  { name; invariants }

let mk_record ?(values = []) ?key name : record =
  { name; key; values }

let mk_record_item ?default name type_ : record_item =
  { name; type_; default }

let mk_storage_item ?(fields = []) ?(invariants = []) ?(init = []) name : storage_item =
  { name; fields; invariants; init }

let mk_item_field ?asset ?(ghost = false) ?default ?(loc = Location.dummy) name typ : item_field =
  { asset; name; typ; ghost; default; loc }

let mk_function_struct ?(args = []) ?(loc = Location.dummy) name body : function_struct =
  { name; args; body; loc }

let mk_function ?verif node : function__ =
  { node; verif }

let mk_signature ?(args = []) ?ret name : signature =
  { name; args; ret}

let mk_model ?(decls = []) name : model =
  { name; decls}


(* -------------------------------------------------------------------- *)

module Utils : sig

  val get_record       : model -> A.lident -> record
  val get_record_field : model -> (A.lident * A.lident ) -> record_item
  val get_record_key   : model -> A.lident -> (A.lident * A.vtyp)

end = struct

  open Tools
  open Location

  exception Anomaly of string

  type error_desc =
    | RecordNotFound of string
    | RecordFieldNotFound of string * string
    | RecordKeyTypeNotFound of string
  [@@deriving show {with_path = false}]

  let emit_error (desc : error_desc) =
    let str = Format.asprintf "%a@." pp_error_desc desc in
    raise (Anomaly str)

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

  let get_record_field model (record_name, field_name) =
    let record = get_record model record_name in
    let res = List.fold_left (fun accu (x : record_item) -> if String.equal (unloc field_name) (unloc x.name) then Some x else accu) None record.values in
    match res with
    | Some v -> v
    | _ -> emit_error (RecordFieldNotFound (unloc record_name, unloc field_name))

  let get_record_key model record_name : (lident * A.vtyp) =
    let record = get_record model record_name in
    let key_id = Option.get record.key in
    let key_field = get_record_field model (record_name, key_id) in
    match key_field.type_ with
    | Tbuiltin v -> (key_id, v)
    | _ -> emit_error (RecordKeyTypeNotFound (unloc record_name))
end