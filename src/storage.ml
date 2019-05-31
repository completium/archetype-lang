open Ident
open Model

type lident = ident Location.loced
[@@deriving show {with_path = false}]

type type_ = ptyp
type term = pterm

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

type enum_value_ident = lident
[@@deriving show {with_path = false}]

type field =
  | Fid of string
  | Fall
[@@deriving show {with_path = false}]

type storage_policy =
  | Record
  | Flat
  | Hybrid of (asset_ident * field) list
[@@deriving show {with_path = false}]

type execution_mode =
  | WithSideEffect
  | WithoutSideEffect
[@@deriving show {with_path = false}]

type sorting_policy =
  | OnTheFly
  | OnChange
  | None
[@@deriving show {with_path = false}]

let storage_policy = ref Record
(*let storage_policy = ref Flat*)
let execution_mode = ref WithSideEffect
let sorting_policy = ref OnTheFly

type item_field_type =
  | FBasic            of vtyp
  | FKeyCollection    of asset_ident * vtyp
  | FRecordMap        of asset_ident
  | FRecordCollection of asset_ident
  | FEnum             of enum_ident
  | FContainer        of Model.container * item_field_type
[@@deriving show {with_path = false}]

type item_field = {
  asset   : asset_ident option;
  name    : field_ident;
  typ     : item_field_type;
  ghost   : bool;
  default : pterm option; (* initial value *)
  loc     : Location.t [@opaque]
}
[@@deriving show {with_path = false}]

type storage_item = {
  name        : field_ident;
  fields      : item_field list;
  invariants  : (lident, (lident) lterm_gen) label_term list;
  init        : ((ident * pterm) list) list;
}
[@@deriving show {with_path = false}]

type storage = storage_item list
[@@deriving show {with_path = false}]

type enum = {
  name: enum_ident;
  values: enum_value_ident list;
}
[@@deriving show {with_path = false}]

type record_item = {
  name: enum_ident;
  type_: type_;
}
[@@deriving show {with_path = false}]

type record = {
  name: enum_ident;
  values: record_item list;
}
[@@deriving show {with_path = false}]

type 'id function_ = {
  name: fun_ident;
}
[@@deriving show {with_path = false}]

type 'id entry = {
  name: fun_ident;
}
[@@deriving show {with_path = false}]

type argument = argument_ident * type_ * pterm option
[@@deriving show {with_path = false}]

type function_struct = {
  name: fun_ident;
  body: pterm;
}
[@@deriving show {with_path = false}]

type function_node =
  | Function           of function_struct
  | Entry              of function_struct
  | Get                of asset_ident
  | Set                of asset_ident
  | Make               of asset_ident
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
  | AddContainer       of asset_ident * field_ident * Model.container
  | RemoveContainer    of asset_ident * field_ident * Model.container
  | ClearContainer     of asset_ident * field_ident * Model.container
  | ContainsContainer  of asset_ident * field_ident * Model.container
  | NthContainer       of asset_ident * field_ident * Model.container
  | SelectContainer    of asset_ident * field_ident * Model.container
  | SortContainer      of asset_ident * field_ident * Model.container
  | ReverseContainer   of asset_ident * field_ident * Model.container
  | CountContainer     of asset_ident * field_ident * Model.container
  | SumContainer       of asset_ident * field_ident * Model.container
  | MinContainer       of asset_ident * field_ident * Model.container
  | MaxContainer       of asset_ident * field_ident * Model.container
  | Other
[@@deriving show {with_path = false}]

type signature = {
  name: fun_ident;
  args: argument list;
  ret: type_;
}
[@@deriving show {with_path = false}]

type function__ = {
  node: function_node;
  sig_: signature;
}
[@@deriving show {with_path = false}]

type type_node =
  | TNenum of enum
  | TNrecord of record
  | TNstorage of storage
  | TNfunction of function__
[@@deriving show {with_path = false}]

type model = type_node list
[@@deriving show {with_path = false}]

let lident_to_string lident = Location.unloc lident

let function_name_from_function_node = function
  | Function           fs           -> lident_to_string fs.name
  | Entry              fs           -> lident_to_string fs.name
  | Get                aid          -> "get_"      ^ lident_to_string aid
  | Set                aid          -> "set_"      ^ lident_to_string aid
  | Make               aid          -> "mk_"       ^ lident_to_string aid
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
