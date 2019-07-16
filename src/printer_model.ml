open Model
open Printer_tools

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_currency fmt = function
  | Tez   -> Format.fprintf fmt "tz"
  | Mutez -> Format.fprintf fmt "mtz"

let pp_btyp fmt = function
  | Bbool       -> Format.fprintf fmt "bool"
  | Bint        -> Format.fprintf fmt "int"
  | Buint       -> Format.fprintf fmt "unit"
  | Brational   -> Format.fprintf fmt "rational"
  | Bdate       -> Format.fprintf fmt "date"
  | Bduration   -> Format.fprintf fmt "duration"
  | Bstring     -> Format.fprintf fmt "string"
  | Baddress    -> Format.fprintf fmt "address"
  | Brole       -> Format.fprintf fmt "role"
  | Bcurrency c -> pp_currency fmt c
  | Bkey        -> Format.fprintf fmt "key"

let pp_container fmt (c : container) =
  match c with
  | Collection -> Format.fprintf fmt "collection"
  | Partition  -> Format.fprintf fmt "partition"

let rec pp_type fmt t =
  match t with
  | Tasset an ->
    Format.fprintf fmt "asset %a" pp_id an
  | Tenum en ->
    Format.fprintf fmt "enum %a" pp_id en
  | Tcontract cn ->
    Format.fprintf fmt "contract %a" pp_id cn
  | Tbuiltin b -> pp_btyp fmt b
  | Tcontainer (t, c) ->
    Format.fprintf fmt "(%a) %a"
      pp_type t
      pp_container c
  | Toption t ->
    Format.fprintf fmt "%a option"
      pp_type_ t
  | Ttuple ts ->
    Format.fprintf fmt "%a"
      (pp_list " * " pp_type_) ts
  | Tunit ->
    Format.fprintf fmt "unit"
  | Tentry ->
    Format.fprintf fmt "entry"
  | Tprog _
  | Tvset _
  | Ttrace _ -> Format.fprintf fmt "todo"


let pp_mterm fmt mt =
  Model.pp_mterm fmt mt

let pp_api_item fmt (api_item : api_item) =
  Format.fprintf fmt "%a"
    pp_api_item_node api_item.node

let pp_enum_item fmt (enum_item : enum_item) =
  Format.fprintf fmt "%a"
    pp_id enum_item.name

let pp_enum fmt (enum : enum) =
  Format.fprintf fmt "enum %a {@\n@[<v 2>  %a@]@\n}@\n"
    pp_id enum.name
    (pp_list "@\n" pp_enum_item) enum.values

let pp_record_item fmt (item : record_item) =
  Format.fprintf fmt "%a : %a%a"
    pp_id item.name
    pp_type item.type_
    (pp_option (fun fmt -> Format.fprintf fmt " := %a" pp_mterm)) item.default

let pp_record fmt (record : record) =
  Format.fprintf fmt "record %a%a {@\n@[<v 2>  %a@]@\n}@\n"
    pp_id record.name
    (pp_option (fun fmt -> Format.fprintf fmt " identified by %a" pp_id)) record.key
    (pp_list "@\n" pp_record_item) record.values

let pp_contract_signature fmt (cs : contract_signature) =
  Format.fprintf fmt "%a : %a"
    pp_id cs.name
    (pp_list " -> " pp_type) cs.args

let pp_contract fmt (contract : contract) =
  Format.fprintf fmt "contract %a {@\n@[<v 2>  %a@]@\n}%a@\n"
    pp_id contract.name
    (pp_list "@\n" pp_contract_signature) contract.signatures
    (pp_option pp_mterm) contract.init

let pp_decl fmt (decl : decl_node) =
  match decl with
  | Denum e -> pp_enum fmt e
  | Drecord r -> pp_record fmt r
  | Dcontract c -> Format.fprintf fmt "TODO@."

let rec pp_item_field_type fmt (ift : 'id item_field_type) =
  match ift with
  | FBasic b -> pp_btyp fmt b
  | FAssetKeys (b, v) -> Format.fprintf fmt "asset keys %a of %a" pp_btyp b pp_id v
  | FAssetRecord (b, v) -> Format.fprintf fmt "asset records %a of %a" pp_btyp b pp_id v
  | FRecordCollection v  -> Format.fprintf fmt "record collection of %a" pp_id v
  | FRecord v -> Format.fprintf fmt "record %a" pp_id v
  | FEnum v -> Format.fprintf fmt "enum %a" pp_id v
  | FContainer (c, ift) -> Format.fprintf fmt "(%a) %a" pp_item_field_type ift pp_container c

let pp_item_field fmt (f : item_field) =
  Format.fprintf fmt "%a : %a%a"
    pp_id f.name
    pp_item_field_type f.typ
    (pp_option (fun fmt -> Format.fprintf fmt " := %a" pp_mterm)) f.default

let pp_storage_item fmt (si : storage_item) =
  Format.fprintf fmt "%a {@\n@[<v 2>  %a@]@\n}@\n"
    pp_id si.name
    (pp_list "@\n" pp_item_field) si.fields

let pp_storage fmt (s : storage) =
  Format.fprintf fmt "storage {@\n@[<v 2>  %a@]@\n}@\n"
    (pp_list "" pp_storage_item) s

let pp_argument fmt arg =
  Format.fprintf fmt "%a"
    pp_argument arg

let pp_model fmt (model : model) =
  Format.fprintf fmt "%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @."
    pp_id model.name
    (pp_list "@\n" pp_api_item) model.api_items
    (pp_list "@\n" pp_decl) model.decls
    pp_storage model.storage

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
