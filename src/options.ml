type target_lang =
  | Michelson
  | MichelsonStorage
  | OffchainViews
  | Javascript
  | Markdown
  | BindingsJs
  | BindingsTs
  | ContractMetadata
  | DebugTrace
[@@deriving show {with_path = false}]

exception UnknownTarget of string

let string_to_target_lang = function
  | "michelson"         -> Michelson
  | "michelson-storage" -> MichelsonStorage
  | "offchain-views"    -> OffchainViews
  | "contract-metadata" -> ContractMetadata
  | "javascript"        -> Javascript
  | "markdown"          -> Markdown
  | "bindings-js"       -> BindingsJs
  | "bindings-ts"       -> BindingsTs
  | "debug-trace"       -> DebugTrace
  | v                   -> raise (UnknownTarget v)

let version = "1.5.3-rc"
let url = "https://archetype-lang.org/"

let target = ref (Michelson : target_lang)

let debug = ref false
let quiet = ref false
let with_init_caller = ref true

let opt_json    = ref false
let opt_input_json = ref false
let opt_rjson   = ref false
let opt_pt      = ref false
let opt_extpt   = ref false
let opt_ext     = ref false
let opt_ast     = ref false
let opt_mdl     = ref false
let opt_omdl    = ref false
let opt_typed   = ref false
let opt_ir      = ref false
let opt_dir     = ref false
let opt_mic     = ref false
let opt_mit     = ref false
let opt_mici    = ref false
let opt_all_parenthesis = ref false
let opt_m     = ref false
let opt_raw   = ref false
let opt_raw_ir = ref false
let opt_raw_michelson = ref false
let opt_raw_debug_trace = ref false
let opt_caller = ref "$CALLER_ADDRESS"
let opt_decomp = ref false
let opt_trace = ref false
let opt_metadata_uri = ref ""
let opt_metadata_storage = ref ""
let opt_with_metadata = ref false
let opt_expr : (string option) ref = ref (None : string option)
let opt_entrypoint : (string option) ref = ref (None : string option)
let opt_type : (string option) ref = ref (None : string option)
let opt_with_contract = ref false
let opt_code_only = ref false
let opt_expr_only = ref false
let opt_init = ref ""
let opt_no_js_header = ref false
let opt_sdir = ref false
let opt_test_mode = ref false
let opt_get_storage_values = ref false
let opt_with_parameters = ref false
let opt_show_entries = ref false
let opt_out = ref ""
let opt_contract_interface = ref false
let opt_contract_interface_michelson = ref false
let opt_event_well_address : (string option) ref = ref (None : string option)
let opt_sandbox_exec_address : (string option) ref = ref (None : string option)
let opt_g = ref false
let opt_path = ref "."
let opt_ama = ref false
let opt_alma = ref false

let opt_property_focused = ref ""

let opt_vids : (string list) ref = ref []
let add_vids s =
  opt_vids := s::!opt_vids

type lsp_kind =
  | Errors
  | Outline
[@@deriving yojson, show {with_path = false}]

exception UnknownLspKind of string

let string_to_kind k =
  match k with
  | "errors" -> Errors
  | "outline" -> Outline
  | v -> raise (UnknownLspKind v)

let opt_lsp_kind = ref (None : lsp_kind option)

type language =
  | Typescript
  | Javascript

let language_to_string = function
  | Typescript -> "typescript"
  | Javascript -> "javascript"
