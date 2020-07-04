type target_lang =
  | Ligo
  | LigoStorage
  | SmartPy
  | Whyml
  | Markdown
  | Scaml
  | None
[@@deriving show {with_path = false}]

type storage_policy =
  | Record
  | Flat
  | Hybrid of (string * string) list
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

let version = "1.1.0"
let url = "https://archetype-lang.org/"

let target = ref (None : target_lang)

let storage_policy = ref Record
let execution_mode = ref WithSideEffect
let sorting_policy = ref OnTheFly


let opt_lsp     = ref false
let opt_service = ref false
let opt_json    = ref false
let opt_pt      = ref false
let opt_ext     = ref false
let opt_ast     = ref false
let opt_typed   = ref false
let opt_all_parenthesis = ref false
let opt_ptc   = ref false
let opt_m     = ref false
let opt_raw   = ref false
let opt_raw_whytree = ref false
let opt_caller = ref ""

let opt_property_focused = ref ""

let opt_vids : (string list) ref = ref []
let add_vids s =
  opt_vids := s::!opt_vids
