type target_lang =
  | Liquidity
  | Whyml
  | Markdown
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

let target = ref (None : target_lang)

let storage_policy = ref Record
let execution_mode = ref WithSideEffect
let sorting_policy = ref OnTheFly


let opt_lsp = ref false
let opt_json = ref false
let opt_pretty_print = ref false
let opt_parse = ref false
let opt_pre_json = ref false
let opt_pre_pretty_print = ref false
let opt_pre_parse = ref false
let opt_ast = ref false
let opt_astr = ref false
let opt_model = ref false
let opt_raw_target = ref false
let debug_mode = ref false
