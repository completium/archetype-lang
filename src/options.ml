type target_lang =
  | Liquidity
  | Ligo
  | SmartPy
  | Whyml
  | Markdown
  | Ocaml
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
let opt_raw_model = ref false
let opt_wse = ref false
let opt_sa = ref false
let opt_api = ref false
let opt_raw_target = ref false
let opt_liq_url = ref false
let opt_cwse = ref false
let debug_mode = ref false

let fake_ast = ref false
let fake_ast2 = ref false
