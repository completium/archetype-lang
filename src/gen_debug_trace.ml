module M = Model
module T = Michelson

type lcc = {
  line: int;
  col: int;
  char: int
}
[@@deriving yojson, show {with_path = false}]

type range = {
  name: string;
  begin_: lcc;
  end_: lcc;
}
[@@deriving yojson, show {with_path = false}]

type node_micheline =
  | Nprim of prim
  | Nstring of string
  | Nbytes of string
  | Nint of string
  | Narray of micheline list

and prim = {
  prim: string;
  args: micheline list;
  annots: string list;
}

and micheline = {
  node: node_micheline;
  debug: T.debug option;
}
[@@deriving show {with_path = false}]

type arg = {
  name: string;
  type_: string;
}
[@@deriving yojson, show {with_path = false}]

type entrypoint = {
  name: string;
  args: arg list;
  range: range;
}
[@@deriving yojson, show {with_path = false}]

type storage_item = {
  name: string;
  type_: string;
  value: string option;
}
[@@deriving yojson, show {with_path = false}]

type interface = {
  entrypoints: entrypoint list;
  storage: storage_item list;
  const_params: storage_item list;
}
[@@deriving yojson, show {with_path = false}]

type debug_trace = {
  name: string;
  path: string;
  interface: interface;
  contract: micheline;
}
[@@deriving show {with_path = false}]

let mk_micheline ?debug node = {node; debug}

let mk_mich_prim    ?debug ?(args = []) ?(annots = []) prim : micheline = mk_micheline ?debug (Nprim {prim; args; annots})
let mk_mich_string  ?debug x  : micheline = mk_micheline ?debug (Nstring x)
let mk_mich_bytes   ?debug x  : micheline = mk_micheline ?debug (Nbytes x)
let mk_mich_int     ?debug x  : micheline = mk_micheline ?debug (Nint x)
let mk_mich_int_int ?debug x  : micheline = mk_micheline ?debug (Nint (string_of_int x))
let mk_mich_array   ?debug xs : micheline = mk_micheline ?debug (Narray xs)


let rec type_to_micheline (t : T.type_) : micheline =
  let f = type_to_micheline in
  let prim, args =
    match t.node with
    | Taddress               -> "address", []
    | Tbig_map (k, v)        -> "big_map", [f k; f v]
    | Tbool                  -> "bool", []
    | Tbytes                 -> "bytes", []
    | Tchain_id              -> "chain_id", []
    | Tcontract t            -> "contract", [f t]
    | Tint                   -> "int", []
    | Tkey                   -> "key", []
    | Tkey_hash              -> "key_hash", []
    | Tlambda (a, r)         -> "lambda", [f a; f r]
    | Tlist t                -> "list", [f t]
    | Tmap (k, v)            -> "map", [f k; f v]
    | Tmutez                 -> "mutez", []
    | Tnat                   -> "nat", []
    | Toperation             -> "operation", []
    | Toption t              -> "option", [f t]
    | Tor (l, r)             -> "or", [f l; f r]
    | Tpair l                -> "pair", List.map f l
    | Tset t                 -> "set", [f t]
    | Tsignature             -> "signature", []
    | Tstring                -> "string", []
    | Ttimestamp             -> "timestamp", []
    | Tunit                  -> "unit", []
    | Tticket t              -> "ticket", [f t]
    | Tsapling_transaction n -> "sapling_transaction", [mk_mich_int_int n]
    | Tsapling_state n       -> "sapling_state", [mk_mich_int_int n]
    | Tbls12_381_g1          -> "bls12_381_g1", []
    | Tbls12_381_g2          -> "bls12_381_g2", []
    | Tbls12_381_fr          -> "bls12_381_fr", []
    | Tnever                 -> "never", []
    | Tchest                 -> "chest", []
    | Tchest_key             -> "chest_key", []
  in
  let annots = match t.annotation with | Some a -> [a] | None -> [] in
  mk_mich_prim ~args ~annots prim

and data_to_micheline (d : T.data) : micheline =
  let f = data_to_micheline in
  match d with
  | Dint n       -> mk_mich_int (Big_int.string_of_big_int n)
  | Dstring v    -> mk_mich_string v
  | Dbytes v     -> mk_mich_bytes v
  | Dunit        -> mk_mich_prim "Unit"
  | Dtrue        -> mk_mich_prim "True"
  | Dfalse       -> mk_mich_prim "False"
  | Dpair l      -> mk_mich_prim ~args:(List.map f l) "Pair"
  | Dleft v      -> mk_mich_prim ~args:[f v] "Left"
  | Dright v     -> mk_mich_prim ~args:[f v] "Right"
  | Dsome v      -> mk_mich_prim ~args:[f v] "Some"
  | Dnone        -> mk_mich_prim "None"
  | Dlist l      -> mk_mich_array (List.map f l)
  | Delt (l, r)  -> mk_mich_prim ~args:[f l; f r] "Elt"
  | Dvar (x, _t, _b)  -> mk_mich_prim x (* TODO *)
  (* begin
      match t.node with
      | Taddress                -> Ovar (OMVstring x)
      | Tbig_map   (_k, _v)     -> Ovar (OMVfree x)
      | Tbool                   -> Ovar (OMVif (x, Oprim (mk_prim "True"), Oprim (mk_prim "False")))
      | Tbytes                  -> Ovar (OMVbytes x)
      | Tchain_id               -> Ovar (OMVfree x)
      | Tcontract  _t           -> Ovar (OMVfree x)
      | Tint                    -> Ovar (OMVint (x, b))
      | Tkey                    -> Ovar (OMVbytes x)
      | Tkey_hash               -> Ovar (OMVbytes x)
      | Tlambda    (_a, _r)     -> Ovar (OMVfree x)
      | Tlist      _t           -> Ovar (OMVfree x)
      | Tmap       (_k, _v)     -> Ovar (OMVfree x)
      | Tmutez                  -> Ovar (OMVint (x, b))
      | Tnat                    -> Ovar (OMVint (x, b))
      | Toperation              -> Ovar (OMVfree x)
      | Toption    _t           -> Ovar (OMVfree x)
      | Tor        (_l, _r)     -> Ovar (OMVfree x)
      | Tpair      _l           -> Ovar (OMVfree x)
      | Tset       _t           -> Ovar (OMVfree x)
      | Tsignature              -> Ovar (OMVbytes x)
      | Tstring                 -> Ovar (OMVstring x)
      | Ttimestamp              -> Ovar (OMVint (x, b))
      | Tunit                   -> Oprim (mk_prim "Unit")
      | Tticket       _t        -> Ovar (OMVfree x)
      | Tsapling_state       _n -> Ovar (OMVfree x)
      | Tsapling_transaction _n -> Ovar (OMVfree x)
      | Tbls12_381_g1           -> Ovar (OMVfree x)
      | Tbls12_381_g2           -> Ovar (OMVfree x)
      | Tbls12_381_fr           -> Ovar (OMVfree x)
      | Tnever                  -> Ovar (OMVfree x)
      | Tchest                  -> Ovar (OMVfree x)
      | Tchest_key              -> Ovar (OMVfree x)
     end *)
  | DIrCode (_id, _c) -> mk_mich_array []
  | Dcode c           -> code_to_micheline c
  | Dlambda_rec c     -> mk_mich_prim ~args:[code_to_micheline c] "Lambda_rec"
  | Dconstant v       -> mk_mich_prim ~args:[mk_mich_string v] "constant"

and obj_micheline_to_micheline (obj : T.obj_micheline) : micheline =
  let mk node : micheline =  {node = node; debug = None} in
  let rec aux (obj : T.obj_micheline) : micheline =
    match obj with
    | Oprim p -> mk (Nprim ({prim = p.prim; args = List.map aux p.args; annots = p.annots}))
    | Ostring v -> mk (Nstring v)
    | Obytes v -> mk (Nbytes v)
    | Oint v ->  mk (Nint v)
    | Oarray v -> mk (Narray (List.map aux v))
    | Ovar v -> begin
        let f id = mk (Nprim ({prim = id; args = []; annots = []})) in
        match v with
        | OMVfree id -> f id
        | OMVint (id, _) -> f id
        | OMVstring id -> f id
        | OMVbytes id -> f id
        | OMVif (id, _, _) -> f id
      end
  in
  aux obj

and code_to_micheline (code : T.code) : micheline =
  let f = code_to_micheline in
  let ft = type_to_micheline in
  let fd = data_to_micheline in
  let fan = function | Some v -> [v] | None -> [] in
  let mk = mk_mich_prim ?debug:code.debug in
  let mk_int x = mk_mich_int_int ?debug:code.debug x in
  let mk_string x = mk_mich_string ?debug:code.debug x in
  let mk_array l = mk_mich_array ?debug:code.debug (List.map f l) in
  match code.node with
  (* Control structures *)
  | SEQ l                    -> mk_array l
  | APPLY                    -> mk "APPLY"
  | EXEC                     -> mk "EXEC"
  | FAILWITH                 -> mk "FAILWITH"
  | IF (t, e)                -> mk ~args:[mk_array t; mk_array e] "IF"
  | IF_CONS (t, e)           -> mk ~args:[mk_array t; mk_array e] "IF_CONS"
  | IF_LEFT (t, e)           -> mk ~args:[mk_array t; mk_array e] "IF_LEFT"
  | IF_NONE (t, e)           -> mk ~args:[mk_array t; mk_array e] "IF_NONE"
  | ITER l                   -> mk ~args:[mk_array l] "ITER"
  | LAMBDA (at, rt, body)    -> mk ~args:[ft at; ft rt; mk_array body] "LAMBDA"
  | LOOP l                   -> mk ~args:[mk_array l] "LOOP"
  | LOOP_LEFT l              -> mk ~args:[mk_array l] "LOOP_LEFT"
  (* Stack manipulation *)
  | DIG n                    -> mk ~args:[mk_int n] "DIG"
  | DIP (n, l)               -> mk ~args:[mk_int n; mk_array l] "DIP"
  | DROP n                   -> mk ~args:[mk_int n] "DROP"
  | DUG n                    -> mk ~args:[mk_int n] "DUG"
  | DUP                      -> mk "DUP"
  | DUP_N n                  -> mk ~args:[mk_int n] "DUP"
  | PUSH (t, d)              -> mk ~args:[ft t; fd d] "PUSH"
  | SWAP                     -> mk "SWAP"
  (* Arthmetic operations *)
  | ABS                      -> mk "ABS"
  | ADD                      -> mk "ADD"
  | COMPARE                  -> mk "COMPARE"
  | EDIV                     -> mk "EDIV"
  | EQ                       -> mk "EQ"
  | GE                       -> mk "GE"
  | GT                       -> mk "GT"
  | NAT                      -> mk "NAT"
  | INT                      -> mk "INT"
  | BYTES                    -> mk "BYTES"
  | ISNAT                    -> mk "ISNAT"
  | LE                       -> mk "LE"
  | LSL                      -> mk "LSL"
  | LSR                      -> mk "LSR"
  | LT                       -> mk "LT"
  | MUL                      -> mk "MUL"
  | NEG                      -> mk "NEG"
  | NEQ                      -> mk "NEQ"
  | SUB                      -> mk "SUB"
  | SUB_MUTEZ                -> mk "SUB_MUTEZ"
  (* Boolean operations *)
  | AND                      -> mk "AND"
  | NOT                      -> mk "NOT"
  | OR                       -> mk "OR"
  | XOR                      -> mk "XOR"
  (* Cryptographic operations *)
  | BLAKE2B                  -> mk "BLAKE2B"
  | CHECK_SIGNATURE          -> mk "CHECK_SIGNATURE"
  | HASH_KEY                 -> mk "HASH_KEY"
  | KECCAK                   -> mk "KECCAK"
  | PAIRING_CHECK            -> mk "PAIRING_CHECK"
  | SAPLING_EMPTY_STATE n    -> mk "SAPLING_EMPTY_STATE" ~args:[mk_int n]
  | SAPLING_VERIFY_UPDATE    -> mk "SAPLING_VERIFY_UPDATE"
  | SHA256                   -> mk "SHA256"
  | SHA512                   -> mk "SHA512"
  | SHA3                     -> mk "SHA3"
  (* Blockchain operations *)
  | ADDRESS                  -> mk "ADDRESS"
  | AMOUNT                   -> mk "AMOUNT"
  | BALANCE                  -> mk "BALANCE"
  | CHAIN_ID                 -> mk "CHAIN_ID"
  | CONTRACT (t, a)          -> mk ~args:[ft t] ~annots:(fan a) "CONTRACT"
  | CREATE_CONTRACT c        -> mk ~args:[obj_micheline_to_micheline c] "CREATE_CONTRACT"
  | EMIT (t, a)              -> mk ~args:[ft t] ~annots:(fan a) "EMIT"
  | IMPLICIT_ACCOUNT         -> mk "IMPLICIT_ACCOUNT"
  | LEVEL                    -> mk "LEVEL"
  | MIN_BLOCK_TIME           -> mk "MIN_BLOCK_TIME"
  | NOW                      -> mk "NOW"
  | SELF a                   -> mk ~annots:(fan a) "SELF"
  | SELF_ADDRESS             -> mk "SELF_ADDRESS"
  | SENDER                   -> mk "SENDER"
  | SET_DELEGATE             -> mk "SET_DELEGATE"
  | SOURCE                   -> mk "SOURCE"
  | TOTAL_VOTING_POWER       -> mk "TOTAL_VOTING_POWER"
  | TRANSFER_TOKENS          -> mk "TRANSFER_TOKENS"
  | VOTING_POWER             -> mk "VOTING_POWER"
  (* Operations on data structures *)
  | CAR                      -> mk "CAR"
  | CDR                      -> mk "CDR"
  | CONCAT                   -> mk "CONCAT"
  | CONS                     -> mk "CONS"
  | EMPTY_BIG_MAP  (k, v)    -> mk ~args:[ft k; ft v] "EMPTY_BIG_MAP"
  | EMPTY_MAP      (k, v)    -> mk ~args:[ft k; ft v] "EMPTY_MAP"
  | EMPTY_SET      t         -> mk ~args:[ft t] "EMPTY_SET"
  | GET                      -> mk "GET"
  | GET_N n                  -> mk ~args:[mk_int n] "GET"
  | GET_AND_UPDATE           -> mk "GET_AND_UPDATE"
  | LEFT t                   -> mk ~args:[ft t] "LEFT"
  | MAP  l                   -> mk ~args:[mk_array l] "MAP"
  | MEM                      -> mk "MEM"
  | NEVER                    -> mk "NEVER"
  | NIL t                    -> mk ~args:[ft t] "NIL"
  | NONE t                   -> mk ~args:[ft t] "NONE"
  | PACK                     -> mk "PACK"
  | PAIR                     -> mk "PAIR"
  | PAIR_N n                 -> mk ~args:[mk_int n] "PAIR"
  | RIGHT t                  -> mk ~args:[ft t] "RIGHT"
  | SIZE                     -> mk "SIZE"
  | SLICE                    -> mk "SLICE"
  | SOME                     -> mk "SOME"
  | UNIT                     -> mk "UNIT"
  | UNPACK t                 -> mk ~args:[ft t] "UNPACK"
  | UNPAIR                   -> mk "UNPAIR"
  | UNPAIR_N n               -> mk ~args:[mk_int n] "UNPAIR"
  | UPDATE                   -> mk "UPDATE"
  | UPDATE_N n               -> mk ~args:[mk_int n] "UPDATE"
  (* Operations on tickets *)
  | JOIN_TICKETS             -> mk "JOIN_TICKETS"
  | READ_TICKET              -> mk "READ_TICKET"
  | SPLIT_TICKET             -> mk "SPLIT_TICKET"
  | TICKET                   -> mk "TICKET"
  (* Other *)
  | CAST t                   -> mk ~args:[ft t] "CAST"
  | RENAME                   -> mk "RENAME"
  | VIEW (c, t)              -> mk ~args:[mk_string c; ft t] "VIEW"
  | OPEN_CHEST               -> mk "OPEN_CHEST"
  (* Macro *)
  | CAR_N n                  -> mk ~args:[mk_int n] "CAR"
  | CDR_N n                  -> mk ~args:[mk_int n] "CDR"
  (* Custom *)
  | CUSTOM _c                -> assert false (* TODO: obj_micheline *)

let for_interface (model : M.model) : interface =
  let for_range (l : Location.t) : range =
    let for_fcc line col char : lcc = {line; col; char} in
    let sl, sc = l.loc_start in
    let el, ec = l.loc_end in
    {name = l.loc_fname; begin_ = for_fcc sl sc l.loc_bchar ; end_ = for_fcc el ec l.loc_echar  }
  in

  let for_type (ty : M.type_) : string =
    let type_michelson = Gen_michelson.to_type model ty in
    Format.asprintf "%a" Printer_michelson.pp_type type_michelson
  in

  let for_data (model : M.model) (d : M.mterm) : string option =
    d
    |> Gen_michelson.to_simple_data model
    |> Option.map (Format.asprintf "%a" Printer_michelson.pp_data)
  in

  let for_interface_entrypoint (fs : M.function_struct) : entrypoint =
    let for_argument arg : arg =
      {name = (M.unloc_mident (Tools.proj3_1 arg)); type_ = (for_type (Tools.proj3_2 arg))}
    in
    {name = (M.unloc_mident fs.name); args = List.map for_argument fs.args; range = for_range fs.loc }
  in
  let for_interface_storage (model : M.model) =
    (* let po = Gen_contract_interface.get_var_decls_size model in
       let s = Gen_contract_interface.for_storage_internal model po in *)
    List.map (fun (si : Model.storage_item) -> {name = (Model.unloc_mident si.id); type_ = for_type (si.typ); value = for_data model si.default }) model.storage
    (* List.map (fun (name, ty, _const, default) -> {name = name; type_ = for_type ty; value = Option.bind default (for_data model)}) s *)
  in
  let interface_entrypoints = List.map for_interface_entrypoint (List.fold_right (fun (x : M.function_node) accu -> match x with | Entry fs -> fs::accu | _ -> accu) model.functions [])  in
  let interface_storage = for_interface_storage model in
  let interface_const_params = model.parameters |> List.filter (fun (x : Model.parameter) -> x.const) |> List.map (fun (x : Model.parameter) -> {name = (Model.unloc_mident x.name); type_ = for_type (x.typ); value = Option.bind x.value (for_data model) }) in
  let interface : interface = {entrypoints = interface_entrypoints; storage = interface_storage; const_params = interface_const_params } in
  interface

let generate_debug_trace_json (model : M.model) (michelson : T.michelson) : debug_trace =
  let storage = mk_mich_prim "storage" ~args:[type_to_micheline michelson.storage] in
  let parameter = mk_mich_prim "parameter" ~args:[type_to_micheline michelson.parameter] in
  let code = mk_mich_prim "code" ~args:[code_to_micheline michelson.code] in
  let contract = mk_mich_array [storage; parameter; code] in
  let interface = for_interface model in
  let res : debug_trace = {name = (Location.unloc model.name); path = ""; interface = interface; contract = contract} in
  res

let pp_range fmt (l : Location.t) =
  let pp_position fmt (a, b, c) =
    Format.fprintf fmt "{\"line\":%d,\"col\":%d,\"char\":%d}"
      a b c
  in
  let sl, sc = l.loc_start in
  let el, ec = l.loc_end in
  Format.fprintf fmt ",\"range\":{\"name\":\"%s\",\"begin\":%a,\"end\":%a}"
    l.loc_fname
    pp_position (sl, sc, l.loc_bchar)
    pp_position (el, ec, l.loc_echar)

let pp_decl_bound fmt (decl_bound : T.decl_bound) =
  Format.fprintf fmt ",\"decl_bound\":{\"kind\":\"%s\",\"name\":\"%s\",\"bound\":\"%s\"}"
    decl_bound.db_kind
    decl_bound.db_name
    decl_bound.db_bound

let pp_debug_ fmt (debug : T.debug) =
  let pp_option = Printer_tools.pp_option in
  let pp_list = Printer_tools.pp_list in
  let pp_stack_item fmt (si : T.stack_item) = Format.fprintf fmt "{\"name\":\"%s\", \"kind\":\"%s\"}" si.stack_item_name si.stack_item_kind in
  Format.fprintf fmt ",\"debug\":{\"stack\":[%a]%a%a}"
    (pp_list "," pp_stack_item) debug.stack
    (pp_option pp_range) debug.loc
    (pp_option pp_decl_bound) debug.decl_bound

let rec pp_micheline_ fmt (mich : micheline) =
  let pp_list = Printer_tools.pp_list in
  let pp_option = Printer_tools.pp_option in
  let pp_string fmt str = Format.fprintf fmt "\"%s\"" str in
  let pp_list_non_empty pp fmt l = if List.length l = 0 then () else pp fmt l in
  let pp_args fmt xs = Format.fprintf fmt ",\"args\":[%a]" (pp_list "," pp_micheline_) xs in
  let pp_annots fmt xs = Format.fprintf fmt ",\"annots\":[%a]" (pp_list "," pp_string) xs in
  match mich.node with
  | Nprim {prim; args; annots} ->
    Format.fprintf fmt "{\"prim\":\"%s\"%a%a%a}"
      prim
      (pp_list_non_empty pp_args) args
      (pp_list_non_empty pp_annots) annots
      (pp_option pp_debug_) mich.debug
  | Nstring v -> Format.fprintf fmt "{\"string\":\"%s\"}" v
  | Nbytes  v -> Format.fprintf fmt "{\"bytes\":\"%s\"}" v
  | Nint    v -> Format.fprintf fmt "{\"int\":\"%s\"}" v
  | Narray  v -> Format.fprintf fmt "[%a]" (Printer_tools.pp_list "," pp_micheline_) v

let pp_trace_json fmt (debug_trace : debug_trace) =
  Format.fprintf fmt "{\"name\":\"%s\",\"interface\":%s,\"contract\":%a}\n"
    debug_trace.name
    (Yojson.Safe.to_string (interface_to_yojson debug_trace.interface))
    pp_micheline_ debug_trace.contract
