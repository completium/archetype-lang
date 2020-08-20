(* open Tools *)
open Printer_tools
open Michelson

let rec pp_type fmt (t : type_) =
  let pp_annot fmt a = Format.fprintf fmt " %%%a" pp_str a in
  let pp_annot_opt fmt _ = (pp_option pp_annot) fmt t.annotation in
  let pp_simple_a str =
    match t.annotation with
    | Some a -> Format.fprintf fmt "(%a%a)" pp_str str pp_annot a
    | _ -> pp_str fmt str
  in
  match t.node with
  | Tkey                -> pp_simple_a "key"
  | Tunit               -> pp_simple_a "unit"
  | Tsignature          -> pp_simple_a "signature"
  | Toption    t        -> Format.fprintf fmt "(option%a %a)"     pp_annot_opt () pp_type t
  | Tlist      t        -> Format.fprintf fmt "(list%a %a)"       pp_annot_opt () pp_type t
  | Tset       t        -> Format.fprintf fmt "(set%a %a)"        pp_annot_opt () pp_type t
  | Toperation          -> pp_simple_a "operation"
  | Tcontract  t        -> Format.fprintf fmt "(contract%a %a)"   pp_annot_opt () pp_type t
  | Tpair      (lt, rt) -> Format.fprintf fmt "(pair%a %a %a)"    pp_annot_opt () pp_type lt  pp_type rt
  | Tor        (lt, rt) -> Format.fprintf fmt "(or%a %a %a)"      pp_annot_opt () pp_type lt  pp_type rt
  | Tlambda    (at, rt) -> Format.fprintf fmt "(lambda%a %a %a)"  pp_annot_opt () pp_type at  pp_type rt
  | Tmap       (kt, vt) -> Format.fprintf fmt "(map%a %a %a)"     pp_annot_opt () pp_type kt  pp_type vt
  | Tbig_map   (kt, vt) -> Format.fprintf fmt "(big_map%a %a %a)" pp_annot_opt () pp_type kt  pp_type vt
  | Tchain_id           -> pp_simple_a "chain_id"
  | Tint                -> pp_simple_a "int"
  | Tnat                -> pp_simple_a "nat"
  | Tstring             -> pp_simple_a "string"
  | Tbytes              -> pp_simple_a "bytes"
  | Tmutez              -> pp_simple_a "mutez"
  | Tbool               -> pp_simple_a "bool"
  | Tkey_hash           -> pp_simple_a "key_hash"
  | Ttimestamp          -> pp_simple_a "timestamp"
  | Taddress            -> pp_simple_a "address"

let rec pp_data fmt (d : data) =
  match d with
  | Dint    v       -> pp_big_int fmt v
  | Dstring v       -> Format.fprintf fmt "\"%s\"" v
  | Dbytes  v       -> Format.fprintf fmt "0x%s"     v
  | Dunit           -> Format.fprintf fmt "Unit"
  | Dtrue           -> Format.fprintf fmt "True"
  | Dfalse          -> Format.fprintf fmt "False"
  | Dpair  (ld, rd) -> Format.fprintf fmt "(Pair %a %a)" pp_data ld pp_data rd
  | Dleft   d       -> Format.fprintf fmt "(Left %a)"      pp_data d
  | Dright  d       -> Format.fprintf fmt "(Right %a)"     pp_data d
  | Dsome   d       -> Format.fprintf fmt "(Some %a)"      pp_data d
  | Dnone           -> Format.fprintf fmt "None"
  | Dlist l         -> Format.fprintf fmt "{ %a }" (pp_list "; " pp_data) l
  | Dplist l        -> Format.fprintf fmt "{ %a }" (pp_list "; " (fun fmt (x, y) -> Format.fprintf fmt "Elt %a %a" pp_data x pp_data y)) l

let pp_id fmt i = Format.fprintf fmt "%s" i

let rec pp_instruction fmt (i : instruction) =
  let f = pp_instruction in
  match i with
  | Iseq l -> (pp_list ";@\n" f) fmt l
  | IletIn (id, v, b) -> Format.fprintf fmt "let %a = %a in@\n  @[%a@]" pp_id id f v f b
  | Ivar id -> pp_id fmt id
  | Icall (id, args) -> Format.fprintf fmt "%a(%a)" pp_id id (pp_list ", " f) args
  | Iassign (id, _, v) -> Format.fprintf fmt "%a := %a" pp_id id f v
  | Izop op -> begin
      match op with
      | Znow               -> pp_id fmt "now"
      | Zamount            -> pp_id fmt "amount"
      | Zbalance           -> pp_id fmt "balance"
      | Zsource            -> pp_id fmt "source"
      | Zsender            -> pp_id fmt "sender"
      | Zaddress           -> pp_id fmt "address"
      | Zchain_id          -> pp_id fmt "chain_id"
      | Zself_address      -> pp_id fmt "self_address"
      | Znone t            -> Format.fprintf fmt "none(%a)" pp_type t
    end
  | Iunop (op, e) -> begin
      match op with
      | Ucar      -> Format.fprintf fmt "car(%a)"      f e
      | Ucdr      -> Format.fprintf fmt "cdr(%a)"      f e
      | Uneg      -> Format.fprintf fmt "neg(%a)"      f e
      | Uint      -> Format.fprintf fmt "int(%a)"      f e
      | Unot      -> Format.fprintf fmt "not(%a)"      f e
      | Uabs      -> Format.fprintf fmt "abs(%a)"      f e
      | Uisnat    -> Format.fprintf fmt "isnat(%a)"    f e
      | Usome     -> Format.fprintf fmt "some(%a)"     f e
      | Usize     -> Format.fprintf fmt "size(%a)"     f e
      | Upack     -> Format.fprintf fmt "pack(%a)"     f e
      | Uunpack t -> Format.fprintf fmt "unpack<%a>(%a)" pp_type t f e
      | Ublake2b  -> Format.fprintf fmt "blake2b(%a)"  f e
      | Usha256   -> Format.fprintf fmt "sha256(%a)"   f e
      | Usha512   -> Format.fprintf fmt "sha512(%a)"   f e
      | Uhash_key -> Format.fprintf fmt "hash_key(%a)" f e
      | Ufail     -> Format.fprintf fmt "fail(%a)"     f e
    end
  | Ibinop (op, lhs, rhs) -> begin
      match op with
      | Badd       -> Format.fprintf fmt "(%a) * (%a)"       f lhs f rhs
      | Bsub       -> Format.fprintf fmt "(%a) - (%a)"       f lhs f rhs
      | Bmul       -> Format.fprintf fmt "(%a) * (%a)"       f lhs f rhs
      | Bediv      -> Format.fprintf fmt "(%a) / (%a)"       f lhs f rhs
      | Blsl       -> Format.fprintf fmt "(%a) << (%a)"      f lhs f rhs
      | Blsr       -> Format.fprintf fmt "(%a) >> (%a)"      f lhs f rhs
      | Bor        -> Format.fprintf fmt "(%a) or (%a)"      f lhs f rhs
      | Band       -> Format.fprintf fmt "(%a) and (%a)"     f lhs f rhs
      | Bxor       -> Format.fprintf fmt "(%a) xor (%a)"     f lhs f rhs
      | Bcompare   -> Format.fprintf fmt "compare (%a, %a)"  f lhs f rhs
      | Beq        -> Format.fprintf fmt "(%a) = (%a)"       f lhs f rhs
      | Bneq       -> Format.fprintf fmt "(%a) <> (%a)"      f lhs f rhs
      | Blt        -> Format.fprintf fmt "(%a) < (%a)"       f lhs f rhs
      | Bgt        -> Format.fprintf fmt "(%a) > (%a)"       f lhs f rhs
      | Ble        -> Format.fprintf fmt "(%a) <= (%a)"      f lhs f rhs
      | Bge        -> Format.fprintf fmt "(%a) >= (%a)"      f lhs f rhs
      | Bget       -> Format.fprintf fmt "get(%a, %a)"       f lhs f rhs
      | Bmem       -> Format.fprintf fmt "mem(%a, %a)"       f lhs f rhs
      | Bconcat    -> Format.fprintf fmt "concat(%a, %a)"    f lhs f rhs
      | Bcons      -> Format.fprintf fmt "cons(%a, %a)"      f lhs f rhs
      | Bpair      -> Format.fprintf fmt "pair(%a, %a)"      f lhs f rhs
    end
  | Iterop (op, a1, a2, a3) -> begin
      match op with
      | Tcheck_signature -> Format.fprintf fmt "check_signature(%a, %a, %a)" f a1 f a2 f a3
      | Tslice           -> Format.fprintf fmt "slice(%a, %a, %a)"           f a1 f a2 f a3
      | Tupdate          -> Format.fprintf fmt "update(%a, %a, %a)"          f a1 f a2 f a3
    end
  | Iconst (t, e)  -> Format.fprintf fmt "const(%a : %a)" pp_data e pp_type t
  | Iif (c, t, e)  -> Format.fprintf fmt "if (%a)@\nthen @[%a@]@\nelse @[%a@]@\n" f c f t f e
  | Iset (t, l)    -> Format.fprintf fmt "set<%a>[%a]" pp_type t (pp_list "; " f) l
  | Ilist (t, l)   -> Format.fprintf fmt "list<%a>[%a]" pp_type t (pp_list "; " f) l
  | Imap (k, v, l) -> Format.fprintf fmt "map<%a, %a>[%a]" pp_type k pp_type v (pp_list "; " (fun fmt (vk, vv) -> Format.fprintf fmt "%a : %a" f vk f vv)) l
  | Irecord l      -> Format.fprintf fmt "record[%a]" (pp_list "; " f) l
  | Iwhile _ -> assert false

let pp_func fmt (f : func) =
  Format.fprintf fmt "function %s (%a) : %a {@\n  @[%a@]@\n}@\n "
    f.name
    (pp_list ", " (fun fmt (id, t) -> Format.fprintf fmt "%s : %a" id pp_type t)) f.args
    pp_type f.ret
    pp_instruction f.body

let pp_entry fmt (e : entry) =
  Format.fprintf fmt "entry %s (%a) {@\n  @[%a@]@\n}@\n "
    e.name
    (pp_list ", " (fun fmt (id, t) -> Format.fprintf fmt "%s : %a" id pp_type t)) e.args
    pp_instruction e.body

let pp_ir fmt (ir : ir) =
  Format.fprintf fmt "storage_type: %a@\n@\n" pp_type (fst ir.storage);
  Format.fprintf fmt "storage_data: %a@\n@\n" pp_data (snd ir.storage);
  (pp_list "@\n@\n" pp_func) fmt ir.funs;
  (pp_list "@\n@\n" pp_entry) fmt ir.entries

let rec pp_code fmt (i : code) =
  let pp_inc fmt i =
    match i with
    | 0 -> ()
    | _ -> Format.fprintf fmt " 0"
  in
  let fs fmt = Format.fprintf fmt "{ @[%a@] }" (pp_list ";@\n" pp_code) in
  match i with
  | SEQ l                -> fs fmt l
  | DROP i               -> Format.fprintf fmt "DROP%a" pp_inc i
  | DUP                  -> Format.fprintf fmt "DUP"
  | SWAP                 -> Format.fprintf fmt "SWAP"
  | DIG i                -> Format.fprintf fmt "DIG%a" pp_inc i
  | DUG i                -> Format.fprintf fmt "DUG%a" pp_inc i
  | PUSH (t, d)          -> Format.fprintf fmt "PUSH %a %a" pp_type t pp_data d
  | SOME                 -> Format.fprintf fmt "SOME"
  | NONE t               -> Format.fprintf fmt "NONE %a" pp_type t
  | UNIT                 -> Format.fprintf fmt "UNIT"
  | IF_NONE (ti, ei)     -> Format.fprintf fmt "IF_NONE %a %a" fs ti fs ei
  | PAIR                 -> Format.fprintf fmt "PAIR"
  | CAR                  -> Format.fprintf fmt "CAR"
  | CDR                  -> Format.fprintf fmt "CDR"
  | LEFT  t              -> Format.fprintf fmt "LEFT %a" pp_type t
  | RIGHT t              -> Format.fprintf fmt "RIGHT %a" pp_type t
  | IF_LEFT (ti, ei)     -> Format.fprintf fmt "IF_LEFT %a %a" fs ti fs ei
  | NIL t                -> Format.fprintf fmt "NIL %a" pp_type t
  | CONS                 -> Format.fprintf fmt "CONS"
  | IF_CONS (ti, ei)     -> Format.fprintf fmt "IF_CONS %a %a" fs ti fs ei
  | SIZE                 -> Format.fprintf fmt "SIZE"
  | EMPTY_SET     t      -> Format.fprintf fmt "EMPTY_SET %a" pp_type t
  | EMPTY_MAP     (k, v) -> Format.fprintf fmt "EMPTY_MAP %a %a" pp_type k pp_type v
  | EMPTY_BIG_MAP (k, v) -> Format.fprintf fmt "EMPTY_BIG_MAP %a %a" pp_type k pp_type v
  | MAP  is              -> Format.fprintf fmt "MAP %a" fs is
  | ITER is              -> Format.fprintf fmt "ITER %a" fs is
  | MEM                  -> Format.fprintf fmt "MEM"
  | GET                  -> Format.fprintf fmt "GET"
  | UPDATE               -> Format.fprintf fmt "UPDATE"
  | IF (ti, ei)          -> Format.fprintf fmt "IF %a %a" fs ti fs ei
  | LOOP is              -> Format.fprintf fmt "LOOP %a" fs is
  | LOOP_LEFT is         -> Format.fprintf fmt "LOOP_LEFT %a" fs is
  | LAMBDA (at, rt, is)  -> Format.fprintf fmt "LAMBDA %a %a %a" pp_type at pp_type rt fs is
  | EXEC                 -> Format.fprintf fmt "EXEC"
  | DIP (i, is)          -> Format.fprintf fmt "DIP%a %a" pp_inc i fs is
  | FAILWITH             -> Format.fprintf fmt "FAILWITH"
  | CAST                 -> Format.fprintf fmt "CAST"
  | RENAME               -> Format.fprintf fmt "RENAME"
  | CONCAT               -> Format.fprintf fmt "CONCAT"
  | SLICE                -> Format.fprintf fmt "SLICE"
  | PACK                 -> Format.fprintf fmt "PACK"
  | UNPACK t             -> Format.fprintf fmt "UNPACK %a" pp_type t
  | ADD                  -> Format.fprintf fmt "ADD"
  | SUB                  -> Format.fprintf fmt "SUB"
  | MUL                  -> Format.fprintf fmt "MUL"
  | EDIV                 -> Format.fprintf fmt "EDIV"
  | ABS                  -> Format.fprintf fmt "ABS"
  | ISNAT                -> Format.fprintf fmt "ISNAT"
  | INT                  -> Format.fprintf fmt "INT"
  | NEG                  -> Format.fprintf fmt "NEG"
  | LSL                  -> Format.fprintf fmt "LSL"
  | LSR                  -> Format.fprintf fmt "LSR"
  | OR                   -> Format.fprintf fmt "OR"
  | AND                  -> Format.fprintf fmt "AND"
  | XOR                  -> Format.fprintf fmt "XOR"
  | NOT                  -> Format.fprintf fmt "NOT"
  | COMPARE              -> Format.fprintf fmt "COMPARE"
  | EQ                   -> Format.fprintf fmt "EQ"
  | NEQ                  -> Format.fprintf fmt "NEQ"
  | LT                   -> Format.fprintf fmt "LT"
  | GT                   -> Format.fprintf fmt "GT"
  | LE                   -> Format.fprintf fmt "LE"
  | GE                   -> Format.fprintf fmt "GE"
  | SELF                 -> Format.fprintf fmt "SELF"
  | CONTRACT t           -> Format.fprintf fmt "CONTRACT %a" pp_type t
  | TRANSFER_TOKENS      -> Format.fprintf fmt "TRANSFER_TOKENS"
  | SET_DELEGATE         -> Format.fprintf fmt "SET_DELEGATE"
  | CREATE_ACCOUNT       -> Format.fprintf fmt "CREATE_ACCOUNT"
  | CREATE_CONTRACT  is  -> Format.fprintf fmt "CREATE_CONTRACT %a" fs is
  | IMPLICIT_ACCOUNT     -> Format.fprintf fmt "IMPLICIT_ACCOUNT"
  | NOW                  -> Format.fprintf fmt "NOW"
  | AMOUNT               -> Format.fprintf fmt "AMOUNT"
  | BALANCE              -> Format.fprintf fmt "BALANCE"
  | CHECK_SIGNATURE      -> Format.fprintf fmt "CHECK_SIGNATURE"
  | BLAKE2B              -> Format.fprintf fmt "BLAKE2B"
  | SHA256               -> Format.fprintf fmt "SHA256"
  | SHA512               -> Format.fprintf fmt "SHA512"
  | HASH_KEY             -> Format.fprintf fmt "HASH_KEY"
  | STEPS_TO_QUOTA       -> Format.fprintf fmt "STEPS_TO_QUOTA"
  | SOURCE               -> Format.fprintf fmt "SOURCE"
  | SENDER               -> Format.fprintf fmt "SENDER"
  | ADDRESS              -> Format.fprintf fmt "ADDRESS"
  | CHAIN_ID             -> Format.fprintf fmt "CHAIN_ID"

let pp_michelson fmt (m : michelson) =
  Format.fprintf fmt
    "{@\n  \
     storage %a;@\n  \
     parameter %a;@\n  \
     code %a;@\n\
     }"
    pp_type m.storage
    pp_type m.parameter
    pp_code m.code

(* -------------------------------------------------------------------------- *)

let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model x = string_of__of_pp pp_michelson x
