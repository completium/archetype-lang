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


let rec pp_instruction fmt (i : instruction) =
  let pp_inc fmt i =
    match i with
    | 0 -> ()
    | _ -> Format.fprintf fmt " 0"
  in
  let fs fmt = Format.fprintf fmt "{ @[%a@] }" (pp_list ";@\n" pp_instruction) in
  match i with
  | SEQ l               -> fs fmt l
  | DROP i              -> Format.fprintf fmt "DROP%a" pp_inc i
  | DUP                 -> Format.fprintf fmt "DUP"
  | SWAP                -> Format.fprintf fmt "SWAP"
  | DIG i               -> Format.fprintf fmt "DIG%a" pp_inc i
  | DUG i               -> Format.fprintf fmt "DUG%a" pp_inc i
  | PUSH (t, d)         -> Format.fprintf fmt "PUSH %a %a" pp_type t pp_data d
  | SOME                -> Format.fprintf fmt "SOME"
  | NONE t              -> Format.fprintf fmt "NONE %a" pp_type t
  | UNIT                -> Format.fprintf fmt "UNIT"
  | IF_NONE (ti, ei)    -> Format.fprintf fmt "IF_NONE %a %a" fs ti fs ei
  | PAIR                -> Format.fprintf fmt "PAIR"
  | CAR                 -> Format.fprintf fmt "CAR"
  | CDR                 -> Format.fprintf fmt "CDR"
  | LEFT  t             -> Format.fprintf fmt "LEFT %a" pp_type t
  | RIGHT t             -> Format.fprintf fmt "RIGHT %a" pp_type t
  | IF_LEFT (ti, ei)    -> Format.fprintf fmt "IF_LEFT %a %a" fs ti fs ei
  | NIL t               -> Format.fprintf fmt "NIL %a" pp_type t
  | CONS                -> Format.fprintf fmt "CONS"
  | IF_CONS (ti, ei)    -> Format.fprintf fmt "IF_CONS %a %a" fs ti fs ei
  | SIZE                -> Format.fprintf fmt "SIZE"
  | EMPTY_SET     t     -> Format.fprintf fmt "EMPTY_SET %a" pp_type t
  | EMPTY_MAP     t     -> Format.fprintf fmt "EMPTY_MAP %a" pp_type t
  | EMPTY_BIG_MAP t     -> Format.fprintf fmt "EMPTY_BIG_MAP %a" pp_type t
  | MAP  is             -> Format.fprintf fmt "MAP %a" fs is
  | ITER is             -> Format.fprintf fmt "ITER %a" fs is
  | MEM                 -> Format.fprintf fmt "MEM"
  | GET                 -> Format.fprintf fmt "GET"
  | UPDATE              -> Format.fprintf fmt "UPDATE"
  | IF (ti, ei)         -> Format.fprintf fmt "IF %a %a" fs ti fs ei
  | LOOP is             -> Format.fprintf fmt "LOOP %a" fs is
  | LOOP_LEFT is        -> Format.fprintf fmt "LOOP_LEFT %a" fs is
  | LAMBDA (at, rt, is) -> Format.fprintf fmt "LAMBDA %a %a %a" pp_type at pp_type rt fs is
  | EXEC                -> Format.fprintf fmt "EXEC"
  | DIP (i, is)         -> Format.fprintf fmt "DIP%a %a" pp_inc i fs is
  | FAILWITH            -> Format.fprintf fmt "FAILWITH"
  | CAST                -> Format.fprintf fmt "CAST"
  | RENAME              -> Format.fprintf fmt "RENAME"
  | CONCAT              -> Format.fprintf fmt "CONCAT"
  | SLICE               -> Format.fprintf fmt "SLICE"
  | PACK                -> Format.fprintf fmt "PACK"
  | UNPACK t            -> Format.fprintf fmt "UNPACK %a" pp_type t
  | ADD                 -> Format.fprintf fmt "ADD"
  | SUB                 -> Format.fprintf fmt "SUB"
  | MUL                 -> Format.fprintf fmt "MUL"
  | EDIV                -> Format.fprintf fmt "EDIV"
  | ABS                 -> Format.fprintf fmt "ABS"
  | ISNAT               -> Format.fprintf fmt "ISNAT"
  | INT                 -> Format.fprintf fmt "INT"
  | NEG                 -> Format.fprintf fmt "NEG"
  | LSL                 -> Format.fprintf fmt "LSL"
  | LSR                 -> Format.fprintf fmt "LSR"
  | OR                  -> Format.fprintf fmt "OR"
  | AND                 -> Format.fprintf fmt "AND"
  | XOR                 -> Format.fprintf fmt "XOR"
  | NOT                 -> Format.fprintf fmt "NOT"
  | COMPARE             -> Format.fprintf fmt "COMPARE"
  | EQ                  -> Format.fprintf fmt "EQ"
  | NEQ                 -> Format.fprintf fmt "NEQ"
  | LT                  -> Format.fprintf fmt "LT"
  | GT                  -> Format.fprintf fmt "GT"
  | LE                  -> Format.fprintf fmt "LE"
  | GE                  -> Format.fprintf fmt "GE"
  | SELF                -> Format.fprintf fmt "SELF"
  | CONTRACT t          -> Format.fprintf fmt "CONTRACT %a" pp_type t
  | TRANSFER_TOKENS     -> Format.fprintf fmt "TRANSFER_TOKENS"
  | SET_DELEGATE        -> Format.fprintf fmt "SET_DELEGATE"
  | CREATE_ACCOUNT      -> Format.fprintf fmt "CREATE_ACCOUNT"
  | CREATE_CONTRACT  is -> Format.fprintf fmt "CREATE_CONTRACT %a" fs is
  | IMPLICIT_ACCOUNT    -> Format.fprintf fmt "IMPLICIT_ACCOUNT"
  | NOW                 -> Format.fprintf fmt "NOW"
  | AMOUNT              -> Format.fprintf fmt "AMOUNT"
  | BALANCE             -> Format.fprintf fmt "BALANCE"
  | CHECK_SIGNATURE     -> Format.fprintf fmt "CHECK_SIGNATURE"
  | BLAKE2B             -> Format.fprintf fmt "BLAKE2B"
  | SHA256              -> Format.fprintf fmt "SHA256"
  | SHA512              -> Format.fprintf fmt "SHA512"
  | HASH_KEY            -> Format.fprintf fmt "HASH_KEY"
  | STEPS_TO_QUOTA      -> Format.fprintf fmt "STEPS_TO_QUOTA"
  | SOURCE              -> Format.fprintf fmt "SOURCE"
  | SENDER              -> Format.fprintf fmt "SENDER"
  | ADDRESS             -> Format.fprintf fmt "ADDRESS"
  | CHAIN_ID            -> Format.fprintf fmt "CHAIN_ID"

let pp_michelson fmt (m : michelson) =
  Format.fprintf fmt
    "{@\n  \
     storage %a;@\n  \
     parameter %a;@\n  \
     code %a;@\n\
     }"
    pp_type m.storage
    pp_type m.parameter
    pp_instruction m.code

(* -------------------------------------------------------------------------- *)

let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model x = string_of__of_pp pp_michelson x
