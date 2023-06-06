open Tools
open Printer_tools
open Michelson

let rec pp_type fmt (t : type_) =
  let pp_annot fmt a = Format.fprintf fmt " %a" pp_str a in
  let pp_annot_opt fmt _ = (pp_option pp_annot) fmt t.annotation in
  let pp_simple_a str =
    match t.annotation with
    | Some a -> Format.fprintf fmt "(%a%a)" pp_str str pp_annot a
    | _ -> pp_str fmt str
  in
  match t.node with
  | Tkey                   -> pp_simple_a "key"
  | Tunit                  -> pp_simple_a "unit"
  | Tsignature             -> pp_simple_a "signature"
  | Toption    t           -> Format.fprintf fmt "(option%a %a)"     pp_annot_opt () pp_type t
  | Tlist      t           -> Format.fprintf fmt "(list%a %a)"       pp_annot_opt () pp_type t
  | Tset       t           -> Format.fprintf fmt "(set%a %a)"        pp_annot_opt () pp_type t
  | Toperation             -> pp_simple_a "operation"
  | Tcontract  t           -> Format.fprintf fmt "(contract%a %a)"   pp_annot_opt () pp_type t
  | Tpair      l           -> Format.fprintf fmt "(pair%a %a)"       pp_annot_opt () (pp_list " " pp_type) l
  | Tor        (lt, rt)    -> Format.fprintf fmt "(or%a %a %a)"      pp_annot_opt () pp_type lt  pp_type rt
  | Tlambda    (at, rt)    -> Format.fprintf fmt "(lambda%a %a %a)"  pp_annot_opt () pp_type at  pp_type rt
  | Tmap       (kt, vt)    -> Format.fprintf fmt "(map%a %a %a)"     pp_annot_opt () pp_type kt  pp_type vt
  | Tbig_map   (kt, vt)    -> Format.fprintf fmt "(big_map%a %a %a)" pp_annot_opt () pp_type kt  pp_type vt
  | Tchain_id              -> pp_simple_a "chain_id"
  | Tint                   -> pp_simple_a "int"
  | Tnat                   -> pp_simple_a "nat"
  | Tstring                -> pp_simple_a "string"
  | Tbytes                 -> pp_simple_a "bytes"
  | Tmutez                 -> pp_simple_a "mutez"
  | Tbool                  -> pp_simple_a "bool"
  | Tkey_hash              -> pp_simple_a "key_hash"
  | Ttimestamp             -> pp_simple_a "timestamp"
  | Taddress               -> pp_simple_a "address"
  | Tticket       t        -> Format.fprintf fmt "(ticket%a %a)"        pp_annot_opt () pp_type t
  | Tsapling_state       n -> Format.fprintf fmt "(sapling_state %i)" n
  | Tsapling_transaction n -> Format.fprintf fmt "(sapling_transaction %i)" n
  | Tbls12_381_fr          -> pp_simple_a "bls12_381_fr"
  | Tbls12_381_g1          -> pp_simple_a "bls12_381_g1"
  | Tbls12_381_g2          -> pp_simple_a "bls12_381_g2"
  | Tnever                 -> pp_simple_a "never"
  | Tchest                 -> pp_simple_a "chest"
  | Tchest_key             -> pp_simple_a "chest_key"

let rec pp_pretty_type fmt (t : type_) =
  match t.node with
  | Toption    t        -> Format.fprintf fmt "option_%a"     pp_pretty_type t
  | Tlist      t        -> Format.fprintf fmt "list_%a"       pp_pretty_type t
  | Tset       t        -> Format.fprintf fmt "set_%a"        pp_pretty_type t
  | Tcontract  t        -> Format.fprintf fmt "contract_%a"   pp_pretty_type t
  | Tpair      l        -> Format.fprintf fmt "pair_%a"       (pp_list "_" pp_pretty_type) l
  | Tor        (lt, rt) -> Format.fprintf fmt "or_%a_%a"      pp_pretty_type lt  pp_pretty_type rt
  | Tlambda    (at, rt) -> Format.fprintf fmt "lambda_%a_%a"  pp_pretty_type at  pp_pretty_type rt
  | Tmap       (kt, vt) -> Format.fprintf fmt "map_%a_%a"     pp_pretty_type kt  pp_pretty_type vt
  | Tbig_map   (kt, vt) -> Format.fprintf fmt "big_map_%a_%a" pp_pretty_type kt  pp_pretty_type vt
  | _ -> pp_type fmt t

let rec pp_data fmt (d : data) =
  let pp s = Format.fprintf fmt s in
  match d with
  | Dint    v         -> pp_big_int fmt v
  | Dstring v         -> pp "\"%s\"" (String.escaped v)
  | Dbytes  v         -> pp "0x%s"     v
  | Dunit             -> pp "Unit"
  | Dtrue             -> pp "True"
  | Dfalse            -> pp "False"
  | Dpair   l         -> pp "(Pair %a)"     (pp_list " " pp_data) l
  | Dleft   d         -> pp "(Left %a)"      pp_data d
  | Dright  d         -> pp "(Right %a)"     pp_data d
  | Dsome   d         -> pp "(Some %a)"      pp_data d
  | Dnone             -> pp "None"
  | Dlist l           -> pp "{ %a }" (pp_list "; " pp_data) l
  | Delt (x, y)       -> pp "Elt %a %a" pp_data x pp_data y
  | Dvar (x, _, _)    -> pp "%s" x
  | DIrCode (_id, _c) -> pp "IrCode"
  | Dcode c           -> pp "{ %a }" pp_code c
  | Dlambda_rec c     -> pp "(Lambda_rec { %a })" pp_code c
  | Dconstant v       -> pp "(constant %s)" v

and pp_code fmt (i : code) =
  let pp s = Format.fprintf fmt s in
  let pp_annot = pp_option (fun fmt -> Format.fprintf fmt " %s") in
  let pp_arg fmt i =
    match i with
    | 0 | 1 -> ()
    | _ -> Format.fprintf fmt " %i" i
  in
  let pp_arg2 fmt i =
    match i with
    | 0 -> ()
    | _ -> Format.fprintf fmt " %i" i
  in
  let rec with_complex_instr (c : code) =
    match c.node with
    | SEQ _
    | IF _
    | IF_CONS _
    | IF_LEFT _
    | IF_NONE _
    | ITER _
    | LAMBDA _
    | LOOP _
    | LOOP_LEFT _
    | CREATE_CONTRACT _
      -> true
    | DIP (_, l) when List.exists with_complex_instr l -> true
    | _ -> false
  in
  let with_complex_instrs l = List.exists with_complex_instr l in
  (* let fsv fmt = Format.fprintf fmt "{ @[%a@] }" (pp_list ";@\n" pp_code) in
     let fsh fmt = Format.fprintf fmt "{ %a }" (pp_list "; " pp_code) in
     let fsl fmt l =
     if with_complex_instrs l
     then fsv fmt l
     else fsh fmt l
     in
     match i with *)
  let fsv fmt = Format.fprintf fmt "{ @[%a@] }" (pp_list ";@\n" pp_code) in
  let pp_dip_arg fmt (i, l) =
    let pp_aux fmt l =
      if with_complex_instrs l
      then fsv fmt l
      else Format.fprintf fmt "{ @[%a@] }" (pp_list "; " pp_code) l
    in
    if i = 1
    then pp_aux fmt l
    else begin
      if List.exists with_complex_instr l
      then Format.fprintf fmt "@[%i@\n%a@]" i pp_aux l
      else Format.fprintf fmt "%i %a" i pp_aux l
    end
  in
  let pp_concrete_michelson fmt (c : obj_micheline) =
    let m_ = Michelson.to_tz_micheline c in
    (* let ppf = Format.std_formatter in *)
    Micheline_printer.print_expr fmt m_

  (* let pp s = Format.fprintf fmt s in
     let pp_empty pp fmt l =
     if List.is_empty l
     then ()
     else (Format.fprintf fmt " "; pp fmt l)
     in
     match c with
     | Oprim prim -> pp "%s%a%a" prim.prim (pp_empty (pp_list " " pp_str)) prim.annots (pp_empty (pp_list " " pp_concrete_michelson)) prim.args
     | Ostring v  -> pp "\"%s\"" v
     | Obytes v   -> pp "0x%s" v
     | Oint v     -> pp "%s" v
     | Oarray l   -> pp "{ %a }" (pp_list "; " pp_concrete_michelson) l
     | Ovar _     -> assert false *)
  in
  match i.node with
  (* Control structures *)
  | SEQ l                    -> fsv fmt l
  | APPLY                    -> pp "APPLY"
  | EXEC                     -> pp "EXEC"
  | FAILWITH                 -> pp "FAILWITH"
  | IF (ti, ei)              -> pp "IF@\n  @[%a@]@\n  @[%a@]" fsv ti fsv ei
  | IF_CONS (ti, ei)         -> pp "IF_CONS@\n  @[%a@]@\n  @[%a@]" fsv ti fsv ei
  | IF_LEFT (ti, ei)         -> pp "IF_LEFT@\n  @[%a@]@\n  @[%a@]" fsv ti fsv ei
  | IF_NONE (ti, ei)         -> pp "IF_NONE@\n  @[%a@]@\n  @[%a@]" fsv ti fsv ei
  | ITER is                  -> pp "ITER %a" fsv is
  | LAMBDA (at, rt, is)      -> pp "LAMBDA@\n  @[%a@]@\n  @[%a@]@\n  @[%a@]" pp_type at pp_type rt fsv is
  | LOOP is                  -> pp "LOOP %a" fsv is
  | LOOP_LEFT is             -> pp "LOOP_LEFT %a" fsv is
  (* Stack manipulation *)
  | DIG i                    -> pp "DIG%a" pp_arg2 i
  | DIP (i, is)              -> pp "DIP %a" pp_dip_arg (i, is)
  | DROP i                   -> pp "DROP%a" pp_arg i
  | DUG i                    -> pp "DUG%a" pp_arg2 i
  | DUP                      -> pp "DUP"
  | DUP_N i                  -> pp "DUP%a" pp_arg2 i
  | PUSH (t, d)              -> pp "PUSH %a %a" pp_type t pp_data d
  | SWAP                     -> pp "SWAP"
  (* Arthmetic operations *)
  | ABS                      -> pp "ABS"
  | ADD                      -> pp "ADD"
  | BYTES                    -> pp "BYTES"
  | COMPARE                  -> pp "COMPARE"
  | EDIV                     -> pp "EDIV"
  | EQ                       -> pp "EQ"
  | GE                       -> pp "GE"
  | GT                       -> pp "GT"
  | INT                      -> pp "INT"
  | ISNAT                    -> pp "ISNAT"
  | LE                       -> pp "LE"
  | LSL                      -> pp "LSL"
  | LSR                      -> pp "LSR"
  | LT                       -> pp "LT"
  | MUL                      -> pp "MUL"
  | NAT                      -> pp "NAT"
  | NEG                      -> pp "NEG"
  | NEQ                      -> pp "NEQ"
  | SUB                      -> pp "SUB"
  | SUB_MUTEZ                -> pp "SUB_MUTEZ"
  (* Boolean operations *)
  | AND                      -> pp "AND"
  | NOT                      -> pp "NOT"
  | OR                       -> pp "OR"
  | XOR                      -> pp "XOR"
  (* Cryptographic operations *)
  | BLAKE2B                  -> pp "BLAKE2B"
  | CHECK_SIGNATURE          -> pp "CHECK_SIGNATURE"
  | HASH_KEY                 -> pp "HASH_KEY"
  | KECCAK                   -> pp "KECCAK"
  | PAIRING_CHECK            -> pp "PAIRING_CHECK"
  | SAPLING_EMPTY_STATE n    -> pp "SAPLING_EMPTY_STATE %i" n
  | SAPLING_VERIFY_UPDATE    -> pp "SAPLING_VERIFY_UPDATE"
  | SHA256                   -> pp "SHA256"
  | SHA512                   -> pp "SHA512"
  | SHA3                     -> pp "SHA3"
  (* Blockchain operations *)
  | ADDRESS                  -> pp "ADDRESS"
  | AMOUNT                   -> pp "AMOUNT"
  | BALANCE                  -> pp "BALANCE"
  | CHAIN_ID                 -> pp "CHAIN_ID"
  | CONTRACT (t, a)          -> pp "CONTRACT%a %a" pp_annot a pp_type t
  | CREATE_CONTRACT c        -> pp "CREATE_CONTRACT@\n  @[%a@]" pp_concrete_michelson c
  | EMIT (t, a)              -> pp "EMIT%a %a" pp_annot a pp_type t
  | IMPLICIT_ACCOUNT         -> pp "IMPLICIT_ACCOUNT"
  | LEVEL                    -> pp "LEVEL"
  | MIN_BLOCK_TIME           -> pp "MIN_BLOCK_TIME"
  | NOW                      -> pp "NOW"
  | SELF a                   -> pp "SELF%a" pp_annot a
  | SELF_ADDRESS             -> pp "SELF_ADDRESS"
  | SENDER                   -> pp "SENDER"
  | SET_DELEGATE             -> pp "SET_DELEGATE"
  | SOURCE                   -> pp "SOURCE"
  | TOTAL_VOTING_POWER       -> pp "TOTAL_VOTING_POWER"
  | TRANSFER_TOKENS          -> pp "TRANSFER_TOKENS"
  | VOTING_POWER             -> pp "VOTING_POWER"
  (* Operations on data structures *)
  | CAR                      -> pp "CAR"
  | CDR                      -> pp "CDR"
  | CONCAT                   -> pp "CONCAT"
  | CONS                     -> pp "CONS"
  | EMPTY_BIG_MAP (k, v)     -> pp "EMPTY_BIG_MAP %a %a" pp_type k pp_type v
  | EMPTY_MAP     (k, v)     -> pp "EMPTY_MAP %a %a" pp_type k pp_type v
  | EMPTY_SET     t          -> pp "EMPTY_SET %a" pp_type t
  | GET                      -> pp "GET"
  | GET_N n                  -> pp "GET%a" pp_arg2 n
  | GET_AND_UPDATE           -> pp "GET_AND_UPDATE"
  | LEFT  t                  -> pp "LEFT %a" pp_type t
  | MAP  is                  -> pp "MAP %a" fsv is
  | MEM                      -> pp "MEM"
  | NEVER                    -> pp "NEVER"
  | NIL t                    -> pp "NIL %a" pp_type t
  | NONE t                   -> pp "NONE %a" pp_type t
  | PACK                     -> pp "PACK"
  | PAIR                     -> pp "PAIR"
  | PAIR_N n                 -> pp "PAIR%a" pp_arg2 n
  | RIGHT t                  -> pp "RIGHT %a" pp_type t
  | SIZE                     -> pp "SIZE"
  | SLICE                    -> pp "SLICE"
  | SOME                     -> pp "SOME"
  | UNIT                     -> pp "UNIT"
  | UNPACK t                 -> pp "UNPACK %a" pp_type t
  | UNPAIR                   -> pp "UNPAIR"
  | UNPAIR_N n               -> pp "UNPAIR%a" pp_arg2 n
  | UPDATE                   -> pp "UPDATE"
  | UPDATE_N n               -> pp "UPDATE%a" pp_arg2 n
  (* Operations on tickets *)
  | JOIN_TICKETS             -> pp "JOIN_TICKETS"
  | READ_TICKET              -> pp "READ_TICKET"
  | SPLIT_TICKET             -> pp "SPLIT_TICKET"
  | TICKET                   -> pp "TICKET"
  (* Other *)
  | CAST t                   -> pp "CAST %a" pp_type t
  | RENAME                   -> pp "RENAME"
  | VIEW (c, t)              -> pp "VIEW \"%s\" %a" c pp_type t
  | OPEN_CHEST               -> pp "OPEN_CHEST"
  (* Macro *)
  | CAR_N n                  -> pp "CAR%a" pp_arg2 n
  | CDR_N n                  -> pp "CDR%a" pp_arg n
  (* Macro *)
  | CUSTOM micheline         -> begin
      let printable_micheline : Micheline_printer.node = Micheline_tools.obj_to_micheline micheline in
      Micheline_printer.print_expr fmt printable_micheline
    end
(*
let pp_simple_code fmt c =
  let pp s = Format.fprintf fmt s in
  match c with
  (* Control structures *)
  | SEQ _                    -> pp "SEQ"
  | APPLY                    -> pp "APPLY"
  | EXEC                     -> pp "EXEC"
  | FAILWITH                 -> pp "FAILWITH"
  | IF _                     -> pp "IF"
  | IF_CONS _                -> pp "IF_CONS"
  | IF_LEFT _                -> pp "IF_LEFT"
  | IF_NONE _                -> pp "IF_NONE"
  | ITER _                   -> pp "ITER"
  | LAMBDA _                 -> pp "LAMBDA"
  | LOOP _                   -> pp "LOOP"
  | LOOP_LEFT _              -> pp "LOOP_LEFT"
  (* Stack manipulation *)
  | DIG  _                   -> pp "DIG"
  | DIP  _                   -> pp "DIP"
  | DROP _                   -> pp "DROP"
  | DUG  _                   -> pp "DUG"
  | DUP                      -> pp "DUP"
  | PUSH _                   -> pp "PUSH"
  | SWAP                     -> pp "SWAP"
  (* Arthmetic operations *)
  | ABS                      -> pp "ABS"
  | ADD                      -> pp "ADD"
  | COMPARE                  -> pp "COMPARE"
  | EDIV                     -> pp "EDIV"
  | EQ                       -> pp "EQ"
  | GE                       -> pp "GE"
  | GT                       -> pp "GT"
  | INT                      -> pp "INT"
  | ISNAT                    -> pp "ISNAT"
  | LE                       -> pp "LE"
  | LSL                      -> pp "LSL"
  | LSR                      -> pp "LSR"
  | LT                       -> pp "LT"
  | MUL                      -> pp "MUL"
  | NEG                      -> pp "NEG"
  | NEQ                      -> pp "NEQ"
  | SUB                      -> pp "SUB"
  (* Boolean operations *)
  | AND                      -> pp "AND"
  | NOT                      -> pp "NOT"
  | OR                       -> pp "OR"
  | XOR                      -> pp "XOR"
  (* Cryptographic operations *)
  | BLAKE2B                  -> pp "BLAKE2B"
  | CHECK_SIGNATURE          -> pp "CHECK_SIGNATURE"
  | HASH_KEY                 -> pp "HASH_KEY"
  | SHA256                   -> pp "SHA256"
  | SHA512                   -> pp "SHA512"
  (* Blockchain operations *)
  | ADDRESS                  -> pp "ADDRESS"
  | AMOUNT                   -> pp "AMOUNT"
  | BALANCE                  -> pp "BALANCE"
  | CHAIN_ID                 -> pp "CHAIN_ID"
  | CONTRACT _               -> pp "CONTRACT"
  | CREATE_CONTRACT _        -> pp "CREATE_CONTRACT"
  | IMPLICIT_ACCOUNT         -> pp "IMPLICIT_ACCOUNT"
  | NOW                      -> pp "NOW"
  | SELF _                   -> pp "SELF"
  | SENDER                   -> pp "SENDER"
  | SET_DELEGATE             -> pp "SET_DELEGATE"
  | SOURCE                   -> pp "SOURCE"
  | TRANSFER_TOKENS          -> pp "TRANSFER_TOKENS"
  (* Operations on data structures *)
  | CAR                      -> pp "CAR"
  | CDR                      -> pp "CDR"
  | CONCAT                   -> pp "CONCAT"
  | CONS                     -> pp "CONS"
  | EMPTY_BIG_MAP _          -> pp "EMPTY_BIG_MAP"
  | EMPTY_MAP     _          -> pp "EMPTY_MAP"
  | EMPTY_SET     _          -> pp "EMPTY_SET"
  | GET                      -> pp "GET"
  | LEFT  _                  -> pp "LEFT"
  | MAP _                    -> pp "MAP"
  | MEM                      -> pp "MEM"
  | NIL _                    -> pp "NIL"
  | NONE _                   -> pp "NONE"
  | PACK                     -> pp "PACK"
  | PAIR                     -> pp "PAIR"
  | RIGHT _                  -> pp "RIGHT"
  | SIZE                     -> pp "SIZE"
  | SLICE                    -> pp "SLICE"
  | SOME                     -> pp "SOME"
  | UNIT                     -> pp "UNIT"
  | UNPACK _                 -> pp "UNPACK"
  | UPDATE                   -> pp "UPDATE"
  (* Operations on tickets *)
  | JOIN_TICKETS             -> pp "JOIN_TICKETS"
  | READ_TICKET              -> pp "READ_TICKET"
  | SPLIT_TICKET             -> pp "SPLIT_TICKET"
  | TICKET                   -> pp "TICKET"
  (* Other *)
  | UNPAIR                   -> pp "UNPAIR"
  | SELF_ADDRESS             -> pp "SELF_ADDRESS"
  | CAST _                   -> pp "CAST"
  | CREATE_ACCOUNT           -> pp "CREATE_ACCOUNT"
  | RENAME                   -> pp "RENAME"
  | STEPS_TO_QUOTA           -> pp "STEPS_TO_QUOTA"
  | LEVEL                    -> pp "LEVEL"
  | SAPLING_EMPTY_STATE _    -> pp "SAPLING_EMPTY_STATE"
  | SAPLING_VERIFY_UPDATE    -> pp "SAPLING_VERIFY_UPDATE"
  | NEVER                    -> pp "NEVER"
  | VOTING_POWER             -> pp "VOTING_POWER"
  | TOTAL_VOTING_POWER       -> pp "TOTAL_VOTING_POWER"
  | KECCAK                   -> pp "KECCAK"
  | SHA3                     -> pp "SHA3"
  | PAIRING_CHECK            -> pp "PAIRING_CHECK"
  | SUBMIT_PROPOSALS         -> pp "SUBMIT_PROPOSALS"
  | SUBMIT_BALLOT            -> pp "SUBMIT_BALLOT"
  | SET_BAKER_ACTIVE         -> pp "SET_BAKER_ACTIVE"
  | TOGGLE_BAKER_DELEGATIONS -> pp "TOGGLE_BAKER_DELEGATIONS"
  | SET_BAKER_CONSENSUS_KEY  -> pp "SET_BAKER_CONSENSUS_KEY"
  | SET_BAKER_PVSS_KEY       -> pp "SET_BAKER_PVSS_KEY"
  (* View *)
  | VIEW _ -> pp ""
  (* Macro *)
  | CAR_N n                    -> pp "CAR%a" pp_arg2 n
  | CDR_N n                    -> pp "CDR%a" pp_arg n *)

let pp_id fmt i = Format.fprintf fmt "%s" i

let pp_zop fmt op =
  let pp s = Format.fprintf fmt s in
  match op with
  | Znow                -> pp "now"
  | Zamount             -> pp "amount"
  | Zbalance            -> pp "balance"
  | Zsource             -> pp "source"
  | Zsender             -> pp "sender"
  | Zaddress            -> pp "address"
  | Zchain_id           -> pp "chain_id"
  | Zself _             -> pp "self"
  | Zself_address       -> pp "self_address"
  | Znone t             -> pp "none(%a)" pp_type t
  | Zunit               -> pp "unit"
  | Znil t              -> pp "nil(%a)" pp_type t
  | Zemptyset t         -> pp "emptyset(%a)" pp_type t
  | Zemptymap (k, v)    -> pp "emptymap(%a, %a)" pp_type k pp_type v
  | Zemptybigmap (k, v) -> pp "emptybigmap(%a, %a)" pp_type k pp_type v
  | Ztotalvotingpower   -> pp "totalvotingpower"
  | Zlevel              -> pp "level"
  | Zmin_block_time     -> pp "min_block_time"
  | Zsapling_empty_state n -> pp "sapling_empty_state(%i)" n

let pp_uop f fmt (op, e) =
  let pp s = Format.fprintf fmt s in
  match op with
  | Ucar        -> pp "car(%a)"          f e
  | Ucdr        -> pp "cdr(%a)"          f e
  | Uleft  t    -> pp "left<%a>(%a)"     pp_type t f e
  | Uright t    -> pp "right<%a>(%a)"    pp_type t f e
  | Uneg        -> pp "neg(%a)"          f e
  | Unat        -> pp "nat(%a)"          f e
  | Uint        -> pp "int(%a)"          f e
  | Ubytes      -> pp "bytes(%a)"        f e
  | Unot        -> pp "not(%a)"          f e
  | Uabs        -> pp "abs(%a)"          f e
  | Uisnat      -> pp "isnat(%a)"        f e
  | Usome       -> pp "some(%a)"         f e
  | Usize       -> pp "size(%a)"         f e
  | Upack       -> pp "pack(%a)"         f e
  | Uunpack t   -> pp "unpack<%a>(%a)"   pp_type t f e
  | Ublake2b    -> pp "blake2b(%a)"      f e
  | Usha256     -> pp "sha256(%a)"       f e
  | Usha512     -> pp "sha512(%a)"       f e
  | Usha3       -> pp "sha3(%a)"         f e
  | Ukeccak     -> pp "keccak(%a)"       f e
  | Uhash_key   -> pp "hash_key(%a)"     f e
  | Ufail       -> pp "fail(%a)"         f e
  | Ucontract (t, a) -> pp "contract%a<%a>(%a)" (pp_option (fun fmt x -> Format.fprintf fmt "%%%a" pp_id x)) a pp_type t f e
  | Usetdelegate     -> pp "setdelegate(%a)" f e
  | Uimplicitaccount -> pp "implicitaccount(%a)" f e
  | Ueq        -> pp "eq(%a)"        f e
  | Une        -> pp "ne(%a)"        f e
  | Ugt        -> pp "gt(%a)"        f e
  | Uge        -> pp "ge(%a)"        f e
  | Ult        -> pp "lt(%a)"        f e
  | Ule        -> pp "le(%a)"        f e
  | Uvotingpower -> pp "votingpower(%a)" f e
  | Ureadticket  -> pp "read_ticket(%a)" f e
  | Ujointickets -> pp "join_tickets(%a)" f e
  | Upairing_check -> pp "pairing_check"
  | Uconcat      -> pp "concat"
  | Uaddress     -> pp "address"
  | UcarN n      -> pp "carN(%i,%a)" n f e
  | UcdrN n      -> pp "carN(%i,%a)" n f e
  | UforcePair   -> pp "pair"
  | Uemit (t, a) -> pp "emit%a<%a>(%a)"  (pp_option (fun fmt x -> Format.fprintf fmt "%%%a" pp_id x)) a pp_type t f e

let pp_bop f fmt (op, lhs, rhs) =
  let pp s = Format.fprintf fmt s in
  match op with
  | Badd       -> pp "(%a) + (%a)"       f lhs f rhs
  | Bsub       -> pp "(%a) - (%a)"       f lhs f rhs
  | Bmul       -> pp "(%a) * (%a)"       f lhs f rhs
  | Bediv      -> pp "(%a) / (%a)"       f lhs f rhs
  | Blsl       -> pp "(%a) << (%a)"      f lhs f rhs
  | Blsr       -> pp "(%a) >> (%a)"      f lhs f rhs
  | Bor        -> pp "(%a) or (%a)"      f lhs f rhs
  | Band       -> pp "(%a) and (%a)"     f lhs f rhs
  | Bxor       -> pp "(%a) xor (%a)"     f lhs f rhs
  | Bcompare   -> pp "compare (%a, %a)"  f lhs f rhs
  | Bget       -> pp "get(%a, %a)"       f lhs f rhs
  | Bmem       -> pp "mem(%a, %a)"       f lhs f rhs
  | Bconcat    -> pp "concat(%a, %a)"    f lhs f rhs
  | Bcons      -> pp "cons(%a, %a)"      f lhs f rhs
  | Bpair      -> pp "pair(%a, %a)"      f lhs f rhs
  | Bexec      -> pp "exec(%a, %a)"      f lhs f rhs
  | Bapply     -> pp "apply(%a, %a)"     f lhs f rhs
  | Bcreateticket -> pp "create_tickets(%a, %a)" f lhs f rhs
  | Bsplitticket  -> pp "split_ticket(%a, %a)"   f lhs f rhs
  | Bsapling_verify_update -> pp "sapling_verify_update"
  | Bview (_, _) -> pp ""
  | Bsubmutez  -> pp "sub_mutez(%a, %a)" f lhs f rhs

let pp_top f fmt (op, a1, a2, a3) =
  let pp s = Format.fprintf fmt s in
  match op with
  | Tcheck_signature   -> pp "check_signature(%a, %a, %a)" f a1 f a2 f a3
  | Tslice             -> pp "slice(%a, %a, %a)"           f a1 f a2 f a3
  | Tupdate            -> pp "update(%a, %a, %a)"          f a1 f a2 f a3
  | Ttransfer_tokens   -> pp "transfer_tokens(%a, %a, %a)" f a1 f a2 f a3
  | Topen_chest        -> pp "open_chest(%a, %a, %a)"      f a1 f a2 f a3
  | Tcreate_contract _ -> pp "create_contract(%a, %a, %a)" f a1 f a2 f a3

let rec pp_instruction fmt (i : instruction) =
  let pp s = Format.fprintf fmt s in
  let f = pp_instruction in
  match i with
  | Iseq [] -> pp "{ }"
  | Iseq l -> (pp_list ";@\n" f) fmt l
  | IletIn (id, v, b, _) -> Format.fprintf fmt "let %a = %a in@\n  @[%a@]" pp_id id f v f b
  | Ivar_access av -> Format.fprintf fmt "%s%a" av.av_ident (pp_list "" (fun fmt x -> Format.fprintf fmt "[%d/%d]" x.ai_index x.ai_length)) av.av_path
  | Icall (id, args, _)        -> Format.fprintf fmt "%a(%a)" pp_id id (pp_list ", " f) args
  | Iassign (id, v)            -> Format.fprintf fmt "%a := @[%a@]" pp_id id f v
  | Iassigntuple (id, i, l, v) -> Format.fprintf fmt "%a[%d/%d] := @[%a@]" pp_id id i l f v
  | Iif (c, t, e, _)           -> pp "if (%a)@\nthen @[%a@]@\nelse @[%a@]" f c f t f e
  | Iifnone (v, t, id, s, _)   -> pp "if_none (%a)@\nthen @[%a@]@\nelse @[fun %s -> %a@]" f v f t id f s
  | Iifleft (v, _, l, _, r, _) -> pp "if_left (%a)@\nthen @[%a@]@\nelse @[%a@]" f v f l f r
  | Iifcons (v, _, _, t, e, _) -> pp "if_cons (%a)@\nthen @[%a@]@\nelse @[%a@]" f v f t f e
  | Iloop (c, b)               -> pp "loop (%a) do@\n  @[%a@]@\ndone" f c f b
  | Iiter (ids, c, b)          -> pp "iter %a on (%a) do@\n  @[%a@]@\ndone" (pp_list ", " pp_id) ids f c f b
  | Iloopleft (l, _, b)        -> pp "@[loop_left (%a) do@\n  @[%a@]@\ndone@]" f l f b
  | Ilambda (rt, id, at, e)    -> pp "lambda<%a>((%s : %a) -> %a)" pp_type rt id pp_type at f e
  | Ilambda_michelson (it, rt, body) -> pp "lambda_michelson<%a, %a>(@[%a@])" pp_type it pp_type rt Micheline_printer.print_expr (Micheline_tools.obj_to_micheline body)
  | Izop op -> begin
      match op with
      | Znow                -> pp "now"
      | Zamount             -> pp "amount"
      | Zbalance            -> pp "balance"
      | Zsource             -> pp "source"
      | Zsender             -> pp "sender"
      | Zaddress            -> pp "address"
      | Zchain_id           -> pp "chain_id"
      | Zself _             -> pp "self"
      | Zself_address       -> pp "self_address"
      | Znone t             -> pp "none(%a)" pp_type t
      | Zunit               -> pp "unit"
      | Znil t              -> pp "nil(%a)" pp_type t
      | Zemptyset t         -> pp "emptyset(%a)" pp_type t
      | Zemptymap (k, v)    -> pp "emptymap(%a, %a)" pp_type k pp_type v
      | Zemptybigmap (k, v) -> pp "emptybigmap(%a, %a)" pp_type k pp_type v
      | Ztotalvotingpower   -> pp "totalvotingpower"
      | Zlevel              -> pp "level"
      | Zmin_block_time     -> pp "min_block_time"
      | Zsapling_empty_state n -> pp "sapling_empty_state(%i)" n
    end
  | Iunop (op, e) -> begin
      match op with
      | Ucar        -> pp "car(%a)"          f e
      | Ucdr        -> pp "cdr(%a)"          f e
      | Uleft  t    -> pp "left<%a>(%a)"     pp_type t f e
      | Uright t    -> pp "right<%a>(%a)"    pp_type t f e
      | Uneg        -> pp "neg(%a)"          f e
      | Unat        -> pp "nat(%a)"          f e
      | Uint        -> pp "int(%a)"          f e
      | Ubytes      -> pp "bytes(%a)"        f e
      | Unot        -> pp "not(%a)"          f e
      | Uabs        -> pp "abs(%a)"          f e
      | Uisnat      -> pp "is_nat(%a)"       f e
      | Usome       -> pp "some(%a)"         f e
      | Usize       -> pp "size(%a)"         f e
      | Upack       -> pp "pack(%a)"         f e
      | Uunpack t   -> pp "unpack<%a>(%a)"   pp_type t f e
      | Ublake2b    -> pp "blake2b(%a)"      f e
      | Usha256     -> pp "sha256(%a)"       f e
      | Usha512     -> pp "sha512(%a)"       f e
      | Usha3       -> pp "sha3(%a)"         f e
      | Ukeccak     -> pp "keccak(%a)"       f e
      | Uhash_key   -> pp "hash_key(%a)"     f e
      | Ufail       -> pp "fail(%a)"         f e
      | Ucontract (t, a) -> pp "contract%a<%a>(%a)" (pp_option (fun fmt x -> Format.fprintf fmt "%%%a" pp_id x)) a pp_type t f e
      | Usetdelegate     -> pp "setdelegate(%a)" f e
      | Uimplicitaccount -> pp "implicitaccount(%a)" f e
      | Ueq        -> pp "eq(%a)"        f e
      | Une        -> pp "ne(%a)"        f e
      | Ugt        -> pp "gt(%a)"        f e
      | Uge        -> pp "ge(%a)"        f e
      | Ult        -> pp "lt(%a)"        f e
      | Ule        -> pp "le(%a)"        f e
      | Uvotingpower -> pp "voting_power(%a)" f e
      | Ureadticket  -> pp "read_ticket(%a)"  f e
      | Ujointickets -> pp "join_tickets(%a)" f e
      | Upairing_check -> pp "pairing_check"
      | Uconcat      -> pp "concat"
      | Uaddress     -> pp "address"
      | UcarN n      -> pp "car(%i, %a)"      n f e
      | UcdrN n      -> pp "cdr(%i, %a)"      n f e
      | UforcePair   -> pp "pair(%a)"         f e
      | Uemit (t, a) -> pp "emit%a<%a>(%a)"  (pp_option (fun fmt x -> Format.fprintf fmt "%%%a" pp_id x)) a pp_type t f e
    end
  | Ibinop (op, lhs, rhs) -> begin
      match op with
      | Badd       -> pp "(%a) + (%a)"       f lhs f rhs
      | Bsub       -> pp "(%a) - (%a)"       f lhs f rhs
      | Bmul       -> pp "(%a) * (%a)"       f lhs f rhs
      | Bediv      -> pp "(%a) / (%a)"       f lhs f rhs
      | Blsl       -> pp "(%a) << (%a)"      f lhs f rhs
      | Blsr       -> pp "(%a) >> (%a)"      f lhs f rhs
      | Bor        -> pp "(%a) or (%a)"      f lhs f rhs
      | Band       -> pp "(%a) and (%a)"     f lhs f rhs
      | Bxor       -> pp "(%a) xor (%a)"     f lhs f rhs
      | Bcompare   -> pp "compare (%a, %a)"  f lhs f rhs
      | Bget       -> pp "get(%a, %a)"       f lhs f rhs
      | Bmem       -> pp "mem(%a, %a)"       f lhs f rhs
      | Bconcat    -> pp "concat(%a, %a)"    f lhs f rhs
      | Bcons      -> pp "cons(%a, %a)"      f lhs f rhs
      | Bpair      -> pp "pair(%a, %a)"      f lhs f rhs
      | Bexec      -> pp "exec(%a, %a)"      f lhs f rhs
      | Bapply     -> pp "apply(%a, %a)"     f lhs f rhs
      | Bcreateticket -> pp "create_ticket(%a, %a)" f lhs f rhs
      | Bsplitticket  -> pp "split_ticket(%a, %a)"  f lhs f rhs
      | Bsapling_verify_update -> pp "sapling_verify_update"
      | Bview (c, t)  -> pp "view<%a, %s>(%a, %a)" pp_type t c f lhs f rhs
      | Bsubmutez  -> pp "sub_mutez(%a, %a)" f lhs f rhs
    end
  | Iterop (op, a1, a2, a3) -> begin
      match op with
      | Tcheck_signature   -> pp "check_signature(%a, %a, %a)" f a1 f a2 f a3
      | Tslice             -> pp "slice(%a, %a, %a)"           f a1 f a2 f a3
      | Tupdate            -> pp "update(%a, %a, %a)"          f a1 f a2 f a3
      | Ttransfer_tokens   -> pp "transfer_tokens(%a, %a, %a)" f a1 f a2 f a3
      | Topen_chest        -> pp "open_chest(%a, %a, %a)"      f a1 f a2 f a3
      | Tcreate_contract _ -> pp "create_contract(%a, %a, %a)" f a1 f a2 f a3
    end
  | Iupdate (_a, _b) -> begin
      pp "update"
    end
  | Icompare (op, lhs, rhs) -> begin
      match op with
      | Ceq        -> pp "(%a) = (%a)"       f lhs f rhs
      | Cne        -> pp "(%a) <> (%a)"      f lhs f rhs
      | Clt        -> pp "(%a) < (%a)"       f lhs f rhs
      | Cgt        -> pp "(%a) > (%a)"       f lhs f rhs
      | Cle        -> pp "(%a) <= (%a)"      f lhs f rhs
      | Cge        -> pp "(%a) >= (%a)"      f lhs f rhs
    end
  | Iconst (t, e)           -> pp "const(%a : %a)" pp_data e pp_type t
  | Iset (t, l)             -> pp "set<%a>[%a]" pp_type t (pp_list "; " f) l
  | Ilist (t, l)            -> pp "list<%a>[%a]" pp_type t (pp_list "; " f) l
  | Imap (b, k, v, l)       -> pp "%smap<%a, %a>[%a]" (if b then "big_" else "") pp_type k pp_type v (pp_list "; " (fun fmt (vk, vv) -> Format.fprintf fmt "%a : %a" f vk f vv)) l
  | Irecord ri              -> pp "record%a" pp_ritem ri
  | Irecupdate (x, r)       -> pp "recupdate[%a with [@[%a@]]]" f x pp_ruitem r
  | Ifold (ix, iy, ia, c, a, b) -> pp "fold %a with (%a) do (%s, %a) ->@\n  @[%a@]@\ndone" f c f a ia (fun fmt _-> match iy with | Some iy -> Format.fprintf fmt "(%s, %s)" ix iy  | None -> Format.fprintf fmt "%s" ix) () f b
  | Imap_ (x, id, e)        -> pp "map(%a, %s -> @[%a@])" f x id f e
  | Ireverse (t, x)         -> pp "reverse<%a>(%a)" pp_type t f x
  | Imichelson (a, c, v)    -> pp "michelson [%a] (%a) {%a}" (pp_list "; " pp_id) v (pp_list "; " f) a pp_code c
  | Iwildcard (_, id)       -> pp "$$%s$$" id
  | Ireplace (id, v, _, fa) -> Format.fprintf fmt "replace %s, %s : %a" id v f fa
  | Ireadticket (x)         -> pp "read_ticket(%a)" f x
  | Imicheline (micheline, ts, args) -> begin
      pp "micheline<[%a]> @[%a@] [%a]"
        (pp_list "; " pp_type) ts
        Micheline_printer.print_expr (Micheline_tools.obj_to_micheline micheline)
        (pp_list "; " f) args
    end

and pp_ritem fmt = function
  | Rtuple l -> Format.fprintf fmt "[%a]" (pp_list "; " pp_instruction) l
  | Rnodes l -> Format.fprintf fmt "[%a]" (pp_list "; " pp_ritem) l

and pp_ruitem fmt = function
  | RUnodes  (s, l) -> Format.fprintf fmt "run|s:%i|@[%a@]" s (pp_list "@\n" (fun fmt (i, v) -> Format.fprintf fmt "%i = %a" i pp_ruitem v)) l
  | RUassign (s, l) -> Format.fprintf fmt "rua|s:%i|@[%a@]" s (pp_list "@\n" (fun fmt (i, v) -> Format.fprintf fmt "%i = %a" i pp_instruction v)) l

let pp_func fmt (f : func) =
  Format.fprintf fmt "function %s %a@\n "
    f.name
    (fun fmt x ->
       match x with
       | Concrete (args, body) ->
         Format.fprintf fmt "(%a) : %a {@\n  @[%a@]@\n}"
           (pp_list ", " (fun fmt (id, t) ->
                Format.fprintf fmt "%s : %a" id pp_type t)) args
           pp_type f.tret
           pp_instruction body
       | Abstract _ ->
         Format.fprintf fmt "(%a) : %a = abstract" pp_type f.targ pp_type f.tret
    ) f.body

let pp_entry fmt (e : entry) =
  Format.fprintf fmt "entry %s (%a) {@\n  @[%a@]@\n}@\n "
    e.name
    (pp_list ", " (fun fmt (id, t) -> Format.fprintf fmt "%s : %a" id pp_type t)) e.args
    pp_instruction e.body

let pp_ir fmt (ir : ir) =
  let pp a = Format.fprintf fmt a in
  Format.fprintf fmt "storage_type: %a@\n@\n" pp_type ir.storage_type;
  Format.fprintf fmt "storage_data: %a@\n@\n" pp_data ir.storage_data;
  (pp_list "@\n@\n" pp_func) fmt ir.funs;
  (pp_list "@\n@\n" pp_func) fmt ir.views;
  (if (List.is_not_empty ir.views) then (pp "@\n"))

let pp_view_struct fmt (vs : view_struct) =
  Format.fprintf fmt "@\n  view@\n    \"%s\"@\n    %a@\n    %a@\n    @[%a@]"
    vs.id
    pp_type vs.param
    pp_type vs.ret
    pp_code vs.body

let pp_michelson fmt (m : michelson) =
  Format.fprintf fmt
    "{@\n  \
     storage %a;@\n  \
     parameter %a;@\n  \
     code %a;%a\
     @\n}"
    pp_type m.storage
    pp_type m.parameter
    pp_code m.code
    (pp_list ";" pp_view_struct) m.views
(* -------------------------------------------------------------------------- *)

let pp_a fmt (tag, value) =
  Format.fprintf fmt "\"%s\": \"%s\"" tag value

let pp_b fmt (tag, value) =
  Format.fprintf fmt "\"%s\": %s" tag value

let pp_prim fmt pp (p : prim) =
  if List.is_empty p.args && List.is_empty p.annots
  then Format.fprintf fmt "{  %a  }" pp_a ("prim", p.prim)
  else
    let ppf fmt (l, tag, pp, nl) =
      if not (List.is_empty l)
      then begin
        Format.fprintf fmt ",@\n\"%s\": [@\n@[  @[%a@]@]@\n]" tag (pp_list ",@\n" pp) l;
        if nl
        then Format.fprintf fmt "@\n"
      end
    in
    Format.fprintf fmt "{  @[%a%a%a@]@\n}"
      pp_a ("prim", p.prim)
      ppf (p.args, "args", pp, not (List.is_empty p.annots))
      ppf (p.annots, "annots", (fun fmt s -> Format.fprintf fmt "\"%s\"" s), false)

let rec pp_obj_micheline ?(var_dynamic = false) fmt (o : obj_micheline) =
  let f = (pp_obj_micheline ~var_dynamic) in
  let pp x = Format.fprintf fmt "{  %a  }" x in
  match o with
  | Oprim   p -> pp_prim fmt f p
  | Ostring v -> pp pp_a ("string", String.escaped v)
  | Obytes  v -> pp pp_a ("bytes", v)
  | Oint    v -> pp pp_a ("int", v)
  | Oarray  l -> Format.fprintf fmt "[  %a  ]" (pp_list ",@\n" f) l
  | Ovar    x -> begin
      if var_dynamic
      then begin
        match x with
        | OMVfree   x -> Format.fprintf fmt "%s" x
        | OMVint    (x, b) -> Format.fprintf fmt "{\"int\": %s%s}" x (if b then "" else ".toString()")
        | OMVstring x -> pp pp_b ("string", x)
        | OMVbytes  x -> pp pp_b ("bytes", x)
        | OMVif (x, a, b) -> Format.fprintf fmt "(%s ? %a : %a)" x f a f b
      end
      else
        let id =
          match x with
          | OMVfree   x -> x
          | OMVint    (x, _) -> x
          | OMVstring x -> x
          | OMVbytes  x -> x
          | OMVif (x, _a, _b) -> x
        in
        pp pp_a ("var", id)
    end

(* let rec pp_raw_prim fmt (p : prim) =
   let pp_space pp fmt l = if List.is_empty l then () else Format.fprintf fmt " %a" pp l in
   let pps fmt str = Format.fprintf fmt "%s" str in
   Format.fprintf fmt "%s%a%a"
    p.prim
    (pp_space (pp_list " " pps)) p.annots
    (pp_space (fun fmt (args : obj_micheline list) ->
         match args with
         | []  -> ()
         | [x] -> pp_raw_obj_micheline fmt x
         | xs  -> (pp_paren (pp_list " " pp_raw_obj_micheline)) fmt xs)) p.args

   and pp_raw_obj_micheline fmt (o : obj_micheline) =
   let pp a = Format.fprintf fmt a in
   match o with
   | Oprim   p -> pp_raw_prim fmt p
   | Ostring v -> pp "%s" v
   | Obytes  v -> pp "%s" v
   | Oint    v -> pp "%s" v
   | Oarray  l -> Format.fprintf fmt "{ %a }" (pp_list "; " pp_obj_micheline) l *)

let pp_micheline fmt (m : micheline) =
  Format.fprintf fmt
    "{@\n  \
     \"code\":@\n    [  @[%a@]  ],@\n  \
     \"storage\":@\n    @[%a@]@\n\
     }"
    (pp_list ",@\n" pp_obj_micheline) m.code
    (pp_obj_micheline ~var_dynamic:false) m.storage

(* -------------------------------------------------------------------------- *)

let rec pp_dcode (fmt : Format.formatter) (c : dcode) =
  match c with
  | [] ->
    Format.fprintf fmt "%s" "pass"

  | _ ->
    Format.fprintf fmt "%a"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n") pp_dinstr)
      c

and pp_dinstr (fmt : Format.formatter) (i : dinstr) =
  match i with
  | DIAssign (x, e) ->
    Format.fprintf fmt "%a <- %a" pp_var x pp_expr e

  | DIIf (c, (b1, b2)) ->
    Format.fprintf fmt "@[<v 2>if (%a):@\n%a@]@\n@[<v 2>else:@\n%a@]"
      pp_expr c pp_dcode b1 pp_dcode b2

  | DIMatch (c, bs) ->
    let rec pp_pattern fmt = function
      | DVar x ->
        Format.fprintf fmt "%a" pp_var (`VLocal x)
      | DPair (p1, p2) ->
        Format.fprintf fmt "(%a, %a)" pp_pattern p1 pp_pattern p2 in

    let pp_branch fmt (c, ptn, body)  =
      Format.fprintf fmt "@[<v 2>%s %a =>@\n%a@]" c
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt " ")
           pp_pattern) ptn
        pp_dcode body
    in

    Format.fprintf fmt "match (%a) with@\n%a@\nend" pp_expr c
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt "@\n")
         pp_branch) bs

  | DIFailwith e ->
    Format.fprintf fmt "failwith (%a)" pp_expr e

  | DIWhile (c, b) ->
    Format.fprintf fmt "@[<v 2>while (%a):@\n%a@]"
      pp_expr c pp_dcode b

  | DIIter (i, c, b) ->
    Format.fprintf fmt "@[<v 2>iter %a in %a:@\n%a@]"
      pp_var i pp_expr c pp_dcode b

  | DILoop (i, b) ->
    Format.fprintf fmt "@[<v 2>loop %a:@\n%a@]"
      pp_var i pp_dcode b

and pp_expr (fmt : Format.formatter) (e : dexpr) =
  let f = pp_expr in
  match e.node with
  | Dvar v              -> pp_var fmt v
  | Depair (e1, e2)     -> Format.fprintf fmt "(%a, %a)" pp_expr e1 pp_expr e2
  | Ddata (_, d)        -> pp_data fmt d
  | Dfun (op, args)     -> begin
      match op, args with
      | `Zop op, []         -> pp_zop fmt op
      | `Uop op, [a      ]  -> pp_uop f fmt (op, a)
      | `Bop op, [a; b   ]  -> pp_bop f fmt (op, a, b)
      | `Top op, [a; b; c]  -> pp_top f fmt (op, a, b, c)
      | _ -> assert false
    end

and pp_var (fmt : Format.formatter) (v : dvar) =
  match v with
  | `VGlobal n ->
    Format.fprintf fmt "%s" n

  | `VLocal x ->
    Format.fprintf fmt "#%d" x

(* let pp_dview fmt (dv : dview) : unit =
   Format.fprintf fmt "view@\n  ident=\"%s\"@\n  param=%a@\n  ret=%a@\n  body=@\n    @[%a@]@\n"
   dv.id
   pp_type dv.param
   pp_type dv.ret
   (pp_list ";@\n" pp_dinstruction) dv.body *)

let pp_dprogram fmt (d : dprogram) : unit =
  Format.fprintf fmt
    "{@\n  name: %s@\n  \
     storage: %a@\n  \
     parameter: %a@\n  \
     storage_data: %a@\n  \
     code:@\n    @[%a@]@\n}"
    d.name
    pp_type d.storage
    pp_type d.parameter
    pp_data d.storage_data
    pp_dcode d.code

(* -------------------------------------------------------------------------- *)

let pp_javascript_header fmt _ =
  Format.fprintf fmt "\
  /* Utils functions */@\n@\n\
  export const mk_int    = v  => {return { \"int\" : v.toString() }}@\n\
  export const mk_string = v  => {return { \"string\" : v }}@\n\
  export const mk_bytes  = v  => {return { \"bytes\" : v.toString() }}@\n\
  export const mk_some   = v  => {return { \"prim\": \"Some\", \"args\": [ v ] }}@\n\
  export const mk_none   = () => {return { \"prim\": \"None\" }}@\n\
  export const mk_rational = (n, d) => {return {  \"prim\": \"Pair\", \"args\": [ {  \"int\": n.toString()  }, {  \"int\": d.toString()  } ] }}@\n@\n"

let pp_javascript_content fmt (micheline : Michelson.micheline) =
  let code : obj_micheline = Michelson.Oarray (micheline.code @ micheline.views) in
  let storage : obj_micheline = micheline.storage in
  let parameters = micheline.parameters in
  Format.fprintf fmt "\
  /* Code */@\n@\n\
  export const code =@\n  @[%a@];@\n@\n\
  export const getStorage = (@[%a@]) => {@\n\  return @[%a@];@\n\  }@\n"
    (pp_obj_micheline ~var_dynamic:false) code
    (pp_list ", " pp_ident) parameters
    (pp_obj_micheline ~var_dynamic:true) storage

let pp_javascript fmt (micheline : Michelson.micheline) =
  Format.fprintf fmt "/* Javascript output generated by %a */@\n@\n" pp_bin ();
  if not !Options.opt_no_js_header then pp_javascript_header fmt ();
  pp_javascript_content fmt micheline

(* -------------------------------------------------------------------------- *)

let pp_opt_pp tag pp fmt o =
  pp_option (fun fmt v -> Format.fprintf fmt "\"%s\":%a," tag pp v) fmt o

let pp_michelson_storage_view_struct fmt (omsvsv : Michelson.michelson_storage_view_struct) =
  pp_str fmt "{";
  (pp_opt_pp "parameter" pp_obj_micheline) fmt omsvsv.parameter;
  (pp_opt_pp "returnType" pp_obj_micheline) fmt omsvsv.returnType;
  (pp_opt_pp "version" pp_str) fmt omsvsv.version;
  Format.fprintf fmt "\"code\":%a" (pp_obj_micheline ~var_dynamic:false) omsvsv.code;
  pp_str fmt "}"

let pp_offchain_view_implem_kind fmt (o : offchain_view_implem_kind) =
  match o with
  | OVIKMichelsonStorageView v -> Format.fprintf fmt "{\"michelsonStorageView\":%a}" pp_michelson_storage_view_struct v
  | OVIKRestApiQuery _ -> ()

let pp_offchain_view fmt (ov : Michelson.offchain_view) =
  Format.fprintf fmt
    "{\"name\":\"%s\",\"implementations\":[%a]}"
    ov.name
    (pp_list "," pp_offchain_view_implem_kind) ov.implementations

(* -------------------------------------------------------------------------- *)

let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_pretty_type x = string_of__of_pp pp_pretty_type x
let show_model x = string_of__of_pp pp_michelson x
