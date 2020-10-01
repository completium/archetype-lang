open Tools
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
  | Tkey                 -> pp_simple_a "key"
  | Tunit                -> pp_simple_a "unit"
  | Tsignature           -> pp_simple_a "signature"
  | Toption    t         -> Format.fprintf fmt "(option%a %a)"     pp_annot_opt () pp_type t
  | Tlist      t         -> Format.fprintf fmt "(list%a %a)"       pp_annot_opt () pp_type t
  | Tset       t         -> Format.fprintf fmt "(set%a %a)"        pp_annot_opt () pp_type t
  | Toperation           -> pp_simple_a "operation"
  | Tcontract  t         -> Format.fprintf fmt "(contract%a %a)"   pp_annot_opt () pp_type t
  | Tpair      (lt, rt)  -> Format.fprintf fmt "(pair%a %a %a)"    pp_annot_opt () pp_type lt  pp_type rt
  | Tor        (lt, rt)  -> Format.fprintf fmt "(or%a %a %a)"      pp_annot_opt () pp_type lt  pp_type rt
  | Tlambda    (at, rt)  -> Format.fprintf fmt "(lambda%a %a %a)"  pp_annot_opt () pp_type at  pp_type rt
  | Tmap       (kt, vt)  -> Format.fprintf fmt "(map%a %a %a)"     pp_annot_opt () pp_type kt  pp_type vt
  | Tbig_map   (kt, vt)  -> Format.fprintf fmt "(big_map%a %a %a)" pp_annot_opt () pp_type kt  pp_type vt
  | Tchain_id            -> pp_simple_a "chain_id"
  | Tint                 -> pp_simple_a "int"
  | Tnat                 -> pp_simple_a "nat"
  | Tstring              -> pp_simple_a "string"
  | Tbytes               -> pp_simple_a "bytes"
  | Tmutez               -> pp_simple_a "mutez"
  | Tbool                -> pp_simple_a "bool"
  | Tkey_hash            -> pp_simple_a "key_hash"
  | Ttimestamp           -> pp_simple_a "timestamp"
  | Taddress             -> pp_simple_a "address"
  | Tsapling_transaction -> pp_simple_a "sapling_transaction"
  | Tsapling_state       -> pp_simple_a "sapling_state"
  | Tnever               -> pp_simple_a "never"
  | Tbls12_381_g1        -> pp_simple_a "bls12_381_g1"
  | Tbls12_381_g2        -> pp_simple_a "bls12_381_g2"
  | Tbls12_381_fr        -> pp_simple_a "bls12_381_fr"
  | Tbaker_hash          -> pp_simple_a "baker_hash"
  | Tbaker_operation     -> pp_simple_a "baker_operation"
  | Tpvss_key            -> pp_simple_a "pvss_key"

let rec pp_pretty_type fmt (t : type_) =
  match t.node with
  | Toption    t        -> Format.fprintf fmt "option_%a"     pp_pretty_type t
  | Tlist      t        -> Format.fprintf fmt "list_%a"       pp_pretty_type t
  | Tset       t        -> Format.fprintf fmt "set_%a"        pp_pretty_type t
  | Tcontract  t        -> Format.fprintf fmt "contract_%a"   pp_pretty_type t
  | Tpair      (lt, rt) -> Format.fprintf fmt "pair_%a_%a"    pp_pretty_type lt  pp_pretty_type rt
  | Tor        (lt, rt) -> Format.fprintf fmt "or_%a_%a"      pp_pretty_type lt  pp_pretty_type rt
  | Tlambda    (at, rt) -> Format.fprintf fmt "lambda_%a_%a"  pp_pretty_type at  pp_pretty_type rt
  | Tmap       (kt, vt) -> Format.fprintf fmt "map_%a_%a"     pp_pretty_type kt  pp_pretty_type vt
  | Tbig_map   (kt, vt) -> Format.fprintf fmt "big_map_%a_%a" pp_pretty_type kt  pp_pretty_type vt
  | _ -> pp_type fmt t

let rec pp_data fmt (d : data) =
  let pp s = Format.fprintf fmt s in
  match d with
  | Dint    v       -> pp_big_int fmt v
  | Dstring v       -> pp "\"%s\"" v
  | Dbytes  v       -> pp "0x%s"     v
  | Dunit           -> pp "Unit"
  | Dtrue           -> pp "True"
  | Dfalse          -> pp "False"
  | Dpair  (ld, rd) -> pp "(Pair %a %a)" pp_data ld pp_data rd
  | Dleft   d       -> pp "(Left %a)"    pp_data d
  | Dright  d       -> pp "(Right %a)"   pp_data d
  | Dsome   d       -> pp "(Some %a)"    pp_data d
  | Dnone           -> pp "None"
  | Dlist l         -> pp "{ %a }" (pp_list "; " pp_data) l
  | Delt (x, y)     -> pp "Elt %a %a" pp_data x pp_data y

let rec pp_code fmt (i : code) =
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
  let fs fmt = Format.fprintf fmt "{ @[%a@] }" (pp_list ";@\n" pp_code) in
  let fsl fmt = Format.fprintf fmt "{ %a }" (pp_list "; " pp_code) in
  match i with
  (* Control structures *)
  | SEQ l                    -> fs fmt l
  | APPLY                    -> pp "APPLY"
  | EXEC                     -> pp "EXEC"
  | FAILWITH                 -> pp "FAILWITH"
  | IF (ti, ei)              -> pp "IF@\n  @[%a@]@\n  @[%a@]" fs ti fs ei
  | IF_CONS (ti, ei)         -> pp "IF_CONS@\n  @[%a@]@\n  @[%a@]" fs ti fs ei
  | IF_LEFT (ti, ei)         -> pp "IF_LEFT@\n  @[%a@]@\n  @[%a@]" fs ti fs ei
  | IF_NONE (ti, ei)         -> pp "IF_NONE@\n  @[%a@]@\n  @[%a@]" fs ti fs ei
  | ITER is                  -> pp "ITER %a" fs is
  | LAMBDA (at, rt, is)      -> pp "LAMBDA@\n  @[%a@]@\n  @[%a@]@\n  @[%a@]" pp_type at pp_type rt fs is
  | LOOP is                  -> pp "LOOP %a" fs is
  | LOOP_LEFT is             -> pp "LOOP_LEFT %a" fs is
  (* Stack manipulation *)
  | DIG i                    -> pp "DIG%a" pp_arg2 i
  | DIP (i, is)              -> pp "DIP%a %a" pp_arg i fsl is
  | DROP i                   -> pp "DROP%a" pp_arg i
  | DUG i                    -> pp "DUG%a" pp_arg2 i
  | DUP                      -> pp "DUP"
  | PUSH (t, d)              -> pp "PUSH %a %a" pp_type t pp_data d
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
  | CONTRACT (t, a)          -> pp "CONTRACT%a %a" pp_annot a pp_type t
  | CREATE_CONTRACT  is      -> pp "CREATE_CONTRACT %a" fs is
  | IMPLICIT_ACCOUNT         -> pp "IMPLICIT_ACCOUNT"
  | NOW                      -> pp "NOW"
  | SELF                     -> pp "SELF"
  | SENDER                   -> pp "SENDER"
  | SET_DELEGATE             -> pp "SET_DELEGATE"
  | SOURCE                   -> pp "SOURCE"
  | TRANSFER_TOKENS          -> pp "TRANSFER_TOKENS"
  (* Operations on data structures *)
  | CAR                      -> pp "CAR"
  | CDR                      -> pp "CDR"
  | CONCAT                   -> pp "CONCAT"
  | CONS                     -> pp "CONS"
  | EMPTY_BIG_MAP (k, v)     -> pp "EMPTY_BIG_MAP %a %a" pp_type k pp_type v
  | EMPTY_MAP     (k, v)     -> pp "EMPTY_MAP %a %a" pp_type k pp_type v
  | EMPTY_SET     t          -> pp "EMPTY_SET %a" pp_type t
  | GET                      -> pp "GET"
  | LEFT  t                  -> pp "LEFT %a" pp_type t
  | MAP  is                  -> pp "MAP %a" fs is
  | MEM                      -> pp "MEM"
  | NIL t                    -> pp "NIL %a" pp_type t
  | NONE t                   -> pp "NONE %a" pp_type t
  | PACK                     -> pp "PACK"
  | PAIR                     -> pp "PAIR"
  | RIGHT t                  -> pp "RIGHT %a" pp_type t
  | SIZE                     -> pp "SIZE"
  | SLICE                    -> pp "SLICE"
  | SOME                     -> pp "SOME"
  | UNIT                     -> pp "UNIT"
  | UNPACK t                 -> pp "UNPACK %a" pp_type t
  | UPDATE                   -> pp "UPDATE"
  (* Other *)
  | UNPAIR                   -> pp "UNPAIR"
  | SELF_ADDRESS             -> pp "SELF_ADDRESS"
  | CAST                     -> pp "CAST"
  | CREATE_ACCOUNT           -> pp "CREATE_ACCOUNT"
  | RENAME                   -> pp "RENAME"
  | STEPS_TO_QUOTA           -> pp "STEPS_TO_QUOTA"
  | LEVEL                    -> pp "LEVEL"
  | SAPLING_EMPTY_STATE      -> pp "SAPLING_EMPTY_STATE"
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

let pp_id fmt i = Format.fprintf fmt "%s" i

let rec pp_instruction fmt (i : instruction) =
  let pp s = Format.fprintf fmt s in
  let f = pp_instruction in
  match i with
  | Iseq [] -> pp "{ }"
  | Iseq l -> (pp_list ";@\n" f) fmt l
  | IletIn (id, v, b, _) -> Format.fprintf fmt "let %a = %a in@\n  @[%a@]" pp_id id f v f b
  | Ivar id -> pp_id fmt id
  | Icall (id, args, _)      -> Format.fprintf fmt "%a(%a)" pp_id id (pp_list ", " f) args
  | Iassign (id, v)          -> Format.fprintf fmt "%a := @[%a@]" pp_id id f v
  | IassignRec (id, s, n, v) -> Format.fprintf fmt "%a[%i]/* size = %i */ := @[%a@]" pp_id id n s f v
  | Iif (c, t, e, _)         -> pp "if (%a)@\nthen @[%a@]@\nelse @[%a@]" f c f t f e
  | Iifnone (v, t, id, s)    -> pp "if_none (%a)@\nthen @[%a@]@\nelse @[fun %s -> %a@]" f v f t id f s
  | Iifcons (v, t, e)        -> pp "if_cons (%a)@\nthen @[%a@]@\nelse @[%a@]" f v f t f e
  | Iwhile (c, b)            -> pp "while (%a) do@\n  @[%a@]@\ndone" f c f b
  | Iiter (ids, c, b)        -> pp "iter %a on (%a) do@\n  @[%a@]@\ndone" (pp_list ", " pp_id) ids f c f b
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
    end
  | Iunop (op, e) -> begin
      match op with
      | Ucar        -> pp "car(%a)"          f e
      | Ucdr        -> pp "cdr(%a)"          f e
      | Uleft  t    -> pp "left<%a>(%a)"     pp_type t f e
      | Uright t    -> pp "right<%a>(%a)"    pp_type t f e
      | Uneg        -> pp "neg(%a)"          f e
      | Uint        -> pp "int(%a)"          f e
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
    end
  | Iterop (op, a1, a2, a3) -> begin
      match op with
      | Tcheck_signature -> pp "check_signature(%a, %a, %a)" f a1 f a2 f a3
      | Tslice           -> pp "slice(%a, %a, %a)"           f a1 f a2 f a3
      | Tupdate          -> pp "update(%a, %a, %a)"          f a1 f a2 f a3
      | Ttransfer_tokens -> pp "transfer_tokens(%a, %a, %a)" f a1 f a2 f a3
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
  | Imap (k, v, l)          -> pp "map<%a, %a>[%a]" pp_type k pp_type v (pp_list "; " (fun fmt (vk, vv) -> Format.fprintf fmt "%a : %a" f vk f vv)) l
  | Irecord l               -> pp "record[%a]" (pp_list "; " f) l
  | Irecupdate (x, s, l)    -> pp "recupdate[size=%i| %a with [@[%a@]]]" s f x (pp_list "; " (fun fmt (i, v) -> Format.fprintf fmt "%i: (%a)" i f v)) l
  | Ifold (ix, iy, ia, c, a, b) -> pp "fold %a with (%a) do (%s, %a) ->@\n  @[%a@]@\ndone" f c f a ia (fun fmt _-> match iy with | Some iy -> Format.fprintf fmt "(%s, %s)" ix iy  | None -> Format.fprintf fmt "%s" ix) () f b
  | Imichelson (a, c, v)    -> pp "michelson [%a] (%a) {%a}" (pp_list "; " pp_id) v (pp_list "; " f) a pp_code c

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
  (if (List.is_not_empty ir.funs) then (pp "@\n"));
  (pp_list "@\n@\n" pp_entry) fmt ir.entries

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

let rec pp_dexpr fmt (de : dexpr) =
  let pp x = Format.fprintf fmt x in
  let f = pp_dexpr in
  match de with
  | Dalpha n           -> pp "x%i" n
  | Dvar t             -> pp "var%a" (fun fmt -> (if Option.is_some t.annotation then pp_type fmt else (pp_paren pp_type) fmt)) t
  | Dstorage t         -> pp "storage(%a)" pp_type t
  | Doperations        -> pp "operations"
  | Ddata d            -> pp "data(%a)" pp_data d
  | Dzop op -> begin
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
    end
  | Duop (op, e) -> begin
      match op with
      | Ucar        -> pp "car(%a)"          f e
      | Ucdr        -> pp "cdr(%a)"          f e
      | Uleft  t    -> pp "left<%a>(%a)"     pp_type t f e
      | Uright t    -> pp "right<%a>(%a)"    pp_type t f e
      | Uneg        -> pp "neg(%a)"          f e
      | Uint        -> pp "int(%a)"          f e
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
    end
  | Dbop (op, lhs, rhs) -> begin
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
    end
  | Dtop (op, a1, a2, a3) -> begin
      match op with
      | Tcheck_signature -> pp "check_signature(%a, %a, %a)" f a1 f a2 f a3
      | Tslice           -> pp "slice(%a, %a, %a)"           f a1 f a2 f a3
      | Tupdate          -> pp "update(%a, %a, %a)"          f a1 f a2 f a3
      | Ttransfer_tokens -> pp "transfer_tokens(%a, %a, %a)" f a1 f a2 f a3
    end

let rec pp_dinstruction fmt i =
  let seq is = (pp_list ";@\n" pp_dinstruction) is in
  match i with
  | Dassign (a, b) -> Format.fprintf fmt "%a <- %a" pp_dexpr a pp_dexpr b
  | Dif (c, a, b)  -> Format.fprintf fmt "if (%a)@\nthen (@[%a@])@\nelse (@[%a@])" pp_dexpr c seq a seq b
  | Dfail e        -> Format.fprintf fmt "fail(%a)" pp_dexpr e
  | Ddecl id       -> Format.fprintf fmt "var x%i" id

let pp_sysofequations fmt (s : sysofequations) =
  (pp_list "@\n" pp_dinstruction) fmt s

let pp_dprogram fmt (d : dprogram) =
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
    (pp_list ";@\n" pp_dinstruction) d.code

(* -------------------------------------------------------------------------- *)

let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_pretty_type x = string_of__of_pp pp_pretty_type x
let show_model x = string_of__of_pp pp_michelson x
