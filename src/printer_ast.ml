open Tools
open Ast
open Printer_tools

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_longident fmt (id : longident) =
  Format.fprintf fmt "%s" (Location.unloc (snd id))

let pp_namespace fmt nm =
  Format.fprintf fmt "%s::" (Location.unloc nm)

let pp_longident fmt ((nm, id) : longident) =
  Format.fprintf fmt "%a%s" pp_namespace nm (Location.unloc id)

let pp_with_paren pp fmt =
  if (!Options.opt_all_parenthesis)
  then Format.fprintf fmt "(@[%a@])" pp
  else pp fmt

let pp_no_paren pp fmt = pp fmt

let pp_currency fmt = function
  | Utz  -> Format.fprintf fmt "utz"

let pp_vtyp fmt = function
  | VTunit         -> Format.fprintf fmt "unit"
  | VTbool         -> Format.fprintf fmt "bool"
  | VTnat          -> Format.fprintf fmt "nat"
  | VTint          -> Format.fprintf fmt "int"
  | VTrational     -> Format.fprintf fmt "rational"
  | VTdate         -> Format.fprintf fmt "date"
  | VTduration     -> Format.fprintf fmt "duration"
  | VTstring       -> Format.fprintf fmt "string"
  | VTaddress      -> Format.fprintf fmt "address"
  | VTcurrency     -> Format.fprintf fmt "tez"
  | VTsignature    -> Format.fprintf fmt "signature"
  | VTkey          -> Format.fprintf fmt "key"
  | VTkeyhash      -> Format.fprintf fmt "key_hash"
  | VTbytes        -> Format.fprintf fmt "bytes"
  | VTchainid      -> Format.fprintf fmt "chain_id"
  | VTbls12_381_fr -> Format.fprintf fmt "bls12_381_fr"
  | VTbls12_381_g1 -> Format.fprintf fmt "bls12_381_g1"
  | VTbls12_381_g2 -> Format.fprintf fmt "bls12_381_g2"
  | VTnever        -> Format.fprintf fmt "never"
  | VTchest        -> Format.fprintf fmt "chest"
  | VTchest_key    -> Format.fprintf fmt "chest_key"

let pp_container fmt = function
  | Collection     -> Format.fprintf fmt "collection"
  | Aggregate      -> Format.fprintf fmt "aggregate"
  | Partition      -> Format.fprintf fmt "partition"
  | AssetContainer -> Format.fprintf fmt "asset_container"
  | AssetKey       -> Format.fprintf fmt "asset_key"
  | AssetValue     -> Format.fprintf fmt "asset_value"
  | AssetView      -> Format.fprintf fmt "asset_view"

let rec pp_ptyp fmt (t : ptyp) =
  match t with
  | Tnamed i ->
    Format.fprintf fmt "#%d" i
  | Tasset an ->
    Format.fprintf fmt "%a" pp_longident an
  | Trecord i ->
    Format.fprintf fmt "%a" pp_longident i
  | Tenum en ->
    Format.fprintf fmt "%a" pp_longident en
  | Tevent ev ->
    Format.fprintf fmt "%a" pp_longident ev
  | Tbuiltin b -> pp_vtyp fmt b
  | Tcontainer (t, c) ->
    Format.fprintf fmt "%a<%a>"
      pp_container c
      pp_type t
  | Tset t ->
    Format.fprintf fmt "set<%a>"
      pp_type t
  | Tlist t ->
    Format.fprintf fmt "list<%a>"
      pp_type t
  | Tmap (k, v) ->
    Format.fprintf fmt "map<%a, %a>"
      pp_type k
      pp_type v
  | Tbig_map (k, v) ->
    Format.fprintf fmt "big_map<%a, %a>"
      pp_type k
      pp_type v
  | Titerable_big_map (k, v) ->
    Format.fprintf fmt "iterable_big_map<%a, %a>"
      pp_type k
      pp_type v
  | Tor (k, v) ->
    Format.fprintf fmt "or<%a, %a>"
      pp_type k
      pp_type v
  | Tlambda (a, r) ->
    Format.fprintf fmt "lambda<%a, %a>"
      pp_type a
      pp_type r
  | Toption t ->
    Format.fprintf fmt "option<%a>"
      pp_type t
  | Ttuple ts ->
    Format.fprintf fmt "(%a)"
      (pp_list " * " pp_type) ts
  | Toperation ->
    Format.fprintf fmt "operation"
  | Tcontract et ->
    Format.fprintf fmt "contract<%a>" pp_type et
  | Tticket et ->
    Format.fprintf fmt "ticket<%a>" pp_type et
  | Tsapling_state n ->
    Format.fprintf fmt "sapling_state(%i)" n
  | Tsapling_transaction n ->
    Format.fprintf fmt "sapling_transaction(%i)" n

and pp_type fmt t = pp_ptyp fmt t
(* match a with
   | Some a -> Format.fprintf fmt "(%a %%%a)" pp_ptyp t pp_id a
   | None -> pp_ptyp fmt t *)

let pp_struct_poly pp_node fmt (s : 'a struct_poly) =
  if !Options.opt_typed then
    Format.fprintf fmt "(%a : %a)"
      pp_node s.node
      (pp_option pp_type) s.type_
  else
    pp_node fmt s.node

let pp_bval fmt (bval : bval) =
  let pp_node fmt = function
    | BVint v                  -> Format.fprintf fmt "%ai" pp_big_int v
    | BVnat v                  -> pp_big_int fmt v
    | BVbool v                 -> pp_str fmt (if v then "true" else "false")
    | BVrational (n, d)        -> Format.fprintf fmt "(%a / %a)" pp_big_int n pp_big_int d
    | BVdate v                 -> Core.pp_date fmt v
    | BVstring s               -> Format.fprintf fmt "\"%s\"" s
    | BVcurrency (c, v)        -> Format.fprintf fmt "%a%a" pp_big_int v pp_currency c
    | BVaddress v              -> Format.fprintf fmt "@@%a" pp_str v
    | BVduration v             -> Core.pp_duration_for_printer fmt v
    | BVbytes s                -> Format.fprintf fmt "0x%a" pp_str s
    | BVunit                   -> Format.fprintf fmt "Unit"
    | BVbls12_381_num_fr n     -> Format.fprintf fmt "%afr" pp_big_int n
    | BVbls12_381_byt_fr v     -> Format.fprintf fmt "0x%sfr" v
    | BVbls12_381_g1 v         -> Format.fprintf fmt "0x%sg1" v
    | BVbls12_381_g2 v         -> Format.fprintf fmt "0x%sg2" v
  in
  pp_struct_poly pp_node fmt bval

let pp_logical_operator fmt = function
  | And   -> pp_str fmt "and"
  | Or    -> pp_str fmt "or"
  | Xor   -> pp_str fmt "xor"

let pp_comparison_operator fmt = function
  | Equal  -> pp_str fmt "="
  | Nequal -> pp_str fmt "<>"
  | Gt     -> pp_str fmt ">"
  | Ge     -> pp_str fmt ">="
  | Lt     -> pp_str fmt "<"
  | Le     -> pp_str fmt "<="

let pp_arithmetic_operator fmt = function
  | Plus   -> pp_str fmt "+"
  | Minus  -> pp_str fmt "-"
  | Mult   -> pp_str fmt "*"
  | DivEuc -> pp_str fmt "div"
  | DivRat -> pp_str fmt "/"
  | Modulo -> pp_str fmt "%"
  | DivMod -> pp_str fmt "/%"
  | ThreeWayCmp -> pp_str fmt "<=>"
  | ShiftLeft   -> pp_str fmt "<<"
  | ShiftRight  -> pp_str fmt ">>"

let pp_unary_arithmetic_operator fmt = function
  | Uminus -> pp_str fmt "-"

let pp_assignment_operator fmt = function
  | ValueAssign -> pp_str fmt ":="
  | PlusAssign  -> pp_str fmt "+="
  | MinusAssign -> pp_str fmt "-="
  | MultAssign  -> pp_str fmt "*="
  | DivAssign   -> pp_str fmt "/="
  | AndAssign   -> pp_str fmt "&="
  | OrAssign    -> pp_str fmt "|="

let pp_operator fmt = function
  | `Logical op -> pp_logical_operator fmt op
  | `Cmp     op -> pp_comparison_operator fmt op
  | `Arith   op -> pp_arithmetic_operator fmt op
  | `Unary   op -> pp_unary_arithmetic_operator fmt op
  | `Assign  op -> pp_assignment_operator fmt op

let rec pp_qualid fmt (q : qualid) =
  let pp_node fmt = function
    | Qident i ->
      pp_id fmt i
    | Qdot (q, i) ->
      Format.fprintf fmt "%a.%a"
        pp_qualid q
        pp_id i
  in
  pp_struct_poly pp_node fmt q

let pp_pattern fmt (p : pattern) =
  let pp_node fmt = function
    | Mconst (c, []) -> pp_id fmt c
    | Mconst (c, xs) -> Format.fprintf fmt "%a (%a)" pp_id c (pp_list ", " pp_id) xs
    | Mwild -> pp_str fmt "_"
  in
  pp_struct_poly pp_node fmt p

let to_const = function
  (* constant *)
  | Cstate                 -> "state"
  | Cnow                   -> "now"
  | Ctransferred           -> "transferred"
  | Ccaller                -> "caller"
  | Cfail                  -> "fail"
  | Cbalance               -> "balance"
  | Csource                -> "source"
  | Cselfaddress           -> "self_address"
  | Cselfchainid           -> "self_chain_id"
  | Coperations            -> "operations"
  | Cmetadata              -> "metadata"
  | Clevel                 -> "level"
  (* function *)
  | Cadd                   -> "add"
  | Cput                   -> "put"
  | Caddupdate             -> "add_update"
  | CputRemove             -> "put_remove"
  | Cceil                  -> "ceil"
  | Cclear                 -> "clear"
  | Cconcat                -> "concat"
  | Ccontains              -> "contains"
  | Ccount                 -> "count"
  | Cfloor                 -> "floor"
  | Cget                   -> "get"
  | Cgetopt                -> "getopt"
  | Cisnone                -> "is_none"
  | Cissome                -> "is_some"
  | Cinttonat              -> "int_to_nat"
  | Clength                -> "length"
  | Cmax                   -> "max"
  | Cmin                   -> "min"
  | Cnth                   -> "nth"
  | Cpack                  -> "pack"
  | Cremove                -> "remove"
  | Cremoveall             -> "remove_all"
  | Cremoveif              -> "remove_if"
  | Cselect                -> "select"
  | Cslice                 -> "slice"
  | Csort                  -> "sort"
  | Csum                   -> "sum"
  | Cunpack                -> "unpack"
  | Cupdate                -> "update"
  | Cupdateall             -> "update_all"
  | Cmakeoperation         -> "make_operation"
  | CtoContainer           -> "to_container"
  | Cnattostring           -> "nat_to_string"
  | Cbytestonat            -> "bytes_to_nat"
  | Cinttobytes            -> "int_to_bytes"
  | Cbytestoint            -> "bytes_to_int"
  | Cnattobytes            -> "nat_to_bytes"
  | Cexec                  -> "exec"
  | Capply                 -> "apply"
  | Cinttodate             -> "int_to_date"
  | CmutezToNat            -> "mutez_to_nat"
  | Csetdelegate           -> "set_delegate"
  | Ckeyhashtocontract     -> "key_hash_to_contract"
  | Csubnat                -> "sub_nat"
  | Csubmutez              -> "sub_mutez"
  | Cgreedyand             -> "greedy_and"
  | Cgreedyor              -> "greedy_or"
  | CmakeAsset             -> "make_asset"
  | CgetEntrypoint         -> "get_entrypoint"
  | CrequireEntrypoint     -> "require_entrypoint"
  | CcallView              -> "call_view"
  | CimportCallView        -> "import_call_view"
  | CselfCallView          -> "self_call_view"
  | Cmakeevent             -> "make_event"
  | Csimplifyrational      -> "simplify_rational"
  | Cgetnumerator          -> "get_numerator"
  | Cgetdenominator        -> "get_denominator"
  | Cglobalconstant        -> "global_constant"
  | Cisimplicitaddress     -> "is_implicit_address"
  | Cexphorner             -> "exp_horner"
  (* set *)
  | Csadd                  -> "set_add"
  | Csremove               -> "set_remove"
  | Csupdate               -> "set_update"
  | Cscontains             -> "set_contains"
  | Cslength               -> "set_length"
  (* list *)
  | Chead                  -> "head"
  | Ctail                  -> "tail"
  | Cabs                   -> "abs"
  | Cprepend               -> "prepend"
  | Creverse               -> "reverse"
  (* map *)
  | Cmput                  -> "put"
  | Cmremove               -> "remove"
  | Cmupdate               -> "update"
  | Cmget                  -> "get"
  | Cmgetopt               -> "getopt"
  | Cmcontains             -> "contains"
  | Cmlength               -> "length"
  (* crypto *)
  | Cblake2b               -> "blake2b"
  | Csha256                -> "sha256"
  | Csha512                -> "sha512"
  | Csha3                  -> "sha3"
  | Ckeccak                -> "keccak"
  | Cchecksignature        -> "check_signature"
  | Ckeytokeyhash          -> "key_to_key_hash"
  | Ccontracttoaddress     -> "contract_to_address"
  | Caddresstocontract     -> "address_to_contract"
  | Ckeytoaddress          -> "key_to_address"
  (* voting *)
  | Ctotalvotingpower      -> "total_voting_power"
  | Cvotingpower           -> "voting_power"
  | Cminblocktime          -> "min_block_time"
  (* ticket *)
  | Ccreateticket          -> "create_ticket"
  | Creadticket            -> "read_ticket"
  | Csplitticket           -> "split_ticket"
  | Cjointickets           -> "join_tickets"
  (* sapling *)
  | Csapling_empty_state   -> "sapling_empty_state"
  | Csapling_verify_update -> "sapling_verify_update"
  (* bls curve *)
  | Cpairing_check         -> "pairing_check"
  (* timelock *)
  | Copen_chest            -> "open_chest"
  (* event *)
  | Cemit                  -> "emit"

let pp_call_kind fmt = function
  | Cid id -> pp_longident fmt id
  | Cconst c -> pp_str fmt (to_const c)

let pp_security_role = pp_lident

let pp_entry_description fmt = function
  | ADAny -> pp_str fmt "anyentry"
  | ADOp (a, b) -> Format.fprintf fmt "%s (%a)" a pp_id b

let rec pp_pterm fmt (pterm : pterm) =
  let pp_node fmt = function
    | Pif (c, t, e) ->
      let pp fmt (c, t, e) =
        Format.fprintf fmt "if %a@\nthen @[%a@]@\nelse @[%a@]"
          pp_pterm c
          pp_pterm t
          pp_pterm e
      in
      (pp_with_paren pp) fmt (c, t, e)

    | Pmatchwith (m, ps) ->
      let pp fmt (m, ps) =
        Format.fprintf fmt "match %a with@\n  @[%a@]@\n"
          pp_pterm m
          (pp_list "@\n" (fun fmt (p, i) ->
               Format.fprintf fmt "| %a -> %a"
                 pp_pattern p
                 pp_pterm i)) ps
      in
      (pp_with_paren pp) fmt (m, ps)

    | Pmatchoption (x, id, ve, ne) ->
      let pp fmt (x, id, ve, ne) =
        Format.fprintf fmt "match %a with@\n  | some (%a) -> (@[%a@])@\n  | none -> (@[%a@])@\nend"
          pp_pterm x
          pp_id id
          pp_pterm ve
          pp_pterm ne
      in
      (pp_with_paren pp) fmt (x, id, ve, ne)

    | Pmatchor (x, lid, le, rid, re) ->
      let pp fmt (x, lid, le, rid, re) =
        Format.fprintf fmt "match %a with@\n  | left (%a) -> (@[%a@])@\n  | right (%a) -> (@[%a@])@\nend"
          pp_pterm x
          pp_id lid
          pp_pterm le
          pp_id rid
          pp_pterm re
      in
      (pp_with_paren pp) fmt (x, lid, le, rid, re)

    | Pmatchlist (x, hid, tid, hte, ee) ->
      let pp fmt (x, hid, tid, hte, ee) =
        Format.fprintf fmt "match %a with@\n  | %a::%a -> (@[%a@])@\n  | [] -> (@[%a@])@\nend"
          pp_pterm x
          pp_id hid
          pp_id tid
          pp_pterm hte
          pp_pterm ee
      in
      (pp_with_paren pp) fmt (x, hid, tid, hte, ee)

    | Pfold (x, id, e) ->
      let pp fmt (x, id, e) =
        Format.fprintf fmt "fold (%a, %a -> (@[%a@]))@\n"
          pp_pterm x
          pp_id id
          pp_pterm e
      in
      (pp_with_paren pp) fmt (x, id, e)

    | Pmap (x, id, e) ->
      let pp fmt (x, id, e) =
        Format.fprintf fmt "map (%a, %a -> (@[%a@]))"
          pp_pterm x
          pp_id id
          pp_pterm e
      in
      (pp_with_paren pp) fmt (x, id, e)

    | Pcall (meth, kind, types, args) ->
      let pp_types fmt l = Format.fprintf fmt "<%a>" (pp_list ", " pp_type) l in
      let pp fmt (meth, kind, types, args) =
        Format.fprintf fmt "%a%a%a(%a)"
          (pp_option (pp_postfix "." pp_pterm)) meth
          pp_call_kind kind
          (pp_if (List.length types > 0) pp_types pp_void) types
          (pp_list ", " pp_term_arg) args
      in
      (pp_with_paren pp) fmt (meth, kind, types, args)

    | Plogical (op, lhs, rhs) ->
      let pp fmt (op, lhs, rhs) =
        Format.fprintf fmt "%a %a %a"
          pp_pterm lhs
          pp_logical_operator op
          pp_pterm rhs
      in
      (pp_with_paren pp) fmt (op, lhs, rhs)

    | Pnot pt ->
      let pp fmt pt =
        Format.fprintf fmt "not (%a)"
          pp_pterm pt
      in
      (pp_with_paren pp) fmt pt

    | Pmulticomp (e, l) ->
      let pp fmt (e, l) =
        let pp_item fmt (op, e) =
          Format.fprintf fmt "%a %a"
            pp_comparison_operator op
            pp_pterm e
        in
        Format.fprintf fmt "%a %a"
          pp_pterm e
          (pp_list " " pp_item) l
      in
      (pp_with_paren pp) fmt (e, l)

    | Pcomp (op, lhs, rhs) ->
      let pp fmt (op, lhs, rhs) =
        Format.fprintf fmt "%a %a %a"
          pp_pterm lhs
          pp_comparison_operator op
          pp_pterm rhs
      in
      (pp_with_paren pp) fmt (op, lhs, rhs)

    | Parith (op, lhs, rhs) ->
      let pp fmt (op, lhs, rhs) =
        Format.fprintf fmt "%a %a %a"
          pp_pterm lhs
          pp_arithmetic_operator op
          pp_pterm rhs
      in
      (pp_with_paren pp) fmt (op, lhs, rhs)

    | Puarith (op, p) ->
      let pp fmt (op, p) =
        Format.fprintf fmt "%a%a"
          pp_unary_arithmetic_operator op
          pp_pterm p
      in
      (pp_with_paren pp) fmt (op, p)

    | Precord l ->
      let pp fmt l =
        Format.fprintf fmt "{%a}"
          (pp_list "; " pp_pterm) l
      in
      (pp_no_paren pp) fmt l

    | Precupdate (e, l) ->
      let pp fmt (e, l) =
        Format.fprintf fmt "{%a with %a}"
          pp_pterm e
          (pp_list "; " (fun fmt (id, v) -> Format.fprintf fmt "%a = %a" pp_id id pp_pterm v)) l
      in
      (pp_no_paren pp) fmt (e, l)

    | Pletin (id, init, t, body, otherwise) ->
      let pp fmt (id, init, t, body) =
        Format.fprintf fmt "let %a%a= %a in@\n%a%a"
          pp_id id
          (pp_option (pp_prefix " : " pp_type)) t
          pp_pterm init
          pp_pterm body
          (pp_option (fun fmt -> Format.fprintf fmt " otherwise %a" pp_pterm)) otherwise
      in
      (pp_with_paren pp) fmt (id, init, t, body)

    | Pdeclvar (i, t, v, c) ->
      let pp fmt (i, t, v) =
        Format.fprintf fmt "%s %a%a = %a"
          (if c then "const" else "var")
          pp_id i
          (pp_option (pp_prefix " : " pp_type)) t
          pp_pterm v
      in
      (pp_with_paren pp) fmt (i, t, v)

    | Pvar id ->
      let pp fmt id =
        Format.fprintf fmt "%a" pp_longident id
      in
      (pp_no_paren pp) fmt id

    | Parray l ->
      let pp fmt l =
        Format.fprintf fmt "[%a]"
          (pp_list "; " pp_pterm) l
      in
      (pp_no_paren pp) fmt l

    | Plit v ->
      let pp fmt v =
        pp_bval fmt v
      in
      (pp_no_paren pp) fmt v

    | Pdot ({ node = Pcall (Some { node = Pvar an },
                            Cconst Cget, [], [AExpr k]) }, fn)
      ->
      let pp fmt (an, k, fn) =
        Format.fprintf fmt "%a[%a].%a"
          pp_longident an
          pp_pterm k
          pp_id fn
      in (pp_with_paren pp) fmt (an, k, fn)

    | Pdot (e, i) ->
      let pp fmt (e, i) =
        Format.fprintf fmt "%a.%a"
          pp_pterm e
          pp_id i
      in
      (pp_with_paren pp) fmt (e, i)

    | Pquestiondot (e, i) ->
      let pp fmt (e, i) =
        Format.fprintf fmt "%a?.%a"
          pp_pterm e
          pp_id i
      in
      (pp_with_paren pp) fmt (e, i)

    | Pconst c ->
      let pp fmt c =
        pp_str fmt (to_const c)
      in
      (pp_no_paren pp) fmt c

    | Ptuple l ->
      let pp fmt l =
        Format.fprintf fmt "(%a)"
          (pp_list ", " pp_pterm) l
      in
      (pp_no_paren pp) fmt l

    | Ptupleaccess (x, k) ->
      let pp fmt (x, k) =
        Format.fprintf fmt "%a[%a]"
          pp_pterm x
          pp_big_int k
      in
      (pp_no_paren pp) fmt (x, k)

    | Pleft (t, x) ->
      let pp fmt (t, x) =
        Format.fprintf fmt "left<%a>(%a)"
          pp_type t
          pp_pterm x
      in
      (pp_no_paren pp) fmt (t, x)

    | Pright (t, x) ->
      let pp fmt (t, x) =
        Format.fprintf fmt "right<%a>(%a)"
          pp_type t
          pp_pterm x
      in
      (pp_no_paren pp) fmt (t, x)

    | Plambda (rt, id, at, e) ->
      let pp fmt (rt, id, at, e) =
        Format.fprintf fmt "lambda<%a>((%a : %a) -> @[%a@])"
          pp_type rt
          pp_id id
          pp_type at
          pp_pterm e
      in
      (pp_no_paren pp) fmt (rt, id, at, e)

    | Plambda_michelson (it, rt, body) ->
      let pp fmt (it, rt, body) =
        Format.fprintf fmt "lambda_michelson<%a, %a>(@[%a@])"
          pp_type it
          pp_type rt
          Micheline_printer.print_expr (Micheline_tools.obj_to_micheline body)
      in
      (pp_no_paren pp) fmt (it, rt, body)

    | Pnone -> pp_str fmt "none"

    | Psome a ->
      let pp fmt a =
        Format.fprintf fmt "some(%a)"
          pp_pterm a
      in
      (pp_no_paren pp) fmt a

    | Pcast (src, dst, a) ->
      let pp fmt (src, dst, a) =
        Format.fprintf fmt "cast_%a_%a(%a)"
          pp_type src
          pp_type dst
          pp_pterm a
      in
      (pp_no_paren pp) fmt (src, dst, a)

    | Pself id ->
      let pp fmt id =
        Format.fprintf fmt "self.%a"
          pp_id id
      in
      (pp_no_paren pp) fmt id

    | Pternary (c, a, b) ->
      let pp fmt (c, a, b) =
        Format.fprintf fmt "%a ? %a : %a"
          pp_pterm c
          pp_pterm a
          pp_pterm b
      in
      (pp_no_paren pp) fmt (c, a, b)

    | Pcreatecontract (okh, amount, cct) ->
      let pp fmt (okh, amount, cct) =
        match cct with
        | CCTz (_, arg_storage) ->
          Format.fprintf fmt "create_contract(%a, %a, %a)"
            pp_pterm okh
            pp_pterm amount
            pp_pterm arg_storage
        | CCArl (name, args) ->
          Format.fprintf fmt "create_contract(%a, %a, Arl(%a, [%a]))"
            pp_pterm okh
            pp_pterm amount
            pp_ident name
            (pp_list ";" (fun fmt (id, v) -> Format.fprintf fmt "%a = %a" pp_ident id pp_pterm v)) args
      in
      (pp_no_paren pp) fmt (okh, amount, cct)

    | Ptz_expr v ->
      let pp fmt v =
        Format.pp_print_string fmt v
      in
      (pp_no_paren pp) fmt v

    | Pmicheline_expr (t, m, a) -> begin
        let pp fmt (t, m, a) =
          Format.fprintf fmt "michelson<%a> @[%a@] [%a]"
            pp_type t
            Micheline_printer.print_expr (Micheline_tools.obj_to_micheline m)
            (pp_list " : " pp_pterm) a
        in
        (pp_no_paren pp) fmt (t, m, a)
      end
  in
  pp_struct_poly pp_node fmt pterm

and pp_term_arg fmt = function
  | AExpr pt -> pp_pterm fmt pt
  | AFun (id, t, l, pt) ->
    if !Options.opt_typed
    then
      Format.fprintf fmt "(%a : %a)%a -> %a"
        pp_id id
        pp_type t
        (pp_list "" (fun fmt (x, y, z) -> Format.fprintf fmt " (%a : %a = %a)" pp_id x pp_type y pp_pterm z)) l
        pp_pterm pt
    else
      pp_pterm fmt pt

  | AEffect l ->
    Format.fprintf fmt "{ %a }"
      (pp_list "; " (fun fmt (id, op, pt) ->
           Format.fprintf fmt "%a %a %a"
             pp_id id
             pp_operator op
             pp_pterm pt)) l

  | ASorting (b, f) ->
    let k = if b then "asc" else "desc" in
    Format.fprintf fmt "%s(%a)"
      k
      pp_id f

  | AIdent id -> pp_id fmt id

let pp_instruction_poly pp fmt i =
  pp fmt i.node

let pp_dk fmt = function
  | DK_option (_, id) -> Format.pp_print_string fmt id
  | DK_map (_, id, k) -> Format.fprintf fmt "%a[%a]" Format.pp_print_string id pp_pterm k

let rec pp_instruction fmt (i : instruction) =
  let pp_node fmt = function
    | Iif (c, t, {node = Iseq []; _}) ->
      let pp fmt (c, t) =
        Format.fprintf fmt "if %a@\nthen @[%a@]"
          pp_pterm c
          pp_instruction t
      in
      (pp_with_paren pp) fmt (c, t)
    | Iif (c, t, e) ->
      let pp fmt (c, t, e) =
        Format.fprintf fmt "if %a@\nthen @[%a@]@\nelse @[%a@]"
          pp_pterm c
          pp_instruction t
          pp_instruction e
      in
      (pp_with_paren pp) fmt (c, t, e)

    | Ifor (fid, c, body) ->
      let pp fmt (fid, c, body) =
        Format.fprintf fmt "for %a%a in %a do@\n  @[%a@]@\ndone"
          (fun fmt x -> match x with Some v -> (Format.fprintf fmt ": %a " pp_str v) | _ -> ()) i.label
          (fun fmt fid ->
             match fid with
             | FIsimple i -> pp_id fmt i
             | FIdouble (x, y) -> Format.fprintf fmt "(%a, %a)" pp_id x pp_id y) fid
          pp_pterm c
          pp_instruction body
      in
      (pp_with_paren pp) fmt (fid, c, body)

    | Iiter (id, a, b, body) ->
      let pp fmt (id, a, b, body) =
        Format.fprintf fmt "iter %a%a from %a to %a do@\n  @[%a@]@\ndone"
          (fun fmt x -> match x with Some v -> (Format.fprintf fmt ": %a " pp_str v) | _ -> ()) i.label
          pp_id id
          pp_pterm a
          pp_pterm b
          pp_instruction body
      in
      (pp_with_paren pp) fmt (id, a, b, body)

    | Iwhile (cond, body) ->
      let pp fmt (cond, body) =
        Format.fprintf fmt "while %a block{@\n  @[%a@]@\n}"
          pp_pterm cond
          pp_instruction body
      in
      (pp_with_paren pp) fmt (cond, body)

    | Iletin (id, init, body) ->
      let pp fmt (id, init, body) =
        Format.fprintf fmt "let %a%a = %a in@\n%a"
          pp_id id
          (pp_option (pp_prefix " : " pp_type)) init.type_
          pp_pterm init
          pp_instruction body
      in
      (pp_with_paren pp) fmt (id, init, body)

    | Ideclvar (ids, v, c) ->
      let pp fmt (ids, v, c) =
        Format.fprintf fmt "%s %a%a = %a"
          (if c then "const" else "var")
          (pp_list ", " (fun fmt (id, ty) -> Format.fprintf fmt "(%a : %a)" pp_id id pp_type ty)) ids
          (pp_option (pp_prefix " : " pp_type)) v.type_
          pp_pterm v
      in
      (pp_with_paren pp) fmt (ids, v, c)

    | Ideclvaropt (ids, v, fa, c) ->
      let pp fmt (ids, v, fa, c) =
        Format.fprintf fmt "%s %a%a ?= %a%a"
          (if c then "const" else "var")
          (pp_list ", " (fun fmt (id, ty) -> Format.fprintf fmt "(%a : %a)" pp_id id pp_type ty)) ids
          (pp_option (pp_prefix " : " pp_type)) v.type_
          pp_pterm v
          (pp_option (fun fmt x -> Format.fprintf fmt " : %a" pp_pterm x)) fa
      in
      (pp_with_paren pp) fmt (ids, v, fa, c)

    | Iseq l ->
      let pp fmt l =
        if List.is_empty l
        then pp_str fmt "()"
        else pp_paren (pp_list ";@\n" pp_instruction) fmt l
      in
      (pp_with_paren pp) fmt l

    | Imatchwith (m, ps) ->
      let pp fmt (m, ps) =
        Format.fprintf fmt "match %a with@\n  @[%a@]@\n"
          pp_pterm m
          (pp_list "@\n" (fun fmt (p, i) ->
               Format.fprintf fmt "| %a -> %a"
                 pp_pattern p
                 pp_instruction i)) ps
      in
      (pp_with_paren pp) fmt (m, ps)

    | Imatchoption (x, id, ve, ne) ->
      let pp fmt (x, id, ve, ne) =
        Format.fprintf fmt "match %a with@\n  | some (%a) -> (@[%a@])@\n  | none -> (@[%a@])@\nend"
          pp_pterm x
          pp_id id
          pp_instruction ve
          pp_instruction ne
      in
      (pp_with_paren pp) fmt (x, id, ve, ne)

    | Imatchor (x, lid, le, rid, re) ->
      let pp fmt (x, lid, le, rid, re) =
        Format.fprintf fmt "match %a with@\n  | left (%a) -> (@[%a@])@\n  | right (%a) -> (@[%a@])@\nend"
          pp_pterm x
          pp_id lid
          pp_instruction le
          pp_id rid
          pp_instruction re
      in
      (pp_with_paren pp) fmt (x, lid, le, rid, re)

    | Imatchlist (x, hid, tid, hte, ee) ->
      let pp fmt (x, hid, tid, hte, ee) =
        Format.fprintf fmt "match %a with@\n  | %a::%a -> (@[%a@])@\n  | [] -> (@[%a@])@\nend"
          pp_pterm x
          pp_id hid
          pp_id tid
          pp_instruction hte
          pp_instruction ee
      in
      (pp_with_paren pp) fmt (x, hid, tid, hte, ee)

    | Imatchdetach (dk, id, ve, ne) ->
      let pp fmt (dk, id, ve, ne) =
        Format.fprintf fmt "match_detach %a with@\n  | some (%a) -> (@[%a@])@\n  | none -> (@[%a@])@\nend"
          pp_dk dk
          pp_id id
          pp_instruction ve
          pp_instruction ne
      in
      (pp_with_paren pp) fmt (dk, id, ve, ne)

    | Iassign (op, _, `Var id, value, fa) ->
      let pp fmt (op, id, value, fa) =
        Format.fprintf fmt "%a %a %a%a"
          pp_id id
          pp_assignment_operator op
          pp_pterm value
          (pp_option (fun fmt x -> Format.fprintf fmt " : %a" pp_pterm x)) fa
      in
      (pp_with_paren pp) fmt (op, id, value, fa)

    | Iassign (op, _, `Field (rn, k, fn), value, fa) ->
      let pp fmt (op, rn, k, fn, value, fa) =
        Format.fprintf fmt "%a[%a].%a %a %a%a"
          pp_longident rn
          pp_pterm k
          pp_id fn
          pp_assignment_operator op
          pp_pterm value
          (pp_option (fun fmt x -> Format.fprintf fmt " : %a" pp_pterm x)) fa
      in
      (pp_with_paren pp) fmt (op, rn, k, fn, value, fa)

    | Iassign (op, _, `Asset (an, k, fn), value, fa) ->
      let pp fmt (op, an, k, fn, value, fa) =
        Format.fprintf fmt "%a[%a].%a %a %a%a"
          pp_longident an
          pp_pterm k
          pp_id fn
          pp_assignment_operator op
          pp_pterm value
          (pp_option (fun fmt x -> Format.fprintf fmt " : %a" pp_pterm x)) fa
      in
      (pp_with_paren pp) fmt (op, an, k, fn, value, fa)

    | Iassign (op, _, `Tuple (e, i, l), value, fa) ->
      let pp fmt (op, e, i, l, value, fa) =
        Format.fprintf fmt "%a[%d/%d] %a %a%a"
          pp_pterm e
          i
          l
          pp_assignment_operator op
          pp_pterm value
          (pp_option (fun fmt x -> Format.fprintf fmt " : %a" pp_pterm x)) fa
      in
      (pp_with_paren pp) fmt (op, e, i, l, value, fa)

    | Irequire (k, pt, f) ->
      let pp fmt (k, pt, f) =
        Format.fprintf fmt "%a (%a, %a)"
          pp_str (if k then "do_require" else "do_fail_if")
          pp_pterm pt
          pp_pterm f
      in
      (pp_with_paren pp) fmt (k, pt, f)

    | Itransfer tr ->
      let pp fmt = function
        | TTsimple (x, dst)               -> Format.fprintf fmt "transfer %a to %a" pp_pterm x pp_pterm dst
        | TTcontract (x, dst, id, t, arg) -> Format.fprintf fmt "transfer %a to %a call %a<%a>(%a)" pp_pterm x pp_pterm dst pp_id id pp_type t pp_pterm arg
        | TTentry (x, e, arg)             -> Format.fprintf fmt "transfer %a to entry %a(%a)" pp_pterm x pp_pterm e pp_pterm arg
        | TTgen (x, en, cn, _t, e, arg)   -> Format.fprintf fmt "transfer %a to entry %s(%a).%s(%a)" pp_pterm x cn pp_pterm e en pp_pterm arg
        | TTself (x, id, args)            -> Format.fprintf fmt "transfer %a to entry self.%a(%a)" pp_pterm x pp_id id (pp_list "," (fun fmt (id, v) ->  Format.fprintf fmt "%a = %a" pp_id id pp_pterm v)) args
        | TToperation x                   -> Format.fprintf fmt "transfer %a" pp_pterm x
      in
      (pp_with_paren pp) fmt tr
    | Iemit (e, v) ->
      let pp fmt (e, v) =
        Format.fprintf fmt "emit<%a>(%a)" pp_longident e pp_pterm v
      in
      (pp_with_paren pp) fmt (e, v)
    | Icall (meth, kind, args) ->
      let pp fmt (meth, kind, args) =
        Format.fprintf fmt "%a%a(%a)"
          (pp_option (pp_postfix "." pp_pterm)) meth
          pp_call_kind kind
          (pp_list ", " pp_term_arg) args
      in
      (pp_with_paren pp) fmt (meth, kind, args)

    | Ireturn pt ->
      let pp fmt pt =
        Format.fprintf fmt "return %a"
          pp_pterm pt
      in
      (pp_with_paren pp) fmt pt

    | Ifail pt ->
      let pp fmt pt =
        Format.fprintf fmt "fail (%a)"
          pp_pterm pt
      in
      (pp_with_paren pp) fmt pt

    | Ifailsome pt ->
      let pp fmt pt =
        Format.fprintf fmt "fail_some (%a)"
          pp_pterm pt
      in
      (pp_with_paren pp) fmt pt

    | Idetach (id, dk, ty, f) ->
      let pp fmt (id, dk, _ty, f) =
        Format.fprintf fmt "detach %a as %a : %a"
          pp_dk dk
          pp_id id
          pp_pterm f
      in
      (pp_no_paren pp) fmt (id, dk, ty, f)

    | Imicheline micheline -> begin
        let printable_micheline : Micheline_printer.node = Micheline_tools.obj_to_micheline micheline in
        Format.fprintf fmt "michelson @[%a@]" Micheline_printer.print_expr printable_micheline
      end

  in
  pp_instruction_poly pp_node fmt i

let pp_label_term fmt (lt : label_term) =
  Format.fprintf fmt "%a%a"
    (pp_option (pp_postfix " : " pp_id)) lt.label
    pp_pterm lt.term

let pp_variable_kind fmt = function
  | VKconstant  -> Format.fprintf fmt "constant"
  | VKvariable  -> Format.fprintf fmt "variable"

let pp_variable fmt (v : variable) =
  Format.fprintf fmt "%a %a : %a%a@\n"
    pp_variable_kind v.kind
    pp_longident v.decl.name
    pp_type (Option.get v.decl.typ)
    (pp_option (pp_prefix " = " pp_pterm)) v.decl.default

let pp_field fmt (f : lident decl_gen) =
  Format.fprintf fmt "%a : %a%a;"
    pp_id f.name
    (pp_option pp_type) f.typ
    (pp_option (pp_prefix " := " pp_pterm)) f.default

let pp_asset fmt (a : asset) =
  let fields = List.filter (fun f -> not f.shadow) a.fields in
  let shadow_fields = List.filter (fun f -> f.shadow) a.fields in
  Format.fprintf fmt "asset %a%a%a%s {@\n  @[%a@]@\n}%a%a%a@\n"
    pp_longident a.name
    (pp_prefix " identified by " (pp_list " " pp_id)) a.keys
    (pp_do_if (not (List.is_empty a.sort)) (pp_prefix " sorted by " (pp_list ", " pp_id))) a.sort
    (match a.map_kind with | MKMap -> "" | MKBigMap -> " to big_map" | MKIterableBigMap -> " to iterable_big_map")
    (pp_list "@\n" pp_field) fields
    (pp_do_if (not (List.is_empty shadow_fields)) (
        fun fmt fields ->
          Format.fprintf fmt " shadow {@\n  @[%a@]@\n} "
            (pp_list "@\n" pp_field) fields)) shadow_fields
    (pp_do_if (not (List.is_empty a.init)) (
        let pp1 fmt init1 =
          Format.fprintf fmt "  {%a};"
            (pp_list "; " pp_pterm) init1
        in
        fun fmt init ->
          Format.fprintf fmt " initialized by {@\n%a@\n} "
            (pp_list "@\n" pp1) init)) a.init
    (pp_do_if (not (List.is_empty a.specs)) (
        fun fmt ->
          Format.fprintf fmt " with {@\n  @[%a@]@\n}"
            (pp_list ";@\n" pp_label_term))) a.specs

let rec pp_position pp fmt = function
  | Pleaf x -> pp fmt x
  | Pnode l -> (pp_paren (pp_list "," (pp_position pp))) fmt l

let pp_record fmt (r : record) =
  Format.fprintf fmt "record %a {@\n  @[%a@]@\n}@\nas %a@\n"
    pp_longident r.name
    (pp_list "@\n" pp_field) r.fields
    (pp_position pp_id) r.pos

let pp_event fmt (r : record) =
  Format.fprintf fmt "event %a {@\n  @[%a@]@\n}@\nas %a@\n"
    pp_longident r.name
    (pp_list "@\n" pp_field) r.fields
    (pp_position pp_id) r.pos

let pp_enum_item fmt (ei : enum_item_struct) =
  Format.fprintf fmt "| %a%a%a%a"
    pp_id ei.name
    (fun fmt l ->
       if List.is_empty l
       then ()
       else (Format.fprintf fmt " of %a" (pp_list " * " pp_type) l)
    ) ei.args
    (pp_do_if ei.initial pp_str) " initial"
    (pp_do_if (not (List.is_empty ei.invariants)) (
        fun fmt ->
          Format.fprintf fmt " with {@[%a@]}"
            (pp_list ";@\n" pp_label_term))) ei.invariants

let pp_enum fmt (e : enum) =
  Format.fprintf fmt "%a =@\n  @[%a@]@\n"
    (fun fmt e ->
       match e.kind with
       | EKenum id -> Format.fprintf fmt "enum %a" pp_longident id
       | EKstate _ -> pp_str fmt "states"
    ) e
    (pp_list "@\n" pp_enum_item) e.items

let rec pp_rexpr fmt (r : rexpr) =
  let pp_node fmt = function
    | Rany -> pp_str fmt "any"
    | Rasset a -> pp_longident fmt a
    | Rexpr e -> pp_pterm fmt e
    | Ror (lhs, rhs) ->
      Format.fprintf fmt "%a or %a"
        pp_rexpr lhs
        pp_rexpr rhs
  in
  pp_struct_poly pp_node fmt r

let rec pp_sexpr fmt (s : sexpr) =
  let pp_node fmt = function
    | Sref id -> pp_id fmt id
    | Sor (lhs, rhs) ->
      Format.fprintf fmt "%a or %a"
        pp_sexpr lhs
        pp_sexpr rhs
    | Sany -> pp_str fmt "any"
  in
  pp_struct_poly pp_node fmt s

let pp_fun_ident_typ fmt (arg : lident decl_gen) =
  Format.fprintf fmt "%a : %a"
    pp_id arg.name
    pp_type (Option.get arg.typ)

let pp_fun_args fmt args =
  Format.fprintf fmt " (%a)"
    (pp_list " " pp_fun_ident_typ) args

let pp_function fmt (f : function_) =
  let pp_returned_fun_type fmt = function
    | Void -> Format.fprintf fmt "void"
    | Typed ty -> pp_type fmt ty
  in
  Format.fprintf fmt "%s %a%a : %a =@\n  @[%a@]@\n"
    (match f.kind with | FKfunction -> "function" | FKview vv -> ((match vv with | VVonchain -> "onchain" | VVoffchain -> "offchain" | VVonoffchain -> "offchain onchain") ^ " view"))
    pp_longident f.name
    pp_fun_args f.args
    pp_returned_fun_type f.return
    pp_instruction f.body

let pp_otherwise fmt o = pp_option (fun fmt x -> Format.fprintf fmt " otherwise %a" pp_pterm x) fmt o

let pp_transaction_entry fmt (t : transaction) =
  let decl, oty = match t.kind with | Entry -> "entry", None | Getter ty -> ("getter", Some ty) in
  Format.fprintf fmt "%s %a%a%a {@\n  @[%a%a%a%a%a%a%a%a%a@]@\n}@\n"
    decl
    pp_id t.name
    pp_fun_args t.args
    (pp_option (fun fmt x -> Format.fprintf fmt " : %a" pp_type x)) oty
    (pp_do_if (not (fst t.accept_transfer)) (fun fmt (_, o) -> Format.fprintf fmt "no transfer%a@\n" pp_otherwise o)) t.accept_transfer
    (pp_option (fun fmt (x, o) -> Format.fprintf fmt "sourced by %a%a@\n" pp_rexpr x pp_otherwise o)) t.sourcedby
    (pp_option (fun fmt (x, o) -> Format.fprintf fmt "called by %a%a@\n" pp_rexpr x pp_otherwise o)) t.calledby
    (pp_option (fun fmt (x ,o) -> Format.fprintf fmt "state is %a%a@\n" pp_id x pp_otherwise o )) t.state_is
    (pp_option (pp_list "@\n " (fun fmt -> Format.fprintf fmt "constant {@\n  @[%a@]@\n}@\n" pp_label_term))) t.constants
    (pp_option (pp_list "@\n " (fun fmt -> Format.fprintf fmt "require {@\n  @[%a@]@\n}@\n" pp_label_term))) t.require
    (pp_option (pp_list "@\n " (fun fmt -> Format.fprintf fmt "failif {@\n  @[%a@]@\n}@\n" pp_label_term))) t.failif
    (pp_list "@\n" pp_function) t.functions
    (pp_option (fun fmt x -> Format.fprintf fmt "effect {@\n  @[%a@]@\n}@\n" pp_instruction x)) t.effect

let rec pp_sexpr fmt (sexpr : sexpr) =
  match sexpr.node with
  | Sref id -> pp_id fmt id
  | Sor (lhs, rhs) -> Format.fprintf fmt "%a or %a" pp_sexpr lhs pp_sexpr rhs
  | Sany -> pp_str fmt "any"

let pp_transaction_transition fmt (t : transaction) (tr : transition) =
  Format.fprintf fmt "transition %a%a {@\n  @[%a%a%a%a%a%a@]@\n}@\n"
    pp_id t.name
    pp_fun_args t.args
    (pp_option (fun fmt (x, _) -> Format.fprintf fmt "called by %a@\n" pp_rexpr x)) t.calledby
    (pp_do_if (not (fst t.accept_transfer)) (fun fmt (_, o) -> Format.fprintf fmt "no transfer%a@\n" pp_otherwise o)) t.accept_transfer
    (pp_option (pp_list "@\n " (fun fmt -> Format.fprintf fmt "require {@\n  @[%a@]@\n}@\n" pp_label_term))) t.require
    (pp_list "@\n" pp_function) t.functions
    (fun fmt from -> Format.fprintf fmt " from %a@\n"
        pp_sexpr from
    ) tr.from
    (pp_list "@\n" (fun fmt (to_, cond, entry) ->
         Format.fprintf fmt "to %a%a@\n%a@\n"
           pp_id to_
           (pp_option (fun fmt x -> (Format.fprintf fmt " when %a" pp_pterm x))) cond
           (pp_option (fun fmt x -> (Format.fprintf fmt "with effect {@\n  @[%a@]}@\n" pp_instruction x))) entry
       )) tr.trs

let pp_transaction fmt (t : transaction) =
  match t.transition with
  | Some tr -> pp_transaction_transition fmt t tr
  | None -> pp_transaction_entry fmt t

let pp_decl_ fmt = function
  | Dvariable v -> pp_variable fmt v
  | Dasset    a -> pp_asset fmt a
  | Drecord   r -> pp_record fmt r
  | Denum     e -> pp_enum fmt e
  | Devent    e -> pp_event fmt e

let pp_fun_ fmt = function
  | Ffunction f    -> pp_function fmt f
  | Ftransaction t -> pp_transaction fmt t

let pp_parameter fmt (p : parameter) =
  Format.fprintf fmt "%a%a : %a%a"
    (pp_do_if p.const (fun fmt _ -> pp_str fmt "const ")) ()
    pp_id p.name
    pp_type p.typ
    (pp_option (fun fmt x -> Format.fprintf fmt " = %a" pp_pterm x)) p.default

let pp_parameters fmt ps =
  match ps with
  | [] -> ()
  | _  -> Format.fprintf fmt "(%a)" (pp_list ", " pp_parameter) ps

let pp_parameter_value fmt (ps : parameter) =
  match ps.value with
  | None   -> pp_str fmt "_"
  | Some v -> pp_pterm fmt v

let pp_parameter_values fmt (ps : parameter list) =
  match ps with
  | [] -> ()
  | _  -> Format.fprintf fmt "// %a@\n" (pp_list ", " pp_parameter_value) ps

let pp_metadata fmt (m : metadata_kind) =
  match m with
  | MKuri  v -> Format.fprintf fmt "\"%s\"" (Location.unloc v)
  | MKjson v -> Format.fprintf fmt "`%s`"   (Location.unloc v)

let pp_ast fmt (ast : ast) =
  Format.fprintf fmt "archetype %a%a%a@\n@\n@."
    pp_id ast.name
    pp_parameters ast.parameters
    (pp_option (fun fmt x -> Format.fprintf fmt "@\nwith metadata %a" pp_metadata x)) ast.metadata;
  pp_parameter_values fmt ast.parameters;
  (pp_no_empty_list2 pp_decl_) fmt ast.decls;
  (pp_no_empty_list2 pp_fun_) fmt ast.funs;
  Format.fprintf fmt "@."

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_ast (x : ast) = string_of__of_pp pp_ast x
