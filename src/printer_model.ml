(* open Location *)
open Tools
open Model
open Printer_tools

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_btyp fmt = function
  | Bunit         -> Format.fprintf fmt "unit"
  | Bbool         -> Format.fprintf fmt "bool"
  | Bint          -> Format.fprintf fmt "int"
  | Brational     -> Format.fprintf fmt "rational"
  | Bdate         -> Format.fprintf fmt "date"
  | Bduration     -> Format.fprintf fmt "duration"
  | Btimestamp    -> Format.fprintf fmt "timestamp"
  | Bstring       -> Format.fprintf fmt "string"
  | Baddress      -> Format.fprintf fmt "address"
  | Btez          -> Format.fprintf fmt "tez"
  | Bsignature    -> Format.fprintf fmt "signature"
  | Bkey          -> Format.fprintf fmt "key"
  | Bkeyhash      -> Format.fprintf fmt "key_hash"
  | Bbytes        -> Format.fprintf fmt "bytes"
  | Bnat          -> Format.fprintf fmt "nat"
  | Bchainid      -> Format.fprintf fmt "chain_id"
  | Bbls12_381_fr -> Format.fprintf fmt "bls12_381_fr"
  | Bbls12_381_g1 -> Format.fprintf fmt "bls12_381_g1"
  | Bbls12_381_g2 -> Format.fprintf fmt "bls12_381_g2"
  | Bnever        -> Format.fprintf fmt "never"
  | Bchest        -> Format.fprintf fmt "chest"
  | Bchest_key    -> Format.fprintf fmt "chest_key"

let pp_container fmt = function
  | Collection     -> Format.fprintf fmt "collection"
  | Aggregate      -> Format.fprintf fmt "aggregate"
  | Partition      -> Format.fprintf fmt "partition"
  | AssetContainer -> Format.fprintf fmt "asset_container"
  | AssetKey       -> Format.fprintf fmt "asset_key"
  | AssetValue     -> Format.fprintf fmt "asset_value"
  | View           -> Format.fprintf fmt "asset_view"

let rec pp_type fmt t =
  let pp_ntype fmt nt =
    match nt with
    | Tasset an ->
      Format.fprintf fmt "%a" pp_mid an
    | Tstate ->
      Format.fprintf fmt "state"
    | Tenum en ->
      Format.fprintf fmt "%a" pp_mid en
    | Tbuiltin b -> pp_btyp fmt b
    | Tcontainer (t, c) ->
      Format.fprintf fmt "%a<%a>"
        pp_container c
        pp_type t
    | Tlist t ->
      Format.fprintf fmt "list<%a>"
        pp_type t
    | Toption t ->
      Format.fprintf fmt "option<%a>"
        pp_type t
    | Ttuple ts ->
      Format.fprintf fmt "%a"
        (pp_list " * " pp_type) ts
    | Tset k ->
      Format.fprintf fmt "set<%a>"
        pp_type k
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
    | Tor (l, r) ->
      Format.fprintf fmt "or<%a, %a>"
        pp_type l
        pp_type r
    | Trecord id ->
      Format.fprintf fmt "%a" pp_mid id
    | Tevent id ->
      Format.fprintf fmt "%a" pp_mid id
    | Tlambda (a, r) ->
      Format.fprintf fmt "(%a -> %a)" pp_type a pp_type r
    | Tunit ->
      Format.fprintf fmt "unit"
    | Toperation ->
      Format.fprintf fmt "operation"
    | Tcontract t ->
      Format.fprintf fmt "contract<%a>" pp_type t
    | Tticket t ->
      Format.fprintf fmt "ticket<%a>" pp_type t
    | Tsapling_state n -> Format.fprintf fmt "sapling_state(%i)" n
    | Tsapling_transaction n -> Format.fprintf fmt "sapling_transaction(%i)" n
  in
  match get_atype t with
  | Some a -> Format.fprintf fmt "(%a %a)" pp_ntype (get_ntype t) pp_id a
  | None -> pp_ntype fmt (get_ntype t)

let pp_operator fmt op =
  let to_str = function
    | ValueAssign -> ":="
    | PlusAssign -> "+="
    | MinusAssign -> "-="
    | MultAssign -> "*="
    | DivAssign -> "/="
    | AndAssign -> "&="
    | OrAssign -> "|="
  in
  pp_str fmt (to_str op)

let pp_pattern fmt (p : pattern) =
  match p.node with
  | Pconst (i, []) -> pp_mid fmt i
  | Pconst (i, xs) -> Format.fprintf fmt "%a (%a)" pp_mid i (pp_list ", " pp_id) xs
  | Pwild -> pp_str fmt "_"

let pp_sort_kind fmt = function
  | SKasc -> pp_str fmt "asc"
  | SKdesc -> pp_str fmt "desc"

let pp_container_kind f fmt = function
  | CKcoll               -> Format.fprintf fmt "_Coll_"
  | CKview mt            -> f fmt mt
  | CKfield (an, fn, mt) -> Format.fprintf fmt "CKfield (%s, %s, %a)" an fn f mt

let pp_iter_container_kind f fmt = function
  | ICKcoll  an         -> Format.fprintf fmt "%a" pp_str an
  | ICKview  mt         -> Format.fprintf fmt "%a" f mt
  | ICKfield (_, _, mt) -> Format.fprintf fmt "%a" f mt
  | ICKset   mt         -> Format.fprintf fmt "%a" f mt
  | ICKlist  mt         -> Format.fprintf fmt "%a" f mt
  | ICKmap   mt         -> Format.fprintf fmt "%a" f mt

let pp_transfer_kind f fmt = function
  | TKsimple (x, d)         -> Format.fprintf fmt "transfer %a to %a" f x f d
  | TKcall (x, id, _, d, a) -> Format.fprintf fmt "transfer %a to %a call %s(%a)" f x f d id f a
  | TKentry (x, e, a)       -> Format.fprintf fmt "transfer %a to entry %a(%a)" f x f e f a
  | TKgen (x, cn, en, _t, a, args) -> Format.fprintf fmt "transfer %a to entry %s(%a).%s(%a)" f x cn f a en f args
  | TKself (x, id, args)    -> Format.fprintf fmt "transfer %a to entry self.%a(%a)" f x pp_str id (pp_list ", " (fun fmt (id, x) -> Format.fprintf fmt "%s = %a" id f x)) args
  | TKoperation x           -> Format.fprintf fmt "transfer %a" f x

let pp_assign_kind f fmt = function
  | Avar k                -> pp_mid fmt k
  | Avarstore l           -> Format.fprintf fmt "s.%a" pp_mid l
  | Aasset (an, fn, k)    -> Format.fprintf fmt "%a[%a].%a" pp_mid an f k pp_mid fn
  | Arecord (lv, _rn, fn) -> Format.fprintf fmt "%a.%a" f lv pp_mid fn
  | Atuple (lv, n, l)     -> Format.fprintf fmt "%a[%d/%d]" f lv n l
  | Astate                -> Format.fprintf fmt "state"
  | Aoperations           -> Format.fprintf fmt "operations"

let pp_dk fmt = function
  | DK_option (_, id) -> Format.pp_print_string fmt id
  | DK_map (_, id, k) -> Format.fprintf fmt "%a[%a]" Format.pp_print_string id pp_mterm k

let pp_mterm fmt (mt : mterm) =
  let rec f fmt (mtt : mterm) =
    match mtt.node with
    (* lambda *)

    | Mletin (ids, a, t, b, o) ->
      Format.fprintf fmt "let %a%a = %a in@\n%a%a"
        (pp_list ", " pp_mid) ids
        (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t
        (fun fmt x -> match x with | LVsimple v -> f fmt v | LVreplace (id, _ty, x) -> Format.fprintf fmt "replace %a : %a" pp_mid id f x) a
        f b
        (pp_option (fun fmt -> Format.fprintf fmt " otherwise %a" f)) o

    | Mdeclvar (ids, t, v, c) ->
      Format.fprintf fmt "%s %a%a = %a"
        (if c then "const" else "var")
        (pp_list ", " pp_mid) ids
        (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t
        f v

    | Mdeclvaropt (ids, t, v, fa, c) ->
      Format.fprintf fmt "%s %a%a ?= %a%a"
        (if c then "const" else "var")
        (pp_list ", " pp_mid) ids
        (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t
        f v
        (pp_option (fun fmt x -> Format.fprintf fmt " : %a" f x)) fa

    | Mapp (e, args) ->
      let pp fmt (e, args) =
        Format.fprintf fmt "%a (%a)"
          pp_mid e
          (pp_list ", " f) args
      in
      pp fmt (e, args)


    (* assign *)

    | Massign (op, _, Avar k, v) ->
      Format.fprintf fmt "%a %a %a"
        pp_mid k
        pp_operator op
        f v

    | Massign (op, _, Avarstore l, r) ->
      Format.fprintf fmt "s.%a %a %a"
        pp_mid l
        pp_operator op
        f r

    | Massign (op, _, Aasset (an, fn, k), v) ->
      Format.fprintf fmt "%a[%a].%a %a %a"
        pp_mid an
        f k
        pp_mid fn
        pp_operator op
        f v

    | Massign (op, _, Arecord (lv, _rn, fn), v) ->
      Format.fprintf fmt "%a.%a %a %a"
        f lv
        pp_mid fn
        pp_operator op
        f v

    | Massign (op, _, Atuple (lv, i, l), v) ->
      Format.fprintf fmt "%a[%d/%d] %a %a"
        f lv
        i
        l
        pp_operator op
        f v

    | Massign (_op, _, Astate, x) ->
      Format.fprintf fmt "state = %a"
        f x

    | Massign (_op, _, Aoperations, v) ->
      Format.fprintf fmt "operations = %a"
        f v

    | Massignopt (_op, _, _, v, fa) ->
      Format.fprintf fmt "TODO ?:= %a : %a"
        f v
        f fa


    (* control *)

    | Mif (c, t, e) ->
      Format.fprintf fmt "if %a@\nthen @[<v 2>%a@]%a"
        f c
        f t
        (pp_option (fun fmt -> Format.fprintf fmt "@\nelse @[<v 2>%a@]" f)) e

    | Mmatchwith (e, l) ->
      let pp fmt (e, l) =
        Format.fprintf fmt "match %a with@\n  @[%a@]"
          f e
          (pp_list "@\n" (fun fmt (p, x) ->
               Format.fprintf fmt "| %a -> %a"
                 pp_pattern p
                 f x
             )) l
      in
      pp fmt (e, l)

    | Minstrmatchoption (x, i, ve, ne) ->
      let pp fmt (x, i, ve, ne) =
        Format.fprintf fmt "match %a with@\n| some (%a) -> @[%a@]@\n| none -> @[%a@]@\nend"
          f x
          pp_mid i
          f ve
          f ne
      in
      pp fmt (x, i, ve, ne)

    | Minstrmatchor (x, lid, le, rid, re) ->
      let pp fmt (x, lid, le, rid, re) =
        Format.fprintf fmt "match %a with@\n  | left (%a) -> (@[%a@])@\n  | right (%a) -> (@[%a@])@\nend"
          f x
          pp_mid lid
          f le
          pp_mid rid
          f re
      in
      pp fmt (x, lid, le, rid, re)

    | Minstrmatchlist (x, hid, tid, hte, ee) ->
      let pp fmt (x, hid, tid, hte, ee) =
        Format.fprintf fmt "match %a with@\n  | %a::%a -> (@[%a@])@\n  | [] -> (@[%a@])@\nend"
          f x
          pp_mid hid
          pp_mid tid
          f hte
          f ee
      in
      pp fmt (x, hid, tid, hte, ee)

    | Minstrmatchdetach (dk, i, ve, ne) ->
      let pp fmt (dk, i, ve, ne) =
        Format.fprintf fmt "match_detach %a with@\n| some (%a) -> @[%a@]@\n| none -> @[%a@]@\nend"
          pp_dk dk
          pp_mid i
          f ve
          f ne
      in
      pp fmt (dk, i, ve, ne)

    | Mfor (i, c, b) ->
      Format.fprintf fmt "for %a in %a do@\n  @[%a@]@\ndone"
        (fun fmt i -> match i with FIsimple x -> pp_mid fmt x | FIdouble (x, y) -> Format.fprintf fmt "(%a, %a)" pp_mid x pp_mid y) i
        (pp_iter_container_kind f) c
        f b

    | Miter (i, a, b, c, s) ->
      Format.fprintf fmt "iter %a from %a to %a%s do@\n  @[%a@]@\ndone"
        pp_mid i
        f a
        f b
        (if s then " (excluded)" else "")
        f c

    | Mwhile (c, b) ->
      Format.fprintf fmt "while %a do@\n  @[%a@]@\ndone"
        f c
        f b

    | Mseq is -> begin
        match is with
        | [] -> Format.fprintf fmt "(nops)"
        | _ ->
          Format.fprintf fmt "%a"
            (pp_list ";@\n" f) is
      end

    | Mreturn x ->
      Format.fprintf fmt "return %a"
        f x


    (* effect *)

    | Mfail ft ->
      let pp_fail_type f fmt a =
        let pp x = Format.fprintf fmt x in
        match a with
        | Invalid e              -> f fmt e
        | InvalidCaller          -> pp "\"%s\"" fail_msg_INVALID_CALLER
        | InvalidSource          -> pp "\"%s\"" fail_msg_INVALID_SOURCE
        | InvalidCondition (id, x) -> (match x with | None -> pp "\"%s\", %a" fail_msg_INVALID_CONDITION pp_str id | Some e -> f fmt e)
        | NotFound               -> pp "\"%s\"" fail_msg_NOT_FOUND
        | AssetNotFound id       -> pp "\"%s\", %a" fail_msg_ASSET_NOT_FOUND pp_str id
        | KeyExists id           -> pp "\"%s\", %a" fail_msg_KEY_EXISTS pp_str id
        | KeyExistsOrNotFound id -> pp "\"%s\", %a" fail_msg_KEY_EXISTS_OR_NOT_FOUND pp_str id
        | DivByZero              -> pp "\"%s\"" fail_msg_DIV_BY_ZERO
        | NatNegAssign           -> pp "\"%s\"" fail_msg_NAT_NEG_ASSIGN
        | NoTransfer             -> pp "\"%s\"" fail_msg_NO_TRANSFER
        | InvalidState           -> pp "\"%s\"" fail_msg_INVALID_STATE
      in
      Format.fprintf fmt "fail (%a)"
        (pp_fail_type f) ft

    | Mfailexpr v ->
      Format.fprintf fmt "fail_expr (%a)"
        f v

    | Mfailsome v ->
      Format.fprintf fmt "fail_some (%a)"
        f v

    | Mtransfer tr -> pp_transfer_kind f fmt tr

    | Memit (e, x) ->
      Format.fprintf fmt "emit<%a>(%a)"
        pp_mid e
        f x

    | Msandboxexec (a, b, c) ->
      Format.fprintf fmt "sandbox_exec(%a, %a, %a)"
        f a
        f b
        f c

    | Mdetach (id, dk, _ty, fa) ->
      Format.fprintf fmt "detach %a as %a : %a"
        pp_dk dk
        pp_mid id
        f fa

    | Mmicheline micheline -> begin
        let printable_micheline : Micheline_printer.node = Micheline_tools.obj_to_micheline micheline in
        Format.fprintf fmt "michelson @[%a@]" Micheline_printer.print_expr printable_micheline
      end

    (* entrypoint *)

    | Mgetentrypoint (t, a, s) ->
      Format.fprintf fmt "get_entrypoint<%a>(%a, %a)"
        pp_type t
        pp_mid a
        f s

    | Mcallview (t, a, b, c) ->
      Format.fprintf fmt "call_view<%a>(%a, %a, %a)"
        pp_type t
        f a
        pp_mid b
        f c

    | Mimportcallview (t, a, b, c) ->
      Format.fprintf fmt "import_call_view<%a>(%a, %a, %a)"
        pp_type t
        f a
        pp_mid b
        f c

    | Mself id ->
      Format.fprintf fmt "self.%a"
        pp_mid id

    | Mselfcallview (_t, id, args) ->
      Format.fprintf fmt "self.%a(%a)"
        pp_id id
        (pp_list ", " f) args


    (* operation *)

    | Moperations ->
      Format.fprintf fmt "operations"

    | Mmakeoperation (v, d, a) ->
      Format.fprintf fmt "make_operation(%a, %a, %a)"
        f v
        f d
        f a

    | Mmakeevent (t, id, a) ->
      Format.fprintf fmt "make_event<%a>(%a, %a)"
        pp_type t
        pp_mid id
        f a

    | Mcreatecontract (cc, d, a) ->
      let pp_create_contract fmt = function
        | CCTz (_, arg) -> Format.fprintf fmt "Tz(%a)" f arg
        | CCArl (id, args) ->
          Format.fprintf fmt "Arl(%a, [%a])"
            pp_ident id
            (pp_list ";" (fun fmt (id, v) -> Format.fprintf fmt "%a = %a" pp_ident id f v)) args
      in
      Format.fprintf fmt "create_contract(%a, %a, %a)"
        pp_create_contract cc
        f d
        f a

    (* literals *)

    | Mint v -> Format.fprintf fmt "%ai" pp_big_int v
    | Mnat v -> pp_big_int fmt v
    | Mbool b -> pp_str fmt (if b then "true" else "false")
    | Mrational (n, d) ->
      Format.fprintf fmt "rat(%a, %a)"
        pp_big_int n
        pp_big_int d
    | Mstring v ->
      Format.fprintf fmt "\"%a\""
        pp_str v
    | Mmutez v ->
      Format.fprintf fmt "%autz"
        pp_big_int v
    | Maddress v -> pp_str fmt v
    | Mdate v -> Core.pp_date fmt v
    | Mduration v -> Core.pp_duration_for_printer fmt v
    | Mtimestamp v -> pp_big_int fmt v
    | Mbytes v     -> Format.fprintf fmt "0x%s" v
    | Mchain_id v  -> Format.fprintf fmt "\"%a\"" pp_str v
    | Mkey v       -> Format.fprintf fmt "\"%a\"" pp_str v
    | Mkey_hash v  -> Format.fprintf fmt "\"%a\"" pp_str v
    | Msignature v -> Format.fprintf fmt "\"%a\"" pp_str v
    | Mbls12_381_fr v -> Format.fprintf fmt "0x%sfr" v
    | Mbls12_381_fr_n v -> Format.fprintf fmt "%afr" pp_big_int v
    | Mbls12_381_g1 v -> Format.fprintf fmt "0x%sg1" v
    | Mbls12_381_g2 v -> Format.fprintf fmt "0x%sg2" v
    | Munit -> Format.fprintf fmt "Unit"
    | MsaplingStateEmpty _ -> Format.fprintf fmt "0x00"
    | MsaplingTransaction (_, v) -> Format.fprintf fmt "0x%s" v
    | Mchest v -> Format.fprintf fmt "0x%s" v
    | Mchest_key v -> Format.fprintf fmt "0x%s" v
    | Mtz_expr v -> Format.fprintf fmt "%s" v

    (* control expression *)

    | Mexprif (c, t, e) ->
      Format.fprintf fmt "if %a@\nthen @[<v 2>%a@]@\nelse @[<v 2>%a@]"
        f c
        f t
        f e

    | Mexprmatchwith (e, l) ->
      let pp fmt (e, l) =
        Format.fprintf fmt "match %a with@\n  @[%a@]"
          f e
          (pp_list "@\n" (fun fmt (p, x) ->
               Format.fprintf fmt "| %a -> %a"
                 pp_pattern p
                 f x
             )) l
      in
      pp fmt (e, l)

    | Mmatchoption (x, i, ve, ne) ->
      let pp fmt (x, i, ve, ne) =
        Format.fprintf fmt "match %a with@\n| some (%a) -> @[%a@]@\n| none -> @[%a@]"
          f x
          pp_mid i
          f ve
          f ne
      in
      pp fmt (x, i, ve, ne)

    | Mmatchor (x, lid, le, rid, re) ->
      let pp fmt (x, lid, le, rid, re) =
        Format.fprintf fmt "match %a with@\n  | left (%a) -> (@[%a@])@\n  | right (%a) -> (@[%a@])@\nend"
          f x
          pp_mid lid
          f le
          pp_mid rid
          f re
      in
      pp fmt (x, lid, le, rid, re)

    | Mmatchlist (x, hid, tid, hte, ee) ->
      let pp fmt (x, hid, tid, hte, ee) =
        Format.fprintf fmt "match %a with@\n  | %a::%a -> (@[%a@])@\n  | [] -> (@[%a@])@\nend"
          f x
          pp_mid hid
          pp_mid tid
          f hte
          f ee
      in
      pp fmt (x, hid, tid, hte, ee)

    | Mternarybool (c, a, b) ->
      Format.fprintf fmt "%a ? %a : %a"
        f c
        f a
        f b

    | Mternaryoption (c, a, b) ->
      Format.fprintf fmt "%a ? %a : %a"
        f c
        f a
        f b

    | Mfold (x, id, e) ->
      let pp fmt (x, id, e) =
        Format.fprintf fmt "fold (%a, %a -> (@[%a@]))@\n"
          f x
          pp_mid id
          f e
      in
      pp fmt (x, id, e)

    | Mmap (x, id, e) ->
      let pp fmt (x, id, e) =
        Format.fprintf fmt "map (%a, %a -> (@[%a@]))"
          f x
          pp_mid id
          f e
      in
      pp fmt (x, id, e)

    | Mexeclambda (l, a) ->
      let pp fmt (l, a) =
        Format.fprintf fmt "exec_lambda (%a, %a)"
          f l
          f a
      in
      pp fmt (l, a)

    | Mapplylambda (l, a) ->
      let pp fmt (l, a) =
        Format.fprintf fmt "apply_lambda (%a, %a)"
          f l
          f a
      in
      pp fmt (l, a)


    (* composite type constructors *)

    | Mleft (t, x) -> Format.fprintf fmt "left<%a>(%a)" pp_type t f x

    | Mright (t, x) -> Format.fprintf fmt "right<%a>(%a)" pp_type t f x

    | Mnone -> pp_str fmt "None"

    | Msome v ->
      Format.fprintf fmt "Some (%a)"
        f v

    | Mtuple l ->
      Format.fprintf fmt "(%a)"
        (pp_list ", " f) l

    | Masset l ->
      Format.fprintf fmt "{%a}"
        (pp_list "; " f) l

    | Massets l ->
      Format.fprintf fmt "[%a]"
        (pp_list "; " f) l

    | Mlitset l ->
      Format.fprintf fmt "set(%a)"
        (pp_list "; " f) l

    | Mlitlist l ->
      Format.fprintf fmt "list(%a)"
        (pp_list "; " f) l

    | Mlitmap (k, l) ->
      let str_map_kind = function
        | MKMap -> "map"
        | MKBigMap -> "big_map"
        | MKIterableBigMap -> "iterable_big_map"
      in
      Format.fprintf fmt "%s%a(%a)"
        (str_map_kind k)
        (fun fmt ty -> match get_ntype ty with | Tmap(k, v) | Tbig_map(k, v) | Titerable_big_map(k, v) -> Format.fprintf fmt "<%a, %a>" pp_type k pp_type v| _ -> ()) mtt.type_
        (pp_list "; " (fun fmt (k, v) -> Format.fprintf fmt "%a : %a"
                          f k
                          f v)) l

    | Mlitrecord l ->
      Format.fprintf fmt "record(%a)"
        (pp_list "; " (fun fmt (k, v) -> Format.fprintf fmt "%s = %a"
                          k
                          f v)) l

    | Mlitevent l ->
      Format.fprintf fmt "event(%a)"
        (pp_list "; " (fun fmt (k, v) -> Format.fprintf fmt "%s = %a"
                          k
                          f v)) l

    | Mlambda (rt, id, at, e) ->
      let pp fmt (rt, id, at, e) =
        Format.fprintf fmt "lambda<%a>((%a : %a) -> @[%a@])"
          pp_type rt
          pp_mid id
          pp_type at
          f e
      in
      pp fmt (rt, id, at, e)

    | Mlambda_michelson (it, rt, body) ->
      let pp fmt (it, rt, body) =
        Format.fprintf fmt "lambda_michelson<%a, %a>(@[%a@])"
          pp_type it
          pp_type rt
          Micheline_printer.print_expr (Micheline_tools.obj_to_micheline body)
      in
      pp fmt (it, rt, body)

    | Mmicheline_expr (t, m, a) ->
      let pp fmt (t, m, a) =
        Format.fprintf fmt "michelson<%a> @[%a@] [%a]"
          pp_type t
          Micheline_printer.print_expr (Micheline_tools.obj_to_micheline m)
          (pp_list " : " f) a
      in
      pp fmt (t, m, a)

    (* access *)

    | Mdot (e, i) ->
      Format.fprintf fmt "%a.%a"
        f e
        pp_mid i

    | Mdotassetfield (an, k, fn) ->
      Format.fprintf fmt "%a[%a].%a"
        pp_mid an
        f k
        pp_mid fn

    | Mquestionoption (a, fn) ->
      Format.fprintf fmt "%a?.%a"
        f a
        pp_mid fn


    (* comparison operators *)

    | Mequal (t, l, r) ->
      let pp fmt (_t, l, r) =
        Format.fprintf fmt "%a = %a"
          f l
          f r
      in
      pp fmt (t, l, r)

    | Mnequal (t, l, r) ->
      let pp fmt (_t, l, r) =
        Format.fprintf fmt "%a <> %a"
          f l
          f r
      in
      pp fmt (t, l, r)

    | Mgt (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a > %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mge (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a >= %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mlt (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a < %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mle (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a <= %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mmulticomp (e, l) ->
      let pp fmt (e, l) =
        let pp_item fmt (op, e) =
          Format.fprintf fmt "%a %a"
            pp_comparison_operator op
            f e
        in
        Format.fprintf fmt "%a %a"
          f e
          (pp_list " " pp_item) l
      in
      pp fmt (e, l)


    (* arithmetic operators *)

    | Mand (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a and %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mor (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a or %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mgreedyand (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "greedy_and(%a, %a)"
          f l
          f r
      in
      pp fmt (l, r)

    | Mgreedyor (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "greedy_or(%a, %a)"
          f l
          f r
      in
      pp fmt (l, r)

    | Mxor (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a xor %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mnot e ->
      let pp fmt e =
        Format.fprintf fmt "not %a"
          f e
      in
      pp fmt e

    | Mplus (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a + %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mminus (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a - %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mmult (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a * %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mdivrat (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a / %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mdiveuc (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a div %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mmodulo (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a %% %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mdivmod (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a /%% %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Muminus e ->
      let pp fmt e =
        Format.fprintf fmt "-%a"
          f e
      in
      pp fmt e

    | MthreeWayCmp (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a <=> %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mshiftleft (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a << %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mshiftright (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a >> %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Msubnat (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "sub_nat(%a, %a)"
          f l
          f r
      in
      pp fmt (l, r)

    | Msubmutez (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "sub_mutez(%a, %a)"
          f l
          f r
      in
      pp fmt (l, r)


    (* asset api effect *)

    | Maddasset (an, i) ->
      let pp fmt (an, i) =
        Format.fprintf fmt "add_%a (%a)"
          pp_str an
          f i
      in
      pp fmt (an, i)

    | Mputsingleasset (an, i) ->
      let pp fmt (an, i) =
        Format.fprintf fmt "put_%a (%a)"
          pp_str an
          f i
      in
      pp fmt (an, i)

    | Mputasset (an, k, v) ->
      let pp fmt (an, k, v) =
        Format.fprintf fmt "put_%a (%a, %a)"
          pp_str an
          f k
          f v
      in
      pp fmt (an, k, v)

    | Maddfield (an, fn, c, i) ->
      let pp fmt (an, fn, c, i) =
        Format.fprintf fmt "add_%a_%a (%a, %a)"
          pp_str an
          pp_str fn
          f c
          f i
      in
      pp fmt (an, fn, c, i)

    | Mremoveasset (an, i) ->
      let pp fmt (an, i) =
        Format.fprintf fmt "remove_%a (%a)"
          pp_str an
          f i
      in
      pp fmt (an, i)

    | Mremovefield (an, fn, c, i) ->
      let pp fmt (an, fn, c, i) =
        Format.fprintf fmt "remove_%a_%a (%a, %a)"
          pp_str an
          pp_str fn
          f c
          f i
      in
      pp fmt (an, fn, c, i)

    | Mremoveall (an, v) ->
      let pp fmt (an, v) =
        Format.fprintf fmt "remove_all_%a (%a)"
          pp_str an
          (pp_container_kind f) v
      in
      pp fmt (an, v)

    | Mremoveif (an, c, la, lb, a) ->
      let pp fmt (an, c, la, lb, a) =
        Format.fprintf fmt "remove_if_%a (%a, (%a) -> %a)(%a)"
          pp_str an
          (pp_container_kind f) c
          (pp_list ", " (fun fmt (id, t) -> Format.fprintf fmt "%s : %a" id pp_type t)) la
          f lb
          (pp_list ", " f) a
      in
      pp fmt (an, c, la, lb, a)

    | Mclear (an, v) ->
      let pp fmt (an, v) =
        Format.fprintf fmt "clear_%a (%a)"
          pp_str an
          (pp_container_kind f) v
      in
      pp fmt (an, v)

    | Mset (c, l, k, v) ->
      let pp fmt (c, _l, k, v) =
        Format.fprintf fmt "set_%a (%a, %a)"
          pp_str c
          f k
          f v
      in
      pp fmt (c, l, k, v)

    | Mupdate (an, k, l) ->
      let pp fmt (an, k, l) =
        Format.fprintf fmt "update_%a (%a, {%a})"
          pp_str an
          f k
          (pp_list "; " (fun fmt (id, op, v) -> Format.fprintf fmt "%a %a %a" pp_mid id pp_operator op f v)) l
      in
      pp fmt (an, k, l)

    | Mupdateall (an, c, l) ->
      let pp fmt (an, c, l) =
        Format.fprintf fmt "update_all_%a (%a, {%a})"
          (pp_container_kind f) c
          pp_str an
          (pp_list "; " (fun fmt (id, op, v) -> Format.fprintf fmt "%a %a %a" pp_mid id pp_operator op f v)) l
      in
      pp fmt (an, c, l)

    | Maddupdate (an, c, k, l) ->
      let pp fmt (an, c, k, l) =
        Format.fprintf fmt "add_update_%a (%a, %a, {%a})"
          pp_str an
          (pp_container_kind f) c
          f k
          (pp_list "; " (fun fmt (id, op, v) -> Format.fprintf fmt "%a %a %a" pp_mid id pp_operator op f v)) l
      in
      pp fmt (an, c, k, l)

    | Mputremove (an, c, k, v) ->
      let pp fmt (an, c, k, v) =
        Format.fprintf fmt "put_remove_%a (%a, %a, %a)"
          pp_str an
          (pp_container_kind f) c
          f k
          f v
      in
      pp fmt (an, c, k, v)

    (* asset api expression *)

    | Mget (an, c, k) ->
      let pp fmt (an, k) =
        Format.fprintf fmt "get_%a (%a, %a)"
          pp_str an
          (pp_container_kind f) c
          f k
      in
      pp fmt (an, k)

    | Mgetsome (an, c, k) ->
      let pp fmt (an, k) =
        Format.fprintf fmt "get_some_%a (%a, %a)"
          pp_str an
          (pp_container_kind f) c
          f k
      in
      pp fmt (an, k)

    | Mselect (an, c, la, lb, a) ->
      let pp fmt (an, c, la, lb, a) =
        Format.fprintf fmt "select_%a (%a, (%a) -> %a)(%a)"
          pp_str an
          (pp_container_kind f) c
          (pp_list ", " (fun fmt (id, t) -> Format.fprintf fmt "%s : %a" id pp_type t)) la
          f lb
          (pp_list ", " f) a
      in
      pp fmt (an, c, la, lb, a)

    | Msort (an, c, l) ->
      let pp fmt (an, c, l) =
        Format.fprintf fmt "sort_%a (%a, %a)"
          pp_str an
          (pp_container_kind f) c
          (pp_list ", " (fun fmt (a, b) -> Format.fprintf fmt "%a(%a)" pp_sort_kind b pp_ident a)) l
      in
      pp fmt (an, c, l)

    | Mcontains (an, c, i) ->
      let pp fmt (an, c, i) =
        Format.fprintf fmt "contains_%a (%a, %a)"
          pp_str an
          (pp_container_kind f) c
          f i
      in
      pp fmt (an, c, i)

    | Mnth (an, c, i) ->
      let pp fmt (an, c, i) =
        Format.fprintf fmt "nth_%a (%a, %a)"
          pp_str an
          (pp_container_kind f) c
          f i
      in
      pp fmt (an, c, i)

    | Mcount (an, c) ->
      let pp fmt (an, c) =
        Format.fprintf fmt "count_%a (%a)"
          pp_str an
          (pp_container_kind f) c
      in
      pp fmt (an, c)

    | Msum (an, c, p) ->
      let pp fmt (an, c, p) =
        Format.fprintf fmt "sum_%a (%a, %a)"
          pp_str an
          (pp_container_kind f) c
          f p
      in
      pp fmt (an, c, p)

    | Mhead (an, c, i) ->
      Format.fprintf fmt "head_%a (%a, %a)"
        pp_str an
        (pp_container_kind f) c
        f i

    | Mtail (an, c, i) ->
      Format.fprintf fmt "tail_%a (%a, %a)"
        pp_str an
        (pp_container_kind f) c
        f i


    (* utils *)

    | Mcast (src, dst, v) ->
      let pp fmt (src, dst, v) =
        Format.fprintf fmt "cast(%a, %a, %a)"
          pp_type src
          pp_type dst
          f v
      in
      pp fmt (src, dst, v)

    | Mtupleaccess (x, k) ->
      let pp fmt (x, k) =
        Format.fprintf fmt "%a[%a]"
          f x
          pp_big_int k
      in
      pp fmt (x, k)

    | Mrecupdate (x, l) ->
      let pp fmt (x, l) =
        Format.fprintf fmt "{ %a with %a }"
          f x
          (pp_list " " (fun fmt (i, v) -> Format.fprintf fmt "%s = %a" i f v)) l
      in
      pp fmt (x, l)

    | Mmakeasset (an, k, v) ->
      let pp fmt (an, k, v) =
        Format.fprintf fmt "make_asset<%s>(%a, %a)"
          an
          f k
          f v
      in
      pp fmt (an, k, v)

    | Mtocontainer an ->
      let pp fmt an =
        Format.fprintf fmt "%s.to_container()"
          an
      in
      pp fmt an

    | Mglobal_constant (t, v) ->
      let pp fmt v =
        Format.fprintf fmt "global_constant<%a>(%a)"
          pp_type t
          f v
      in
      pp fmt v

    (* set api expression *)

    | Msetadd (t, c, a) ->
      Format.fprintf fmt "set_%a_add (%a, %a)"
        pp_type t
        f c
        f a

    | Msetremove (t, c, a) ->
      Format.fprintf fmt "set_%a_remove (%a, %a)"
        pp_type t
        f c
        f a

    | Msetupdate (t, c, b, v) ->
      Format.fprintf fmt "set_%a_update (%a, %a, %a)"
        pp_type t
        f c
        f b
        f v

    | Msetcontains (t, c, a) ->
      Format.fprintf fmt "set_%a_contains (%a, %a)"
        pp_type t
        f c
        f a

    | Msetlength (t, c) ->
      Format.fprintf fmt "set_%a_length (%a)"
        pp_type t
        f c

    | Msetfold (t, ix, ia, c, a, b) ->
      Format.fprintf fmt "set_%a_fold (%a, %a, (%a, %a) ->@\n  @[%a@])"
        pp_type t
        f c
        f a
        pp_mid ix
        pp_mid ia
        f b


    (* set api instruction *)

    | Msetinstradd (_, ak, a) ->
      Format.fprintf fmt "%a.add (%a)"
        (pp_assign_kind f) ak
        f a

    | Msetinstrremove (_, ak, a) ->
      Format.fprintf fmt "%a.remove (%a)"
        (pp_assign_kind f) ak
        f a


    (* list api expression *)

    | Mlistprepend (_, c, a) ->
      Format.fprintf fmt "list_prepend (%a, %a)"
        f c
        f a

    | Mlistlength (_, c) ->
      Format.fprintf fmt "list_length (%a)"
        f c

    | Mlistcontains (_, c, a) ->
      Format.fprintf fmt "list_contains (%a, %a)"
        f c
        f a

    | Mlistnth (_, c, a) ->
      Format.fprintf fmt "list_nth (%a, %a)"
        f c
        f a

    | Mlisthead (_, c, a) ->
      Format.fprintf fmt "list_head (%a, %a)"
        f c
        f a

    | Mlisttail (_, c, a) ->
      Format.fprintf fmt "list_tail (%a, %a)"
        f c
        f a

    | Mlistreverse (_, l) ->
      Format.fprintf fmt "list_reverse (%a)"
        f l

    | Mlistconcat (_, l, m) ->
      Format.fprintf fmt "list_concat (%a, %a)"
        f l
        f m

    | Mlistfold (t, ix, ia, c, a, b) ->
      Format.fprintf fmt "list_%a_fold (%a, %a, (%a, %a) ->@\n  @[%a@])"
        pp_type t
        f c
        f a
        pp_mid ix
        pp_mid ia
        f b


    (* list api instruction *)

    | Mlistinstrprepend (_, ak, a) ->
      Format.fprintf fmt "%a.prepend (%a)"
        (pp_assign_kind f) ak
        f a

    | Mlistinstrconcat (_, ak, a) ->
      Format.fprintf fmt "%a.concat (%a)"
        (pp_assign_kind f) ak
        f a


    (* map api expression *)

    | Mmapput (_, _, _, c, k, v) ->
      Format.fprintf fmt "map_put (%a, %a, %a)"
        f c
        f k
        f v

    | Mmapremove (_, _, _, c, k) ->
      Format.fprintf fmt "map_remove (%a, %a)"
        f c
        f k

    | Mmapupdate (_, _, _, c, k, v) ->
      Format.fprintf fmt "map_update (%a, %a, %a)"
        f c
        f k
        f v

    | Mmapget (_, _, _, c, k, _) ->
      Format.fprintf fmt "map_get (%a, %a)"
        f c
        f k

    | Mmapgetopt (_, _, _, c, k) ->
      Format.fprintf fmt "map_getopt (%a, %a)"
        f c
        f k

    | Mmapcontains (_, _, _, c, k) ->
      Format.fprintf fmt "map_contains (%a, %a)"
        f c
        f k

    | Mmaplength (_, _, _, c) ->
      Format.fprintf fmt "map_length (%a)"
        f c

    | Mmapfold (_, t, ik, iv, ia, c, a, b) ->
      Format.fprintf fmt "map_%a_fold (%a, %a, (%a, (%a, %a)) ->@\n  @[%a@])"
        pp_type t
        f c
        f a
        pp_mid ia
        pp_mid ik
        pp_mid iv
        f b


    (* map api instruction *)

    | Mmapinstrput (_, _, _, ak, k, v) ->
      Format.fprintf fmt "%a.put(%a, %a)"
        (pp_assign_kind f) ak
        f k
        f v

    | Mmapinstrremove (_, _, _, ak, k) ->
      Format.fprintf fmt "%a.remove(%a)"
        (pp_assign_kind f) ak
        f k

    | Mmapinstrupdate (_, _, _, ak, k, v) ->
      Format.fprintf fmt "%a.update(%a, %a)"
        (pp_assign_kind f) ak
        f k
        f v


    (* builtin functions *)

    | Mmax (l, r) ->
      Format.fprintf fmt "max (%a, %a)"
        f l
        f r
    | Mmin (l, r) ->
      Format.fprintf fmt "min (%a, %a)"
        f l
        f r

    | Mabs a ->
      Format.fprintf fmt "abs (%a)"
        f a

    | Mconcat (x, y) ->
      Format.fprintf fmt "concat (%a, %a)"
        f x
        f y

    | Mconcatlist x ->
      Format.fprintf fmt "concat (%a)"
        f x

    | Mslice (x, s, e) ->
      Format.fprintf fmt "slice (%a, %a, %a)"
        f x
        f s
        f e

    | Mlength x ->
      Format.fprintf fmt "length (%a)"
        f x

    | Misnone x ->
      Format.fprintf fmt "is_none (%a)"
        f x

    | Missome x ->
      Format.fprintf fmt "is_some (%a)"
        f x

    | Minttonat x ->
      Format.fprintf fmt "int_to_nat (%a)"
        f x

    | Mfloor x ->
      Format.fprintf fmt "floor (%a)"
        f x

    | Mceil x ->
      Format.fprintf fmt "ceil (%a)"
        f x

    | Mnattostring x ->
      Format.fprintf fmt "nat_to_string (%a)"
        f x

    | Mbytestonat x ->
      Format.fprintf fmt "bytes_to_nat (%a)"
        f x

    | Mnattobytes x ->
      Format.fprintf fmt "nat_to_bytes (%a)"
        f x

    | Mbytestoint x ->
      Format.fprintf fmt "bytes_to_int (%a)"
        f x

    | Minttobytes x ->
      Format.fprintf fmt "int_to_bytes (%a)"
        f x

    | Mpack x ->
      Format.fprintf fmt "pack (%a)"
        f x

    | Munpack (t, x) ->
      Format.fprintf fmt "unpack<%a>(%a)"
        pp_type t
        f x

    | Msetdelegate x ->
      Format.fprintf fmt "set_delegate (%a)"
        f x

    | Mkeyhashtocontract x ->
      Format.fprintf fmt "key_hash_to_contract (%a)"
        f x

    | Mcontracttoaddress x ->
      Format.fprintf fmt "contract_to_address (%a)"
        f x

    | Maddresstocontract (t, x) ->
      Format.fprintf fmt "address_to_contract<%a>(%a)"
        pp_type t
        f x

    | Mkeytoaddress x ->
      Format.fprintf fmt "key_to_address (%a)"
        f x

    | Msimplify_rational x ->
      Format.fprintf fmt "simplify_rational (%a)"
        f x

    | Mget_numerator x ->
      Format.fprintf fmt "get_numerator (%a)"
        f x

    | Mget_denominator x ->
      Format.fprintf fmt "get_denominator (%a)"
        f x

    | Misimplicitaddress x ->
      Format.fprintf fmt "is_implicit_address (%a)"
        f x

    | Mexp_horner (x, s) ->
      Format.fprintf fmt "exp_horner (%a, %a)"
        f x
        f s


    (* crypto functions *)

    | Mblake2b x ->
      Format.fprintf fmt "blake2b (%a)"
        f x

    | Msha256 x ->
      Format.fprintf fmt "sha256 (%a)"
        f x

    | Msha512 x ->
      Format.fprintf fmt "sha512 (%a)"
        f x

    | Msha3 x ->
      Format.fprintf fmt "sha3 (%a)"
        f x

    | Mkeccak x ->
      Format.fprintf fmt "keccak (%a)"
        f x

    | Mkeytokeyhash x ->
      Format.fprintf fmt "key_to_key_hash (%a)"
        f x

    | Mchecksignature (k, s, x) ->
      Format.fprintf fmt "check_signature (%a, %a, %a)"
        f k
        f s
        f x


    (* voting *)

    | Mtotalvotingpower -> pp_str fmt "total_voting_power"

    | Mvotingpower x ->
      Format.fprintf fmt "voting_power (%a)"
        f x


    (* ticket *)

    | Mcreateticket (x, a) ->
      Format.fprintf fmt "create_ticket (%a, %a)"
        f x f a

    | Mreadticket x ->
      Format.fprintf fmt "read_ticket (%a)"
        f x

    | Msplitticket (x, a, b) ->
      Format.fprintf fmt "split_ticket (%a, %a, %a)"
        f x f a f b

    | Mjointickets (x, y) ->
      Format.fprintf fmt "join_tickets (%a, %a)"
        f x f y


    (* sapling *)

    | Msapling_empty_state n ->
      Format.fprintf fmt "sapling_empty_state (%i)" n

    | Msapling_verify_update (s, t) ->
      Format.fprintf fmt "sapling_verify_update (%a, %a)" f s f t


    (* bls curve *)

    | Mpairing_check x -> Format.fprintf fmt "pairing_check (%a)" f x


    (* timelock *)

    | Mopen_chest (x, y, z) -> Format.fprintf fmt "open_chest (%a, %a, %a)" f x f y f z


    (* constants *)

    | Mnow           -> pp_str fmt "now"
    | Mtransferred   -> pp_str fmt "transferred"
    | Mcaller        -> pp_str fmt "caller"
    | Mbalance       -> pp_str fmt "balance"
    | Msource        -> pp_str fmt "source"
    | Mselfaddress   -> pp_str fmt "self_address"
    | Mselfchainid   -> pp_str fmt "self_chain_id"
    | Mmetadata      -> pp_str fmt "metadata"
    | Mlevel         -> pp_str fmt "level"
    | Mminblocktime  -> pp_str fmt "min_block_time"


    (* variable *)

    | Mvar (an, Vassetstate k) -> Format.fprintf fmt "state_%a(%a)" pp_str (unloc_mident an) f k
    | Mvar(v, Vstorevar)       -> Format.fprintf fmt "s.%a" pp_mid v
    | Mvar(v, Vstorecol)       -> Format.fprintf fmt "%a" pp_mid v
    | Mvar(v, Vlocal)          -> Format.fprintf fmt "%a" pp_mid v
    | Mvar(v, Vparam)          -> Format.fprintf fmt "%a" pp_mid v
    | Mvar(v, Vfield)          -> Format.fprintf fmt "%a" pp_mid v
    | Mvar(_, Vthe)            -> Format.fprintf fmt "the"
    | Mvar(_, Vstate)          -> Format.fprintf fmt "state"
    | Mvar(v, Vparameter)      -> Format.fprintf fmt "%a" pp_mid v
    | Menumval (id, args, _e)        -> begin
        match args with
        | [] -> Format.fprintf fmt "%a" pp_mid id
        | _  -> Format.fprintf fmt "%a (%a)"
                  pp_mid id
                  (pp_list ", " f) args
      end

    (* rational *)

    | Mrateq (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "rat_eq (%a, %a)"
          f l
          f r
      in
      pp fmt (l, r)

    | Mratcmp (op, l, r) ->
      let pp fmt (op, l, r) =
        let to_str (c : comparison_operator) =
          match c with
          | Lt -> "lt"
          | Le -> "le"
          | Gt -> "gt"
          | Ge -> "ge"
        in
        let str_op = to_str op in
        Format.fprintf fmt "rat_cmp (%s, %a, %a)"
          str_op
          f l
          f r
      in
      pp fmt (op, l, r)

    | Mratarith (op, l, r) ->
      let pp fmt (op, l, r) =
        let to_str = function
          | Rplus  -> "plus"
          | Rminus -> "minus"
          | Rmult  -> "mult"
          | Rdiv   -> "div"
        in
        let str_op = to_str op in
        Format.fprintf fmt "rat_arith (%s, %a, %a)"
          str_op
          f l
          f r
      in
      pp fmt (op, l, r)

    | Mratuminus v ->
      let pp fmt v =
        Format.fprintf fmt "rat_uminus (%a)"
          f v
      in
      pp fmt v

    | Mrattez (c, t) ->
      let pp fmt (c, t) =
        Format.fprintf fmt "rat_tez (%a, %a)"
          f c
          f t
      in
      pp fmt (c, t)

    | Mnattoint e ->
      let pp fmt e =
        Format.fprintf fmt "nat_to_int (%a)"
          f e
      in
      pp fmt e

    | Mnattorat e ->
      let pp fmt e =
        Format.fprintf fmt "nat_to_rat (%a)"
          f e
      in
      pp fmt e

    | Minttorat e ->
      let pp fmt e =
        Format.fprintf fmt "int_to_rat (%a)"
          f e
      in
      pp fmt e

    | Mratdur (c, t) ->
      let pp fmt (c, t) =
        Format.fprintf fmt "rat_dur (%a, %a)"
          f c
          f t
      in
      pp fmt (c, t)


    (* others *)

    | Minttodate v ->
      let pp fmt v =
        Format.fprintf fmt "int_to_date (%a)"
          f v
      in
      pp fmt v

    | Mmuteztonat v ->
      let pp fmt v =
        Format.fprintf fmt "mutez_to_nat (%a)"
          f v
      in
      pp fmt v

  in
  f fmt mt

let pp_ck fmt = function
  | Coll  -> Format.fprintf fmt "collection"
  | View  -> Format.fprintf fmt "view"
  | Field (an, fn) -> Format.fprintf fmt "field(%s, %s)" an fn

let pp_api_asset fmt = function
  | Get an -> pp_str fmt ("get\t " ^ an)
  | Set an -> pp_str fmt ("set\t " ^ an)
  | Add an -> pp_str fmt ("add\t " ^ an)
  | Remove an              -> pp_str fmt ("remove\t " ^ an)
  | Clear (an, c)          -> Format.fprintf fmt "clear %s on %a" an pp_ck c
  | Update (an, l)         -> Format.fprintf fmt "update\t%a with %a" pp_str an (pp_list ", " (fun fmt (id, op, v) -> Format.fprintf fmt "%s %a %a)" id pp_assignment_operator op pp_mterm v)) l
  | FieldAdd (an, fn)      -> pp_str fmt ("field_add\t " ^ an ^ " " ^ fn)
  | FieldRemove (an, fn)   -> pp_str fmt ("field_remove\t " ^ an ^ " " ^ fn)
  | RemoveAll (an, c)      -> Format.fprintf fmt "remove_all %s on %a" an pp_ck c
  | RemoveIf (an, c, _, p) -> Format.fprintf fmt "remove_if %s %a on %a" an pp_mterm p pp_ck c
  | Contains (an, c)       -> Format.fprintf fmt "contains %s on %a" an pp_ck c
  | Nth (an, c)            -> Format.fprintf fmt "nth %s on %a" an pp_ck c
  | Select (an, c, _, p)   -> Format.fprintf fmt "select %s %a on %a" an pp_mterm p pp_ck c
  | Sort (an, c, l)        -> Format.fprintf fmt "sort %a on %a  on %a" pp_str an (pp_list ", " (fun fmt (a, b) -> Format.fprintf fmt "%a(%a)" pp_sort_kind b pp_ident a)) l pp_ck c
  | Count (an, c)          -> Format.fprintf fmt "count %s on %a" an pp_ck c
  | Sum (an, c, t, p)      -> Format.fprintf fmt "sum (:%a) %s %a on %a" pp_type t an pp_mterm p pp_ck c
  | Head (an, c)           -> Format.fprintf fmt "head %s on %a" an pp_ck c
  | Tail (an, c)           -> Format.fprintf fmt "tail %s on %a" an pp_ck c

let pp_api_list fmt = function
  | Lprepend t  -> Format.fprintf fmt "list_prepend\t %a" pp_type t
  | Lcontains t -> Format.fprintf fmt "list_contains\t %a" pp_type t
  | Llength t   -> Format.fprintf fmt "list_length\t %a" pp_type t
  | Lnth t      -> Format.fprintf fmt "list_nth\t %a" pp_type t
  | Lreverse t  -> Format.fprintf fmt "list_reverse\t %a" pp_type t

let pp_api_builtin fmt = function
  | Bmin    t   -> Format.fprintf fmt "min on %a"    pp_type t
  | Bmax    t   -> Format.fprintf fmt "max on %a"    pp_type t
  | Babs    t   -> Format.fprintf fmt "abs on %a"    pp_type t
  | Bconcat t   -> Format.fprintf fmt "concat on %a" pp_type t
  | Bslice  t   -> Format.fprintf fmt "slice on %a"  pp_type t
  | Blength t   -> Format.fprintf fmt "length on %a" pp_type t
  | Bisnone t   -> Format.fprintf fmt "isnone on %a" pp_type t
  | Bissome t   -> Format.fprintf fmt "issome on %a" pp_type t
  | Boptget t   -> Format.fprintf fmt "getopt on %a" pp_type t
  | Bfloor      -> pp_str fmt "floor"
  | Bceil       -> pp_str fmt "ceil"
  | Bfail t     -> Format.fprintf fmt "fail on %a" pp_type t

let pp_api_internal fmt = function
  | RatEq        -> Format.fprintf fmt "rat_eq"
  | RatCmp       -> Format.fprintf fmt "rat_cmp"
  | RatArith     -> Format.fprintf fmt "rat_arith"
  | RatUminus    -> Format.fprintf fmt "rat_uminus"
  | RatTez       -> Format.fprintf fmt "rat_to_tez"
  | RatDur       -> Format.fprintf fmt "ratdur"

let pp_api_item_node fmt = function
  | APIAsset      v -> pp_api_asset    fmt v
  | APIList       v -> pp_api_list     fmt v
  | APIBuiltin    v -> pp_api_builtin  fmt v
  | APIInternal   v -> pp_api_internal fmt v

let pp_api_storage fmt (api_storage : api_storage) =
  Format.fprintf fmt "%a"
    pp_api_item_node api_storage.node_item

let pp_api_items fmt l =
  if List.is_empty l
  then pp_str fmt "no api items"
  else
    Format.fprintf fmt "api items:@\n%a@\n--@\n"
      (pp_list "@\n" pp_api_storage) l

let pp_var fmt (var : var) =
  Format.fprintf fmt "%a : %a%a"
    pp_mid var.name
    pp_type var.type_
    (pp_option (fun fmt x -> Format.fprintf fmt " = %a" pp_mterm x)) var.default

let pp_enum_item fmt (ei : enum_item) =
  Format.fprintf fmt "| %a%a"
    pp_mid ei.name
    (fun fmt l ->
       if List.is_empty l
       then ()
       else (Format.fprintf fmt " of %a" (pp_list " * " pp_type) l)
    ) ei.args

let pp_enum fmt (enum : enum) =
  Format.fprintf fmt "enum %a =@\n@[<v 2>  %a@]@\n"
    pp_mid enum.name
    (pp_list "@\n" pp_enum_item) enum.values

let pp_asset_item fmt (item : asset_item) =
  Format.fprintf fmt "%a : %a%a;"
    pp_mid item.name
    pp_type item.type_
    (pp_option (fun fmt -> Format.fprintf fmt " = %a" pp_mterm)) item.default

let pp_map_kind fmt = function
  | MKMap            -> Format.fprintf fmt "map"
  | MKBigMap         -> Format.fprintf fmt "big_map"
  | MKIterableBigMap -> Format.fprintf fmt "iterable_big_map"

let pp_asset fmt (asset : asset) =
  let fields = List.filter (fun f -> not f.shadow) asset.values in
  let shadow_fields = List.filter (fun f -> f.shadow) asset.values in
  Format.fprintf fmt "asset %a identified by %a%a to %a {@\n  @[%a@]@\n}%a%a%a@\n"
    pp_mid asset.name
    (pp_list " " pp_str) asset.keys
    (pp_do_if (not (List.is_empty asset.sort)) (fun fmt xs -> Format.fprintf fmt " sorted by %a" (pp_list ";@\n" pp_mid) xs)) asset.sort
    pp_map_kind asset.map_kind
    (pp_list "@\n" pp_asset_item) fields
    (pp_do_if (not (List.is_empty shadow_fields)) (fun fmt xs -> Format.fprintf fmt "@\nshadow {@\n  @[%a@]@\n}@\n" (pp_list ";@\n" pp_asset_item) xs)) shadow_fields
    (pp_do_if (not (List.is_empty asset.init)) (fun fmt xs -> Format.fprintf fmt "@\ninitialized by {@\n  @[%a@]@\n}@\n" (pp_list ";@\n" pp_mterm) xs)) asset.init
    (pp_option (fun fmt id -> Format.fprintf fmt "@\nwith states %a@\n" pp_id id)) asset.state

let pp_record fmt (r : record) =
  let pp_record_field fmt (rf : record_field) =
    Format.fprintf fmt "%a : %a;"
      pp_mid rf.name
      pp_type rf.type_
  in
  let rec pp_pos fmt pos =
    Format.fprintf fmt "(%a)"
      (fun fmt pos ->
         match pos with
         | Ptuple l -> (pp_list ", " pp_ident) fmt l
         | Pnode l  -> (pp_list ", " pp_pos)   fmt l) pos
  in
  Format.fprintf fmt "record %a {@\n  @[%a@]@\n}%a@\n"
    pp_mid r.name
    (pp_list "@\n" pp_record_field) r.fields
    (fun fmt x ->
       match x with
       | Pnode [] -> ()
       | _ -> Format.fprintf fmt " as %a" pp_pos x) r.pos

let pp_decl fmt = function
  | Dvar v -> pp_var fmt v
  | Denum e -> pp_enum fmt e
  | Dasset a -> pp_asset fmt a
  | Drecord r -> pp_record fmt r
  | Devent r -> pp_record fmt r

let pp_storage_item fmt (si : storage_item) =
  Format.fprintf fmt "%a : %a%a (no_storage: %b)"
    pp_mid si.id
    pp_type si.typ
    (fun fmt -> Format.fprintf fmt " := %a" pp_mterm) si.default
    si.no_storage

let pp_storage fmt (s : storage) =
  match s with
  | [] -> ()
  | _ ->
    Format.fprintf fmt "storage {@\n@[<v 2>  %a@]@\n}@\n"
      (pp_list "@\n" pp_storage_item) s

let pp_argument fmt ((id, t, dv) : argument) =
  Format.fprintf fmt "%a : %a%a"
    pp_mid id
    pp_type t
    (pp_option (fun fmt -> Format.fprintf fmt " := %a" pp_mterm)) dv

let pp_function fmt f =
  let vv_to_str = function | VVonchain -> ""  | VVoffchain -> "offchain "  | VVonoffchain -> "onoffchain " in
  let k, fs, ret = match f with
    | Entry f         -> "entry",    f, None
    | Getter (f, a)   -> "getter",   f, Some a
    | View (f, a, vv) -> (vv_to_str vv ^ "view"), f, Some a
    | Function (f, rty) -> "function", f, (match rty with | Typed ty -> Some ty | Void -> None)
  in
  Format.fprintf fmt "%a %a %a%a {@\n@[<v 2>  %a@]@\n}@\n"
    pp_str k
    pp_mid fs.name
    (fun fmt -> Format.fprintf fmt "(%a)" (pp_list ", " pp_argument)) fs.args
    (pp_option (fun fmt -> Format.fprintf fmt " : %a" pp_type)) ret
    pp_mterm fs.body

let pp_parameters fmt = function
  | [] -> ()
  | params -> Format.fprintf fmt "(%a)" (pp_list ", " (
      fun fmt (param : parameter) ->
        Format.fprintf fmt "%a%a : %a%a"
          (pp_do_if param.const (fun fmt _ -> pp_str fmt "const ")) ()
          pp_mid param.name
          pp_type param.typ
          (pp_option (fun fmt x -> Format.fprintf fmt " = %a" pp_mterm x)) param.default)) params

let pp_metadata fmt = function
  | MKuri  v -> Format.fprintf fmt "\"%s\"" (Location.unloc v)
  | MKjson v -> Format.fprintf fmt "`%s`"   (Location.unloc v)

let pp_model fmt (model : model) =
  Format.fprintf fmt "%a%a%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @."
    pp_id model.name
    pp_parameters model.parameters
    (pp_option (fun fmt x -> Format.fprintf fmt "@\nwith metadata %a" pp_metadata x)) model.metadata
    pp_api_items model.api_items
    (pp_list "@\n" pp_decl) model.decls
    pp_storage model.storage
    (pp_list "@\n" pp_function) model.functions

(* -------------------------------------------------------------------------- *)

let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
