(* open Location *)
open Tools
open Model
open Printer_tools

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_currency fmt = function
  | Tz  -> Format.fprintf fmt "tz"
  | Mtz -> Format.fprintf fmt "mtz"
  | Utz -> Format.fprintf fmt "utz"

let pp_btyp fmt = function
  | Bunit       -> Format.fprintf fmt "unit"
  | Bbool       -> Format.fprintf fmt "bool"
  | Bint        -> Format.fprintf fmt "int"
  | Brational   -> Format.fprintf fmt "rational"
  | Bdate       -> Format.fprintf fmt "date"
  | Bduration   -> Format.fprintf fmt "duration"
  | Btimestamp  -> Format.fprintf fmt "timestamp"
  | Bstring     -> Format.fprintf fmt "string"
  | Baddress    -> Format.fprintf fmt "address"
  | Brole       -> Format.fprintf fmt "role"
  | Bcurrency   -> Format.fprintf fmt "tez"
  | Bsignature  -> Format.fprintf fmt "signature"
  | Bkey        -> Format.fprintf fmt "key"
  | Bkeyhash    -> Format.fprintf fmt "key_hash"
  | Bbytes      -> Format.fprintf fmt "bytes"
  | Bnat        -> Format.fprintf fmt "nat"
  | Bchainid    -> Format.fprintf fmt "chain_id"

let pp_container fmt = function
  | Collection -> Format.fprintf fmt "collection"
  | Aggregate  -> Format.fprintf fmt "aggregate"
  | Partition  -> Format.fprintf fmt "partition"
  | View       -> Format.fprintf fmt "view"

let rec pp_type fmt t =
  match t with
  | Tasset an ->
    Format.fprintf fmt "%a" pp_id an
  | Tstate ->
    Format.fprintf fmt "state"
  | Tenum en ->
    Format.fprintf fmt "%a" pp_id en
  | Tbuiltin b -> pp_btyp fmt b
  | Tcontainer (t, c) ->
    Format.fprintf fmt "%a %a"
      pp_type t
      pp_container c
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
  | Tmap (false, k, v) ->
    Format.fprintf fmt "map<%a, %a>"
      pp_type k
      pp_type v
  | Tmap (true, k, v) ->
    Format.fprintf fmt "bigmap<%a, %a>"
      pp_type k
      pp_type v
  | Trecord id ->
    Format.fprintf fmt "%a" pp_id id
  | Tlambda (a, r) ->
    Format.fprintf fmt "(%a -> %a)" pp_type a pp_type r
  | Tunit ->
    Format.fprintf fmt "unit"
  | Tstorage ->
    Format.fprintf fmt "storage"
  | Toperation ->
    Format.fprintf fmt "operation"
  | Tcontract t ->
    Format.fprintf fmt "contract<%a>" pp_type t
  | Tprog _
  | Tvset _
  | Ttrace _ -> Format.fprintf fmt "todo"

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
  | Pconst i -> pp_id fmt i
  | Pwild -> pp_str fmt "_"

let pp_sort_kind fmt = function
  | SKasc -> pp_str fmt "asc"
  | SKdesc -> pp_str fmt "desc"

let pp_entry_description fmt ad =
  match ad with
  | ADany         -> pp_str fmt "anyentry"
  | ADadd      id -> Format.fprintf fmt "add (%a)" pp_ident id
  | ADremove   id -> Format.fprintf fmt "remove (%a)" pp_ident id
  | ADupdate   id -> Format.fprintf fmt "update (%a)" pp_ident id
  | ADtransfer id -> Format.fprintf fmt "transfer (%a)" pp_ident id
  | ADget      id -> Format.fprintf fmt "get (%a)" pp_ident id
  | ADiterate  id -> Format.fprintf fmt "iterate (%a)" pp_ident id
  | ADcall     id -> Format.fprintf fmt "call (%a)" pp_ident id

let pp_temp fmt = function
  | Tbefore -> Format.fprintf fmt "before."
  | Tat i   -> Format.fprintf fmt "at(%s)." i
  | Tnone   -> ()

let pp_delta fmt = function
  | Dadded   -> Format.fprintf fmt "added."
  | Dremoved -> Format.fprintf fmt "removed."
  | Dunmoved -> Format.fprintf fmt "unmoved."
  | Dnone    -> ()

let pp_container_kind f fmt = function
  | CKcoll (t, d)              -> Format.fprintf fmt "_%a%aColl_" pp_temp t pp_delta d
  | CKview mt                  -> f fmt mt
  | CKfield (an, fn, mt, t, d) -> Format.fprintf fmt "%a%aCKfield (%s, %s, %a)" pp_temp t pp_delta d an fn f mt
  | CKdef v                    -> Format.fprintf fmt "_Def(%s)_" v

let pp_iter_container_kind f fmt = function
  | ICKcoll  an         -> Format.fprintf fmt "%a" pp_str an
  | ICKview  mt         -> Format.fprintf fmt "%a" f mt
  | ICKfield (_, _, mt) -> Format.fprintf fmt "%a" f mt
  | ICKset   mt         -> Format.fprintf fmt "%a" f mt
  | ICKlist  mt         -> Format.fprintf fmt "%a" f mt
  | ICKmap   mt         -> Format.fprintf fmt "%a" f mt

let pp_transfer_kind f fmt = function
  | TKsimple d           -> Format.fprintf fmt "to %a" f d
  | TKcall (id, _, d, a) -> Format.fprintf fmt "to %a call %s(%a)" f d id f a
  | TKentry (e, a)       -> Format.fprintf fmt "to entry %a(%a)" f e f a
  | TKself (id, args)    -> Format.fprintf fmt "to entry self.%a(%a)" pp_str id (pp_list ", " (fun fmt (id, x) -> Format.fprintf fmt "%s = %a" id f x)) args

let pp_mterm fmt (mt : mterm) =
  let rec f fmt (mtt : mterm) =
    match mtt.node with
    (* lambda *)

    | Mletin (ids, a, t, b, o) ->
      Format.fprintf fmt "let %a%a = %a in@\n%a%a"
        (pp_list ", " pp_id) ids
        (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t
        f a
        f b
        (pp_option (fun fmt -> Format.fprintf fmt " otherwise %a" f)) o

    | Mdeclvar (ids, t, v) ->
      Format.fprintf fmt "var %a%a = %a"
        (pp_list ", " pp_id) ids
        (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t
        f v

    | Mapp (e, args) ->
      let pp fmt (e, args) =
        Format.fprintf fmt "%a (%a)"
          pp_id e
          (pp_list ", " f) args
      in
      pp fmt (e, args)


    (* assign *)

    | Massign (op, _, Avar k, v) ->
      Format.fprintf fmt "%a %a %a"
        pp_id k
        pp_operator op
        f v

    | Massign (op, _, Avarstore l, r) ->
      Format.fprintf fmt "s.%a %a %a"
        pp_id l
        pp_operator op
        f r

    | Massign (op, _, Aasset (an, fn, k), v) ->
      Format.fprintf fmt "%a[%a].%a %a %a"
        pp_id an
        f k
        pp_id fn
        pp_operator op
        f v

    | Massign (op, _, Arecord (_rn, fn, r), v) ->
      Format.fprintf fmt "%a.%a %a %a"
        f r
        pp_id fn
        pp_operator op
        f v

    | Massign (_op, _, Astate, x) ->
      Format.fprintf fmt "state = %a"
        f x

    | Massign (_op, _, Aassetstate (an, k), v) ->
      Format.fprintf fmt "state_%a(%a) = %a"
        pp_ident an
        f k
        f v

    | Massign (_op, _, Aoperations, v) ->
      Format.fprintf fmt "operations = %a"
        f v



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

    | Mfor (i, c, b, l) ->
      Format.fprintf fmt "for %a%a in %a do@\n  @[%a@]@\ndone"
        (pp_option (fun fmt -> Format.fprintf fmt ": %a " pp_str)) l
        (fun fmt i -> match i with FIsimple x -> pp_id fmt x | FIdouble (x, y) -> Format.fprintf fmt "(%a, %a)" pp_id x pp_id y) i
        (pp_iter_container_kind f) c
        f b

    | Miter (i, a, b, c, l) ->
      Format.fprintf fmt "iter %a%a from %a to %a do@\n  @[%a@]@\ndone"
        (pp_option (fun fmt -> Format.fprintf fmt ": %a " pp_str)) l
        pp_id i
        f a
        f b
        f c

    | Mwhile (c, b, l) ->
      Format.fprintf fmt "while %a%a do@\n  @[%a@]@\ndone"
        (pp_option (fun fmt -> Format.fprintf fmt ": %a " pp_str)) l
        f c
        f b

    | Mseq is ->
      Format.fprintf fmt "%a"
        (pp_list ";@\n" f) is

    | Mreturn x ->
      Format.fprintf fmt "return %a"
        f x

    | Mlabel i ->
      Format.fprintf fmt "label %a"
        pp_id i

    | Mmark (i, x) ->
      Format.fprintf fmt "label %a in@\n@[%a@]"
        pp_id i
        f x


    (* effect *)

    | Mfail ft ->
      Format.fprintf fmt "fail (%a)"
        (pp_fail_type f) ft

    | Mtransfer (v, k) ->
      Format.fprintf fmt "transfer %a %a"
        f v
        (pp_transfer_kind f) k


    (* entrypoint *)

    | Mentrypoint (t, a, s) ->
      Format.fprintf fmt "entrypoint<%a>(%a, %a)"
        pp_type t
        pp_id a
        f s

    | Mself id ->
      Format.fprintf fmt "self.%a"
        pp_id id


    (* operation *)

    | Moperations ->
      Format.fprintf fmt "operations"

    | Mmkoperation (v, d, a) ->
      Format.fprintf fmt "mkoperation(%a, %a, %a)"
        f v
        f d
        f a


    (* literals *)

    | Mint v -> pp_big_int fmt v
    | Mnat v -> pp_big_int fmt v
    | Mbool b -> pp_str fmt (if b then "true" else "false")
    | Menum v -> pp_str fmt v
    | Mrational (n, d) ->
      Format.fprintf fmt "rat(%a, %a)"
        pp_big_int n
        pp_big_int d
    | Mstring v ->
      Format.fprintf fmt "\"%a\""
        pp_str v
    | Mcurrency (v, c) ->
      Format.fprintf fmt "%a%a"
        pp_big_int v
        pp_currency c
    | Maddress v -> pp_str fmt v
    | Mdate v -> Core.pp_date fmt v
    | Mduration v -> Core.pp_duration_for_printer fmt v
    | Mtimestamp v -> pp_big_int fmt v
    | Mbytes v -> Format.fprintf fmt "0x%s" v
    | Munit -> Format.fprintf fmt "Unit"


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

    | Mmatchsome (e, n, i, s) ->
      let pp fmt (e, n, i, s) =
        Format.fprintf fmt "match_some %a with@\n| None -> @[%a@]@\n| Some %s -> @[%a@]"
          f e
          f n
          i
          f s
      in
      pp fmt (e, n, i, s)

    (* composite type constructors *)

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

    | Mlitmap l ->
      Format.fprintf fmt "map(%a)"
        (pp_list "; " (fun fmt (k, v) -> Format.fprintf fmt "%a : %a"
                          f k
                          f v)) l

    | Mlitrecord l ->
      Format.fprintf fmt "record(%a)"
        (pp_list "; " (fun fmt (k, v) -> Format.fprintf fmt "%s = %a"
                          k
                          f v)) l

    (* access *)

    | Mdot (e, i) ->
      Format.fprintf fmt "%a.%a"
        f e
        pp_id i

    | Mdotassetfield (an, k, fn) ->
      Format.fprintf fmt "%a[%a].%a"
        pp_id an
        f k
        pp_id fn


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

    | Muminus e ->
      let pp fmt e =
        Format.fprintf fmt "-%a"
          f e
      in
      pp fmt e


    (* asset api effect *)

    | Maddasset (an, i) ->
      let pp fmt (an, i) =
        Format.fprintf fmt "add_%a (%a)"
          pp_str an
          f i
      in
      pp fmt (an, i)

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

    | Mremoveall (an, fn, a) ->
      let pp fmt (an, fn, a) =
        Format.fprintf fmt "removeall_%a_%a (%a)"
          pp_str an
          pp_str fn
          f a
      in
      pp fmt (an, fn, a)

    | Mremoveif (an, c, la, lb, a) ->
      let pp fmt (an, c, la, lb, a) =
        Format.fprintf fmt "removeif_%a (%a, (%a) -> %a)(%a)"
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
          (pp_list "; " (fun fmt (id, op, v) -> Format.fprintf fmt "%a %a %a" pp_id id pp_operator op f v)) l
      in
      pp fmt (an, k, l)

    | Maddupdate (an, c, k, l) ->
      let pp fmt (an, c, k, l) =
        Format.fprintf fmt "add_update_%a (%a, %a, {%a})"
          pp_str an
          (pp_container_kind f) c
          f k
          (pp_list "; " (fun fmt (id, op, v) -> Format.fprintf fmt "%a %a %a" pp_id id pp_operator op f v)) l
      in
      pp fmt (an, c, k, l)

    | Maddforce (an, v) ->
      let pp fmt (an, v) =
        Format.fprintf fmt "add_force_%a (%a)"
          pp_str an
          f v
      in
      pp fmt (an, v)


    (* asset api expression *)

    | Mget (an, c, k) ->
      let pp fmt (an, k) =
        Format.fprintf fmt "get_%a (%a, %a)"
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
        pp_id ix
        pp_id ia
        f b


    (* list api expression *)

    | Mlistprepend (_, c, a) ->
      Format.fprintf fmt "list_prepend (%a, %a)"
        f c
        f a

    | Mlistheadtail (_, c) ->
      Format.fprintf fmt "list_head_tail (%a)"
        f c

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

    | Mlistreverse (_, l) ->
      Format.fprintf fmt "list_reverse (%a)"
        f l

    | Mlistfold (t, ix, ia, c, a, b) ->
      Format.fprintf fmt "list_%a_fold (%a, %a, (%a, %a) ->@\n  @[%a@])"
        pp_type t
        f c
        f a
        pp_id ix
        pp_id ia
        f b

    (* map api expression *)

    | Mmapput (_, _, c, k, v) ->
      Format.fprintf fmt "map_put (%a, %a, %a)"
        f c
        f k
        f v

    | Mmapremove (_, _, c, k) ->
      Format.fprintf fmt "map_remove (%a, %a)"
        f c
        f k

    | Mmapget (_, _, c, k) ->
      Format.fprintf fmt "map_get (%a, %a)"
        f c
        f k

    | Mmapgetopt (_, _, c, k) ->
      Format.fprintf fmt "map_getopt (%a, %a)"
        f c
        f k

    | Mmapcontains (_, _, c, k) ->
      Format.fprintf fmt "map_contains (%a, %a)"
        f c
        f k

    | Mmaplength (_, _, c) ->
      Format.fprintf fmt "map_length (%a)"
        f c

    | Mmapfold (t, ik, iv, ia, c, a, b) ->
      Format.fprintf fmt "map_%a_fold (%a, %a, (%a, (%a, %a)) ->@\n  @[%a@])"
        pp_type t
        f c
        f a
        pp_id ia
        pp_id ik
        pp_id iv
        f b

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

    | Mslice (x, s, e) ->
      Format.fprintf fmt "slice (%a, %a, %a)"
        f x
        f s
        f e

    | Mlength x ->
      Format.fprintf fmt "length (%a)"
        f x

    | Misnone x ->
      Format.fprintf fmt "isnone (%a)"
        f x

    | Missome x ->
      Format.fprintf fmt "issome (%a)"
        f x

    | Moptget x ->
      Format.fprintf fmt "opt_get (%a)"
        f x

    | Mfloor x ->
      Format.fprintf fmt "floor (%a)"
        f x

    | Mceil x ->
      Format.fprintf fmt "ceil (%a)"
        f x

    | Mtostring (_, x) ->
      Format.fprintf fmt "to_string (%a)"
        f x

    | Mpack x ->
      Format.fprintf fmt "pack (%a)"
        f x

    | Munpack (t, x) ->
      Format.fprintf fmt "unpack<%a>(%a)"
        pp_type t
        f x

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

    | Mhashkey x ->
      Format.fprintf fmt "hash_key (%a)"
        f x

    | Mchecksignature (k, s, x) ->
      Format.fprintf fmt "check_signature (%a, %a, %a)"
        f k
        f s
        f x


    (* constants *)

    | Mnow           -> pp_str fmt "now"
    | Mtransferred   -> pp_str fmt "transferred"
    | Mcaller        -> pp_str fmt "caller"
    | Mbalance       -> pp_str fmt "balance"
    | Msource        -> pp_str fmt "source"
    | Mselfaddress   -> pp_str fmt "selfaddress"
    | Mchainid       -> pp_str fmt "chain_id"
    | Mmetadata      -> pp_str fmt "metadata"


    (* variable *)

    | Mvar (an, Vassetstate k, t, d) -> Format.fprintf fmt "%a%astate_%a(%a)" pp_temp t pp_delta d pp_str (Location.unloc an) f k
    | Mvar(v, Vstorevar, t, d)       -> Format.fprintf fmt "%a%as.%a" pp_temp t pp_delta d pp_id v
    | Mvar(v, Vstorecol, t, d)       -> Format.fprintf fmt "%a%a%a" pp_temp t pp_delta d pp_id v
    | Mvar(v, Venumval, t, d)        -> Format.fprintf fmt "%a%a%a" pp_temp t pp_delta d pp_id v
    | Mvar(v, Vdefinition, t, d)     -> Format.fprintf fmt "%a%a%a" pp_temp t pp_delta d pp_id v
    | Mvar(v, Vlocal, t, d)          -> Format.fprintf fmt "%a%a%a" pp_temp t pp_delta d pp_id v
    | Mvar(v, Vparam, t, d)          -> Format.fprintf fmt "%a%a%a" pp_temp t pp_delta d pp_id v
    | Mvar(v, Vfield, t, d)          -> Format.fprintf fmt "%a%a%a" pp_temp t pp_delta d pp_id v
    | Mvar(_, Vthe, t, d)            -> Format.fprintf fmt "%a%athe" pp_temp t pp_delta d
    | Mvar(_, Vstate, t, d)          -> Format.fprintf fmt "%a%astate" pp_temp t pp_delta d


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


    (* quantifiers *)

    | Mforall (i, t, None, e) ->
      Format.fprintf fmt "forall (%a : %a), %a"
        pp_id i
        pp_type t
        f e

    | Mforall (i, t, Some s, e) ->
      Format.fprintf fmt "forall (%a : %a) in %a, %a"
        pp_id i
        pp_type t
        f s
        f e

    | Mexists (i, t, None, e) ->
      Format.fprintf fmt "exists (%a : %a), %a"
        pp_id i
        pp_type t
        f e

    | Mexists (i, t, Some s, e) ->
      Format.fprintf fmt "exists (%a : %a) in %a, %a"
        pp_id i
        pp_type t
        f s
        f e


    (* formula operators *)

    | Mimply (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a -> %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mequiv  (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a <-> %a"
          f l
          f r
      in
      pp fmt (l, r)


    (* formula asset collection *)

    | Msetiterated e ->
      Format.fprintf fmt "iterated (%a)"
        (pp_iter_container_kind f) e

    | Msettoiterate e ->
      Format.fprintf fmt "to_iterate (%a)"
        (pp_iter_container_kind f) e


    (* formula asset collection methods *)

    | Mempty an ->
      let pp fmt an =
        Format.fprintf fmt "empty_%a"
          pp_str an
      in
      pp fmt an

    | Msingleton (an, k) ->
      let pp fmt (an, k) =
        Format.fprintf fmt "singleton_%a (%a)"
          pp_str an
          f k
      in
      pp fmt (an, k)

    | Msubsetof (an, c, i) ->
      let pp fmt (an, c, i) =
        Format.fprintf fmt "subset_%a (%a, %a)"
          pp_str an
          (pp_container_kind f) c
          f i
      in
      pp fmt (an, c, i)

    | Misempty  (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "isempty (%a, %a)"
          pp_str l
          f r
      in
      pp fmt (l, r)

    | Munion (an, l, r) ->
      let pp fmt (an, l, r) =
        Format.fprintf fmt "union_%a (%a, %a)"
          pp_str an
          f l
          f r
      in
      pp fmt (an, l, r)

    | Minter (an, l, r) ->
      let pp fmt (an, l, r) =
        Format.fprintf fmt "inter_%a (%a, %a)"
          pp_str an
          f l
          f r
      in
      pp fmt (an, l, r)

    | Mdiff (an, l, r) ->
      let pp fmt (an, l, r) =
        Format.fprintf fmt "diff_%a (%a, %a)"
          pp_str an
          f l
          f r
      in
      pp fmt (an, l, r)

  in
  f fmt mt

let pp_label_term fmt (lt : label_term) =
  Format.fprintf fmt "%a : %a"
    pp_id lt.label
    pp_mterm lt.term

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
  | RemoveAll (an, fn)     -> pp_str fmt ("removeall\t " ^ an ^ " " ^ fn)
  | RemoveIf (an, c, _, p) -> Format.fprintf fmt "removeif %s %a on %a" an pp_mterm p pp_ck c
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
  | Btostring t -> Format.fprintf fmt "to_string on %a" pp_type t
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

let pp_api_verif fmt = function
  | StorageInvariant (l, an, mt) -> Format.fprintf fmt "storage_invariant on %a %a %a" pp_ident l pp_ident an pp_mterm mt

let pp_api_storage fmt (api_storage : api_storage) =
  Format.fprintf fmt "%a %a"
    pp_api_item_node api_storage.node_item
    pp_api_loc api_storage.api_loc

let pp_api_items fmt l =
  if List.is_empty l
  then pp_str fmt "no api items"
  else
    Format.fprintf fmt "api items:@\n%a@\n--@\n"
      (pp_list "@\n" pp_api_storage) l

let pp_var fmt (var : var) =
  Format.fprintf fmt "%a : %a%a%a"
    pp_id var.name
    pp_type var.type_
    (fun fmt x -> pp_do_if (not (is_none x)) (fun fmt x -> Format.fprintf fmt " = %a" pp_mterm x) fmt (Option.get x)) var.default
    (pp_do_if (not (List.is_empty var.invariants)) (fun fmt xs -> Format.fprintf fmt "@\nwith {@\n  @[%a@]@\n}@\n" (pp_list ";@\n" pp_label_term) xs)) var.invariants

let pp_enum_item fmt (enum_item : enum_item) =
  Format.fprintf fmt "%a"
    pp_id enum_item.name

let pp_enum fmt (enum : enum) =
  Format.fprintf fmt "enum %a {@\n@[<v 2>  %a@]@\n}@\n"
    pp_id enum.name
    (pp_list "@\n" pp_enum_item) enum.values

let pp_asset_item fmt (item : asset_item) =
  Format.fprintf fmt "%a : %a%a;"
    pp_id item.name
    pp_type item.type_
    (pp_option (fun fmt -> Format.fprintf fmt " = %a" pp_mterm)) item.default

let pp_asset fmt (asset : asset) =
  let fields = List.filter (fun f -> not f.shadow) asset.values in
  let shadow_fields = List.filter (fun f -> f.shadow) asset.values in
  Format.fprintf fmt "asset %a identified by %a%a%a {@\n  @[%a@]@\n}%a%a%a%a@\n"
    pp_id asset.name
    (pp_list " " pp_str) asset.keys
    (pp_do_if (asset.big_map) (pp_str)) " to big_map"
    (pp_do_if (not (List.is_empty asset.sort)) (fun fmt xs -> Format.fprintf fmt " sorted by %a" (pp_list ";@\n" pp_id) xs)) asset.sort
    (pp_list "@\n" pp_asset_item) fields
    (pp_do_if (not (List.is_empty shadow_fields)) (fun fmt xs -> Format.fprintf fmt "@\nshadow {@\n  @[%a@]@\n}@\n" (pp_list ";@\n" pp_asset_item) xs)) shadow_fields
    (pp_do_if (not (List.is_empty asset.init)) (fun fmt xs -> Format.fprintf fmt "@\ninitialized by {@\n  @[%a@]@\n}@\n" (pp_list ";@\n" pp_mterm) xs)) asset.init
    (pp_do_if (not (List.is_empty asset.invariants)) (fun fmt xs -> Format.fprintf fmt "@\nwith {@\n  @[%a@]@\n}@\n" (pp_list ";@\n" pp_label_term) xs)) asset.invariants
    (pp_option (fun fmt id -> Format.fprintf fmt "@\nwith states %a@\n" pp_id id)) asset.state

let pp_record fmt (r : record) =
  let pp_record_field fmt (rf : record_field) =
    Format.fprintf fmt "%a : %a;"
      pp_id rf.name
      pp_type rf.type_
  in
  Format.fprintf fmt "record %a {@\n  @[%a@]@\n}@\n"
    pp_id r.name
    (pp_list "@\n" pp_record_field) r.fields

let pp_decl fmt = function
  | Dvar v -> pp_var fmt v
  | Denum e -> pp_enum fmt e
  | Dasset a -> pp_asset fmt a
  | Drecord r -> pp_record fmt r

let pp_storage_item fmt (si : storage_item) =
  Format.fprintf fmt "%a : %a%a"
    pp_id si.id
    pp_type si.typ
    (fun fmt -> Format.fprintf fmt " := %a" pp_mterm) si.default

let pp_storage fmt (s : storage) =
  match s with
  | [] -> ()
  | _ ->
    Format.fprintf fmt "storage {@\n@[<v 2>  %a@]@\n}@\n"
      (pp_list "@\n" pp_storage_item) s

let pp_invariant fmt (inv : invariant) =
  Format.fprintf fmt "invariant %a {@\n\
                      @[<v 2>  %a@]@\n\
                      }"
    pp_id inv.label
    (pp_list "@\n" pp_mterm) inv.formulas

let pp_invariants fmt is =
  (pp_do_if (match is with | [] -> false | _ -> true) (fun fmt -> Format.fprintf fmt "@\n  @[%a@]" (pp_list "@\n" pp_invariant))) fmt is

let pp_use fmt u =
  (pp_do_if (match u with | [] -> false | _ -> true) (fun fmt -> Format.fprintf fmt "@\n  @[use: %a;@]" (pp_list "@ " pp_id))) fmt u

let pp_postcondition fmt (postcondition : postcondition) =
  Format.fprintf fmt "%s %a {@\n  @[%a@]%a%a@\n}@\n"
    (match postcondition.mode with | Post -> "postcondition" | Assert -> "assert" )
    pp_id postcondition.name
    pp_mterm postcondition.formula
    pp_invariants postcondition.invariants
    pp_use postcondition.uses

let pp_assert_ fmt (s : assert_) =
  Format.fprintf fmt "assert %a on %a {@\n  @[%a@]%a%a@\n}@\n"
    pp_id s.name
    pp_id s.label
    pp_mterm s.formula
    pp_invariants s.invariants
    pp_use s.uses

let pp_specification fmt (v : specification) =
  let pp_predicate fmt (p : predicate) =
    Format.fprintf fmt "predicate %a (%a) {@\n  @[%a@]@\n}@\n"
      pp_id p.name
      (pp_list ", " (fun fmt (id, typ) -> Format.fprintf fmt "%a : %a" pp_id id pp_type typ)) p.args
      pp_mterm p.body
  in
  let pp_definitions fmt (d : definition) =
    Format.fprintf fmt "definition %a {@\n  @[%a : %a |@\n  %a @]@\n}@\n"
      pp_id d.name
      pp_id d.var
      pp_type d.typ
      pp_mterm d.body
  in
  let pp_fail fmt (f : fail) =
    Format.fprintf fmt "%a with (%a : %a):@\n  @[%a@];"
      pp_id f.label
      pp_id f.arg
      pp_type f.atype
      pp_mterm f.formula
  in
  let pp_fails fmt l = if List.is_empty l then () else Format.fprintf fmt "fails {@\n  @[%a@]@\n}" (pp_list "@\n" pp_fail) l in
  let pp_variable_spec fmt (v : variable) =
    let id, type_, dv = v.decl in
    Format.fprintf fmt "%s %a : %a%a@\n"
      (if v.constant then "constant" else "variable")
      pp_id id
      pp_type type_
      (pp_option (pp_prefix " = " pp_mterm)) dv
  in
  let pp_shadow_effect fmt (instrs : mterm list) =
    match instrs with
    | [] -> ()
    | _ ->
      Format.fprintf fmt "shadow effect {@\n  @[%a@]@\n}@\n@\n" (pp_list ";@\n" pp_mterm) instrs
  in
  (*let pp_invariant fmt (i : lident invariant) =
    Format.fprintf fmt "invariant for %a {@\n  @[%a@]@\n}"
      pp_id i.label
      (pp_list ";@\n" pp_pterm) i.formulas
    in
    let pp_invariants fmt is =
    (pp_do_if (match is with | [] -> false | _ -> true) (fun fmt -> Format.fprintf fmt "@\n  @[%a@]" (pp_list "@\n" pp_invariant))) fmt is
    in
    let pp_use fmt u =
    (pp_do_if (match u with | [] -> false | _ -> true) (fun fmt -> Format.fprintf fmt "@\n  @[use: %a;@]" (pp_list "@ " pp_id))) fmt u
    in
    let pp_assert fmt (s : lident assert_) : unit =
    Format.fprintf fmt "assert %a {@\n  @[%a@]%a%a@\n}"
      pp_id s.name
      pp_pterm s.formula
      pp_invariants s.invariants
      pp_use s.uses
    in *)
  let pp_main fmt (v : specification) =
    (pp_no_empty_list2 pp_predicate) fmt v.predicates;
    (pp_no_empty_list2 pp_definitions) fmt v.definitions;
    (* (pp_no_empty_list2 (fun fmt -> Format.fprintf fmt "axioms:@\n  @[%a@]@\n" pp_label_term)) v.lemmas *)
    (* (pp_no_empty_list2 (fun fmt -> Format.fprintf fmt "theorems:@\n  @[%a@]@\n" pp_label_term)) v.theorems *)
    pp_fails fmt v.fails;
    (pp_no_empty_list2 pp_variable_spec) fmt v.variables;
    (* (pp_no_empty_list2 (fun fmt (id, l : lident * lident label_term list) ->
         Format.fprintf fmt "invariants:@\n  @[%a@]@\n"
           (pp_list "@\n" (fun fmt (lt : lident label_term) ->
                Format.fprintf fmt "%a : %a"
                  pp_id id
                  pp_label_term lt
              )) l)) v.invariants *)
    (* (pp_option (fun fmt -> Format.fprintf fmt "shadow effect {@\n  @[%a@]@\n}@\n" pp_instruction)) v.effect *)
    pp_shadow_effect fmt v.effects;
    (pp_no_empty_list2 pp_postcondition) fmt v.postconditions
  in
  let empty =
    List.is_empty v.predicates     &&
    List.is_empty v.definitions    &&
    List.is_empty v.lemmas         &&
    List.is_empty v.theorems       &&
    List.is_empty v.variables      &&
    List.is_empty v.fails          &&
    List.is_empty v.invariants     &&
    List.is_empty v.effects        &&
    List.is_empty v.postconditions
  in
  if empty
  then ()
  else Format.fprintf fmt "specification {@\n  @[%a@]@\n}@\n" pp_main v

let pp_security fmt (s : security) =
  let pp_security_entry fmt (a : security_entry)=
    match a with
    | Sany -> Format.fprintf fmt "any"
    | Sentry l ->
      if List.length l = 1
      then pp_id fmt (List.nth l 0)
      else Format.fprintf fmt "[%a]" (pp_list " or " pp_id) l
  in
  let pp_security_role = pp_id in
  let pp_security_roles fmt l =
    if List.length l = 1
    then pp_id fmt (List.nth l 0)
    else Format.fprintf fmt "[%a]" (pp_list " or " pp_security_role) l
  in
  let pp_security_predicate fmt (sp : security_predicate) =
    match sp.s_node with
    | SonlyByRole (ad, roles) ->
      Format.fprintf fmt "only_by_role (%a, %a)"
        pp_entry_description ad
        pp_security_roles roles

    | SonlyInEntry (ad, entry) ->
      Format.fprintf fmt "only_in_entry (%a, %a)"
        pp_entry_description ad
        pp_security_entry entry

    | SonlyByRoleInEntry (ad, roles, entry) ->
      Format.fprintf fmt "only_by_role_in_entry (%a, %a, %a)"
        pp_entry_description ad
        pp_security_roles roles
        pp_security_entry entry

    | SnotByRole (ad, roles) ->
      Format.fprintf fmt "not_by_role (%a, %a)"
        pp_entry_description ad
        pp_security_roles roles

    | SnotInEntry (ad, entry) ->
      Format.fprintf fmt "not_in_entry (%a, %a)"
        pp_entry_description ad
        pp_security_entry entry

    | SnotByRoleInEntry (ad, roles, entry) ->
      Format.fprintf fmt "not_by_role_in_entry (%a, %a, %a)"
        pp_entry_description ad
        pp_security_roles roles
        pp_security_entry entry

    | StransferredBy ad ->
      Format.fprintf fmt "transferred_by (%a)"
        pp_entry_description ad

    | StransferredTo ad ->
      Format.fprintf fmt "transferred_to (%a)"
        pp_entry_description ad

    | SnoStorageFail entry ->
      Format.fprintf fmt "no_storage_fail (%a)"
        pp_security_entry entry
  in

  let pp_security_item fmt (si : security_item) =
    Format.fprintf fmt "%a : %a;"
      pp_id si.label
      pp_security_predicate si.predicate
  in
  let empty = List.is_empty s.items
  in
  if empty
  then ()
  else
    Format.fprintf fmt "security {@\n  @[%a@]@\n}@\n"
      (pp_no_empty_list pp_security_item) s.items

let pp_argument fmt ((id, t, dv) : argument) =
  Format.fprintf fmt "%a : %a%a"
    pp_id id
    pp_type t
    (pp_option (fun fmt -> Format.fprintf fmt " := %a" pp_mterm)) dv

let pp_function fmt f =
  let k, fs, ret = match f.node with
    | Entry f         -> "entry",    f, None
    | Getter (f, a)   -> "getter",   f, Some a
    | Function (f, a) -> "function", f, Some a
  in
  Format.fprintf fmt "%a %a %a%a {@\n@[<v 2>  %a%a@]@\n}@\n"
    pp_str k
    pp_id fs.name
    (fun fmt -> Format.fprintf fmt "(%a)" (pp_list ", " pp_argument)) fs.args
    (pp_option (fun fmt -> Format.fprintf fmt " : %a" pp_type)) ret
    (pp_option pp_specification) f.spec
    pp_mterm fs.body

let pp_model fmt (model : model) =
  Format.fprintf fmt "%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @\n@\n%a\
                      @."
    pp_id model.name
    pp_api_items model.api_items
    (pp_list "@\n" pp_api_verif) model.api_verif
    (pp_list "@\n" pp_decl) model.decls
    pp_storage model.storage
    (pp_list "@\n" pp_function) model.functions
    pp_specification model.specification
    pp_security model.security

(* -------------------------------------------------------------------------- *)

let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
