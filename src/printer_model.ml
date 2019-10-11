(* open Location *)
open Tools
open Model
open Printer_tools

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let pp_currency fmt = function
  | Tz -> Format.fprintf fmt "tz"
  | Mtz -> Format.fprintf fmt "mtz"

let pp_btyp fmt = function
  | Bbool       -> Format.fprintf fmt "bool"
  | Bint        -> Format.fprintf fmt "int"
  | Brational   -> Format.fprintf fmt "rational"
  | Bdate       -> Format.fprintf fmt "date"
  | Bduration   -> Format.fprintf fmt "duration"
  | Bstring     -> Format.fprintf fmt "string"
  | Baddress    -> Format.fprintf fmt "address"
  | Brole       -> Format.fprintf fmt "role"
  | Bcurrency   -> Format.fprintf fmt "tez"
  | Bkey        -> Format.fprintf fmt "key"

let pp_container fmt = function
  | Collection -> Format.fprintf fmt "collection"
  | Partition  -> Format.fprintf fmt "partition"
  | List       -> Format.fprintf fmt "list"

let rec pp_type fmt t =
  match t with
  | Tasset an ->
    Format.fprintf fmt "%a" pp_id an
  | Tenum en ->
    Format.fprintf fmt "%a" pp_id en
  | Tcontract cn ->
    Format.fprintf fmt "%a" pp_id cn
  | Tbuiltin b -> pp_btyp fmt b
  | Tcontainer (t, c) ->
    Format.fprintf fmt "%a %a"
      pp_type t
      pp_container c
  | Toption t ->
    Format.fprintf fmt "%a option"
      pp_type_ t
  | Ttuple ts ->
    Format.fprintf fmt "%a"
      (pp_list " * " pp_type) ts
  | Tassoc (k, v) ->
    Format.fprintf fmt "(%a, %a) map"
      pp_btyp k
      pp_type v
  | Tunit ->
    Format.fprintf fmt "unit"
  | Tstorage ->
    Format.fprintf fmt "storage"
  | Toperation ->
    Format.fprintf fmt "operation"
  | Tentry ->
    Format.fprintf fmt "entry"
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

let rec pp_qualid fmt (q : qualid) =
  match q.node with
  | Qdot (q, i) ->
    Format.fprintf fmt "%a.%a"
      pp_qualid q
      pp_id i
  | Qident i -> pp_id fmt i

let pp_pattern fmt (p : pattern) =
  match p.node with
  | Pconst i -> pp_id fmt i
  | Pwild -> pp_str fmt "_"

let pp_action_description fmt ad =
  match ad with
  | ADany         -> pp_str fmt "anyaction"
  | ADadd      id -> Format.fprintf fmt "add (%a)" pp_ident id
  | ADremove   id -> Format.fprintf fmt "remove (%a)" pp_ident id
  | ADupdate   id -> Format.fprintf fmt "update (%a)" pp_ident id
  | ADtransfer id -> Format.fprintf fmt "transfer (%a)" pp_ident id
  | ADget      id -> Format.fprintf fmt "get (%a)" pp_ident id
  | ADiterate  id -> Format.fprintf fmt "iterate (%a)" pp_ident id
  | ADcall     id -> Format.fprintf fmt "call (%a)" pp_ident id

let pp_mterm fmt (mt : mterm) =
  let rec f fmt (mtt : mterm) =
    match mtt.node with
    | Mif (c, t, e) ->
      Format.fprintf fmt "if %a@\nthen @[<v 2>%a@]%a"
        f c
        f t
        (pp_option (fun fmt -> Format.fprintf fmt "@\nelse @[<v 2>%a@]" f)) e

    | Mmatchwith (e, l) ->
      let pp fmt (e, l) =
        Format.fprintf fmt "match %a with@\n@[<v 2>%a@]"
          f e
          (pp_list "@\n" (fun fmt (p, x) ->
               Format.fprintf fmt "| %a -> %a"
                 pp_pattern p
                 f x
             )) l
      in
      pp fmt (e, l)

    | Mapp (e, args) ->
      let pp fmt (e, args) =
        Format.fprintf fmt "%a (%a)"
          pp_id e
          (pp_list ", " f) args
      in
      pp fmt (e, args)

    | Maddshallow (e, args) ->
      let pp fmt (e, args) =
        Format.fprintf fmt "add_shallow_%a (%a)"
          pp_str e
          (pp_list ", " f) args
      in
      pp fmt (e, args)

    | Mexternal (_, _, c, args) ->
      let pp fmt (c, args) =
        Format.fprintf fmt "%a (%a)"
          f c
          (pp_list ", " f) args
      in
      pp fmt (c, args)

    | Mget (c, k) ->
      let pp fmt (c, k) =
        Format.fprintf fmt "get_%a (%a)"
          pp_str c
          f k
      in
      pp fmt (c, k)

    | Mset (c, l, k, v) ->
      let pp fmt (c, l, k, v) =
        Format.fprintf fmt "set_%a (%a, %a)"
          pp_str c
          f k
          f v
      in
      pp fmt (c, l, k, v)

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

    | Maddlocal (c, i) ->
      let pp fmt (c, i) =
        Format.fprintf fmt "add (%a, %a)"
          f c
          f i
      in
      pp fmt (c, i)

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

    | Mremovelocal (c, i) ->
      let pp fmt (c, i) =
        Format.fprintf fmt "remove (%a, %a)"
          f c
          f i
      in
      pp fmt (c, i)

    | Mclearasset (an) ->
      let pp fmt (an) =
        Format.fprintf fmt "clear_%a ()"
          pp_str an
      in
      pp fmt (an)

    | Mclearfield (an, fn, i) ->
      let pp fmt (an, fn, i) =
        Format.fprintf fmt "clear_%a_%a (%a)"
          pp_str an
          pp_str fn
          f i
      in
      pp fmt (an, fn, i)

    | Mremoveif (an, fn, i) ->
      let pp fmt (an, fn, i) =
        Format.fprintf fmt "removeif_%a (%a) (%a)"
          pp_str an
          f fn
          f i
      in
      pp fmt (an, fn, i)

    | Mclearlocal (i) ->
      let pp fmt (i) =
        Format.fprintf fmt "clear (%a)"
          f i
      in
      pp fmt (i)

    | Mreverseasset (an) ->
      let pp fmt (an) =
        Format.fprintf fmt "reverse_%a ()"
          pp_str an
      in
      pp fmt (an)

    | Mreversefield (an, fn, i) ->
      let pp fmt (an, fn, i) =
        Format.fprintf fmt "reverse_%a_%a (%a)"
          pp_str an
          pp_str fn
          f i
      in
      pp fmt (an, fn, i)

    | Mreverselocal (i) ->
      let pp fmt (i) =
        Format.fprintf fmt "reverse (%a)"
          f i
      in
      pp fmt (i)

    | Mselect (an, c, p) ->
      let pp fmt (an, c, p) =
        Format.fprintf fmt "select_%a (%a, %a)"
          pp_str an
          f c
          f p
      in
      pp fmt (an, c, p)

    | Msort (an, c, fn, k) ->
      let pp fmt (an, c, fn, k) =
        Format.fprintf fmt "sort_%a_%a (%a %a)"
          pp_str an
          pp_str fn
          f c
          pp_sort_kind k
      in
      pp fmt (an, c, fn, k)

    | Mcontains (an, c, i) ->
      let pp fmt (an, c, i) =
        Format.fprintf fmt "contains_%a (%a, %a)"
          pp_str an
          f c
          f i
      in
      pp fmt (an, c, i)

    | Mmem (an, c, i) ->
      let pp fmt (an, c, i) =
        Format.fprintf fmt "mem_%a (%a, %a)"
          pp_str an
          f c
          f i
      in
      pp fmt (an, c, i)

    | Msubsetof (an, c, i) ->
      let pp fmt (an, c, i) =
        Format.fprintf fmt "subset_%a (%a, %a)"
          pp_str an
          f c
          f i
      in
      pp fmt (an, c, i)

    | Mnth (an, c, i) ->
      let pp fmt (an, c, i) =
        Format.fprintf fmt "nth_%a (%a, %a)"
          pp_str an
          f c
          f i
      in
      pp fmt (an, c, i)

    | Mcount (an, c) ->
      let pp fmt (an, c) =
        Format.fprintf fmt "count_%a (%a)"
          pp_str an
          f c
      in
      pp fmt (an, c)

    | Msum (an, fd, c) ->
      let pp fmt (an, fd, c) =
        Format.fprintf fmt "sum_%a_%a (%a)"
          pp_str an
          pp_id fd
          f c
      in
      pp fmt (an, fd, c)

    | Mmin (an, fd, c) ->
      let pp fmt (an, fd, c) =
        Format.fprintf fmt "min_%a_%a (%a)"
          pp_str an
          pp_id fd
          f c
      in
      pp fmt (an, fd, c)

    | Mmax (an, fd, c) ->
      let pp fmt (an, fd, c) =
        Format.fprintf fmt "max_%a_%a (%a)"
          pp_str an
          pp_id fd
          f c
      in
      pp fmt (an, fd, c)

    | Mfail ft ->

      let pp_fail_type fmt = function
        | Invalid e -> f fmt e
        | InvalidCaller -> Format.fprintf fmt "invalid caller"
        | InvalidCondition c ->
          Format.fprintf fmt "require %afailed"
            (pp_option (pp_postfix " " pp_str)) c
        | NoTransfer -> Format.fprintf fmt "no transfer"
        | InvalidState -> Format.fprintf fmt "invalid state"
      in

      Format.fprintf fmt "Current.failwith \"%a\""
        pp_fail_type ft

    | Mmathmin (l, r) ->
      Format.fprintf fmt "min (%a, %a)"
        f l
        f r

    | Mmathmax (l, r) ->
      Format.fprintf fmt "max (%a, %a)"
        f l
        f r

    | Mhead (an, c, i) ->
      Format.fprintf fmt "head_%a (%a, %a)"
        pp_str an
        f c
        f i

    | Mtail (an, c, i) ->
      Format.fprintf fmt "tail_%a (%a, %a)"
        pp_str an
        f c
        f i

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

    | Misempty  (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a.isempty %a"
          pp_str l
          f r
      in
      pp fmt (l, r)

    | Mnot e ->
      let pp fmt e =
        Format.fprintf fmt "not %a"
          f e
      in
      pp fmt e

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

    | Mequal (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a = %a"
          f l
          f r
      in
      pp fmt (l, r)

    | Mnequal (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a <> %a"
          f l
          f r
      in
      pp fmt (l, r)

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

    | Mdiv (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "%a / %a"
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

    | Muplus e ->
      let pp fmt e =
        Format.fprintf fmt "+%a"
          f e
      in
      pp fmt e

    | Muminus e ->
      let pp fmt e =
        Format.fprintf fmt "-%a"
          f e
      in
      pp fmt e

    | Mrecord l ->
      Format.fprintf fmt "{%a}"
        (pp_list "; " f) l
    | Mletin (ids, a, t, b) ->
      Format.fprintf fmt "let %a%a = %a in@\n%a"
        (pp_list ", " pp_id) ids
        (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t
        f a
        f b
    | Mvarstorevar v -> pp_id fmt v
    | Mvarstorecol v -> pp_id fmt v
    | Mvarenumval v  -> pp_id fmt v
    | Mvarfield v    -> pp_id fmt v
    | Mvarlocal v    -> pp_id fmt v
    | Mvarparam v    -> pp_id fmt v
    | Mvarthe        -> pp_str fmt "the"
    | Mstate         -> pp_str fmt "state"
    | Mnow           -> pp_str fmt "now"
    | Mtransferred   -> pp_str fmt "transferred"
    | Mcaller        -> pp_str fmt "caller"
    | Mbalance       -> pp_str fmt "balance"
    | Mnone          -> pp_str fmt "None"
    | Msome v        ->
      Format.fprintf fmt "Some (%a)"
        f v
    | Marray l ->
      Format.fprintf fmt "[%a]"
        (pp_list "; " f) l
    | Mint v -> pp_big_int fmt v
    | Muint v -> pp_big_int fmt v
    | Mbool b -> pp_str fmt (if b then "true" else "false")
    | Menum v -> pp_str fmt v
    | Mrational (n, d) ->
      Format.fprintf fmt "(%a div %a)"
        pp_big_int n
        pp_big_int d
    | Mdate v -> pp_str fmt v
    | Mstring v ->
      Format.fprintf fmt "\"%a\""
        pp_str v
    | Mcurrency (v, c) ->
      Format.fprintf fmt "%a%a"
        pp_big_int v
        pp_currency c
    | Maddress v -> pp_str fmt v
    | Mduration v -> Core.pp_duration_for_printer fmt v
    | Mdotasset (e, i)
    | Mdotcontract (e, i) ->
      Format.fprintf fmt "%a (%a)"
        pp_id i
        f e
    | Mtuple l ->
      Format.fprintf fmt "(%a)"
        (pp_list ", " f) l
    | Massoc (k, v) ->
      Format.fprintf fmt "(%a : %a)"
        f k
        f v
    | Mfor (i, c, b, l) ->
      Format.fprintf fmt "for %a(%a in %a) (@\n  @[%a@]@\n)@\n"
        (pp_option (fun fmt -> Format.fprintf fmt ": %a " pp_str)) l
        pp_id i
        f c
        f b
    | Miter (i, a, b, c, l) ->
      Format.fprintf fmt "iter %a(%a from %a to %a) (@\n  @[%a@]@\n)@\n"
        (pp_option (fun fmt -> Format.fprintf fmt ": %a " pp_str)) l
        pp_id i
        f a
        f b
        f c
    | Mfold (i, is, c, b) ->
      Format.fprintf fmt "fold %a %a %a (@\n  @[%a@]@\n)@\n"
        pp_id i
        (pp_list "%@," pp_id) is
        f c
        f b
    | Mseq is ->
      Format.fprintf fmt "%a"
        (pp_list ";@\n" f) is

    | Massign (op, l, r) ->
      Format.fprintf fmt "%a %a %a"
        pp_id l
        pp_operator op
        f r
    | Massignfield (op, a, field , r) ->
      Format.fprintf fmt "%a.%a %a %a"
        pp_id a
        pp_id field
        pp_operator op
        f r
    | Mtransfer (x, b, q) ->
      Format.fprintf fmt "transfer%s %a%a"
        (if b then " back" else "")
        f x
        (pp_option (fun fmt -> Format.fprintf fmt " to %a" pp_qualid)) q
    | Mbreak -> pp_str fmt "break"
    | Massert x ->
      Format.fprintf fmt "assert %a"
        f x
    | Mreturn x ->
      Format.fprintf fmt "return %a"
        f x
    | Mlabel i ->
      Format.fprintf fmt "label %a"
        pp_id i
    | Mshallow (i, x) ->
      Format.fprintf fmt "shallow_%a %a"
        pp_str i
        f x
    | Mlisttocoll (i, x) ->
      Format.fprintf fmt "listtocoll_%a %a"
        pp_str i
        f x
    | Munshallow (i, x) ->
      Format.fprintf fmt "unshallow_%a (%a)"
        pp_str i
        f x
    | Mtokeys (an, x) ->
      Format.fprintf fmt "%s.to_keys (%a)"
        an
        f x
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

    | Msetbefore e ->
      Format.fprintf fmt "before %a"
        f e

    | Msetunmoved e ->
      Format.fprintf fmt "unmoved %a"
        f e

    | Msetadded e ->
      Format.fprintf fmt "added %a"
        f e

    | Msetremoved e ->
      Format.fprintf fmt "removed %a"
        f e

    | Msetiterated e ->
      Format.fprintf fmt "iterated %a"
        f e

    | Msettoiterate e ->
      Format.fprintf fmt "to_iterate %a"
        f e
  in
  f fmt mt

let pp_storage_const fmt = function
  | Get an -> pp_str fmt ("get\t " ^ an)
  | Set an -> pp_str fmt ("set\t " ^ an)
  | Add an -> pp_str fmt ("add\t " ^ an)
  | Remove an -> pp_str fmt ("remove\t " ^ an)
  | Clear an -> pp_str fmt ("clear\t " ^ an)
  | Reverse an -> pp_str fmt ("reverse " ^ an)
  | UpdateAdd (an, fn) -> pp_str fmt ("add\t " ^ an ^ " " ^ fn)
  | UpdateRemove (an, fn) -> pp_str fmt ("remove\t " ^ an ^ " " ^ fn)
  | UpdateClear (an, fn) -> pp_str fmt ("clear\t " ^ an ^ " " ^ fn)
  | UpdateReverse (an, fn) -> pp_str fmt ("reverse " ^ an ^ " " ^ fn)
  | ToKeys an -> pp_str fmt ("to_keys\t " ^ an)

let pp_container_const fmt = function
  | AddItem t-> Format.fprintf fmt "add item\t %a" pp_type t
  | RemoveItem t -> Format.fprintf fmt "remove item\t %a" pp_type t
  | ClearItem t -> Format.fprintf fmt "clear item\t %a" pp_type t
  | ReverseItem t -> Format.fprintf fmt "reverse item %a" pp_type t

let pp_function_const fmt = function
  | Select (an, p) ->
    Format.fprintf fmt "select\t %s %a" an pp_mterm p
  | Sort (an, fn) -> pp_str fmt ("sort\t " ^ an ^ " " ^ fn)
  | Contains an -> pp_str fmt ("contains " ^ an)
  | Nth an -> pp_str fmt ("nth\t " ^ an)
  | Count an -> pp_str fmt ("count\t " ^ an)
  | Sum (an, fn) -> pp_str fmt ("sum\t " ^ an ^ " " ^ fn)
  | Min (an, fn) -> pp_str fmt ("min\t " ^ an ^ " " ^ fn)
  | Max (an, fn) -> pp_str fmt ("max\t " ^ an ^ " " ^ fn)
  | Shallow an -> pp_str fmt ("shallow\t " ^ an)
  | Unshallow an -> pp_str fmt ("unshallow\t " ^ an)
  | Listtocoll an -> pp_str fmt ("listtocoll\t " ^ an)
  | Head an -> pp_str fmt ("head\t " ^ an)
  | Tail an -> pp_str fmt ("tail\t " ^ an)

let pp_builtin_const fmt = function
  | MinBuiltin t-> Format.fprintf fmt "min on %a" pp_type t
  | MaxBuiltin t-> Format.fprintf fmt "max on %a" pp_type t

let pp_api_item_node fmt = function
  | APIStorage      v -> pp_storage_const fmt v
  | APIContainer    v -> pp_container_const fmt v
  | APIFunction     v -> pp_function_const fmt v
  | APIBuiltin      v -> pp_builtin_const fmt v

let pp_api_verif fmt = function
  | StorageInvariant (l, an, mt) -> Format.fprintf fmt "storage_invariant on %a %a %a" pp_ident l pp_ident an pp_mterm mt

let pp_api_item fmt (api_item : api_item) =
  Format.fprintf fmt "%a%a"
    pp_api_item_node api_item.node_item
    (fun fmt x -> if x then pp_str fmt "\t[only_formula]" else pp_str fmt "") api_item.only_formula

let pp_api_items fmt l =
  if List.is_empty l
  then pp_str fmt "no api items"
  else
    Format.fprintf fmt "api items:@\n%a@\n--@\n"
      (pp_list "@\n" pp_api_item) l

let pp_info_var fmt (iv : info_var) =
  Format.fprintf fmt "%s %a %a%a@\n"
    (if iv.constant then "constant" else "variable")
    pp_ident iv.name
    pp_type iv.type_
    (pp_option (pp_prefix " := " pp_mterm)) iv.init

let pp_info_enum fmt (ie : info_enum) =
  Format.fprintf fmt "enum %a:@\n  @[%a@]@\n"
    pp_ident ie.name
    (pp_list "@\n" pp_ident) ie.values

let pp_info_asset fmt (ia : info_asset) =
  Format.fprintf fmt "asset %a = {@\n  @[%a@]@\n}@\n"
    pp_ident ia.name
    (pp_list "@\n" (fun fmt (i, t, init) ->
         Format.fprintf fmt "%a : %a%a%a"
           pp_ident i
           pp_type t
           (pp_option (pp_prefix " := " pp_mterm)) init
           (pp_do_if (String.equal ia.key i) pp_str) " [key]"
       )) ia.values

let pp_info_contract fmt (ic : info_contract) =
  Format.fprintf fmt "contract %a:@\n  @[%a@]@\n"
    pp_ident ic.name
    (pp_list "@\n" (fun fmt (i, ts) ->
         Format.fprintf fmt "%a : %a;"
           pp_ident i
           (pp_list ", " pp_type_) ts
       )) ic.signatures

let pp_info_item fmt = function
  | Ivar iv -> pp_info_var fmt iv
  | Ienum ie -> pp_info_enum fmt ie
  | Iasset ia -> pp_info_asset fmt ia
  | Icontract ic -> pp_info_contract fmt ic

let pp_infos fmt l =
  if List.is_empty l
  then pp_str fmt "no infos"
  else
    Format.fprintf fmt "info items:@\n@\n%a@\n--@\n"
      (pp_list "@\n" pp_info_item) l

let pp_enum_item fmt (enum_item : enum_item) =
  Format.fprintf fmt "%a"
    pp_id enum_item.name

let pp_enum fmt (enum : enum) =
  Format.fprintf fmt "enum %a {@\n@[<v 2>  %a@]@\n}@\n"
    pp_id enum.name
    (pp_list "@\n" pp_enum_item) enum.values

let pp_record_item fmt (item : record_item) =
  Format.fprintf fmt "%a : %a%a"
    pp_id item.name
    pp_type item.type_
    (pp_option (fun fmt -> Format.fprintf fmt " := %a" pp_mterm)) item.default

let pp_record fmt (record : record) =
  Format.fprintf fmt "record %a%a {@\n@[<v 2>  %a@]@\n}@\n"
    pp_id record.name
    (pp_option (fun fmt -> Format.fprintf fmt " identified by %a" pp_id)) record.key
    (pp_list "@\n" pp_record_item) record.values

let pp_contract_signature fmt (cs : contract_signature) =
  Format.fprintf fmt "%a : %a"
    pp_id cs.name
    (pp_list " -> " pp_type) cs.args

let pp_contract fmt (contract : contract) =
  Format.fprintf fmt "contract %a {@\n@[<v 2>  %a@]@\n}%a@\n"
    pp_id contract.name
    (pp_list "@\n" pp_contract_signature) contract.signatures
    (pp_option pp_mterm) contract.init

let pp_decl fmt = function
  | Denum e -> pp_enum fmt e
  | Drecord r -> pp_record fmt r
  | Dcontract c -> pp_contract fmt c

let pp_label_term fmt (lt : label_term) =
  Format.fprintf fmt "%a : %a"
    (pp_option pp_id) lt.label
    pp_mterm lt.term

let pp_storage_item fmt (si : storage_item) =
  Format.fprintf fmt "%a : %a%a%a"
    pp_id si.name
    pp_type si.typ
    (fun fmt -> Format.fprintf fmt " := %a" pp_mterm) si.default
    (pp_do_if (not (List.is_empty si.invariants)) (fun fmt xs -> Format.fprintf fmt " with {%a}" (pp_list "; " pp_label_term) xs)) si.invariants

let pp_storage fmt (s : storage) =
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
  Format.fprintf fmt "postcondition %a {@\n  @[%a@]%a%a@\n}@\n"
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
  let empty = List.is_empty v.postconditions in
  if empty
  then ()
  else
    Format.fprintf fmt "specification {@\n  @[%a@]@\n}@\n"
      (pp_list "@\n" pp_postcondition) v.postconditions

let pp_security fmt (s : security) =
  let pp_security_action fmt (a : security_action)=
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
        pp_action_description ad
        pp_security_roles roles

    | SonlyInAction (ad, action) ->
      Format.fprintf fmt "only_in_action (%a, %a)"
        pp_action_description ad
        pp_security_action action

    | SonlyByRoleInAction (ad, roles, action) ->
      Format.fprintf fmt "only_by_role_in_action (%a, %a, %a)"
        pp_action_description ad
        pp_security_roles roles
        pp_security_action action

    | SnotByRole (ad, roles) ->
      Format.fprintf fmt "not_by_role (%a, %a)"
        pp_action_description ad
        pp_security_roles roles

    | SnotInAction (ad, action) ->
      Format.fprintf fmt "not_in_action (%a, %a)"
        pp_action_description ad
        pp_security_action action

    | SnotByRoleInAction (ad, roles, action) ->
      Format.fprintf fmt "not_by_role_in_action (%a, %a, %a)"
        pp_action_description ad
        pp_security_roles roles
        pp_security_action action

    | StransferredBy ad ->
      Format.fprintf fmt "transferred_by (%a)"
        pp_action_description ad

    | StransferredTo ad ->
      Format.fprintf fmt "transferred_to (%a)"
        pp_action_description ad

    | SnoStorageFail action ->
      Format.fprintf fmt "no_storage_fail (%a)"
        pp_security_action action
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
  Format.fprintf fmt "%a %a%a"
    pp_type t
    pp_id id
    (pp_option (fun fmt -> Format.fprintf fmt " := %a" pp_mterm)) dv

let pp_function fmt f =
  let k, fs, ret = match f.node with
    | Entry f -> "entry", f, None
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
                      @\n@\n%a\
                      @."
    pp_id model.name
    pp_api_items model.api_items
    (pp_list "@\n" pp_api_verif) model.api_verif
    pp_infos model.info
    (pp_list "@\n" pp_decl) model.decls
    pp_storage model.storage
    (pp_list "@\n" pp_function) model.functions
    pp_specification model.specification
    pp_security model.security

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
