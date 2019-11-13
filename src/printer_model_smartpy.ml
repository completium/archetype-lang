open Location
open Tools
open Model
open Printer_tools

exception Anomaly of string

type error_desc =
  | UnsupportedBreak
  | UnsupportedTerm of string
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

type operator =
  | Equal
  | Nequal
  | Lt
  | Le
  | Gt
  | Ge
  | Plus
  | Minus
  | Mult
  | Div
  | Modulo

type position =
  | Lhs
  | Rhs

let pp_cast (pos : position) (ltype : type_) (rtype : type_) (pp : 'a -> mterm -> unit) (fmt : Format.formatter) =
  match pos, ltype, rtype with
  | Lhs, Tbuiltin Brole, Tbuiltin Baddress ->
    Format.fprintf fmt "(%a : address)" pp
  | Rhs, Tbuiltin Baddress, Tbuiltin Brole ->
    Format.fprintf fmt "(%a : address)" pp
  | _ -> pp fmt

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let to_lident = dumloc

let pp_nothing (_fmt : Format.formatter) = ()

let pp_model fmt (model : model) =

  let pp_model_name (fmt : Format.formatter) _ =
    Format.fprintf fmt "# contract: %a "
      pp_id model.name
  in

  let pp_currency fmt = function
    | Tz   -> Format.fprintf fmt "tz"
    | Mtz  -> Format.fprintf fmt "mtz"
    | Mutz -> Format.fprintf fmt "mutz"
  in

  let pp_btyp fmt = function
    | Bbool       -> Format.fprintf fmt "bool"
    | Bint        -> Format.fprintf fmt "int"
    | Brational   -> Format.fprintf fmt "rational"
    | Bdate       -> Format.fprintf fmt "timestamp"
    | Bduration   -> Format.fprintf fmt "duration"
    | Bstring     -> Format.fprintf fmt "string"
    | Baddress    -> Format.fprintf fmt "address"
    | Brole       -> Format.fprintf fmt "key_hash"
    | Bcurrency   -> Format.fprintf fmt "tez"
    | Bkey        -> Format.fprintf fmt "key"
  in

  let pp_container fmt = function
    | Collection -> Format.fprintf fmt "list"
    | Partition  -> Format.fprintf fmt "list"
    | List       -> Format.fprintf fmt "list"
  in

  let rec pp_type fmt t =
    match t with
    | Tasset an ->
      Format.fprintf fmt "%a" pp_id an
    | Tstate ->
      Format.fprintf fmt "state"
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
        pp_type t
    | Ttuple ts ->
      Format.fprintf fmt "%a"
        (pp_list " * " pp_type) ts
    | Tassoc (k, v) ->
      Format.fprintf fmt "(%a, %a) map"
        pp_btyp k
        pp_type_ v
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
  in


  let pp_storage_const fmt = function
    | Get an ->
      Format.fprintf fmt
        "def get_%s (self, key):@\n\
         \t\tself.data.%s_assets[key]@\n"
        an an
    | Set an ->
      Format.fprintf fmt
        "def set_%s (self, key, asset):@\n\
         \t\tself.data.%s_assets[key] = asset@\n"
        an an

    | Add an ->
      let k, _t = Utils.get_asset_key model (to_lident an) in
      Format.fprintf fmt
        "def add_%s (self, asset):@\n\
         \t\tkey = asset.%a@\n\
         \t\tself.data.%s_keys.append(key)@\n\
         \t\tself.data.%s_assets[key] = asset@\n"
        an pp_str k an an

    | Remove an ->
      Format.fprintf fmt
        "def remove_%s (self, key):@\n\
         \t\tself.data.%s_keys.remove(key)@\n\
         \t\tdel self.data.%s_assets[key]@\n"
        an an an

    | Clear an ->
      Format.fprintf fmt
        "def clear_%s (self):@\n\
         \t\t#TODO@\n"
        an

    | Reverse an ->
      Format.fprintf fmt
        "def reverse_%s (self):@\n\
         \t\t#TODO@\n"
        an

    | UpdateAdd (an, fn) ->
      let k, _t = Utils.get_asset_key model (to_lident an) in
      Format.fprintf fmt
        "def add_%s_%s (s, asset, b):@\n\
         \t\tasset = asset.%s.insert(b)@\n\
         \t\tself.data.%s_assets[asset.%a] = asset@\n"
        an fn
        fn
        an pp_str k

    | UpdateRemove (an, fn) ->
      let k, _t = Utils.get_asset_key model (to_lident an) in
      Format.fprintf fmt
        "def remove_%s_%s (s, asset, key):@\n\
         \t\tasset = asset.%s.pop(key)@\n\
         \t\tself.data.%s_assets[asset.%a] = asset@\n"
        an fn
        fn
        an pp_str k

    | UpdateClear (an, fn) ->
      Format.fprintf fmt
        "def clear_%s_%s (self):@\n\
         \t\t#TODO@\n"
        an fn

    | UpdateReverse (an, fn) ->
      Format.fprintf fmt
        "def reverse_%s_%s (self):@\n\
         \t\t#TODO@\n"
        an fn

    | ToKeys an ->
      Format.fprintf fmt
        "def to_keys_%s (self):@\n\
         \t\t#TODO@\n"
        an
  in

  let pp_container_const fmt = function
    | AddItem t-> Format.fprintf fmt "add\t %a" pp_type t
    | RemoveItem t -> Format.fprintf fmt "remove\t %a" pp_type t
    | ClearItem t -> Format.fprintf fmt "clear\t %a" pp_type t
    | ReverseItem t -> Format.fprintf fmt "reverse %a" pp_type t
  in

  let pp_function_const fmt = function
    | Select (an, _) ->
      Format.fprintf fmt
        "def select_%s (self, c, p):@\n\
         \t\treduce(@\n\
         \t\t(lambda x, key:@\n\
         \t\t\titem = get_%s(self, key)@\n\
         \t\t\tif (p item):@\n\
         \t\t\t\tx.insert (key)@\n\
         \t\t\t\tx@\n\
         \t\t\telse:@\n\
         \t\t\t\tx@\n\
         \t\t\t),@\n\
         \t\tself.data.%s_keys,@\n\
         \t\t[])@\n"
        an an an

    | Sort (an, fn) ->
      Format.fprintf fmt
        "def sort_%s_%s (s : storage) : unit =@\n  \
         \t\t#TODO@\n"
        an fn

    | Contains an ->
      Format.fprintf fmt
        "def contains_%s (l, key):@\n\
         \t\tkey in l@\n"
        an

    | Nth an ->
      Format.fprintf fmt
        "def nth_%s (self):@\n\
         \t\t#TODO@\n"
        an

    | Count an ->
      Format.fprintf fmt
        "def count_%s (self):@\n\
         \t\t#TODO@\n"
        an

    | Sum (an, fn) ->
      Format.fprintf fmt
        "def sum_%s_%s (self):@\n\
         \t\treduce(@\n\
         \t\t(lambda x, key: self.data.%s_assets[key].%s + x),@\n\
         \t\tself.data.%s_keys,@\n\
         \t\t0)@\n"
        an fn an fn an

    | Min (an, fn) ->
      Format.fprintf fmt
        "def min_%s_%s (self):@\n\
         \t\t#TODO@\n"
        an fn

    | Max (an, fn) ->
      Format.fprintf fmt
        "def max_%s_%s (self):@\n\
         \t\t#TODO@\n"
        an fn
    | Shallow _ -> ()
    | Unshallow _ -> ()
    | Listtocoll _ -> ()
    | Head an ->
      Format.fprintf fmt
        "def head_%s (self):@\n\
         \t\t#TODO@\n"
        an

    | Tail an ->
      Format.fprintf fmt
        "def tail_%s (self):@\n\
         \t\t#TODO@\n"
        an
  in

  let pp_builtin_const fmt = function
    | MinBuiltin t-> Format.fprintf fmt "min on %a" pp_type t
    | MaxBuiltin t-> Format.fprintf fmt "max on %a" pp_type t
  in

  let pp_api_item_node fmt = function
    | APIStorage   v -> pp_storage_const fmt v
    | APIContainer v -> pp_container_const fmt v
    | APIFunction  v -> pp_function_const fmt v
    | APIBuiltin   v -> pp_builtin_const fmt v
  in

  let pp_api_item fmt (api_item : api_item) =
    if api_item.only_formula
    then ()
    else pp_api_item_node fmt api_item.node_item
  in

  let pp_api_items fmt l =
    let filter_api_items l : api_item list =
      let contains_select_asset_name a_name l : bool =
        List.fold_left (fun accu x ->
            match x.node_item with
            | APIFunction  (Select (an, _)) -> accu || String.equal an a_name
            | _ -> accu
          ) false l
      in
      List.fold_right (fun (x : api_item) accu ->
          match x.node_item with
          | APIFunction  (Select (an, _p)) when contains_select_asset_name an accu -> accu
          | _ -> x::accu
        ) l []
    in
    let l : api_item list = filter_api_items l in
    if List.is_empty l
    then pp_nothing fmt
    else
      Format.fprintf fmt "# API function@\n@\n\t%a@\n"
        (* Format.pp_print_tab () *)
        (pp_list "@\n\t" pp_api_item) l
  in

  let pp_operator fmt op =
    let to_str = function
      | ValueAssign -> "="
      | PlusAssign -> "+="
      | MinusAssign -> "-="
      | MultAssign -> "*="
      | DivAssign -> "/="
      | AndAssign -> "&="
      | OrAssign -> "|="
    in
    pp_str fmt (to_str op)
  in

  (* let rec pp_qualid fmt (q : qualid) =
    match q.node with
    | Qdot (q, i) ->
      Format.fprintf fmt "%a.%a"
        pp_qualid q
        pp_id i
    | Qident i -> pp_id fmt i
  in *)

  let pp_pattern fmt (p : pattern) =
    match p.node with
    | Pconst i -> pp_id fmt i
    | Pwild -> pp_str fmt "_"
  in

  let pp_mterm fmt (mt : mterm) =
    let rec f fmt (mtt : mterm) =
      match mtt.node with
      | Mif (c, { node = Mfail _}, None) ->
        Format.fprintf fmt "@[sp.verify(%a)@]"
          f c

      | Mif (c, t, None) ->
        Format.fprintf fmt "@[sp.if (%a):@\n @[<v 4>%a@]@]"
          f c
          f t

      | Mif (c, t, Some e) ->
        Format.fprintf fmt "@[sp.if (%a):@\n\t@[<v 4>%a@]@\nsp.else:@\n\t@[<v 4>%a@]@]"
          f c
          f t
          f e

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
          Format.fprintf fmt "%a (self, %a)"
            pp_id e
            (pp_list ", " f) args
        in
        pp fmt (e, args)

      | Maddshallow (e, args) ->
        let pp fmt (e, args) =
          Format.fprintf fmt "add_shallow_%a (self, %a)"
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
          Format.fprintf fmt "get_%a (self, %a)"
            pp_str c
            f k
        in
        pp fmt (c, k)

      | Mset (c, l, k, v) ->
        let pp fmt (c, _l, k, v) =
          Format.fprintf fmt "set_%a (self, %a, %a)"
            pp_str c
            f k
            f v
        in
        pp fmt (c, l, k, v)

      | Maddasset (an, i) ->
        let pp fmt (an, i) =
          Format.fprintf fmt "add_%a (self, %a)"
            pp_str an
            f i
        in
        pp fmt (an, i)

      | Maddfield (an, fn, c, i) ->
        let pp fmt (an, fn, c, i) =
          Format.fprintf fmt "add_%a_%a (self, %a, %a)"
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
        let cond, str =
          (match i.type_ with
           | Tasset an ->
             let k, _ = Utils.get_asset_key model an in
             true, "." ^ k
           | _ -> false, ""
          ) in
        let pp fmt (an, i) =
          Format.fprintf fmt "remove_%a (self, %a%a)"
            pp_str an
            f i
            (pp_do_if cond pp_str) str
        in
        pp fmt (an, i)

      | Mremovefield (an, fn, c, i) ->
        let cond, str =
          (match i.type_ with
           | Tasset an ->
             let k, _ = Utils.get_asset_key model an in
             true, "." ^ k
           | _ -> false, ""
          ) in
        let pp fmt (an, fn, c, i) =
          Format.fprintf fmt "remove_%a_%a (self, %a, %a%a)"
            pp_str an
            pp_str fn
            f c
            f i
            (pp_do_if cond pp_str) str
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
          Format.fprintf fmt "clear_%a (self)"
            pp_str an
        in
        pp fmt (an)

      | Mclearfield (an, fn, i) ->
        let pp fmt (an, fn, i) =
          Format.fprintf fmt "clear_%a_%a (self, %a)"
            pp_str an
            pp_str fn
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
          Format.fprintf fmt "reverse_%a (self)"
            pp_str an
        in
        pp fmt (an)

      | Mreversefield (an, fn, i) ->
        let pp fmt (an, fn, i) =
          Format.fprintf fmt "reverse_%a_%a (self, %a)"
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
          Format.fprintf fmt "select_%a (self, %a, fun the -> %a)"
            pp_str an
            f c
            f p
        in
        pp fmt (an, c, p)

      | Msort (an, c, fn, k) ->
        let pp fmt (an, c, fn, _k) =
          Format.fprintf fmt "sort_%a_%a (%a)"
            pp_str an
            pp_str fn
            f c
            (* pp_sort_kind k *) (* TODO: asc / desc *)
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
        let pp fmt (an, fd, _c) =
          Format.fprintf fmt "sum_%a_%a (self)"
            pp_str an
            pp_id fd
            (* f c *)
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
          Format.fprintf fmt "%a & %a"
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
          Format.fprintf fmt "is_empty_%a %a"
            pp_str l
            f r
        in
        pp fmt (l, r)

      | Mnot e ->
        let pp fmt e =
          Format.fprintf fmt "not (%a)"
            f e
        in
        pp fmt e

      | Mmulticomp (_e, _l) ->
        assert false

      | Mequal (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "%a = %a"
            (pp_cast Lhs l.type_ r.type_ f) l
            (pp_cast Rhs l.type_ r.type_ f) r
        in
        pp fmt (l, r)

      | Mnequal (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "%a <> %a"
            (pp_cast Lhs l.type_ r.type_ f) l
            (pp_cast Rhs l.type_ r.type_ f) r
        in
        pp fmt (l, r)

      | Mgt (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "%a > %a"
            (pp_cast Lhs l.type_ r.type_ f) l
            (pp_cast Rhs l.type_ r.type_ f) r
        in
        pp fmt (l, r)

      | Mge (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "%a >= %a"
            (pp_cast Lhs l.type_ r.type_ f) l
            (pp_cast Rhs l.type_ r.type_ f) r
        in
        pp fmt (l, r)

      | Mlt (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "%a < %a"
            (pp_cast Lhs l.type_ r.type_ f) l
            (pp_cast Rhs l.type_ r.type_ f) r
        in
        pp fmt (l, r)

      | Mle (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "%a <= %a"
            (pp_cast Lhs l.type_ r.type_ f) l
            (pp_cast Rhs l.type_ r.type_ f) r
        in
        pp fmt (l, r)

      | Mplus (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "%a + %a"
            (pp_cast Lhs l.type_ r.type_ f) l
            (pp_cast Rhs l.type_ r.type_ f) r
        in
        pp fmt (l, r)

      | Mminus (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "%a - %a"
            (pp_cast Lhs l.type_ r.type_ f) l
            (pp_cast Rhs l.type_ r.type_ f) r
        in
        pp fmt (l, r)

      | Mmult (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "%a * %a"
            (pp_cast Lhs l.type_ r.type_ f) l
            (pp_cast Rhs l.type_ r.type_ f) r
        in
        pp fmt (l, r)

      | Mdiv (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "%a / %a"
            (pp_cast Lhs l.type_ r.type_ f) l
            (pp_cast Rhs l.type_ r.type_ f) r
        in
        pp fmt (l, r)

      | Mmodulo (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "%a %% %a"
            (pp_cast Lhs l.type_ r.type_ f) l
            (pp_cast Rhs l.type_ r.type_ f) r
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

      | Masset l ->
        let asset_name =
          match mtt.type_ with
          | Tasset asset_name -> asset_name
          | _ -> assert false
        in
        let a = Utils.get_info_asset model asset_name in
        let ll = List.map (fun (i,_,_) -> dumloc i) a.values in

        let lll = List.map2 (fun x y -> (x, y)) ll l in

        Format.fprintf fmt "sp.Record ( %a )"
          (pp_list ", " (fun fmt (a, b)->
               Format.fprintf fmt "%a = %a"
                 pp_id a
                 f b)) lll
      | Mletin (ids, ({node = Mseq _l} as a), t, b, _) ->
        Format.fprintf fmt "let %a%a =@\n%ain@\n@[%a@]"
          (pp_if (List.length ids > 1) (pp_paren (pp_list ", " pp_id)) (pp_list ", " pp_id)) ids
          (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t
          f a
          f b
      | Mletin (ids, a, _t, b, _) ->
        Format.fprintf fmt "%a = %a@\n%a"
          (pp_if (List.length ids > 1) (pp_paren (pp_list ", " pp_id)) (pp_list ", " pp_id)) ids
          (* (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t *)
          f a
          f b
      | Mdeclvar (ids, _t, v) ->
        Format.fprintf fmt "%a = %a"
          (pp_if (List.length ids > 1) (pp_paren (pp_list ", " pp_id)) (pp_list ", " pp_id)) ids
          (* (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t *)
          f v
      | Mvarstorevar v -> Format.fprintf fmt "self.data.%a" pp_id v
      | Mvarstorecol v -> Format.fprintf fmt "self.data.%a_keys" pp_id v
      | Mvarenumval v  -> pp_id fmt v
      | Mvarfield v    -> pp_id fmt v
      | Mvarlocal v    -> pp_id fmt v
      | Mvarparam v    -> pp_id fmt v
      | Mvarthe        -> pp_str fmt "the"
      | Mvarstate      -> pp_str fmt "state_"
      | Mnow           -> pp_str fmt "sp.currentTime"
      | Mtransferred   -> pp_str fmt "sp.amount"
      | Mcaller        -> pp_str fmt "sp.sender"
      | Mbalance       -> pp_str fmt "sp.balance"
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
        Format.fprintf fmt "sp.%a(%a)"
          pp_currency c
          pp_big_int v
      | Maddress v ->
        Format.fprintf fmt "sp.address(\"%a\")"
          pp_str v
      | Mduration v -> Core.pp_duration_in_seconds fmt v
      | Mdotasset (e, i)
      | Mdotcontract (e, i) ->
        Format.fprintf fmt "%a.%a"
          f e
          pp_id i
      | Mtuple l ->
        Format.fprintf fmt "(%a)"
          (pp_list ", " f) l
      | Massoc (k, v) ->
        Format.fprintf fmt "(%a : %a)"
          f k
          f v
      | Mfor (i, c, b, _) ->
        Format.fprintf fmt "sp.for %a in %a:@\n\t@[<v 4>%a@]@\n"
          pp_id i
          f c
          f b
      | Miter (_i, _a, _b, _c, _) -> Format.fprintf fmt "TODO: iter@\n"
      | Mfold (i, is, c, b) ->
        let t : lident option =
          match c with
          | {node = Mvarstorecol an; _} -> Some an
          | _ -> None
        in

        let cond = Option.is_some t in

        Format.fprintf fmt
          "List.fold (fun (%a, (%a)) ->@\n\
           %a@[  %a@]) %a (%a)@\n"
          pp_id i (pp_list ", " pp_id) is
          (pp_do_if cond (fun fmt _c ->
               let an = Option.get t in
               Format.fprintf fmt "let %a : %a = get_%a (_s, %a) in  @\n"
                 pp_id i
                 pp_id an
                 pp_id an
                 pp_id i)) c
          f b
          f c
          (pp_list ", " pp_id) is
      | Mseq is ->
        Format.fprintf fmt "@[<v 4>%a@]"
          (pp_list "@\n\t" f) is
      | Massign (op, l, r) ->
        Format.fprintf fmt "%a %a %a"
          pp_id l
          pp_operator op
          f r
      | Massignstate x ->
        Format.fprintf fmt "state = %a"
          f x
      | Massignfield (op, a, field , r) ->
        Format.fprintf fmt "%a.%a %a %a"
          f a
          pp_id field
          pp_operator op
          f r
      | Mtransfer (v, d) ->
        Format.fprintf fmt "transfer %a to %a"
          f v
          f d
      | Mbreak -> emit_error UnsupportedBreak
      | Massert x ->
        Format.fprintf fmt "assert %a"
          f x
      | Mreturn x ->
        Format.fprintf fmt "return %a"
          f x
      | Mlabel _i -> ()
      | Mshallow (i, x) ->
        Format.fprintf fmt "shallow_%a %a"
          pp_str i
          f x
      | Munshallow (i, x) ->
        Format.fprintf fmt "unshallow_%a %a"
          pp_str i
          f x
      | Mtokeys (an, x) ->
        Format.fprintf fmt "%s.to_keys (%a)"
          an
          f x
      | Mlisttocoll (_, x) -> f fmt x
      | Mforall _                        -> emit_error (UnsupportedTerm ("forall"))
      | Mexists _                        -> emit_error (UnsupportedTerm ("exists"))
      | Msetbefore _                     -> emit_error (UnsupportedTerm ("setbefore"))
      | Msetat _                         -> emit_error (UnsupportedTerm ("setat"))
      | Msetunmoved _                    -> emit_error (UnsupportedTerm ("setunmoved"))
      | Msetadded _                      -> emit_error (UnsupportedTerm ("setadded"))
      | Msetremoved _                    -> emit_error (UnsupportedTerm ("setremoved"))
      | Msetiterated _                   -> emit_error (UnsupportedTerm ("setiterated"))
      | Msettoiterate _                  -> emit_error (UnsupportedTerm ("settoiterate"))
      | Mremoveif _                      -> emit_error (UnsupportedTerm ("removeif"))
      | Mgetat _                         -> emit_error (UnsupportedTerm ("getat"))
      | Mgetbefore _                     -> emit_error (UnsupportedTerm ("getbefore"))
    in
    f fmt mt
  in

  let pp_function fmt f =
    let pp_prelude_entrypoint fmt _ =
      Format.fprintf fmt "@@sp.entryPoint@\n\t"
    in
    let pp_args fmt f  =
      match f.node with
      | Entry _ -> Format.fprintf fmt "(self, params)"
      | Function (fs, _) ->
        Format.fprintf fmt "(self, %a)"
          (pp_list ", " (fun fmt (x : argument) -> Format.fprintf fmt "%a" pp_id (proj3_1 x))) fs.args
    in
    let fs = match f.node with
      | Entry f -> f
      | Function (f, _a) -> f
    in
    Format.fprintf fmt "%adef %a%a:@\n\t\t%a"
      (pp_do_if (match f.node with | Entry _ -> true | _ -> false) pp_prelude_entrypoint) ()
      pp_id fs.name
      pp_args f
      pp_mterm fs.body
  in

  let pp_functions fmt fs =
    Format.fprintf fmt "%a"
      (pp_list "@\n@\n\t" pp_function) fs
  in

  let pp_init_function fmt (s : storage) =
    let pp_storage_item fmt (si : storage_item) =
      match si.model_type with
      | MTasset an ->
        Format.fprintf fmt "%s_keys = [],@\n\t\t%s_assets = {}"
          an
          an

      | _ ->
        Format.fprintf fmt "%a = %a"
          pp_id si.id
          pp_mterm si.default
    in

    Format.fprintf fmt "def __init__(self):@\n\tself.init(%a)@\n"
      ( Format.pp_print_list
          ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@\n\t\t")
          pp_storage_item) s
  in

  let pp_test fmt name =
    Format.fprintf fmt "# Tests@\n\
                        @addTest(name = \"test\")@\n\
                        def test():@\n\
                        \t# define a contract@\n\
                        \tc1 = %s()@\n\
                        \t# show its representation@\n\
                        \thtml = c1.fullHtml()@\n\
                        \tsetOutput(html)@\n\
                        \t@\n"
      name
  in

  let name = "Mwe" in
  Format.fprintf fmt "# Smartpy output generated by archetype@\n\
                      @\n\
                      %a@\n\
                      @\n\
                      @\n\
                      import smartpy as sp@\n\
                      @\n\
                      class %s(sp.Contract):@\n\
                      \t%a@\n\
                      \t%a@\n\
                      \t#Functions@\n\
                      @\n\
                      \t@[%a@]@\n\
                      @\n\
                      %a
                      @."
    pp_model_name ()
    name
    pp_init_function model.storage
    pp_api_items model.api_items
    pp_functions model.functions
    pp_test name

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
