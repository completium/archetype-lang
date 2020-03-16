open Location
open Tools
open Model
open Printer_tools

exception Anomaly of string

type error_desc =
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

let const_params = "params"

let pp_cast (pos : position) (ltype : type_) (rtype : type_) (pp : 'a -> mterm -> unit) (fmt : Format.formatter) =
  match pos, ltype, rtype with
  (* | Lhs, Tbuiltin Brole, Tbuiltin Baddress ->
     Format.fprintf fmt "(%a : address)" pp
     | Rhs, Tbuiltin Baddress, Tbuiltin Brole ->
     Format.fprintf fmt "(%a : address)" pp *)
  | _ -> pp fmt

let pp_str fmt str =
  Format.fprintf fmt "%s" str

let to_lident = dumloc

let pp_nothing (_fmt : Format.formatter) = ()

let pp_model fmt (model : model) =

  let contract_name = Tools.String.up_firstcase_only (unloc model.name) in

  let pp_model_name (fmt : Format.formatter) _ =
    Format.fprintf fmt "# contract: %a@\n"
      pp_id model.name
  in

  let pp_currency fmt = function
    | Tz   -> Format.fprintf fmt "tz"
    | Mtz  -> Format.fprintf fmt "mtz"
    | Utz  -> Format.fprintf fmt "utz"
  in

  let pp_btyp fmt = function
    | Bbool       -> Format.fprintf fmt "bool"
    | Bint        -> Format.fprintf fmt "int"
    | Brational   -> Format.fprintf fmt "rational"
    | Bdate       -> Format.fprintf fmt "date"
    | Bduration   -> Format.fprintf fmt "duration"
    | Btimestamp  -> Format.fprintf fmt "timestamp"
    | Bstring     -> Format.fprintf fmt "string"
    | Baddress    -> Format.fprintf fmt "address"
    | Brole       -> Format.fprintf fmt "key_hash"
    | Bcurrency   -> Format.fprintf fmt "tez"
    | Bkey        -> Format.fprintf fmt "key"
    | Bbytes      -> Format.fprintf fmt "bytes"
    | Bnat        -> Format.fprintf fmt "nat"
  in

  let pp_container fmt = function
    | Collection -> Format.fprintf fmt "list"
    | Partition  -> Format.fprintf fmt "list"
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
    | Tlist t ->
      Format.fprintf fmt "%a list"
        pp_type t
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


  let pp_api_asset fmt = function
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
      let k, _t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "def add_%s (self, asset):@\n  \
         key = asset.%a@\n  \
         self.data.%s_assets[key] = asset@\n"
        an pp_str k an

    | Remove an ->
      Format.fprintf fmt
        "def remove_%s (self, key):@\n\
         \t\tself.data.%s_keys.remove(key)@\n\
         \t\tdel self.data.%s_assets[key]@\n"
        an an an

    | Clear _ -> Format.fprintf fmt "// TODO api storage: clear"

    | UpdateAdd (an, fn) ->
      let k, _t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "def add_%s_%s (s, asset, b):@\n\
         \t\tasset = asset.%s.insert(b)@\n\
         \t\tself.data.%s_assets[asset.%a] = asset@\n"
        an fn
        fn
        an pp_str k

    | UpdateRemove (an, fn) ->
      let k, _t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "def remove_%s_%s (s, asset, key):@\n\
         \t\tasset = asset.%s.pop(key)@\n\
         \t\tself.data.%s_assets[asset.%a] = asset@\n"
        an fn
        fn
        an pp_str k

    | UpdateClear _ -> Format.fprintf fmt "// TODO api storage: UpdateClear"

    | ToKeys an ->
      Format.fprintf fmt
        "def to_keys_%s (self):@\n\
         \t\t#TODO@\n"
        an

    | ColToKeys an ->
      Format.fprintf fmt
        "def col_to_keys_%s (self):@\n\
         \t\t#TODO@\n"
        an

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

    | Sort (an, _l) ->
      Format.fprintf fmt
        "def sort_%s (s : storage) : unit =@\n  \
         \t\t#TODO@\n"
        an

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

    | Sum (an, _, _) -> (* TODO *)
      Format.fprintf fmt
        "def sum_%s (self, p):@\n\
         \t\treduce(@\n\
         \t\t(lambda x, key: p(self.data.%s_assets[key]) + x),@\n\
         \t\tself.data.%s_keys,@\n\
         \t\t0)@\n"
        an an an

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

  let pp_api_list fmt = function
    | Lprepend t  -> Format.fprintf fmt "list_prepend\t %a" pp_type t
    | Lcontains t -> Format.fprintf fmt "list_contains\t %a" pp_type t
    | Lcount t    -> Format.fprintf fmt "list_count\t %a" pp_type t
    | Lnth t      -> Format.fprintf fmt "list_nth\t %a" pp_type t
  in

  let pp_api_builtin fmt = function
    | MinBuiltin t -> Format.fprintf fmt "min on %a" pp_type t
    | MaxBuiltin t -> Format.fprintf fmt "max on %a" pp_type t
    | AbsBuiltin t -> Format.fprintf fmt "abs on %a" pp_type t
  in

  let pp_api_internal fmt = function
    | RatEq        -> Format.fprintf fmt "rat_eq"
    | RatCmp       -> Format.fprintf fmt "rat_cmp"
    | RatArith     -> Format.fprintf fmt "rat_arith"
    | RatUminus    -> Format.fprintf fmt "rat_uminus"
    | RatTez       -> Format.fprintf fmt "rat_to_tez"
    | StrConcat    -> Format.fprintf fmt "str_concat"
  in

  let pp_api_item_node fmt = function
    | APIAsset      v -> pp_api_asset    fmt v
    | APIList       v -> pp_api_list     fmt v
    | APIBuiltin    v -> pp_api_builtin  fmt v
    | APIInternal   v -> pp_api_internal fmt v
  in

  let pp_api_item fmt (api_storage : api_storage) =
    pp_api_item_node fmt api_storage.node_item
  in

  let pp_api_items fmt _ =
    let filter_api_items l : api_storage list =
      List.fold_right (fun (x : api_storage) accu ->
          match x.api_loc with
          | OnlyExec | ExecFormula -> x::accu
          | OnlyFormula -> accu
        ) l []
    in
    let l : api_storage list = filter_api_items model.api_items in
    if List.is_empty l
    then pp_nothing fmt
    else
      Format.fprintf fmt "# API function@\n@\n  @[%a@]@\n"
        (pp_list "@\n" (pp_api_item)) l
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
      (* lambda *)
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

      | Mapp (e, args) ->
        let pp fmt (e, args) =
          Format.fprintf fmt "%a (self, %a)"
            pp_id e
            (pp_list ", " f) args
        in
        pp fmt (e, args)


      (* assign *)

      | Massign (op, _, l, r) ->
        Format.fprintf fmt "%a %a %a"
          pp_id l
          pp_operator op
          f r

      | Massignvarstore (op, _, l, r) ->
        Format.fprintf fmt "self.%a %a %a"
          pp_id l
          pp_operator op
          f r

      | Massignfield (op, _, a, field , r) ->
        Format.fprintf fmt "%a.%a %a %a"
          f a
          pp_id field
          pp_operator op
          f r

      | Massignstate x ->
        Format.fprintf fmt "state = %a"
          f x

      | Massignassetstate (an, k, v) ->
        Format.fprintf fmt "state_%a(%a) = %a"
          pp_ident an
          f k
          f v


      (* control *)

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

      | Mfor (i, c, b, _) ->
        Format.fprintf fmt "sp.for %a in %a:@\n\t@[<v 4>%a@]@\n"
          pp_id i
          f c
          f b

      | Miter (_i, _a, _b, _c, _) -> Format.fprintf fmt "TODO: iter@\n"

      | Mseq is ->
        Format.fprintf fmt "@[%a@]"
          (pp_list "@\n" f) is

      | Mreturn x ->
        Format.fprintf fmt "return %a"
          f x

      | Mlabel _ -> ()
      | Mmark  _ -> ()


      (* effect *)

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

      | Mtransfer (v, d) ->
        Format.fprintf fmt "transfer %a to %a"
          f v
          f d

      | Mentrycall (v, d, _, fid, args) ->
        let pp fmt (v, d, fid, args) =
          Format.fprintf fmt "transfer %a to %a call %a (%a)"
            f v
            f d
            pp_id fid
            (pp_list ", " (fun fmt (_, x) -> f fmt x)) args
        in
        pp fmt (v, d, fid, args)

      (* literals *)

      | Mint v -> pp_big_int fmt v
      | Muint v -> pp_big_int fmt v
      | Mbool b -> pp_str fmt (if b then "true" else "false")
      | Menum v -> pp_str fmt v
      | Mrational (n, d) ->
        Format.fprintf fmt "(%a div %a)"
          pp_big_int n
          pp_big_int d
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
      | Mdate v -> Core.pp_date fmt v
      | Mduration v -> Core.pp_duration_in_seconds fmt v
      | Mtimestamp v -> pp_big_int fmt v
      | Mbytes v -> Format.fprintf fmt "0x%s" v


      (* control expression *)

      | Mexprif (c, t, e) ->
        Format.fprintf fmt "@[sp.if (%a):@\n\t@[<v 4>%a@]@\nsp.else:@\n\t@[<v 4>%a@]@]"
          f c
          f t
          f e

      | Mexprmatchwith (e, l) ->
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


      (* composite type constructors *)

      | Mnone ->
        pp_str fmt "None"

      | Msome v ->
        Format.fprintf fmt "Some (%a)"
          f v

      | Marray l ->
        Format.fprintf fmt "{%a}"
          (pp_list "; " f) l

      | Mtuple l ->
        Format.fprintf fmt "(%a)"
          (pp_list ", " f) l

      | Masset l ->
        let asset_name =
          match mtt.type_ with
          | Tasset asset_name -> asset_name
          | _ -> assert false
        in
        let a = Utils.get_asset model (unloc asset_name) in
        let ll = List.map (fun (x : asset_item) -> x.name) a.values in
        let lll = List.map2 (fun x y -> (x, y)) ll l in
        Format.fprintf fmt "sp.Record ( %a )"
          (pp_list ", " (fun fmt (a, b)->
               Format.fprintf fmt "%a = %a"
                 pp_id a
                 f b)) lll

      | Massoc (k, v) ->
        Format.fprintf fmt "(%a : %a)"
          f k
          f v


      (* dot *)

      | Mdotasset (e, i)
      | Mdotcontract (e, i) ->
        Format.fprintf fmt "%a.%a"
          f e
          pp_id i


      (* comparison operators *)

      | Mequal (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "%a == %a"
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

      | Mmulticomp (_e, _l) -> assert false


      (* arithmetic operators *)

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

      | Mnot e ->
        let pp fmt e =
          Format.fprintf fmt "~(%a)"
            f e
        in
        pp fmt e

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


      (* asset api effect *)

      | Maddasset (an, i) ->
        let pp fmt (an, i) =
          Format.fprintf fmt "self.add_%a (%a)"
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

      | Mremoveasset (an, i) ->
        let cond, str =
          (match i.type_ with
           | Tasset an ->
             let k, _ = Utils.get_asset_key model (unloc an) in
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
             let k, _ = Utils.get_asset_key model (unloc an) in
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

      | Mclearasset (an) ->
        let pp fmt (an) =
          Format.fprintf fmt "clear_%a (self)"
            pp_str an
        in
        pp fmt (an)

      | Mclearfield (an, fn, a) ->
        let pp fmt (an, fn, a) =
          Format.fprintf fmt "clear_%a_%a (self, %a)"
            pp_str an
            pp_str fn
            f a
        in
        pp fmt (an, fn, a)

      | Mset (c, l, k, v) ->
        let pp fmt (c, _l, k, v) =
          Format.fprintf fmt "set_%a (self, %a, %a)"
            pp_str c
            f k
            f v
        in
        pp fmt (c, l, k, v)

      | Mupdate _ -> emit_error (UnsupportedTerm ("update"))
      | Mremoveif _ -> emit_error (UnsupportedTerm ("removeif"))
      | Maddupdate _ -> emit_error (UnsupportedTerm ("add_update"))


      (* asset api expression *)

      | Mget (c, k) ->
        let pp fmt (c, k) =
          Format.fprintf fmt "get_%a (self, %a)"
            pp_str c
            f k
        in
        pp fmt (c, k)

      | Mselect (an, c, p) ->
        let pp fmt (an, c, p) =
          Format.fprintf fmt "select_%a (self, %a, fun the -> %a)"
            pp_str an
            f c
            f p
        in
        pp fmt (an, c, p)

      | Msort (an, c, l) ->
        let pp fmt (an, c, l) =
          Format.fprintf fmt "sort_%a (%a, %a)"
            pp_str an
            f c
            (pp_list ", " (fun fmt (a, b) -> Format.fprintf fmt "%a %a" pp_ident a pp_sort_kind b)) l
        in
        pp fmt (an, c, l)

      | Mcontains (an, c, i) ->
        let pp fmt (an, c, i) =
          Format.fprintf fmt "contains_%a (%a, %a)"
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

      | Msum (an, c, p) ->
        let pp fmt (an, c, p) =
          Format.fprintf fmt "sum_%a (self, %a, fun the -> %a)"
            pp_str an
            f c
            f p
        in
        pp fmt (an, c, p)

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


      (* utils *)

      | Mcast (src, dst, v) ->
        let pp fmt (src, dst, v) =
          Format.fprintf fmt "cast_%a_%a(%a)"
            pp_type src
            pp_type dst
            f v
        in
        pp fmt (src, dst, v)

      | Mgetfrommap (an, k, c) ->
        let pp fmt (an, k, c) =
          Format.fprintf fmt "Mgetfrommap_%a (%a, %a)"
            pp_str an
            f k
            f c
        in
        pp fmt (an, k, c)


      (* list api effect *)

      | Mlistprepend (_, c, a) ->
        Format.fprintf fmt "list_prepend (%a, %a)"
          f c
          f a


      (* list api expression *)

      | Mlistcontains (_, c, a) ->
        Format.fprintf fmt "list_contains (%a, %a)"
          f c
          f a

      | Mlistcount (_, c) ->
        Format.fprintf fmt "list_count (%a)"
          f c

      | Mlistnth (_, c, a) ->
        Format.fprintf fmt "list_nth (%a, %a)"
          f c
          f a


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

      | Mchecksignature (k, s, x) ->
        Format.fprintf fmt "check_signature (%a, %a, %a)"
          f k
          f s
          f x


      (* internal functions *)

      | Mstrconcat (l, r)->
        Format.fprintf fmt "str_concat (%a, %a)"
          f l
          f r

      (* constants *)

      | Mvarstate      -> pp_str fmt "state_"
      | Mnow           -> pp_str fmt "sp.currentTime"
      | Mtransferred   -> pp_str fmt "sp.amount"
      | Mcaller        -> pp_str fmt "sp.sender"
      | Mbalance       -> pp_str fmt "sp.balance"
      | Msource        -> pp_str fmt "sp.source"


      (* variables *)

      | Mvarassetstate (an, k) -> Format.fprintf fmt "state_%a(%a)" pp_str an f k
      | Mvarstorevar v -> Format.fprintf fmt "self.data.%a" pp_id v
      | Mvarstorecol v -> Format.fprintf fmt "self.data.%a" pp_id v
      | Mvarenumval v  -> pp_id fmt v
      | Mvarlocal v    -> pp_id fmt v
      | Mvarparam v    -> Format.fprintf fmt "%s.%a" const_params pp_id v
      | Mvarfield v    -> pp_id fmt v
      | Mvarthe        -> pp_str fmt "the"


      (* rational *)

      | Mdivrat    _ -> emit_error (UnsupportedTerm ("div"))
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

      | Minttorat e ->
        let pp fmt e =
          Format.fprintf fmt "int_to_rat (%a)"
            f e
        in
        pp fmt e


      (* functional *)

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


      (* imperative *)

      | Mbreak -> emit_error (UnsupportedTerm ("break"))


      (* shallowing *)

      | Mshallow (i, x) ->
        Format.fprintf fmt "shallow_%a %a"
          pp_str i
          f x

      | Munshallow (i, x) ->
        Format.fprintf fmt "unshallow_%a %a"
          pp_str i
          f x

      | Mlisttocoll (_, x) -> f fmt x

      | Maddshallow (e, args) ->
        let pp fmt (e, args) =
          Format.fprintf fmt "add_shallow_%a (self, %a)"
            pp_str e
            (pp_list ", " f) args
        in
        pp fmt (e, args)


      (* collection keys *)

      | Mtokeys (an, x) ->
        Format.fprintf fmt "%s.to_keys (%a)"
          an
          f x
      | Mcoltokeys an ->
        Format.fprintf fmt "col_to_keys_%s ()"
          an


      (* quantifiers *)

      | Mforall _ -> emit_error (UnsupportedTerm ("forall"))
      | Mexists _ -> emit_error (UnsupportedTerm ("exists"))


      (* formula operators *)

      | Mimply _ -> emit_error (UnsupportedTerm ("imply"))
      | Mequiv _ -> emit_error (UnsupportedTerm ("equiv"))


      (* formula asset collection *)

      | Msetbefore    _ -> emit_error (UnsupportedTerm ("setbefore"))
      | Msetat        _ -> emit_error (UnsupportedTerm ("setat"))
      | Msetunmoved   _ -> emit_error (UnsupportedTerm ("setunmoved"))
      | Msetadded     _ -> emit_error (UnsupportedTerm ("setadded"))
      | Msetremoved   _ -> emit_error (UnsupportedTerm ("setremoved"))
      | Msetiterated  _ -> emit_error (UnsupportedTerm ("setiterated"))
      | Msettoiterate _ -> emit_error (UnsupportedTerm ("settoiterate"))


      (* formula asset collection methods *)

      | Mapifget       _ -> emit_error (UnsupportedTerm ("apifget"))
      | Mapifsubsetof  _ -> emit_error (UnsupportedTerm ("apifsubsetof"))
      | Mapifisempty   _ -> emit_error (UnsupportedTerm ("apifisempty"))
      | Mapifselect    _ -> emit_error (UnsupportedTerm ("apifselect"))
      | Mapifsort      _ -> emit_error (UnsupportedTerm ("apifsort"))
      | Mapifcontains  _ -> emit_error (UnsupportedTerm ("apifcontains"))
      | Mapifnth       _ -> emit_error (UnsupportedTerm ("apifnth"))
      | Mapifcount     _ -> emit_error (UnsupportedTerm ("apifcount"))
      | Mapifsum       _ -> emit_error (UnsupportedTerm ("apifsum"))
      | Mapifhead      _ -> emit_error (UnsupportedTerm ("apifhead"))
      | Mapiftail      _ -> emit_error (UnsupportedTerm ("apiftail"))

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

  let _pp_functions fmt fs =
    Format.fprintf fmt "%a"
      (pp_list "@\n@\n\t" pp_function) fs
  in

  let _pp_init_function fmt (s : storage) =
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

  let pp_contract_init fmt _ =
    Format.fprintf fmt
      "def __init__(self):@\n    \
       self.init(@[%a@])@\n\
       @\n"
      (pp_list ",@\n" (fun fmt (si : storage_item) ->
           Format.fprintf fmt "%a = %a" pp_id si.id (pp_mterm) si.default)
      ) model.storage
  in

  let pp_contract_entry fmt (fs : function_struct) =
    Format.fprintf fmt
      "@sp.entry_point@\n\
       def %a(self, %s):@\n  \
       @[%a@]@\n"
      pp_id fs.name
      const_params
      pp_mterm fs.body
  in

  let pp_contract_fun fmt (fn : function_node) =
    match fn with
    | Entry fs -> pp_contract_entry fmt fs
    | Function _ -> ()
  in

  let pp_contract_funs fmt _ =
    (pp_list "@\n" (fun fmt (f_ : function__) ->
         pp_contract_fun fmt f_.node)
    ) fmt model.functions
  in

  let pp_contract fmt _ =
    Format.fprintf fmt
      "class %s(sp.Contract):@\n  \
       %a@\n\
       @\n  %a  @[%a@]"
      contract_name
      pp_api_items ()
      pp_contract_init ()
      pp_contract_funs ()
  in

  let pp_contract_parameter fmt _ =
    Format.fprintf fmt
      "# We evaluate a contract with parameters.@\n\
       contract = %s()@\n"
      contract_name
  in

  let pp_outro fmt _ =
    Format.fprintf fmt
      "# We need to compile the contract.@\n\
       # It can be done with the following command.@\n\
       import smartpybasic as spb@\n\
       spb.compileContract(contract, targetBaseFilename = \"/tmp/%a\")@\n\
       @\n\
       print(\"Contract compiled in /tmp/%aCode.tz\")"
      pp_id model.name
      pp_id model.name
  in

  Format.fprintf fmt "# Smartpy output generated by %a@\n" pp_bin ();
  Format.fprintf fmt "@\n";
  pp_model_name fmt ();
  Format.fprintf fmt "@\n";
  Format.fprintf fmt "import smartpy as sp@\n";
  Format.fprintf fmt "@\n";
  pp_contract fmt ();
  Format.fprintf fmt "@\n";
  pp_contract_parameter fmt ();
  Format.fprintf fmt "@\n";
  pp_outro fmt ()


(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
