open Location
open Tools
open Model
open Printer_tools
open Printer_model_tools

let const_params = "params"

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
    | Tz   -> Format.fprintf fmt "tez"
    | Mtz  -> emit_error (UnsupportedValue ("Mtz"))
    | Utz  -> Format.fprintf fmt "mutez"
  in

  let pp_btyp fmt = function
    | Bbool       -> Format.fprintf fmt "sp.TBool"
    | Bint        -> Format.fprintf fmt "sp.TInt"
    | Brational   -> Format.fprintf fmt "rational"
    | Bdate       -> Format.fprintf fmt "date"
    | Bduration   -> Format.fprintf fmt "duration"
    | Btimestamp  -> Format.fprintf fmt "sp.TTimestamp"
    | Bstring     -> Format.fprintf fmt "sp.TString"
    | Baddress    -> Format.fprintf fmt "sp.TAddress"
    | Brole       -> Format.fprintf fmt "key_hash"
    | Bcurrency   -> Format.fprintf fmt "sp.TMutez"
    | Bsignature  -> Format.fprintf fmt "sp.TSignature"
    | Bkey        -> Format.fprintf fmt "sp.TKey"
    | Bbytes      -> Format.fprintf fmt "sp.TBytes"
    | Bnat        -> Format.fprintf fmt "sp.TNat"
  in

  let pp_container fmt = function
    | Collection
    | Partition
    | View  -> Format.fprintf fmt "list"
  in

  let rec pp_type fmt t =
    match t with
    | Tasset an ->
      let fields : (string * type_) list =
        Utils.get_asset model (unloc an)
        |> (fun x -> x.values)
        |> List.map (fun (x : asset_item) -> (unloc x.name, x.type_))
      in
      Format.fprintf fmt "sp.TRecord(%a)" (pp_list ", " (fun fmt (i, t) -> Format.fprintf fmt "%a = %a" pp_ident i pp_type t)) fields
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
    | Tset k ->
      Format.fprintf fmt "%a set"
        pp_btyp k
    | Tmap (k, v) ->
      Format.fprintf fmt "(%a, %a) map"
        pp_btyp k
        pp_type_ v
    | Trecord l ->
      Format.fprintf fmt "(%a) record"
        (pp_list "; " (fun fmt (lbl, x) -> Format.fprintf fmt "(%s, %a)" lbl  pp_type x)) l
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
    | Get _ -> ()
    (* Format.fprintf fmt
       "def get_%s (self, key):@\n  \
       self.data.%s_assets[key]@\n"
       an an *)

    | Set an ->
      Format.fprintf fmt
        "def set_%s (self, key, asset):@\n  \
         self.data.%s_assets[key] = asset@\n"
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
        "def remove_%s (self, key):@\n  \
         del self.data.%s_assets[key]@\n"
        an an

    | Clear (an, _) ->
      Format.fprintf fmt
        "def clear_%s (self):@\n  \
         self.data.%s_assets = {}@\n"
        an
        an

    | Update (an, _l) ->
      (* let k, _t = Utils.get_asset_key model an in *)
      Format.fprintf fmt
        "def update_%s (self, asset, b):@\n  \
         TODO@\n"
        an

    | FieldAdd (an, fn) ->
      let k, _t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "def add_%s_%s (self, asset, b):@\n  \
         self.data.%s_assets[asset.%a] = asset@\n"
        an fn
        an pp_str k

    | FieldRemove (an, fn) ->
      let k, _t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "def remove_%s_%s (self, asset, key):@\n  \
         asset = asset.%s.pop(key)@\n  \
         self.data.%s_assets[asset.%a] = asset@\n"
        an fn
        fn
        an pp_str k

    | RemoveAll (an, fn) ->
      Format.fprintf fmt
        "def remove_all_%s_%s (self, s : storage) : unit =@\n  \
         #TODO@\n"
        an fn

    | Contains (an, _) ->
      Format.fprintf fmt
        "def contains_%s (self, l, key):@\n  \
         key in l@\n"
        an

    | Select (an, _, _, _) ->
      Format.fprintf fmt
        "def select_%s (self, c, p):@\n  \
         reduce(@\n  \
         (lambda x, key:@\n  \
         item = get_%s(self, key)@\n  \
         if (p item):@\n  \
         x.insert (key)@\n  \
         x@\n  \
         else:@\n  \
         x@\n  \
         ),@\n  \
         self.%s_keys,@\n  \
         [])@\n"
        an an an

    | Sort (an, _, _l) ->
      Format.fprintf fmt
        "def sort_%s (self, s : storage) : unit =@\n  \
         #TODO@\n"
        an

    | Nth (an, _) ->
      Format.fprintf fmt
        "def nth_%s (self):@\n  \
         #TODO@\n"
        an

    | Count (an, _) ->
      Format.fprintf fmt
        "def count_%s (self):@\n  \
         #TODO@\n"
        an

    | Sum (an, _, _, _) -> (* TODO *)
      Format.fprintf fmt
        "def sum_%s (self, p):@\n  \
         reduce(@\n  \
         (lambda x, key: p(self.data.%s_assets[key]) + x),@\n  \
         self.data.%s_keys,@\n  \
         0)@\n"
        an an an

    | Head (an, _) ->
      Format.fprintf fmt
        "def head_%s (self):@\n  \
         #TODO@\n"
        an

    | Tail (an, _) ->
      Format.fprintf fmt
        "def tail_%s (self):@\n  \
         #TODO@\n"
        an
  in

  let pp_api_list fmt = function
    | Lprepend t  -> Format.fprintf fmt "list_prepend\t %a" pp_type t
    | Lcontains t -> Format.fprintf fmt "list_contains\t %a" pp_type t
    | Lcount t    -> Format.fprintf fmt "list_count\t %a" pp_type t
    | Lnth t      -> Format.fprintf fmt "list_nth\t %a" pp_type t
  in

  let pp_api_builtin fmt = function
    | Bmin _ -> ()
    | Bmax _ -> ()
    | Babs    t -> Format.fprintf fmt "abs on %a" pp_type t
    | Bconcat t -> Format.fprintf fmt "concat on %a" pp_type t
    | Bslice  t -> Format.fprintf fmt "slice on %a"  pp_type t
    | Blength t -> Format.fprintf fmt "length on %a" pp_type t
    | Bisnone t -> Format.fprintf fmt "isnone on %a" pp_type t
    | Bissome t -> Format.fprintf fmt "issome on %a" pp_type t
    | Bgetopt t -> Format.fprintf fmt "getopt on %a" pp_type t
    | Bfloor    -> pp_str fmt "floor"
    | Bceil     -> pp_str fmt "ceil"
  in

  let pp_api_internal fmt = function
    | RatEq        -> Format.fprintf fmt "rat_eq"
    | RatCmp       -> Format.fprintf fmt "rat_cmp"
    | RatArith     -> Format.fprintf fmt "rat_arith"
    | RatUminus    -> Format.fprintf fmt "rat_uminus"
    | RatTez       -> Format.fprintf fmt "rat_to_tez"
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

  let _pp_pretty_type fmt t =
    match t with
    | Ttuple[Tbuiltin Bint; Tbuiltin Bint] -> pp_type fmt (Tbuiltin Brational)
    | _ -> pp_type fmt t
  in

  let pp_operator fmt op =
    let to_str = function
      | ValueAssign -> "="
      | PlusAssign  -> "+="
      | MinusAssign -> "-="
      | MultAssign  -> "*="
      | DivAssign   -> "/="
      | AndAssign   -> "&="
      | OrAssign    -> "|="
    in
    pp_str fmt (to_str op)
  in

  let pp_container_kind f fmt = function
    | CKcoll -> pp_str fmt "_Coll_"
    | CKview mt -> f fmt mt
  in

  let pp_iter_container_kind f fmt = function
    | ICKcoll an -> Format.fprintf fmt "%a" pp_str an
    | ICKview mt -> Format.fprintf fmt "%a" f mt
    | ICKlist mt -> Format.fprintf fmt "%a" f mt
  in

  let pp_mterm (env : Printer_model_tools.env) fmt (mt : mterm) =
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
          Format.fprintf fmt "self.%a (%a)"
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

      | Massignfield (op, _t, an, fn, k, v) ->
        Format.fprintf fmt "%a[%a].%a %a %a"
          pp_id an
          f k
          pp_id fn
          pp_operator op
          f v

      | Massignstate x ->
        Format.fprintf fmt "self.data.state = %a"
          f x

      | Massignassetstate (an, k, v) ->
        Format.fprintf fmt "state_%a(%a) = %a"
          pp_ident an
          f k
          f v


      (* control *)

      | Mif (c, t, None)
      | Mif (c, t, Some {node = Mseq []; _}) ->
        Format.fprintf fmt "sp.if (%a):@\n  @[%a@]"
          f c
          f t

      | Mif (c, t, Some e) ->
        Format.fprintf fmt "sp.if (%a):@\n  @[%a@]@\nsp.else:@\n  @[%a@]"
          f c
          f t
          f e

      | Mmatchwith _ -> emit_error (UnsupportedTerm ("Mmatchwith"))

      | Mfor (i, c, b, _) ->
        Format.fprintf fmt "sp.for %a in %a:@\n  @[%a@]@\n"
          pp_id i
          (pp_iter_container_kind f) c
          f b

      | Miter (i, a, b, c, _) ->
        Format.fprintf fmt "sp.for %a in range(%a, %a):@\n  @[%a@]@\n"
          pp_id i
          f a
          f b
          f c

      | Mseq is -> (pp_list "@\n" f) fmt is

      | Mreturn x -> f fmt x

      | Mlabel _ -> emit_error (UnsupportedTerm ("Mlabel"))
      | Mmark  _ -> emit_error (UnsupportedTerm ("Mmark"))


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
        Format.fprintf fmt "sp.failwith (\"%a\")"
          pp_fail_type ft

      | Mtransfer (v, d) ->
        Format.fprintf fmt "sp.send(%a, %a)"
          f d
          f v

      | Mentrycall (v, d, _, fid, args) ->
        let pp fmt (v, d, fid, args) =
          Format.fprintf fmt "sp.transfer(%a(%a), %a, %a)"
            pp_id fid
            (pp_list ", " (fun fmt (_, x) -> f fmt x)) args
            f v
            f d
        in
        pp fmt (v, d, fid, args)

      (* literals *)

      | Mint v -> pp_big_int fmt v
      | Muint v -> pp_big_int fmt v
      | Mbool b -> Format.fprintf fmt "sp.bool(%s)" (if b then "True" else "False")
      | Menum v -> pp_str fmt v
      | Mrational _ -> assert false
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
      | Mtimestamp v ->
        Format.fprintf fmt "sp.timestamp(%a)"
          pp_big_int v
      | Mbytes v -> Format.fprintf fmt "sp.bytes('0x%s')" v


      (* control expression *)

      | Mexprif (c, t, e) ->
        Format.fprintf fmt "sp.if (%a):@\n  @[%a@]@\nsp.else:@\n  @[%a@]"
          f c
          f t
          f e

      | Mexprmatchwith _ -> emit_error (UnsupportedTerm ("Mexprmatchwith"))


      (* composite type constructors *)

      | Mnone ->
        pp_str fmt "sp.none"

      | Msome v ->
        Format.fprintf fmt "sp.some(%a)"
          f v

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
        Format.fprintf fmt "sp.record ( %a )"
          (pp_list ", " (fun fmt (a, b)->
               Format.fprintf fmt "%a = %a"
                 pp_id a
                 f b)) lll

      | Massets l ->
        Format.fprintf fmt "[%a]"
          (pp_list "; " f) l

      | Mlitset l ->
        Format.fprintf fmt "[%a]"
          (pp_list "; " f) l

      | Mlitlist l ->
        Format.fprintf fmt "[%a]"
          (pp_list "; " f) l

      | Mlitmap l ->
        Format.fprintf fmt "%a"
          (fun fmt _ ->
             match l with
             | [] ->
               begin
                 let k, v =
                   match mtt.type_ with
                   | Tmap (k, v) -> k, v
                   | _ -> assert false
                 in
                 Format.fprintf fmt "sp.map(tkey=%a, tvalue= %a)" pp_btyp k pp_type v
               end
             | _  -> emit_error (TODO ("Mlitmap : handle map with data")))
          ()

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

      | Mdotcontract (e, i) ->
        Format.fprintf fmt "%a.%a"
          f e
          pp_id i

      | Maccestuple (e, i) ->
        Format.fprintf fmt "%a[%a]"
          f e
          pp_big_int i

      (* comparison operators *)

      | Mequal (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "(%a) == (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Mnequal (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "(%a) != (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Mgt (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "(%a) > (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Mge (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "(%a) >= (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Mlt (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "(%a) < (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Mle (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "(%a) <= (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Mmulticomp (_e, _l) -> assert false


      (* arithmetic operators *)

      | Mand (l, r) ->
        let pp fmt (l, r) =
          Format.fprintf fmt "(%a) & (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Mor (l, r) ->
        let pp fmt (l, r) =
          Format.fprintf fmt "(%a) | (%a)"
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
          Format.fprintf fmt "(%a) + (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Mminus (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "(%a) - (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Mmult (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "(%a) * (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Mdiv (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "(%a) / (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Mmodulo (l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "(%a) %% (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Muplus e ->
        let pp fmt e =
          Format.fprintf fmt "+(%a)"
            f e
        in
        pp fmt e

      | Muminus e ->
        let pp fmt e =
          Format.fprintf fmt "-(%a)"
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
          Format.fprintf fmt "self.add_%a_%a (%a, %a)"
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
          Format.fprintf fmt "self.remove_%a (%a%a)"
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
          Format.fprintf fmt "self.remove_%a_%a (self, %a, %a%a)"
            pp_str an
            pp_str fn
            f c
            f i
            (pp_do_if cond pp_str) str
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

      | Mclear (an, v) ->
        let pp fmt (an, v) =
          Format.fprintf fmt "self.clear_%a (%a)"
            pp_str an
            (pp_container_kind f) v
        in
        pp fmt (an, v)

      | Mset (c, l, k, v) ->
        let pp fmt (c, _l, k, v) =
          Format.fprintf fmt "self.set_%a (%a, %a)"
            pp_str c
            f k
            f v
        in
        pp fmt (c, l, k, v)

      | Mupdate    _ -> emit_error (UnsupportedTerm ("Mupdate"))
      | Maddupdate _ -> emit_error (UnsupportedTerm ("Maddupdate"))


      (* asset api expression *)

      | Mget (an, c, k) ->
        let pp fmt (an, _c, k) =
          Format.fprintf fmt "self.data.%a_assets[%a]"
            pp_str an
            f k
        in
        pp fmt (an, c, k)

      | Mselect (an, c, la, lb, a) ->
        let pp fmt (an, c, _la, lb, _a) =
          Format.fprintf fmt "self.select_%a (%a, fun the -> %a)"
            pp_str an
            (pp_container_kind f) c
            f lb
        in
        pp fmt (an, c, la, lb, a)

      | Msort (an, c, l) ->
        let pp fmt (an, c, l) =
          Format.fprintf fmt "self.sort_%a (%a, %a)"
            pp_str an
            (pp_container_kind f) c
            (pp_list ", " (fun fmt (a, b) -> Format.fprintf fmt "%a %a" pp_ident a pp_sort_kind b)) l
        in
        pp fmt (an, c, l)

      | Mcontains (an, c, i) ->
        let pp fmt (an, c, i) =
          Format.fprintf fmt "self.contains_%a (%a, %a)"
            pp_str an
            (pp_container_kind f) c
            f i
        in
        pp fmt (an, c, i)

      | Mnth (an, c, i) ->
        let pp fmt (an, c, i) =
          Format.fprintf fmt "self.nth_%a (%a, %a)"
            pp_str an
            (pp_container_kind f) c
            f i
        in
        pp fmt (an, c, i)

      | Mcount (an, c) ->
        let pp fmt (an, c) =
          Format.fprintf fmt "self.count_%a (%a)"
            pp_str an
            (pp_container_kind f) c
        in
        pp fmt (an, c)

      | Msum (an, c, p) ->
        let pp fmt (an, c, p) =
          Format.fprintf fmt "self.sum_%a (%a, fun the -> %a)"
            pp_str an
            (pp_container_kind f) c
            f p
        in
        pp fmt (an, c, p)

      | Mhead (an, c, i) ->
        Format.fprintf fmt "self.head_%a (%a, %a)"
          pp_str an
          (pp_container_kind f) c
          f i

      | Mtail (an, c, i) ->
        Format.fprintf fmt "self.tail_%a (%a, %a)"
          pp_str an
          (pp_container_kind f) c
          f i


      (* utils *)

      | Mcast (_src, _dst, v) -> f fmt v
      (* let pp fmt (src, dst, v) =
         Format.fprintf fmt "cast_%a_%a(%a)"
          pp_type src
          pp_type dst
          f v
         in
         pp fmt (src, dst, v) *)


      (* list api expression *)

      | Mlistprepend (_, c, a) ->
        Format.fprintf fmt "list_prepend (%a, %a)"
          f c
          f a

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
        Format.fprintf fmt "sp.max (%a, %a)"
          f l
          f r

      | Mmin (l, r) ->
        Format.fprintf fmt "sp.min (%a, %a)"
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

      | Mgetopt x ->
        Format.fprintf fmt "getopt (%a)"
          f x

      | Mfloor x ->
        Format.fprintf fmt "floor (%a)"
          f x

      | Mceil x ->
        Format.fprintf fmt "ceil (%a)"
          f x


      (* crypto functions *)

      | Mblake2b x ->
        Format.fprintf fmt "sp.blake2b (%a)"
          f x

      | Msha256 x ->
        Format.fprintf fmt "sp.sha256 (%a)"
          f x

      | Msha512 x ->
        Format.fprintf fmt "sp.sha512 (%a)"
          f x

      | Mchecksignature (k, s, x) ->
        Format.fprintf fmt "sp.check_signature (%a, %a, %a)"
          f k
          f s
          f x


      (* constants *)

      | Mvarstate      -> pp_str fmt "self.data.state"
      | Mnow           -> pp_str fmt "sp.now"
      | Mtransferred   -> pp_str fmt "sp.amount"
      | Mcaller        -> pp_str fmt "sp.sender"
      | Mbalance       -> pp_str fmt "sp.balance"
      | Msource        -> pp_str fmt "sp.source"


      (* variables *)

      | Mvarassetstate (an, k) -> Format.fprintf fmt "state_%a(%a)" pp_str an f k
      | Mvarstorevar v ->
        Format.fprintf fmt "self%a.%a"
          (fun fmt b -> match b with false -> pp_str fmt ".data" | _ -> ()) (is_const env v)
          pp_id v
      | Mvarstorecol v -> Format.fprintf fmt "self.data.%a" pp_id v
      | Mvarenumval v  -> pp_id fmt v
      | Mvarlocal v    ->
        Format.fprintf fmt "%a%a"
          (fun fmt b -> match b with true -> pp_str fmt "self." | _ -> ()) (is_const env v)
          pp_id v
      | Mvarparam v    -> Format.fprintf fmt "%s.%a" const_params pp_id v
      | Mvarfield v    -> pp_id fmt v
      | Mvarthe        -> pp_str fmt "the"


      (* rational *)

      | Mdivrat    _ -> emit_error (UnsupportedTerm ("Mdivrat"))
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

      | Mfold _ -> emit_error (UnsupportedTerm "Mfold")


      (* imperative *)

      | Mbreak -> emit_error (UnsupportedTerm ("Mbreak"))


      (* quantifiers *)

      | Mforall _ -> emit_error (UnsupportedTerm ("Mforall"))
      | Mexists _ -> emit_error (UnsupportedTerm ("Mexists"))


      (* formula operators *)

      | Mimply _ -> emit_error (UnsupportedTerm ("Mimply"))
      | Mequiv _ -> emit_error (UnsupportedTerm ("Mequiv"))


      (* formula asset collection *)

      | Msetbefore    _ -> emit_error (UnsupportedTerm ("Msetbefore"))
      | Msetat        _ -> emit_error (UnsupportedTerm ("Msetat"))
      | Msetunmoved   _ -> emit_error (UnsupportedTerm ("Msetunmoved"))
      | Msetadded     _ -> emit_error (UnsupportedTerm ("Msetadded"))
      | Msetremoved   _ -> emit_error (UnsupportedTerm ("Msetremoved"))
      | Msetiterated  _ -> emit_error (UnsupportedTerm ("Msetiterated"))
      | Msettoiterate _ -> emit_error (UnsupportedTerm ("Msettoiterate"))


      (* formula asset collection methods *)

      | Mapifget       _ -> emit_error (UnsupportedTerm ("Mapifget"))
      | Mapifpureget   _ -> emit_error (UnsupportedTerm ("Mapifpureget"))
      | Mapifsubsetof  _ -> emit_error (UnsupportedTerm ("Mapifsubsetof"))
      | Mapifisempty   _ -> emit_error (UnsupportedTerm ("Mapifisempty"))
      | Mapifselect    _ -> emit_error (UnsupportedTerm ("Mapifselect"))
      | Mapifsort      _ -> emit_error (UnsupportedTerm ("Mapifsort"))
      | Mapifcontains  _ -> emit_error (UnsupportedTerm ("Mapifcontains"))
      | Mapifnth       _ -> emit_error (UnsupportedTerm ("Mapifnth"))
      | Mapifcount     _ -> emit_error (UnsupportedTerm ("Mapifcount"))
      | Mapifsum       _ -> emit_error (UnsupportedTerm ("Mapifsum"))
      | Mapifhead      _ -> emit_error (UnsupportedTerm ("Mapifhead"))
      | Mapiftail      _ -> emit_error (UnsupportedTerm ("Mapiftail"))

    in
    f fmt mt
  in


  let pp_var env (fmt : Format.formatter) (var : var) =
    if (var.constant) then
      begin
        if Option.is_none var.default
        then assert false;
        Format.fprintf fmt "self.%a = %a@\n"
          pp_id var.name
          (pp_mterm env) (Option.get var.default)
      end
  in

  let pp_decl (env : env) (fmt : Format.formatter) (decl : decl_node) =
    match decl with
    | Dvar v       -> pp_var env fmt v
    | Denum _e     -> ()
    | Dasset _r    -> ()
    | Dcontract _c -> ()
  in

  let pp_decls (env : env) (fmt : Format.formatter) _ =
    match model.decls with
    | [] -> ()
    | l -> (pp_list "" (pp_decl env)) fmt l
  in

  let pp_contract_init_call (env : env) fmt _ =
    let l = List.filter (fun x -> not x.const) model.storage in
    Format.fprintf fmt
      "self.init(@[%a@])"
      (pp_list ",@\n" (fun fmt (si : storage_item) ->
           Format.fprintf fmt "%a = %a" pp_id si.id (pp_mterm env) si.default)
      ) l
  in

  let pp_contract_init (env : env) fmt _ =
    Format.fprintf fmt
      "def __init__(self):@\n    @[%a%a@]@\n@\n"
      (pp_decls env) ()
      (pp_contract_init_call env) ()
  in

  let pp_contract_entry (env : env) fmt (fs : function_struct) =
    Format.fprintf fmt
      "@sp.entry_point@\n\
       def %a(self, %s):@\n  \
       @[%a@]@\n"
      pp_id fs.name
      const_params
      (pp_mterm env) fs.body
  in

  let pp_function (env : env) fmt (fs, _r : function_struct * type_) =
    Format.fprintf fmt
      "def %a(self, %a):@\n  \
       @[%a@]@\n"
      pp_id fs.name
      (pp_list ", " (fun fmt (x : argument) -> Format.fprintf fmt "%a" pp_id (proj3_1 x))) fs.args
      (pp_mterm env) fs.body
  in

  let pp_contract_fun (env : env) fmt (fn : function_node) =
    match fn with
    | Entry fs -> (pp_contract_entry env) fmt fs
    | Function (fs, r) -> (pp_function env) fmt (fs, r)
  in

  let pp_contract_funs (env : env) fmt _ =
    (pp_list "@\n" (fun fmt (f_ : function__) ->
         pp_contract_fun env fmt f_.node)
    ) fmt model.functions
  in

  let pp_contract (env : env) fmt _ =
    Format.fprintf fmt
      "class %s(sp.Contract):@\n  \
       %a@\n\
       @\n  %a  @[%a@]"
      contract_name
      pp_api_items ()
      (pp_contract_init env) ()
      (pp_contract_funs env) ()
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

  let env = compute_env model in
  Format.fprintf fmt "# Smartpy output generated by %a@\n" pp_bin ();
  Format.fprintf fmt "@\n";
  pp_model_name fmt ();
  Format.fprintf fmt "@\n";
  Format.fprintf fmt "import smartpy as sp@\n";
  Format.fprintf fmt "@\n";
  pp_contract env fmt ();
  Format.fprintf fmt "@\n";
  pp_contract_parameter fmt ();
  Format.fprintf fmt "@\n";
  pp_outro fmt ()


(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
