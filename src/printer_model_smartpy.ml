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
    | Bunit       -> Format.fprintf fmt "sp.TUnit"
    | Bbool       -> Format.fprintf fmt "sp.TBool"
    | Bint        -> Format.fprintf fmt "sp.TInt"
    | Brational   -> Format.fprintf fmt "rational"
    | Bdate       -> Format.fprintf fmt "sp.TTimestamp"
    | Bduration   -> Format.fprintf fmt "sp.Tint"
    | Btimestamp  -> Format.fprintf fmt "sp.TTimestamp"
    | Bstring     -> Format.fprintf fmt "sp.TString"
    | Baddress    -> Format.fprintf fmt "sp.TAddress"
    | Brole       -> Format.fprintf fmt "key_hash"
    | Bcurrency   -> Format.fprintf fmt "sp.TMutez"
    | Bsignature  -> Format.fprintf fmt "sp.TSignature"
    | Bkey        -> Format.fprintf fmt "sp.TKey"
    | Bkeyhash    -> Format.fprintf fmt "sp.TKeyHash"
    | Bbytes      -> Format.fprintf fmt "sp.TBytes"
    | Bnat        -> Format.fprintf fmt "sp.TNat"
    | Bchainid    -> Format.fprintf fmt "sp.TChainId"
  in

  let pp_container fmt = function
    | Collection
    | Aggregate
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
    | Tbuiltin b -> pp_btyp fmt b
    | Tcontainer (Tasset an, (Aggregate | Partition)) ->
      let _, ak = Utils.get_asset_key model (unloc an) in
      Format.fprintf fmt "sp.TSet(%a)"
        pp_type ak
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
        pp_type k
    | Tmap (true, k, v) ->
      Format.fprintf fmt "(%a, %a) bigmap"
        pp_type k
        pp_type_ v
    | Tmap (false, k, v) ->
      Format.fprintf fmt "(%a, %a) map"
        pp_type k
        pp_type_ v
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
  in


  let pp_prefix_api_container_kind an fmt = function
    | Coll  -> Format.fprintf fmt "c_%s" an
    | View  -> Format.fprintf fmt "v_%s" an
    | Field (an, fn) -> Format.fprintf fmt "f_%s_%s" an fn
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
         sp.failwith(\"TODO\")@\n"
        an fn

    | RemoveIf (an, _, _, _) ->
      Format.fprintf fmt
        "def removeif_%s (self, s : storage) : unit =@\n  \
         sp.failwith(\"TODO\")@\n"
        an

    | Contains (an, ck) ->
      Format.fprintf fmt
        "def contains_%a (self, %akey):@\n  \
         %a@\n"
        (pp_prefix_api_container_kind an) ck
        (fun fmt _ ->
           match ck with
           | Coll -> ()
           | View -> Format.fprintf fmt "v, "
           | Field (_an, _fn) -> Format.fprintf fmt "ka, "
        ) ()
        (fun fmt _ ->
           match ck with
           | Coll -> Format.fprintf fmt "return self.data.%s_assets.contains(key)" an
           | View ->
             Format.fprintf fmt
               "res = sp.bool(False)@\n  \
                sp.for x in v:@\n    \
                res.value |= x == key@\n  \
                return res"
           | Field (an, fn) -> Format.fprintf fmt "return self.data.%s_assets[ka].%s.contains(key)" an fn) ()

    | Select (an, ck, _, _) ->
      let _, tk = Utils.get_asset_key model an in
      Format.fprintf fmt
        "def select_%a (self%a):@\n  \
         l = sp.local(\"l\", sp.list(t = %a))@\n  \
         sp.for k in self.data.my_asset_assets.keys():@\n    \
         l.value.push(k)@\n  \
         return l@\n"
        (pp_prefix_api_container_kind an) ck
        (fun fmt _ ->
           match ck with
           | Coll -> ()
           | View -> Format.fprintf fmt ", v"
           | Field (_an, _fn) -> Format.fprintf fmt ", ka"
        ) ()
        pp_type tk
    (* Format.fprintf fmt
       "def select_%a (self, c, p):@\n  \
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
       (pp_prefix_api_container_kind an) ck
       an an *)

    | Sort (an, ck, _l) ->
      Format.fprintf fmt
        "def sort_%a (self):@\n  \
         %a@\n"
        (pp_prefix_api_container_kind an) ck
        (fun fmt _ ->
           match ck with
           | Coll -> Format.fprintf fmt "sp.failwith(\"TODO\")"
           | View -> Format.fprintf fmt "sp.failwith(\"TODO\")"
           | Field (_an, _fn) -> Format.fprintf fmt "sp.failwith(\"TODO\")") ()

    | Nth (an, ck) ->
      let _, tk = Utils.get_asset_key model an in
      Format.fprintf fmt
        "def nth_%a (self%a, idx):@\n  \
         %a@\n"
        (pp_prefix_api_container_kind an) ck
        (fun fmt _ ->
           match ck with
           | Coll -> ()
           | View -> Format.fprintf fmt ", v"
           | Field (_an, _fn) -> Format.fprintf fmt ", ka"
        ) ()
        (fun fmt _ ->
           match ck with
           | Coll ->
             Format.fprintf fmt
               "m = sp.map(tkey=sp.TInt, tvalue=%a)@\n  \
                i = 0@\n  \
                sp.for x in self.data.%s_assets.keys():@\n    \
                #m[i] = x@\n    \
                i += 1@\n  \
                return m[idx]"
               pp_type tk
               an

           | View -> Format.fprintf fmt "sp.failwith(\"TODO\")"
           | Field (_an, _fn) -> Format.fprintf fmt "sp.failwith(\"TODO\")") ()

    | Count (an, ck) ->
      Format.fprintf fmt
        "def count_%a (self%a):@\n  \
         %a@\n"
        (pp_prefix_api_container_kind an) ck
        (fun fmt _ ->
           match ck with
           | Coll -> ()
           | View -> Format.fprintf fmt ", v"
           | Field (_an, _fn) -> Format.fprintf fmt ", ka"
        ) ()
        (fun fmt _ ->
           match ck with
           | Coll -> Format.fprintf fmt "return sp.len(self.data.%s_assets)" an
           | View -> Format.fprintf fmt "return sp.to_int(sp.len(v))"
           | Field (an, fn) -> Format.fprintf fmt "return sp.to_int(sp.len(self.data.%s_assets[ka].%s))" an fn) ()

    | Sum (an, ck, _, _) ->
      Format.fprintf fmt
        "def sum_%a (self, p):@\n  \
         reduce(@\n  \
         (lambda x, key: p(self.data.%s_assets[key]) + x),@\n  \
         self.data.%s_keys,@\n  \
         0)@\n"
        (pp_prefix_api_container_kind an) ck
        an an

    | Head (an, ck) ->
      Format.fprintf fmt
        "def head_%a (self, n):@\n  \
         %a@\n"
        (pp_prefix_api_container_kind an) ck
        (fun fmt _ ->
           match ck with
           | Coll -> Format.fprintf fmt "sp.failwith(\"TODO\")"
           | View -> Format.fprintf fmt "sp.failwith(\"TODO\")"
           | Field (_an, _fn) -> Format.fprintf fmt "sp.failwith(\"TODO\")") ()

    | Tail (an, ck) ->
      Format.fprintf fmt
        "def tail_%a (self):@\n  \
         %a@\n"
        (pp_prefix_api_container_kind an) ck
        (fun fmt _ ->
           match ck with
           | Coll -> Format.fprintf fmt "sp.failwith(\"TODO\")"
           | View -> Format.fprintf fmt "sp.failwith(\"TODO\")"
           | Field (_an, _fn) -> Format.fprintf fmt "sp.failwith(\"TODO\")") ()
  in

  let pp_api_list _fmt = function
    | Lprepend  _ -> ()
    | Lcontains _ -> ()
    | Llength   _ -> ()
    | Lnth      _ -> ()
    | Lreverse  _ -> ()

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
    | Boptget t -> Format.fprintf fmt "getopt on %a" pp_type t
    | Bfloor    -> pp_str fmt "floor"
    | Bceil     -> pp_str fmt "ceil"
    | Btostring t -> Format.fprintf fmt "to_string on %a" pp_type t
    | Bfail t     -> Format.fprintf fmt "fail on %a" pp_type t
  in

  let pp_api_internal fmt = function
    | RatEq        -> Format.fprintf fmt "rat_eq"
    | RatCmp       -> Format.fprintf fmt "rat_cmp"
    | RatArith     -> Format.fprintf fmt "rat_arith"
    | RatUminus    -> Format.fprintf fmt "rat_uminus"
    | RatTez       -> Format.fprintf fmt "rat_to_tez"
    | RatDur       -> Format.fprintf fmt "ratdur"
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

  let pp_iter_container_kind f fmt = function
    | ICKcoll an  -> Format.fprintf fmt "%a" pp_str an
    | ICKview mt  -> Format.fprintf fmt "%a" f mt
    | ICKfield (_, _, mt) -> Format.fprintf fmt "%a" f mt
    | ICKset mt  -> Format.fprintf fmt "%a" f mt
    | ICKlist mt  -> Format.fprintf fmt "%a" f mt
    | ICKmap mt  -> Format.fprintf fmt "%a" f mt
  in

  let pp_prefix_container_kind an fmt = function
    | CKcoll _  -> Format.fprintf fmt "c_%s" an
    | CKview _  -> Format.fprintf fmt "v_%s" an
    | CKfield (an, fn, _, _, _) -> Format.fprintf fmt "f_%s_%s" an fn
    | CKdef _   -> assert false
  in

  let pp_transfer_kind f fmt = function
    | TKsimple d           -> Format.fprintf fmt "to %a" f d
    | TKcall (id, _, d, a) -> Format.fprintf fmt "to %a call %s(%a)" f d id f a
    | TKentry (e, a)       -> Format.fprintf fmt "to entry %a(%a)" f e f a
    | TKself (id, args)    -> Format.fprintf fmt "to entry self.%a(%a)" pp_str id (pp_list ", " (fun fmt (id, x) -> Format.fprintf fmt "%s = %a" id f x)) args
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

      | Massign (op, _, Avar l, r) ->
        Format.fprintf fmt "%a %a %a"
          pp_id l
          pp_operator op
          f r

      | Massign (op, _, Avarstore l, r) ->
        Format.fprintf fmt "self.data.%a %a %a"
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
        Format.fprintf fmt "self.data.state = %a"
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
          (fun fmt i -> match i with FIsimple x -> pp_id fmt x | FIdouble (x, y) -> Format.fprintf fmt "(%a, %a)" pp_id x pp_id y) i
          (pp_iter_container_kind f) c
          f b

      | Mwhile (c, b, _) ->
        Format.fprintf fmt "sp.while (%a):@\n  @[%a@]@\n"
          f c
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
        Format.fprintf fmt "sp.failwith (\"%a\")"
          (pp_fail_type f) ft

      | Mtransfer (v, k) ->

        Format.fprintf fmt "transfer %a %a"
          f v
          (pp_transfer_kind f) k
      (* Format.fprintf fmt "sp.send(%a, %a)"
         f d
         f v *)


      (* entrypoint *)

      | Mentrypoint (_, a, s) ->
        Format.fprintf fmt "entrypoint(\"%a\", %a)"
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
      | Munit -> Format.fprintf fmt "Unit"


      (* control expression *)

      | Mexprif (c, t, e) ->
        Format.fprintf fmt "sp.if (%a):@\n  @[%a@]@\nsp.else:@\n  @[%a@]"
          f c
          f t
          f e

      | Mexprmatchwith _ -> emit_error (UnsupportedTerm ("Mexprmatchwith"))
      | Mmatchsome     _ -> emit_error (UnsupportedTerm ("Mmatchsome"))


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
        let t =
          match mtt.type_ with
          | Tset t -> t
          | _ -> assert false
        in
        Format.fprintf fmt "sp.set(l=[%a], t= %a)"
          (pp_list ", " f) l
          pp_type t

      | Mlitlist l ->
        let t =
          match mtt.type_ with
          | Tlist t -> t
          | _ -> assert false
        in
        Format.fprintf fmt "sp.list(l=[%a], t= %a)"
          (pp_list ", " f) l
          pp_type t

      | Mlitmap l ->
        Format.fprintf fmt "%a"
          (fun fmt _ -> begin
               let k, v =
                 match mtt.type_ with
                 | Tmap (_, k, v) -> k, v
                 | _ -> assert false
               in
               Format.fprintf fmt "sp.map(%atkey=%a, tvalue= %a)"
                 (fun fmt _ -> begin
                      match l with
                      | [] -> ()
                      | _  -> Format.fprintf fmt "l = {%a}, " (pp_list ", " (fun fmt (k, v) -> Format.fprintf fmt "%a : %a" f k f v)) l
                    end) l
                 pp_type k
                 pp_type v
             end)
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


      (* comparison operators *)

      | Mequal (_, l, r) ->
        let pp fmt (l, r : mterm * mterm) =
          Format.fprintf fmt "(%a) == (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Mnequal (_, l, r) ->
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

      | Mxor (l, r) ->
        let pp fmt (l, r) =
          Format.fprintf fmt "(%a) ^ (%a)"
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

      | Mdivrat    _ -> emit_error (UnsupportedTerm ("Mdivrat"))

      | Mdiveuc (l, r) ->
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

      | Mremoveif (an, c, la, lb, a) ->
        let pp fmt (an, c, _la, _lb, _a) =
          Format.fprintf fmt "self.removeif_%s (%a)"
            an
            (fun fmt _ ->
               match c with
               | CKcoll _ -> ()
               | CKview mt -> f fmt mt
               | CKfield (_an, _fn, _k, _, _) -> ()
               | CKdef _ -> assert false) ()
        in
        pp fmt (an, c, la, lb, a)

      | Mclear (an, c) ->
        let pp fmt (an, c) =
          Format.fprintf fmt "self.clear_%s (%a)"
            an
            (fun fmt _ ->
               match c with
               | CKcoll _ -> ()
               | CKview mt -> f fmt mt
               | CKfield (_an, _fn, _k, _, _) -> ()
               | CKdef _ -> assert false) ()
        in
        pp fmt (an, c)

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
      | Maddforce  _ -> emit_error (UnsupportedTerm ("Maddforce"))


      (* asset api expression *)

      | Mget (an, c, k) ->
        let pp fmt (an, _c, k) =
          Format.fprintf fmt "self.data.%a_assets[%a]"
            pp_str an
            f k
        in
        pp fmt (an, c, k)

      | Mselect (an, c, la, lb, a) ->
        let pp fmt (an, c, _la, _lb, _a) =
          Format.fprintf fmt "self.select_%a (%a)"
            (pp_prefix_container_kind an) c
            (fun fmt _ ->
               match c with
               | CKcoll _ -> ()
               | CKview mt -> f fmt mt
               | CKfield (_an, _fn, k, _, _) -> f fmt k
               | CKdef _ -> assert false) ()
        in
        pp fmt (an, c, la, lb, a)

      | Msort (an, c, l) ->
        let pp fmt (an, c, l) =
          Format.fprintf fmt "self.sort_%a (%a, %a)"
            pp_str an
            (fun fmt _ ->
               match c with
               | CKcoll _ -> ()
               | CKview mt -> f fmt mt
               | CKfield (_an, _fn, k, _, _) -> f fmt k
               | CKdef _ -> assert false) ()
            (pp_list ", " (fun fmt (a, b) -> Format.fprintf fmt "%a %a" pp_ident a pp_sort_kind b)) l
        in
        pp fmt (an, c, l)

      | Mcontains (an, c, i) ->
        let pp fmt (an, c, i) =
          Format.fprintf fmt "self.contains_%a (%a)"
            (pp_prefix_container_kind an) c
            (fun fmt _ ->
               match c with
               | CKcoll _ -> f fmt i
               | CKview mt -> Format.fprintf fmt "%a, %a" f mt f i
               | CKfield (_an, _fn, k, _, _) -> Format.fprintf fmt "%a, %a" f k f i
               | CKdef _ -> assert false) ()
        in
        pp fmt (an, c, i)

      | Mnth (an, c, i) ->
        let pp fmt (an, c, i) =
          Format.fprintf fmt "self.nth_%a (%a)"
            (pp_prefix_container_kind an) c
            (fun fmt _ ->
               match c with
               | CKcoll _ -> f fmt i
               | CKview _mt -> ()
               | CKfield (_an, _fn, _, _, _) -> ()
               | CKdef _ -> assert false) ()
        in
        pp fmt (an, c, i)

      | Mcount (an, c) ->
        let pp fmt (an, c) =
          Format.fprintf fmt "self.count_%a (%a)"
            (pp_prefix_container_kind an) c
            (fun fmt _ ->
               match c with
               | CKcoll _ -> ()
               | CKview mt -> f fmt mt
               | CKfield (_an, _fn, k, _, _) -> f fmt k
               | CKdef _ -> assert false) ()
        in
        pp fmt (an, c)

      | Msum (an, c, p) ->
        let pp fmt (an, c, _p) =
          Format.fprintf fmt "self.sum_%a (%a)"
            pp_str an
            (fun fmt _ ->
               match c with
               | CKcoll _ -> ()
               | CKview mt -> f fmt mt
               | CKfield (_an, _fn, _k, _, _) -> ()
               | CKdef _ -> assert false) ()
        in
        pp fmt (an, c, p)

      | Mhead (an, c, i) ->
        Format.fprintf fmt "self.head_%a (%a)"
          (pp_prefix_container_kind an) c
          (fun fmt _ ->
             match c with
             | CKcoll _ -> f fmt i
             | CKview _mt -> ()
             | CKfield (_an, _fn, _, _, _) -> ()
             | CKdef _ -> assert false) ()

      | Mtail (an, c, i) ->
        Format.fprintf fmt "self.tail_%a (%a)"
          (pp_prefix_container_kind an) c
          (fun fmt _ ->
             match c with
             | CKcoll _ -> f fmt i
             | CKview _mt -> ()
             | CKfield (_an, _fn, _, _, _) -> ()
             | CKdef _ -> assert false) ()


      (* utils *)

      | Mcast (_src, _dst, v) -> f fmt v
      (* let pp fmt (src, dst, v) =
         Format.fprintf fmt "cast_%a_%a(%a)"
          pp_type src
          pp_type dst
          f v
         in
         pp fmt (src, dst, v) *)

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

      | Msetcontains (_t, c, a) ->
        Format.fprintf fmt "%a.contains (%a)"
          (* pp_type t *)
          f c
          f a

      | Msetlength (_t, c) ->
        Format.fprintf fmt "sp.len (%a)"
          (* pp_type t *)
          f c

      | Msetfold _ -> emit_error (UnsupportedTerm ("Msetfold"))


      (* list api expression *)

      | Mlistprepend (_, c, a) ->
        Format.fprintf fmt "list_prepend (%a, %a)"
          f c
          f a

      | Mlistheadtail _ -> assert false

      | Mlistlength (_, c) ->
        Format.fprintf fmt "sp.len (%a)"
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

      | Mlistfold _ -> emit_error (UnsupportedTerm ("Mlistfold"))

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
        Format.fprintf fmt "%a.get(%a, defaultValue = None)"
          f c
          f k

      | Mmapgetopt (_, _, c, k) ->
        Format.fprintf fmt "%a.get(%a)"
          f c
          f k

      | Mmapcontains (_, _, c, k) ->
        Format.fprintf fmt "%a.contains(%a)"
          f c
          f k

      | Mmaplength (_, _, c) ->
        Format.fprintf fmt "sp.len(%a)"
          f c

      | Mmapfold  _ -> emit_error (UnsupportedTerm ("Mmapfold"))


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
        Format.fprintf fmt "sp.concat (%a, %a)"
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
        Format.fprintf fmt "getopt (%a)"
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
        Format.fprintf fmt "sp.blake2b (%a)"
          f x

      | Msha256 x ->
        Format.fprintf fmt "sp.sha256 (%a)"
          f x

      | Msha512 x ->
        Format.fprintf fmt "sp.sha512 (%a)"
          f x

      | Mhashkey x ->
        Format.fprintf fmt "hash_key (%a)"
          f x

      | Mchecksignature (k, s, x) ->
        Format.fprintf fmt "sp.check_signature (%a, %a, %a)"
          f k
          f s
          f x


      (* constants *)

      | Mnow           -> pp_str fmt "sp.now"
      | Mtransferred   -> pp_str fmt "sp.amount"
      | Mcaller        -> pp_str fmt "sp.sender"
      | Mbalance       -> pp_str fmt "sp.balance"
      | Msource        -> pp_str fmt "sp.source"
      | Mselfaddress   -> pp_str fmt "sp.selfaddress"
      | Mchainid       -> pp_str fmt "sp.chain_id"
      | Mmetadata      -> pp_str fmt "metadata"


      (* variable *)

      | Mvar (an, Vassetstate k, _, _) -> Format.fprintf fmt "state_%a(%a)" pp_str (unloc an) f k
      | Mvar (v, Vstorevar, _, _) ->
        Format.fprintf fmt "self%a.%a"
          (fun fmt b -> match b with false -> pp_str fmt ".data" | _ -> ()) (is_const env v)
          pp_id v
      | Mvar (v, Vstorecol, _, _) -> Format.fprintf fmt "self.data.%a" pp_id v
      | Mvar (v, Venumval, _, _)  -> pp_id fmt v
      | Mvar (v, Vdefinition, _, _)  -> pp_id fmt v
      | Mvar (v, Vlocal, _, _)    ->
        Format.fprintf fmt "%a%a"
          (fun fmt b -> match b with true -> pp_str fmt "self." | _ -> ()) (is_const env v)
          pp_id v
      | Mvar (v, Vparam, _, _)    -> Format.fprintf fmt "%s.%a" const_params pp_id v
      | Mvar (v, Vfield, _, _)    -> pp_id fmt v
      | Mvar (_, Vthe, _, _)      -> pp_str fmt "the"
      | Mvar (_, Vstate, _, _)    -> pp_str fmt "self.data.state"


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

      | Mforall _ -> emit_error (UnsupportedTerm ("Mforall"))
      | Mexists _ -> emit_error (UnsupportedTerm ("Mexists"))


      (* formula operators *)

      | Mimply _ -> emit_error (UnsupportedTerm ("Mimply"))
      | Mequiv _ -> emit_error (UnsupportedTerm ("Mequiv"))


      (* formula asset collection *)

      | Msetiterated  _ -> emit_error (UnsupportedTerm ("Msetiterated"))
      | Msettoiterate _ -> emit_error (UnsupportedTerm ("Msettoiterate"))


      (* formula asset collection methods *)

      | Mempty     _ -> emit_error (UnsupportedTerm ("Mempty"))
      | Msingleton _ -> emit_error (UnsupportedTerm ("Msingleton"))
      | Msubsetof  _ -> emit_error (UnsupportedTerm ("Msubsetof"))
      | Misempty   _ -> emit_error (UnsupportedTerm ("Misempty"))
      | Munion     _ -> emit_error (UnsupportedTerm ("Munion"))
      | Minter     _ -> emit_error (UnsupportedTerm ("Minter"))
      | Mdiff      _ -> emit_error (UnsupportedTerm ("Mdiff"))
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
    | Dasset _a    -> ()
    | Drecord _r   -> ()
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
    | Getter (fs, r) -> (pp_function env) fmt (fs, r)
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
