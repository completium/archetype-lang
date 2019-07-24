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


let pp_str fmt str =
  Format.fprintf fmt "%s" str

let to_lident = dumloc

let pp_nothing (fmt : Format.formatter) = ()

let pp_model fmt (model : model) =

  let pp_model_name (fmt : Format.formatter) _ =
    Format.fprintf fmt "(* contract: %a *)"
      pp_id model.name
  in

  let pp_currency fmt = function
    | Tez   -> Format.fprintf fmt "tz"
    | Mutez -> Format.fprintf fmt "mtz"
  in

  let pp_btyp fmt = function
    | Bbool       -> Format.fprintf fmt "bool"
    | Bint        -> Format.fprintf fmt "int"
    | Buint       -> Format.fprintf fmt "nat"
    | Brational   -> Format.fprintf fmt "rational"
    | Bdate       -> Format.fprintf fmt "timestamp"
    | Bduration   -> Format.fprintf fmt "duration"
    | Bstring     -> Format.fprintf fmt "string"
    | Baddress    -> Format.fprintf fmt "address"
    | Brole       -> Format.fprintf fmt "address"
    | Bcurrency c -> pp_currency fmt c
    | Bkey        -> Format.fprintf fmt "key"
  in

  let pp_container fmt = function
    | Collection -> Format.fprintf fmt "list"
    | Partition  -> Format.fprintf fmt "list"
  in

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
        pp_type t
    | Ttuple ts ->
      Format.fprintf fmt "%a"
        (pp_list " * " pp_type) ts
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
      let _, t = Utils.get_record_key model (to_lident an) in
      Format.fprintf fmt
        "let[@inline] get_%s (s, key : storage * %a) : %s =@\n  \
         match Map.find key s.%s_assets with@\n  \
         | Some v -> v@\n  \
         | _ -> failwith \"not_found\"@\n"
        an
        pp_btyp t
        an
        an
    | Set an ->
      let _, t = Utils.get_record_key model (to_lident an) in
      Format.fprintf fmt
        "let[@inline] set_%s (s, key, asset : storage * %a * %s) : storage =@\n  \
         () (*TODO*)@\n"
        an
        pp_btyp t
        an

    | Add an ->
      Format.fprintf fmt
        "let[@inline] add_%s (s, asset : storage * %s) : storage =@\n  \
         () (*TODO*)@\n"
        an an

    | Remove an ->
      let _, t = Utils.get_record_key model (to_lident an) in
      Format.fprintf fmt
        "let[@inline] remove_%s (s, key : storage * %a) : storage =@\n  \
         () (*TODO*)@\n"
        an
        pp_btyp t

    | Clear an ->
      Format.fprintf fmt
        "let[@inline] clear_%s (s : storage) : storage =@\n  \
         () (*TODO*)@\n"
        an

    | Reverse an ->
      Format.fprintf fmt
        "let[@inline] reverse_%s (s : storage) : storage =@\n  \
         () (*TODO*)@\n"
        an

    | UpdateAdd (an, fn) ->
      (* let _, t = Utils.get_record_key model (to_lident an) in *)
      let ft, c = Utils.get_field_container model an fn in
      Format.fprintf fmt
        "let[@inline] add_%s_%s (s, a, b : storage * %s * %s) : storage =@\n  \
         () (*TODO*)@\n"
        an
        fn
        an
        ft

    | UpdateRemove (an, fn) ->
      let ft, c = Utils.get_field_container model an fn in
      let _, t = Utils.get_record_key model (to_lident ft) in
      Format.fprintf fmt
        "let[@inline] remove_%s_%s (s, key : storage * %s * %a) : storage =@\n  \
         () (*TODO*)@\n"
        an
        fn
        an
        pp_btyp t

    | UpdateClear (an, fn) ->
      Format.fprintf fmt
        "let[@inline] clear_%s_%s (s : storage * %s) : storage =@\n  \
         () (*TODO*)@\n"
        an fn an

    | UpdateReverse (an, fn) ->
      Format.fprintf fmt
        "let[@inline] reverse_%s_%s (s : storage * %s) : storage =@\n  \
         () (*TODO*)@\n"
        an fn an


    | ToKeys an ->
      Format.fprintf fmt
        "let[@inline] to_keys_%s (s : storage) : storage =@\n  \
         () (*TODO*)@\n"
        an
  in

  let pp_container_const fmt = function
    | Add t-> Format.fprintf fmt "add\t %a" pp_type t
    | Remove t -> Format.fprintf fmt "remove\t %a" pp_type t
    | Clear t -> Format.fprintf fmt "clear\t %a" pp_type t
    | Reverse t -> Format.fprintf fmt "reverse %a" pp_type t
  in

  let pp_function_const fmt = function
    | Select an ->
      Format.fprintf fmt
        "let[@inline] select_%s (s : storage) : unit =@\n  \
         () (*TODO*)@\n"
        an

    | Sort (an, fn) ->
      Format.fprintf fmt
        "let[@inline] sort_%s_%s (s : storage) : unit =@\n  \
         () (*TODO*)@\n"
        an fn

    | Contains an ->
      let _, t = Utils.get_record_key model (to_lident an) in
      Format.fprintf fmt
        "let[@inline] contains_%s ((s, key) : storage * %a) : bool =@\n  \
         match Map.find key s.%s_assets with@\n  \
         | Some _ -> true@\n  \
         | _ -> false@\n"
        an
        pp_btyp t
        an

    | Nth an ->
      Format.fprintf fmt
        "let[@inline] nth_%s (s : storage) : unit =@\n  \
         () (*TODO*)@\n"
        an

    | Count an ->
      Format.fprintf fmt
        "let[@inline] count_%s (s : storage) : unit =@\n  \
         () (*TODO*)@\n"
        an

    | Sum (an, fn) ->
      Format.fprintf fmt
        "let[@inline] sum_%s_%s (s : storage) : unit =@\n  \
         () (*TODO*)@\n"
        an fn

    | Min (an, fn) ->
      Format.fprintf fmt
        "let[@inline] min_%s_%s (s : storage) : unit =@\n  \
         () (*TODO*)@\n"
        an fn

    | Max (an, fn) ->
      Format.fprintf fmt
        "let[@inline] max_%s_%s (s : storage) : unit =@\n  \
         () (*TODO*)@\n"
        an fn

  in

  let pp_builtin_const fmt = function
    | Min t-> Format.fprintf fmt "min on %a" pp_type t
    | Max t-> Format.fprintf fmt "max on %a" pp_type t
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
    else pp_api_item_node fmt api_item.node
  in

  let pp_api_items fmt l =
    if List.is_empty l
    then pp_nothing fmt
    else
      Format.fprintf fmt "(* API function*)@\n%a@\n"
        (pp_list "@\n" pp_api_item) l
  in

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
  in

  let rec pp_qualid fmt (q : qualid) =
    match q.node with
    | Qdot (q, i) ->
      Format.fprintf fmt "%a.%a"
        pp_qualid q
        pp_id i
    | Qident i -> pp_id fmt i
  in

  let pp_pattern fmt (p : pattern) =
    match p.node with
    | Pconst i -> pp_id fmt i
    | Pwild -> pp_str fmt "_"
  in

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

      | Mexternal (_, _, c, args) ->
        let pp fmt (c, args) =
          Format.fprintf fmt "%a (%a)"
            f c
            (pp_list ", " f) args
        in
        pp fmt (c, args)

      | Mget (c, k) ->
        let pp fmt (c, k) =
          Format.fprintf fmt "get_%a (_s, %a)"
            pp_str c
            f k
        in
        pp fmt (c, k)

      | Mset (c, k, v) ->
        let pp fmt (c, k, v) =
          Format.fprintf fmt "set_%a (_s, %a, %a)"
            pp_str c
            f k
            f v
        in
        pp fmt (c, k, v)

      | Maddasset (an, i, es) ->
        let pp fmt (an, i, es) =
          Format.fprintf fmt "add_%a (_s, %a)"
            pp_str an
            f i
        in
        pp fmt (an, i, es)

      | Maddfield (an, fn, c, i, es) ->
        let pp fmt (an, fn, c, i, es) =
          Format.fprintf fmt "add_%a_%a (_s, %a, %a)"
            pp_str an
            pp_str fn
            f c
            f i
        in
        pp fmt (an, fn, c, i, es)

      | Maddlocal (c, i) ->
        let pp fmt (c, i) =
          Format.fprintf fmt "add (%a, %a)"
            f c
            f i
        in
        pp fmt (c, i)

      | Mremoveasset (an, i) ->
        let pp fmt (an, i) =
          Format.fprintf fmt "remove_%a (_s, %a)"
            pp_str an
            f i
        in
        pp fmt (an, i)

      | Mremovefield (an, fn, c, i) ->
        let pp fmt (an, fn, c, i) =
          Format.fprintf fmt "remove_%a_%a (_s, %a, %a)"
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
          Format.fprintf fmt "clear_%a (_s)"
            pp_str an
        in
        pp fmt (an)

      | Mclearfield (an, fn, i) ->
        let pp fmt (an, fn, i) =
          Format.fprintf fmt "clear_%a_%a (_s, %a)"
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
          Format.fprintf fmt "reverse_%a (_s)"
            pp_str an
        in
        pp fmt (an)

      | Mreversefield (an, fn, i) ->
        let pp fmt (an, fn, i) =
          Format.fprintf fmt "reverse_%a_%a (_s, %a)"
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

      | Mfail (msg) ->
        Format.fprintf fmt "Current.failwith %a"
          f msg

      | Mmathmin (l, r) ->
        Format.fprintf fmt "min (%a, %a)"
          f l
          f r

      | Mmathmax (l, r) ->
        Format.fprintf fmt "max (%a, %a)"
          f l
          f r

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

      | Mnot e ->
        let pp fmt e =
          Format.fprintf fmt "not (%a)"
            f e
        in
        pp fmt e

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
      | Mletin (i, a, t, b) ->
        Format.fprintf fmt "let %a%a = %a in@\n@[<v 2>%a@]"
          pp_id i
          (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t
          f a
          f b
      | Mvarstorevar v -> Format.fprintf fmt "_s.%a" pp_id v
      | Mvarstorecol v -> Format.fprintf fmt "%a_keys" pp_id v
      | Mvarenumval v  -> pp_id fmt v
      | Mvarfield v    -> pp_id fmt v
      | Mvarlocal v    -> pp_id fmt v
      | Mvarthe        -> pp_str fmt "the"
      | Mstate         -> pp_str fmt "state"
      | Mnow           -> pp_str fmt "now"
      | Mtransferred   -> pp_str fmt "Current.amount()"
      | Mcaller        -> pp_str fmt "Current.sender()"
      | Mbalance       -> pp_str fmt "Current.balance()"
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
      | Mduration v -> pp_str fmt v
      | Mdotasset (e, i)
      | Mdotcontract (e, i) ->
        Format.fprintf fmt "%a.%a"
          f e
          pp_id i
      | Mtuple l ->
        Format.fprintf fmt "(%a)"
          (pp_list ", " f) l
      | Mfor (i, c, b) ->
        Format.fprintf fmt "for (%a in %a)@\n (@[<v 2>%a@])@\n"
          pp_id i
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
      | Mbreak -> emit_error UnsupportedBreak
      | Massert x ->
        Format.fprintf fmt "assert %a"
          f x
      | Mreturn x ->
        Format.fprintf fmt "return %a"
          f x
      | Mtokeys (an, x) ->
        Format.fprintf fmt "%s.to_keys (%a)"
          an
          f x
      | Mforall _                        -> emit_error (UnsupportedTerm ("forall"))
      | Mexists _                        -> emit_error (UnsupportedTerm ("exists"))
      | Msetbefore _                     -> emit_error (UnsupportedTerm ("setbefore"))
      | Msetunmoved _                    -> emit_error (UnsupportedTerm ("setunmoved"))
      | Msetadded _                      -> emit_error (UnsupportedTerm ("setadded"))
      | Msetremoved _                    -> emit_error (UnsupportedTerm ("setremoved"))
      | Msetiterated _                   -> emit_error (UnsupportedTerm ("setiterated"))
      | Msettoiterate _                  -> emit_error (UnsupportedTerm ("settoiterate"))
      | MsecMayBePerformedOnlyByRole _   -> emit_error (UnsupportedTerm ("secMayBePerformedOnlyByRole"))
      | MsecMayBePerformedOnlyByAction _ -> emit_error (UnsupportedTerm ("secMayBePerformedOnlyByAction"))
      | MsecMayBePerformedByRole _       -> emit_error (UnsupportedTerm ("secMayBePerformedByRole"))
      | MsecMayBePerformedByAction _     -> emit_error (UnsupportedTerm ("secMayBePerformedByAction"))
      | MsecTransferredBy _              -> emit_error (UnsupportedTerm ("secTransferredBy"))
      | MsecTransferredTo _              -> emit_error (UnsupportedTerm ("secTransferredTo"))
      | Manyaction                       -> emit_error (UnsupportedTerm ("anyaction"))
    in
    f fmt mt
  in

  let pp_enum_item fmt (enum_item : enum_item) =
    Format.fprintf fmt "| %a"
      pp_id enum_item.name
  in

  let pp_enum fmt (enum : enum) =
    Format.fprintf fmt "type %a =@\n[<v 2>  %a@]@\n"
      pp_id enum.name
      (pp_list "@\n" pp_enum_item) enum.values
  in

  let pp_record_item fmt (item : record_item) =
    Format.fprintf fmt "%a : %a;"
      pp_id item.name
      pp_type item.type_
      (* (pp_option (fun fmt -> Format.fprintf fmt " := %a" pp_mterm)) item.default *)
  in

  let pp_record fmt (record : record) =
    Format.fprintf fmt "type %a = {@\n@[<v 2>  %a@]@\n}@\n"
      pp_id record.name
      (pp_list "@\n" pp_record_item) record.values
  in

  let pp_decl fmt = function
    | Denum e -> pp_enum fmt e
    | Drecord r -> pp_record fmt r
    | _ -> ()
  in

  let pp_storage_item fmt (si : storage_item) =
    match si with
    | { asset = Some an; _} ->
      let _, t = Utils.get_record_key model an in
      Format.fprintf fmt "%s_keys: %a list;@\n%s_assets: (%a, %s) map;"
        (unloc an)
        pp_btyp t
        (unloc an)
        pp_btyp t
        (unloc an)

    | _ ->
      Format.fprintf fmt "%a : %a;"
        pp_id si.name
        pp_type si.typ
  in

  let pp_storage fmt (s : storage) =
    Format.fprintf fmt "type storage = {@\n@[<v 2>  %a@]@\n}@\n"
      (pp_list "@\n" pp_storage_item) s
  in

  let pp_init_function fmt (s : storage) =
    let pp_storage_item fmt (si : storage_item) =
      match si with
      | { asset = Some an; _} ->
        let _, t = Utils.get_record_key model an in
        Format.fprintf fmt "%s_keys = [];@\n%s_assets = (Map : (%a, %s) map);"
          (unloc an)
          (unloc an)
          pp_btyp t
          (unloc an)

      | _ ->
        Format.fprintf fmt "%a = %a;"
          pp_id si.name
          pp_mterm si.default
    in

    Format.fprintf fmt "let initialize _ = {@\n@[<v 2>  %a@]@\n}@\n"
      (pp_list "@\n" pp_storage_item) s
  in

  let pp_args fmt args =
    match args with
    | [] -> Format.fprintf fmt "()"
    | [(id, t, _)] ->
      Format.fprintf fmt "(%a : %a)"
        pp_id id
        pp_type t
    | _ ->
      Format.fprintf fmt "(%a : %a)"
        (pp_list ", " (fun fmt (id, _, _) -> pp_id fmt id)) args
        (pp_list " * " (fun fmt (_ , t, _) -> pp_type fmt t)) args

  in

  let pp_function fmt f =
    let k, fs, ret, extra_arg = match f.node with
      | Entry f -> "let%entry", f, Some (Ttuple [Tcontainer (Toperation, Collection); Tstorage]), " (_s : storage)"
      | Function (f, a) -> "let", f, Some a, ""
    in
    Format.fprintf fmt "%a %a %a%s%a =@\n@[<v 2>  %a@]@\n"
      pp_str k
      pp_id fs.name
      pp_args fs.args
      extra_arg
      (pp_option (fun fmt -> Format.fprintf fmt " : %a" pp_type)) ret
      pp_mterm fs.body
  in
  Format.fprintf fmt "%a@\n\
                      @\n%a\
                      @\n%a\
                      @\n%a\
                      @\n%a\
                      @\n%a\
                      @."
    pp_model_name ()
    (pp_list "@\n" pp_decl) model.decls
    pp_storage model.storage
    pp_init_function model.storage
    pp_api_items model.api_items
    (pp_list "@\n" pp_function) model.functions

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
