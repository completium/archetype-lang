open Location
open Tools
open Model
open Printer_tools

exception Anomaly of string

type error_desc =
  | UnsupportedDeclVar
  | UnsupportedTerm of string
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

let const_storage = "_s"
let const_state = "state"
let const_operations = "ops_"

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
  let remove_shallow (model : model) : model =
    let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
      match mt.node with
      | Mshallow (_, x)
      | Munshallow (_, x) -> aux ctx x
      | _ -> map_mterm (aux ctx) mt
    in
    map_mterm_model aux model
  in
  let model = remove_shallow model in
  let pp_model_name (fmt : Format.formatter) _ =
    Format.fprintf fmt "(* contract: %a *)"
      pp_id model.name
  in

  let pp_prelude fmt _ =
    Format.fprintf fmt
      "open SCaml@\n"
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
    | Brole       -> Format.fprintf fmt "address"
    | Bcurrency   -> Format.fprintf fmt "tz"
    | Bkey        -> Format.fprintf fmt "key"
    | Bbytes      -> Format.fprintf fmt "bytes"
    | Bnat        -> Format.fprintf fmt "nat"
  in

  let pp_container fmt = function
    | Collection
    | Partition
    | View  -> Format.fprintf fmt "list"
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
    | Tmap (k, v) ->
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
  in

  let show_zero = function
    | _ -> "(Int 0)"
  in

  let pp_api_asset fmt = function
    | Get an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let get_%s (s, key : storage * %a) : %s =@\n  \
         match Map.get key s.%s_assets with@\n  \
         | Some v -> v@\n  \
         | _ -> failwith \"not_found\"@\n"
        an pp_btyp t an an

    | Set an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let set_%s (s, key, asset : storage * %a * %s) : storage =@\n  \
         { s with@\n    \
         %s_assets = Map.update key (Some asset) s.%s_assets; }@\n"
        an pp_btyp t an
        an an

    | Add an ->
      let k, _t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let add_%s (s, asset : storage * %s) : storage =@\n  \
         let key = asset.%s in@\n  \
         if Map.mem key s.%s_assets then failwith \"key already exists\";@\n  \
         { s with@\n    \
         %s_assets = Map.update key (Some asset) s.%s_assets; }@\n"
        an an
        k
        an
        an an

    | Remove an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let remove_%s (s, key : storage * %a) : storage =@\n  \
         { s with@\n    \
         %s_assets = Map.update key None s.%s_assets; }@\n"
        an pp_btyp t
        an an

    | Clear _ -> Format.fprintf fmt "// TODO api storage: clear"

    | UpdateAdd (an, fn) ->
      let k, _t = Utils.get_asset_key model an in
      let ft, c = Utils.get_field_container model an fn in
      let kk, _ = Utils.get_asset_key model ft in
      Format.fprintf fmt
        "let add_%s_%s (s, a, b : storage * %s * %s) : storage =@\n  \
         let asset = { a with %s = (b.%a)::(a.%s); } in@\n  \
         %a\
         %a\
         { s with %s_assets = Map.update a.%a (Some asset) s.%s_assets }@\n"
        an fn an ft
        fn pp_str kk fn
        (pp_do_if (match c with | Partition -> true | _ -> false) (fun fmt -> Format.fprintf fmt "let s = add_%s(s, b) in@\n  ")) ft
        (pp_do_if (match c with | Collection -> true | _ -> false)
             (fun fmt _ -> Format.fprintf fmt "if not (Map.mem b.%s s.%s_assets) then failwith \"key of b does not exist\";@\n  " kk ft)) ()
        an pp_str k an

    | UpdateRemove (an, fn) ->
      let k, _t = Utils.get_asset_key model an in
      let ft, c = Utils.get_field_container model an fn in
      let _kk, tt = Utils.get_asset_key model ft in
      Format.fprintf fmt
        "let remove_%s_%s (s, a, key : storage * %s * %a) : storage =@\n  \
         let asset = { a with %s = List.rev (List.fold_left (fun accu k -> if k = key then accu else k::accu) [] a.%s) } in@\n  \
         %a
         { s with %s_assets = Map.update a.%a (Some asset) s.%s_assets }@\n"
        an fn an pp_btyp tt
        fn fn
        (pp_do_if (match c with | Partition -> true | _ -> false) (fun fmt -> Format.fprintf fmt "let s = remove_%s(s, key) in@\n")) ft
        an pp_str k an

    | UpdateClear _ -> Format.fprintf fmt "// TODO api storage: UpdateClear"

    | ToKeys an ->
      Format.fprintf fmt
        "let to_keys_%s (s : storage) : storage =@\n  \
         s (*TODO*)@\n"
        an

    | ColToKeys an ->
      let _k, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let col_to_keys_%s (s : storage) : %a list =@\n  \
         let res = Map.fold (fun k v accu -> k::accu) s.%s_assets [] in@\n  \
         List.rev res@\n"
        an pp_btyp t
        an

    | Select (an, _, _) ->
      let k, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let select_%s (s, l, p : storage * %a list * (%s -> bool)) : %a list =@\n  \
         List.fold_left (fun accu x ->@\n      \
         let a = get_%s (s, x) in@\n      \
         if p a@\n      \
         then a.%s::accu@\n      \
         else accu@\n    \
         ) [] l@\n"
        an pp_btyp t an pp_btyp t
        an
        k

    | Sort (an, _l) ->
      Format.fprintf fmt
        "let sort_%s (s : storage) : unit =@\n  \
         () (*TODO*)@\n"
        an

    | Contains an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let contains_%s (l, key : %a list * %a) : bool =@\n  \
         List.fold_left (fun accu x ->@\n      \
         accu || x = key@\n    \
         ) false l@\n"
        an
        pp_btyp t
        pp_btyp t

    | Nth an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let[@inline] nth_%s (s, l, idx : storage * %a list * int) : %s =@\n  \
         match l with@\n  \
         | [] -> failwith \"empty list\"@\n  \
         | _ ->@\n  \
         begin@\n  \
         let cpt = idx in@\n  \
         let _, res =@\n  \
         List.fold (fun (x, accu) ->@\n  \
         let cpt, res = accu in@\n  \
         if cpt = 0@\n  \
         then (cpt - 1, Some x)@\n  \
         else (cpt - 1, res)@\n  \
         ) l (cpt, None) in@\n  \
         match res with@\n  \
         | None -> failwith \"index out of bounds\"@\n  \
         | Some k -> get_%s (s, k)@\n  \
         end@\n"
        an pp_btyp t an
        an

    | Count an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let count_%s (l : %a list) : int =@\n  \
         List.length l@\n"
        an
        pp_btyp t

    | Sum (an, t, _p) -> (* TODO *)
      let _, tk = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let sum_%s (s, l : storage * %a list) : %a =@\n  \
         List.fold_left (fun accu k ->@\n      \
         let x =@\n        \
         match Map.get k s.%s_assets with@\n        \
         | Some v -> v@\n        \
         | _ -> failwith \"not_found\"@\n      \
         in@\n      \
         accu + x@\n    \
         ) %s l@\n"
        an pp_btyp tk pp_type t
        an
        (show_zero t)

    | Min (an, fn) ->
      let _, tk = Utils.get_asset_key model an in
      let _, t, _ = Utils.get_asset_field model (an, fn) in
      Format.fprintf fmt
        "let min_%s_%s (s, l : storage * %a list) : %a =@\n  \
         match l with@\n  \
         | [] -> failwith \"empty list\"@\n  \
         | e::t ->@\n    \
         let x = @\n      \
         match Map.get e s.%s_assets with@\n      \
         | Some v -> v@\n      \
         | _ -> failwith \"not_found\" @\n    \
         in@\n    \
         let init = x.%s in@\n    \
         List.fold_right (fun accu k ->@\n        \
         let x = @\n          \
         match Map.get k s.%s_assets with@\n          \
         | Some v -> v@\n          \
         | _ -> failwith \"not_found\" @\n        \
         in@\n        \
         if accu > x.%s@\n        \
         then x.%s@\n        \
         else accu@\n      \
         ) init t@\n"
        an fn pp_btyp tk pp_type t
        an
        fn
        an
        fn
        fn

    | Max (an, fn) ->
      let _, tk = Utils.get_asset_key model an in
      let _, t, _ = Utils.get_asset_field model (an, fn) in
      Format.fprintf fmt
        "let max_%s_%s (s, l : storage * %a list) : %a =@\n  \
         match l with@\n  \
         | [] -> failwith \"empty list\"@\n  \
         | e::t ->@\n    \
         let x = @\n      \
         match Map.get e s.%s_assets with@\n      \
         | Some v -> v@\n      \
         | _ -> failwith \"not_found\" @\n    \
         in@\n    \
         let init = x.%s in@\n    \
         List.fold_right (fun accu k ->@\n        \
         let x = @\n          \
         match Map.get k s.%s_assets with@\n          \
         | Some v -> v@\n          \
         | _ -> failwith \"not_found\" @\n        \
         in@\n        \
         if accu < x.%s@\n        \
         then x.%s@\n        \
         else accu@\n      \
         ) init t@\n"
        an fn pp_btyp tk pp_type t
        an
        fn
        an
        fn
        fn

    | Shallow _ -> ()
    | Unshallow _ -> ()
    | Listtocoll _ -> ()

    | Head an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let head_%s (l : %a list) : %a list =@\n  \
         List.fold (fun (_, accu) ->@\n    \
         accu@\n  \
         ) l []@\n"
        an
        pp_btyp t
        pp_btyp t

    | Tail an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let tail_%s (l : %a list)  : %a list =@\n  \
         List.fold (fun (_, accu) ->@\n    \
         accu@\n  \
         ) l []@\n"
        an
        pp_btyp t
        pp_btyp t

  in

  let pp_api_list fmt = function
    | Lprepend t  -> Format.fprintf fmt "list_prepend\t %a" pp_type t
    | Lcontains t -> Format.fprintf fmt "list_contains\t %a" pp_type t
    | Lcount t    -> Format.fprintf fmt "list_count\t %a" pp_type t
    | Lnth t      -> Format.fprintf fmt "list_nth\t %a" pp_type t
  in

  let pp_api_builtin fmt = function
    | Bmin    t -> Format.fprintf fmt "min on %a" pp_type t
    | Bmax    t -> Format.fprintf fmt "max on %a" pp_type t
    | Babs    t -> Format.fprintf fmt "abs on %a" pp_type t
    | Bconcat t -> Format.fprintf fmt "concat on %a" pp_type t
    | Bslice  t -> Format.fprintf fmt "slice on %a"  pp_type t
    | Blength t -> Format.fprintf fmt "length on %a" pp_type t
    | Bisnone t -> Format.fprintf fmt "isnone on %a" pp_type t
    | Bissome t -> Format.fprintf fmt "issome on %a" pp_type t
    | Bgetopt t -> Format.fprintf fmt "getopt on %a" pp_type t
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

  let pp_api_items fmt l =
    let filter_api_items l : api_storage list =
      let contains_select_asset_name a_name l : bool =
        List.fold_left (fun accu x ->
            match x.node_item with
            | APIAsset  (Select (an, _, _)) -> accu || String.equal an a_name
            | _ -> accu
          ) false l
      in
      List.fold_right (fun (x : api_storage) accu ->
          if (match x.api_loc with | OnlyExec | ExecFormula -> true | OnlyFormula -> false)
          then accu
          else
            match x.node_item with
            | APIAsset  (Select (an, _p, _)) when contains_select_asset_name an accu -> accu
            | _ -> x::accu
        ) l []
    in
    let l : api_storage list = filter_api_items l in
    if List.is_empty l
    then pp_nothing fmt
    else
      Format.fprintf fmt "(* API function *)@\n%a@\n"
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
        Format.fprintf fmt "let %a%a =@\n  @[%a@]in@\n@[%a@]"
          (pp_if (List.length ids > 1) (pp_paren (pp_list ", " pp_id)) (pp_list ", " pp_id)) ids
          (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t
          f a
          f b

      | Mletin (ids, a, t, b, _) ->
        Format.fprintf fmt "let %a%a = %a in@\n@[%a@]"
          (pp_if (List.length ids > 1) (pp_paren (pp_list ", " pp_id)) (pp_list ", " pp_id)) ids
          (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t
          f a
          f b

      | Mdeclvar (_ids, _t, _v) ->
        emit_error UnsupportedDeclVar

      | Mapp (e, args) ->
        let pp fmt (e, args) =
          Format.fprintf fmt "%a (%a)"
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
        Format.fprintf fmt "s.%a %a %a"
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
        Format.fprintf fmt "state_ = %a"
          f x

      | Massignassetstate (an, k, v) ->
        Format.fprintf fmt "state_%a(%a) = %a"
          pp_ident an
          f k
          f v


      (* control *)

      | Mif (c, t, None) ->
        Format.fprintf fmt "@[if %a@ then %a@]"
          f c
          f t

      | Mif (c, t, Some e) ->
        Format.fprintf fmt "@[if %a then @\n  @[%a @]@\nelse @\n  @[%a @]@]"
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
        Format.fprintf fmt "for (%a in %a) (@\n  @[%a@])@\n"
          pp_id i
          f c
          f b

      | Miter (_i, _a, _b, _c, _) -> Format.fprintf fmt "TODO: iter@\n"

      | Mseq is ->
        Format.fprintf fmt "(@[%a@])"
          (pp_list ";@\n" f) is

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
        Format.fprintf fmt "failwith \"%a\""
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

      | Mint v -> Format.fprintf fmt "(Int %a)" pp_big_int v
      | Muint v -> pp_big_int fmt v
      | Mbool b -> pp_str fmt (if b then "true" else "false")
      | Menum v -> pp_str fmt v
      | Mrational (n, d) ->
        Format.fprintf fmt "(%a. /. %a.)"
          pp_big_int n
          pp_big_int d
      | Mstring v ->
        Format.fprintf fmt "\"%a\""
          pp_str v
      | Mcurrency (v, c) ->
        let b : Big_int.big_int =
          begin
            match c with
            | Tz   -> Big_int.mult_int_big_int 1000000 v
            | Mtz  -> Big_int.mult_int_big_int 1000 v
            | Utz  -> v
          end
        in
        Format.fprintf fmt "%a"
          pp_big_int b
      | Maddress v ->
        Format.fprintf fmt "\"%a\""
          pp_str v
      | Mdate v -> Core.pp_date fmt v
      | Mduration v -> Core.pp_duration_in_seconds fmt v
      | Mtimestamp v -> pp_big_int fmt v
      | Mbytes v -> Format.fprintf fmt "0x%s" v


      (* control expression *)

      | Mexprif (c, t, e) ->
        Format.fprintf fmt "@[if %a then @\n  @[%a @]@\nelse @\n  @[%a @]@]"
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

        Format.fprintf fmt "{ %a }"
          (pp_list "; " (fun fmt (a, b)->
               Format.fprintf fmt "%a = %a"
                 pp_id a
                 f b)) lll

      | Massets l ->
        begin
          match mtt.type_ with
          | Tmap (_k , _v) ->
            begin
              match l with
              | [] -> Format.fprintf fmt "[]"
              | _ ->
                Format.fprintf fmt "[%a]"
                  (pp_list "; " f) l
            end
          | _ ->
            Format.fprintf fmt "[%a]"
              (pp_list "; " f) l
        end

      | Mlitset l ->
        Format.fprintf fmt "[%a]"
          (pp_list "; " f) l

      | Mlitlist l ->
        Format.fprintf fmt "[%a]"
          (pp_list "; " f) l

      | Mlitmap l ->
        Format.fprintf fmt "[%a]"
          (pp_list "; " (fun fmt (k, v) -> Format.fprintf fmt "%a : %a"
                            f k
                            f v)) l

      (* access *)

      | Mdotasset (e, i)
      | Mdotcontract (e, i) ->
        Format.fprintf fmt "(%a).%a"
          f e
          pp_id i

      | Maccestuple (e, i) ->
        Format.fprintf fmt "%a[%a]"
          f e
          pp_big_int i

      (* comparison operators *)

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

      | Mmulticomp (_e, _l) ->
        assert false


      (* arithmetic operators *)

      | Mand (l, r) ->
        let pp fmt (l, r) =
          Format.fprintf fmt "(%a) && (%a)"
            f l
            f r
        in
        pp fmt (l, r)

      | Mor (l, r) ->
        let pp fmt (l, r) =
          Format.fprintf fmt "(%a) || (%a)"
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
          Format.fprintf fmt "add_%a (%s, %a)"
            pp_str an
            const_storage
            f i
        in
        pp fmt (an, i)

      | Maddfield (an, fn, c, i) ->
        let pp fmt (an, fn, c, i) =
          Format.fprintf fmt "add_%a_%a (%s, %a, %a)"
            pp_str an
            pp_str fn
            const_storage
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
          Format.fprintf fmt "remove_%a (%s, %a%a)"
            pp_str an
            const_storage
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
          Format.fprintf fmt "remove_%a_%a (%s, %a, %a%a)"
            pp_str an
            pp_str fn
            const_storage
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
          Format.fprintf fmt "clear_%a_%a (%a)"
            pp_str an
            pp_str fn
            f a
        in
        pp fmt (an, fn, a)

      | Mset (c, _l, k, v) ->
        let pp fmt (c, k, v) =
          Format.fprintf fmt "set_%a (%s, %a, %a)"
            pp_str c
            const_storage
            f k
            f v
        in
        pp fmt (c, k, v)

      | Mupdate _    -> emit_error (UnsupportedTerm ("update"))
      | Mremoveif _  -> emit_error (UnsupportedTerm ("removeif"))
      | Maddupdate _ -> emit_error (UnsupportedTerm ("add_update"))


      (* asset api expression *)

      | Mget (an, c, k) ->
        let pp fmt (an, _c, k) =
          Format.fprintf fmt "get_%a (%s, %a)"
            pp_str an
            const_storage
            f k
        in
        pp fmt (an, c, k)

      | Mselect (an, c, la, lb, a) ->
        let pp fmt (an, c, _la, lb, _a) =
          Format.fprintf fmt "select_%a (%s, %a, fun the -> %a)"
            pp_str an
            const_storage
            f c
            f lb
        in
        pp fmt (an, c, la, lb, a)

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

      | Msum (an, _, c) -> (* TODO *)
        let pp fmt (an, c) =
          Format.fprintf fmt "sum_%a (%s, %a)"
            pp_str an
            const_storage
            f c
        in
        pp fmt (an, c)

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
          Format.fprintf fmt "(match Map.get (%a) (%a) with | Some x -> x | None -> failwith \"%s not_found\")"
            f k
            f c
            an
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


      (* constants *)

      | Mvarstate      -> Format.fprintf fmt "%s.%s" const_storage const_state
      | Mnow           -> pp_str fmt "Global.get_now ()"
      | Mtransferred   -> pp_str fmt "Global.get_amount ()"
      | Mcaller        -> pp_str fmt "Global.get_sender ()"
      | Mbalance       -> pp_str fmt "Global.get_balance ()"
      | Msource        -> pp_str fmt "Global.get_source ()"


      (* variables *)

      | Mvarassetstate (an, k) -> Format.fprintf fmt "state_%a(%a)" pp_str an f k
      | Mvarstorevar v -> Format.fprintf fmt "%s.%a" const_storage pp_id v
      | Mvarstorecol v -> Format.fprintf fmt "%s.%a" const_storage pp_id v
      | Mvarenumval v  -> pp_id fmt v
      | Mvarlocal v    -> pp_id fmt v
      | Mvarparam v    -> pp_id fmt v
      | Mvarfield v    -> pp_id fmt v
      | Mvarthe        -> pp_str fmt "the"


      (* rational *)

      | Mdivrat _ -> emit_error (UnsupportedTerm ("div"))
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
        Format.fprintf fmt
          "List.fold_left (fun (%a) %a ->@\n    @[%a@]) (%a) (%a)@\n"
          (pp_list ", " pp_id) is pp_id i
          f b
          (pp_list ", " pp_id) is
          f c

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
          Format.fprintf fmt "add_shallow_%a (%a)"
            pp_str e
            (pp_list ", " f) args
        in
        pp fmt (e, args)


      (* collection keys *)

      | Mtokeys (an, x) ->
        Format.fprintf fmt "%s.to_keys (%a)"
          an
          f x

      | Mcoltokeys (an) ->
        Format.fprintf fmt "col_to_keys_%s (%s)"
          an
          const_storage


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

  let pp_enum_item fmt (enum_item : enum_item) =
    Format.fprintf fmt "| %a"
      pp_id enum_item.name
  in

  let pp_enum fmt (enum : enum) =
    Format.fprintf fmt "type %a =@\n  @[%a@]@\n"
      pp_id enum.name
      (pp_list "@\n" pp_enum_item) enum.values
  in

  let pp_asset_item fmt (item : asset_item) =
    let pp_typ fmt t =
      match t with
      | Tcontainer (Tasset an, _) ->
        let _, t = Utils.get_asset_key model (unloc an) in
        Format.fprintf fmt "%a list"
          pp_btyp t
      | _ -> pp_type fmt t
    in
    Format.fprintf fmt "%a : %a;"
      pp_id item.name
      pp_typ item.type_
      (* (pp_option (fun fmt -> Format.fprintf fmt " := %a" pp_mterm)) item.default *)
  in

  let pp_asset fmt (asset : asset) =
    Format.fprintf fmt "type %a = {@\n  @[%a@]@\n}@\n"
      pp_id asset.name
      (pp_list "@\n" pp_asset_item) asset.values
  in

  let pp_decl fmt = function
    | Denum e -> pp_enum fmt e
    | Dasset r -> pp_asset fmt r
    | _ -> ()
  in

  let pp_storage_item fmt (si : storage_item) =
    Format.fprintf fmt "%a : %a;"
      pp_id si.id
      pp_type si.typ
  in

  let pp_storage fmt (s : storage) =
    match s with
    | [] -> Format.fprintf fmt "type storage = unit@\n"
    | [i] ->
      Format.fprintf fmt "type storage = %a@\n"
        pp_type i.typ
    | _ ->
      Format.fprintf fmt "type storage = {@\n  @[%a@]@\n}@\n"
        (pp_list "@\n" pp_storage_item) s
  in

  let pp_args fmt args =
    match args with
    | [] -> Format.fprintf fmt "()"
    | [(id, t, _)] ->
      Format.fprintf fmt "(%a : %a)"
        pp_id id
        pp_type t
    | _   ->
      Format.fprintf fmt "(%a : %a)"
        (pp_list ", " (fun fmt (id, _, _) -> pp_id fmt id)) args
        (pp_list " * " (fun fmt (_ , t, _) -> pp_type fmt t)) args

  in

  let pp_function fmt f =
    let k, fs, ret, extra_arg = match f.node with
      | Entry f ->
        let str : string = Format.asprintf "let [@entry name=\"%a\"]" pp_id f.name in
        str, f, Some (Ttuple [Tcontainer (Toperation, Collection); Tstorage]), " (_s : storage)"
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
  Format.fprintf fmt "(* Scaml output generated by %a *)@\n\
                      @\n%a@\n\
                      @\n%a\
                      @\n%a\
                      @\n%a\
                      @\n%a\
                      @\n%a\
                      @."
    pp_bin ()
    pp_model_name ()
    pp_prelude ()
    (pp_list "@\n" pp_decl) model.decls
    pp_storage model.storage
    pp_api_items model.api_items
    (pp_list "@\n" pp_function) model.functions

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
