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
  let pp_model_name (fmt : Format.formatter) _ =
    Format.fprintf fmt "(* contract: %a *)"
      pp_id model.name
  in

  let pp_prelude fmt _ =
    Format.fprintf fmt
      "open SCaml@\n"
  in

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
    | Brole       -> Format.fprintf fmt "address"
    | Bcurrency   -> Format.fprintf fmt "tz"
    | Bsignature  -> Format.fprintf fmt "signature"
    | Bkey        -> Format.fprintf fmt "key"
    | Bkeyhash    -> Format.fprintf fmt "key_hash"
    | Bbytes      -> Format.fprintf fmt "bytes"
    | Bnat        -> Format.fprintf fmt "nat"
    | Bchainid    -> Format.fprintf fmt "chain_id"
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
    | Tset k ->
      Format.fprintf fmt "%a set"
        pp_type k
    | Tlist t ->
      Format.fprintf fmt "%a list"
        pp_type t
    | Toption t ->
      Format.fprintf fmt "%a option"
        pp_type t
    | Ttuple ts ->
      Format.fprintf fmt "%a"
        (pp_list " * " pp_type) ts
    | Tmap (true, k, v) ->
      Format.fprintf fmt "(%a, %a) bigmap"
        pp_type k
        pp_type v
    | Tmap (false, k, v) ->
      Format.fprintf fmt "(%a, %a) map"
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
        an pp_type t an an

    | Set an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let set_%s (s, key, asset : storage * %a * %s) : storage =@\n  \
         { s with@\n    \
         %s_assets = Map.update key (Some asset) s.%s_assets; }@\n"
        an pp_type t an
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
        an pp_type t
        an an

    | Clear _ -> Format.fprintf fmt "// TODO api storage: clear"

    | Update _ -> Format.fprintf fmt "// TODO api storage: update"

    | FieldAdd (an, fn) ->
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

    | FieldRemove (an, fn) ->
      let k, _t = Utils.get_asset_key model an in
      let ft, c = Utils.get_field_container model an fn in
      let _kk, tt = Utils.get_asset_key model ft in
      Format.fprintf fmt
        "let remove_%s_%s (s, a, key : storage * %s * %a) : storage =@\n  \
         let asset = { a with %s = List.rev (List.fold_left (fun accu k -> if k = key then accu else k::accu) [] a.%s) } in@\n  \
         %a
         { s with %s_assets = Map.update a.%a (Some asset) s.%s_assets }@\n"
        an fn an pp_type tt
        fn fn
        (pp_do_if (match c with | Partition -> true | _ -> false) (fun fmt -> Format.fprintf fmt "let s = remove_%s(s, key) in@\n")) ft
        an pp_str k an

    | RemoveAll (an, fn) ->
      Format.fprintf fmt
        "let to_keys_%s_%s (s : storage) : storage =@\n  \
         s (*TODO*)@\n"
        an fn

    | RemoveIf (an, _, _, _) ->
      Format.fprintf fmt
        "let removeif_%s (s : storage) : storage =@\n  \
         s (*TODO*)@\n"
        an

    | Contains (an, _) ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let contains_%s (l, key : %a list * %a) : bool =@\n  \
         List.fold_left (fun accu x ->@\n      \
         accu || x = key@\n    \
         ) false l@\n"
        an
        pp_type t
        pp_type t

    | Nth (an, _) ->
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
        an pp_type t an
        an

    | Select (an, _, _, _) ->
      let k, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let select_%s (s, l, p : storage * %a list * (%s -> bool)) : %a list =@\n  \
         List.fold_left (fun accu x ->@\n      \
         let a = get_%s (s, x) in@\n      \
         if p a@\n      \
         then a.%s::accu@\n      \
         else accu@\n    \
         ) [] l@\n"
        an pp_type t an pp_type t
        an
        k

    | Sort (an, _, _l) ->
      Format.fprintf fmt
        "let sort_%s (s : storage) : unit =@\n  \
         () (*TODO*)@\n"
        an

    | Count (an, _) ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let count_%s (l : %a list) : int =@\n  \
         List.length l@\n"
        an
        pp_type t

    | Sum (an, _, t, _p) -> (* TODO *)
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
        an pp_type tk pp_type t
        an
        (show_zero t)

    | Head (an, _) ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let head_%s (l : %a list) : %a list =@\n  \
         List.fold (fun (_, accu) ->@\n    \
         accu@\n  \
         ) l []@\n"
        an
        pp_type t
        pp_type t

    | Tail (an, _) ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "let tail_%s (l : %a list)  : %a list =@\n  \
         List.fold (fun (_, accu) ->@\n    \
         accu@\n  \
         ) l []@\n"
        an
        pp_type t
        pp_type t
  in

  let pp_api_list fmt = function
    | Lprepend t  -> Format.fprintf fmt "list_prepend\t %a" pp_type t
    | Lcontains t -> Format.fprintf fmt "list_contains\t %a" pp_type t
    | Llength t   -> Format.fprintf fmt "list_length\t %a" pp_type t
    | Lnth t      -> Format.fprintf fmt "list_nth\t %a" pp_type t
    | Lreverse t  -> Format.fprintf fmt "list_reverse\t %a" pp_type t
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

  let pp_api_items fmt l =
    let filter_api_items l : api_storage list =
      let contains_select_asset_name a_name l : bool =
        List.fold_left (fun accu x ->
            match x.node_item with
            | APIAsset  (Select (an, _, _, _)) -> accu || String.equal an a_name
            | _ -> accu
          ) false l
      in
      List.fold_right (fun (x : api_storage) accu ->
          if (match x.api_loc with | OnlyExec | ExecFormula -> true | OnlyFormula -> false)
          then accu
          else
            match x.node_item with
            | APIAsset  (Select (an, _, _p, _)) when contains_select_asset_name an accu -> accu
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

  let pp_temp fmt = function
    | Tbefore -> Format.fprintf fmt "before."
    | Tat i   -> Format.fprintf fmt "at(%s)." i
    | Tnone   -> ()
  in

  let pp_delta fmt = function
    | Dadded   -> Format.fprintf fmt "added."
    | Dremoved -> Format.fprintf fmt "removed."
    | Dunmoved -> Format.fprintf fmt "unmoved."
    | Dnone    -> ()
  in

  let pp_container_kind f fmt = function
    | CKcoll (t, d)              -> Format.fprintf fmt "_%a%aColl_" pp_temp t pp_delta d
    | CKview mt                  -> f fmt mt
    | CKfield (an, fn, mt, t, d) -> Format.fprintf fmt "%a%aCKfield (%s, %s, %a)" pp_temp t pp_delta d an fn f mt
    | CKdef v                    -> Format.fprintf fmt "_Def(%s)_" v
  in

  let pp_iter_container_kind f fmt = function
    | ICKcoll an  -> Format.fprintf fmt "%a" pp_str an
    | ICKview mt  -> Format.fprintf fmt "%a" f mt
    | ICKfield (_, _, mt) -> Format.fprintf fmt "%a" f mt
    | ICKset mt  -> Format.fprintf fmt "%a" f mt
    | ICKlist mt  -> Format.fprintf fmt "%a" f mt
    | ICKmap mt  -> Format.fprintf fmt "%a" f mt
  in

  let pp_transfer_kind f fmt = function
    | TKsimple d        -> Format.fprintf fmt "to %a" f d
    | TKcall (id, _, d, a) -> Format.fprintf fmt "to %a call %s(%a)" f d id f a
    | TKentry (e, a)    -> Format.fprintf fmt "to entry %a(%a)" f e f a
    | TKself (id, args) -> Format.fprintf fmt "to entry self.%a(%a)" pp_str id (pp_list ", " (fun fmt (id, x) -> Format.fprintf fmt "%s = %a" id f x)) args
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

      | Massign (op, _, Avar l, r) ->
        Format.fprintf fmt "%a %a %a"
          pp_id l
          pp_operator op
          f r

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
        Format.fprintf fmt "state_ = %a"
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
          (fun fmt i -> match i with FIsimple x -> pp_id fmt x | FIdouble (x, y) -> Format.fprintf fmt "(%a, %a)" pp_id x pp_id y) i
          (pp_iter_container_kind f) c
          f b

      | Miter (_i, _a, _b, _c, _) -> Format.fprintf fmt "TODO: iter@\n"

      | Mwhile (c, b, l) ->
        Format.fprintf fmt "while %a%a do@\n  @[%a@]@\ndone"
          (pp_option (fun fmt -> Format.fprintf fmt ": %a " pp_str)) l
          f c
          f b

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
        Format.fprintf fmt "failwith \"%a\""
          (pp_fail_type f) ft

      | Mtransfer (v, k) ->
        Format.fprintf fmt "transfer %a %a"
          f v
          (pp_transfer_kind f) k


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

      | Mint v -> Format.fprintf fmt "(Int %a)" pp_big_int v
      | Mnat v -> pp_big_int fmt v
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
      | Munit -> Format.fprintf fmt "Unit"


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

      | Mmatchsome _ -> emit_error (UnsupportedTerm ("Mmatchsome"))


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
          | Tmap (_, _k , _v) ->
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
          Format.fprintf fmt "%a = %a"
            (pp_cast Lhs l.type_ r.type_ f) l
            (pp_cast Rhs l.type_ r.type_ f) r
        in
        pp fmt (l, r)

      | Mnequal (_, l, r) ->
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

      | Mxor (l, r) ->
        let pp fmt (l, r) =
          Format.fprintf fmt "(%a) ^ (%a)"
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

      | Mdivrat _ -> emit_error (UnsupportedTerm ("div"))

      | Mdiveuc (l, r) ->
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

      | Mremoveall (an, fn, a) ->
        let pp fmt (an, fn, a) =
          Format.fprintf fmt "removeall_%a_%a (%a)"
            pp_str an
            pp_str fn
            f a
        in
        pp fmt (an, fn, a)

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

      | Mremoveif (an, c, la, lb, a) ->
        let pp fmt (an, c, _la, lb, _a) =
          Format.fprintf fmt "removeif_%a (%s, %a, fun the -> %a)"
            pp_str an
            const_storage
            (pp_container_kind f) c
            f lb
        in
        pp fmt (an, c, la, lb, a)

      | Mclear (an, v) ->
        let pp fmt (an, v) =
          Format.fprintf fmt "clear_%a (%a)"
            pp_str an
            (pp_container_kind f) v
        in
        pp fmt (an, v)

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
      | Maddupdate _ -> emit_error (UnsupportedTerm ("addupdate"))
      | Maddforce  _ -> emit_error (UnsupportedTerm ("addforce"))


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
            (pp_container_kind f) c
            f lb
        in
        pp fmt (an, c, la, lb, a)

      | Msort (an, c, l) ->
        let pp fmt (an, c, l) =
          Format.fprintf fmt "sort_%a (%a, %a)"
            pp_str an
            (pp_container_kind f) c
            (pp_list ", " (fun fmt (a, b) -> Format.fprintf fmt "%a %a" pp_ident a pp_sort_kind b)) l
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
          Format.fprintf fmt "cast_%a_%a(%a)"
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

      | Msetfold _ -> emit_error (UnsupportedTerm ("Msetfold"))


      (* list api expression *)

      | Mlistprepend (_, c, a) ->
        Format.fprintf fmt "list_prepend (%a, %a)"
          f c
          f a

      | Mlistheadtail _ -> assert false

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

      | Mmapfold _ -> emit_error (UnsupportedTerm ("Mmapfold"))

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

      | Mnow           -> pp_str fmt "Global.get_now ()"
      | Mtransferred   -> pp_str fmt "Global.get_amount ()"
      | Mcaller        -> pp_str fmt "Global.get_sender ()"
      | Mbalance       -> pp_str fmt "Global.get_balance ()"
      | Msource        -> pp_str fmt "Global.get_source ()"
      | Mselfaddress   -> pp_str fmt "Global.get_self_address()"
      | Mchainid       -> pp_str fmt "Global.get_chain_id ()"
      | Mmetadata      -> pp_str fmt "metadata"


      (* variable *)

      | Mvar (an, Vassetstate k, _, _) -> Format.fprintf fmt "state_%a(%a)" pp_str (Location.unloc an) f k
      | Mvar (v, Vstorevar, _, _)      -> Format.fprintf fmt "%s.%a" const_storage pp_id v
      | Mvar (v, Vstorecol, _, _)      -> Format.fprintf fmt "%s.%a" const_storage pp_id v
      | Mvar (v, Venumval, _, _)       -> pp_id fmt v
      | Mvar (v, Vdefinition, _, _)    -> pp_id fmt v
      | Mvar (v, Vlocal, _, _)         -> pp_id fmt v
      | Mvar (v, Vparam, _, _)         -> pp_id fmt v
      | Mvar (v, Vfield, _, _)         -> pp_id fmt v
      | Mvar (_, Vthe, _, _)           -> pp_str fmt "the"
      | Mvar (_, Vstate, _, _)         -> Format.fprintf fmt "%s.%s" const_storage const_state


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

      | Mforall _ -> emit_error (UnsupportedTerm ("forall"))
      | Mexists _ -> emit_error (UnsupportedTerm ("exists"))


      (* formula operators *)

      | Mimply _ -> emit_error (UnsupportedTerm ("imply"))
      | Mequiv _ -> emit_error (UnsupportedTerm ("equiv"))


      (* formula asset collection *)

      | Msetiterated  _ -> emit_error (UnsupportedTerm ("setiterated"))
      | Msettoiterate _ -> emit_error (UnsupportedTerm ("settoiterate"))


      (* formula asset collection methods *)

      | Mempty     _ -> emit_error (UnsupportedTerm ("empty"))
      | Msingleton _ -> emit_error (UnsupportedTerm ("singleton"))
      | Msubsetof  _ -> emit_error (UnsupportedTerm ("subsetof"))
      | Misempty   _ -> emit_error (UnsupportedTerm ("isempty"))
      | Munion     _ -> emit_error (UnsupportedTerm ("union"))
      | Minter     _ -> emit_error (UnsupportedTerm ("inter"))
      | Mdiff      _ -> emit_error (UnsupportedTerm ("diff"))

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
          pp_type t
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
      | Getter (f, a) -> "let", f, Some a, ""
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
