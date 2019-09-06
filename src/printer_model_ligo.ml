open Location
open Tools
open Model
open Printer_tools
open Ident

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

let pp_nothing (fmt : Format.formatter) = ()

type action = {
  name : ident;
  fun_name: ident;
  args: (ident * type_) list
}

module LigoUtils : sig
  val get_actions : model -> action list
  val is_param : action -> ident -> bool
end = struct
  let mk_action (fs : function_struct) =
    let fun_name = fs.name |> unloc in
    let id = fs.name |> unloc |> String.up_firstcase in
    let args = List.map (fun (id, t, _) -> (unloc id, t)) fs.args in
    {
      name = id;
      fun_name = fun_name;
      args = args;
    }

  let get_actions (model : model) : action list =
    List.fold_right (fun (f : function__) accu ->
        match f.node with
        | Entry fs ->
          (mk_action fs)::accu
        | _ -> accu)
      model.functions []

  let is_param (action : action) (arg_id : ident) : bool =
    List.fold_left (fun accu (id, _) ->
        accu || String.equal id arg_id) false action.args
end

let pp_model fmt (model : model) =

  let pp_model_name (fmt : Format.formatter) _ =
    Format.fprintf fmt "// contract: %a@\n"
      pp_id model.name
  in

  let pp_btyp fmt = function
    | Bbool       -> Format.fprintf fmt "bool"
    | Bint        -> Format.fprintf fmt "int"
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
    | List       -> Format.fprintf fmt "list"
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
      Format.fprintf fmt "%a(%a)"
        pp_container c
        pp_type t
    | Toption t ->
      Format.fprintf fmt "%a option"
        pp_type t
    | Ttuple ts ->
      Format.fprintf fmt "%a"
        (pp_list " * " pp_type) ts
    | Tassoc (k, v) ->
      Format.fprintf fmt "map(%a, %a)"
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
  (* const operations : list(operation) = nil ; *)


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
      | Mif (c, t, None) ->
        Format.fprintf fmt "@[if (%a) then @[%a@] else skip@]"
          f c
          f t

      | Mif (c, t, Some e) ->
        Format.fprintf fmt "@[if (%a) then @\n  @[%a@]@\nelse @\n  @[%a @]@]"
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
          Format.fprintf fmt "get_%a (_s, %a)"
            pp_str c
            f k
        in
        pp fmt (c, k)

      | Mset (c, l, k, v) ->
        let pp fmt (c, l, k, v) =
          Format.fprintf fmt "set_%a (_s, %a, %a)"
            pp_str c
            f k
            f v
        in
        pp fmt (c, l, k, v)

      | Maddasset (an, i) ->
        let pp fmt (an, i) =
          Format.fprintf fmt "add_%a (_s, %a)"
            pp_str an
            f i
        in
        pp fmt (an, i)

      | Maddfield (an, fn, c, i) ->
        let pp fmt (an, fn, c, i) =
          Format.fprintf fmt "add_%a_%a (_s, %a, %a)"
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
          Format.fprintf fmt "remove_%a (_s, %a%a)"
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
          Format.fprintf fmt "remove_%a_%a (_s, %a, %a%a)"
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
          Format.fprintf fmt "select_%a (_s, %a, fun the -> %a)"
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

      | Mmem (an, c, i) ->
        let pp fmt (an, c, i) =
          Format.fprintf fmt "mem_%a (%a, %a)"
            pp_str an
            f c
            f i
        in
        pp fmt (an, c, i)

      | Msubset (an, c, i) ->
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
          Format.fprintf fmt "sum_%a_%a (_s, %a)"
            pp_str an
            pp_id fd
            f c
        in
        pp fmt (an, fd, c)

      | Mmin (an, fd, c) ->
        let pp fmt (an, fd, c) =
          Format.fprintf fmt "min_%a_%a (_s, %a)"
            pp_str an
            pp_id fd
            f c
        in
        pp fmt (an, fd, c)

      | Mmax (an, fd, c) ->
        let pp fmt (an, fd, c) =
          Format.fprintf fmt "max_%a_%a (_s, %a)"
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

        Format.fprintf fmt "fail \"%a\""
          pp_fail_type ft

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

      | Misempty  (l, r) ->
        let pp fmt (l, r) =
          Format.fprintf fmt "isempty_%a <-> %a"
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

      | Mmulticomp (e, l) ->
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
          Format.fprintf fmt "%a =/= %a"
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

      | Mrecord l ->
        let asset_name =
          match mtt.type_ with
          | Tasset asset_name -> asset_name
          | _ -> assert false
        in
        let a = Utils.get_info_asset model asset_name in
        let ll = List.map (fun (i,_,_) -> dumloc i) a.values in

        let lll = List.map2 (fun x y -> (x, y)) ll l in

        Format.fprintf fmt "{ %a }"
          (pp_list "; " (fun fmt (a, b)->
               Format.fprintf fmt "%a = %a"
                 pp_id a
                 f b)) lll
      | Mletin (ids, a, t, b) ->
        Format.fprintf fmt "const %a%a = %a ;@\n@[%a@]"
          (pp_if (List.length ids > 1) (pp_paren (pp_list ", " pp_id)) (pp_list ", " pp_id)) ids
          (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t
          f a
          f b
      | Mvarstorevar v -> Format.fprintf fmt "_s.%a" pp_id v
      | Mvarstorecol v -> Format.fprintf fmt "_s.%a" pp_id v
      | Mvarenumval v  -> pp_id fmt v
      | Mvarfield v    -> pp_id fmt v
      | Mvarlocal v    -> pp_id fmt v
      | Mvarparam v    -> pp_str fmt ("action." ^ (unloc v))
      | Mvarthe        -> pp_str fmt "the"
      | Mstate         -> pp_str fmt "state"
      | Mnow           -> pp_str fmt "now"
      | Mtransferred   -> pp_str fmt "amount"
      | Mcaller        -> pp_str fmt "sender"
      | Mbalance       -> pp_str fmt "balance"
      | Mnone          -> pp_str fmt "None"
      | Msome v        ->
        Format.fprintf fmt "Some (%a)"
          f v
      | Marray l ->
        begin
          match mtt.type_ with
          | Tassoc (k , v) ->
            begin
              match l with
              | [] -> Format.fprintf fmt "(Map : (%a, %a) map)"
                        pp_btyp k
                        pp_type v
              | _ ->
                Format.fprintf fmt "[%a]"
                  (pp_list "; " f) l
            end
          | _ ->
            Format.fprintf fmt "[%a]"
              (pp_list "; " f) l
        end
      | Mint v -> pp_big_int fmt v
      | Muint v -> pp_big_int fmt v
      | Mbool b -> pp_str fmt (if b then "True" else "False")
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
        let v =
          match c with
          | Tez   -> Big_int.mult_big_int v (Big_int.big_int_of_int 1000)
          | Mutez -> v
        in
        Format.fprintf fmt "%amtz"
          pp_big_int v
      | Maddress v ->
        Format.fprintf fmt "(%a : address)"
          pp_str v
      | Mduration v -> pp_str fmt v
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
        Format.fprintf fmt "for (%a in %a)@\n (@[<v 2>%a@])@\n"
          pp_id i
          f c
          f b
      | Mfold (i, is, c, b) ->
        Format.fprintf fmt
          "List.fold (fun (%a, (%a)) ->@\n\
           @[  %a@]) %a (%a)@\n"
          pp_id i (pp_list ", " pp_id) is
          f b
          f c
          (pp_list ", " pp_id) is
      | Mseq is ->
        Format.fprintf fmt "@[%a@]"
          (pp_list ";@\n" f) is

      | Massign (op, l, r) ->
        let lhs : ident =
          if Utils.is_field_storage model (unloc l)
          then "s." ^ (unloc l)
          else (unloc l)
        in
        Format.fprintf fmt "%s %a %a"
          lhs
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
      | Mlabel i -> ()
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
      | Msetunmoved _                    -> emit_error (UnsupportedTerm ("setunmoved"))
      | Msetadded _                      -> emit_error (UnsupportedTerm ("setadded"))
      | Msetremoved _                    -> emit_error (UnsupportedTerm ("setremoved"))
      | Msetiterated _                   -> emit_error (UnsupportedTerm ("setiterated"))
      | Msettoiterate _                  -> emit_error (UnsupportedTerm ("settoiterate"))
      | MOnlyByRole _                    -> emit_error (UnsupportedTerm ("onlyByRole"))
      | MOnlyInAction _                  -> emit_error (UnsupportedTerm ("onlyInAction"))
      | MOnlyByRoleInAction _            -> emit_error (UnsupportedTerm ("onlyByRoleInAction"))
      | MNotByRole _                     -> emit_error (UnsupportedTerm ("notByRole"))
      | MNotInAction _                   -> emit_error (UnsupportedTerm ("notInAction"))
      | MNotByRoleInAction _             -> emit_error (UnsupportedTerm ("notByRoleInAction"))
      | MsecTransferredBy _              -> emit_error (UnsupportedTerm ("secTransferredBy"))
      | MsecTransferredTo _              -> emit_error (UnsupportedTerm ("secTransferredTo"))
      | Manyaction                       -> emit_error (UnsupportedTerm ("anyaction"))
      | Mremoveif _                      -> emit_error (UnsupportedTerm ("removeif"))
    in
    f fmt mt
  in

  let pp_action_action (fmt : Format.formatter) (action : action) =
    match action.args with
    | [] ->
      Format.fprintf fmt "type action_%a is unit@\n"
        pp_str action.fun_name
    | _ ->
      Format.fprintf fmt
        "type action_%a is record [@\n  \
         @[%a@]@\n\
         ]@\n"
        pp_str action.fun_name
        (pp_list "@\n" (fun fmt (id, t) ->
             Format.fprintf fmt "%a : %a;"
               pp_str id
               pp_type t)) action.args
  in

  let pp_action_type (fmt : Format.formatter) _ =
    let actions = LigoUtils.get_actions model in
    Format.fprintf fmt
      "%a@\n\
       type action is@\n  \
       @[%a@]@\n"
      (pp_list "@\n" pp_action_action) actions
      (pp_list "@\n" (fun fmt action ->
           Format.fprintf fmt "| %s of action_%s"
             action.name
             action.fun_name)) actions
  in

  let pp_record_item (fmt : Format.formatter) (record_item : record_item) =
    Format.fprintf fmt
      "%a : %a;"
      pp_id record_item.name
      pp_type record_item.type_
  in

  let pp_record (fmt : Format.formatter) (record : record) =
    Format.fprintf fmt
      "type %a is record [@\n  \
       @[%a@]@\n\
       ]@\n"
      pp_id record.name
      (pp_list "@\n" pp_record_item) record.values
  in

  let pp_decl (fmt : Format.formatter) (decl : decl_node) =
    match decl with
    | Denum e -> ()
    | Drecord r -> pp_record fmt r
    | Dcontract c -> ()
  in

  let pp_decls (fmt : Format.formatter) _ =
    (pp_list "@\n" pp_decl) fmt model.decls
  in

  let pp_storage_item (fmt : Format.formatter) (si : storage_item) =
    Format.fprintf fmt
      "%a : %a;"
      pp_id si.name
      pp_type si.typ
  in

  let pp_storage (fmt : Format.formatter) _ =
    Format.fprintf fmt
      "type storage_type is record [@\n  \
       @[%a@]@\n\
       ]@\n"
      (pp_list "@\n" pp_storage_item) model.storage
  in


  let pp_storage_const fmt = function
    | Get an ->
      let _, t = Utils.get_asset_key model (to_lident an) in
      Format.fprintf fmt
        "function get_%s (const s : storage_type; const key : %a) : %s is@\n  \
         begin@\n    \
         const res : %s = get_force(key , s.%s_assets);@\n  \
         end with (res)@\n"
        an pp_btyp t an an an
    | Set an ->
      (* let _, t = Utils.get_asset_key model (to_lident an) in *)
      Format.fprintf fmt "// TODO api storage: Set"
    (* "let[@inline] set_%s (s, key, asset : storage * %a * %s) : storage =@\n  \
       s.%s_assets <- Map.update key (Some asset) s.%s_assets@\n"
       an pp_btyp t an an an *)

    | Add an ->
      (* let k, t = Utils.get_asset_key model (to_lident an) in *)
      Format.fprintf fmt "// TODO api storage: Add"
    (* "let[@inline] add_%s (s, asset : storage * %s) : storage =@\n  \
       let key = asset.%s in@\n  \
       let s = s.%s_keys <- add_list key s.%s_keys in@\n  \
       s.%s_assets <- Map.update key (Some asset) s.%s_assets@\n"
       an an k an an an an *)

    | Remove an ->
      (* let _, t = Utils.get_asset_key model (to_lident an) in *)
      Format.fprintf fmt "// TODO api storage: Remove"
    (* "let[@inline] remove_%s (s, key : storage * %a) : storage =@\n  \
       let s = s.%s_keys <- remove_list key s.%s_keys in@\n  \
       s.%s_assets <- Map.update key None s.%s_assets@\n"
       an pp_btyp t an an an an *)

    | Clear an ->
      (* let k, t = Utils.get_asset_key model (to_lident an) in *)
      Format.fprintf fmt "// TODO api storage: Clear"
    (* "let[@inline] clear_%s (s : storage) : storage =@\n  \
       let s = s.%s_keys <- [] in@\n  \
       s.%s_assets <- (Map : (%a, %s) map)@\n"
       an an an pp_btyp t an *)

    | Reverse an ->
      Format.fprintf fmt "// TODO api storage: Reverse"
    (* "let[@inline] reverse_%s (s : storage) : storage =@\n  \
       s.%s_keys <- List.rev s.%s_keys@\n"
       an an an *)

    | UpdateAdd (an, fn) ->
      (* let k, t = Utils.get_asset_key model (to_lident an) in
         let ft, c = Utils.get_field_container model an fn in
         let kk, _ = Utils.get_asset_key model (to_lident ft) in *)
      Format.fprintf fmt "// TODO api storage: UpdateAdd"
    (* "let[@inline] add_%s_%s (s, a, b : storage * %s * %s) : storage =@\n  \
       let asset = a.%s <- add_list b.%a a.%s in@\n  \
       s.%s_assets <- Map.update a.%a (Some asset) s.%s_assets@\n"
       an fn an ft
       fn pp_str kk fn
       an pp_str k an *)

    | UpdateRemove (an, fn) ->
      (* let k, t = Utils.get_asset_key model (to_lident an) in
         let ft, c = Utils.get_field_container model an fn in
         let kk, tt = Utils.get_asset_key model (to_lident ft) in *)
      Format.fprintf fmt "// TODO api storage: UpdateRemove"
    (* "let[@inline] remove_%s_%s (s, a, key : storage * %s * %a) : storage =@\n  \
       let asset = a.%s <- remove_list key a.%s in@\n  \
       s.%s_assets <- Map.update a.%a (Some asset) s.%s_assets@\n"
       an fn an pp_btyp tt
       fn fn
       an pp_str k an *)

    | UpdateClear (an, fn) ->
      (* let k, t = Utils.get_asset_key model (to_lident an) in *)
      Format.fprintf fmt "// TODO api storage: UpdateClear"
    (* "let[@inline] clear_%s_%s (s, a : storage * %s) : storage =@\n  \
       let key = a.%s in@\n  \
       let asset = get_%s (s, key) in@\n  \
       let asset = asset.%s <- [] in@\n  \
       s.%s_assets <- Map.update a.%s (Some asset) s.%s_assets@\n"
       an fn an
       k
       an
       fn
       an k an *)

    | UpdateReverse (an, fn) ->
      (* let k, t = Utils.get_asset_key model (to_lident an) in *)
      Format.fprintf fmt "// TODO api storage: UpdateReverse"
    (* "let[@inline] reverse_%s_%s (s, a : storage * %s) : storage =@\n  \
       let key = a.%s in@\n  \
       let asset = get_%s (s, key) in@\n  \
       let asset = asset.%s <- List.rev asset.%s in@\n  \
       s.%s_assets <- Map.update a.%s (Some asset) s.%s_assets@\n"
       an fn an
       k
       an
       fn fn
       an k an *)

    | ToKeys an ->
      Format.fprintf fmt "// TODO api storage: ToKeys"
      (* "let[@inline] to_keys_%s (s : storage) : storage =@\n  \
         s (*TODO*)@\n"
         an *)
  in

  let pp_container_const fmt = function
    | AddItem t-> Format.fprintf fmt "add\t %a" pp_type t
    | RemoveItem t -> Format.fprintf fmt "remove\t %a" pp_type t
    | ClearItem t -> Format.fprintf fmt "clear\t %a" pp_type t
    | ReverseItem t -> Format.fprintf fmt "reverse %a" pp_type t
  in

  let pp_function_const fmt = function
    | Select (an, _) ->
      (* let k, t = Utils.get_asset_key model (to_lident an) in *)
      Format.fprintf fmt "// TODO api storage: Select"
    (* "let[@inline] select_%s (s, l, p : storage * %a list * (%s -> bool)) : %a list =@\n  \
       List.fold (fun (x, accu) ->@\n  \
       let a = get_%s (s, x) in@\n  \
       if p a@\n  \
       then add_list a.%s accu@\n  \
       else accu@\n  \
       ) l []@\n"
       an pp_btyp t an pp_btyp t
       an
       k *)

    | Sort (an, fn) ->
      Format.fprintf fmt "// TODO api storage: Sort"
    (* "let[@inline] sort_%s_%s (s : storage) : unit =@\n  \
       () (*TODO*)@\n"
       an fn *)

    | Contains an ->
      (* let _, t = Utils.get_asset_key model (to_lident an) in *)
      Format.fprintf fmt "// TODO api storage: Contains"
    (* "let[@inline] contains_%s ((l, key) : %a list * %a) : bool =@\n  \
       List.fold (fun (x, accu) ->@\n    \
       accu || x = key@\n  \
       ) l false@\n"
       an
       pp_btyp t
       pp_btyp t *)

    | Nth an ->
      (* let _, t = Utils.get_asset_key model (to_lident an) in *)
      Format.fprintf fmt "// TODO api storage: Nth"
    (* "let[@inline] nth_%s (s, l, idx : storage * %a list * int) : %s =@\n  \
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
       an *)

    | Count an ->
      (* let _, t = Utils.get_asset_key model (to_lident an) in *)
      Format.fprintf fmt "// TODO api storage: Count"
    (* "let[@inline] count_%s (l : %a list) : int =@\n  \
       List.fold (fun (_, accu) ->@\n    \
       accu + 1@\n  \
       ) l 0@\n"
       an
       pp_btyp t *)

    | Sum (an, fn) ->
      (* let _, tk = Utils.get_asset_key model (to_lident an) in
         let _, t, _ = Utils.get_asset_field model (dumloc an, fn) in *)
      Format.fprintf fmt "// TODO api storage: Sum"
    (* "let[@inline] sum_%s_%s (s, l : storage * %a list) : %a =@\n  \
       List.fold (fun (k, accu) ->@\n    \
       let x = @\n   \
       match Map.find k s.%s_assets with@\n  \
       | Some v -> v@\n  \
       | _ -> failwith \"not_found\" @\n    \
       in@\n    \
       accu + x.%s@\n  \
       ) l %s@\n"
       an fn pp_btyp tk pp_type t
       an
       fn
       (show_zero t) *)

    | Min (an, fn) ->
      (* let _, tk = Utils.get_asset_key model (to_lident an) in
         let _, t, _ = Utils.get_asset_field model (dumloc an, fn) in *)
      Format.fprintf fmt "// TODO api storage: Min"
    (* "let[@inline] min_%s_%s (s, l : storage * %a list) : %a =@\n  \
       match l with@\n  \
       | [] -> failwith \"empty list\"@\n  \
       | e::t ->@\n  \
       let x = @\n   \
       match Map.find e s.%s_assets with@\n  \
       | Some v -> v@\n  \
       | _ -> failwith \"not_found\" @\n    \
       in@\n    \
       let init = x.%s in@\n    \
       List.fold (fun (k, accu) ->@\n    \
       let x = @\n   \
       match Map.find k s.%s_assets with@\n  \
       | Some v -> v@\n  \
       | _ -> failwith \"not_found\" @\n    \
       in@\n    \
       if accu > x.%s@\n  \
       then x.%s@\n  \
       else accu@\n  \
       ) t init@\n"
       an fn pp_btyp tk pp_type t
       an
       fn
       an
       fn
       fn *)

    | Max (an, fn) ->
      (* let _, tk = Utils.get_asset_key model (to_lident an) in
         let _, t, _ = Utils.get_asset_field model (dumloc an, fn) in *)
      Format.fprintf fmt "// TODO api storage: Max"
    (* "let[@inline] max_%s_%s (s, l : storage * %a list) : %a =@\n  \
       match l with@\n  \
       | [] -> failwith \"empty list\"@\n  \
       | e::t ->@\n  \
       let x = @\n   \
       match Map.find e s.%s_assets with@\n  \
       | Some v -> v@\n  \
       | _ -> failwith \"not_found\" @\n    \
       in@\n    \
       let init = x.%s in@\n    \
       List.fold (fun (k, accu) ->@\n    \
       let x = @\n   \
       match Map.find k s.%s_assets with@\n  \
       | Some v -> v@\n  \
       | _ -> failwith \"not_found\" @\n    \
       in@\n    \
       if accu < x.%s@\n  \
       then x.%s@\n  \
       else accu@\n  \
       ) t init@\n"
       an fn pp_btyp tk pp_type t
       an
       fn
       an
       fn
       fn *)

    | Shallow _ -> ()
    | Unshallow _ -> ()
    | Listtocoll _ -> ()

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
    pp_api_item_node fmt api_item.node_item
  in

  let pp_api_items fmt _ =
    let filter_api_items l : api_item list =
      let contains_select_asset_name a_name l : bool =
        List.fold_left (fun accu x ->
            match x.node_item with
            | APIFunction  (Select (an, _)) -> accu || String.equal an a_name
            | _ -> accu
          ) false l
      in
      List.fold_right (fun (x : api_item) accu ->
          if x.only_formula
          then accu
          else
            match x.node_item with
            | APIFunction  (Select (an, p)) when contains_select_asset_name an accu -> accu
            | _ -> x::accu
        ) l []
    in
    let l : api_item list = filter_api_items model.api_items in
    if List.is_empty l
    then pp_nothing fmt
    else
      Format.fprintf fmt "(* API function *)@\n@\n%a@\n"
        (pp_list "@\n" pp_api_item) l
  in

  let pp_function (fmt : Format.formatter) (f : function__) =
    match f.node with
    | Entry fs ->
      let name = fs.name in
      Format.fprintf fmt
        "function %a(const action : action_%a; const s : storage_type) : (list(operation) * storage_type) is@\n  \
         begin@\n    \
         @[%a@]@\n  \
         end with ((nil : list(operation)), s)@\n"
        pp_id name
        pp_id name
        (fun fmt x -> begin
             match unloc name with
             | "add" | "consume" | "clear_expired" -> pp_str fmt "skip"
             | _ -> pp_mterm fmt x
           end) fs.body

    | Function _ -> ()
  in

  let pp_functions (fmt : Format.formatter) _ =
    (pp_list "@\n" pp_function) fmt model.functions
  in

  let pp_main_function (fmt : Format.formatter) _ =
    let actions = LigoUtils.get_actions model in
    Format.fprintf fmt
      "function main(const action : action ; const s : storage_type) : (list(operation) * storage_type) is@\n  \
       block {skip} with@\n  \
       case action of@\n  \
       @[%a@]@\n  \
       end@\n"
      (pp_list "@\n"
         (fun fmt action -> Format.fprintf fmt "| %s (a) -> %s(a, s)"
             action.name
             action.fun_name
         )) actions
  in

  Format.fprintf fmt "// LIGO output generated by archetype@\n@\n\
                      %a@\n\
                      %a@\n\
                      %a@\n\
                      %a@\n\
                      %a@\n\
                      %a@\n\
                      %a@\n\
                      @."
    pp_model_name ()
    pp_decls ()
    pp_storage ()
    pp_action_type ()
    pp_api_items ()
    pp_functions ()
    pp_main_function ()

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
