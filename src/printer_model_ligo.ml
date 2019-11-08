open Location
open Tools
open Model
open Printer_tools
open Ident
open Ligo_fun

exception Anomaly of string

type error_desc =
  | UnsupportedBreak
  | UnsupportedTerm of string
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

let const_storage = "s_"
let const_state = "state"

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


type env = {
  f: function__ option;
  select_preds: mterm list;
}

let mk_env ?f ?(select_preds=[]) () : env =
  { f; select_preds }

let get_preds_index l e : int =
  match List.index_of (fun x -> Model.cmp_mterm x e) l with
  | -1 -> assert false
  | _ as i -> i

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
    | Pconst i -> Format.fprintf fmt "%a(unit)" pp_id i
    | Pwild -> pp_str fmt "_"
  in

  let pp_mterm (env : env) fmt (mt : mterm) =
    let rec f fmt (mtt : mterm) =
      let pp_mterm_block fmt (x : mterm) =
        match x with
        | { node = Mseq l; _} when List.length l >= 2 ->
          Format.fprintf fmt " block {@\n  @[%a@] }"
            (pp_list ";@\n" f) l
        | _ ->
          Format.fprintf fmt " @\n  @[%a@]"
            f x
      in
      match mtt.node with
      | Mif (c, t, None) when (match t.node with | Mfail _ -> true | _ -> false) ->
        Format.fprintf fmt "@[if (%a) then @[%a@] else skip@]"
          f c
          f t

      | Mif (c, t, None) ->
        Format.fprintf fmt "@[if %a then%a@\nelse @\n  skip@]"
          pp_mterm_block c
          pp_mterm_block t

      | Mif (c, t, Some e) ->
        Format.fprintf fmt "@[if %a then%a@\nelse%a@]"
          f c
          pp_mterm_block t
          pp_mterm_block e

      | Mmatchwith (e, l) ->
        let pp fmt (e, l) =
          Format.fprintf fmt "case %a of@\n  @[%a@]@\nend"
            f e
            (pp_list "@\n" (fun fmt (p, x) ->
                 Format.fprintf fmt "| %a -> block {@\n  @[%a@]@\n}"
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
          Format.fprintf fmt "%s := add_shallow_%a (%s, %a)"
            const_storage
            pp_str e
            const_storage
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
          Format.fprintf fmt "get_%a (%s, %a)"
            pp_str c
            const_storage
            f k
        in
        pp fmt (c, k)

      | Mset (c, l, k, v) ->
        let pp fmt (c, _l, k, v) =
          Format.fprintf fmt "%s := set_%a (%s, %a, %a)"
            const_storage
            pp_str c
            const_storage
            f k
            f v
        in
        pp fmt (c, l, k, v)

      | Maddasset (an, i) ->
        let pp fmt (an, i) =
          Format.fprintf fmt "%s := add_%a (%s, %a)"
            const_storage
            pp_str an
            const_storage
            f i
        in
        pp fmt (an, i)

      | Maddfield (an, fn, c, i) ->
        let pp fmt (an, fn, c, i) =
          Format.fprintf fmt "%s := add_%a_%a (%s, %a, %a)"
            const_storage
            pp_str an
            pp_str fn
            const_storage
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
          Format.fprintf fmt "%s := remove_%a (%s, %a%a)"
            const_storage
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
             let k, _ = Utils.get_asset_key model an in
             true, "." ^ k
           | _ -> false, ""
          ) in
        let pp fmt (an, fn, c, i) =
          Format.fprintf fmt "%s := remove_%a_%a (%s, %a, %a%a)"
            const_storage
            pp_str an
            pp_str fn
            const_storage
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
          Format.fprintf fmt "clear_%a (%s)"
            pp_str an
            const_storage
        in
        pp fmt (an)

      | Mclearfield (an, fn, i) ->
        let pp fmt (an, fn, i) =
          Format.fprintf fmt "clear_%a_%a (%s, %a)"
            pp_str an
            pp_str fn
            const_storage
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
          Format.fprintf fmt "reverse_%a (%s)"
            pp_str an
            const_storage
        in
        pp fmt (an)

      | Mreversefield (an, fn, i) ->
        let pp fmt (an, fn, i) =
          Format.fprintf fmt "reverse_%a_%a (%s, %a)"
            pp_str an
            pp_str fn
            const_storage
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
        let index : int = get_preds_index env.select_preds p in
        let pp fmt (an, c, _p) =
          Format.fprintf fmt "select_%a_%i (%s, %a)"
            pp_str an index
            const_storage
            f c
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
        let pp fmt (an, fd, c) =
          Format.fprintf fmt "sum_%a_%a (%s, %a)"
            pp_str an
            pp_id fd
            const_storage
            f c
        in
        pp fmt (an, fd, c)

      | Mmin (an, fd, c) ->
        let pp fmt (an, fd, c) =
          Format.fprintf fmt "min_%a_%a (%s, %a)"
            pp_str an
            pp_id fd
            const_storage
            f c
        in
        pp fmt (an, fd, c)

      | Mmax (an, fd, c) ->
        let pp fmt (an, fd, c) =
          Format.fprintf fmt "max_%a_%a (%s, %a)"
            pp_str an
            pp_id fd
            const_storage
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

        Format.fprintf fmt "failwith (\"%a\")"
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

        Format.fprintf fmt "record@\n  @[%a@]@\nend"
          (pp_list "@\n" (fun fmt (a, b)->
               Format.fprintf fmt "%a = %a;"
                 pp_id a
                 f b)) lll
      | Mletin (ids, a, t, b, _) ->
        Format.fprintf fmt "const %a%a = %a ;@\n@[%a@]"
          (pp_if (List.length ids > 1) (pp_paren (pp_list ", " pp_id)) (pp_list ", " pp_id)) ids
          (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t
          f a
          f b
      | Mdeclvar (ids, t, v) ->
        Format.fprintf fmt "const %a%a = %a"
          (pp_if (List.length ids > 1) (pp_paren (pp_list ", " pp_id)) (pp_list ", " pp_id)) ids
          (pp_option (fun fmt -> Format.fprintf fmt  " : %a" pp_type)) t
          f v
      | Mvarstorevar v -> Format.fprintf fmt "%s.%a" const_storage pp_id v
      | Mvarstorecol v -> Format.fprintf fmt "%s.%a" const_storage pp_id v
      | Mvarenumval v  -> pp_id fmt v
      | Mvarfield v    -> pp_id fmt v
      | Mvarlocal v    ->
        begin
        match mtt.type_ with
        | Tstate
        | Tenum _ -> Format.fprintf fmt "%a (unit)" pp_id v
        | _ -> pp_id fmt v
       end
      | Mvarparam v    ->
        Format.fprintf fmt "%a%a"
          (fun fmt x ->
             match x with
             | Some ({node = Entry _ }) -> pp_str fmt "action."
             | _ -> ()
          ) env.f
          pp_id v
      | Mvarthe        -> pp_str fmt "the"
      | Mvarstate      -> Format.fprintf fmt "%s.%s" const_storage const_state
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
          match l with
          | [] -> Format.fprintf fmt "(nil : %a)" pp_type mtt.type_
          | _ ->
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
                Format.fprintf fmt "list@\n  @[%a@]@\nend"
                  (pp_list "@\n" (fun fmt -> Format.fprintf fmt "%a;" f)) l
            end
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
        begin
          let v =
            match c with
            | Tz -> v
            | Mtz -> Big_int.mult_int_big_int 1000 v
            | Mutz -> assert false
          in
          Format.fprintf fmt "%atz"
            pp_big_int v
        end
      | Maddress v ->
        Format.fprintf fmt "(\"%a\" : address)"
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
      | Mfor _ -> assert false
      (* let t, dv, sep =
         match c with
         | {type_ = Tcontainer (Tbuiltin Bstring, _)} -> "string", "\"\"", false
         | {type_ = Tcontainer (Tbuiltin (Baddress | Brole), _)} -> "address", "(\"\" : address)", false
         | {type_ = Tcontainer (Tasset an, _); node = Mvarparam _ } -> unloc an, "nth_" ^ unloc an ^ "(s_, loop_index_)", true
         | {type_ = Tcontainer (Tasset an, _)} ->
          begin
            let _, t = Utils.get_asset_key model an in
            match t with
            | Bstring -> "string", "nth_list_string (loop_col_, loop_index_)", false
            | Baddress | Brole -> "address", "(\"\" : address)", false
            | _ -> "", "", false
          end
         | _ ->
          "FIXME", "FIXME", false

         in
         Format.fprintf fmt
         "const loop_col_ : list(%s) = %a;@\n\
         var loop_index_ : nat := 0n;@\n\
         while (loop_index_ < size(loop_col_)) block {@\n  \
         %a\
         loop_index_ := loop_index_ + 1n;@\n\
         }"
         t f c
         (fun fmt _ ->
           if sep
           then ()
           else
             Format.fprintf fmt "const %a : %s = %s;@\n  \
                                 @[%a@];@\n  \ "
               pp_id i t dv
               f b
         ) () *)

      | Miter (_i, _a, _b, _c, _) -> Format.fprintf fmt "TODO: iter@\n"
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

      | Massign (op, lhs, r) ->
        Format.fprintf fmt "%a := %a"
          (
            fun fmt x ->
              if Utils.is_field_storage model (unloc x)
              then
                Format.fprintf fmt "%s.%s"
                  const_storage
                  (unloc x)
              else pp_id fmt x
          ) lhs
          (
            fun fmt r ->
              match op with
              | ValueAssign -> f fmt r
              | PlusAssign  -> Format.fprintf fmt "%a + %a" pp_id lhs f r
              | MinusAssign -> Format.fprintf fmt "%a - %a" pp_id lhs f r
              | MultAssign  -> Format.fprintf fmt "%a * %a" pp_id lhs f r
              | DivAssign   -> Format.fprintf fmt "%a / %a" pp_id lhs f r
              | AndAssign   -> Format.fprintf fmt "%a and %a" pp_id lhs f r
              | OrAssign    -> Format.fprintf fmt "%a or %a" pp_id lhs f r
          ) r

      | Massignfield (op, a, field , r) ->
        Format.fprintf fmt "%a.%a %a %a"
          f a
          pp_id field
          pp_operator op
          f r
      | Massignstate x ->
        Format.fprintf fmt "%s.%s := %a"
          const_storage
          const_state
          f x
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
      | Msetat     _                     -> emit_error (UnsupportedTerm ("setat"))
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


  let pp_enum (fmt : Format.formatter) (enum : enum) =
    let pp_enum_item (fmt : Format.formatter) (enum_item : enum_item) =
      Format.fprintf fmt
        "| %a of unit "
        pp_id enum_item.name
    in
    Format.fprintf fmt
      "type %a is@\n  \
       @[%a@]@\n"
      pp_id enum.name
      (pp_list "@\n" pp_enum_item) enum.values
  in

  let pp_record (fmt : Format.formatter) (record : record) =
    let pp_record_item (fmt : Format.formatter) (record_item : record_item) =
      Format.fprintf fmt
        "%a : %a;"
        pp_id record_item.name
        pp_type record_item.type_
    in
    Format.fprintf fmt
      "type %a is record [@\n  \
       @[%a@]@\n\
       ]@\n"
      pp_id record.name
      (pp_list "@\n" pp_record_item) record.values
  in

  let pp_decl (fmt : Format.formatter) (decl : decl_node) =
    match decl with
    | Denum e -> pp_enum fmt e
    | Drecord r -> pp_record fmt r
    | Dcontract _c -> ()
  in

  let pp_decls (fmt : Format.formatter) _ =
    (pp_list "@\n" pp_decl) fmt model.decls
  in

  let pp_storage_item (fmt : Format.formatter) (si : storage_item) =
    Format.fprintf fmt
      "%a : %a;"
      pp_str (Model.Utils.get_storage_id_name si.id)
      pp_type si.typ
  in

  let pp_storage (fmt : Format.formatter) _ =
    Format.fprintf fmt
      "type storage_type is record [@\n  \
       @[%a@]@\n\
       ]@\n"
      (pp_list "@\n" pp_storage_item) model.storage
  in


  let pp_storage_const (_env : env) fmt = function
    | Get an ->
      let _, t = Utils.get_asset_key model (to_lident an) in
      Format.fprintf fmt
        "function get_%s (const s : storage_type; const key : %a) : %s is@\n  \
         begin@\n    \
         const res : %s = get_force(key, s.%s_assets);@\n  \
         end with (res)@\n"
        an pp_btyp t an an an

    | Set an ->
      let _, t = Utils.get_asset_key model (to_lident an) in
      Format.fprintf fmt
        "function set_%s (const s : storage_type; const key : %a; const a : %s) : storage_type is@\n  \
         begin@\n    \
         const map_local : map(%a, %s) = s.%s_assets;@\n    \
         map_local[key] := a;@\n    \
         s.%s_assets := map_local;@\n  \
         end with (s)@\n"
        an pp_btyp t an
        pp_btyp t an an
        an

    | Add an ->
      let k, t = Utils.get_asset_key model (to_lident an) in
      Format.fprintf fmt
        "function add_%s (const s : storage_type; const a : %s) : storage_type is@\n  \
         begin@\n    \
         const key : %a = a.%s;@\n    \
         s.%s_keys := cons(key, s.%s_keys);@\n    \
         const map_local : map(%a, %s) = s.%s_assets;@\n    \
         map_local[key] := a;@\n    \
         s.%s_assets := map_local;@\n  \
         end with (s)@\n"
        an an
        pp_btyp t k
        an an
        pp_btyp t an an
        an

    | Remove an ->
      let _, t = Utils.get_asset_key model (to_lident an) in
      Format.fprintf fmt
        "function remove_%s (const s : storage_type; const key : %a) : storage_type is@\n  \
         var new_keys : list(%a) := (nil : list(%a));@\n  \
         function aux (const i : %a) : unit is@\n  \
         begin@\n    \
         if (key =/= i) then@\n      \
         new_keys := cons(i, new_keys);@\n    \
         else@\n      \
         skip;@\n  \
         end with unit@\n  \
         begin@\n    \
         list_iter(s.%s_keys, aux);@\n    \
         s.%s_keys := new_keys;@\n    \
         const map_local : map(%a, %s) = s.%s_assets;@\n    \
         remove key from map map_local;@\n    \
         s.%s_assets := map_local;@\n  \
         end with (s)@\n"
        an pp_btyp t
        pp_btyp t pp_btyp t
        pp_btyp t
        an
        an
        pp_btyp t an an
        an

    | Clear _an ->
      (* let k, t = Utils.get_asset_key model (to_lident an) in *)
      Format.fprintf fmt "// TODO api storage: Clear"
    (* "let[@inline] clear_%s (s : storage) : storage =@\n  \
       let s = s.%s_keys <- [] in@\n  \
       s.%s_assets <- (Map : (%a, %s) map)@\n"
       an an an pp_btyp t an *)

    | Reverse _an ->
      Format.fprintf fmt "// TODO api storage: Reverse"
    (* "let[@inline] reverse_%s (s : storage) : storage =@\n  \
       s.%s_keys <- List.rev s.%s_keys@\n"
       an an an *)

    | UpdateAdd (an, fn) ->
      let k, t = Utils.get_asset_key model (to_lident an) in
      let ft, _c = Utils.get_field_container model an fn in
      let kk, _ = Utils.get_asset_key model (to_lident ft) in
      Format.fprintf fmt
        "function add_%s_%s (const s : storage_type; const a : %s; const b : %s) : storage_type is@\n  \
         begin@\n    \
         const asset_key : %a = a.%s;@\n    \
         const asset_val : %s = get_%s(s, asset_key);@\n    \
         asset_val.%s := cons(b.%s, asset_val.%s);@\n    \
         const map_local : map(%a, %s) = s.%s_assets;@\n    \
         map_local[asset_key] := asset_val;@\n    \
         s.%s_assets := map_local;@\n    \
         s := add_%s(s, b);@\n  \
         end with (s)@\n"
        an fn an ft
        pp_btyp t k
        an an
        fn kk fn
        pp_btyp t an an
        an
        ft

    | UpdateRemove (an, fn) ->
      let k, t = Utils.get_asset_key model (to_lident an) in
      let ft, _c = Utils.get_field_container model an fn in
      let _kk, tt = Utils.get_asset_key model (to_lident ft) in
      Format.fprintf fmt
        "function remove_%s_%s (const s : storage_type; const a : %s; const key : %a) : storage_type is@\n  \
         var new_keys : list(%a) := (nil : list(%a));@\n  \
         function aux (const i : %a) : unit is@\n  \
         begin@\n    \
         if (key =/= i) then@\n      \
         new_keys := cons(i, new_keys);@\n    \
         else@\n      \
         skip;@\n  \
         end with unit@\n  \
         begin@\n    \
         const asset_key : %a = a.%s;@\n    \
         const asset_val : %s = get_%s(s, asset_key);@\n    \
         list_iter(asset_val.%s, aux);@\n    \
         asset_val.%s := new_keys;@\n    \
         const map_local : map(%a, %s) = s.%s_assets;@\n    \
         map_local[asset_key] := asset_val;@\n    \
         s.%s_assets := map_local;@\n    \
         s := remove_%s(s, key);@\n  \
         end with (s)@\n"
        an fn an pp_btyp tt
        pp_btyp tt pp_btyp tt
        pp_btyp tt
        pp_btyp t k
        an an
        fn
        fn
        pp_btyp t an an
        an
        ft

    | UpdateClear (_an, _fn) ->
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

    | UpdateReverse (_an, _fn) ->
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

    | ToKeys _an ->
      Format.fprintf fmt "// TODO api storage: ToKeys"
      (* "let[@inline] to_keys_%s (s : storage) : storage =@\n  \
         s (*TODO*)@\n"
         an *)
  in

  let pp_container_const (_env : env) fmt = function
    | AddItem t-> Format.fprintf fmt "add\t %a" pp_type t
    | RemoveItem t -> Format.fprintf fmt "remove\t %a" pp_type t
    | ClearItem t -> Format.fprintf fmt "clear\t %a" pp_type t
    | ReverseItem t -> Format.fprintf fmt "reverse %a" pp_type t
  in

  let pp_function_const (env : env) fmt = function
    | Select (an, f) ->
      let k, t = Utils.get_asset_key model (to_lident an) in
      let i = get_preds_index env.select_preds f in
      Format.fprintf fmt
        "function select_%s_%i (const s : storage_type; const l : list(%a)) : list(%a) is@\n  \
         var res : list(%a) := (nil : list(%a));@\n  \
         function aggregate (const i : %a) : unit is@\n  \
         begin@\n    \
         const the : %s = get_force(i, s.%s_assets);@\n    \
         if (%a) then@\n      \
         res := cons(the.%s, res);@\n    \
         else@\n      \
         skip;@\n  \
         end with unit@\n  \
         begin@\n    \
         list_iter(l, aggregate)@\n  \
         end with res@\n"
        an i pp_btyp t pp_btyp t
        pp_btyp t pp_btyp t
        pp_btyp t
        an an
        (pp_mterm (mk_env ())) f
        k

    | Sort (_an, _fn) ->
      Format.fprintf fmt "// TODO api storage: Sort"
    (* "let[@inline] sort_%s_%s (s : storage) : unit =@\n  \
       () (*TODO*)@\n"
       an fn *)

    | Contains an ->
      let _, t = Utils.get_asset_key model (to_lident an) in
      Format.fprintf fmt
        "function contains_%s (const l : list(%a); const key : %a) : bool is@\n  \
         var r : bool := False;@\n  \
         function aggregate (const i : %a) : unit is@\n  \
         begin@\n    \
         r := r or i = key;@\n  \
         end with unit@\n  \
         begin@\n    \
         list_iter(l, aggregate)@\n  \
         end with r@\n"
        an pp_btyp t pp_btyp t
        pp_btyp t

    | Nth _an ->
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

    | Count _an ->
      (* let _, t = Utils.get_asset_key model (to_lident an) in *)
      Format.fprintf fmt "// TODO api storage: Count"
    (* "let[@inline] count_%s (l : %a list) : int =@\n  \
       List.fold (fun (_, accu) ->@\n    \
       accu + 1@\n  \
       ) l 0@\n"
       an
       pp_btyp t *)

    | Sum (an, fn) ->
      let get_zero = function
        | _ -> "0"
      in

      let _, tk = Utils.get_asset_key model (to_lident an) in
      let _, t, _ = Utils.get_asset_field model (dumloc an, fn) in
      Format.fprintf fmt
        "function sum_%s_%s (const s : storage_type; const l : list(%a)) : %a is@\n  \
         var r : %a := %s;@\n  \
         function aggregate (const i : %a) : unit is@\n  \
         begin@\n    \
         const a : %s = get_force(i, s.%s_assets);@\n    \
         r := r + a.%s;@\n  \
         end with unit@\n  \
         begin@\n    \
         list_iter(l, aggregate)@\n  \
         end with r@\n"
        an fn pp_btyp tk pp_type t
        pp_type t (get_zero t)
        pp_btyp tk
        an an
        fn

    | Min (_an, _fn) ->
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

    | Max (_an, _fn) ->
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
    | Head _an -> Format.fprintf fmt "// TODO api storage: head"
    | Tail _an -> Format.fprintf fmt "// TODO api storage: tail"

  in

  let pp_builtin_const (_env : env) fmt = function
    | MinBuiltin t-> Format.fprintf fmt "min on %a" pp_type t
    | MaxBuiltin t-> Format.fprintf fmt "max on %a" pp_type t
  in

  let pp_api_item_node (env : env) fmt = function
    | APIStorage   v -> pp_storage_const env fmt v
    | APIContainer v -> pp_container_const env fmt v
    | APIFunction  v -> pp_function_const env fmt v
    | APIBuiltin   v -> pp_builtin_const env fmt v
  in

  let pp_api_item (env : env) fmt (api_item : api_item) =
    pp_api_item_node env fmt api_item.node_item
  in

  let pp_api_items (env : env) fmt _ =
    let filter_api_items l : api_item list =
      List.fold_right (fun (x : api_item) accu ->
          if x.only_formula
          then accu
          else x::accu
        ) l []
    in
    let l : api_item list = filter_api_items model.api_items in
    if List.is_empty l
    then pp_nothing fmt
    else
      Format.fprintf fmt "(* API function *)@\n@\n%a@\n"
        (pp_list "@\n" (pp_api_item env)) l
  in

  let pp_function (env : env) (fmt : Format.formatter) (f : function__) =
    let env = {env with f = Some f} in
    let pp_variables fmt vars =
      match vars with
      | [] -> ()
      | _ ->
        Format.fprintf fmt "@[%a@]@\n  "
          (pp_list "@\n" (fun fmt (name, type_) ->
               Format.fprintf fmt "var %s : %a := %a;"
                 name
                 pp_type type_
                 (pp_mterm env) (Model.Utils.get_default_value model type_))) vars
    in
    let pp_iterfuns fmt (iterfuns : s_interfun list) =
      match iterfuns with
      | [] -> ()
      | _ ->
        Format.fprintf fmt "@[%a@]@\n  "
          (pp_list "@\n" (fun fmt (interfun : s_interfun) ->
               Format.fprintf fmt
                 "function %s (const %s : %a) : unit is@\n  \
                  begin@\n  \
                  @[%a@]@\n  \
                  end with unit"
                 interfun.loop_id
                 interfun.arg_id
                 pp_type interfun.arg_type
                 (pp_mterm env) interfun.body
             )) iterfuns
    in
    let ligo_fun = to_ligo_fun model f in
    let name = ligo_fun.name in
    match ligo_fun.ret with
    | None ->
      Format.fprintf fmt
        "function %s(const action : action_%s; const %s : storage_type) : (list(operation) * storage_type) is@\n  \
         %a\
         %a\
         begin@\n    \
         @[%a@]@\n  \
         end with ((nil : list(operation)), %s)@\n"
        name name const_storage
        pp_variables ligo_fun.vars
        pp_iterfuns ligo_fun.iterfuns
        (pp_mterm env) ligo_fun.body
        const_storage

    | Some _ret ->
      Format.fprintf fmt
        "function %s(const %s : storage_type%a) : storage_type is@\n  \
         %a\
         %a\
         begin@\n    \
         @[%a@]@\n  \
         end with (%s)@\n"
        name
        const_storage
        (pp_list "" (fun fmt (id, type_ : ident * type_) ->
             Format.fprintf fmt
               "; const %s : %a"
               id
               pp_type type_
           )) ligo_fun.args
        pp_variables ligo_fun.vars
        pp_iterfuns ligo_fun.iterfuns
        (pp_mterm env) ligo_fun.body
        const_storage
  in

  let pp_functions (env : env) (fmt : Format.formatter) _ =
    (pp_list "@\n" (pp_function env)) fmt model.functions
  in

  let pp_main_function (fmt : Format.formatter) _ =
    let actions = LigoUtils.get_actions model in
    Format.fprintf fmt
      "function main(const action : action ; const %s : storage_type) : (list(operation) * storage_type) is@\n  \
       block {skip} with@\n  \
       case action of@\n  \
       @[%a@]@\n  \
       end@\n"
      const_storage
      (pp_list "@\n"
         (fun fmt action -> Format.fprintf fmt "| %s (a) -> %s(a, %s)"
             action.name
             action.fun_name
             const_storage
         )) actions
  in

  let compute_env _ =
    let select_preds =
      List.fold_right (fun x accu ->
          match x.only_formula, x.node_item with
          | false, APIFunction (Select (_, pred)) ->
            if not (List.exists (Model.cmp_mterm pred) accu)
            then pred::accu
            else accu
          | _ -> accu
        ) model.api_items []
    in
    mk_env ~select_preds:select_preds ()
  in

  let env = compute_env () in
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
    (pp_api_items env) ()
    (pp_functions env) ()
    pp_main_function ()

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
