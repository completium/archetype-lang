open Location
open Tools
open Model
open Printer_tools
open Ident

exception Anomaly of string

type error_desc =
  | UnsupportedTerm of string
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

let const_storage = "s_"
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

type env = {
  f: function__ option;
  select_preds: mterm list;
  sum_preds: mterm list;
  consts: (ident * mterm) list;
}

let mk_env ?f ?(select_preds=[]) ?(sum_preds=[]) ?(consts=[]) () : env =
  { f; select_preds; sum_preds; consts }

exception Found

let is_internal l (id : lident) : bool =
  try
    List.iter (fun (x : ident * mterm) -> if (String.equal (unloc id) (fst x)) then raise Found) l ;
    false
  with
  | Found -> true

let is_const (env : env) (id : lident) : bool = is_internal env.consts id

let get_const_dv (env : env) (id : lident) : mterm =
  List.assoc (unloc id) env.consts

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
  val get_contract_actions : model -> (ident * action list) list
end = struct
  let mk_action_fs (fs : function_struct) =
    let fun_name = fs.name |> unloc in
    let id = fs.name |> unloc |> String.up_firstcase in
    let args = List.map (fun (id, t, _) -> (unloc id, t)) fs.args in
    {
      name = id;
      fun_name = fun_name;
      args = args;
    }

  let mk_action_contract_signature (cs : contract_signature) : action =
    let fun_name = cs.name |> unloc in
    let id = cs.name |> unloc |> String.up_firstcase in
    let args = List.map (fun (id, t) -> (unloc id, t)) cs.args in
    {
      name = id;
      fun_name = fun_name;
      args = args;
    }

  let get_actions (model : model) : action list =
    List.fold_right (fun (f : function__) accu ->
        match f.node with
        | Entry fs ->
          (mk_action_fs fs)::accu
        | _ -> accu)
      model.functions []

  let is_param (action : action) (arg_id : ident) : bool =
    List.fold_left (fun accu (id, _) ->
        accu || String.equal id arg_id) false action.args

  let get_contract_actions (model : model) : (ident * action list) list =
    List.fold_right
      (fun x accu ->
         match x with
         | Dcontract x -> (unloc x.name, List.map mk_action_contract_signature x.signatures)::accu
         | _ -> accu
      ) model.decls []
end

let pp_model_internal fmt (model : model) b =

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
    | Bdate       -> Format.fprintf fmt "date"
    | Bduration   -> Format.fprintf fmt "duration"
    | Btimestamp  -> Format.fprintf fmt "timestamp"
    | Bstring     -> Format.fprintf fmt "string"
    | Baddress    -> Format.fprintf fmt "address"
    | Brole       -> Format.fprintf fmt "address"
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
      Format.fprintf fmt "int"
    | Tenum en ->
      Format.fprintf fmt "%a" pp_id en
    | Tcontract _ -> pp_type fmt (Tbuiltin Baddress)
    | Tbuiltin b -> pp_btyp fmt b
    | Tcontainer (t, c) ->
      Format.fprintf fmt "%a(%a)"
        pp_container c
        pp_type t
    | Tlist t ->
      Format.fprintf fmt "list(%a)"
        pp_type t
    | Toption t ->
      Format.fprintf fmt "option(%a)"
        pp_type t
    | Ttuple ts ->
      Format.fprintf fmt "(%a)"
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

  let pp_pattern fmt (p : pattern) =
    match p.node with
    | Pconst i -> Format.fprintf fmt "%a(unit)" pp_id i
    | Pwild -> pp_str fmt "_"
  in

  let pp_sort_kind fmt = function
    | SKasc -> pp_str fmt "asc"
    | SKdesc -> pp_str fmt "desc"
  in

  let pp_postfix_sort = (pp_list "_" (fun fmt (a, b) -> Format.fprintf fmt "%s_%a" a pp_sort_kind b)) in

  let pp_abs_type fmt = function
    | Ttuple [Tbuiltin Bint; Tbuiltin Bint] -> pp_type fmt (Tbuiltin Brational)
    | t -> pp_type fmt t
  in

  let pp_mterm_gen (env : env) f fmt (mtt : mterm) =
    let pp_mterm_block fmt (x : mterm) =
      match x with
      | { node = Mseq l; _} when List.length l >= 2 ->
        Format.fprintf fmt " block {@\n  @[%a@] }"
          (pp_list ";@\n" f) l
      | { node = Mletin _; _} as a ->
        Format.fprintf fmt " block {@\n   @[%a@]@\n}"
          f a
      | _ ->
        Format.fprintf fmt " @\n  @[%a@]"
          f x
    in
    match mtt.node with
    (* lambda *)

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

    | Mapp (e, args) ->
      let pp fmt (e, args) =
        Format.fprintf fmt "%a (%a)"
          pp_id e
          (pp_list ", " f) args
      in
      pp fmt (e, args)


    (* assign *)

    | Massign (op, _, lhs, r) ->
      Format.fprintf fmt "%a := %a"
        pp_id lhs
        (
          fun fmt r ->
            match op with
            | ValueAssign -> f fmt r
            | PlusAssign  -> Format.fprintf fmt "%a + (%a)" pp_id lhs f r
            | MinusAssign -> Format.fprintf fmt "%a - (%a)" pp_id lhs f r
            | MultAssign  -> Format.fprintf fmt "%a * (%a)" pp_id lhs f r
            | DivAssign   -> Format.fprintf fmt "%a / (%a)" pp_id lhs f r
            | AndAssign   -> Format.fprintf fmt "%a and (%a)" pp_id lhs f r
            | OrAssign    -> Format.fprintf fmt "%a or (%a)" pp_id lhs f r
        ) r

    | Massignvarstore (op, _, lhs, r) ->
      Format.fprintf fmt "%s.%a := %a"
        const_storage
        pp_id lhs
        (
          fun fmt r ->
            match op with
            | ValueAssign -> f fmt r
            | PlusAssign  -> Format.fprintf fmt "%s.%a + (%a)"   const_storage pp_id lhs f r
            | MinusAssign -> Format.fprintf fmt "%s.%a - (%a)"   const_storage pp_id lhs f r
            | MultAssign  -> Format.fprintf fmt "%s.%a * (%a)"   const_storage pp_id lhs f r
            | DivAssign   -> Format.fprintf fmt "%s.%a / (%a)"   const_storage pp_id lhs f r
            | AndAssign   -> Format.fprintf fmt "%s.%a and (%a)" const_storage pp_id lhs f r
            | OrAssign    -> Format.fprintf fmt "%s.%a or (%a)"  const_storage pp_id lhs f r
        ) r

    | Massignfield (op, _, a, field , r) ->
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

    | Massignassetstate (an, k, v) ->
      Format.fprintf fmt "state_%a(%a) = %a"
        pp_ident an
        f k
        f v


    (* control *)

    | Mif (c, t, (None | Some {node = Mseq []; _})) when (match t.node with | Mfail _ -> true | _ -> false) ->
      Format.fprintf fmt "@[if (%a) then @[%a@] else skip@]"
        f c
        f t

    | Mif (c, t, (None | Some {node = Mseq []; _}) ) ->
      Format.fprintf fmt "@[if %a then%a@\nelse@\n  skip@]"
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

    | Mfor (_, _, _, None) -> assert false
    | Mfor (id, col, body, Some label) ->
      let typ =
        begin
          let is_get_body (mt : mterm) (id : ident) (asset_name : ident) =
            match mt.node with
            | Mletin ([{pldesc = i; _}], {node = (Mget (an, _) | Mgetfrommap (an, _, _))}, _, _, _) -> String.equal i id && String.equal asset_name an
            | _ -> false
          in
          match col.type_ with
          | Tcontainer (Tasset an, _) when is_get_body body (unloc id) (unloc an) ->
            begin
              let _, t = Utils.get_asset_key model (unloc an) in
              Tbuiltin t
            end
          | Tcontainer (t, _) -> t
          | _ -> assert false
        end
      in

      Format.fprintf fmt
        "function %s (const %a : %a) : unit is@\n  \
         begin@\n  \
         @[%a@]@\n\
         end with unit;@\n\
         list_iter (%s, %a)"
        label pp_id id pp_type typ
        f body
        label f col

    | Miter (_i, _a, _b, _c, _) -> Format.fprintf fmt "TODO: iter@\n"

    | Mseq is ->
      begin
        match is with
        | [] -> Format.fprintf fmt "skip"
        | _ ->
          Format.fprintf fmt "@[%a@]"
            (pp_list ";@\n" f) is
      end

    | Mreturn x ->
      Format.fprintf fmt "return %a"
        f x

    | Mlabel _i -> ()


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
      Format.fprintf fmt "failwith (\"%a\")"
        pp_fail_type ft

    | Mtransfer (v, d) ->
      Format.fprintf fmt "%s := cons(transaction(unit, %a, (get_contract(%a) : contract(unit))), %s)"
        const_operations
        f v
        f d
        const_operations

    | Mexternal (t, fid, c, args) ->
      let pp fmt (t, fid, c, args) =
        let fid = fid |> unloc |> String.up_firstcase in
        Format.fprintf fmt
          "begin@\n  \
           const c_ : contract(action_%s) = get_contract(%a);@\n  \
           const param_ : action_%s = %s (record %a end);@\n  \
           const op_: operation = transaction(param_, 0mutez, c_);@\n  \
           %s := cons(op_, %s);@\n\
           end"
          t f c
          t fid (pp_list "; " (fun fmt (id, v) -> Format.fprintf fmt "%a = %a" pp_id id f v)) args
          const_operations const_operations
      in
      pp fmt (t, fid, c, args)


    (* literals *)

    | Mint v -> pp_big_int fmt v
    | Muint v -> pp_big_int fmt v
    | Mbool b -> pp_str fmt (if b then "True" else "False")
    | Menum v -> pp_str fmt v
    | Mrational (n, d) ->
      Format.fprintf fmt "(%a div %a)"
        pp_big_int n
        pp_big_int d
    | Mstring v ->
      Format.fprintf fmt "\"%a\""
        pp_str v
    | Mcurrency (v, c) ->
      begin
        let v =
          match c with
          | Tz  -> assert false
          | Mtz -> assert false
          | Utz -> v
        in
        Format.fprintf fmt "%amutez"
          pp_big_int v
      end
    | Maddress v ->
      Format.fprintf fmt "(\"%a\" : address)"
        pp_str v
    | Mdate v ->
      Format.fprintf fmt "(\"%a\" : timestamp)"
        Core.pp_date v
    | Mduration v -> Core.pp_duration_in_seconds fmt v
    | Mtimestamp v ->
      Format.fprintf fmt "(%a : timestamp)"
        pp_big_int v
    | Mbytes v -> Format.fprintf fmt "0x%s" v


    (* control expression *)

    | Mexprif (c, t, e) ->
      Format.fprintf fmt "@[if %a then%a@\nelse%a@]"
        f c
        pp_mterm_block t
        pp_mterm_block e

    | Mexprmatchwith (e, l) ->
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


    (* composite type constructors *)

    | Mnone -> Format.fprintf fmt "(None : %a)" pp_type mtt.type_

    | Msome v ->
      Format.fprintf fmt "Some (%a)"
        f v

    | Marray l ->
      begin
        match l, mtt.type_ with
        | [], Tassoc (k , v) -> Format.fprintf fmt "(map end : map(%a, %a))" pp_btyp k pp_type v
        | _, Tassoc (k , v) -> Format.fprintf fmt "(map %a end : map(%a, %a))" (pp_list "; " f) l pp_btyp k pp_type v
        | [], _ -> Format.fprintf fmt "(nil : %a)" pp_type mtt.type_
        | _, _ -> Format.fprintf fmt "list@\n  @[%a@]@\nend"
                    (pp_list "@\n" (fun fmt -> Format.fprintf fmt "%a;" f)) l
      end

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
      Format.fprintf fmt "record@\n  @[%a@]@\nend"
        (pp_list "@\n" (fun fmt (a, b)->
             Format.fprintf fmt "%a = %a;"
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

    | Mmulticomp (_e, _l) ->
      assert false


    (* arithmetic operators *)

    | Mand (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "(%a) and (%a)"
          f l
          f r
      in
      pp fmt (l, r)

    | Mor (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "(%a) or (%a)"
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
        Format.fprintf fmt "(%a) + (%a)"
          (pp_cast Lhs l.type_ r.type_ f) l
          (pp_cast Rhs l.type_ r.type_ f) r
      in
      pp fmt (l, r)

    | Mminus (l, r) ->
      let pp fmt (l, r : mterm * mterm) =
        Format.fprintf fmt "(%a) - (%a)"
          (pp_cast Lhs l.type_ r.type_ f) l
          (pp_cast Rhs l.type_ r.type_ f) r
      in
      pp fmt (l, r)

    | Mmult (l, r) ->
      let pp fmt (l, r : mterm * mterm) =
        Format.fprintf fmt "(%a) * (%a)"
          (pp_cast Lhs l.type_ r.type_ f) l
          (pp_cast Rhs l.type_ r.type_ f) r
      in
      pp fmt (l, r)

    | Mdiv (l, r) ->
      let pp fmt (l, r : mterm * mterm) =
        Format.fprintf fmt "(%a) / (%a)"
          (pp_cast Lhs l.type_ r.type_ f) l
          (pp_cast Rhs l.type_ r.type_ f) r
      in
      pp fmt (l, r)

    | Mmodulo (l, r) ->
      let pp fmt (l, r : mterm * mterm) =
        Format.fprintf fmt "int((%a) mod (%a))"
          (pp_cast Lhs l.type_ r.type_ f) l
          (pp_cast Rhs l.type_ r.type_ f) r
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

    | Mremoveasset (an, i) ->
      let cond, str =
        (match i.type_ with
         | Tasset an ->
           let k, _ = Utils.get_asset_key model (unloc an) in
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
           let k, _ = Utils.get_asset_key model (unloc an) in
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

    | Mclearasset (an) ->
      let pp fmt (an) =
        Format.fprintf fmt "%s := clear_%a (%s)"
          const_storage
          pp_str an
          const_storage
      in
      pp fmt (an)

    | Mclearfield (an, fn, a) ->
      let pp fmt (an, fn, a) =
        Format.fprintf fmt "%s := clear_%a_%a (%s, %a)"
          const_storage
          pp_str an
          pp_str fn
          const_storage
          f a
      in
      pp fmt (an, fn, a)

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

    | Mupdate _ -> emit_error (UnsupportedTerm ("update"))
    | Mremoveif _ -> emit_error (UnsupportedTerm ("removeif"))
    | Maddupdate _ -> emit_error (UnsupportedTerm ("add_update"))


    (* asset api expression *)

    | Mget (c, k) ->
      let pp fmt (c, k) =
        Format.fprintf fmt "get_%a (%s, %a)"
          pp_str c
          const_storage
          f k
      in
      pp fmt (c, k)

    | Mselect (an, c, p) ->
      let index : int = get_preds_index env.select_preds p in
      let pp fmt (an, c, _p) =
        Format.fprintf fmt "select_%a_%i (%s, %a)"
          pp_str an index
          const_storage
          f c
      in
      pp fmt (an, c, p)

    | Msort (an, c, l) -> (* TODO *)
      let pp fmt (an, c, l) =
        Format.fprintf fmt "sort_%a_%a (%s, %a)"
          pp_str an
          pp_postfix_sort l
          const_storage
          f c
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
        Format.fprintf fmt "nth_%a (%s, %a, %a)"
          pp_str an
          const_storage
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
      let index : int = get_preds_index env.sum_preds p in
      let pp fmt (an, c, _p) =
        Format.fprintf fmt "sum_%a_%i (%s, %a)"
          pp_str an index
          const_storage
          f c
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
      let pp fmt (_src, dst, v) =
        Format.fprintf fmt "(%a : %a)"
          f v
          pp_type dst
      in
      pp fmt (src, dst, v)

    | Mgetfrommap (an, k, m) ->
      let pp fmt (_an, k, m) =
        Format.fprintf fmt "get_force(%a, %a)"
          f k
          f m
      in
      pp fmt (an, k, m)


    (* list api effect *)

    | Mlistprepend (t, c, a) ->
      Format.fprintf fmt "list_%a_prepend (%a, %a)"
        pp_type t
        f c
        f a


    (* list api expression *)

    | Mlistcontains (t, c, a) ->
      Format.fprintf fmt "list_%a_contains (%a, %a)"
        pp_type t
        f c
        f a

    | Mlistcount (t, c) ->
      Format.fprintf fmt "list_%a_count (%a)"
        pp_type t
        f c

    | Mlistnth (t, c, a) ->
      Format.fprintf fmt "list_%a_nth (%a, %a)"
        pp_type t
        f c
        f a


    (* builtin functions *)

    | Mmax (l, r) ->
      Format.fprintf fmt "max_%a (%a, %a)"
        pp_type mtt.type_
        f l
        f r

    | Mmin (l, r) ->
      Format.fprintf fmt "min_%a (%a, %a)"
        pp_type mtt.type_
        f l
        f r

    | Mabs a ->
      let pp_tmp fmt t =
        match t with
        | Tbuiltin Bnat -> ()
        | _ -> Format.fprintf fmt "_%a" pp_abs_type t
      in
      Format.fprintf fmt "abs%a (%a)"
        pp_tmp mtt.type_
        f a

    (* internal functions *)

    | Mstrconcat (l, r)->
      Format.fprintf fmt "str_concat (%a, %a)"
        f l
        f r


    (* constants *)

    | Mvarstate      -> Format.fprintf fmt "%s.%s" const_storage const_state
    | Mnow           -> pp_str fmt "now"
    | Mtransferred   -> pp_str fmt "amount"
    | Mcaller        -> pp_str fmt "sender"
    | Mbalance       -> pp_str fmt "balance"
    | Msource        -> pp_str fmt "source"


    (* variables *)

    | Mvarassetstate (an, k) ->
      Format.fprintf fmt "state_%a(%a)"
        pp_str an
        f k

    | Mvarstorevar v ->
      if (is_const env v)
      then pp_id fmt v
      else Format.fprintf fmt "%s.%a" const_storage pp_id v

    | Mvarstorecol v -> Format.fprintf fmt "%s.%a" const_storage pp_id v

    | Mvarenumval v  -> pp_id fmt v

    | Mvarlocal v    -> pp_id fmt v

    | Mvarparam v    ->
      Format.fprintf fmt "%a%a"
        (fun fmt x ->
           match x with
           | Some ({node = Entry _ }) -> pp_str fmt "action."
           | _ -> ()
        ) env.f
        pp_id v

    | Mvarfield v    -> pp_id fmt v

    | Mvarthe        -> pp_str fmt "the"


    (* rational *)

    | Mdivrat _ -> emit_error (UnsupportedTerm ("divrat"))

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
          | Lt -> "OpCmpLt(unit)"
          | Le -> "OpCmpLe(unit)"
          | Gt -> "OpCmpGt(unit)"
          | Ge -> "OpCmpGe(unit)"
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
          | Rplus  -> "OpArithPlus(unit)"
          | Rminus -> "OpArithMinus(unit)"
          | Rmult  -> "OpArithMult(unit)"
          | Rdiv   -> "OpArithDiv(unit)"
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
        Format.fprintf fmt "((%a), 1)"
          f e
      in
      pp fmt e


    (* functional *)

    | Mfold (i, is, c, b) ->
      Format.fprintf fmt
        "List.fold (fun (%a, (%a)) ->@\n\
         @[  %a@]) %a (%a)@\n"
        pp_id i (pp_list ", " pp_id) is
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
        Format.fprintf fmt "%s := add_shallow_%a (%s, %a)"
          const_storage
          pp_str e
          const_storage
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

  let rec pp_mterm (env : env) fmt (mt : mterm) = pp_mterm_gen env (pp_mterm env) fmt mt in

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

  let pp_actions (fmt : Format.formatter) postfix actions =
    Format.fprintf fmt
      "%a@\n\
       type action%s is@\n  \
       @[%a@]@\n"
      (pp_list "@\n" pp_action_action) actions
      postfix
      (pp_list "@\n" (fun fmt action ->
           Format.fprintf fmt "| %s of action_%s"
             action.name
             action.fun_name)) actions
  in

  let pp_action_type (fmt : Format.formatter) _ =
    List.iter
      (fun (name, actions) ->
         pp_actions fmt ("_" ^ name) actions;
         Format.fprintf fmt "@\n";)
      (LigoUtils.get_contract_actions model);
    pp_actions fmt "" (LigoUtils.get_actions model)
  in

  let pp_var env (fmt : Format.formatter) (var : var) =
    if (var.constant) then
      begin
        if Option.is_none var.default
        then assert false;
        Format.fprintf fmt "const %a : %a = %a@\n"
          pp_id var.name
          pp_type var.type_
          (pp_mterm env) (Option.get var.default)
      end
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

  let pp_asset (fmt : Format.formatter) (asset : asset) =
    let pp_asset_item (fmt : Format.formatter) (asset_item : asset_item) =
      Format.fprintf fmt
        "%a : %a;"
        pp_id asset_item.name
        pp_type asset_item.type_
    in
    Format.fprintf fmt
      "type %a is record [@\n  \
       @[%a@]@\n\
       ]@\n"
      pp_id asset.name
      (pp_list "@\n" pp_asset_item) asset.values
  in

  let pp_decl env (fmt : Format.formatter) (decl : decl_node) =
    match decl with
    | Dvar v -> pp_var env fmt v
    | Denum e -> pp_enum fmt e
    | Dasset r -> pp_asset fmt r
    | Dcontract _c -> ()
  in

  let pp_decls env (fmt : Format.formatter) _ =
    (pp_list "@\n" (pp_decl env)) fmt model.decls
  in

  let pp_storage_item (fmt : Format.formatter) (si : storage_item) =
    Format.fprintf fmt
      "%a : %a;"
      pp_id si.id
      pp_type si.typ
  in

  let pp_storage (fmt : Format.formatter) _ =
    Format.fprintf fmt "type storage_type is ";
    match model.storage with
    | [] ->  Format.fprintf fmt "unit"
    | _ ->
      let l = List.filter (fun x -> not x.const) model.storage in
      Format.fprintf fmt
        "record [@\n  \
         @[%a@]@\n\
         ]@\n"
        (pp_list "@\n" pp_storage_item) l
  in

  let pp_default fmt = function
    | Tbuiltin Bbool       -> Format.fprintf fmt "False"
    | Tbuiltin Bint        -> Format.fprintf fmt "0"
    | Tbuiltin Brational   -> Format.fprintf fmt "0"
    | Tbuiltin Bdate       -> Format.fprintf fmt "0"
    | Tbuiltin Bduration   -> Format.fprintf fmt "0"
    | Tbuiltin Btimestamp  -> Format.fprintf fmt "(0 : timestamp)"
    | Tbuiltin Bstring     -> Format.fprintf fmt "\"\""
    | Tbuiltin Baddress    -> Format.fprintf fmt "(\"tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg\" : address) /* fake tmp address */"
    | Tbuiltin Brole       -> Format.fprintf fmt "(\"tz1Lc2qBKEWCBeDU8npG6zCeCqpmaegRi6Jg\" : address) /* fake tmp address */"
    | Tbuiltin Bcurrency   -> Format.fprintf fmt "0tz"
    | Tbuiltin Bkey        -> Format.fprintf fmt "0x00"
    | Tbuiltin Bbytes      -> Format.fprintf fmt "0x00"
    | _ -> assert false
  in

  let pp_api_asset (env : env) fmt = function
    | Get an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function get_%s (const s : storage_type; const key : %a) : %s is@\n  \
         begin@\n    \
         const res : %s = get_force(key, s.%s_assets);@\n  \
         end with (res)@\n"
        an pp_btyp t an an an

    | Set an ->
      let _, t = Utils.get_asset_key model an in
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
      let k, t = Utils.get_asset_key model an in
      (* let ft, c = Utils.get_field_container model an fn in *)
      Format.fprintf fmt
        "function add_%s (const s : storage_type; const a : %s) : storage_type is@\n  \
         begin@\n    \
         const key : %a = a.%s;@\n    \
         const map_local : map(%a, %s) = s.%s_assets;@\n    \
         if map_mem(key, map_local) then failwith (\"key already exists\") else skip;@\n    \
         map_local[key] := a;@\n    \
         s.%s_assets := map_local;@\n  \
         end with (s)@\n"
        an an
        pp_btyp t k
        pp_btyp t an an
        (* (pp_do_if (match c with | Partition -> true | _ -> false)
           (fun fmt -> Format.fprintf fmt "")) ft *)
        an

    | Remove an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function remove_%s (const s : storage_type; const key : %a) : storage_type is@\n  \
         begin@\n    \
         const map_local : map(%a, %s) = s.%s_assets;@\n    \
         remove key from map map_local;@\n    \
         s.%s_assets := map_local;@\n  \
         end with (s)@\n"
        an pp_btyp t
        pp_btyp t an an
        an

    | Clear an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function clear_%s (const s : storage_type) : storage_type is@\n  \
         begin@\n    \
         s.%s_assets := (map [] : map(%a, %s));@\n  \
         end with (s)@\n"
        an
        an pp_btyp t an

    | UpdateAdd (an, fn) ->
      let k, t = Utils.get_asset_key model an in
      let ft, c = Utils.get_field_container model an fn in
      let kk, _ = Utils.get_asset_key model ft in
      Format.fprintf fmt
        "function add_%s_%s (const s : storage_type; const a : %s; const b : %s) : storage_type is@\n  \
         begin@\n    \
         const asset_key : %a = a.%s;@\n    \
         const asset_val : %s = get_%s(s, asset_key);@\n    \
         const map_local : map(%a, %s) = s.%s_assets;@\n    \
         %a\
         asset_val.%s := cons(b.%s, asset_val.%s);@\n    \
         map_local[asset_key] := asset_val;@\n    \
         s.%s_assets := map_local;@\n    \
         %a  \
         end with (s)@\n"
        an fn an ft
        pp_btyp t k
        an an
        pp_btyp t an an
        (pp_do_if (match c with | Collection -> true | _ -> false)
             (fun fmt _ -> Format.fprintf fmt "if not map_mem(b.%s, s.%s_assets) then failwith (\"key of b does not exist\") else skip;@\n    " kk ft)) ()
        fn kk fn
        an
        (pp_do_if (match c with | Partition -> true | _ -> false) (fun fmt -> Format.fprintf fmt "s := add_%s(s, b);@\n")) ft

    | UpdateRemove (an, fn) ->
      let k, t = Utils.get_asset_key model an in
      let ft, c = Utils.get_field_container model an fn in
      let _kk, tt = Utils.get_asset_key model ft in
      Format.fprintf fmt
        "function remove_%s_%s (const s : storage_type; const a : %s; const key : %a) : storage_type is@\n  \
         begin@\n    \
         var new_keys : list(%a) := (nil : list(%a));@\n    \
         function aux (const i : %a) : unit is@\n      \
         begin@\n        \
         if (key =/= i) then@\n          \
         new_keys := cons(i, new_keys);@\n        \
         else@\n          \
         skip;@\n        \
         end with unit;@\n    \
         const asset_key : %a = a.%s;@\n    \
         const asset_val : %s = get_%s(s, asset_key);@\n    \
         list_iter(aux, asset_val.%s);@\n    \
         asset_val.%s := new_keys;@\n    \
         const map_local : map(%a, %s) = s.%s_assets;@\n    \
         map_local[asset_key] := asset_val;@\n    \
         s.%s_assets := map_local;@\n    \
         %a  \
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
        (pp_do_if (match c with | Partition -> true | _ -> false) (fun fmt -> Format.fprintf fmt "s := remove_%s(s, key);@\n")) ft

    | UpdateClear (an, fn) ->
      let k, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function clear_%s_%s (const s : storage_type; const a : %s) : storage_type is@\n  \
         begin@\n    \
         const asset_key : %a = a.%s;@\n    \
         const asset_val : %s = get_%s(s, asset_key);@\n    \
         %a\
         asset_val.%s := (list [] : list(%a));@\n    \
         const map_local : map(%a, %s) = s.my_asset_assets;@\n    \
         map_local[asset_key] := asset_val;@\n    \
         s.%s_assets := map_local;@\n    \
         end with (s)@\n"
        an fn an
        pp_btyp t k
        an an
        (fun fmt _ ->
           let anc, c = Utils.get_field_container model an fn in
           let _, kt = Utils.get_asset_key model anc in
           match c with
           | Partition ->
             Format.fprintf fmt
               "// partition@\n    \
                const ar : list(%a) = asset_val.%s;@\n    \
                const map_local_%s : map(%a, %s) = s.%s_assets;@\n    \
                function aux (const key : %a) : unit is block {remove key from map map_local_%s } with unit;@\n    \
                list_iter (aux, ar);@\n    \
                s.%s_assets := map_local_%s;@\n    \
                // end partition@\n    "
               pp_btyp t fn
               anc pp_btyp kt anc anc
               pp_btyp kt anc
               anc anc
           | _ -> ()) ()
        fn pp_btyp t
        pp_btyp t an
        an

    | ToKeys _an ->
      Format.fprintf fmt "// TODO api asset: ToKeys"
    (* "let[@inline] to_keys_%s (s : storage) : storage =@\n  \
       s (*TODO*)@\n"
       an *)

    | ColToKeys an ->
      let _k, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function col_to_keys_%s (const s : storage_type) : list(%a) is@\n \
         begin@\n \
         function to_keys (const accu : list(%a); const v : (%a * %s)) : list(%a) is block { skip } with cons(v.0, accu);@\n \
         function rev     (const accu : list(%a); const v : %a) : list(%a) is block { skip } with cons(v, accu);@\n \
         var res : list(%a) := (nil : list(%a));@\n \
         res := map_fold(to_keys, s.%s_assets, res);@\n \
         res := list_fold(rev, res, (nil : list(%a)));@\n \
         end with res@\n"
        an pp_btyp t
        pp_btyp t pp_btyp t an pp_btyp t
        pp_btyp t pp_btyp t pp_btyp t
        pp_btyp t pp_btyp t
        an
        pp_btyp t

    | Select (an, f) ->
      let k, t = Utils.get_asset_key model an in
      let i = get_preds_index env.select_preds f in
      Format.fprintf fmt
        "function select_%s_%i (const s : storage_type; const l : list(%a)) : list(%a) is@\n  \
         begin@\n    \
         var res : list(%a) := (nil : list(%a));@\n    \
         function aggregate (const i : %a) : unit is@\n      \
         begin@\n        \
         const the : %s = get_force(i, s.%s_assets);@\n        \
         if (%a) then@\n          \
         res := cons(the.%s, res);@\n        \
         else@\n          \
         skip;@\n      \
         end with unit;@\n    \
         list_iter(aggregate, l)@\n  \
         end with res@\n"
        an i pp_btyp t pp_btyp t
        pp_btyp t pp_btyp t
        pp_btyp t
        an an
        (pp_mterm (mk_env ())) f
        k

    | Sort (an, l) ->
      let _, t = Utils.get_asset_key model an in
      let pp_criteria fmt (fn, c) =
        let op1, op2, d =
          match c with
          | SKasc -> ">", "<", "asc"
          | SKdesc -> "<", ">", "desc"
        in
        Format.fprintf fmt
          "// %s %s@\n    \
           if (a1.%s %s a2.%s)@\n    \
           then res := 1@\n    \
           else if (a1.%s %s a2.%s)@\n    \
           then res := -1"
          fn d
          fn op1 fn
          fn op2 fn
      in

      let pp_fun_cmp fmt _ =
        Format.fprintf fmt
          "function cmp (const k1 : %a; const k2: %a) : int is@\n  \
           block {@\n    \
           var res : int := 0;@\n    \
           const a1 : %s = get_force(k1, s.%s_assets);@\n    \
           const a2 : %s = get_force(k2, s.%s_assets);@\n    \
           %a@\n    \
           else skip@\n  \
           } with res;@\n"
          pp_btyp t pp_btyp t
          an an
          an an
          (pp_list "@\n    else " pp_criteria) l
      in

      let pp_fun_insert fmt _ =
        Format.fprintf fmt
          "function insert(const accu: option(%a) * list(%a); const x : %a) : option(%a) * list(%a) is@\n  \
           block {@\n    \
           const res : option(%a) * list(%a) =@\n    \
           case accu.0 of@\n    \
           | Some(v) ->@\n    \
           if (cmp(x, v) < 0)@\n    \
           then ((None : option(%a)), cons(x, cons(v, accu.1)))@\n    \
           else (Some(v), cons(x, accu.1))@\n    \
           | None -> ((None : option(%a)), cons(x, accu.1))@\n    \
           end;@\n  \
           } with res;@\n"
          pp_btyp t pp_btyp t pp_btyp t pp_btyp t pp_btyp t
          pp_btyp t pp_btyp t
          pp_btyp t
          pp_btyp t
      in

      let pp_fun_sort fmt _ =
        Format.fprintf fmt
          "function sort (const accu: list(%a); const x: %a) : list(%a) is@\n  \
           block {@\n    \
           const init : option(%a) * list(%a) = (Some(x), (list [] : list(%a)));@\n    \
           const res_opt : option(%a) * list(%a) = list_fold(insert, accu, init);@\n    \
           const res : list(%a) =@\n    \
           case res_opt.0 of@\n    \
           | Some(v) -> cons(v, res_opt.1)@\n    \
           | None -> res_opt.1@\n    \
           end;@\n  \
           } with res;@\n"
          pp_btyp t pp_btyp t pp_btyp t
          pp_btyp t pp_btyp t pp_btyp t
          pp_btyp t pp_btyp t
          pp_btyp t
      in

      Format.fprintf fmt
        "function sort_%s_%a (const s : storage_type; const l : list(%a)) : list(%a) is@\n  @\n  \
         begin@\n    \
         @[%a@]@\n    \
         @[%a@]@\n    \
         @[%a@]@\n    \
         const init : list(%a) = list [];@\n    \
         const res : list(%a) = list_fold (sort, l, init);@\n  \
         end with res@\n"
        an pp_postfix_sort l pp_btyp t pp_btyp t
        pp_fun_cmp ()
        pp_fun_insert ()
        pp_fun_sort ()
        pp_btyp t
        pp_btyp t

    | Contains an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function contains_%s (const l : list(%a); const key : %a) : bool is@\n  \
         begin@\n    \
         var r : bool := False;@\n    \
         function aggregate (const i : %a) : unit is@\n      \
         begin@\n        \
         r := r or i = key;@\n      \
         end with unit;@\n    \
         list_iter(aggregate, l)@\n  \
         end with r@\n"
        an pp_btyp t pp_btyp t
        pp_btyp t

    | Nth an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function nth_%s (const s : storage_type; const l : list(%a); const index : int) : %s is@\n  \
         block {@\n    \
         function aggregate (const accu: int * option(%a); const x: %a) : int * option(%a) is@\n    \
         if accu.0 = index@\n    \
         then (accu.0 + 1, Some(x));@\n    \
         else (accu.0 + 1, accu.1);@\n    \
         const init : int * option(%a) = (0, (None : option(%a)));@\n    \
         const res_opt : int * option(%a) = list_fold (aggregate, l, init);@\n    \
         var key : %a := %a;@\n    \
         case res_opt.1 of@\n    \
         | Some(v) -> key := v@\n    \
         | None -> failwith(\"nth_%s failed\")@\n    \
         end;@\n    \
         const res : %s = get_force(key, s.%s_assets);@\n  \
         } with res@\n"
        an pp_btyp t an
        pp_btyp t pp_btyp t pp_btyp t
        pp_btyp t pp_btyp t
        pp_btyp t
        pp_btyp t pp_default (Tbuiltin t)
        an
        an an

    | Count an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function count_%s (const l : list(%a)) : int is@\n  \
         block { skip }@\n  \
         with int(size(l))@\n"
        an pp_btyp t

    | Sum (an, t, p) ->
      let rec pp_expr fmt (mt : mterm) =
        match mt.node with
        | Mdotasset ({node = Mvarlocal ({pldesc = "the"; _}) }, fn) ->
          Format.fprintf fmt "a.%a"
            pp_id fn
        | _ -> (pp_mterm_gen (mk_env ()) pp_expr) fmt mt
      in
      let get_zero = function
        | _ -> "0"
      in
      let _, tk = Utils.get_asset_key model an in
      let expr = p in
      let i = get_preds_index env.sum_preds p in
      Format.fprintf fmt
        "function sum_%s_%i (const s : storage_type; const l : list(%a)) : %a is@\n  \
         begin@\n    \
         var r : %a := %s;@\n    \
         function aggregate (const i : %a) : unit is@\n      \
         begin@\n        \
         const a : %s = get_force(i, s.%s_assets);@\n        \
         r := r + (%a);@\n      \
         end with unit;@\n    \
         list_iter(aggregate, l)@\n  \
         end with r@\n"
        an i pp_btyp tk pp_type t
        pp_type t (get_zero t)
        pp_btyp tk
        an an
        pp_expr expr

    | Min (an, fn) ->
      let _, t = Utils.get_asset_key model an in
      let _, ct, _ = Utils.get_asset_field model (an, fn) in
      Format.fprintf fmt
        "function min_%s_%s (const s : storage_type; const l : list(%a)) : %a is@\n  \
         block {@\n    \
         function aggregate (const accu: option(%a); const x: %a) : option(%a) is@\n    \
         block {@\n      \
         const a : %s = get_force(x, s.%s_assets);@\n      \
         const r : option(%a) = Some(a.%s);@\n      \
         case accu of@\n      \
         | Some(v) -> r := if v < a.%s then Some (v) else accu@\n      \
         | None -> skip@\n      \
         end;@\n    \
         } with r;@\n    \
         var init : option(%a) := (None : option(%a));@\n    \
         const res_opt : option(%a) = list_fold (aggregate, l, init);@\n    \
         var res : %a := %a;@\n    \
         case res_opt of@\n    \
         | Some(v) -> res := v@\n    \
         | None -> failwith(\"min_%s_%s failed\")@\n    \
         end;@\n  \
         } with res@\n"
        an fn pp_btyp t pp_type ct
        pp_type ct pp_btyp t pp_type ct
        an an
        pp_type ct fn
        fn
        pp_type ct pp_type ct
        pp_type ct
        pp_type ct pp_default ct
        an fn

    | Max (an, fn) ->
      let _, t = Utils.get_asset_key model an in
      let _, ct, _ = Utils.get_asset_field model (an, fn) in
      Format.fprintf fmt
        "function max_%s_%s (const s : storage_type; const l : list(%a)) : %a is@\n  \
         block {@\n    \
         function aggregate (const accu: option(%a); const x: %a) : option(%a) is@\n    \
         block {@\n      \
         const a : %s = get_force(x, s.%s_assets);@\n      \
         const r : option(%a) = Some(a.%s);@\n      \
         case accu of@\n      \
         | Some(v) -> r := if v > a.%s then Some (v) else accu@\n      \
         | None -> skip@\n      \
         end;@\n    \
         } with r;@\n    \
         var init : option(%a) := (None : option(%a));@\n    \
         const res_opt : option(%a) = list_fold (aggregate, l, init);@\n    \
         var res : %a := %a;@\n    \
         case res_opt of@\n    \
         | Some(v) -> res := v@\n    \
         | None -> failwith(\"max_%s_%s failed\")@\n    \
         end;@\n  \
         } with res@\n"
        an fn pp_btyp t pp_type ct
        pp_type ct pp_btyp t pp_type ct
        an an
        pp_type ct fn
        fn
        pp_type ct pp_type ct
        pp_type ct
        pp_type ct pp_default ct
        an fn

    | Shallow _ -> ()
    | Unshallow _ -> ()
    | Listtocoll _ -> ()
    | Head an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function head_%s (const l : list(%a); const i : int) : list(%a) is@\n  \
         block {@\n    \
         const length : int = int(size(l));@\n    \
         const bound : int = if i < length then i else length;@\n    \
         if (i < 0) then failwith(\"head_%s: index out of bound\") else skip;@\n    \
         function rev (const accu: list(%a); const x: %a) : list(%a) is x # accu;@\n    \
         function aggregate (const accu: int * list(%a); const x: %a) : int * list(%a) is@\n    \
         if accu.0 < bound@\n    \
         then (accu.0 + 1, x # accu.1 );@\n    \
         else (accu.0 + 1, accu.1 );@\n    \
         const init : int * list(%a) = (0, ((list [] : list(%a))));@\n    \
         const ltmp : int * list(%a) = list_fold (aggregate, l, init);@\n    \
         const res  : list(%a) = list_fold (rev, ltmp.1, ((list [] : list(%a))));@\n  \
         } with res@\n"
        an pp_btyp t pp_btyp t
        an
        pp_btyp t pp_btyp t pp_btyp t
        pp_btyp t pp_btyp t pp_btyp t
        pp_btyp t pp_btyp t
        pp_btyp t
        pp_btyp t pp_btyp t

    | Tail an ->
      let _, t = Utils.get_asset_key model an in
      Format.fprintf fmt
        "function tail_%s (const l : list(%a); const i : int) : list(%a) is@\n  \
         block {@\n    \
         const length : int = int(size(l));@\n    \
         const bound : int = if i < length then i else length;@\n    \
         if (i < 0) then failwith(\"tail_%s: index out of bound\") else skip;@\n    \
         const p : int = bound - i;@\n    \
         function rev (const accu: list(%a); const x: %a) : list(%a) is x # accu;@\n    \
         function aggregate (const accu: int * list(%a); const x: %a) : int * list(%a) is@\n    \
         if accu.0 >= p@\n    \
         then (accu.0 + 1, x # accu.1 );@\n    \
         else (accu.0 + 1, accu.1 );@\n    \
         const init : int * list(%a) = (0, ((list [] : list(%a))));@\n    \
         const ltmp : int * list(%a) = list_fold (aggregate, l, init);@\n    \
         const res  : list(%a) = list_fold (rev, ltmp.1, ((list [] : list(%a))));@\n  \
         } with res@\n"
        an pp_btyp t pp_btyp t
        an
        pp_btyp t pp_btyp t pp_btyp t
        pp_btyp t pp_btyp t pp_btyp t
        pp_btyp t pp_btyp t
        pp_btyp t
        pp_btyp t pp_btyp t
  in

  let pp_api_list (_env : env) fmt = function
    | Lprepend t  ->
      Format.fprintf fmt
        "function list_%a_prepend (const l : list(%a); const i : %a) : list(%a) is@\n  \
         block { skip }@\n  \
         with i # l@\n"
        pp_type t pp_type t pp_type t pp_type t

    | Lcontains t ->
      Format.fprintf fmt
        "function list_%a_contains (const l : list(%a); const item : %a) : bool is@\n  \
         block {@\n  \
         function aggregate (const accu: bool; const x: %a) : bool is (%a) or accu @\n  \
         } with list_fold (aggregate, l, False)@\n"
        pp_type t pp_type t pp_type t
        pp_type t
        (fun fmt t ->
           match t with
           | _ -> Format.fprintf fmt "item = x") t

    | Lcount t ->
      Format.fprintf fmt
        "function list_%a_count (const l : list(%a)) : int is@\n  \
         block { skip }@\n  \
         with int(size(l))@\n"
        pp_type t pp_type t

    | Lnth t ->
      Format.fprintf fmt
        "function list_%a_nth (const l : list(%a); const index : int) : %a is@\n  \
         block {@\n\
         function aggregate (const accu: int * option(%a); const x: %a) : int * option(%a) is@\n\
         if accu.0 = index@\n\
         then (accu.0 + 1, Some(x));@\n\
         else (accu.0 + 1, accu.1);@\n\
         const init : int * option(%a) = (0, (None : option(%a)));@\n\
         const res_opt : int * option(%a) = list_fold (aggregate, l, init);@\n\
         var res : %a := %a;@\n\
         case res_opt.1 of@\n\
         | Some(v) -> res := v@\n\
         | None -> failwith(\"list_%a_nth failed\")@\n\
         end@\n\
         } with res@\n"
        pp_type t pp_type t pp_type t
        pp_type t pp_type t pp_type t
        pp_type t pp_type t
        pp_type t
        pp_type t pp_default t
        pp_type t
  in

  let pp_api_builtin (_env : env) fmt = function
    | MinBuiltin t ->
      Format.fprintf fmt
        "function min_%a (const a : %a; const b : %a) : %a is if a < b then a else b@\n"
        pp_type t pp_type t pp_type t pp_type t

    | MaxBuiltin t ->
      Format.fprintf fmt
        "function max_%a (const a : %a; const b : %a) : %a is if a > b then a else b@\n"
        pp_type t pp_type t pp_type t pp_type t

    | AbsBuiltin t ->
      begin
        match t with
        | Tbuiltin Bnat -> ()
        | _ ->
          begin
            let pp_body fmt _ =
              match t with
              | Tbuiltin Bint -> Format.fprintf fmt "int(abs(a))"
              | Ttuple [Tbuiltin Bint; Tbuiltin Bint] -> Format.fprintf fmt "(int(abs(a.0)),int(abs(a.1)))"
              | _ -> assert false
            in
            Format.fprintf fmt
              "function abs_%a (const a : %a) : %a is %a@\n"
              pp_abs_type t pp_type t pp_type t pp_body ()
          end
      end
  in

  let pp_api_internal (_env : env) fmt = function
    | RatEq        ->
      Format.fprintf fmt
        "function rat_eq (const lhs : (int * int); const rhs : (int * int)) : bool is@\n  \
         block {skip} with@\n  \
         lhs.0 * rhs.1 = rhs.0 * lhs.1 @\n"
    | RatCmp       ->
      Format.fprintf fmt
        "type op_cmp is@\n\
         | OpCmpLt of unit@\n\
         | OpCmpLe of unit@\n\
         | OpCmpGt of unit@\n\
         | OpCmpGe of unit@\n\
         @\n\
         function rat_cmp (const op : op_cmp; const lhs : (int * int); const rhs : (int * int)) : bool is@\n  \
         begin@\n    \
         const a : int = lhs.0 * rhs.1;@\n    \
         const b : int = lhs.1 * rhs.0;@\n    \
         const pos : bool = lhs.1 * rhs.1 > 0;@\n    \
         var r : bool := False;@\n    \
         case op of@\n    \
         | OpCmpLt -> if pos then r := a <  b else r := a >  b@\n    \
         | OpCmpLe -> if pos then r := a <= b else r := a >= b@\n    \
         | OpCmpGt -> if pos then r := a >  b else r := a <  b@\n    \
         | OpCmpGe -> if pos then r := a >= b else r := a <= b@\n    \
         end@\n  \
         end with r@\n"
    | RatArith     ->
      Format.fprintf fmt
        "type op_arith is@\n\
         | OpArithPlus  of unit@\n\
         | OpArithMinus of unit@\n\
         | OpArithMult  of unit@\n\
         | OpArithDiv   of unit@\n\
         @\n\
         function rat_arith (const op : op_arith; const lhs : (int * int); const rhs : (int * int)) : (int * int) is@\n  \
         begin@\n    \
         const r : (int * int) =@\n    \
         case op of@\n    \
         | OpArithPlus  -> (lhs.0 * rhs.1 + rhs.0 * lhs.1, lhs.1 * rhs.1)@\n    \
         | OpArithMinus -> (lhs.0 * rhs.1 - rhs.0 * lhs.1, lhs.1 * rhs.1)@\n    \
         | OpArithMult  -> (lhs.0 * rhs.0, lhs.1 * rhs.1)@\n    \
         | OpArithDiv   -> (lhs.0 * rhs.1, lhs.1 * rhs.0)@\n    \
         end@\n  \
         end with r@\n"
    | RatUminus ->
      Format.fprintf fmt
        "function rat_uminus (const x : (int * int)) : (int * int) is (- x.0, x.1)@\n"
    | RatTez ->
      Format.fprintf fmt
        "function rat_tez (const c : (int * int); const t : tez) : tez is@\n\
         begin@\n  \
         const r : tez = abs(c.0) * t / abs(c.1);@\n  \
         end with r@\n"
    | StrConcat ->
      Format.fprintf fmt "function str_concat (const a : string; const b : string) : string is a ^ b@\n"
  in

  let pp_api_item_node (env : env) fmt = function
    | APIAsset      v -> pp_api_asset    env fmt v
    | APIList       v -> pp_api_list     env fmt v
    | APIBuiltin    v -> pp_api_builtin  env fmt v
    | APIInternal   v -> pp_api_internal env fmt v
  in

  let pp_api_item (env : env) fmt (api_storage : api_storage) =
    pp_api_item_node env fmt api_storage.node_item
  in

  let pp_api_items (env : env) fmt _ =
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
      Format.fprintf fmt "(* API function *)@\n@\n%a@\n"
        (pp_list "@\n" (pp_api_item env)) l
  in

  let pp_function (env : env) (fmt : Format.formatter) (f : function__) =
    let env = {env with f = Some f} in
    match f.node with
    | Entry fs ->
      let with_operations = Utils.with_operations_for_mterm fs.body in
      Format.fprintf fmt
        "function %a(const action : action_%a; const %s : storage_type) : (list(operation) * storage_type) is@\n  \
         begin@\n  \
         %a\
         @[%a@]@\n  \
         end with (%s, %s)@\n"
        pp_id fs.name pp_id fs.name const_storage
        (pp_do_if with_operations (fun fmt x ->
             Format.fprintf fmt "var %s : list(operation) := (nil : list(operation));@\n  " x
           )) const_operations
        (pp_mterm env) fs.body
        (if with_operations then const_operations else "(nil : list(operation))") const_storage

    | Function (fs, _ret) ->
      Format.fprintf fmt
        "function %a(const %s : storage_type%a) : storage_type is@\n  \
         begin@\n  \
         @[%a@]@\n  \
         end with (%s)@\n"
        pp_id fs.name
        const_storage
        (pp_list "" (fun fmt (id, type_, _ : lident * type_ * 'a) ->
             Format.fprintf fmt
               "; const %a : %a"
               pp_id id
               pp_type type_
           )) fs.args
        (pp_mterm env) fs.body
        const_storage
  in

  let pp_functions (env : env) (fmt : Format.formatter) _ =
    (pp_list "@\n" (pp_function env)) fmt model.functions
  in

  let pp_main_function (fmt : Format.formatter) _ =
    let actions = LigoUtils.get_actions model in
    let arg = "a_" in
    Format.fprintf fmt
      "function main(const action : action ; const %s : storage_type) : (list(operation) * storage_type) is@\n  \
       block {skip} with@\n  \
       case action of@\n  \
       @[%a@]@\n  \
       end@\n"
      const_storage
      (pp_list "@\n"
         (fun fmt action -> Format.fprintf fmt "| %s (%s) -> %s(%s, %s)"
             action.name
             arg
             action.fun_name
             arg
             const_storage
         )) actions
  in

  let compute_env _ =
    let select_preds =
      List.fold_right (fun x accu ->
          match x.api_loc, x.node_item with
          | (OnlyExec | ExecFormula), APIAsset (Select (_, pred)) ->
            if not (List.exists (Model.cmp_mterm pred) accu)
            then pred::accu
            else accu
          | _ -> accu
        ) model.api_items []
    in
    let sum_preds =
      List.fold_right (fun x accu ->
          match x.api_loc, x.node_item with
          | (OnlyExec | ExecFormula), APIAsset (Sum (_, _, pred)) ->
            if not (List.exists (Model.cmp_mterm pred) accu)
            then pred::accu
            else accu
          | _ -> accu
        ) model.api_items []
    in
    let consts =
      List.fold_right (fun (x : decl_node) accu ->
          match x with
          | Dvar v when v.constant -> (unloc v.name, Option.get v.default)::accu
          | _ -> accu
        ) model.decls [] in
    mk_env ~select_preds:select_preds ~sum_preds:sum_preds ~consts:consts ()
  in

  let pp_storage_term env fmt _ =
    match model.storage with
    | []  -> pp_str fmt "Unit"
    | l   ->
      let l = List.filter (fun (x : storage_item) -> not x.const) l in
      Format.fprintf fmt "record %a end"
        (pp_list "; " (fun fmt (si : storage_item) ->
             Format.fprintf fmt "%a = %a" pp_id si.id (pp_mterm env) si.default )
        ) l
  in

  let pp_ligo_disclaimer env fmt _ =
    Format.fprintf fmt
      "// To generate origination storage string please execute the following command:@\n\
       // ligo compile-storage %a.ligo main '%a'@\n"
      pp_id model.name
      (pp_storage_term env) ()
  in

  let env = compute_env () in
  match b with
  | true ->
    Format.fprintf fmt "// LIGO output generated by %a@\n@\n\
                        %a@\n\
                        %a@\n\
                        %a@\n\
                        %a@\n\
                        %a@\n\
                        %a@\n\
                        %a@\n\
                        %a@\n\
                        @."
      pp_bin ()
      pp_model_name ()
      (pp_ligo_disclaimer env) ()
      (pp_decls env) ()
      pp_storage ()
      pp_action_type ()
      (pp_api_items env) ()
      (pp_functions env) ()
      pp_main_function ()
  | false ->
    pp_storage_term env fmt ()

let pp_model fmt (model : model) =
  pp_model_internal fmt model true

let pp_storage fmt (model : model) =
  pp_model_internal fmt model false

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : model) = string_of__of_pp pp_model x
