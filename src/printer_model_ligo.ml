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
  consts: (ident * mterm) list;
  vars: (ident * mterm) list;
}

let mk_env ?f ?(select_preds=[]) ?(consts=[]) ?(vars=[]) () : env =
  { f; select_preds; consts; vars }

exception Found

let is_internal l (id : lident) : bool =
  try
    List.iter (fun (x : ident * mterm) -> if (String.equal (unloc id) (fst x)) then raise Found) l ;
    false
  with
  | Found -> true

let is_const (env : env) (id : lident) : bool = is_internal env.consts id
let is_var_with_init (env : env) (id : lident) : bool = is_internal env.vars id


let get_const_dv (env : env) (id : lident) : mterm =
  List.assoc (unloc id) env.consts

let get_var_dv (env : env) (id : lident) : mterm =
  List.assoc (unloc id) env.vars

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
      Format.fprintf fmt "int"
    | Tenum en ->
      Format.fprintf fmt "%a" pp_id en
    | Tcontract _ -> pp_type fmt (Tbuiltin Baddress)
    | Tbuiltin b -> pp_btyp fmt b
    | Tcontainer (t, c) ->
      Format.fprintf fmt "%a(%a)"
        pp_container c
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
              | PlusAssign  -> Format.fprintf fmt "%a + %a" pp_id lhs f r
              | MinusAssign -> Format.fprintf fmt "%a - %a" pp_id lhs f r
              | MultAssign  -> Format.fprintf fmt "%a * %a" pp_id lhs f r
              | DivAssign   -> Format.fprintf fmt "%a / %a" pp_id lhs f r
              | AndAssign   -> Format.fprintf fmt "%a and %a" pp_id lhs f r
              | OrAssign    -> Format.fprintf fmt "%a or %a" pp_id lhs f r
          ) r

      | Massignvarstore (op, _, lhs, r) ->
        Format.fprintf fmt "%s.%a := %a"
          const_storage
          pp_id lhs
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

      | Mseq is ->
        Format.fprintf fmt "@[%a@]"
          (pp_list ";@\n" f) is

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

      | Mnone -> pp_str fmt "None"

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
          Format.fprintf fmt "int(%a mod %a)"
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
          Format.fprintf fmt "clear_%a (self)"
            pp_str an
        in
        pp fmt (an)

      | Mclearfield (an, fn) ->
        let pp fmt (an, fn) =
          Format.fprintf fmt "clear_%a_%a (self)"
            pp_str an
            pp_str fn
        in
        pp fmt (an, fn)

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

      | Mgetfrommap (an, k, m) ->
        let pp fmt (_an, k, m) =
          Format.fprintf fmt "get_force(%a, %a)"
            f k
            f m
        in
        pp fmt (an, k, m)


      (* list api effect *)

      | Mlistprepend (c, a) ->
        Format.fprintf fmt "list_prepend (%a, %a)"
          f c
          f a


      (* list api expression *)

      | Mlistcontains (c, a) ->
        Format.fprintf fmt "list_contains (%a, %a)"
          f c
          f a

      | Mlistcount c ->
        Format.fprintf fmt "list_count (%a)"
          f c

      | Mlistnth (c, a) ->
        Format.fprintf fmt "list_nth (%a, %a)"
          f c
          f a


      (* builtin functions *)

      | Mfunmax (l, r) ->
        Format.fprintf fmt "max (%a, %a)"
          f l
          f r

      | Mfunmin (l, r) ->
        Format.fprintf fmt "min (%a, %a)"
          f l
          f r

      | Mfunabs a ->
        Format.fprintf fmt "abs (%a)"
          f a


      (* constants *)

      | Mvarstate      -> Format.fprintf fmt "%s.%s" const_storage const_state
      | Mnow           -> pp_str fmt "now"
      | Mtransferred   -> pp_str fmt "amount"
      | Mcaller        -> pp_str fmt "sender"
      | Mbalance       -> pp_str fmt "balance"
      | Msource        -> pp_str fmt "source"


      (* variables *)

      | Mvarstorevar v ->
        if (is_const env v)
        then pp_id fmt v
        else Format.fprintf fmt "%s.%a" const_storage pp_id v

      | Mvarstorecol v -> Format.fprintf fmt "%s.%a" const_storage pp_id v

      | Mvarenumval v  -> pp_id fmt v

      | Mvarlocal v    ->
        begin
          match mtt.type_ with
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
      | Mapifsubsetof  _ -> emit_error (UnsupportedTerm ("subsetof"))
      | Mapifisempty   _ -> emit_error (UnsupportedTerm ("isempty"))

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

    | ToKeys _an ->
      Format.fprintf fmt "// TODO api storage: ToKeys"
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

    | Sort (_an, _fn) ->
      Format.fprintf fmt "// TODO api storage: Sort"
    (* "let[@inline] sort_%s_%s (s : storage) : unit =@\n  \
       () (*TODO*)@\n"
       an fn *)

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

    | Nth _an ->
      (* let _, t = Utils.get_asset_key model an in *)
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
      (* let _, t = Utils.get_asset_key model an in *)
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

      let _, tk = Utils.get_asset_key model an in
      let _, t, _ = Utils.get_asset_field model (an, fn) in
      Format.fprintf fmt
        "function sum_%s_%s (const s : storage_type; const l : list(%a)) : %a is@\n  \
         begin@\n    \
         var r : %a := %s;@\n    \
         function aggregate (const i : %a) : unit is@\n      \
         begin@\n        \
         const a : %s = get_force(i, s.%s_assets);@\n        \
         r := r + a.%s;@\n      \
         end with unit;@\n    \
         list_iter(aggregate, l)@\n  \
         end with r@\n"
        an fn pp_btyp tk pp_type t
        pp_type t (get_zero t)
        pp_btyp tk
        an an
        fn

    | Min (_an, _fn) ->
      (* let _, tk = Utils.get_asset_key model an in
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
      (* let _, tk = Utils.get_asset_key model an in
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

  let pp_api_list (_env : env) fmt = function
    | Lprepend t  -> Format.fprintf fmt "list_prepend\t %a" pp_type t
    | Lcontains t -> Format.fprintf fmt "list_contains\t %a" pp_type t
    | Lcount t    -> Format.fprintf fmt "list_count\t %a" pp_type t
    | Lnth t      -> Format.fprintf fmt "list_nth\t %a" pp_type t
  in

  let pp_api_builtin (_env : env) fmt = function
    | MinBuiltin t -> Format.fprintf fmt "min on %a" pp_type t
    | MaxBuiltin t -> Format.fprintf fmt "max on %a" pp_type t
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
         | OpCmpLt(unit) -> if pos then r := a <  b else r := a >  b@\n    \
         | OpCmpLe(unit) -> if pos then r := a <= b else r := a >= b@\n    \
         | OpCmpGt(unit) -> if pos then r := a >  b else r := a <  b@\n    \
         | OpCmpGe(unit) -> if pos then r := a >= b else r := a <= b@\n    \
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
         | OpArithPlus(unit)  -> (lhs.0 * rhs.1 + rhs.0 * lhs.1, lhs.1 * rhs.1)@\n    \
         | OpArithMinus(unit) -> (lhs.0 * rhs.1 - rhs.0 * lhs.1, lhs.1 * rhs.1)@\n    \
         | OpArithMult(unit)  -> (lhs.0 * rhs.0, lhs.1 * rhs.1)@\n    \
         | OpArithDiv(unit)   -> (lhs.0 * rhs.1, lhs.1 * rhs.0)@\n    \
         end@\n  \
         end with r@\n"
    | RatTez ->
      Format.fprintf fmt
        "function rat_tez (const c : (int * int); const t : tez) : tez is@\n\
         begin@\n  \
         if (c.0 * c.1 < 0) then failwith(\"c must be positive\") else skip;@\n  \
         const r : tez = abs(c.0) * t / abs(c.1);@\n  \
         end with r@\n"
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
          if x.only_formula
          then accu
          else x::accu
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
          match x.only_formula, x.node_item with
          | false, APIAsset (Select (_, pred)) ->
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
    let vars =
      List.fold_right (fun (x : decl_node) accu ->
          match x with
          | Dvar v when Option.is_some v.default -> (unloc v.name, Option.get v.default)::accu
          | _ -> accu
        ) model.decls [] in
    mk_env ~select_preds:select_preds ~consts:consts ~vars:vars ()
  in

  let pp_storage_term env fmt _ =
    match model.storage with
    | []  -> pp_str fmt "Unit"
    | l   ->
      let l = List.filter (fun (x : storage_item) -> not x.const) l in
      Format.fprintf fmt "record %a end"
        (pp_list "; " (fun fmt (si : storage_item) ->
             let map_const_value : (ident * mterm) list = env.consts @ env.vars in
             let default : mterm = si.default in
             let dmt : mterm = Model.Utils.eval default map_const_value in
             Format.fprintf fmt "%a = %a" pp_id si.id (pp_mterm env) dmt )
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
