open Location
open Ident
open Tools
open Model
open Printer_tools
open Printer_model_tools

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
let const_result = "r_"

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

type action = {
  name : ident;
  fun_name: ident;
  args: (ident * type_) list
}

module LigoUtils : sig
  val get_actions : model -> action list
  val is_param : action -> ident -> bool
end = struct
  let mk_action_fs (fs : function_struct) =
    let fun_name = fs.name |> unloc in
    let id = fs.name |> unloc |> String.up_firstcase_lower in
    let args = List.map
        (fun (id, t, _) ->
           (unloc id,
            match t with
            | Tenum _ -> Tbuiltin Bint
            | _ -> t)) fs.args
    in
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

end

let pp_model_internal fmt (model : model) b =

  let pp_model_name (fmt : Format.formatter) _ =
    Format.fprintf fmt "// contract: %a@\n"
      pp_id model.name
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
    | Bcurrency   -> Format.fprintf fmt "tez"
    | Bsignature  -> Format.fprintf fmt "signature"
    | Bkey        -> Format.fprintf fmt "key"
    | Bkeyhash    -> Format.fprintf fmt "key_hash"
    | Bbytes      -> Format.fprintf fmt "bytes"
    | Bnat        -> Format.fprintf fmt "nat"
    | Bchainid    -> Format.fprintf fmt "chain_id"
  in

  let pp_container fmt = function
    | Collection -> Format.fprintf fmt "list"
    | Aggregate     -> Format.fprintf fmt "set"
    | Partition  -> Format.fprintf fmt "set"
    | View       -> Format.fprintf fmt "list"
  in

  let pp_pretty_container fmt = function
    | Collection -> Format.fprintf fmt "collection"
    | Aggregate     -> Format.fprintf fmt "Aggregate"
    | Partition  -> Format.fprintf fmt "partition"
    | View       -> Format.fprintf fmt "view"
  in

  let rec pp_type fmt t =
    match t with
    | Tasset an ->
      Format.fprintf fmt "%a" pp_id an
    | Tstate ->
      Format.fprintf fmt "int"
    | Tenum en ->
      Format.fprintf fmt "%a" pp_id en
    | Tbuiltin b -> pp_btyp fmt b
    | Tcontainer (Tasset an, (Aggregate | Partition)) -> pp_type fmt (Tset ((Utils.get_asset_key model (unloc an) |> snd)))
    | Tcontainer (Tasset an, _) -> pp_type fmt (Tlist ((Utils.get_asset_key model (unloc an) |> snd)))
    | Tlist (Tasset an) -> pp_type fmt (Tlist ((Utils.get_asset_key model (unloc an) |> snd)))
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
    | Tset k ->
      Format.fprintf fmt "set(%a)"
        pp_type k
    | Tmap (true, k, v) ->
      Format.fprintf fmt "big_map(%a, %a)"
        pp_type k
        pp_type v
    | Tmap (false, k, v) ->
      Format.fprintf fmt "map(%a, %a)"
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
      Format.fprintf fmt "contract(%a)" pp_type t
    | Tprog _
    | Tvset _
    | Ttrace _ -> Format.fprintf fmt "todo"
  in
  (* const operations : list(operation) = nil ; *)

  let rec pp_pretty_type fmt t =
    match t with
    | Ttuple[Tbuiltin Bint; Tbuiltin Bint] -> pp_type fmt (Tbuiltin Brational)
    | Tlist t            -> Format.fprintf fmt "list_%a" pp_pretty_type t
    | Tcontainer (t, c)  -> Format.fprintf fmt "container_%a_%a" pp_pretty_type t pp_pretty_container c
    | Toption t          -> Format.fprintf fmt "option_%a" pp_pretty_type t
    | Ttuple l           -> Format.fprintf fmt "tuple_%a" (pp_list "_" pp_pretty_type) l
    | Tset t             -> Format.fprintf fmt "set_%a" pp_pretty_type t
    | Tmap (true,  k, v) -> Format.fprintf fmt "bigmap_%a_%a" pp_pretty_type k pp_pretty_type v
    | Tmap (false, k, v) -> Format.fprintf fmt "map_%a_%a" pp_pretty_type k pp_pretty_type v
    | Tcontract t        -> Format.fprintf fmt "contract_%a" pp_pretty_type t
    | _ -> pp_type fmt t
  in

  let extract_option_type = function | Toption x -> x | _ -> assert false in

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

  let pp_prefix_container_kind an fmt = function
    | CKcoll _  -> Format.fprintf fmt "c_%s" an
    | CKview _  -> Format.fprintf fmt "v_%s" an
    | CKfield (an, fn, _, _, _) -> Format.fprintf fmt "f_%s_%s" an fn
    | CKdef _   -> assert false (* no definition in execution *)
  in

  let pp_mterm_gen (env : env) f fmt (mtt : mterm) =
    let pp_mterm_block fmt (x : mterm) =
      match x with
      | { node = Mseq l; _} when List.length l >= 2 ->
        Format.fprintf fmt " block {@\n  @[%a@] }"
          (pp_list ";@\n" f) l
      | { node = Mseq ([{node = Mletin _}] as l); _} ->
        Format.fprintf fmt " block {@\n  @[%a@] }"
          (pp_list ";@\n" f) l
      | { node = Mletin _; _} as a ->
        Format.fprintf fmt " block {@\n   @[%a@]@\n}"
          f a
      | { node = (Mupdate _); _} as a ->
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
        let args = (mk_mterm (Mvar (dumloc const_storage, Vlocal, Tnone, Dnone)) Tstorage)::args in
        Format.fprintf fmt "%a (%a)"
          pp_id e
          (pp_list ", " f) args
      in
      pp fmt (e, args)


    (* assign *)

    | Massign (op, _, Avar lhs, r) ->
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

    | Massign (op, _, Avarstore lhs, r) ->
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
      Format.fprintf fmt "%s.%s := %a"
        const_storage
        const_state
        f x

    | Massign (_op, _, Aassetstate (an, k), v) ->
      Format.fprintf fmt "state_%a(%a) = %a"
        pp_ident an
        f k
        f v

    | Massign (_op, _, Aoperations, v) ->
      Format.fprintf fmt "%s := %a"
        const_operations
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

    | Mfor (id, col, body, _) ->
      let postvar = function
        | ICKcoll an when not (Model.Utils.is_asset_single_field model an) -> " -> value_"
        | _ -> ""
      in
      let pp_iter_container_kind f fmt = function
        | ICKcoll an ->
          let is_asset_single_field = Model.Utils.is_asset_single_field model an in
          Format.fprintf fmt "%s (%s.%s)"
            (if is_asset_single_field then "set" else "map")
            const_storage
            an
        | ICKview  mt         -> Format.fprintf fmt "list (%a)" f mt
        | ICKfield (_, _, mt) -> Format.fprintf fmt "set (%a)"  f mt
        | ICKset   mt         -> Format.fprintf fmt "set (%a)"  f mt
        | ICKlist  mt         -> Format.fprintf fmt "list (%a)" f mt
        | ICKmap   mt         -> Format.fprintf fmt "map (%a)"  f mt
      in

      Format.fprintf fmt
        "for %a%s in %a block {@\n  @[%a@] }@\n"
        (fun fmt i -> match i with FIsimple x -> pp_id fmt x | FIdouble (x, y) -> Format.fprintf fmt "%a -> %a" pp_id x pp_id y) id
        (postvar col)
        (pp_iter_container_kind f) col
        f body

    (* | Mfor (_, _, _, None) -> assert false
       | Mfor (id, col, body, Some label) -> *)
    (* let typ =
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
       label f col *)

    | Miter (i, a, b, c, _) ->
      Format.fprintf fmt
        "for %a := %a to (%a) block {@\n  @[%a@] }@\n"
        pp_id i
        f a
        f b
        f c

    | Mwhile (c, b, _) ->
      Format.fprintf fmt "while (%a) block{@\n  @[%a@]@\n}"
        f c
        f b

    | Mseq is ->
      begin
        match is with
        | [] -> Format.fprintf fmt "skip"
        | _ ->
          Format.fprintf fmt "@[%a@]"
            (pp_list ";@\n" f) is
      end

    | Mreturn x ->
      Format.fprintf fmt "const %s : %a = %a;"
        const_result
        pp_type x.type_
        f x

    | Mlabel _ -> ()
    | Mmark  _ -> ()


    (* effect *)

    | Mfail (Invalid mt) when Utils.is_not_string_nat_int mt.type_ ->
      let t = mt.type_ in
      Format.fprintf fmt "if True then fail_%a (%a) else skip;"
        (* pp_type t *)
        pp_pretty_type t
        f mt

    | Mfail ft ->
      Format.fprintf fmt "failwith (%a)"
        (pp_fail_type f) ft

    | Mtransfer (v, k) -> begin
        match k with
        | TKsimple d ->
          Format.fprintf fmt "%s := cons(transaction(unit, %a, (get_contract(%a) : contract(unit))), %s)"
            const_operations
            f v
            f d
            const_operations
        | TKcall (id, t, d, a) ->
          let pp_entrypoint fmt _ =
            Format.fprintf fmt "case (Tezos.get_entrypoint_opt(\"%%%s\", %a) : option(contract(%a))) of Some (v) -> v | None -> (failwith(\"error self\") : contract(%a)) end"
              id f d pp_type t pp_type t
          in
          Format.fprintf fmt "%s := cons(transaction(%a, %a, %a), %s)"
            const_operations
            f a
            f v
            pp_entrypoint ()
            const_operations

        | TKentry (e, a) ->
          Format.fprintf fmt "%s := cons(transaction(%a, %a, %a), %s)"
            const_operations
            f a
            f v
            f e
            const_operations

        | TKself (id, args) ->
          let pp_entrypoint fmt _ =
            Format.fprintf fmt "case (Tezos.get_entrypoint_opt(\"%%%s\", Tezos.self_address) : option(contract(action_%s))) of Some (v) -> v | None -> (failwith(\"error self\") : contract(action_%s)) end"
              id id id
          in
          Format.fprintf fmt
            "%s := cons(transaction((%a : action_%s), %a, %a), %s)@\n"
            const_operations
            (fun fmt l ->
               match l with
               | [] -> pp_str fmt "unit"
               | _  -> Format.fprintf fmt "record %a end" (pp_list "; " (fun fmt (id, v) -> Format.fprintf fmt "%s = %a" id f v)) l) args
            id
            f v pp_entrypoint () const_operations
      end


    (* entrypoint *)

    | Mentrypoint (_, a, s) ->
      Format.fprintf fmt "Tezos.get_entrypoint_opt(\"%a\", %a)"
        pp_id a
        f s

    | Mself id ->
      let t = mtt.type_ in
      Format.fprintf fmt "case (Tezos.get_entrypoint_opt(\"%%%a\", Tezos.self_address) : option(%a)) of Some (v) -> v | None -> (failwith(\"error self\") : %a) end"
        pp_id id
        pp_type t
        pp_type t


    (* operation *)

    | Moperations -> pp_str fmt const_operations

    | Mmkoperation (v, d, a) ->
      Format.fprintf fmt "Tezos.transaction(%a, %a, %a)"
        f a
        f v
        f d

    (* literals *)

    | Mint v -> pp_big_int fmt v
    | Mnat v -> Format.fprintf fmt "%an" pp_big_int v
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
    | Munit -> Format.fprintf fmt "unit"


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
               Format.fprintf fmt "| %a -> (@\n  @[%a@]@\n)"
                 pp_pattern p
                 f x
             )) l
      in
      pp fmt (e, l)

    | Mmatchsome _ -> emit_error (UnsupportedTerm ("Mmatchsome"))

    (* composite type constructors *)

    | Mnone -> Format.fprintf fmt "(None : %a)" pp_type mtt.type_

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
      Format.fprintf fmt "(record %a end : %s)"
        (pp_list " " (fun fmt (a, b)->
             Format.fprintf fmt "%a = %a;"
               pp_id a
               f b)) lll
        (unloc asset_name)

    | Massets l ->
      begin
        match l, mtt.type_ with
        | [], Tmap (_b, k , v) -> Format.fprintf fmt "(map end : map(%a, %a))" pp_type k pp_type v
        | _, Tmap (_b, k , v) -> Format.fprintf fmt "(map %a end : map(%a, %a))" (pp_list "; " f) l pp_type k pp_type v
        | [], _ -> Format.fprintf fmt "(set[] : %a)" pp_type mtt.type_
        | _, _ -> Format.fprintf fmt "set@\n  @[%a@]@\nend"
                    (pp_list "@\n" (fun fmt -> Format.fprintf fmt "%a;" f)) l
      end

    | Mlitset l ->
      Format.fprintf fmt "(set [%a] : %a)"
        (pp_list "; " f) l
        pp_type mtt.type_

    | Mlitlist l ->
      begin
        match l with
        | [] -> Format.fprintf fmt "(nil : %a)" pp_type mtt.type_
        | _ ->
          Format.fprintf fmt "(list [%a])"
            (pp_list "; " f) l
      end

    | Mlitmap l -> begin
        match mtt.type_ with
        | Tmap (true, _, _) ->
          Format.fprintf fmt "(Big_map.literal (list [%a]) : %a)"
            (pp_list "; " (fun fmt (k, v) -> Format.fprintf fmt "(%a, %a)"
                              f k
                              f v)) l
            pp_type mtt.type_
        | _ ->
          Format.fprintf fmt "(map [%a] : %a)"
            (pp_list "; " (fun fmt (k, v) -> Format.fprintf fmt "%a -> %a"
                              f k
                              f v)) l
            pp_type mtt.type_
      end

    | Mlitrecord l ->
      Format.fprintf fmt "(record[%a] : %a)"
        (pp_list "; " (fun fmt (k, v) -> Format.fprintf fmt "%s = %a"
                          k
                          f v)) l
        pp_type mtt.type_

    (* access *)

    | Mdot (e, i) ->
      Format.fprintf fmt "%a.%a"
        f e
        pp_id i

    | Mdotassetfield _ -> emit_error (UnsupportedTerm ("Mdotassetfield"))


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

    | Mxor (l, r) ->
      let pp fmt (l, r) =
        Format.fprintf fmt "Bitwise.xor(%a, %a)"
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

    | Mdivrat _ -> emit_error (UnsupportedTerm ("divrat"))

    | Mdiveuc (l, r) ->
      let pp fmt (l, r : mterm * mterm) =
        Format.fprintf fmt "(%a) / (%a)"
          (pp_cast Lhs l.type_ r.type_ f) l
          (pp_cast Rhs l.type_ r.type_ f) r
      in
      pp fmt (l, r)

    | Mmodulo (l, r) ->
      let pp fmt (l, r : mterm * mterm) =
        Format.fprintf fmt "(%a) mod (%a)"
          (pp_cast Lhs l.type_ r.type_ f) l
          (pp_cast Rhs l.type_ r.type_ f) r
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
      let pp fmt (an, i) =
        Format.fprintf fmt "%s := remove_%a (%s, %a)"
          const_storage
          pp_str an
          const_storage
          f i
      in
      pp fmt (an, i)

    | Mremoveall (an, fn, a) ->
      let pp fmt (an, fn, a) =
        Format.fprintf fmt "%s := removeall_%a_%a (%s, %a)"
          const_storage
          pp_str an
          pp_str fn
          const_storage
          f a
      in
      pp fmt (an, fn, a)

    | Mremovefield (an, fn, c, i) ->
      let pp fmt (an, fn, c, i) =
        Format.fprintf fmt "%s := remove_%a_%a (%s, %a, %a)"
          const_storage
          pp_str an
          pp_str fn
          const_storage
          f c
          f i
      in
      pp fmt (an, fn, c, i)

    | Mremoveif (an, c, la, lb, a) ->
      let index : int = get_preds_index env.removeif_preds lb in
      let pp fmt (an, c, _la, _lb, a) =
        let pp fmt _ =
          match c with
          | CKcoll _          -> pp_str fmt const_storage
          | CKview _          -> assert false
          | CKfield (_, _, k, _, _) -> Format.fprintf fmt "%s, %a" const_storage f k
          | CKdef _           -> assert false
        in
        Format.fprintf fmt "%s := removeif_%a_%i (%a%a)"
          const_storage
          (pp_prefix_container_kind an) c index
          pp ()
          (pp_list "" (pp_prefix ", " f)) a
      in
      pp fmt (an, c, la, lb, a)

    | Mclear (an, v) ->
      let pp fmt (an, v) =
        let pp_arg fmt _ =
          match v with
          | CKcoll _                -> Format.fprintf fmt "%s" const_storage
          | CKview c                -> Format.fprintf fmt "%s, %a" const_storage f c
          | CKfield (_, _, c, _, _) -> Format.fprintf fmt "%s, %a" const_storage f c
          | CKdef _                 -> assert false
        in
        Format.fprintf fmt "%s := clear_%a (%a)"
          const_storage
          (pp_prefix_container_kind an) v
          pp_arg ()
      in
      pp fmt (an, v)

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

    | Mupdate (an, k, l)   ->
      (* let index : int = get_preds_index_gen Printer_model_tools.cmp_update env.update_preds (List.map (fun (x, y, z) -> (unloc x, y, z)) l) in *)
      let compute_ (k : mterm) : mterm =
        begin
          match k.node with
          | Mget (_, _, k) -> k
          | _ -> k
        end
      in
      let _, t = Utils.get_asset_key model an in
      let pp fmt (an, k, l) =
        let asset_key = "key_" ^ an ^ "_" in
        let asset_val = an ^ "_" in
        Format.fprintf fmt
          "const %s : %a = %a;@\n\
           const %s : %s_storage = get_force(%s, %s.%s);@\n\
           %s.%s[%s] := %s%a"
          asset_key pp_type t f (compute_ k)
          asset_val an asset_key const_storage an
          const_storage an asset_key asset_val
          (fun fmt _ ->
             match l with
             | [] -> ()
             | _ ->
               Format.fprintf fmt " with record [%a]"
                 (pp_list "; " (fun fmt (id, op, v) ->
                      let id = unloc id in
                      let (_, type_, _) = Utils.get_asset_field model (an, id) in
                      Format.fprintf fmt "%s = %a" id
                        (fun fmt _ ->
                           match op, type_ with
                           | ValueAssign, _ -> f fmt v
                           | PlusAssign , _ -> Format.fprintf fmt "%s.%s + (%a)"   asset_val id f v
                           | MinusAssign, Tbuiltin Bnat -> Format.fprintf fmt "(if (%s.%s - (%a)) >= 0 then abs(%s.%s - (%a)) else (failwith (%a) : nat))"   asset_val id f v asset_val id f v (pp_fail_type f) AssignNat
                           | MinusAssign, _ -> Format.fprintf fmt "%s.%s - (%a)"   asset_val id f v
                           | MultAssign , _ -> Format.fprintf fmt "%s.%s * (%a)"   asset_val id f v
                           | DivAssign  , _ -> Format.fprintf fmt "%s.%s / (%a)"   asset_val id f v
                           | AndAssign  , _ -> Format.fprintf fmt "%s.%s and (%a)" asset_val id f v
                           | OrAssign   , _ -> Format.fprintf fmt "%s.%s or (%a)"  asset_val id f v
                        ) ()
                    )
                 ) l
          ) ()
      in
      pp fmt (an, k, l)

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
      let index : int = get_preds_index env.select_preds lb in
      let pp fmt (an, c, _la, _lb, a) =
        let pp fmt _ =
          match c with
          | CKcoll _ -> pp_str fmt const_storage
          | CKview mt
          | CKfield (_, _, mt, _, _) -> Format.fprintf fmt "%s, %a" const_storage f mt
          | CKdef _   -> assert false
        in
        Format.fprintf fmt "select_%a_%i (%a%a)"
          (pp_prefix_container_kind an) c index
          pp ()
          (pp_list "" (pp_prefix ", " f)) a
      in
      pp fmt (an, c, la, lb, a)

    | Msort (an, c, l) ->
      let pp fmt (an, c, l) =
        let pp fmt _ =
          match c with
          | CKcoll _ -> pp_str fmt const_storage
          | CKview mt
          | CKfield (_, _, mt, _, _) -> Format.fprintf fmt "%s, %a" const_storage f mt
          | CKdef _   -> assert false
        in
        Format.fprintf fmt "sort_%a_%a (%a)"
          (pp_prefix_container_kind an) c
          pp_postfix_sort l
          pp ()
      in
      pp fmt (an, c, l)

    | Mcontains (an, c, i) ->
      let pp fmt (an, c, i) =
        Format.fprintf fmt "contains_%a (%a, %a)"
          (pp_prefix_container_kind an) c
          (fun fmt c ->
             match c with
             | CKcoll _           -> (pp_str fmt const_storage)
             | CKview mt          -> f fmt mt
             | CKfield (_, _, mt, _, _) -> Format.fprintf fmt "%s, %a" const_storage f mt
             | CKdef _   -> assert false) c
          f i
      in
      pp fmt (an, c, i)

    | Mnth (an, c, i) ->
      let pp fmt (an, c, i) =
        let pp =
          match c with
          | CKcoll _    -> (fun fmt _ -> Format.fprintf fmt "%s" const_storage)
          | CKview mt
          | CKfield (_, _, mt, _, _) -> (fun fmt _ -> Format.fprintf fmt "%s, %a" const_storage f mt)
          | CKdef _   -> assert false
        in
        Format.fprintf fmt "nth_%a (%a, %a)"
          (pp_prefix_container_kind an) c
          pp c
          f i
      in
      pp fmt (an, c, i)

    | Mcount (an, c) ->
      let pp fmt (an, c) =
        Format.fprintf fmt "count_%a (%a)"
          (pp_prefix_container_kind an) c
          (fun fmt _ ->
             match c with
             | CKcoll _ -> pp_str fmt const_storage
             | CKview mt -> f fmt mt
             | CKfield (_, _, mt, _, _) -> Format.fprintf fmt "%s, %a" const_storage f mt
             | CKdef _   -> assert false) ()
      in
      pp fmt (an, c)

    | Msum (an, c, p) ->
      let index : int = get_preds_index env.sum_preds p in
      let pp fmt (an, c, _p) =
        let pp fmt _ =
          match c with
          | CKcoll _   -> pp_str fmt const_storage
          | CKview  mt
          | CKfield (_, _, mt, _, _) -> Format.fprintf fmt "%s, %a" const_storage f mt
          | CKdef _   -> assert false
        in
        Format.fprintf fmt "sum_%a_%i (%a)"
          (pp_prefix_container_kind an) c index
          pp ()
      in
      pp fmt (an, c, p)

    | Mhead (an, c, i) ->
      begin
        Format.fprintf fmt "head_%a (%a, %a)"
          (pp_prefix_container_kind an) c
          (fun fmt c ->
             match c with
             | CKcoll _ -> pp_str fmt const_storage
             | CKview mt -> f fmt mt
             | CKfield (_, _, mt, _, _) -> Format.fprintf fmt "%s, %a" const_storage f mt
             | CKdef _   -> assert false) c
          f i
      end

    | Mtail (an, c, i) ->
      begin
        Format.fprintf fmt "tail_%a (%a, %a)"
          (pp_prefix_container_kind an) c
          (fun fmt c ->
             match c with
             | CKcoll _ -> pp_str fmt const_storage
             | CKview mt -> f fmt mt
             | CKfield (_, _, mt, _, _) -> Format.fprintf fmt "%s, %a" const_storage f mt
             | CKdef _   -> assert false) c
          f i
      end

    (* utils *)

    | Mcast (src, dst, v) ->
      let pp fmt (src, dst, v) =
        Format.fprintf fmt "(%a : %a)"
          (fun fmt x -> begin
               match src, dst with
               | Tbuiltin (Baddress | Brole), Tcontract _ -> Format.fprintf fmt "get_contract(%a)" f x
               | Tbuiltin Bcurrency, Tbuiltin Bnat        -> Format.fprintf fmt "((%a) / 1mutez)" f x
               | _ -> f fmt x
             end) v
          pp_type dst
      in
      pp fmt (src, dst, v)

    | Mtupleaccess (x, k) ->
      let pp fmt (x, k) =
        Format.fprintf fmt "%a.%a"
          f x
          pp_big_int k
      in
      pp fmt (x, k)

    | Mrecupdate (x, l) ->
      let pp fmt (x, l) =
        Format.fprintf fmt "%a with record [%a]"
          f x
          (pp_list " " (fun fmt (i, v) -> Format.fprintf fmt "%s = %a" i f v)) l
      in
      pp fmt (x, l)


    (* set api expression *)

    | Msetadd (_, c, a) ->
      Format.fprintf fmt "Set.add (%a, %a)"
        f a
        f c

    | Msetremove (_, c, a) ->
      Format.fprintf fmt "Set.remove (%a, %a)"
        f a
        f c

    | Msetcontains (_, c, a) ->
      Format.fprintf fmt "Set.mem (%a, %a)"
        f a
        f c

    | Msetlength (_, c) ->
      Format.fprintf fmt "Set.size (%a)"
        f c

    | Msetfold _ -> emit_error (UnsupportedTerm ("Msetfold"))


    (* list api expression *)

    | Mlistprepend (t, c, a) ->
      Format.fprintf fmt "list_%a_prepend (%a, %a)"
        pp_pretty_type t
        f c
        f a

    | Mlistheadtail (t, c) ->
      Format.fprintf fmt "(case (%a) of | h#t -> (h, t) | nil -> (failwith (\"EmptyList\") : (%a * list(%a))) end)"
        f c
        pp_type t
        pp_type t

    | Mlistlength (_t, c) ->
      Format.fprintf fmt "size (%a)"
        f c

    | Mlistcontains (t, c, a) ->
      Format.fprintf fmt "list_%a_contains (%a, %a)"
        pp_pretty_type t
        f c
        f a

    | Mlistnth (t, c, a) ->
      Format.fprintf fmt "list_%a_nth (%a, %a)"
        pp_pretty_type t
        f c
        f a

    | Mlistreverse (t, l) ->
      Format.fprintf fmt "list_%a_reverse (%a)"
        pp_pretty_type t
        f l

    | Mlistfold _ -> emit_error (UnsupportedTerm ("Mlistfold"))

    (* map api expression *)

    | Mmapput (_, _, c, k, v) ->
      Format.fprintf fmt "map_update (%a, Some (%a), %a)"
        f k
        f v
        f c

    | Mmapremove (_, tv, c, k) ->
      Format.fprintf fmt "map_update (%a, (None : option(%a)), %a)"
        f k
        pp_type tv
        f c

    | Mmapget (_, _, c, k) ->
      Format.fprintf fmt "get_force (%a, %a)"
        f k
        f c

    | Mmapgetopt (_, _, c, k) ->
      Format.fprintf fmt "%a[%a]"
        f c
        f k

    | Mmapcontains (_, _, c, k) ->
      Format.fprintf fmt "map_mem(%a, %a)"
        f k
        f c

    | Mmaplength (_, _, c) ->
      Format.fprintf fmt "Map.size(%a)"
        f c

    | Mmapfold _ -> emit_error (UnsupportedTerm ("Mmapfold"))

    (* builtin functions *)

    | Mmax (l, r) ->
      Format.fprintf fmt "max_%a (%a, %a)"
        pp_pretty_type mtt.type_
        f l
        f r

    | Mmin (l, r) ->
      Format.fprintf fmt "min_%a (%a, %a)"
        pp_pretty_type mtt.type_
        f l
        f r

    | Mabs a ->
      let pp_tmp fmt t =
        match t with
        | Tbuiltin Bnat -> ()
        | _ -> Format.fprintf fmt "_%a" pp_pretty_type t
      in
      Format.fprintf fmt "abs%a (%a)"
        pp_tmp mtt.type_
        f a

    | Mconcat (x, y) ->
      Format.fprintf fmt "concat_%a (%a, %a)"
        pp_pretty_type mtt.type_
        f x
        f y

    | Mslice (x, s, e) ->
      let prefix =
        match mtt.type_ with
        | Tbuiltin Bstring -> "string"
        | Tbuiltin Bbytes -> "bytes"
        | _ -> assert false
      in
      Format.fprintf fmt "%s_slice (%a, %a, %a)"
        prefix
        f s
        f e
        f x

    | Mlength x ->
      Format.fprintf fmt "size(%a)"
        f x

    | Misnone x ->
      Format.fprintf fmt "isnone_%a (%a)"
        pp_pretty_type (extract_option_type x.type_)
        f x

    | Missome x ->
      Format.fprintf fmt "issome_%a (%a)"
        pp_pretty_type (extract_option_type x.type_)
        f x

    | Moptget x ->
      Format.fprintf fmt "getopt_%a (%a)"
        pp_pretty_type (extract_option_type x.type_)
        f x

    | Mfloor x ->
      Format.fprintf fmt "floor (%a)"
        f x

    | Mceil x ->
      Format.fprintf fmt "ceil (%a)"
        f x

    | Mtostring (t, x) ->
      Format.fprintf fmt "to_string_%a (%a)"
        pp_pretty_type t
        f x

    | Mpack x ->
      Format.fprintf fmt "Bytes.pack (%a)"
        f x

    | Munpack (t, x) ->
      Format.fprintf fmt "(Bytes.unpack (%a) : option (%a))"
        f x
        pp_type t

    (* crypto functions *)

    | Mblake2b x ->
      Format.fprintf fmt "Crypto.blake2b (%a)"
        f x

    | Msha256 x ->
      Format.fprintf fmt "Crypto.sha256 (%a)"
        f x

    | Msha512 x ->
      Format.fprintf fmt "Crypto.sha512 (%a)"
        f x

    | Mhashkey x ->
      Format.fprintf fmt "Crypto.hash_key (%a)"
        f x

    | Mchecksignature (k, s, x) ->
      Format.fprintf fmt "Crypto.check (%a, %a, %a)"
        f k
        f s
        f x


    (* constants *)

    | Mnow           -> pp_str fmt "now"
    | Mtransferred   -> pp_str fmt "amount"
    | Mcaller        -> pp_str fmt "sender"
    | Mbalance       -> pp_str fmt "balance"
    | Msource        -> pp_str fmt "source"
    | Mselfaddress   -> pp_str fmt "Tezos.self_address"
    | Mchainid       -> pp_str fmt "chain_id"
    | Mmetadata      -> pp_str fmt "metadata"


    (* variable *)

    | Mvar (an, Vassetstate k, _, _) ->
      Format.fprintf fmt "state_%a(%a)"
        pp_str (unloc an)
        f k

    | Mvar (v, Vstorevar, _, _) ->
      if (is_const env v)
      then pp_id fmt v
      else Format.fprintf fmt "%s.%a" const_storage pp_id v

    | Mvar (v, Vstorecol, _, _) -> Format.fprintf fmt "%s.%a" const_storage pp_id v

    | Mvar (v, Venumval, _, _)  -> pp_id fmt v

    | Mvar (v, Vdefinition, _, _) -> pp_id fmt v

    | Mvar (v, Vlocal, _, _)    -> pp_id fmt v

    | Mvar (v, Vparam, _, _)    ->
      Format.fprintf fmt "%a%a"
        (fun fmt x ->
           match x with
           | Some ({node = Entry _ }) -> pp_str fmt "action."
           | _ -> ()
        ) env.f
        pp_id v

    | Mvar (v, Vfield, _, _)    -> pp_id fmt v

    | Mvar (_, Vthe, _, _)      -> pp_str fmt "the"

    | Mvar (_, Vstate, _, _)    -> Format.fprintf fmt "%s.%s" const_storage const_state

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

    | Mnattoint e ->
      let pp fmt e =
        Format.fprintf fmt "int(%a)"
          f e
      in
      pp fmt e

    | Mnattorat e ->
      let pp fmt e =
        Format.fprintf fmt "(int(%a), 1n)"
          f e
      in
      pp fmt e

    | Minttorat e ->
      let pp fmt e =
        Format.fprintf fmt "((%a), 1n)"
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
      "type %a is record [@\n  @[%a@]@\n]@\n"
      pp_id asset.name
      (pp_list "@\n" pp_asset_item) asset.values;
    let an = unloc asset.name in
    if (not (Utils.is_asset_single_field model an)) then
      begin
        let k, _ = Utils.get_asset_key model an in
        let ll = List.filter (fun (x : asset_item) -> not (String.equal (unloc x.name) k)) asset.values in
        Format.fprintf fmt
          "@\ntype %a_storage is record [@\n  @[%a@]@\n]@\n"
          pp_id asset.name
          (pp_list "@\n" pp_asset_item) ll
      end
  in

  let pp_record (fmt : Format.formatter) (r : record) =
    let pp_record_field (fmt : Format.formatter) (rf : record_field) =
      Format.fprintf fmt
        "%a : %a;"
        pp_id rf.name
        pp_type rf.type_
    in
    Format.fprintf fmt
      "type %a is record [@\n  @[%a@]@\n]@\n"
      pp_id r.name
      (pp_list "@\n" pp_record_field) r.fields;
  in

  let pp_decl env (fmt : Format.formatter) (decl : decl_node) =
    match decl with
    | Dvar v    -> pp_var env fmt v
    | Denum e   -> pp_enum    fmt e
    | Dasset a  -> pp_asset   fmt a
    | Drecord r -> pp_record  fmt r
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
    let is_emtpy l =
      let l = List.filter (fun (x : storage_item) -> not x.const) l in
      match l with
      | [] -> true
      | _ -> false
    in
    Format.fprintf fmt "type storage_type is ";
    match is_emtpy model.storage with
    | true ->  Format.fprintf fmt "unit"
    | false ->
      let l = List.filter (fun x -> not x.const) model.storage in
      Format.fprintf fmt
        "record [@\n  \
         @[%a@]@\n\
         ]@\n"
        (pp_list "@\n" pp_storage_item) l
  in

  let pp_prefix_api_container_kind an fmt = function
    | Coll  -> Format.fprintf fmt "c_%s" an
    | View  -> Format.fprintf fmt "v_%s" an
    | Field (an, fn) -> Format.fprintf fmt "f_%s_%s" an fn
  in

  let pp_api_asset (env : env) fmt = function
    | Get an ->
      let k, t = Utils.get_asset_key model an in
      if (Utils.is_asset_single_field model an) then
        Format.fprintf fmt
          "function get_%s (const s : storage_type; const key : %a) : %s is@\n  \
           begin@\n    \
           if not set_mem(key, s.%s) then failwith (\"key does not exists\") else skip;@\n    \
           const res : %s = record[%s = key]@\n  \
           end with (res)@\n"
          an pp_type t an
          an
          an k
      else
        let asset = Utils.get_asset model an in
        Format.fprintf fmt
          "function get_%s (const s : storage_type; const key : %a) : %s is@\n  \
           begin@\n    \
           const a : %s_storage = get_force(key, s.%s);@\n    \
           const res : %s = record[%a]@\n  \
           end with (res)@\n"
          an pp_type t an an an an
          (pp_list "; " (fun fmt (x : asset_item) ->
               let fn = unloc x.name in
               Format.fprintf fmt "%s = %s" fn (if (String.equal k fn) then "key" else ("a." ^ fn)))) asset.values

    | Set an ->
      let k, t = Utils.get_asset_key model an in
      let asset = Utils.get_asset model an in
      let fns = asset.values |> List.map (fun (ai : asset_item) -> unloc ai.name) |> List.filter (fun x -> not (String.equal k x)) in
      Format.fprintf fmt
        "function set_%s (const s : storage_type; const key : %a; const a : %s) : storage_type is@\n  \
         begin@\n    \
         const map_local : map(%a, %s_storage) = s.%s;@\n    \
         map_local[key] := record[%a];@\n    \
         s.%s := map_local;@\n  \
         end with (s)@\n"
        an pp_type t an
        pp_type t an an
        (pp_list "; " (fun fmt fn -> Format.fprintf fmt "%s = a.%s" fn fn)) fns
        an

    | Add an ->
      let k, t = Utils.get_asset_key model an in
      let asset = Utils.get_asset model an in
      let fns = asset.values |> List.map (fun (ai : asset_item) -> unloc ai.name) |> List.filter (fun x -> not (String.equal k x)) in
      (* let ft, c = Utils.get_field_container model an fn in *)
      if Utils.is_asset_single_field model an
      then
        Format.fprintf fmt
          "function add_%s (const s : storage_type; const a : %s) : storage_type is@\n  \
           begin@\n    \
           const key : %a = a.%s;@\n    \
           if set_mem(key, s.%s) then failwith (\"key already exists\") else skip;@\n    \
           s.%s := set_add(key, s.%s);@\n  \
           end with (s)@\n"
          an an
          pp_type t k
          an
          an an
      else
        let big = asset.big_map in
        Format.fprintf fmt
          "function add_%s (const s : storage_type; const a : %s) : storage_type is@\n  \
           begin@\n    \
           const key : %a = a.%s;@\n    \
           const map_local : %amap(%a, %s_storage) = s.%s;@\n    \
           if map_mem(key, map_local) then failwith (\"key already exists\") else skip;@\n    \
           const asset : %s_storage = record[%a];@\n    \
           map_local[key] := asset;@\n    \
           s.%s := map_local;@\n  \
           end with (s)@\n"
          an an
          pp_type t k
          (pp_do_if big pp_str) "big_" pp_type t an an
          an (pp_list "; " (fun fmt fn -> Format.fprintf fmt "%s = a.%s" fn fn)) fns
          an

    | Remove an ->
      let _, t = Utils.get_asset_key model an in
      let aps : (ident * ident) list =
        begin
          let asset = Utils.get_asset model an in
          List.fold_left (fun accu (x : asset_item) ->
              match x.original_type with
              | Tcontainer (Tasset an, Partition) -> (unloc x.name, unloc an)::accu
              | _ -> accu
            ) [] asset.values
        end
      in
      Format.fprintf fmt
        "function remove_%s (const s : storage_type; const key : %a) : storage_type is@\n  \
         begin@\n    \
         %aremove key from %s s.%s;@\n  \
         end with (s)@\n"
        an pp_type t
        (fun fmt aps ->
           match aps with
           | [] -> ()
           | _ ->
             Format.fprintf fmt "const asset : %s_storage = get_force(key, s.%s);@\n    " an an;
             (pp_list "" (fun fmt (fn, fan : ident * ident) ->
                  Format.fprintf fmt
                    "for i in set (asset.%s) block {@\n      s := remove_%s(s, i)@\n    };@\n    "
                    fn fan)) fmt aps
        ) aps
        (if Utils.is_asset_single_field model an then "set" else "map") an

    | Clear (an, c) ->
      begin
        let _, t = Utils.get_asset_key model an in
        match c with
        | Coll -> begin
            let single = Model.Utils.is_asset_single_field model an in
            let i, a = if single then "i", "set" else "i -> v", "map" in
            Format.fprintf fmt
              "function clear_c_%s (const s : storage_type) : storage_type is@\n  \
               begin@\n  \
               for %s in %s (s.%s) block {@\n    \
               s := remove_%s(s, i)@\n  \
               }@\n  \
               end with (s)@\n"
              an
              i a an
              an
          end
        | View ->
          Format.fprintf fmt
            "function clear_v_%s (const s : storage_type; const l : list(%a)) : storage_type is@\n  \
             begin@\n  \
             for i in list (l) block {@\n    \
             s := remove_%s(s, i)@\n  \
             }@\n  \
             end with (s)@\n"
            an pp_type t an
        | Field (an, fn) ->
          Format.fprintf fmt
            "function clear_%a (const s : storage_type; const k : %a) : storage_type is@\n  \
             begin@\n  \
             const a : %s_storage = get_force(k, s.%s);@\n  \
             for i in set (a.%s) block {@\n    \
             s := remove_%s_%s (s, k, i)@\n  \
             }@\n  \
             end with (s)@\n"
            (pp_prefix_api_container_kind an) c pp_type (Utils.get_asset_key model an |> snd)
            an an
            fn
            an fn
      end

    | Update _ -> ()

    | FieldAdd (an, fn) ->
      let _, t = Utils.get_asset_key model an in
      let ft, c = Utils.get_field_container model an fn in
      let kk, kt = Utils.get_asset_key model ft in
      let single = Utils.is_asset_single_field model ft in
      let bkey =
        match c with
        | Aggregate    -> "b"
        | Partition -> "b." ^ kk
        | _ -> assert false
      in
      let pp_instr, pp_b_arg_type =
        match c with
        | Aggregate ->
          (fun fmt _ ->
             Format.fprintf fmt
               "if not %s.mem(%s, s.%s) then failwith (\"key does not exist\") else skip;@\n    "
               (if single then "Set" else "Map") bkey ft),
          (fun fmt _ -> pp_type fmt kt)
        | Partition ->
          (fun fmt _ ->
             Format.fprintf fmt "s := add_%s(s, b);@\n    " ft),
          (fun fmt _ -> pp_str fmt ft)
        | _ -> assert false
      in
      Format.fprintf fmt
        "function add_%s_%s (const s : storage_type; const asset_key : %a; const b : %a) : storage_type is@\n  \
         begin@\n    \
         const asset_val : %s_storage = get_force(asset_key, s.%s);@\n    \
         %a\
         s.%s[asset_key] := asset_val with record[%s = Set.add(%s, asset_val.%s)];@\n  \
         end with (s)@\n"
        an fn pp_type t pp_b_arg_type ()
        an
        an
        pp_instr ()
        an fn bkey fn

    | FieldRemove (an, fn) ->
      let _k, t = Utils.get_asset_key model an in
      let ft, c = Utils.get_field_container model an fn in
      let _kk, tt = Utils.get_asset_key model ft in
      Format.fprintf fmt
        "function remove_%s_%s (const s : storage_type; const asset_key : %a; const removed_key : %a) : storage_type is@\n  \
         begin@\n    \
         const asset_val : %s_storage = get_force(asset_key, s.%s);@\n    \
         remove removed_key from set asset_val.%s;@\n    \
         s.%s[asset_key] := asset_val;@\n  \
         %a\
         end with (s)@\n"
        an fn pp_type t pp_type tt
        an an
        fn an
        (pp_do_if (match c with | Partition -> true | _ -> false) (fun fmt -> Format.fprintf fmt "  s := remove_%s(s, removed_key);@\n  ")) ft

    | RemoveAll (an, fn) ->
      let _, t = Utils.get_asset_key model an in
      let ft, _c = Utils.get_field_container model an fn in
      let _kk, tt = Utils.get_asset_key model ft in
      Format.fprintf fmt
        "function removeall_%s_%s (const s : storage_type; const k : %a) : storage_type is@\n  \
         begin@\n    \
         const asset_val : %s_storage = get_force(k, s.%s);@\n    \
         const l : set(%a) = asset_val.%s;@\n    \
         for i in set (l) block {@\n      \
         s := remove_%s_%s(s, k, i)@\n    \
         }@\n  \
         end with (s)@\n"
        an fn pp_type t
        an an
        pp_type tt fn
        an fn

    | RemoveIf (an, c, args, f) ->
      let pp_arg fmt (arg_id, arg_type) =
        Format.fprintf fmt "; const %s : %a" arg_id pp_type arg_type
      in
      let _k, t = Utils.get_asset_key model an in
      let i = get_preds_index env.removeif_preds f in
      let is_one_field = Model.Utils.is_asset_single_field model an in
      begin
        match c with
        | Coll -> begin
            let container, iter_type, iter_val =
              if is_one_field
              then "set", "", ""
              else "map", " * " ^ an ^ "_storage", ".0"
            in
            Format.fprintf fmt
              "function removeif_%a_%i (const s : storage_type%a) : storage_type is@\n  \
               begin@\n    \
               function aggregate (const accu : storage_type; const i : %a%s) : storage_type is@\n      \
               begin@\n        \
               const the : %s = get_%s(s, i%s);@\n        \
               end with (if (%a) then remove_%s (accu, i%s) else accu);@\n    \
               end with (%s_fold(aggregate, s.%s, s))@\n"
              (pp_prefix_api_container_kind an) c i (pp_list "" pp_arg) args
              pp_type t iter_type
              an an iter_val
              (pp_mterm (mk_env ())) f an iter_val
              container an
          end
        | Field (an , fn) -> begin
            let aan, _ = Utils.get_field_container model an fn in
            let _kk, tt = Utils.get_asset_key model aan in

            Format.fprintf fmt
              "function removeif_%a_%i (const s : storage_type; const key : %a%a) : storage_type is@\n  \
               begin@\n    \
               function aggregate (const accu : storage_type; const i : %a) : storage_type is@\n      \
               begin@\n          \
               const the : %s = get_%s(s, i);@\n      \
               end with (if (%a) then remove_%s_%s (accu, key, i) else accu);@\n  \
               end with (case s.%s[key] of None -> s | Some(a) -> set_fold(aggregate, a.%s, s) end)"
              (pp_prefix_api_container_kind an) c i pp_type t (pp_list "" pp_arg) args
              pp_type tt
              aan aan
              (pp_mterm (mk_env ())) f an fn
              an fn
          end
        | _ -> assert false
      end

    | Contains (an, c) ->
      begin
        let _, t = Utils.get_asset_key model an in
        Format.fprintf fmt
          "function contains_%a (%a; const key : %a) : bool is block {%a} with %a@\n"
          (pp_prefix_api_container_kind an) c
          (fun fmt c ->
             match c with
             | Coll  -> Format.fprintf fmt "const s : storage_type"
             | View  -> Format.fprintf fmt "const l : list(%a)" pp_type t
             | Field (an, _) -> Format.fprintf fmt "const s : storage_type; const k : %a" pp_type (Utils.get_asset_key model an |> snd)) c
          pp_type t
          (fun fmt c ->
             match c with
             | Coll  -> Format.fprintf fmt " skip "
             | View  -> Format.fprintf fmt "@\n  function aggregate (const accu : bool; const v : %a) : bool is block { skip } with (accu or v = key);@\n" pp_type t
             | Field (an, fn) -> Format.fprintf fmt "@\n  const a : %s_storage = get_force(k, s.%s);@\n  const l : set(%a) = a.%s;@\n" an an pp_type t fn) c
          (fun fmt c ->
             match c with
             | Coll  -> Format.fprintf fmt "%s.mem (key, s.%s)" (if Utils.is_asset_single_field model an then "Set" else "Map") an
             | View  -> Format.fprintf fmt "list_fold(aggregate, l, False)"
             | Field _ -> Format.fprintf fmt "Set.mem(key, l)" ) c
      end

    | Nth (an, c) ->
      let _k, t = Utils.get_asset_key model an in
      let is_one_field = Model.Utils.is_asset_single_field model an in
      let pp_fun_arg fmt () =
        match c with
        | Coll -> ()
        | View -> Format.fprintf fmt "; const l : list(%a)" pp_type t
        | Field (an, _) -> Format.fprintf fmt "; const k : %a" pp_type (Utils.get_asset_key model an |> snd)
      in
      let container, src, iter_type, iter_val, pp_src =
        match c with
        | Coll when is_one_field ->
          "set", "s." ^ an ^ "", "", "", (fun _ _ -> ())
        | Coll ->
          "map", "s." ^ an ^ "", " * " ^ an ^ "_storage", ".0", (fun _ _ -> ())
        | View ->
          "list", "l", "", "", (fun _ _ -> ())
        | Field (an, fn) ->
          "set", "a." ^ fn, "", "",
          (fun fmt _ -> Format.fprintf fmt "const a : %s_storage = get_force(k, s.%s);@\n    " an an)
      in
      Format.fprintf fmt
        "function nth_%a (const s : storage_type%a; const index : nat) : %a is@\n  \
         block {@\n  \
         %a\
         function aggregate (const accu: map(nat, %a); const x: %a%s) : map(nat, %a) is@\n  \
         block {@\n  \
         const le : nat = Map.size(accu);@\n  \
         accu[le] := x%s;@\n  \
         } with accu;@\n  \
         const map_ : map(nat, %a) = %s_fold(aggregate, %s, (map [] : map(nat, %a)));@\n  \
         const res : %a = get_force(index, map_);@\n  \
         } with res@\n"
        (pp_prefix_api_container_kind an) c pp_fun_arg () pp_type t
        pp_src ()
        pp_type t pp_type t iter_type pp_type t
        iter_val
        pp_type t container src pp_type t
        pp_type t

    | Select (an, c, args, f) ->
      let pp_arg fmt (arg_id, arg_type) =
        Format.fprintf fmt "; const %s : %a" arg_id pp_type arg_type
      in
      let k, t = Utils.get_asset_key model an in
      let i = get_preds_index env.select_preds f in
      let is_one_field = Model.Utils.is_asset_single_field model an in
      let pp_fun_arg fmt () =
        match c with
        | Coll  -> ()
        | View  -> Format.fprintf fmt "; const l : list(%a)" pp_type t
        | Field (an, _) -> Format.fprintf fmt "; const k : %a" pp_type (Utils.get_asset_key model an |> snd)
      in
      let container, src, iter_type, iter_val, pp_src =
        match c with
        | Coll when is_one_field ->
          "set", "s." ^ an ^ "", "", "", (fun _ _ -> ())
        | Coll ->
          "map", "s." ^ an ^ "", " * " ^ an ^ "_storage", ".0", (fun _ _ -> ())
        | View ->
          "list", "l", "", "", (fun _ _ -> ())
        | Field (an, fn) ->
          "set", "a." ^ fn, "", "",
          (fun fmt _ -> Format.fprintf fmt "const a : %s_storage = get_force(k, s.%s);@\n    " an an)
      in
      Format.fprintf fmt
        "function select_%a_%i (const s : storage_type%a%a) : list(%a) is@\n  \
         begin@\n    \
         %a\
         function rev (const accu: list(%a); const x: %a) : list(%a) is x # accu;@\n    \
         function aggregate (const accu : list(%a); const i : %a%s) : list(%a) is@\n      \
         begin@\n        \
         const the : %s = get_%s(s, i%s);@\n      \
         end with (if (%a) then cons(the.%s, accu) else accu);@\n  \
         end with (list_fold (rev, %s_fold(aggregate, %s, (nil : list(%a))), (list [] : list(%a))))@\n"
        (pp_prefix_api_container_kind an) c i pp_fun_arg () (pp_list "" pp_arg) args pp_type t
        pp_src ()
        pp_type t pp_type t pp_type t
        pp_type t pp_type t iter_type pp_type t
        an an iter_val
        (pp_mterm (mk_env ())) f k
        container src pp_type t pp_type t

    | Sort (an, c, l) ->
      let _, t = Utils.get_asset_key model an in

      let is_one_field = Model.Utils.is_asset_single_field model an in
      let pp_fun_arg fmt () =
        match c with
        | Coll  -> ()
        | View  -> Format.fprintf fmt "; const l : list(%a)" pp_type t
        | Field (an, _) -> Format.fprintf fmt "; const k : %a" pp_type (Utils.get_asset_key model an |> snd)
      in
      let container, src, iter_type, iter_val, pp_src =
        match c with
        | Coll when is_one_field ->
          "set", "s." ^ an ^ "", "", "", (fun _ _ -> ())
        | Coll ->
          "map", "s." ^ an ^ "", " * " ^ an ^ "_storage", ".0", (fun _ _ -> ())
        | View ->
          "list", "l", "", "", (fun _ _ -> ())
        | Field (an, fn) ->
          "set", "a." ^ fn, "", "",
          (fun fmt _ -> Format.fprintf fmt "const a : %s_storage = get_force(k, s.%s);@\n    " an an)
      in

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

      let pp_rev fmt _ =
        Format.fprintf fmt
          "function rev (const accu: list(%a); const x: %a) : list(%a) is x # accu;@\n"
          pp_type t
          pp_type t
          pp_type t
      in

      let pp_fun_cmp fmt _ =
        Format.fprintf fmt
          "function cmp (const k1 : %a; const k2: %a) : int is@\n  \
           block {@\n    \
           var res : int := 0;@\n    \
           const a1 : %s = get_%s(s, k1);@\n    \
           const a2 : %s = get_%s(s, k2);@\n    \
           %a@\n    \
           else skip@\n  \
           } with res;@\n"
          pp_type t pp_type t
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
           if (cmp(x, v) > 0)@\n    \
           then ((None : option(%a)), cons(x, cons(v, accu.1)))@\n    \
           else (Some(v), cons(x, accu.1))@\n    \
           | None -> ((None : option(%a)), cons(x, accu.1))@\n    \
           end;@\n  \
           } with res;@\n"
          pp_type t pp_type t pp_type t pp_type t pp_type t
          pp_type t pp_type t
          pp_type t
          pp_type t
      in

      let pp_fun_sort fmt _ =
        Format.fprintf fmt
          "function sort (const accu: list(%a); const x: %a%s) : list(%a) is@\n  \
           block {@\n    \
           const init : option(%a) * list(%a) = (Some(x%s), (list [] : list(%a)));@\n    \
           const res_opt : option(%a) * list(%a) = list_fold(insert, accu, init);@\n    \
           const res : list(%a) =@\n    \
           case res_opt.0 of@\n    \
           | Some(v) -> cons(v, res_opt.1)@\n    \
           | None -> res_opt.1@\n    \
           end;@\n  \
           } with list_fold(rev, res, (list [] : list(%a)));@\n"
          pp_type t pp_type t iter_type pp_type t
          pp_type t pp_type t iter_val pp_type t
          pp_type t pp_type t
          pp_type t pp_type t
      in

      Format.fprintf fmt
        "function sort_%a_%a (const s : storage_type%a) : list(%a) is@\n  @\n  \
         begin@\n    \
         @[%a@]@\n    \
         @[%a@]@\n    \
         @[%a@]@\n    \
         @[%a@]@\n    \
         %a\
         const init : list(%a) = list [];@\n    \
         const res : list(%a) = %s_fold (sort, %s, init);@\n  \
         end with res@\n"
        (pp_prefix_api_container_kind an) c pp_postfix_sort l pp_fun_arg () pp_type t
        pp_rev ()
        pp_fun_cmp ()
        pp_fun_insert ()
        pp_fun_sort ()
        pp_src ()
        pp_type t
        pp_type t container src

    | Count (an, c) ->
      begin
        let _, t = Utils.get_asset_key model an in
        Format.fprintf fmt
          "function count_%a (%a) : nat is block { %a} with %a@\n"
          (pp_prefix_api_container_kind an) c
          (fun fmt c ->
             match c with
             | Coll -> Format.fprintf fmt "const s : storage_type"
             | View -> Format.fprintf fmt "const l : list(%a)" pp_type t
             | Field (an, _) -> Format.fprintf fmt "const s : storage_type; const k : %a" pp_type (Utils.get_asset_key model an |> snd)) c
          (fun fmt c ->
             match c with
             | Coll -> Format.fprintf fmt "skip "
             | View -> Format.fprintf fmt "skip "
             | Field (an, fn) -> Format.fprintf fmt "@\n  const a : %s_storage = get_force(k, s.%s);@\n  const l : set(%a) = a.%s;@\n" an an pp_type t fn) c
          (fun fmt c ->
             match c with
             | Coll ->
               let src = "s." ^ an ^ "" in
               let size = if Model.Utils.is_asset_single_field model an then "Set.size" else "Map.size" in
               Format.fprintf fmt "%s(%s)" size src
             | View -> Format.fprintf fmt "size(l)"
             | Field _ -> Format.fprintf fmt "Set.size(l)") c
      end

    | Sum (an, c, t, p) ->
      let rec pp_expr fmt (mt : mterm) =
        match mt.node with
        | Mdot ({node = Mvar ({pldesc = "the"; _}, Vlocal, _, _) }, fn) ->
          Format.fprintf fmt "a.%a"
            pp_id fn
        | _ -> (pp_mterm_gen (mk_env ()) pp_expr) fmt mt
      in
      let get_zero _ =
        match t with
        | Ttuple [Tbuiltin Bint; Tbuiltin Bnat] -> "(0, 1n)"
        | Tbuiltin Bnat -> "0n"
        | _ -> "0"
      in
      let _, tk = Utils.get_asset_key model an in
      let i = get_preds_index env.sum_preds p in
      let is_one_field = Model.Utils.is_asset_single_field model an in
      let pp_fun_arg fmt () =
        match c with
        | Coll  -> ()
        | View  -> Format.fprintf fmt "; const l : list(%a)" pp_type tk
        | Field (an, _) -> Format.fprintf fmt "; const k : %a" pp_type (Utils.get_asset_key model an |> snd)
      in
      let pp_formula fmt _ =
        match t with
        | Ttuple [Tbuiltin Bint; Tbuiltin Bnat] -> pp_str fmt "e.0 * int(accu.1) + accu.0 * int(e.1), e.1 * accu.1"
        | _ -> pp_str fmt "accu + e"
      in
      let container, src, iter_type, iter_val, pp_src =
        match c with
        | Coll when is_one_field ->
          "set", "s." ^ an ^ "", "", "", (fun _ _ -> ())
        | Coll ->
          "map", "s." ^ an ^ "", " * " ^ an ^ "_storage", ".0", (fun _ _ -> ())
        | View ->
          "list", "l", "", "", (fun _ _ -> ())
        | Field (an, fn) ->
          "set", "a." ^ fn, "", "",
          (fun fmt _ -> Format.fprintf fmt "@\n  const a : %s_storage = get_force(k, s.%s);" an an)
      in
      Format.fprintf fmt
        "function sum_%a_%i (const s : storage_type%a) : %a is@\n  \
         begin@\n    \
         function aggregate (const accu : %a; const i : %a%s) : %a is@\n      \
         block {@\n        \
         const a : %s = get_%s(s, i%s);@\n      \
         const e : %a = %a;@\n      \
         } with (%a);%a  @\n  \
         end with (%s_fold(aggregate, %s, %s))@\n"
        (pp_prefix_api_container_kind an) c i pp_fun_arg () pp_type t
        pp_type t pp_type tk iter_type pp_type t
        an an iter_val
        pp_type t pp_expr p
        pp_formula ()
        pp_src ()
        container src (get_zero t)

    | Head (an, c) ->
      let _, t = Utils.get_asset_key model an in
      let is_one_field = Model.Utils.is_asset_single_field model an in
      let pp_first_arg fmt () =
        match c with
        | Coll  -> Format.fprintf fmt "const s : storage_type"
        | View  -> Format.fprintf fmt "const l : list(%a)" pp_type t
        | Field (an, _) -> Format.fprintf fmt "const s : storage_type; const k : %a" pp_type (Utils.get_asset_key model an |> snd)
      in
      let size, container, src, iter_type, iter_val, pp_src =
        match c with
        | Coll when is_one_field ->
          "Set.size", "set", "s." ^ an ^ "", "", "", (fun _ _ -> ())
        | Coll ->
          "Map.size", "map", "s." ^ an ^ "", " * " ^ an ^ "_storage", ".0", (fun _ _ -> ())
        | View ->
          "size", "list", "l", "", "", (fun _ _ -> ())
        | Field (an, fn) ->
          "Set.size", "set", "a." ^ fn, "", "",
          (fun fmt _ -> Format.fprintf fmt "const a : %s_storage = get_force(k, s.%s);@\n    " an an)
      in
      Format.fprintf fmt
        "function head_%a (%a; const i : nat) : list(%a) is@\n  \
         block {@\n    \
         %a\
         const length : nat = %s(%s);@\n    \
         const bound : nat = if i < length then i else length;@\n    \
         function rev (const accu: list(%a); const x: %a) : list(%a) is x # accu;@\n    \
         function aggregate (const accu: nat * list(%a); const x: %a%s) : nat * list(%a) is@\n    \
         if accu.0 < bound@\n    \
         then (accu.0 + 1n, x%s # accu.1 );@\n    \
         else (accu.0 + 1n, accu.1 );@\n    \
         const init : nat * list(%a) = (0n, ((list [] : list(%a))));@\n    \
         const ltmp : nat * list(%a) = %s_fold (aggregate, %s, init);@\n    \
         const res  : list(%a) = list_fold (rev, ltmp.1, ((list [] : list(%a))));@\n  \
         } with res@\n"
        (pp_prefix_api_container_kind an) c pp_first_arg () pp_type t
        pp_src ()
        size src
        pp_type t pp_type t pp_type t
        pp_type t pp_type t iter_type pp_type t
        iter_val
        pp_type t pp_type t
        pp_type t container src
        pp_type t pp_type t

    | Tail (an, c) ->
      let _, t = Utils.get_asset_key model an in
      let is_one_field = Model.Utils.is_asset_single_field model an in
      let pp_first_arg fmt () =
        match c with
        | Coll  -> Format.fprintf fmt "const s : storage_type"
        | View  -> Format.fprintf fmt "const l : list(%a)" pp_type t
        | Field (an, _) -> Format.fprintf fmt "const s : storage_type; const k : %a" pp_type (Utils.get_asset_key model an |> snd)
      in
      let size, container, src, iter_type, iter_val, pp_src =
        match c with
        | Coll when is_one_field ->
          "Set.size", "set", "s." ^ an ^ "", "", "", (fun _ _ -> ())
        | Coll ->
          "Map.size", "map", "s." ^ an ^ "", " * " ^ an ^ "_storage", ".0", (fun _ _ -> ())
        | View ->
          "size", "list", "l", "", "", (fun _ _ -> ())
        | Field (an, fn) ->
          "Set.size", "set", "a." ^ fn, "", "",
          (fun fmt _ -> Format.fprintf fmt "const a : %s_storage = get_force(k, s.%s);@\n    " an an)
      in
      Format.fprintf fmt
        "function tail_%a (%a; const i : nat) : list(%a) is@\n  \
         block {@\n    \
         %a\
         const length : nat = %s(%s);@\n    \
         const p : nat = if (length - i) >= 0 then abs(length - i) else 0n;@\n    \
         function rev (const accu: list(%a); const x: %a) : list(%a) is x # accu;@\n    \
         function aggregate (const accu: nat * list(%a); const x: %a%s) : nat * list(%a) is@\n    \
         if accu.0 >= p@\n    \
         then (accu.0 + 1n, x%s # accu.1 );@\n    \
         else (accu.0 + 1n, accu.1 );@\n    \
         const init : nat * list(%a) = (0n, ((list [] : list(%a))));@\n    \
         const ltmp : nat * list(%a) = %s_fold (aggregate, %s, init);@\n    \
         const res  : list(%a) = list_fold (rev, ltmp.1, ((list [] : list(%a))));@\n  \
         } with res@\n"
        (pp_prefix_api_container_kind an) c pp_first_arg () pp_type t
        pp_src ()
        size src
        pp_type t pp_type t pp_type t
        pp_type t pp_type t iter_type pp_type t
        iter_val
        pp_type t pp_type t
        pp_type t container src
        pp_type t pp_type t

  in

  let pp_api_list (_env : env) fmt = function
    | Lprepend t  ->
      Format.fprintf fmt
        "function list_%a_prepend (const l : list(%a); const i : %a) : list(%a) is@\n  \
         block { skip }@\n  \
         with i # l@\n"
        pp_pretty_type t pp_type t pp_type t pp_type t

    | Lcontains t ->
      Format.fprintf fmt
        "function list_%a_contains (const l : list(%a); const item : %a) : bool is@\n  \
         block {@\n  \
         function aggregate (const accu: bool; const x: %a) : bool is (%a) or accu @\n  \
         } with list_fold (aggregate, l, False)@\n"
        pp_pretty_type t pp_type t pp_type t
        pp_type t
        (fun fmt t ->
           match t with
           | _ -> Format.fprintf fmt "item = x") t

    | Llength _ -> ()

    | Lnth t ->
      Format.fprintf fmt
        "function list_%a_nth (const l : list(%a); const index : nat) : %a is@\n  \
         block {@\n\
         function aggregate (const accu: nat * option(%a); const x: %a) : nat * option(%a) is@\n\
         if accu.0 = index@\n\
         then (accu.0 + 1n, Some(x));@\n\
         else (accu.0 + 1n, accu.1);@\n\
         const init : nat * option(%a) = (0n, (None : option(%a)));@\n\
         const res_opt : nat * option(%a) = list_fold (aggregate, l, init);@\n\
         const res : %a = @\n\
         case res_opt.1 of@\n\
         | Some(v) -> v@\n\
         | None -> (failwith(\"list_%a_nth failed\") : %a)@\n\
         end@\n\
         } with res@\n"
        pp_pretty_type t pp_type t pp_type t
        pp_type t pp_type t pp_type t
        pp_type t pp_type t
        pp_type t
        pp_type t
        pp_type t pp_type t

    | Lreverse t  ->
      Format.fprintf fmt
        "function list_%a_reverse (const l : list(%a)) : list(%a) is@\n  \
         block {@\ \
         function rev (const accu: list(%a); const x: %a) : list(%a) is x # accu;@\n    \
         }@\n  \
         with list_fold (rev, l, ((list [] : list(%a))))@\n"
        pp_pretty_type t pp_type t pp_type t
        pp_type t pp_type t pp_type t
        pp_type t
  in

  let pp_api_builtin (_env : env) fmt = function
    | Bmin t ->
      let cond =
        match t with
        | Tbuiltin Brational | Ttuple [Tbuiltin Bint; Tbuiltin Bnat] -> "rat_cmp(OpCmpLt(unit), a, b)"
        | _ -> "a < b"
      in
      Format.fprintf fmt
        "function min_%a (const a : %a; const b : %a) : %a is if %s then a else b@\n"
        pp_pretty_type t pp_type t pp_type t pp_type t cond

    | Bmax t ->
      let cond =
        match t with
        | Tbuiltin Brational | Ttuple [Tbuiltin Bint; Tbuiltin Bnat] -> "rat_cmp(OpCmpGt(unit), a, b)"
        | _ -> "a > b"
      in
      Format.fprintf fmt
        "function max_%a (const a : %a; const b : %a) : %a is if %s then a else b@\n"
        pp_pretty_type t pp_type t pp_type t pp_type t cond

    | Babs t ->
      begin
        match t with
        | Tbuiltin Bnat -> ()
        | _ ->
          begin
            let pp_body fmt _ =
              match t with
              | Tbuiltin Bint -> Format.fprintf fmt "int(abs(a))"
              | Ttuple [Tbuiltin Bint; Tbuiltin Bnat] -> Format.fprintf fmt "(int(abs(a.0)),a.1)"
              | _ -> assert false
            in
            Format.fprintf fmt
              "function abs_%a (const a : %a) : %a is %a@\n"
              pp_pretty_type t pp_type t pp_type t pp_body ()
          end
      end

    | Bconcat t ->
      let body =
        match t with
        | Tbuiltin Bstring -> "a ^ b"
        | Tbuiltin Bbytes -> "bytes_concat(a, b)"
        | _ -> assert false
      in
      Format.fprintf fmt "function concat_%a (const a : %a; const b : %a) : %a is %s@\n"
        pp_pretty_type t
        pp_type t
        pp_type t
        pp_type t
        body

    | Bslice  _ -> ()

    | Blength _ -> ()

    | Bisnone t ->
      Format.fprintf fmt "function isnone_%a (const a : option(%a)) : bool is @\n  \
                          block {@\n    \
                          var res : bool := False;@\n    \
                          case a of@\n      \
                          None -> res := True@\n    \
                          | Some (s) -> res := False@\n    \
                          end@\n  \
                          } with res@\n"
        pp_pretty_type t pp_type t

    | Bissome t ->
      Format.fprintf fmt "function issome_%a (const a : option(%a)) : bool is@\n  \
                          block {@\n    \
                          var res : bool := False;@\n    \
                          case a of@\n      \
                          None -> res := False@\n    \
                          | Some (s) -> res := True@\n    \
                          end@\n  \
                          } with res@\n"
        pp_pretty_type t pp_type t

    | Boptget t ->
      Format.fprintf fmt
        "function getopt_%a (const a : option(%a)) : %a is@\n  \
         block { skip } with (case a of@\n      \
         None -> (failwith (\"getopt_%a: argument is none\") : %a)@\n      \
         | Some (s) -> s@\n    \
         end)@\n"
        pp_pretty_type t pp_type t pp_type t
        pp_pretty_type t pp_type t

    | Bfloor    -> Format.fprintf fmt "function floor (const r : %a) : int is block {skip} with r.0 / int(r.1)@\n" pp_type Utils.type_rational
    | Bceil     -> Format.fprintf fmt "function ceil (const r : %a) : int is block {skip} with r.0 / int(r.1) + (if r.0 mod r.1 = 0n then 0n else 1n)@\n" pp_type Utils.type_rational
    | Btostring t ->
      Format.fprintf fmt
        "function to_string_%a (const n : %a) : string is@\n  \
         block {@\n    \
         const res : string = \"0\";@\n    \
         if (n > 0n) then block {@\n      \
         var tmp : string := \"\";@\n      \
         var v : nat := n;@\n      \
         const m : map(nat, string) = (map [@\n        \
         0n -> \"0\";@\n        \
         1n -> \"1\";@\n        \
         2n -> \"2\";@\n        \
         3n -> \"3\";@\n        \
         4n -> \"4\";@\n        \
         5n -> \"5\";@\n        \
         6n -> \"6\";@\n        \
         7n -> \"7\";@\n        \
         8n -> \"8\";@\n        \
         9n -> \"9\";@\n        \
         ] : map(nat, string));@\n      \
         while (v > 0n) block {@\n        \
         const i : nat = v mod 10n;@\n        \
         const str : string = get_force(i, m);@\n        \
         tmp := String.concat(str, tmp);@\n        \
         v := v / 10n;@\n      \
         };@\n    \
         res := tmp;@\n    \
         } else skip;@\n  \
         } with res@\n"
        pp_pretty_type t
        pp_type t

    | Bfail t ->
      Format.fprintf fmt
        "function fail_%a(const v : %a) : unit is@\n  \
         block {@\n  \
         const f : (%a -> unit) = [%%Michelson ({| { FAILWITH } |} : %a -> unit)];@\n  \
         } with f(v)@\n"
        pp_pretty_type t
        pp_type t
        pp_type t pp_type t

  in

  let pp_api_internal (_env : env) fmt = function
    | RatEq        ->
      Format.fprintf fmt
        "function rat_eq (const lhs : %a; const rhs : %a) : bool is@\n  \
         block {skip} with@\n  \
         lhs.0 * int(rhs.1) = rhs.0 * int(lhs.1)@\n"
        pp_type Utils.type_rational
        pp_type Utils.type_rational
    | RatCmp       ->
      Format.fprintf fmt
        "type op_cmp is@\n\
         | OpCmpLt of unit@\n\
         | OpCmpLe of unit@\n\
         | OpCmpGt of unit@\n\
         | OpCmpGe of unit@\n\
         @\n\
         function rat_cmp (const op : op_cmp; const lhs : %a; const rhs : %a) : bool is@\n  \
         begin@\n    \
         const a : int = lhs.0 * int(rhs.1);@\n    \
         const b : int = rhs.0 * int(lhs.1);@\n  \
         end with (case op of@\n    \
         | OpCmpLt -> a <  b@\n    \
         | OpCmpLe -> a <= b@\n    \
         | OpCmpGt -> a >  b@\n    \
         | OpCmpGe -> a >= b@\n    \
         end)@\n"
        pp_type Utils.type_rational
        pp_type Utils.type_rational
    | RatArith     ->
      Format.fprintf fmt
        "type op_arith is@\n\
         | OpArithPlus  of unit@\n\
         | OpArithMinus of unit@\n\
         | OpArithMult  of unit@\n\
         | OpArithDiv   of unit@\n\
         @\n\
         function rat_arith (const op : op_arith; const lhs : %a; const rhs : %a) : %a is@\n  \
         begin@\n    \
         const r : %a =@\n    \
         case op of@\n    \
         | OpArithPlus  -> (lhs.0 * int(rhs.1) + rhs.0 * int(lhs.1), lhs.1 * rhs.1)@\n    \
         | OpArithMinus -> (lhs.0 * int(rhs.1) - rhs.0 * int(lhs.1), lhs.1 * rhs.1)@\n    \
         | OpArithMult  -> (lhs.0 * rhs.0, lhs.1 * rhs.1)@\n    \
         | OpArithDiv   -> (lhs.0 * (if rhs.0 >= 0 then int(rhs.1) else -(int(rhs.1))), lhs.1 * abs(rhs.0))@\n    \
         end@\n  \
         end with r@\n"
        pp_type Utils.type_rational
        pp_type Utils.type_rational
        pp_type Utils.type_rational
        pp_type Utils.type_rational
    | RatUminus ->
      Format.fprintf fmt
        "function rat_uminus (const x : %a) : %a is (- x.0, x.1)@\n"
        pp_type Utils.type_rational
        pp_type Utils.type_rational
    | RatTez ->
      Format.fprintf fmt
        "function rat_tez (const c : %a; const t : tez) : tez is@\n\
         begin@\n  \
         const r : tez = abs(c.0) * t / c.1;@\n  \
         end with r@\n"
        pp_type Utils.type_rational
    | RatDur ->
      Format.fprintf fmt
        "function rat_dur (const c : %a; const d : int) : int is@\n\
         begin@\n  \
         skip;@\n  \
         end with (c.0 * d / int(c.1))@\n"
        pp_type Utils.type_rational
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

    | Getter _ -> emit_error (UnsupportedTerm ("Function"))

    | Function (fs, ret) ->
      Format.fprintf fmt
        "function %a(const %s : storage_type%a) : %a is@\n  \
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
        pp_type ret
        (pp_mterm env) fs.body
        const_result
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

  let pp_storage_term env fmt _ =
    let l = List.filter (fun (x : storage_item) -> not x.const) model.storage in
    match l with
    | []  -> pp_str fmt "Unit"
    | _   ->
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

  let env = compute_env model in
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
