open Location
open Ident
open Tools

module A = Ast
module M = Model

exception Anomaly
type error_desc =
  | CannotConvertToAssignOperator
  | CannotExtractBody
  | AnyNotAuthorizedInTransitionTo
  | NoRemoveAllOnCollection
  | RecordNotFound of ident
[@@deriving show {with_path = false}]

type error = Location.t * error_desc

let pp_error_desc fmt = function
  | CannotConvertToAssignOperator  -> Format.fprintf fmt "cannot convert to assign operator"
  | CannotExtractBody              -> Format.fprintf fmt "cannot extract body"
  | AnyNotAuthorizedInTransitionTo -> Format.fprintf fmt "any not authorized in transition to"
  | NoRemoveAllOnCollection        -> Format.fprintf fmt "remove all cannot be called for a collection of asset"
  | RecordNotFound id              -> Format.fprintf fmt "record not found: %s" id

let emit_error (lc, error : Location.t * error_desc) =
  let str : string = Format.asprintf "%a@." pp_error_desc error in
  let pos : Position.t list = [location_to_position lc] in
  Error.error_alert pos str (fun _ -> ())

let bailout = fun () -> raise (Error.Stop 5)

let unloc_longident (lid : A.longident) : ident = unloc (snd lid)

let longident_to_lident (lid : A.longident) : M.lident = snd lid

type env = {
  formula: bool;
  asset_name: ident option;
  function_p: (M.mident * (M.mident * M.type_ * M.mterm option) list) option
}
[@@deriving show {with_path = false}]

let mk_env ?(formula=false) ?asset_name ?function_p () =
  { formula; asset_name; function_p }

let rec to_model ((_tenv, ast) : Typing.env * A.ast) : M.model =

  let is_current_namespace nm = String.equal "" (unloc nm) || String.equal (unloc ast.name) (unloc nm) in
  let get_namespace nm = if is_current_namespace nm then None else Some nm in
  let to_mident ((nm, id) : A.longident) : M.mident =
    let namespace = get_namespace nm in
    M.mk_mident ?namespace id
  in

  let vtyp_to_btyp = function
    | A.VTunit         -> M.Bunit
    | A.VTbool         -> M.Bbool
    | A.VTnat          -> M.Bnat
    | A.VTint          -> M.Bint
    | A.VTrational     -> M.Brational
    | A.VTdate         -> M.Bdate
    | A.VTduration     -> M.Bduration
    | A.VTstring       -> M.Bstring
    | A.VTaddress      -> M.Baddress
    | A.VTcurrency     -> M.Btez
    | A.VTsignature    -> M.Bsignature
    | A.VTkey          -> M.Bkey
    | A.VTkeyhash      -> M.Bkeyhash
    | A.VTbytes        -> M.Bbytes
    | A.VTchainid      -> M.Bchainid
    | A.VTbls12_381_fr -> M.Bbls12_381_fr
    | A.VTbls12_381_g1 -> M.Bbls12_381_g1
    | A.VTbls12_381_g2 -> M.Bbls12_381_g2
    | A.VTnever        -> M.Bnever
    | A.VTchest        -> M.Bchest
    | A.VTchest_key    -> M.Bchest_key
  in

  let rec type_to_type (t : A.type_) : M.type_ =
    let f = function
      | A.Tnamed _                        -> assert false
      | A.Tasset  id                      -> M.Tasset     (to_mident id)
      | A.Trecord id                      -> M.Trecord    (to_mident id)
      | A.Tenum   id                      -> M.Tenum      (to_mident id)
      | A.Tevent  id                      -> M.Tevent     (to_mident id)
      | A.Tbuiltin b                      -> M.Tbuiltin   (vtyp_to_btyp b)
      | A.Tcontainer (t, Collection)      -> M.Tcontainer (type_to_type t, Collection)
      | A.Tcontainer (t, Aggregate)       -> M.Tcontainer (type_to_type t, Aggregate)
      | A.Tcontainer (t, Partition)       -> M.Tcontainer (type_to_type t, Partition)
      | A.Tcontainer (t, AssetContainer)  -> M.Tcontainer (type_to_type t, AssetContainer)
      | A.Tcontainer (t, AssetKey)        -> M.Tcontainer (type_to_type t, AssetKey)
      | A.Tcontainer (t, AssetValue)      -> M.Tcontainer (type_to_type t, AssetValue)
      | A.Tcontainer (t, AssetView)       -> M.Tcontainer (type_to_type t, View)
      | A.Tset t                          -> M.Tset (type_to_type t)
      | A.Tlist t                         -> M.Tlist (type_to_type t)
      | A.Tmap (k, v)                     -> M.Tmap (type_to_type k, type_to_type v)
      | A.Tbig_map (k, v)                 -> M.Tbig_map (type_to_type k, type_to_type v)
      | A.Titerable_big_map (k, v)        -> M.Titerable_big_map (type_to_type k, type_to_type v)
      | A.Tor (l, r)                      -> M.Tor (type_to_type l, type_to_type r)
      | A.Tlambda (a, r)                  -> M.Tlambda (type_to_type a, type_to_type r)
      | A.Ttuple l                        -> M.Ttuple (List.map type_to_type l)
      | A.Toperation                      -> M.Toperation
      | A.Tcontract t                     -> M.Tcontract (type_to_type t)
      | A.Toption t                       -> M.Toption (type_to_type t)
      | A.Tticket t                       -> M.Tticket (type_to_type t)
      | A.Tsapling_state n                -> M.Tsapling_state n
      | A.Tsapling_transaction n          -> M.Tsapling_transaction n
    in
    M.mktype (f t)
  in

  let to_assignment_operator = function
    | A.ValueAssign  -> M.ValueAssign
    | A.PlusAssign   -> M.PlusAssign
    | A.MinusAssign  -> M.MinusAssign
    | A.MultAssign   -> M.MultAssign
    | A.DivAssign    -> M.DivAssign
    | A.AndAssign    -> M.AndAssign
    | A.OrAssign     -> M.OrAssign
  in

  let to_pattern_node (n : A.pattern_node) : M.pattern_node =
    match n with
    | A.Mconst (id, xs) -> M.Pconst (M.mk_mident id, xs)
    | A.Mwild -> M.Pwild
  in

  let to_pattern (p : A.pattern) : M.pattern =
    let node = to_pattern_node p.node in
    M.mk_pattern node ~loc:p.loc
  in

  let to_comparison (op : A.comparison_operator) : M.comparison_operator =
    match op with
    | Gt -> Gt
    | Ge -> Ge
    | Lt -> Lt
    | Le -> Le
    | _ -> assert false
  in

  let term_arg_to_expr : 't. (A.pterm -> M.mterm) -> (A.pterm_arg) -> M.mterm =
    fun f a ->
      match a with
      | A.AExpr x -> f x
      | _ -> assert false
      (*| A.AEffect l -> M.AEffect (List.map (fun (id, op, term) -> (id, to_assignment_operator2 op, f term)) l)
        | A.AFun _ -> assert false (* TODO *)*)
  in

  let fail (ft : M.fail_type) : M.mterm =
    M.mk_mterm (Mfail ft) M.tunit
  in

  let term_not x : M.mterm =
    M.mk_mterm (M.Mnot x) M.tbool
  in

  (* let unit : M.mterm = M.mk_mterm (M.Mseq []) M.Tunit in *)
  (* let is_list (mt : mterm) =
     match mt with
     | {type_ = Tcontainer (a, _); _} -> unloc asset_name
     | _ ->
      Format.printf "extract_asset_name error: %a@\n" M.pp_type_ mterm.type_;
      assert false
     in *)

  let is_asset_container (v : A.pterm) : bool =
    match v with
    | {type_ = Some (Tcontainer (Tasset _, _)); _} -> true
    | _ -> false
  in

  let is_list (v : A.pterm) : bool =
    match v with
    | {type_ = Some (Tlist _); _} -> true
    | _ -> false
  in

  let extract_asset_name_from_type (t : M.type_) : ident =
    match M.get_ntype t with
    | M.Tcontainer ((Tasset asset_name, _), _) -> M.unloc_mident asset_name
    | _ -> assert false
  in

  let extract_asset_name (mterm : M.mterm) : ident =
    extract_asset_name_from_type mterm.type_
  in

  let _extract_field_name (_id, _type_, body : A.lident * A.ptyp * A.pterm) : M.lident =
    match body.node with
    | A.Pdot (_, fn) -> fn
    | _ ->
      Format.printf "extract_field_name error: %a@\n" A.pp_pterm body;
      assert false
  in

  let extract_builtin_type_list (v : M.mterm) : M.type_ =
    match v with
    | {type_ = (Tlist t, _); _} -> t
    | _ -> assert false
  in

  let extract_builtin_type_set (v : M.mterm) : M.type_ =
    match v with
    | {type_ = (Tset t, _); _} -> t
    | _ -> assert false
  in

  let extract_builtin_type_map (v : M.mterm) : M.map_kind * M.type_ * M.type_ =
    match v with
    | {type_ = (Tmap (k, v), _); _}              -> MKMap, k, v
    | {type_ = (Tbig_map (k, v), _); _}          -> MKBigMap, k, v
    | {type_ = (Titerable_big_map (k, v), _); _} -> MKIterableBigMap, k, v
    | _ -> assert false
  in

  let to_ck (env : env) (fp : M.mterm) : M.container_kind =
    match fp.node, fp.type_ with
    | M.Mdotassetfield (an, _k, fn), (Tcontainer ((Tasset _, _), (Aggregate | Partition)), _) -> M.CKfield (M.unloc_mident an, M.unloc_mident fn, fp)
    | M.Mdot ({type_ = (Tasset an, _)}, fn), (Tcontainer ((Tasset _, _), (Aggregate | Partition)), _) -> M.CKfield (M.unloc_mident an, M.unloc_mident fn, fp)
    | M.Mvar (fn, _), (Tcontainer ((Tasset _, _), (Aggregate | Partition)), _) -> begin
        let an = match env.asset_name with
          | Some v -> v
          | None -> assert false
        in
        M.CKfield (an, M.unloc_mident fn, fp)
      end
    | M.Mvar (_, _), (Tcontainer ((Tasset _, _), Collection), _) -> M.CKcoll
    | _, (Tcontainer ((Tasset _, _), Collection), _) -> M.CKcoll
    | _ -> M.CKview fp
  in

  let is_param env (id : M.mident) =
    match env.function_p with
    | Some (_, l) -> l |> List.map proj3_1 |> List.exists (fun x -> String.equal (M.unloc_mident id) (M.unloc_mident x))
    | _ -> false
  in

  let get_enum_type_opt = function | Some (A.Tenum id) -> Some id | _ -> None in

  let is_enum_type a id =
    match get_enum_type_opt a with
    | None -> false
    | Some lid -> begin
        let event_id = to_mident lid in
        let oast =
          match fst event_id with
          | Some v -> begin
              let a = Typing.Env.Import.get _tenv (unloc v) in
              (match a.id_ast with
               | Some a -> Some a
               | _ -> None)
            end
          | None -> Some ast
        in
        match oast with
        | Some ast -> begin
            let denum = List.fold_left (fun accu v ->
                match v with
                | A.Denum e when String.equal (match e.kind with | EKenum lid -> unloc_longident lid | EKstate _ -> "$state") (M.unloc_mident event_id) -> Some e
                | _ -> accu
              ) None ast.decls in
            match denum with
            | Some enum -> begin
                List.exists (fun (x : A.enum_item_struct) -> String.equal id (unloc x.name)) enum.items
              end
            | None -> false
          end
        | None -> false
      end
  in

  let get_enum_type a = a |> get_enum_type_opt |> Option.get in

  let rec to_mterm (env : env) (pterm : A.pterm) : M.mterm =
    let is_record t = match M.get_ntype t with | M.Trecord _ | M.Tevent _ -> true | _ -> false in
    let type_ = type_to_type (Option.get pterm.type_) in
    let f x = to_mterm env x in
    let node =
      match pterm.node with
      | A.Pif (c, t, e)                     -> M.Mexprif        (f c, f t, f e)
      | A.Pmatchwith (m, l)                 -> M.Mexprmatchwith (f m, List.map (fun (p, e) -> (to_pattern p, f e)) l)
      | A.Pmatchoption (x, id, ve, ne)      -> M.Mmatchoption   (f x, M.mk_mident id, f ve, f ne)
      | A.Pmatchor (x, lid, le, rid, re)    -> M.Mmatchor       (f x, M.mk_mident lid, f le, M.mk_mident rid, f re)
      | A.Pmatchlist (x, hid, tid, hte, ee) -> M.Mmatchlist     (f x, M.mk_mident hid, M.mk_mident tid, f hte, f ee)
      | A.Pfold (x, i, e)                   -> M.Mfold          (f x, M.mk_mident i, f e)
      | A.Pmap (x, i, e)                    -> M.Mmap           (f x, M.mk_mident i, f e)
      | A.Plogical (A.And, l, r)            -> M.Mand           (f l, f r)
      | A.Plogical (A.Or, l, r)             -> M.Mor            (f l, f r)
      | A.Plogical (A.Xor, l, r)            -> M.Mxor           (f l, f r)
      | A.Pnot e                            -> M.Mnot           (f e)
      | A.Pmulticomp (e, l)                 -> M.Mmulticomp     (f e, List.map (fun (op, e) -> (to_comparison op, f e)) l)
      | A.Pcomp (A.Equal, l, r)             -> let l = f l in M.Mequal  (l.type_, l, f r)
      | A.Pcomp (A.Nequal, l, r)            -> let l = f l in M.Mnequal (l.type_, l, f r)
      | A.Pcomp (A.Gt, l, r)                -> M.Mgt            (f l, f r)
      | A.Pcomp (A.Ge, l, r)                -> M.Mge            (f l, f r)
      | A.Pcomp (A.Lt, l, r)                -> M.Mlt            (f l, f r)
      | A.Pcomp (A.Le, l, r)                -> M.Mle            (f l, f r)
      | A.Parith (A.Plus, l, r)             -> M.Mplus          (f l, f r)
      | A.Parith (A.Minus, l, r)            -> M.Mminus         (f l, f r)
      | A.Parith (A.Mult, l, r)             -> M.Mmult          (f l, f r)
      | A.Parith (A.DivRat, l, r)           -> M.Mdivrat        (f l, f r)
      | A.Parith (A.DivEuc, l, r)           -> M.Mdiveuc        (f l, f r)
      | A.Parith (A.Modulo, l, r)           -> M.Mmodulo        (f l, f r)
      | A.Parith (A.DivMod, l, r)           -> M.Mdivmod        (f l, f r)
      | A.Puarith (A.Uminus, e)             -> begin
          match f e with
          | {node = Mint n } -> M.Mint (Big_int.minus_big_int n)
          | v -> M.Muminus v
        end
      | A.Parith (A.ThreeWayCmp, l, r)      -> M.MthreeWayCmp   (f l, f r)
      | A.Parith (A.ShiftLeft, l, r)        -> M.Mshiftleft     (f l, f r)
      | A.Parith (A.ShiftRight, l, r)       -> M.Mshiftright    (f l, f r)
      | A.Precord l when is_record type_    -> begin
          let record_name : M.mident =  match M.get_ntype type_ with | M.Trecord name | M.Tevent name -> name | _ -> assert false in
          let w_ast = match fst record_name with | None -> ast | Some id -> (let import =  Typing.Env.Import.get _tenv (unloc id) in Option.get import.id_ast)
          in
          let ids, k =
            List.fold_left (fun accu (x : A.decl_) ->
                match x with
                | A.Drecord r when String.equal (unloc_longident r.name) (M.unloc_mident record_name) -> (List.map (fun (x : A.lident A.decl_gen) -> unloc x.name) r.fields, `Record)
                | A.Devent  r when String.equal (unloc_longident r.name) (M.unloc_mident record_name) -> (List.map (fun (x : A.lident A.decl_gen) -> unloc x.name) r.fields, `Event)
                | _ -> accu) ([], `None) w_ast.decls
          in
          if List.length ids <> List.length l
          then (emit_error (pterm.loc, RecordNotFound (M.unloc_mident record_name)); bailout ());
          match k with
          | `Record -> M.Mlitrecord (List.map2 (fun x y -> x, f y) ids l)
          | `Event  -> M.Mlitevent  (List.map2 (fun x y -> x, f y) ids l)
          | `None   -> assert false
        end
      | A.Precord l                         -> M.Masset         (List.map f l)
      | A.Precupdate (e, l)                 -> M.Mrecupdate     (f e, List.map (fun (id, v) -> unloc id, f v) l)
      | A.Pletin (id, init, typ, body, o)   -> M.Mletin         ([M.mk_mident id], LVsimple (f init), Option.map type_to_type typ, f body, Option.map f o)
      | A.Pdeclvar (i, t, v, c)             -> M.Mdeclvar       ([M.mk_mident  i], Option.map type_to_type t, f v, c)

      (* enum value *)
      | A.Pvar id when is_enum_type pterm.type_ (unloc_longident id) -> M.Menumval (to_mident id, [], to_mident (get_enum_type pterm.type_))
      | A.Pcall (_, Cid id, [], args) when is_enum_type pterm.type_ (unloc id) -> M.Menumval (M.mk_mident id, List.map (function | A.AExpr x -> f x | _ -> assert false) args, to_mident (get_enum_type pterm.type_))


      | A.Pvar ((_, { pldesc = "state" }))           -> M.Mvar (M.mk_mident (dumloc ""), Vstate)
      | A.Pvar (id) when is_param env (to_mident id) -> M.Mvar (to_mident id, Vparam)
      | A.Pvar (id) when A.Utils.is_variable ast id  -> M.Mvar (to_mident id, Vstorevar)
      | A.Pvar (id) when A.Utils.is_asset ast (longident_to_lident id)      -> M.Mvar (to_mident id, Vstorecol)
      | A.Pvar (id) when A.Utils.is_parameter ast (longident_to_lident id)  -> M.Mvar (to_mident id, Vparameter)
      | A.Pvar (id)                                   -> M.Mvar (to_mident id, Vlocal)
      | A.Parray l ->
        begin
          let l = List.map f l in
          match M.get_ntype type_ with
          | Tcontainer ((Tasset _, _), _)   -> M.Massets l
          | Tset _ -> M.Mlitset l
          | Tmap ( _, _) -> M.Mlitmap (MKMap, List.map (fun (x : M.mterm) -> match x.node with | M.Mtuple [k; v] -> (k, v)  | _ -> assert false) l)
          | Tbig_map ( _, _) -> M.Mlitmap (MKBigMap, List.map (fun (x : M.mterm) -> match x.node with | M.Mtuple [k; v] -> (k, v)  | _ -> assert false) l)
          | Titerable_big_map (_, _) -> M.Mlitmap (MKIterableBigMap, List.map (fun (x : M.mterm) -> match x.node with | M.Mtuple [k; v] -> (k, v)  | _ -> assert false) l)
          | _ -> M.Mlitlist l
        end
      | A.Plit ({node = BVint i; _})                  -> M.Mint i
      | A.Plit ({node = BVnat i; _})                  -> M.Mnat i
      | A.Plit ({node = BVbool b; _})                 -> M.Mbool b
      | A.Plit ({node = BVrational (d, n); _})        -> M.Mrational (d, n)
      | A.Plit ({node = BVdate s; _})                 -> M.Mdate s
      | A.Plit ({node = BVstring s; _})               -> M.Mstring s
      | A.Plit ({node = BVcurrency (_c, v); _})       -> M.Mmutez v
      | A.Plit ({node = BVaddress s; _})              -> M.Maddress s
      | A.Plit ({node = BVduration d; _})             -> M.Mduration d
      | A.Plit ({node = BVbytes v; _})                -> M.Mbytes v
      | A.Plit ({node = BVunit; _})                   -> M.Munit


      | A.Pdot (e, id) -> begin
          match e with
          | {node = Pcall (Some a, Cconst Cget, [], [AExpr k])} -> begin
              let b = f a in
              match M.get_ntype b.type_ with
              | M.Tcontainer ((Tasset an, _), Collection) -> M.Mdotassetfield (an, f k, M.mk_mident id)
              | _ -> M.Mdot (f e, M.mk_mident id)
            end

          | _ ->
            (* handle dot contract too *)
            M.Mdot (f e, M.mk_mident id)
        end
      (* | A.Pquestion (e, id, edv) -> assert false *)

      | A.Pquestiondot (e, id) -> M.Mquestionoption (f e, M.mk_mident id)

      | A.Pconst Cstate                        -> M.Mvar(M.mk_mident (dumloc ""), Vstate)
      | A.Pconst Cnow                          -> M.Mnow
      | A.Pconst Ctransferred                  -> M.Mtransferred
      | A.Pconst Ccaller                       -> M.Mcaller
      | A.Pconst Cbalance                      -> M.Mbalance
      | A.Pconst Csource                       -> M.Msource
      | A.Pconst Cselfaddress                  -> M.Mselfaddress
      | A.Pconst Cselfchainid                  -> M.Mselfchainid
      | A.Pconst Coperations                   -> M.Moperations
      | A.Pconst Cmetadata                     -> M.Mmetadata
      | A.Pconst Ctotalvotingpower             -> M.Mtotalvotingpower
      | A.Pconst Clevel                        -> M.Mlevel
      | A.Pconst Cminblocktime                 -> M.Mminblocktime
      | A.Pconst c                             ->
        Format.eprintf "expr const unkown: %a@." A.pp_const c;
        assert false

      | A.Ptuple l                             -> M.Mtuple (List.map f l)
      | A.Ptupleaccess (p, idx)                -> M.Mtupleaccess (f p, idx)
      | A.Pnone                                -> M.Mnone
      | A.Psome a                              -> M.Msome (f a)
      | A.Pleft (t, x)                         -> M.Mleft (type_to_type t, f x)
      | A.Pright (t, x)                        -> M.Mright (type_to_type t, f x)
      | A.Plambda (rt, id, at, e)              -> M.Mlambda (type_to_type rt, M.mk_mident id, type_to_type at, f e)
      | A.Plambda_michelson (it, rt, body)     -> M.Mlambda_michelson (type_to_type it, type_to_type rt, body)
      | A.Pmicheline_expr (t, m, a)            -> M.Mmicheline_expr (type_to_type t, m, List.map f a)
      | A.Pcast (src, dst, v)                  -> begin
          let v = f v in
          match src, dst, v with
          | A.Tbuiltin VTnat, A.Tbuiltin VTint, { node = Mnat v; _} -> M.Mint v
          | A.Tbuiltin VTnat, A.Tbuiltin VTint, _                  -> M.Mnattoint v
          | A.Tbuiltin VTnat, A.Tbuiltin VTrational, _             -> M.Mnattorat v
          | A.Tbuiltin VTint, A.Tbuiltin VTrational, _             -> M.Minttorat v
          | A.Tbuiltin VTbls12_381_fr, A.Tbuiltin VTint, _         -> M.Mnattoint v
          (* | A.Tbuiltin VTbls12_381_fr, A.Tbuiltin VTint, _         -> M.Mnattoint v *)
          | A.Tbuiltin VTstring, A.Tbuiltin VTchainid,      { node = Mstring v; _} -> M.Mchain_id v
          | A.Tbuiltin VTstring, A.Tbuiltin VTkey,          { node = Mstring v; _} -> M.Mkey v
          | A.Tbuiltin VTstring, A.Tbuiltin VTkeyhash,      { node = Mstring v; _} -> M.Mkey_hash v
          | A.Tbuiltin VTstring, A.Tbuiltin VTsignature,    { node = Mstring v; _} -> M.Msignature v
          | A.Tbuiltin VTbytes,  A.Tbuiltin VTbls12_381_fr, { node = Mbytes v; _}  -> M.Mbls12_381_fr v
          | A.Tbuiltin VTnat,    A.Tbuiltin VTbls12_381_fr, { node = Mnat v; _}    -> M.Mbls12_381_fr_n v
          | A.Tbuiltin VTint,    A.Tbuiltin VTbls12_381_fr, { node = Mint v; _}    -> M.Mbls12_381_fr_n v
          | A.Tbuiltin VTbytes,  A.Tbuiltin VTbls12_381_g1, { node = Mbytes v; _}  -> M.Mbls12_381_g1 v
          | A.Tbuiltin VTbytes,  A.Tbuiltin VTbls12_381_g2, { node = Mbytes v; _}  -> M.Mbls12_381_g2 v
          | A.Tbuiltin VTbytes,  A.Tsapling_transaction n,  { node = Mbytes v; _}  -> M.MsaplingTransaction (n, v)
          | A.Tbuiltin VTbytes,  A.Tbuiltin VTchest,        { node = Mbytes v; _}  -> M.Mchest v
          | A.Tbuiltin VTbytes,  A.Tbuiltin VTchest_key,    { node = Mbytes v; _}  -> M.Mchest_key v
          | _ -> M.Mcast (type_to_type src, type_to_type dst, v)
        end
      | A.Pself id -> M.Mself (M.mk_mident id)

      | A.Pternary (c, a, b) -> begin
          let c = f c in
          let a = f a in
          let b = f b in
          match c with
          | {type_ = (M.Tbuiltin Bbool, _)} -> M.Mternarybool   (c, a, b)
          | {type_ = (M.Toption _, _)}      -> M.Mternaryoption (c, a, b)
          | _ -> assert false
        end

      | A.Pcreatecontract (okh, amount, cct) -> begin
          let cc =
            match cct with
            | A.CCTz (ms, arg_storage) -> M.CCTz ({ ms_content = ms.ms_content }, f arg_storage)
            | A.CCArl (id, args) -> M.CCArl (id, List.map (fun (id, v) -> (id, f v)) args)
          in
          M.Mcreatecontract (cc, f okh, f amount)
        end
      | A.Ptz_expr s -> M.Mtz_expr s

      (* | A.Pcall (Some p, A.Cconst A.Cbefore,    []) -> M.Msetbefore    (f p) *)
      (* | A.Pcall (Some p, A.Cconst A.Cunmoved,   []) -> M.Msetunmoved   (f p)
         | A.Pcall (Some p, A.Cconst A.Cadded,     []) -> M.Msetadded     (f p)
         | A.Pcall (Some p, A.Cconst A.Cremoved,   []) -> M.Msetremoved   (f p) *)

      (* Asset *)

      | A.Pcall (Some p, A.Cconst (A.Cget), [], [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Mget (asset_name, to_ck env fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Cgetopt), [], [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Mgetsome (asset_name, to_ck env fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Cselect), [], [AFun (_id, _type, l, q)]) when is_asset_container p ->
        let fp = f p in
        let lambda_body = f q in
        let asset_name = extract_asset_name fp in
        let lambda_args, args = List.fold_right (fun (x, y, z) (l1, l2) -> ((unloc x, type_to_type y)::l1, (f z)::l2)) l ([], []) in
        M.Mselect (asset_name, to_ck env fp, lambda_args, lambda_body, args)

      | A.Pcall (Some p, A.Cconst (A.Csort), [], args) when is_asset_container p ->
        let fp = f p in
        let asset_name = extract_asset_name fp in
        let args =
          List.map (fun x -> match x with
              | A.ASorting (asc, field_name) ->
                begin
                  let sort_kind = match asc with | true -> M.SKasc | false -> M.SKdesc in
                  unloc field_name, sort_kind
                end
              | _ -> assert false) args
        in
        M.Msort (asset_name, to_ck env fp, args)

      | A.Pcall (Some p, A.Cconst (A.Ccontains), [], [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Mcontains (asset_name, to_ck env fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Cnth), [], [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Mnth (asset_name, to_ck env fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Ccount), [], []) when is_asset_container p ->
        let fp = f p in
        let asset_name = extract_asset_name fp in
        M.Mcount (asset_name, to_ck env fp)

      | A.Pcall (Some p, A.Cconst (A.Csum), [], [AFun (_qi, _qt, _l, q)]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Msum (asset_name, to_ck env fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Chead), [], [AExpr e]) when is_asset_container p ->
        let fp = f p in
        let fe = f e in
        let asset_name = extract_asset_name fp in
        M.Mhead (asset_name, to_ck env fp, fe)

      | A.Pcall (Some p, A.Cconst (A.Ctail), [], [AExpr e]) when is_asset_container p ->
        let fp = f p in
        let fe = f e in
        let asset_name = extract_asset_name fp in
        M.Mtail (asset_name, to_ck env fp, fe)


      | A.Pcall (None, A.Cconst (A.CmakeAsset), [A.Tasset an], [AExpr k; AExpr v]) ->
        (* let vt = ft t in *)
        let fk = f k in
        let fv = f v in
        M.Mmakeasset (unloc (longident_to_lident an), fk, fv)

      | A.Pcall (Some p, A.Cconst (A.CtoContainer), [], []) ->
        let fp = f p in
        let asset_name = extract_asset_name fp in
        M.Mtocontainer asset_name

      (* Set*)

      | A.Pcall (None, A.Cconst (A.Csadd), [], [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_set fp in
        M.Msetadd (t, fp, fq)

      | A.Pcall (None, A.Cconst (A.Csremove), [], [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_set fp in
        M.Msetremove (t, fp, fq)

      | A.Pcall (None, A.Cconst (A.Csupdate), [], [AExpr p; AExpr q; AExpr r]) ->
        let fp = f p in
        let fq = f q in
        let fr = f r in
        let t = extract_builtin_type_set fp in
        M.Msetupdate (t, fp, fq, fr)

      | A.Pcall (None, A.Cconst (A.Cscontains), [], [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_set fp in
        M.Msetcontains (t, fp, fq)

      | A.Pcall (None, A.Cconst (A.Cslength), [], [AExpr p]) ->
        let fp = f p in
        let t = extract_builtin_type_set fp in
        M.Msetlength (t, fp)


      (* List*)

      | A.Pcall (None, A.Cconst (A.Cprepend), [], [AExpr p; AExpr q]) when is_list p -> (
          let fp = f p in
          let fq = f q in
          let t = extract_builtin_type_list fp in
          M.Mlistprepend (t, fp, fq)
        )

      | A.Pcall (None, A.Cconst (A.Clength), [], [AExpr p]) when is_list p ->
        let fp = f p in
        let t = extract_builtin_type_list fp in
        M.Mlistlength (t, fp)

      | A.Pcall (None, A.Cconst (A.Ccontains), [], [AExpr p; AExpr q]) when is_list p ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_list fp in
        M.Mlistcontains (t, fp, fq)

      | A.Pcall (None, A.Cconst (A.Cnth), [], [AExpr p; AExpr q]) when is_list p ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_list fp in
        M.Mlistnth (t, fp, fq)

      | A.Pcall (None, A.Cconst (A.Creverse), [], [AExpr p]) when is_list p ->
        let fp = f p in
        let t = extract_builtin_type_list fp in
        M.Mlistreverse (t, fp)

      | A.Pcall (None, A.Cconst (A.Cconcat), [], [AExpr p; AExpr q]) when is_list p ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_list fp in
        M.Mlistconcat (t, fp, fq)

      | A.Pcall (None, A.Cconst (A.Chead), [], [AExpr p; AExpr q]) when is_list p -> (
          let fp = f p in
          let fq = f q in
          let t = extract_builtin_type_list fp in
          M.Mlisthead (t, fp, fq)
        )

      | A.Pcall (None, A.Cconst (A.Ctail), [], [AExpr p; AExpr q]) when is_list p -> (
          let fp = f p in
          let fq = f q in
          let t = extract_builtin_type_list fp in
          M.Mlisttail (t, fp, fq)
        )

      (* Map *)

      | A.Pcall (None, A.Cconst (A.Cmput), [], [AExpr p; AExpr q; AExpr r]) ->
        let fp = f p in
        let fq = f q in
        let fr = f r in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmapput (mk, kt, vt, fp, fq, fr)

      | A.Pcall (None, A.Cconst (A.Cmremove), [], [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmapremove (mk, kt, vt, fp, fq)

      | A.Pcall (None, A.Cconst (A.Cmupdate), [], [AExpr p; AExpr q; AExpr r]) ->
        let fp = f p in
        let fq = f q in
        let fr = f r in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmapupdate (mk, kt, vt, fp, fq, fr)

      | A.Pcall (None, A.Cconst (A.Cmget), [], [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmapget (mk, kt, vt, fp, fq, None)

      | A.Pcall (None, A.Cconst (A.Cmgetopt), [], [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmapgetopt (mk, kt, vt, fp, fq)

      | A.Pcall (None, A.Cconst (A.Cmcontains), [], [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmapcontains (mk, kt, vt, fp, fq)

      | A.Pcall (None, A.Cconst (A.Cmlength), [], [AExpr p]) ->
        let fp = f p in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmaplength (mk, kt, vt, fp)


      (* Builtin functions*)

      | A.Pcall (None, A.Cconst A.Cmin, [], [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mmin (fa, fb)

      | A.Pcall (None, A.Cconst A.Cmax, [], [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mmax (fa, fb)

      | A.Pcall (None, A.Cconst A.Cabs, [], [AExpr a]) ->
        let fa = f a in
        M.Mabs (fa)

      | A.Pcall (None, A.Cconst A.Cconcat, [], [AExpr x; AExpr y]) ->
        let fx = f x in
        let fy = f y in
        M.Mconcat (fx, fy)

      | A.Pcall (None, A.Cconst A.Cconcat, [], [AExpr x]) ->
        let fx = f x in
        M.Mconcatlist (fx)

      | A.Pcall (None, A.Cconst A.Cslice, [], [AExpr x; AExpr s; AExpr e]) ->
        let fx = f x in
        let fs = f s in
        let fe = f e in
        M.Mslice (fx, fs, fe)

      | A.Pcall (None, A.Cconst A.Clength, [], [AExpr x]) ->
        let fx = f x in
        M.Mlength (fx)

      | A.Pcall (None, A.Cconst A.Cisnone, [], [AExpr x]) ->
        let fx = f x in
        M.Misnone (fx)

      | A.Pcall (None, A.Cconst A.Cissome, [], [AExpr x]) ->
        let fx = f x in
        M.Missome (fx)

      | A.Pcall (None, A.Cconst A.Cinttonat, [], [AExpr x]) ->
        let fx = f x in
        M.Minttonat (fx)

      | A.Pcall (None, A.Cconst A.Cfloor, [], [AExpr x]) ->
        let fx = f x in
        M.Mfloor (fx)

      | A.Pcall (None, A.Cconst A.Cceil, [], [AExpr x]) ->
        let fx = f x in
        M.Mceil (fx)

      | A.Pcall (None, A.Cconst A.Cnattostring, [], [AExpr x]) ->
        let fx = f x in
        M.Mnattostring fx

      | A.Pcall (None, A.Cconst A.Cbytestonat, [], [AExpr x]) ->
        let fx = f x in
        M.Mbytestonat fx

      | A.Pcall (None, A.Cconst A.Cnattobytes, [], [AExpr x]) ->
        let fx = f x in
        M.Mnattobytes fx

      | A.Pcall (None, A.Cconst A.Cbytestoint, [], [AExpr x]) ->
        let fx = f x in
        M.Mbytestoint fx

      | A.Pcall (None, A.Cconst A.Cinttobytes, [], [AExpr x]) ->
        let fx = f x in
        M.Minttobytes fx

      | A.Pcall (None, A.Cconst A.Cpack, [], [AExpr x]) ->
        let fx = f x in
        M.Mpack (fx)

      | A.Pcall (None, A.Cconst A.Cunpack, [], [AExpr x]) ->
        let fx = f x in
        let t =
          match M.get_ntype type_ with
          | Toption t -> t
          | _ -> assert false
        in
        M.Munpack (t, fx)

      | A.Pcall (None, A.Cconst A.Csetdelegate, [], [AExpr x]) ->
        let fx = f x in
        M.Msetdelegate (fx)

      | A.Pcall (None, A.Cconst A.Ckeyhashtocontract, [], [AExpr x]) ->
        let fx = f x in
        M.Mkeyhashtocontract (fx)

      | A.Pcall (None, A.Cconst A.Csubnat, [], [AExpr x; AExpr y]) ->
        let fx = f x in
        let fy = f y in
        M.Msubnat (fx, fy)

      | A.Pcall (None, A.Cconst A.Csubmutez, [], [AExpr x; AExpr y]) ->
        let fx = f x in
        let fy = f y in
        M.Msubmutez (fx, fy)

      | A.Pcall (None, A.Cconst A.Cblake2b, [], [AExpr x]) ->
        let fx = f x in
        M.Mblake2b (fx)

      | A.Pcall (None, A.Cconst A.Csha256, [], [AExpr x]) ->
        let fx = f x in
        M.Msha256 (fx)

      | A.Pcall (None, A.Cconst A.Csha512, [], [AExpr x]) ->
        let fx = f x in
        M.Msha512 (fx)

      | A.Pcall (None, A.Cconst A.Csha3, [], [AExpr x]) ->
        let fx = f x in
        M.Msha3 (fx)

      | A.Pcall (None, A.Cconst A.Ckeccak, [], [AExpr x]) ->
        let fx = f x in
        M.Mkeccak (fx)

      | A.Pcall (None, A.Cconst A.Ckeytokeyhash, [], [AExpr x]) ->
        let fx = f x in
        M.Mkeytokeyhash (fx)

      | A.Pcall (None, A.Cconst A.Csimplifyrational, [], [AExpr x]) ->
        let fx = f x in
        M.Msimplify_rational (fx)

      | A.Pcall (None, A.Cconst A.Cgetnumerator, [], [AExpr x]) ->
        let fx = f x in
        M.Mget_numerator (fx)

      | A.Pcall (None, A.Cconst A.Cgetdenominator, [], [AExpr x]) ->
        let fx = f x in
        M.Mget_denominator (fx)

      | A.Pcall (None, A.Cconst A.Cglobalconstant, [ty], [AExpr x]) ->
        let fx = f x in
        M.Mglobal_constant (type_to_type ty, fx)

      | A.Pcall (None, A.Cconst A.Cchecksignature, [], [AExpr k; AExpr s; AExpr x]) ->
        let fk = f k in
        let fs = f s in
        let fx = f x in
        M.Mchecksignature (fk, fs, fx)

      | A.Pcall (None, A.Cconst A.Ccontracttoaddress, [], [AExpr x]) ->
        let fx = f x in
        M.Mcontracttoaddress (fx)

      | A.Pcall (None, A.Cconst A.Caddresstocontract, [t], [AExpr x]) ->
        let fx = f x in
        M.Maddresstocontract (type_to_type t, fx)

      | A.Pcall (None, A.Cconst A.Ckeytoaddress, [], [AExpr x]) ->
        let fx = f x in
        M.Mkeytoaddress (fx)

      | A.Pcall (None, A.Cconst A.Cisimplicitaddress, [], [AExpr x]) ->
        let fx = f x in
        M.Misimplicitaddress (fx)

      | A.Pcall (_, A.Cid id, _, args) ->
        M.Mapp (M.mk_mident id, List.map (fun x -> term_arg_to_expr f x) args)

      | A.Pcall (None, A.Cconst A.Cgreedyand, [], [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mgreedyand (fa, fb)

      | A.Pcall (None, A.Cconst A.Cgreedyor, [], [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mgreedyor (fa, fb)

      (* | A.Pcall (None, A.Cconst (A.Cmaybeperformedonlybyrole), [AExpr l; AExpr r]) ->
         M.MsecMayBePerformedOnlyByRole (f l, f r)

         | A.Pcall (None, A.Cconst (A.Cmaybeperformedonlybyaction), [AExpr l; AExpr r]) ->
         M.MsecMayBePerformedOnlyByEntry (f l, f r)

         | A.Pcall (None, A.Cconst (A.Cmaybeperformedbyrole), [AExpr l; AExpr r]) ->
         M.MsecMayBePerformedByRole (f l, f r)

         | A.Pcall (None, A.Cconst (A.Cmaybeperformedbyaction), [AExpr l; AExpr r]) ->
         M.MsecMayBePerformedByEntry (f l, f r) *)


      (* Voting *)

      | A.Pcall (None, A.Cconst A.Cvotingpower, [], [AExpr x]) ->
        let fx = f x in
        M.Mvotingpower (fx)


      (* Ticket *)

      | A.Pcall (None, A.Cconst A.Ccreateticket, [], [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mcreateticket (fa, fb)

      | A.Pcall (None, A.Cconst A.Creadticket, [], [AExpr x]) ->
        let fx = f x in
        M.Mreadticket fx

      | A.Pcall (None, A.Cconst A.Csplitticket, [], [AExpr a; AExpr b; AExpr c]) ->
        let fa = f a in
        let fb = f b in
        let fc = f c in
        M.Msplitticket (fa, fb, fc)

      | A.Pcall (None, A.Cconst A.Cjointickets, [], [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mjointickets (fa, fb)


      (* Sapling *)

      | A.Pcall (None, A.Cconst A.Csapling_empty_state, [], [AExpr x]) -> begin
          let fx = f x in
          match fx.node with
          | M.Mnat n -> M.Msapling_empty_state (Big_int.int_of_big_int n)
          | _ -> assert false
        end

      | A.Pcall (None, A.Cconst A.Csapling_verify_update, [], [AExpr x; AExpr y]) ->
        let fx = f x in
        let fy = f y in
        M.Msapling_verify_update (fx, fy)


      (* Bls curve *)

      | A.Pcall (None, A.Cconst A.Cpairing_check, [], [AExpr x]) ->
        let fx = f x in
        M.Mpairing_check fx


      (* Timelock *)

      | A.Pcall (None, A.Cconst A.Copen_chest, [], [AExpr x; AExpr y; AExpr z]) ->
        let fx = f x in
        let fy = f y in
        let fz = f z in
        M.Mopen_chest(fx, fy, fz)


      (* Operation *)

      | A.Pcall (None, A.Cconst (A.Cmakeoperation), [], [AExpr a; AExpr b; AExpr c]) ->
        let fa = f a in
        let fb = f b in
        let fc = f c in
        M.Mmakeoperation (fa, fb, fc)

      | A.Pcall (None, A.Cconst (A.Cmakeevent), [ty], [AIdent id; AExpr a]) ->
        M.Mmakeevent (type_to_type ty, M.mk_mident id, f a)

      (* Lambda *)

      | A.Pcall (None, A.Cconst (A.Cexec), [], [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mexeclambda (fa, fb)

      | A.Pcall (None, A.Cconst (A.Capply), [], [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mapplylambda (fa, fb)

      (* Other *)

      | A.Pcall (None, A.Cconst A.Cinttodate, [], [AExpr x]) ->
        let fx = f x in
        M.Minttodate (fx)

      | A.Pcall (None, A.Cconst A.CmutezToNat, [], [AExpr x]) ->
        let fx = f x in
        M.Mmuteztonat (fx)

      | A.Pcall (None, A.Cconst A.CgetEntrypoint, [t], [AIdent id; AExpr arg]) ->
        let arg = f arg in
        let t = type_to_type t in
        M.Mgetentrypoint (t, M.mk_mident id, arg)

      | A.Pcall (None, A.Cconst A.CrequireEntrypoint, [t], [AIdent id; AExpr arg; AExpr err]) ->
        let arg = f arg in
        let err = f err in
        let t = type_to_type t in
        let ma = M.mk_mterm (M.Mgetentrypoint (t, M.mk_mident id, arg)) (M.toption (M.tcontract t)) in
        let idv = dumloc "_v" in
        let s = M.mk_mvar (M.mk_mident idv) (M.tcontract t) in
        M.Mmatchoption(ma, M.mk_mident idv, s, M.failg err)

      | A.Pcall (None, A.Cconst A.CcallView, [t], [AIdent id; AExpr addr; AExpr arg]) ->
        let addr = f addr in
        let arg = f arg in
        let t = type_to_type t in
        M.Mcallview (t, addr, M.mk_mident id, arg)

      | A.Pcall (None, A.Cconst A.CimportCallView, [t], [AIdent id; AExpr addr; AExpr arg]) ->
        let addr = f addr in
        let arg = f arg in
        let t = type_to_type t in
        M.Mimportcallview (t, addr, M.mk_mident id, arg)

      (* Fail *)

      | A.Pcall (aux, A.Cconst c, types, args) ->
        Format.eprintf "expr const unkown: %a with types: [%a] nb args: %d [%a] %s@."
          A.pp_const c
          (Printer_tools.pp_list ", " Printer_ast.pp_ptyp) types
          (List.length args)
          (Printer_tools.pp_list "; " (fun fmt x ->
               let str = match x with | A.AExpr _ -> "AExpr" | A.AEffect _ -> "AEffect" | A.AFun _ -> "AFun" | A.ASorting _ -> "ASorting" | AIdent _ -> "AIdent" in
               Printer_tools.pp_str fmt str)) args
          (match aux with | Some _ -> "with aux" | _ -> "without aux");
        assert false
    in
    M.mk_mterm node type_ ~loc:pterm.loc
  in

  let extract_asset_name (pterm : M.mterm) : Ident.ident =
    match pterm with
    | {type_ = (Tcontainer ((Tasset asset_name, _), _), _); _ } -> M.unloc_mident asset_name
    | _ -> assert false
  in

  let to_variable_kind = function
    | A.VKconstant  -> M.VKconstant
    | A.VKvariable  -> M.VKvariable
  in

  let process_var (env : env) (v : A.variable) : M.decl_node =
    let t : M.type_ = type_to_type (Option.get v.decl.typ) in
    let var : M.var = M.mk_var (to_mident v.decl.name) t t (to_variable_kind v.kind) ?default:(Option.map (to_mterm env) v.decl.default) ~loc:v.loc in
    M.Dvar var
  in

  let process_enum (_env : env) (e : A.enum) : M.decl_node =
    let values = List.map (fun (x : A.enum_item_struct) ->
        let id : M.lident = x.name in
        M.mk_enum_item (M.mk_mident id) ~args:(List.map type_to_type x.args)
      ) e.items in
    let initial : A.lident option = List.fold_left (fun accu (x : A.enum_item_struct) -> match x.initial with | true -> Some x.name | _ -> accu) None e.items in
    let enum_name =
      match e.kind with
      | EKenum id -> to_mident id
      | EKstate nm -> M.mk_mident ?namespace:(get_namespace nm) (dumloc "state")
    in
    let enum = M.mk_enum enum_name (M.mk_mident (Option.get initial)) ~values:values in
    M.Denum enum
  in

  let process_asset (env : env) (a : A.asset) : M.decl_node =
    let env = {env with asset_name = Some (unloc_longident a.name)} in
    let values = List.map (fun (x : A.lident A.decl_gen) ->
        let typ = Option.get (Option.map type_to_type x.typ) in
        let default = Option.map (to_mterm env) x.default in
        M.mk_asset_item (M.mk_mident x.name) typ typ ?default:default ~shadow:x.shadow ~loc:x.loc) a.fields
    in
    let mk_asset an l : M.mterm = let l = List.map (to_mterm env) l in M.mk_mterm (M.Masset l) (M.tasset an) ~loc:(Location.mergeall (List.map (fun (x : M.mterm) -> x.loc) l)) in
    let mp = match a.map_kind with | A.MKMap -> M.MKMap | A.MKBigMap -> M.MKBigMap | A.MKIterableBigMap -> M.MKIterableBigMap in
    let no_storage = not (is_current_namespace (fst a.name)) in
    let r : M.asset = M.mk_asset (to_mident a.name) ~keys:(List.map unloc (a.keys)) ~values:values ~sort:(List.map M.mk_mident a.sort) ~map_kind:mp ~init:(List.map (fun x -> (mk_asset (to_mident a.name)) x) a.init) ~no_storage ~loc:a.loc in
    M.Dasset r
  in

  let process_record (r : A.record) : M.record =
    let rec for_pos (pos : A.position) : M.position =
      match pos with
      | A.Pleaf id -> M.Ptuple [unloc id]
      | A.Pnode n -> begin
          let is_all_leaf l = List.for_all (function | A.Pleaf _ -> true | _ -> false) l in
          match n with
          | [] -> M.Pnode []
          | _ when is_all_leaf n -> M.Ptuple (List.map (function | A.Pleaf id -> (unloc id) | _ -> assert false) n)
          | _ -> begin
              let update_res accu res =
                match accu with
                | [] -> res
                | _  -> res @ [M.Ptuple accu]
              in

              let accu, res = List.fold_left (fun (accu, res) x ->
                  match x with
                  | A.Pleaf id -> (accu @ [unloc id], res)
                  | _ -> begin
                      let res = update_res accu res in
                      ([], res @ [for_pos x])
                    end) ([], []) n
              in
              M.Pnode (update_res accu res)
            end
        end
    in

    let pos = for_pos r.pos in
    let fs : M.record_field list =
      List.map (fun (x : A.lident A.decl_gen) ->
          let typ = Option.get (Option.map type_to_type x.typ) in
          M.mk_record_field (M.mk_mident x.name) typ ~loc:x.loc) r.fields
    in
    M.mk_record (to_mident r.name) ~fields:fs ~pos ~loc:r.loc
  in

  let rec to_instruction (env : env) (instr : A.instruction) : M.mterm =
    let is_empty_seq (instr : A.instruction) =
      match instr.node with
      | A.Iseq [] -> true
      | _ -> false
    in

    let node =
      let f = to_mterm env in
      let g = to_instruction env in
      let n : A.instruction_node = instr.node in

      match n with
      | A.Iif (c, t, e) when is_empty_seq e -> M.Mif (f c, g t, None)
      | A.Iif (c, t, e)           -> M.Mif (f c, g t, Some (g e))
      | A.Ifor (i, col, body)     ->
        begin
          let ncol =
            let x = f col in
            match x.node, M.get_ntype x.type_ with
            | _, M.Tset  _ -> M.ICKset x
            | _, M.Tlist _ -> M.ICKlist x
            | _, M.Tmap  _
            | _, M.Tbig_map  _
            | _, M.Titerable_big_map  _ -> M.ICKmap x
            | _, M.Tcontainer ((Tasset an, _), Collection) -> M.ICKcoll (M.unloc_mident an)
            | M.Mdotassetfield (an, _k, fn), M.Tcontainer ((Tasset _, _), (Aggregate | Partition)) -> M.ICKfield (M.unloc_mident an, M.unloc_mident fn, x)
            | _ -> M.ICKview x
          in
          let i =
            match i with
            | A.FIsimple x      -> M.FIsimple (M.mk_mident x)
            | A.FIdouble (x, y) -> M.FIdouble (M.mk_mident x, M.mk_mident y)
          in
          M.Mfor (i, ncol, g body)
        end
      | A.Iiter (i, a, b, body)   -> M.Miter (M.mk_mident i, f a, f b, g body, false)
      | A.Iwhile (c, body)        -> M.Mwhile (f c, g body)
      | A.Iletin (i, init, cont)  -> M.Mletin ([M.mk_mident i], LVsimple (f init), Option.map type_to_type init.type_, g cont, None) (* TODO *)
      | A.Ideclvar (i, v, c)      -> M.Mdeclvar ([M.mk_mident i], Option.map type_to_type v.type_, f v, c) (* TODO *)
      | A.Ideclvaropt (i, v, fa, c)-> M.Mdeclvaropt ([M.mk_mident i], Option.map type_to_type (match v.type_ with | Some (A.Toption ty) -> Some ty | _ -> None), f v, Option.map f fa, c) (* TODO *)
      | A.Iseq l                  -> M.Mseq (List.map g l)
      | A.Imatchwith (m, l)       -> M.Mmatchwith (f m, List.map (fun (p, i) -> (to_pattern p, g i)) l)
      | A.Imatchoption (x, id, ve, ne)      -> M.Minstrmatchoption   (f x, M.mk_mident id, g ve, g ne)
      | A.Imatchor (x, lid, le, rid, re)    -> M.Minstrmatchor       (f x, M.mk_mident lid, g le, M.mk_mident rid, g re)
      | A.Imatchlist (x, hid, tid, hte, ee) -> M.Minstrmatchlist     (f x, M.mk_mident hid, M.mk_mident tid, g hte, g ee)
      | A.Iassign (op, t, lv, e, fa) -> begin
          let to_ak (lv : A.lvalue) =
            match lv with
            | `Var x -> (match unloc x with | "operations" -> M.Aoperations | _ -> M.Avar (M.mk_mident x))
            | `Field (rn, o, fn) -> (match o.type_ with | Some (A.Trecord rn) -> M.Arecord(f o, to_mident rn, M.mk_mident fn) | _ -> M.Aasset (to_mident rn, M.mk_mident fn, f o))
            | `Asset (an, k, fn) -> M.Aasset (to_mident an, M.mk_mident fn, f k)
            | `Tuple (lv, i, l) -> M.Atuple (f lv, i, l)
          in
          let e = f e in
          let t = type_to_type t in
          match fa with
          | Some fa -> M.Massignopt (to_assignment_operator op, t, to_ak lv, e, f fa)
          | None -> M.Massign (to_assignment_operator op, t, to_ak lv, e)
        end
      | A.Irequire (b, t, e) ->
        let cond : M.mterm =
          if b
          then term_not (f t)
          else (f t)
        in
        let e : M.mterm = f e in
        M.Mif (cond, fail (Invalid e), None)

      | A.Itransfer tr -> begin
          let tr =
            match tr with
            | TTsimple (v, d)               -> M.TKsimple (f v, f d)
            | TTcontract (v, d, id, t, arg) -> M.TKcall (f v, unloc id, type_to_type t, f d, f arg)
            | TTentry (v, e, arg)           -> M.TKentry (f v, f e, f arg)
            | TTgen (v, en, cn, t, e, arg)  -> M.TKgen (f v, en, cn, type_to_type t, f e, f arg)
            | TTself (v, id, args)          -> M.TKself (f v, unloc id, List.map (fun (id, v) -> unloc id, f v) args)
            | TToperation v                 -> M.TKoperation (f v)
          in
          M.Mtransfer tr
        end
      | A.Iemit (e, v) -> M.Memit (to_mident e, f v)
      | A.Ireturn e -> M.Mreturn (f e)
      | A.Ifail   m -> M.Mfail (Invalid (f m))
      | A.Ifailsome v -> M.Mfailsome (f v)
      | A.Idetach (id, dk, ty, fa) ->
        let to_dk = function
          | A.DK_option (ty, id) -> M.DK_option (type_to_type ty, id)
          | A.DK_map (ty, id, k) -> M.DK_map (type_to_type ty, id, f k)
        in
        M.Mdetach (M.mk_mident id, to_dk dk, type_to_type ty, f fa)

      | A.Imicheline micheline -> M.Mmicheline micheline

      | A.Icall (i, Cid id, args) -> M.Mapp (M.mk_mident id, Option.map_dfl (fun v -> [f v]) [] i @ List.map (term_arg_to_expr f) args)

      | A.Icall (_, A.Cconst (A.Cfail), [AExpr p]) ->
        M.Mfail (Invalid (f p))

      | A.Icall (Some p, A.Cconst (A.Cadd), [AExpr q]) when is_asset_container p -> (
          let fp = f p in
          let fq = f q in
          match fp with
          | {node = M.Mvar (asset_name, Vstorecol); _} -> M.Maddasset (M.unloc_mident asset_name, fq)
          | {node = M.Mdotassetfield (asset_name , k, fn); _} -> M.Maddfield (M.unloc_mident asset_name, M.unloc_mident fn, k, fq)
          | _ -> assert false
        )

      | A.Icall (Some p, A.Cconst (A.Cput), [AExpr q]) when is_asset_container p -> (
          let fp = f p in
          let fq = f q in
          match fp with
          | {node = M.Mvar (asset_name, Vstorecol); _} -> M.Mputsingleasset (M.unloc_mident asset_name, fq)
          | _ -> assert false
        )

      | A.Icall (Some p, A.Cconst (A.Cput), [AExpr k; AExpr v]) when is_asset_container p -> (
          let fp = f p in
          let fk = f k in
          let fv = f v in
          match fp with
          | {node = M.Mvar (asset_name, Vstorecol); _} -> M.Mputasset (M.unloc_mident asset_name, fk, fv)
          | _ -> assert false
        )

      | A.Icall (Some p, A.Cconst (A.Cremove), [AExpr q]) when is_asset_container p -> (
          let fp = f p in
          let fq = f q in
          match fp with
          | {node = M.Mvar (asset_name, Vstorecol); _} -> M.Mremoveasset (M.unloc_mident asset_name, fq)
          | {node = M.Mdotassetfield (asset_name , k, fn); _} -> M.Mremovefield (M.unloc_mident asset_name, M.unloc_mident fn, k, fq)
          | _ -> assert false
        )

      | A.Icall (Some p, A.Cconst (A.Cremoveall), []) when is_asset_container p -> (
          let fp = f p in
          match fp with
          | {node = M.Mdotassetfield (an , k, fn); _}       -> M.Mremoveall (M.unloc_mident an, CKfield (M.unloc_mident an, M.unloc_mident fn, k))
          | {type_ = (M.Tcontainer ((Tasset an, _), _), _)} -> M.Mremoveall (M.unloc_mident an, to_ck env fp)
          | _ -> assert false
        )

      | A.Icall (Some p, A.Cconst (A.Cremoveif), [AFun (_id, _type, l, q)]) ->
        let fp = f p in
        let lambda_body = f q in
        let lambda_args, args = List.fold_right (fun (x, y, z) (l1, l2) -> ((unloc x, type_to_type y)::l1, (f z)::l2)) l ([], []) in
        begin
          match fp.node, M.get_ntype fp.type_ with
          | Mdotassetfield (an, k, fn), _ -> M.Mremoveif (M.unloc_mident an, CKfield (M.unloc_mident an, M.unloc_mident fn, k), lambda_args, lambda_body, args)
          | _, Tcontainer ((Tasset an, _), _)  -> M.Mremoveif (M.unloc_mident an, CKcoll, lambda_args, lambda_body, args)
          | _ -> assert false
        end

      | A.Icall (Some p, A.Cconst (A.Cclear), []) -> (
          let fp = f p in
          begin
            match fp.node, M.get_ntype fp.type_ with
            | Mdotassetfield (an, k, fn), _ -> M.Mclear (M.unloc_mident an, CKfield (M.unloc_mident an, M.unloc_mident fn, k))
            | _, Tcontainer ((Tasset an, _), _)  -> M.Mclear (M.unloc_mident an, to_ck env fp)
            | _ -> assert false
          end
        )

      | A.Icall (Some p, A.Cconst (A.CputRemove), [AExpr k; AExpr v]) -> (
          let fp = f p in
          let fk = f k in
          let fv = f v in
          begin
            match fp.node, M.get_ntype fp.type_ with
            | _, Tcontainer ((Tasset an, _), _)  -> M.Mputremove (M.unloc_mident an, to_ck env fp, fk, fv)
            | _ -> assert false
          end
        )

      | A.Icall (Some p, A.Cconst (A.Caddupdate), [AExpr k; AEffect e]) when is_asset_container p ->
        let to_op = function
          | `Assign op -> to_assignment_operator op
          | _ -> emit_error (instr.loc, CannotConvertToAssignOperator); bailout ()
        in
        let fp = f p in
        let fk = f k in
        let fe = List.map (fun (id, op, c) -> (M.mk_mident id, to_op op, f c)) e in
        begin
          match fp.node, M.get_ntype fp.type_ with
          | Mdotassetfield (_, _k, fn), Tcontainer ((Tasset an, _), (Aggregate | Partition)) -> M.Maddupdate (M.unloc_mident an, CKfield (M.unloc_mident an, M.unloc_mident fn, fp), fk, fe)
          | _, Tcontainer ((Tasset an, _), Collection)  -> M.Maddupdate (M.unloc_mident an, CKcoll, fk, fe)
          | _ -> assert false
        end

      | A.Icall (Some p, A.Cconst (A.Cupdate), [AExpr k; AEffect e]) when is_asset_container p ->
        let to_op = function
          | `Assign op -> to_assignment_operator op
          | _ -> emit_error (instr.loc, CannotConvertToAssignOperator); bailout ()
        in
        let fp = f p in
        let fk = f k in
        let fe = List.map (fun (id, op, c) -> (M.mk_mident id, to_op op, f c)) e in
        let asset_name = extract_asset_name fp in
        M.Mupdate (asset_name, fk, fe)

      | A.Icall (Some p, A.Cconst (A.Cupdateall), [AEffect e]) when is_asset_container p ->
        let to_op = function
          | `Assign op -> to_assignment_operator op
          | _ -> emit_error (instr.loc, CannotConvertToAssignOperator); bailout ()
        in
        let fp = f p in
        let fe = List.map (fun (id, op, c) -> (M.mk_mident id, to_op op, f c)) e in
        begin
          match fp.node, M.get_ntype fp.type_ with
          | Mdotassetfield (_, _k, fn), Tcontainer ((Tasset an, _), (Aggregate | Partition)) -> M.Mupdateall (M.unloc_mident an, CKfield (M.unloc_mident an, M.unloc_mident fn, fp), fe)
          | _, Tcontainer ((Tasset an, _), View) -> M.Mupdateall (M.unloc_mident an, CKview fp, fe)
          | _, Tcontainer ((Tasset an, _), Collection)  -> M.Mupdateall (M.unloc_mident an, CKcoll, fe)
          | _ -> assert false
        end

      | A.Icall (aux, A.Cconst c, args) ->
        Format.eprintf "instr const unkown: %a with nb args: %d [%a] %s@."
          A.pp_const c
          (List.length args)
          (Printer_tools.pp_list "; " (fun fmt (x : A.pterm_arg) ->
               let str = match x with | AExpr _ -> "AExpr" | AEffect _ -> "AEffect" | AFun _ -> "AFun" | ASorting _ -> "ASorting" | AIdent _ -> "AIdent" in
               Printer_tools.pp_str fmt str)) args
          (match aux with | Some _ -> "with aux" | _ -> "without aux");
        assert false
    in
    M.mk_mterm node M.tunit ~loc:instr.loc
  in

  let process_fun_gen name args side_effect (body : M.mterm) loc f : M.function_node =
    f (M.mk_function_struct name body ~side_effect:side_effect ~args:args ~loc:loc)
  in

  let process_function (env : env) (function_ : A.function_) : M.function_node =
    let name  = M.mk_mident function_.name in
    let args  = List.map (fun (x : A.lident A.decl_gen) -> (M.mk_mident x.name, (type_to_type |@ Option.get) x.typ, None)) function_.args in
    let env   = {env with function_p = Some (name, args); } in
    let body  = to_instruction env function_.body in
    let loc   = function_.loc in
    let get_ret_type rty = type_to_type (match rty with | A.Typed ty ->  ty | A.Void -> A.vtunit) in
    let to_vv = function | A.VVonchain -> M.VVonchain | A.VVoffchain -> M.VVoffchain | A.VVonoffchain -> M.VVonoffchain in
    let side_effect = match function_.storage_usage with | A.SUpure -> false | A.SUread -> false | A.SUwrite -> true in
    let f     = match function_.kind with
      | FKfunction -> (fun x -> M.Function (x, (match function_.return with | A.Typed ty -> M.Typed (type_to_type ty) | A.Void -> M.Void)))
      | FKgetter   -> (fun x -> M.Getter (x, get_ret_type function_.return))
      | FKview vv  -> (fun x -> M.View (x, get_ret_type function_.return, to_vv vv))
    in
    process_fun_gen name args side_effect body loc f
  in

  let add_seq (s1 : M.mterm) (s2 : M.mterm) =
    let extract (s : M.mterm) =
      match s.node with
        M.Mseq l -> l
      | _ -> [s]
    in

    let l1 = extract s1 in
    let l2 = extract s2 in

    M.mk_mterm (M.Mseq (l1 @ l2)) M.tunit
  in

  let process_transaction (env : env) (transaction : A.transaction) : M.function_node =
    let process_calledby env (body : M.mterm) : M.mterm =
      let process_cb ((caller, fa) : M.mterm * M.fail_type) (cb : (A.rexpr * A.pterm option)) (body : M.mterm) : M.mterm =
        let rec process_rexpr (rq : A.rexpr) : M.mterm option =
          match rq.node with
          | Rany -> None
          | Rasset (_, a) -> begin (* FIXME: namespace *)
              let an = unloc a in
              Some (M.mk_mterm (M.Mcontains(an, CKcoll, caller)) M.tbool ~loc:(loc a))
            end
          | Rexpr e ->
            begin
              let mt = to_mterm env e in
              Some (M.mk_mterm (M.Mequal (M.taddress, caller, mt)) (M.tbool) ~loc:rq.loc)
            end
          | Ror (l, r) -> begin
              let l = process_rexpr l in
              let r = process_rexpr r in
              match l, r with
              | Some l, Some r -> Some (M.mk_mterm (M.Mor (l, r)) (M.tbool) ~loc:rq.loc)
              | _ -> None
            end
        in
        let rexpr = fst cb in
        match process_rexpr rexpr with
        | Some a ->
          let require : M.mterm = M.mk_mterm (M.Mnot (a)) (M.tbool) ~loc:rexpr.loc in
          let fail_auth : M.mterm =
            match snd cb with
            | Some o -> fail (Invalid (to_mterm env o))
            | None -> fail fa
          in
          let cond_if = M.mk_mterm (M.Mif (require, fail_auth, None)) M.tunit in
          add_seq cond_if body
        | _ -> body
      in
      begin
        let process tc caller body =
          match tc with
          | None -> body
          | Some cb -> process_cb caller cb body
        in

        body
        |> process transaction.calledby  (M.mcaller, M.InvalidCaller)
        |> process transaction.sourcedby (M.msource, M.InvalidSource)
      end
    in

    let process_state_is env (body : M.mterm) : M.mterm =
      match transaction.state_is with
      | Some (id, o) -> begin
          let var     = M.mk_state_value (M.mk_mident id) in
          let state   = M.mk_state_var () in
          let c       = M.mk_mterm (M.Mnequal (M.tstate, var, state)) (M.tbool) ~loc:(loc id) in
          let cond_if =
            let fail = match o with Some o -> fail (Invalid (to_mterm env o)) | None -> fail InvalidState in
            M.mk_mterm (M.Mif (c, fail, None)) M.tunit in
          add_seq cond_if body
        end
      | _ -> body
    in

    let process env b (x : A.label_term) (body : M.mterm) : M.mterm =
      let term = to_mterm env x.term in
      let cond : M.mterm =
        match b with
        | `Require ->  M.mk_mterm (M.Mnot term) (M.tbool) ~loc:x.loc
        | `Failif -> term
      in
      let fail_cond : M.mterm = fail (M.InvalidCondition (x.label |> Option.get |> unloc, Option.map (to_mterm env) x.error)) in
      let cond_if = M.mk_mterm (M.Mif (cond, fail_cond, None)) M.tunit ~loc:x.loc in
      add_seq cond_if body
    in
    let apply env b li body =
      match li with
      | None -> body
      | Some l -> List.fold_right (fun (x : A.label_term) (accu : M.mterm) -> process env b x accu) l body
    in
    let apply_cst env li body =
      let process env (x : A.label_term) (body : M.mterm) : M.mterm =
        let id    : M.lident       = Option.get x.label in
        let value : M.mterm        = to_mterm env x.term in
        let fa    : M.mterm option = Option.map (to_mterm env) x.error in
        let node =
          match fa with
          | Some fa -> M.Mdeclvaropt([M.mk_mident id], Some value.type_, value, Some fa, true)
          | None    -> M.Mdeclvar   ([M.mk_mident id], Some value.type_, value, true)
        in
        let term  = M.mk_mterm node M.tunit in
        add_seq term body
      in
      match li with
      | None -> body
      | Some l -> List.fold_right (fun (x : A.label_term) (accu : M.mterm) -> process env x accu) l body
    in

    let process_requires env (body : M.mterm) : M.mterm =
      body
      |>  apply env `Failif  transaction.failif
      |>  apply env `Require transaction.require
      |>  apply_cst env transaction.constants
    in

    let process_accept_transfer env (body : M.mterm) : M.mterm =
      if (not (fst transaction.accept_transfer))
      then
        let lhs : M.mterm = M.mk_mterm (M.Mtransferred) M.ttez in
        let rhs : M.mterm = M.mk_mterm (M.Mmutez Big_int.zero_big_int) M.ttez in
        let eq : M.mterm = M.mk_mterm (M.Mequal (M.ttez, lhs, rhs)) M.tbool in
        let cond : M.mterm = M.mk_mterm (M.Mnot eq) M.tbool in
        let fail = match snd transaction.accept_transfer with | Some o -> fail (Invalid (to_mterm env o)) | None -> fail NoTransfer in
        let cond_if : M.mterm = M.mk_mterm (M.Mif (cond, fail, None)) M.tunit in
        add_seq cond_if body
      else
        body
    in

    let process_body_args env : M.argument list * M.mterm * env =
      let args  = List.map (fun (x : A.lident A.decl_gen) -> (M.mk_mident x.name, (type_to_type |@ Option.get) x.typ, None)) transaction.args in
      let env   = {env with function_p = Some (M.mk_mident transaction.name, args); } in
      let empty : M.mterm = M.mk_mterm (M.Mseq []) M.tunit in
      match transaction.transition, transaction.effect with
      | None, None ->
        let body = empty in
        args, body, env
      | None, Some e ->
        let body = to_instruction env e in
        args, body, env
      | Some t, None ->
        let env = {env with function_p = Some (M.mk_mident transaction.name, args); } in
        let build_code (body : M.mterm) : M.mterm =
          (List.fold_right (fun ((id, cond, effect) : (A.lident * A.pterm option * A.instruction option)) (acc : M.mterm) : M.mterm ->
               let tre : M.mterm =
                 let v : M.mterm = M.mk_mterm (Menumval (M.mk_mident id, [], M.mk_mident (dumloc "state"))) (M.tenum (M.mk_mident (dumloc "state"))) ~loc:(Location.loc id) in
                 M.mk_mterm (M.Massign (ValueAssign, v.type_, Astate, v)) M.tunit
               in
               let code : M.mterm =
                 match effect with
                 | Some e -> M.mk_mterm (M.Mseq [to_instruction env e; tre]) M.tunit
                 | None -> tre
               in

               match cond with
               | Some c -> M.mk_mterm (M.Mif (to_mterm env c, code, Some acc)) M.tunit
               | None -> code
             ) t.trs body)
        in
        let body : M.mterm = build_code empty in
        let body = match t.from.node with
          | Sany -> body
          | _ ->
            begin
              let rec compute_patterns (a : A.sexpr) : M.pattern list =
                match a.node with
                | Sref id -> [M.mk_pattern (M.Pconst (M.mk_mident id, []))]
                | Sor (a, b) -> [a; b] |> List.map (fun x -> compute_patterns x) |> List.flatten
                | Sany -> emit_error (a.loc, AnyNotAuthorizedInTransitionTo); bailout ()
              in
              let list_patterns : M.pattern list =
                compute_patterns t.from in

              let pattern : M.pattern = M.mk_pattern M.Pwild in
              let fail_instr : M.mterm = fail InvalidState in

              let w = M.mk_mterm (M.Mvar(M.mk_mident (dumloc ""), Vstate)) M.tstate in
              M.mk_mterm (M.Mmatchwith (w, List.map (fun x -> (x, body)) list_patterns @ [pattern, fail_instr])) M.tunit
            end
        in
        args, body, env

      | _ -> emit_error (transaction.loc, CannotExtractBody); bailout ()
    in

    (* let list  = list |> cont process_function ast.functions in *)
    let args, body, env = process_body_args env in
    let body =
      body
      |> process_requires env
      |> process_accept_transfer env
      |> process_state_is env
      |> process_calledby env
    in
    let side_effect = false in
    let loc   = transaction.loc in

    process_fun_gen (M.mk_mident transaction.name) args side_effect body loc (fun x -> M.Entry x)
  in

  let process_parameter env (p : A.parameter) : M.parameter =
    {
      name    = M.mk_mident p.name;
      typ     = type_to_type p.typ;
      default = Option.map (to_mterm env) p.default;
      value   = Option.map (to_mterm env) p.value;
      const   = p.const;
      loc     = p.loc;
    }
  in

  (* let process_import (i : A.import_struct) : M.import =
     M.{
      name        = i.name;
      path        = i.path;
      kind_node   = (match i.kind_node with | A.INMichelson { ms_content } -> M.INMichelson { ms_content } | A.INArchetype -> M.INArchetype);
      views       = List.map (fun (x, (y, z)) -> (x, (type_to_type y, type_to_type z))) i.views;
      entrypoints = List.map (fun (x, y) -> (x, type_to_type y)) i.entrypoints;
     }
     in *)

  let process_decl_ (env : env) = function
    | A.Dvariable v -> process_var env v
    | A.Dasset    a -> process_asset env a
    | A.Drecord   r -> M.Drecord (process_record r)
    | A.Denum     e -> process_enum env e
    | A.Devent    e -> M.Devent (process_record e)
  in

  let process_fun_ (env : env) = function
    | A.Ffunction f    -> process_function env f
    | A.Ftransaction t -> process_transaction env t
  in

  let name = ast.name in
  let env = mk_env () in

  let iasts = Typing.Env.Import.get_all _tenv in
  let adecls =
    List.fold_right (fun (_, x : ident * 'b Typing.importdecl) accu -> begin
          match x.id_ast with
          | None -> accu
          | Some x -> begin
              let ds = x.decls |> List.filter (function
                  | A.Dvariable v -> (match v.kind with | A.VKconstant -> true | _ -> false)
                  | A.Dasset    _ -> true
                  | A.Drecord   _ -> true
                  | A.Denum     _ -> true
                  | A.Devent    _ -> true) in
              ds @ accu
            end
        end) iasts ast.decls
  in

  let extract_cc_models (model : M.model) =
    let rec aux ctx accu (mt : M.mterm) : M.model list =
      match mt.node with
      | M.Mcreatecontract (CCArl(id, _), _, _) -> begin
        let a = Typing.Env.Import.get _tenv id in
        let ast = Option.get a.id_ast in
        let m = to_model (_tenv, ast) in
        m::accu
      end
      | _ -> M.fold_term (aux ctx) accu mt
    in
    M.fold_model aux model []
  in

  let parameters = List.map (process_parameter env) ast.parameters in
  let metadata = Option.map (function | A.MKuri x -> M.MKuri x | A.MKjson x -> M.MKjson x) ast.metadata in
  let decls = List.map (process_decl_ env) adecls in
  let functions = List.map (process_fun_ env) ast.funs in
  let model = M.mk_model ~parameters ?metadata ~decls ~functions ~loc:ast.loc name in

  let cc_models = extract_cc_models model in
  {model with cc_models = cc_models }
