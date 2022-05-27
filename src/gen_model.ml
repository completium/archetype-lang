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

type env = {
  formula: bool;
  asset_name: ident option;
  function_p: (M.lident * (M.lident * M.type_ * M.mterm option) list) option
}
[@@deriving show {with_path = false}]

let mk_env ?(formula=false) ?asset_name ?function_p () =
  { formula; asset_name; function_p }

let to_model (ast : A.ast) : M.model =

  let to_currency = function
    | A.Utz  -> M.Utz
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
    | A.VTcurrency     -> M.Bcurrency
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

  let to_trtyp = function
    | A.TRentry  -> M.TRentry
    | A.TRaction -> M.TRaction
    | A.TRasset  -> M.TRasset
    | A.TRfield  -> M.TRfield
  in

  let rec type_to_type (t : A.type_) : M.type_ =
    let f = function
      | A.Tnamed _                  -> assert false
      | A.Tasset id                 -> M.Tasset id
      | A.Trecord id                -> M.Trecord id
      | A.Tenum id                  -> M.Tenum id
      | A.Tevent id                 -> M.Tevent id
      | A.Tbuiltin b                -> M.Tbuiltin (vtyp_to_btyp b)
      | A.Tcontainer (t, Collection)-> M.Tcontainer (type_to_type t, Collection)
      | A.Tcontainer (t, Aggregate) -> M.Tcontainer (type_to_type t, Aggregate)
      | A.Tcontainer (t, Partition) -> M.Tcontainer (type_to_type t, Partition)
      | A.Tcontainer (t, AssetKey)  -> M.Tcontainer (type_to_type t, AssetKey)
      | A.Tcontainer (t, AssetValue)-> M.Tcontainer (type_to_type t, AssetValue)
      | A.Tcontainer (t, AssetView) -> M.Tcontainer (type_to_type t, View)
      | A.Tset t                    -> M.Tset (type_to_type t)
      | A.Tlist t                   -> M.Tlist (type_to_type t)
      | A.Tmap (k, v)               -> M.Tmap (type_to_type k, type_to_type v)
      | A.Tbig_map (k, v)           -> M.Tbig_map (type_to_type k, type_to_type v)
      | A.Titerable_big_map (k, v)  -> M.Titerable_big_map (type_to_type k, type_to_type v)
      | A.Tor (l, r)                -> M.Tor (type_to_type l, type_to_type r)
      | A.Tlambda (a, r)            -> M.Tlambda (type_to_type a, type_to_type r)
      | A.Ttuple l                  -> M.Ttuple (List.map type_to_type l)
      | A.Toperation                -> M.Toperation
      | A.Tcontract t               -> M.Tcontract (type_to_type t)
      | A.Toption t                 -> M.Toption (type_to_type t)
      | A.Tticket t                 -> M.Tticket (type_to_type t)
      | A.Ttrace tr                 -> M.Ttrace (to_trtyp tr)
      | A.Tsapling_state n          -> M.Tsapling_state n
      | A.Tsapling_transaction n    -> M.Tsapling_transaction n
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

  let to_pattern_node (n : A.lident A.pattern_node) : 'id M.pattern_node =
    match n with
    | A.Mconst (id, xs) -> M.Pconst (id, xs)
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

  let term_arg_to_expr : 't. (A.lident A.term_gen -> M.mterm) -> (A.lident A.term_arg) -> M.mterm =
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

  let extract_asset_name (mterm : M.mterm) : ident =
    match mterm with
    | {type_ = (Tcontainer ((Tasset asset_name, _), _), _); _} -> unloc asset_name
    | _ -> assert false
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

  let to_entry_description (ad : A.entry_description) : M.entry_description =
    match ad with
    | ADAny -> M.ADany
    | ADOp ("add", id)      -> M.ADadd (unloc id)
    | ADOp ("remove", id)   -> M.ADremove    (unloc id)
    | ADOp ("update", id)   -> M.ADupdate    (unloc id)
    | ADOp ("transfer", id) -> M.ADtransfer  (unloc id)
    | ADOp ("get", id)      -> M.ADget       (unloc id)
    | ADOp ("iterate", id)  -> M.ADiterate   (unloc id)
    | ADOp ("call", id)     -> M.ADcall      (unloc id)
    | _ -> assert false
  in

  let to_ck (env : env) (fp : M.mterm) : M.container_kind =
    match fp.node, fp.type_ with
    | M.Mdotassetfield (an, _k, fn), (Tcontainer ((Tasset _, _), (Aggregate | Partition)), _) -> M.CKfield (unloc an, unloc fn, fp, Tnone, Dnone)
    | M.Mdot ({type_ = (Tasset an, _)}, fn), (Tcontainer ((Tasset _, _), (Aggregate | Partition)), _) -> M.CKfield (unloc an, unloc fn, fp, Tnone, Dnone)
    | M.Mvar (v, Vdefinition, _, _), _ -> M.CKdef (unloc v)
    | M.Mvar (fn, _, t, d), (Tcontainer ((Tasset _, _), (Aggregate | Partition)), _) -> begin
        let an = match env.asset_name with
          | Some v -> v
          | None -> assert false
        in
        M.CKfield (an, unloc fn, fp, t, d)
      end
    | M.Mvar (_, _, t, d), (Tcontainer ((Tasset _, _), Collection), _) -> M.CKcoll (t, d)
    | _, (Tcontainer ((Tasset _, _), Collection), _) -> M.CKcoll (Tnone, Dnone)
    | _ -> M.CKview fp
  in

  let is_param env id =
    match env.function_p with
    | Some (_, l) -> l |> List.map proj3_1 |> List.exists (fun x -> String.equal (unloc id) (unloc x))
    | _ -> false
  in

  let get_kind_var env id =
    if is_param env id
    then M.Vparam
    else M.Vlocal
  in

  let build_mvar ?(loc = Location.dummy) env id t =
    M.mk_mterm (Mvar (id, get_kind_var env id, Tnone, Dnone)) t ~loc:loc
  in

  let rec to_mterm (env : env) (pterm : A.pterm) : M.mterm =
    let to_temp = function
      | A.VTbefore -> M.Tbefore
      | A.VTat lbl -> M.Tat lbl
      | A.VTnone   -> M.Tnone
    in
    let to_delta = function
      | A.Vadded   -> M.Dadded
      | A.Vremoved -> M.Dremoved
      | A.Vunmoved -> M.Dunmoved
      | A.Vnone    -> M.Dnone
    in
    let is_record t = match M.get_ntype t with | M.Trecord _ | M.Tevent _ -> true | _ -> false in
    let type_ = type_to_type (Option.get pterm.type_) in
    let f x = to_mterm env x in
    let node =
      match pterm.node with
      | A.Pif (c, t, e)                     -> M.Mexprif        (f c, f t, f e)
      | A.Pmatchwith (m, l)                 -> M.Mexprmatchwith (f m, List.map (fun (p, e) -> (to_pattern p, f e)) l)
      | A.Pmatchoption (x, id, ve, ne)      -> M.Mmatchoption   (f x, id, f ve, f ne)
      | A.Pmatchor (x, lid, le, rid, re)    -> M.Mmatchor       (f x, lid, f le, rid, f re)
      | A.Pmatchlist (x, hid, tid, hte, ee) -> M.Mmatchlist     (f x, hid, tid, f hte, f ee)
      | A.Pfold (x, i, e)                   -> M.Mfold          (f x, i, f e)
      | A.Pmap (x, i, e)                    -> M.Mmap           (f x, i, f e)
      | A.Plogical (A.And, l, r)            -> M.Mand           (f l, f r)
      | A.Plogical (A.Or, l, r)             -> M.Mor            (f l, f r)
      | A.Plogical (A.Xor, l, r)            -> M.Mxor           (f l, f r)
      | A.Plogical (A.Imply, l, r)          -> M.Mimply         (f l, f r)
      | A.Plogical (A.Equiv, l, r)          -> M.Mequiv         (f l, f r)
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
          let record_name =  match M.get_ntype type_ with | M.Trecord name | M.Tevent name -> unloc name | _ -> assert false in
          let ids, k =
            List.fold_left (fun accu (x : A.lident A.decl_) ->
                match x with
                | A.Drecord r when String.equal (unloc r.name) record_name -> (List.map (fun (x : A.lident A.decl_gen) -> unloc x.name) r.fields, `Record)
                | A.Devent  r when String.equal (unloc r.name) record_name -> (List.map (fun (x : A.lident A.decl_gen) -> unloc x.name) r.fields, `Event)
                | _ -> accu) ([], `None) ast.decls
          in
          if List.length ids <> List.length l
          then (emit_error (pterm.loc, RecordNotFound record_name); bailout ());
          match k with
          | `Record -> M.Mlitrecord (List.map2 (fun x y -> x, f y) ids l)
          | `Event  -> M.Mlitevent  (List.map2 (fun x y -> x, f y) ids l)
          | `None   -> assert false
        end
      | A.Precord l                         -> M.Masset         (List.map f l)
      | A.Precupdate (e, l)                 -> M.Mrecupdate     (f e, List.map (fun (id, v) -> unloc id, f v) l)
      | A.Pletin (id, init, typ, body, o)   -> M.Mletin         ([id], f init, Option.map type_to_type typ, f body, Option.map f o)
      | A.Pdeclvar (i, t, v, c)             -> M.Mdeclvar       ([i], Option.map type_to_type t, f v, c)

      (* enum value *)
      | A.Pvar (_b, _vs, id) when A.Utils.is_enum_value ast id      -> M.Menumval (id, [], A.Utils.get_enum_values ast id |> Option.get |> unloc)
      | A.Pcall (_, Cid id, args) when A.Utils.is_enum_value ast id -> M.Menumval (id, List.map (function | A.AExpr x -> f x | _ -> assert false) args, A.Utils.get_enum_values ast id |> Option.get |> unloc)


      | A.Pvar (b, vs, {pldesc = "state"; _})                -> M.Mvar (dumloc "", Vstate, to_temp b, to_delta vs)
      | A.Pvar (b, vs, id) when is_param env id              -> M.Mvar (id, Vparam, to_temp b, to_delta vs)
      | A.Pvar (b, vs, id) when A.Utils.is_variable ast id   -> M.Mvar (id, Vstorevar, to_temp b, to_delta vs)
      | A.Pvar (b, vs, id) when A.Utils.is_asset ast id      -> M.Mvar (id, Vstorecol, to_temp b, to_delta vs)
      | A.Pvar (b, vs, id) when A.Utils.is_definition ast id -> M.Mvar (id, Vdefinition, to_temp b, to_delta vs)
      | A.Pvar (b, vs, id) when A.Utils.is_parameter ast id  -> M.Mvar (id, Vparameter, to_temp b, to_delta vs)
      | A.Pvar (b, vs, id)                                   -> M.Mvar (id, Vlocal, to_temp b, to_delta vs)
      | A.Parray l                             ->
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
      | A.Plit ({node = BVint i; _})           -> M.Mint i
      | A.Plit ({node = BVnat i; _})           -> M.Mnat i
      | A.Plit ({node = BVbool b; _})          -> M.Mbool b
      | A.Plit ({node = BVrational (d, n); _}) -> M.Mrational (d, n)
      | A.Plit ({node = BVdate s; _})          -> M.Mdate s
      | A.Plit ({node = BVstring s; _})        -> M.Mstring s
      | A.Plit ({node = BVcurrency (c, i); _}) -> M.Mcurrency (i, to_currency c)
      | A.Plit ({node = BVaddress s; _})       -> M.Maddress s
      | A.Plit ({node = BVduration d; _})      -> M.Mduration d
      | A.Plit ({node = BVbytes v; _})         -> M.Mbytes v
      | A.Plit ({node = BVunit; _})            -> M.Munit


      | A.Pdot (e, id) -> begin
          match e with
          | {node = Pcall (Some a, Cconst Cget, [AExpr k])} -> begin
              let b = f a in
              match M.get_ntype b.type_ with
              | M.Tcontainer ((Tasset an, _), Collection) -> M.Mdotassetfield (an, f k, id)
              | _ -> M.Mdot (f e, id)
            end

          | _ ->
            (* handle dot contract too *)
            M.Mdot (f e, id)
        end
      (* | A.Pquestion (e, id, edv) -> assert false *)

      | A.Pquestion (e, id, dv) -> begin
          match dv with
          | None -> M.Mquestionoption (f e, id)
          | Some dv -> M.Mquestiondefault (f e, id, f dv)
        end

      | A.Pconst Cstate                        -> M.Mvar(dumloc "", Vstate, Tnone, Dnone)
      | A.Pconst Cnow                          -> M.Mnow
      | A.Pconst Ctransferred                  -> M.Mtransferred
      | A.Pconst Ccaller                       -> M.Mcaller
      | A.Pconst Cbalance                      -> M.Mbalance
      | A.Pconst Csource                       -> M.Msource
      | A.Pconst Cselfaddress                  -> M.Mselfaddress
      | A.Pconst Cchainid                      -> M.Mchainid
      | A.Pconst Coperations                   -> M.Moperations
      | A.Pconst Cmetadata                     -> M.Mmetadata
      | A.Pconst Ctotalvotingpower             -> M.Mtotalvotingpower
      | A.Pconst Clevel                        -> M.Mlevel
      | A.Pconst c                             ->
        Format.eprintf "expr const unkown: %a@." A.pp_const c;
        assert false

      | A.Ptuple l                             -> M.Mtuple (List.map f l)
      | A.Ptupleaccess (p, idx)                -> M.Mtupleaccess (f p, idx)
      | A.Pnone                                -> M.Mnone
      | A.Psome a                              -> M.Msome (f a)
      | A.Pleft (t, x)                         -> M.Mleft (type_to_type t, f x)
      | A.Pright (t, x)                        -> M.Mright (type_to_type t, f x)
      | A.Plambda (rt, id, at, e)              -> M.Mlambda (type_to_type rt, id, type_to_type at, f e)
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
      | A.Pquantifer (Forall, i, (coll, typ), term)    -> M.Mforall (i, type_to_type typ, Option.map f coll, f term)
      | A.Pquantifer (Exists, i, (coll, typ), term)    -> M.Mexists (i, type_to_type typ, Option.map f coll, f term)

      | A.Pself id -> M.Mself id


      | A.Pentrypoint (t, a, b, r) -> M.Mentrypoint (type_to_type t, a, f b, Option.map f r)

      | A.Pcallview (t, a, b, c) -> M.Mcallview (type_to_type t, f a, b,f c)

      | A.Pternary (c, a, b) -> begin
          let c = f c in
          let a = f a in
          let b = f b in
          match c.type_ with
          | (M.Tbuiltin Bbool, _) -> M.Mternarybool   (c, a, b)
          | (M.Toption _, _)      -> M.Mternaryoption (c, a, b)
          | _ -> assert false
        end

      (* | A.Pcall (Some p, A.Cconst A.Cbefore,    []) -> M.Msetbefore    (f p) *)
      (* | A.Pcall (Some p, A.Cconst A.Cunmoved,   []) -> M.Msetunmoved   (f p)
         | A.Pcall (Some p, A.Cconst A.Cadded,     []) -> M.Msetadded     (f p)
         | A.Pcall (Some p, A.Cconst A.Cremoved,   []) -> M.Msetremoved   (f p) *)

      (* Asset *)

      | A.Pcall (Some p, A.Cconst (A.Cget), [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Mget (asset_name, to_ck env fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Cgetopt), [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Mgetopt (asset_name, to_ck env fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Cselect), [AFun (_id, _type, l, q)]) when is_asset_container p ->
        let fp = f p in
        let lambda_body = f q in
        let asset_name = extract_asset_name fp in
        let lambda_args, args = List.fold_right (fun (x, y, z) (l1, l2) -> ((unloc x, type_to_type y)::l1, (f z)::l2)) l ([], []) in
        M.Mselect (asset_name, to_ck env fp, lambda_args, lambda_body, args)

      | A.Pcall (Some p, A.Cconst (A.Csort), args) when is_asset_container p ->
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

      | A.Pcall (Some p, A.Cconst (A.Ccontains), [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Mcontains (asset_name, to_ck env fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Cnth), [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Mnth (asset_name, to_ck env fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Ccount), []) when is_asset_container p ->
        let fp = f p in
        let asset_name = extract_asset_name fp in
        M.Mcount (asset_name, to_ck env fp)

      | A.Pcall (Some p, A.Cconst (A.Csum), [AFun (_qi, _qt, _l, q)]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Msum (asset_name, to_ck env fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Chead), [AExpr e]) when is_asset_container p ->
        let fp = f p in
        let fe = f e in
        let asset_name = extract_asset_name fp in
        M.Mhead (asset_name, to_ck env fp, fe)

      | A.Pcall (Some p, A.Cconst (A.Ctail), [AExpr e]) when is_asset_container p ->
        let fp = f p in
        let fe = f e in
        let asset_name = extract_asset_name fp in
        M.Mtail (asset_name, to_ck env fp, fe)


      (* Set*)

      | A.Pcall (None, A.Cconst (A.Csadd), [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_set fp in
        M.Msetadd (t, fp, fq)

      | A.Pcall (None, A.Cconst (A.Csremove), [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_set fp in
        M.Msetremove (t, fp, fq)

      | A.Pcall (None, A.Cconst (A.Cscontains), [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_set fp in
        M.Msetcontains (t, fp, fq)

      | A.Pcall (None, A.Cconst (A.Cslength), [AExpr p]) ->
        let fp = f p in
        let t = extract_builtin_type_set fp in
        M.Msetlength (t, fp)


      (* List*)

      | A.Pcall (None, A.Cconst (A.Cprepend), [AExpr p; AExpr q]) when is_list p -> (
          let fp = f p in
          let fq = f q in
          let t = extract_builtin_type_list fp in
          M.Mlistprepend (t, fp, fq)
        )

      | A.Pcall (None, A.Cconst (A.Clength), [AExpr p]) when is_list p ->
        let fp = f p in
        let t = extract_builtin_type_list fp in
        M.Mlistlength (t, fp)

      | A.Pcall (None, A.Cconst (A.Ccontains), [AExpr p; AExpr q]) when is_list p ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_list fp in
        M.Mlistcontains (t, fp, fq)

      | A.Pcall (None, A.Cconst (A.Cnth), [AExpr p; AExpr q]) when is_list p ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_list fp in
        M.Mlistnth (t, fp, fq)

      | A.Pcall (None, A.Cconst (A.Creverse), [AExpr p]) when is_list p ->
        let fp = f p in
        let t = extract_builtin_type_list fp in
        M.Mlistreverse (t, fp)

      | A.Pcall (None, A.Cconst (A.Cconcat), [AExpr p; AExpr q]) when is_list p ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_list fp in
        M.Mlistconcat (t, fp, fq)


      (* Map *)

      | A.Pcall (None, A.Cconst (A.Cmput), [AExpr p; AExpr q; AExpr r]) ->
        let fp = f p in
        let fq = f q in
        let fr = f r in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmapput (mk, kt, vt, fp, fq, fr)

      | A.Pcall (None, A.Cconst (A.Cmremove), [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmapremove (mk, kt, vt, fp, fq)

      | A.Pcall (None, A.Cconst (A.Cmupdate), [AExpr p; AExpr q; AExpr r]) ->
        let fp = f p in
        let fq = f q in
        let fr = f r in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmapupdate (mk, kt, vt, fp, fq, fr)

      | A.Pcall (None, A.Cconst (A.Cmget), [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmapget (mk, kt, vt, fp, fq, None)

      | A.Pcall (None, A.Cconst (A.Cmgetopt), [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmapgetopt (mk, kt, vt, fp, fq)

      | A.Pcall (None, A.Cconst (A.Cmcontains), [AExpr p; AExpr q]) ->
        let fp = f p in
        let fq = f q in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmapcontains (mk, kt, vt, fp, fq)

      | A.Pcall (None, A.Cconst (A.Cmlength), [AExpr p]) ->
        let fp = f p in
        let mk, kt, vt = extract_builtin_type_map fp in
        M.Mmaplength (mk, kt, vt, fp)


      (* Formula *)

      | A.Pcall (Some p, A.Cconst (A.Cempty), []) when is_asset_container p ->
        let fp = f p in
        let asset_name = extract_asset_name fp in
        M.Mempty (asset_name)

      | A.Pcall (Some p, A.Cconst (A.Csingleton), [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Msingleton (asset_name, fq)

      | A.Pcall (Some p, A.Cconst (A.Csubsetof), [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Msubsetof (asset_name, to_ck env fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Cisempty), []) when is_asset_container p ->
        let fp = f p in
        let asset_name = extract_asset_name fp in
        M.Misempty (asset_name, fp)

      | A.Pcall (Some p, A.Cconst (A.Cunion), [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Munion (asset_name, fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Cinter), [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Minter (asset_name, fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Cdiff), [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Mdiff (asset_name, fp, fq)


      (* Builtin functions*)

      | A.Pcall (None, A.Cconst A.Cmin, [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mmin (fa, fb)

      | A.Pcall (None, A.Cconst A.Cmax, [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mmax (fa, fb)

      | A.Pcall (None, A.Cconst A.Cabs, [AExpr a]) ->
        let fa = f a in
        M.Mabs (fa)

      | A.Pcall (None, A.Cconst A.Cconcat, [AExpr x; AExpr y]) ->
        let fx = f x in
        let fy = f y in
        M.Mconcat (fx, fy)

      | A.Pcall (None, A.Cconst A.Cconcat, [AExpr x]) ->
        let fx = f x in
        M.Mconcatlist (fx)

      | A.Pcall (None, A.Cconst A.Cslice, [AExpr x; AExpr s; AExpr e]) ->
        let fx = f x in
        let fs = f s in
        let fe = f e in
        M.Mslice (fx, fs, fe)

      | A.Pcall (None, A.Cconst A.Clength, [AExpr x]) ->
        let fx = f x in
        M.Mlength (fx)

      | A.Pcall (None, A.Cconst A.Cisnone, [AExpr x]) ->
        let fx = f x in
        M.Misnone (fx)

      | A.Pcall (None, A.Cconst A.Cissome, [AExpr x]) ->
        let fx = f x in
        M.Missome (fx)

      | A.Pcall (None, A.Cconst A.Cisnat, [AExpr x]) ->
        let fx = f x in
        M.Misnat (fx)

      | A.Pcall (None, A.Cconst A.Ctonat, [AExpr x]) ->
        let fx = f x in
        M.Mtonat (fx)

      | A.Pcall (None, A.Cconst A.Cgetopt, [AExpr x]) ->
        let fx = f x in
        M.Moptget (fx)

      | A.Pcall (None, A.Cconst A.Crequiresome, [AExpr x; AExpr y]) ->
        let fx = f x in
        let fy = f y in
        M.Mrequiresome (fx, fy)

      | A.Pcall (None, A.Cconst A.Cfloor, [AExpr x]) ->
        let fx = f x in
        M.Mfloor (fx)

      | A.Pcall (None, A.Cconst A.Cceil, [AExpr x]) ->
        let fx = f x in
        M.Mceil (fx)

      | A.Pcall (None, A.Cconst A.Ctostring, [AExpr x]) ->
        let fx = f x in
        let t = fx.type_ in
        M.Mtostring (t, fx)

      | A.Pcall (None, A.Cconst A.Cpack, [AExpr x]) ->
        let fx = f x in
        M.Mpack (fx)

      | A.Pcall (None, A.Cconst A.Cunpack, [AExpr x]) ->
        let fx = f x in
        let t =
          match M.get_ntype type_ with
          | Toption t -> t
          | _ -> assert false
        in
        M.Munpack (t, fx)

      | A.Pcall (None, A.Cconst A.Csetdelegate, [AExpr x]) ->
        let fx = f x in
        M.Msetdelegate (fx)

      | A.Pcall (None, A.Cconst A.Cimplicitaccount, [AExpr x]) ->
        let fx = f x in
        M.Mimplicitaccount (fx)

      | A.Pcall (None, A.Cconst A.Csubnat, [AExpr x; AExpr y]) ->
        let fx = f x in
        let fy = f y in
        M.Msubnat (fx, fy)

      | A.Pcall (None, A.Cconst A.Csubmutez, [AExpr x; AExpr y]) ->
        let fx = f x in
        let fy = f y in
        M.Msubmutez (fx, fy)

      | A.Pcall (None, A.Cconst A.Cblake2b, [AExpr x]) ->
        let fx = f x in
        M.Mblake2b (fx)

      | A.Pcall (None, A.Cconst A.Csha256, [AExpr x]) ->
        let fx = f x in
        M.Msha256 (fx)

      | A.Pcall (None, A.Cconst A.Csha512, [AExpr x]) ->
        let fx = f x in
        M.Msha512 (fx)

      | A.Pcall (None, A.Cconst A.Csha3, [AExpr x]) ->
        let fx = f x in
        M.Msha3 (fx)

      | A.Pcall (None, A.Cconst A.Ckeccak, [AExpr x]) ->
        let fx = f x in
        M.Mkeccak (fx)

      | A.Pcall (None, A.Cconst A.Chashkey, [AExpr x]) ->
        let fx = f x in
        M.Mhashkey (fx)

      | A.Pcall (None, A.Cconst A.Cchecksignature, [AExpr k; AExpr s; AExpr x]) ->
        let fk = f k in
        let fs = f s in
        let fx = f x in
        M.Mchecksignature (fk, fs, fx)

      | A.Pcall (None, A.Cconst A.Ccontractaddress, [AExpr x]) ->
        let fx = f x in
        M.Mcontractaddress (fx)

      | A.Pcall (None, A.Cconst A.Caddresscontract, [AExpr x]) ->
        let fx = f x in
        M.Maddresscontract (fx)

      | A.Pcall (None, A.Cconst A.Ckeyaddress, [AExpr x]) ->
        let fx = f x in
        M.Mkeyaddress (fx)

      | A.Pcall (_, A.Cid id, args) ->
        M.Mapp (id, List.map (fun x -> term_arg_to_expr f x) args)


      (* | A.Pcall (None, A.Cconst (A.Cmaybeperformedonlybyrole), [AExpr l; AExpr r]) ->
         M.MsecMayBePerformedOnlyByRole (f l, f r)

         | A.Pcall (None, A.Cconst (A.Cmaybeperformedonlybyaction), [AExpr l; AExpr r]) ->
         M.MsecMayBePerformedOnlyByEntry (f l, f r)

         | A.Pcall (None, A.Cconst (A.Cmaybeperformedbyrole), [AExpr l; AExpr r]) ->
         M.MsecMayBePerformedByRole (f l, f r)

         | A.Pcall (None, A.Cconst (A.Cmaybeperformedbyaction), [AExpr l; AExpr r]) ->
         M.MsecMayBePerformedByEntry (f l, f r) *)


      (* Voting *)

      | A.Pcall (None, A.Cconst A.Cvotingpower, [AExpr x]) ->
        let fx = f x in
        M.Mvotingpower (fx)


      (* Ticket *)

      | A.Pcall (None, A.Cconst A.Ccreateticket, [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mcreateticket (fa, fb)

      | A.Pcall (None, A.Cconst A.Creadticket, [AExpr x]) ->
        let fx = f x in
        M.Mreadticket fx

      | A.Pcall (None, A.Cconst A.Csplitticket, [AExpr a; AExpr b; AExpr c]) ->
        let fa = f a in
        let fb = f b in
        let fc = f c in
        M.Msplitticket (fa, fb, fc)

      | A.Pcall (None, A.Cconst A.Cjointickets, [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mjointickets (fa, fb)


      (* Sapling *)

      | A.Pcall (None, A.Cconst A.Csapling_empty_state, [AExpr x]) -> begin
          let fx = f x in
          match fx.node with
          | M.Mnat n -> M.Msapling_empty_state (Big_int.int_of_big_int n)
          | _ -> assert false
        end

      | A.Pcall (None, A.Cconst A.Csapling_verify_update, [AExpr x; AExpr y]) ->
        let fx = f x in
        let fy = f y in
        M.Msapling_verify_update (fx, fy)


      (* Bls curve *)

      | A.Pcall (None, A.Cconst A.Cpairing_check, [AExpr x]) ->
        let fx = f x in
        M.Mpairing_check fx


      (* Timelock *)

      | A.Pcall (None, A.Cconst A.Copen_chest, [AExpr x; AExpr y; AExpr z]) ->
        let fx = f x in
        let fy = f y in
        let fz = f z in
        M.Mopen_chest(fx, fy, fz)


      (* Operation *)

      | A.Pcall (None, A.Cconst (A.Cmkoperation), [AExpr a; AExpr b; AExpr c]) ->
        let fa = f a in
        let fb = f b in
        let fc = f c in
        M.Mmkoperation (fa, fb, fc)


      (* Lambda *)

      | A.Pcall (None, A.Cconst (A.Cexec), [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mexeclambda (fa, fb)

      | A.Pcall (None, A.Cconst (A.Capply), [AExpr a; AExpr b]) ->
        let fa = f a in
        let fb = f b in
        M.Mapplylambda (fa, fb)

      (* Other *)

      | A.Pcall (None, A.Cconst A.CdateFromTimestamp, [AExpr x]) ->
        let fx = f x in
        M.Mdatefromtimestamp (fx)

      | A.Pcall (None, A.Cconst A.CmutezToNat, [AExpr x]) ->
        let fx = f x in
        M.Mmuteztonat (fx)

      (* Fail *)

      | A.Pcall (aux, A.Cconst c, args) ->
        Format.eprintf "expr const unkown: %a with nb args: %d [%a] %s@."
          A.pp_const c
          (List.length args)
          (Printer_tools.pp_list "; " (fun fmt x ->
               let str = match x with | A.AExpr _ -> "AExpr" | A.AEffect _ -> "AEffect" | A.AFun _ -> "AFun" | A.ASorting _ -> "ASorting" in
               Printer_tools.pp_str fmt str)) args
          (match aux with | Some _ -> "with aux" | _ -> "without aux");
        assert false
    in
    M.mk_mterm node type_ ~loc:pterm.loc
  in

  let to_label_lterm (env : env) (x : A.lident A.label_term) : M.label_term =
    M.mk_label_term (to_mterm { env with formula = true } x.term) (Option.get x.label) ~loc:x.loc
  in


  let extract_asset_name (pterm : M.mterm) : Ident.ident =
    match pterm with
    | {type_ = (Tcontainer ((Tasset asset_name, _), _), _); _ } -> unloc asset_name
    | _ -> assert false
  in

  let to_variable_kind = function
    | A.VKconstant  -> M.VKconstant
    | A.VKvariable  -> M.VKvariable
  in

  let process_var (env : env) (v : A.lident A.variable) : M.decl_node =
    let t : M.type_ = type_to_type (Option.get v.decl.typ) in
    let invariants = List.map (fun x -> to_label_lterm env x) v.invs in
    let var : M.var = M.mk_var v.decl.name t t (to_variable_kind v.kind) ?default:(Option.map (to_mterm env) v.decl.default) ~invariants:invariants ~loc:v.loc in
    M.Dvar var
  in

  let process_enum (env : env) (e : A.enum) : M.decl_node =
    let values = List.map (fun (x : A.lident A.enum_item_struct) ->
        let id : M.lident = x.name in
        M.mk_enum_item id ~args:(List.map type_to_type x.args) ~invariants:(List.map (fun x -> to_label_lterm env x) x.invariants)
      ) e.items in
    let initial : A.lident option = List.fold_left (fun accu (x : A.lident A.enum_item_struct) -> match x.initial with | true -> Some x.name | _ -> accu) None e.items in
    (* let initial = (match initial with | Some x -> x | _ -> emit_error (NoInitialValueFor (unloc e.name))) in *)
    let enum = M.mk_enum (A.Utils.get_enum_name e) (Option.get initial) ~values:values in
    M.Denum enum
  in

  let process_asset (env : env) (a : A.asset) : M.decl_node =
    let env = {env with asset_name = Some (unloc a.name)} in
    let values = List.map (fun (x : A.lident A.decl_gen) ->
        let typ = Option.get (Option.map type_to_type x.typ) in
        let default = Option.map (to_mterm env) x.default in
        M.mk_asset_item x.name typ typ ?default:default ~shadow:x.shadow ~loc:x.loc) a.fields
    in
    let mk_asset an l = let l = List.map (to_mterm env) l in M.mk_mterm (M.Masset l) (M.tasset an) ~loc:(Location.mergeall (List.map (fun (x : M.mterm) -> x.loc) l)) in
    let mp = match a.map_kind with | A.MKMap -> M.MKMap | A.MKBigMap -> M.MKBigMap | A.MKIterableBigMap -> M.MKIterableBigMap in
    let r : M.asset = M.mk_asset a.name ~keys:(List.map unloc (a.keys)) ~values:values ~sort:a.sort ~map_kind:mp ?state:a.state ~invariants:(List.map (fun x -> (to_label_lterm env) x) a.specs) ~init:(List.map (fun x -> (mk_asset a.name) x) a.init) ~loc:a.loc in
    M.Dasset r
  in

  let process_record (r : A.record) : M.record =
    let rec for_pos (pos : A.lident A.position) : M.position =
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
          M.mk_record_field x.name typ ~loc:x.loc) r.fields
    in
    M.mk_record r.name ~fields:fs ~pos ~loc:r.loc
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
      let n : A.lident A.instruction_node = instr.node in

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
            | _, M.Tcontainer ((Tasset an, _), Collection) -> M.ICKcoll (unloc an)
            | M.Mdotassetfield (an, _k, fn), M.Tcontainer ((Tasset _, _), (Aggregate | Partition)) -> M.ICKfield (unloc an, unloc fn, x)
            | _ -> M.ICKview x
          in
          let i =
            match i with
            | A.FIsimple x      -> M.FIsimple x
            | A.FIdouble (x, y) -> M.FIdouble (x, y)
          in
          M.Mfor (i, ncol, g body, instr.label)
        end
      | A.Iiter (i, a, b, body)   -> M.Miter (i, f a, f b, g body, instr.label, false)
      | A.Iwhile (c, body)        -> M.Mwhile (f c, g body, instr.label)
      | A.Iletin (i, init, cont)  -> M.Mletin ([i], f init, Option.map type_to_type init.type_, g cont, None) (* TODO *)
      | A.Ideclvar (i, v, c)      -> M.Mdeclvar ([i], Option.map type_to_type v.type_, f v, c) (* TODO *)
      | A.Iseq l                  -> M.Mseq (List.map g l)
      | A.Imatchwith (m, l)       -> M.Mmatchwith (f m, List.map (fun (p, i) -> (to_pattern p, g i)) l)
      | A.Imatchoption (x, id, ve, ne)      -> M.Minstrmatchoption   (f x, id, g ve, g ne)
      | A.Imatchor (x, lid, le, rid, re)    -> M.Minstrmatchor       (f x, lid, g le, rid, g re)
      | A.Imatchlist (x, hid, tid, hte, ee) -> M.Minstrmatchlist     (f x, hid, tid, g hte, g ee)
      | A.Iassign (op, t, `Var x, e) -> begin
          let e = f e in
          let t = type_to_type t in
          let assign_kind =
            match unloc x with
            | "operations" -> M.Aoperations
            | _            -> M.Avar x
          in
          M.Massign (to_assignment_operator op, t, assign_kind, e)
        end
      | A.Iassign (op, t, `Field (rn, o, fn), v) -> begin
          let v = f v in
          let t = type_to_type t in
          let ak =
            match o.type_ with
            | Some (A.Trecord rn) -> M.Arecord(rn, fn, f o)
            | _ -> M.Aasset (rn, fn, f o)
          in
          M.Massign (to_assignment_operator op, t, ak, v)
        end
      | A.Iassign (op, t, `Asset (an, k, fn), v) -> begin
          let v = f v in
          let t = type_to_type t in
          let ak = M.Aasset (an, fn, f k) in
          M.Massign (to_assignment_operator op, t, ak, v)
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
            | TTself (v, id, args)          -> M.TKself (f v, unloc id, List.map (fun (id, v) -> unloc id, f v) args)
            | TToperation v                 -> M.TKoperation (f v)
          in
          M.Mtransfer tr
        end
      | A.Iemit (e, v) -> M.Memit (e, f v)
      | A.Ireturn e -> M.Mreturn (f e)
      | A.Ilabel  i -> M.Mlabel i
      | A.Ifail   m -> M.Mfail (Invalid (f m))

      | A.Icall (i, Cid id, args) -> M.Mapp (id, Option.map_dfl (fun v -> [f v]) [] i @ List.map (term_arg_to_expr f) args)

      | A.Icall (_, A.Cconst (A.Cfail), [AExpr p]) ->
        M.Mfail (Invalid (f p))

      | A.Icall (Some p, A.Cconst (A.Cadd), [AExpr q]) when is_asset_container p -> (
          let fp = f p in
          let fq = f q in
          match fp with
          | {node = M.Mvar (asset_name, Vstorecol, _, _); _} -> M.Maddasset (unloc asset_name, fq)
          | {node = M.Mdotassetfield (asset_name , k, fn); _} -> M.Maddfield (unloc asset_name, unloc fn, k, fq)
          | _ -> assert false
        )

      | A.Icall (Some p, A.Cconst (A.Cput), [AExpr q]) when is_asset_container p -> (
          let fp = f p in
          let fq = f q in
          match fp with
          | {node = M.Mvar (asset_name, Vstorecol, _, _); _} -> M.Mputsingleasset (unloc asset_name, fq)
          | _ -> assert false
        )

      | A.Icall (Some p, A.Cconst (A.Cput), [AExpr k; AExpr v]) when is_asset_container p -> (
          let fp = f p in
          let fk = f k in
          let fv = f v in
          match fp with
          | {node = M.Mvar (asset_name, Vstorecol, _, _); _} -> M.Mputasset (unloc asset_name, fk, fv)
          | _ -> assert false
        )

      | A.Icall (Some p, A.Cconst (A.Cremove), [AExpr q]) when is_asset_container p -> (
          let fp = f p in
          let fq = f q in
          match fp with
          | {node = M.Mvar (asset_name, Vstorecol, _, _); _} -> M.Mremoveasset (unloc asset_name, fq)
          | {node = M.Mdotassetfield (asset_name , k, fn); _} -> M.Mremovefield (unloc asset_name, unloc fn, k, fq)
          | _ -> assert false
        )

      | A.Icall (Some p, A.Cconst (A.Cremoveall), []) when is_asset_container p -> (
          let fp = f p in
          match fp with
          | {node = M.Mvar (_, Vstorecol, _, _); _} -> emit_error (instr.loc, NoRemoveAllOnCollection); bailout ()
          | {node = M.Mdotassetfield (asset_name , k, fn); _} -> M.Mremoveall (unloc asset_name, unloc fn, k)
          | _ -> assert false
        )

      | A.Icall (Some p, A.Cconst (A.Cremoveif), [AFun (_id, _type, l, q)]) ->
        let fp = f p in
        let lambda_body = f q in
        let lambda_args, args = List.fold_right (fun (x, y, z) (l1, l2) -> ((unloc x, type_to_type y)::l1, (f z)::l2)) l ([], []) in
        begin
          match fp.node, M.get_ntype fp.type_ with
          | Mdotassetfield (an, k, fn), _ -> M.Mremoveif (unloc an, CKfield (unloc an, unloc fn, k, Tnone, Dnone), lambda_args, lambda_body, args)
          | _, Tcontainer ((Tasset an, _), _)  -> M.Mremoveif (unloc an, CKcoll (Tnone, Dnone), lambda_args, lambda_body, args)
          | _ -> assert false
        end

      | A.Icall (Some p, A.Cconst (A.Cclear), []) -> (
          let fp = f p in
          begin
            match fp.node, M.get_ntype fp.type_ with
            | Mdotassetfield (an, k, fn), _ -> M.Mclear (unloc an, CKfield (unloc an, unloc fn, k, Tnone, Dnone))
            | _, Tcontainer ((Tasset an, _), _)  -> M.Mclear (unloc an, to_ck env fp)
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
        let fe = List.map (fun (id, op, c) -> (id, to_op op, f c)) e in
        begin
          match fp.node, M.get_ntype fp.type_ with
          | Mdotassetfield (_, _k, fn), Tcontainer ((Tasset an, _), (Aggregate | Partition)) -> M.Maddupdate (unloc an, CKfield (unloc an, unloc fn, fp, Tnone, Dnone), fk, fe)
          | _, Tcontainer ((Tasset an, _), Collection)  -> M.Maddupdate (unloc an, CKcoll (Tnone, Dnone), fk, fe)
          | _ -> assert false
        end

      | A.Icall (Some p, A.Cconst (A.Cupdate), [AExpr k; AEffect e]) when is_asset_container p ->
        let to_op = function
          | `Assign op -> to_assignment_operator op
          | _ -> emit_error (instr.loc, CannotConvertToAssignOperator); bailout ()
        in
        let fp = f p in
        let fk = f k in
        let fe = List.map (fun (id, op, c) -> (id, to_op op, f c)) e in
        let asset_name = extract_asset_name fp in
        M.Mupdate (asset_name, fk, fe)

      | A.Icall (aux, A.Cconst c, args) ->
        Format.eprintf "instr const unkown: %a with nb args: %d [%a] %s@."
          A.pp_const c
          (List.length args)
          (Printer_tools.pp_list "; " (fun fmt (x : A.pterm_arg) ->
               let str = match x with | AExpr _ -> "AExpr" | AEffect _ -> "AEffect" | AFun _ -> "AFun" | ASorting _ -> "ASorting" in
               Printer_tools.pp_str fmt str)) args
          (match aux with | Some _ -> "with aux" | _ -> "without aux");
        assert false
    in
    M.mk_mterm node M.tunit ~loc:instr.loc
  in

  let to_predicate (env : env) (p : A.lident A.predicate) : M.predicate =
    M.mk_predicate p.name (to_mterm { env with formula = true } p.body) ~args:(List.map (fun (id, type_) -> (id, type_to_type type_)) p.args) ~loc:p.loc
  in

  let to_definition (env : env) (d : A.lident A.definition ): M.definition =
    let env = { env with formula = true } in
    M.mk_definition d.name (type_to_type d.typ) d.var (to_mterm env d.body) ~loc:d.loc
  in

  let to_fail (env : env) (p : A.lident A.fail) : M.fail =
    M.mk_fail p.label p.fid p.arg (type_to_type p.atype) (to_mterm { env with formula = true } p.formula) ~loc:p.loc
  in

  let to_variable (env : env) (v : A.lident A.variable) : M.variable =
    M.mk_variable
      ((fun (arg : A.lident A.decl_gen) : (M.lident * M.type_ * M.mterm option) ->
          (arg.name, type_to_type (Option.get arg.typ), Option.map (to_mterm env) arg.default)) v.decl)
      (to_variable_kind v.kind)
      ~loc:v.loc
  in

  let to_invariant (env : env) (i : A.lident A.invariant) :M.invariant  =
    M.mk_invariant i.label ~formulas:(List.map (to_mterm { env with formula = true }) i.formulas)
  in

  let to_postcondition (env : env) (s : A.lident A.postcondition) : M.postcondition  =
    M.mk_postcondition s.name Post (to_mterm { env with formula = true } s.formula)
      ~invariants:(List.map (to_invariant env) s.invariants) ~uses:s.uses
  in

  let to_assert (env : env) (s : A.lident A.assert_) : M.postcondition  =
    M.mk_postcondition s.name Assert (to_mterm env s.formula)
      ~invariants:(List.map (to_invariant env) s.invariants) ~uses:s.uses
  in

  let to_specification (env : env) (v : A.lident A.specification) : M.specification =
    let predicates     = List.map (to_predicate   env) v.predicates  in
    let definitions    = List.map (to_definition  env) v.definitions in
    let lemmas         = List.map (to_label_lterm env) v.lemmas      in
    let theorems       = List.map (to_label_lterm env) v.theorems    in
    let fails          = List.map (to_fail env)        v.fails in
    let variables      = List.map (fun x -> to_variable env x) v.variables in
    let invariants     = List.map (fun (a, l) -> (a, List.map (fun x -> to_label_lterm env x) l)) v.invariants in
    let effects        = Option.map_dfl (fun x -> [to_instruction env x]) [] v.effect in
    let postconditions = List.map (to_postcondition env) v.specs @ List.map (to_assert env) v.asserts in
    M.mk_specification
      ~predicates:predicates
      ~definitions:definitions
      ~lemmas:lemmas
      ~theorems:theorems
      ~fails:fails
      ~variables:variables
      ~invariants:invariants
      ~effects:effects
      ~postconditions:postconditions
      ~loc:v.loc ()
  in

  let cont_specification (env : env) (v : A.lident A.specification) (spec : M.specification) : M.specification =
    let v = to_specification env v in
    { spec with
      predicates     = spec.predicates @ v.predicates;
      definitions    = spec.definitions @ v.definitions;
      lemmas         = spec.lemmas @ v.lemmas;
      theorems       = spec.theorems @ v.theorems;
      variables      = spec.variables @ v.variables;
      invariants     = spec.invariants @ v.invariants;
      effects        = spec.effects @ v.effects;
      postconditions = spec.postconditions @ v.postconditions;
      loc            = Location.merge spec.loc v.loc;
    }
  in

  let cont_security (s : A.security) (sec : M.security) : M.security =
    let to_security_item (si : A.security_item) : M.security_item =
      let to_security_predicate (sn : A.security_predicate) : M.security_predicate =
        let to_security_node (sn : A.security_node) : M.security_node =
          let to_security_entry (sa : A.security_entry) : M.security_entry =
            match sa with
            | Sany -> Sany
            | Sentry l -> Sentry l
          in
          match sn with
          | SonlyByRole         (ad, roles)         -> SonlyByRole         (to_entry_description ad, roles)
          | SonlyInEntry        (ad, action)        -> SonlyInEntry        (to_entry_description ad, to_security_entry action)
          | SonlyByRoleInEntry  (ad, roles, action) -> SonlyByRoleInEntry  (to_entry_description ad, roles, to_security_entry action)
          | SnotByRole          (ad, roles)         -> SnotByRole          (to_entry_description ad, roles)
          | SnotInEntry         (ad, action)        -> SnotInEntry         (to_entry_description ad, to_security_entry action)
          | SnotByRoleInEntry   (ad, roles, action) -> SnotByRoleInEntry   (to_entry_description ad, roles, to_security_entry action)
          | StransferredBy      (ad)                -> StransferredBy      (to_entry_description ad)
          | StransferredTo      (ad)                -> StransferredTo      (to_entry_description ad)
          | SnoStorageFail      (action)            -> SnoStorageFail      (to_security_entry action)
        in
        M.mk_security_predicate (to_security_node sn.s_node) ~loc:sn.loc
      in
      M.mk_security_item
        si.label
        (to_security_predicate si.predicate)
        ~loc:si.loc
    in

    let new_s : M.security = M.mk_security
        ~items:(List.map to_security_item s.items)
        ~loc:s.loc
        ()
    in
    { items = sec.items @ new_s.items; loc = new_s.loc; }
  in

  let process_fun_gen name args (body : M.mterm) loc spec f : M.function__ =
    let node = f (M.mk_function_struct name body
                    ~args:args
                    ~loc:loc) in
    M.mk_function ?spec:spec node
  in

  let process_function (env : env) (function_ : A.function_) : M.function__ =
    let name  = function_.name in
    let args  = List.map (fun (x : A.lident A.decl_gen) -> (x.name, (type_to_type |@ Option.get) x.typ, None)) function_.args in
    let env   = {env with function_p = Some (name, args); } in
    let body  = to_instruction env function_.body in
    let loc   = function_.loc in
    let ret   = type_to_type function_.return in
    let spec : M.specification option = Option.map (to_specification env) function_.specification in
    let f     = match function_.kind with
      | FKfunction -> (fun x -> M.Function (x, ret))
      | FKgetter -> (fun x -> M.Getter (x, ret))
      | FKview -> (fun x -> M.View (x, ret))
    in
    process_fun_gen name args body loc spec f
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

  let process_transaction (env : env) (transaction : A.transaction) : M.function__ =
    let process_calledby env (body : M.mterm) : M.mterm =
      let process_cb caller (cb : A.rexpr) (body : M.mterm) : M.mterm =
        let rec process_rexpr (rq : A.rexpr) : M.mterm option =
          match rq.node with
          | Rany -> None
          | Rasset a -> begin
              let an = unloc a in
              Some (M.mk_mterm (M.Mcontains(an, CKcoll(Tnone, Dnone), caller)) M.tbool ~loc:(loc a))
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
        match process_rexpr cb with
        | Some a ->
          let require : M.mterm = M.mk_mterm (M.Mnot (a)) (M.tbool) ~loc:cb.loc in
          let fail_auth : M.mterm = fail InvalidCaller in
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
        |> process transaction.calledby  M.mcaller
        |> process transaction.sourcedby M.msource
      end
    in

    let process_state_is _env (body : M.mterm) : M.mterm =
      match transaction.state_is with
      | Some id -> begin
          let var     = M.mk_state_value id in
          let state   = M.mk_state_var () in
          let c       = M.mk_mterm (M.Mnequal (M.tstate, var, state)) (M.tbool) ~loc:(loc id) in
          let cond_if = M.mk_mterm (M.Mif (c, fail InvalidState, None)) M.tunit in
          add_seq cond_if body
        end
      | _ -> body
    in

    let process env b (x : A.lident A.label_term) (body : M.mterm) : M.mterm =
      let term = to_mterm env x.term in
      let cond : M.mterm =
        match b with
        | `Require ->  M.mk_mterm (M.Mnot term) (M.tbool) ~loc:x.loc
        | `Failif -> term
      in
      let fail_cond : M.mterm =
        let a =
          match x.error with
          | Some v -> (M.Invalid (to_mterm env v))
          | None   -> (M.InvalidCondition (x.label |> Option.get |> unloc))
        in
        fail a
      in
      let cond_if = M.mk_mterm (M.Mif (cond, fail_cond, None)) M.tunit ~loc:x.loc in
      add_seq cond_if body
    in
    let apply env b li body =
      match li with
      | None -> body
      | Some l -> List.fold_right (fun (x : A.lident A.label_term) (accu : M.mterm) -> process env b x accu) l body
    in
    let process_requires env (body : M.mterm) : M.mterm =
      body
      |>  apply env `Failif  transaction.failif
      |>  apply env `Require transaction.require
    in

    let process_accept_transfer _env (body : M.mterm) : M.mterm =
      if (not transaction.accept_transfer)
      then
        let lhs : M.mterm = M.mk_mterm (M.Mtransferred) M.ttez in
        let rhs : M.mterm = M.mk_mterm (M.Mcurrency (Big_int.zero_big_int, Utz)) M.ttez in
        let eq : M.mterm = M.mk_mterm (M.Mequal (M.ttez, lhs, rhs)) M.tbool in
        let cond : M.mterm = M.mk_mterm (M.Mnot eq) M.tbool in
        let cond_if : M.mterm = M.mk_mterm (M.Mif (cond, fail (NoTransfer), None)) M.tunit in
        add_seq cond_if body
      else
        body
    in

    let process_body_args env : M.argument list * M.mterm * env =
      let args  = List.map (fun (x : A.lident A.decl_gen) -> (x.name, (type_to_type |@ Option.get) x.typ, None)) transaction.args in
      let env   = {env with function_p = Some (transaction.name, args); } in
      let empty : M.mterm = M.mk_mterm (M.Mseq []) M.tunit in
      match transaction.transition, transaction.effect with
      | None, None ->
        let body = empty in
        args, body, env
      | None, Some e ->
        let body = to_instruction env e in
        args, body, env
      | Some t, None ->
        let p_on =
          match t.on with
          | Some (key_ident, key_type, {pldesc = asset_name}, enum_type) ->
            Some (key_ident, type_to_type key_type, asset_name, type_to_type enum_type)
          | None -> None
        in
        let args =
          match p_on with
          | Some (ki, kt, _an, _) -> args @ [(ki, kt, None)]
          | None -> args
        in
        let env = {env with function_p = Some (transaction.name, args); } in
        let build_code (body : M.mterm) : M.mterm =
          (List.fold_right (fun ((id, cond, effect) : (A.lident * A.pterm option * A.instruction option)) (acc : M.mterm) : M.mterm ->
               let tre : M.mterm =
                 match p_on with
                 | Some (key_ident, key_type, an, enum_type) ->
                   let k : M.mterm = build_mvar env key_ident key_type ~loc:(Location.loc key_ident) in
                   let et = match M.get_ntype enum_type with | M.Tenum id -> unloc id | _ -> assert false in
                   let v : M.mterm = M.mk_mterm (M.Menumval (id, [], et)) enum_type ~loc:(Location.loc id) in
                   M.mk_mterm (M.Massign (ValueAssign, v.type_, Aassetstate (an, k), v)) M.tunit
                 | _ ->
                   (* let v : M.mterm = build_mvar env id M.tstate ~loc:(Location.loc id) in *)
                   let v : M.mterm = M.mk_mterm (Menumval (id, [], "state")) (M.tenum (dumloc "state")) ~loc:(Location.loc id) in
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
                | Sref id -> [M.mk_pattern (M.Pconst (id, []))]
                | Sor (a, b) -> [a; b] |> List.map (fun x -> compute_patterns x) |> List.flatten
                | Sany -> emit_error (a.loc, AnyNotAuthorizedInTransitionTo); bailout ()
              in
              let list_patterns : M.pattern list =
                compute_patterns t.from in

              let pattern : M.pattern = M.mk_pattern M.Pwild in
              let fail_instr : M.mterm = fail InvalidState in

              let w =
                match p_on with
                | Some (ki, kt, an, et) ->
                  let k : M.mterm = build_mvar env ki kt ~loc:(Location.loc ki) in
                  M.mk_mterm (M.Mvar (dumloc an, Vassetstate k, Tnone, Dnone)) et
                | _ -> M.mk_mterm (M.Mvar(dumloc "", Vstate, Tnone, Dnone)) M.tstate
              in
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
    let loc   = transaction.loc in
    let spec : M.specification option = Option.map (to_specification env) transaction.specification in

    process_fun_gen transaction.name args body loc spec (fun x -> M.Entry x)
  in

  let process_parameter env (p : A.lident A.parameter) : M.parameter =
    {
      name    = p.name;
      typ     = type_to_type p.typ;
      default = Option.map (to_mterm env) p.default;
      value   = Option.map (to_mterm env) p.value;
      const   = p.const;
      loc     = p.loc;
    }
  in

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

  let parameters = List.map (process_parameter env) ast.parameters in
  let metadata = Option.map (function | A.MKuri x -> M.MKuri x | A.MKjson x -> M.MKjson x) ast.metadata in
  let decls = List.map (process_decl_ env) ast.decls in
  let functions = List.map (process_fun_ env) ast.funs in

  let specification =
    M.mk_specification ()
    |> (fun spec -> List.fold_left (fun accu x -> cont_specification env x accu) spec ast.specifications)
  in

  let security =
    M.mk_security ()
    |> (fun sec -> List.fold_left (fun accu x -> cont_security x accu) sec ast.securities)
  in

  M.mk_model ~parameters ?metadata ~decls ~functions ~specification ~security ~loc:ast.loc name
