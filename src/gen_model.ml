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
[@@deriving show {with_path = false}]

type error = Location.t * error_desc

let pp_error_desc fmt = function
  | CannotConvertToAssignOperator  -> Format.fprintf fmt "cannot convert to assign operator"
  | CannotExtractBody              -> Format.fprintf fmt "cannot extract body"
  | AnyNotAuthorizedInTransitionTo -> Format.fprintf fmt "any not authorized in transition to"
  | NoRemoveAllOnCollection        -> Format.fprintf fmt "remove all cannot be called for a collection of asset"

let emit_error (lc, error : Location.t * error_desc) =
  let str : string = Format.asprintf "%a@." pp_error_desc error in
  let pos : Position.t list = [location_to_position lc] in
  Error.error_alert pos str (fun _ -> ())

let bailout = fun () -> raise (Error.Stop 5)

let to_model (ast : A.model) : M.model =

  let to_container c =
    match c with
    | A.Collection -> M.Collection
    | A.Aggregate     -> M.Aggregate
    | A.Partition  -> M.Partition
    | A.View       -> M.View
    (* | _            -> emit_error (NotSupportedContainer (Format.asprintf "%a@." A.pp_container c)) *)
  in

  let to_currency = function
    | A.Tz   -> M.Tz
    | A.Mtz  -> M.Mtz
    | A.Utz  -> M.Utz
  in

  let vtyp_to_btyp = function
    | A.VTbool       -> M.Bbool
    | A.VTint        -> M.Bint
    | A.VTrational   -> M.Brational
    | A.VTdate       -> M.Bdate
    | A.VTduration   -> M.Bduration
    | A.VTstring     -> M.Bstring
    | A.VTaddress    -> M.Baddress
    | A.VTrole       -> M.Brole
    | A.VTcurrency   -> M.Bcurrency
    | A.VTsignature  -> M.Bsignature
    | A.VTkey        -> M.Bkey
    | A.VTkeyhash    -> M.Bkeyhash
    | A.VTbytes      -> M.Bbytes
  in

  let to_trtyp = function
    | A.TRentry  -> M.TRentry
    | A.TRaction -> M.TRaction
    | A.TRasset  -> M.TRasset
    | A.TRfield  -> M.TRfield
  in

  let rec ptyp_to_type (t : A.ptyp) : M.type_ =
    match t with
    | A.Tnamed _           -> assert false
    | A.Tasset id          -> M.Tasset id
    | A.Tenum id           -> M.Tenum id
    | A.Tcontract id       -> M.Tcontract id
    | A.Tbuiltin b         -> M.Tbuiltin (vtyp_to_btyp b)
    | A.Tcontainer (t, c)  -> M.Tcontainer (ptyp_to_type t, to_container c)
    | A.Tlist t            -> M.Tlist (ptyp_to_type t)
    | A.Ttuple l           -> M.Ttuple (List.map ptyp_to_type l)
    | A.Tentry             -> M.Tentry
    | A.Toption t          -> M.Toption (ptyp_to_type t)
    | A.Ttrace tr          -> M.Ttrace (to_trtyp tr)
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

  let rec to_qualid_node (n : A.lident A.qualid_node) : ('id, 'qualid) M.qualid_node =
    match n with
    | A.Qident i    -> M.Qident i
    | A.Qdot (d, i) -> M.Qdot (to_qualid_gen d, i)

  and to_qualid_gen (q : A.qualid) : M.qualid =
    let node = to_qualid_node q.node in
    let type_ = ptyp_to_type (Option.get q.type_) in
    M.mk_qualid node type_
  in

  let to_pattern_node (n : A.lident A.pattern_node) : 'id M.pattern_node =
    match n with
    | A.Mconst id -> M.Pconst id
    | A.Mwild    -> M.Pwild
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
    M.mk_mterm (Mfail ft) M.Tunit
  in

  let term_not x : M.mterm =
    M.mk_mterm (M.Mnot x) (M.Tbuiltin Bbool)
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
    | {type_ = Tcontainer (Tasset asset_name, _); _} -> unloc asset_name
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
    | {type_ = Tlist t; _} -> t
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

  let to_ck (fp : M.mterm) : M.container_kind =
    match fp.node, fp.type_ with
    | M.Mdotassetfield (an, _k, fn), Tcontainer ((Tasset _), (Aggregate | Partition)) -> M.CKfield (unloc an, unloc fn, fp)
    | _, Tcontainer ((Tasset _), Collection) -> M.CKcoll
    | _ -> M.CKview fp
  in

  let rec to_mterm ?(formula=false) (pterm : A.pterm) : M.mterm =
    let process_before vt e =
      match vt with
      | A.VTbefore -> M.Msetbefore (M.mk_mterm e (ptyp_to_type (Option.get pterm.type_)) ~loc:pterm.loc)
      | A.VTat lbl -> M.Msetat (lbl, M.mk_mterm e (ptyp_to_type (Option.get pterm.type_)) ~loc:pterm.loc)
      | A.VTnone -> e
    in
    let type_ = ptyp_to_type (Option.get pterm.type_) in
    let f x = to_mterm x ~formula:formula in
    let node =
      match pterm.node with
      | A.Pif (c, t, e)                   -> M.Mexprif        (f c, f t, f e)
      | A.Pmatchwith (m, l)               -> M.Mexprmatchwith (f m, List.map (fun (p, e) -> (to_pattern p, f e)) l)
      | A.Plogical (A.And, l, r)          -> M.Mand           (f l, f r)
      | A.Plogical (A.Or, l, r)           -> M.Mor            (f l, f r)
      | A.Plogical (A.Imply, l, r)        -> M.Mimply         (f l, f r)
      | A.Plogical (A.Equiv, l, r)        -> M.Mequiv         (f l, f r)
      | A.Pnot e                          -> M.Mnot           (f e)
      | A.Pmulticomp (e, l)               -> M.Mmulticomp     (f e, List.map (fun (op, e) -> (to_comparison op, f e)) l)
      | A.Pcomp (A.Equal, l, r)           -> M.Mequal         (f l, f r)
      | A.Pcomp (A.Nequal, l, r)          -> M.Mnequal        (f l, f r)
      | A.Pcomp (A.Gt, l, r)              -> M.Mgt            (f l, f r)
      | A.Pcomp (A.Ge, l, r)              -> M.Mge            (f l, f r)
      | A.Pcomp (A.Lt, l, r)              -> M.Mlt            (f l, f r)
      | A.Pcomp (A.Le, l, r)              -> M.Mle            (f l, f r)
      | A.Parith (A.Plus, l, r)           -> M.Mplus          (f l, f r)
      | A.Parith (A.Minus, l, r)          -> M.Mminus         (f l, f r)
      | A.Parith (A.Mult, l, r)           -> M.Mmult          (f l, f r)
      | A.Parith (A.DivRat, l, r)         -> M.Mdivrat        (f l, f r)
      | A.Parith (A.DivEuc, l, r)         -> M.Mdiveuc        (f l, f r)
      | A.Parith (A.Modulo, l, r)         -> M.Mmodulo        (f l, f r)
      | A.Puarith (A.Uplus, e)            -> M.Muplus         (f e)
      | A.Puarith (A.Uminus, e)           -> M.Muminus        (f e)
      | A.Precord l                       -> M.Masset         (List.map f l)
      | A.Pcall (Some p, A.Cconst A.Cbefore,    []) -> M.Msetbefore    (f p)
      | A.Pletin (id, init, typ, body, o) -> M.Mletin         ([id], f init, Option.map ptyp_to_type typ, f body, Option.map f o)
      | A.Pdeclvar (i, t, v)              -> M.Mdeclvar       ([i], Option.map ptyp_to_type t, f v)
      | A.Pvar (b, _vs, {pldesc = "state"; _})                -> let e = M.Mvar (dumloc "", Vstate) in process_before b e
      | A.Pvar (b, _vs, id) when A.Utils.is_variable ast id   -> let e = M.Mvar (id, Vstorevar)     in process_before b e
      | A.Pvar (b, _vs, id) when A.Utils.is_asset ast id      -> let e = M.Mvar (id, Vstorecol)     in process_before b e
      | A.Pvar (b, _vs, id) when A.Utils.is_enum_value ast id -> let e = M.Mvar (id, Venumval)      in process_before b e
      | A.Pvar (b, _vs, id)                                   -> let e = M.Mvar (id, Vlocal)        in process_before b e
      | A.Parray l                             ->
        begin
          let l = List.map f l in
          match type_ with
          | Tcontainer (Tasset _, _)   -> M.Massets l
          | _ -> M.Mlitlist l
        end
      | A.Plit ({node = BVint i; _})           -> M.Mint i
      | A.Plit ({node = BVuint i; _})          -> M.Muint i
      | A.Plit ({node = BVbool b; _})          -> M.Mbool b
      | A.Plit ({node = BVenum s; _})          -> M.Menum s
      | A.Plit ({node = BVrational (d, n); _}) -> M.Mrational (d, n)
      | A.Plit ({node = BVdate s; _})          -> M.Mdate s
      | A.Plit ({node = BVstring s; _})        -> M.Mstring s
      | A.Plit ({node = BVcurrency (c, i); _}) -> M.Mcurrency (i, to_currency c)
      | A.Plit ({node = BVaddress s; _})       -> M.Maddress s
      | A.Plit ({node = BVduration d; _})      -> M.Mduration d
      | A.Plit ({node = BVbytes v; _})         -> M.Mbytes v

      | A.Pdot (e, fn) -> begin
          match e.node with
          | Pcall (Some { node = node }, Cconst Cget, [AExpr k]) ->
            let an =
              let rec aux = function
                | A.Pvar (VTnone, Vnone, an) -> an
                | A.Pcast (_, _, v) -> aux v.node
                | _ -> assert false
              in
              aux node
            in
            M.Mdotassetfield (an, f k, fn)

          | _ ->
            (* handle dot contract too *)
            M.Mdot (f e, fn)
        end

      | A.Pconst Cstate                        -> M.Mvar(dumloc "", Vstate)
      | A.Pconst Cnow                          -> M.Mnow
      | A.Pconst Ctransferred                  -> M.Mtransferred
      | A.Pconst Ccaller                       -> M.Mcaller
      | A.Pconst Cbalance                      -> M.Mbalance
      | A.Pconst Csource                       -> M.Msource
      | A.Pconst c                             ->
        Format.eprintf "expr const unkown: %a@." A.pp_const c;
        assert false

      | A.Ptuple l                             -> M.Mtuple (List.map f l)
      | A.Pnone                                -> M.Mnone
      | A.Psome a                              -> M.Msome (f a)
      | A.Pcast (src, dst, v)                  -> begin
          let v = f v in
          match src, dst with
          | A.Tbuiltin VTcurrency, A.Tbuiltin VTint -> begin
              let one : Core.big_int = Big_int.unit_big_int in
              let u : M.mterm = M.mk_mterm (M.Mcurrency (one, M.Utz)) (M.Tbuiltin Bcurrency) in
              M.Mdivtez (v, u)
            end
          | _, _ -> M.Mcast (ptyp_to_type src, ptyp_to_type dst, v)
        end
      | A.Pquantifer (Forall, i, (coll, typ), term)    -> M.Mforall (i, ptyp_to_type typ, Option.map f coll, f term)
      | A.Pquantifer (Exists, i, (coll, typ), term)    -> M.Mexists (i, ptyp_to_type typ, Option.map f coll, f term)

      (* | A.Pcall (Some p, A.Cconst A.Cbefore,    []) -> M.Msetbefore    (f p) *)
      (* | A.Pcall (Some p, A.Cconst A.Cunmoved,   []) -> M.Msetunmoved   (f p)
         | A.Pcall (Some p, A.Cconst A.Cadded,     []) -> M.Msetadded     (f p)
         | A.Pcall (Some p, A.Cconst A.Cremoved,   []) -> M.Msetremoved   (f p) *)

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

      | A.Pcall (None, A.Cconst A.Cgetopt, [AExpr x]) ->
        let fx = f x in
        M.Mgetopt (fx)

      | A.Pcall (None, A.Cconst A.Cfloor, [AExpr x]) ->
        let fx = f x in
        M.Mfloor (fx)

      | A.Pcall (None, A.Cconst A.Cceil, [AExpr x]) ->
        let fx = f x in
        M.Mceil (fx)

      | A.Pcall (None, A.Cconst A.Cpack, [AExpr x]) ->
        let fx = f x in
        M.Mpack (fx)

      | A.Pcall (None, A.Cconst A.Cunpack, [AExpr x]) ->
        let fx = f x in
        let t =
          match type_ with
          | Toption t -> t
          | _ -> assert false
        in
        M.Munpack (t, fx)

      | A.Pcall (None, A.Cconst A.Cblake2b, [AExpr x]) ->
        let fx = f x in
        M.Mblake2b (fx)

      | A.Pcall (None, A.Cconst A.Csha256, [AExpr x]) ->
        let fx = f x in
        M.Msha256 (fx)

      | A.Pcall (None, A.Cconst A.Csha512, [AExpr x]) ->
        let fx = f x in
        M.Msha512 (fx)

      | A.Pcall (None, A.Cconst A.Chashkey, [AExpr x]) ->
        let fx = f x in
        M.Mhashkey (fx)

      | A.Pcall (None, A.Cconst A.Cchecksignature, [AExpr k; AExpr s; AExpr x]) ->
        let fk = f k in
        let fs = f s in
        let fx = f x in
        M.Mchecksignature (fk, fs, fx)

      | A.Pcall (_, A.Cid id, args) ->
        M.Mapp (id, List.map (fun x -> term_arg_to_expr f x) args)

      (* Asset *)

      | A.Pcall (Some p, A.Cconst (A.Csubsetof), [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Msubsetof (asset_name, to_ck fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Cisempty), []) when is_asset_container p ->
        let fp = f p in
        let asset_name = extract_asset_name fp in
        M.Misempty (asset_name, fp)

      | A.Pcall (Some p, A.Cconst (A.Cget), [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Mget (asset_name, to_ck fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Cselect), [AFun (_id, _type, l, q)]) when is_asset_container p ->
        let fp = f p in
        let lambda_body = f q in
        let asset_name = extract_asset_name fp in
        let lambda_args, args = List.fold_right (fun (x, y, z) (l1, l2) -> ((unloc x, ptyp_to_type y)::l1, (f z)::l2)) l ([], []) in
        M.Mselect (asset_name, to_ck fp, lambda_args, lambda_body, args)

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
        M.Msort (asset_name, to_ck fp, args)

      | A.Pcall (Some p, A.Cconst (A.Ccontains), [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Mcontains (asset_name, to_ck fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Cnth), [AExpr q]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Mnth (asset_name, to_ck fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Ccount), []) when is_asset_container p ->
        let fp = f p in
        let asset_name = extract_asset_name fp in
        M.Mcount (asset_name, to_ck fp)

      | A.Pcall (Some p, A.Cconst (A.Csum), [AFun (_qi, _qt, _l, q)]) when is_asset_container p ->
        let fp = f p in
        let fq = f q in
        let asset_name = extract_asset_name fp in
        M.Msum (asset_name, to_ck fp, fq)

      | A.Pcall (Some p, A.Cconst (A.Chead), [AExpr e]) when is_asset_container p ->
        let fp = f p in
        let fe = f e in
        let asset_name = extract_asset_name fp in
        M.Mhead (asset_name, to_ck fp, fe)

      | A.Pcall (Some p, A.Cconst (A.Ctail), [AExpr e]) when is_asset_container p ->
        let fp = f p in
        let fe = f e in
        let asset_name = extract_asset_name fp in
        M.Mtail (asset_name, to_ck fp, fe)

      (* List*)

      | A.Pcall (None, A.Cconst (A.Cprepend), [AExpr p; AExpr q]) when is_list p -> (
          let fp = f p in
          let fq = f q in
          let t = extract_builtin_type_list fp in
          M.Mlistprepend (t, fp, fq)
        )

      | A.Pcall (None, A.Cconst (A.Ccontains), [AExpr p; AExpr q]) when is_list p ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_list fp in
        M.Mlistcontains (t, fp, fq)

      | A.Pcall (None, A.Cconst (A.Ccount), [AExpr p]) when is_list p ->
        let fp = f p in
        let t = extract_builtin_type_list fp in
        M.Mlistcount (t, fp)

      | A.Pcall (None, A.Cconst (A.Cnth), [AExpr p; AExpr q]) when is_list p ->
        let fp = f p in
        let fq = f q in
        let t = extract_builtin_type_list fp in
        M.Mlistnth (t, fp, fq)


      (* | A.Pcall (None, A.Cconst (A.Cmaybeperformedonlybyrole), [AExpr l; AExpr r]) ->
         M.MsecMayBePerformedOnlyByRole (f l, f r)

         | A.Pcall (None, A.Cconst (A.Cmaybeperformedonlybyaction), [AExpr l; AExpr r]) ->
         M.MsecMayBePerformedOnlyByEntry (f l, f r)

         | A.Pcall (None, A.Cconst (A.Cmaybeperformedbyrole), [AExpr l; AExpr r]) ->
         M.MsecMayBePerformedByRole (f l, f r)

         | A.Pcall (None, A.Cconst (A.Cmaybeperformedbyaction), [AExpr l; AExpr r]) ->
         M.MsecMayBePerformedByEntry (f l, f r) *)

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

  let to_label_lterm (x : A.lident A.label_term) : M.label_term =
    M.mk_label_term (to_mterm x.term ~formula:true) (Option.get x.label) ~loc:x.loc
  in


  let extract_asset_name (pterm : M.mterm) : Ident.ident =
    match pterm with
    | {type_ = Tcontainer (Tasset asset_name, _); _ } -> unloc asset_name
    | _ -> assert false
  in

  let process_var (v : A.lident A.variable) : M.decl_node =
    let t : M.type_ = ptyp_to_type (Option.get v.decl.typ) in
    let invariants = List.map (fun x -> to_label_lterm x) v.invs in
    let var : M.var = M.mk_var v.decl.name t t ~constant:v.constant ?default:(Option.map to_mterm v.decl.default) ~invariants:invariants ~loc:v.loc in
    M.Dvar var
  in

  let process_enum (e : A.enum) : M.decl_node =
    let values = List.map (fun (x : A.lident A.enum_item_struct) ->
        let id : M.lident = x.name in
        M.mk_enum_item id ~invariants:(List.map (fun x -> to_label_lterm x) x.invariants)
      ) e.items in
    let initial : A.lident option = List.fold_left (fun accu (x : A.lident A.enum_item_struct) -> match x.initial with | true -> Some x.name | _ -> accu) None e.items in
    (* let initial = (match initial with | Some x -> x | _ -> emit_error (NoInitialValueFor (unloc e.name))) in *)
    let enum = M.mk_enum (A.Utils.get_enum_name e) (Option.get initial) ~values:values in
    M.Denum enum
  in

  let process_asset (a : A.asset) : M.decl_node =
    let values = List.map (fun (x : A.lident A.decl_gen) ->
        let typ = Option.get (Option.map ptyp_to_type x.typ) in
        let default = Option.map to_mterm x.default in
        M.mk_asset_item x.name typ typ ?default:default ~shadow:x.shadow ~loc:x.loc) a.fields
    in
    let mk_asset an l = M.mk_mterm (M.Masset (List.map to_mterm l)) (M.Tasset an) in
    let r : M.asset = M.mk_asset a.name (unloc (Option.get a.key)) ~values:values ~sort:(List.map unloc (a.sort)) ?state:a.state ~invariants:(List.map (fun x -> to_label_lterm x) a.specs) ~init:(List.map (fun x -> (mk_asset a.name) x) a.init) ~loc:a.loc in
    M.Dasset r
  in

  let to_contract_signature (s : A.lident A.signature) : M.contract_signature =
    let name = s.name in
    M.mk_contract_signature name ~args:(List.map (fun (id, typ) -> (id, ptyp_to_type typ)) s.args) ~loc:s.loc
  in
  let to_contract (c : A.contract) : M.contract =
    M.mk_contract c.name
      ~signatures:(List.map to_contract_signature c.signatures)
      ?init:(Option.map to_mterm c.init)
      ~loc:c.loc
  in


  let extract_contract_type_id (c : A.lident A.term_poly) =
    let aux (t : A.ptyp) =
      match t with
      | A.Tcontract v -> unloc v
      | _ -> assert false
    in
    match c.node, c.type_ with
    | A.Pcast (d, _, _), _ -> aux d
    | _, Some t -> aux t
    | _ -> assert false
  in

  let rec to_instruction (instr : A.instruction) : M.mterm =
    let is_empty_seq (instr : A.instruction) =
      match instr.node with
      | A.Iseq [] -> true
      | _ -> false
    in
    let node =
      let f = to_mterm in
      let g = to_instruction in
      let n : A.lident A.instruction_node = instr.node in

      match n with
      | A.Iif (c, t, e) when is_empty_seq e -> M.Mif (f c, g t, None)
      | A.Iif (c, t, e)           -> M.Mif (f c, g t, Some (g e))
      | A.Ifor (i, col, body)     ->
        begin
          let ncol =
            let x = f col in
            match x.node, x.type_ with
            | _, M.Tlist _ -> M.ICKlist x
            | _, M.Tcontainer ((Tasset an), Collection) -> M.ICKcoll (unloc an)
            | M.Mdotassetfield (an, _k, fn), M.Tcontainer ((Tasset _), (Aggregate | Partition)) -> M.ICKfield (unloc an, unloc fn, x)
            | _ -> M.ICKview x
          in
          M.Mfor (i, ncol, g body, instr.label)
        end
      | A.Iiter (i, a, b, body)   -> M.Miter (i, f a, f b, g body, instr.label)
      | A.Iletin (i, init, cont)  -> M.Mletin ([i], f init, Option.map ptyp_to_type init.type_, g cont, None) (* TODO *)
      | A.Ideclvar (i, v)         -> M.Mdeclvar ([i], Option.map ptyp_to_type v.type_, f v) (* TODO *)
      | A.Iseq l                  -> M.Mseq (List.map g l)
      | A.Imatchwith (m, l)       -> M.Mmatchwith (f m, List.map (fun (p, i) -> (to_pattern p, g i)) l)
      | A.Iassign (op, `Var x, e) -> M.Massign (to_assignment_operator op, Avar x, to_mterm e)
      | A.Iassign (op, `Field (an, k, fn), v) -> M.Massign (to_assignment_operator op, Afield (an, fn, to_mterm k), to_mterm v)
      | A.Irequire (b, t)         ->
        let cond : M.mterm =
          if b
          then term_not (f t)
          else (f t)
        in
        M.Mif (cond, fail (InvalidCondition None), None)

      | A.Itransfer (v, d, None) -> M.Mtransfer (f v, f d)
      | A.Itransfer (v, d, Some (id, args))   ->
        begin
          let contract_id = extract_contract_type_id d in
          let d = f d in
          let v = f v in
          let ids = A.Utils.get_contract_sig_ids ast contract_id (unloc id) in
          let vs = List.map f args in
          let args = List.map2 (fun x y -> (x, y)) ids vs in
          M.Mentrycall (v, d, contract_id, id, args)
        end
      | A.Ibreak                  -> M.Mbreak
      | A.Ireturn e               -> M.Mreturn (f e)
      | A.Ilabel i                -> M.Mlabel i
      | A.Ifail m                 -> M.Mfail (Invalid (f m))
      (* | A.Icall (Some c, Cid id, args) when (match c.type_ with | Some (A.Tcontract _) -> true | _ -> false) -> (* TODO: delete this case *)
         let contract_id = extract_contract_type_id c in
         let c = f c in
         let ids = A.Utils.get_contract_sig_ids ast contract_id (unloc id) in
         let vs = List.map (term_arg_to_expr f) args in
         let args = List.map2 (fun x y -> (x, y)) ids vs in
         let zerotz : M.mterm = M.mk_mterm (Mcurrency (Big_int.zero_big_int, Tz)) (Tbuiltin Bcurrency) in
         M.Mentrycall (zerotz, c, contract_id, id, args) *)

      | A.Icall (i, Cid id, args) -> M.Mapp (id, Option.map_dfl (fun v -> [to_mterm v]) [] i @ List.map (term_arg_to_expr f) args)

      | A.Icall (_, A.Cconst (A.Cfail), [AExpr p]) ->
        M.Mfail (Invalid (f p))

      | A.Icall (Some p, A.Cconst (A.Cadd), [AExpr q]) when is_asset_container p -> (
          let fp = f p in
          let fq = f q in
          match fp with
          | {node = M.Mvar (asset_name, Vstorecol); _} -> M.Maddasset (unloc asset_name, fq)
          | {node = M.Mdotassetfield (asset_name , k, fn); _} -> M.Maddfield (unloc asset_name, unloc fn, k, fq)
          | _ -> assert false
        )

      | A.Icall (Some p, A.Cconst (A.Cremove), [AExpr q]) when is_asset_container p -> (
          let fp = f p in
          let fq = f q in
          match fp with
          | {node = M.Mvar (asset_name, Vstorecol); _} -> M.Mremoveasset (unloc asset_name, fq)
          | {node = M.Mdotassetfield (asset_name , k, fn); _} -> M.Mremovefield (unloc asset_name, unloc fn, k, fq)
          | _ -> assert false
        )

      | A.Icall (Some p, A.Cconst (A.Cremoveall), []) when is_asset_container p -> (
          let fp = f p in
          match fp with
          | {node = M.Mvar (_, Vstorecol); _} -> emit_error (instr.loc, NoRemoveAllOnCollection); bailout ()
          | {node = M.Mdotassetfield (asset_name , k, fn); _} -> M.Mremoveall (unloc asset_name, unloc fn, k)
          | _ -> assert false
        )

      | A.Icall (Some p, A.Cconst (A.Cremoveif), [AFun (_id, _type, l, q)]) ->
        let fp = f p in
        let lambda_body = f q in
        let lambda_args, args = List.fold_right (fun (x, y, z) (l1, l2) -> ((unloc x, ptyp_to_type y)::l1, (f z)::l2)) l ([], []) in
        begin
          match fp.node, fp.type_ with
          | Mdotassetfield (an, k, fn), _ -> M.Mremoveif (unloc an, CKfield (unloc an, unloc fn, k), lambda_args, lambda_body, args)
          | _, Tcontainer (Tasset an, _)  -> M.Mremoveif (unloc an, CKcoll, lambda_args, lambda_body, args)
          | _ -> assert false
        end

      | A.Icall (Some p, A.Cconst (A.Cclear), []) -> (
          let fp = f p in
          let an =
            begin
              match fp.type_ with
              | Tcontainer (Tasset an, _) -> unloc an
              | _ -> assert false
            end
          in
          M.Mclear (an, to_ck fp)
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
          match fp.node, fp.type_ with
          | Mdotassetfield (_, _k, fn), Tcontainer (Tasset an, (Aggregate | Partition)) -> M.Maddupdate (unloc an, CKfield (unloc an, unloc fn, fp), fk, fe)
          | _, Tcontainer (Tasset an, Collection)  -> M.Maddupdate (unloc an, CKcoll, fk, fe)
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
    M.mk_mterm node (M.Tunit) ~loc:instr.loc
  in

  let to_predicate (p : A.lident A.predicate) : M.predicate =
    M.mk_predicate p.name (to_mterm p.body ~formula:true) ~args:(List.map (fun (id, type_) -> (id, ptyp_to_type type_)) p.args) ~loc:p.loc
  in

  let to_definition (d : A.lident A.definition ): M.definition =
    M.mk_definition d.name (ptyp_to_type d.typ) d.var (to_mterm d.body ~formula:true) ~loc:d.loc
  in

  let to_variable (v : A.lident A.variable) : M.variable =
    M.mk_variable
      ((fun (arg : A.lident A.decl_gen) : (M.lident * M.type_ * M.mterm option) ->
          (arg.name, ptyp_to_type (Option.get arg.typ), Option.map to_mterm arg.default)) v.decl)
      ~constant:v.constant
      ?from:(Option.map to_qualid_gen v.from)
      ?to_:(Option.map to_qualid_gen v.to_)
      ~loc:v.loc
  in

  let to_invariant (i : A.lident A.invariant) :M.invariant  =
    M.mk_invariant i.label ~formulas:(List.map (to_mterm ~formula:true) i.formulas)
  in

  let to_postcondition (s : A.lident A.postcondition) : M.postcondition  =
    M.mk_postcondition s.name Post (to_mterm ~formula:true s.formula)
      ~invariants:(List.map to_invariant s.invariants) ~uses:s.uses
  in

  let to_assert (s : A.lident A.assert_) : M.postcondition  =
    M.mk_postcondition s.name Assert (to_mterm s.formula)
      ~invariants:(List.map to_invariant s.invariants) ~uses:s.uses
  in

  let to_specification (v : A.lident A.specification) : M.specification =
    let predicates     = List.map to_predicate   v.predicates  in
    let definitions    = List.map to_definition  v.definitions in
    let lemmas         = List.map to_label_lterm v.lemmas      in
    let theorems       = List.map to_label_lterm v.theorems    in
    let variables      = List.map (fun x -> to_variable x) v.variables in
    let invariants     = List.map (fun (a, l) -> (a, List.map (fun x -> to_label_lterm x) l)) v.invariants in
    let effects        = Option.map_dfl (fun x -> [to_instruction x]) [] v.effect in
    let postconditions = List.map to_postcondition v.specs @ List.map to_assert v.asserts in
    M.mk_specification
      ~predicates:predicates
      ~definitions:definitions
      ~lemmas:lemmas
      ~theorems:theorems
      ~variables:variables
      ~invariants:invariants
      ~effects:effects
      ~postconditions:postconditions
      ~loc:v.loc ()
  in

  let cont_specification (v : A.lident A.specification) (spec : M.specification) : M.specification =
    let v = to_specification v in
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
          | SonlyInEntry        (ad, action)        -> SonlyInEntry       (to_entry_description ad, to_security_entry action)
          | SonlyByRoleInEntry  (ad, roles, action) -> SonlyByRoleInEntry (to_entry_description ad, roles, to_security_entry action)
          | SnotByRole          (ad, roles)         -> SnotByRole          (to_entry_description ad, roles)
          | SnotInEntry         (ad, action)        -> SnotInEntry        (to_entry_description ad, to_security_entry action)
          | SnotByRoleInEntry   (ad, roles, action) -> SnotByRoleInEntry  (to_entry_description ad, roles, to_security_entry action)
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
    { sec with items = sec.items @ new_s.items; loc = new_s.loc; }
  in

  let process_fun_gen name args (body : M.mterm) loc spec f : M.function__ =
    let node = f (M.mk_function_struct name body
                    ~args:args
                    ~loc:loc) in
    M.mk_function ?spec:spec node
  in


  let replace_var_by_param (args : M.argument list) mt : M.mterm =
    let ident_args : ident list = List.map (fun (id, _, _) -> unloc id) args in
    let is_arg (id : M.lident) = List.mem (unloc id) ident_args in
    let rec aux (mt : M.mterm) : M.mterm =
      match mt.node with
      | M.Mvar (id, Vlocal) when is_arg id -> {mt with node = M.Mvar (id, Vparam)}
      | _ -> M.map_mterm aux mt
    in
    aux mt
  in

  let process_function (function_ : A.function_) : M.function__ =
    let name  = function_.name in
    let args  = List.map (fun (x : A.lident A.decl_gen) -> (x.name, (ptyp_to_type |@ Option.get) x.typ, None)) function_.args in
    let body  = to_instruction function_.body |> replace_var_by_param args in
    let loc   = function_.loc in
    let ret   = ptyp_to_type function_.return in
    let spec : M.specification option = Option.map to_specification function_.specification in
    process_fun_gen name args body loc spec (fun x -> M.Function (x, ret))
  in

  let add_seq (s1 : M.mterm) (s2 : M.mterm) =
    let extract (s : M.mterm) =
      match s.node with
        M.Mseq l -> l
      | _ -> [s]
    in

    let l1 = extract s1 in
    let l2 = extract s2 in

    M.mk_mterm (M.Mseq (l1 @ l2)) M.Tunit
  in

  let process_transaction (transaction : A.transaction) : M.function__ =
    let process_calledby (body : M.mterm) : M.mterm =
      let process_cb (cb : A.rexpr) (body : M.mterm) : M.mterm =
        let rec process_rexpr (rq : A.rexpr) : M.mterm option =
          let caller : M.mterm = M.mk_mterm M.Mcaller (M.Tbuiltin Baddress) in
          match rq.node with
          | Rany -> None
          | Rexpr e ->
            begin
              let mt = to_mterm e in
              Some (M.mk_mterm (M.Mequal (caller, mt)) (M.Tbuiltin Bbool) ~loc:rq.loc)
            end
          | Ror (l, r) ->
            let l = Option.get (process_rexpr l) in
            let r = Option.get (process_rexpr r) in
            Some (M.mk_mterm (M.Mor (l, r)) (M.Tbuiltin Bbool) ~loc:rq.loc)
        in
        match process_rexpr cb with
        | Some a ->
          let require : M.mterm = M.mk_mterm (M.Mnot (a)) (M.Tbuiltin Bbool) ~loc:cb.loc in
          let fail_auth : M.mterm = fail InvalidCaller in
          let cond_if = M.mk_mterm (M.Mif (require, fail_auth, None)) M.Tunit in
          add_seq cond_if body
        | _ -> body
      in
      begin
        match transaction.calledby with
        | None -> body
        | Some cb -> process_cb cb body
      end
    in

    let process b (x : A.lident A.label_term) (body : M.mterm) : M.mterm =
      let term = to_mterm x.term in
      let cond : M.mterm =
        match b with
        | `Require ->  M.mk_mterm (M.Mnot term) (Tbuiltin Bbool) ~loc:x.loc
        | `Failif -> term
      in
      let fail_cond : M.mterm = fail (InvalidCondition (Option.map unloc x.label)) in
      let cond_if = M.mk_mterm (M.Mif (cond, fail_cond, None)) M.Tunit ~loc:x.loc in
      add_seq cond_if body
    in
    let apply b li body =
      match li with
      | None -> body
      | Some l -> List.fold_right (fun (x : A.lident A.label_term) (accu : M.mterm) -> process b x accu) l body
    in
    let process_requires (body : M.mterm) : M.mterm =
      body
      |>  apply `Failif  transaction.failif
      |>  apply `Require transaction.require
    in

    let process_accept_transfer (body : M.mterm) : M.mterm =
      if (not transaction.accept_transfer)
      then
        let type_currency = M.Tbuiltin Bcurrency in
        let lhs : M.mterm = M.mk_mterm (M.Mtransferred) type_currency in
        let rhs : M.mterm = M.mk_mterm (M.Mcurrency (Big_int.zero_big_int, Tz)) type_currency in
        let eq : M.mterm = M.mk_mterm (M.Mequal (lhs, rhs)) (M.Tbuiltin Bbool) in
        let cond : M.mterm = M.mk_mterm (M.Mnot eq) (M.Tbuiltin Bbool) in
        let cond_if : M.mterm = M.mk_mterm (M.Mif (cond, fail (NoTransfer), None)) M.Tunit in
        add_seq cond_if body
      else
        body
    in

    let process_body_args () : M.argument list * M.mterm =
      let args  = List.map (fun (x : A.lident A.decl_gen) -> (x.name, (ptyp_to_type |@ Option.get) x.typ, None)) transaction.args in
      let empty : M.mterm = M.mk_mterm (M.Mseq []) Tunit in
      match transaction.transition, transaction.effect with
      | None, None ->
        let body = empty in
        args, body
      | None, Some e ->
        let body = to_instruction e in
        args, body
      | Some t, None ->
        let p_on =
          match t.on with
          | Some (key_ident, key_type, {pldesc = asset_name}, enum_type) ->
            Some (key_ident, ptyp_to_type key_type, asset_name, ptyp_to_type enum_type)
          | None -> None
        in
        let args =
          match p_on with
          | Some (ki, kt, _an, _) -> args @ [(ki, kt, None)]
          | None -> args
        in
        let build_code (body : M.mterm) : M.mterm =
          (List.fold_right (fun ((id, cond, effect) : (A.lident * A.pterm option * A.instruction option)) (acc : M.mterm) : M.mterm ->
               let tre : M.mterm =
                 match p_on with
                 | Some (key_ident, key_type, an, enum_type) ->
                   let k : M.mterm = M.mk_mterm (M.Mvar (key_ident, Vlocal)) key_type ~loc:(Location.loc key_ident) in
                   let v : M.mterm = M.mk_mterm (M.Mvar (id, Venumval)) enum_type ~loc:(Location.loc id) in
                   M.mk_mterm (M.Massign (ValueAssign, Aassetstate (an, k), v)) Tunit
                 | _ ->
                   let v : M.mterm = M.mk_mterm (M.Mvar (id, Vlocal)) (M.Tstate) ~loc:(Location.loc id) in
                   M.mk_mterm (M.Massign (ValueAssign, Astate, v)) Tunit
               in
               let code : M.mterm =
                 match effect with
                 | Some e -> M.mk_mterm (M.Mseq [to_instruction e; tre]) Tunit
                 | None -> tre
               in

               match cond with
               | Some c -> M.mk_mterm (M.Mif (to_mterm c, code, Some acc)) Tunit
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
                | Sref id -> [M.mk_pattern (M.Pconst id)]
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
                  let k : M.mterm = M.mk_mterm (M.Mvar (ki, Vlocal)) kt ~loc:(Location.loc ki) in
                  M.mk_mterm (M.Mvar (dumloc an, Vassetstate k)) et
                | _ -> M.mk_mterm (M.Mvar(dumloc "", Vstate)) Tstate
              in
              M.mk_mterm (M.Mmatchwith (w, List.map (fun x -> (x, body)) list_patterns @ [pattern, fail_instr])) Tunit
            end
        in
        args, body

      | _ -> emit_error (transaction.loc, CannotExtractBody); bailout ()
    in

    (* let list  = list |> cont process_function ast.functions in *)
    let name  = transaction.name in
    let args, body = process_body_args () in
    let body =
      body
      |> process_requires
      |> process_accept_transfer
      |> process_calledby
      |> replace_var_by_param args
    in
    let loc   = transaction.loc in
    let spec : M.specification option = Option.map to_specification transaction.specification in

    process_fun_gen name args body loc spec (fun x -> M.Entry x)
  in

  let process_decl_ = function
    | A.Dvariable v -> process_var v
    | A.Dasset    a -> process_asset a
    | A.Denum     e -> process_enum e
    | A.Dcontract c -> M.Dcontract (to_contract c)
  in

  let process_fun_ = function
    | A.Ffunction f -> process_function f
    | A.Ftransaction t -> process_transaction t
  in

  let name = ast.name in

  let decls = List.map process_decl_ ast.decls in
  let functions = List.map process_fun_ ast.funs in

  let specification =
    M.mk_specification ()
    |> (fun spec -> List.fold_left (fun accu x -> cont_specification x accu) spec ast.specifications)
  in

  let security =
    M.mk_security ()
    |> (fun sec -> List.fold_left (fun accu x -> cont_security x accu) sec ast.securities)
  in

  M.mk_model ~decls:decls ~functions:functions ~specification:specification ~security:security ~loc:ast.loc name
