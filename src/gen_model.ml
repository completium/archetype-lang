open Location
open Ident
open Tools
open Gen_transform

module A = Ast
module M = Model

exception Anomaly of string
type error_desc =
  | NotSupportedContainer of string
  | UnsupportedTypeForFile of A.type_
  | CannotConvertToAssignOperator
  | CannotSetApiItem
  | CannotExtractBody
  | CannotExtractAssetName
  | AnyNotAuthorizedInTransitionTo of Location.t
  | NoInitExprFor of string
  | TODO
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

let to_model (ast : A.model) : M.model =

  let to_container c =
    match c with
    | A.Collection -> M.Collection
    | A.Partition  -> M.Partition
    | _            -> emit_error (NotSupportedContainer (Format.asprintf "%a@." A.pp_container c))
  in

  let to_currency = function
    | A.Tez   -> M.Tez
    | A.Mutez -> M.Mutez
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
    | A.VTcurrency c -> M.Bcurrency (to_currency c)
    | A.VTkey        -> M.Bkey
  in

  let to_trtyp = function
    | A.TRentry  -> M.TRentry
    | A.TRaction -> M.TRaction
    | A.TRasset  -> M.TRasset
    | A.TRfield  -> M.TRfield
  in

  let rec ptyp_to_type (t : A.ptyp) : M.type_ =
    match t with
    | A.Tasset id          -> M.Tasset id
    | A.Tenum id           -> M.Tenum id
    | A.Tcontract id       -> M.Tcontract id
    | A.Tbuiltin b         -> M.Tbuiltin (vtyp_to_btyp b)
    | A.Tcontainer (t, c)  -> M.Tcontainer (ptyp_to_type t, to_container c)
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

  let to_assign_operator = function
    | `Assign op -> to_assignment_operator op
    | _ -> emit_error CannotConvertToAssignOperator
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


  let extract_asset_name (mterm : M.mterm) : ident =
    match mterm with
    | {type_ = Tcontainer (Tasset asset_name, _); _} -> unloc asset_name
    | _ -> assert false
  in

  let extract_field_name (id, type_, body : A.lident * A.ptyp * A.pterm) : M.lident =
    match body.node with
    | A.Pdot (_, fn) -> fn
    | _ -> assert false
  in

  let to_mterm_node (n : A.lident A.term_node) (f : A.lident A.term_gen -> M.mterm) (ftyp : 't -> M.type_) : (M.lident, M.mterm) M.mterm_node =
    match n with
    | A.Pif (c, t, e)                -> M.Mif        (f c, f t, Some (f e))
    | A.Pmatchwith (m, l)            -> M.Mmatchwith (f m, List.map (fun (p, e) -> (to_pattern p, f e)) l)
    | A.Plogical (A.And, l, r)       -> M.Mand       (f l, f r)
    | A.Plogical (A.Or, l, r)        -> M.Mor        (f l, f r)
    | A.Plogical (A.Imply, l, r)     -> M.Mimply     (f l, f r)
    | A.Plogical (A.Equiv, l, r)     -> M.Mequiv     (f l, f r)
    | A.Pnot e                       -> M.Mnot       (f e)
    | A.Pcomp (A.Equal, l, r)        -> M.Mequal     (f l, f r)
    | A.Pcomp (A.Nequal, l, r)       -> M.Mnequal    (f l, f r)
    | A.Pcomp (A.Gt, l, r)           -> M.Mgt        (f l, f r)
    | A.Pcomp (A.Ge, l, r)           -> M.Mge        (f l, f r)
    | A.Pcomp (A.Lt, l, r)           -> M.Mlt        (f l, f r)
    | A.Pcomp (A.Le, l, r)           -> M.Mle        (f l, f r)
    | A.Parith (A.Plus, l, r)        -> M.Mplus      (f l, f r)
    | A.Parith (A.Minus, l, r)       -> M.Mminus     (f l, f r)
    | A.Parith (A.Mult, l, r)        -> M.Mmult      (f l, f r)
    | A.Parith (A.Div, l, r)         -> M.Mdiv       (f l, f r)
    | A.Parith (A.Modulo, l, r)      -> M.Mmodulo    (f l, f r)
    | A.Puarith (A.Uplus, e)         -> M.Muplus     (f e)
    | A.Puarith (A.Uminus, e)        -> M.Muminus    (f e)
    | A.Precord l                    -> M.Mrecord    (List.map f l)
    | A.Pletin (id, init, typ, cont) -> M.Mletin     ([id], f init, Option.map ftyp typ, f cont)
    | A.Pvar {pldesc = "now"}                -> M.Mnow (* TODO: use const Cnow instead *)
    | A.Pvar id when A.Utils.is_variable ast id   -> M.Mvarstorevar id
    | A.Pvar id when A.Utils.is_asset ast id      -> M.Mvarstorecol id
    | A.Pvar id when A.Utils.is_enum_value ast id -> M.Mvarenumval id
    | A.Pvar id                                   -> M.Mvarlocal id
    | A.Parray l                             -> M.Marray (List.map f l)
    | A.Plit ({node = BVint i; _})           -> M.Mint i
    | A.Plit ({node = BVuint i; _})          -> M.Muint i
    | A.Plit ({node = BVbool b; _})          -> M.Mbool b
    | A.Plit ({node = BVenum s; _})          -> M.Menum s
    | A.Plit ({node = BVrational (d, n); _}) -> M.Mrational (d, n)
    | A.Plit ({node = BVdate s; _})          -> M.Mdate s
    | A.Plit ({node = BVstring s; _})        -> M.Mstring s
    | A.Plit ({node = BVcurrency (c, i); _}) -> M.Mcurrency (i, to_currency c)
    | A.Plit ({node = BVaddress s; _})       -> M.Maddress s
    | A.Plit ({node = BVduration s; _})      -> M.Mduration s
    | A.Pdot (d, i) ->
      (* handle dot contract too *)
      M.Mdotasset (f d, i)
    | A.Pconst Cstate                        -> M.Mstate
    | A.Pconst Cnow                          -> M.Mnow
    | A.Pconst Ctransferred                  -> M.Mtransferred
    | A.Pconst Ccaller                       -> M.Mcaller
    | A.Pconst Cbalance                      -> M.Mbalance
    | A.Pconst Canyaction                    -> M.Manyaction
    | A.Pconst c                             ->
      Format.eprintf "expr const unkown: %a@." A.pp_const c;
      assert false

    | A.Ptuple l                             -> M.Mtuple (List.map f l)
    | A.Pquantifer (Forall, i, typ, term)    -> M.Mforall (i, ptyp_to_type typ, f term)
    | A.Pquantifer (Exists, i, typ, term)    -> M.Mexists (i, ptyp_to_type typ, f term)

    | A.Pcall (_, A.Cconst A.Cbefore,    [AExpr p]) -> M.Msetbefore    (f p)
    | A.Pcall (_, A.Cconst A.Cunmoved,   [AExpr p]) -> M.Msetunmoved   (f p)
    | A.Pcall (_, A.Cconst A.Cadded,     [AExpr p]) -> M.Msetadded     (f p)
    | A.Pcall (_, A.Cconst A.Cremoved,   [AExpr p]) -> M.Msetremoved   (f p)
    | A.Pcall (_, A.Cconst A.Citerated,  [AExpr p]) -> M.Msetiterated  (f p)
    | A.Pcall (_, A.Cconst A.Ctoiterate, [AExpr p]) -> M.Msettoiterate (f p)

    | A.Pcall (aux, A.Cid id, args) ->
      M.Mapp (id, List.map (fun x -> term_arg_to_expr f x) args)

    | A.Pcall (Some p, A.Cconst (A.Cget), [AExpr q]) ->
      let fp = f p in
      let fq = f q in
      let asset_name = extract_asset_name fp in
      M.Mget (asset_name, fq)

    | A.Pcall (Some c, A.Cconst (A.Cselect), [AFun (pi, pt, p)]) ->
      let fc = f c in
      let fp = f p in
      let asset_name =
        match fc with
        | {type_ = M.Tcontainer (M.Tasset asset_name, _); _} -> unloc asset_name
        | _ -> "todo0"
      in
      M.Mselect (asset_name, fc, fp)

    | A.Pcall (Some p, A.Cconst (A.Ccontains), [AExpr q]) ->
      let fp = f p in
      let asset_name =
        match fp with
        | {type_ = M.Tcontainer (M.Tasset asset_name, _); _} -> unloc asset_name
        | _ -> "todo"
      in
      M.Mcontains (asset_name, f p, f q)

    (* TODO: nth *)

    | A.Pcall (Some p, A.Cconst (A.Ccount), []) ->
      let fp = f p in
      let asset_name = extract_asset_name fp in
      M.Mcount (asset_name, fp)

    | A.Pcall (Some p, A.Cconst (A.Csum), [AFun (qi, qt, q)]) ->
      let fp = f p in
      let asset_name = extract_asset_name fp in
      let field_name = extract_field_name (qi, qt, q) in
      M.Msum (asset_name, field_name, fp)

    | A.Pcall (Some p, A.Cconst (A.Cmin), [AFun (qi, qt, q)]) ->
      let fp = f p in
      let asset_name = extract_asset_name fp in
      let field_name = extract_field_name (qi, qt, q) in
      M.Mmin (asset_name, field_name, fp)

    | A.Pcall (Some p, A.Cconst (A.Cmax), [AFun (qi, qt, q)]) ->
      let fp = f p in
      let asset_name = extract_asset_name fp in
      let field_name = extract_field_name (qi, qt, q) in
      M.Mmax (asset_name, field_name, fp)

    | A.Pcall (None, A.Cconst (A.Cmaybeperformedonlybyrole), [AExpr l; AExpr r]) ->
      M.MsecMayBePerformedOnlyByRole (f l, f r)

    | A.Pcall (None, A.Cconst (A.Cmaybeperformedonlybyaction), [AExpr l; AExpr r]) ->
      M.MsecMayBePerformedOnlyByAction (f l, f r)

    | A.Pcall (None, A.Cconst (A.Cmaybeperformedbyrole), [AExpr l; AExpr r]) ->
      M.MsecMayBePerformedByRole (f l, f r)

    | A.Pcall (None, A.Cconst (A.Cmaybeperformedbyaction), [AExpr l; AExpr r]) ->
      M.MsecMayBePerformedByAction (f l, f r)

    | A.Pcall (aux, A.Cconst c, args) ->
      Format.eprintf "expr const unkown: %a with nb args: %d [%a] %s@."
        A.pp_const c
        (List.length args)
        (Printer_tools.pp_list "; " (fun fmt x ->
             let str = match x with | A.AExpr _ -> "AExpr" | A.AEffect _ -> "AEffect" | A.AFun _ -> "AFun" in
             Printer_tools.pp_str fmt str)) args
        (match aux with | Some _ -> "with aux" | _ -> "without aux");
      assert false

    | A.PsecurityActionRole _ ->
      assert false

    | A.PsecurityActionAction _ ->
      assert false
  in

  let rec to_mterm (pterm : A.pterm) : M.mterm =
    let node = to_mterm_node pterm.node to_mterm ptyp_to_type in
    let type_ = ptyp_to_type (Option.get pterm.type_) in
    M.mk_mterm node type_ ~loc:pterm.loc
  in

  let to_label_lterm (x : A.lident A.label_term) : M.label_term =
    M.mk_label_term (to_mterm x.term) ?label:x.label ~loc:x.loc
  in


  let extract_asset_name (pterm : M.mterm) : Ident.ident =
    match pterm with
    | {type_ = Tcontainer (Tasset asset_name, _); _ } -> unloc asset_name
    | _ -> assert false
  in

  (* myasset.update k {f1 = v1; f2 = v2}

     let _k = k in
     let _myasset = myasset.get _k in
     let _myasset = {id = _myasset.id; f1 = v1; f2 = v2} in
     set_myasset s _k _myasset *)

  let extract_letin (c : M.mterm) k (e : (A.lident * A.operator * M.mterm) list) : M.mterm__node =

    let asset_name = extract_asset_name c in
    let asset_loced = dumloc asset_name in
    let asset = A.Utils.get_asset ast asset_loced in

    let type_asset = M.Tasset asset_loced in
    let type_container_asset = M.Tcontainer (type_asset, Collection) in

    let var_name = dumloc ("_" ^ asset_name) in
    let var_mterm : M.mterm = M.mk_mterm (M.Mvarlocal var_name) type_asset in

    (* let asset_mterm : M.mterm = M.mk_mterm (M.Mvarstorecol (dumloc (asset_name))) type_container_asset in *)

    let key_name = "_k" in
    let key_loced : M.lident = dumloc (key_name) in
    let key_mterm : M.mterm = M.mk_mterm (M.Mvarlocal key_loced) type_container_asset in

    let set_mterm : M.mterm = M.mk_mterm (M.Mset (asset_name, key_mterm, var_mterm)) Tunit in

    let lref : (Ident.ident * (A.operator * M.mterm)) list = List.map (fun (x, y, z) -> (unloc x, (y, z))) e in
    let lrecorditems =
      List.fold_left (fun accu (x : A.lident A.decl_gen) ->
          let v = List.assoc_opt (unloc x.name) lref in
          let type_ = ptyp_to_type (Option.get x.typ) in
          let var = M.mk_mterm (Mdotasset (var_mterm, x.name)) type_ in
          match v with
          | Some y ->
            accu @ [
              let value = snd y in
              match y |> fst |> to_assign_operator with
              | M.ValueAssign -> value
              | M.PlusAssign  -> M.mk_mterm (Mplus (var, value)) type_
              | M.MinusAssign -> M.mk_mterm (Mminus (var, value)) type_
              | M.MultAssign  -> M.mk_mterm (Mmult (var, value)) type_
              | M.DivAssign   -> M.mk_mterm (Mdiv (var, value)) type_
              | M.AndAssign   -> M.mk_mterm (Mand (var, value)) type_
              | M.OrAssign    -> M.mk_mterm (Mor (var, value)) type_
            ]
          | _ -> accu @ [var]
        ) [] asset.fields in
    let record : M.mterm = M.mk_mterm (M.Mrecord lrecorditems) type_asset in

    let letinasset : M.mterm = M.mk_mterm (M.Mletin ([var_name],
                                                     record,
                                                     Some (type_asset),
                                                     set_mterm
                                                    )) Tunit in

    (* let seq : M.mterm list = (List.map (fun ((id, op, term) : ('a * A.operator * 'c)) -> M.mk_mterm
                                           (M.Massignfield (to_assign_operator op, var_name, id, term))
                                           Tunit
                                       ) e) @ [set_mterm] in *)

    (* let body : M.mterm = M.mk_mterm (M.Mseq seq) Tunit in *)

    let get_mterm : M.mterm = M.mk_mterm (M.Mget (asset_name, key_mterm)) type_asset in

    let letinasset : M.mterm = M.mk_mterm (M.Mletin ([var_name],
                                                     get_mterm,
                                                     Some (type_asset),
                                                     letinasset
                                                    ))
        Tunit in

    let res : M.mterm__node = M.Mletin ([key_loced],
                                        k,
                                        None,
                                        letinasset
                                       ) in
    res

  in


  (* col.removeif pred

     let _col = col.select pred in
     for (_asset in _col)
       _col_asset.remove _asset.key
  *)

  let extract_removeif (m : M.model) (c : M.mterm) (pid, tid, p : M.lident * M.type_* M.mterm) : M.mterm__node =
    let asset_str = extract_asset_name c in
    (* let key_str, key_type = A.Utils.get_asset_key ast (dumloc asset_str) |> fun (x, y) -> (unloc x, M.Tbuiltin (vtyp_to_btyp y)) in *)

    let asset_name = dumloc asset_str in
    (* let key_name = dumloc key_str in *)
    let type_asset = M.Tasset asset_name in
    (* let _, t = A.Utils.get_asset_key ast (dumloc asset_str) in *)

    let assetv_str = dumloc ("_" ^ asset_str) in
    let asset_var = M.mk_mterm (M.Mvarlocal assetv_str) type_asset in

    let key, key_type = M.Utils.get_asset_key m asset_name in
    let asset_key : M.mterm = M.mk_mterm (M.Mdotasset (asset_var,dumloc key)) (Tbuiltin key_type) in

    let assets_var_name = dumloc ("_assets") in
    (* let type_assets = M.Tcontainer (Tbuiltin (vtyp_to_btyp t), Collection) in *)
    let type_assets = M.Tcontainer (Tasset asset_name, Collection) in
    let assets_var = M.mk_mterm (M.Mvarlocal assets_var_name) type_assets in

    let select : M.mterm =  M.mk_mterm (M.Mselect (asset_str, c, p) ) type_asset in

    (* let asset_sortcol : M.mterm = M.mk_mterm (M.Mvarstorecol asset_name) type_asset in *)
    (* let asset_key : M.mterm = M.mk_mterm (M.Mdotasset (asset_var, key_name)) key_type in *)

    (* let remove : M.mterm = M.mk_mterm (M.Mremoveasset (asset_str, asset_sortcol, asset_key)) Tunit in *)
    let remove : M.mterm = M.mk_mterm (M.Mremoveasset (asset_str, asset_key)) Tunit in

    let for_ = M.mk_mterm (M.Mfor (assetv_str, assets_var, remove, None) ) Tunit in

    let res : M.mterm__node = M.Mletin ([assets_var_name], select, Some type_assets, for_) in
    res

  in

  let process_enums list =
    let process_enum (e : A.enum) : M.decl_node =
      let values = List.map (fun (x : A.lident A.enum_item_struct) ->
          let id : M.lident = x.name in
          M.mk_enum_item id ~invariants:(List.map (fun x -> to_label_lterm x) x.invariants)
        ) e.items in
      let enum = M.mk_enum e.name ~values:values in
      M.Denum enum
    in
    list @ List.map (fun x -> process_enum x) ast.enums in

  let process_info_enums list =
    let process_enum (e : A.enum) : M.info_item =
      let name = unloc e.name in
      let values = List.map (fun (x : A.lident A.enum_item_struct) ->
          unloc x.name) e.items in
      let enum = M.mk_info_enum name ~values:values in
      M.Ienum enum
    in
    list @ List.map (fun x -> process_enum x) ast.enums in

  let process_records list =
    let process_asset (a : A.asset) : M.decl_node =
      let values = List.map (fun (x : A.lident A.decl_gen) ->
          let typ = Option.map ptyp_to_type x.typ in
          let default = Option.map to_mterm x.default in
          M.mk_record_item x.name (Option.get typ) ?default:default) a.fields in
      let r : M.record = M.mk_record a.name ?key:a.key ~values:values in
      M.Drecord r
    in
    list @ List.map (fun x -> process_asset x) ast.assets
  in

  let process_info_records list =
    let process_asset (a : A.asset) : M.info_item =
      let values : (ident * M.type_ * M.mterm option) list = List.map (fun (x : A.lident A.decl_gen) ->
          let typ = Option.map ptyp_to_type x.typ in
          unloc x.name, (Option.get typ), None) a.fields in (* TODO : set actual default value *)
      let a : M.info_asset = M.mk_info_asset (unloc a.name) (unloc (Option.get a.key)) ~values:values in
      M.Iasset a
    in
    list @ List.map (fun x -> process_asset x) ast.assets
  in

  let process_contracts list =
    let to_contract_signature (s : A.lident A.signature) : M.contract_signature =
      let name = s.name in
      M.mk_contract_signature name ~args:(List.map (fun arg -> ptyp_to_type arg) s.args) ~loc:s.loc
    in
    let to_contract (c : A.lident A.contract) : M.contract =
      M.mk_contract c.name
        ~signatures:(List.map to_contract_signature c.signatures)
        ?init:(Option.map to_mterm c.init)
        ~loc:c.loc
    in
    list @ List.map (fun (x : A.lident A.contract) -> M.Dcontract (to_contract x)) ast.contracts
  in

  let process_info_contracts list =
    let to_contract (c : A.lident A.contract) : M.info_contract =
      let signatures : (ident * M.type_ list) list =
        List.map (fun (s : A.lident A.signature) ->
            unloc s.name, List.map ptyp_to_type s.args) c.signatures
      in
      M.mk_info_contract (unloc c.name) ~signatures:signatures
    in
    list @ List.map (fun (x : A.lident A.contract) -> M.Icontract (to_contract x)) ast.contracts
  in

  let info =
    []
    |> process_info_enums
    |> process_info_records
    |> process_info_contracts
  in

  let to_instruction_node (n : A.lident A.instruction_node) lbl g f : ('id, 'instr) M.mterm_node =
    let is_empty_seq (instr : A.instruction) =
      match instr.node with
      | A.Iseq [] -> true
      | _ -> false
    in

    match n with
    | A.Iif (c, t, e) when is_empty_seq e -> M.Mif (f c, g t, None)
    | A.Iif (c, t, e)           -> M.Mif (f c, g t, Some (g e))
    | A.Ifor (i, col, body)     -> M.Mfor (i, f col, g body, lbl)
    | A.Iletin (i, init, cont)  -> M.Mletin ([i], f init, Option.map ptyp_to_type init.type_, g cont) (* TODO *)
    | A.Iseq l                  -> M.Mseq (List.map g l)
    | A.Imatchwith (m, l)       -> M.Mmatchwith (f m, List.map (fun (p, i) -> (to_pattern p, g i)) l)
    | A.Iassign (op, i, e)      -> M.Massign (to_assignment_operator op, i, to_mterm e)
    | A.Irequire (b, t)         ->
      let cond : M.mterm =
        if b
        then term_not (f t)
        else (f t)
      in
      M.Mif (cond, fail (InvalidCondition None), None)

    | A.Itransfer (i, b, q)     -> M.Mtransfer (f i, b, Option.map to_qualid_gen q)
    | A.Ibreak                  -> M.Mbreak
    | A.Iassert e               -> M.Massert (f e)
    | A.Ireturn e               -> M.Mreturn (f e)
    | A.Ilabel i                -> M.Mlabel i
    | A.Icall (i, Cid id, args) -> M.Mapp (id, Option.map_dfl (fun v -> [to_mterm v]) [] i @ List.map (term_arg_to_expr f) args)

    | A.Icall (_, A.Cconst (A.Cfail), [AExpr p]) ->
      M.Mfail (Invalid (f p))

    | A.Icall (Some p, A.Cconst (A.Cadd), [AExpr q]) -> (
        let fp = f p in
        let fq = f q in
        match fp with
        | {node = M.Mvarstorecol asset_name; _} -> M.Maddasset (unloc asset_name, fq)
        | {node = M.Mdotasset ({type_ = M.Tasset asset_name ; _} as arg, f); _} -> M.Maddfield (unloc asset_name, unloc f, arg, fq)
        | _ -> M.Maddlocal (fp, fq)
      )

    | A.Icall (Some p, A.Cconst (A.Cremove), [AExpr q]) -> (
        let fp = f p in
        let fq = f q in
        match fp with
        | {node = M.Mvarstorecol asset_name; _} -> M.Mremoveasset (unloc asset_name, fq)
        | {node = M.Mdotasset ({type_ = M.Tasset asset_name ; _} as arg, f); _} -> M.Mremovefield (unloc asset_name, unloc f, arg, fq)
        | _ -> M.Mremovelocal (fp, fq)
      )

    | A.Icall (Some p, A.Cconst (A.Cclear), []) -> (
        let fp = f p in
        match fp with
        | {node = M.Mvarstorecol asset_name; _} -> M.Mclearasset (unloc asset_name)
        | {node = M.Mdotasset ({type_ = M.Tasset asset_name ; _} as arg, f); _} -> M.Mclearfield (unloc asset_name, unloc f, arg)
        | _ -> M.Mclearlocal (fp)
      )

    | A.Icall (Some p, A.Cconst (A.Creverse), []) -> (
        let fp = f p in
        match fp with
        | {node = M.Mvarstorecol asset_name; _} -> M.Mreverseasset (unloc asset_name)
        | {node = M.Mdotasset ({type_ = M.Tasset asset_name ; _} as arg, f); _} -> M.Mreversefield (unloc asset_name, unloc f, arg)
        | _ -> M.Mreverselocal (fp)
      )

    | A.Icall (Some p, A.Cconst (A.Cupdate), [AExpr k; AEffect e]) ->
      let p = f p in
      let k = f k in
      let e = List.map (fun (a, b, c) -> (a, b, f c)) e in
      extract_letin p k e

    | A.Icall (Some c, A.Cconst (A.Cremoveif), [AFun (i, t, p)]) ->
      extract_removeif (M.mk_model ~info:info [] (dumloc ""))(f c) (i, ptyp_to_type t, f p)

    | A.Icall (aux, A.Cconst c, args) ->
      Format.eprintf "instr const unkown: %a with nb args: %d [%a] %s@."
        A.pp_const c
        (List.length args)
        (Printer_tools.pp_list "; " (fun fmt (x : A.pterm_arg) ->
             let str = match x with | AExpr _ -> "AExpr" | AEffect _ -> "AEffect" | AFun _ -> "AFun" in
             Printer_tools.pp_str fmt str)) args
        (match aux with | Some _ -> "with aux" | _ -> "without aux");
      assert false
  in

  let rec to_instruction (instr : A.instruction) : M.mterm =
    let node = to_instruction_node instr.node instr.label to_instruction to_mterm in
    M.mk_mterm node (M.Tunit) ~subvars:instr.subvars ~loc:instr.loc
  in

  let to_predicate (p : A.lident A.predicate) : M.predicate =
    M.mk_predicate p.name (to_mterm p.body) ~args:(List.map (fun (id, type_) -> (id, ptyp_to_type type_)) p.args) ~loc:p.loc
  in

  let to_definition (d : A.lident A.definition ): M.definition =
    M.mk_definition d.name (ptyp_to_type d.typ) d.var (to_mterm d.body) ~loc:d.loc
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
    M.mk_invariant i.label ~formulas:(List.map to_mterm i.formulas)
  in

  let to_spec (s : A.lident A.specification) : M.specification  =
    M.mk_specification s.name Post (to_mterm s.formula)
      ~invariants:(List.map to_invariant s.invariants)
  in

  let to_assert (s : A.lident A.assert_) : M.specification  =
    M.mk_specification s.name Assert (to_mterm s.formula)
      ~invariants:(List.map to_invariant s.invariants)
  in

  let to_verification (v : A.lident A.verification) : M.verification =
    let predicates  = List.map to_predicate   v.predicates  in
    let definitions = List.map to_definition  v.definitions in
    let lemmas      = List.map to_label_lterm v.lemmas      in
    let theorems    = List.map to_label_lterm v.theorems    in
    let variables   = List.map (fun x -> to_variable x) v.variables in
    let invariants  = List.map (fun (a, l) -> (a, List.map (fun x -> to_label_lterm x) l)) v.invariants in
    let effects     = Option.map_dfl (fun x -> [to_mterm x]) [] v.effect in
    let specs       = List.map to_spec v.specs @ List.map to_assert v.asserts in
    M.mk_verification
      ~predicates:predicates
      ~definitions:definitions
      ~lemmas:lemmas
      ~theorems:theorems
      ~variables:variables
      ~invariants:invariants
      ~effects:effects
      ~specs:specs
      ~loc:v.loc ()
  in

  let cont_verification (v : A.lident A.verification) (verif : M.verification) : M.verification =
    let v = to_verification v in
    { verif with
      predicates  = verif.predicates @ v.predicates;
      definitions = verif.definitions @ v.definitions;
      lemmas      = verif.lemmas @ v.lemmas;
      theorems    = verif.theorems @ v.theorems;
      variables   = verif.variables @ v.variables;
      invariants  = verif.invariants @ v.invariants;
      effects     = verif.effects @ v.effects;
      specs       = verif.specs @ v.specs;
      loc         = Location.merge verif.loc v.loc;
    }
  in

  let process_storage list =
    let variable_to_storage_items (var : A.lident A.variable) : M.storage_item =

      let init_ b =
        match b with
        | M.Bbool       -> M.mk_mterm (M.Mbool false) (M.Tbuiltin b)
        | M.Bint        -> M.mk_mterm (M.Mint (Big_int.zero_big_int)) (M.Tbuiltin b)
        | M.Brational   -> M.mk_mterm (M.Mrational (Big_int.zero_big_int, Big_int.unit_big_int)) (M.Tbuiltin b)
        | M.Bdate       -> emit_error (NoInitExprFor "date")
        | M.Bduration   -> M.mk_mterm (M.Mduration "0s") (M.Tbuiltin b)
        | M.Bstring     -> M.mk_mterm (M.Mstring "") (M.Tbuiltin b)
        | M.Baddress    -> emit_error (NoInitExprFor "address")
        | M.Brole       -> emit_error (NoInitExprFor "role")
        | M.Bcurrency _ -> M.mk_mterm (M.Mcurrency (Big_int.zero_big_int, M.Tez)) (M.Tbuiltin b)
        | M.Bkey        -> emit_error (NoInitExprFor "key")
      in

      let init_default_value = function
        | M.Tbuiltin b        -> init_ b
        | M.Tcontainer (t, _) -> M.mk_mterm (M.Marray []) (M.Tcontainer(t, Collection))
        | M.Toption t         -> M.mk_mterm (M.Mnone) (M.Toption t)
        | M.Tasset v
        | M.Tenum v
        | M.Tcontract v       -> emit_error (NoInitExprFor (unloc v))
        | M.Ttuple _          -> emit_error (NoInitExprFor "tuple")
        | M.Tassoc _          -> emit_error (NoInitExprFor "tassoc")
        | M.Tunit             -> emit_error (NoInitExprFor "unit")
        | M.Tstorage          -> emit_error (NoInitExprFor "storage")
        | M.Toperation        -> emit_error (NoInitExprFor "operation")
        | M.Tentry            -> emit_error (NoInitExprFor "entry")
        | M.Tprog _           -> emit_error (NoInitExprFor "prog")
        | M.Tvset _           -> emit_error (NoInitExprFor "vset")
        | M.Ttrace _          -> emit_error (NoInitExprFor "trace")
      in

      let arg = var.decl in
      let type_ : A.type_ = Option.get arg.typ in
      let typ = ptyp_to_type type_ in
      let dv =
        match arg.default with
        | Some v -> to_mterm v
        | None   -> init_default_value typ
      in
      M.mk_storage_item arg.name typ dv
    in

    let asset_to_storage_items (asset : A.asset) : M.storage_item =
      let asset_name = asset.name in
      let typ_ = M.Tcontainer (Tasset asset_name, Collection) in
      M.mk_storage_item
        asset_name
        typ_
        (M.mk_mterm (M.Marray []) typ_)
        ~asset:asset_name
        ~invariants:(List.map (fun x -> to_label_lterm x) asset.specs)
    in

    let cont f x l = l @ (List.map f x) in
    []
    |> cont variable_to_storage_items ast.variables
    |> cont asset_to_storage_items ast.assets
  in

  let cont f x l = List.fold_left (fun accu x -> f x accu) l x in

  let process_fun_gen name args (body : M.mterm) loc verif f (list : M.function__ list) : M.function__ list =
    let node = f (M.mk_function_struct name body
                    ~args:args
                    ~loc:loc) in
    list @ [M.mk_function ?verif:verif node]
  in


  let replace_var_by_param (args : M.argument list) mt : M.mterm =
    let ident_args : ident list = List.map (fun (id, _, _) -> unloc id) args in
    let is_arg (id : M.lident) = List.mem (unloc id) ident_args in
    let rec aux (mt : M.mterm) : M.mterm =
      match mt.node with
      | M.Mvarlocal id when is_arg id -> {mt with node = M.Mvarparam id}
      | _ -> M.map_mterm aux mt
    in
    aux mt
  in

  let process_function
      (function_ : A.function_)
      (list : M.function__ list) : M.function__ list =
    let name  = function_.name in
    let args  = List.map (fun (x : A.lident A.decl_gen) -> (x.name, (ptyp_to_type |@ Option.get) x.typ, None)) function_.args in
    let body  = to_instruction function_.body |> replace_var_by_param args in
    let loc   = function_.loc in
    let ret   = ptyp_to_type function_.return in
    let verif : M.verification option = Option.map to_verification function_.verification in
    process_fun_gen name args body loc verif (fun x -> M.Function (x, ret)) list
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

  let process_transaction (transaction : A.transaction) (list : M.function__ list) : M.function__ list =
    let process_calledby (body : M.mterm) : M.mterm =
      let process_cb (cb : A.rexpr) (body : M.mterm) : M.mterm =
        let rec process_rexpr (rq : A.rexpr) : M.mterm =
          let caller : M.mterm = M.mk_mterm M.Mcaller (M.Tbuiltin Baddress) in
          match rq.node with
          | Rqualid q ->
            begin
              let qualid_to_pterm (q : A.qualid) : M.mterm =
                match q.node with
                | Qident i ->
                  let t : A.ptyp = A.Utils.get_var_type ast i in
                  let type_ = ptyp_to_type t in
                  M.mk_mterm (M.Mvarstorevar i) type_ ~loc:q.loc
                (* | Qdot ({node = Qident a}, i) ->
                   M.mk_mterm (M.Mvarstorevar i) type_ ~loc:q.loc *)
                | _ -> emit_error TODO
              in
              let addr : M.mterm = qualid_to_pterm q in
              M.mk_mterm (M.Mequal (caller, addr)) (M.Tbuiltin Bbool) ~loc:rq.loc
            end
          | Ror (l, r) ->
            M.mk_mterm (M.Mor (process_rexpr l, process_rexpr r)) (M.Tbuiltin Bbool) ~loc:rq.loc
          | Raddress a ->
            let addr   : M.mterm = M.mk_mterm (M.Maddress (unloc a)) (M.Tbuiltin Baddress) in
            M.mk_mterm (M.Mequal (caller, addr)) (M.Tbuiltin Bbool) ~loc:rq.loc
        in
        let require : M.mterm = M.mk_mterm (M.Mnot (process_rexpr cb)) (M.Tbuiltin Bbool) ~loc:cb.loc in
        let fail_auth : M.mterm = fail InvalidCaller in
        let cond_if = M.mk_mterm (M.Mif (require, fail_auth, None)) M.Tunit in
        add_seq cond_if body
      in
      begin
        match transaction.calledby with
        | None -> body
        | Some cb -> process_cb cb body
      end
    in

    let process_requires (body : M.mterm) : M.mterm =
      let process_require (x : A.lident A.label_term) (body : M.mterm) : M.mterm =
        let term = to_mterm x.term in
        let cond : M.mterm = M.mk_mterm (M.Mnot term) (Tbuiltin Bbool) ~loc:x.loc in
        let fail_cond : M.mterm = fail (InvalidCondition (Option.map unloc x.label)) in
        let cond_if = M.mk_mterm (M.Mif (cond, fail_cond, None)) M.Tunit ~loc:x.loc in
        add_seq cond_if body
      in
      match transaction.require with
      | None -> body
      | Some requires -> List.fold_right (fun (x : A.lident A.label_term) (accu : M.mterm) -> process_require x accu) requires body
    in

    let process_accept_transfer (body : M.mterm) : M.mterm =
      if (not transaction.accept_transfer)
      then
        let type_currency = M.Tbuiltin (Bcurrency Tez) in
        let lhs : M.mterm = M.mk_mterm (M.Mtransferred) type_currency in
        let rhs : M.mterm = M.mk_mterm (M.Mcurrency (Big_int.zero_big_int, Tez)) type_currency in
        let eq : M.mterm = M.mk_mterm (M.Mequal (lhs, rhs)) (M.Tbuiltin Bbool) in
        let cond : M.mterm = M.mk_mterm (M.Mnot eq) (M.Tbuiltin Bbool) in
        let cond_if : M.mterm = M.mk_mterm (M.Mif (cond, fail (NoTransfer), None)) M.Tunit in
        add_seq cond_if body
      else
        body
    in

    let process_body_args () : M.argument list * M.mterm =
      let args  = List.map (fun (x : A.lident A.decl_gen) -> (x.name, (ptyp_to_type |@ Option.get) x.typ, None)) transaction.args in
      match transaction.transition, transaction.effect with
      | None, Some e ->
        let body = to_instruction e in
        args, body
      | Some t, None ->
        let args =
          match t.on with
          | Some (id, id2) -> args @ [(id, M.Tasset id2, None)]
          | None -> args
        in
        let state : M.lident = dumloc "_state" in
        let build_code (body : M.mterm) : M.mterm =
          (List.fold_right (fun ((id, cond, effect) : (A.lident * A.pterm option * A.instruction option)) (acc : M.mterm) : M.mterm ->
               let tre : M.mterm =
                 match t.on with
                 | Some (id, id_asset) ->
                   (
                     (* let asset : M.mterm = M.mk_mterm (M.Mvarstorecol id_asset) (M.Tasset id_asset) in *)

                     (* let q : qualid = mk_sp (Qident state) in
                        let aid : pterm = mk_sp (Pvar id) in *)

                     (* let arg : pterm = mk_sp (Precord [q; aid]) in *)
                     (* let args : ('id, 'typ, 'term) term_arg list = [] in *)

                     (* M.mk_mterm (M.Mcall Icall (Some asset, Cconst Cupdate, args)) Tunit *)

                     emit_error TODO
                   )
                 | _ ->
                   let a : M.mterm = M.mk_mterm (M.Mvarlocal id) (M.Tbuiltin Bbool) ~loc:(Location.loc id) in
                   M.mk_mterm (M.Massign (ValueAssign, state, a)) Tunit in
               let code : M.mterm =
                 match effect with
                 | Some e -> M.mk_mterm (M.Mseq [tre; to_instruction e]) Tunit
                 | None -> tre
               in

               match cond with
               | Some c -> M.mk_mterm (M.Mif (to_mterm c, code, Some acc)) Tunit
               | None -> code
             ) t.trs body)
        in
        let code : M.mterm = M.mk_mterm (M.Mseq []) Tunit in
        let body : M.mterm = build_code code in

        let body = match t.from.node with
          | Sany -> body
          | _ ->
            begin
              let rec compute_patterns (a : A.sexpr) : M.pattern list =
                match a.node with
                | Sref id -> [M.mk_pattern (M.Pconst id)]
                | Sor (a, b) -> [a; b] |> List.map (fun x -> compute_patterns x) |> List.flatten
                | Sany -> emit_error (AnyNotAuthorizedInTransitionTo a.loc)
              in
              let list_patterns : M.pattern list =
                compute_patterns t.from in

              let pattern : M.pattern = M.mk_pattern M.Pwild in
              let fail_instr : M.mterm = fail InvalidState in

              let w = M.mk_mterm (M.Mvarstorevar state) (Tenum (dumloc "_state")) in
              M.mk_mterm (M.Mmatchwith (w, List.map (fun x -> (x, code)) list_patterns @ [pattern, fail_instr])) Tunit
            end
        in
        args, body

      | _ -> emit_error CannotExtractBody
    in

    let list  = list |> cont process_function ast.functions in
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
    let verif : M.verification option = Option.map to_verification transaction.verification in

    process_fun_gen name args body loc verif (fun x -> M.Entry x) list
  in

  let name = ast.name in

  let decls =
    []
    |> process_enums
    |> process_records
    |> process_contracts
  in

  let storage_items = process_storage () in
  let storage = storage_items in

  let functions =
    []
    |> cont process_function ast.functions
    |> cont process_transaction ast.transactions
  in

  let verification =
    M.mk_verification ()
    |> (fun verif -> List.fold_left (fun accu x -> cont_verification x accu) verif ast.verifications)
  in

  M.mk_model ~info:info ~decls:decls ~functions:functions ~verification:verification storage name
  |> reduce_forall
