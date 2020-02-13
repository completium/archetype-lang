open Location
open Ident
open Tools

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
  | NoInitialValueFor of string
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
    | A.Subset     -> M.Collection
    | A.List       -> M.List
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
    | A.VTkey        -> M.Bkey
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


  let extract_asset_name (mterm : M.mterm) : ident =
    match mterm with
    | {type_ = Tcontainer (Tasset asset_name, _); _} -> unloc asset_name
    | _ ->
      Format.printf "extract_asset_name error: %a@\n" M.pp_type_ mterm.type_;
      assert false
  in

  let extract_field_name (_id, _type_, body : A.lident * A.ptyp * A.pterm) : M.lident =
    match body.node with
    | A.Pdot (_, fn) -> fn
    | _ ->
      Format.printf "extract_field_name error: %a@\n" A.pp_pterm body;
      assert false
  in

  let to_action_description (ad : A.action_description) : M.action_description=
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


  let to_mterm_node (n : A.lident A.term_node) (f : A.lident A.term_gen -> M.mterm) (ftyp : 't -> M.type_) (pterm : A.pterm) (formula : bool) : (M.lident, M.mterm) M.mterm_node =
    let process_before vt e =
      match vt with
      | A.VTbefore -> M.Msetbefore (M.mk_mterm e (ftyp (Option.get pterm.type_)) ~loc:pterm.loc)
      | A.VTat lbl -> M.Msetat (lbl, M.mk_mterm e (ftyp (Option.get pterm.type_)) ~loc:pterm.loc)
      | A.VTnone -> e
    in
    match n with
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
    | A.Parith (A.Div, l, r)            -> M.Mdiv           (f l, f r)
    | A.Parith (A.DivRat, l, r)         -> M.Mdivrat        (f l, f r)
    | A.Parith (A.Modulo, l, r)         -> M.Mmodulo        (f l, f r)
    | A.Puarith (A.Uplus, e)            -> M.Muplus         (f e)
    | A.Puarith (A.Uminus, e)           -> M.Muminus        (f e)
    | A.Precord l                       -> M.Masset         (List.map f l)| A.Pcall (Some p, A.Cconst A.Cbefore,    []) -> M.Msetbefore    (f p)
    | A.Pletin (id, init, typ, body, o) -> M.Mletin         ([id], f init, Option.map ftyp typ, f body, Option.map f o)
    | A.Pdeclvar (i, t, v)              -> M.Mdeclvar       ([i], Option.map ftyp t, f v)
    | A.Pvar (b, id) when A.Utils.is_variable ast id   -> let e = M.Mvarstorevar id in process_before b e
    | A.Pvar (b, id) when A.Utils.is_asset ast id      -> let e = M.Mvarstorecol id in process_before b e
    | A.Pvar (b, id) when A.Utils.is_enum_value ast id -> let e = M.Mvarenumval  id in process_before b e
    | A.Pvar (b, id)                                   -> let e = M.Mvarlocal    id in process_before b e
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
    | A.Plit ({node = BVduration d; _})      -> M.Mduration d
    | A.Plit ({node = BVbytes v; _})         -> M.Mbytes v
    | A.Pdot (d, i) ->
      (* handle dot contract too *)
      M.Mdotasset (f d, i)
    | A.Pconst Cstate                        -> M.Mvarstate
    | A.Pconst Cnow                          -> M.Mnow
    | A.Pconst Ctransferred                  -> M.Mtransferred
    | A.Pconst Ccaller                       -> M.Mcaller
    | A.Pconst Cbalance                      -> M.Mbalance
    | A.Pconst Csource                       -> M.Msource
    | A.Pconst c                             ->
      Format.eprintf "expr const unkown: %a@." A.pp_const c;
      assert false

    | A.Ptuple l                             -> M.Mtuple (List.map f l)
    | A.Pquantifer (Forall, i, (coll, typ), term)    -> M.Mforall (i, ptyp_to_type typ, Option.map f coll, f term)
    | A.Pquantifer (Exists, i, (coll, typ), term)    -> M.Mexists (i, ptyp_to_type typ, Option.map f coll, f term)

    (* | A.Pcall (Some p, A.Cconst A.Cbefore,    []) -> M.Msetbefore    (f p) *)
    | A.Pcall (Some p, A.Cconst A.Cunmoved,   []) -> M.Msetunmoved   (f p)
    | A.Pcall (Some p, A.Cconst A.Cadded,     []) -> M.Msetadded     (f p)
    | A.Pcall (Some p, A.Cconst A.Cremoved,   []) -> M.Msetremoved   (f p)
    | A.Pcall (Some p, A.Cconst A.Citerated,  []) -> M.Msetiterated  (f p)
    | A.Pcall (Some p, A.Cconst A.Ctoiterate, []) -> M.Msettoiterate (f p)

    | A.Pcall (None, A.Cconst A.Cmin, [AExpr a; AExpr b]) ->
      let fa = f a in
      let fb = f b in
      M.Mfunmin (fa, fb)

    | A.Pcall (None, A.Cconst A.Cmax, [AExpr a; AExpr b]) ->
      let fa = f a in
      let fb = f b in
      M.Mfunmax (fa, fb)

    | A.Pcall (None, A.Cconst A.Cabs, [AExpr a]) ->
      let fa = f a in
      M.Mfunabs (fa)

    | A.Pcall (_, A.Cid id, args) ->
      M.Mapp (id, List.map (fun x -> term_arg_to_expr f x) args)

    | A.Pcall (Some p, A.Cconst (A.Csubsetof), [AExpr q]) ->
      let fp = f p in
      let fq = f q in
      let asset_name = extract_asset_name fp in
      M.Mapifsubsetof (asset_name, fp, fq)

    | A.Pcall (Some p, A.Cconst (A.Cisempty), []) ->
      let fp = f p in
      let asset_name = extract_asset_name fp in
      M.Mapifisempty (asset_name, fp)

    | A.Pcall (Some p, A.Cconst (A.Cget), [AExpr q]) ->
      let fp = f p in
      let fq = f q in
      let asset_name = extract_asset_name fp in
      if formula
      then M.Mapifget (asset_name, fp, fq)
      else M.Mget (asset_name, fq)

    | A.Pcall (Some p, A.Cconst (A.Cselect), [AFun (_qi, _qt, q)]) ->
      let fp = f p in
      let fq = f q in
      let asset_name = extract_asset_name fp in
      if formula
      then M.Mapifselect (asset_name, fp, fq)
      else M.Mselect (asset_name, fp, fq)

    | A.Pcall (Some p, A.Cconst (A.Csort), [ASorting (asc, field_name)]) ->
      let fp = f p in
      let asset_name = extract_asset_name fp in
      let sort_kind = match asc with | true -> M.SKasc | false -> M.SKasc in
      if formula
      then M.Mapifsort (asset_name, fp, unloc field_name, sort_kind)
      else M.Msort (asset_name, fp, unloc field_name, sort_kind)

    | A.Pcall (Some p, A.Cconst (A.Ccontains), [AExpr q]) ->
      let fp = f p in
      let fq = f q in
      let asset_name = extract_asset_name fp in
      if formula
      then M.Mapifcontains (asset_name, fp, fq)
      else M.Mcontains (asset_name, fp, fq)

    | A.Pcall (Some p, A.Cconst (A.Cnth), [AExpr q]) ->
      let fp = f p in
      let fq = f q in
      let asset_name = extract_asset_name fp in
      if formula
      then M.Mapifnth (asset_name, fp, fq)
      else M.Mnth (asset_name, fp, fq)

    | A.Pcall (Some p, A.Cconst (A.Ccount), []) ->
      let fp = f p in
      let asset_name = extract_asset_name fp in
      if formula
      then M.Mapifcount (asset_name, fp)
      else M.Mcount (asset_name, fp)

    | A.Pcall (Some p, A.Cconst (A.Csum), [AFun (qi, qt, q)]) ->
      let fp = f p in
      let asset_name = extract_asset_name fp in
      let field_name = extract_field_name (qi, qt, q) in
      if formula
      then M.Mapifsum (asset_name, field_name, fp)
      else M.Msum (asset_name, field_name, fp)

    | A.Pcall (Some p, A.Cconst (A.Cmin), [AFun (qi, qt, q)]) ->
      let fp = f p in
      let asset_name = extract_asset_name fp in
      let field_name = extract_field_name (qi, qt, q) in
      if formula
      then M.Mapifmin (asset_name, field_name, fp)
      else M.Mmin (asset_name, field_name, fp)

    | A.Pcall (Some p, A.Cconst (A.Cmax), [AFun (qi, qt, q)]) ->
      let fp = f p in
      let asset_name = extract_asset_name fp in
      let field_name = extract_field_name (qi, qt, q) in
      if formula
      then M.Mapifmax (asset_name, field_name, fp)
      else M.Mmax (asset_name, field_name, fp)

    | A.Pcall (Some p, A.Cconst (A.Chead), [AExpr e]) ->
      let fp = f p in
      let fe = f e in
      let asset_name = extract_asset_name fp in
      if formula
      then M.Mapifhead (asset_name, fp, fe)
      else M.Mhead (asset_name, fp, fe)

    | A.Pcall (Some p, A.Cconst (A.Ctail), [AExpr e]) ->
      let fp = f p in
      let fe = f e in
      let asset_name = extract_asset_name fp in
      if formula
      then M.Mapiftail (asset_name, fp, fe)
      else M.Mtail (asset_name, fp, fe)

    (* | A.Pcall (None, A.Cconst (A.Cmaybeperformedonlybyrole), [AExpr l; AExpr r]) ->
       M.MsecMayBePerformedOnlyByRole (f l, f r)

       | A.Pcall (None, A.Cconst (A.Cmaybeperformedonlybyaction), [AExpr l; AExpr r]) ->
       M.MsecMayBePerformedOnlyByAction (f l, f r)

       | A.Pcall (None, A.Cconst (A.Cmaybeperformedbyrole), [AExpr l; AExpr r]) ->
       M.MsecMayBePerformedByRole (f l, f r)

       | A.Pcall (None, A.Cconst (A.Cmaybeperformedbyaction), [AExpr l; AExpr r]) ->
       M.MsecMayBePerformedByAction (f l, f r) *)

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

  let rec to_mterm ?(formula=false) (pterm : A.pterm) : M.mterm =
    let node = to_mterm_node pterm.node (to_mterm ~formula:formula) ptyp_to_type pterm formula in
    let type_ = ptyp_to_type (Option.get pterm.type_) in
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
        M.mk_asset_item x.name typ typ ?default:default ~shadow:x.shadow) a.fields in
    let r : M.asset = M.mk_asset a.name (unloc (Option.get a.key)) ~values:values ~sort:(List.map unloc (a.sort)) ~invariants:(List.map (fun x -> to_label_lterm x) a.specs) in
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

  let mk_contract_call (c : A.lident A.term_poly) =
    match c.type_ with
    | Some (Tcontract v) -> Some (unloc v)
    | _ -> None
  in

  let is_contract_call c =
    match mk_contract_call c with
    | Some _ -> true
    | _ -> false
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
    | A.Iiter (i, a, b, body)   -> M.Miter (i, f a, f b, g body, lbl)
    | A.Iletin (i, init, cont)  -> M.Mletin ([i], f init, Option.map ptyp_to_type init.type_, g cont, None) (* TODO *)
    | A.Ideclvar (i, v)         -> M.Mdeclvar ([i], Option.map ptyp_to_type v.type_, f v) (* TODO *)
    | A.Iseq l                  -> M.Mseq (List.map g l)
    | A.Imatchwith (m, l)       -> M.Mmatchwith (f m, List.map (fun (p, i) -> (to_pattern p, g i)) l)
    | A.Iassign (t, op, `Var x, e) -> M.Massign (to_assignment_operator op, ptyp_to_type t, x, to_mterm e)
    | A.Iassign (t, op, `Field (nm, x), e) -> M.Massignfield (to_assignment_operator op, ptyp_to_type t, to_mterm nm, x, to_mterm e)
    | A.Irequire (b, t)         ->
      let cond : M.mterm =
        if b
        then term_not (f t)
        else (f t)
      in
      M.Mif (cond, fail (InvalidCondition None), None)

    | A.Itransfer (d, v)        -> M.Mtransfer (f d, f v)
    | A.Ibreak                  -> M.Mbreak
    | A.Ireturn e               -> M.Mreturn (f e)
    | A.Ilabel i                -> M.Mlabel i
    | A.Ifail m                 -> M.Mfail (Invalid (f m))
    | A.Icall (Some c, Cid id, args) when (is_contract_call c) ->
      let contract_id = Option.get (mk_contract_call c) in
      let c = f c in
      let ids = A.Utils.get_contract_sig_ids ast contract_id (unloc id) in
      let vs = List.map (term_arg_to_expr f) args in
      let args = List.map2 (fun x y -> (x, y)) ids vs in
      M.Mexternal (contract_id, id, c, args)

    | A.Icall (i, Cid id, args) -> M.Mapp (id, Option.map_dfl (fun v -> [to_mterm v]) [] i @ List.map (term_arg_to_expr f) args)

    | A.Icall (_, A.Cconst (A.Cfail), [AExpr p]) ->
      M.Mfail (Invalid (f p))

    | A.Icall (Some p, A.Cconst (A.Cadd), [AExpr q]) -> (
        let fp = f p in
        let fq = f q in
        match fp with
        | {node = M.Mvarstorecol asset_name; _} -> M.Maddasset (unloc asset_name, fq)
        | {node = M.Mdotasset ({type_ = M.Tasset asset_name ; _} as arg, f); _} -> M.Maddfield (unloc asset_name, unloc f, arg, fq)
        | _ -> assert false
      )

    | A.Icall (Some p, A.Cconst (A.Cremove), [AExpr q]) -> (
        let fp = f p in
        let fq = f q in
        match fp with
        | {node = M.Mvarstorecol asset_name; _} -> M.Mremoveasset (unloc asset_name, fq)
        | {node = M.Mdotasset ({type_ = M.Tasset asset_name ; _} as arg, f); _} -> M.Mremovefield (unloc asset_name, unloc f, arg, fq)
        | _ -> assert false
      )

    | A.Icall (Some p, A.Cconst (A.Cclear), []) -> (
        let fp = f p in
        match fp with
        | {node = M.Mvarstorecol asset_name; _} -> M.Mclearasset (unloc asset_name)
        | {node = M.Mdotasset ({type_ = M.Tasset asset_name ; _}, f); _} -> M.Mclearfield (unloc asset_name, unloc f)
        | _ -> assert false
      )

    | A.Icall (Some p, A.Cconst (A.Caddupdate), [AExpr k; AEffect e]) ->
      let to_op = function
        | `Assign op -> to_assignment_operator op
        | _ -> emit_error CannotConvertToAssignOperator
      in
      let fp = f p in
      let fk = f k in
      let fe = List.map (fun (id, op, c) -> (id, to_op op, f c)) e in
      let asset_name = extract_asset_name fp in
      M.Maddupdate (asset_name, fk, fe)

    | A.Icall (Some p, A.Cconst (A.Cupdate), [AExpr k; AEffect e]) ->
      let to_op = function
        | `Assign op -> to_assignment_operator op
        | _ -> emit_error CannotConvertToAssignOperator
      in
      let fp = f p in
      let fk = f k in
      let fe = List.map (fun (id, op, c) -> (id, to_op op, f c)) e in
      let asset_name = extract_asset_name fp in
      M.Mupdate (asset_name, fk, fe)

    | A.Icall (Some p, A.Cconst (A.Cremoveif), [AFun (_qi, _qtt, q)]) ->
      let fp = f p in
      let fq = f q in
      let asset_name = extract_asset_name fp in
      M.Mremoveif (asset_name, fp, fq)

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

  let rec to_instruction (instr : A.instruction) : M.mterm =
    let node = to_instruction_node instr.node instr.label to_instruction to_mterm in
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
          let to_security_action (sa : A.security_action) : M.security_action =
            match sa with
            | Sany -> Sany
            | Sentry l -> Sentry l
          in
          match sn with
          | SonlyByRole         (ad, roles)         -> SonlyByRole         (to_action_description ad, roles)
          | SonlyInAction       (ad, action)        -> SonlyInAction       (to_action_description ad, to_security_action action)
          | SonlyByRoleInAction (ad, roles, action) -> SonlyByRoleInAction (to_action_description ad, roles, to_security_action action)
          | SnotByRole          (ad, roles)         -> SnotByRole          (to_action_description ad, roles)
          | SnotInAction        (ad, action)        -> SnotInAction        (to_action_description ad, to_security_action action)
          | SnotByRoleInAction  (ad, roles, action) -> SnotByRoleInAction  (to_action_description ad, roles, to_security_action action)
          | StransferredBy      (ad)                -> StransferredBy      (to_action_description ad)
          | StransferredTo      (ad)                -> StransferredTo      (to_action_description ad)
          | SnoStorageFail      (action)            -> SnoStorageFail      (to_security_action action)
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

  let cont f x l = List.fold_left (fun accu x -> f x accu) l x in

  let process_fun_gen name args (body : M.mterm) loc spec f (list : M.function__ list) : M.function__ list =
    let node = f (M.mk_function_struct name body
                    ~args:args
                    ~loc:loc) in
    list @ [M.mk_function ?spec:spec node]
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
    let spec : M.specification option = Option.map to_specification function_.specification in
    process_fun_gen name args body loc spec (fun x -> M.Function (x, ret)) list
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
        let rec process_rexpr (rq : A.rexpr) : M.mterm option =
          let caller : M.mterm = M.mk_mterm M.Mcaller (M.Tbuiltin Baddress) in
          match rq.node with
          | Rany -> None
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
              Some (M.mk_mterm (M.Mequal (caller, addr)) (M.Tbuiltin Bbool) ~loc:rq.loc)
            end
          | Ror (l, r) ->
            let l = Option.get (process_rexpr l) in
            let r = Option.get (process_rexpr r) in
            Some (M.mk_mterm (M.Mor (l, r)) (M.Tbuiltin Bbool) ~loc:rq.loc)
          | Raddress a ->
            let addr   : M.mterm = M.mk_mterm (M.Maddress (unloc a)) (M.Tbuiltin Baddress) in
            Some (M.mk_mterm (M.Mequal (caller, addr)) (M.Tbuiltin Bbool) ~loc:rq.loc)
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
        let args =
          match t.on with
          | Some (id, id2) -> args @ [(id, M.Tasset id2, None)]
          | None -> args
        in
        let build_code (body : M.mterm) : M.mterm =
          (List.fold_right (fun ((id, cond, effect) : (A.lident * A.pterm option * A.instruction option)) (acc : M.mterm) : M.mterm ->
               let tre : M.mterm =
                 match t.on with
                 | Some (_id, _id_asset) -> assert false
                 | _ ->
                   let a : M.mterm = M.mk_mterm (M.Mvarlocal id) (M.Tstate) ~loc:(Location.loc id) in
                   M.mk_mterm (M.Massignstate a) Tunit in
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
                | Sany -> emit_error (AnyNotAuthorizedInTransitionTo a.loc)
              in
              let list_patterns : M.pattern list =
                compute_patterns t.from in

              let pattern : M.pattern = M.mk_pattern M.Pwild in
              let fail_instr : M.mterm = fail InvalidState in

              let w = M.mk_mterm (M.Mvarstate) Tstate in
              M.mk_mterm (M.Mmatchwith (w, List.map (fun x -> (x, body)) list_patterns @ [pattern, fail_instr])) Tunit
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
    let spec : M.specification option = Option.map to_specification transaction.specification in

    process_fun_gen name args body loc spec (fun x -> M.Entry x) list
  in

  let name = ast.name in

  let decls =
    []
    |> (@) (List.map (fun x -> process_var x) ast.variables)
    |> (@) (List.map (fun x -> process_enum x) ast.enums)
    |> (@) (List.map (fun x -> process_asset x) ast.assets)
    |> (@) (List.map (fun x -> M.Dcontract (to_contract x)) ast.contracts)
  in

  let functions =
    []
    |> cont process_function ast.functions
    |> cont process_transaction ast.transactions
  in

  let specification =
    M.mk_specification ()
    |> (fun spec -> List.fold_left (fun accu x -> cont_specification x accu) spec ast.specifications)
  in

  let security =
    M.mk_security ()
    |> (fun sec -> List.fold_left (fun accu x -> cont_security x accu) sec ast.securities)
  in

  M.mk_model ~decls:decls ~functions:functions ~specification:specification ~security:security name
