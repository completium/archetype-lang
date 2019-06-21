(* open Location *)
open Tools

module A = Ast
module M = Model

exception Anomaly of string
type error_desc =
  | NotSupportedContainer of string
  | TODO
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

type enum_item_struct = (A.lident, A.type_, A.pterm) A.enum_item_struct
type verification     = (A.lident, A.type_, A.pterm) A.verification
type record           = (A.lident, A.type_, A.pterm) A.decl_gen
type field            = (A.lident, A.type_, A.pterm) A.decl_gen
type variable         = (A.lident, A.type_, A.pterm) A.variable

let to_container c =
  match c with
  | A.Collection -> M.Collection
  | A.Partition  -> M.Partition
  | _            -> emit_error (NotSupportedContainer (Format.asprintf "%a@." A.pp_container c))

let to_currency = function
  | A.Tez   -> M.Tez
  | A.Mutez -> M.Mutez

let vtyp_to_btyp = function
  | A.VTbool       -> M.Bbool
  | A.VTint        -> M.Bint
  | A.VTuint       -> M.Buint
  | A.VTrational   -> M.Brational
  | A.VTdate       -> M.Bdate
  | A.VTduration   -> M.Bduration
  | A.VTstring     -> M.Bstring
  | A.VTaddress    -> M.Baddress
  | A.VTrole       -> M.Brole
  | A.VTcurrency c -> M.Bcurrency (to_currency c)
  | A.VTkey        -> M.Bkey

let rec ptyp_to_type t : M.type_ =
  match t with
  | A.Tasset id          -> M.Tasset id
  | A.Tenum id           -> M.Tenum id
  | A.Tcontract id       -> M.Tcontract id
  | A.Tbuiltin b         -> M.Tbuiltin (vtyp_to_btyp b)
  | A.Tcontainer (t, c)  -> M.Tcontainer (ptyp_to_type t, to_container c)
  | A.Ttuple l           -> M.Ttuple (List.map ptyp_to_type l)

let to_vset = function
  | A.VSremoved -> M.VSremoved
  | A.VSadded   -> M.VSadded
  | A.VSstable  -> M.VSstable
  | A.VSbefore  -> M.VSbefore
  | A.VSafter   -> M.VSafter
  | A.VSfixed   -> M.VSfixed

let to_trtyp = function
  | A.TRentry  -> M.TRentry
  | A.TRaction -> M.TRaction
  | A.TRasset  -> M.TRasset
  | A.TRfield  -> M.TRfield

let rec ltyp_to_type t : M.type_ =
  match t with
  | A.LTprog t      -> ptyp_to_type t
  | A.LTvset (v, t) -> M.Tvset (to_vset v, ltyp_to_type t)
  | A.LTtrace tr    -> M.Ttrace (to_trtyp tr)

let to_logical_operator = function
  | A.And   -> M.And
  | A.Or    -> M.Or
  | A.Imply -> M.Imply
  | A.Equiv -> M.Equiv

let to_comparison_operator = function
  | A.Equal  -> M.Equal
  | A.Nequal -> M.Nequal
  | A.Gt     -> M.Gt
  | A.Ge     -> M.Ge
  | A.Lt     -> M.Lt
  | A.Le     -> M.Le

let to_arithmetic_operator = function
  | A.Plus   -> M.Plus
  | A.Minus  -> M.Minus
  | A.Mult   -> M.Mult
  | A.Div    -> M.Div
  | A.Modulo -> M.Modulo

let to_unary_arithmetic_operator = function
  | A.Uplus  -> M.Uplus
  | A.Uminus -> M.Uminus

let to_assignment_operator = function
  | A.ValueAssign  -> M.ValueAssign
  | A.PlusAssign   -> M.PlusAssign
  | A.MinusAssign  -> M.MinusAssign
  | A.MultAssign   -> M.MultAssign
  | A.DivAssign    -> M.DivAssign
  | A.AndAssign    -> M.AndAssign
  | A.OrAssign     -> M.OrAssign

let to_operator = function
  | `Logical op -> `Logical (to_logical_operator op)
  | `Cmp     op -> `Cmp     (to_comparison_operator op)
  | `Arith   op -> `Arith   (to_arithmetic_operator op)
  | `Unary   op -> `Unary   (to_unary_arithmetic_operator op)
  | `Assign  op -> `Assign  (to_assignment_operator op)

let to_const = function
  | A.Cstate                      -> M.Cstate
  | A.Cnow                        -> M.Cnow
  | A.Ctransferred                -> M.Ctransferred
  | A.Ccaller                     -> M.Ccaller
  | A.Cfail                       -> M.Cfail
  | A.Cbalance                    -> M.Cbalance
  | A.Cconditions                 -> M.Cconditions
  | A.Cactions                    -> M.Cactions
  | A.Cnone                       -> M.Cnone
  | A.Cany                        -> M.Cany
  | A.Canyaction                  -> M.Canyaction
  | A.Cget                        -> M.Cget
  | A.Cadd                        -> M.Cadd
  | A.Caddnofail                  -> M.Caddnofail
  | A.Cremove                     -> M.Cremove
  | A.Cremovenofail               -> M.Cremovenofail
  | A.Cremoveif                   -> M.Cremoveif
  | A.Cupdate                     -> M.Cupdate
  | A.Cupdatenofail               -> M.Cupdatenofail
  | A.Cclear                      -> M.Cclear
  | A.Ccontains                   -> M.Ccontains
  | A.Cnth                        -> M.Cnth
  | A.Creverse                    -> M.Creverse
  | A.Cselect                     -> M.Cselect
  | A.Csort                       -> M.Csort
  | A.Ccount                      -> M.Ccount
  | A.Csum                        -> M.Csum
  | A.Cmax                        -> M.Cmax
  | A.Cmin                        -> M.Cmin
  | A.Cbefore                     -> M.Cbefore
  | A.Cunmoved                    -> M.Cunmoved
  | A.Cadded                      -> M.Cadded
  | A.Cremoved                    -> M.Cremoved
  | A.Citerated                   -> M.Citerated
  | A.Ctoiterate                  -> M.Ctoiterate
  | A.Cmaybeperformedonlybyrole   -> M.Cmaybeperformedonlybyrole
  | A.Cmaybeperformedonlybyaction -> M.Cmaybeperformedonlybyaction
  | A.Cmaybeperformedbyrole       -> M.Cmaybeperformedbyrole
  | A.Cmaybeperformedbyaction     -> M.Cmaybeperformedbyaction


let rec to_qualid_node (n : ('a, 'b) A.qualid_node) : ('id, 'qualid) M.qualid_node =
  match n with
  | A.Qident i    -> M.Qident i
  | A.Qdot (d, i) -> M.Qdot (to_qualid_gen d, i)

and to_qualid_gen (q : A.qualid) : M.qualid =
  let node = to_qualid_node q.node in
  let type_ = ptyp_to_type (Option.get q.type_) in
  M.mk_qualid node type_

let to_pattern_node (n : ('a, 'b) A.pattern_node) : 'id M.pattern_node =
  match n with
  | A.Mconst id -> M.Pconst id
  | A.Mwild    -> M.Pwild

let to_pattern (p : A.pattern) : M.pattern =
  let node = to_pattern_node p.node in
  M.mk_pattern node ~loc:p.loc

let to_quantifier = function
  | A.Forall -> M.Forall
  | A.Exists -> M.Exists

let to_call_kind = function
  | A.Cid i    -> M.Cid i
  | A.Cconst c -> M.Cconst (to_const c)

let to_lit_value (b : 'typ A.bval_gen) =
  match b.node with
  | A.BVint i           -> M.BVint i
  | A.BVuint i          -> M.BVuint i
  | A.BVbool b          -> M.BVbool b
  | A.BVenum s          -> M.BVenum s
  | A.BVrational (d, n) -> M.BVrational (d, n)
  | A.BVdate s          -> M.BVdate s
  | A.BVstring s        -> M.BVstring s
  | A.BVcurrency (c, i) -> M.BVcurrency (to_currency c, i)
  | A.BVaddress s       -> M.BVaddress s
  | A.BVduration s      -> M.BVduration s

let rec to_mterm_node (n : ('a, 'b, 'c) A.term_node) f (ftyp : 'b -> M.type_) : ('id, 'term) M.mterm_node =
  match n with
  | A.Lquantifer (q, i, typ, term) -> M.Mquantifer (to_quantifier q, i, ltyp_to_type typ, f term)
  | A.Pif (c, t, e)                -> M.Mif (f c, f t, f e)
  | A.Pmatchwith (m, l)            -> M.Mmatchwith (f m, List.map (fun (p, e) -> (to_pattern p, f e)) l)
  | A.Pcall (id, ck, args)         -> M.Mcall (id, to_call_kind ck, List.map (fun x -> to_term_arg f x) args)
  | A.Plogical (op, l, r)          -> M.Mlogical (to_logical_operator op, f l, f r)
  | A.Pnot e                       -> M.Mnot (f e)
  | A.Pcomp (op, l, r)             -> M.Mcomp (to_comparison_operator op, f l, f r)
  | A.Parith (op, l, r)            -> M.Marith (to_arithmetic_operator op, f l, f r)
  | A.Puarith (op, e)              -> M.Muarith (to_unary_arithmetic_operator op, f e)
  | A.Precord l                    -> M.Mrecord (List.map f l)
  | A.Pletin (id, init, typ, cont) -> M.Mletin (id, f init, Option.map ftyp typ, f cont)
  | A.Pvar id                      -> M.Mvar id
  | A.Parray l                     -> M.Marray (List.map f l)
  | A.Plit lit                     -> M.Mlit (to_lit_value lit)
  | A.Pdot (d, i)                  -> M.Mdot (f d, i)
  | A.Pconst c                     -> M.Mconst (to_const c)
  | A.Ptuple l                     -> M.Mtuple (List.map f l)

and to_term_arg f = function
  | A.AExpr x -> M.AExpr (f x)
  | A.AEffect l -> M.AEffect (List.map (fun (id, op, term) -> (id, to_operator op, f term)) l)

let rec to_mterm (pterm : A.pterm) : M.mterm =
  let node = to_mterm_node pterm.node to_mterm ptyp_to_type in
  let type_ = ptyp_to_type (Option.get pterm.type_) in
  M.mk_mterm node type_ ~loc:pterm.loc

let rec lterm_to_mterm (lterm : A.lterm) : M.mterm =
  let node = to_mterm_node lterm.node lterm_to_mterm ltyp_to_type in
  let type_ = ltyp_to_type (Option.get lterm.type_) in
  M.mk_mterm node type_ ~loc:lterm.loc

let to_label_lterm (x : ('id, ('id, A.ltype_) A.term_gen) A.label_term) : M.lident M.label_term_gen =
  M.mk_label_term (lterm_to_mterm x.term) ?label:x.label ~loc:x.loc


let to_instruction_node (n : ('a, 'b, 'c, 'd) A.instruction_node) fi ft : ('id, 'instr) M.instruction_node =
  match n with
  | A.Iif (c, t, e)          -> M.Iif (ft c, fi t, fi e)
  | A.Ifor (i, col, body)    -> M.Ifor (i, ft col, fi body)
  | A.Iletin (i, init, cont) -> M.Iletin (i, ft init, fi cont)
  | A.Iseq l                 -> M.Iseq (List.map fi l)
  | A.Imatchwith (m, l)      -> M.Imatchwith (ft m, List.map (fun (p, i) -> (to_pattern p, fi i)) l)
  | A.Iassign (op, i, e)     -> M.Iassign (to_assignment_operator op, i, to_mterm e)
  | A.Irequire (b, t)        -> M.Irequire (b, ft t)
  | A.Itransfer (i, b, q)    -> M.Itransfer (ft i, b, Option.map to_qualid_gen q)
  | A.Ibreak                 -> M.Ibreak
  | A.Iassert e              -> M.Iassert (ft e)
  | A.Icall (i, ck, args)    -> M.Icall (Option.map to_mterm i, to_call_kind ck, List.map (to_term_arg ft) args)

let rec to_instruction (instr : A.instruction) : M.instruction =
  let node = to_instruction_node instr.node to_instruction to_mterm in
  M.mk_instruction node ~subvars:instr.subvars ~loc:instr.loc

let to_predicate (p : ('a, A.ptyp) A.predicate) : 'id M.predicate =
  M.mk_predicate p.name (lterm_to_mterm p.body) ~args:(List.map (fun (id, body) -> (id, lterm_to_mterm body)) p.args) ~loc:p.loc

let to_definition (d : ('a, A.ptyp) A.definition ): 'id M.definition =
  M.mk_definition d.name (ptyp_to_type d.typ) d.var (lterm_to_mterm d.body) ~loc:d.loc

(* let to_variable (v : (A.lident, A.ptyp, A.pterm) A.variable) : M.lident M.variable = *)

let to_invariant (i : (A.lident, A.ptyp) A.invariant) : M.lident M.invariant  =
  M.mk_invariant i.label ~formulas:(List.map lterm_to_mterm i.formulas)

(* let to_spec       : 'id spec       = () in *)
(* let to_assert     : 'id assert     = () in *)

let to_verification (v : (A.lident, A.ptyp, A.pterm) A.verification) : M.lident M.verification =
  let predicates  = List.map (fun x -> to_predicate x) v.predicates in
  let definitions = List.map (fun x -> to_definition x) v.definitions in
  let axioms      = List.map (fun x -> to_label_lterm x) v.axioms in
  let theorems    = List.map (fun x -> to_label_lterm x) v.theorems in
  (* let variables   = List.map (fun x -> to_variable x) v.variables in *)
  (* let invariants  = List.map (fun x -> to_label_lterm x) v.invariants in *)
  let effect      = Option.map to_mterm v.effect in
  (* let specs       = () in *)
  (* let asserts     = () in *)
  M.mk_verification
    ~predicates:predicates
    ~definitions:definitions
    ~axioms:axioms
    ~theorems:theorems
    (* ~variables:variables *)
    (* ~invariants:invariants *)
    ?effect:effect
    (* ~specs:specs *)
    (* ~asserts:asserts *)
    ~loc:v.loc ()

let to_model (ast : A.model) : M.model =
  let process_enums list =
    let process_enum (e : A.enum) : 'id M.decl_node =
      let enum = M.mk_enum e.name in
      M.TNenum {
        enum with
        values = List.map (fun (x : enum_item_struct) ->
            let id : M.lident = x.name in
            let enum_item = M.mk_enum_item id in
            {
              enum_item with
              invariants = [] (* TODO : Option.map_dfl (fun (x : verification) -> x.specs) [] x.verification*);
            }
          ) e.items;
      }
    in
    list @ List.map (fun x -> process_enum x) ast.enums in

  let process_records list =
    let process_asset (a : A.asset) : 'id M.decl_node =
      let r : 'id M.record = M.mk_record a.name in
      let r = {
        r with
        key = a.key;
        values=(List.map (fun (x : (A.lident, A.type_, A.pterm) A.decl_gen) ->
            let typ = Option.map ptyp_to_type x.typ in
            let default = Option.map to_mterm x.default in
            M.mk_record_item x.name (Option.get typ) ?default:default) a.fields);
      }
      in
      M.TNrecord r
    in
    list @ List.map (fun x -> process_asset x) ast.assets in

  let process_contracts list =
    let to_contract_signature (s : (A.lident, A.ptyp) A.signature) : M.lident M.contract_signature =
      let name = s.name in
      M.mk_contract_signature name ~args:(List.map (fun arg -> ptyp_to_type arg) s.args) ~loc:s.loc
    in
    let to_contract (c : (A.lident, A.ptyp, A.pterm) A.contract) : M.contract =
      M.mk_contract c.name
        ~signatures:(List.map to_contract_signature c.signatures)
        ?init:(Option.map to_mterm c.init)
        ~loc:c.loc
    in
    list @ List.map (fun (x : (A.lident, A.ptyp, A.pterm) A.contract) -> M.TNcontract (to_contract x)) ast.contracts in

  let process_storage list =
    let variable_to_storage_items (var : variable) : M.storage_item =
      let arg = var.decl in
      let compute_field (type_ : A.type_) : 'id M.item_field =
        let rec ptyp_to_item_field_type = function
          | A.Tbuiltin vtyp -> M.FBasic (vtyp_to_btyp vtyp)
          | A.Tenum id      -> M.FEnum id
          | A.Tasset id     -> M.FRecord id
          | A.Tcontract x   -> M.FBasic Brole
          | A.Tcontainer (ptyp, container) -> M.FContainer (to_container container, ptyp_to_item_field_type ptyp)
          | A.Ttuple _      -> assert false
        in
        let a = ptyp_to_item_field_type type_ in
        M.mk_item_field arg.name a (* TODO: ?default:(Option.get to_mterm arg.default)*)
      in

      let storage_item = M.mk_storage_item arg.name in
      let typ : A.type_ = Option.get arg.typ in {
        storage_item with
        fields = [compute_field typ];
        init = [];
      }
    in

    let asset_to_storage_items (asset : A.asset) : M.storage_item =
      let asset_name = asset.name in
      let compute_fields =
        let _, key_type = A.Utils.get_asset_key ast asset_name in
        let key_asset_name = Location.mkloc (Location.loc asset_name) ((Location.unloc asset_name) ^ "_keys") in
        let map_asset_name = Location.mkloc (Location.loc asset_name) ((Location.unloc asset_name) ^ "_assets") in
        [M.mk_item_field key_asset_name (FAssetKeys (vtyp_to_btyp key_type, asset_name))
           ~asset:asset_name
        (*?default:None TODO: uncomment this*);
         M.mk_item_field map_asset_name (FAssetRecord (vtyp_to_btyp key_type, asset_name))
           ~asset:asset_name
           (* ~default:arg.default TODO: uncomment this*)] in
      M.mk_storage_item asset.name ~fields:compute_fields (*TODO: ~invariants:asset.specs*) (*~init:asset.init D uncomment this *)
    in

    let cont f x l = l @ (List.map f x) in
    []
    |> cont variable_to_storage_items ast.variables
    |> cont asset_to_storage_items ast.assets
    |> (fun x -> list @ [M.TNstorage x])
  in

  let process_functions list : 'id M.decl_node list =
    let extract_function_from_instruction (instr : A.instruction) (list : 'id  M.decl_node list) : (A.instruction * 'id M.decl_node list) =
      let add l i =
        let e = List.fold_left (fun accu x ->
            if x = i
            then true
            else accu) false l in
        if e then
          l
        else
          i::l
      in
      let mk_function t field_name c (e : (A.lident, A.pterm) A.term_arg option) : ('id M.function__ * (A.lident, A.pterm) A.term_arg option) option =
        let is_global_asset (asset_name : A.lident) (e : (A.lident, A.pterm) A.term_arg option) =
          match e with
          | Some AExpr {node = Pvar {pldesc = id; _}; _} when String.equal (Location.unloc asset_name) id -> true
          | _ -> false
        in
        let get_first_arg asset_name (e : (A.lident, A.pterm) A.term_arg option) : (A.lident, A.pterm) A.term_arg option =
          if (is_global_asset asset_name e)
          then None
          else e
        in
        let node = match t, field_name, c, e with
          | A.Tcontainer (Tasset asset, Collection), None, A.Cget,      _ when is_global_asset asset e -> Some (M.Get asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cadd,      _ when is_global_asset asset e -> Some (M.AddAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cremove,   _ when is_global_asset asset e -> Some (M.RemoveAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cclear,    _ when is_global_asset asset e -> Some (M.ClearAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cupdate,   _ when is_global_asset asset e -> Some (M.UpdateAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Ccontains, _ when is_global_asset asset e -> Some (M.ContainsAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cnth,      _ when is_global_asset asset e -> Some (M.NthAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cselect,   _ when is_global_asset asset e -> Some (M.SelectAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Creverse,  _ when is_global_asset asset e -> Some (M.ReverseAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Csort,     _ when is_global_asset asset e -> Some (M.SortAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Ccount,    _ when is_global_asset asset e -> Some (M.CountAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Csum,      _ when is_global_asset asset e -> Some (M.SumAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cmin,      _ when is_global_asset asset e -> Some (M.MinAsset asset, get_first_arg asset e)
          | A.Tcontainer (Tasset asset, Collection), None, A.Cmax,      _ when is_global_asset asset e -> Some (M.MaxAsset asset, get_first_arg asset e)
          | A.Tasset asset, Some field, A.Cadd,      Some AExpr {node = A.Pdot (a, _)}  -> Some (M.AddContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Cremove,   Some AExpr {node = A.Pdot (a, _)}  -> Some (M.RemoveContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Cclear,    Some AExpr {node = A.Pdot (a, _)}  -> Some (M.ClearContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Ccontains, Some AExpr {node = A.Pdot (a, _)}  -> Some (M.ContainsContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Cnth,      Some AExpr {node = A.Pdot (a, _)}  -> Some (M.NthContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Cselect,   Some AExpr {node = A.Pdot (a, _)}  -> Some (M.SelectContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Creverse,  Some AExpr {node = A.Pdot (a, _)}  -> Some (M.ReverseContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Csort,     Some AExpr {node = A.Pdot (a, _)}  -> Some (M.SortContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Ccount,    Some AExpr {node = A.Pdot (a, _)}  -> Some (M.CountContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Csum,      Some AExpr {node = A.Pdot (a, _)}  -> Some (M.SumContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Cmax,      Some AExpr {node = A.Pdot (a, _)}  -> Some (M.MaxContainer (asset, field), Some (AExpr a))
          | A.Tasset asset, Some field, A.Cmin,      Some AExpr {node = A.Pdot (a, _)}  -> Some (M.MinContainer (asset, field), Some (AExpr a))
          | _ -> None in
        match node with
        | Some (node, x) -> Some (M.mk_function node, x)
        | _ -> None
      in

      let ge (e : A.pterm) = (fun node -> {e with node = node}) in

      let rec fe accu (term : A.pterm) : A.pterm * 'id M.decl_node list =
        match term.node with
        | A.Pcall (Some asset_name, Cconst c, args) -> (
            let _, accu = A.fold_map_term (fun node -> {term with node = node} ) fe accu term in
            let function__ = mk_function (A.Tcontainer (Tasset asset_name, Collection)) None c (Some (AExpr (A.mk_sp (A.Pvar asset_name)))) in
            let term, accu =
              match function__ with
              | Some (f, _) -> (
                  let node = f.node in
                  let fun_name = Location.dumloc (M.function_name_from_function_node node) in
                  {term with node = A.Pcall (None, Cid fun_name, args) }, add accu (M.TNfunction f)
                )
              | None -> term, accu in
            term, accu
          )
        | _ -> A.fold_map_term (ge term) fe accu term in

      let process_instr accu t (c : A.const) field_name gi fi node (args : (A.lident, A.pterm) A.term_arg list) =
        let a =
          match node with
          | A.Icall (a, _, _) -> a
          | _ -> emit_error TODO in

        let xe, xa =
          match a with
          | Some x -> fe accu x |> (fun (a, b) -> (Some a, b))
          | None -> None, accu in

        let (argss, argsa) =
          List.fold_left
            (fun (pterms, accu) arg ->
               match arg with
               | A.AExpr x ->
                 let p, accu = fe accu x in
                 [A.AExpr p] @ pterms, accu
               | _ -> (pterms, accu) (*TODO*))
            ([], xa) args
        in

        let first_arg = Option.map (fun x -> A.AExpr x) xe in
        let function__ = mk_function (Option.get t) field_name c first_arg in
        let instr, accu =
          match function__ with
          | Some (f, arg) -> (
              let new_args =
                match argss, arg with
                | _, Some arg -> arg::argss
                | _ -> argss in
              let node = f.node in
              let fun_name = Location.dumloc (M.function_name_from_function_node node) in
              {instr with node = A.Icall (None, Cid fun_name, new_args) }, add argsa (M.TNfunction f)
            )
          | None -> instr, accu in
        instr, accu in

      let rec fi accu (instr : A.instruction) : A.instruction * 'id M.decl_node list =
        let gi = (fun node -> {instr with node = node}) in
        match instr.node with
        | A.Icall (Some {node = A.Pdot ({type_ = t; _}, id); _}, Cconst c, args) ->
          process_instr accu t c (Some id) gi fi instr.node args

        | A.Icall (Some {type_ = t; _}, Cconst c, args) ->
          process_instr accu t c None gi fi instr.node args

        | _ ->
          A.fold_map_instr_term (fun node -> { instr with node = node } ) ge fi fe accu instr

      in
      fi list instr in

    let cont f x l = List.fold_left (fun accu x -> f x accu) l x in

    let process_fun_gen name args body loc verif f (list : 'id M.decl_node list) : 'id M.decl_node list =
      let instr, list = extract_function_from_instruction body list in
      let node = f (M.mk_function_struct name (to_instruction instr)
                      ~args:(List.map (fun (x : ('id, 'typ, 'term) A.decl_gen) -> (x.name, Option.get x.typ, None)) args)
                      ~loc:loc) in
      list @ [TNfunction (M.mk_function ?verif:verif node)]
    in

    let process_function (function_ : A.function_) (list : 'id M.decl_node list) : 'id M.decl_node list =
      let name  = function_.name in
      let args  = [] in (*function_.args in*)
      let body  = function_.body in
      let loc   = function_.loc in
      let ret   = ptyp_to_type function_.return in
      let verif : M.lident M.verification option = Option.map to_verification function_.verification in
      process_fun_gen name args body loc verif (fun x -> M.Function (x, ret)) list
    in

    let process_transaction (transaction : A.transaction) (list : 'id M.decl_node list) : 'id M.decl_node list =
      let list  = list |> cont process_function ast.functions in
      let name  = transaction.name in
      let args  = [] in (*transaction.args in*)
      let body  = Option.get transaction.effect in
      let loc   = transaction.loc in
      let verif : M.lident M.verification option = Option.map to_verification transaction.verification in
      process_fun_gen name args body loc verif (fun x -> M.Entry x) list
    in

    list
    |> cont process_function ast.functions
    |> cont process_transaction ast.transactions
  in

  let name = ast.name in
  let decls =
    []
    |> process_enums
    |> process_records
    |> process_contracts
    |> process_storage
    |> process_functions
  in
  M.mk_model name ~decls:decls
