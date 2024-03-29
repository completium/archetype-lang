open Model
open Gen_transform
open Tools
open Location
open Ident

module T = Michelson

type model_entries =
  | Munion of model_entries * model_entries
  | Mentry of ident * T.type_
[@@deriving show {with_path = false}]

let rec pp_model_entries fmt =
  function
  | Munion (lt, rt) -> Format.fprintf fmt "+@\n  @[%a@]@\n  @[%a@]@\n" pp_model_entries lt pp_model_entries rt
  | Mentry (id, args) -> Format.fprintf fmt "%s(%a)" id Printer_michelson.pp_type args

let get_model_entries (parameter : T.type_) : model_entries =
  let rec aux (ty : T.type_) : model_entries =
    match ty with
    | {annotation = Some id} -> Mentry (Gen_decompile.remove_prefix_annot id, ty)
    | { node = Tor(lt, rt) } -> Munion (aux lt, aux rt)
    | _ -> Mentry ("default", ty)
  in
  let res = aux parameter in
  (* Format.eprintf "%a@\n" pp_model_entries res; *)
  res

let build_entries (env : Gen_decompile.env) (model : model) : model =
  let entries = get_model_entries (Option.get env.type_parameter) in
  let mk_seq a b c =
    let aux (a : mterm) (b : mterm) : mterm =
      match a.node, b.node with
      | Mseq aa, Mseq bb -> mk_mterm (Mseq (aa @ bb)) tunit
      | Mseq aa, _ -> {a with node = Mseq (aa @ [b])}
      | _ , Mseq bb -> {b with node = Mseq (a::bb)}
      | _, _ -> mk_mterm (Mseq ([a; b])) tunit
    in
    aux (seq a) b |> aux (seq c)
  in
  let seek_ifleft param code =
    let f code =
      match code.node with
      | Mdmatchor ({node = Mvar (id, _) }, pl, bl, pr, br) when String.equal (unloc_mident id) param -> Some (pl, bl, pr, br)
      | _ -> None
    in
    match code.node with
    | Mseq s -> begin
        let rec aux accu xs =
          match xs with
          | a::tl -> begin
              match f a with
              | Some (lids, le, rids, re) -> Some (lids, mk_seq accu le tl, rids, mk_seq accu re tl)
              | None -> aux (accu @ [a]) tl
            end
          | _ -> None
        in
        aux [] s
      end
    | _ -> f code
  in
  let rec aux (entries : model_entries) (arg_pattern : dpattern) (code : mterm) : function_node list =
    match entries with
    | Munion (l, r) -> begin
        let opt_param = match arg_pattern with | DPid id -> Some id | _ -> None in
        let param = Option.get opt_param in
        match seek_ifleft param code with
        | Some (pl, bl, pr, br) -> (aux l pl bl) @ (aux r pr br)
        | None -> []
      end
    | Mentry (name, pty) -> begin
        let replace_var src dst code =
          let rec aux mt =
            match mt.node with
            | Mvar (id, vk) when String.equal (unloc_mident id) src-> {mt with node = Mvar (mk_mident (dumloc dst), vk)}
            | _ -> map_mterm aux mt
          in
          aux code
        in
        let flat_pair (tys : T.type_ list) =
          match List.rev tys with
          | (T.{node = Tpair a})::tl -> List.rev tl @ a
          | _ -> tys
        in
        let rec flat_all_pair (ty : T.type_) =
          match ty.node with
          | Tpair ls -> List.map flat_all_pair ls |> List.flatten
          | _ -> [ty]
        in
        let rec flat_dpattern = function
          | DPid id -> [id]
          | DPlist dps -> List.map flat_dpattern dps |> List.flatten
        in
        let get_annot = Gen_decompile.get_annot_from_type in
        let do_it ids tys =
          let a = List.map2 (fun id ty -> (mk_mident (dumloc (match get_annot ty with Some id -> id | None -> id)), Gen_decompile.ttype_to_mtype ty, None)) ids tys in
          let c = List.map2 (fun src ty -> (src, (match get_annot ty with | Some annot -> annot | _ -> src))) ids tys in
          let b = List.fold_left (fun code (src, dst) -> replace_var src dst code) code c in
          (a, b)
        in
        let args, code =
          match arg_pattern, pty.node with
          | _, T.Tunit -> [], code
          | DPid id, _ -> [(mk_mident (dumloc "arg"), Gen_decompile.ttype_to_mtype pty, None)], replace_var id "arg" code
          | DPlist ids, T.Tpair tys when List.length ids = List.length (flat_pair tys) && List.for_all (function | DPid _ -> true | _ -> false) ids -> begin
              let tys = flat_pair tys in
              let ids = List.map (function | DPid id -> id | _ -> assert false) ids in
              do_it ids tys
            end
          | dps, _ when List.length (flat_dpattern dps) = List.length (flat_all_pair pty) -> begin
              let tys = flat_all_pair pty in
              let ids = flat_dpattern dps in
              do_it ids tys
            end
          | _, _ ->
            Format.eprintf "node:%a@\narg_pattern:%a@\n"
              Printer_michelson.pp_type pty
              pp_dpattern arg_pattern;
            assert false
        in
        [Entry (mk_function_struct ~args (mk_mident (dumloc name)) code)]
      end
  in
  let def_entry : mterm option =
    match List.find_opt (fun (x : function_node) -> (match x with | Entry fs -> String.equal (unloc_mident fs.name) "default" | _ -> false)) model.functions with
    | Some (Entry fs) -> Some fs.body
    | _ -> None
  in
  match def_entry with
  | Some code -> {model with functions = aux entries (DPid "arg_1") code }
  | None -> model

let add_decls_var (_env : Gen_decompile.env) (model : model) : model =
  let rec get_init_value (ty : type_) : mterm =
    let f = get_init_value in
    match get_ntype ty with
    | Tasset _                   -> assert false
    | Tenum _                    -> assert false
    | Tstate                     -> mk_int 0
    | Tbuiltin Bunit             -> unit
    | Tbuiltin Bbool             -> mk_bool false
    | Tbuiltin Bint              -> mk_int 0
    | Tbuiltin Brational         -> mk_rat 0 1
    | Tbuiltin Bdate             -> mk_date (Core.mk_date ())
    | Tbuiltin Bduration         -> mk_duration (Core.mk_duration ())
    | Tbuiltin Btimestamp        -> mk_int 0
    | Tbuiltin Bstring           -> mk_string ""
    | Tbuiltin Baddress          -> mk_address "tz1XvkuUNDk8j2tG3RJaRUo4Xppcjc6FvK39"
    | Tbuiltin Btez              -> mk_tez 0
    | Tbuiltin Bsignature        -> mk_string "signature"
    | Tbuiltin Bkey              -> mk_string "key"
    | Tbuiltin Bkeyhash          -> mk_string "key_hash"
    | Tbuiltin Bbytes            -> mk_bytes ""
    | Tbuiltin Bnat              -> mk_nat 0
    | Tbuiltin Bchainid          -> mk_string "chain_id"
    | Tbuiltin Bbls12_381_fr     -> mk_string "bls12_381_fr"
    | Tbuiltin Bbls12_381_g1     -> mk_string "bls12_381_g1"
    | Tbuiltin Bbls12_381_g2     -> mk_string "bls12_381_g2"
    | Tbuiltin Bnever            -> mk_string "never"
    | Tbuiltin Bchest            -> mk_string "chest"
    | Tbuiltin Bchest_key        -> mk_string "chest_key"
    | Tcontainer (_, _)          -> assert false
    | Tlist t                    -> mk_litlist t []
    | Toption t                  -> mk_none t
    | Ttuple ts                  -> let vs = List.map f ts in mk_tuple vs
    | Tset t                     -> mk_litset t []
    | Tmap (kt, vt)              -> mk_litmap kt vt []
    | Tbig_map (kt, vt)          -> mk_litbig_map kt vt []
    | Titerable_big_map (_kt, _vt) -> assert false
    | Tor _                      -> assert false
    | Trecord _                  -> assert false
    | Tevent _                   -> assert false
    | Tlambda _                  -> assert false
    | Tunit                      -> unit
    | Toperation                 -> assert false
    | Tcontract _                -> assert false
    | Tticket _                  -> assert false
    | Tsapling_state _           -> assert false
    | Tsapling_transaction _     -> assert false
  in
  let mk_decl (id, ty : ident * type_) : mterm =
    let mk_dvar id ty v = mk_declvar (mk_mident (dumloc id)) ty v in
    let v = get_init_value ty in
    mk_dvar id ty v
  in
  let for_mterm (mt : mterm) : mterm =
    let add_var (accu : (ident * type_) list) (id, ty : ident * type_) =
      match List.find_opt (fun x -> String.equal x id) (List.map fst accu) with
      | Some _ -> accu
      | None -> (id, ty)::accu
    in
    let fetched_vars : (ident * type_) list =
      let is_var input =
        let r = Str.regexp "^x[0-9]+" in
        let res = Str.string_match r input 0 in
        res
      in
      let rec aux (accu : (ident * type_) list) (mt : mterm) : (ident * type_) list =
        match mt.node with
        | Mvar (id, _) when is_var (unloc_mident id) -> add_var accu (unloc_mident id, mt.type_)
        | _ -> fold_term aux accu mt
      in
      aux [] mt
    in
    let removed_ids =
      let rec for_dpatterm = function
        | DPid id -> [id]
        | DPlist dps -> List.map for_dpatterm dps |> List.flatten
      in

      let for_for_ident = function
        | FIsimple id -> [unloc_mident id]
        | FIdouble (i, j) -> List.map unloc_mident [i; j]
      in

      let rec aux (accu : ident list) (mt : mterm) : ident list =
        match mt.node with
        | Mfor (fi, _, b) -> aux accu b @ for_for_ident fi
        | Mdmatchoption (_, ps, bs, bn) -> aux (aux accu bs) bn @ for_dpatterm ps
        | Mdmatchor (_, psl, bl, psr, br) -> aux (aux accu bl) br @ for_dpatterm psl @ for_dpatterm psr
        | _ -> fold_term aux accu mt
      in
      aux [] mt
    in
    let lid_types : (ident * type_) list = List.filter  (fun (id, _) -> not (List.exists (fun x -> String.equal id x) removed_ids)) fetched_vars in
    let res = seq ((List.map mk_decl lid_types) @ [mt]) in
    res
  in
  let for_fs (fs : function_struct) : function_struct =
    {fs with body = for_mterm fs.body}
  in
  let for_fn = function
    | Function (fs, rft) -> Function (for_fs fs, rft)
    | Getter (fs, ty)    -> Getter (for_fs fs, ty)
    | View (fs, ty, vv)  -> View (for_fs fs, ty, vv)
    | Entry fs           -> Entry (for_fs fs)
  in
  {model with functions = List.map for_fn model.functions}

let remove_operations_nil (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Massign(ValueAssign, _, Avar (_, {pldesc = "operations"}), { node = (Mlitlist []) }) -> seq []
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let remove_tupleaccess (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Mtupleaccess({node = Mtuple vs}, i) when Big_int.lt_big_int i (Big_int.big_int_of_int (List.length vs)) -> begin
      let n = Big_int.int_of_big_int i in
      aux ctx (List.nth vs n)
    end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let remove_useless_else (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    let f = aux ctx in
    match mt.node with
    | Mif(cond, th, Some {node = Mseq []}) -> {mt with node = Mif(f cond, f th, None)}
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let optimize (model, env : model * Gen_decompile.env) =
  let model =
    model
    |> remove_tupleaccess
    |> remove_useless_else
    |> build_entries env
    |> add_decls_var env
    |> remove_operations_nil
    |> clean_mterm
    |> apply_syntactic_sugar
    |> flat_sequence
    |> transform_match
  in
  model, env
