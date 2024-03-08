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
    | _ -> Mentry ("_", ty)
  in
  let res = aux parameter in
  (* Format.eprintf "%a@\n" pp_model_entries res; *)
  res

(* let build_entries (env : Gen_decompile.env) (model : model) : model =
  let p : (type_ * mterm) option =
    match model.functions with
    | [Entry {args = [(_, pty, _)]; body = code}] -> Some (pty, code)
    | _ -> None
  in
  let rec split (code : mterm) : (mterm * mterm) option =
    match code.node with
    | Mseq [a]
    | Mseq [a; {node = (Massign (ValueAssign, (Tunit, None), (Avar (None, {pldesc = "ops"})), { node = (Mlitlist []); type_ = ((Tlist (Toperation, _)), _) })) }] -> split a
    | Minstrmatchor (_a, _b, c, _d, e) -> Some (c, e)
    | _ -> (Format.eprintf "%a" pp_mterm code; None) in
  let process_arg (t : type_) =
    match t with
    | _ -> [mk_mident (dumloc "arg"), t, None]
  in
  let rec process (pty, code : type_ * mterm) =
    match pty with
    | Tor (o1, o2), None -> begin
        match split code with
        | Some (c1, c2) -> process (o1, c1) @ process (o2, c2)
        | None -> assert false
      end
    | _, Some annot -> [Some (mk_mident annot), pty, code]
    | _, _ -> [Some (mk_mident (dumloc "default")), pty, code]
  in
  Option.fold (fun model p ->
      let l = process p in
      let ps = List.mapi (fun k (name, pty, code) ->
          let name = match name with | Some x -> x | None -> mk_mident (dumloc (Format.asprintf "entry_%i" k)) in
          Entry (mk_function_struct ~args:(process_arg pty) name code)
        ) l in
      { model with functions = ps }) model p *)

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
      | Minstrmatchor ({node = Mvar (id, _) }, lids, le, rids, re)
      | Mmatchor ({node = Mvar (id, _) }, lids, le, rids, re) when String.equal (unloc_mident id) param -> Some (lids, le, rids, re)
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
  let rec aux (entries : model_entries) (arg_ids : string list) (code : mterm) : function_node list =
    match entries with
    | Munion (l, r) -> begin
        let opt_param = match arg_ids with | [id] -> Some id | _ -> None in
        let param = Option.get opt_param in
        match seek_ifleft param code with
        | Some (lids, le, rids, re) -> (aux l (List.map unloc_mident lids) le) @ (aux r (List.map unloc_mident rids) re)
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
        let args, code =
          match arg_ids, pty.node with
          | _, T.Tunit -> [], code
          | [id], _ -> [(mk_mident (dumloc "arg"), Gen_decompile.ttype_to_mtype pty, None)], replace_var id "arg" code
          | _, _ -> assert false
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
  | Some code -> {model with functions = aux entries ["args_1"] code }
  | None -> model

let remove_operations_nil (model : model) : model =
  let rec aux ctx (mt : mterm) : mterm =
    match mt.node with
    | Massign(ValueAssign, _, Avar (_, {pldesc = "ops"}), { node = (Mlitlist []) }) -> seq []
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let optimize (model, env : model * Gen_decompile.env) =
  let model =
    model
    |> build_entries env
    |> remove_operations_nil
    |> flat_sequence
  in
  model, env
