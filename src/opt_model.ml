open Model
open Gen_transform
open Tools
open Location

let build_entries (model : model) : model =
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
      { model with functions = ps }) model p

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
    |> build_entries
    |> remove_operations_nil
    |> flat_sequence
  in
  model, env
