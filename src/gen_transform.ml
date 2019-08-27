open Location
open Model
open Tools

let reduce_forall (model : model) : model =
  let rec type_to_coll t : mterm =
    match t with
    | Tasset i -> mk_mterm (Mvarstorecol i) (Tcontainer (Tasset i,Collection))
    | Tcontainer (Tasset i,Collection) -> mk_mterm (Mvarstorecol i) t
    | Tvset (VSadded, typ) ->
      let coll = type_to_coll typ in
      mk_mterm (Msetadded coll) coll.type_
    | Tvset (VSremoved, typ) ->
      let coll = type_to_coll typ in
      mk_mterm (Msetremoved coll) coll.type_
    | _ -> assert false
  in
  let rec aux (ctx : ctx_model) (t : mterm) : mterm =
    match t.node with
    | Mforall (i, typ, b) ->
      let coll = type_to_coll typ in
      let asset = Utils.get_asset_type coll in
      mk_mterm (Mforall (i,
                         Tasset asset,
                         mk_mterm (Mimply (
                             mk_mterm (Mmem (unloc asset,
                                             mk_mterm (Mvarlocal i) (Tasset asset),
                                             coll))
                               Tunit,
                             b)
                           ) t.type_)) t.type_
    | _ -> map_mterm (aux ctx) t
  in
  map_mterm_model_formula aux model


let remove_label (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Mlabel _ -> mk_mterm (Mseq []) Tunit
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let flat_sequence (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Mseq l ->
      begin
        match l with
        | [] -> mt
        | [e] -> e
        | l ->
          let l = List.fold_right (fun (x : mterm) accu ->
              match x.node with
              | Mseq [] -> accu
              | _ -> x::accu) l [] in
          begin
            match l with
            | [] -> mk_mterm (Mseq []) Tunit
            | [e] -> e
            | _ -> mk_mterm (Mseq l) (List.last l).type_
          end
      end
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

let replace_lit_address_by_role (model : model) : model =
  let rec aux (ctx : ctx_model) (mt : mterm) : mterm =
    match mt.node with
    | Maddress _ as node -> mk_mterm node (Tbuiltin Brole)
    | _ -> map_mterm (aux ctx) mt
  in
  map_mterm_model aux model

(* transforms vars "to_iter" and "itereated" to M.toiterated and M.iterated
   with iterated collection as argument
   it works if loop lables are unique over the contract
 *)
let extend_loop_iter (model : model) : model =
  let get_for_collections () =
    let rec internal_get_for (ctx : ctx_model) acc (t : mterm) =
      match t.node with
      | Mfor (_, c, _, Some id) -> acc @ [id,c]
      | _ -> fold_term (internal_get_for ctx) acc t
    in
    fold_model internal_get_for model [] in
  let for_colls = get_for_collections () in
  let map_invariant_iter () =
    let rec internal_map_inv_iter (ctx : ctx_model) (t : mterm) : mterm =
      let mk_term const =
        let loop_id = Tools.Option.get ctx.invariant_id |> unloc in
        if List.mem_assoc loop_id for_colls then
          let coll = List.assoc loop_id for_colls in
          match const with
          | `Toiterate -> mk_mterm (Msettoiterate coll) (coll.type_)
          | `Iterated ->  mk_mterm (Msetiterated coll) (coll.type_)
        else
          t in
      match t.node with
      | Mvarlocal v when cmp_lident v (dumloc "to_iter") -> mk_term `Toiterate
      | Mvarlocal v when cmp_lident v (dumloc "iterated") -> mk_term `Iterated
      | _ -> map_mterm (internal_map_inv_iter ctx) t in
    map_mterm_model internal_map_inv_iter model in
  map_invariant_iter ()
