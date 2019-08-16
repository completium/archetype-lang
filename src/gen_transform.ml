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
