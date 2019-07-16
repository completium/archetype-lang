(* open Tools
   open Location
   open Ident *)

open Location

module M = Model
module I = Ident

exception Anomaly of string
type error_desc =
  | UnsupportedContainer of string
  | UnsupportedType of string
  | RecordNotFound
  | NotSupportedType of string
[@@deriving show {with_path = false}]

let emit_error (desc : error_desc) =
  let str = Format.asprintf "%a@." pp_error_desc desc in
  raise (Anomaly str)

let mk_type d = function
  | M.Tcontainer ((Tasset i), _) ->
    let rec rec_mk_type i acc =
      if compare i 0 = 0 then
        acc
      else
        rec_mk_type (i-1) (M.Tcontainer (acc, M.Collection))
    in
    rec_mk_type d (M.Tcontainer ((Tasset i), M.Collection))
  | _ as t -> t

let rec gen_shallow_args (m : M.model) d (id : I.ident) (t : M.type_) (acc : M.argument list)
  : M.argument list =
  match t with
  | M.Tasset i                 when M.Utils.has_partition m (unloc i) ->
    extract_asset_collection m d id (unloc i) acc
  | M.Tcontainer ((Tasset i), _) when M.Utils.has_partition m (unloc i) ->
    extract_asset_collection m d id (unloc i) acc
  | _ ->
    (*let str = Format.asprintf "%a@." M.pp_type_ t in
        print_endline str;*)
    acc
and extract_asset_collection m d id i acc =
  let colls = M.Utils.get_record_partitions m i in
  List.fold_left (fun acc (coll : M.record_item) ->
      let id  = id ^ "_" ^ (unloc coll.name) in
      let arg = dumloc id, mk_type d coll.type_, coll.default in
      acc @ (gen_shallow_args m (succ d) id coll.type_ [arg])
    ) acc colls

let gen_shallow_args (m : M.model) (id : M.lident) (t : M.type_) (arg : M.argument)
  : ((I.ident * (M.lident * M.type_) list) list) * M.argument list =
  match t with
  | M.Tasset _ ->
    let shallow_args = gen_shallow_args m 0 (unloc id) t [arg] in
    let acc_ctx =
      if List.length shallow_args > 1 then
        [unloc id,List.map (fun (i,t,_) -> (i,t)) (List.tl shallow_args)]
      else
        [] in
    (acc_ctx,shallow_args)
  | M.Tcontainer (Tasset i, Collection) ->
    let k,kt = M.Utils.get_record_key m i in
    let arg = (id,M.Tcontainer (M.Tbuiltin kt,Collection),None) in
    let arg_values = (dumloc ((unloc id)^"_values"),M.Tcontainer (Tasset i, Collection),None) in
    let shallow_args = gen_shallow_args m 1 (unloc id) t [arg;arg_values] in
    let acc_ctx =
      if List.length shallow_args > 1 then
        [unloc id,List.map (fun (i,t,_) -> (i,t)) (List.tl shallow_args)]
      else
        [] in
    (acc_ctx,shallow_args)
  | _ -> ([],[arg])

let has_shallow_vars id = List.mem_assoc (unloc id)

let get_shallow_vars id (ctx : (I.ident * (M.lident * M.type_) list) list) : M.mterm list =
  List.assoc (unloc id) ctx |> List.map (fun (i,t) ->
      M.mk_mterm (M.Mvarlocal i) t
    )

let record_to_key m n (t : M.mterm) =
  let key_pos = M.Utils.get_key_pos m n in
  match t.node with
  | M.Mrecord l ->
    let key = List.nth l key_pos in
    key
  | M.Mvarlocal _ ->
    let (key,typ) = M.Utils.get_record_key m n in
    M.mk_mterm (M.Mdotasset (t,key)) (M.Tbuiltin typ)
  | _ -> assert false

let tl = function
  | [] -> []
  | _ as l -> List.tl l

let rec map_shallow_record m ctx (t : M.mterm) : M.mterm list =
  match t.node with
  | M.Mrecord l ->
    let fields,mapped =
      List.fold_left (fun (fields,acc) f ->
          let mapped_vals = map_shallow_record m ctx f in

          (fields @ [List.hd mapped_vals],acc @ (List.tl mapped_vals))
        ) ([],[]) l
    in
    (M.mk_mterm (M.Mrecord fields) t.type_) :: mapped
  | M.Mvarlocal id when M.Utils.is_container t.type_ ->
    t :: (get_shallow_vars id ctx)
  | M.Marray l ->
    begin
      match t.type_ with
      | Tcontainer (Tasset n, _) ->
        (* split array in collection of keys and collection of shallow assets *)
        (* each element of l is an asset : each asset must be transmuted to the key *)
        let keys = List.map (record_to_key m n) l in
        let array = M.mk_mterm (M.Marray keys) t.type_ in (* TODO : make real type *)
        (*let str = Format.asprintf "%a@." M.pp_mterm array in
            print_endline str;*)
        let mapped_vals = List.map (map_shallow_record m ctx) l in
        (* take head of mapped_vals *)
        let hds = List.map List.hd mapped_vals in
        let tls = List.flatten (List.map tl mapped_vals) in
        array :: ([M.mk_mterm (M.Marray hds) t.type_] @ tls)
        | _ -> [t]
    end
  | _ -> [t]

(* returns a list of pairs of 'id, value' to declare oas letin *)
let mk_new_letins prefix shallow_vals =
  let _,letins = List.fold_left (fun (i,acc) v ->
      if M.Utils.is_varlocal v
      then (i,acc)
      else (succ i,acc @ [dumloc (prefix^"_"^(string_of_int (succ i))),v])
    ) (0,[]) shallow_vals in
  letins

(* make context data for id from map_shallow_record values *)
let mk_ctx ctx id shallow_vals =
  let (_,idctx) = List.fold_left (fun (i,acc) v ->
      if M.Utils.is_varlocal v
      then
        let vid = M.Utils.dest_varlocal v in
        (i, acc @ [vid,v.type_])
      else
        (succ i, acc @ [dumloc (id^"_"^(string_of_int (succ i))),v.type_])
    ) (0,[]) shallow_vals
  in
  ctx @ [id,idctx]

let rec map_shallow m (ctx : (I.ident * (M.lident * M.type_) list) list) (t : M.mterm) : M.mterm =
  let t_gen =
    match t.node with
    | M.Maddasset (n,e,a,l) when M.Utils.is_varlocal a ->
      let id = M.Utils.dest_varlocal a in
      if has_shallow_vars id ctx then
        let shallow_vars = get_shallow_vars id ctx  in
        M.Maddasset (n,e,a,l @ shallow_vars)
        else  M.Maddasset (n,e,a,l)
    | M.Maddasset (n,e,a,l) when M.Utils.is_record a ->
      if M.Utils.has_partition m n
      then
        let shallow_args = map_shallow_record m ctx a in
        M.Maddasset (n,e,List.hd shallow_args,l @ (tl shallow_args))
      else
        M.Maddasset (n,e,a,l)
    | M.Mletin (id,v,t,b) when M.Utils.is_record v ->
      begin
        match v.type_ with
        | Tasset a when M.Utils.has_partition m (unloc a) ->
          let shallow_args = map_shallow_record m ctx v in
          let new_letins   = mk_new_letins (unloc id) (tl shallow_args) in
          let new_ctx      = mk_ctx ctx (unloc id) (tl shallow_args) in
          M.Mletin (id,
                    List.hd shallow_args,
                    Some (Tasset a),
                    map_letin_shallow m new_ctx b new_letins)
        | _ -> M.Mletin (id,v,t,map_shallow m ctx b)
      end
    | _ as tn -> M.map_term_node ctx (map_shallow m) tn
  in
  M.mk_mterm ~loc:(t.loc) t_gen t.type_
and map_letin_shallow m ctx b = function
  | (id,v)::tl -> M.mk_mterm (M.Mletin (id,v,None,map_letin_shallow m ctx b tl)) v.type_
  | [] -> map_shallow m ctx b

let process_shallow_function m f =
  let args = M.Utils.get_function_args f in
  (* mk initial context and shallowed arguments *)
  let (ctx,args) = List.fold_left (fun (ctx,acc) arg ->
      let (id,t,e) = arg in
      let (acc_ctx,shallow_args) = gen_shallow_args m id t arg in
      (ctx @ acc_ctx, acc @ shallow_args)
    ) ([],[]) args in
  let f = M.Utils.set_function_args f args in
  let f = M.Utils.map_function_terms (map_shallow m ctx) f in
  f

let shallow_asset (model : M.model) : M.model = {
  model with
  functions = List.map (process_shallow_function model) model.functions
}
