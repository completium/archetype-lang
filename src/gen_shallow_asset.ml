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

(*let mk_type d = function
  | M.Tcontainer ((Tasset i), _) ->
    let rec rec_mk_type i acc =
      if compare i 0 = 0 then
        acc
      else
        rec_mk_type (i-1) (M.Tcontainer (acc, M.Collection))
    in
    rec_mk_type d (M.Tcontainer ((Tasset i), M.Collection))
  | _ as t -> t*)

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
  let colls = M.Utils.get_asset_partitions m i in
  List.fold_left (fun acc (i,t,dv) ->
      let id  = id ^ "_" ^ i in
      let arg = dumloc id, (*mk_type d*) t, dv in
      acc @ (gen_shallow_args m (succ d) id t [arg])
    ) acc colls

(* same as gen_shallow_args but returns context *)
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
    let k,kt = M.Utils.get_asset_key m i in
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
  | M.Mvarlocal _
  | M.Mvarparam _ ->
    let (key,typ) = M.Utils.get_asset_key m n in
    M.mk_mterm (M.Mdotasset (t,dumloc key)) (M.Tbuiltin typ)
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
        let typ  =  M.Utils.get_asset_key m n |> snd in
        let array = M.mk_mterm (M.Marray keys) (Tcontainer (Tbuiltin typ,Collection)) in
        (*let str = Format.asprintf "%a@." M.pp_mterm array in
            print_endline str;*)
        let mapped_vals = List.map (map_shallow_record m ctx) l in
        (* take head of mapped_vals *)
        let hds = List.map List.hd mapped_vals in
        let tls = List.flatten (List.map tl mapped_vals) in
        array :: ([M.mk_mterm (M.Mlisttocoll (unloc n,
                                              M.mk_mterm (M.Marray hds) t.type_)) t.type_] @ tls)
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

let rec map_shallow (ctx : (I.ident * (M.lident * M.type_) list) list) m (t : M.mterm) : M.mterm =
  let t_gen =
    match t.node with
    | M.Maddasset (n,a) when M.Utils.is_varlocal a ->
      let id = M.Utils.dest_varlocal a in
      if has_shallow_vars id ctx then
        let shallow_args = get_shallow_vars id ctx  in
        M.Mapp (dumloc ("add_shallow_"^n),shallow_args)
      else  M.Maddasset (n,a)
    | M.Maddasset (n,a) when M.Utils.is_record a ->
      if M.Utils.has_partition m n
      then
        let shallow_args = map_shallow_record m ctx a in
        M.Mapp (dumloc ("add_shallow_"^n),shallow_args)
      else
        M.Maddasset (n,a)
    | M.Maddfield (n,f,a,v) when M.Utils.is_varlocal v ->
      let id = M.Utils.dest_varlocal v in
      if has_shallow_vars id ctx then
        let shallow_args = get_shallow_vars id ctx in
        M.Mapp (dumloc ("add_shallow_"^n^"_"^f),[a] @ shallow_args)
      else M.Maddfield (n,f,a,v)
    | M.Maddfield (n,f,a,v) when M.Utils.is_record v ->
      let vt = M.Utils.get_asset_type v in
      if M.Utils.has_partition m (unloc vt) then
        let shallow_args = map_shallow_record m ctx v in
        M.Mapp (dumloc ("add_shallow_"^n^"_"^f),[a] @ shallow_args)
      else M.Maddfield (n,f,a,v)
    | M.Mletin ([id],v,t,b) when M.Utils.is_record v ->
      begin
        match v.type_ with
        | Tasset a when M.Utils.has_partition m (unloc a) ->
          let shallow_args = map_shallow_record m ctx v in
          let new_letins   = mk_new_letins (unloc id) (tl shallow_args) in
          let new_ctx      = mk_ctx ctx (unloc id) (tl shallow_args) in
          M.Mletin ([id],
                    List.hd shallow_args,
                    Some (Tasset a),
                    map_letin_shallow m new_ctx b new_letins)
        | _ -> M.Mletin ([id],v,t,map_shallow ctx m b)
      end
    | M.Mdotasset (e,i) ->
      let asset = M.Utils.get_asset_type e in
      let partitions = M.Utils.get_asset_partitions m (asset |> unloc) in
      begin
        if List.exists (fun (pi,pt,pd) ->
            compare (i |> unloc) pi = 0) partitions then
          let rec get_partition_type = function
            | (pi,pt,pd)::tl
              when compare (i |> unloc) pi = 0 -> pt
            | r::tl -> get_partition_type tl
            | [] -> assert false in
          let ty = get_partition_type partitions in
          let pa = M.Utils.dest_partition ty |> unloc in
          M.Munshallow (pa,M.mk_mterm (M.Mdotasset (map_shallow ctx m e, i)) t.M.type_)
        else
          M.Mdotasset (map_shallow ctx m e,i)
      end
    | _ as tn -> M.map_term_node ((map_shallow ctx) m) tn
  in
  M.mk_mterm ~loc:(t.loc) t_gen t.type_
and map_letin_shallow m ctx b = function
  | (id,v)::tl -> M.mk_mterm (M.Mletin ([id],v,None,map_letin_shallow m ctx b tl)) v.type_
  | [] -> map_shallow ctx m b

let process_shallow_function m f =
  let args = M.Utils.get_function_args f in
  (* mk initial context and shallowed arguments *)
  let (ctx,args) = List.fold_left (fun (ctx,acc) arg ->
      let (id,t,e) = arg in
      let (acc_ctx,shallow_args) = gen_shallow_args m id t arg in
      (ctx @ acc_ctx, acc @ shallow_args)
    ) ([],[]) args in
  let f = M.Utils.set_function_args f args in
  let f = M.Utils.map_function_terms (map_shallow ctx m) f in
  f

let rec gen_add_shallow_asset (arg : M.argument) : M.mterm =
  let tnode =
    match arg with
    | id, Tasset a,_ ->
      M.Maddasset (unloc a,
                   M.mk_mterm (M.Mvarlocal a) (Tcontainer (Tasset a, Collection)))
    | id,Tcontainer (Tasset a,_),_ ->
      M.Mfor (a,
              M.mk_mterm (M.Mvarlocal id) (Tcontainer (Tasset a,Collection)),
              gen_add_shallow_asset (a, Tasset a, None),
              None
             )
    | _,t,_ ->
      let str = Format.asprintf "%a@." M.pp_type_ t in
      print_endline str;
      M.Mbreak in
  M.mk_mterm tnode Tunit

let gen_add_shallow_fun (model : M.model) (n : I.ident) : M.function__ =
  let arg    = (dumloc n,M.Tasset (dumloc n),None) in
  let _,args = gen_shallow_args model (dumloc n) (Tasset (dumloc n)) arg in
  let body   =
    if List.length args > 1
    then M.mk_mterm (M.Mseq (List.map gen_add_shallow_asset args)) Tunit
    else gen_add_shallow_asset (List.hd args) in
  {
    node = Function ({
        name = dumloc ("add_shallow_"^n);
        args = args;
        body = body;
        loc = Location.dummy;
      },Tunit);
    verif = None;
  }

let gen_add_shallow_field_fun (model : M.model) (n,f : I.ident * I.ident) : M.function__ =
  let pa,k,kt = M.Utils.get_partition_asset_key model (dumloc n) (dumloc f) in
  let arg    = (dumloc "added_asset",M.Tasset (dumloc pa),None) in
  let _,asset_args = gen_shallow_args model (dumloc "added_asset") (Tasset (dumloc pa)) arg in
  let asset_arg = (dumloc "asset",M.Tasset (dumloc n),None) in
  let args = asset_arg::asset_args in
  let body   =
    M.mk_mterm (M.Mseq ([
        M.mk_mterm (M.Maddfield (
            n,
            f,
            M.mk_mterm (M.Mvarlocal (dumloc "asset")) (Tasset (dumloc n)),
            M.mk_mterm (M.Mvarlocal (dumloc "added_asset")) (Tasset (dumloc pa)))) Tunit;
      ] @ (List.map gen_add_shallow_asset (List.tl asset_args)))) Tunit in
  {
    node = Function ({
        name = dumloc ("add_shallow_"^n^"_"^f);
        args = args;
        body = body;
        loc = Location.dummy;
      },Tunit);
    verif = None;
  }

let get_added_assets (model : M.model) : I.ident list =
  let rec f ctx acc (t : M.mterm) =
    match t.node with
    | M.Maddasset (n,_) ->
      if List.mem n acc then
        acc
      else if M.Utils.has_partition model n then
        acc @ [n]
      else acc
    | _ -> M.fold_term (f ctx) acc t
  in
  M.fold_model f model []

let get_added_asset_fields (model : M.model) : (I.ident * I.ident) list =
  let rec f ctx acc (t : M.mterm) =
    match t.node with
    | M.Maddfield (n,fd,_,v) ->
      if List.mem (n,fd) acc then
        acc
      else
        let vt = M.Utils.get_asset_type v in
        if M.Utils.has_partition model (unloc vt) then
          acc @ [n,fd]
        else acc
    | _ -> M.fold_term (f ctx) acc t
  in
  M.fold_model f model []

let shallow_decls (model : M.model) decls : M.decl_node list =
  let shallow_storage_type = function
    | M.Tcontainer (Tasset a,_) ->
      let keyt =  M.Utils.get_asset_key model a |> snd in
      M.Tcontainer (Tbuiltin keyt,Collection)
    | _ as t -> t in
  List.map (fun (decl : M.decl_node) ->
      match decl with
      | M.Drecord r ->
        M.Drecord {
          r with
          values = List.map (fun (ri : M.record_item) ->
              { ri with type_ = shallow_storage_type ri.type_ } (* TODO : map default value *)
            ) r.values
        }
      | _ as d -> d
    ) decls

let shallow_asset (model : M.model) : M.model =
  let added_assets = get_added_assets model in
  let added_asset_fields = get_added_asset_fields model in
  let add_asset_functions = List.map (gen_add_shallow_fun model) added_assets in
  let add_asset_field_functions = List.map (gen_add_shallow_field_fun model) added_asset_fields in
  let shallowed_decls = shallow_decls model model.M.decls in
  {
    model with
    decls     = shallowed_decls;
    functions = add_asset_functions @
                add_asset_field_functions @
                (List.map (process_shallow_function model) model.functions)
  } |> Gen_api_storage.process_api_storage
