open Ident
open Location
open Model
open Tools

type s_interfun = {
  loop_id : ident;
  arg_id: ident;
  arg_type: type_;
  body: mterm;
}
type ligo_fun = {
  name: ident;
  ret:  type_ option;
  args: (ident * type_) list;
  vars: (ident * type_) list;
  iterfuns: s_interfun list;
  body: mterm;
}

let mk_s_interfun loop_id arg_id arg_type body =
  { loop_id; arg_id; arg_type; body }

let mk_ligo_fun ?(args=[]) ?ret ?(vars=[]) ?(iterfuns=[]) name body : ligo_fun =
  { name; ret; args; vars; iterfuns; body }

let to_ligo_fun (model : model) (f : function__) : ligo_fun =
  let fs, ret =
    match f.node with
    | Function (fs, ret) -> fs, Some ret
    | Entry  fs -> fs, None
  in
  let name = unloc fs.name in
  let accu : s_interfun list = [] in
  let extract_side_var_ids mt :Ident.ident list =
    let rec aux (refs, accu : string list * Ident.ident list) (mt : mterm) : string list * Ident.ident list =
      match mt.node with
      | Mletin (ids, init, _, body, _) ->
        begin
          let refs, accu = fold_term aux (refs, accu) init in
          let refs = (List.map unloc ids) @ refs in
          fold_term aux (refs, accu) body
        end
      | Massign (_, a, v) when not (List.mem (unloc a) refs) ->
        let str = unloc a in
        (* List.iter (fun x -> Format.eprintf "refs: %s@\n" x) refs;
           Format.eprintf "str: %s@\n" str; *)
        let refs, accu = fold_term aux (refs, accu) v in
        refs, str::accu
      | _ -> fold_term aux (refs, accu) mt
    in
    let _, res = aux ([], []) mt in
    res
  in
  let var_ids = ref [] in
  let add_vars = List.iter (fun x -> if List.mem x !var_ids then var_ids := x::!var_ids) in
  let _seek_type body id : type_ =
    let rec aux accu (mt : mterm) : type_ option =
      match mt.node with
      | Mletin ([{pldesc = i; _}], _, t, _, _) when String.equal i id -> t
      | _ -> fold_term aux accu mt
    in
    match aux None body with
    | Some a -> a
    | None -> assert false
  in
  let is_get_body (mt : mterm) (id : ident) (asset_name : ident) =
    match mt.node with
    | Mletin ([{pldesc = i; _}], {node = Mget (an, _)}, _, _, _) -> String.equal i id && String.equal asset_name an
    | _ -> false
  in
  let body, iterfuns =
    begin
      let rec aux (accu : s_interfun list) (mt : mterm) : mterm * s_interfun list =
        let g (x : mterm__node) : mterm = { mt with node = x; } in
        match mt.node with
        | Mfor(arg_id, c, body, Some label) ->
          begin
            let nbody, accu = fold_map_term g aux accu body in
            let vids = extract_side_var_ids nbody in
            (match name with
             | "consume" ->
               List.iter (fun x -> Format.eprintf "v: %s@\n" x) vids
             | _ -> ());
            add_vars vids;
            (match name with
             | "consume" ->
               List.iter (fun x -> Format.eprintf "@\nvar_id: %s@\n" x) !var_ids
             | _ -> ());
            let app_id = dumloc "list_iter" in
            let fun_name = label in
            let n = mk_mterm (Mvarlocal (dumloc fun_name)) Tunit in
            let mtt : mterm = mk_mterm (Mapp (app_id, [c; n])) Tunit in
            let typ =
              begin
                match c.type_ with
                | Tcontainer (Tasset an, _) when is_get_body nbody (unloc arg_id) (unloc an) ->
                  begin
                    let _, t = Utils.get_asset_key model an in
                    Tbuiltin t
                  end
                | Tcontainer (t, _) -> t
                | _ -> assert false
              end
            in
            let interfun = mk_s_interfun fun_name (unloc arg_id) typ nbody in
            mtt, interfun::accu
          end
        | _ -> fold_map_term g aux accu mt
      in
      aux accu fs.body
    end
  in
  let iterfuns = List.rev iterfuns in
  (* let vars : (ident * type_) list = List.map (fun x -> (x, (seek_type body x))) !var_ids in *)
  let vars =
    match name with
    | "consume" -> [("remainder", Tbuiltin Bint); ("ow", Tasset (dumloc "owner"))]
    | _ -> []
  in
  let body =
    let rec aux (mt : mterm) : mterm =
      match mt.node with
      | Mletin ([i], v, t, body, _) when List.fold_left (fun accu (x, _) -> accu || String.equal (unloc i) x) false vars ->
        let nbody = aux body in
        let r = mk_mterm (Massign (ValueAssign, i, v)) (Option.get t) in
        merge_seq r nbody
      | _ -> map_mterm aux mt
    in
    aux body |> Gen_transform.flat_sequence_mterm
  in
  let args =
    match f.node with
    | Function (fs, _) -> List.map (fun (x, y, _) -> unloc x, y) fs.args
    | _ -> []
  in
  let ligo_fun = mk_ligo_fun ~args:args ?ret:ret ~vars:vars ~iterfuns:iterfuns name body in
  ligo_fun
