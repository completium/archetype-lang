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
  transfer: bool;
  body: mterm;
}

let mk_s_interfun loop_id arg_id arg_type body =
  { loop_id; arg_id; arg_type; body }

let mk_ligo_fun ?(args=[]) ?ret ?(vars=[]) ?(iterfuns=[]) ?(transfer=false) name body : ligo_fun =
  { name; ret; args; vars; iterfuns; transfer; body }

type ligo_fun_accu = {
  funs : s_interfun list;
  vars : ident list;
}

let to_ligo_fun (model : model) (f : function__) : ligo_fun =
  let fs, ret =
    match f.node with
    | Function (fs, ret) -> fs, Some ret
    | Entry  fs -> fs, None
  in
  let name = unloc fs.name in
  let extract_side_var_ids mt (vars : (ident * type_) list) : ident list =
    let var_ids = List.map fst vars in
    let rec aux (refs, accu : string list * ident list) (mt : mterm) : string list * ident list =
      match mt.node with
      | Mvarlocal a when (List.mem (unloc a) var_ids) ->
        let str = unloc a in
        refs, str::accu
      | Massign (_, a, v) when (List.mem (unloc a) var_ids) ->
        let str = unloc a in
        (* List.iter (fun x -> Format.eprintf "refs: %s@\n" x) refs;
           Format.eprintf "str: %s@\n" str; *)
        let refs, accu = aux (refs, accu) v in
        refs, str::accu
      | _ -> fold_term aux (refs, accu) mt
    in
    let _, res = aux ([], []) mt in
    res
  in
  let seek_type body id : type_ =
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
  let body, accu =
    begin
      let rec aux (env : (ident * type_) list) (accu : ligo_fun_accu) (mt : mterm) : mterm * ligo_fun_accu =
        let g (x : mterm__node) : mterm = { mt with node = x; } in
        match mt.node with
        | Mletin ([id], init, Some type_, body, None) ->
          begin
            let ninit, accu = aux env accu init in
            let env = (unloc id, type_)::env in
            let nbody, accu = aux env accu body in
            let new_mt = mk_mterm (Mletin ([id], ninit, Some type_, nbody, None)) nbody.type_ in
            new_mt, accu
          end
        | Mfor(arg_id, c, body, Some label) ->
          begin
            let nbody, accu = aux env accu body in
            let vids = extract_side_var_ids nbody env in
            let app_id = dumloc "list_iter" in
            let fun_name = label in
            let n = mk_mterm (Mvarlocal (dumloc fun_name)) Tunit in
            let mtt : mterm = mk_mterm (Mapp (app_id, [c; n])) Tunit in
            let typ =
              begin
                match c.type_ with
                | Tcontainer (Tasset an, _) when is_get_body nbody (unloc arg_id) (unloc an) ->
                  begin
                    let _, t = Utils.get_asset_key model (unloc an) in
                    Tbuiltin t
                  end
                | Tcontainer (t, _) -> t
                | _ -> assert false
              end
            in
            let interfun = mk_s_interfun fun_name (unloc arg_id) typ nbody in
            let accu =
              { accu with
                funs = interfun::accu.funs;
                vars = List.fold_left (fun accu x -> if List.mem x accu then accu else x::accu) accu.vars vids}
            in
            mtt, accu
          end
        | _ -> fold_map_term g (aux env) accu mt
      in
      let accu : ligo_fun_accu = {
        funs = [];
        vars = [];
      } in
      aux [] accu fs.body
    end
  in
  let iterfuns = List.rev accu.funs in
  let vars : (ident * type_) list = List.map (fun x -> (x, (seek_type body x))) accu.vars in
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
  let transfer = Model.Utils.with_transfer_for_mterm body in
  let ligo_fun = mk_ligo_fun ~args:args ?ret:ret ~vars:vars ~iterfuns:iterfuns ~transfer:transfer name body in
  ligo_fun
