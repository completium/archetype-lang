open Ident
open Location
open Model

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

let to_ligo_fun (f : function__) : ligo_fun =
  let fs, ret =
    match f.node with
    | Function (fs, ret) -> fs, Some ret
    | Entry  fs -> fs, None
  in
  let name = unloc fs.name in
  let vars =
    match name with
    | "consume" -> [("remainder", Tbuiltin Bint)]
    | _ -> []
  in
  let accu : s_interfun list = [] in
  let body, iterfuns =
    begin
      let rec aux (accu : s_interfun list) (mt : mterm) : mterm * s_interfun list =
        match mt.node with
        | Mfor(arg_id, c, body, Some label) ->
          let app_id = dumloc "list_iter" in
          let fun_name = label in
          let n = mk_mterm (Mvarlocal (dumloc fun_name)) Tunit in
          let mtt : mterm = mk_mterm (Mapp(app_id, [c; n])) Tunit in
          let interfun = mk_s_interfun fun_name (unloc arg_id) c.type_ body in
          mtt, interfun::accu
        | _ ->
          begin
            let g (x : mterm__node) : mterm = { mt with node = x; } in
            fold_map_term g aux accu mt
          end
      in
      aux accu fs.body
    end
  in
  let ligo_fun = mk_ligo_fun ?ret:ret ~vars:vars ~iterfuns:iterfuns name body in
  ligo_fun
