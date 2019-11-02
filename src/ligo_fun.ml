open Ident
open Location
open Model


type ligo_fun = {
  name: ident;
  ret:  type_ option;
  args: (ident * type_) list;
  vars: (ident * type_) list;
  funs: (ident * type_ option * mterm) list;
  body: mterm;
}

let mk_ligo_fun ?(args=[]) ?ret ?(vars=[]) ?(funs=[]) name body : ligo_fun =
  { name; ret; args; vars; funs; body }

let to_ligo_fun (f : function__) : ligo_fun =
  let fs, ret =
    match f.node with
    | Function (fs, ret) -> fs, Some ret
    | Entry  fs -> fs, None
  in
  let ligo_fun = mk_ligo_fun ?ret:ret (unloc fs.name) fs.body in
  ligo_fun
