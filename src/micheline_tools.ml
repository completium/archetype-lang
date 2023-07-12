module PT = ParseTree
module A  = Ast
module M  = Model
module TZ = Michelson

type micheline = Micheline_printer.node

let emptyloc : Micheline_printer.location = {comment = None}

let mkstring  v = Micheline.String (emptyloc, v)
let mkint     v = Micheline.Int (emptyloc, v)
let mkbytes   v = Micheline.Bytes (emptyloc, v)
let mkprim    (p, args, annot) = Micheline.Prim (emptyloc, p, args, annot)
let mkseq nodes = Micheline.Seq (emptyloc, nodes)

let pt_to_micheline (pt : PT.micheline_t) : micheline =
  let rec aux (pt : PT.micheline_t) : micheline =
    match pt with
    | MIstring v -> mkstring v
    | MIint    v -> mkint v
    | MIbytes  v -> mkbytes (Hex.to_bytes (`Hex v))
    | MIprim   (p, args, annots) -> mkprim (p, List.map aux args, annots)
    | MIseq    vs -> mkseq (List.map aux vs)
  in
  aux pt

let pt_to_obj (pt : PT.micheline_t) : TZ.obj_micheline =
  let rec aux (pt : PT.micheline_t) : TZ.obj_micheline =
    match pt with
    | MIstring v -> Ostring v
    | MIint    v -> Oint (Big_int.string_of_big_int v)
    | MIbytes  v -> Obytes v
    | MIprim   (prim, args, annots) -> Oprim {prim; args = List.map aux args ; annots}
    | MIseq    vs -> Oarray (List.map aux vs)
  in
  aux pt

let obj_to_micheline (obj : TZ.obj_micheline) : micheline =
  let rec aux (obj : TZ.obj_micheline) : micheline =
    match obj with
    | Oprim p -> mkprim (p.prim, List.map aux p.args, p.annots)
    | Ostring v -> mkstring v
    | Obytes v -> mkbytes (Hex.to_bytes (`Hex v))
    | Oint v -> mkint (Big_int.big_int_of_string v)
    | Oarray v -> mkseq (List.map aux v)
    | Ovar v -> begin
        let f id = mkprim (id, [], []) in
        match v with
        | OMVfree id -> f id
        | OMVint (id, _) -> f id
        | OMVstring id -> f id
        | OMVbytes id -> f id
        | OMVif (id, _, _) -> f id
      end
  in
  aux obj

let obj_to_micheline_t (obj : TZ.obj_micheline) : PT.micheline_t =
  let rec aux (obj : TZ.obj_micheline) : PT.micheline_t =
    match obj with
    | Oprim p -> PT.MIprim (p.prim, List.map aux p.args, p.annots)
    | Ostring v -> PT.MIstring v
    | Obytes v -> PT.MIbytes v
    | Oint v ->  PT.MIint (Big_int.big_int_of_string v)
    | Oarray v -> PT.MIseq (List.map aux v)
    | Ovar v -> begin
        let f id = PT.MIprim (id, [], []) in
        match v with
        | OMVfree id -> f id
        | OMVint (id, _) -> f id
        | OMVstring id -> f id
        | OMVbytes id -> f id
        | OMVif (id, _, _) -> f id
       end
  in
  aux obj
