open Tools
open Location
(* open Ident *)
open Model

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

let storage_lident : lident = dumloc "_s"
let storage_var : mterm = mk_mterm (Mvarlocal storage_lident) Tstorage

let operations_lident : lident = dumloc "_ops"
let operations_type : type_ = Tcontainer (Toperation, Collection)
let operations_var : mterm = mk_mterm (Mvarlocal operations_lident) operations_type

let is_fail (t : mterm) (e : mterm option) : bool =
  match t.node , e with
  | Mfail _, None -> true
  | _ -> false

let rec process_mtern (s : s_red) (mt : mterm) : mterm * s_red =
  let fold_list x y : mterm list * s_red = fold_map_term_list process_mtern x y in
  match mt.node with
  (* api storage *)
  | Maddasset (an, arg, l) ->
    let arg, s = process_mtern s arg in
    let l, s = fold_list s l in
    mk_mterm (Maddasset (an, arg, l)) Tstorage, s
  | Maddfield (an, fn, col, arg, l) ->
    let col, s = process_mtern s col in
    let arg, s = process_mtern s arg in
    let l, s = fold_list s l in
    mk_mterm (Maddfield (an, fn, col, arg, l)) Tstorage, s
  (* | Maddlocal     of 'term * 'term *)
  | Mremoveasset (an, arg) ->
    let arg, s = process_mtern s arg in
    mk_mterm (Mremoveasset (an, arg)) Tstorage, s
  | Mremovefield (an, fn, col, arg) ->
    let col, s = process_mtern s col in
    let arg, s = process_mtern s arg in
    mk_mterm (Mremovefield (an, fn, col, arg)) Tstorage, s
  (* | Mremovelocal  of 'term * 'term *)
  | Mclearasset an ->
    mk_mterm (Mclearasset an) Tstorage, s
  | Mclearfield (an, fn, col) ->
    let col, s = process_mtern s col in
    mk_mterm (Mclearfield (an, fn, col)) Tstorage, s
  (* | Mclearlocal   of 'term *)
  | Mreverseasset an ->
    mk_mterm (Mreverseasset an) Tstorage, s
  | Mreversefield (an, fn, col) ->
    let col, s = process_mtern s col in
    mk_mterm (Mreversefield (an, fn, col)) Tstorage, s
  (* | Mreverselocal of 'term *)

  (* controls *)
  | Mif (_, t, e) when is_fail t e -> mt, s
  | Mif (c, t, e) ->
    let t, s = process_mtern s t in
    let e, s =
      begin
        match e with
        | Some v -> process_mtern s v |> (fun (x, y) -> Some x, y)
        | None -> None, s
      end
    in
    let mif = mk_mterm (Mif (c, t, e)) Tstorage in
    let node = Mletin (storage_lident, mif, Some Tstorage, storage_var) in
    mk_mterm node Tunit, s



  (* | Mseq l ->
     let node = List.fold_left (fun accu item -> accu) (Mseq []) l in
     mk_mterm node Tunit *)

  (* operation *)
  | Mtransfer _ -> { mt with type_ = Toperation }, {s with with_ops = true}

  | _ ->
    let g (x : mterm__node) : mterm = { mt with node = x; } in
    fold_map_term g process_mtern s mt


let process_body (mt : mterm) : mterm =
  let s : s_red = {
    with_ops = true;
  } in
  let mt, s = process_mtern s mt in
  if s.with_ops
  then
    let init = mk_mterm (Marray []) operations_type in
    mk_mterm (Mletin (operations_lident, init, Some operations_type, mt)) Tunit
  else mt

let process_functions (model : model) : model =
  let process_function__ (function__ : function__) : function__ =
    let process_function_node (function_node : function_node) : function_node =
      let process_fs (fs : function_struct) : function_struct =
        let args = fs.args in
        let body = process_body fs.body in
        {
          fs with
          args = args;
          body = body;
        }
      in

      match function_node with
      | Function (fs, t) ->
        let fs = process_fs fs in
        Function (fs, t)
      | Entry fs ->
        let fs = process_fs fs in
        Entry fs
    in
    {
      function__ with
      node = process_function_node function__.node;
    }
  in
  {
    model with
    functions = List.map (fun f -> process_function__ f) model.functions
  }


let reduce (model : model) : model =
  model
  |> process_functions
