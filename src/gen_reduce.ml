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
let operations_var : mterm = mk_mterm (Mvarlocal operations_lident) (Tcontainer (Toperation, Collection))

let is_fail (t : mterm) (e : mterm option) : bool =
  match t.node , e with
  | Mfail _, None -> true
  | _ -> false

let rec process_mtern (mt : mterm) : mterm =
  match mt.node with
  (* api storage *)
  | Maddasset (an, arg, l) ->
    let arg = process_mtern arg in
    let l = List.map process_mtern l in
    mk_mterm (Maddasset (an, arg, l)) Tstorage
  | Maddfield (an, fn, col, arg, l) ->
    let col = process_mtern col in
    let arg = process_mtern arg in
    let l = List.map process_mtern l in
    mk_mterm (Maddfield (an, fn, col, arg, l)) Tstorage
  (* | Maddlocal     of 'term * 'term *)
  | Mremoveasset (an, arg) ->
    let arg = process_mtern arg in
    mk_mterm (Mremoveasset (an, arg)) Tstorage
  | Mremovefield (an, fn, col, arg) ->
    let col = process_mtern col in
    let arg = process_mtern arg in
    mk_mterm (Mremovefield (an, fn, col, arg)) Tstorage
  (* | Mremovelocal  of 'term * 'term *)
  | Mclearasset an ->
    mk_mterm (Mclearasset an) Tstorage
  | Mclearfield (an, fn, col) ->
    let col = process_mtern col in
    mk_mterm (Mclearfield (an, fn, col)) Tstorage
  (* | Mclearlocal   of 'term *)
  | Mreverseasset an ->
    mk_mterm (Mreverseasset an) Tstorage
  | Mreversefield (an, fn, col) ->
    let col = process_mtern col in
    mk_mterm (Mreversefield (an, fn, col)) Tstorage
  (* | Mreverselocal of 'term *)

  (* controls *)
  | Mif (_, t, e) when is_fail t e -> mt
  | Mif (c, t, e) ->
    let t = process_mtern t in
    let e = Option.map process_mtern e in

    let mif = mk_mterm (Mif (c, t, e)) Tstorage in
    let node = Mletin (storage_lident, mif, Some Tstorage, storage_var) in
    mk_mterm node Tstorage



  (* | Mseq l ->
     let node = List.fold_left (fun accu item -> accu) (Mseq []) l in
     mk_mterm node Tunit *)

  (* operation *)
  | Mtransfer _ -> { mt with type_ = Toperation }

  | _ -> map_term process_mtern mt



let process_functions (model : model) : model =
  let process_function__ (function__ : function__) : function__ =
    let process_function_node (function_node : function_node) : function_node =
      let process_fs (fs : function_struct) : function_struct =
        let args = fs.args in
        let body = process_mtern fs.body in
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
