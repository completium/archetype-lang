open Tools
open Location
open Ident
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
let operations_init : mterm = mk_mterm (Marray []) operations_type

let operations_storage_type : type_ = Ttuple [Tcontainer (Toperation, Collection); Tstorage]
let operations_storage_var : mterm = mk_mterm (Mtuple [operations_var; storage_var]) operations_storage_type

let is_fail (t : mterm) (e : mterm option) : bool =
  match t.node , e with
  | Mfail _, None -> true
  | _ -> false

type s_red = {
  with_ops : bool;
}

type ctx_red = {
  local_fun_types : (ident * type_) list;
}

let rec process_mtern (ctx : ctx_red) (s : s_red) (mt : mterm) : mterm * s_red =
  let fold_list x y : mterm list * s_red = fold_map_term_list (process_mtern ctx) x y in

  let process_non_empty_list_term (s : s_red) (mts : mterm list) : mterm * s_red =

    let split_last_list l : 'a * 'a list =
      let rec split_last_list_rec accu l =
        match l with
        | [] -> assert false
        | t::[] -> (t, (List.rev accu))
        | t::l -> split_last_list_rec (t::accu) l
      in
      split_last_list_rec [] l
    in
    let last, list = split_last_list mts in

    let last, s = process_mtern ctx s last in

    List.fold_right (fun x (accu, s) ->
        let x, s = process_mtern ctx s x in
        match x with
        | {type_ = Tstorage; _} ->
          mk_mterm (Mletin ([storage_lident], x, Some Tstorage, accu)) Tunit, s
        | {type_ = Tcontainer (Toperation, Collection); _} ->
          mk_mterm (Mletin ([operations_lident], x, Some operations_type, accu)) Tunit, s
        | {type_ = Ttuple [Tcontainer (Toperation, Collection); Tstorage]; _} ->
          mk_mterm (Mletin ([operations_lident; storage_lident], x, Some operations_storage_type, accu)) Tunit, s
        | _ ->
          mk_mterm (Mseq [x ; accu]) accu.type_, s
      ) list (last, s)
  in

  let get_type (ctx : ctx_red) (id : ident) : type_ =
    List.assoc id ctx.local_fun_types
  in

  match mt.node with
  (* api storage *)
  | Maddasset (an, arg, l) ->
    let arg, s = process_mtern ctx s arg in
    let l, s = fold_list s l in
    mk_mterm (Maddasset (an, arg, l)) Tstorage, s
  | Maddfield (an, fn, col, arg, l) ->
    let col, s = process_mtern ctx s col in
    let arg, s = process_mtern ctx s arg in
    let l, s = fold_list s l in
    mk_mterm (Maddfield (an, fn, col, arg, l)) Tstorage, s
  (* | Maddlocal     of 'term * 'term *)
  | Mremoveasset (an, arg) ->
    let arg, s = process_mtern ctx s arg in
    mk_mterm (Mremoveasset (an, arg)) Tstorage, s
  | Mremovefield (an, fn, col, arg) ->
    let col, s = process_mtern ctx s col in
    let arg, s = process_mtern ctx s arg in
    mk_mterm (Mremovefield (an, fn, col, arg)) Tstorage, s
  (* | Mremovelocal  of 'term * 'term *)
  | Mclearasset an ->
    mk_mterm (Mclearasset an) Tstorage, s
  | Mclearfield (an, fn, col) ->
    let col, s = process_mtern ctx s col in
    mk_mterm (Mclearfield (an, fn, col)) Tstorage, s
  (* | Mclearlocal   of 'term *)
  | Mreverseasset an ->
    mk_mterm (Mreverseasset an) Tstorage, s
  | Mreversefield (an, fn, col) ->
    let col, s = process_mtern ctx s col in
    mk_mterm (Mreversefield (an, fn, col)) Tstorage, s
  (* | Mreverselocal of 'term *)

  (* app local *)
  | Mapp (id, args) ->
    let type_ = get_type ctx (unloc id) in
    let args =
      begin
        match type_ with
        | Tstorage ->
          let args = storage_var::args in
          args
        | Tcontainer (Toperation, Collection) ->
          let args = operations_var::args in
          args
        | Ttuple [Tcontainer (Toperation, Collection); Tstorage] ->
          let args = storage_var::operations_var::args in
          args
        | _ -> args
      end
    in
    {
      mt with
      node = Mapp (id, args);
      type_ = type_;
    }, s

  (* controls *)
  | Mif (_, t, e) when is_fail t e -> mt, s
  | Mif (c, t, e) ->
    let t, s = process_mtern ctx s t in
    let e, s =
      begin
        match e with
        | Some v -> process_mtern ctx s v |> (fun (x, y) -> Some x, y)
        | None -> None, s
      end
    in
    mk_mterm (Mif (c, t, e)) Tstorage, s

  (* let node = Mletin ([storage_lident], mif, Some Tstorage, storage_var) in
     mk_mterm node Tunit, s *)



  | Mseq l ->
    begin
      match l with
      | [] -> mt, s
      | i::[] ->
        process_mtern ctx s i
      | _ -> process_non_empty_list_term s l
    end

  | Mfor (a, col, body) ->
    let col, s = process_mtern ctx s col in
    let body, s = process_mtern ctx s body in
    let is = [storage_lident] in
    mk_mterm (Mfold (a, is, col, body)) Tstorage, s

  (* let node = List.fold_left (fun accu item -> accu) (Mseq []) l in
     mk_mterm node Tunit, s *)

  (* operation *)
  | Mtransfer _ ->
    let ops =  { mt with type_ = Toperation } in
    let node = Mapp (dumloc "add_list", [operations_var; ops]) in
    let mt = mk_mterm node operations_storage_type in
    mt, {s with with_ops = true}

  | _ ->
    let g (x : mterm__node) : mterm = { mt with node = x; } in
    fold_map_term g (process_mtern ctx) s mt


let process_body (ctx : ctx_red) (mt : mterm) : mterm =
  let s : s_red = {
    with_ops = false;
  } in
  let mt, _s = process_mtern ctx s mt in
  mt

let analyse_type (mt : mterm) : type_ = Tstorage

let merge_seq (mt1 : mterm) (mt2 : mterm) (t : type_) : mterm =
  match mt1.node, mt2.node with
  | Mseq l1, Mseq l2 -> mk_mterm (Mseq (l1 @ l2)) t
  | _, Mseq l -> mk_mterm (Mseq ([mt1] @ l)) t
  | Mseq l, _ -> mk_mterm (Mseq (l @ [mt2])) t
  | _ -> mk_mterm (Mseq [mt1; mt2]) t

let process_functions (model : model) : model =
  let process_functions l =
    let process_function__ (ctx : ctx_red) (function__ : function__) : function__ * ctx_red =
      let process_function_node (function_node : function_node) : function_node * ctx_red =
        match function_node with
        | Function (fs, t) ->
          begin
            match t with
            | Tunit ->
              let ret : type_  = analyse_type fs.body in
              let args, fun_body =
                begin
                  match ret with
                  | Tstorage ->
                    let seq = merge_seq fs.body storage_var Tstorage in
                    let arg : argument = (storage_lident, Tstorage, None) in
                    let args = arg::fs.args in
                    args, seq
                  | Tcontainer (Toperation, Collection) ->
                    let seq = merge_seq fs.body operations_var operations_type in
                    let arg : argument = (operations_lident, operations_type, None) in
                    let args = arg::fs.args in
                    args, seq
                  | Ttuple [Tcontainer (Toperation, Collection); Tstorage] ->
                    let seq = merge_seq fs.body operations_storage_var operations_storage_type in
                    let arg_s_ : argument = (storage_lident, Tstorage, None) in
                    let arg_ops_ : argument = (operations_lident, operations_type, None) in
                    let args = arg_s_::arg_ops_::fs.args  in
                    args, seq
                  | _ -> assert false
                end in
              let body = process_body ctx fun_body in
              let fs = {
                fs with
                args = args;
                body = body;
              } in
              let ctx : ctx_red = {
                ctx with
                local_fun_types = (unloc fs.name, ret)::ctx.local_fun_types;
              } in
              Function (fs, ret), ctx
            | _ -> assert false
          end
        | Entry fs ->
          let ret = mk_mterm (Mtuple [operations_var; storage_var]) operations_storage_type in
          let seq = mk_mterm (Mseq [fs.body; ret]) operations_storage_type in
          let entry_body = mk_mterm (Mletin ([operations_lident], operations_init, Some operations_type, seq)) Tunit in
          let body = process_body ctx entry_body in
          let fs = {
            fs with
            body = body;
          } in
          Entry fs, ctx
      in
      let node, ctx = process_function_node function__.node in
      {
        function__ with
        node = node;
      }, ctx
    in
    let ctx : ctx_red = {
      local_fun_types = [];
    } in
    let res, _ = List.fold_left (fun (l, ctx) f ->
        let item, ctx =  process_function__ ctx f in
        (l @ [item], ctx)
      ) ([], ctx) l in
    res
  in
  {
    model with
    functions = process_functions model.functions;
  }


let reduce (model : model) : model =
  model
  |> process_functions
