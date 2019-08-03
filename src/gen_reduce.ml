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
  subs: (ident * type_) list;
}

type ctx_red = {
  local_fun_types : (ident * type_) list;
  vars: (ident * type_) list;
  target: mterm option;
}

let merge_seq (mt1 : mterm) (mt2 : mterm) : mterm =
  match mt1.node, mt2.node with
  | Mseq l1, Mseq l2 -> mk_mterm (Mseq (l1 @ l2)) mt2.type_
  | _, Mseq l -> mk_mterm (Mseq ([mt1] @ l)) mt2.type_
  | Mseq l, _ -> mk_mterm (Mseq (l @ [mt2])) mt2.type_
  | _ -> mk_mterm (Mseq [mt1; mt2]) mt2.type_

let rec compute_side_effect_aux (ctx : ctx_red)(accu : (ident * type_) list) (mt : mterm) : (ident * type_) list =
  match mt.node with
  | Massign (_, a, b) ->
    let id : ident = unloc a in
    let type_ = List.assoc_opt id ctx.vars in
    (match type_ with
     | Some v ->
       if List.mem (id, v) accu
       then accu
       else (id, v)::accu
     | None -> accu)
  | _ -> fold_term (compute_side_effect_aux ctx) accu mt

let compute_side_effect (ctx : ctx_red) (mt : mterm) =
  compute_side_effect_aux ctx [] mt

let compute_side_effect_for_list (ctx : ctx_red) (l : mterm list) : (ident * type_) list =
  List.fold_left (fun accu x ->
      compute_side_effect_aux ctx accu x
    ) [] l


let rec process_non_empty_list_term (ctx : ctx_red) (s : s_red) (mts : mterm list) : mterm * s_red =
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
      | { node = Massign (op, id, value); _} ->
        mk_mterm (Mletin ([id], value, Some value.type_, accu)) accu.type_, s
      | {type_ = Ttuple (Tcontainer(Toperation, Collection)::Tstorage::l); _} ->
        mk_mterm (Mletin ([storage_lident; operations_lident] @ (List.fold_left (fun accu x -> (dumloc "_")::accu) [] l), x, Some x.type_, accu)) x.type_, s
      | {type_ = Ttuple (Tstorage::l); _} ->
        let aaa : lident list = (List.mapi (fun i x ->
            if i < List.length s.subs
            then dumloc (List.nth s.subs i |> fst)
            else (dumloc "_")) l) in
        mk_mterm (Mletin ([storage_lident] @ aaa, x, Some x.type_, accu)) x.type_, s
      | {type_ = Ttuple (Tcontainer(Toperation, Collection)::l); _} ->
        mk_mterm (Mletin ([operations_lident] @ (List.fold_left (fun accu x -> (dumloc "_")::accu) [] l), x, Some x.type_, accu)) x.type_, s
      | {type_ = Tstorage; _} ->
        mk_mterm (Mletin ([storage_lident], x, Some x.type_, accu)) x.type_, s
      | {type_ = Tcontainer (Toperation, Collection); _} ->
        mk_mterm (Mletin ([operations_lident], x, Some x.type_, accu)) x.type_, s
      | _ ->
        (* Format.eprintf "not_found: %a@\n" pp_mterm x; *)
        merge_seq x accu, s
    ) list (last, s)

and process_mtern (ctx : ctx_red) (s : s_red) (mt : mterm) : mterm * s_red =
  (* let fold_list x y : mterm list * s_red = fold_map_term_list (process_mtern ctx) x y in *)

  let get_type (ctx : ctx_red) (id : ident) : type_ =
    List.assoc id ctx.local_fun_types
  in

  match mt.node with
  (* api storage *)
  | Mset (an, col, value) ->
    let col, s = process_mtern ctx s col in
    let value, s = process_mtern ctx s value in
    mk_mterm (Mset (an, col, value)) Tstorage, s
  | Maddasset (an, arg) ->
    let arg, s = process_mtern ctx s arg in
    mk_mterm (Maddasset (an, arg)) Tstorage, s
  | Maddfield (an, fn, col, arg) ->
    let col, s = process_mtern ctx s col in
    let arg, s = process_mtern ctx s arg in
    mk_mterm (Maddfield (an, fn, col, arg)) Tstorage, s
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

  (* lambda calculus *)
  | Mletin (ids, init, t, body) ->
    let extract_vars (ids : lident list) (t : type_ option) : (ident * type_) list =
      begin
        match ids, t with
        | ids, Some Ttuple tl -> List.map2 (fun x y -> (unloc x, y)) ids tl
        | [id], Some t -> [unloc id, t]
        | _ -> []
      end
    in
    let ctx = {
      ctx with
      vars = ctx.vars @ extract_vars ids t;
    } in
    let init, s = process_mtern ctx s init in
    let body, s = process_mtern ctx s body in
    mk_mterm (Mletin (ids, init, t, body)) body.type_, s

  (* controls *)
  | Mif (c, t, e) when is_fail t e ->
    let c, s = process_mtern ctx s c in
    mk_mterm (Mif (c, t, None)) Tunit, s
  | Mif (c, t, e) ->
    let c, s = process_mtern ctx s c in
    let target, subs =
      (match ctx.target with
       | Some {node = (Mtuple l); _} ->
         let subs : (ident * type_) list = compute_side_effect_for_list ctx ([t] @ (Option.map_dfl (fun x -> [x]) [] e)) in
         mk_mterm (Mtuple (storage_var::(List.map (fun (x, y : ident * type_) -> mk_mterm (Mvarlocal (dumloc x)) y) subs)))
           (Ttuple (Tstorage::(List.map (fun (x, y : ident * type_) -> y) subs))), subs
       | _ -> storage_var, [])
    in
    (* let t, s = process_mtern ctx s t in *)
    let t, s = process_non_empty_list_term ctx s [t; target] in
    let e, s =
      begin
        match e with
        | Some v -> process_non_empty_list_term ctx s [v; target]
                    |> (fun (x, y) -> Some x, y)
        | None -> Some target, s(*{s with subs = subs}*)
      end
    in
    mk_mterm (Mif (c, t, e)) target.type_, {s with subs = subs}

  | Mseq l ->
    let l : mterm list = List.filter (fun (x : mterm) ->
        match x.node with
        | Massert _ -> false
        | _ -> true) l in

    begin
      match l with
      | [] -> mt, s
      | i::[] ->
        process_mtern ctx s i
      | _ -> process_non_empty_list_term ctx s l
    end

  | Mfor (a, col, body) ->
    let col, s = process_mtern ctx s col in
    let subs : (ident * type_) list = compute_side_effect ctx body in
    let is = [storage_lident] @ (List.map (fun (x, y) -> dumloc x) subs) in
    let type_tuple = Ttuple ([Tstorage] @ List.map snd subs) in
    let tuple : mterm = mk_mterm (Mtuple ([storage_var] @ List.map (fun (x, y) -> mk_mterm (Mvarlocal (dumloc x)) y) subs)) type_tuple in
    let ctx = {
      ctx with
      target = Some tuple;
    } in
    let body, s = process_non_empty_list_term ctx s [body; tuple] in
    mk_mterm (Mfold (a, is, col, body)) tuple.type_, s


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
    subs = [];
  } in
  let target = Option.get ctx.target in
  let mt, _s = process_non_empty_list_term ctx s [mt; target] in
  mt

let analyse_type (mt : mterm) : type_ = Tstorage

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
              let args, target =
                begin
                  match ret with
                  | Tstorage ->
                    let arg : argument = (storage_lident, Tstorage, None) in
                    let args = arg::fs.args in
                    args, storage_var
                  | Tcontainer (Toperation, Collection) ->
                    let arg : argument = (operations_lident, operations_type, None) in
                    let args = arg::fs.args in
                    args, operations_var
                  | Ttuple [Tcontainer (Toperation, Collection); Tstorage] ->
                    let arg_s_ : argument = (storage_lident, Tstorage, None) in
                    let arg_ops_ : argument = (operations_lident, operations_type, None) in
                    let args = arg_s_::arg_ops_::fs.args  in
                    args, operations_storage_var
                  | _ -> assert false
                end in
              let ctx : ctx_red = {
                ctx with
                target = Some target;
              } in
              let body = process_body ctx fs.body in
              let fs = {
                fs with
                args = args;
                body = body;
              } in
              let ctx : ctx_red = {
                ctx with
                local_fun_types = (unloc fs.name, ret)::ctx.local_fun_types;
                target = None;
              } in
              Function (fs, ret), ctx
            | _ -> assert false
          end
        | Entry fs ->
          (* let entry_body = mk_mterm (Mletin ([operations_lident], operations_init, Some operations_type, fs.body)) operations_storage_type in *)
          let ctx = {
            ctx with
            target = Some operations_storage_var;
          } in
          let body = process_body ctx fs.body in
          let body = mk_mterm (Mletin ([operations_lident], operations_init, Some operations_type, body)) operations_storage_type in
          let fs = {
            fs with
            body = body;
          } in
          let ctx = {
            ctx with
            target = None;
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
      vars = [];
      target = None;
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
