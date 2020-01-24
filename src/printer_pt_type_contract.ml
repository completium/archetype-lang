(* -------------------------------------------------------------------------- *)
open Tools
open Location
open ParseTree
open Printer_tools

let emit_error msg =
  let str : string = Format.asprintf "%s@." msg in
  let pos : Position.t list = [] in
  Error.error_alert pos str (fun _ -> ());
  raise (Core.Error_Stop 7)

let pp_archetype fmt (decls : declaration list) =
  let pp_action fmt (id, args) =
  Format.fprintf fmt
    "action %a(%a)"
    pp_str id
    (pp_list ", " (fun fmt (x, t) -> Format.fprintf fmt "%a : %a" pp_str x Printer_pt.pp_type t)) args
   in
  let get_id () =
    let id = List.fold_left (fun accu x -> match unloc x with | Darchetype (x, _) -> Some x | _ -> accu) None decls in
    match id with
    | Some v -> v
    | _ -> emit_error "Error: no id found"
  in

  let extract_action () =
    let to_arg ((a, b, _) : lident_typ) = (unloc a, b) in
    List.fold_right (fun x accu ->
        match unloc x with
        | Daction (id, args, _, _, _) -> (unloc id, List.map to_arg args)::accu
        | Dtransition _ -> accu
        | _ -> accu) decls []
  in
  let id   = get_id () in
  let acts = extract_action () in

  Format.fprintf fmt
    "contract %a {@\n  @[%a@]@\n}"
    pp_id id
    (pp_list "@\n" pp_action) acts

let pp_archetype fmt pt =
  match unloc pt with
  | Marchetype es -> pp_archetype fmt es
  | Mextension (_id, _ds, _es) -> emit_error "Error: Cannot display contract from extension"

(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : archetype) = string_of__of_pp pp_archetype x


(* -------------------------------------------------------------------------- *)
let string_of__of_pp pp x =
  Format.asprintf "%a@." pp x

let show_model (x : archetype) = string_of__of_pp pp_archetype x
