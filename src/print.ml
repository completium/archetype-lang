(* -------------------------------------------------------------------- *)
open Location
open Ast

let field_to_str { pldesc = f } =
match f with
| Tfield (id, typ) -> (unloc id) ^ " : " ^ (unloc typ) ^ ";"

let entity_to_str { pldesc = e } =
match e with
| Tuse id -> "use " ^ (unloc id)
| Tmodel id -> "model " ^ (unloc id)
| Tconstant (id, typ) -> "constant " ^ (unloc id) ^ " " ^ (unloc typ)
| Trole id -> "role " ^ (unloc id)
| Tasset (id, fields) -> "asset " ^ (unloc id) ^ " = {" ^ (List.fold_left (fun s e -> (s ^ "\n  " ^ (field_to_str e))) "" fields) ^ "\n}"
| Textension l -> "[%" ^ (List.fold_left (fun s e -> (s ^ " " ^ (unloc e))) "" l) ^ "]"

let model_to_str m =
match m with
| Imodel l -> List.fold_left (fun s e -> (s ^ (entity_to_str e)^ "\n")) "" l
