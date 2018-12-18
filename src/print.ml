open Ast

let field_to_str f =
match f with
| Tfield (id, typ) -> id ^ " : " ^ typ ^ ";"

let entity_to_str e =
match e with
| Tuse id -> "use " ^ id
| Tmodel id -> "model " ^ id
| Tconstant (id, typ) -> "constant " ^ id ^ " " ^ typ
| Trole id -> "role " ^ id
| Tasset (id, fields) -> "asset " ^ id ^ " = {" ^ (List.fold_left (fun s e -> (s ^ "\n  " ^ (field_to_str e))) "" fields) ^ "\n}"
| Textension l -> "[%" ^ (List.fold_left (fun s e -> (s ^ " " ^ e)) "" l) ^ "]"

let model_to_str m =
match m with
| Imodel l -> List.fold_left (fun s e -> (s ^ (entity_to_str e)^ "\n")) "" l
