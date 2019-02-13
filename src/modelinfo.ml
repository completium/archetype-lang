open Location
open Model

(* type mapping exceptions : asset name, field name, type location *)
exception InvalidKeyType     of lident * lident * Location.t
exception UnsupportedType    of lident * lident * Location.t
exception VarNoType          of Location.t
exception UnsupportedVartype of Location.t
exception NoFieldType        of lident
exception CannotConvert      of string
exception StringUnsupported
exception ExpectsOneInitialState of lident
exception NotFound           of string

type info = {
  key_types  : (string * vtyp) list; (* asset name, key type *)
  state_init : (string * lident) list; (* state name, initial value *)
}

let get_key_type (a : asset) =
  let assetid = get_asset_name a in
  let keyid = a |> unloc |> fun x -> x.key |> unloc in
  let rec rec_get_key = function
    | arg::tl ->
      if compare keyid (get_decl_id arg) = 0
      then
        let typ =
          match (unloc arg).typ with
          | Some t ->
            begin
              match unloc t with
              | Tbuiltin typ -> typ
              | _ -> raise (UnsupportedVartype (loc t))
            end
          | None   -> raise (NoFieldType (unloc arg).name)
        in (assetid, typ)
      else rec_get_key tl
    | [] -> raise Not_found in
  a |> unloc |> fun x -> x.args |> rec_get_key

let get_state_init (s : state) =
  match  List.filter (fun it -> it.initial) s.items with
  | [init] -> (unloc s.name, init.name)
  | _ -> raise (ExpectsOneInitialState (s.name))

let mk_info m =
  let kt = m.assets |> List.fold_left (fun acc a -> acc @ [get_key_type a]) [] in
  let si = m.states |> List.fold_left (fun acc s -> acc @ [get_state_init s]) [] in
  { key_types = kt; state_init = si; }

let get_key_type fname info =
  let id = unloc fname in
  if List.mem_assoc id info.key_types
  then List.assoc id info.key_types
  else raise Not_found

(* TODO *)
let is_state info id = List.mem_assoc (unloc id) info.state_init

(* TODO *)
let get_initial_state info id =
  if List.mem_assoc (unloc id) info.state_init
  then
    List.assoc (unloc id) info.state_init
  else
    raise (NotFound (unloc id))
