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
  key_types   : (string * vtyp) list;           (* asset name, key type         *)
  state_init  : (string * lident) list;         (* state name, initial value    *)
  dummy_vars  : (string * (string * vtyp)) list (* variable name, (value, vtyp) *)
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

(* retrieve string and datetime constant values and create dummy variable
   which won't be printed out *)
let dummy_counter = ref 0

let fresh_dummy () =
  dummy_counter := !dummy_counter + 1;
  "_dummy_var_"^(string_of_int (!dummy_counter))

let default_to_dummy acc = function
  | Some default ->
    begin
      match unloc default with
      | BVdate    s -> acc @ [fresh_dummy (),(s, VTdate)]
      | BVaddress s -> acc @ [fresh_dummy (),(s, VTaddress)]
      | BVstring  s -> acc @ [fresh_dummy (),(s, VTstring)]
      | _ -> acc
    end
  | None -> acc

let mk_dummy_variables (m : model_unloc) : (string * (string * vtyp)) list =
  (* look in model variables and asset fields *)
  m.variables |>
  List.fold_left (fun acc v ->
      let value = v |> unloc |> fun x -> x.decl |> unloc |> fun x -> x.default in
      default_to_dummy acc value
    ) []
  |> fun x -> List.fold_left (fun acc (a : asset) ->
      let fields = (unloc a).args in
      List.fold_left (fun acc f ->
          let decl = unloc f in
          default_to_dummy acc decl.default
        ) acc fields
    ) x m.assets
  |> fun x -> List.fold_left (fun acc (r : role) ->
      let r = unloc r in
      match r.default with
      | Some (Raddress a) -> acc @ [fresh_dummy (), (a, VTaddress)]
      | _ -> acc
    ) x m.roles
(* TODO : scan transactions *)

let mk_info m =
  let kt = m.assets |> List.fold_left (fun acc a -> acc @ [get_key_type a]) [] in
  let si = m.states |> List.fold_left (fun acc s -> acc @ [get_state_init s]) [] in
  let vr = m |> mk_dummy_variables in
  { key_types = kt; state_init = si; dummy_vars = vr }

let get_key_type fname info =
  let id = unloc fname in
  if List.mem_assoc id info.key_types
  then List.assoc id info.key_types
  else raise Not_found

let is_state info id = List.mem_assoc (unloc id) info.state_init

let get_initial_state info id =
  if List.mem_assoc (unloc id) info.state_init
  then
    List.assoc (unloc id) info.state_init
  else
    raise (NotFound (unloc id))

let get_dummy_for info value =
  let rec get l =
    match l with
    | (n,(v,_))::_ when compare v value = 0 -> n
    | _::tl -> get tl
    | [] -> raise (NotFound (value^" in "^(String.concat "\n" (List.map (fun (n,(v,_)) -> n^" "^v) info.dummy_vars)))) in
  get info.dummy_vars
