open Location
open Model

(* type mapping exceptions : asset name, field name, type location *)
exception InvalidKeyType         of lident * lident * Location.t
exception UnsupportedType        of lident * lident * Location.t
exception VarNoType              of Location.t
exception UnsupportedVartype     of Location.t
exception ExpectedVarType        of lident
exception CannotConvert          of string
exception StringUnsupported
exception ExpectsOneInitialState of lident
exception UnsupportedPolicy      of string * string
exception UnsupportedFeature     of string
exception NotFound               of string
exception Anomaly                of string

type storage_policy = Record | Flat
[@@deriving show {with_path = false}]

let storage_policy = ref Record
(*let storage_policy = ref Flat*)


type info = {
  assets_pol  : (string * storage_policy)       list; (* asset name, storage policy     *)
  key_ids     : (string * string)               list; (* asset name, key id             *)
  key_types   : (string * (string * vtyp))      list; (* asset name,(key name,key type) *)
  asset_vars  : (string * (string * ptyp) list) list; (* asset name,(field name, type)  *)
  partitions  : (string * string)               list; (* partition field, asset name    *)
  state_init  : (string * lident)               list; (* state name, initial value      *)
  dummy_vars  : (string * (string * vtyp))      list; (* variable name, (value, vtyp)   *)
}
[@@deriving show {with_path = false}]


let get_asset_policy aname info =
  let id = unloc aname in
  if List.mem_assoc id info.assets_pol
  then List.assoc id info.assets_pol
  else raise (NotFound ("get_asset_policy "^id))

let get_key_type (a : asset) =
  let keyid = a |> fun x -> x.key |> unloc in
  let aname = a.name |> unloc in
  let rec rec_get_key = function
    | arg::tl ->
      if compare keyid (get_decl_id arg) = 0
      then
        let typ =
          match arg.typ with
          | Some t ->
            begin
              match unloc t with
              | Tbuiltin typ -> typ
              | _ -> raise (InvalidKeyType (a.name,arg.name,(loc t)))
            end
          | None   -> raise (ExpectedVarType arg.name)
        in (aname, (aname^"_"^keyid, typ))
      else rec_get_key tl
    | [] -> raise (NotFound "get_key_type") in
  a |> fun x -> x.args |> rec_get_key

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

let lit_dummy_val acc = function
  | BVdate    s -> acc @ [fresh_dummy (),(s, VTdate)]
  | BVaddress s -> acc @ [fresh_dummy (),(s, VTaddress)]
  | BVstring  s -> acc @ [fresh_dummy (),(s, VTstring)]
  | _ -> acc

let default_to_dummy_val acc = function
  | Some default -> lit_dummy_val acc (unloc default)
  | None -> acc

let default_to_dummy acc = function
  | Some default ->
    begin
      match unloc default with
      | Plit l -> lit_dummy_val acc (unloc l)
      | _ -> acc
    end
  | None -> acc

let mk_dummy_variables (m : model_unloc) : (string * (string * vtyp)) list =
  (* look in model variables and asset fields *)
  m.variables
  |> List.fold_left (fun acc v ->
      let value = v |> fun x -> x.decl |> fun x -> x.default in
      default_to_dummy acc value
    ) []
  |> (fun x -> List.fold_left (fun acc (a : asset) ->
      let fields = a.args in
      List.fold_left (fun acc f ->
          let decl = f in
          default_to_dummy_val acc decl.default
        ) acc fields
    ) x m.assets)
  |> (fun x -> List.fold_left (fun acc (r : role) ->
      match r.default with
      | Some (Raddress a) -> acc @ [fresh_dummy (), (a, VTaddress)]
      | _ -> acc
    ) x m.roles)
(* TODO : scan transactions *)

let get_asset_vars (a : asset) : (string * ptyp) list =
  let keyid = a |> fun x -> x.key |> unloc in
  let prefix = (match !storage_policy with
      | Flat -> (unloc a.name) ^ "_"
      | _ -> "") in
  let rec rec_get_vars acc = function
    | arg :: tl ->
      if compare keyid (get_decl_id arg) = 0
      then rec_get_vars acc tl
      else
        let t =
          match arg.typ with
          | Some t -> t
          | None   -> raise (ExpectedVarType a.name)
        in
        rec_get_vars (acc @ [prefix ^ get_decl_id arg, t]) tl
    | [] -> acc in
  a.args |> rec_get_vars []

let get_partitions (a : asset) : (string * string) list =
  let rec rec_get_p acc = function
    | arg :: tl ->
      begin
        match arg.typ with
        | Some typ ->
          begin
            match unloc typ with
            | (Tcontainer (typ,Partition)) ->
              begin
                match unloc typ with
                | Tasset id ->
                  let fid = unloc arg.name in
                  rec_get_p (acc @ [fid,unloc id]) tl
                | _ -> rec_get_p acc tl
              end
            | _ -> rec_get_p acc tl
          end
        | _ -> rec_get_p acc tl
      end
    | [] -> acc in
  rec_get_p [] a.args

let mk_info m =
  let ap = m.assets |> List.map (fun (a:asset) -> (unloc a.name), !storage_policy) in
  let kt = m.assets |> List.fold_left (fun acc a -> acc @ [get_key_type a]) [] in
  let ki = m.assets |> List.map (fun (a:asset) -> (unloc a.name, unloc a.key)) in
  let si = m.states |> List.fold_left (fun acc s -> acc @ [get_state_init s]) [] in
  let vr = m |> mk_dummy_variables in
  let av = m.assets |> List.map (fun a -> get_asset_name a, get_asset_vars a) in
  let pa = m.assets |> List.fold_left (fun acc a -> acc @ (get_partitions a)) [] in
  {
    assets_pol = ap;
    key_types  = kt;
    key_ids    = ki;
    asset_vars = av;
    partitions = pa;
    state_init = si;
    dummy_vars = vr }

let is_key aname fname info =
  let id = unloc aname in
  if List.mem_assoc id info.key_types
  then
    let n,_ = List.assoc id info.key_types in
    compare (unloc fname) n = 0
  else false

let get_key_id aname info =
  let id = unloc aname in
  if List.mem_assoc id info.key_types
  then let n = List.assoc id info.key_ids in n
  else raise (NotFound ("get_key_name "^id))

let get_key_name aname info =
  let id = unloc aname in
  if List.mem_assoc id info.key_types
  then let n,_ = List.assoc id info.key_types in n
  else raise (NotFound ("get_key_name "^id))

let get_key_type aname info =
  let id = unloc aname in
  if List.mem_assoc id info.key_types
  then let _,t = List.assoc id info.key_types in t
  else raise (NotFound ("get_key_type "^id))

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

let is_partition id info = List.mem_assoc (unloc id) info.partitions

let get_asset_vars_id_typs aname info =
  let a = unloc aname in
  if List.mem_assoc a info.asset_vars
  then List.assoc a info.asset_vars
  else raise (NotFound a)

let get_asset_vars_typs aname info =
  let a = unloc aname in
  if List.mem_assoc a info.asset_vars
  then snd (List.split (List.assoc a info.asset_vars))
  else raise (NotFound a)

let get_asset_vars aname info =
  let a = unloc aname in
  if List.mem_assoc a info.asset_vars
  then fst (List.split (List.assoc a info.asset_vars))
  else raise (NotFound a)

let get_asset_var_typ aname var info =
  let a = unloc aname in
  if List.mem_assoc a info.asset_vars
  then
    let types = List.assoc a info.asset_vars in
    if List.mem_assoc var types
    then List.assoc var types
    else raise (NotFound var)
  else raise (NotFound a)
