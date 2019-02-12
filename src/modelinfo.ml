open Location
open Model

(* type mapping exceptions : asset name, field name, type location *)
exception InvalidKeyType     of lident * lident * Location.t
exception UnsupportedType    of lident * lident * Location.t
exception VarNoType          of Location.t
exception UnsupportedVartype of Location.t
exception NoFieldType        of lident

type info = {
  key_types : (string * vtyp) list; (* asset name, key type *)
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

let mk_info m =
  let kt = m.assets |> List.fold_left (fun acc a -> acc @ [get_key_type a]) [] in
  { key_types = kt }

let get_key_type fname key_types =
  let id = unloc fname in
  if List.mem_assoc id key_types
  then List.assoc id key_types
  else raise Not_found
