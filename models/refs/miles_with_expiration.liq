type mile = {
  amount: nat;
  expiration: timestamp;
}

type owner = string list

type storage = {
  admin: address;
  mile_col: (string, mile) map;
  owner_col: (address, owner) map;
}

let%init init (admin1: address) : storage =
  {
    admin = admin1;
    mile_col = (Map : (string, mile) map);
    owner_col = (Map : (address, owner) map)
  }

let[@inline] get_admin (s: storage) : address = s.admin

let[@inline] set_admin (s: storage) (v: address) : storage = s.admin <- v

let[@inline] get_mile (p: storage * string) : mile =
  let s = get p 0 in
  let v = get p 1 in
  begin match Map.find v (s.mile_col) with
    | Some k -> k
    | None -> Current.failwith ("not found")
  end

let mk_mile (p : nat * timestamp) : mile =
  {
    amount = get p 0;
    expiration = get p 1;
  }

let[@inline] get_owner (p: storage * address) : owner =
  let s = get p 0 in
  let v = get p 1 in
  begin match Map.find v (s.owner_col) with
    | Some k -> k
    | None -> Current.failwith ("not found")
  end

let mk_owner (p : string list) : owner = p

let[@inline] addifnotexists_owner (p: storage * address * string list) : storage =
  let s = get p 0 in
  let owner_key = get p 1 in
  let owner_miles = get p 2 in
  match Map.find owner_key (s.owner_col) with
  | Some _ -> s
  | None -> s.owner_col <- Map.update owner_key (Some (mk_owner owner_miles)) s.owner_col

let[@inline] add_owner_miles (p : storage * string * mile * (address * owner)) : storage =
  let s = get p 0 in
  let mile_key = get p 1 in
  let mile = get p 2 in
  let owner = get p 3 in
  let owner_key = get owner 0 in
  let owner_value = get owner 1 in
  s.owner_col <- Map.update owner_key (Some (mile_key::owner_value)) s.owner_col


let%entry add (p : address * string * nat * timestamp) (s : storage) =
  let ow = get p 0 in
  let newmile_key = get p 1 in
  let newmile_amount = get p 2 in
  let newmile_expiration = get p 3 in
  let newmile = mk_mile (newmile_amount, newmile_expiration) in

  let s = addifnotexists_owner (s, ow, []) in
  let s = add_owner_miles (s, newmile_key, newmile, (ow, get_owner (s,ow))) in
  (empty_ops, s)
