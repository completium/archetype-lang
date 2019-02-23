type mile = {
  amount: nat;
  expiration: timestamp;
  }

type owner = mile list

type storage = {
  admin: address;
  mile_col: (string, mile) map;
  owner_col: (address, owner) map;
  }

let%init init (admin1: address) : storage =
  { admin = admin1; mile_col = (Map : (string, mile) map); owner_col =
    (Map : (address, owner) map) }

let[@inline] get_admin (s: storage) : address = s.admin

let[@inline] set_admin (s: storage) (v: address) : storage = s.admin <- v

let[@inline] get_mile (p: storage * string) : mile =
  let s = get p 0 in
  let v = get p 1 in
  begin match Map.find v (s.mile_col) with
  | Some k -> k
  | None -> Current.failwith ("not found")
  end

let mk_mile (p : nat * timestamp) : owner =
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

let mk_owner (p : mile list) : owner = p

let[@inline] addifnotexists_owner (p: storage * address * mile list) : storage =
  let s = get p 0 in
  let owner_id = get p 1 in
  let owner_miles = get p 2 in
  match get_owner (s, owner_id) with
  | Some v -> s
  | None -> s.ow_col <- Map.update owner_id (mk_owner owner_miles)

let[@inline] add_owner_miles (p : storage * mile * (address * owner)) : storage =
  let s = get p 0 in
  let mile = get p 1 in
  let owner = get p 2 in
  let owner_id = get owner 2 in
  let owner_value = get owner 2 in
  let owner = owner <- List.add mile owner_value.miles in
  s.owner <- Map.add owner_id owner_value s.owner


let%entry add (p : address * string * nat * timestamp) (s : storage) =
  let ow = get p 0 in
  let newmile_id = get p 1 in
  let newmile_amount = get p 2 in
  let newmile_expiration = get p 3 in
  let newmile = mk_mile (newmile_id, newmile_amount, newmile_expiration) in

  addifnotexists_owner (s, ow, []);
  add_owner_miles (s, newmile, (ow, get_owner (s,ow)))
