[%%version 0.5]

(* CONS PRELUDE ****)

let[@inline] get_caller () = Current.sender ()

let[@inline] now () = Current.time ()

let[@inline] empty_ops = ([] : operation list)

(* insertion sort *)
let[@inline] sort (cmp, l) = l
(*  let rec insert v l =
    match l with
    | [] -> [ v ]
    | e::t -> if (cmp e v > 0) then v::e::l else e::(insert v t) in
  let rec sort l =
    match l with
    | [] -> []
    | e::t -> insert e (sort t) in
  sort l*)

let[@inline] nat_sub ((a : nat), (b : nat)) =
  match%nat a - b with
  | Plus v -> v
  | Minus _ -> Current.failwith "nat_sub result is not a nat."

let[@inline] cmp (s1, s2) = if s1 < s2 then -1 else if s1 > s2 then 1 else 0

let[@inline] mem (k, l) =
  List.fold (fun (e,acc) ->
      match acc with
      | Some _ -> acc
      | None ->
         if cmp (e, k) = 0
         then Some e
         else acc
    ) l None

let[@inline] append (l1, l2) =
  let l = List.fold (fun (e,acc) ->
      e::acc
    ) l1 l2 in
  sort (cmp, l)

let[@inline] add (a, l) = sort (cmp, a::l)

(* STORAGE ****)

type storage = {
  admin            : address;
  admin_tmp        : address;
  miles_id         : string list;
  miles_amount     : (string, nat) map;
  miles_expiration : (string, timestamp) map;
  owner_addr       : address set;
  owner_miles      : (address, string list) map;
}

let%init storage (admin : address) = {
  admin     = admin;
  admin_tmp = admin;
  miles_id = [];
  miles_amount = (Map : (string, nat) map);
  miles_expiration = (Map : (string, timestamp) map);
  owner_addr = (Set : address set);
  owner_miles = (Map : (address, string list) map);
}

let[@inline] get_admin (s : storage) = s.admin

let[@inline] get_admin_tmp (s : storage) = s.admin_tmp

let[@inline] get_miles_id ((s : storage), (k : string)) =
  match mem (k, s.miles_id) with
  | Some v -> v
  | None -> Current.failwith "get_miles_id: not found"

let[@inline] get_miles_amount ((s : storage), (k : string)) =
  match Map.find k s.miles_amount with
  | Some v -> v
  | None -> Current.failwith "get_miles_amount: not found"

let[@inline] get_miles_expiration ((s : storage), (k : string)) =
  match Map.find k s.miles_expiration with
  | Some v -> v
  | None -> Current.failwith "get_miles_expiration: not found"

let[@inline] get_owners_miles ((s : storage), (k : address)) =
  match Map.find k s.owner_miles with
  | Some v -> v
  | None -> Current.failwith "get_owners_miles: not found"

let[@inline] get_owner_addr ((s : storage), (k : address)) =
  if Set.mem k s.owner_addr
  then k
  else Current.failwith "get_owner: not found"

let[@inline] owner_addifnotexist ((s : storage), (k : address), (m : string list)) =
  let s =
    if not (Set.mem k s.owner_addr)
    then
      let s = s.owner_addr <- Set.add k s.owner_addr in
      s
    else s in
  let s = s.owner_miles <- Map.add k m s.owner_miles in
  let s = s.miles_id <- append(s.miles_id, m) in
  s

let[@inline] owner_add_miles ((s : storage), (k : address), (i : string), (a : nat), (e : timestamp)) =
   if not (Set.mem k s.owner_addr)
    then
      Current.failwith "owner_add_miles: not found";
   let tmp1 =
     match Map.find k s.owner_miles with
     | Some miles -> miles
     | None ->  Current.failwith "owner_add_miles: not found critical" in
   let s = s.owner_miles <- Map.add k (add (i, tmp1)) s.owner_miles in
   let s = s.miles_id <- add(i, s.miles_id) in
   let s = s.miles_amount <- Map.add i a s.miles_amount in
   let s = s.miles_expiration <- Map.add i e s.miles_expiration in
   s

(* TRANSACTIONS ****)

let%entry add (params : address * string * nat * timestamp) s =
  let caller     = get_caller () in
  let owner      = get params 0 in
  let id         = get params 1 in
  let amount     = get params 2 in
  let expiration = get params 3 in
  if (caller <> get_admin s)
  then (Current.failwith ("Caller is not allowed to call this function."));
  let ops, s = empty_ops, owner_addifnotexist (s, owner, []) in
  let ops, s = ops, owner_add_miles (s, owner, id, amount, expiration) in
  ops, s

(*let%entry consume (params : address * nat) storage =
  let caller = Current.sender () in
  let now    = Current.time() in
  if (caller <> storage.admin)
  then (Current.failwith ("Caller is not allowed to call this function."));

  let owner  = params.(0) in
  let amount = params.(1) in

  let owner_asset =
    match Map.find owner storage.owners with
    | Some o -> o
    | None -> Current.failwith ("Owner not found.")  in

  let available_miles_value = List.fold (fun (elt, a) ->
                                  if elt.expiration > now
                                  then a + elt.amount
                                  else a
                                ) owner_asset.miles (0 : nat)
  in

  if (available_miles_value < amount)
  then Current.failwith ("Not enough miles.");

  let available_miles = List.fold (fun (elt, acc) ->
                            if elt.expiration > now
                            then elt::acc
                            else acc
                          ) owner_asset.miles []
  in
  let _remainder, miles = List.fold (fun (elt, (remainder, l)) ->
                              if (remainder = (0 : nat))
                              then (remainder, elt::l)
                              else if (remainder > elt.amount)
                              then (
                                let new_elt = elt.amount <- nat_sub elt.amount remainder in
                                ((0 : nat), new_elt::l)
                              )
                              else if (elt.amount = remainder)
                              then ((0 : nat), l)
                              else (nat_sub remainder elt.amount, l)
                            ) available_miles (amount, []) in
  let owner_asset = owner_asset.miles <- miles in
  let storage = storage.owners <- Map.update owner (Some owner_asset) storage.owners in
  (empty_operations, storage)



let%entry assign_new_admin (param : address) storage =
let caller = Current.sender () in

if (caller <> storage.admin)
then (Current.failwith ("Caller is not allowed to call this function."));

let storage = storage.admin_tmp <- param in
(empty_operations, storage)



let%entry confirm_admin (param : address) storage =
let caller = Current.sender () in

if (caller <> storage.admin_tmp)
then (Current.failwith ("Caller is not allowed to call this function."));

let storage = storage.admin     <- storage.admin_tmp in
(empty_operations, storage)
 *)
