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

let[@inline] remove (a, l) =
  let l =
    List.fold (fun (e, acc) ->
        if cmp (e, a) = 0
        then acc
        else e::acc
      ) l [] in
  sort (cmp, l)

(* STORAGE ****)

type storage = {
  admin            : address;
  admin_tmp        : address;
  mile_id         : string list;
  mile_amount     : (string, nat) map;
  mile_expiration : (string, timestamp) map;
  owner_addr       : address set;
  owner_miles      : (address, string list) map;
}

let%init storage (admin : address) = {
  admin     = admin;
  admin_tmp = admin;
  mile_id = [];
  mile_amount = (Map : (string, nat) map);
  mile_expiration = (Map : (string, timestamp) map);
  owner_addr = (Set : address set);
  owner_miles = (Map : (address, string list) map);
}

let[@inline] get_admin (s : storage) = s.admin

let[@inline] get_admin_tmp (s : storage) = s.admin_tmp

let[@inline] get_mile_id ((s : storage), (k : string)) =
  match mem (k, s.mile_id) with
  | Some v -> v
  | None -> Current.failwith "get_mile_id: not found"

let[@inline] get_mile_amount ((s : storage), (k : string)) =
  match Map.find k s.mile_amount with
  | Some v -> v
  | None -> Current.failwith "get_mile_amount: not found"

let[@inline] get_mile_expiration ((s : storage), (k : string)) =
  match Map.find k s.mile_expiration with
  | Some v -> v
  | None -> Current.failwith "get_mile_expiration: not found"

let[@inline] get_owner_miles ((s : storage), (k : address)) =
  match Map.find k s.owner_miles with
  | Some v -> v
  | None -> Current.failwith "get_owner_miles: not found"

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
  let s = s.mile_id <- append(s.mile_id, m) in
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
   let s = s.mile_id <- add (i, s.mile_id) in
   let s = s.mile_amount <- Map.add i a s.mile_amount in
   let s = s.mile_expiration <- Map.add i e s.mile_expiration in
   s

let[@inline] owner_remove_miles (params : storage * address * string)  =
  let s = get params 0 in
  let o = get params 1 in
  let m = get params 2 in
  if not (Set.mem o s.owner_addr)
    then
      Current.failwith "owner_add_miles: not found";
  let tmp1 =
    match Map.find o s.owner_miles with
    | Some miles -> miles
    | None ->  Current.failwith "owner_add_miles: not found critical" in
  let s = s.owner_miles <- Map.add o (remove (m, tmp1)) s.owner_miles in
  let s = s.mile_id <- remove (m, s.mile_id) in
  let s = s.mile_amount <- Map.remove m s.mile_amount in
  let s = s.mile_expiration <- Map.remove m s.mile_expiration in
  s

let[@inline] mile_set_amount (params : storage * string * nat) =
  let s = get params 0 in
  let k = get params 1 in
  let v = get params 2 in
  let s = s.mile_amount <- Map.add k v s.mile_amount in
  s

let[@inline] when (params : ('a -> bool) * ('a list)) =
  let f = get params 0 in
  let l = get params 1 in
  List.fold (fun (e, acc) ->
      if f e
      then e::acc
      else acc
    ) l []

let[@inline] sum_nat (params : ('a -> nat) * ('a list)) =
  let f = get params 0 in
  let l = get params 1 in
  List.fold (fun (e, acc) -> f e + acc) l (0:nat)

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

let[@inline] by_expiration (params : storage * string) =
  let s = get params 0 in
  let k = get params 1 in
  get_mile_expiration (s, k) >= now ()

let%entry consume (params : address * nat) s =
  let ops = empty_ops in
  let caller = get_caller () in
  let now    = now () in
  if (caller <> get_admin s)
  then (Current.failwith ("consume: called by fails."));
  let owner  = params.(0) in
  let val_ = params.(1) in
  let o = get_owner_addr (s, owner) in
  let m = get_owner_miles (s, o) in
  let l = when ((fun k -> by_expiration (s, k)), m) in
  let sum_amount = sum_nat ((fun k -> get_mile_amount (s, k)), l) in
  if not (sum_amount >= val_)
  then Current.failwith ("consume: condition 1 fails.");
  let remainder = val_ in
  let o = get_owner_addr (s, owner) in
  let m = get_owner_miles (s, o) in
  let l = when ((fun k -> by_expiration (s, k)), m) in
  let (ops, s, remainder, o) =
    List.fold (fun (k,(ops, s, remainder, o)) ->
        if remainder > (0:nat)
        then
          if get_mile_amount (s, k) > remainder
          then
            let s = mile_set_amount (s, k, (nat_sub (get_mile_amount (s, k), remainder))) in
            let remainder = (0:nat) in
            (ops, s, remainder, o)
          else
            if get_mile_amount (s, k) = remainder
            then
              let s = owner_remove_miles (s, o, k) in
              let remainder = (0:nat) in
              (ops, s, remainder, o)
            else
              let remainder = nat_sub (remainder, (get_mile_amount (s, k))) in
              let s = owner_remove_miles (s, o, k) in
              (ops, s, remainder, o)
        else
          (ops, s, remainder, o)
      ) l (ops, s, remainder, o) in
  (ops, s)
