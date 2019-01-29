[%%version 0.5]

(* CONS PRELUDE ****)

let[@inline] upd (f1,f2) = f1 <- f2

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

let[@inline] list_mem (k, l) =
  List.fold (fun (e,acc) ->
      match acc with
      | Some _ -> acc
      | None ->
         if cmp (e, k) = 0
         then Some e
         else acc
    ) l None

let[@inline] list_append (l1, l2) =
  let l = List.fold (fun (e,acc) ->
      e::acc
    ) l1 l2 in
  sort (cmp, l)

let[@inline] list_add (a, l) = sort (cmp, a::l)

let[@inline] list_remove (a, l) =
  let l =
    List.fold (fun (e, acc) ->
        if cmp (e, a) = 0
        then acc
        else e::acc
      ) l [] in
  sort (cmp, l)

let[@inline] when (params : 's * (('s * 'a) -> bool) * ('a list)) =
  let s = get params 0 in
  let f = get params 1 in
  let l = get params 2 in
  List.fold (fun (e, acc) ->
      if f (s, e)
      then e::acc
      else acc
    ) l []

let[@inline] sum_nat (params : 's * (('s * 'a) -> nat) * ('a list)) =
  let s = get params 0 in
  let f = get params 1 in
  let l = get params 2 in
  List.fold (fun (e, acc) -> f (s, e) + acc) l (0:nat)


type storage = {
  admin: address;
  admin_tmp: address;
  mile_id: string list;
  mile_amount: (string, nat) map;
  mile_expiration: (string, timestamp) map;
  owner_addr: address set;
  owner_miles: (address, string list) map;
  }

let%init init (s: storage) (admin1: address) : storage =
  { admin = admin1; admin_tmp = admin1; mile_id = []; mile_amount =
    (Map : (string, nat) map); mile_expiration =
    (Map : (string, timestamp) map); owner_addr = (Set : address set);
    owner_miles = (Map : (address, string list) map) }

let[@inline] get_admin (s: storage) : address = s.admin

let[@inline] get_admin_tmp (s: storage) : address = s.admin_tmp

let[@inline] get_mile_id (params: storage * string) : string =
  let s = get params 0 in
  let k = get params 1 in
  begin match mem ((k, s.mile_id)) with
  | Some v -> v
  | None -> Current.failwith ("not found")
  end

let[@inline] get_mile_amount (params: storage * string) : nat =
  let s = get params 0 in
  let k = get params 1 in
  begin match Map.find k (s.mile_amount) with
  | Some v -> v
  | None -> Current.failwith ("not found")
  end

let[@inline] get_mile_expiration (params: storage * string) : timestamp =
  let s = get params 0 in
  let k = get params 1 in
  begin match Map.find k (s.mile_expiration) with
  | Some v -> v
  | None -> Current.failwith ("not found")
  end

let[@inline] get_owner_miles (params: storage * address) : string list =
  let s = get params 0 in
  let k = get params 1 in
  begin match Map.find k (s.owner_miles) with
  | Some v -> v
  | None -> Current.failwith ("not found")
  end

let[@inline] get_owner_addr (params: storage * address) : address =
  let s = get params 0 in
  let k = get params 1 in
  if Set.mem k (s.owner_addr) then begin k end
  else
  begin
    Current.failwith ("not found") end

let[@inline] owner_addifnotexist (params: storage * address * (string list)) : storage
  =
  let s = get params 0 in
  let k = get params 1 in
  let m = get params 2 in
  let s1 =
    if not (Set.mem k (s.owner_addr)) then begin
      let s2 = upd (s.owner_addr) (Set.add k (s.owner_addr)) in s2 end
    else
    begin
      s end in
  let s2 = upd (s1.owner_miles) (Map.add k m (s1.owner_miles)) in
  let s3 = upd (s2.mile_id) (append ((s2.mile_id, m))) in s3

let[@inline] owner_add_miles (params: storage * address * string * nat * timestamp) : storage
  =
  let s = get params 0 in
  let k = get params 1 in
  let i = get params 2 in
  let a = get params 3 in
  let e = get params 4 in
  let tmp1 =
    begin match Map.find k (s.owner_miles) with
    | Some miles -> miles
    | None -> Current.failwith ("not found")
    end in
  let s1 = upd (s.owner_miles) (Map.add k (add ((i, tmp1))) (s.owner_miles)) in
  let s2 = upd (s1.mile_id) (add ((i, s1.mile_id))) in
  let s3 = upd (s2.mile_amount) (Map.add i a (s2.mile_amount)) in
  let s4 = upd (s3.mile_expiration) (Map.add i e (s3.mile_expiration)) in s4

let[@inline] owner_remove_miles (params: storage * address * string) : storage =
  let s = get params 0 in
  let o = get params 1 in
  let m = get params 2 in
  let tmp1 =
    begin match Map.find o (s.owner_miles) with
    | Some miles -> miles
    | None -> Current.failwith ("not found")
    end in
  let s1 =
    upd (s.owner_miles) (Map.add o (remove ((m, tmp1))) (s.owner_miles)) in
  let s2 = upd (s1.mile_id) (remove ((m, s1.mile_id))) in
  let s3 = upd (s2.mile_amount) (Map.remove m (s2.mile_amount)) in
  let s4 = upd (s3.mile_expiration) (Map.remove m (s3.mile_expiration)) in s4

let[@inline] mile_set_amount (params: storage * string * nat) : storage =
  let s = get params 0 in
  let k = get params 1 in
  let v = get params 2 in
  let s1 = upd (s.mile_amount) (Map.add k v (s.mile_amount)) in s1

let%entry add1 (params: address * string * nat * timestamp) (s: storage) :
  (operation list) * storage =
  let ops = empty_ops in
  let caller = get_caller () in
  let owner = get params 0 in
  let id = get params 1 in
  let amount = get params 2 in
  let expiration = get params 3 in
  let (ops1, s1) = (ops, owner_addifnotexist ((s, owner, []))) in let (ops2,
  s2) = (ops1, owner_add_miles ((s1, owner, id, amount, expiration))) in
  (ops2, s2)

let[@inline] by_expiration (params: storage * string) : bool =
  let s = get params 0 in
  let k = get params 1 in (get_mile_expiration ((s, k))) >= (now ())

let%entry consume (params: address * nat) (s: storage) : (operation list) * storage
  =
  let ops = empty_ops in
  let caller = get_caller () in
  let owner = get params 0 in
  let val_ = get params 1 in
  let o = get_owner_addr ((s, owner)) in
  let m = get_owner_miles ((s, o)) in
  let l = when1 ((s, by_expiration, m)) in
  let sum_amount = sum_nat ((s, get_mile_amount (), l)) in
  let remainder = val_ in
  let o1 = get_owner_addr ((s, owner)) in
  let m1 = get_owner_miles ((s, o1)) in
  let l1 = when1 ((s, by_expiration, m1)) in
  let (ops3, s3, remainder1, o2) =
  List.fold (fun (arg: string * ((operation list) * storage * nat * address)) ->
               let k = get arg 0 in
               let acc = get arg 1 in
               let ops4 = get acc 0 in
               let s4 = get acc 1 in
               let remainder2 = get acc 2 in
               let o3 = get acc 3 in
               if nat_gt remainder2 ((0:nat)) then begin
                 if nat_gt (get_mile_amount ((s4, k))) remainder2 then begin
                   let s5 =
                     mile_set_amount ((s4, k,
                                      nat_sub ((get_mile_amount ((s4, k)),
                                               remainder2)))) in
                   let remainder3 = (0:nat) in (ops4, s5, remainder3, o3) end
                 else
                 begin
                   if (get_mile_amount ((s4, k))) = remainder2 then begin
                     let s5 = owner_remove_miles ((s4, o3, k)) in
                     let remainder3 = (0:nat) in (ops4, s5, remainder3, o3) end
                   else
                   begin
                     let remainder3 =
                       nat_sub ((remainder2, get_mile_amount ((s4, k)))) in
                     let s5 = owner_remove_miles ((s4, o3, k)) in
                     (ops4, s5, remainder3, o3) end end end
               else
               begin
                 (ops4, s4, remainder2, o3) end) l1 ((ops, s, remainder, o1)) in
  (ops3, s3)
