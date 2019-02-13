[%%version 0.5]

(* CONS PRELUDE ****)

let[@inline] get_caller () = Current.sender ()

let[@inline] now () = Current.time ()

let[@inline] empty_ops = ([] : operation list)

(* insertion sort *)
(*let[@inline] sort (cmp, l) = l*)
(*  let rec insert v l =
    match l with
    | [] -> [ v ]
    | e::t -> if (cmp e v > 0) then v::e::l else e::(insert v t) in
  let rec sort l =
    match l with
    | [] -> []
    | e::t -> insert e (sort t) in
  sort l*)

(*let[@inline] nat_sub ((a : nat), (b : nat)) =
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

let[@inline] list_when (params : 's * (('s * 'a) -> bool) * ('a list)) =
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
  List.fold (fun (e, acc) -> f (s, e) + acc) l (0:nat) *)
