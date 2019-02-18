[%%version 0.5]

let[@inline] get_caller () = Current.sender ()

let[@inline] now () = Current.time ()

let[@inline] empty_ops = ([] : operation list)

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

let[@inline] list_add (a, l) = a::l

let[@inline] list_remove (a, l) =
  List.fold (fun (e, acc) ->
      if cmp (e, a) = 0
      then acc
      else e::acc
    ) l []
