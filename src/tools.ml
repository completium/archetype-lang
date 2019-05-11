exception No_value
exception Unsupported_yet

let debug_mode = ref false

let id x = x

let get = function
  | Some e -> e
  | None -> raise No_value

let map_option f = function
  | Some x -> Some (f x)
  | None -> None

  let map_option_neutral f n = function
  | Some x -> f x
  | None -> n

let map_list = function
  | Some l -> l
  | None -> []

let int_fold f acc n =
  let rec int_fold_rec acc i =
    if (i = n)
    then acc
    else int_fold_rec (f acc i) (i + 1) in
  int_fold_rec acc 0
