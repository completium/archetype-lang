exception No_value
exception Unsupported_yet

let debug_mode = ref false

let is_none = function None -> true  | Some _ -> false
let is_some = function None -> false | Some _ -> true

let id x = x

let get = function
  | Some e -> e
  | None -> raise No_value

let get_dfl dfl = function None -> dfl | Some e -> e

let map_option f = function
  | Some x -> Some (f x)
  | None -> None

let (|?>) x f = map_option f x
let (|? ) x f = ignore (map_option f x)

let map_option_neutral f n = function
  | Some x -> f x
  | None -> n

let fold_option f x y =
  match y with None -> x | Some y -> f x y

let map_list = function
  | Some l -> l
  | None -> []

(* -------------------------------------------------------------------- *)
module List : sig
  include module type of List

  val is_empty : 'a list -> bool
  val int_fold : ('a -> int -> 'a) -> 'a -> int -> 'a

  module Exn : sig
    val assoc : 'a -> ('a * 'b) list -> 'b option
  end
end = struct
  include List

  let is_empty = function [] -> true | _ -> false

  let int_fold f acc n =
    let rec int_fold_rec acc i =
      if (i = n)
      then acc
      else int_fold_rec (f acc i) (i + 1) in
    int_fold_rec acc 0

  module Exn = struct
    let assoc x xs =
      try Some (List.assoc x xs) with Not_found -> None
  end
end
