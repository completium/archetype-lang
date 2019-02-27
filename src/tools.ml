exception No_value
exception Unsupported_yet

let id x = x

let get = function
  | Some e -> e
  | None -> raise No_value

let map_option f = function
  | Some x -> Some (f x)
  | None -> None

let map_list = function
  | Some l -> l
  | None -> []
