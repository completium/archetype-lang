exception No_value
exception Unsupported_yet

let get = function
  | Some e -> e
  | None -> raise No_value
