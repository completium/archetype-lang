[%%version 0.5]

let[@inline] get_caller () = Current.sender ()

let[@inline] now () = Current.time ()

let[@inline] empty_ops = [] : (operation list)
