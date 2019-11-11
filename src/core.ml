(* -------------------------------------------------------------------- *)
(*include BatPervasives*)

(* -------------------------------------------------------------------- *)
(*module String  = BatString
  module List    = List
  module Int     = BatInt
  module Ord     = BatOrd
  module Set     = BatSet
  module Map     = BatMap
  module Num     = BatNum
  module Opt     = BatOption
  module IO      = BatIO
  module Lexing  = BatLexing*)

(* -------------------------------------------------------------------- *)
module Format = struct
  include Format

  type 'a pp = Format.formatter -> 'a -> unit
end

type big_int = Big_int.big_int

let pp_big_int fmt n = Format.fprintf fmt "%s" (Big_int.string_of_big_int n)

let big_int_of_yojson (s : Yojson.Safe.t) : (big_int, string) Result.result =
  try Ok (Big_int.big_int_of_string (Yojson.Safe.to_string s))
  with _ -> Error "big_int_of_yojson"

let big_int_to_yojson (n : big_int) : Yojson.Safe.t = Yojson.Safe.from_string (Big_int.string_of_big_int n)

(* -------------------------------------------------------------------- *)

type duration = {
  weeks   : big_int;
  days    : big_int;
  hours   : big_int;
  minutes : big_int;
  seconds : big_int;
}
[@@deriving show {with_path = false}]

let mk_duration
    ?(weeks   = Big_int.zero_big_int)
    ?(days    = Big_int.zero_big_int)
    ?(hours   = Big_int.zero_big_int)
    ?(minutes = Big_int.zero_big_int)
    ?(seconds = Big_int.zero_big_int) () = {
  weeks   = weeks;
  days    = days;
  hours   = hours;
  minutes = minutes;
  seconds = seconds;
}

let string_to_duration (input : string) : duration =
  let extract_value c : big_int  =
    let pattern = "[0-9]+" ^ (String.make 1 c) in
    let re = Str.regexp pattern in
    try
      match Str.search_forward re input 0 with
      | _ ->
        let str = Str.matched_group 0 input in
        let str = (String.sub str 0 ((String.length str) - 1)) in
        Big_int.big_int_of_string str
    with
      Not_found -> Big_int.zero_big_int
  in
  let weeks   = extract_value 'w' in
  let days    = extract_value 'd' in
  let hours   = extract_value 'h' in
  let minutes = extract_value 'm' in
  let seconds = extract_value 's' in
  mk_duration
    ~weeks:weeks
    ~days:days
    ~hours:hours
    ~minutes:minutes
    ~seconds:seconds
    ()

let cmp_duration (d1 : duration) (d2 : duration) : bool =
  let cmp_aux d1 d2 = Big_int.compare_big_int d1 d2 = 0 in

  cmp_aux d1.weeks   d2.weeks
  && cmp_aux d1.days    d2.days
  && cmp_aux d1.hours   d2.hours
  && cmp_aux d1.minutes d2.minutes
  && cmp_aux d1.seconds d2.seconds

let pp_duration_for_printer fmt (d : duration) =
  let is_zero : bool =
    Big_int.eq_big_int d.weeks   Big_int.zero_big_int
    && Big_int.eq_big_int d.days    Big_int.zero_big_int
    && Big_int.eq_big_int d.hours   Big_int.zero_big_int
    && Big_int.eq_big_int d.minutes Big_int.zero_big_int
    && Big_int.eq_big_int d.seconds Big_int.zero_big_int
  in
  let pp_aux (postfix : string) fmt (n : big_int) =
    if Big_int.eq_big_int Big_int.zero_big_int n
    then ()
    else
      Format.fprintf fmt "%a%s"
        pp_big_int n
        postfix
  in
  if is_zero
  then Format.fprintf fmt "0s"
  else
    Format.fprintf fmt "%a%a%a%a%a"
      (pp_aux "w") d.weeks
      (pp_aux "d") d.days
      (pp_aux "h") d.hours
      (pp_aux "m") d.minutes
      (pp_aux "s") d.seconds

let duration_to_seconds (d : duration) : big_int =
  Big_int.zero_big_int
  |> Big_int.add_big_int (Big_int.mult_int_big_int 7  d.weeks)
  |> Big_int.add_big_int (Big_int.mult_int_big_int 24 d.days)
  |> Big_int.add_big_int (Big_int.mult_int_big_int 60 d.hours)
  |> Big_int.add_big_int (Big_int.mult_int_big_int 60 d.minutes)
  |> Big_int.add_big_int                              d.seconds

let pp_duration_in_seconds fmt (d : duration) =
  let s = duration_to_seconds d in
  Format.fprintf fmt "%a" pp_big_int s

let date_str_to_big_int _ = Big_int.zero_big_int
