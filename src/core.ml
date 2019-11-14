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

(* -------------------------------------------------------------------- *)

type timezone_sign =
  | TZplus
  | TZminus
  | TZZ
  | TZNone
[@@deriving show {with_path = false}]

type date = {
  year:          int;
  month:         int;
  day:           int;
  hour:          int;
  minute:        int;
  second:        int;
  timezone_sign: timezone_sign;
  timezone_hour: int;
  timezone_min:  int;
}
[@@deriving show {with_path = false}]

let mk_date ?(year=1970) ?(month=1) ?(day=1) ?(hour=0) ?(minute=0) ?(second=0) ?(timezone_sign=TZNone) ?(timezone_hour=0) ?(timezone_min=0) () = {
  year          = year;
  month         = month;
  day           = day;
  hour          = hour;
  minute        = minute;
  second        = second;
  timezone_sign = timezone_sign;
  timezone_hour = timezone_hour;
  timezone_min  = timezone_min;
}

let pp_date fmt (date : date) =
  let pp_int2 fmt (x : int) = Format.fprintf fmt "%02d" x in
  let pp_int4 fmt (x : int) = Format.fprintf fmt "%04d" x in
  Format.fprintf fmt "%a-%a-%aT%a:%a:%a%a"
    pp_int4 date.year
    pp_int2 date.month
    pp_int2 date.day
    pp_int2 date.hour
    pp_int2 date.minute
    pp_int2 date.second
    (fun fmt (s, h, m) ->
       match s with
       | TZNone -> ()
       | TZZ -> Format.fprintf fmt "Z"
       | _ -> Format.fprintf fmt "%s%a:%a" (match s with TZplus -> "+" | TZminus -> "-" | _ -> assert false) pp_int2 h pp_int2 m
    ) (date.timezone_sign, date.timezone_hour, date.timezone_min)

let cmp_date (d1 : date) (d2 : date) : bool =
  let cmp_aux d1 d2 = d1 = d2 in

  cmp_aux d1.year          d2.year
  && cmp_aux d1.month         d2.month
  && cmp_aux d1.day           d2.day
  && cmp_aux d1.hour          d2.hour
  && cmp_aux d1.minute        d2.minute
  && cmp_aux d1.second        d2.second
  && cmp_aux d1.timezone_sign d2.timezone_sign
  && cmp_aux d1.timezone_hour d2.timezone_hour
  && cmp_aux d1.timezone_min  d2.timezone_min

exception MalformedDate of string

let string_to_date (str : string) : date =
  let eat (nb : int) (buf : string) : string * string =
    let l = String.length buf in
    String.sub buf 0 nb, String.sub buf nb (l - nb)
  in

  let eat_and_check (ref : string) (nb : int) (buf : string) =
    let str, buf = eat nb buf in
    if (not (String.equal str ref)) then raise (MalformedDate str);
    buf
  in

  let get_next_char (buf : string) =
    match buf with
    | "" -> None
    | _ -> Some (String.sub buf 0 1)
  in

  let input = str in
  let c_int (a, b) = (int_of_string a, b) in
  let year, input = eat 4 input |> c_int in
  let input = eat_and_check "-" 1 input in
  let month, input = eat 2 input |> c_int in
  let input = eat_and_check "-" 1 input in
  let day, input = eat 2 input |> c_int in
  match get_next_char input with
  | None -> mk_date () ~year:year ~month:month ~day:day
  | _ ->
    begin
      let input = eat_and_check "T" 1 input in
      let hour, input = eat 2 input |> c_int in
      let input = eat_and_check ":" 1 input in
      let minute, input = eat 2 input |> c_int in
      let input = eat_and_check ":" 1 input in
      let second, input = eat 2 input |> c_int in
      match get_next_char input with
      | None ->  mk_date () ~year:year ~month:month ~day:day ~hour:hour ~minute:minute ~second:second
      | Some "Z" -> mk_date () ~year:year ~month:month ~day:day ~hour:hour ~minute:minute ~second:second ~timezone_sign:TZZ
      | Some c ->
        begin
          let input, timezone_sign =
            match c with
            | "+" -> eat_and_check "+" 1 input, TZplus
            | "-" -> eat_and_check "-" 1 input, TZminus
            | _ -> raise (MalformedDate str)
          in
          let timezone_hour, input = eat 2 input |> c_int in
          let input = eat_and_check ":" 1 input in
          let timezone_min, _ = eat 2 input |> c_int in
          mk_date () ~year:year ~month:month ~day:day ~hour:hour ~minute:minute ~second:second ~timezone_sign:timezone_sign ~timezone_hour:timezone_hour ~timezone_min:timezone_min
        end
    end


let date_to_timestamp (date : date) : big_int = (* TODO: it's fake *)
  Big_int.zero_big_int
  |> Big_int.add_big_int (Big_int.mult_int_big_int 12 (Big_int.big_int_of_int date.year))
  |> Big_int.add_big_int (Big_int.mult_int_big_int 30 (Big_int.big_int_of_int date.month))
  |> Big_int.add_big_int (Big_int.mult_int_big_int 24 (Big_int.big_int_of_int date.day))
  |> Big_int.add_big_int (Big_int.mult_int_big_int 60 (Big_int.big_int_of_int date.hour))
  |> Big_int.add_big_int (Big_int.mult_int_big_int 60 (Big_int.big_int_of_int date.minute))
  |> Big_int.add_big_int                              (Big_int.big_int_of_int date.second)
  |> Big_int.add_big_int                              (Big_int.big_int_of_int date.second)
