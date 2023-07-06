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
type from_input =
  | FIChannel of string * in_channel (* path * content*)
  | FIString  of string * string     (* path * content*)

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

let big_int_to_string = Big_int.string_of_big_int

(* -------------------------------------------------------------------- *)
let try_finally ~(finally : unit -> unit) (f : 'a -> 'b) (x : 'a) =
  let aout =
    try
      f x
    with e ->
      (finally (); raise e)
  in finally (); aout

(* -------------------------------------------------------------------- *)
let with_open_in (f : in_channel -> 'a) (filename : string) =
  let channel = open_in filename in
  try_finally ~finally:(fun () -> close_in channel) f channel

(* -------------------------------------------------------------------- *)
let compute_irr_fract (n, d) =
  let rec gcd a b =
    if Big_int.eq_big_int b Big_int.zero_big_int
    then a
    else gcd b (Big_int.mod_big_int a b) in
  let g = gcd n d in
  (Big_int.div_big_int n g), (Big_int.div_big_int d g)

let decimal_string_to_rational (input : string) : big_int * big_int =
  let l = Str.split (Str.regexp "\\.") input in
  let n = Big_int.big_int_of_string ((List.nth l 0) ^ (List.nth l 1)) in
  let d = Big_int.big_int_of_string ("1" ^ (String.make (String.length (List.nth l 1)) '0')) in
  compute_irr_fract (n, d)

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

let duration_to_string (duration : duration) : string =
  Format.asprintf "%a" pp_duration_for_printer duration

let duration_to_timestamp (d : duration) : big_int =
  Big_int.zero_big_int
  |> fun x -> Big_int.mult_int_big_int 7  (Big_int.add_big_int x d.weeks)
              |> fun x -> Big_int.mult_int_big_int 24 (Big_int.add_big_int x d.days)
                          |> fun x -> Big_int.mult_int_big_int 60 (Big_int.add_big_int x d.hours)
                                      |> fun x -> Big_int.mult_int_big_int 60 (Big_int.add_big_int x d.minutes)
                                                  |> fun x -> Big_int.mult_int_big_int 1  (Big_int.add_big_int x d.seconds)

let pp_duration_in_seconds fmt (d : duration) =
  let s = duration_to_timestamp d in
  Format.fprintf fmt "%a" pp_big_int s

(* -------------------------------------------------------------------- *)

type timezone =
  | TZnone
  | TZZ
  | TZplus of int * int
  | TZminus of int * int
[@@deriving show {with_path = false}]

type date = {
  year:     int;
  month:    int;
  day:      int;
  hour:     int;
  minute:   int;
  second:   int;
  timezone: timezone;
}
[@@deriving show {with_path = false}]

let mk_date ?(year=1970) ?(month=1) ?(day=1) ?(hour=0) ?(minute=0) ?(second=0) ?(timezone=TZnone) () = {
  year     = year;
  month    = month;
  day      = day;
  hour     = hour;
  minute   = minute;
  second   = second;
  timezone = timezone;
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
    (fun fmt t ->
       match t with
       | TZnone         -> ()
       | TZZ            -> Format.fprintf fmt "Z"
       | TZplus (h, m)  -> Format.fprintf fmt "+%a:%a" pp_int2 h pp_int2 m
       | TZminus (h, m) -> Format.fprintf fmt "-%a:%a" pp_int2 h pp_int2 m
    ) (date.timezone)

let date_to_string (date : date) : string =
  Format.asprintf "%a" pp_date date

let cmp_timezone (t1 : timezone) (t2 : timezone) : bool =
  match t1, t2 with
  | TZnone, TZnone -> true
  | TZZ, TZZ -> true
  | TZplus(h1, m1), TZplus(h2, m2)
  | TZminus(h1, m1), TZminus(h2, m2) -> h1 = h2 && m1 = m2
  | _ -> false

let cmp_date (d1 : date) (d2 : date) : bool =
  let cmp_aux d1 d2 = d1 = d2 in

  cmp_aux d1.year             d2.year
  && cmp_aux d1.month         d2.month
  && cmp_aux d1.day           d2.day
  && cmp_aux d1.hour          d2.hour
  && cmp_aux d1.minute        d2.minute
  && cmp_aux d1.second        d2.second
  && cmp_timezone d1.timezone d2.timezone

exception MalformedDate of string

let string_to_date =
  let datere, timere, tzre =
    let digitre i =
      let re = String.concat "" (List.init i (fun _ -> "[0-9]")) in
      "\\(" ^ re ^ "\\)" in

    let datere = String.concat "-" (List.map digitre [4; 2; 2]) in
    let timere = String.concat ":" (List.map digitre [2; 2]) in
    let timere = timere ^ ("\\(:\\([0-9][0-9]\\)\\)?") in
    let tzre   = String.concat ":" (List.map digitre [2; 2]) in

    (Str.regexp datere, Str.regexp timere, Str.regexp tzre) in

  let days = [|
    [| 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |];
    [| 31; 29; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 |];
  |] in

  let validate_date ~year ~month ~day =
    if year < 1 || (month < 1 || month > 12) then false else

      let isleap =
        year mod 4 = 0 && (year mod 100 <> 0 || year mod 400 = 0) in

      1 <= day && day <= days.(if isleap then 1 else 0).(month-1) in

  let validate_time ?(second = 0) ~hour ~minute () =
    (0 <= hour   && hour   < 24)
    && (0 <= minute && minute < 60)
    && (0 <= second && second < 60) in

  fun (s : string) ->
    let failure () = raise (MalformedDate s) in

    let year, month, day, i =
      if not (Str.string_match datere s 0) then failure ();

      let year =  int_of_string (Str.matched_group 1 s) in
      let month = int_of_string (Str.matched_group 2 s) in
      let day   = int_of_string (Str.matched_group 3 s) in

      if not (validate_date ~year ~month ~day) then
        failure();
      (year, month, day, Str.match_end ()) in

    let hour, minute, second, i =
      if i = String.length s then (None, None, None, i) else begin
        if s.[i] <> 'T' || not (Str.string_match timere s (i+1)) then
          failure ();
        let hour   = int_of_string (Str.matched_group 1 s) in
        let minute = int_of_string (Str.matched_group 2 s) in
        let second =
          try  Some (int_of_string (Str.matched_group 4 s))
          with Not_found -> None in

        if not (validate_time ?second ~hour ~minute ()) then
          failure ();
        (Some hour, Some minute, second, Str.match_end ())
      end in

    let timezone, i =
      if i = String.length s then (None, i) else begin
        if String.sub s i (String.length s - i) = "Z" then
          (Some TZZ, String.length s)
        else
          match s.[i] with
          | c when c = '+' || c = '-' ->
            if not (Str.string_match tzre s (i+1)) then
              failure ();
            let tzh = int_of_string (Str.matched_group 1 s) in
            let tzm = int_of_string (Str.matched_group 2 s) in
            if not (validate_time ~hour:tzh ~minute:tzm ()) then
              failure ();
            let tz = if c = '+' then TZplus(tzh, tzm) else TZminus (tzh, tzm) in
            (Some tz, Str.match_end ())
          | _ -> failure ()
      end in

    if i <> String.length s then failure ();

    mk_date ~year ~month ~day ?hour ?minute ?second ?timezone ()

(* let date_to_big_int (date : date) : big_int =
   Big_int.zero_big_int
   |> fun x -> Big_int.mult_int_big_int 12 (Big_int.add_big_int x (Big_int.big_int_of_int date.year))
   |> fun x -> Big_int.mult_int_big_int 31 (Big_int.add_big_int x (Big_int.big_int_of_int date.month))
   |> fun x -> Big_int.mult_int_big_int 24 (Big_int.add_big_int x (Big_int.big_int_of_int date.day))
   |> fun x -> Big_int.mult_int_big_int 60 (Big_int.add_big_int x (Big_int.big_int_of_int date.hour))
   |> fun x -> Big_int.mult_int_big_int 60 (Big_int.add_big_int x (Big_int.big_int_of_int date.minute))
   |> fun x -> Big_int.mult_int_big_int 1  (Big_int.add_big_int x (Big_int.big_int_of_int date.second))
   |> fun x ->
   begin
    let f (h, m) =
      Big_int.zero_big_int
      |> Big_int.add_big_int (Big_int.mult_int_big_int (60 * 60) (Big_int.big_int_of_int h))
      |> Big_int.add_big_int (Big_int.mult_int_big_int 60 (Big_int.big_int_of_int m))
    in
    let c =
      match date.timezone with
      | TZnone
      | TZZ -> Big_int.zero_big_int
      | TZplus (h, m) ->
        f (h, m)
        |> Big_int.minus_big_int
      | TZminus (h, m) ->
        f (h, m)
    in
    Big_int.add_big_int x c
   end *)


(** inspired from
    https://discuss.ocaml.org/t/how-to-expose-date-time-types-in-a-library-nicely/1653/6 *)

(** [is_leapyear] is true, if and only if a year is a leap year *)
let is_leapyear year =
  year mod 4    = 0
  &&  year mod 400 != 100
  &&  year mod 400 != 200
  &&  year mod 400 != 300

let ( ** ) x y    = Big_int.mult_int_big_int (x) y
let sec           = Big_int.unit_big_int
let sec_per_min   = 60 ** sec
let sec_per_hour  = 60 ** sec_per_min
let sec_per_day   = 24 ** sec_per_hour

(* The following calculations are based on the following book: Nachum
   Dershowitz, Edward M. Reingold: Calendrical calculations (3. ed.).
   Cambridge University Press 2008, ISBN 978-0-521-88540-9, pp. I-XXIX,
   1-479, Chapter 2, The Gregorian Calendar *)

let days_since_epoch yy mm dd =
  let epoch       = 1       in
  let y'          = yy - 1  in
  let correction  =
    if mm <= 2                          then 0
    else if mm > 2 && is_leapyear yy    then -1
    else -2
  in
  epoch - 1 + 365*y' + y'/4 - y'/100 + y'/400 +
  (367 * mm - 362)/12 + correction + dd

let seconds_since_epoch d =
  let ( ++ )        = Big_int.add_big_int in
  (days_since_epoch d.year d.month d.day ** sec_per_day)
  ++ (d.hour ** sec_per_hour)
  ++ (d.minute  ** sec_per_min)
  ++ (d.second  ** sec)

let epoch = seconds_since_epoch (mk_date ())

let date_to_timestamp (date : date) : big_int =
  let res = seconds_since_epoch date in
  let res = Big_int.sub_big_int res epoch in
  let correction =
    let f (h, m) =
      Big_int.zero_big_int
      |> Big_int.add_big_int (Big_int.mult_int_big_int (60 * 60) (Big_int.big_int_of_int h))
      |> Big_int.add_big_int (Big_int.mult_int_big_int 60 (Big_int.big_int_of_int m))
    in
    match date.timezone with
    | TZnone
    | TZZ -> Big_int.zero_big_int
    | TZplus (h, m) ->
      f (h, m)
      |> Big_int.minus_big_int
    | TZminus (h, m) ->
      f (h, m)
  in
  Big_int.add_big_int res correction

type tzkind =
  | Ktz
  | Kmtz
  | Kutz

let string_to_big_int_tz kind input =
  let b =
    match kind with
    | Ktz  -> 1000000
    | Kmtz -> 1000
    | Kutz -> 1
  in
  if String.contains input '.'
  then begin
    let n, d = decimal_string_to_rational input in
    let n, d = compute_irr_fract (Big_int.mult_int_big_int b n, d) in
    if Big_int.eq_big_int Big_int.unit_big_int d
    then n
    else begin
      let module E = struct exception Invalid end in
      raise E.Invalid
    end
  end
  else Big_int.mult_int_big_int b (Big_int.big_int_of_string input)

let string_to_big_int_percent input =
  let n, d =
    if String.contains input '.'
    then decimal_string_to_rational input
    else Big_int.big_int_of_string input, Big_int.unit_big_int
  in
  compute_irr_fract (n, Big_int.mult_int_big_int 100 d)

let is_valid_string v =
  let rec check_printable_ascii i =
    if (i < 0) then true
    else
      match v.[i] with
      | '\n' | '\x20' .. '\x7E' ->
        check_printable_ascii (i - 1)
      | _ ->
        false
  in
  check_printable_ascii (String.length v - 1)

let is_valid_path path =
  Sys.file_exists path
