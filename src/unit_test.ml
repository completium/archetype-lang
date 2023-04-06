open Alcotest
open Archetype

(* Define your test cases here *)
let test_string_to_duration () =
  let expected : string = Core.mk_duration
   ~weeks:(Big_int.big_int_of_int 3)
   ~days:(Big_int.big_int_of_int 8)
   ~hours:(Big_int.big_int_of_int 4)
   ~minutes:(Big_int.big_int_of_int 34)
   ~seconds:(Big_int.big_int_of_int 18)
    () |> Core.duration_to_timestamp |> Core.big_int_to_string
     in
  let actual : string = Core.string_to_duration "3w8d4h34m18s" |> Core.duration_to_timestamp |> Core.big_int_to_string in
  Alcotest.(check string) "big_int equality" expected actual
  (* (let _ = Core.cmp_duration actual expected in ()) *)
  (* check string "same duration" expected actual *)
  (* let result = 2 + 2 in *)
  (* check int "result should be 4" 4 result *)

(* List your test cases here *)
let suite_core = [
  "Test string_to_duration", `Quick, test_string_to_duration
]

(* Run the test suite *)
let () =
  run "Test Archetype Suite" [
    "Test Core", suite_core;
  ]
