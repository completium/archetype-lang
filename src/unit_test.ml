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

let test_string_to_duration_36s () =
  let expected : string = Core.mk_duration
      ~seconds:(Big_int.big_int_of_int 36)
      () |> Core.duration_to_timestamp |> Core.big_int_to_string
  in
  let actual : string = Core.string_to_duration "36s" |> Core.duration_to_timestamp |> Core.big_int_to_string in
  Alcotest.(check string) "big_int equality" expected actual

type type_tree =
  | TL of string
  | TN of (type_tree * type_tree)
[@@deriving yojson, show {with_path = false}]

let rec pprint_type_tree ppf =  function
  | TL v -> Fmt.pf ppf "%s" v
  | TN (x, y) -> Fmt.pf ppf "[%a; %a]" pprint_type_tree x pprint_type_tree y

(* let rec pprint_type_tree ppf =  function
  | TL v -> Fmt.pf ppf "TL \"%s\"" v
  | TN (x, y) -> Fmt.pf ppf "TN (%a, %a)" pprint_type_tree x pprint_type_tree y *)

let rec type_tree_eq x y =
  match x, y with
  | TL x, TL y -> String.equal x y
  | TN (x1, y1), TN (x2, y2) -> type_tree_eq x1 x2 && type_tree_eq y1 y2
  | _, _ -> false

let testable_type_tree : type_tree Alcotest.testable =
  let module M = struct
    type t = type_tree

    let equal = type_tree_eq

    let pp = pprint_type_tree

  end in
  (module M : Alcotest.TESTABLE with type t = M.t)

let test_shape_entrypoints _ =
  let ttvoid = TL "0" in
  let f x = Archetype.Gen_michelson.shape_entrypoints (fun x y -> TN (x, y)) ttvoid (List.map (fun x -> TL x) x) in

  let data_shape_entrypoints =
    [
      (* [0] *)
      TL "0";
      (* [a] *)
      TL "a";
      (* [a; b] *)
      TN (TL "a", TL "b");
      (* [[a; b]; c] *)
      TN (TN (TL "a", TL "b"), TL "c");
      (* [[a; b]; [c; d]] *)
      TN (TN (TL "a", TL "b"), TN (TL "c", TL "d"));
      (* [[[a; b]; [c; d]]; e] *)
      TN (TN (TN (TL "a", TL "b"), TN (TL "c", TL "d")), TL "e");
      (* [[a; b]; [[c; d]; [e; f]]] *)
      TN (TN (TL "a", TL "b"), TN (TN (TL "c", TL "d"), TN (TL "e", TL "f")));
      (* [[[a; b]; [c; d]]; [[e; f]; g]] *)
      TN (TN (TN (TL "a", TL "b"), TN (TL "c", TL "d")), TN (TN (TL "e", TL "f"), TL "g"));
      (* [[[a; b]; [c; d]]; [[e; f]; [g; h]]] *)
      TN (TN (TN (TL "a", TL "b"), TN (TL "c", TL "d")), TN (TN (TL "e", TL "f"), TN (TL "g", TL "h")));
      (* [[[[a; b]; [c; d]]; [[e; f]; [g; h]]]; i] *)
      TN (TN (TN (TN (TL "a", TL "b"), TN (TL "c", TL "d")), TN (TN (TL "e", TL "f"), TN (TL "g", TL "h"))), TL "i");
      (* [[[a; b]; [c; d]]; [[e; f]; [[g; h]; [i; j]]]] *)
      TN ((TN (TN (TL "a", TL "b"), TN (TL "c", TL "d"))), TN (TN (TL "e", TL "f"), TN (TN (TL "g", TL "h"), TN (TL "i", TL "j"))));
      (* [[[[a; b]; [c; d]]; [[e; f]; [g; h]]]; [[i; j]; k]] *)
      TN (TN (TN (TN (TL "a", TL "b"), TN (TL "c", TL "d")), TN (TN (TL "e", TL "f"), TN (TL "g", TL "h"))), TN (TN (TL "i", TL "j"), TL "k"));
      (* [[[a; b]; [c; d]]; [[[e; f]; [g; h]]; [[i; j]; [k; l]]]] *)
      TN (TN (TN (TL "a", TL "b"), TN (TL "c", TL "d")), TN (TN (TN (TL "e", TL "f"), TN (TL "g", TL "h")), TN (TN (TL "i", TL "j"), TN (TL "k", TL "l"))));
      (* [[[[a; b]; [c; d]]; [[e; f]; [g; h]]]; [[[i; j]; [k; l]]; m]] *)
      TN (TN (TN (TN (TL "a", TL "b"), TN (TL "c", TL "d")), TN (TN (TL "e", TL "f"), TN (TL "g", TL "h"))), TN (TN (TN (TL "i", TL "j"), TN (TL "k", TL "l")), TL "m"));
      (* [[[a; b]; [c; d]]; [[[e; f]; [g; h]]; [[i; j]; [[k; l]; [m; n]]]]] *)
      TN (TN (TN (TL "a", TL "b"), TN (TL "c", TL "d")), TN (TN (TN (TL "e", TL "f"), TN (TL "g", TL "h")), TN (TN (TL "i", TL "j"), TN (TN (TL "k", TL "l"), TN (TL "m", TL "n")))));
      (* [[[[a; b]; [c; d]]; [[e; f]; [g; h]]]; [[[i; j]; [k; l]]; [[m; n]; o]]] *)
      TN (TN (TN (TN (TL "a", TL "b"), TN (TL "c", TL "d")), TN (TN (TL "e", TL "f"), TN (TL "g", TL "h"))), TN (TN (TN (TL "i", TL "j"), TN (TL "k", TL "l")), TN (TN (TL "m", TL "n"), TL "o")))
    ]
  in
  let rec generate_list n =
    if n <= 0 then []
    else
      let letter = String.make 1 (Char.chr (96 + n)) in
      generate_list (n - 1) @ [letter]
  in
  List.iteri (fun i d -> Alcotest.(check testable_type_tree) "test_shape_entrypoints" d (f (generate_list i))) data_shape_entrypoints

(* List your test cases here *)
let suite_core = [
  "Test string_to_duration_3w8d4h34m18s", `Quick, test_string_to_duration;
  "Test string_to_duration_36s", `Quick, test_string_to_duration_36s;
  "Test test_shape_entrypoints", `Quick, test_shape_entrypoints
]

(* Run the test suite *)
let () =
  run "Test Archetype Suite" [
    "Test Core", suite_core;
  ]
