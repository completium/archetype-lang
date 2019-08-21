open Archetype

let test_parse_file dir filename () =
  let path = "./" ^ dir ^ "/" ^ filename in
  let channel = open_in path in
  let pt = Io.parse_archetype_strict ~name:filename channel in
  let ref = Format.asprintf "%a@." ParseTree.pp_archetype pt in
  let input = Format.asprintf "%a@." Printer_pt.pp_archetype pt in
  let pt2 = Io.parse_archetype_strict_from_string ~name:filename input in
  let my = Format.asprintf "%a@." ParseTree.pp_archetype pt2 in
  Alcotest.(check bool) ("test parse: " ^ filename) (String.equal ref my) true

(* Run it *)
let () =
  let open Alcotest in
  let generate_test_cases dir =
    let l = Sys.readdir ("./" ^ dir ^ "/") |> Array.to_list in
    List.map (fun file -> test_case file `Quick (test_parse_file dir file)) l
  in
  let dirs = ["contracts"; "tests"; "extensions"] in
  let rl = List.map (fun x -> (x, generate_test_cases x)) dirs in
  run "Utils" rl
