open Location
open Tools
open ParseTree

let check_json (pt : archetype) =
  let process (input : string loced) =
    let location, input = deloc input in
    try
      let _ = Yojson.Raw.from_string input in
      ()
    with
      Yojson.Json_error str -> begin
        (* let regexp = Str.regexp "Line \\([0-9]+\\), bytes \\([0-9]+\\)-\\([0-9]+\\)" in

           let _n = Str.search_forward regexp str 0 in
           let a = Str.matched_group 1 str |> int_of_string in
           let b = Str.matched_group 2 str |> int_of_string in
           let c = Str.matched_group 3 str |> int_of_string in

           let pos = Position.mk_position "fname" (a, b, b) (a, c, c) in *)

        (* let s = Str.bounded_full_split regexp str in
           Format.eprintf "%a@\n" (Printer_tools.pp_list ", " (fun fmt str -> Format.fprintf fmt "%s" str)) s; *)

        let str = String.concat "" [str; "\n"] in
        let poss : Position.t list = [location_to_position location] in
        Error.errors := (poss, str)::!Error.errors
      end
  in

  match unloc pt with
  | Marchetype ds -> begin
      List.iter (fun d ->
          match unloc d with
          | Darchetype (_, _, Some Mjson data, _) -> process data
          | _ ->  ()) ds
    end
  | _ -> ()
