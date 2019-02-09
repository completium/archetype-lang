(* -------------------------------------------------------------------- *)
open Core

(* -------------------------------------------------------------------- *)
let parse_and_extract (filename, channel) =
  let pt = Io.parse_model ~name:filename channel in
  let model = Translate.parseTree_to_model pt in
  (*  print_endline @@ show_magic @@ model*)
  Format.printf "%a\n" Model.pp_model model

(* -------------------------------------------------------------------- *)
let main () =
  try
    let filename, channel, dispose =
      if Array.length Sys.argv > 1 then
        let filename = Sys.argv.(1) in
        (filename, open_in filename, true)
      else ("<stdin>", stdin, false)
    in

    finally
      (fun () -> if dispose then close_in channel)
      parse_and_extract (filename, channel)

  with ParseUtils.ParseError exn ->
    Format.eprintf "%a@." ParseUtils.pp_parse_error exn;
    exit 1

(* -------------------------------------------------------------------- *)
let _ = main ()
