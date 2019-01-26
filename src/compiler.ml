(* -------------------------------------------------------------------- *)
open CmlCore

(* -------------------------------------------------------------------- *)
let parse_and_print (filename, channel) =
  let model = CmlIo.parse_model ~name:filename channel in
  Format.printf "%a@." CmlPrint.pp_model model

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
      parse_and_print (filename, channel)

  with CmlParseUtils.ParseError exn ->
    Format.eprintf "%a@." CmlParseUtils.pp_parse_error exn;
    exit 1

(* -------------------------------------------------------------------- *)
let _ = main ()
