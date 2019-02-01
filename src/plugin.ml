(* -------------------------------------------------------------------- *)
open Why3
open CmlCore

let mk_loadpath main = (Whyconf.loadpath main)

let config = Whyconf.read_config None
let main = Whyconf.get_main config
let env = Env.create_env (mk_loadpath main)

(* -------------------------------------------------------------------- *)
let main () =
  try
    let path, filename, channel, dispose =
      if Array.length Sys.argv > 1 then
        let filename = Sys.argv.(1) in
        (filename, filename, open_in filename, true)
      else ("path", "<stdin>", stdin, false)
    in

    finally
      (fun () -> if dispose then close_in channel)
      Cml_main.read_channel env path filename channel

  with CmlParseUtils.ParseError exn ->
    Format.eprintf "%a@." CmlParseUtils.pp_parse_error exn;
    exit 1

(* -------------------------------------------------------------------- *)
let _ = main ()
