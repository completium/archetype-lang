open Why3

let mk_loadpath main = (Whyconf.loadpath main)

let parse_and_print (filename, channel) =
  let config = Whyconf.read_config None in
  let main = Whyconf.get_main config in
  let env = Env.create_env (mk_loadpath main) in
  let _mm = Env.read_channel Pmodule.mlw_language env filename channel in
  print_endline "ok";
  (*  Format.printf "%a@." mm;*)
  ()
(*Format.printf "%a@." Ptree.pp_mlw pt*)

let main () =
      let filename, channel, dispose =
      if Array.length Sys.argv > 1 then
        let filename = Sys.argv.(1) in
        (filename, open_in filename, true)
      else ("<stdin>", stdin, false)
      in

      (*    finally*)
      (*      (fun () -> if dispose then close_in channel)*)
      parse_and_print (filename, channel);
      if dispose then close_in channel

let _ = main()
