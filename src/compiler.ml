
open Intern
open Print

let _ =
  let stdinbuf = Lexing.from_channel stdin in
  let model = str_to_model stdinbuf in
  let output = model_to_str model in
  print_string output
