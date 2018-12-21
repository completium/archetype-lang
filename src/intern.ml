(* -------------------------------------------------------------------- *)
let str_to_model buff =
  try
    Parser.main Lexer.token buff
  with Parseutils.ParseError exn ->
    Format.eprintf "%a@." Parseutils.pp_parse_error exn;
    exit 1
