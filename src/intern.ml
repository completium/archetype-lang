let str_to_model buff =
  try
    Parser.main Lexer.token buff
  with
  | Lexer.Error c ->
     Printf.fprintf stderr ("Illegal character (%c)\n%!") c;
     exit (-1)
  | Parser.Error ->
     Printf.fprintf stderr ("Syntax error\n%!");
     exit (-1)
