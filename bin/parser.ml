let parse_file filename =
  let input = open_in filename in
  let lexbuf = Lexing.from_channel input in
  try
    let e = Grammar.main Lexer.token lexbuf in
    close_in input;
    Ok e
  with
  | Lexer.Error msg -> close_in input; Error (Printf.sprintf "%s%!" msg)
  | Grammar.Error -> close_in input; Error (Printf.sprintf "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf))
  | Ast.NoVar s -> close_in input; Error ("No variable '" ^ s ^ "'")