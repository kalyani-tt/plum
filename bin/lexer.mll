{
    open Grammar
    exception Error of string
}

rule token = parse
| "." { DOT }
| "U" (['0'-'9']+ as i) { UNIV (int_of_string i) }
| "\\" { FUN }
| [' ' '\t' '\n' '\r'] { token lexbuf }
| ['a'-'z' 'A'-'Z']['0'-'9' 'a'-'z' 'A'-'Z']* as s { NAME s }
| "->" { ARROW }
| ":" { COLON }
| '(' { LPAREN }
| ')' { RPAREN }
| eof { EOF }
| _ { raise (Error (Printf.sprintf "At offset %d: unexpected character.\n" (Lexing.lexeme_start lexbuf))) }