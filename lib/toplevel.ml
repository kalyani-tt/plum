open Parsers
open Proof_state

let is_alnum = function
| '0'..'9' | 'a'..'z' | 'A'..'Z' -> true
| _ -> false

let is_space = function
| ' ' | '\n' | '\t' | '\r' -> true
| _ -> false

let rec interpret (s : string) (st : proof_state) =
    let (_, s) = take_while is_space s in
    let (name, s) = take_while (fun c -> not (is_space c)) s in
    let cmd = Hashtbl.find (get_cmds st) name in
    let s = cmd st s in
    interpret s st