type 'a state_key = ..

type proof_state

type command = proof_state -> string -> string

val get_cmds : proof_state -> (string, command) Hashtbl.t

val set_cmd : proof_state -> string -> command -> unit

val get_state : proof_state -> 'a state_key -> 'a

val set_state : proof_state -> 'a state_key -> 'a -> unit

exception Plugin of (proof_state -> unit)

val init_state : proof_state