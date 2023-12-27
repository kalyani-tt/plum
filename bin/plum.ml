open Plum_core.Toplevel
open Plum_core.Proof_state

let doc =
"
load_plugin zarith
"

let () = interpret doc init_state