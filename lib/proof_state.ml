open Syntax
open Parsers

type 'a state_key = ..

type proof_state = {
    cmds : (string, command) Hashtbl.t;
    states : (unit, unit) Hashtbl.t
}
and command = proof_state -> string -> string

let get_cmds st = st.cmds

let set_cmd st name cmd = Hashtbl.replace st.cmds name cmd

let get_state st k =
    Obj.magic (Hashtbl.find st.states (Obj.magic k))

let set_state st k v =
    Hashtbl.replace st.states (Obj.magic k) (Obj.magic v)

exception Plugin of (proof_state -> unit)

let load_plugin st input =
    let r = take_while (fun s -> s <> '\n') input in
    let pkg_name = String.trim (fst r) in
    let obj_name = Dynlink.adapt_filename (pkg_name ^ ".cma") in
    let dir = Findlib.package_directory pkg_name in
    let path' = dir ^ "/" ^ obj_name in
    begin try
        Dynlink.loadfile path';
        failwith "no plugin to load"
    with
    | Plugin f -> f st
    end;
    snd r

let prims = [
    ("load_plugin", load_plugin)
]

let init_state : proof_state = {
    cmds =
        (let tbl = Hashtbl.create ~random:false 1 in
        List.iter (fun (name, def) -> Hashtbl.replace tbl name def) prims;
        Obj.magic tbl);
    states = Hashtbl.create ~random:false 10
}