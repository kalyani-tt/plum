open Plum_core

let () =
    match Sys.getenv_opt "PLUM_INIT" with
    | Some init_path ->
        begin match Parser.parse_file init_path with
        | Ok init ->
            print_endline "Loaded init file"
        | Error e -> print_endline ("Could not parse init file: " ^ e)
        end
    | None -> print_endline "PLUM_INIT not set"