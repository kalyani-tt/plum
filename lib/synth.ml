open Raw
open Verify

let ( let* ) xs k = List.concat (List.map k xs)

let rec intro g = function
| Fun (a, b) ->
    let* e = intro (snoc g a) b in
    [Lam e]
(* | Sig (a, b) ->
    let* x = intro g *)
