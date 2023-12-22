open Raw

exception NoConv of raw * raw
exception NotFun of raw
exception NotSig of raw
exception NotTyp of raw
exception NoInfer of raw

let print_errors f =
    try f () with
    | NoConv (x, y) -> print_endline (string_of_raw x ^ " !<= " ^ string_of_raw y)
    | NotFun a -> print_endline ("not a function type: " ^ string_of_raw a)
    | NotSig a -> print_endline ("not a tuple type: " ^ string_of_raw a)
    | NotTyp a -> print_endline ("not a type: " ^ string_of_raw a)
    | NoInfer x -> print_endline ("cannot infer type of: " ^ string_of_raw x)

let rec lookup i = function
| Nil -> failwith "ill-scoped"
| Snoc (g, a) ->
    if i = 0 then
        a
    else
        lookup (i - 1) g

let exp_typ = function
| Typ i -> i
| a -> raise (NotTyp a)

let compatible exp inf = match exp, inf with
| Typ i, Typ j -> j <= i
| _, _ -> exp = inf

let rec check g tm ty = match tm, ty with
| Lam e, Fun (a, b) ->
    check (shift_ctx 0 (Snoc (g, a))) e b
| Gen (x, y), Sig (a, b) ->
    check g x a;
    check g y (subst b 0 x)
| _, _ ->
    let ty2 = infer g tm in
    if compatible (norm ty) (norm ty2) then
        ()
    else
        raise (NoConv (ty2, ty))

and infer g = function
| Var i -> lookup i g
| Fun (a, b) ->
    let i = exp_typ (infer g a) in
    let j = exp_typ (infer (shift_ctx 0 (Snoc (g, a))) b) in
    Typ (max i j)
| App (f, x) ->
    begin match infer g f with
    | Fun (a, b) ->
        check g x a;
        subst b 0 x
    | ty -> raise (NotFun ty)
    end
| Sig (a, b) ->
    let i = exp_typ (infer g a) in
    let j = exp_typ (infer (shift_ctx 0 (Snoc (g, a))) b) in
    Typ (max i j)
| PrL x ->
    begin match infer g x with
    | Sig (a, _) -> a
    | ty -> raise (NotSig ty)
    end
| PrR x ->
    begin match infer g x with
    | Sig (_, b) -> subst b 0 (PrL x)
    | ty -> raise (NotSig ty)
    end
| Typ i -> Typ (i + 1)
| tm -> raise (NoInfer tm)