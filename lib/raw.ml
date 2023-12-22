type raw =
| Var of int
| Fun of raw * raw
| Lam of raw
| App of raw * raw
| Sig of raw * raw
| Gen of raw * raw
| PrL of raw
| PrR of raw
| Typ of int
| Hole

let rec string_of_raw = function
| Var i -> "(Var " ^ string_of_int i ^ ")"
| Fun (a, b) -> "(Fun " ^ string_of_raw a ^ " " ^ string_of_raw b ^ ")"
| Lam a -> "(Lam " ^ string_of_raw a ^ ")"
| App (f, a) -> "(App " ^ string_of_raw f ^ " " ^ string_of_raw a ^ ")"
| Sig (a, b) -> "(Sig " ^ string_of_raw a ^ " " ^ string_of_raw b ^ ")"
| Gen (a, b) -> "(Gen " ^ string_of_raw a ^ " " ^ string_of_raw b ^ ")"
| PrL a -> "(PrL " ^ string_of_raw a ^ ")"
| PrR b -> "(PrL " ^ string_of_raw b ^ ")"
| Typ i -> "(Typ " ^ string_of_int i ^ ")"
| Hole -> "Hole"

type ctx =
| Nil
| Snoc of ctx * raw

let rec string_of_ctx = function
| Nil -> "Nil"
| Snoc (g, a) -> "(Snoc " ^ string_of_ctx g ^ " " ^ string_of_raw a ^ ")"

let rec shift c = function
| Var i ->
    if i < c then
        Var i
    else
        Var (i + 1)
| Fun (a, b) -> Fun (shift c a, shift (c + 1) b)
| Lam e -> Lam (shift (c + 1) e)
| App (f, a) -> App (shift c f, shift c a)
| Sig (a, b) -> Sig (shift c a, shift (c + 1) b)
| Gen (a, b) -> Gen (shift c a, shift c b)
| PrL a -> PrL (shift c a)
| PrR a -> PrR (shift c a)
| Typ i -> Typ i
| Hole -> Hole

let rec shift_ctx c = function
| Nil -> Nil
| Snoc (g, a) -> Snoc (shift_ctx c g, shift c a)

let snoc g a = shift_ctx 0 (Snoc (g, a))

let rec subst e i x = match e with
| Var j ->
    if i = j then
        x
    else if j < i then
        Var j
    else
        Var (j - 1)
| Fun (a, b) -> Fun (subst a i x, subst b (i + 1) (shift 0 x))
| Lam a -> Lam (subst a (i + 1) (shift 0 x))
| App (f, a) -> App (subst f i x, subst a i x)
| Sig (a, b) -> Sig (subst a i x, subst b (i + 1) (shift 0 x))
| Gen (a, b) -> Gen (subst a i x, subst b i x)
| PrL a -> PrL (subst a i x)
| PrR a -> PrR (subst a i x)
| Typ i -> Typ i
| Hole -> Hole

exception Stuck

let rec step = function
| Var _ | Fun _ | Lam _ | Typ _ | Sig _ | Hole | Gen _ -> raise Stuck
| App (Lam b, a) -> subst b 0 a
| App (f, a) -> App (step f, a)
| PrL (Gen (a, _)) -> a
| PrR (Gen (_, a)) -> a
| PrL a -> PrL (step a)
| PrR a -> PrR (step a)

let rec norm e =
    try norm (step e)
    with Stuck -> e