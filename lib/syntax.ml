type term =
| Var of int
| Fun of term * term
| Lam of term
| App of term * term
| Sig of term * term
| Gen of term * term
| PrL of term
| PrR of term
| Typ of int
| Hole of int

let rec string_of_term = function
| Var i -> "(Var " ^ string_of_int i ^ ")"
| Fun (a, b) -> "(Fun " ^ string_of_term a ^ " " ^ string_of_term b ^ ")"
| Lam a -> "(Lam " ^ string_of_term a ^ ")"
| App (f, a) -> "(App " ^ string_of_term f ^ " " ^ string_of_term a ^ ")"
| Sig (a, b) -> "(Sig " ^ string_of_term a ^ " " ^ string_of_term b ^ ")"
| Gen (a, b) -> "(Gen " ^ string_of_term a ^ " " ^ string_of_term b ^ ")"
| PrL a -> "(PrL " ^ string_of_term a ^ ")"
| PrR b -> "(PrL " ^ string_of_term b ^ ")"
| Typ i -> "(Typ " ^ string_of_int i ^ ")"
| Hole i -> "(Hole " ^ string_of_int i ^ ")"

type ctx =
| Nil
| Snoc of ctx * term

let rec string_of_ctx = function
| Nil -> "Nil"
| Snoc (g, a) -> "(Snoc " ^ string_of_ctx g ^ " " ^ string_of_term a ^ ")"

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
| Hole i -> Hole i

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
| Hole i -> Hole i

exception Stuck

let rec step = function
| Var _ | Fun _ | Lam _ | Typ _ | Sig _ | Hole _ | Gen _ -> raise Stuck
| App (Lam b, a) -> subst b 0 a
| App (f, a) -> App (step f, a)
| PrL (Gen (a, _)) -> a
| PrR (Gen (_, a)) -> a
| PrL a -> PrL (step a)
| PrR a -> PrR (step a)

let rec norm e =
    try norm (step e)
    with Stuck -> e