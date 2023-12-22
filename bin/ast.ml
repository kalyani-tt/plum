open Plum_core

type ast =
| Var of string
| Typ of int
| App of ast * ast
| Lam of string * ast
| Fun of string * ast * ast

exception NoVar of string

let rec lookup s = function
| [] -> raise (NoVar s)
| n::ns -> if s = n then 0 else 1 + lookup s ns

let rec desugar env = function
| Var s -> Raw.Var (lookup s env)
| Typ i -> Raw.Typ i
| App (f, x) -> Raw.App (desugar env f, desugar env x)
| Lam (n, e) -> Raw.Lam (desugar (n::env) e)
| Fun (n, a, b) -> Raw.Fun (desugar env a, desugar (n::env) b)

let rec desugar_top env = function
| [] -> Raw.Typ 0
| (n, a)::ds -> Raw.Sig (desugar env a, desugar_top (n::env) ds)