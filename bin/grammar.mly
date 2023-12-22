%{
    open Ast
    open Plum_core
%}

%token COLON
%token LPAREN
%token RPAREN
%token ARROW
%token EOF
%token <string> NAME
%token <int> UNIV
%token FUN
%token DOT

%start <Raw.raw> main

%%

main:
| ds=toplevel { desugar_top [] ds }

prec0:
| n=NAME { Var n }
| i=UNIV { Typ i }
| LPAREN e=prec3 RPAREN { e }

prec1:
| f=prec0 xs=prec0* { List.fold_right (fun x acc -> App (acc, x)) xs f }

prec2:
| a=prec1 ARROW b=prec2 { Fun ("_", a, b) }
| a=prec1 { a }

prec3:
| FUN n=NAME ARROW e=prec3 { Lam (n, e) }
| LPAREN n=NAME COLON a=prec3 RPAREN ARROW b=prec3 { Fun (n, a, b) }
| e=prec2 { e }

toplevel:
| n=NAME COLON a=prec3 DOT ps=toplevel { (n, a)::ps }
| EOF { [] }