module Minits.Transform
open Types
let transformStatement = function
| ExpressionStatement(_) as e -> e
| Var(name, _, init) -> Var(name, None, init)
let transform = List.map transformStatement