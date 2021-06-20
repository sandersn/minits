module Minits.Transform
open Types
let transformDeclaration = function
| ExpressionStatement(_) as e -> e
| Var(name, _, init) -> Var(name, None, init)
| Type _ as t -> t // TODO: Delete, I guess? (This step is typescript-only actually)
| Function _ as t -> t // TODO: Delete types
let transform = List.map transformDeclaration