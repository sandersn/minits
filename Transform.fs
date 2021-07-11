module Minits.Transform
open Types
let rec transformDeclaration = function
| File decls -> List.map transformDeclaration decls |> File
| ExpressionStatement(_) as e -> e
| Var(name, _, init) -> Var(name, None, init)
| Declaration.Type _ as t -> t // TODO: Delete, I guess? (This step is typescript-only actually)
| Function _ as t -> t // TODO: Delete types
let transform = transformDeclaration