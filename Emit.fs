module Minits.Emit
open Types
let emitType t = sprintf "%A" t
let rec emitExpression = function
| Expression.Identifier(name) -> name
| IntLiteral(value) -> string value
| Assignment(name, value) -> sprintf "%s = %s" name (emitExpression value)
let emitDeclaration = function
| ExpressionStatement(e) -> emitExpression e
| Var(name, t, init) -> 
  let typestring = match t with
                   | Some(name) -> ": " + emitType name
                   | None -> ""
  sprintf "var %s%s = %s" name typestring (emitExpression init)
| Type _ -> "Types do not emit yet"
let emit = List.map emitDeclaration >> String.concat "\n"