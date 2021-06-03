module Minits.Emit
open Types
let rec emitExpression = function
| Identifier(name) -> name
| IntLiteral(value) -> string value
| Assignment(name, value) -> sprintf "%s = %s" name (emitExpression value)
let emitStatement = function
| ExpressionStatement(e) -> emitExpression e
| Var(name, typename, init) -> 
  let typestring = match typename with
                   | Some(name) -> ": " + name
                   | None -> ""
  sprintf "var %s%s = %s" name typestring (emitExpression init)
let emit = List.map emitStatement >> String.concat "\n"