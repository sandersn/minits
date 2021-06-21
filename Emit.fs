module Minits.Emit
open Types
let emitType t = sprintf "%A" t
let emitLValue = function
| Identifier(name) -> name
| Property _ -> "Properties do not emit yet"
| Array _ -> "Arrays do not emit yet"
let rec emitExpression = function
| LValue(lvalue) -> emitLValue lvalue
| IntLiteral(value) -> string value
| Assignment(name, value) -> sprintf "%s = %s" (emitLValue name) (emitExpression value)
let emitDeclaration = function
| ExpressionStatement(e) -> emitExpression e
| Var(name, t, init) -> 
  let typestring = match t with
                   | Some(name) -> ": " + emitType name
                   | None -> ""
  sprintf "var %s%s = %s" name typestring (emitExpression init)
| Type _ -> "Types do not emit yet"
| Function _ -> "Functions do not emit yet"
let emit = List.map emitDeclaration >> String.concat "\n"