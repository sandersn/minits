module Minits.Emit
open Types
let emitType t = sprintf "%A" t
let emitLValue = function
| Identifier(name) -> name
| PropertyAccess _ -> "Properties do not emit yet"
| LValue.ArrayAccess _ -> "Arrays do not emit yet"
let rec emitExpression = function
| LValue(lvalue) -> emitLValue lvalue
| IntLiteral(value) -> string value
| StringLiteral(value) -> sprintf "%A" value
| Negative(e) -> "-" + emitExpression e
| Binary (l,op,r) -> "binary expressions do not emit yet"
| Assignment(name, value) -> sprintf "%s = %s" (emitLValue name) (emitExpression value)
| Call(name, parameters) -> "calls do not emit yet"
| Sequence es -> "sequences do not emit yet"
| RecordCons _ -> "records don't emit yet"
| ArrayCons _ -> "arrays don't emit yet"
| If _ -> "if doesn't emit yet"
| For _ -> "for doesn't emit yet"
| While _ -> "while doesn't emit yet"
| Let _ -> "let doesn't emit yet"
| Break -> "break"
| Null -> "null"
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