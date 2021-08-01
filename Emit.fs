module Minits.Emit
open Types
let emitTypeAnnotation = function
| Some t -> ": " + Check.typeToString t
| _ -> ""
// TODO: This is duplicated in the checker
let emitProperty (name, t) = sprintf "%s: %s" name (Check.typeToString t)
let emitToken = function
| Pipe -> "|"
| Ampersand -> "&"
| Plus -> "+"
| Minus -> "-"
| Asterisk -> "*"
| ForwardSlash -> "/"
| LessThan -> "<"
| GreaterThan -> ">"
| LessThanEquals -> "<="
| GreaterThanEquals -> ">="
| DoubleEquals -> "=="
| ForwardSlashEquals -> "/="
| t -> failwith $"Unexpected binary operator token {t}"
let rec emitLValue = function
| Identifier(name) -> name
| PropertyAccess (l,r) -> $"{emitLValue l}.{r}"
| ArrayAccess (l,r) -> sprintf "%s[%s]" (emitLValue l) (emitExpression r)
and emitExpression = function
| LValue(lvalue) -> emitLValue lvalue
| IntLiteral(value) -> string value
| StringLiteral(value) -> sprintf "%A" value
| Negative(e) -> "-" + emitExpression e
| Binary (l,op,r) -> sprintf "%s %s %s" (emitExpression l) (emitToken op) (emitExpression r)
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
and emitDeclaration = function
| File decls -> decls |> List.map emitDeclaration |> String.concat "\n"
| ExpressionStatement e -> emitExpression e
| Var(name, t, init) -> 
  sprintf "var %s%s = %s" name (emitTypeAnnotation t) (emitExpression init)
| Declaration.Type (name,t) -> sprintf "type %s = %s" name (Check.typeToString t)
| Function (name, parameters, ret, body) ->
  let sparams = parameters |> List.map emitProperty |> String.concat ", "
  let sbody = emitExpression body
  sprintf "function %s(%s)%s = %s" name sparams (emitTypeAnnotation ret) sbody
let emit = emitDeclaration 