module Minits.Emit
open Types
let emitTypeAnnotation = function
| Some t -> ": " + Check.typeToString t
| _ -> ""
// TODO: This is duplicated in the checker
let emitProperty (name, t) = $"{name}: {Check.typeToString t}"
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
| ArrayAccess (l,r) -> $"{emitLValue l}[{emitExpression r}]"
and emitExpression = function
| LValue(lvalue) -> emitLValue lvalue
| IntLiteral(value) -> string value
| StringLiteral(value) -> sprintf "%A" value
| Negative(e) -> "-" + emitExpression e
| Binary (l,op,r) -> sprintf "(%s %s %s)" (emitExpression l) (emitToken op) (emitExpression r)
| Assignment(name, value) -> $"{emitLValue name} = {emitExpression value}"
| Call(name, parameters) -> 
  sprintf "%s(%s)" (emitExpression name) (parameters |> List.map emitExpression |> String.concat ", ")
| Sequence es -> sprintf "(%s)" (es |> List.map emitExpression |> String.concat "; ")
| RecordCons (name,inits) -> sprintf "%s {%s}" name (inits |> List.map (fun (n,e) -> $"{n} = {emitExpression e}") |> String.concat ", ")
| ArrayCons es -> sprintf "[%s]" (es |> List.map emitExpression |> String.concat ", ")
| If (cond,cons,alt) -> $"if {emitExpression cond} then {emitExpression cons} else {emitExpression alt}"
| For (name,start,stop,action) -> $"for {name} = {emitExpression start} to {emitExpression stop} do\n{emitExpression action}"
| While (cond,action) -> $"while {emitExpression cond} do\n{emitExpression action}"
| Let (decls,e) -> sprintf "let\n%s in\n%s" (decls |> List.map emitDeclaration |> String.concat "\n") (emitExpression e)
| Break -> "break"
| Null -> "null"
and emitDeclaration = function
| File decls -> decls |> List.map emitDeclaration |> String.concat "\n"
| ExpressionStatement e -> emitExpression e
| Var(name, t, init) -> 
  $"var {name}{emitTypeAnnotation t} = {emitExpression init}"
| Param(name, t) -> name + emitTypeAnnotation (Some t)
| Declaration.Type (name,t) -> $"type {name} = {Check.typeToString t}"
| Function (name, parameters, ret, body) ->
  let sparams = parameters |> List.map emitDeclaration |> String.concat ", "
  let sbody = emitExpression body
  sprintf "function %s(%s)%s =\n%s" name sparams (emitTypeAnnotation ret) sbody
// TODO: This emits Tiger instead of JS. I want to change this later.
let emit = emitDeclaration 