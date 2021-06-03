module Minits.Check
open Types
open Bind
let intType = Type "int"
let errorType = Type "error"
let typeToString (Type name) = name
let check (env, statements) =
  let rec checkExpression expression =
    match expression with
    | Identifier(name) ->
      match resolve name env with
      | Some(statement) -> checkStatement statement
      | None -> (errorType, ["Could not resolve " + name])
    | IntLiteral(_) -> (intType, [])
    | Assignment(name, value) -> 
      let (v, e) = checkExpression value
      let (n, e') = checkExpression (Identifier(name))
      printfn "Checking assignment of %A to %A" v n
      let error = 
        if v = n 
        then []
        else [sprintf "Got type '%s' but expected '%s'" (typeToString v) (typeToString n)]
      (n, e @ e' @ error)
  and checkStatement statement =
    match statement with
    | ExpressionStatement(e) -> checkExpression e
    | Var(_, init) -> checkExpression init // TODO: Check type annotation too
  List.map (checkStatement >> snd) statements |> List.concat