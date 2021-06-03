module Minits.Check
open Types
open Bind
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
      let (n, _) = checkExpression (Identifier(name))
      let error = 
        if v = n 
        then []
        else [sprintf "Cannot assign value of type '%s' to variable of type '%s'" (typeToString v) (typeToString n)]
      (n, e @ error)
  and checkStatement statement =
    match statement with
    | ExpressionStatement(e) -> checkExpression e
    | Var(_, typename, init) ->
      let (i, e) = checkExpression init
      match typename with
      | Some(name) ->
          let (t, e') = checkType name
          let error = 
            if t = i 
            then []
            else [sprintf "Cannot assign initialiser of type '%s' to variable with declared type '%s'" (typeToString i) (typeToString t)]
          (t, e @ e' @ error)
      | None -> (i, e)
  and checkType = function
    | "string" -> (stringType, [])
    | "int" -> (intType, [])
    | name -> (errorType, ["Could not resolve type " + name])
  List.map (checkStatement >> snd) statements |> List.concat