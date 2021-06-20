module Minits.Check
open Types
open Bind
let rec typeToString = function
| Type.Identifier name -> name
| Literal ps  -> ps |> List.map propertyToString |> String.concat ", " |> sprintf "{%s}"
| Array t -> sprintf "Array<%s>" <| typeToString t
and propertyToString (name, t) = name + typeToString t
let check (env, statements) =
  let rec checkExpression = function
  | Expression.Identifier(name) ->
    match resolve name env with
    | Some(statement) -> checkDeclaration statement
    | None -> (errorType, ["Could not resolve " + name])
  | IntLiteral(_) -> (intType, [])
  | Assignment(name, value) -> 
    let (v, e) = checkExpression value
    let (n, _) = checkExpression (Expression.Identifier(name))
    let error = 
      if v = n 
      then []
      else [sprintf "Cannot assign value of type '%s' to variable of type '%s'" (typeToString v) (typeToString n)]
    (n, e @ error)
  and checkDeclaration = function
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
  | Type(_, t) -> checkType t
  | Function _ -> (errorType, ["Cannot check functions yet"])
  and checkType = function
  | Type.Identifier _ -> (stringType, []) // TODO: resolve
  | Type.Literal _ -> (intType, []) // TODO: Create from properties
  | Array _ -> (errorType, []) // TODO: Recur
  List.map (checkDeclaration >> snd) statements |> List.concat