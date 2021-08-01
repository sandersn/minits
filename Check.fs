module Minits.Check
open Types
open Bind
let stringType = Type.Identifier "string"
let intType = Type.Identifier "int"
let errorType = Type.Identifier "error"
let nullType = Type.Identifier "null"
let globals : Table = Map.empty
let first f (a,b) = (f a, b)
let second f (a,b) = (a, f b)
let rec typeToString = function
| Type.Identifier name -> name
| Literal ps -> ps |> List.map propertyToString |> String.concat ", " |> sprintf "{%s}"
| Type.Array t -> sprintf "Array<%s>" <| typeToString t
and propertyToString (name, t) = sprintf "%s: %s" name  (typeToString t)
// TODO: also someday call the emitter to convert ASTs to strings
let check (env : Environment) (decl: Declaration) =
  let errors = System.Collections.Generic.List()
  let cache: ResolvedTypes = {
    declarations = System.Collections.Generic.Dictionary()
    expressions = System.Collections.Generic.Dictionary()
    types = System.Collections.Generic.Dictionary()
    lvalues = System.Collections.Generic.Dictionary()
  }
  let rec checkExpression scope expression = 
    if cache.expressions.ContainsKey expression then cache.expressions.[expression] else
    let e = checkExpression' scope expression
    cache.expressions.Add (expression,e)
    e
  and checkLValue scope lvalue =
    if cache.lvalues.ContainsKey lvalue then cache.lvalues.[lvalue] else
    let l = checkLValue' scope lvalue
    cache.lvalues.Add (lvalue,l)
    l
  and checkDeclaration (scope : list<Table>) (decl : Declaration) = 
    if cache.declarations.ContainsKey decl then cache.declarations.[decl] else
    let d = checkDeclaration' scope decl
    cache.declarations.Add (decl,d)
    d
  and resolveType scope typ = 
    if cache.types.ContainsKey typ then cache.types.[typ] else
    let t = resolveType' scope typ
    cache.types.Add (typ,t)
    t
  and checkExpression' (scope : list<Table>) expression =
    match expression with
    | LValue lvalue -> checkLValue scope lvalue
    | IntLiteral _ -> intType
    | StringLiteral _ -> stringType
    | Negative e -> 
      let t = checkExpression scope e
      if t <> intType then errors.Add <| sprintf "Negative: expected int but got %s" (typeToString t)
      intType
    | Binary(l,op,r) -> errors.Add "Binary expressions don't check yet"; errorType
    | Assignment(lvalue, value) -> 
      let v = checkExpression scope value
      let n = checkLValue scope lvalue
      if v <> n then errors.Add <| sprintf "Cannot assign value of type '%s' to variable of type '%s'" (typeToString v) (typeToString n)
      n
    | Call(e, parameters) -> errors.Add "Cannot check calls yet"; errorType
    | Sequence es -> errorType // List.map checkExpression es |> List.last
    | RecordCons _ -> errors.Add "Records don't check yet"; errorType
    | ArrayCons _ -> errors.Add "Arrays don't check yet"; errorType
    | If _ -> errors.Add "If doesn't check yet"; errorType
    | While _ -> errors.Add "While doesn't check yet"; nullType
    | For _ -> errors.Add "For doesn't check yet"; nullType
    | Let _ -> errors.Add "Let doesn't check yet"; errorType
    | Break -> errors.Add "Break doesn't check yet"; nullType
    | Null -> nullType
  and checkLValue' (scope : list<Table>) (lvalue : LValue) =
    match lvalue with
    | Identifier(name) -> 
      match resolve name scope Value with
      | Some(statement) -> checkDeclaration scope statement
      | _ -> errors.Add <| "Could not resolve " + name; errorType
    | PropertyAccess _ -> errorType // TODO: REcursive resolve
    | ArrayAccess _ -> errorType // TODO: Recursive resolve
  and checkDeclaration' (scope : list<Table>) (decl : Declaration) = 
    match decl with
    | File decls as f -> 
      decls |> List.map (checkDeclaration (Map.find f env :: scope)) |> List.last
    | ExpressionStatement(e) -> checkExpression scope e
    | Var(_, typename, init) ->
      let i = checkExpression scope init
      match typename with
      | Some(name) ->
          let t = resolveType scope name
          if t <> i then errors.Add <| sprintf "Cannot assign initialiser of type '%s' to variable with declared type '%s'" (typeToString i) (typeToString t)
          t
      | None -> i
    | Declaration.Type(_, t) -> resolveType scope t
    | Function _ -> errors.Add "Cannot check functions yet"; errorType
  and resolveType' scope typ = 
    match typ with
    | Type.Identifier name -> 
      match name with
      | "string" -> stringType
      | "int" -> intType
      | "null" -> nullType
      | name -> resolve name scope Type |> Option.map (checkDeclaration scope) |>defaultArg<| errorType
    | Type.Array elt -> Type.Array (resolveType scope elt)
    | Type.Literal properties -> Type.Literal (List.map (second (resolveType scope)) properties)
  decl |> checkDeclaration [globals] |> ignore
  (cache, errors :> seq<_> |> List.ofSeq)
let getTypeOfDeclaration (cache: ResolvedTypes) (node: Declaration) =
  if cache.declarations.ContainsKey node then Some cache.declarations.[node] else None
let getTypeOfExpression (cache: ResolvedTypes) (node: Expression) =
  if cache.expressions.ContainsKey node then Some cache.expressions.[node] else None
let getTypeOfLValue (cache: ResolvedTypes) (node: LValue) =
  if cache.lvalues.ContainsKey node then Some cache.lvalues.[node] else None
let getTypeOfType (cache: ResolvedTypes) (node: Type) =
  if cache.types.ContainsKey node then Some cache.types.[node] else None
  