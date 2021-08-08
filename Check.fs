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
| Type.Array t -> $"Array<{typeToString t}>"
| Type.Arrow (ps, ret) -> sprintf "(%s) -> %s" (List.map propertyToString ps |> String.concat ", ") (typeToString ret)
and propertyToString (name, t) = $"{name}: {typeToString t}"
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
      if t <> intType then errors.Add $"Negative: expected int but got {typeToString t}"
      intType
    | Binary(l,op,r) -> 
      let lt = checkExpression scope l
      let rt = checkExpression scope r
      let checkBinary = function
      | Plus | Minus | Asterisk | ForwardSlash | Pipe | Ampersand | LessThan | GreaterThan | LessThanEquals | GreaterThanEquals -> 
        expectType intType lt rt
        intType
      | DoubleEquals | ForwardSlashEquals ->
        if lt <> rt then errors.Add $"Expected both sides to have same type, but left={typeToString lt} and right={typeToString rt}"
        intType
      | t -> failwith $"Unexpected binary operator token {t}"
      checkBinary op
    | Assignment(lvalue, value) -> 
      let v = checkExpression scope value
      let n = checkLValue scope lvalue
      if v <> n then errors.Add $"Cannot assign value of type '{typeToString v}' to variable of type '{typeToString n}'"
      n
    | Call(e, args) -> 
      match checkExpression scope e with
      | Type.Arrow(parameters, ret) ->
        List.iter2 (fun (name,t) arg -> 
            if t <> arg then errors.Add $"Parameter {name} expected type {typeToString t} but got {typeToString arg}.") 
          parameters 
          (List.map (checkExpression scope) args)
        ret
      | t -> errors.Add $"{t} is not callable."; errorType
    | Sequence es -> 
      match es with 
      | [] -> nullType
      | es -> List.map (checkExpression scope) es |> List.last
    | RecordCons (name,inits) ->
      match resolve name scope Type with
      | Some (Declaration.Type _ as decl) -> 
        match checkDeclaration scope decl with
        | Literal props -> 
          let propTypes = Map.ofList props
          inits |> List.iter (fun (name,i) -> 
            let pt = Map.find name propTypes
            let it = checkExpression scope i
            if pt <> it then errors.Add $"{name} expected type {typeToString pt} but got {typeToString it}.")
          ()
        | _ -> errors.Add $"Type {name} is not a record type."
      | Some _ -> errors.Add $"{name} is not a type declaration."
      | None -> errors.Add $"Could not resolve type {name}."
      Type.Literal <| List.map (second (checkExpression scope)) inits
    | ArrayCons es ->
      match List.map (checkExpression scope) es with
      | (t :: ts) -> 
        ts |> List.iter (fun t' -> if t <> t' then errors.Add $"Expected array elements to have type {typeToString t} but got {typeToString t'}.")
        Array t
      | [] -> Array nullType
    | If (cond, cons, alt) ->
      let condt = checkExpression scope cond 
      if condt <> intType then errors.Add $"If condition must have type int, but got {typeToString condt}."
      let ct = checkExpression scope cons
      let at = checkExpression scope alt
      if ct <> at then errors.Add $"Both branches of an if must have the same type. then-branch: {typeToString ct}; else-branch {typeToString at}."
      ct
    | While (cond, action) -> 
      let condt = checkExpression scope cond 
      if condt <> intType then errors.Add $"While condition must have type int, but got {typeToString condt}."
      checkExpression scope action
      // if at <> nullType then errors.Add $"While action must have type null, but got {typeToString at}."
    | For (_, start, stop, action) as f -> 
      let startt = checkExpression scope start 
      if startt <> intType then errors.Add $"For start value must have type int, but got {typeToString startt}."
      let stopt = checkExpression scope stop 
      if stopt <> intType then errors.Add $"For stop value must have type int, but got {typeToString stopt}."
      checkExpression (Map.find (ExpressionStatement f) env :: scope) action
      // if at <> nullType then errors.Add $"For action must have type null, but got {typeToString at}."
    | Let (decls, body) as l ->
      decls |> List.iter (fun d -> checkDeclaration scope d |> ignore)
      checkExpression (Map.find (ExpressionStatement l) env :: scope) body
    | Break -> nullType // TODO: Error if not inside a for (not sure how to do this without parent pointers)
    | Null -> nullType
  and expectType expected lt rt = 
    if lt <> expected then errors.Add $"Left side expected {typeToString expected} but got {typeToString lt}" 
    elif rt <> expected then errors.Add $"Right side expected {typeToString expected} but got {typeToString rt}"
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
          if t <> i then errors.Add $"Cannot assign initialiser of type '{typeToString i}' to variable with declared type '{typeToString t}'"
          t
      | None -> i
    | Param(_, typename) -> resolveType scope typename
    | Declaration.Type(_, t) -> resolveType scope t
    | Function (name,parameters,ret,body) as f ->
      let bt = checkExpression (Map.find f env :: scope) body
      let ps' = 
        parameters 
        |> List.map (function
          | Param (n,_) as p -> (n, checkDeclaration scope p)
          | d  -> failwith "Only expected parameters in parameter list, got {d}")
      let ret' = 
        match ret with
        | Some t -> if t = bt then t else errors.Add $"Expected {name} to return {typeToString t} but got {typeToString bt}"; t
        | None -> bt 
      Arrow (ps', ret')
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
    | Type.Arrow (parameters, ret) -> Type.Arrow (List.map (second (resolveType scope)) parameters, resolveType scope ret)
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
  