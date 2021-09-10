module Minits.Check
open Types
open Bind
open Traverse
open System.Collections.Generic
let stringType = Type.Identifier "string"
let intType = Type.Identifier "int"
let errorType = Type.Identifier "error"
let nullType = Type.Identifier "null"
let circularType = Type.Identifier "circular"
let first f (a,b) = (f a, b)
let second f (a,b) = (a, f b)
let deref = function
| Reference t -> t.contents
| t -> t
let rec typeToString = function
| Type.Identifier name -> name
| Literal ps -> ps |> List.map propertyToString |> String.concat ", " |> sprintf "{%s}"
| Array t -> $"Array<{typeToString t}>"
| Arrow (ps, ret) -> sprintf "(%s) -> %s" (List.map propertyToString ps |> String.concat ", ") (typeToString ret)
| Reference r ->
  match r.contents with
  | Type.Literal ps -> "{...}" 
  | Type.Arrow _ -> "(...) -> ()"
  | _ -> failwith $"Should not have reference to non-recursive type {r.contents}"
and propertyToString (name, t) = $"{name}: {typeToString t}"
// TODO: also someday call the emitter to convert ASTs to strings
let check (env : Environment) (globals: Table) (decl: Declaration) =
  let errors = List()
  let cache: ResolvedTypes = {
    declarations = Dictionary()
    expressions = Dictionary()
    types = Dictionary()
    lvalues = Dictionary()
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
  and checkDeclaration scope decl =
    if cache.declarations.ContainsKey decl then cache.declarations.[decl] else
    let mutateCircularity circular =
      let circularRef = (ref circular)
      cache.declarations.Add (decl,Reference circularRef)
      circularRef.contents <- checkDeclaration' scope decl
      cache.declarations.Remove decl |> ignore
      cache.declarations.Add (decl,circularRef.contents)
      circularRef.contents
    match decl with
    | Declaration.Type (_,Literal _)
    | Declaration.Type (_,Arrow _) -> mutateCircularity circularType
    | Function (_, ps, ret, _) ->
      let circular = match ret with 
                     | Some(t) -> Arrow (ps |> List.map (paramOnly (fun _ (n,t) -> (n, resolveType scope t))), resolveType scope t)
                     | _ -> circularType
      mutateCircularity circular
    | _ -> 
      let t = checkDeclaration' scope decl
      cache.declarations.Add (decl,t)
      t
  and resolveType scope typ = 
    if cache.types.ContainsKey typ then cache.types.[typ] else
    let t = resolveType' scope typ
    cache.types.Add (typ,t)
    t
  and checkRelatedTo source target message = 
    // TODO: isRelatedTo should probably be cached
    if isRelatedTo (source,target)
    then ()
    else errors.Add (message + $" expected type {typeToString target} but got {typeToString source}.") 
  and isRelatedTo = function
  | (Type.Identifier i1, Type.Identifier i2) -> i1 = i2
  | (Literal ps1, Literal ps2) -> 
    List.zip (List.map snd ps1) (List.map snd ps2) |> List.forall isRelatedTo 
  | (Arrow (ps1,r1), Arrow (ps2,r2)) ->
    List.zip (List.map snd ps1) (List.map snd ps2) |> List.forall isRelatedTo && isRelatedTo (r1,r2)
  | (Array t1, Array t2) -> isRelatedTo (t1,t2)
  | (Reference r1, Reference r2) -> LanguagePrimitives.PhysicalEquality r1.contents r2.contents
  | (Reference r, t) -> isRelatedTo (r.contents, t)
  | (s, Reference r) -> isRelatedTo (s, r.contents)
  | (s,t) -> s = nullType || t = nullType
  and checkExpression' (scope : list<Table>) expression =
    match expression with
    | LValue lvalue -> checkLValue scope lvalue
    | IntLiteral _ -> intType
    | StringLiteral _ -> stringType
    | Negative e -> 
      let t = checkExpression scope e
      checkRelatedTo t intType "Negative: "
      intType
    | Binary(l,op,r) -> 
      let lt = checkExpression scope l
      let rt = checkExpression scope r
      let checkBinary = function
      | Plus | Minus | Asterisk | ForwardSlash | Pipe | Ampersand | LessThan | GreaterThan | LessThanEquals | GreaterThanEquals -> 
        checkExpectedType intType lt rt
        intType
      | DoubleEquals | ForwardSlashEquals ->
        checkRelatedTo lt rt "Both sides should have the same type;"
        intType
      | t -> failwith $"Unexpected binary operator token {t}"
      checkBinary op
    | Assignment(lvalue, value) -> 
      let v = checkExpression scope value
      let n = checkLValue scope lvalue
      checkRelatedTo v n "Cannot assign value of"
      n
    | Call(e, args) -> 
      let args' = (List.map (checkExpression scope) args)
      match checkExpression scope e |> deref with
      | Type.Arrow(parameters, ret) ->
        List.iter2 (fun (name,t) arg -> checkRelatedTo arg t $"Parameter {name}") 
          parameters 
          args'
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
            checkRelatedTo (checkExpression scope i) (Map.find name propTypes) name)
        | _ -> errors.Add $"Type {name} is not a record type."
      | Some _ -> errors.Add $"{name} is not a type declaration."
      | None -> errors.Add $"Could not resolve type {name}."
      Type.Literal <| List.map (second (checkExpression scope)) inits
    | ArrayCons es ->
      match List.map (checkExpression scope) es with
      | (t :: ts) -> 
        ts |> List.iter (fun t' -> checkRelatedTo t t' "Array elements:")
        Array t
      | [] -> Array nullType
    | If (cond, cons, alt) ->
      checkRelatedTo (checkExpression scope cond) intType "If condition:"
      let ct = checkExpression scope cons
      let at = checkExpression scope alt
      checkRelatedTo ct at "Both branches of an if must have the same type."
      ct
    | While (cond, action) -> 
      checkRelatedTo (checkExpression scope cond) intType "While condition:"
      checkExpression scope action
      // if at <> nullType then errors.Add $"While action must have type null, but got {typeToString at}."
    | For (_, start, stop, action) as f -> 
      checkRelatedTo (checkExpression scope start) intType "For start value:"
      checkRelatedTo (checkExpression scope stop) intType "For stop value:"
      checkExpression (Map.find (ExpressionStatement f) env :: scope) action
      // if at <> nullType then errors.Add $"For action must have type null, but got {typeToString at}."
    | Let (decls, body) as l ->
      let scope' = Map.find (ExpressionStatement l) env :: scope
      decls |> List.iter (fun d -> checkDeclaration scope' d |> ignore)
      checkExpression scope' body
    | Break -> nullType // TODO: Error if not inside a for (not sure how to do this without parent pointers)
    | Null -> nullType
  and checkExpectedType expected lt rt = 
    if lt <> expected then errors.Add $"Left side expected {typeToString expected} but got {typeToString lt}" 
    elif rt <> expected then errors.Add $"Right side expected {typeToString expected} but got {typeToString rt}"
  and checkLValue' (scope : list<Table>) (lvalue : LValue) =
    match lvalue with
    | Identifier(name) -> 
      match resolve name scope Value with
      | Some(statement) -> checkDeclaration scope statement
      | _ -> errors.Add <| "Could not resolve " + name; errorType
    | PropertyAccess (l,r) ->
      match checkLValue scope l |> deref with
      | Literal ps -> ps |> List.find (fst >> (=) r) |> snd
      | t -> errors.Add $"Property access is not allowed on {typeToString t}."; errorType
    | ArrayAccess (l,r) ->
      checkRelatedTo (checkExpression scope r) intType "Index of element access:"
      match checkLValue scope l with
      | Array e -> e
      | t -> errors.Add $"Element access is not allowed on {typeToString t}."; errorType
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
        |> List.map (paramOnly (fun p (n,t) -> (n, checkDeclaration scope p)))
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
    | Array elt -> Array (resolveType scope elt)
    | Literal properties -> Literal (List.map (second (resolveType scope)) properties)
    | Arrow (parameters, ret) -> Arrow (List.map (second (resolveType scope)) parameters, resolveType scope ret)
    | Reference r -> failwith $"Do not resolveType on reference types: {r}"
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
  