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
  let rec checkExpression scope level expression = 
    if cache.expressions.ContainsKey expression then cache.expressions.[expression] else
    let e = checkExpression' scope level expression
    cache.expressions.Add (expression,e)
    e
  and checkLValue scope level lvalue =
    if cache.lvalues.ContainsKey lvalue then cache.lvalues.[lvalue] else
    let l = checkLValue' scope level lvalue
    cache.lvalues.Add (lvalue,l)
    l
  and checkDeclaration scope level decl =
    if cache.declarations.ContainsKey decl then cache.declarations.[decl] else
    let mutateCircularity circular =
      let circularRef = (ref circular)
      cache.declarations.Add (decl,Reference circularRef)
      circularRef.contents <- checkDeclaration' scope level decl
      cache.declarations.Remove decl |> ignore
      cache.declarations.Add (decl,circularRef.contents)
      circularRef.contents
    match decl with
    | Declaration.Type (_,Literal _)
    | Declaration.Type (_,Arrow _) -> mutateCircularity circularType
    | Function (_, ps, ret, _) ->
      let circular = match ret with 
                     | Some(t) -> Arrow (ps |> List.map (paramOnly (fun _ (n,t) -> (n, resolveType scope level t))), resolveType scope level t)
                     | _ -> circularType
      mutateCircularity circular
    | _ -> 
      let t = checkDeclaration' scope level decl
      cache.declarations.Add (decl,t)
      t
  and resolveType scope level typ = 
    if cache.types.ContainsKey typ then cache.types.[typ] else
    let t = resolveType' scope level typ
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
  and checkExpression' (scope : list<Table>) level expression =
    match expression with
    | LValue lvalue -> checkLValue scope level lvalue
    | IntLiteral _ -> intType
    | StringLiteral _ -> stringType
    | Negative e -> 
      let t = checkExpression scope level e
      checkRelatedTo t intType "Negative: "
      intType
    | Binary(l,op,r) -> 
      let lt = checkExpression scope level l
      let rt = checkExpression scope level r
      match op with
      | Token.Plus | Token.Minus | Asterisk | ForwardSlash | Pipe | Ampersand | LessThan | GreaterThan | LessThanEquals | GreaterThanEquals -> 
        checkExpectedType intType lt rt
        intType
      | DoubleEquals | ForwardSlashEquals ->
        checkRelatedTo lt rt "Both sides should have the same type;"
        intType
      | t -> failwith $"Unexpected binary operator token {t}"
      //checkBinary op
    | Assignment(lvalue, value) -> 
      let v = checkExpression scope level value
      let n = checkLValue scope level lvalue
      checkRelatedTo v n "Cannot assign value of"
      n
    | Expression.Call(e, args) -> 
      let args' = (List.map (checkExpression scope level) args)
      match checkExpression scope level e |> deref with
      | Type.Arrow(parameters, ret) ->
        List.iter2 (fun (name,t) arg -> checkRelatedTo arg t $"Parameter {name}") 
          parameters 
          args'
        ret
      | t -> errors.Add $"{t} is not callable."; errorType
    | Sequence es -> 
      match es with 
      | [] -> nullType
      | es -> List.map (checkExpression scope level) es |> List.last
    | RecordCons (name,inits) ->
      match resolve name scope Type with
      | Some (Declaration.Type _ as decl) -> 
        match checkDeclaration scope level decl with
        | Literal props -> 
          let propTypes = Map.ofList props
          inits |> List.iter (fun (name,i) -> 
            checkRelatedTo (checkExpression scope level i) (Map.find name propTypes) name)
        | _ -> errors.Add $"Type {name} is not a record type."
      | Some _ -> errors.Add $"{name} is not a type declaration."
      | None -> errors.Add $"Could not resolve type {name}."
      Type.Literal <| List.map (second (checkExpression scope level)) inits
    | ArrayCons es ->
      match List.map (checkExpression scope level) es with
      | (t :: ts) -> 
        ts |> List.iter (fun t' -> checkRelatedTo t t' "Array elements:")
        Array t
      | [] -> Array nullType
    | If (cond, cons, alt) ->
      checkRelatedTo (checkExpression scope level cond) intType "If condition:"
      let ct = checkExpression scope level cons
      let at = checkExpression scope level alt
      checkRelatedTo ct at "Both branches of an if must have the same type."
      ct
    | While (cond, action) -> 
      checkRelatedTo (checkExpression scope level cond) intType "While condition:"
      checkExpression scope level action
      // if at <> nullType then errors.Add $"While action must have type null, but got {typeToString at}."
    | For (_, start, stop, action) as f -> 
      checkRelatedTo (checkExpression scope level start) intType "For start value:"
      checkRelatedTo (checkExpression scope level stop) intType "For stop value:"
      checkExpression (Map.find (ExpressionStatement f) env :: scope) level action
      // if at <> nullType then errors.Add $"For action must have type null, but got {typeToString at}."
    | Let (decls, body) as l ->
      let scope' = Map.find (ExpressionStatement l) env :: scope
      decls |> List.iter (fun d -> checkDeclaration scope' level d |> ignore)
      checkExpression scope' level body
    | Break -> nullType // TODO: Error if not inside a for (not sure how to do this without parent pointers)
    | Null -> nullType
  and checkExpectedType expected lt rt = 
    if lt <> expected then errors.Add $"Left side expected {typeToString expected} but got {typeToString lt}" 
    elif rt <> expected then errors.Add $"Right side expected {typeToString expected} but got {typeToString rt}"
  and checkLValue' (scope : list<Table>) (level: Translate.Level) (lvalue : LValue) =
    match lvalue with
    | Identifier(name) -> 
      match resolve name scope Value with
      | Some(statement) -> checkDeclaration scope level statement
      | _ -> errors.Add <| "Could not resolve " + name; errorType
    | PropertyAccess (l,r) ->
      match checkLValue scope level l |> deref with
      | Literal ps -> ps |> List.find (fst >> (=) r) |> snd
      | t -> errors.Add $"Property access is not allowed on {typeToString t}."; errorType
    | ArrayAccess (l,r) ->
      checkRelatedTo (checkExpression scope level r) intType "Index of element access:"
      match checkLValue scope level l with
      | Array e -> e
      | t -> errors.Add $"Element access is not allowed on {typeToString t}."; errorType
  and checkDeclaration' (scope : list<Table>) (level: Translate.Level) (decl : Declaration) = 
    match decl with
    | File decls as f -> 
      decls |> List.map (checkDeclaration (Map.find f env :: scope) level) |> List.last
    | ExpressionStatement(e) -> checkExpression scope level e
    | Var(_, typename, init) ->
      let i = checkExpression scope level init
      match typename with
      | Some(name) ->
          let t = resolveType scope level name
          if t <> i then errors.Add $"Cannot assign initialiser of type '{typeToString i}' to variable with declared type '{typeToString t}'"
          t
      | None -> i
    | Param(_, typename) -> resolveType scope level typename
    | Declaration.Type(_, t) -> resolveType scope level t
    | Function (name,parameters,ret,body) as f ->
      let level' = Translate.newLevel level (Temp.newlabel ()) (parameters |> List.map (fun _ -> false))
      let bt = checkExpression (Map.find f env :: scope) level' body
      let ps' = 
        parameters 
        |> List.map (paramOnly (fun p (n,t) -> (n, checkDeclaration scope level p)))
      let ret' = 
        match ret with
        | Some t -> if t = bt then t else errors.Add $"Expected {name} to return {typeToString t} but got {typeToString bt}"; t
        | None -> bt 
      Arrow (ps', ret')
  and resolveType' scope level typ = 
    match typ with
    | Type.Identifier name -> 
      match name with
      | "string" -> stringType
      | "int" -> intType
      | "null" -> nullType
      | name -> resolve name scope Type |> Option.map (checkDeclaration scope level) |>defaultArg<| errorType
    | Array elt -> Array (resolveType scope level elt)
    | Literal properties -> Literal (List.map (second (resolveType scope level)) properties)
    | Arrow (parameters, ret) -> Arrow (List.map (second (resolveType scope level)) parameters, resolveType scope level ret)
    | Reference r -> failwith $"Do not resolveType on reference types: {r}"
  decl |> checkDeclaration [globals] Translate.outermost |> ignore
  (cache, errors :> seq<_> |> List.ofSeq)
let getTypeOfDeclaration (cache: ResolvedTypes) (node: Declaration) =
  if cache.declarations.ContainsKey node then Some cache.declarations.[node] else None
let getTypeOfExpression (cache: ResolvedTypes) (node: Expression) =
  if cache.expressions.ContainsKey node then Some cache.expressions.[node] else None
let getTypeOfLValue (cache: ResolvedTypes) (node: LValue) =
  if cache.lvalues.ContainsKey node then Some cache.lvalues.[node] else None
let getTypeOfType (cache: ResolvedTypes) (node: Type) =
  if cache.types.ContainsKey node then Some cache.types.[node] else None
  