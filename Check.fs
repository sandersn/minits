module Minits.Check
open Types
open Bind
let globals = Map.empty
let rec typeToString = function
| Type.Identifier name -> name
| Literal ps  -> ps |> List.map propertyToString |> String.concat ", " |> sprintf "{%s}"
| Type.Array t -> sprintf "Array<%s>" <| typeToString t
and propertyToString (name, t) = name + typeToString t
// TODO:
// 2. add a cache for expression checking, keyed by node (it has to be a record of maps because of different types). 
//    return it
// 3. expose a function that takes a node and cache and returns the type
// 4. walk the tree in tests and call this function to create type baselines
let check (env : Environment) (decl: Declaration) =
  let errors = System.Collections.Generic.List()
  let rec checkExpression (scope : list<Table>) expression =
    match expression with
    | LValue(lvalue) -> checkLValue scope lvalue
    | IntLiteral(_) -> intType
    | StringLiteral(_) -> stringType
    | Negative(e) -> errors.Add "Negatives don't check yet"; errorType
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
  and checkLValue (scope : list<Table>) (lvalue : LValue) =
    match lvalue with
    | Identifier(name) -> 
      match resolve name scope Value with
      | Some(statement) -> checkDeclaration scope statement
      | _ -> errors.Add <| "Could not resolve " + name; errorType
    | PropertyAccess _ -> stringType // TODO: REcursive resolve
    | ArrayAccess _ -> intType // TODO: Recursive resolve
  and checkDeclaration (scope : list<Table>) (decl : Declaration) = 
    match decl with
    | File decls as f -> 
      decls |> List.map (checkDeclaration (Map.find f env :: scope)) |> List.last
    | ExpressionStatement(e) -> checkExpression scope e
    | Var(_, typename, init) ->
      let i = checkExpression scope init
      match typename with
      | Some(name) ->
          let t = checkType name
          if t <> i then errors.Add <| sprintf "Cannot assign initialiser of type '%s' to variable with declared type '%s'" (typeToString i) (typeToString t)
          t
      | None -> i
    | Declaration.Type(_, t) -> checkType t
    | Function _ -> errors.Add"Cannot check functions yet"; errorType
  and checkType = function
  | Type.Identifier _ -> errorType // TODO: resolve
  | Type.Literal _ -> errorType // TODO: Create from properties
  | Type.Array _ -> errorType // TODO: Recur
  decl |> checkDeclaration [globals] |> ignore
  (errors :> seq<_> |> List.ofSeq)
  