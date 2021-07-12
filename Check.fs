module Minits.Check
open Types
open Bind
let globals = Map.empty
let rec typeToString = function
| Type.Identifier name -> name
| Literal ps  -> ps |> List.map propertyToString |> String.concat ", " |> sprintf "{%s}"
| Type.Array t -> sprintf "Array<%s>" <| typeToString t
and propertyToString (name, t) = name + typeToString t
let check (env : Environment) (decl: Declaration) =
  let rec checkExpression (scope : list<Table>) expression =
    match expression with
    | LValue(lvalue) -> checkLValue scope lvalue
    | IntLiteral(_) -> (intType, [])
    | StringLiteral(_) -> (stringType, [])
    | Negative(e) -> (errorType, ["Negatives don't check yet"])
    | Binary(l,op,r) -> (errorType, ["Binary expressions don't check yet"])
    | Assignment(lvalue, value) -> 
      let (v, e) = checkExpression scope value
      let (n, _) = checkLValue scope lvalue
      let error = 
        if v = n 
        then []
        else [sprintf "Cannot assign value of type '%s' to variable of type '%s'" (typeToString v) (typeToString n)]
      (n, e @ error)
    | Call(e, parameters) -> (errorType, ["Cannot check calls yet"])
    | Sequence es -> (errorType, []) // List.map checkExpression es |> List.head // TODO: Last, and concat errors
    | RecordCons _ -> (errorType, ["Records don't check yet"])
    | ArrayCons _ -> (errorType, ["Arrays don't check yet"])
    | If _ -> (errorType, ["If doesn't check yet"])
    | While _ -> (nullType, ["While doesn't check yet"])
    | For _ -> (nullType, ["For doesn't check yet"])
    | Let _ -> (errorType, ["Let doesn't check yet"])
    | Break -> (nullType, ["Break doesn't check yet"])
    | Null -> (nullType, [])
  and checkLValue (scope : list<Table>) (lvalue : LValue) =
    match lvalue with
    | Identifier(name) -> 
      match resolve name scope Value with
      | Some(statement) -> checkDeclaration scope statement
      | _ -> (errorType, ["Could not resolve " + name])
    | PropertyAccess _ -> (stringType, []) // TODO: REcursive resolve
    | ArrayAccess _ -> (intType, []) // TODO: Recursive resolve
  and checkDeclaration (scope : list<Table>) (decl : Declaration) = 
    match decl with
    | File decls as f -> 
      let x = List.map (checkDeclaration (Map.find f env :: scope)) decls
      (List.last x |> fst, List.collect snd x)
    | ExpressionStatement(e) -> checkExpression scope e
    | Var(_, typename, init) ->
      let (i, e) = checkExpression scope init
      match typename with
      | Some(name) ->
          let (t, e') = checkType name
          let error = 
            if t = i 
            then []
            else [sprintf "Cannot assign initialiser of type '%s' to variable with declared type '%s'" (typeToString i) (typeToString t)]
          (t, e @ e' @ error)
      | None -> (i, e)
    | Declaration.Type(_, t) -> checkType t
    | Function _ -> (errorType, ["Cannot check functions yet"])
  and checkType = function
  | Type.Identifier _ -> (stringType, []) // TODO: resolve
  | Type.Literal _ -> (intType, []) // TODO: Create from properties
  | Type.Array _ -> (errorType, []) // TODO: Recur
  decl |> checkDeclaration [globals] |> snd
  