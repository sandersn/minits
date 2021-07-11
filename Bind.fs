module Minits.Bind
open Types
let bind (decl : Declaration) = 
  let env = System.Collections.Generic.Dictionary()
  let rec bindDeclaration declaration = 
    match declaration with
    | Var(name,_,_) -> Some name
    | Declaration.Type(name,_) -> Some name
    | Function (name,parameters,_,body) as f ->
      let params' : Table = 
        parameters 
        |> List.map (fun (name, t) -> (name, Var (name, Some t, Sequence []))) 
        |> Map.ofList
      bindExpression body
      env.Add(f, params')
      Some name
    | ExpressionStatement e -> 
      bindExpression e
      None
    | File decls -> None
  // TODO: Error on conflicts + add two namespaces
  and bindDeclarations declarations = 
    List.choose (fun d -> Option.map (fun name -> (name, d)) (bindDeclaration d)) declarations
    |> Map.ofList
  and bindExpression expression =
    let expressions = 
      match expression with
      | Negative e -> [e]
      | Binary(l,_,r) -> [l; r]
      | Assignment (_,e) -> [e]
      | Sequence es -> es
      | Call (e, args) -> (e :: args)
      | RecordCons (_, inits) -> List.map snd inits
      | ArrayCons inits -> inits
      | If (cond,cons,alt) -> [cond; cons; alt]
      | While (cond, action) -> [cond; action]
      | For (name, start, stop, action) as f ->
        env.Add(ExpressionStatement f, Map.ofList [name, (Var (name,None,start))])
        [start; stop; action]
      | Let (decls, e) as l -> 
        env.Add(ExpressionStatement l, bindDeclarations decls)
        [e]
      | LValue lvalue -> walkLValue lvalue
      | IntLiteral _ | StringLiteral _ | Break | Null -> []
    List.iter bindExpression expressions
  and walkLValue lvalue =
    match lvalue with
    | PropertyAccess (l, _) -> walkLValue l
    | ArrayAccess (l, r) -> r :: walkLValue l
    | Identifier _ -> []
  match decl with
  | File decls as f -> 
    env.Add(f, bindDeclarations decls)
  | _ -> ()
  (env :> seq<_>) |> Seq.map (|KeyValue|) |> Map.ofSeq 
  // TODO: Namespaces + meanings
let resolve name (tables : list<Table>) meaning = List.tryPick (Map.tryFind name) tables
