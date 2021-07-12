module Minits.Bind
open Types
let bind (decl : Declaration) = 
  let env = System.Collections.Generic.Dictionary()
  let errors = System.Collections.Generic.List()
  let createSymbol (d : Declaration) (s: option<Symbol>): Symbol =
    let s' = defaultArg s { var = None; typ = None }
    match d with
    | Declaration.Type (name,_) as t -> 
      match s' with
      | { typ = Some _ } -> errors.Add (sprintf "Duplicate declaration of %s" name); s'
      | _ -> { s' with typ = Some t }
    | Var (name,_,_) as v -> 
      match s' with
      | { var = Some _ } -> errors.Add (sprintf "Duplicate declaration of %s" name); s'
      | _ -> { s' with var = Some v }
    | Function (name,_,_,_) as f ->
      match s' with
      | { var = Some _ } -> errors.Add (sprintf "Duplicate declaration of %s" name); s'
      | _ -> { s' with var = Some f }
    | _ -> failwith <| sprintf "Should only create symbols for Function, Var and Type, got %A" d
  let createTable : list<string * Declaration> -> Table =
    List.fold 
      (fun locals (name,d) -> Map.add name (createSymbol d (Map.tryFind name locals)) locals)
      Map.empty
  let rec bindDeclaration declaration = 
    match declaration with
    | Var(name,_,_) -> Some name
    | Declaration.Type(name,_) -> Some name
    | Function (name,parameters,_,body) as f ->
      let params' : Table = 
        parameters 
        |> List.map (fun (name, t) -> (name, Var (name, Some t, Sequence []))) 
        |> createTable
      bindExpression body
      env.Add(f, params')
      Some name
    | ExpressionStatement e -> 
      bindExpression e
      None
    | File _ -> None
  // TODO: Error on conflicts + add two namespaces
  and bindDeclarations declarations = 
    declarations
    |> List.choose (fun d -> Option.map (fun name -> (name, d)) (bindDeclaration d))
    |> createTable
  and bindExpression expression =
    let kids = 
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
        env.Add(ExpressionStatement f, createTable [name, (Var (name,None,start))])
        [start; stop; action]
      | Let (decls, e) as l -> 
        env.Add(ExpressionStatement l, bindDeclarations decls)
        [e]
      | LValue lvalue -> walkLValue lvalue
      | IntLiteral _ | StringLiteral _ | Break | Null -> []
    List.iter bindExpression kids
  and walkLValue lvalue =
    match lvalue with
    | PropertyAccess (l, _) -> walkLValue l
    | ArrayAccess (l, r) -> r :: walkLValue l
    | Identifier _ -> []
  match decl with
  | File decls as f -> 
    env.Add(f, bindDeclarations decls)
  | _ -> ()
  ((env :> seq<_>) |> Seq.map (|KeyValue|) |> Map.ofSeq, 
   (errors :> seq<_> |> List.ofSeq))
let resolve name (scope : list<Table>) meaning = 
  scope |> List.tryPick (fun table -> 
    match meaning, Map.tryFind name table with
    | (Type, Some({ typ = Some(typ) })) -> Some typ
    | (Value, Some({ var = Some(var)})) -> Some var
    | _ -> None)
