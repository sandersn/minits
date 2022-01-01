module Minits.Bind
open System.Collections.Generic
open Types
open Traverse
let createSymbol (d : Declaration) (s: option<Symbol>) (errors : List<string>): Symbol =
  let s' = defaultArg s { var = None; typ = None }
  match d with
  | Declaration.Type (name,_) as t -> 
    match s' with
    | { typ = Some _ } -> errors.Add $"Duplicate declaration of {name}"; s'
    | _ -> { s' with typ = Some t }
  | Var (name,_,_) as v -> 
    match s' with
    | { var = Some _ } -> errors.Add $"Duplicate declaration of {name}"; s'
    | _ -> { s' with var = Some v }
  | Function (name,_,_,_) as f ->
    match s' with
    | { var = Some _ } -> errors.Add $"Duplicate declaration of {name}"; s'
    | _ -> { s' with var = Some f }
  | Param (name,_) as p ->
    match s' with
    | { var = Some _ } -> errors.Add $"Duplicate declaration of {name}"; s'
    | _ -> { s' with var = Some p }
  | d -> failwith $"Should only create symbols for Function, Var and Type, got {d}"
let createTable errors =
  List.fold 
    (fun locals (name,d) -> locals |> Map.add name (createSymbol d (Map.tryFind name locals) errors))
    Map.empty
let bind (decl : Declaration) = 
  let env = Dictionary()
  let errors = List()
  let rec bindDeclaration = function
  | Var(name,_,init) -> 
    bindExpression init
    Some name
  | Param(name,_) -> Some name
  | Declaration.Type(name,_) -> Some name
  | Function (name,parameters,_,body) as f ->
    bindExpression body
    env.Add(f, createTable errors (parameters |> List.map (paramOnly (fun p (n,_) -> (n,p)))))
    Some name
  | ExpressionStatement e -> 
    bindExpression e
    None
  | File _ -> None
  and bindDeclarations declarations = 
    declarations
    |> List.choose (fun d -> Option.map (fun name -> (name, d)) (bindDeclaration d))
    |> createTable errors
  and walkExpression (f : Expression -> list<Expression> -> 'a) (expression : Expression) =
    let kids = 
      match expression with
      | Negative e -> [e]
      | Binary(l,_,r) -> [l; r]
      | Assignment (_,e) -> [e]
      | Sequence es -> es
      | Expression.Call (e, args) -> (e :: args)
      | RecordCons (_, inits) -> List.map snd inits
      | ArrayCons inits -> inits
      | If (cond,cons,alt) -> [cond; cons; alt]
      | While (cond, action) -> [cond; action]
      | For (_, start, stop, action) as f -> [start; stop; action]
      | Let (_, e) as l -> [e]
      | LValue lvalue -> walkLValue lvalue
      | IntLiteral _ | StringLiteral _ | Break | Null -> []
    f expression kids
  and bindExpression expression  =
    walkExpression (fun e kids ->
      match e with
      | For (name, start, _, _) as f ->
        env.Add(ExpressionStatement f, createTable errors [name, (Var (name,None,start))])
      | Let (decls, e) as l -> 
        env.Add(ExpressionStatement l, bindDeclarations decls)
      | _ -> ()
      List.iter bindExpression kids
    ) expression
  and walkLValue = function
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
