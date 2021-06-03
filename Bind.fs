module Minits.Bind
open Types
let bindStatement locals statement = 
  match statement with
  | Var(name,_) as v -> Map.add name v locals
  | _ -> locals
let bind (_, statements) =
  (List.fold bindStatement Map.empty statements, statements)
let resolve = Map.tryFind