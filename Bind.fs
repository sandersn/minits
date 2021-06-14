module Minits.Bind
open Types
let bindDeclaration locals declaration = 
  match declaration with
  | Var(name,_,_) as v -> Map.add name v locals
  | _ -> locals
let bind (_, declarations) =
  (List.fold bindDeclaration Map.empty declarations, declarations)
let resolve = Map.tryFind
