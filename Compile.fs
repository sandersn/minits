module Minits.Compile
open Lex
open Parse
open Bind
open Check
open Transform
open Emit
// TODO: Should just support loading multiple files from disk
// TODO: Support [foreign] signature declarations
let globalS = $" 
function concat(left: string, right: string): string = left
"
let mapMerge m1 m2 = Map.fold (fun m k v -> Map.add k v m) m1 m2
let compile (s: string) = 
    let (file, parseErrors) = lex s |> parse
    let (environment, bindErrors) = bind file
    // for debugging purposes, retaining global errors is a good idea
    let (globalFile, _) = lex globalS |> parse
    let (globalEnvironment, _) = bind globalFile
    let env = globalEnvironment |>mapMerge<| environment
    let (types, checkErrors) = check env (Map.find globalFile globalEnvironment) file
    let ir = Translate.translate env file
    (file, 
     environment, 
     types, 
     parseErrors @ bindErrors @ checkErrors, 
     file |> transform |> emit)