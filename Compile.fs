module Minits.Compile
open Lex
open Parse
open Bind
open Check
open Transform
open Emit
open Types
let compile (s: string) = 
    let (file, parseErrors) = lex s |> parse
    let (environment, bindErrors) = bind file
    let (types, checkErrors) = check environment file
    (file, environment, types, parseErrors @ bindErrors @ checkErrors, file |> transform |> emit)