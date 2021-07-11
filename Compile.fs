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
    let environment = bind file
    (file, environment, parseErrors @ check environment file, file |> transform |> emit)