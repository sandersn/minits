module Minits.Compile
open Lex
open Parse
let compile (s: string) = 
    lex s |> parse