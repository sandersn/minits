module Minits.Compile
open Lex
open Parse
open Bind
let compile (s: string) = 
    let (tree, errors) = lex s false |> parse
    (bind tree, errors)