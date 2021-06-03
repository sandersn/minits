module Minits.Compile
open Lex
open Parse
open Bind
open Check
open Transform
open Emit
let compile (s: string) = 
    let (tree, parseErrors) = lex s false |> parse
    let boundTree = bind tree
    let js = snd tree |> transform |> emit
    (boundTree, parseErrors @ check boundTree, js)