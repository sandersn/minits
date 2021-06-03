module Minits.Compile
open Lex
open Parse
open Bind
open Check
let compile (s: string) = 
    let (tree, errors) = lex s false |> parse
    let boundTree = bind tree
    (boundTree, errors @ check boundTree)