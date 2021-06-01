module Minits.Test
open Lex
open Parse
open Compile
let test name value =
    let reference = "baselines/reference/" + name + ".baseline"
    let local = "baselines/local/" + name + ".baseline"
    let actual = sprintf "%A" value
    let expected = if System.IO.File.Exists reference then System.IO.File.ReadAllText reference else ""
    if actual <> expected then
        System.IO.File.WriteAllText(local, actual)
        1
    else
        0
let lexTests = [
    "basicLex", "x"
    "firstLex", " 1200Hello    World1! 14d"
    "underscoreLex", "x_y is _aSingle Identifier_"
    "varLex", "var x = 1"
    "functionLex", "function f (x) { return x }"
    "ifLex", "if (f(x)) y else { z }"
    "semicolonLex", "x; y"
    "newlineLex", "x\n y  \n"
]
let parseTests = [
    "basicParse", "x"
]
let compileTests: list<string * string> = [ ]
let run () =
    let lexResult = 
        lexTests 
        |> List.map (fun (name,text) -> lex text |> test name) 
        |> List.sum
    let parseResult = 
        parseTests 
        |> List.map (fun (name,text) -> lex text |> parse |> test name) 
        |> List.sum
    let compileResult = 
        // TODO: Read test from files instead
        compileTests
        |> List.map (fun (name, text) -> compile text |> test name)
        |> List.sum
    let result = lexResult + parseResult + compileResult
    if result = 0 then printf "All tests passed." else printfn "%d tests failed." result
    result