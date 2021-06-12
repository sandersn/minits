module Minits.Test
open Lex
open Compile
let test (kind : string) (name : string) value =
    let reference = "baselines/reference/" + name + "." + kind + ".baseline"
    let local = "baselines/local/" + name + "." + kind + ".baseline"
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
    "functionLex", "function f (x) = (x)"
    "ifLex", "if f(x) then y else (z)"
    "semicolonLex", "x; y"
    "stringLex", "var s: string = \"xyz\"\n"
    "commentLex", "x // this is a comment\nthisisnt"
    "newlineLex", "x\n y  \n"
    "commaLex", "x, y, z"
    "angleBracketsLex", "A<T>"
    "typeLex", "type x = y"
    "dotLex", "x.y"
    "bracketLex", "x[y]"
    "nullLex", "evil = null"
    "keywordLex", "type var function if then else while do for to in break let null"
    "operatorLex", "< > <= >= = + - * / == /= & |"
]
let compileTests: list<string * string> = [ ]
let run () =
    let lexResult = 
        lexTests 
        |> List.map (fun (name,text) -> lexAll text |> test "lex" name) 
        |> List.sum
    let compileResult = 
        System.IO.Directory.GetFiles "tests"
        |> Array.map (fun file -> 
          let (tree, errors, emit) = file |> System.IO.File.ReadAllText |> compile
          let name = file.Substring ("tests/".Length, file.IndexOf ".ts" - "tests/".Length)
          test "tree" name tree + test "error" name errors + test "js" name emit)
        |> Array.sum
    let result = lexResult + compileResult
    if result = 0 then printfn "All tests passed." else printfn "%d tests failed." result
    result