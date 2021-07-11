module Minits.Test
open Types
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
let formatEnvironment (env: Environment) = 
  let shorten a =
    let s = sprintf "%A" a
    if s.Length > 25 then s.Replace("\n", "").[0..22] + "..." else s
  env 
  |> Map.toList
  |> List.map (fun (d,t) -> 
     sprintf "%s:\n  %s" 
       (shorten d) 
       (System.String.Join("\n  ", (Map.map (fun _ d -> shorten d) t))))
let run () =
    let lexResult = 
        lexTests 
        |> List.sumBy (fun (name,text) -> lexAll text |> test "lex" name) 
    let compileResult = 
        System.IO.Directory.GetFiles "tests"
        |> Array.sumBy (fun file -> 
          let (tree, environment, errors, emit) = file |> System.IO.File.ReadAllText |> compile
          let name = file.Substring ("tests/".Length, file.IndexOf ".tig" - "tests/".Length)
          test "tree" name (tree, formatEnvironment environment) + test "error" name errors + test "js" name emit)
    let result = lexResult + compileResult
    if result = 0 then printfn "All tests passed." else printfn "%d tests failed." result
    result