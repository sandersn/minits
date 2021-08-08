module Minits.Test
open Types
open Lex
open Compile
open Check
open Emit
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
let shorten (s: string) =
  let s' = s.Replace("\n", " ")
  if s'.Length > 35 then s'.[0..32] + "..." else s'
let formatEnvironment (env: Environment) = 
  let formatSymbol = function
  | { var = Some(var); typ = None } -> shorten <| emitDeclaration var
  | { var = None; typ = Some(typ) } -> shorten <| emitDeclaration typ
  | { var=Some(var); typ=Some(typ) } -> sprintf "{ var=%s; typ=%s }" (shorten <| emitDeclaration var) (shorten <| emitDeclaration typ)
  | s -> failwith <| $"Should not have an empty symbol like {s}"
  env 
  |> Map.toList
  |> List.map (fun (d,t) -> 
     sprintf "%s:\n    %s" 
       (shorten <| emitDeclaration d) 
       (System.String.Join("\n    ", (Map.map (fun _ s -> formatSymbol s) t))))
let getTypesOfNodes (decl: Declaration) (types: ResolvedTypes): string =
  let getTypesOfDeclaration = function
  | ExpressionStatement _ -> None
  | decl -> Some $"{shorten <| emitDeclaration decl} :: %A{getTypeOfDeclaration types decl |> Option.map typeToString}"
  let getTypesOfExpression = function
  | LValue _ -> None
  | e -> Some $"{shorten <| emitExpression e} :: %A{getTypeOfExpression types e |> Option.map typeToString}"
  let getTypesOfLValue l = Some $"{shorten <| emitLValue l} :: %A{getTypeOfLValue types l |> Option.map typeToString}"
  let getTypesOfType t = Some $"{shorten <| typeToString t} :: %A{getTypeOfType types t |> Option.map typeToString}"
  Traverse.toList decl getTypesOfDeclaration getTypesOfExpression getTypesOfLValue getTypesOfType |> List.filter Option.isSome |> List.map Option.get |> String.concat "\n"
let run () =
    let lexResult = 
        lexTests 
        |> List.sumBy (fun (name,text) -> lexAll text |> test "lex" name) 
    let compileResult = 
        System.IO.Directory.GetFiles "tests"
        |> Array.sumBy (fun file -> 
          let (tree, environment, types, errors, emit) = file |> System.IO.File.ReadAllText |> compile
          let name = file.Substring ("tests/".Length, file.IndexOf ".tig" - "tests/".Length)
          test "tree" name (tree, formatEnvironment environment) 
          + test "types" name (getTypesOfNodes tree types)
          + test "error" name errors 
          + test "js" name emit)
    let result = lexResult + compileResult
    if result = 0 then printfn "All tests passed." else printfn "%d tests failed." result
    result