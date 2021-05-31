module Minits.Test
open Lex
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
]
let run () =
    lexTests 
    |> List.map (fun (name,text) -> lex text |> test name) 
    |> List.sum