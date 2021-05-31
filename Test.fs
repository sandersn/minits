module Lex.Test
open Lex
let test name value =
    let actual = sprintf "%A" value
    let expected = System.IO.File.ReadAllText("baselines/reference/" + name + ".baseline")
    if actual <> expected then
        System.IO.File.WriteAllText("baselines/local/" + name + ".baseline", actual)
        1
    else
        0
let run () =
    lex "x" |> test "basicLex" |> ignore
    lex " 1200Hello    World1! 14d" |> test "firstLex"