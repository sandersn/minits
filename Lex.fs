module Minits.Lex
open System
open Types
let lex (s : string) = 
    let mutable pos =  0
    let scan () = 
        if pos = s.Length then EOF else
        let scanForward pred =
            while pos < s.Length && pred s.[pos] do
              pos <- pos + 1
        let start = pos
        match s.[pos] with
        | '+' -> pos <- pos + 1; Plus
        | '-' -> pos <- pos + 1; Minus
        | '*' -> pos <- pos + 1; Star
        | '/' -> pos <- pos + 1; ForwardSlash
        | c when Char.IsWhiteSpace c ->
          scanForward Char.IsWhiteSpace
          Whitespace
        | c when Char.IsNumber c-> 
          scanForward Char.IsNumber
          IntLiteral(s.[start..pos - 1], int s.[start..pos - 1]) // TODO: Catch too-large exceptions and whatnot
        | c when Char.IsLetter c ->
          scanForward Char.IsLetterOrDigit
          Identifier s.[start..pos - 1]
        | _ -> 
          pos <- pos + 1
          Unknown
    let rec scanLoop acc = // TODO: Mutable state probs
        match scan () with
        | EOF -> List.rev acc
        | t -> scanLoop (t :: acc)
    scanLoop []
