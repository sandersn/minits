module Minits.Lex
open System
open Types
let lex (s : string) = 
    let mutable pos =  0
    let scan () = 
        if pos = s.Length then EOF else
        let start = pos
        let scanForward pred =
            while pos < s.Length && pred s.[pos] do
              pos <- pos + 1
        let scanIdentifier () = 
          scanForward (fun c -> c = '_' || Char.IsLetterOrDigit c)
          Identifier s.[start..pos - 1]
        match s.[pos] with
        | c when Char.IsWhiteSpace c ->
          scanForward Char.IsWhiteSpace
          Whitespace
        | c when Char.IsNumber c-> 
          scanForward Char.IsNumber
          IntLiteral(s.[start..pos - 1], int s.[start..pos - 1]) // TODO: Catch too-large exceptions and whatnot
        | c when Char.IsLetter c  -> scanIdentifier ()
        | '_' as c -> scanIdentifier ()
        | _ -> 
          pos <- pos + 1
          Unknown
    // TODO: A real scanner would return `scan` and let the parser drive.
    // It would also return an accessor for `pos` and the rest of its state.
    let rec scanLoop acc =
        match scan () with
        | EOF -> List.rev acc
        | t -> scanLoop (t :: acc)
    scanLoop []
