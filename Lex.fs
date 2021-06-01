module Minits.Lex
open System
open Types
let keywords = [
  "function", Function
  "var", Var
  "if", If
  "else", Else
  "return", Return
]
let longestKeyword = keywords |> List.map fst |> List.map (fun s -> s.Length) |> List.max
let lex (s : string) = 
    let mutable pos =  0
    let scanKeyword () =
      let prefix = s.Substring(pos, min longestKeyword (s.Length - pos))
      keywords |> List.tryFind (fst >> prefix.StartsWith)
    let scanToken () =
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
        | '{' -> pos <- pos + 1; LeftBrace
        | '}' -> pos <- pos + 1; RightBrace
        | '(' -> pos <- pos + 1; LeftParen
        | ')' -> pos <- pos + 1; RightParen
        | '=' -> pos <- pos + 1; Equals
        | _ -> 
          pos <- pos + 1
          Unknown
    let scan () = 
        if pos = s.Length then EOF else
        match scanKeyword () with
        | Some (keyword,token) -> 
          pos <- pos + keyword.Length
          token
        | None -> scanToken ()
    // TODO: A real scanner would return `scan` and let the parser drive.
    // It would also return an accessor for `pos` and the rest of its state.
    let rec scanLoop acc =
        match scan () with
        | EOF -> List.rev acc
        | t -> scanLoop (t :: acc)
    scanLoop []
