module Minits.Lex
open System
open Types
let keywords = Map.ofList [
  "function", Function
  "var", Token.Var
  "if", If
  "else", Else
  "return", Return
]
let lex (s : string) (includeWhitespace : bool)= 
    let mutable pos = 0
    let mutable token = BOF
    let rec scanToken () =
        let start = pos
        let scanForward pred =
            while pos < s.Length && pred s.[pos] do
              pos <- pos + 1
        let scanIdentifier () = 
          scanForward (fun c -> c = '_' || Char.IsLetterOrDigit c)
          let id = s.[start..pos - 1]
          defaultArg (Map.tryFind id keywords) (Token.Identifier id)
        match s.[pos] with
        | '\n' -> pos <- pos + 1; Newline
        | c when Char.IsWhiteSpace c ->
          scanForward (fun c -> c <> '\n' && Char.IsWhiteSpace c)
          if includeWhitespace then Whitespace else scanToken ()
        | c when Char.IsNumber c-> 
          scanForward Char.IsNumber
          Token.IntLiteral(s.[start..pos - 1], int s.[start..pos - 1]) // TODO: Catch too-large exceptions and whatnot
        | c when Char.IsLetter c  -> scanIdentifier ()
        | '"' as c ->
          pos <- pos + 1
          scanForward (fun c -> c <> '"')
          pos <- pos + 1
          Token.StringLiteral(s.[start..pos - 1], s.[start + 1..pos - 2])
        | '/' as c ->
          pos <- pos + 1
          if pos < s.Length && s.[pos] = '/' 
          then scanForward (fun c -> c <> '\n'); scanToken() 
          else Unknown
        | '_' as c -> scanIdentifier ()
        | '{' -> pos <- pos + 1; LeftBrace
        | '}' -> pos <- pos + 1; RightBrace
        | '(' -> pos <- pos + 1; LeftParen
        | ')' -> pos <- pos + 1; RightParen
        | '=' -> pos <- pos + 1; Equals
        | ';' -> pos <- pos + 1; Semicolon
        | ':' -> pos <- pos + 1; Colon
        | _ -> 
          pos <- pos + 1
          Unknown
    { scan = fun () -> 
        token <- if pos = s.Length then EOF else scanToken ()
      pos = fun () -> pos
      token = fun () -> token
    }
let lexAll (s: string) =
  let lexer = lex s false
  let rec scanLoop acc =
      lexer.scan ()
      match lexer.token () with
      | EOF -> List.rev acc
      | t -> scanLoop (t :: acc)
  scanLoop []
