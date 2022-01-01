module Minits.Lex
open System
open Types
let keywords = Map.ofList [
  "type", Token.Type
  "var", Token.Var
  "function", Token.Function
  "if", Token.If
  "then", Then
  "else", Else
  "while", Token.While
  "do", Do
  "for", Token.For
  "to", To
  "in", In
  "break", Token.Break
  "let", Token.Let
  "null", Token.Null
]
// / and /= are handled with comment handling
let punctuation = Map.ofList [
  '{', LeftBrace
  '}', RightBrace
  '(', LeftParen
  ')', RightParen
  '[', LeftBracket
  ']', RightBracket
  '<', LessThan
  '>', GreaterThan
  '=', Equals
  '+', Token.Plus
  '-', Token.Minus
  '*', Asterisk
  '&', Ampersand
  '|', Pipe
  ';', Semicolon
  ':', Colon
  ',', Comma
  '.', Period
]
let digraphs = Map.ofList [
  '<', LessThanEquals
  '>', GreaterThanEquals
  '=', DoubleEquals
]
let lex (s : string) = 
    let mutable pos = 0
    let mutable token = BOF
    let rec scanToken () =
        let scanForward pred =
            while pos < s.Length && pred s.[pos] do
              pos <- pos + 1
        scanForward Char.IsWhiteSpace
        if pos = s.Length then EOF else
        let start = pos
        let scanIdentifier () = 
          scanForward (fun c -> c = '_' || Char.IsLetterOrDigit c)
          let id = s.[start..pos - 1]
          defaultArg (Map.tryFind id keywords) (Token.Identifier id)
        match s.[pos] with
        | c when Char.IsNumber c -> 
          scanForward Char.IsNumber
          Token.IntLiteral(s.[start..pos - 1], int s.[start..pos - 1]) // TODO: Catch too-large exceptions and whatnot
        | c when Char.IsLetter c  -> scanIdentifier ()
        | '_' as c -> scanIdentifier ()
        | '"' as c ->
          pos <- pos + 1
          scanForward (fun c -> c <> '"')
          pos <- pos + 1
          Token.StringLiteral(s.[start..pos - 1], s.[start + 1..pos - 2])
        | '/' as c ->
          pos <- pos + 1
          // TODO: Need to check pos < length before match and scanToken
          match s.[pos] with
          | '/' -> scanForward (fun c -> c <> '\n'); scanToken()
          | '=' -> pos <- pos + 1; ForwardSlashEquals
          | _ -> ForwardSlash
        | c -> 
          pos <- pos + 1
          match Map.tryFind c digraphs with
          | Some digraph when pos < s.Length && s.[pos] = '=' -> pos <- pos + 1; digraph
          | _ -> defaultArg (Map.tryFind c punctuation) Unknown
    { scan = fun () -> token <- scanToken ()
      pos = fun () -> pos
      token = fun () -> token
    }
let lexAll (s: string) =
  let lexer = lex s
  let rec scanLoop acc =
      lexer.scan ()
      match lexer.token () with
      | EOF -> List.rev acc
      | t -> scanLoop (t :: acc)
  scanLoop []
