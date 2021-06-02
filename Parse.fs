module Minits.Parse
open Types
let parse (lexer: Lexer) = 
// TODO: lexer needs to skip whitespace (by default)
// TODO: Errors should have positions, maybe
  let errors = System.Collections.Generic.List ()
  let parseSeparated parser token =
    // TODO: This is wrong
    [parser ()]
  let parseIdentifier () =
    match lexer.scan () with
    | Token.Identifier(text) -> text
    | _ -> 
      errors.Add "parseIdentifier: Expected"
      "(missing)"
  let parseToken token =
    match lexer.scan () with
    | t when t = token -> t
    | _ -> 
      errors.Add <| sprintf "parseToken: Expected %A" token
      token
  let rec parseExpression () =
    match lexer.scan () with
    // TODO: Ints are expressions too
    | Token.Var as t ->
      let name = parseIdentifier ()
      parseToken Equals |> ignore
      let init = parseExpression ()
      Expression.Var (name, init)
    | Token.Identifier(text) as t -> Expression.Identifier text
    | Token.IntLiteral(_,value) -> Expression.IntLiteral value
    | t -> 
      errors.Add <| sprintf "parseExpression: expected 'var' or an identifier, got %A" t
      Expression.Identifier "(missing)"
  let parseStatement () =
    ExpressionStatement <| parseExpression ()
  let parseProgram () =
    parseSeparated parseStatement Newline
  (parseProgram (), List.ofSeq errors)