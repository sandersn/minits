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
      errors.Add "Expected: identifier"
      "(missing)"
  let parseToken token =
    match lexer.scan () with
    | t when t = token -> t
    | _ -> 
      errors.Add <| sprintf "Expected: token %A" token
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
    | _ -> failwith "parseExpression: expected 'var' or an identifier."
  let parseStatement () =
    ExpressionStatement <| parseExpression ()
  let parseProgram () =
    parseSeparated parseStatement Newline
  (parseProgram (), List.ofSeq errors)