module Minits.Parse
open Types
let parse (lexer: Lexer) = 
// TODO: lexer needs to skip whitespace (by default)
// TODO: All the token matches need to fail on the default case
  let parseSeparated parser token =
    // TODO: This is wrong
    [parser ()]
  let parseIdentifier () =
    match lexer.scan () with
    | Token.Identifier(text) -> Expression.Identifier text
    | _ -> failwith "ho no"
  let parseToken token =
    match lexer.scan () with
    | t when t = token -> t
    | _ -> failwith "open"
  let rec parseExpression () =
    match lexer.scan () with
    // TODO: Ints are expressions too
    | Token.Var as t ->
      let name = parseIdentifier ()
      parseToken Equals |> ignore
      let init = parseExpression ()
      Expression.Var (name, init)
    | Token.Identifier(text) as t -> Expression.Identifier text
  let parseStatement () =
    ExpressionStatement <| parseExpression ()
  let parseProgram () =
    parseSeparated parseStatement Newline
  parseProgram ()