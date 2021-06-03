module Minits.Parse
open Types
let parse (lexer: Lexer) : Module * list<string> = 
  let errors = System.Collections.Generic.List ()
  let parseOptional token =
    if lexer.token () = token then
      lexer.scan ()
      true
    else
      false
  let parseToken () =
    let t = lexer.token ()
    lexer.scan ()
    t
  let parseExpected token =
    match parseToken () with
    | t when t = token -> ()
    | _ -> errors.Add <| sprintf "parseToken: Expected %A" token
  let parseSeparated elt sep =
    let rec loop acc =
      let list = elt () :: acc
      if sep () then loop list else List.rev list
    loop []
  let parseIdentifier () =
    match parseToken () with
    | Token.Identifier(text) -> text
    | _ -> 
      errors.Add "parseIdentifier: Expected"
      "(missing)"
  let rec parseExpression () =
    match parseToken () with
    | Token.Identifier(text) as t -> 
      if parseOptional Equals 
      then Assignment (text, parseExpression ()) 
      else Expression.Identifier text
    | Token.IntLiteral(_,value) -> Expression.IntLiteral value
    | t -> 
      errors.Add <| sprintf "parseExpression: expected 'var' or an identifier, got %A" t
      Expression.Identifier "(missing)"
  let parseStatement () =
    match lexer.token () with
    | Token.Var ->
      lexer.scan ()
      let name = parseIdentifier ()
      let typename = if parseOptional Colon then Some <| parseIdentifier () else None
      parseExpected Equals
      let init = parseExpression ()
      Statement.Var (name, typename, init)
    | _ -> ExpressionStatement <| parseExpression ()
  let parseProgram () =
    let statements = parseSeparated parseStatement (fun () -> parseOptional Newline)
    parseExpected EOF
    (Map.empty, statements)
  lexer.scan ()
  (parseProgram (), List.ofSeq errors)