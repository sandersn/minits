module Minits.Parse
open Types
let parse (lexer: Lexer) = 
  let errors = System.Collections.Generic.List ()
  let parseOptional token =
    printfn "optional expected %A, found %A" token <| lexer.token ()
    if lexer.token () = token then
      lexer.scan ()
      true
    else
      false
  let parseToken () =
    let t = lexer.token ()
    printfn "%A" t
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
    printfn "parsing expression"
    match parseToken () with
    | Token.Var as t ->
      let name = parseIdentifier ()
      parseExpected Equals
      let init = parseExpression ()
      Expression.Var (name, init)
    | Token.Identifier(text) as t -> 
      if parseOptional Equals 
      then Assignment (text, parseExpression ()) 
      else Expression.Identifier text
    | Token.IntLiteral(_,value) -> Expression.IntLiteral value
    | t -> 
      errors.Add <| sprintf "parseExpression: expected 'var' or an identifier, got %A" t
      Expression.Identifier "(missing)"
  let parseStatement () =
    ExpressionStatement <| parseExpression ()
  let parseProgram () =
    let statements = parseSeparated parseStatement (fun () -> parseOptional Newline)
    parseExpected EOF
    statements
  lexer.scan ()
  (parseProgram (), List.ofSeq errors)