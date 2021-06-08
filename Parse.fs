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
  let parseMany elt =
    let rec loop acc =
      match elt () with
      | Some item -> loop (item :: acc)
      | None -> List.rev acc
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
      errors.Add <| sprintf "parseExpression: expected literal or an identifier, got %A" t
      Expression.Identifier "(missing)"
  let isStartOfExpression = function
  | Token.Identifier _ | Token.IntLiteral _ -> true
  | _ -> false
  let parseStatement () =
    let st = match lexer.token () with
             | Token.Var ->
               lexer.scan ()
               let name = parseIdentifier ()
               let typename = if parseOptional Colon then Some <| parseIdentifier () else None
               parseExpected Equals
               let init = parseExpression ()
               Statement.Var (name, typename, init) |> Some
             | t when isStartOfExpression t -> ExpressionStatement <| parseExpression () |> Some
             | _ -> None
    parseOptional Newline |> ignore
    st
  let parseProgram () =
    let statements = parseMany parseStatement
    parseExpected EOF
    (Map.empty, statements)
  lexer.scan ()
  (parseProgram (), List.ofSeq errors)