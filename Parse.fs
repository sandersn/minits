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
  let rec parseType () =
    match parseToken () with
    | Token.Identifier(text) when text = "Array" -> 
      parseExpected LessThan
      let t = parseType ()
      parseExpected GreaterThan
      Array t
    | Token.Identifier(text) -> Type.Identifier text
    | LeftBrace -> 
      let literal = Literal <| parseMany parseProperty
      parseExpected RightBrace
      literal
    | t ->
      errors.Add <| sprintf "parseType: expected identifier or {, got %A" t
      Type.Identifier "(missing)"
  and parseProperty () =
    match lexer.token () with
    | Token.Identifier _ ->
      let id = parseIdentifier ()
      parseExpected Colon
      let t = parseType ()
      parseOptional Comma |> ignore
      Some (id, t)
    | _ -> None
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
  let parseDeclaration () =
    let st = match lexer.token () with
             | Token.Var ->
               lexer.scan ()
               let name = parseIdentifier ()
               let typename = if parseOptional Colon then Some <| parseType () else None
               parseExpected Equals
               let init = parseExpression ()
               Declaration.Var (name, typename, init) |> Some
             | Token.Type ->
               lexer.scan ()
               let name = parseIdentifier ()
               parseExpected Equals
               let t = parseType ()
               Declaration.Type (name, t) |> Some
             | Token.Function ->
               lexer.scan ()
               let name = parseIdentifier ()
               parseExpected LeftParen
               let parameters = parseMany parseProperty
               parseExpected RightParen
               let ret = if parseOptional Colon then Some <| parseType () else None
               parseExpected Equals
               let body = parseExpression ()
               Declaration.Function (name, parameters, ret, body) |> Some
             | t when isStartOfExpression t -> ExpressionStatement <| parseExpression () |> Some
             | _ -> None
    parseOptional Semicolon |> ignore
    st
  let parseProgram () =
    let statements = parseMany parseDeclaration
    parseExpected EOF
    (Map.empty, statements)
  lexer.scan ()
  (parseProgram (), List.ofSeq errors)