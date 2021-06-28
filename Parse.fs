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
  let parseMany element =
    let rec loop acc =
      match element () with
      | Some item -> loop (item :: acc)
      | None -> List.rev acc
    loop []
  let parseTerminated element isStart terminal =
    parseMany (fun () -> 
      let d = if isStart (lexer.token ()) then element () |> Some else None
      parseOptional terminal |> ignore
      d)
  let parseName () =
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
      Type.Array t
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
      let id = parseName ()
      parseExpected Colon
      let t = parseType ()
      parseOptional Comma |> ignore
      Some (id, t)
    | _ -> None
  let isStartOfExpression = function
  | Token.Identifier _ | Token.StringLiteral _ | Token.IntLiteral _ | Token.Null | Token.LeftParen -> true
  | _ -> false
  let isStartOfDeclaration = function
  | Token.Var | Token.Type | Token.Function -> true
  | t -> isStartOfExpression t
  // parseExpression -> parseAssignment -> parseBoolean -> parseLogicalComparison -> parsePlusMinus
  let rec parseExpression () =
    parseTimesDivide ()
  and parseTimesDivide () = 
  // TODO: Normal recursion produces a right-associative parse (right-deep);
  // use a linear recursion like in parseLValue; parseExpression can probably reach through
  // to parseNegative and pass the result to parseTimesDivide (eventually, the top-most associative
  // construct)
    let e = parseNegative ()
    if parseOptional Token.Asterisk then Binary (e, Asterisk, parseExpression ())
    elif parseOptional Token.ForwardSlash then Binary (e, ForwardSlash, parseExpression ())
    else e
  and parseNegative () =
    if parseOptional Minus then Negative (parseCall ()) else parseCall ()
  and parseCall () =
    let e = parseSingleExpression ()
    if parseOptional LeftParen then
      let call = Call (e, parseTerminated parseExpression isStartOfExpression Comma)
      parseExpected RightParen |> ignore
      call
    else e
  and parseSingleExpression () =
    match parseToken () with
    | Token.Identifier(text) as t -> 
      let lvalue = parseLValue (LValue.Identifier text)
      // TODO: Move assignment parsing outside
      if parseOptional Equals then Assignment (lvalue, parseExpression ()) 
      else LValue lvalue
    | Token.IntLiteral(_,value) -> Expression.IntLiteral value
    | Token.StringLiteral(_,value) -> Expression.StringLiteral value
    | LeftParen -> 
      let es = parseTerminated parseExpression isStartOfExpression Semicolon
      parseExpected RightParen
      Sequence es
    | Token.Null -> Expression.Null
    | t -> 
      errors.Add <| sprintf "parseExpression: expected literal or an identifier, got %A" t
      LValue <| Identifier "(missing)"
  and parseLValue (acc: LValue) =
    if parseOptional Period then 
      LValue.Property(acc, parseName ()) |> parseLValue
    elif parseOptional LeftBracket then 
      let acc' = LValue.Array(acc, parseExpression ())
      parseExpected RightBracket
      parseLValue acc'
    else acc
  let parseDeclaration () =
     if parseOptional Token.Var then
       let name = parseName ()
       let typename = if parseOptional Colon then Some <| parseType () else None
       parseExpected Equals
       let init = parseExpression ()
       Declaration.Var (name, typename, init)
     elif parseOptional Token.Type then
       let name = parseName ()
       parseExpected Equals
       let t = parseType ()
       Declaration.Type (name, t)
     elif parseOptional Token.Function then
       let name = parseName ()
       parseExpected LeftParen
       let parameters = parseMany parseProperty
       parseExpected RightParen
       let ret = if parseOptional Colon then Some <| parseType () else None
       parseExpected Equals
       let body = parseExpression ()
       Declaration.Function (name, parameters, ret, body)
     elif isStartOfExpression (lexer.token ()) then 
       parseExpression () |> ExpressionStatement
     else
       errors.Add <| sprintf "parseDeclaration: expected 'var' or 'type' or 'function', got %A" (parseToken ())
       Identifier "missing" |> LValue |> ExpressionStatement
  let parseProgram () =
    let statements = parseTerminated parseDeclaration isStartOfDeclaration Semicolon
    parseExpected EOF
    (Map.empty, statements)
  lexer.scan ()
  (parseProgram (), List.ofSeq errors)