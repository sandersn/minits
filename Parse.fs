module Minits.Parse
open Types
let parse (lexer: Lexer) : Declaration * list<string> = 
  let errors = System.Collections.Generic.List ()
  let parseOptional expected =
    let t = lexer.token ()
    if t = expected then
      lexer.scan ()
    t = expected
  let parseSome expecteds = 
    match List.tryFind ((=) (lexer.token ())) expecteds with
    | Some t -> lexer.scan(); Some t
    | None -> None
  let parseToken () =
    let t = lexer.token ()
    lexer.scan ()
    t
  let parseExpected expected =
    if parseOptional expected 
    then ()
    else errors.Add $"parseToken: Expected {expected} but got {lexer.token()} at {lexer.pos()}"
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
    (*
      Option.default (parseOne [Identifier, LeftBrace] (function | Identifier -> ...)) (Type.Identifier "(missing)")
    *)
    match parseToken () with
    | Token.Identifier(text) when text = "Array" -> 
      parseExpected LessThan
      let t = parseType ()
      parseExpected GreaterThan
      Type.Array t
    | Token.Identifier(text) -> Type.Identifier text
    | LeftBrace -> 
      let literal = Literal <| parseMany parseProperty // TODO: Should be parseTerminated?
      parseExpected RightBrace
      literal
    | t ->
      errors.Add (sprintf "parseType: expected identifier or {, got %A" t)
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
  | Token.Identifier _ | Token.StringLiteral _ | Token.IntLiteral _ 
  | Token.If | Token.For | Token.While | Token.Let
  | Token.Null | Token.Break
  | Token.LeftParen | Token.LeftBracket -> true
  | _ -> false
  let isStartOfDeclaration = function
  | Token.Var | Token.Type | Token.Function -> true
  | t -> isStartOfExpression t
  let rec parseExpression () =
    let l = parseOr ()
    if parseOptional Equals then 
      let r = parseExpression ()
      match l with
      | LValue l' -> Assignment (l', r)
      | _ -> 
        errors.Add "Expected identifier, property access or array access on lhs of assignment"
        Binary (l, Equals, r)
    else l
  and parseOr () = parseBinary [Pipe] parseAnd
  and parseAnd () = parseBinary [Ampersand] parseLogicalComparison
  and parseLogicalComparison () = parseLogicalComparison' (parsePlusMinus ())
  and parseLogicalComparison' acc = 
    match parseSome [LessThan; GreaterThan; LessThanEquals; GreaterThanEquals; DoubleEquals; ForwardSlashEquals] with
    | Some t -> Binary (acc, t, parsePlusMinus ())
    | None -> acc
  and parsePlusMinus () = parseBinary [Plus; Minus] parseTimesDivide
  and parseTimesDivide () = parseBinary  [Asterisk; ForwardSlash] parseNegative
  and parseNegative () =
    if parseOptional Minus then Negative (parseConstructor ()) else parseConstructor ()
  and parseBinary ops seed = parseBinary' ops seed (seed ())
  and parseBinary' ops seed acc =
    match parseSome ops with
    | Some t -> parseBinary' ops seed (Binary (acc, t, seed ()))
    | None -> acc
  and parseConstructor () = 
    match parseCall () with
    | LValue l ->
      match l with
      | Identifier name ->
        if parseOptional LeftBrace then
          let inits = parseRecordInitialisers ()
          parseExpected RightBrace
          RecordCons(name, inits)
        else LValue (Identifier name)
      | l -> LValue l
    | e -> e
  and parseRecordInitialisers () = parseTerminated parseRecordInitialiser isStartOfExpression Comma
  and parseRecordInitialiser () =
    let name = parseName ()
    parseExpected Equals
    let e = parseExpression ()
    (name, e)
  and parseCall () =
    let e = parseSingleExpression ()
    if parseOptional LeftParen then
      let call = Call (e, parseTerminated parseExpression isStartOfExpression Comma)
      parseExpected RightParen |> ignore
      call
    else e
  and parseSingleExpression () =
    match parseToken () with
    | Token.Identifier(text) as t -> parseLValue (Identifier text) |> LValue
    | Token.IntLiteral(_,value) -> IntLiteral value
    | Token.StringLiteral(_,value) -> StringLiteral value
    | LeftParen -> 
      let es = parseTerminated parseExpression isStartOfExpression Semicolon
      parseExpected RightParen
      if List.length es = 1 then List.head es else Sequence es
    | LeftBracket -> 
      let inits = parseTerminated parseExpression isStartOfExpression Comma
      parseExpected RightBracket
      ArrayCons(inits)
    | Token.If ->
      let cond = parseExpression ()
      parseExpected Then
      let cons = parseExpression ()
      parseExpected Else
      let alt = parseExpression ()
      If (cond, cons, alt)
    | Token.While ->
      let cond = parseExpression ()
      parseExpected Do
      let action = parseExpression ()
      While (cond, action)
    | Token.For ->
      let id = parseName ()
      parseExpected Equals
      let start = parseExpression ()
      parseExpected To
      let stop = parseExpression ()
      parseExpected Do
      let action = parseExpression ()
      For (id, start, stop, action)
    | Token.Let ->
      let decls = parseTerminated parseDeclaration isStartOfDeclaration Semicolon
      parseExpected In
      let body = parseExpression ()
      Let (decls, body)
    | Token.Null -> Expression.Null
    | Token.Break -> Expression.Break
    | t -> 
      errors.Add $"parseExpression: expected literal or an identifier, got {t}"
      LValue <| Identifier "(missing)"
  and parseLValue (acc: LValue) =
    if parseOptional Period then 
      PropertyAccess(acc, parseName ()) |> parseLValue
    elif parseOptional LeftBracket then 
      let acc' = ArrayAccess(acc, parseExpression ())
      parseExpected RightBracket
      parseLValue acc'
    else acc
  and parseDeclaration () =
     if parseOptional Token.Var then
       let name = parseName ()
       let typename = if parseOptional Colon then Some <| parseType () else None
       parseExpected Equals
       let init = parseExpression ()
       Var (name, typename, init)
     elif parseOptional Token.Type then
       let name = parseName ()
       parseExpected Equals
       let t = parseType ()
       Declaration.Type (name, t)
     elif parseOptional Token.Function then
       let name = parseName ()
       parseExpected LeftParen
       let parameters = parseMany parseProperty |> List.map Param
       parseExpected RightParen
       let ret = if parseOptional Colon then Some <| parseType () else None
       parseExpected Equals
       let body = parseExpression ()
       Function (name, parameters, ret, body)
     elif isStartOfExpression (lexer.token ()) then 
       parseExpression () |> ExpressionStatement
     else
       errors.Add $"parseDeclaration: expected 'var' or 'type' or 'function', got {parseToken ()}"
       Identifier "missing" |> LValue |> ExpressionStatement
  let parseProgram () =
    let statements = parseTerminated parseDeclaration isStartOfDeclaration Semicolon
    parseExpected EOF
    File statements
  lexer.scan ()
  (parseProgram (), List.ofSeq errors)