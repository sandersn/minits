module Minits.Types
type Token =
 | Function
 | Var
 | If
 | Else
 | Return
 | LeftBrace
 | RightBrace
 | LeftParen
 | RightParen
 | Equals
 | IntLiteral of text: string * value: int
 | Identifier of text: string
 | Newline
 | Semicolon
 | Whitespace
 | Unknown
 | EOF
type Lexer = {
    scan: unit -> Token
    pos: unit -> int
}
type Expression = 
 | Identifier of string // MORE TO COME, OBVS
 | IntLiteral of int
 | Var of name: string * init: Expression
type Statement = ExpressionStatement of Expression // MORE TO COME
type Program = list<Statement>
