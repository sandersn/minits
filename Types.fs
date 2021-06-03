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
 | BOF
 | EOF
type Lexer = {
    scan: unit -> unit
    token: unit -> Token
    pos: unit -> int
}
type Expression = 
 | Identifier of string
 | IntLiteral of int
 | Assignment of name: string * value: Expression
type Statement =
 | ExpressionStatement of Expression
 | Var of name: string * init: Expression
type Type = Type of string
type Table = Map<string,Statement>
type Module = Table * list<Statement>
