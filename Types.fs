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
 | StringLiteral of text: string * value: string
 | Identifier of text: string
 | Newline
 | Semicolon
 | Colon
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
 | Var of name: string * typename: Option<string> * init: Expression
type Table = Map<string,Statement>
type Module = Table * list<Statement>
type Type = Type of string
let stringType = Type "string"
let intType = Type "int"
let errorType = Type "error"
