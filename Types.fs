module Minits.Types
type Token =
 | Type
 | Var
 | Function
 | If
 | Then
 | Else
 | While
 | Do
 | For
 | To
 | In
 | Break
 | Let
 | Null
 | LeftBrace
 | RightBrace
 | LeftParen
 | RightParen
 | LeftBracket
 | RightBracket
 | LessThan
 | LessThanEquals
 | GreaterThan
 | GreaterThanEquals
 | Equals
 | DoubleEquals
 | Plus
 | Minus
 | Asterisk
 | ForwardSlash
 | ForwardSlashEquals
 | Ampersand
 | Pipe
 | IntLiteral of text: string * value: int
 | StringLiteral of text: string * value: string
 | Identifier of text: string
 | Newline
 | Semicolon
 | Colon
 | Comma
 | Period
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
