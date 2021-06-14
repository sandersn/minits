module Minits.Types
// TODO: Change token names to be less convenient to avoid clash with others
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
type Type =
 | Identifier of string
 | Literal of list<Property>
 | Array of Type
and Property = string * Type
type Expression = 
 | Identifier of string
 | IntLiteral of int
 | Assignment of name: string * value: Expression
type Declaration =
 | ExpressionStatement of Expression
 | Type of name: string * t: Type
 | Var of name: string * typename: Option<Type> * init: Expression
type Table = Map<string,Declaration>
type Module = Table * list<Declaration>
let stringType = Type.Identifier "string"
let intType = Type.Identifier "int"
let errorType = Type.Identifier "error"
