module Minits.Types
// TODO: Change token nam/intes to be less convenient to avoid clash with others
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
type LValue =
| Identifier of string
| Property of l: LValue * r: string
| Array of l: LValue * r: Expression
and Expression = 
 | LValue of LValue
 | IntLiteral of int
 | StringLiteral of string
 | Assignment of lvalue: LValue * value: Expression
 | Sequence of list<Expression>
 | Null
type Declaration =
 | ExpressionStatement of Expression
 | Type of name: string * Type
 | Var of name: string * t: Option<Type> * init: Expression
 | Function of name: string * parameters: list<Property> * ret: Option<Type> * body: Expression
type Table = Map<string,Declaration>
type Module = Table * list<Declaration>
let stringType = Type.Identifier "string"
let intType = Type.Identifier "int"
let errorType = Type.Identifier "error"
let nullType = Type.Identifier "null"
