module Minits.Types
type Node =
 | Statement
 | Program of list<Node>
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
