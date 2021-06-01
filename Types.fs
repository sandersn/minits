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
 | Whitespace
 | Unknown
 | EOF
