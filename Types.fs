module Minits.Types
type Token =
 | IntLiteral of text: string * value: int
 | Identifier of text: string
 | Whitespace
 | Plus
 | Minus
 | Star
 | ForwardSlash
 | Unknown
 | EOF
