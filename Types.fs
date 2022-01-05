module Minits.Types
open System.Collections.Generic
(* Lexer *)
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
 | IntLiteral of text: string * value: int // TODO: This was a bad idea
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
(* Parser *)
type Type =
| Identifier of string
| Literal of list<Property>
| Arrow of parameters: list<Property> * ret: Type
| Array of Type
| Reference of Ref<Type>
and Property = string * Type
type LValue =
| Identifier of string
| PropertyAccess of l: LValue * r: string
| ArrayAccess of l: LValue * r: Expression
and Expression = 
| LValue of LValue
| IntLiteral of int
| StringLiteral of string
| Negative of Expression
| Binary of l: Expression * op: Token * r: Expression
| Assignment of lvalue: LValue * value: Expression
| Sequence of list<Expression>
| Call of Expression * arguments: list<Expression>
| RecordCons of typename: string * inits: list<string * Expression>
| ArrayCons of inits: list<Expression>
| If of condition: Expression * consequent: Expression * alternate: Expression
| While of condition: Expression * action: Expression
| For of name: string * start: Expression * stop: Expression * action: Expression
| Let of decls: list<Declaration> * body: Expression
| Break
| Null
and Declaration =
| File of list<Declaration>
| ExpressionStatement of Expression
| Type of name: string * Type
| Var of name: string * t: Option<Type> * init: Expression
| Param of Property
| Function of name: string * parameters: list<Declaration> * ret: Option<Type> * body: Expression
(* Binder *)
type Symbol = {
  var: option<Declaration>
  typ: option<Declaration>
}
type Table = Map<string, Symbol>
type Environment = Map<Declaration, Table>
type Meaning = Type | Value
type Module = Table * list<Declaration>
(* Checker *)
type ResolvedTypes = {
  declarations: Dictionary<Declaration, Type>
  expressions: Dictionary<Expression, Type>
  types: Dictionary<Type, Type>
  lvalues: Dictionary<LValue, Type>
}
(* * IR emit * *)
(* Frames *)
// TODO: depth: int doesn't seem to be used later, only escape: bool
type Escape = Map<Declaration, int * bool>
type Label = string
type Temp = string * int
type Frame = {
    decl: Declaration
    name: Label
    formals: list<bool>
}
type Access = 
  | InFrame of int 
  | InReg of Temp
(* Translater *)
type Level = {
    parent: Option<Level>
    frame: Frame
}
type Fragment = String of Label * string
type TrAccess = Level * Access
type IExpression = 
| Const of int
| Name of Label
| Temp of Temp
| Binop of l: IExpression * IBinop * r: IExpression
| Mem of IExpression
| Call of IExpression * list<IExpression>
| Eseq of IStatement * IExpression
and IStatement =
| Move of IExpression * IExpression
| Exp of IExpression
| Jump of IExpression * list<Label>
| CJump of l: IExpression * op: IRelop * r: IExpression * t: Label * f: Label
| Seq of list<IStatement>
| Label of Label
and IBinop =
| Plus | Minus | Mul | Div | And | Or | LShift | RShift | ARShift | Xor
and IRelop =
| Eq | Ne | Lt | Gt | Le | Ge | Ult | Ule | Ugt | Uge
