module Minits.Translate
open Types
(*
Temp?
Frame?
Semantic
Tree 
Check : Node * Env -> Map Node Type

Check ->
*)
let rec outermost: Level = {
    parent = outermost // lol @ letrec
    name = Temp.newlabel ()
    formals = []
}
let newLevel (parent: Level) (name: Label) (formals: list<bool>) = {
    parent = parent
    name = name
    formals = formals
}
// let formals (l: Level): list<TrAccess> = l.formals |> List.map (fun f -> (l, ()))
let allocLocal (l: Level) (escapes: bool): Access = 
  // TODO: Returning level as the first part of the tuple is redundant and probably wrong
  Frame.allocLocal (Frame.newFrame l.name l.formals) escapes
type Exp =
  | Cx of (Label * Label -> IStatement)
  | Ex of IExpression
  | Nx of IStatement
let toEx = function
| Ex e -> e
| Nx s -> Eseq (s, Const 0)
| Cx c ->
  let r = Temp.newtemp ()
  let t = Temp.newlabel ()
  let f = Temp.newlabel ()
  Eseq (
      Seq [
          Move (Temp r, Const 1)
          c (t,f)
          Label f
          Move (Temp r, Const 0)
          Label t
      ],
      Temp r
  )
let toCx = function
| Ex e -> fun (t,f) -> Label t // TODO
| Cx c -> c
| Nx s -> fun (t,f) -> Label f // TODO
let todoE = Ex (Const 0)
let todoD = Nx <| Exp (Const 0)
// TODO: It would be really nice to have a generic bound-tree Traverse function
// that maintains scope properly and lets you maintain a state variable
// guess I need to read Scrap Your Boilerplate or maybe its descendants
let translate (env: Environment) (decl: Declaration) (globals : Table) =
  let escapes = Frame.escape env decl globals
  let rec translateExpression scope exp level =
    match exp with
    | LValue lvalue -> translateLValue scope lvalue level
    | IntLiteral i -> Const i |> Ex
    | StringLiteral s -> todoE
    | Negative e -> todoE
    | Binary(l,op,r) -> 
        let lt = translateExpression scope l level |> toEx
        let rt = translateExpression scope r level |> toEx
        match op with
        | Token.Plus -> Binop (lt, Plus, rt) |> Ex
        | Token.Minus -> Binop (lt, Minus, rt) |> Ex
        | Asterisk -> Binop (lt, Mul, rt) |> Ex
        | ForwardSlash -> Binop (lt, Div, rt) |> Ex
        | Pipe -> Binop (lt, Or, rt) |> Ex // TODO: if l then 1 else r
        | Ampersand -> Binop (lt, And, rt) |> Ex // TODO: if l then r else 0
        | LessThan -> Cx (fun (t,f) -> CJump(lt, Lt, rt, t, f))
        | GreaterThan -> Cx (fun (t,f) -> CJump(lt, Gt, rt, t, f))
        | LessThanEquals -> Cx (fun (t,f) -> CJump(lt, Le, rt, t, f))
        | GreaterThanEquals -> Cx (fun (t,f) -> CJump(lt, Ge, rt, t, f))
        | DoubleEquals -> Cx (fun (t,f) -> CJump(lt, Eq, rt, t, f))
        | ForwardSlashEquals -> Cx (fun (t,f) -> CJump(lt, Ne, rt, t, f))
        | t -> failwith $"Unexpected binary operator token {t}"
        //checkBinary op
    | Assignment(lvalue, value) -> todoE
    | Expression.Call(e, args) -> todoE
    | Sequence es -> todoE
    | RecordCons (name, inits) -> todoE
    | ArrayCons es -> todoE
    | If (cond, cons, alt) ->
        let r = Temp.newtemp ()
        let t = Temp.newlabel ()
        let f = Temp.newlabel ()
        let join = Temp.newlabel ()
        let cond' = toCx <| translateExpression scope cond level
        // TODO: Write a special-case toEx that generates better code for Cx and Nx
        // if both branches are Nx: generate a version that omits the result
        // and either branch is Cx: copy from the book
        // if both brances are Cx: modify the book's example somehow
        let cons' = toEx <| translateExpression scope cons level
        let alt' = toEx <| translateExpression scope alt level
        Ex (
          Eseq (
              Seq [
                  cond' (t,f)
                  Label t
                  Move (Temp r, cons')
                  Jump (Name join, [join])
                  Label t
                  Move (Temp r, alt')
                  Jump (Name join, [join])
                  Label join
              ],
              Temp r
          ) 
        )
    | While (cond, action) -> todoE
    | For (_, start, stop, action) -> todoE
    | Let (decls, body) as l -> todoE
    | Break -> todoE
    | Null -> todoE
  and translateLValue scope lvalue level =
    match lvalue with
    | Identifier name ->
      simpleVar (Bind.resolve name scope Value |> Option.get) level
    | PropertyAccess (l,r) -> todoE
    | ArrayAccess (l,r) -> todoE
  and simpleVar (statement : Declaration) (level : Level) = 
    match allocLocal level (snd escapes.[statement]) with
    | InReg x -> Ex <| Mem (Temp x)
    | InFrame k -> Ex <| Mem (Binop (Temp Frame.FP, Plus, Const k))
    
  and translateDeclaration scope decl level =
    match decl with
    | File decls -> todoD
    | ExpressionStatement e -> Nx <| Exp (toEx <| translateExpression scope e level)
    | Var(_, typename, init) -> todoD
    | Param(_, typename) -> todoD
    | Declaration.Type(_, t) -> todoD
    | Function (name, parameters, ret, body) -> 
        let level' = newLevel level (Temp.newlabel ()) (parameters |> List.map (fun _ -> false))
        // TODO:use level' when translating body
        todoD
  translateDeclaration [globals] decl outermost
