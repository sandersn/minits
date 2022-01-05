module Minits.Traverse
open Types
let paramOnly f = function
| Param (n,t) as p -> f p (n,t)
| d -> failwith $"Only expected parameters in parameter list, got {d}"
let defaultOptionToList f x = defaultArg (Option.map f x) []
let toList (tree: Declaration) mapDecl mapExpr mapLVal mapType : list<'a> = 
  let rec mapDeclToList decl = mapDecl decl :: mapDeclToList' decl
  and mapDeclToList' = function
  | File decls -> List.collect mapDeclToList decls
  | ExpressionStatement e -> mapExprToList e
  | Declaration.Type(_, t) -> mapTypeToList t
  | Var(_, t, init) -> defaultOptionToList mapTypeToList t @ mapExprToList init
  | Param(_, t) -> mapTypeToList t
  | Function(_, parameters, ret, body) -> 
    List.collect mapDeclToList parameters 
    @ defaultOptionToList mapTypeToList ret 
    @ mapExprToList body
  and mapExprToList expr = mapExpr expr :: mapExprToList' expr
  and mapExprToList'  = function
  | LValue l -> mapLValToList l
  | Negative e -> mapExprToList e
  | Binary(l, op, r) -> mapExprToList l @ mapExprToList r
  | Assignment(lval, init) -> mapLValToList lval @ mapExprToList init
  | Sequence es -> List.collect mapExprToList es
  | Expression.Call(e, args) -> mapExprToList e @ List.collect mapExprToList args
  | RecordCons(_, inits) -> List.collect (snd >> mapExprToList) inits
  | ArrayCons inits -> List.collect mapExprToList inits
  | If(cond, cons, alt) -> mapExprToList cond @ mapExprToList cons @ mapExprToList alt
  | While(cond, action) -> mapExprToList cond @ mapExprToList action
  | For(_, start, stop, action) -> mapExprToList start @ mapExprToList stop @ mapExprToList action
  | Let(decls, body) -> List.collect mapDeclToList decls @ mapExprToList body
  | IntLiteral _ | StringLiteral _ | Break | Null -> []
  and mapLValToList lval = mapLVal lval :: mapLValToList' lval
  and mapLValToList' = function
  | Identifier _ -> []
  | PropertyAccess (l,_) -> mapLValToList l
  | ArrayAccess(l,r) -> mapLValToList l @ mapExprToList r
  and mapTypeToList t = mapType t :: mapTypeToList' t
  and mapTypeToList' = function
  | Type.Identifier _ -> []
  | Literal props -> List.collect (snd >> mapTypeToList) props
  | Array t -> mapTypeToList t
  | Arrow (ps, ret) -> List.collect (snd >> mapTypeToList) ps @ mapTypeToList ret
  | Reference r -> mapTypeToList r.contents
  mapDeclToList tree
let addToScope (env : Environment) (decl : Declaration) (scope : list<Table>) =
  Map.find decl env :: scope
