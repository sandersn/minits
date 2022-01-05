module Minits.Frame
open Traverse
open Types
let newFrame decl name formals = {
    decl = decl
    name = name
    formals = formals
}
let formals (frame: Frame): list<Access> = 
  frame.formals 
  |> List.mapi (fun i escapes -> if escapes then InFrame i else InReg (Temp.newtemp()))
let allocLocal (decl : Declaration) (frame: Frame) (escapes: bool): Access = 
  match escapes, frame.decl with
  | false, Function _ -> InReg (Temp.newtemp())
  // the (1+) in 1 + index-of-param skips over the frame pointer added at index 0
  | true, Function (_, parameters, _, _) -> InFrame <| 1 + List.findIndex ((=)decl) parameters
  | _ -> failwith "Should only get functions in allocLocal"
  
let wordSize = 1
let FP: Temp = ("frame-pointer", 0)
(*
function f(x) =
  x
  function g(x) =
     x // no escape
  function h(y) =
     x // yes escape
     function i(x) =
       x // no escape
*)
let escape (env : Environment) (decl : Declaration) (globals : Table) =
  let mapM f table es = List.fold f table es
  let rec escapeDecl (scope : list<Table>) (table : Escape) depth = function
  | File decls as f -> List.fold (fun t d -> escapeDecl (addToScope env f scope) t depth d) table decls
  | ExpressionStatement e -> escapeExp scope table depth e
  | Var _ as v -> Map.add v (depth, false) table
  | Param _ as p -> Map.add p (depth, false) table
  | Function (_, parameters, _, body) as f -> 
    let table' = (List.fold (fun t d -> escapeDecl (addToScope env f scope) t (depth + 1) d) table parameters)
    escapeExp scope table' (depth + 1) body
  | _ -> table
  and escapeExp (scope : list<Table>) table depth = function
  | LValue lval -> escapeLVal scope table depth lval
  | Negative e -> escapeExp scope table depth e
  | Binary (l,_,r) -> List.fold (fun t e -> escapeExp scope t depth e) table [l; r]
  | Assignment (lval, init) -> escapeExp scope (escapeLVal scope table depth lval) depth init
  | Sequence es -> List.fold (fun t e -> escapeExp scope t depth e) table es
  | Expression.Call (e, args) -> List.fold (fun t e -> escapeExp scope t depth e) table (e :: args)
  | RecordCons (name, inits) ->  List.fold (fun t e -> escapeExp scope t depth e) table (List.map snd inits)
  | ArrayCons inits -> List.fold (fun t e -> escapeExp scope t depth e) table inits
  | If (cond,cons,alt) -> List.fold (fun t e -> escapeExp scope t depth e) table [cond;cons;alt]
  | While (cond,action) -> List.fold (fun t e -> escapeExp scope t depth e) table [cond;action]
  | For (_,start,stop,action) as f -> 
    List.fold (fun t e -> escapeExp (addToScope env (ExpressionStatement f) scope) t depth e) table [start;stop;action]
  | Let (decls,body) as l -> 
    escapeExp scope (List.fold (fun t d -> escapeDecl (addToScope env (ExpressionStatement l) scope) t depth d) table decls) depth body
  | _ -> table
  and escapeLVal scope table depth = function
  // also there is a LOT of boilerplate for something which only really matters for
  // Var, Param, Function, Identifier
  | Identifier id -> 
    let decl = Bind.resolve id scope Value |> Option.get
    let (depth',_) = Map.find decl table
    if depth > depth' then Map.add decl (depth', true) table else table
  | PropertyAccess (l,r) -> escapeLVal scope table depth l
  | ArrayAccess (l,r) -> escapeExp scope (escapeLVal scope table depth l) depth r
  escapeDecl [globals] Map.empty 0 decl