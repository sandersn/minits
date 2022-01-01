module Minits.Frame
open Types
type Frame = {
    name: Label
    formals: list<bool>
}
type Access = 
  | InFrame of int 
  | InReg of Temp
let newFrame (name: Label) (formals: list<bool>): Frame = {
    name = name
    formals = formals
}
let formals (frame: Frame): list<Access> = 
  frame.formals 
  |> List.mapi (fun i escapes -> if escapes then InFrame i else InReg (Temp.newtemp()))
let allocLocal (frame: Frame) (escapes: bool): Access = 
  // TODO: Also side-effect frame?!
  if escapes then InFrame (List.length frame.formals) else InReg (Temp.newtemp())
type Escape = Map<string, int * bool>
let escape (decl : Declaration) =
  let mapM f table es = List.fold f table es
  let rec escapeDecl (table : Escape) depth = function
  | File decls -> List.fold (fun t d -> escapeDecl t depth d) table decls
  | ExpressionStatement e -> escapeExp table depth e
  | Var (name,_,_) as v -> Map.add name (depth, false) table
  | Param (name,_) as p -> Map.add name (depth, false) table
  | Function (_, parameters, _, body) -> 
    let table' = (List.fold (fun t d -> escapeDecl t (depth + 1) d) table parameters)
    escapeExp table' (depth + 1) body
  | _ -> table
  and escapeExp table depth = function
  | LValue lval -> escapeLVal table depth lval
  | Negative e -> escapeExp table depth e
  | Binary (l,_,r) -> List.fold (fun t e -> escapeExp t depth e) table [l; r]
  | Assignment (lval, init) -> escapeExp (escapeLVal table depth lval) depth init
  | Sequence es -> List.fold (fun t e -> escapeExp t depth e) table es
  | Expression.Call (e, args) -> List.fold (fun t e -> escapeExp t depth e) table (e :: args)
  | RecordCons (name, inits) ->  List.fold (fun t e -> escapeExp t depth e) table (List.map snd inits)
  | ArrayCons inits -> List.fold (fun t e -> escapeExp t depth e) table inits
  | If (cond,cons,alt) -> List.fold (fun t e -> escapeExp t depth e) table [cond;cons;alt]
  | While (cond,action) -> List.fold (fun t e -> escapeExp t depth e) table [cond;action]
  | For (_,start,stop,action) -> List.fold (fun t e -> escapeExp t depth e) table [start;stop;action]
  | Let (decls,body) -> escapeExp (List.fold (fun t d -> escapeDecl t depth d) table decls) depth body
  | _ -> table
  and escapeLVal table depth = function
  // This is correct, but the answers aren't correctly stored -- nested names will disappear
  // at least if there are shadowing names -- you really do have to store the decl and resolve by name
  // on the other hand, making this work correctly means integrating with symbol binding,
  // which is a giant pain.
  // also there is a LOT of boilerplate for something which only really matters for
  // Var, Param, Function, Identifier
  | Identifier id -> 
    let (d,_) = Map.find id table
    if depth > d then Map.add id (d, true) table else table
  | PropertyAccess (l,r) -> escapeLVal table depth l
  | ArrayAccess (l,r) -> escapeExp (escapeLVal table depth l) depth r
  escapeDecl Map.empty 0 decl