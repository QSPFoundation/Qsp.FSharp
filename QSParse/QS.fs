module QS

type value =
    | Int of int  
    | Float of float  
    | String of string

type Ops =
    | Plus | Minus | Times | Divide | Mod
    | Eq | Gt | Ge | Lt | Le | Ne   // =, >, >=, <, <=, (!, <>)
    | And | Or
    
type UnarOp = Obj | No

type Expr =
    | Val of value
    | Var of string
    | Func of string * Expr list
    | UnarExpr of UnarOp * Expr
    | Expr of Ops * Expr * Expr
//and Call = { FunName: string; Args: Expr list }
// cond не покрывает случай когда в выражении стоит переменная типа bool или же напрямую 
// одно из значений данного типа. К примеру: let var = true in if var then true else false
//type If = { Cond:Expr; IfTrue:Statements list; Else: Statements list }
//and Act = { Loc:string; ImagePath:string; Body: Statements list }
type Statements =
    | Assert of Expr * Expr
    | AssertCode of Expr * Statements list
    | ExprS of Expr // вообще, достаточно какой-нибудь функции с побочным действием.
    | FuncS of string * Expr list
    | StringS of string
    | If of Expr * Statements list * Statements list
    | Act of Expr list * Statements list
    | Sign of string
    | Comment of string
    | Constr of string * Expr list * Statements list
type Location = Location of string * Statements list

let join s (xs:seq<string>) = System.String.Join(s, xs)
let nl = "\r\n"
let printState =
    let brake s = "[" + s + "]"
    let paren s = "(" + s + ")"
    let value = function
        | Int x -> x.ToString() | Float x -> x.ToString()
        | String x -> "'" + x + "'"
    let ops = function
        | Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/"
        | Eq -> "=" | Gt -> ">" | Ge -> ">=" | Lt -> "<" | Le -> "<" | Ne -> "!"   // =, >, >=, <, <=, !
        | And -> "and" | Or -> "or" | Mod -> "mod"
    let unar = function No -> "no" | Obj -> "obj"
    let rec expr = function
        | Val v -> value v
        | Var v -> v
        | Func("idx", (Var x)::t) -> x + (List.map expr t |> join ", " |> brake)
        //| Func(x, [Func("idx", xs)]) -> x + (List.map expr xs |> join ", " |> brake)
        | Func(x, xs) -> x + (List.map expr xs |> join ", " |> paren)
        | UnarExpr(op, e) -> unar op + expr e |> paren
        | Expr(op, e1, e2) -> expr e1 + " " + ops op + " " + expr e2 |> paren
    
    let tabss n = String.replicate n "\t"
    let rec state tabs xs = 
        let f = function [] -> "(NEWLINE)" | xs -> nl + join nl (List.map (state (tabs + 1)) xs)
        //let f' = function [] -> "(NEWLINE)" | xs -> " " + join "&" (List.map (state 0) xs)
        let rec f' = function
            | Assert(e1, e2) -> expr e1 + " = " + expr e2
            | AssertCode(e, xs) -> expr e + " = " + "{" + f xs + "}"
            | ExprS e -> expr e
            | Sign s -> ": " + s
            | StringS x -> "'" + x + "'"
            | If(e, [FuncS _ as x], []) -> "if " + expr e + ":" + " " + f' x
            | If(e, [FuncS _ as x], [FuncS _ as y]) -> "if " + expr e + ":" + " " + f' x + " else " + f' y
            | If(e, [Assert _ as x], []) -> "if " + expr e + ":" + " " + f' x
            | If(e, xs, ys) -> 
                let rec els = function
                    | [] -> ""
                    | [If(e, xs, ys)] -> nl + tabss tabs + "elseif " + expr e + ":" + f xs + els ys
                    | ys -> nl + tabss tabs + "else" + f ys
                "if " + expr e + ":" + f xs + els ys
            | Act(es, [FuncS _ as x]) -> "act " + join ", " (List.map expr es) + ":" + " " + f' x
            | Act(es, xs) -> "act " + join ", " (List.map expr es) + ":" + f xs
            | Comment s -> "! " + s
            | FuncS(name, xs) -> name + " " + (List.map expr xs |> join ", ")
            | x -> failwithf "%A absent" x
        tabss tabs + f' xs
    state 0
let printLoc (Location(name, statements)) = 
    sprintf "# %s" name + nl +
    join nl (List.map printState statements) + nl +
    sprintf "--- %s ----------" name
let printLocs xs = List.map printLoc xs |> join nl
    
assert
    let sample =
      If
         (Var "v1",
          [FuncS ("gs",[]);
           If
             (Var "v2",
              [If
                 (Var "v3",[Act ([Var "v4"],[FuncS ("gt",[])])],
                  [If
                     (Var "v5",[Act ([Var "v6"],[FuncS ("gt",[])])],
                      [If
                         (Var "v7",
                          [Act
                             ([Var "v8"],
                              [Act ([Var "v9"],[FuncS ("gt",[])]);
                               Act
                                 ([Var "v10"],
                                  [If
                                     (Var "v",[Act ([Var "v"],[FuncS ("gt",[])])],
                                      [If
                                         (Var "v",
                                          [Act ([Var "v"],[FuncS ("gt",[])])],[])])])])],
                          [])])])],[]);
           If
             (Var "v",
              [If
                 (Var "v",
                  [If
                     (Var "v",
                      [If
                         (Var "v",
                          [Act
                             ([Var "v"],
                              [FuncS ("gs",[]); Act ([Var "v"],[FuncS ("gt",[])])])],
                          [])],
                      [If
                         (Var "v",
                          [If
                             (Var "v",
                              [Act
                                 ([Var "v"],
                                  [FuncS ("gs",[]);
                                   Act ([Var "v"],[FuncS ("gt",[])])])],[])],
                          [If
                             (Var "v",
                              [If
                                 (Var "v",
                                  [Act
                                     ([Var "v"],
                                      [FuncS ("gs",[]);
                                       Act ([Var "v"],[FuncS ("gt",[])])])],[])],
                              [])])])],[])],[]); Act ([Var "v"],[FuncS ("gt",[])])],
          [])
    printState sample |> ignore
    true