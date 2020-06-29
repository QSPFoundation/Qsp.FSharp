module Qsp.Show
open FsharpMyExtension
open FsharpMyExtension.ShowList
open Qsp.Ast

let printState =
    let showValue = function
        | Int x -> shows x //| Float x -> x.ToString()
        | String x -> bet "'" "'" (x.Replace("'", "''") |> showString)
    let ops = Op.toString >> showString
//    let ops = function
//        | Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/"
//        | Eq -> "=" | Gt -> ">" | Ge -> ">=" | Lt -> "<" | Le -> "<=" | Ne -> "<>"   // =, >, >=, <, <=, (<>|!)
//        | And -> "and" | Or -> "or" | Mod -> "mod"
    let unar = function No -> "no" | Obj -> "obj" | Neg -> "-"
    let rec showExpr = function
        | Val v -> showValue v
        | Var v -> showString v
        | Func(name, es) -> showString name << showParen true (List.map showExpr es |> join ", ")
        | UnarExpr(op, e) ->
            let space = function Obj | No -> showChar ' ' | Neg -> id
            showString (unar op) << space op << showExpr e
        //| Expr(op, e1, e2) -> showExpr e1 << showChar ' ' << showString(ops op) << showChar ' ' << showExpr e2
        | Expr(op, e1, e2) -> 
            let prec = Precedences.OpB >> Precedences.prec
            let f = function
                | Expr(op', _, _) -> showParen (prec op > prec op')
                | UnarExpr _ -> showParen true | _ -> id
            let f x = f x (showExpr x)
            f e1 << showChar ' ' << ops op << showChar ' ' << f e2
        | Arr(name, es) -> showString name << bet "[" "]" (List.map showExpr es |> join ", ")
    let showAssign = function
        | Assign.AssignArr(nameVar, expr) -> showString nameVar << bet "[" "]" (showExpr expr)
        | Assign.AssignVar name -> showString name
    
    let (|OneStmt|_|) = function
        | [x] -> 
            match x with
            | Assign _ | CallSt _ | StarPl _ | Comment _ -> Some x
            | AssingCode _ -> None // спорно
            | Act _ | If _ -> None
            | Sign _ -> None // эту нечисть нужно как можно более нагляднее подчеркнуть. Да странно будет, если она окажется одна в списке инструкций.
        | _ -> None
    
    //let (OneStmt x) = [ parsing pstmt "a = 1"; ]
    let (|AssingName|) = function AssignArr(x, _) -> x | AssignVar x -> x
    let tabss n = replicate n '\t'
    let rec state tabs xs = 
        let f = function [] -> nl | xs -> nl << joinEmpty "\n" (List.map (state <| tabs + 1) xs)

        let indent = nl << tabss tabs : ShowS
        let rec f' = function
            | Assign(AssingName name' as ass, Expr((Plus|Minus) as op, Var name, e)) when name' = name -> 
                showAssign ass << showChar ' ' << ops op << showString "= " << showExpr e
            | Assign(AssingName name' as ass, Expr((Plus|Minus) as op, e, Var name)) when name' = name -> 
                showAssign ass << showString " =" << ops op << showChar ' ' << showExpr e
            | Assign(ass, e) -> showAssign ass << showString " = " << showExpr e
            | CallSt(name, es) -> showString name << showChar ' ' << (List.map showExpr es |> join ", ")
            | StarPl e -> showExpr e
            | Sign s -> showChar ':' << showString s
            | If(e, body, elseBody) -> 
                let ifBegin e = showString "if " << showExpr e << showChar ':'
                let body = 
                    match body, elseBody with
                    | OneStmt x, OneStmt y -> showChar ' ' << f' x << showString " else " << f' y
                    | OneStmt x, [] -> showChar ' ' << f' x
                    //| [CallSt _ as x], [CallSt _ as y] -> showChar ' ' << f' x << showString " else " << f' y
                    //| [Assign _ as x], [] | [CallSt _ as x], [] | [StarPl _ as x], [] -> showChar ' ' << f' x
                    | xs, ys -> 
                        let rec els xs =
                            if List.isEmpty xs then id
                            else 
                                let body = function [If(e, xs, ys)] -> ifBegin e << f xs << els ys | xs -> f xs
                                indent << showString "else" << body xs
//                            | [] -> id
//                            | [If(e, xs, ys)] -> 
//                                indent << showString "else" << ifBegin e << f xs << els ys
//                            | ys -> 
//                                indent << showString "else" << f ys
                        f xs << els ys << indent << showString "end"
                ifBegin e << body
            | Act(es, body) -> 
                let fbody = function
                    | OneStmt x -> showChar ' ' << f' x
                    | xs -> f xs << indent << showString "end"
                showString "act " << join ", " (List.map showExpr es) << showChar ':' << fbody body
            | Comment s -> showChar '!' << showString s
            | AssingCode(ass, stmts) -> 
                showAssign ass << showString " = " << showChar '{' << nl << (f stmts) << indent << showChar '}'
        tabss tabs << f' xs
    state 0
let showLoc (Location(name, statements)) = 
    showString "# " << showString name << nl
    << joinEmpty "\n" (List.map printState statements) << nl
    << showString (sprintf "--- %s ----------" name)
let printLocs xs = List.map showLoc xs |> joinEmpty "\n\n" |> show