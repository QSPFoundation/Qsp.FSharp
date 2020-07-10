module Qsp.Show
open FsharpMyExtension
open FsharpMyExtension.ShowList
open Qsp.Ast


let showVarType = function
    | StringType -> showChar '$'
    | ImplicitNumericType -> empty
    | ExplicitNumericType -> showChar '#'
let showVar (typ:VarType, varName:string) =
    showVarType typ << showString varName

let showValue showExpr = function
    | Int x -> shows x
    | String lines ->
        lines
        |> List.map (
            List.map (
                function
                | StringKind x ->
                    showString (x.Replace("'", "''"))
                | ExprKind x ->
                    showExpr x
                    |> show
                    |> fun x -> x.Replace("'", "''")
                    |> showString // м-да, но иначе непонятно, как экранировать string в выражениях
                    |> bet "<<" ">>"
            ) >> joinsEmpty empty
        )
        |> joinsEmpty (showString "\n")
        |> bet "'" "'"
let ops = Op.toString >> showString

let unar = function No -> "no" | Obj -> "obj" | Neg -> "-"

let rec simpleShowExpr expr: ShowS =
    let rec f = function
        | Val v -> showValue simpleShowExpr v
        | Var v -> showVar v
        | Func(name, es) ->
            showString name << showParen true (List.map f es |> join ", ")
        | UnarExpr(op, e) ->
            let space = function Obj | No -> showChar ' ' | Neg -> id
            let x =
                match e with
                | Expr(_, _, _) ->
                    showParen true (f e)
                | Arr(_, _) // `-(arr[idx])` лучше выглядит, чем `-arr[idx]`?
                | Func(_, _) // `-(func(idx))` лучше выглядит, чем `-(arr(idx))`?
                | UnarExpr _
                | Val _
                | Var _ ->
                    space op << f e
            showString (unar op) << x
        | Expr(op, e1, e2) ->
            let f body =
                match body with
                | Val(_)
                | Var _ ->  f body
                | UnarExpr(_, _)
                | Expr(_, _, _) ->
                    showParen true (f body)
                | Func(_, _)
                | Arr(_, _) ->
                    f body
            f e1 << showChar ' '
            << ops op << showChar ' '
            << f e2
        | Arr(var, es) ->
            showVar var << bet "[" "]" (List.map f es |> join ", ")
    f expr
let rec showExpr = function
    | Val v -> showValue showExpr v
    | Var v -> showVar v
    | Func(name, es) -> showString name << showParen true (List.map showExpr es |> join ", ")
    | UnarExpr(op, e) ->
        let space = function Obj | No -> showChar ' ' | Neg -> id
        showString (unar op) << space op << showExpr e
    | Expr(op, e1, e2) ->
        let prec = Precedences.OpB >> Precedences.prec
        let f = function
            | Expr(op', _, _) -> showParen (prec op > prec op')
            | UnarExpr _ -> showParen true | _ -> id
        let f x = f x (showExpr x)
        f e1 << showChar ' ' << ops op << showChar ' ' << f e2
    | Arr(var, es) -> showVar var << bet "[" "]" (List.map showExpr es |> join ", ")


let showAssign = function
    | Assign.AssignArr(var, key) -> showVar var << bet "[" "]" (showExpr key)
    | Assign.AssignVar v -> showVar v

let (|OneStmt|_|) = function
    | [x] ->
        match x with
        | Assign _ | CallSt _ | StarPl _ | Comment _ -> Some x
        | AssingCode _ -> None // спорно
        | Act _ | If _ -> None
        | Label _ -> None // эту нечисть нужно как можно более нагляднее подчеркнуть. Да странно будет, если она окажется одна в списке инструкций.
        | Exit -> None // ¯\_(ツ)_/¯
    | _ -> None

let (|AssingName|) = function AssignArr(x, _) -> x | AssignVar x -> x
type IndentsOption =
    | UsingSpaces of int
    | UsingTabs
let showStmt indentsOption =
    let tabss =
        match indentsOption with
        | UsingTabs ->
            fun n -> replicate n '\t'
        | UsingSpaces spacesCount ->
            fun n -> replicate (n * spacesCount) ' '
    let rec state tabs xs =
        let f = function [] -> nl | xs -> nl << joinEmpty "\n" (List.map (state <| tabs + 1) xs)

        let indent = nl << tabss tabs : ShowS
        let rec f' = function
            | Assign(AssingName name' as ass, Expr((Plus|Minus) as op, Var name, e)) when name' = name ->
                showAssign ass << showChar ' ' << ops op << showString "= " << showExpr e
            | Assign(AssingName name' as ass, Expr((Plus|Minus) as op, e, Var name)) when name' = name ->
                showAssign ass << showString " =" << ops op << showChar ' ' << showExpr e
            | Assign(ass, e) -> showAssign ass << showString " = " << showExpr e
            | CallSt(name, es) ->
                let args =
                    if List.isEmpty es then
                        empty
                    else
                        showChar ' ' << (List.map showExpr es |> join ", ")
                showString name << args
            | StarPl e -> showExpr e
            | Label s -> showChar ':' << showString s
            | If(e, body, elseBody) ->
                let ifBegin e = showString "if " << showExpr e << showChar ':'
                let body =
                    match body, elseBody with
                    | OneStmt x, OneStmt y -> showChar ' ' << f' x << showString " else " << f' y
                    | OneStmt x, [] -> showChar ' ' << f' x
                    | xs, ys ->
                        let rec els xs =
                            if List.isEmpty xs then id
                            else
                                let body = function [If(e, xs, ys)] -> ifBegin e << f xs << els ys | xs -> f xs
                                indent << showString "else" << body xs
                        f xs << els ys << indent << showString "end"
                ifBegin e << body
            | Act(es, body) ->
                let fbody = function
                    | OneStmt x -> showChar ' ' << f' x
                    | xs -> f xs << indent << showString "end"
                showString "act " << join ", " (List.map showExpr es) << showChar ':' << fbody body
            | Comment s -> showChar '!' << showString s
            | AssingCode(ass, stmts) ->
                showAssign ass << showString " = " << showChar '{' << f stmts << indent << showChar '}'
            | Exit -> showString "exit"
        tabss tabs << f' xs
    state 0
let showLoc indentsOption (Location(name, statements)) =
    showString "# " << showString name << nl
    << joinEmpty "\n" (List.map (showStmt indentsOption) statements) << nl
    << showString (sprintf "--- %s ----------" name)
let printLocs indentsOption xs =
    List.map (showLoc indentsOption) xs
    |> joinEmpty "\n\n"
    |> show
