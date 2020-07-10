module Qsp.Show
open FsharpMyExtension
open FsharpMyExtension.ShowList
open Qsp.Ast

type FormatConfig =
    {
        IsSplitStringPl: bool
        TrimWhitespaceWhenSplit: bool
    }
    static member Default =
        {
            IsSplitStringPl = false
            TrimWhitespaceWhenSplit = false
        }

let showVarType = function
    | StringType -> showChar '$'
    | ImplicitNumericType -> empty
    | ExplicitNumericType -> showChar '#'
let showVar (typ:VarType, varName:string) =
    showVarType typ << showString varName

let showStringLines showExpr (lines:list<Line>) =
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
let showValue showExpr = function
    | Int x -> shows x
    | String lines ->
        showStringLines showExpr lines
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
            let space = function Obj | No -> showSpace | Neg -> id
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
            f e1 << showSpace
            << ops op << showSpace
            << f e2
        | Arr(var, es) ->
            showVar var << bet "[" "]" (List.map f es |> join ", ")
    f expr
let rec showExpr = function
    | Val v -> showValue showExpr v
    | Var v -> showVar v
    | Func(name, es) -> showString name << showParen true (List.map showExpr es |> join ", ")
    | UnarExpr(op, e) ->
        let space = function Obj | No -> showSpace | Neg -> id
        showString (unar op) << space op << showExpr e
    | Expr(op, e1, e2) ->
        let prec = Precedences.OpB >> Precedences.prec
        let f = function
            | Expr(op', _, _) -> showParen (prec op > prec op')
            | UnarExpr _ -> showParen true | _ -> id
        let f x = f x (showExpr x)
        f e1 << showSpace << ops op << showSpace << f e2
    | Arr(var, es) -> showVar var << bet "[" "]" (List.map showExpr es |> join ", ")


let showAssign = function
    | Assign.AssignArr(var, key) -> showVar var << bet "[" "]" (showExpr key)
    | Assign.AssignVar v -> showVar v

let (|OneStmt|_|) = function
    | [x] ->
        match x with
        // | StarPl(Val (String _)) -> None
        | StarPl _ -> None // Как правило, строки очень длинные, потому пусть лучше будет так
        | Assign _ | CallSt _ | Comment _ -> Some x
        | AssingCode _ -> None // спорно
        | Act _ | If _ -> None
        | Label _ -> None // эту нечисть нужно как можно более нагляднее подчеркнуть. Да странно будет, если она окажется одна в списке инструкций.
        | Exit -> None // ¯\_(ツ)_/¯
    | _ -> None

let (|AssingName|) = function AssignArr(x, _) -> x | AssignVar x -> x
type IndentsOption =
    | UsingSpaces of int
    | UsingTabs

let spaceBetween (s:ShowS) : ShowS =
    showSpace << s << showSpace
let showStmt indentsOption (formatConfig:FormatConfig) =
    let tabs =
        match indentsOption with
        | UsingTabs ->
            showChar '\t'
        | UsingSpaces spacesCount ->
            replicate spacesCount ' '
    let rec f' stmt =
        let showStmtsInline xs : ShowS =
            List.collect f' xs // TODO
            |> join "&"
        match stmt with
        | Assign(AssingName name' as ass, Expr((Plus|Minus) as op, Var name, e)) when name' = name ->
            [showAssign ass << spaceBetween (ops op << showChar '=') << showExpr e]
        | Assign(AssingName name' as ass, Expr((Plus|Minus) as op, e, Var name)) when name' = name ->
            [showAssign ass << spaceBetween (showChar '=' << ops op) << showExpr e]
        | Assign(ass, e) ->
            [showAssign ass << spaceBetween (showChar '=') << showExpr e]
        | CallSt(name, es) ->
            let args =
                if List.isEmpty es then
                    empty
                else
                    showSpace << (List.map showExpr es |> join ", ")
            [ showString name << args ]
        | StarPl e ->
            if formatConfig.IsSplitStringPl then
                match e with
                | Val(String str) ->
                    let str =
                        if formatConfig.TrimWhitespaceWhenSplit then
                            str
                            |> List.map (
                                List.map (function
                                    | StringKind x -> StringKind (x.Trim())
                                    | x -> x)
                            )
                        else
                            str
                    showStringLines showExpr str
                    |> List.map (bet "'" "'")
                | _ ->
                    [ showExpr e ]
            else
                [ showExpr e ]
        | Label s -> [showChar ':' << showString s]
        | If(e, thenBody, elseBody) ->
            let ifHeader e = showString "if" << showSpace << showExpr e << showChar ':'
            [
                match thenBody, elseBody with
                | OneStmt x, OneStmt y ->
                    yield ifHeader e
                          << showSpace << showStmtsInline [x]
                          << spaceBetween (showString "else")
                          << showStmtsInline [y]
                | OneStmt x, [] ->
                    yield ifHeader e
                          << showSpace << showStmtsInline [x]
                | _ ->
                    let rec body : _ -> ShowS list = function
                        | [If(e, thenBody, elseBody)] ->
                            [
                                yield showString "elseif" << showSpace << showExpr e << showChar ':'
                                yield! thenBody
                                       |> List.collect
                                           (f' >> List.map ((<<) tabs))
                                yield! body elseBody
                            ]
                        | [] -> []
                        | xs ->
                            [
                                yield showString "else"
                                yield!
                                    xs
                                    |> List.collect
                                        (f' >> List.map ((<<) tabs))
                            ]
                    yield ifHeader e
                    yield! thenBody
                           |> List.collect
                               (f' >> List.map ((<<) tabs))
                    yield! body elseBody
                    yield showString "end"
            ]
        | Act(es, body) ->
            let header = showString "act" << showSpace << join ", " (List.map showExpr es) << showChar ':'
            [
                match body with
                | OneStmt x ->
                    yield header << showSpace << showStmtsInline [x]
                | _ ->
                    yield header
                    yield!
                        body
                        |> List.collect
                            (f' >> List.map ((<<) tabs))
                    yield showString "end"
            ]
        | Comment s -> [showChar '!' << showString s]
        | AssingCode(ass, stmts) ->
            let header = showAssign ass << spaceBetween (showChar '=') << showChar '{'
            [
                if List.isEmpty stmts then
                    yield header << showChar '}'
                else
                    yield header
                    yield!
                        stmts
                        |> List.collect
                            (f' >> List.map ((<<) tabs))
                    yield showChar '}'
            ]

        | Exit -> [showString "exit"]
    f'

let showLoc indentsOption isSplitStringPl (Location(name, statements)) : ShowS list =
    [
        yield showString "# " << showString name
        yield! List.collect (showStmt indentsOption isSplitStringPl) statements
        yield showString (sprintf "--- %s ----------" name)
    ]

let printLocs indentsOption isSplitStringPl xs =
    List.map (lines << showLoc indentsOption isSplitStringPl) xs
    |> joinEmpty "\n\n"
    |> show
