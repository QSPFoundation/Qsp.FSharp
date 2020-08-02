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

let rec showStringLines showExpr showStmtsInline (lines:list<Line>) =
    lines
    |> List.map (
        List.collect (
            function
            | StringKind x ->
                showString (x.Replace("'", "''"))
                |> List.singleton
            | ExprKind x ->
                showExpr x
                |> show
                |> fun x -> x.Replace("'", "''") // TODO: стоит ли говорить, что все эти былины с `.Replace("'", "''")` нужно превратить в нормальный код?
                |> showString
                |> bet "<<" ">>"
                |> List.singleton
            | HyperLinkKind(x, body) ->
                let attValue =
                    match x with
                    | Raw x ->
                        x.Replace("'", "''")
                        |> showString
                    | StaticStmts(x) ->
                        showStmtsInline x
                        |> show
                        |> fun x -> x.Replace("'", "''")
                        |> showString
                let header =
                    showString "<a href=\"exec: "
                    << attValue
                    << showString "\">"
                match showStringLines showExpr showStmtsInline body with
                | [] ->
                    header
                    << showString "</a>"
                    |> List.singleton
                | [x] ->
                    header
                    << x
                    << showString "</a>"
                    |> List.singleton
                | xs ->
                    xs
                    |> List.mapStartMidEnd
                        (fun x -> header << x)
                        id
                        (fun x -> x << showString "</a>")
                    |> fun x -> x // TODO: и все строки позже соединятся воедино, даже пробелов не удостоятся, ага.
        ) >> joinsEmpty empty
    )
let showValue showExpr showStmtsInline = function
    | Int x -> shows x
    | String lines ->
        showStringLines showExpr showStmtsInline lines
        |> joinsEmpty (showString "\n")
        |> bet "'" "'"
let ops = Op.toString >> showString

let unar = function No -> "no" | Obj -> "obj" | Neg -> "-" | Loc -> "loc"

let rec simpleShowExpr showStmtsInline expr : ShowS =
    let rec f = function
        | Val v -> showValue (simpleShowExpr showStmtsInline) showStmtsInline v
        | Var v -> showVar v
        | Func(name, args) ->
            let args =
                if List.isEmpty args then
                    empty
                else
                    showParen true (List.map f args |> join ", ")
            showString name << args
        | UnarExpr(op, e) ->
            let space = function Obj | No | Loc -> showSpace | Neg -> id
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
let rec showExpr showStmtsInline = function
    | Val v -> showValue (showExpr showStmtsInline) showStmtsInline v
    | Var v -> showVar v
    | Func(name, args) ->
        let args =
            if List.isEmpty args then
                empty
            else
                showParen true
                    (List.map (showExpr showStmtsInline) args |> join ", ")
        showString name << args
    | UnarExpr(op, e) ->
        let space = function Obj | No | Loc -> showSpace | Neg -> id
        showString (unar op) << space op << showExpr showStmtsInline e
    | Expr(op, e1, e2) ->
        let prec = Precedences.OpB >> Precedences.prec
        let f = function
            | Expr(op', _, _) -> showParen (prec op > prec op')
            | UnarExpr _ -> showParen true | _ -> id
        let f x = f x (showExpr showStmtsInline x)
        f e1 << showSpace << ops op << showSpace << f e2
    | Arr(var, es) -> showVar var << bet "[" "]" (List.map (showExpr showStmtsInline) es |> join ", ")


let showAssign showStmtsInline = function
    | AssignWhat.AssignArr(var, key) -> showVar var << bet "[" "]" (showExpr showStmtsInline key)
    | AssignWhat.AssignVar var -> showVar var
    | AssignWhat.AssignArrAppend var -> showVar var << showString "[]"

let (|OneStmt|_|) = function
    | [x] ->
        match x with
        // | StarPl(Val (String _)) -> None
        | StarPl _ -> None // Как правило, строки очень длинные, потому пусть лучше будет так
        | Assign _ | CallSt _ | Comment _ -> Some x
        | AssignCode _ -> None // спорно
        | Act _ | If _ -> None
        | Label _ -> None // эту нечисть нужно как можно более нагляднее подчеркнуть. Да странно будет, если она окажется одна в списке инструкций.
        | Exit -> None // ¯\_(ツ)_/¯
        | For _ -> None
    | _ -> None

let (|AssingName|) = function AssignArr(x, _) -> x | AssignVar x -> x | AssignArrAppend x -> x
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
        let showAssign = showAssign showStmtsInline
        let showExpr = showExpr showStmtsInline
        let showStringLines = showStringLines showExpr showStmtsInline
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
                    showStringLines str
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
        | For(var, fromExpr, toExpr, body) ->
            let header =
                showString "for"
                << showSpace << showVar var
                << showSpace << showChar '='
                << showSpace << showExpr fromExpr
                << showSpace << showString "to"
                << showSpace << showExpr toExpr
                << showChar ':'
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
        | AssignCode(ass, stmts) ->
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
