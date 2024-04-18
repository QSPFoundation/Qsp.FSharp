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
    | NumericType -> empty

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
    | Int x -> showByToString x
    | String lines ->
        showStringLines showExpr showStmtsInline lines
        |> joinsEmpty (showString "\n")
        |> bet "'" "'"
let ops = Op.toString >> showString

let unar = function No -> "no" | Obj -> "obj" | Neg -> "-" | Loc -> "loc"
let showFuncName = function
    | PredefUndef.Predef name ->
        match Map.tryFind name Qsp.Defines.functionBySymbolic with
        | Some x ->
            let _, returnedType = x.Signature
            let returnedType =
                match returnedType with
                | Defines.Numeric -> id
                | Defines.String -> showChar '$'
                | Defines.Any -> id // TODO: defines by argument type
            let nameStr = (string name).ToLower()
            returnedType << showString nameStr
        | None -> failwithf "%A not found in `functionBySymbolic`" name
    | PredefUndef.Undef name ->
        showString name

let showArrayArgs xs =
    bet "[" "]" (join ", " xs)

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
            showFuncName name << args
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
        | Arr(var, args) ->
            showVar var << showArrayArgs (List.map f args)
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
        showFuncName name << args
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
    | Arr(var, args) ->
        showVar var << showArrayArgs (List.map (showExpr showStmtsInline) args)

let showAssign showStmtsInline = function
    | AssignWhat.AssignArr(var, args) ->
        showVar var
        << showArrayArgs (List.map (showExpr showStmtsInline) args)
    | AssignWhat.AssignVar var -> showVar var

let (|OneStmt|_|) = function
    | [pos, x] ->
        match x with
        // | StarPl(Val (String _)) -> None
        | Proc(name, _) when name.ToLower() = "*pl" -> None // Как правило, строки очень длинные, потому пусть лучше будет так
        | Assign _ | Proc _ | Comment _ -> Some (pos, x)
        | AssignCode _ -> None // спорно
        | Act _ | If _ -> None
        | Label _ -> None // эту нечисть нужно как можно более нагляднее подчеркнуть. Да странно будет, если она окажется одна в списке инструкций.
        | Exit -> None // ¯\_(ツ)_/¯
        | For _ | Loop _ -> None
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
    let rec f' (pos, stmt) =
        let showStmtsInline xs : ShowS =
            List.collect f' xs // TODO
            |> join "&"
        let showAssign = showAssign showStmtsInline
        let showExpr = showExpr showStmtsInline
        let showStringLines = showStringLines showExpr showStmtsInline
        let showLocal isLocal =
            if isLocal then
                showString "local" << showSpace
            else id
        match stmt with
        | Assign(isLocal, (AssingName name' as ass), Expr((Plus|Minus) as op, Var name, e)) when name' = name ->
            [showLocal isLocal << showAssign ass << spaceBetween (ops op << showChar '=') << showExpr e]
        | Assign(isLocal, (AssingName name' as ass), Expr((Plus|Minus) as op, e, Var name)) when name' = name ->
            [showLocal isLocal << showAssign ass << spaceBetween (showChar '=' << ops op) << showExpr e]
        | Assign(isLocal, ass, e) ->
            [showLocal isLocal << showAssign ass << spaceBetween (showChar '=') << showExpr e]
        | Proc(name, [e]) when name.ToLower() = "*pl" ->
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
        | Proc(name, e) ->
            let args =
                if List.isEmpty e then
                    empty
                else
                    showSpace << (List.map showExpr e |> join ", ")
            [ showString name << args ]
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
                        | [pos, If(e, thenBody, elseBody)] ->
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
        | For(var, fromExpr, toExpr, stepExpr, body) ->
            let header =
                showString "for"
                << showSpace << showVar var
                << showSpace << showChar '='
                << showSpace << showExpr fromExpr
                << showSpace << showString "to"
                << showSpace << showExpr toExpr
                << (stepExpr
                    |> Option.map (fun expr ->
                        showSpace << showString "step"
                        << showSpace << showExpr expr
                    ) |> Option.defaultValue empty)
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
        | Loop(preStmts, condExpr, step, body) ->
            let header =
                showString "loop"
                << if List.isEmpty preStmts then id else showSpace << showStmtsInline preStmts
                << showSpace << showString "while"
                << showSpace << showExpr condExpr
                << if List.isEmpty step then id
                   else
                    showSpace << showString "step"
                    << showSpace << showStmtsInline step
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
