[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Printer.Ast.Statement

module Printer =
    open FsharpMyExtension
    open FsharpMyExtension.ShowList

    open Qsp
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

    let showAssign showStmtsInline = function
        | AssignWhat.AssignArr(var, args) ->
            Value.Printer.showVar var
            << Expr.Printer.showArrayArgs (List.map (Expr.Printer.showExpr showStmtsInline) args)
        | AssignWhat.AssignVar var -> Value.Printer.showVar var

    let (|OneStmt|_|) = function
        | [pos, x] ->
            match x with
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
            let showExpr = Expr.Printer.showExpr showStmtsInline
            let showStringLines = Value.Printer.showStringLines showExpr showStmtsInline
            let showLocal isLocal =
                if isLocal then
                    showString "local" << showSpace
                else id
            match stmt with
            | Assign(isLocal, (AssingName name' as ass), Expr((Plus|Minus) as op, Var name, e)) when name' = name ->
                [showLocal isLocal << showAssign ass << spaceBetween (Expr.Printer.ops op << showChar '=') << showExpr e]
            | Assign(isLocal, (AssingName name' as ass), Expr((Plus|Minus) as op, e, Var name)) when name' = name ->
                [showLocal isLocal << showAssign ass << spaceBetween (showChar '=' << Expr.Printer.ops op) << showExpr e]
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
                    << showSpace << Value.Printer.showVar var
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
                    << (
                        if List.isEmpty step then id
                        else
                            showSpace << showString "step"
                            << showSpace << showStmtsInline step
                    )
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
