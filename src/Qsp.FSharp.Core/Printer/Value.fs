[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Printer.Ast.Value

module Printer =
    open FsharpMyExtension
    open FsharpMyExtension.ShowList

    open Qsp.Ast

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
