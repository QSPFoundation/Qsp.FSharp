open FsharpMyExtension
open FsharpMyExtension
open FsharpMyExtension.Either
open FParsec
#if INTERACTIVE
#load @"..\QSParse\Tokens.fs"
#load @"..\QSParse\Ast.fs"
#load @"..\QSParse\Defines.fs"
#load @"..\QSParse\Show.fs"
#load @"..\QSParse\ParserGeneric.fs"
#load @"..\QSParse\ParserExpr.fs"
#load @"..\QSParse\Parsec.fs"
#endif
open Qsp
open Qsp.Ast
open Qsp.Printer
open Qsp.Printer.Ast
open Qsp.Parser.Generic
open Qsp.Parser.Ast

open Fuchu

[<Tests>]
let noEqualityPositionTests =
    testList "noEqualityPositionTests" [
        testCase "base" <| fun () ->
            Assert.Equal(
                "",
                NoEqualityPosition(Position.create "" 0L 0L 0L),
                NoEqualityPosition(Position.create "" 1L 1L 1L)
            )
    ]

// #load "Parsec.fs"

let printStmts stmts =
    List.collect (Statement.Printer.showStmt (IndentsOption.UsingSpaces 4) Printer.FormatConfig.Default) stmts
    |> ShowList.joinEmpty "\n"
    |> ShowList.show
let printStmt stmt =
    Statement.Printer.showStmt (IndentsOption.UsingSpaces 4) Printer.FormatConfig.Default stmt
    |> ShowList.joinEmpty "\n"
    |> ShowList.show

module TestOnMocks =
    type T = Location list
    let enc = System.Text.Encoding.UTF8
    let startOnFile path =
        match Document.startOnFile enc path with
        | Success(x, _, _) -> x
        | Failure(x, _, _) -> failwithf "%s\n%s" path x
    let replaceOrNot expPath actPath =
        printfn "\"%s\"\nnot equal\n\"%s\""
            (System.IO.Path.GetFullPath expPath)
            (System.IO.Path.GetFullPath actPath)
        let rec whileYOrN () =
            match System.Console.ReadKey().Key with
            | System.ConsoleKey.Y -> true
            | System.ConsoleKey.N -> false
            | x ->
                printfn "need (y/n) but %A" x
                whileYOrN ()
        printfn "Replace? (y/n)"
        let res = whileYOrN()
        if res then
            System.IO.File.Copy(actPath, expPath, true)
            printfn "replaced"
        res
    let addExpToPath path =
        path
        |> Path.changeFileNameWithoutExt (sprintf "%sExp")
    let outputDir = @"..\..\..\Mocks"
    let copyAsExp path =
        System.IO.File.Copy(path, addExpToPath path, true)
    let getPathActLocal (pathAct:string) =
        sprintf "%s\\%s" outputDir (System.IO.Path.GetFileName pathAct)
        |> fun x -> System.IO.Path.ChangeExtension(x, ".json")
    let showTest path =
        let srcPath = path
        let parseActPath = getPathActLocal srcPath
        let parseExpPath = addExpToPath parseActPath
        let getPath (path:string) =
            sprintf "%s\\%s" outputDir (System.IO.Path.GetFileName path)
            |> fun x -> System.IO.Path.ChangeExtension(x, ".qsps")
        let showActPath = getPath srcPath
        let showExpPath = addExpToPath showActPath

        let act =
            // if System.IO.File.Exists parseExpPath then
            //     let src : T = Json.desf parseExpPath
            //     src |> Qsp.Show.printLocs Qsp.Show.UsingTabs
            // else
                let act = startOnFile srcPath
                // act |> Json.serf parseExpPath
                // failwithf "\"%s\" не найден, потому пришлось его создать. Естественно, все тесты пошли коту под хвост." parseExpPath
                act |> Document.print IndentsOption.UsingTabs FormatConfig.Default
        let exp =
            if System.IO.File.Exists showExpPath then
                System.IO.File.ReadAllText showExpPath
            else
                System.IO.File.WriteAllText(showExpPath, act)
                failwithf "\"%s\" не найден, потому пришлось его создать. Естественно, все тесты пошли коту под хвост." showExpPath
        if exp <> act then
            System.IO.File.WriteAllText(showActPath, act)

            if replaceOrNot showExpPath showActPath then ()
            else failwithf "not pass"
    let mockTestList = "mock tests"
    [<Tests>]
    let showTests =
        let mocksDir = outputDir + @"\Src"
        let tests =
            if System.IO.Directory.Exists mocksDir then
                System.IO.Directory.GetFiles(mocksDir, "*.qsps")
                |> Array.map (fun path ->
                    testCase (sprintf "'%s' test" (System.IO.Path.GetFullPath path)) <| fun () ->
                        showTest path
                        Assert.Equal("", true, true)
                )
            else [||]
        testList mockTestList tests

[<EntryPoint;System.STAThread>]
let main args =
    let isFullTest () =
        let rec whileYOrN () =
            match System.Console.ReadKey().Key with
            | System.ConsoleKey.Y -> true
            | System.ConsoleKey.N -> false
            | x ->
                printfn "`y` or `n` but %A" x
                whileYOrN ()
        printfn "Full test? (`y` or `n`)"
        whileYOrN ()
    let f isFullTest =
        if isFullTest then
            defaultMainThisAssembly args
        else
            defaultMainThisAssemblyFilter args
                (fun x ->
                    x.Where(fun x -> not <| x.StartsWith TestOnMocks.mockTestList))
    match args with
    | [|"--full"|] -> f true
    | [||] ->
        f (isFullTest ())
    | _ ->
        printfn "`--full` or pass args but: %A" args
        1
