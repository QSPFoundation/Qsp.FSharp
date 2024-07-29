module Qsp.Tests.TestOnMocks
open Fuchu
open FsharpMyExtension
open FParsec

open Qsp.Ast
open Qsp.Printer
open Qsp.Printer.Ast
open Qsp.Parser.Ast

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
