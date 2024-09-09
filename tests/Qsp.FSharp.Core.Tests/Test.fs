module Qsp.Tests.Test
open Fuchu
open Argu

open Qsp
open Tests

[<Struct;RequireQualifiedAccess>]
type TestType =
    | MockTest
    | MainTest
module TestType =
    let ofTestName (name: string) =
        match name.StartsWith TestOnMocks.mockTestList with
        | true -> TestType.MockTest
        | false -> TestType.MainTest

[<RequireQualifiedAccess>]
type TestOnMocksType =
    | True
    | Interactive

[<RequireQualifiedAccess>]
type CliArguments =
    | Test_Main
    | Test_Mocks of TestOnMocksType

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Test_Main -> "run main tests"
            | Test_Mocks _ -> "run tests on the sources, which are placed in the \"mocks\" folder"

[<EntryPoint;System.STAThread>]
let main args =
    let argParser = ArgumentParser.Create<CliArguments>(programName = "qsp-toolbox")
    match args with
    | [||] ->
        argParser.PrintUsage() |> printfn "%s"
        0
    | _ ->
        let results =
            try
                Ok (argParser.ParseCommandLine(args))
            with e ->
                Error e.Message
        match results with
        | Error errMsg ->
            printfn "%s" errMsg
            1

        | Ok results ->
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

            let isTestMocks =
                match results.TryGetResult CliArguments.Test_Mocks with
                | None ->
                    false
                | Some typ ->
                    match typ with
                    | TestOnMocksType.Interactive ->
                        isFullTest ()
                    | TestOnMocksType.True ->
                        true

            let isTestMain =
                match results.TryGetResult CliArguments.Test_Main with
                | None -> false
                | Some _ -> true

            if isTestMain && isTestMocks then
                defaultMainThisAssembly args
            else
                defaultMainThisAssemblyFilter
                    args
                    (fun x ->
                        x.Where(fun testName ->
                            let testType = TestType.ofTestName testName
                            match testType with
                            | TestType.MainTest -> isTestMain
                            | TestType.MockTest -> isTestMocks
                        )
                    )
