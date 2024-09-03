module Program
open Argu

open Qsp.FSharp.Cli.Search

[<RequireQualifiedAccess>]
type CliArguments =
    | [<CliPrefix(CliPrefix.None)>] Search of ParseResults<SearchCliArguments>

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Search _ -> "searching code in QSP games by pattern"

[<EntryPoint>]
let main argv =
    let argParser = ArgumentParser.Create<CliArguments>(programName = "qsp-toolbox")
    match argv with
    | [||] ->
        argParser.PrintUsage() |> printfn "%s"
        0
    | _ ->
        let results =
            try
                Ok (argParser.ParseCommandLine(argv))
            with e ->
                Error e.Message

        match results with
        | Error errMsg ->
            printfn "%s" errMsg
            1

        | Ok results ->
            match results.GetSubCommand() with
            | CliArguments.Search results ->
                SearchCliArguments.exec results

                0
