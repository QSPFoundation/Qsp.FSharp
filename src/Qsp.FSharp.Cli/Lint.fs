module Qsp.FSharp.Cli.Lint
open Qsp.Tokens
open Qsp.Parser
open Qsp.Parser.Ast
open FParsec
open Argu

open Qsp.FSharp.Cli.Commons

[<RequireQualifiedAccess>]
type LintCliArguments =
    | [<MainCommand; ExactlyOnce; Last>] Source_Path of path: FilePath
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Source_Path _ -> "path to decoded source game (.qsps)."

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
[<RequireQualifiedAccess>]
module LintCliArguments =
    let exec (results: ParseResults<LintCliArguments>) =
        let sourcePath = results.GetResult LintCliArguments.Source_Path
        let enc = System.Text.UTF8Encoding.UTF8
        let result = Document.startOnFile enc sourcePath
        let printState (state: Generic.State) =
            let format (inlineRange: InlineRange) description =
                printfn "%s(%d,%d): %s"
                    (System.IO.Path.GetFullPath sourcePath)
                    inlineRange.Line
                    inlineRange.Column1
                    description

            state.NotDefinedLocs
            |> Seq.iter (fun (KeyValue(locationName, tokens)) ->
                tokens
                |> List.iter (fun range ->
                    format range "location definition is not found"
                )
            )
            state.SemanticErrors
            |> List.rev
            |> List.iter (fun (range, description) ->
                format range description
            )

        match result with
        | Success(_, state, _) ->
            printState state

        | Failure(errMsg, _, state) ->
            printState state
            printfn "%s" errMsg
