module Program
open Argu
open FsharpMyExtension
open FsharpMyExtension.Either
type FilePath = string
type Txt2qspConfig = { Path: string; Args: string }

let qspSourceExt = ".qsps"

let changeExtensionToQsp (path:FilePath) =
    System.IO.Path.ChangeExtension(path, qspSourceExt)

let decodeGame config (src:FilePath) (dst:FilePath) =
    if System.IO.File.Exists config.Path then
        let args = sprintf "\"%s\" \"%s\" D %s" src dst config.Args
        let startProcString path args =
            printfn "start: `%s %s`" path args
            let drivenOutput = new System.Text.StringBuilder()
            Proc.startProc (fun e ->
                drivenOutput.AppendLine(e) |> ignore
            ) path args
            |> fun code -> code, drivenOutput.ToString()
        let code, output = startProcString config.Path args
        if code = 0 then
            Right dst
        else
            Left output
    else
        Left (sprintf "txt2qsp not found in '%s'" config.Path)


let doFile txt2qspConfig src =
    let dst = changeExtensionToQsp src
    if System.IO.File.Exists dst then
        Right (dst:FilePath)
    else
        decodeGame txt2qspConfig src dst

let doDir txt2qspConfig dir =
    System.IO.Directory.EnumerateFiles(dir, "*.qsp", System.IO.SearchOption.AllDirectories)
    |> Seq.map (fun src ->
        doFile txt2qspConfig src
    )

type CliArguments =
    | Working_Directory of path:FilePath
    | Txt2qsp of path:FilePath * args:string
    | Source_Path of path:FilePath

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Working_Directory _ -> "specify a working directory."
            | Txt2qsp _ -> "specify a txt2gam (path : args)."
            | Source_Path _ -> "path to encoded game (.qps) or decoded source game (.qsps)."

module Parser = 
    open FParsec
    open Qsp.Parser.Generic
    open Qsp.Parser.Main
    let parserStmt str =
        let emptyState =
            { emptyState with PStmts = pstmts }
        let p =
            spaces >>. pstmt
            .>> (getPosition >>= fun p ->
                    updateUserState (fun st ->
                        { st with LastSymbolPos = p}))
        runParserOnString (p .>> pAfterAll)
            emptyState
            ""
            str

open Qsp.Ast
let patternMatching pattern =
    let rec f acc stmts =
        stmts
        |> List.collect (fun stmt ->
            if stmt = pattern then
                stmt::acc
            else
                match stmt with
                | Assign(_, _)
                | CallSt(_, _)
                | Exit -> acc
                | Act(_, body)
                | AssignCode(_, body) -> f acc body
                | StarPl(_) -> acc
                | If(_, _, _) -> acc
                | For(var, from, to', step, body) -> f acc body
                | Label(_) -> acc
                | Comment(_) -> acc
        )
    f []

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArguments>(programName = "Utility.exe")
    let enc = System.Text.Encoding.UTF8
    let results = parser.Parse argv
    let results = results.GetAllResults()
    results |> printfn "%A"

    let txt2qspConfig =
        results
        |> List.tryPick (function
            | Txt2qsp(path, args) ->
                { Path = path; Args = args }
                |> Some
            | _ -> None
        ) |> Either.ofOption "notDefinedTxt2qspConfig"
    let path =
        results
        |> List.tryPick (function
            | Source_Path src ->
                match String.toLower (System.IO.Path.GetExtension src) with
                | ".qsp" ->
                    txt2qspConfig
                    |> Either.bind (fun config ->
                        doFile config src
                    )
                    |> Some
                | ".qsps" -> Some (Right src)
                | ext ->
                    Some (Left (sprintf "expected .qsp or .qsps extension but %s\nin\n%s" ext src))
            | _ -> None
        )
        |> Either.ofOption "not defined path"
        |> Either.concat

    let tree =
        path
        |> Either.bind (fun path ->
            match Qsp.Parser.Main.startOnFile enc path with
            | FParsec.CharParsers.Success(tree, st, _) ->
                Right tree
            | FParsec.CharParsers.Failure(err, st, _) ->
                Left err
        )
    let parse locs =
        // the ideal solution would be to parse the text on the fly, but need to somehow define the boundaries of Stream
        let terminator = ";;"
        printfn "input statement (`%s` — end):" terminator
        let rec f acc =
            let line = System.Console.ReadLine()
            let acc = line::acc
            if line.Contains terminator then
                List.rev acc |> System.String.Concat
            else
                f acc
        let str = f []
        match Parser.parserStmt str with
        | FParsec.CharParsers.Success(pattern, st, _) ->
            locs
            |> List.map (fun (Location (locName, loc)) ->
                locName, patternMatching pattern loc
            )
            |> Right
        | FParsec.CharParsers.Failure(err, st, _) ->
            Left err

    tree
    |> Either.bind parse
    |> sprintf "%A"
    |> printfn "%A"
    // |> uncurry System.IO.File.WriteAllText "output.txt"

    0
