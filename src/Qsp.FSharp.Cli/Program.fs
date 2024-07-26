module Program
open Argu
open FsharpMyExtension
open FsharpMyExtension.Either

open Qsp.Ast

type FilePath = string
type Txt2qspConfig = { Path: string; Args: string }

let qspSourceExt = ".qsps"

let changeExtensionToQsp (path:FilePath) =
    System.IO.Path.ChangeExtension(path, qspSourceExt)

module ThreadSafePrint =
    let mail = MailboxProcessor.Start (fun agent ->
        let rec loop () =
            async {
                let! msg = agent.Receive()
                printfn "%s" msg
                return! loop ()
            }
        loop ()
    )
    let printfn fmt = Printf.ksprintf mail.Post fmt

let decodeGame config (src:FilePath) (dst:FilePath) =
    if System.IO.File.Exists config.Path then
        let args = sprintf "\"%s\" \"%s\" D %s" src dst config.Args
        let startProcString path args =
            ThreadSafePrint.printfn "decoding: `%s %s`" path args
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


let doFile txt2qspConfig updateSourceIfExists src =
    let dst = changeExtensionToQsp src
    if System.IO.File.Exists dst then
        if updateSourceIfExists then
            decodeGame txt2qspConfig src dst
        else
            Right (dst:FilePath)
    else
        decodeGame txt2qspConfig src dst

let threadsExec threads fn xs =
    if threads > 1 then
        xs
        |> Seq.map (fun x -> async { return fn x })
        |> fun xs -> Async.Parallel(xs, threads)
        |> Async.RunSynchronously
    else
        xs |> Seq.map fn |> Array.ofSeq

let doDir txt2qspConfig updateSourceIfExists threads dir =
    // TODO:
    // let xs =
    //     System.IO.Directory.EnumerateFiles(dir, "*.*", System.IO.SearchOption.AllDirectories)
    //     |> Seq.filter (fun path ->
    //         match (System.IO.Path.GetExtension path).ToLower() with
    //         | ".qsp" | ".qsps" -> true
    //         | _ -> false
    //     )
    //     |> Seq.groupBy (fun x ->
    //         System.IO.Path.GetFileNameWithoutExtension x
    //     )

    // if use the "*.qsp" mask, it will capture the ".qsps" files as well
    let xs =
        System.IO.Directory.EnumerateFiles(dir, "*.*", System.IO.SearchOption.AllDirectories)
        |> Seq.filter
            (System.IO.Path.GetExtension
             >> String.toLower
             >> fun ext -> ext = ".qsp" || ext = ".gam")
    xs
    |> threadsExec threads (doFile txt2qspConfig updateSourceIfExists)

type CliArguments =
    | Working_Directory of path:FilePath
    | Txt2qsp of path:FilePath * args:string
    | Source_Path of path:FilePath
    | UpdateSourceIfExists
    | Threads of int
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Working_Directory _ -> "specify a working directory."
            | Txt2qsp _ -> "specify a txt2gam (path : args)."
            | Source_Path _ -> "path to encoded game (.qps) or decoded source game (.qsps)."
            | UpdateSourceIfExists -> "decodes the source, even if it exists."
            | Threads _ -> "number of threads per file, by default is 1."
module Parser =
    open FParsec
    open Qsp.Parser.Generic

    let parserStmt str =
        let p =
            spaces >>. Statements.Parser.Intermediate.pstmt
            .>> (getPosition >>= fun p ->
                    updateUserState (fun st ->
                        { st with LastSymbolPos = p}))
        runParserOnString p
            State.empty
            ""
            str

let patternMatching pattern =
    let rec stmtsOrRawEqual (acc:_ list) x =
        match x with
        | StaticStmts xs ->
            stmtsMatcher acc xs
        | Raw _ -> acc
    and lineKindEqual acc x =
        match x with
        | HyperLinkKind(stmtOrRaw, xs) ->
            List.fold lineEqual (stmtsOrRawEqual acc stmtOrRaw) xs
        | HyperLinkKind _
        | ExprKind _
        | StringKind _ -> acc
    and lineEqual acc (xs:Qsp.Ast.Line) =
        List.fold lineKindEqual acc xs
    and valueEqual acc x =
        match x with
        | String lines ->
            List.fold lineEqual acc lines
        | Int _ -> acc
    and exprEqual acc x =
        match x with
        | Val x ->
            valueEqual acc x
        | Arr _
        | Expr _
        | Func _
        | UnarExpr _
        | Tuple _
        | Var _
            -> acc
    and stmtsMatcher (acc:list<PosStatement>) stmts =
        stmts
        |> List.fold (fun acc stmt ->
            if pattern = stmt then
                stmt::acc
            else
                let _, stmt = stmt
                match stmt with
                | Assign(_, _, expr) ->
                    exprEqual acc expr
                | Proc(_, exprs) ->
                    List.fold exprEqual acc exprs
                | Exit -> acc
                | Act(exprs, body) ->
                    let acc = List.fold exprEqual acc exprs
                    stmtsMatcher acc body
                | AssignCode(_, body) -> stmtsMatcher acc body
                | If(expr, thenBody, elseBody) ->
                    let acc = exprEqual acc expr
                    let acc = stmtsMatcher acc thenBody
                    stmtsMatcher acc elseBody
                | For(var, from, to', step, body) ->
                    let acc = exprEqual acc from
                    let acc = exprEqual acc to'
                    let acc =
                        match step with
                        | Some step ->
                            exprEqual acc step
                        | _ -> acc
                    stmtsMatcher acc body
                | Loop(preStmts, condExpr, step, body) ->
                    let acc = stmtsMatcher acc preStmts
                    let acc = exprEqual acc condExpr
                    let acc = stmtsMatcher acc step
                    stmtsMatcher acc body
                | Label(_) -> acc
                | Comment(_) -> acc
        ) acc
    stmtsMatcher []
let parse patternRaw locs =
    match Parser.parserStmt patternRaw with
    | FParsec.CharParsers.Success(pattern, st, _) ->
        // printfn "pattern:\n%A\n" pattern
        locs
        |> List.map (fun (Location (locName, loc)) ->
            locName, patternMatching pattern loc
        )
        |> Right
    | FParsec.CharParsers.Failure(err, st, _) ->
        Left err

[<EntryPoint>]
let main argv =
    let parser = ArgumentParser.Create<CliArguments>(programName = "Utility.exe")
    let enc = System.Text.Encoding.UTF8
    let results = parser.Parse argv
    let results = results.GetAllResults()

    let updateSourceIfExists =
        results |> List.exists ((=) UpdateSourceIfExists)
    let txt2qspConfig =
        results
        |> List.tryPick (function
            | Txt2qsp(path, args) ->
                { Path = path; Args = args }
                |> Some
            | _ -> None
        ) |> Either.ofOption "not defined --txt2qsp"
    let threads =
        results
        |> List.tryPick (function
            | Threads i -> Some i
            | _ -> None
        ) |> Option.defaultValue 1
    let folder =
        results
        |> List.tryPick (function
            | Working_Directory dir -> Some dir
            | _ -> None
        )
        |> Either.ofOption "not defined --working-directory"

    let folderExec folder =
        txt2qspConfig
        |> Either.map (fun config ->
            doDir config updateSourceIfExists threads folder
        )
        |> Either.either (Left >> Array.singleton) id

    let path =
        results
        |> List.tryPick (function
            | Source_Path src -> Some src
            | _ -> None
        )
        |> Either.ofOption "not defined --source-path"

    let pathExec () =
        path
        |> Either.bind (fun src ->
            match String.toLower (System.IO.Path.GetExtension src) with
            | ".qsp" | ".gam" ->
                txt2qspConfig
                |> Either.bind (fun config ->
                    doFile config updateSourceIfExists src
                )
            | ".qsps" -> Right src // TODO: updateSourceIfExists
            | ext ->
                Left (sprintf "expected .qsp or .qsps extension but %s\nin\n%s" ext src)
        )
    let getPattern () =
        let getPattern () =
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
            f []
        let rec f () =
            let patternRaw = getPattern ()
            match Parser.parserStmt patternRaw with
            | FParsec.CharParsers.Success(pattern, st, _) ->
                pattern
            | FParsec.CharParsers.Failure(err, st, _) ->
                printfn "%s" err
                f ()
        f ()
    let parse locs =
        let pattern = getPattern ()
        locs
        |> List.map (fun (Location (locName, loc)) ->
            locName, patternMatching pattern loc
        )
    let all () =
        match folder with
        | Right folder ->
            let pattern = getPattern ()
            folderExec folder
            |> threadsExec threads
                (Either.bind (fun path ->
                    ThreadSafePrint.printfn "parse: %s" path
                    match Document.startOnFile enc path with
                    | FParsec.CharParsers.Success(locs, st, _) ->
                        locs
                        |> List.choose (fun (Location (locName, loc)) ->
                            match patternMatching pattern loc with
                            | [] -> None
                            | xs -> Some(locName, xs)
                        )
                        |> fun res -> Right (path, res)
                    | FParsec.CharParsers.Failure(errMsg, err, _) ->
                        Left errMsg
                )
            )
        | Left err -> [| Left err |]
    // let tree =
    //     pathExec ()
    //     |> Either.bind (fun path ->
    //         match Qsp.Parser.Main.startOnFile enc path with
    //         | FParsec.CharParsers.Success(tree, st, _) ->
    //             Right tree
    //         | FParsec.CharParsers.Failure(err, st, _) ->
    //             Left err
    //     )
    // tree
    // |> Either.bind parse
    // |> sprintf "%A"
    // |> printfn "%A"

    all ()
    |> Seq.map (sprintf "%A")
    |> uncurry System.IO.File.WriteAllLines "output.log"

    0
