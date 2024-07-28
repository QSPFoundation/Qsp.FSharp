module Program
open FsharpMyExtension
open FsharpMyExtension.Either
open LanguageServerProtocol
open LanguageServerProtocol.Server
open LanguageServerProtocol.Types

open Qsp
open Qsp.Printer
open Qsp.Printer.Ast

// берётся — `FSharp.Compiler.Range.range`
type VscodeRange =
    { StartColumn : int
      StartLine : int
      EndColumn : int
      EndLine : int }
let toVscodeRange ((p1, p2):Qsp.Tokens.Range) =
    { StartColumn = int p1.Column
      StartLine = int p1.Line
      EndColumn = int p2.Column
      EndLine = int p2.Line }

let toVscodeRange2 (r:Qsp.Tokens.InlineRange) =
    { StartColumn = int r.Column1
      StartLine = int r.Line
      EndColumn = int r.Column2
      EndLine = int r.Line }
module Position =
    let ofRange ((p1, p2):Qsp.Tokens.Range) =
        {
            Start = { Line = int p1.Line - 1
                      Character = int p1.Column - 1 }
            End = { Line = int p2.Line - 1
                    Character = int p2.Column - 1 }
        }
    let ofInlineRange (r:Qsp.Tokens.InlineRange) =
        {
            Start = { Line = int r.Line - 1
                      Character = int r.Column1 - 1 }
            End = { Line = int r.Line - 1
                    Character = int r.Column2 - 1 }
        }
type UriString = string
type SourceFilePath = string
type HighlightingRequest = { FileName: string }
type Serializer = obj -> string

type PlainNotification= { Content: string }

module CommandResponse =
    type ResponseMsg<'T> =
        {
            Kind: string
            Data: 'T
        }

    type HighlightingRange = { Range: VscodeRange; TokenType: string }

    type HighlightingResponse = { Highlights: HighlightingRange [] }
    open Qsp.Tokens

    let highlighting ranges =
        let map (t: TokenType): string =
            match t with
            | TokenType.ConstantNumericInteger -> "constantNumericInteger"
            | TokenType.NameLabel -> "string"
            | TokenType.LabelColon -> "punctuationSeparatorColon"
            | TokenType.InterpolationEnd -> "interpolationEnd"
            | TokenType.InterpolationBegin -> "interpolationBegin"
            | TokenType.Procedure -> "procedure"
            | TokenType.Type -> "storage"
            | TokenType.Variable -> "variable"
            | TokenType.Keyword -> "keyword"
            | TokenType.KeywordSymbol -> "keywordSymbol"
            | TokenType.Comment -> "comment"
            | TokenType.Function -> "function"
            | TokenType.If
            | TokenType.ElseIf
            | TokenType.Else
            | TokenType.Act
            | TokenType.Colon
            | TokenType.End
            | TokenType.Underscore
            | TokenType.Exit
            | TokenType.For
            | TokenType.To
            | TokenType.Step
            | TokenType.Loop
            | TokenType.While
            | TokenType.SharpBeginLoc
            | TokenType.MinusEndLoc -> "keywordControl"
            | TokenType.BinaryOperator op ->
                match op with
                | Ast.Plus
                | Ast.Minus
                | Ast.Times
                | Ast.Divide
                | Ast.Mod ->
                    "operatorArithmetic"
                | Ast.Eq ->
                    "operatorComparison"
                | Ast.Gt
                | Ast.Ge
                | Ast.Lt
                | Ast.Le
                | Ast.Bang
                | Ast.Ne
                | Ast.El
                | Ast.Eg ->
                    "operatorRelational"
                | Ast.Or
                | Ast.And ->
                    "operatorLogical"
            | TokenType.UnaryOperator(unaryOperator) ->
                match unaryOperator with
                | Ast.UnarOp.Obj -> "operator"
                | Ast.UnarOp.Loc -> "operator"
                | Ast.UnarOp.No -> "operatorLogical"
                | Ast.UnarOp.Neg -> "operatorArithmetic"
            | TokenType.OperatorAssignment -> "operatorAssignment"
            | TokenType.PunctuationTerminatorStatement -> "punctuationTerminatorStatement"
            | TokenType.StringQuotedSingle
            | TokenType.StringQuotedDouble
            | TokenType.StringBraced -> "string"
            | TokenType.BraceSquareOpened
            | TokenType.BraceSquareClosed -> "metaBraceSquare"
        {
            Kind = "highlighting"
            Data =
              { Highlights =
                  ranges
                  |> Array.map (fun (struct ((pos:Tokens.InlineRange), tk)) ->
                      { Range = toVscodeRange2 pos; TokenType = map tk }) }
        }
let changeExtensionToQsp path =
    System.IO.Path.ChangeExtension(path, ".qsp")
let txt2qspPath = @"3rd\txt2gam.exe"
let buildQsp src =
    let dst = changeExtensionToQsp src
    let args = sprintf "\"%s\" \"%s\"" src dst
    let startProcString path args =
        let drivenOutput = new System.Text.StringBuilder()
        Proc.startProc (fun e ->
            drivenOutput.AppendLine(e) |> ignore
        ) path args
        |> fun code -> code, drivenOutput.ToString()
    startProcString txt2qspPath args
let buildQspTest () =
    let src = @"E:\Project\Qsp\QspSyntax\sample-code\Sandbox.qsps"
    buildQsp src

type Commands() =
    member x.GetHighlighting documentText = // (file: SourceFilePath) =
        // let file = Path.GetFullPath file
        // async {
        //     let! res = x.TryGetLatestTypeCheckResultsForFile file
        //     let res =
        //         match res with
        //         | Some res ->
        //             let r = res.GetCheckResults.GetSemanticClassification(None)
        //             Some r
        //         | None -> None
        //     return CoreResponse.Res res

            // return
        // }
        Parser.Ast.Document.start documentText
let isValidDoc uri =
    // git-файлы лучше обходить стороной, чтобы не смущать пространство лишний раз
    let uri = System.Uri uri
    uri.Scheme <> "git"

type WorkspaceLoadParms = {
    /// Project files to load
    TextDocuments: TextDocumentIdentifier []
}

type FsacClient(sendServerRequest: ClientNotificationSender) =
    inherit LspClient ()

    override __.WindowShowMessage(p) =
        sendServerRequest "window/showMessage" (box p) |> Async.Ignore

    override __.WindowLogMessage(p) =
        sendServerRequest "window/logMessage" (box p) |> Async.Ignore

    override __.TextDocumentPublishDiagnostics(p) =
        sendServerRequest "textDocument/publishDiagnostics" (box p) |> Async.Ignore
    override __.WorkspaceWorkspaceFolders () = // TODO
        failwith ""
type State = unit

type UpdateFileParms = {
    // File: BackgroundFileCheckType
    Content: string
    Version: int
}


type QspConfig =
    {
        FormatConfig: Printer.FormatConfig
    }
    static member Default =
        {
            FormatConfig = Printer.FormatConfig.Default
        }

type Config =
    { Qsp : QspConfig option }
    static member Default =
        {
            Qsp = None
        }
/// Перезапускающийся таймер: как только подаются новые данные, старый счетчик сбрасывается, и наступает вновь отсчет, но уже с новыми данными. Если счетчик дойдет до нуля, то вызовется функция.
let restartableTimer interval f =
    let m = MailboxProcessor.Start(fun agent ->
        let mutable data = None
        let rec loop (timer:System.Timers.Timer) =
            async {
                let! msg = agent.Receive()
                data <- Some msg

                timer.Stop()
                timer.Start()
                return! loop timer
            }
        let timer = new System.Timers.Timer(interval)
        timer.AutoReset <- false
        timer.Elapsed.Add(fun x -> f x data.Value)
        loop timer
    )
    m.Post
let test () =
    let interval = 1500.
    let time = restartableTimer interval (fun e (x:string, y:int) -> printfn "%A" (x, y))
    time("1", 1)
    time("2", 1)
    time("3", 1)


type BackgroundServiceServer(state: State, client: FsacClient) =
    inherit LspServer()
    let mutable clientCapabilities = None

    let mutable currentDocument : TextDocumentItem option = None
    let mutable currentWorkspacePath = ""
    let mutable lastCharPos = None
    let mutable parserResult = None

    let mutable hovers = []
    let mutable highlights = Qsp.Parser.Generic.Highlights.empty
    let mutable config = QspConfig.Default
    let getVarHighlight (pos:Position) =
        highlights.VarHighlights.Ranges
        |> List.tryFind (fun (r, _) ->
            if (int (r.Line - 1L) = pos.Line) then
                int (r.Column1 - 1L) <= pos.Character && pos.Character <= int (r.Column2 - 1L)
            else
                false
        )
        |> Option.map (fun (_, varId) ->
            Map.find varId highlights.VarHighlights.VarScopeSystem.Result // находить должно всегда
            |> snd
        )
    let getLocHighlight (pos:Position) =
        highlights.LocHighlights.Ranges
        |> List.tryFind (fun (r, _) ->
            if (int (r.Line - 1L) = pos.Line) then
                int (r.Column1 - 1L) <= pos.Character && pos.Character <= int (r.Column2 - 1L)
            else
                false
        )
        |> Option.map (fun (_, var) ->
            Map.find var highlights.LocHighlights.Ma) // находить должно всегда
    let commands = Commands()

    let publishDiagnostics uri (res:list<_>) =
        let diagnostics =
            res
            |> List.map (fun (range, msg) ->
                {
                    Range = range
                    Severity = Some (DiagnosticSeverity.Error)
                    Code = None
                    Source = "qsp"
                    Message = msg
                    RelatedInformation = None
                    Tags = None
                })
            |> Array.ofList
        client.TextDocumentPublishDiagnostics {
            Uri = uri
            Diagnostics = diagnostics
        }

    // let interval = 500.
    // let reactor =
    //     restartableTimer interval
    //         (fun e (uri, text) ->
    //             publishDiagnostics uri text
    //             |> Async.RunSynchronously
    //         )
    let parse uri documentText =
        let res = commands.GetHighlighting documentText
        parserResult <- Some res
        let genericFromState (st:Qsp.Parser.Generic.State) =
            hovers <- st.Hovers |> List.map (mapFst Position.ofInlineRange)
            highlights <- st.Highlights

            lastCharPos <- Some st.LastSymbolPos

        match res with
        | FParsec.CharParsers.Success(_, st, _) ->
            genericFromState st

            st.SemanticErrors
            |> List.map (mapFst Position.ofInlineRange)
            |> publishDiagnostics uri
            |> Async.RunSynchronously

            st.Tokens
            |> List.map (fun x -> struct (x.Range, x.TokenType) )
            |> Array.ofList
        | FParsec.CharParsers.Failure(msg, err, st) ->
            // client.WindowLogMessage {
            //     Type = MessageType.Error
            //     Message = sprintf "%A" msg
            // }
            // |> Async.RunSynchronously

            genericFromState st

            let pos = err.Position

            let range =
                {
                    Start = { Line = int pos.Line - 1
                              Character = int pos.Column - 1 }
                    End = { Line = int pos.Line - 1
                            Character = int pos.Column + 1000 }
                }
            // client.WindowLogMessage {
            //     Type = MessageType.Error
            //     Message = sprintf "%A\n%A\n%A" pos range pos.Index
            // }
            // |> Async.RunSynchronously
            // TODO: придется лезть в документацию, чтобы постичь всю боль
            // FParsec.ErrorMessageList.ToSortedArray err.Messages // TODO
            // |> Array.map (function
            //     | FParsec.Error.CompoundError(msg, pos, someMagicObj, errs) ->

            //         failwith ""
            //     // | FParsec.Error.Unexpected
            //     // x.Type
            // )
            let xs =
                st.SemanticErrors
                |> List.map (mapFst Position.ofInlineRange)
            (range, msg) :: xs
            |> publishDiagnostics uri
            |> Async.RunSynchronously


            st.Tokens
            |> List.map (fun x -> struct (x.Range, x.TokenType) )
            |> Array.ofList
    member __.GetHighlighting(p : HighlightingRequest) =
        let uri = System.Uri p.FileName
        let res =
            currentDocument
            |> Option.map (fun x ->
                let res = parse uri.AbsoluteUri x.Text
                { Content =
                    CommandResponse.highlighting res
                    |> FSharpJsonType.SerializeOption.serNotIndent }
            )
        async {
            return LspResult.success res
        }

    override __.TextDocumentDidChange(p) = async {
        if isValidDoc p.TextDocument.Uri then
            currentDocument <-
                currentDocument
                |> Option.map (fun x ->
                    { x with
                            Uri = p.TextDocument.Uri
                            Version = Option.defaultValue x.Version p.TextDocument.Version
                            Text =
                                match Array.tryExactlyOne p.ContentChanges with
                                | Some x ->
                                    // documentRange <- x.Range // увы, но при `TextDocumentSyncKind.Full` он всегда равен `None`
                                    x.Text
                                | None ->
                                    // do! client.WindowLogMessage {
                                    //     Type = MessageType.Error
                                    //     Message = sprintf "Array.tryExactlyOne p.ContentChanges error:\n%A" p.ContentChanges
                                    // }
                                    failwith "Array.tryExactlyOne p.ContentChanges = None"
                    }
                )
    }
    override __.TextDocumentDidOpen(p: DidOpenTextDocumentParams) = async {
        // Вот что, этот негодяй одновременно открывает целую кучу всего: здесь и git, и обычный файл, и даже output. Надо бы как-то за всем этим уследить.
        // "git:/e%3A/Project/Qsp/QspSyntax/sample-code/Sandbox.qsps?%7B%22path%22%3A%22e%3A%5C%5CProject%5C%5CQsp%5C%5CQspSyntax%5C%5Csample-code%5C%5CSandbox.qsps%22%2C%22ref%22%3A%22~%22%7D"
        // p.TextDocument
        if p.TextDocument.LanguageId = "qsp" && isValidDoc p.TextDocument.Uri then
            currentDocument <- Some p.TextDocument
            // documentUri <- p.TextDocument.Uri
            // documentVersion <- Some p.TextDocument.Version
            // documentText <- p.TextDocument.Text

            do! client.WindowLogMessage {
                Type = MessageType.Info
                Message =
                    let txt = p.TextDocument
                    sprintf "TextDocumentDidOpen\n%A"
                        ( txt.LanguageId, txt.Uri, txt.Version)
            }
            // if Set.contains (p.TextDocument.Uri.ToLower()) spellcheckIgnore then
            //     do! client.SpellcheckDecorate []
            // else
            //     let textDoc = p.TextDocument
            //     documentVersion <- Some textDoc.Version
            //     do! client.WindowLogMessage {
            //         Type = MessageType.Info
            //         Message = "TextDocumentDidOpen"
            //     }
            //     reactor (textDoc.Uri, textDoc.Text)
    }
    override __.TextDocumentDidClose p = async {
        do! client.WindowLogMessage {
            Type = MessageType.Info
            Message =
                sprintf "TextDocumentDidClose\n%A" p
        }
    }
    override __.WorkspaceDidChangeWatchedFiles p = async {
        do! client.WindowLogMessage {
            Type = MessageType.Info
            Message =
                sprintf "WorkspaceDidChangeWatchedFiles\n%A" p
        }
    }

    member private __.IfDiagnostic (str: string) handler p =
        let diag =
            p.Context.Diagnostics |> Seq.tryFind (fun n -> n.Message.Contains str)
        match diag with
        | None -> async.Return []
        | Some d -> handler d

    member private __.CreateFix uri ver title (d: Diagnostic option) range replacement =
        let e =
            {
                Range = range
                NewText = replacement
            }
        let edit =
            {
                TextDocument =
                    {
                        Uri = uri
                        Version = ver
                    }
                Edits = [|e|]
            }
        let we = WorkspaceEdit.Create([|edit|], clientCapabilities.Value)

        { CodeAction.Title = title
          Kind = Some "quickfix"
          Diagnostics = d |> Option.map Array.singleton
          Edit = we
          Command = None}

    // override this.TextDocumentCodeAction p = async {
    //     if Set.contains (p.TextDocument.Uri.ToLower()) spellcheckIgnore then
    //         // TODO: если текущий файл отсеивается, то самое время как-то избавить весь документ от ошибок, вот только как это сделать?
    //         // return LspResult.Ok None // Пробовал — выбивает ошибку
    //         return LspResult.success (Some (TextDocumentCodeActionResult.CodeActions [||]))
    //     else
    //         let! res =
    //             p
    //             |> this.IfDiagnostic "unknown " (fun d ->
    //                 async {
    //                     do! client.WindowLogMessage {
    //                         Type = MessageType.Info
    //                         Message = (sprintf "TextDocumentCodeAction 'unknown ...'")
    //                     }
    //                     match Map.tryFind d.Range lastWords with
    //                     | Some word ->
    //                         let words =
    //                             Suggestion.LevenshteinDistance.suggestions3 word dic
    //                             |> Suggestion.LevenshteinDistance.mapTruncate 10

    //                         let actions =
    //                             words
    //                             |> List.map (fun word ->
    //                                 this.CreateFix p.TextDocument.Uri documentVersion (sprintf "replace on '%s'" word) (Some d) d.Range word)
    //                         return actions
    //                     | None ->
    //                         do! client.WindowLogMessage {
    //                             Type = MessageType.Info
    //                             Message = (sprintf "range not found:\n%A" d.Range)
    //                         }
    //                         return []
    //                 }
    //             )
    //         return res |> Array.ofList |> TextDocumentCodeActionResult.CodeActions |> Some |> LspResult.success
    // }


    override __.WorkspaceDidChangeConfiguration (x) = async {
        do! client.WindowLogMessage {
            Type = MessageType.Info
            Message = sprintf "WorkspaceDidChangeConfiguration\n%A" (x.Settings.ToString())
        }

        let configResult : Either<_, Config> =
            let ser = Newtonsoft.Json.JsonSerializer()
            ser.Converters.Add FSharpJsonType.SerializeOption.converter
            try
                x.Settings.ToObject(ser)
                |> Right
            with
                e -> Left e.Message
        match configResult with
        | Right x ->
            x.Qsp
            |> Option.iter (fun x ->
                config <- x
            )
        | Left msg ->
            do! client.WindowLogMessage {
                Type = MessageType.Error
                Message = sprintf "%s\n%s" (x.Settings.ToString()) msg
            }
    }
    override x.TextDocumentFormatting p = async {
        // p.Options.AdditionalData // версия 1.46.1, и их всё еще не завезли https://code.visualstudio.com/api/references/vscode-api#FormattingOptions
        match currentDocument with
        | Some currentDocument ->
            if p.TextDocument.Uri = currentDocument.Uri then
                match lastCharPos with
                | Some lastCharPos ->
                    match parserResult with
                    | Some r ->
                        match r with
                        | FParsec.CharParsers.Success(x, _, _) ->
                            return
                                { TextEdit.Range =
                                    {
                                        Start = { Line = 0
                                                  Character = 0 }
                                        End = { Line = int lastCharPos.Line - 1
                                                Character = int lastCharPos.Column - 1 } // а быть может, даже `- 2`
                                    }
                                  NewText =
                                    if p.Options.InsertSpaces then
                                        IndentsOption.UsingSpaces p.Options.TabSize
                                    else
                                        IndentsOption.UsingTabs
                                    |> fun indentsOpt -> Document.print indentsOpt config.FormatConfig x }
                                |> Array.singleton
                                |> Some
                                |> LspResult.success
                        | FParsec.CharParsers.Failure(_, _, _) ->
                            do! client.WindowShowMessage {
                                Type = MessageType.Error
                                Message = sprintf "Синтаксис содержит ошибки, потому форматировать его невозможно"
                            }
                            return LspResult.success None
                    | None ->
                        do! client.WindowLogMessage {
                            Type = MessageType.Error
                            Message = sprintf "lastSymbolPos = None"
                        }
                        return LspResult.success None
                | None ->
                    do! client.WindowLogMessage {
                        Type = MessageType.Error
                        Message = sprintf "documentRange = None"
                    }
                    return LspResult.success None
            else
                do! client.WindowLogMessage {
                    Type = MessageType.Error
                    Message = sprintf "p.TextDocument.Uri <> documentUri"
                }
                return LspResult.success None
        | None ->
            return LspResult.success None
    }
    override __.TextDocumentDocumentHighlight(x) = async {
        // do! client.WindowLogMessage {
        //     Type = MessageType.Error
        //     Message = sprintf "%A" (varHovers, x.Position)
        // }
        let f fn =
            match fn x.Position with
            | None -> None
            | Some xs ->
                xs // должно находить всегда
                |> List.map (fun (r, kind) ->
                    {
                        DocumentHighlight.Range = Position.ofInlineRange r
                        Kind =
                            match kind with
                            | Qsp.Parser.Generic.WriteAccess -> DocumentHighlightKind.Write
                            | Qsp.Parser.Generic.ReadAccess -> DocumentHighlightKind.Read
                            |> Some
                    }
                )
                |> Array.ofList
                |> Some
        let x =
            f getVarHighlight
            |> Option.orElseWith (fun () ->
                f getLocHighlight
            )
        return LspResult.success x
    }

    override __.TextDocumentRename renameParams = async {
        let f fn =
            match fn renameParams.Position with
            | None -> None
            | Some xs ->
                let edits =
                    [|
                        {
                            Edits =
                                xs
                                |> List.map (fun (r, _) ->
                                    {
                                        TextEdit.Range = Position.ofInlineRange r
                                        NewText = renameParams.NewName
                                    }
                                )
                                |> Array.ofList
                            TextDocument =
                                {
                                    Version = currentDocument |> Option.map (fun x -> x.Version)
                                    Uri = renameParams.TextDocument.Uri
                                }
                        }
                    |]
                WorkspaceEdit.Create(edits, clientCapabilities.Value) // TODO: а если `None`?
                |> Some
        let x =
            f getVarHighlight
            |> Option.orElseWith (fun () ->
                f getLocHighlight
            )
        return LspResult.success x
    }
    override __.TextDocumentDefinition textDocumentPositionParams = async {
        let f fn =
            match fn textDocumentPositionParams.Position with
            | None -> None
            | Some xs ->
                xs
                |> List.choose (fun (r, kind) ->
                    match kind with
                    | Qsp.Parser.Generic.WriteAccess ->
                        {
                            Location.Uri = textDocumentPositionParams.TextDocument.Uri
                            Location.Range = Position.ofInlineRange r
                        }
                        |> Some
                    | _ -> None
                )
                |> Array.ofList
                |> GotoResult.Multiple
                |> Some
        let x =
            f getVarHighlight
            |> Option.orElseWith (fun () ->
                f getLocHighlight
            )
        return LspResult.success x
    }

    override __.TextDocumentHover textDocumentPositionParams =
        async {
            let res =
                hovers
                |> List.tryFind (fun (r, _) ->
                    if (r.Start.Line = textDocumentPositionParams.Position.Line) && (r.Start.Line = r.End.Line) then
                        r.Start.Character <= textDocumentPositionParams.Position.Character && textDocumentPositionParams.Position.Character <= r.End.Character
                    elif r.Start.Line <= textDocumentPositionParams.Position.Line && textDocumentPositionParams.Position.Line <= r.End.Line then
                        false // TODO: @high ¯\_(ツ)_/¯
                    else
                        false // TODO: @high ¯\_(ツ)_/¯
                )
            // do! client.WindowLogMessage {
            //     Type = MessageType.Error
            //     Message = sprintf "%A" (hovers, x.Position)
            // }
            let x =
                match res with
                | None -> None
                | Some(r, msg) ->
                    let msg =
                        match msg with
                        | Parser.Generic.HoverDescription.FuncDescription predefFunc ->
                            Map.tryFind predefFunc Defines.functionBySymbolic
                            |> Option.map (fun x -> x.Description)
                            |> Option.defaultValue ""
                        | Parser.Generic.HoverDescription.RawDescription x -> x
                    {
                        Hover.Contents =
                            HoverContent.MarkupContent (markdown msg)
                        Range = Some r
                    }
                    |> Some
            return LspResult.success x
        }
    override __.TextDocumentReferences refParams = async {
        // refParams.Context.IncludeDeclaration // загадочный параметр
        let f fn =
            match fn refParams.Position with
            | None -> None
            | Some xs ->
                xs
                |> List.map (fun (r, _) ->
                    {
                        Location.Uri = refParams.TextDocument.Uri
                        Location.Range = Position.ofInlineRange r
                    }
                )
                |> Array.ofList
                |> Some
        let x =
            f getVarHighlight
            |> Option.orElseWith (fun () ->
                f getLocHighlight
            )
        return LspResult.success x
    }
    override __.TextDocumentFoldingRange foldingRangeParams =
        // let x =
        //     {
        //         FoldingRange.StartLine = 0
        //         StartCharacter = failwith "Not Implemented"
        //         EndLine = failwith "Not Implemented"
        //         EndCharacter = failwith "Not Implemented"
        //         Kind = Some FoldingRangeKind.Region

        //     }
        // foldingRangeParams.TextDocument.Uri
        async {
            return LspResult.success None
        }
    override __.WorkspaceDidChangeWorkspaceFolders p = async {
        do! client.WindowLogMessage {
            Type = MessageType.Info
            Message = sprintf "WorkspaceDidChangeWorkspaceFolders:\n%A" p
        }
    }
    // override __.WorkspaceWorkspaceFolders p = async {

    // }
    member __.FSharpWorkspaceLoad (p:WorkspaceLoadParms) = async {
        // Возвращает что-то в духе "e:\Project\Qsp\QspSyntax\sample-code", а не Uri, как там написано

        // currentWorkspacePath <- p.TextDocuments.[0].Uri

        // let dir = @"e:\Project\Qsp\QspSyntax\sample-code"
        // let projFiles = System.IO.Directory.GetFiles(dir, "*.qproj", System.IO.SearchOption.AllDirectories)
        // match projFiles with
        // | [||] -> () // "`.qproj` не найден"
        // | [|projFile|] ->
        //     projFile <- System.IO.Path.GetDirectoryName projFile
        // | projFiles ->
        //     // TODO: ошибкой было бы, если бы в одной и той же папке (или подпапке) было бы несколько файлов .qproj. Для всех остальных случаев нужно организовать работу с несколькими проектами. А ведь есть еще WorkspaceFolders.
        //     do! client.WindowShowMessage {
        //         Type = MessageType.Error
        //         Message = sprintf "`.qproj` должен быть только один на весь проект, однако:\n%A" projFiles
        //     }


        // do! client.WindowLogMessage {
        //     Type = MessageType.Info
        //     Message = sprintf "FSharpWorkspaceLoad:\n%A" p
        // }
        return LspResult.success None
    }

    override __.TextDocumentDocumentSymbol documentSymbolParams = async {
        let x =
            match currentDocument with
            | Some currentDocument ->
                let documentUri = currentDocument.Uri
                if documentUri = documentSymbolParams.TextDocument.Uri then
                    highlights.LocHighlights.Ma
                    |> Seq.choose (fun (KeyValue(locName, v)) ->
                        v
                        |> List.tryPick (fun (r, typ) ->
                            if typ = Parser.Generic.VarHighlightKind.WriteAccess then
                                {
                                    ContainerName = None
                                    Name = locName
                                    Kind = SymbolKind.Function
                                    Location =
                                        {
                                            Location.Uri = documentUri
                                            Range = Position.ofInlineRange r
                                        }
                                } |> Some
                            else None
                        )
                    )
                    |> Seq.sortBy (fun x -> x.Location.Range.Start.Line)
                    |> Array.ofSeq
                    |> Some
                else None
            | None -> None
        return LspResult.success x
    }

    override __.CompletionItemResolve completionItem = async {
        do! client.WindowLogMessage {
            Type = MessageType.Info
            Message = sprintf "CompletionItemResolve:\n%A" completionItem
        }

        return LspResult.success completionItem
    }
    // override __.TextDocumentCompletion completionParams = async {
    //     do! client.WindowLogMessage {
    //         Type = MessageType.Info
    //         Message = sprintf "TextDocumentCompletion:\n%A" completionParams
    //     }
    //     let x =
    //         {
    //             CompletionList.IsIncomplete = false
    //             Items = [
    //                 CompletionItem.Label = ""

    //             ]
    //         }
    //     return LspResult.success None
    // }
    member __.BuildSource (uriStr:UriString) isRun =
        async {
            if System.Environment.OSVersion.Platform = System.PlatformID.Win32NT then
                // let uri = "file:///e:/Project/Qsp/QSP-LSP/3rd/txt2gam.exe"
                let uri =
                    try
                        let uri = System.Uri uriStr
                        uri.LocalPath
                        |> Right
                    with e ->
                        Left e.Message
                let res =
                    match uri with
                    | Right path ->
                        try
                            let code, output = buildQsp path
                            if code = 0 then
                                if isRun then
                                    changeExtensionToQsp path
                                    |> System.Diagnostics.Process.Start
                                    |> ignore

                                Choice2Of2 "Ok"
                            else
                                Choice1Of2 (sprintf "txt2gam returned:\n%s" output)
                        with e ->
                            Choice1Of2 e.Message
                    | Left err ->
                        Choice1Of2 (sprintf "'%s'\n%A" uriStr err)
                return LspResult.success res
            else
                let res = Choice1Of2 (sprintf "Пока что txt2gam есть только Windows")
                return LspResult.success res
        }

    override __.Initialize p =
        async {
            clientCapabilities <- p.Capabilities
            /// { "AutomaticWorkspaceInit": false }
            let c =
                p.InitializationOptions
                |> Option.map (fun x -> x.ToString())

            return
                { Types.InitializeResult.Default with
                    Capabilities =
                        { Types.ServerCapabilities.Default with
                            HoverProvider = Some true
                            RenameProvider = Some true
                            DefinitionProvider = Some true
                            TypeDefinitionProvider = Some true
                            ImplementationProvider = Some true
                            ReferencesProvider = Some true
                            DocumentHighlightProvider = Some true
                            DocumentSymbolProvider = Some true
                            WorkspaceSymbolProvider = Some false
                            DocumentFormattingProvider = Some true
                            DocumentRangeFormattingProvider = Some false
                            SignatureHelpProvider =
                            // Some {
                            //     SignatureHelpOptions.TriggerCharacters = Some [| "("; ","|]
                            // }
                                None
                            CompletionProvider =
                                None
                            CodeLensProvider =
                            // Some {
                            //     CodeLensOptions.ResolveProvider = Some true
                            // }
                                None
                            CodeActionProvider = Some false
                            TextDocumentSync =
                                Some { TextDocumentSyncOptions.Default with
                                         OpenClose = Some true
                                         Change = Some TextDocumentSyncKind.Full
                                         Save = Some { IncludeText = Some true }
                                     }
                            FoldingRangeProvider = None
                        }
                }
                |> LspResult.success

    }
type LocalSetting = {
    DicPath : string
}
open FsharpMyExtension
[<EntryPoint>]
let main argv =
    match argv with
    | [| "--ping" |] ->
        printfn "PONG!"
        0
    | _ ->
        use input = System.Console.OpenStandardInput()
        use output = System.Console.OpenStandardOutput()

        let requestsHandlings =
            defaultRequestHandlings<BackgroundServiceServer>()
            |> Map.add "fsharp/highlighting" (requestHandling (fun s p -> s.GetHighlighting(p) ))
            |> Map.add "fsharp/workspaceLoad" (requestHandling (fun s p -> s.FSharpWorkspaceLoad(p) ))
            |> Map.add "qsp/build" (requestHandling (fun s p -> s.BuildSource p false ))
            |> Map.add "qsp/buildAndRun" (requestHandling (fun s p -> s.BuildSource p true ))

        Server.start requestsHandlings input output FsacClient (fun lspClient -> BackgroundServiceServer((), lspClient))
        |> printfn "%A"

        0
