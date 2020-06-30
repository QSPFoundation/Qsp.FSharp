module Program
open FsharpMyExtension
open FsharpMyExtension.Either
open LanguageServerProtocol
open LanguageServerProtocol.Server
open LanguageServerProtocol.Types

// type Msg = { Value: string }
type SourceFilePath = string


type FsacClient(sendServerRequest: ClientNotificationSender) =
    inherit LspClient ()

    override __.WindowShowMessage(p) =
        sendServerRequest "window/showMessage" (box p) |> Async.Ignore

    override __.WindowLogMessage(p) =
        sendServerRequest "window/logMessage" (box p) |> Async.Ignore

    override __.TextDocumentPublishDiagnostics(p) =
        sendServerRequest "textDocument/publishDiagnostics" (box p) |> Async.Ignore

type State = {
    DicPath : string
}

type UpdateFileParms = {
    // File: BackgroundFileCheckType
    Content: string
    Version: int
}

type YASpellChecker =
    { SpellcheckIgnore : DocumentUri Set }
type Config =
    { YASpellChecker : YASpellChecker option }
    static member Default =
        {
            YASpellChecker = None
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

    let mutable documentVersion : int option = None
    let mutable lastWords = Map.empty
    let publishDiagnostics uri (text:string) =
        let publishDiagnostics (res:list<_>) =
            res
            |> List.map (fun (range, word) ->
                {
                    Range = range
                    Severity = Some (DiagnosticSeverity.Hint)
                    Code = None
                    Source = "qsp"
                    Message = sprintf "unknown '%s'" word
                    RelatedInformation = None
                    Tags = None
                })
            |> Array.ofList
        async {
            match parser text with
            | Right res2 ->
                let res =
                    res2
                    |> List.map (fun (word, pos) ->
                        let range =
                            {
                                Start = { Line = int pos.StartLine - 1
                                          Character = int pos.StartColumn - 1 }
                                End = { Line = int pos.EndLine - 1
                                        Character = int pos.EndColumn - 1 }
                            }
                        range, word
                    )
                lastWords <- Map.ofList res
                do! client.TextDocumentPublishDiagnostics {
                    Uri = uri
                    Diagnostics = publishDiagnostics res
                }

                do! client.SpellcheckDecorate (res2 |> List.map snd)
            | Left x ->
                do! client.WindowLogMessage {
                    Type = MessageType.Error
                    Message = sprintf "Parser error:\n%A" x
                }
        }
    let interval = 500.
    let reactor =
        restartableTimer interval
            (fun e (uri, text) ->
                publishDiagnostics uri text
                |> Async.RunSynchronously
            )

    override __.TextDocumentDidChange(p) = async {
        if Set.contains (p.TextDocument.Uri.ToLower()) spellcheckIgnore then
            // Удаляет ошибки, если были проставлены до того, как документ попал в фильтр
            do! client.TextDocumentPublishDiagnostics {
                Uri = p.TextDocument.Uri
                Diagnostics = [||]
            }
            do! client.SpellcheckDecorate []
        else
            do! client.WindowLogMessage {
                Type = MessageType.Info
                Message = sprintf "TextDocumentDidChange\n%A" p.TextDocument
            }

            documentVersion <- p.TextDocument.Version
            match Array.tryExactlyOne p.ContentChanges with
            | Some x ->
                reactor (p.TextDocument.Uri, x.Text)
            | None ->
                do! client.WindowLogMessage {
                    Type = MessageType.Error
                    Message = sprintf "Array.tryExactlyOne p.ContentChanges error:\n%A" p.ContentChanges
                }
    }
    override __.TextDocumentDidOpen(p: DidOpenTextDocumentParams) = async {
        // Вот что, этот негодяй одновременно открывает целую кучу всего: здесь и git, и обычный файл, и даже output. Надо бы как-то за всем этим уследить.
        // TODO: когда найду способ выделять именно текущий документ, тогда и раскомментирую
        // do! client.WindowLogMessage {
        //     Type = MessageType.Info
        //     Message =
        //         let txt = p.TextDocument
        //         sprintf "TextDocumentDidOpen\n%A"
        //             ( txt.LanguageId, txt.Uri, txt.Version)
        // }
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
        ()
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

    override this.TextDocumentCodeAction p = async {
        if Set.contains (p.TextDocument.Uri.ToLower()) spellcheckIgnore then
            // TODO: если текущий файл отсеивается, то самое время как-то избавить весь документ от ошибок, вот только как это сделать?
            // return LspResult.Ok None // Пробовал — выбивает ошибку
            return LspResult.success (Some (TextDocumentCodeActionResult.CodeActions [||]))
        else
            let! res =
                p
                |> this.IfDiagnostic "unknown " (fun d ->
                    async {
                        do! client.WindowLogMessage {
                            Type = MessageType.Info
                            Message = (sprintf "TextDocumentCodeAction 'unknown ...'")
                        }
                        match Map.tryFind d.Range lastWords with
                        | Some word ->
                            let words =
                                Suggestion.LevenshteinDistance.suggestions3 word dic
                                |> Suggestion.LevenshteinDistance.mapTruncate 10

                            let actions =
                                words
                                |> List.map (fun word ->
                                    this.CreateFix p.TextDocument.Uri documentVersion (sprintf "replace on '%s'" word) (Some d) d.Range word)
                            return actions
                        | None ->
                            do! client.WindowLogMessage {
                                Type = MessageType.Info
                                Message = (sprintf "range not found:\n%A" d.Range)
                            }
                            return []
                    }
                )
            return res |> Array.ofList |> TextDocumentCodeActionResult.CodeActions |> Some |> LspResult.success
    }


    override __.WorkspaceDidChangeConfiguration (x) = async {
            do! client.WindowLogMessage {
                Type = MessageType.Info
                Message = sprintf "WorkspaceDidChangeConfiguration\n%A" (x.Settings.ToString())
            }

            let config : Either<_, Config> =
                let ser = Newtonsoft.Json.JsonSerializer()
                ser.Converters.Add FSharpJsonType.SerializeOption.converter
                try
                    x.Settings.ToObject(ser)
                    |> Right
                with
                    e -> Left e.Message
            match config with
            | Right x ->
                x.YASpellChecker
                |> Option.iter (fun x ->
                    spellcheckIgnore <- x.SpellcheckIgnore
                )
            | Left msg ->
                do! client.WindowLogMessage {
                    Type = MessageType.Error
                    Message = sprintf "%s\n%s" (x.Settings.ToString()) msg
                }
        }
    override x.TextDocumentDocumentHighlight(p) =
        // p.Position
        // p.TextDocument.Uri
        ()
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
                            HoverProvider = None
                            RenameProvider = Some true
                            DefinitionProvider = Some true
                            TypeDefinitionProvider = Some true
                            ImplementationProvider = Some true
                            ReferencesProvider = Some true
                            DocumentHighlightProvider = Some true
                            DocumentSymbolProvider = None
                            WorkspaceSymbolProvider = Some true
                            DocumentFormattingProvider = Some true
                            DocumentRangeFormattingProvider = Some false
                            SignatureHelpProvider =
                            // Some {
                            //     SignatureHelpOptions.TriggerCharacters = Some [| "("; ","|]
                            // }
                                None
                            CompletionProvider =
                                None
                                // Some {
                                //     ResolveProvider = Some true
                                //     TriggerCharacters = Some ([| "."; "'"; |])
                                // }
                            CodeLensProvider =
                            // Some {
                            //     CodeLensOptions.ResolveProvider = Some true
                            // }
                                None
                            CodeActionProvider = Some true
                            TextDocumentSync =
                                // None
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
    let localSettingPath = @"E:\Project\YetAnotherSpellCheckerServer\YacsServer\bin\Debug\net461\setting.json"
    // Json.serf localSettingPath { DicPath = @"E:\Project\YetAnotherSpellCheckerServer\fullRussian.txt"}
    let localSetting : LocalSetting = Json.desf localSettingPath
    let state = {
        State.DicPath = localSetting.DicPath
    }

    use input = System.Console.OpenStandardInput()
    use output = System.Console.OpenStandardOutput()

    let requestsHandlings =
        defaultRequestHandlings<BackgroundServiceServer>()
        // |> Map.add "spellcheck/setSpellcheckIgnore" (requestHandling (fun (s) p -> s.SetSpellcheckIgnore(p) ))

    Server.start requestsHandlings input output FsacClient (fun lspClient -> BackgroundServiceServer(state, lspClient))
    |> printfn "%A"

    0
