module Qsp.Parser.Main
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either
open Qsp

open Qsp.Ast
open Qsp.Parser.Generic
open Qsp.Parser.Expr


let ppunctuationTerminator : _ Parser =
    appendToken Tokens.TokenType.PunctuationTerminatorStatement (pchar '&')

let pImplicitVarWhenAssign p =
    applyRange p
    >>= fun (range, (name:string)) ->
        let nameLower = name.ToLower()
        Defines.vars
        |> Map.tryFind nameLower
        |> function
            | Some dscr ->
                appendHover2 dscr range
            | None ->
                if Map.containsKey nameLower Defines.procs then
                    appendSemanticError range "Нельзя переопределять процедуру"
                elif Map.containsKey nameLower Defines.functions then
                    appendSemanticError range "Нельзя переопределять функцию"
                else
                    let dscr = "Пользовательская глобальная переменная числового типа"
                    appendHover2 dscr range
        >>. appendToken2 Tokens.TokenType.Variable range
        >>. appendVarHighlight range (ImplicitNumericType, name) VarHighlightKind.WriteAccess
        >>. preturn name

let pAssign stmts =
    let assdef name ass =
        let asscode =
            between (pchar '{' >>. spaces) (spaces >>. char_ws '}') stmts
            |>> fun stmts -> AssignCode(ass, stmts)

        let call =
            ident >>=?
            fun name ->
                followedBy (
                    ident
                    <|> (puint32 >>% "")
                    <|> stringLiteral)
                >>. sepBy1 pexpr (char_ws ',')
                |>> fun args -> Assign(ass, Func(name, args)) // То есть `a = min x, y` можно, что ли? Хм...
        let assexpr = call <|> (pexpr |>> fun defExpr -> Assign(ass, defExpr))

        let str_ws s =
            appendToken Tokens.TokenType.OperatorAssignment
                (pstring s)
            .>> ws

        choice [
            str_ws "-=" >>. pexpr |>> fun defExpr -> Assign(ass, Expr.Expr(Minus, Var name, defExpr))
            str_ws "=-" >>. pexpr |>> fun defExpr -> Assign(ass, Expr.Expr(Minus, defExpr, Var name))
            (str_ws "+=" <|> str_ws "=+") >>. pexpr |>> fun defExpr -> Assign(ass, Expr.Expr(Plus, Var name, defExpr))
            str_ws "=" >>. (asscode <|> assexpr)
        ]

    let assign name =
        let arr =
            bet_ws '[' ']' (opt pexpr)
            |>> fun braketExpr ->
                match braketExpr with
                | Some braketExpr ->
                    AssignArr(name, braketExpr)
                | None -> AssignArrAppend name
        arr <|>% AssignVar name >>=? assdef name
    let pExplicitAssign =
        let p =
            appendToken
                Tokens.TokenType.Type
                ((pstringCI "set" <|> pstringCI "let") .>>? notFollowedVarCont)
            .>> ws
            >>. (pexplicitVar VarHighlightKind.WriteAccess <|> (pImplicitVarWhenAssign ident |>> fun name -> ImplicitNumericType, name))
        p <|> pexplicitVar VarHighlightKind.WriteAccess .>>? ws
        >>=? assign

    let pImlicitAssign =
        pImplicitVarWhenAssign notFollowedByBinOpIdent .>>? ws
        >>=? fun name ->
            assign (ImplicitNumericType, name)
    pExplicitAssign <|> pImlicitAssign

let pcallProc =
    let f defines p =
        applyRange p
        >>= fun (range, name) ->
            let p =
                defines
                |> Map.tryFind (String.toLower name)
                |> function
                    | Some (dscr, sign) ->
                        appendHover2 dscr range
                        >>% Some sign
                    | None ->
                        [
                            "Такой процедуры нет, а если есть, то напишите мне, автору расширения, пожалуйста, и я непременно добавлю."
                            "Когда-нибудь добавлю: 'Возможно, вы имели ввиду: ...'"
                        ]
                        |> String.concat "\n"
                        |> appendSemanticError range
                        >>% None
            appendToken2 Tokens.TokenType.Procedure range
            >>. p
            |>> fun sign -> name, range, sign
    let pProcWithAsterix: _ Parser =
        let p =
            pchar '*' >>. many1Satisfy isIdentifierChar
            |>> sprintf "*%s" // да, так и хочется использоваться `many1Satisfy2`, но она довольствуется лишь первым символом, то есть '*', потому не подходит

        f Defines.proceduresWithAsterix p

    let procHoverAndToken =
        f Defines.procs notFollowedByBinOpIdent

    let pDefProc : _ Parser =
        Defines.procs
        |> Seq.sortByDescending (fun (KeyValue(name, _)) -> name) // для жадности
        |> Seq.map (fun (KeyValue(name, (dscr, sign))) ->
            applyRange (pstringCI name .>>? notFollowedVarCont)
            >>= fun (range, name) ->
                appendToken2 Tokens.TokenType.Procedure range
                >>. appendHover2 dscr range
                >>% (name, range, sign)
        )
        |> List.ofSeq
        |> choice
    /// Особый случай, который ломает к чертям весь заявленный синтаксис
    let adhoc =
        let createIdent name =
            pstringCI name .>>? notFollowedVarCont
        let p name name2 =
            createIdent name .>>? ws1 .>>.? createIdent name2
        applyRange
            ((p "add" "obj"
              <|> (createIdent "del" .>>? ws1 .>>.? (createIdent "obj" <|> createIdent "act"))
              |>> fun (name1, name2) -> name1 + name2)
             <|> (p "close" "all" |>> fun (name1, name2) -> sprintf "%s %s" name1 name2))
        >>= fun (range, name) ->
            match Map.tryFind (String.toLower name) Defines.procs with
            | Some (dscr, sign) ->
                appendToken2 Tokens.TokenType.Procedure range
                >>. appendHover2 dscr range
                >>% (name, range, sign)
            | None -> failwithf "'%s' not found in predef procs" name
    pProcWithAsterix
    .>> ws .>>. sepBy (applyRange pexpr) (char_ws ',') // Кстати, `,` — "punctuation.separator.parameter.js"
    <|> (adhoc <|> pDefProc .>> ws
         .>>. (followedBy (skipNewline <|> skipChar '&' <|> eof) >>% []
               <|> bet_ws '(' ')' (sepBy (applyRange pexpr) (pchar ',' >>. ws))
               <|> sepBy1 (applyRange pexpr) (char_ws ','))
         |>> fun ((name, range, sign), args) -> ((name, range, Some sign), args))
    <|> (procHoverAndToken
         .>>? (ws1 <|> followedBy (satisfy (isAnyOf "'\"")))
         .>>.? sepBy1 (applyRange pexpr) (char_ws ','))
    >>= fun ((name, range, sign), args) ->
        match sign with
        | None ->
            preturn (Proc(name, List.map snd args))
        | Some x ->
            let procNameLower = String.toLower name
            let pLoc =
                if Set.contains procNameLower Defines.transferOperatorsSet then
                    match args with
                    | (r, Val (String [[StringKind locName]]))::_ ->
                        getUserState
                        >>= fun (x:State) ->
                        let nested =
                            if x.SingleQuotNestedCount > x.DoubleQuotNestedCount then // TODO: ничего хорошего из этого не получится
                                x.SingleQuotNestedCount
                            else
                                x.DoubleQuotNestedCount
                            |> (+) 1
                        let r =
                            { r with
                                Column1 = r.Column1 + int64 nested // чтобы `'` или `"` пропустить
                                Column2 = r.Column2 - int64 nested
                            }
                        let locNameLower = String.toLower locName
                        appendLocHighlight r locNameLower VarHighlightKind.ReadAccess
                        >>. pGetDefLocPos locNameLower
                            >>= function
                                | None ->
                                    updateUserState (fun st ->
                                        { st with
                                            NotDefinedLocs =
                                                st.NotDefinedLocs
                                                |> Map.addOrMod locNameLower [r] (fun xs -> r::xs)
                                        }
                                    )
                                | Some _ -> preturn ()
                    | _ -> preturn ()
                else
                    preturn ()
            args
            |> Array.ofList
            |> Defines.getFuncByOverloadType x
            |> function
                | None ->
                    let msg =
                        Defines.Show.printSignature name x
                        |> sprintf "Ожидается одна из перегрузок:\n%s"
                    appendSemanticError range msg
                | Some () ->
                    preturn ()
            >>. pLoc
            >>% Proc(name, List.map snd args)

let pcomment : _ Parser =
    let stringLiteralWithToken : _ Parser =
        let bet tokenType openedChar closedChar =
            let p =
                many1Satisfy (fun c' -> not (c' = closedChar || c' = '\n'))
                <|> (attempt(skipChar closedChar >>. skipChar closedChar)
                      >>% string closedChar + string closedChar)
            pipe2
                (appendToken tokenType (pchar openedChar)
                 >>. appendToken tokenType (manyStrings p))
                (many
                    (newline >>. appendToken tokenType (manyStrings p))
                 .>> appendToken tokenType (pchar closedChar)) // TODO: Здесь самое то использовать `PunctuationDefinitionStringEnd`
                (fun x xs ->
                    x::xs |> String.concat "\n"
                    |> fun x -> sprintf "%c%s%c" openedChar x closedChar
                    )
        bet Tokens.TokenType.Comment '\'' '\''
        <|> bet Tokens.TokenType.Comment '"' '"'
    let p =
        appendToken Tokens.TokenType.Comment
            (many1Satisfy (fun c -> c <> '\n' && c <> ''' && c <> '"' && c <> '{'))
        <|> stringLiteralWithToken
        <|> (pbraces Tokens.TokenType.Comment |>> sprintf "{%s}")
    appendToken Tokens.TokenType.Comment (pchar '!')
    >>. manyStrings p |>> Statement.Comment

let psign =
    appendToken Tokens.TokenType.LabelColon
        (pchar ':')
    >>. ws
    >>. appendToken Tokens.TokenType.NameLabel
            (many1SatisfyL ((<>) '\n') "labelName") // TODO: literal? Trim trailing spaces
    |>> Label
let genKeywordParser tokenType keyword =
    let dscr =
        Defines.keywords
        |> List.tryPick (fun (name, dscr) ->
            if name = keyword then Some dscr
            else None)
        |> Option.defaultWith (fun () -> failwithf "not found %s" keyword)
    appendTokenHover tokenType dscr
        (pstringCI keyword .>>? notFollowedVarCont)
let pexit : _ Parser =
    genKeywordParser Tokens.TokenType.Exit "exit"
    >>% Exit

let pendKeyword : _ Parser =
    genKeywordParser Tokens.TokenType.End "end"
let pstmts' pstmt =
    many
        (pstmt .>> spaces
         .>> (skipMany (ppunctuationTerminator .>> spaces)))
let pstmts1' pstmt =
    many1
        (pstmt .>> spaces
         .>> (skipMany (ppunctuationTerminator .>> spaces)))
let pstmt =
    let pstmt, pstmtRef = createParserForwardedToRef<Statement, _>()
    let pInlineStmts =
        many (pstmt .>> ws .>> skipMany (ppunctuationTerminator .>> ws))
    let pInlineStmts1 =
        many1 (pstmt .>> ws .>> skipMany (ppunctuationTerminator .>> ws))
    let pstmts = pstmts' pstmt

    let pcolonKeyword : _ Parser =
        appendToken Tokens.TokenType.Colon (pchar ':')

    let pAct =
        let pactKeyword : _ Parser =
            genKeywordParser Tokens.TokenType.Act "act"

        let pactHeader = pactKeyword .>> ws >>. sepBy1 pexpr (char_ws ',') .>> pcolonKeyword

        pipe2
            pactHeader
            ((ws >>? skipNewline >>. spaces >>. pstmts .>> pendKeyword)
              <|> (spaces >>. pInlineStmts .>> optional pendKeyword))
            (fun expr body ->
                Act(expr, body))
    let pFor =
        let pForHeader =
            genKeywordParser Tokens.TokenType.For "for" >>. ws
            >>. (pexplicitVar VarHighlightKind.WriteAccess
                 <|> (pImplicitVarWhenAssign ident |>> fun name -> ImplicitNumericType, name))
            .>> ws .>> appendToken Tokens.TokenType.OperatorAssignment (pchar '=')
            .>> ws .>>. pexpr
            .>> genKeywordParser Tokens.TokenType.To "to"
            .>> ws .>>. pexpr
            .>>. opt (genKeywordParser Tokens.TokenType.Step "step"
                      .>> ws >>. pexpr)
            .>> pcolonKeyword

        pipe2
            pForHeader
            ((ws >>? skipNewline >>. spaces >>. pstmts .>> pendKeyword)
              <|> (spaces >>. pInlineStmts .>> optional pendKeyword))
            (fun (((var, fromExpr), toExpr), stepExpr) body ->
                For(var, fromExpr, toExpr, stepExpr, body))
    let pIf =
        let pifKeyword : _ Parser =
            genKeywordParser Tokens.TokenType.If "if"
        let pelseifKeyword : _ Parser =
            genKeywordParser Tokens.TokenType.ElseIf "elseif"
        let pelseKeyword : _ Parser =
            genKeywordParser Tokens.TokenType.Else "else"
        let pifHeader = pifKeyword .>> ws >>. pexpr .>> pcolonKeyword
        let pelseifHeader = pelseifKeyword .>> ws >>. pexpr .>> pcolonKeyword

        let setIsEndOptionalTo boolean =
            updateUserState (fun x -> { x with IsEndOptional = boolean })

        let p =
            ws .>>? skipNewline >>. spaces >>. pstmts .>> setIsEndOptionalTo false
            <|> (spaces >>. pInlineStmts .>> setIsEndOptionalTo true)
        let pElse1 =
            pelseKeyword >>. ws
            >>. (pInlineStmts1 .>> opt pendKeyword
                 <|> (spaces >>. pstmts .>> pendKeyword))
        let pend =
            getUserState
            >>= fun x ->
                if x.IsEndOptional then
                    optional pendKeyword
                else
                    pendKeyword >>% ()

        let pelseIf =
            many1 (pelseifHeader .>>. p)
            .>>. (pElse1 <|> (pend >>% []))
            |>> fun (elifs, elseBody) ->
                let rec f = function
                    | (expr, thenBody)::xs ->
                        [If(expr, thenBody, f xs)]
                    | [] -> elseBody
                f elifs

        // `end` нужен, чтобы инструкции, определенные ниже, не ушли в тело `if`
        // ```qsps
        // if expr:
        //     stmt1
        // end & ! без него `stmt2` станет принадлежать телу `if`
        // stmt2
        // ...
        // ```

        // `if expr: stmt1 & stmt2 & ...` — такому выражению `end` не нужен, поскольку эту роль выполняет конец строки.
        // Также работает и с `elif expr: stmt1 & stmt2 & ...`, и с `else expr: stmt1 & stmt2 & ...`.
        pipe2
            (pifHeader .>> ws)
            ((pInlineStmts1 .>>. (pelseIf <|> pElse1 <|> (opt pendKeyword >>% []))
             <|> (spaces >>. pstmts .>>. (pelseIf <|> pElse1 <|> (pendKeyword >>% [])))))
            (fun expr (thenBody, elseBody) ->
                If(expr, thenBody, elseBody))
    pstmtRef :=
        choice [
            pcomment
            pexit
            psign
            pIf
            pAct
            pFor
            pAssign pstmts
            pcallProc
            notFollowedBy (pchar '-' >>. ws >>. (skipNewline <|> skipChar '-' <|> eof)) // `-` завершает локацию
            >>. (pexpr |>> fun arg -> Proc("*pl", [arg]))
        ]
    pstmt

let pstmts = pstmts' pstmt
let pstmts1 = pstmts1' pstmt

let psharpKeyword : _ Parser =
    appendToken Tokens.TokenType.SharpBeginLoc (pchar '#')
let pminusKeyword : _ Parser =
    appendToken Tokens.TokenType.MinusEndLoc (pchar '-') // хотя здесь больше подошел бы обычный `end`
let ploc =
    let pendKeyword =
        applyRange (pstringCI "end" .>>? notFollowedVarCont)
        >>= fun (range, _) ->
            appendToken2 Tokens.TokenType.End range
            >>. appendSemanticError range "Лишний `end`"
    pipe2
        (psharpKeyword .>> ws
         >>. (applyRange
                (many1Strings
                    (many1Satisfy (isNoneOf " \t\n")
                     <|> (many1Satisfy (isAnyOf " \t") .>>? notFollowedByNewline))
                 <?> "location name")
              >>= fun (r, name) ->
                let pCheckLocExists r2 locName =
                    pGetDefLocPos locName
                    >>= function
                        | Some r ->
                            sprintf "Локация уже определена в\n%A" r
                            |> appendSemanticError r2
                        | None -> preturn ()

                let locNameLower = String.toLower name
                pCheckLocExists r locNameLower
                >>. updateUserState (fun st ->
                        { st with
                            NotDefinedLocs =
                                Map.remove locNameLower st.NotDefinedLocs // ну да, к чему проверки? И так удалит
                        }
                    )
                >>. appendLocHighlight r locNameLower VarHighlightKind.WriteAccess // и все равно добавить, даже в случае семантической ошибки? Хм, ¯\_(ツ)_/¯
                >>. appendToken2 Tokens.TokenType.StringQuotedSingle r
                >>. preturn name
             )
         .>> spaces)
        (many (pstmts1 .>> many (pendKeyword .>> spaces)) |>> List.concat
         .>> (pminusKeyword .>> ws
              .>> appendToken Tokens.TokenType.Comment
                    (skipManySatisfy ((<>) '\n'))))
        (fun name body -> Location(name, body))

let pAfterAll =
    preturn ()
let start str =
    let emptyState =
        { emptyState with PStmts = pstmts }
    let p =
        spaces >>. many (ploc .>> spaces)
        .>> (getPosition >>= fun p ->
                updateUserState (fun st ->
                    { st with LastSymbolPos = p}))
    runParserOnString (p .>> pAfterAll .>> eof)
        emptyState
        ""
        str
let startOnFile enc path =
    let emptyState =
        { emptyState with PStmts = pstmts }
    let p =
        spaces >>. many (ploc .>> spaces)
        .>> (getPosition >>= fun p ->
                updateUserState (fun st ->
                    { st with LastSymbolPos = p}))
    runParserOnFile (p .>> pAfterAll .>> eof)
        emptyState
        path
        enc
