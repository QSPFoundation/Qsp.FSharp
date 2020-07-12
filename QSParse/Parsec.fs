module Qsp.Parser.Main
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either
open Qsp

open Qsp.Ast
open Qsp.Parser.Generic
open Qsp.Parser.Expr
open Qsp.Tokens

let ppunctuationTerminator : _ Parser =
    appendToken TokenType.KeywordControl (pchar '&')

let pAssign stmts =
    let assdef name ass =
        let asscode =
            between (pchar '{' >>. spaces) (spaces >>. char_ws '}') stmts
            |>> fun stmts -> AssingCode(ass, stmts)

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
            appendToken TokenType.OperatorAssignment
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
            bet_ws '[' ']' pexpr
            |>> fun braketExpr -> AssignArr(name, braketExpr)
        arr <|>% AssignVar name >>=? assdef name
    let highlightImplicitVar p =
        getPosition .>>.? p .>>. getPosition
        >>= fun ((p1, (name:string)), p2) ->
            let range = toRange p1 p2
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
            >>. appendToken2 TokenType.Variable range
            >>. appendVarHighlight range (ImplicitNumericType, name) VarHighlightKind.WriteAccess
            >>. preturn name
    let pExplicitAssign =
        let p =
            appendToken
                TokenType.Type
                ((pstringCI "set" <|> pstringCI "let") .>>? notFollowedVarCont)
            .>> ws
            >>. (pexplicitVar VarHighlightKind.WriteAccess <|> (highlightImplicitVar ident |>> fun name -> ImplicitNumericType, name))
        p <|> pexplicitVar VarHighlightKind.WriteAccess .>>? ws
        >>=? assign

    let pImlicitAssign =
        highlightImplicitVar notFollowedByBinOpIdent .>>? ws
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
            appendToken2 TokenType.Procedure range
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
            applyRange (pstringCI name .>> notFollowedVarCont)
            >>= fun (range, name) ->
                appendToken2 TokenType.Procedure range
                >>. appendHover2 dscr range
                >>% (name, range, sign)
        )
        |> List.ofSeq
        |> choice

    pProcWithAsterix
    .>> ws .>>. sepBy (applyRange pexpr) (char_ws ',') // Кстати, `,` — "punctuation.separator.parameter.js"
    <|> (pDefProc .>> ws
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
            preturn (CallSt(name, List.map snd args))
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
                                            LocsThatNeedCheck =
                                                st.LocsThatNeedCheck
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
            >>% CallSt(name, List.map snd args)

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
        bet TokenType.Comment '\'' '\''
        <|> bet TokenType.Comment '"' '"'
    let p =
        appendToken TokenType.Comment
            (many1Satisfy (fun c -> c <> '\n' && c <> ''' && c <> '"' && c <> '{'))
        <|> stringLiteralWithToken
        <|> (pbraces TokenType.Comment |>> sprintf "{%s}")
    appendToken TokenType.Comment (pchar '!')
    >>. manyStrings p |>> Statement.Comment

let psign =
    appendToken TokenType.LabelColon
        (pchar ':')
    >>. ws
    >>. appendToken TokenType.NameLabel
            (many1SatisfyL ((<>) '\n') "labelName") // TODO: literal? Trim trailng spaces
    |>> Label
let genKeywordParser keyword =
    let dscr =
        Qsp.Defines.keywords
        |> List.tryPick (fun (name, dscr) ->
            if name = keyword then Some dscr
            else None)
        |> Option.defaultWith (fun () -> failwithf "not found %s" keyword)
    appendTokenHover TokenType.KeywordControl dscr
        (pstringCI keyword .>>? notFollowedVarCont)
let pexit : _ Parser =
    genKeywordParser "exit"
    >>% Exit

let pendKeyword : _ Parser =
    genKeywordParser "end"

let pstmt =
    let pstmt, pstmtRef = createParserForwardedToRef<Statement, _>()
    let pInlineStmts =
        many (pstmt .>> ws .>> skipMany (ppunctuationTerminator .>> ws))
    let pInlineStmts1 =
        many1 (pstmt .>> ws .>> skipMany (ppunctuationTerminator .>> ws))
    let pstmts =
        many
            (pstmt .>> spaces
             .>> (skipMany (ppunctuationTerminator .>> spaces)))

    let pcolonKeyword : _ Parser =
        appendToken TokenType.KeywordControl (pchar ':')

    let pAct =
        let pactKeyword : _ Parser =
            genKeywordParser "act"

        let pactHeader = pactKeyword .>> ws >>. sepBy1 pexpr (char_ws ',') .>> pcolonKeyword

        pipe2
            pactHeader
            ((ws >>? skipNewline >>. spaces >>. pstmts .>> pendKeyword)
              <|> (spaces >>. pInlineStmts .>> optional pendKeyword))
            (fun expr body ->
                Act(expr, body))
    let pIf =
        let pifKeyword : _ Parser =
            genKeywordParser "if"
        let pelseifKeyword : _ Parser =
            genKeywordParser "elseif"
        let pelseKeyword : _ Parser =
            genKeywordParser "else"
        let pifHeader = pifKeyword .>> ws >>. pexpr .>> pcolonKeyword
        let pelseifHeader = pelseifKeyword .>> ws >>. pexpr .>> pcolonKeyword

        // ```qsp
        // if expr1:
        //     ...
        // elif expr2:
        //     ...
        // elif expr3:
        //     ...
        // else
        //     ...
        // end
        // ```
        // И это превращается в:
        // ```qsp
        // if expr1:
        //     ...
        // else
        //     if expr2:
        //         ...
        //     else
        //         if expr3:
        //             ...
        //         else
        //             ...
        //         end
        //     end
        // end
        // ```
        let setIsEndOptionalTo boolean =
            updateUserState (fun x -> { x with IsEndOptional = boolean })

        let p =
            ws .>>? skipNewline >>. spaces >>. pstmts .>> setIsEndOptionalTo false
            <|> (spaces >>. pInlineStmts .>> setIsEndOptionalTo true) // .>> spaces
        let pElse1 =
            pelseKeyword >>. ws
            >>. (pInlineStmts1 .>> opt pendKeyword // .>> (skipNewline <|> eof)
                 <|> (spaces >>. pstmts .>> pendKeyword))
        let pend =
            getUserState
            >>= fun x ->
                if x.IsEndOptional then
                    optional pendKeyword
                else
                    pendKeyword >>% ()
        // let pelseIf, pelseIfRef = createParserForwardedToRef()
        let pelseIf =
            // let pelseIf' =
            //     pelseifHeader .>> ws
            //     .>>. (pInlineStmts1 <|> (spaces >>. pstmts))


            // pelseifHeader .>> ws
            // .>>. ((pInlineStmts1 .>>. many (pelseifHeader .>> ws .>>. pInlineStmts1)) .>>. (pElse1 <|> (opt pendKeyword >>% [])) // .>> (skipNewline <|> eof)
            //       <|> (spaces >>. (pstmts .>>. many (pelseifHeader .>> ws .>>. pInlineStmts1)) .>>. (pElse1 <|> (pendKeyword >>% []))))
            // |>> fun (expr, ((thenBody, elifs), elseBody)) ->
            //     let rec f = function
            //         | (expr, thenBody)::xs ->
            //             [If(expr, thenBody, f xs)]
            //         | [] -> elseBody
            //     [If(expr, thenBody, f elifs)]
            many1 (pelseifHeader .>>. p)
            .>>. (pElse1 <|> (pend >>% []))
            |>> fun (elifs, elseBody) ->
                let rec f = function
                    | (expr, thenBody)::xs ->
                        [If(expr, thenBody, f xs)]
                    | [] -> elseBody
                f elifs
        let pElseIfs1 =
            pipe2
                (many1
                    (pelseifHeader .>>. p))
                    //  .>> optional (notFollowedBy pendKeyword >>. spaces))) // TODO: UserState не меняется, если изменить его в `notFollowedBy`?
                (opt (pelseKeyword >>. p))
                (fun elifs elseBody ->
                    let elseBody = elseBody |> Option.defaultValue []
                    let rec f = function
                        | (expr, thenBody)::xs ->
                            [If(expr, thenBody, f xs)]
                        | [] -> elseBody
                    f elifs
                    // List.fold (fun st ->
                    //     function
                    //     | (If(expr, thenBody, [])) ->
                    //         [If(expr, thenBody, st)]
                    //     | x -> failwithf "%A" x)
                    //     elseBody
                )

        let pElse =
            pipe2
                (many
                    (pelseifHeader .>>. p))
                    //  .>> optional (notFollowedBy pendKeyword >>. spaces))) // TODO: UserState не меняется, если изменить его в `notFollowedBy`?
                (opt
                    (pelseKeyword >>. p))
                (fun elifs elseBody ->
                    let elseBody = elseBody |> Option.defaultValue []
                    let rec f = function
                        | (expr, thenBody)::xs ->
                            [If(expr, thenBody, f xs)]
                        | [] -> elseBody
                    f elifs
                    // List.fold (fun st ->
                    //     function
                    //     | (If(expr, thenBody, [])) ->
                    //         [If(expr, thenBody, st)]
                    //     | x -> failwithf "%A" x)
                    //     elseBody
                )

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
            // pifHeader
            // (ws >>? skipNewline >>. spaces >>. pstmts .>>. (setIsEndOptionalTo false >>. pElse .>> pend)
            //  <|> (spaces >>. pInlineStmts .>>. (setIsEndOptionalTo true >>. pElse .>> pend)))
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
            pAssign pstmts
            pcallProc
            notFollowedBy (pchar '-' >>. ws >>. (skipNewline <|> skipChar '-')) >>. (pexpr |>> StarPl) // `-` завершает локацию
        ]
    pstmt
let pstmts =
    many
        (pstmt .>> spaces
         .>> (skipMany (ppunctuationTerminator .>> spaces)))
let psharpKeyword : _ Parser =
    appendToken TokenType.KeywordControl (pchar '#')
let pminusKeyword : _ Parser =
    appendToken TokenType.KeywordControl (pchar '-') // хотя здесь больше подошел бы обычный `end`
let ploc =
    pipe2
        (psharpKeyword .>> ws
         >>. (applyRange
                (many1Strings
                    (many1Satisfy (isNoneOf " \t\n")
                     <|> (many1Satisfy (isAnyOf " \t") .>>? notFollowedByNewline))
                 <?> "location name")
              >>= fun (r, name) ->
                let checkLocsWhatNeedToCheck locName = // да-да, название шикарное
                    updateUserState (fun st ->
                        { st with
                            LocsThatNeedCheck =
                                Map.remove locName st.LocsThatNeedCheck // ну да, к чему проверки? И так удалит
                        }
                    )
                let pCheckLocExists r2 locName =
                    pGetDefLocPos locName
                    >>= function
                        | Some r ->
                            sprintf "Локация уже определена в\n%A" r
                            |> appendSemanticError r2
                        | None -> preturn ()

                let locNameLower = String.toLower name
                pCheckLocExists r locNameLower
                >>. checkLocsWhatNeedToCheck locNameLower
                >>. appendLocHighlight r locNameLower VarHighlightKind.WriteAccess // и все равно добавить, даже в случае семантической ошибки? Хм, ¯\_(ツ)_/¯
                >>. appendToken2 TokenType.StringQuotedSingle r
                >>. preturn name
             )
         .>> spaces)
        (pstmts
         .>> (pminusKeyword .>> ws
              .>> appendToken TokenType.Comment
                    (skipManySatisfy ((<>) '\n'))))
        (fun name body -> Location(name, body))
let pAfterAll =
    updateUserState (fun st ->
        let errors =
            st.LocsThatNeedCheck
            |> Map.fold (fun st locNameLower ->
                List.fold (fun st range ->
                    let msg = sprintf "'%s' — эта локация не определена" locNameLower
                    (range, msg) :: st) st
            ) st.SemanticErrors
        { st with SemanticErrors = errors }
    )

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
