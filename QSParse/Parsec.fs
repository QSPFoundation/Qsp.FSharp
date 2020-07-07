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

    let pExplicitAssign =
        let p =
            appendToken
                TokenType.Type
                ((pstringCI "set" <|> pstringCI "let") .>>? notFollowedVarCont)
            .>> ws
            >>. (pexplicitVar <|> (ident |>> fun name -> ImplicitNumericType, name))
        p <|> pexplicitVar .>>? ws
        >>=? assign

    let pImlicitAssign =
        appendToken TokenType.Variable notFollowedByBinOpIdent .>>? ws
        >>=? fun name ->
            assign (ImplicitNumericType, name)
    pExplicitAssign <|> pImlicitAssign

let pcallProc =
    let pProcWithAsterix: _ Parser =
        // TODO: вообще-то, это уже уровень семантики, ведь синтаксис вполне допускает название такой процедуры
        let p =
            Defines.proceduresWithAsterix
            |> List.sortDescending
            |> List.map pstringCI
            |> choice
        appendToken TokenType.Procedure
            (p .>>? notFollowedVarCont)

    (appendToken TokenType.Procedure
         pProcWithAsterix
     .>> ws
     .>>. sepBy pexpr (char_ws ',')) // Кстати, `,` — "punctuation.separator.parameter.js"
     <|> (appendToken TokenType.Procedure
              notFollowedByBinOpIdent
          .>>? (ws1 <|> followedBy (satisfy (isAnyOf "'\"")))
          .>>.? sepBy1 pexpr (char_ws ','))
    |>> fun (name, args) ->
        CallSt(name, args)

let pcomment : _ Parser =
    let p =
        appendToken TokenType.Comment
            (many1Satisfy (fun c -> c <> '\n' && c <> ''' && c <> '"' && c <> '{'))
        <|> stringLiteralWithToken
        <|> pbraces
    appendToken TokenType.Comment (pchar '!')
    >>. manyStrings p |>> Statement.Comment

let psign =
    appendToken TokenType.LabelColon
        (pchar ':')
    >>. ws
    >>. appendToken TokenType.NameLabel
            (many1SatisfyL ((<>) '\n') "labelName") // TODO: literal? Trim trailng spaces
    |>> Label
let pexit : _ Parser =
    appendToken TokenType.KeywordControl
        (pstringCI "exit" .>>? notFollowedVarCont)
    >>% Exit

let pendKeyword : _ Parser =
    appendToken TokenType.KeywordControl
        (pstringCI "end" .>>? notFollowedVarCont)

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
            appendToken TokenType.KeywordControl
                (pstringCI "act" .>>? notFollowedVarCont)

        let pactHeader = pactKeyword .>> ws >>. sepBy1 pexpr (char_ws ',') .>> pcolonKeyword

        pipe2
            pactHeader
            ((ws >>? skipNewline >>. spaces >>. pstmts .>> pendKeyword)
              <|> (spaces >>. pInlineStmts .>> optional pendKeyword))
            (fun expr body ->
                Act(expr, body))
    let pIf =
        let pifKeyword : _ Parser =
            appendToken TokenType.KeywordControl
                (pstringCI "if" .>>? notFollowedVarCont)
        let pelseifKeyword : _ Parser =
            appendToken TokenType.KeywordControl
                (pstringCI "elseif" .>>? notFollowedVarCont)
        let pelseKeyword : _ Parser =
            appendToken TokenType.KeywordControl
                (pstringCI "else" .>>? notFollowedVarCont)
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
            attempt (pexpr |>> StarPl) // `attempt` — только ради одного единственного случая: `-`, который завершает локацию
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
         >>. appendToken TokenType.StringQuotedSingle // надо же как-то подчеркнуть название локации
                (many1Satisfy ((<>) '\n'))
         .>> spaces)
        (pstmts
         .>> (pminusKeyword .>> ws
              .>> appendToken TokenType.Comment
                    (skipManySatisfy ((<>) '\n'))))
        (fun name body -> Location(name, body))

let start str =
    let p =
        spaces >>. many (ploc .>> spaces)
        .>> (getPosition >>= fun p ->
                updateUserState (fun st ->
                    { st with LastSymbolPos = p}))
    runParserOnString (p .>> eof)
        emptyState
        ""
        str
let startOnFile enc path =
    let p =
        spaces >>. many (ploc .>> spaces)
        .>> (getPosition >>= fun p ->
                updateUserState (fun st ->
                    { st with LastSymbolPos = p}))
    runParserOnFile (p .>> eof)
        emptyState
        path
        enc
