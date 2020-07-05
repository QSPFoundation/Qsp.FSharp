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

let pAssign =
    let assdef name ass =
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
            str_ws "=" >>. assexpr
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
            |> List.map pstring
            |> choice
        appendToken TokenType.Procedure
            (p .>>? notFollowedVarCont)

    (appendToken TokenType.Procedure
         pProcWithAsterix
     .>> ws
     .>>. sepBy pexpr (char_ws ',')) // Кстати, `,` — "punctuation.separator.parameter.js"
     <|> (appendToken TokenType.Procedure
              notFollowedByBinOpIdent
          .>>? (ws1 <|> followedBy (satisfy (isAnyOf "'\"(")))
          .>>.? sepBy1 pexpr (char_ws ','))
    |>> fun (name, args) ->
        CallSt(name, args)

let pcomment : _ Parser =
    let stringLiteral2 c =
        let normalCharSnippet = many1Satisfy ((<>) c)
        let cs = string c
        let escapedChar = pstring (cs + cs)
        between (pchar c) (pchar c)
                (manyStrings (normalCharSnippet <|> escapedChar)) |>> fun x -> System.String.Concat([|cs; x; cs|])
    let brace =
        let normalCharSnippet = many1Satisfy ((<>) '}')
        let escapedChar = pstring "}}"
        between (pchar '{') (pchar '}')
                (manyStrings (normalCharSnippet <|> escapedChar)) |>> fun x -> System.String.Concat([|"{"; x; "}"|])
    let p =
        many1Satisfy (fun c -> c <> '\n' && c <> ''' && c <> '"' && c <> '{')
        <|> stringLiteral2 '"'
        <|> stringLiteral2 '''
        <|> brace
    appendToken TokenType.Comment
        (pchar '!' >>. manyStrings p |>> Statement.Comment)

let psign = char_ws ':' >>. manySatisfy ((<>) '\n') |>> Label
let pexit : _ Parser =
    appendToken TokenType.KeywordControl
        (pstring "exit" .>>? notFollowedVarCont)
    >>% Exit

let pendKeyword : _ Parser =
    appendToken TokenType.KeywordControl
        (pstring "end" .>>? notFollowedVarCont)

let pstmt =
    let pstmt, pstmtRef = createParserForwardedToRef<Statement, _>()
    let pInlineStmts =
        many (pstmt .>> ws .>> skipMany (ppunctuationTerminator .>> ws))
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
              <|> (spaces >>. pInlineStmts))
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


        let pElse =
            pipe2
                (many
                    (pipe2
                        (pelseifHeader .>>? ws .>>? skipNewline)
                        (spaces >>. pstmts)
                        (fun expr thenBody  ->
                            If(expr, thenBody, []))))
                (opt (pelseKeyword .>> spaces >>. pstmts))
                (fun elifs elseBody ->
                    let elseBody = elseBody |> Option.defaultValue []
                    let rec f = function
                        | If(expr, thenBody, [])::xs ->
                            [If(expr, thenBody, f xs)]
                        | [] -> elseBody
                        | x -> failwithf "%A" x
                    f elifs
                    // List.fold (fun st ->
                    //     function
                    //     | (If(expr, thenBody, [])) ->
                    //         [If(expr, thenBody, st)]
                    //     | x -> failwithf "%A" x)
                    //     elseBody
                )
            .>> pendKeyword

        pipe2
            pifHeader
            ((ws >>? skipNewline >>. spaces >>. pstmts .>>. pElse)
             <|> (spaces >>. pInlineStmts |>> fun x -> x, [])) // TODO: по-идеи, еще может быть `else`
            (fun expr (thenBody, elseBody) ->
                If(expr, thenBody, elseBody))
    pstmtRef :=
        choice [
            pcomment
            pexit
            psign
            pIf
            pAct
            pcallProc
            pAssign
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
        (psharpKeyword .>> ws >>. (many1Satisfy ((<>) '\n')) .>> spaces)
        (pstmts
         .>> ((pminusKeyword .>> ws) .>> (skipManySatisfy ((<>) '\n'))))
        (fun name body -> Location(name, body))

let start str =
    runParserOnString (spaces >>. many (ploc .>> spaces) .>> eof)
        emptyState
        ""
        str
