module Qsp.Parser.Main
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either
open Qsp

open Qsp.Ast
open Qsp.Parser.Generic
open Qsp.Parser.Expr

module Statements =
    module Parser =
        module Intermediate =
            let pInlineStmts pstmt =
                updateScope (fun scopeSystem ->
                    { scopeSystem with
                        Scopes = Scope.appendScope scopeSystem.Scopes
                    }
                )
                >>. many (pstmt .>> ws .>> skipMany (ppunctuationTerminator .>> ws))
                .>> updateScope (fun scopeSystem ->
                    { scopeSystem with
                        Scopes = Scope.removeScope scopeSystem.Scopes
                    }
                )

            let pInlineStmts1 pstmt =
                updateScope (fun scopeSystem ->
                    { scopeSystem with
                        Scopes = Scope.appendScope scopeSystem.Scopes
                    }
                )
                >>? many1 (pstmt .>> ws .>> skipMany (ppunctuationTerminator .>> ws))
                .>> updateScope (fun scopeSystem ->
                    { scopeSystem with
                        Scopes = Scope.removeScope scopeSystem.Scopes
                    }
                )

            let pstmts pstmt =
                updateScope (fun scopeSystem ->
                    { scopeSystem with
                        Scopes = Scope.appendScope scopeSystem.Scopes
                    }
                )
                >>. many (
                    pstmt .>> spaces
                    .>> (skipMany (ppunctuationTerminator .>> spaces))
                )
                .>> updateScope (fun scopeSystem ->
                    { scopeSystem with
                        Scopes = Scope.removeScope scopeSystem.Scopes
                    }
                )

            let pstmts1 pstmt =
                updateScope (fun scopeSystem ->
                    { scopeSystem with
                        Scopes = Scope.appendScope scopeSystem.Scopes
                    }
                )
                >>? many1 (
                    pstmt .>> spaces
                    .>> (skipMany (ppunctuationTerminator .>> spaces))
                )
                .>> updateScope (fun scopeSystem ->
                    { scopeSystem with
                        Scopes = Scope.removeScope scopeSystem.Scopes
                    }
                )

            let pstmt =
                let pstmt, pstmtRef = createParserForwardedToRef<PosStatement, _>()
                pstmtRef.Value <-
                    Statement.Parser.pstmt
                        (pInlineStmts pstmt)
                        (pInlineStmts1 pstmt)
                        (pstmts pstmt)
                pstmt

        let pstmts : Parser<Statements> =
            Intermediate.pstmts Intermediate.pstmt

        let pstmts1 : Parser<Statements> =
            Intermediate.pstmts1 Intermediate.pstmt

module Location =
    module Parser =
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
                (many (Statements.Parser.pstmts1 .>> many (pendKeyword .>> spaces)) |>> List.concat
                .>> (pminusKeyword .>> ws
                    .>> appendToken Tokens.TokenType.Comment
                            (skipManySatisfy ((<>) '\n'))))
                (fun name body -> Location(name, body))

module Document =
    module Parser =
        let pdocument =
            let ploc =
                updateScope (fun ss ->
                    { ss with
                        Scopes = Scope.appendScope ss.Scopes
                    }
                    |> Scope.addAsWrite ("args", id)
                    |> snd
                    |> Scope.addAsWrite ("result", id)
                    |> snd
                )
                >>? Location.Parser.ploc
                .>> updateScope (fun ss ->
                    { ss with
                        Scopes = Scope.removeScope ss.Scopes
                    }
                )
            spaces >>. many (ploc .>> spaces)
            .>> (getPosition >>= fun p ->
                    updateUserState (fun st ->
                        { st with LastSymbolPos = p }))

    let emptyState =
        { State.empty with PStmts = Statements.Parser.pstmts }

    let start str =
        runParserOnString (Parser.pdocument .>> eof)
            emptyState
            ""
            str
    let startOnFile enc path =
        runParserOnFile (Parser.pdocument .>> eof)
            emptyState
            path
            enc
