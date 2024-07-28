[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Parser.Ast.Location

module Parser =
    open FParsec
    open FsharpMyExtension

    open Qsp
    open Qsp.Ast
    open Qsp.Parser.Ast
    open Qsp.Parser.Generic

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
