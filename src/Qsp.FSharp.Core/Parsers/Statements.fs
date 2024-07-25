[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Ast.Statements

module Parser =
    open FParsec

    open Qsp.Parser
    open Qsp.Parser.Generic

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
