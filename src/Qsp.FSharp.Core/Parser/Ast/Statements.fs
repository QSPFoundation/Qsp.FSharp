[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Parser.Ast.Statements

module Parser =
    open FParsec

    open Qsp.Ast
    open Qsp.Parser
    open Qsp.Parser.Generic
    open Qsp.Parser.Ast

    module Intermediate =
        let skipSeparators1 =
            skipMany1 (ppunctuationTerminator .>> ws)

        let pInlineStmts pstmt =
            updateScope (fun scopeSystem ->
                { scopeSystem with
                    Scopes = Scope.Scopes.push scopeSystem.Scopes
                }
            )
            >>. sepEndBy (pstmt .>> ws) skipSeparators1
            .>> updateScope (fun scopeSystem ->
                { scopeSystem with
                    Scopes = Scope.Scopes.pop scopeSystem.Scopes
                }
            )

        let pInlineStmts1 pstmt =
            updateScope (fun scopeSystem ->
                { scopeSystem with
                    Scopes = Scope.Scopes.push scopeSystem.Scopes
                }
            )
            >>? sepEndBy1 (pstmt .>> ws) skipSeparators1
            .>> updateScope (fun scopeSystem ->
                { scopeSystem with
                    Scopes = Scope.Scopes.pop scopeSystem.Scopes
                }
            )

        let pstmts pstmt =
            updateScope (fun scopeSystem ->
                { scopeSystem with
                    Scopes = Scope.Scopes.push scopeSystem.Scopes
                }
            )
            >>. many (
                pstmt .>> spaces
                .>> (skipMany (ppunctuationTerminator .>> spaces))
            )
            .>> updateScope (fun scopeSystem ->
                { scopeSystem with
                    Scopes = Scope.Scopes.pop scopeSystem.Scopes
                }
            )

        let pstmts1 pstmt =
            updateScope (fun scopeSystem ->
                { scopeSystem with
                    Scopes = Scope.Scopes.push scopeSystem.Scopes
                }
            )
            >>? many1 (
                pstmt .>> spaces
                .>> (skipMany (ppunctuationTerminator .>> spaces))
            )
            .>> updateScope (fun scopeSystem ->
                { scopeSystem with
                    Scopes = Scope.Scopes.pop scopeSystem.Scopes
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
