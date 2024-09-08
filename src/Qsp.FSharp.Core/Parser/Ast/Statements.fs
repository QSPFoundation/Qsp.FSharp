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
            updateScope Scope.ScopeSystem.pushEmptyScope
            >>. sepEndBy (pstmt .>> ws) skipSeparators1
            .>> updateScope Scope.ScopeSystem.popScope

        let pInlineStmts1 pstmt =
            updateScope Scope.ScopeSystem.pushEmptyScope
            >>? sepEndBy1 (pstmt .>> ws) skipSeparators1
            .>> updateScope Scope.ScopeSystem.popScope

        let pstmts pstmt =
            updateScope Scope.ScopeSystem.pushEmptyScope
            >>. many (
                pstmt .>> spaces
                .>> (skipMany (ppunctuationTerminator .>> spaces))
            )
            .>> updateScope Scope.ScopeSystem.popScope

        let pstmts1 pstmt =
            updateScope Scope.ScopeSystem.pushEmptyScope
            >>? many1 (
                pstmt .>> spaces
                .>> (skipMany (ppunctuationTerminator .>> spaces))
            )
            .>> updateScope Scope.ScopeSystem.popScope

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
