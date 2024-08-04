[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Parser.Ast.DocumentElement
open FParsec

open Qsp
open Qsp.Ast
open Qsp.Parser
open Qsp.Parser.Ast
open Qsp.Parser.Generic

module Parser =
    let pLocationElement : _ Parser =
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
        .>> optional newline

    let pCommentLineElement : _ Parser =
        choice [
            newlineReturn "" <?> "empty comment line"
            appendToken Tokens.TokenType.Comment
                (many1Satisfy2L ((<>) '#') ((<>) '\n') "comment line" .>> optional newline)
        ]

    let parser : DocumentElement Parser =
        pLocationElement |>> DocumentElement.Location
        <|> (pCommentLineElement |>> DocumentElement.CommentLine)
