[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Ast.Document
open FParsec

open Qsp.Parser
open Qsp.Ast
open Qsp.Parser.Generic

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