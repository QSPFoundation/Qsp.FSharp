[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Parser.Ast.Document
open FParsec

open Qsp.Ast
open Qsp.Parser
open Qsp.Parser.Ast
open Qsp.Parser.Generic

module Parser =
    let parser : Document Parser =
        many DocumentElement.Parser.parser
        .>> (
            getPosition >>= fun p ->
                updateUserState (fun st ->
                    { st with LastSymbolPos = p })
        )

let start str =
    runParserOnString (Parser.parser .>> eof)
        State.empty
        ""
        str

let startOnFile enc path =
    runParserOnFile (Parser.parser .>> eof)
        State.empty
        path
        enc
