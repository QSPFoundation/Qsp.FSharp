module Qsp.Parser.Ast.Document.Tests
open Fuchu
open FParsec
open FsharpMyExtension.Either

open Qsp.Ast
open Qsp.Parser.Ast

[<Tests>]
let ``Document.Parser.parser`` =
    let parse =
        Document.start
        >> function
            | Success(result, state, pos) ->
                Result.Ok result
            | Failure(errMsg, _, _) ->
                Result.Error errMsg

    testList "Document.Parser.parser" [
        testCase "base" <| fun () ->
            Assert.Equal (
                "",
                Result.Ok [
                    DocumentElement.CommentLine ""
                    DocumentElement.Location (
                        Location (
                            "begin", [
                                NoEqualityPosition(Position.empty),
                                Proc ("*pl", [Val (String [[StringKind "Hello, world!"]])])
                            ]
                        )
                    )
                    DocumentElement.CommentLine "multiline"
                    DocumentElement.CommentLine "comment"
                    DocumentElement.Location (
                        Location (
                            "location", []
                        )
                    )
                    DocumentElement.CommentLine ""
                    DocumentElement.Location (
                        Location (
                            "location2", []
                        )
                    )
                ],
                parse (
                    String.concat System.Environment.NewLine [
                        ""
                        "# begin"
                        "  'Hello, world!'"
                        "-"
                        "multiline"
                        "comment"
                        "# location"
                        ""
                        "--- location ---"
                        ""
                        "# location2"
                        "-"
                    ]
                )
            )
    ]
