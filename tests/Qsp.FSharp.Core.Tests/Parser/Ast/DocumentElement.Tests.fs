module Qsp.Parser.Ast.DocumentElement.Tests
open Fuchu
open FsharpMyExtension.Either

open Qsp.Ast
open Qsp.Tokens
open Qsp.Parser.Generic
open Qsp.Parser.Ast

[<Tests>]
let ``DocumentElement.Parser.pCommentLineElement`` =
    let parse =
        runStateEither DocumentElement.Parser.pCommentLineElement State.empty
        >> fun (state, result) -> state.Tokens, result

    testList "DocumentElement.Parser.pCommentLineElement" [
        testCase "\n" <| fun () ->
            Assert.Equal (
                "",
                ([], Right ""),
                parse "\n"
            )

        testCase "#" <| fun () ->
            Assert.Equal (
                "",
                (
                    [],
                    Left (
                        String.concat System.Environment.NewLine [
                            "Error in Ln: 1 Col: 1"
                            "#"
                            "^"
                            "Expecting: comment line or empty comment line"
                            ""
                        ]
                    )
                ),
                parse "#"
            )

        testCase "line" <| fun () ->
            Assert.Equal (
                "",
                (
                    [
                        {
                            TokenType = TokenType.Comment
                            Range = {
                                Line = 1L
                                Column1 = 1L
                                Column2 = 5L
                            }
                        }
                    ],
                    Right "line"
                ),
                parse "line"
            )

        testCase "multiline\ncomment" <| fun () ->
            Assert.Equal (
                "",
                (
                    [
                        {
                            TokenType = TokenType.Comment
                            Range = {
                                Line = 1L
                                Column1 = 1L
                                Column2 = 10L
                            }
                        }
                    ],
                    Right "multiline"
                ),
                "multiline\ncomment" |> parse
            )
    ]

[<Tests>]
let ``DocumentElement.Parser.pLocationElement`` =
    let parse =
        runStateEither DocumentElement.Parser.pLocationElement State.empty
        >> fun (_, result) -> result

    testList "DocumentElement.Parser.pLocationElement" [
        testCase "location with comment at the end" <| fun () ->
            Assert.Equal (
                "",
                Right (
                    Location ("location", [])
                ),
                parse (
                    String.concat System.Environment.NewLine [
                        "# location"
                        "--- comment"
                    ]
                )
            )
    ]