module Qsp.Parser.Ast.DocumentElement.Tests
open Fuchu
open FsharpMyExtension.Either

open Qsp.Ast
open Qsp.Parser.Generic
open Qsp.Parser.Ast

[<Tests>]
let ``DocumentElement.Parser.pCommentLineElement`` =
    let parse =
        runStateEither DocumentElement.Parser.pCommentLineElement State.empty
        >> fun (_, result) -> result

    testList "DocumentElement.Parser.pCommentLineElement" [
        testCase "\n" <| fun () ->
            Assert.Equal (
                "",
                Right "",
                parse "\n"
            )

        testCase "#" <| fun () ->
            Assert.Equal (
                "",
                Left (
                    String.concat System.Environment.NewLine [
                        "Error in Ln: 1 Col: 1"
                        "#"
                        "^"
                        "Expecting: comment line or empty comment line"
                        ""
                    ]
                ),
                parse "#"
            )

        testCase "multiline\ncomment" <| fun () ->
            Assert.Equal (
                "",
                Right "multiline",
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