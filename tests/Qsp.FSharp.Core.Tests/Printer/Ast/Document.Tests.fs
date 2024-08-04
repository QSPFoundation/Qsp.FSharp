module Qsp.Printer.Ast.Document.Tests
open Fuchu
open FsharpMyExtension
open FsharpMyExtension

open Qsp.Ast
open Qsp.Printer
open Qsp.Printer.Ast

[<Tests>]
let ``Document.Printer.show`` =
    let show =
        Document.Printer.show
            (IndentsOption.UsingSpaces 2)
            FormatConfig.Default
        >> ShowList.joinsEmpty ShowList.nl
        >> ShowList.show

    testList "Document.Printer.show" [
        testCase "1" <| fun () ->
            Assert.Equal (
                "",
                String.concat System.Environment.NewLine [
                    ""
                    "# begin"
                    "'Hello, world!'"
                    "--- begin ----------"
                    "multiline"
                    "comment"
                    "# location"
                    "--- location ----------"
                    ""
                    "# location2"
                    "--- location2 ----------"
                ],
                show [
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
                ]
            )
    ]
