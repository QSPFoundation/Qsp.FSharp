module Qsp.Printer.Ast.DocumentElement.Tests
open Fuchu
open FsharpMyExtension

open Qsp.Ast
open Qsp.Printer
open Qsp.Printer.Ast

[<Tests>]
let ``DocumentElement.Printer.showLocation`` =
    let show =
        DocumentElement.Printer.showLocation
            (IndentsOption.UsingSpaces 2)
            FormatConfig.Default
        >> ShowList.show

    testList "DocumentElement.Printer.showLocation" [
        testCase "base" <| fun () ->
            Assert.Equal(
                "",
                String.concat System.Environment.NewLine [
                    "# begin"
                    "'Hello, world!'"
                    "--- begin ----------"
                ],
                show (
                    Location (
                        "begin", [
                            NoEqualityPosition(Position.empty),
                            Proc ("*pl", [Val (String [[StringKind "Hello, world!"]])])
                        ]
                    )
                )
            )
    ]
