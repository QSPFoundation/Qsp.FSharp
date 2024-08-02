module Qsp.Parser.Ast.Statements.Tests
open Fuchu
open FParsec
open FsharpMyExtension.Either

open Qsp.Ast
open Qsp.Parser.Generic
open Qsp.Parser.Ast

let pInlineStmts str =
    runStateEither
        (Statements.Parser.Intermediate.pInlineStmts Statements.Parser.Intermediate.pstmt .>> eof)
        State.empty str
    |> snd

[<Tests>]
let inlineStmtsTests =
    testList "inlineStmtsTests" [
        testCase "proc 1 2" <| fun () ->
            Assert.Equal (
                "",
                Left (
                    [
                        "Error in Ln: 1 Col: 8"
                        "proc 1 2"
                        "       ^"
                        "Expecting: end of input"
                        ""
                    ] |> String.concat System.Environment.NewLine
                ),
                pInlineStmts "proc 1 2"
            )

        testCase "proc 1 & 2" <| fun () ->
            Assert.Equal (
                "",
                Right [
                    NoEqualityPosition(Position.empty), Proc ("proc", [Val (Int 1)])
                    NoEqualityPosition(Position.empty), Proc ("*pl", [Val (Int 2)])
                ],
                pInlineStmts "proc 1 & 2"
            )

        testCase "proc 1 & && 2" <| fun () ->
            Assert.Equal (
                "",
                Right [
                    NoEqualityPosition(Position.empty), Proc ("proc", [Val (Int 1)])
                    NoEqualityPosition(Position.empty), Proc ("*pl", [Val (Int 2)])
                ],
                pInlineStmts "proc 1 & && 2"
            )
    ]
