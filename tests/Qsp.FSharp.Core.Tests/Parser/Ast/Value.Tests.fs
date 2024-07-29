module Qsp.Parser.Ast.Value.Tests
open Fuchu
open FParsec
open FsharpMyExtension.Either

open Qsp
open Qsp.Defines
open Qsp.Ast
open Qsp.Printer.Ast
open Qsp.Parser.Generic
open Qsp.Parser.Ast

[<Tests>]
let stringLiteralWithTokenTest =
    let runEither str =
        runStateEither
            (Value.Parser.stringLiteralWithToken (Expr.Parser.pexpr Statements.Parser.pstmts) Statements.Parser.pstmts)
            State.empty
            str
        |> snd

    let f str =
        [[StringKind str]]

    testList "stringLiteralWithTokenTest" [
        testCase "1" <| fun () ->
            Assert.Equal("", Right (f " "), runEither "\" \"")
        testCase "2" <| fun () ->
            Assert.Equal("", Right (f "\""), runEither "\"\"\"\"")
        testCase "3" <| fun () ->
            Assert.Equal("", Right (f "\"'\""), runEither "\"\"\"'\"\"\"")
        testCase "5" <| fun () ->
            Assert.Equal("", Right [[]], runEither "''")
        testCase "6" <| fun () ->
            Assert.Equal("", Right (f "'"), runEither "''''")
        testCase "4" <| fun () ->
            Assert.Equal("", Right (f "\""), runEither "'\"'")
        testCase "multiline `'` test" <| fun () ->
            let input =
                [
                    "'"
                    "    a"
                    "'"
                ] |> String.concat "\n"
            let exp =
                [
                    []
                    [ StringKind "    a"]
                    []
                ]
            Assert.Equal("", Right exp, runEither input)
        testCase "multiline `'` test2" <| fun () ->
            let input =
                [
                    "'"
                    "    a"
                    ""
                    "b"
                    "'"
                ] |> String.concat "\n"
            let exp =
                [
                    []
                    [ StringKind "    a" ]
                    []
                    [ StringKind "b" ]
                    []
                ]
            Assert.Equal("", Right exp, runEither input)
        testCase "test '<<''x''>>'" <| fun () ->
            let input = "'<<''x''>>'"
            let exp = [[ExprKind (Val (String [[StringKind "x"]]))]]
            Assert.Equal("", Right exp, runEither input)
        testCase "test '<<''<<''''x''''>>''>>'" <| fun () ->
            let input = "'<<''<<''''x''''>>''>>'"
            let exp = [[ExprKind (Val (String [[ExprKind (Val (String [[StringKind "x"]]))]]))]]
            Assert.Equal("", Right exp, runEither input)
        testCase "test '<<''<<''''<<''''''''x''''''''>>''''>>''>>'" <| fun () ->
            let input = "'<<''<<''''<<''''''''x''''''''>>''''>>''>>'"
            let exp =
              [[ExprKind
                  (Val
                     (String
                        [[ExprKind
                            (Val (String [[ExprKind (Val (String [[StringKind "x"]]))]]))]]))]]
            Assert.Equal("", Right exp, runEither input)
        testCase "test \"<<'x'>>\"" <| fun () ->
            let input = "\"<<'x'>>\""
            let exp = [[ExprKind (Val (String [[StringKind "x"]]))]]
            Assert.Equal("", Right exp, runEither input)

        testCase "test '<a href=\"exec:GT ''changes''\">changes</a>'" <| fun () ->
            let input = "'<a href=\"exec:GT ''changes''\">changes</a>'"
            let exp : Line list =
                [[
                    HyperLinkKind (
                        StaticStmts [
                            (
                                NoEqualityPosition Position.empty,
                                Proc ("GT", [Val (String [[StringKind "changes"]])])
                            )
                        ],
                        [[StringKind "changes"]]
                   )
                ]]
            Assert.Equal("", Right exp, runEither input)
        testCase "test '<a href=\"exec: ''<<''x''>>''\">action</a>'" <| fun () ->
            let input = "'<a href=\"exec: ''<<''x''>>''\">action</a>'"
            let exp =
                [[HyperLinkKind (Raw " '<<'x'>>'", [[StringKind "action"]])]]
            Assert.Equal("", Right exp, runEither input)
    ]
