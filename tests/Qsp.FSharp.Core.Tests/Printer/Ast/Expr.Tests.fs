module Qsp.Printer.Ast.Expr.Tests
open Fuchu
open FsharpMyExtension

open Qsp.Ast
open Qsp.Printer.Ast

[<Tests>]
let exprShowTests =
    let show expr =
        Expr.Printer.showExpr (fun _ -> ShowList.empty) expr
        |> ShowList.show

    testList "exprShowTests" [
        testCase "tuple (a, b + c)" <| fun () ->
            Assert.Equal(
                "",
                "(a, b + c)",
                Tuple [
                    Var (NumericType, "a")
                    Expr (
                        Plus,
                        Var (NumericType, "b"),
                        Var (NumericType, "c")
                    )
                ]
                |> show
            )
        testCase "tuple (a + b, c, 'e')" <| fun () ->
            Assert.Equal(
                "",
                "(a + b, c, 'e')",
                Tuple [
                    Expr (
                        Plus,
                        Var (NumericType, "a"),
                        Var (NumericType, "b")
                    )
                    Var (NumericType, "c")
                    Val (Value.String [[ LineKind.StringKind "e" ]])
                ]
                |> show
            )
    ]
