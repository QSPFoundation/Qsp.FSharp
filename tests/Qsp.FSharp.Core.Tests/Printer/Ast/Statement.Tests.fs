module Qsp.Printer.Ast.Statement.Tests
open Fuchu
open FsharpMyExtension

open Qsp.Ast
open Qsp.Printer
open Qsp.Printer.Ast

[<Tests>]
let showAssignTest =
    let showAssign str =
        let emptyPos = NoEqualityPosition Position.empty
        Statement.Printer.showStmt
            (IndentsOption.UsingSpaces 4)
            FormatConfig.Default
            (emptyPos, str)
        |> ShowList.joinEmpty "\n"
        |> ShowList.show

    testList "showAssignTest" [
        testCase "implicit assign explicit two demention array" <| fun () ->
            let act =
                Assign
                   (false, AssignArr ((NumericType, "x"), [ Var (NumericType, "firstKeyExpr"); Var (NumericType, "secondKeyExpr") ]),
                    Val (Int 42))
                |> showAssign
            let exp =
                "x[firstKeyExpr, secondKeyExpr] = 42"
            Assert.Equal("", exp, act)

        testCase "implicit assign explicit three dimensions array" <| fun () ->
            let act =
                Assign
                   (false, AssignArr ((NumericType, "x"), [ Var (NumericType, "firstKeyExpr"); Var (NumericType, "secondKeyExpr"); Var (StringType, "threeKeyExpr") ]),
                    Val (Int 42))
                |> showAssign
            let exp =
                "x[firstKeyExpr, secondKeyExpr, $threeKeyExpr] = 42"
            Assert.Equal("", exp, act)
    ]
