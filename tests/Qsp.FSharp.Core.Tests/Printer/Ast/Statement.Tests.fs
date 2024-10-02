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
        |> ShowList.joinEmpty System.Environment.NewLine
        |> ShowList.show

    testList "showAssignTest" [
        testCase "implicit assign explicit two demention array" <| fun () ->
            let act =
                Assign (
                    false,
                    [
                        AssignArr (
                            (NumericType, "x"),
                            [
                                Var (NumericType, "firstKeyExpr")
                                Var (NumericType, "secondKeyExpr")
                            ]
                        )
                    ],
                    Val (Int 42)
                )
                |> showAssign
            let exp =
                "x[firstKeyExpr, secondKeyExpr] = 42"
            Assert.Equal("", exp, act)

        testCase "implicit assign explicit three dimensions array" <| fun () ->
            let act =
                Assign (
                    false,
                    [
                        AssignArr (
                            (NumericType, "x"),
                            [
                                Var (NumericType, "firstKeyExpr")
                                Var (NumericType, "secondKeyExpr")
                                Var (StringType, "threeKeyExpr")
                            ]
                        )
                    ],
                    Val (Int 42)
                )
                |> showAssign
            let exp =
                "x[firstKeyExpr, secondKeyExpr, $threeKeyExpr] = 42"
            Assert.Equal("", exp, act)

        testCase """$code = {\n    foo = 42\n}""" <| fun () ->
            Assert.Equal(
                "",
                String.concat System.Environment.NewLine [
                    "$code = {"
                    "    foo = 42"
                    "}"
                ],
                showAssign (
                    AssignCode (
                        AssignVar (StringType, "code"), [
                            (
                                NoEqualityPosition Position.empty,
                                Assign (false, [AssignVar (NumericType, "foo")], Val (Int 42))
                            )
                        ]
                    )
                )
            )

        testCase """$code = {\n    itemsCounts = 10\n    $name = "foo"\n}""" <| fun () ->
            Assert.Equal(
                "",
                String.concat System.Environment.NewLine [
                    "$code = {"
                    "    itemsCounts = 10"
                    "    $name = 'foo'"
                    "}"
                ],
                showAssign (
                    AssignCode (
                        AssignVar (StringType, "code"), [
                            (
                                NoEqualityPosition Position.empty,
                                Assign (false, [AssignVar (NumericType, "itemsCounts")], Val (Int 10))
                            )
                            (
                                NoEqualityPosition Position.empty,
                                Assign (false, [AssignVar (StringType, "name")], Val (String [[StringKind "foo"]]))
                            )
                    ])
                )
            )

        testCase "num, $str, arrNum[0], $arrStr['some'] = $tuple" <| fun () ->
            Assert.Equal(
                "",
                "num, $str, arrNum[0], $arrStr['key'] = $tuple",
                Assign (
                    false,
                    [
                        AssignWhat.AssignVar (VarType.NumericType, "num")
                        AssignWhat.AssignVar (VarType.StringType, "str")
                        AssignArr (
                            (NumericType, "arrNum"),
                            [
                                Expr.Val (Value.Int 0)
                            ]
                        )
                        AssignArr (
                            (VarType.StringType, "arrStr"),
                            [
                                Expr.Val (Value.String [[LineKind.StringKind "key"]])
                            ]
                        )
                    ],
                    Expr.Var (VarType.StringType, "tuple")
                )
                |> showAssign
            )

        testCase "local num, $str, arrNum[0], $arrStr['some'] = $tuple" <| fun () ->
            Assert.Equal(
                "",
                "local num, $str, arrNum[0], $arrStr['key'] = $tuple",
                Assign (
                    true,
                    [
                        AssignWhat.AssignVar (VarType.NumericType, "num")
                        AssignWhat.AssignVar (VarType.StringType, "str")
                        AssignArr (
                            (NumericType, "arrNum"),
                            [
                                Expr.Val (Value.Int 0)
                            ]
                        )
                        AssignArr (
                            (VarType.StringType, "arrStr"),
                            [
                                Expr.Val (Value.String [[LineKind.StringKind "key"]])
                            ]
                        )
                    ],
                    Expr.Var (VarType.StringType, "tuple")
                )
                |> showAssign
            )
    ]
