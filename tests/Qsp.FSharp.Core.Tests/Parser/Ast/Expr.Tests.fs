module Qsp.Parser.Ast.Expr.Tests
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
let pexprTest =
    let runExpr str =
        runStateEither (Expr.Parser.pexpr pzero) State.empty str
        |> snd

    let sprintExpr =
        Expr.Printer.simpleShowExpr (failwithf "showStmtsInline not implemented %A")
        >> FsharpMyExtension.ShowList.show

    let equalWithShow (exp:Expr) (act:Either<_, Expr>) =
        match act with
        | Left _ ->
            failtestf "%A" act
        | Right act ->
            if exp <> act then
                failtestf "Expected:\n%A\n\"%s\"\n\nActual:\n%A\n\"%s\"" exp (sprintExpr exp) act (sprintExpr act)

    let testf input exp =
        testCase input <| fun () ->
            equalWithShow exp (runExpr input)

    testList "pexpr test" [
        testf
            "object" // test for prevent `obj ect`
            (Var (NumericType, "object"))

        testf
            "obj something"
            (UnarExpr (
                Obj, Var (NumericType, "something")
            ))

        testf
            "obj 'яблоко' = 0"
            (UnarExpr (
                Obj,
                Expr (
                    Eq,
                    Val (String [[StringKind "яблоко"]]),
                    Val (Int 0)
                )
            ))

        testf
            "notFollowedBy" // test for prevent `no tFollowedBy`
            (Var (NumericType, "notFollowedBy"))

        testf
            "no obj 'apple'"
            (UnarExpr (
                No,
                UnarExpr (
                    Obj,
                    Val (String [[StringKind "apple"]])
                )
            ))

        testf
            "var1 and var2 and no var3 and obj var4"
            (Expr (
                And,
                Expr (
                    And,
                    Expr (
                        And,
                        Var (NumericType, "var1"),
                        Var (NumericType, "var2")
                    ),
                    UnarExpr (No, Var (NumericType, "var3"))
                ),
                UnarExpr (Obj, Var (NumericType, "var4"))
            ))

        testf
            "input 'How do you do?'"
            (Func (
                Predef Defines.Input, [Val (String [[StringKind "How do you do?"]])]
            ))

        testf
            "- x"
            (UnarExpr (Neg, Var (NumericType, "x")))

        testf
            "-x + -y"
            (Expr (
                Plus,
                UnarExpr (Neg, Var (NumericType, "x")),
                UnarExpr (Neg, Var (NumericType, "y"))
            ))

        testf
            "-1 - -1"
            (Expr (
                Minus,
                UnarExpr (Neg, Val (Int 1)),
                UnarExpr (Neg, Val (Int 1))
            ))

        testf
            "-(a + b)"
            (UnarExpr (
                Neg,
                Expr (
                    Plus, Var (NumericType, "a"), Var (NumericType, "b")
                )
            ))

        testf
            "var1[var1 + var2] and func(arg1, arg2[expr], x + y)"
            (Expr (
                And,
                Arr (
                    (NumericType, "var1"),
                    [
                        Expr (
                            Plus,
                            Var (NumericType, "var1"),
                            Var (NumericType, "var2")
                        )
                    ]
                ),
                Func (
                    Predef PredefFunc.Func,
                    [
                        Var (NumericType, "arg1")
                        Arr ((NumericType, "arg2"), [Var (NumericType, "expr")])
                        Expr (Plus, Var (NumericType, "x"), Var (NumericType, "y"))
                    ]
                )
            ))

        testf
            "a = 10 or b = 20 and c = 30"
            (Expr (
                Or,
                Expr (Eq, Var (NumericType, "a"), Val (Int 10)),
                Expr (
                    And,
                    Expr (Eq, Var (NumericType, "b"), Val (Int 20)),
                    Expr (Eq, Var (NumericType, "c"), Val (Int 30))
                )
            ))

        testf
            "a = pstam > (pmaxstam / 4) * 2 and pstam <= (pmaxstam / 4) * 3"
            (Expr (
                And,
                Expr (
                    Gt,
                    Expr (Eq, Var (NumericType, "a"), Var (NumericType, "pstam")),
                    Expr (
                        Times,
                        Expr (Divide, Var (NumericType, "pmaxstam"), Val (Int 4)),
                        Val (Int 2)
                    )
                ),
                Expr (
                    Le,
                    Var (NumericType, "pstam"),
                    Expr (
                        Times,
                        Expr (Divide, Var (NumericType, "pmaxstam"), Val (Int 4)),
                        Val (Int 3)
                    )
                )
            ))

        testf
            "1 + 2 * 3"
            (Expr (
                Plus,
                Val (Int 1),
                Expr (Times, Val (Int 2), Val (Int 3))
            ))

        testf
            "1 + 2 mod 3 * 4"
            (Expr (
                Plus,
                Val (Int 1),
                Expr (
                    Mod,
                    Val (Int 2),
                    Expr (Times, Val (Int 3), Val (Int 4))
                )
            ))

        testf
            "a and b = c and d"
            (Expr (
                And,
                Expr (
                    And,
                    Var (NumericType, "a"),
                    Expr (Eq, Var (NumericType, "b"), Var (NumericType, "c"))
                ),
                Var (NumericType, "d")
            ))

        testf
            (
                [
                    "x + _"
                    "         "
                    "         _"
                    "    _"
                    ""
                    "    z + y"
                ] |> String.concat "\n"
            )
            (Expr (
                Plus,
                Expr (
                    Plus,
                    Var (NumericType, "x"),
                    Var (NumericType, "z")
                ),
                Var (NumericType, "y")
            ))

        testCase "()" <| fun () ->
            Assert.Equal(
                "",
                Right (Tuple []),
                runExpr "()"
            )

        testCase "(1 + 2)" <| fun () ->
            Assert.Equal(
                "",
                Right (
                    Expr (
                        Plus,
                        Val (Int 1),
                        Val (Int 2)
                    )
                ),
                runExpr "(1 + 2)"
            )

        testCase "(1, 2)" <| fun () ->
            Assert.Equal(
                "",
                Right (
                    Tuple [
                        Val (Int 1)
                        Val (Int 2)
                    ]
                ),
                runExpr "(1, 2)"
            )

        testCase "(1, 2, 3)" <| fun () ->
            Assert.Equal(
                "",
                Right (
                    Tuple [
                        Val (Int 1)
                        Val (Int 2)
                        Val (Int 3)
                    ]
                ),
                runExpr "(1, 2, 3)"
            )

        testCase "(1, (2, 3))" <| fun () ->
            Assert.Equal(
                "",
                Right (
                    Tuple [
                        Val (Int 1)
                        Tuple [
                            Val (Int 2)
                            Val (Int 3)
                        ]
                    ]
                ),
                runExpr "(1, (2, 3))"
            )

        testCase "(1, (2 + 3), x[4]) + x[5]" <| fun () ->
            Assert.Equal(
                "",
                Right (
                    Expr (
                        Plus,
                        Tuple [
                            Val (Int 1)
                            Expr (
                                Plus,
                                Val (Int 2),
                                Val (Int 3)
                            )
                            Arr(
                                (NumericType, "x"),
                                [ Val (Int 4) ]
                            )
                        ],
                        Arr(
                            (NumericType, "x"),
                            [ Val (Int 5) ]
                        )
                    )
                ),
                runExpr "(1, (2 + 3), x[4]) + x[5]"
            )
    ]
