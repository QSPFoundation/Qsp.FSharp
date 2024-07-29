module Qsp.Parser.Ast.Expr.Tests
open Fuchu
open FParsec
open FsharpMyExtension.Either

open Qsp
open Qsp.Ast
open Qsp.Printer.Ast
open Qsp.Parser.Generic
open Qsp.Parser.Ast

[<Tests>]
let pexprTest =
    let runExpr str =
        runStateEither (Expr.Parser.pexpr pzero) Qsp.Parser.Generic.State.empty str
        |> snd
    let sprintExpr =
        Expr.Printer.simpleShowExpr (failwithf "showStmtsInline not implemented %A")
        >> FsharpMyExtension.ShowList.show
    let runExprShow str =
        runExpr str
        |> Either.map sprintExpr
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
        testCase "строчные бинарные операторы и названия переменных, которые начинаются с них" <| fun () ->
            let input = "notFollowedBy" // Уж точно не должно быть "no tFollowedBy"
            let exp =
                Var (NumericType, "notFollowedBy")
            Assert.Equal("", Right exp, runExpr input)
        testCase "строчные бинарные операторы и названия переменных, которые начинаются с них" <| fun () ->
            let input = "object"
            let exp =
                Var (NumericType, "object")
            Assert.Equal("", Right exp, runExpr input)
        testCase "строчные бинарные операторы и названия переменных, которые начинаются с них" <| fun () ->
            let input = "obj something"
            let exp =
                UnarExpr (Obj, Var (NumericType, "something"))
            Assert.Equal("", Right exp, runExpr input)

        let input = "var1 and var2 and no var3 and obj var4"
        let exp =
            Expr
              (And,
               Expr
                 (And,
                  Expr
                    (And, Var (NumericType, "var1"),
                     Var (NumericType, "var2")),
                  UnarExpr (No, Var (NumericType, "var3"))),
               UnarExpr (Obj, Var (NumericType, "var4")))
        testf input exp

        testCase "2" <| fun () ->
            let input = "var1[var1 + var2] and func(arg1, arg2[expr], x + y)"
            let exp = "var1[var1 + var2] and func(arg1, arg2[expr], x + y)"
            Assert.Equal("", Right exp, runExprShow input)
        testCase "3" <| fun () ->
            let input = "a = 10 or b = 20 and c = 30"
            let exp = "(a = 10) or ((b = 20) and (c = 30))"
            Assert.Equal("", Right exp, runExprShow input)
        testCase "4" <| fun () ->
            let input = "a = pstam> (pmaxstam/4)*2 and pstam <= (pmaxstam/4)*3"
            let exp = "((a = pstam) > ((pmaxstam / 4) * 2)) and (pstam <= ((pmaxstam / 4) * 3))"
            Assert.Equal("", Right exp, runExprShow input)
        testCase "no obj 'apple'" <| fun () ->
            let input = "no obj 'apple'"
            let exp =
                UnarExpr (No, UnarExpr (Obj, Val (String [[StringKind "apple"]])))
            Assert.Equal("", Right exp, runExpr input)

        let input = "- x"
        let exp =
            UnarExpr (Neg, Var (NumericType, "x"))
        testf input exp

        let input = "-x + -y"
        let exp =
            Expr
              (Plus, UnarExpr (Neg, Var (NumericType, "x")),
               UnarExpr (Neg, Var (NumericType, "y")))
        testf input exp

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
            "obj 'яблоко' = 0"
            (UnarExpr (
                Obj,
                Expr (
                    Eq,
                    Val (String [[StringKind "яблоко"]]),
                    Val (Int 0)
                )
            ))

        let input =
            [
                "x + _"
                "         "
                "         _"
                "    _"
                ""
                "    z + y"
            ] |> String.concat "\n"
        let exp =
            Expr
              (Plus,
               Expr (Plus, Var (NumericType, "x"), Var (NumericType, "z")),
               Var (NumericType, "y"))
        testf input exp

        let input =
            "input 'How do you do?'"
        let exp =
            Func (Predef Defines.Input, [Val (String [[StringKind "How do you do?"]])])
        testf input exp

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
