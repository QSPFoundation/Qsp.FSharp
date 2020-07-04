open FsharpMyExtension
open FsharpMyExtension
open FsharpMyExtension.Either
open FParsec
#if INTERACTIVE
#load @"..\QSParse\Tokens.fs"
#load @"..\QSParse\Ast.fs"
#load @"..\QSParse\Defines.fs"
#load @"..\QSParse\Show.fs"
#load @"..\QSParse\ParserGeneric.fs"
#load @"..\QSParse\ParserExpr.fs"
#load @"..\QSParse\Parsec.fs"
#endif
open Qsp.Ast
open Qsp.Parser.Generic
open Qsp.Parser.Expr


open Fuchu
[<Tests>]
let pexprTest =
    let runExpr str =
        runStateEither pexpr Qsp.Parser.Generic.emptyState str
        |> snd
    let runExprShow str =
        runExpr str
        |> Either.map
            (Qsp.Show.simpleShowExpr
             >> FsharpMyExtension.ShowList.show)
    testList "pexpr test" [
        testCase "строчные бинарные операторы и названия переменных, которые начинаются с них" <| fun () ->
            let input = "notFollowedBy" // Уж точно не должно быть "no tFollowedBy"
            let exp =
                Var (ImplicitNumericType, "notFollowedBy")
            Assert.Equal("", Right exp, runExpr input)
        testCase "строчные бинарные операторы и названия переменных, которые начинаются с них" <| fun () ->
            let input = "object"
            let exp =
                Var (ImplicitNumericType, "object")
            Assert.Equal("", Right exp, runExpr input)
        testCase "строчные бинарные операторы и названия переменных, которые начинаются с них" <| fun () ->
            let input = "obj something"
            let exp =
                UnarExpr (Obj, Var (ImplicitNumericType, "something"))
            Assert.Equal("", Right exp, runExpr input)

        testCase "1" <| fun () ->
            let input = "var1 and var2 and no var3 and obj var4"
            let exp = "((var1 and var2) and (no var3)) and (obj var4)"

            Assert.Equal("", Right exp, runExprShow input)
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

        // testCase "3" <| fun () ->
        //     Assert.Equal("", Right "\"'", runEither stringLiteral "\"\"'\"\"")
        // testCase "5" <| fun () ->
        //     Assert.Equal("", Right "", runEither stringLiteral "''")
        // testCase "6" <| fun () ->
        //     Assert.Equal("", Right "'", runEither stringLiteral "''''")
        // testCase "4" <| fun () ->
        //     Assert.Equal("", Right "\"", runEither stringLiteral "'\"'")
        // testCase "braces1" <| fun () ->
        //     Assert.Equal("", Right "abc", runEither stringLiteral "{abc}")
        // testCase "braces escaped" <| fun () ->
        //     Assert.Equal("", Right "}", runEither stringLiteral "{}}}")
    ]
// #load "Parsec.fs"

[<Tests>]
let assignTest =
    let runExpr str =
        Qsp.Parser.Generic.runStateEither Qsp.Parser.Main.assign Qsp.Parser.Generic.emptyState str
        |> snd
    testList "assignTest" [
        testCase "implicit assign implicit var" <| fun () ->
            let input = "x = 21 + 21"
            let exp =
                (Assign
                   (AssignVar (ImplicitNumericType, "x"),
                    Expr (Plus, Val (Int 21), Val (Int 21))))
            Assert.Equal("", Right exp, runExpr input)
        testCase "implicit assign implicit array var" <| fun () ->
            let input = "x[expr] = 42"
            let exp =
                (Assign
                   (AssignArr
                      ((ImplicitNumericType, "x"), Var (ImplicitNumericType, "expr")),
                    Val (Int 42)))
            Assert.Equal("", Right exp, runExpr input)
        testCase "implicit `-=` implicit var" <| fun () ->
            let input = "php -= 3*emdmg*2 - parm"
            let exp =
                (Assign
                   (AssignVar (ImplicitNumericType, "php"),
                    Expr
                      (Minus, Var (ImplicitNumericType, "php"),
                       Expr
                         (Minus,
                          Expr
                            (Times,
                             Expr (Times, Val (Int 3), Var (ImplicitNumericType, "emdmg")),
                             Val (Int 2)), Var (ImplicitNumericType, "parm")))))
            Assert.Equal("", Right exp, runExpr input)
        testCase "5" <| fun () ->
            let input = "a = a = no -a > b"
            let exp =
                (Assign
                   (AssignVar (ImplicitNumericType, "a"),
                    Expr
                      (Eq, Var (ImplicitNumericType, "a"),
                       UnarExpr
                         (No,
                          Expr
                            (Gt, UnarExpr (Neg, Var (ImplicitNumericType, "a")),
                             Var (ImplicitNumericType, "b"))))))
            Assert.Equal("", Right exp, runExpr input)
        testCase "implicit assign explicit array var" <| fun () ->
            let input = "$x[expr] = 42"
            let exp =
                (Assign
                   (AssignArr ((StringType, "x"), Var (ImplicitNumericType, "expr")),
                    Val (Int 42)))
            Assert.Equal("", Right exp, runExpr input)
        testCase "implicit assign explicit var" <| fun () ->
            let input = "#x = 21 + 21"
            let exp =
                (Assign
                   (AssignVar (ExplicitNumericType, "x"),
                    Expr (Plus, Val (Int 21), Val (Int 21))))
            Assert.Equal("", Right exp, runExpr input)
        // ложные случаи:
        testCase "attempt assign function" <| fun () ->
            let input = "f(expr) = 42" // поскольку `=` — это одновременно и оператор присваивания и оператор равности, так что сойдет за выражение
            let exp =
                [
                    "Error in Ln: 1 Col: 1"
                    "f(expr) = 42"
                    "^"
                    "Expecting: '#', '$', 'let' (case-insensitive) or 'set' (case-insensitive)"
                    ""
                    "The parser backtracked after:"
                    "  Error in Ln: 1 Col: 2"
                    "  f(expr) = 42"
                    "   ^"
                    "  Expecting: '+=', '-=', '=', '=+', '=-' or '['"
                    ""
                ] |> String.concat "\r\n"
            Assert.Equal("", Left exp, runExpr input)
    ]


    // // ломает семантику:
    // "min = 10" // нельзя, потому что `min` — встроенная функция. Семантику ломает, дерево — нет.

    // // Ломает дерево:
    // "f(expr) += 10" // Да `var += value` — это `var = var + value`, но выражение всегда ложное


[<EntryPoint;System.STAThread>]
let main arg =
    defaultMainThisAssembly arg
