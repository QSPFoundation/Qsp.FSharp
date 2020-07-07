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
    ]
// #load "Parsec.fs"

[<Tests>]
let assignTest =
    let runExpr str =
        Qsp.Parser.Generic.runStateEither (Qsp.Parser.Main.pAssign FParsec.Primitives.pzero) Qsp.Parser.Generic.emptyState str
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
            let input = "years -= 10"
            let exp =
              (Assign
                 (AssignVar (ImplicitNumericType, "years"),
                  Expr (Minus, Var (ImplicitNumericType, "years"), Val (Int 10))))
            Assert.Equal("", Right exp, runExpr input)
        testCase "implicit `-=` implicit var 2" <| fun () ->
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
        testCase "attempt assign var without body" <| fun () ->
            let input = "justName"
            let act =
                runExpr input
                |> Option.ofEither
            Assert.None("", act)
        testCase "attempt assign var without body space" <| fun () ->
            let input = "justName "
            let act =
                runExpr input
                |> Option.ofEither
            Assert.None("", act)
        testCase "just `x[expr]`" <| fun () ->
            let input = "x[expr]"
            let act =
                runExpr input
                |> Option.ofEither
            Assert.None("", act)
    ]

[<Tests>]
let stringLiteralTest =
    testList "stringLiteralTest" [
        testCase "1" <| fun () ->
            Assert.Equal("", Right " ", runEither stringLiteral "\" \"")
        testCase "2" <| fun () ->
            Assert.Equal("", Right "\"", runEither stringLiteral "\"\"\"\"")
        testCase "3" <| fun () ->
            Assert.Equal("", Right "\"'\"", runEither stringLiteral "\"\"\"'\"\"\"")
        testCase "5" <| fun () ->
            Assert.Equal("", Right "", runEither stringLiteral "''")
        testCase "6" <| fun () ->
            Assert.Equal("", Right "'", runEither stringLiteral "''''")
        testCase "4" <| fun () ->
            Assert.Equal("", Right "\"", runEither stringLiteral "'\"'")
        testCase "braces1" <| fun () ->
            Assert.Equal("", Right "abc", runEither stringLiteral "{abc}")
        testCase "braces escaped" <| fun () ->
            Assert.Equal("", Right "}", runEither stringLiteral "{}}}")
    ]

[<Tests>]
let stringLiteralWithTokenTest =
    let runEither str =
        Qsp.Parser.Generic.runStateEither stringLiteralWithToken Qsp.Parser.Generic.emptyState str
        |> snd
    testList "stringLiteralWithTokenTest" [
        testCase "1" <| fun () ->
            Assert.Equal("", Right " ", runEither "\" \"")
        testCase "2" <| fun () ->
            Assert.Equal("", Right "\"", runEither "\"\"\"\"")
        testCase "3" <| fun () ->
            Assert.Equal("", Right "\"'\"", runEither "\"\"\"'\"\"\"")
        testCase "5" <| fun () ->
            Assert.Equal("", Right "", runEither "''")
        testCase "6" <| fun () ->
            Assert.Equal("", Right "'", runEither "''''")
        testCase "4" <| fun () ->
            Assert.Equal("", Right "\"", runEither "'\"'")
        testCase "multiline test" <| fun () ->
            let input =
                [
                    "'"
                    "    a"
                    "'"
                ] |> String.concat "\n"
            let exp =
                [
                    ""
                    "    a"
                    ""
                ] |> String.concat "\n"
            Assert.Equal("", Right exp, runEither input)
    ]
[<Tests>]
let pbracesTests =
    let runEither str =
        Qsp.Parser.Generic.runStateEither pbraces Qsp.Parser.Generic.emptyState str
        |> snd
    testList "stringLiteralWithTokenTest" [
        testCase "1" <| fun () ->
            let input =
                [
                    "{"
                    "    asdf"
                    "    {"
                    "        asdf"
                    "    }"
                    "}"
                ] |> String.concat "\n"
            let exp =
                [
                    ""
                    "    asdf"
                    "    {"
                    "        asdf"
                    "    }"
                    ""
                ] |> String.concat "\n"
            Assert.Equal("", Right exp, runEither input)
    ]


[<Tests>]
let pcallProcTests =
    let runStmts str =
        Qsp.Parser.Generic.runStateEither Qsp.Parser.Main.pcallProc Qsp.Parser.Generic.emptyState str
        |> snd
    testList "pcallProcTests" [
        testCase "pcallProcTests base" <| fun () ->
            let input = "someProc arg1"
            let exp =
                CallSt ("someProc", [Var (ImplicitNumericType, "arg1")])

            Assert.Equal("", Right exp, runStmts input)
        testCase "pcallProcTests base many args" <| fun () ->
            let input = "someProc z / 2, x + y"
            let exp =
                (CallSt
                   ("someProc",
                    [Expr (Divide, Var (ImplicitNumericType, "z"), Val (Int 2));
                     Expr
                       (Plus, Var (ImplicitNumericType, "x"), Var (ImplicitNumericType, "y"))]))
            Assert.Equal("", Right exp, runStmts input)
        testCase "pcallProcTests false with space" <| fun () ->
            let input = "someProc "
            // let exp =
            //     [
            //         "Error in Ln: 1 Col: 1"
            //         "someProc "
            //         "^"
            //         ""
            //         "The parser backtracked after:"
            //         "  Error in Ln: 1 Col: 10"
            //         "  someProc "
            //         "           ^"
            //         "  Note: The error occurred at the end of the input stream."
            //         "  Expecting: identifier, integer number (32-bit, signed), prefix operator, '\"',"
            //         "  '#', '$', '\\'', '(', '_' or '{'"
            //         ""
            //     ] |> String.concat "\r\n"
            // Assert.Equal("", Left exp, runStmts input)
            let act =
                runStmts input
                |> Option.ofEither
            Assert.None("", act)
        testCase "pcallProcTests false" <| fun () ->
            let input = "someProc"
            // let exp =
            //     [
            //         "Error in Ln: 1 Col: 1"
            //         "someProc"
            //         "^"
            //         ""
            //         "The parser backtracked after:"
            //         "  Error in Ln: 1 Col: 9"
            //         "  someProc"
            //         "          ^"
            //         "  Note: The error occurred at the end of the input stream."
            //         "  Unknown Error(s)"
            //         ""
            //     ] |> String.concat "\r\n"
            // Assert.Equal("", Left exp, runStmts input)
            let act =
                runStmts input
                |> Option.ofEither
            Assert.None("", act)
        testCase "*pl" <| fun () ->
            let input = "*pl"
            let exp = CallSt ("*pl", [])
            Assert.Equal("", Right exp, runStmts input)
        testCase "*pl arg1, arg2" <| fun () ->
            let input = "*pl arg1, arg2"
            let exp =
                (CallSt
                   ("*pl",
                    [Var (ImplicitNumericType, "arg1"); Var (ImplicitNumericType, "arg2")]))
            Assert.Equal("", Right exp, runStmts input)
    ]
[<Tests>]
let ifTests =
    let runStmts str =
        Qsp.Parser.Generic.runStateEither
            Qsp.Parser.Main.pstmt
            Qsp.Parser.Generic.emptyState str
        |> snd
    let runStmtsEof str =
        Qsp.Parser.Generic.runStateEither
            (Qsp.Parser.Main.pstmt .>> eof)
            Qsp.Parser.Generic.emptyState str
        |> snd
    testList "ifTests" [
        testCase "inline if" <| fun () ->
            let input =
                [
                    "if expr: gt 'hall'"
                    "'инструкция, которая не принадлежит конструкции'"
                ] |> String.concat "\n"
            let exp =
                (If
                   (Var (ImplicitNumericType, "expr"), [CallSt ("gt", [Val (String "hall")])],
                    []))
            Assert.Equal("", Right exp, runStmts input)
        testCase "inline if 2" <| fun () ->
            let input =
                [
                    "if expr:"
                    "    if expr2: stmt1"
                    "    if expr3:"
                    "        stmt1"
                    "    else stmt2"
                    "    if expr4: stmt3"
                    "    elseif expr5: stmt4"
                    "    stmt5"
                    "end"
                ] |> String.concat "\n"
            let exp =
                If
                  (Var (ImplicitNumericType, "expr"),
                   [If
                      (Var (ImplicitNumericType, "expr2"),
                       [StarPl (Var (ImplicitNumericType, "stmt1"))], []);
                    If
                      (Var (ImplicitNumericType, "expr3"),
                       [StarPl (Var (ImplicitNumericType, "stmt1"))],
                       [StarPl (Var (ImplicitNumericType, "stmt2"))]);
                    If
                      (Var (ImplicitNumericType, "expr4"),
                       [StarPl (Var (ImplicitNumericType, "stmt3"))],
                       [If
                          (Var (ImplicitNumericType, "expr5"),
                           [StarPl (Var (ImplicitNumericType, "stmt4"))], [])]);
                    StarPl (Var (ImplicitNumericType, "stmt5"))], [])
            Assert.Equal("", Right exp, runStmtsEof input)
        testCase "simple if" <| fun () ->
            let input =
                [
                    "if expr:"
                    "    someStmt"
                    "end"
                ] |> String.concat "\n"
            let exp =
                (If
                   (Var (ImplicitNumericType, "expr"),
                    [StarPl (Var (ImplicitNumericType, "someStmt"))], []))

            Assert.Equal("", Right exp, runStmtsEof input)
        testCase "elseif test" <| fun () ->
            let input =
                [
                    "if expr1:"
                    "    stmt1"
                    "elseif expr2:"
                    "    stmt2"
                    "elseif expr3:"
                    "    stmt3"
                    "else"
                    "    stmt4"
                    "end"
                ] |> String.concat "\n"
            let exp =
                (If
                   (Var (ImplicitNumericType, "expr1"),
                    [StarPl (Var (ImplicitNumericType, "stmt1"))],
                    [If
                       (Var (ImplicitNumericType, "expr2"),
                        [StarPl (Var (ImplicitNumericType, "stmt2"))],
                        [If
                           (Var (ImplicitNumericType, "expr3"),
                            [StarPl (Var (ImplicitNumericType, "stmt3"))],
                            [StarPl (Var (ImplicitNumericType, "stmt4"))])])]))
            Assert.Equal("", Right exp, runStmtsEof input)
        testCase "elseif test2" <| fun () ->
            let input =
                [
                    "if expr1:"
                    "    stmt1"
                    "elseif expr2:"
                    "    stmt2"
                    "elseif expr3:"
                    "    stmt3"
                    "end"
                ] |> String.concat "\n"
            let exp =
                (If
                   (Var (ImplicitNumericType, "expr1"),
                    [StarPl (Var (ImplicitNumericType, "stmt1"))],
                    [If
                       (Var (ImplicitNumericType, "expr2"),
                        [StarPl (Var (ImplicitNumericType, "stmt2"))],
                        [If
                           (Var (ImplicitNumericType, "expr3"),
                            [StarPl (Var (ImplicitNumericType, "stmt3"))], [])])]))
            Assert.Equal("", Right exp, runStmtsEof input)
        testCase "another inline if" <| fun () ->
            let input =
                [
                    "if expr:"
                    "elseif expr: stmt"
                    "else"
                    "end"
                ] |> String.concat "\n"
            let exp =
              (If
                 (Var (ImplicitNumericType, "expr"), [],
                  [If
                     (Var (ImplicitNumericType, "expr"),
                      [StarPl (Var (ImplicitNumericType, "stmt"))], [])]))
            Assert.Equal("", Right exp, runStmtsEof input)
        testCase "elseif test2" <| fun () ->
            let input =
                [
                    "if expr1:"
                    "    stmt1"
                    "elseif expr2:"
                    "    stmt2"
                    "    if expr4:"
                    "        stmt4"
                    "    elseif expr5:"
                    "        stmt5"
                    "    end"
                    "    stmt6"
                    "elseif expr3:"
                    "    stmt3"
                    "end"
                ] |> String.concat "\n"
            let exp =
              (If
                 (Var (ImplicitNumericType, "expr1"),
                  [StarPl (Var (ImplicitNumericType, "stmt1"))],
                  [If
                     (Var (ImplicitNumericType, "expr2"),
                      [StarPl (Var (ImplicitNumericType, "stmt2"));
                       If
                         (Var (ImplicitNumericType, "expr4"),
                          [StarPl (Var (ImplicitNumericType, "stmt4"))],
                          [If
                             (Var (ImplicitNumericType, "expr5"),
                              [StarPl (Var (ImplicitNumericType, "stmt5"))], [])]);
                       StarPl (Var (ImplicitNumericType, "stmt6"))],
                      [If
                         (Var (ImplicitNumericType, "expr3"),
                          [StarPl (Var (ImplicitNumericType, "stmt3"))], [])])]))
            Assert.Equal("", Right exp, runStmtsEof input)
        testCase "if" <| fun () ->
            let input =
                [
                    "if expr1:"
                    "    stmt1"
                    "    act 'arg': pl"
                    "elseif expr2:"
                    "    if expr3: stmt2 else stmt3 if expr4: stmt4 elseif expr5: stmt5"
                    "    stmt6"
                    "end"
                ] |> String.concat "\n"

            let exp =
              (If
                 (Var (ImplicitNumericType, "expr1"),
                  [StarPl (Var (ImplicitNumericType, "stmt1"));
                   Act ([Val (String "arg")], [StarPl (Var (ImplicitNumericType, "pl"))])],
                  [If
                     (Var (ImplicitNumericType, "expr2"),
                      [If
                         (Var (ImplicitNumericType, "expr3"),
                          [StarPl (Var (ImplicitNumericType, "stmt2"))],
                          [StarPl (Var (ImplicitNumericType, "stmt3"));
                           If
                             (Var (ImplicitNumericType, "expr4"),
                              [StarPl (Var (ImplicitNumericType, "stmt4"))],
                              [If
                                 (Var (ImplicitNumericType, "expr5"),
                                  [StarPl (Var (ImplicitNumericType, "stmt5"))], [])]);
                           StarPl (Var (ImplicitNumericType, "stmt6"))])], [])]))

            Assert.Equal("", Right exp, runStmtsEof input)
    ]
[<Tests>]
let stmtTests =
    let runStmts str =
        Qsp.Parser.Generic.runStateEither
            Qsp.Parser.Main.pstmt
            Qsp.Parser.Generic.emptyState str
        |> snd
    let runStmtsEof str =
        Qsp.Parser.Generic.runStateEither
            (Qsp.Parser.Main.pstmt .>> eof)
            Qsp.Parser.Generic.emptyState str
        |> snd
    testList "stmtTests" [
        testCase "inline act" <| fun () ->
            let input =
                [
                    "act 'some act': gt 'hall'"
                    "'инструкция, которая не принадлежит конструкции'"
                ] |> String.concat "\n"
            let exp =
                Act ([Val (String "some act")], [CallSt ("gt", [Val (String "hall")])])

            Assert.Equal("", Right exp, runStmts input)


        // порядок разбора
        testCase "stmt `years -= 10`" <| fun () ->
            let input = "years -= 10"
            let exp =
              (Assign
                 (AssignVar (ImplicitNumericType, "years"),
                  Expr (Minus, Var (ImplicitNumericType, "years"), Val (Int 10))))
            Assert.Equal("", Right exp, runStmtsEof input)
        testCase "call function as expression" <| fun () ->
            // f(1) — должно обрабатываться раньше, чем `callProc arg1, arg2`
            let input = "iif(somevar >= 2, 'thenBody', 'elseBody')"
            let exp =
              (StarPl
                 (Func
                    ("iif",
                     [Expr (Ge, Var (ImplicitNumericType, "somevar"), Val (Int 2));
                      Val (String "thenBody"); Val (String "elseBody")])))
            Assert.Equal("", Right exp, runStmtsEof input)
        testCase "call procedure" <| fun () ->
            let input = "gt 'begin', 'real_character'"
            let exp =
                CallSt ("gt", [Val (String "begin"); Val (String "real_character")])
            Assert.Equal("", Right exp, runStmtsEof input)
        // testCase "call " <| fun () ->
        //     let input = "The(Lady), or, the, Tiger"
        //     let exp =
        //         CallSt ("gt", [Val (String "begin"); Val (String "real_character")])
        //     Assert.Equal("", Right exp, runStmts input)
    ]

[<EntryPoint;System.STAThread>]
let main arg =
    defaultMainThisAssembly arg
