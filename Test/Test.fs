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
open Qsp
open Qsp.Ast
open Qsp.Parser.Generic
open Qsp.Parser.Expr


open Fuchu
[<Tests>]
let pexprTest =
    let runExpr str =
        runStateEither pexpr Qsp.Parser.Generic.emptyState str
        |> snd
    let sprintExpr =
        Show.simpleShowExpr (failwithf "showStmtsInline not implemented %A")
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

        let input = "var1 and var2 and no var3 and obj var4"
        let exp =
            Expr
              (And,
               Expr
                 (And,
                  Expr
                    (And, Var (ImplicitNumericType, "var1"),
                     Var (ImplicitNumericType, "var2")),
                  UnarExpr (No, Var (ImplicitNumericType, "var3"))),
               UnarExpr (Obj, Var (ImplicitNumericType, "var4")))
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
            UnarExpr (Neg, Var (ImplicitNumericType, "x"))
        testf input exp

        let input = "-x + -y"
        let exp =
            Expr
              (Plus, UnarExpr (Neg, Var (ImplicitNumericType, "x")),
               UnarExpr (Neg, Var (ImplicitNumericType, "y")))
        testf input exp

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
               Expr (Plus, Var (ImplicitNumericType, "x"), Var (ImplicitNumericType, "z")),
               Var (ImplicitNumericType, "y"))
        testf input exp

        let input =
            "input 'How do you do?'"
        let exp =
            Func ("input", [Val (String [[StringKind "How do you do?"]])])
        testf input exp
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
        testCase "`x[] = 1`" <| fun () ->
            let input = "x[] = 1"
            let exp =
                Assign (AssignArrAppend (ImplicitNumericType, "x"), Val (Int 1))
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
                    "  Expecting: '+=', '-=', '=', '=+', '=-', '[' or '_'"
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
        Qsp.Parser.Generic.runStateEither
            (stringLiteralWithToken pexpr)
            { Qsp.Parser.Generic.emptyState with
                PStmts = Parser.Main.pstmts
            }
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
            let exp =
              [[HyperLinkKind
                  (StaticStmts [Proc ("GT", [Val (String [[StringKind "changes"]])])],
                   [[StringKind "changes"]])]]
            Assert.Equal("", Right exp, runEither input)
        testCase "test '<a href=\"exec: ''<<''x''>>''\">action</a>'" <| fun () ->
            let input = "'<a href=\"exec: ''<<''x''>>''\">action</a>'"
            let exp =
                [[HyperLinkKind (Raw " '<<'x'>>'", [[StringKind "action"]])]]
            Assert.Equal("", Right exp, runEither input)
    ]
[<Tests>]
let pbracesTests =
    let runEither str =
        Qsp.Parser.Generic.runStateEither
            (pbraces Tokens.TokenType.StringBraced)
            Qsp.Parser.Generic.emptyState
            str
        |> snd
    testList "stringLiteralWithTokenTest" [
        testCase "base" <| fun () ->
            Assert.Equal("", Right "", runEither "{}")
        testCase "braces1" <| fun () ->
            Assert.Equal("", Right "abc", runEither "{abc}")
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
                Proc ("someProc", [Var (ImplicitNumericType, "arg1")])

            Assert.Equal("", Right exp, runStmts input)
        testCase "pcallProcTests base many args" <| fun () ->
            let input = "someProc z / 2, x + y"
            let exp =
                (Proc
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
            let exp = Proc ("*pl", [])
            Assert.Equal("", Right exp, runStmts input)
        testCase "*pl arg1, arg2" <| fun () ->
            let input = "*pl arg1, arg2"
            let exp =
                (Proc
                   ("*pl",
                    [Var (ImplicitNumericType, "arg1"); Var (ImplicitNumericType, "arg2")]))
            Assert.Equal("", Right exp, runStmts input)
        testCase "call `p2 x`, который начинается на заданный оператор `p`, но образует новый" <| fun () ->
            let input = "p2 x"
            let exp =
                Proc ("p2", [Var (ImplicitNumericType, "x")])
            Assert.Equal("", Right exp, runStmts input)
        testCase "call ad-hoc `add obj`" <| fun () ->
            let input = "add obj"
            let exp =
                Proc ("addobj", [])
            Assert.Equal("", Right exp, runStmts input)
        testCase "call ad-hoc `close all`" <| fun () ->
            let input = "close all"
            let exp =
                Proc ("close all", [])
            Assert.Equal("", Right exp, runStmts input)
    ]

let printStmts stmts =
    List.collect (Show.showStmt (Qsp.Show.UsingSpaces 4) Show.FormatConfig.Default) stmts
    |> ShowList.joinEmpty "\n"
    |> ShowList.show
let printStmt stmt =
    Qsp.Show.showStmt (Qsp.Show.UsingSpaces 4) Show.FormatConfig.Default stmt
    |> ShowList.joinEmpty "\n"
    |> ShowList.show
let StarPl arg = Proc("*pl", [arg])
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
                    "'statement that not belong to construction'"
                ] |> String.concat "\n"
            let exp =
                (If
                   (Var (ImplicitNumericType, "expr"), [Proc ("gt", [Val (String [[StringKind "hall"]])])],
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
                    "elseif expr5:"
                    "    stmt6"
                    "elseif expr6: stmt4"
                ] |> String.concat "\n"
            // tested
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
                       [StarPl (Var (ImplicitNumericType, "stmt3"))], [])],
                   [If
                      (Var (ImplicitNumericType, "expr5"),
                       [StarPl (Var (ImplicitNumericType, "stmt6"))],
                       [If
                          (Var (ImplicitNumericType, "expr6"),
                           [StarPl (Var (ImplicitNumericType, "stmt4"))], [])])])
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
                   Act ([Val (String [[StringKind "arg"]])], [Proc ("pl", [])])],
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
                                  [StarPl (Var (ImplicitNumericType, "stmt5"))], [])])]);
                       StarPl (Var (ImplicitNumericType, "stmt6"))], [])]))
            Assert.Equal("", Right exp, runStmtsEof input)
    ]

[<Tests>]
let forTests =
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
    testList "forTests" [
        testCase "multiline `for i = 4 + x to 45 / x + y:`" <| fun () ->
            let input =
                [
                    "for i = 4 + x to 45 / x + y:"
                    "    stmt"
                    "end"
                ] |> String.concat "\n"
            let exp =
              (For
                 ((ImplicitNumericType, "i"),
                  Expr (Plus, Val (Int 4), Var (ImplicitNumericType, "x")),
                  Expr
                    (Plus, Expr (Divide, Val (Int 45), Var (ImplicitNumericType, "x")),
                     Var (ImplicitNumericType, "y")),
                  None,
                  [StarPl (Var (ImplicitNumericType, "stmt"))]))
            Assert.Equal("", Right exp, runStmtsEof input)
        testCase "inline `for i = 4 + x to 45 / x + y: stmt`" <| fun () ->
            let input =
                [
                    "for i = 4 + x to 45 / x + y: stmt"
                    "'statement that not belong to construction'"
                ] |> String.concat "\n"
            let exp =
              (For
                 ((ImplicitNumericType, "i"),
                  Expr (Plus, Val (Int 4), Var (ImplicitNumericType, "x")),
                  Expr
                    (Plus, Expr (Divide, Val (Int 45), Var (ImplicitNumericType, "x")),
                     Var (ImplicitNumericType, "y")),
                  None,
                  [StarPl (Var (ImplicitNumericType, "stmt"))]))
            Assert.Equal("", Right exp, runStmts input)
        testCase "inline `for i = 4 + x to 45 / x + y step x + 1: stmt`" <| fun () ->
            let input =
                [
                    "for i = 4 + x to 45 / x + y step x + 1: stmt"
                    "'statement that not belong to construction'"
                ] |> String.concat "\n"
            let exp =
              (For
                 ((ImplicitNumericType, "i"),
                  Expr (Plus, Val (Int 4), Var (ImplicitNumericType, "x")),
                  Expr
                    (Plus, Expr (Divide, Val (Int 45), Var (ImplicitNumericType, "x")),
                     Var (ImplicitNumericType, "y")),
                  Some (Expr (Plus, Var (ImplicitNumericType, "x"), Val (Int 1))),
                  [StarPl (Var (ImplicitNumericType, "stmt"))]))
            Assert.Equal("", Right exp, runStmts input)
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
                    "'statement that not belong to construction'"
                ] |> String.concat "\n"
            let exp =
                Act ([Val (String [[StringKind "some act"]])], [Proc ("gt", [Val (String [[StringKind "hall"]])])])

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
                      Val (String [[StringKind "thenBody"]]); Val (String [[StringKind "elseBody"]])])))
            Assert.Equal("", Right exp, runStmtsEof input)
        testCase "call procedure" <| fun () ->
            let input = "gt 'begin', 'real_character'"
            let exp =
                Proc ("gt", [Val (String [[StringKind "begin"]]); Val (String [[StringKind "real_character"]])])
            Assert.Equal("", Right exp, runStmtsEof input)
        // testCase "call " <| fun () ->
        //     let input = "The(Lady), or, the, Tiger"
        //     let exp =
        //         CallSt ("gt", [Val (String "begin"); Val (String "real_character")])
        //     Assert.Equal("", Right exp, runStmts input)
    ]

module TestOnMocks =
    type T = Location list
    let enc = System.Text.Encoding.UTF8
    let startOnFile path =
        match Qsp.Parser.Main.startOnFile enc path with
        | Success(x, _, _) -> x
        | Failure(x, _, _) -> failwithf "%s\n%s" path x
    let replaceOrNot expPath actPath =
        printfn "\"%s\"\nnot equal\n\"%s\""
            (System.IO.Path.GetFullPath expPath)
            (System.IO.Path.GetFullPath actPath)
        let rec whileYOrN () =
            match System.Console.ReadKey().Key with
            | System.ConsoleKey.Y -> true
            | System.ConsoleKey.N -> false
            | x ->
                printfn "need (y/n) but %A" x
                whileYOrN ()
        printfn "Replace? (y/n)"
        let res = whileYOrN()
        if res then
            System.IO.File.Copy(actPath, expPath, true)
            printfn "replaced"
        res
    let addExpToPath path =
        path
        |> Path.changeFileNameWithoutExt (sprintf "%sExp")
    let outputDir = @"..\..\..\Mocks"
    let copyAsExp path =
        System.IO.File.Copy(path, addExpToPath path, true)
    let getPathActLocal (pathAct:string) =
        sprintf "%s\\%s" outputDir (System.IO.Path.GetFileName pathAct)
        |> fun x -> System.IO.Path.ChangeExtension(x, ".json")
    let showTest path =
        let srcPath = path
        let parseActPath = getPathActLocal srcPath
        let parseExpPath = addExpToPath parseActPath
        let getPath (path:string) =
            sprintf "%s\\%s" outputDir (System.IO.Path.GetFileName path)
            |> fun x -> System.IO.Path.ChangeExtension(x, ".qsps")
        let showActPath = getPath srcPath
        let showExpPath = addExpToPath showActPath

        let act =
            // if System.IO.File.Exists parseExpPath then
            //     let src : T = Json.desf parseExpPath
            //     src |> Qsp.Show.printLocs Qsp.Show.UsingTabs
            // else
                let act = startOnFile srcPath
                // act |> Json.serf parseExpPath
                // failwithf "\"%s\" не найден, потому пришлось его создать. Естественно, все тесты пошли коту под хвост." parseExpPath
                act |> Qsp.Show.printLocs Qsp.Show.UsingTabs Show.FormatConfig.Default
        let exp =
            if System.IO.File.Exists showExpPath then
                System.IO.File.ReadAllText showExpPath
            else
                System.IO.File.WriteAllText(showExpPath, act)
                failwithf "\"%s\" не найден, потому пришлось его создать. Естественно, все тесты пошли коту под хвост." showExpPath
        if exp <> act then
            System.IO.File.WriteAllText(showActPath, act)

            if replaceOrNot showExpPath showActPath then ()
            else failwithf "not pass"
    let mockTestList = "mock tests"
    [<Tests>]
    let showTests =
        let mocksDir = outputDir + @"\Src"
        let tests =
            if System.IO.Directory.Exists mocksDir then
                System.IO.Directory.GetFiles(mocksDir, "*.qsps")
                |> Array.map (fun path ->
                    testCase (sprintf "'%s' test" (System.IO.Path.GetFullPath path)) <| fun () ->
                        showTest path
                        Assert.Equal("", true, true)
                )
            else [||]
        testList mockTestList tests

[<EntryPoint;System.STAThread>]
let main args =
    let isFullTest () =
        let rec whileYOrN () =
            match System.Console.ReadKey().Key with
            | System.ConsoleKey.Y -> true
            | System.ConsoleKey.N -> false
            | x ->
                printfn "`y` or `n` but %A" x
                whileYOrN ()
        printfn "Full test? (`y` or `n`)"
        whileYOrN ()
    let f isFullTest =
        if isFullTest then
            defaultMainThisAssembly args
        else
            defaultMainThisAssemblyFilter args
                (fun x ->
                    x.Where(fun x -> not <| x.StartsWith TestOnMocks.mockTestList))
    match args with
    | [|"--full"|] -> f true
    | [||] ->
        f (isFullTest ())
    | _ ->
        printfn "`--full` or pass args but: %A" args
        1
