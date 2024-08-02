module Qsp.Parser.Ast.Statement.Tests
open Fuchu
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either

open Qsp
open Qsp.Ast
open Qsp.Parser.Generic
open Qsp.Parser.Ast

[<Tests>]
let assignTest =
    let runExpr str =
        runStateEither (Statement.Parser.pAssign pzero) State.empty str
        |> snd
    testList "assignTest" [
        testCase "implicit assign implicit var" <| fun () ->
            let input = "x = 21 + 21"
            let exp =
                (Assign (
                    false,
                    [ AssignVar (NumericType, "x") ],
                    Expr (Plus, Val (Int 21), Val (Int 21))
                ))
            Assert.Equal("", Right exp, runExpr input)
        testCase "implicit assign implicit array var" <| fun () ->
            let input = "x[expr] = 42"
            let exp =
                (Assign (
                    false,
                    [ AssignArr ((NumericType, "x"), [ Var (NumericType, "expr") ]) ],
                    Val (Int 42)
                ))
            Assert.Equal("", Right exp, runExpr input)
        testCase "implicit `-=` implicit var" <| fun () ->
            let input = "years -= 10"
            let exp =
                Assign (
                    false,
                    [ AssignVar (NumericType, "years") ],
                    Expr (Minus, Var (NumericType, "years"), Val (Int 10))
                )
            Assert.Equal("", Right exp, runExpr input)
        testCase "implicit `-=` implicit var 2" <| fun () ->
            let input = "php -= 3*emdmg*2 - parm"
            let exp =
                Assign (
                    false,
                    [ AssignVar (NumericType, "php") ],
                    Expr (
                        Minus,
                        Var (NumericType, "php"),
                        Expr (
                            Minus,
                            Expr (
                                Times,
                                Expr (
                                    Times,
                                    Val (Int 3),
                                    Var (NumericType, "emdmg")
                                ),
                                Val (Int 2)
                            ),
                            Var (NumericType, "parm")
                        )
                    )
                )
            Assert.Equal("", Right exp, runExpr input)
        testCase "5" <| fun () ->
            let input = "a = a = no -a > b"
            let exp =
                Assign (
                    false,
                    [ AssignVar (NumericType, "a") ],
                    Expr (
                        Eq,
                        Var (NumericType, "a"),
                        UnarExpr (
                            No,
                            Expr
                                (
                                    Gt,
                                    UnarExpr (Neg, Var (NumericType, "a")),
                                    Var (NumericType, "b")
                                )
                        )
                    )
                )
            Assert.Equal("", Right exp, runExpr input)
        testCase "implicit assign explicit array var" <| fun () ->
            let input = "$x[expr] = 42"
            let exp =
                Assign (
                    false,
                    [ AssignArr ((StringType, "x"), [Var (NumericType, "expr")]) ],
                    Val (Int 42)
                )
            Assert.Equal("", Right exp, runExpr input)
        testCase "implicit assign explicit two demention array" <| fun () ->
            let input = "$x[firstKeyExpr, secondKeyExpr] = 42"
            let exp =
                Assign (
                    false,
                    [
                        AssignArr (
                            (StringType, "x"),
                            [
                                Var (NumericType, "firstKeyExpr")
                                Var (NumericType, "secondKeyExpr")
                            ]
                        )
                    ],
                    Val (Int 42)
                )
            Assert.Equal("", Right exp, runExpr input)
        testCase "implicit assign explicit tree demention array" <| fun () ->
            let input = "$x[firstKeyExpr, secondKeyExpr, $thirdKeyExpr] = 42"
            let exp =
                Assign (
                    false,
                    [
                        AssignArr (
                            (StringType, "x"),
                            [
                                Var (NumericType, "firstKeyExpr")
                                Var (NumericType, "secondKeyExpr")
                                Var (StringType, "thirdKeyExpr")
                            ]
                        )
                    ],
                    Val (Int 42)
                )
            Assert.Equal("", Right exp, runExpr input)
        testCase "#x = 21 + 21" <| fun () ->
            let input = "#x = 21 + 21"
            let exp =
                Assign (
                    false,
                    [ AssignVar (NumericType, "#x") ],
                    Expr (Plus, Val (Int 21), Val (Int 21))
                )
            Assert.Equal("", Right exp, runExpr input)
        testCase "`x[] = 1`" <| fun () ->
            let input = "x[] = 1"
            let exp =
                Assign (
                    false,
                    [ AssignArr((NumericType, "x"), []) ],
                    Val (Int 1)
                )
            Assert.Equal("", Right exp, runExpr input)
        // ложные случаи:
        testCase "attempt assign function" <| fun () ->
            let input = "f(expr) = 42" // поскольку `=` — это одновременно и оператор присваивания и оператор равности, так что сойдет за выражение
            let exp =
                [
                    "Error in Ln: 1 Col: 1"
                    "f(expr) = 42"
                    "^"
                    "Expecting: 'let' (case-insensitive), 'local' (case-insensitive) or 'set'"
                    "(case-insensitive)"
                    ""
                    "The parser backtracked after:"
                    "  Error in Ln: 1 Col: 2"
                    "  f(expr) = 42"
                    "   ^"
                    "  Expecting: '*=', '+=', ',', '-=', '/=', '=', '[' or '_'"
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
        testCase "num, $str, arrNum[0], $arrStr['key'] = $tuple" <| fun () ->
            Assert.Equal (
                "",
                Right (
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
                ),
                runExpr "num, $str, arrNum[0], $arrStr['key'] = $tuple"
            )
        testCase "local num, $str, arrNum[0], $arrStr['key'] = $tuple" <| fun () ->
            Assert.Equal (
                "",
                Right (
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
                ),
                runExpr "local num, $str, arrNum[0], $arrStr['key'] = $tuple"
            )
    ]

[<Tests>]
let pcallProcTests =
    let runStmts str =
        runStateEither
            (Statement.Parser.pcallProc Statements.Parser.pstmts)
            State.empty
            str
        |> snd
    testList "pcallProcTests" [
        testCase "pcallProcTests base" <| fun () ->
            let input = "someProc arg1"
            let exp =
                Proc ("someProc", [Var (NumericType, "arg1")])

            Assert.Equal("", Right exp, runStmts input)
        testCase "pcallProcTests base many args" <| fun () ->
            let input = "someProc z / 2, x + y"
            let exp =
                (Proc
                   ("someProc",
                    [Expr (Divide, Var (NumericType, "z"), Val (Int 2));
                     Expr
                       (Plus, Var (NumericType, "x"), Var (NumericType, "y"))]))
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
                    [Var (NumericType, "arg1"); Var (NumericType, "arg2")]))
            Assert.Equal("", Right exp, runStmts input)
        testCase "call `p2 x`, который начинается на заданный оператор `p`, но образует новый" <| fun () ->
            let input = "p2 x"
            let exp =
                Proc ("p2", [Var (NumericType, "x")])
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

let StarPl arg = Proc("*pl", [arg])

let emptyPos = NoEqualityPosition Position.empty

let emptyPoss x = x |> List.map (fun x -> emptyPos, x)

let StaticStmts x =
    emptyPoss x
    |> StaticStmts

let If(expr, thenBody, elseBody) =
    If(expr, emptyPoss thenBody, emptyPoss elseBody)

let Act(exprs, thenBody) = Act(exprs, emptyPoss thenBody)

let For(x, y, z, w, thenBody) = For(x, y, z, w, emptyPoss thenBody)

let equalTwoPosStmt (note, stmt1, stmt2) =
    match stmt1, stmt2 with
    | Right stmt1', Right stmt2' ->
        if stmt1' <> stmt2' then
            failtestf "Expected:\n%A\n\nActual:\n%A\nWarning: ignores Position when comparing!" stmt1 stmt2
    | _ -> failtestf "Expected:\n%A\n\nActual:\n%A\nWarning: ignores Position when comparing!" stmt1 stmt2

let exprEqual (note, expr1, expr2) =
    match expr1, expr2 with
    | Right expr1', Right expr2' ->
        if expr1' <> expr2' then
            failtestf "Expected:\n%A\n\nActual:\n%A\nWarning: ignores Position when comparing!" expr1 expr2
    | _ -> failtestf "Expected:\n%A\n\nActual:\n%A\nWarning: ignores Position when comparing!" expr1 expr2

[<Tests>]
let ifTests =
    let runStmts str =
        runStateEither
            Statements.Parser.Intermediate.pstmt
            State.empty str
        |> snd
    let runStmtsEof str =
        runStateEither
            (Statements.Parser.Intermediate.pstmt .>> eof)
            State.empty str
        |> snd
    testList "ifTests" [
        testCase "inline if" <| fun () ->
            let input =
                [
                    "if expr: gt 'hall'"
                    "'statement that not belong to construction'"
                ] |> String.concat "\n"
            let exp =
                (emptyPos, If
                   (Var (NumericType, "expr"), [Proc ("gt", [Val (String [[StringKind "hall"]])])],
                    []))
            equalTwoPosStmt("", Right exp, runStmts input)
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
                  (Var (NumericType, "expr"),
                   [If
                      (Var (NumericType, "expr2"),
                       [StarPl (Var (NumericType, "stmt1"))], []);
                    If
                      (Var (NumericType, "expr3"),
                       [StarPl (Var (NumericType, "stmt1"))],
                       [StarPl (Var (NumericType, "stmt2"))]);
                    If
                      (Var (NumericType, "expr4"),
                       [StarPl (Var (NumericType, "stmt3"))], [])],
                   [If
                      (Var (NumericType, "expr5"),
                       [StarPl (Var (NumericType, "stmt6"))],
                       [If
                          (Var (NumericType, "expr6"),
                           [StarPl (Var (NumericType, "stmt4"))], [])])])
            equalTwoPosStmt("", Right (emptyPos, exp), runStmtsEof input)
        testCase "simple if" <| fun () ->
            let input =
                [
                    "if expr:"
                    "    someStmt"
                    "end"
                ] |> String.concat "\n"
            let exp =
                (If
                   (Var (NumericType, "expr"),
                    [StarPl (Var (NumericType, "someStmt"))], []))

            equalTwoPosStmt("", Right (emptyPos, exp), runStmtsEof input)
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
                   (Var (NumericType, "expr1"),
                    [StarPl (Var (NumericType, "stmt1"))],
                    [If
                       (Var (NumericType, "expr2"),
                        [StarPl (Var (NumericType, "stmt2"))],
                        [If
                           (Var (NumericType, "expr3"),
                            [StarPl (Var (NumericType, "stmt3"))],
                            [StarPl (Var (NumericType, "stmt4"))])])]))
            equalTwoPosStmt("", Right (emptyPos, exp), runStmtsEof input)
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
                   (Var (NumericType, "expr1"),
                    [StarPl (Var (NumericType, "stmt1"))],
                    [If
                       (Var (NumericType, "expr2"),
                        [StarPl (Var (NumericType, "stmt2"))],
                        [If
                           (Var (NumericType, "expr3"),
                            [StarPl (Var (NumericType, "stmt3"))], [])])]))
            equalTwoPosStmt("", Right (emptyPos, exp), runStmtsEof input)
        testCase "another inline if" <| fun () ->
            let input =
                [
                    "if expr:"
                    "elseif expr: stmt"
                ] |> String.concat "\n"
            let exp =
              (If
                 (Var (NumericType, "expr"), [],
                  [If
                     (Var (NumericType, "expr"),
                      [StarPl (Var (NumericType, "stmt"))], [])]))
            equalTwoPosStmt("", Right (emptyPos, exp), runStmtsEof input)
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
                 (Var (NumericType, "expr1"),
                  [StarPl (Var (NumericType, "stmt1"))],
                  [If
                     (Var (NumericType, "expr2"),
                      [StarPl (Var (NumericType, "stmt2"));
                       If
                         (Var (NumericType, "expr4"),
                          [StarPl (Var (NumericType, "stmt4"))],
                          [If
                             (Var (NumericType, "expr5"),
                              [StarPl (Var (NumericType, "stmt5"))], [])]);
                       StarPl (Var (NumericType, "stmt6"))],
                      [If
                         (Var (NumericType, "expr3"),
                          [StarPl (Var (NumericType, "stmt3"))], [])])]))
            equalTwoPosStmt("", Right (emptyPos, exp), runStmtsEof input)
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
                 (Var (NumericType, "expr1"),
                  [StarPl (Var (NumericType, "stmt1"));
                   Act ([Val (String [[StringKind "arg"]])], [Proc ("pl", [])])],
                  [If
                     (Var (NumericType, "expr2"),
                      [If
                         (Var (NumericType, "expr3"),
                          [StarPl (Var (NumericType, "stmt2"))],
                          [StarPl (Var (NumericType, "stmt3"));
                           If
                             (Var (NumericType, "expr4"),
                              [StarPl (Var (NumericType, "stmt4"))],
                              [If
                                 (Var (NumericType, "expr5"),
                                  [StarPl (Var (NumericType, "stmt5"))], [])])]);
                       StarPl (Var (NumericType, "stmt6"))], [])]))
            equalTwoPosStmt("", Right (emptyPos, exp), runStmtsEof input)
    ]

[<Tests>]
let forTests =
    let runStmts str =
        runStateEither
            Statements.Parser.Intermediate.pstmt
            State.empty str
        |> snd
    let runStmtsEof str =
        runStateEither
            (Statements.Parser.Intermediate.pstmt .>> eof)
            State.empty str
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
                 ((NumericType, "i"),
                  Expr (Plus, Val (Int 4), Var (NumericType, "x")),
                  Expr
                    (Plus, Expr (Divide, Val (Int 45), Var (NumericType, "x")),
                     Var (NumericType, "y")),
                  None,
                  [StarPl (Var (NumericType, "stmt"))]))
            equalTwoPosStmt("", Right (emptyPos, exp), runStmtsEof input)
        testCase "inline `for i = 4 + x to 45 / x + y: stmt`" <| fun () ->
            let input =
                [
                    "for i = 4 + x to 45 / x + y: stmt"
                    "'statement that not belong to construction'"
                ] |> String.concat "\n"
            let exp =
              (For
                 ((NumericType, "i"),
                  Expr (Plus, Val (Int 4), Var (NumericType, "x")),
                  Expr
                    (Plus, Expr (Divide, Val (Int 45), Var (NumericType, "x")),
                     Var (NumericType, "y")),
                  None,
                  [StarPl (Var (NumericType, "stmt"))]))
            equalTwoPosStmt("", Right (emptyPos, exp), runStmts input)
        testCase "inline `for i = 4 + x to 45 / x + y step x + 1: stmt`" <| fun () ->
            let input =
                [
                    "for i = 4 + x to 45 / x + y step x + 1: stmt"
                    "'statement that not belong to construction'"
                ] |> String.concat "\n"
            let exp =
              (For
                 ((NumericType, "i"),
                  Expr (Plus, Val (Int 4), Var (NumericType, "x")),
                  Expr
                    (Plus, Expr (Divide, Val (Int 45), Var (NumericType, "x")),
                     Var (NumericType, "y")),
                  Some (Expr (Plus, Var (NumericType, "x"), Val (Int 1))),
                  [StarPl (Var (NumericType, "stmt"))]))
            equalTwoPosStmt("", Right (emptyPos, exp), runStmts input)
    ]

[<Tests>]
let stmtTests =
    let runStmts str =
        runStateEither
            Statements.Parser.Intermediate.pstmt
            State.empty str
        |> snd
    let runStmtsEof str =
        runStateEither
            (Statements.Parser.Intermediate.pstmt .>> eof)
            State.empty str
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

            equalTwoPosStmt("", Right (emptyPos, exp), runStmts input)

        // порядок разбора
        testCase "stmt `years -= 10`" <| fun () ->
            let input = "years -= 10"
            let exp =
                Assign (
                    false,
                    [ AssignVar (NumericType, "years") ],
                    Expr (Minus, Var (NumericType, "years"), Val (Int 10))
                )
            equalTwoPosStmt("", Right (emptyPos, exp), runStmtsEof input)
        testCase "call function as expression" <| fun () ->
            // f(1) — должно обрабатываться раньше, чем `callProc arg1, arg2`
            let input = "iif(somevar >= 2, 'thenBody', 'elseBody')"
            let exp =
              (StarPl
                 (Func
                    (Predef Defines.Iif,
                     [Expr (Ge, Var (NumericType, "somevar"), Val (Int 2));
                      Val (String [[StringKind "thenBody"]]); Val (String [[StringKind "elseBody"]])])))
            equalTwoPosStmt("", Right (emptyPos, exp), runStmtsEof input)
        testCase "call procedure" <| fun () ->
            let input = "gt 'begin', 'real_character'"
            let exp =
                Proc ("gt", [Val (String [[StringKind "begin"]]); Val (String [[StringKind "real_character"]])])
            equalTwoPosStmt("", Right (emptyPos, exp), runStmtsEof input)
        // testCase "call " <| fun () ->
        //     let input = "The(Lady), or, the, Tiger"
        //     let exp =
        //         CallSt ("gt", [Val (String "begin"); Val (String "real_character")])
        //     Assert.Equal("", Right exp, runStmts input)
    ]
