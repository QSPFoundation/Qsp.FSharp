module Qsp.Parser.Ast.DocumentElement.Tests
open Fuchu
open FsharpMyExtension.Either

open Qsp.Ast
open Qsp.Tokens
open Qsp.Parser.Generic
open Qsp.Parser.Ast
open FsharpMyExtension

[<Tests>]
let ``DocumentElement.Parser.pCommentLineElement`` =
    let parse =
        runStateEither DocumentElement.Parser.pCommentLineElement State.empty
        >> fun (state, result) -> state.Tokens, result

    testList "DocumentElement.Parser.pCommentLineElement" [
        testCase "\n" <| fun () ->
            Assert.Equal (
                "",
                ([], Right ""),
                parse "\n"
            )

        testCase "#" <| fun () ->
            Assert.Equal (
                "",
                (
                    [],
                    Left (
                        String.concat System.Environment.NewLine [
                            "Error in Ln: 1 Col: 1"
                            "#"
                            "^"
                            "Expecting: comment line or empty comment line"
                            ""
                        ]
                    )
                ),
                parse "#"
            )

        testCase "line" <| fun () ->
            Assert.Equal (
                "",
                (
                    [
                        {
                            TokenType = TokenType.Comment
                            Range = {
                                Line = 1L
                                Column1 = 1L
                                Column2 = 5L
                            }
                        }
                    ],
                    Right "line"
                ),
                parse "line"
            )

        testCase "multiline\ncomment" <| fun () ->
            Assert.Equal (
                "",
                (
                    [
                        {
                            TokenType = TokenType.Comment
                            Range = {
                                Line = 1L
                                Column1 = 1L
                                Column2 = 10L
                            }
                        }
                    ],
                    Right "multiline"
                ),
                "multiline\ncomment" |> parse
            )
    ]

[<Tests>]
let ``DocumentElement.Parser.pLocationElement`` =
    let parseState =
        runStateEither DocumentElement.Parser.pLocationElement State.empty

    let parse =
        parseState >> snd

    testList "DocumentElement.Parser.pLocationElement" [
        testCase "location with comment at the end" <| fun () ->
            Assert.Equal (
                "",
                Right (
                    Location ("location", [])
                ),
                parse (
                    String.concat System.Environment.NewLine [
                        "# location"
                        "--- comment"
                    ]
                )
            )

        testCase "scopes" <| fun () ->
            Assert.Equal (
                "",
                {
                    VarScopeSystem = {
                        Scopes = [
                            Map [ "foo", 2 ]
                        ]
                        NewVarId = 6
                        Result =
                            Map [
                                0, ("args", [])
                                1, ("result", [])
                                2, (
                                    "foo", [
                                        InlineRange.create 3L 4L 7L, ReadAccess
                                        InlineRange.create 2L 4L 7L, WriteAccess
                                    ]
                                )
                                3, (
                                    "foo", [
                                        InlineRange.create 19L 4L 7L, ReadAccess
                                        InlineRange.create 8L 6L 9L, WriteAccess
                                        InlineRange.create 7L 6L 9L, ReadAccess
                                        InlineRange.create 5L 4L 7L, ReadAccess
                                        InlineRange.create 4L 10L 13L, WriteAccess
                                    ]
                                )
                                4, (
                                    "foo", [
                                        InlineRange.create 17L 6L 9L, ReadAccess
                                        InlineRange.create 13L 8L 11L, WriteAccess
                                        InlineRange.create 12L 8L 11L, ReadAccess
                                        InlineRange.create 10L 6L 9L, ReadAccess
                                        InlineRange.create 9L 11L 14L, WriteAccess
                                    ]
                                )
                                5, (
                                    "foo", [
                                        InlineRange.create 15L 8L 11L, ReadAccess
                                        InlineRange.create 14L 13L 16L, WriteAccess
                                    ]
                                )
                            ]
                    }

                    Ranges = [
                        InlineRange.create 19L 4L 7L, 3
                        InlineRange.create 17L 6L 9L, 4
                        InlineRange.create 15L 8L 11L, 5
                        InlineRange.create 14L 13L 16L, 5
                        InlineRange.create 13L 8L 11L, 4
                        InlineRange.create 12L 8L 11L, 4
                        InlineRange.create 10L 6L 9L, 4
                        InlineRange.create 9L 11L 14L, 4
                        InlineRange.create 8L 6L 9L, 3
                        InlineRange.create 7L 6L 9L, 3
                        InlineRange.create 5L 4L 7L, 3
                        InlineRange.create 4L 10L 13L, 3
                        InlineRange.create 3L 4L 7L, 2
                        InlineRange.create 2L 4L 7L, 2
                    ]
                },
                parseState (
                    String.concat System.Environment.NewLine [
                        "# begin"
                        "  $foo = 'global_foo — глобальная переменная, работающая вне локаций'"
                        "  $foo &! считывание global_foo"
                        "  local $foo = 'begin_foo — переменная, видимая в пределах этой локации. Перезаписывает `global_foo`'"
                        "  $foo &! считывание begin_foo"
                        "  if 1:"
                        "    $foo &! begin_foo \"проваливается\" в if блок"
                        "    $foo = 'присваивание к begin_foo'"
                        "    local foo = 'begin_if_foo — переменная, видимая в пределах if блока. Перезаписывает `begin_foo`'"
                        "    $foo &! считывание begin_if_foo"
                        "    if 1:"
                        "      $foo &! begin_if_foo \"проваливается\" в if блок"
                        "      $foo = 'присваивание к begin_if_foo'"
                        "      local foo = 'begin_if_if_foo — переменная, видимая в пределах if-if блока. Перезаписывает `begin_if_foo`'"
                        "      $foo &! считывание begin_if_if_foo"
                        "    end"
                        "    $foo &! считывание begin_if_foo"
                        "  end"
                        "  $foo &! считывание begin_foo"
                        "-"
                    ]
                )
                |> fst |> fun state -> state.Highlights.VarHighlights
            )
    ]
