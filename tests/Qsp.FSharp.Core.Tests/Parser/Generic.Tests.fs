module Tests
open Fuchu
open FsharpMyExtension.Either

open Qsp
open Qsp.Parser.Generic

[<Tests>]
let pbracesTests =
    let runEither str =
        runStateEither
            (pbraces Tokens.TokenType.StringBraced)
            State.empty
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
