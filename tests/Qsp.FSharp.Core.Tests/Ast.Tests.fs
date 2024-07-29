module Qsp.Ast.Tests
open Fuchu

open Qsp.Ast

[<Tests>]
let noEqualityPositionTests =
    testList "noEqualityPositionTests" [
        testCase "base" <| fun () ->
            Assert.Equal(
                "",
                NoEqualityPosition(Position.create "" 0L 0L 0L),
                NoEqualityPosition(Position.create "" 1L 1L 1L)
            )
    ]
