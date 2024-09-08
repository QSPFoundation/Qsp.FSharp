module Qsp.FSharp.Scope.Tests
open Fuchu

open Qsp.Parser.Scope

[<Tests>]
let ``Scope.addAsRead`` =
    testList "Scope.addAsRead" [
        testCase "base" <| fun () ->
            Assert.Equal(
                "",
                (0, {
                    Scopes = [Map [("x", 0)]]
                    NewVarId = 1
                    Result = Map [(0, ("x", [(0, 0); (0, 0)]))]
                }),
                (
                    let (_, scopeSystem) =
                        ScopeSystem.empty
                        |> addAsRead ("x", (fun xs -> (0, 0)::xs))
                    addAsRead ("x", (fun xs -> (0, 0)::xs)) scopeSystem
                )
            )
    ]
