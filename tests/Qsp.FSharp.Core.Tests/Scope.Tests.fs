module Qsp.FSharp.Scope.Tests
open Fuchu

open Qsp.Parser.Scope

[<Tests>]
let ``Scope.ScopeSystem.addAsRead`` =
    testList "Scope.ScopeSystem.addAsRead" [
        testCase "base" <| fun () ->
            Assert.Equal(
                "",
                (0, {
                    Scopes = [Map [("x", 0)]]
                    NewVarId = 1
                    Variables = Map [(0, ("x", [(0, 0); (0, 0)]))]
                }),
                (
                    let (_, scopeSystem) =
                        ScopeSystem.empty
                        |> ScopeSystem.addAsRead ("x", (fun xs -> (0, 0)::xs))
                    ScopeSystem.addAsRead ("x", (fun xs -> (0, 0)::xs)) scopeSystem
                )
            )
    ]
