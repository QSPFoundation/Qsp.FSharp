module Qsp.Tests.Test
open Fuchu

open Qsp
open Tests

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
            defaultMainThisAssemblyFilter
                args
                (fun x ->
                    x.Where(fun x -> not <| x.StartsWith TestOnMocks.mockTestList)
                )

    match args with
    | [|"--full"|] ->
        f true
    | [||] ->
        f (isFullTest ())
    | _ ->
        printfn "`--full` or pass args but: %A" args
        1
