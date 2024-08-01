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
open Qsp.Printer
open Qsp.Printer.Ast
open Qsp.Parser.Generic
open Qsp.Parser.Ast

open Fuchu

open Tests

// #load "Parsec.fs"

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
