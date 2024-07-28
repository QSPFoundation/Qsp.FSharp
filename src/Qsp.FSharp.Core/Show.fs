module Qsp.Show
open FsharpMyExtension
open FsharpMyExtension.ShowList

open Qsp.Ast
open Qsp.Printer.Ast

let showLoc indentsOption isSplitStringPl (Location(name, statements)) : ShowS list =
    [
        yield showString "# " << showString name
        yield! List.collect (Statement.Printer.showStmt indentsOption isSplitStringPl) statements
        yield showString (sprintf "--- %s ----------" name)
    ]

let printLocs indentsOption isSplitStringPl xs =
    List.map (lines << showLoc indentsOption isSplitStringPl) xs
    |> joinEmpty "\n\n"
    |> show
