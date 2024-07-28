module Qsp.Show
open FsharpMyExtension
open FsharpMyExtension.ShowList

open Qsp.Ast
open Qsp.Printer.Ast

let printLocs indentsOption isSplitStringPl xs =
    List.map (lines << Location.Printer.showLoc indentsOption isSplitStringPl) xs
    |> joinEmpty "\n\n"
    |> show
