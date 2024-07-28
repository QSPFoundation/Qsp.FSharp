[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Printer.Ast.Document
open FsharpMyExtension.ShowList

module Printer =
    open FsharpMyExtension

    open Qsp.Ast

    let show indentsOption isSplitStringPl xs =
        List.map (lines << Location.Printer.showLoc indentsOption isSplitStringPl) xs

let print indentsOption isSplitStringPl xs =
    Printer.show indentsOption isSplitStringPl xs
    |> joinEmpty "\n\n"
    |> show
