[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Printer.Ast.Document
open FsharpMyExtension.ShowList

module Printer =
    open FsharpMyExtension

    open Qsp.Ast

    let show indentsOption isSplitStringPl (document: Document) =
        document
        |> List.map (
            DocumentElement.Printer.show indentsOption isSplitStringPl
        )

let print indentsOption isSplitStringPl xs =
    Printer.show indentsOption isSplitStringPl xs
    |> joinEmpty "\n"
    |> show
