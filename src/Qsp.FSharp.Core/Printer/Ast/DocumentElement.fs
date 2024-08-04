[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Printer.Ast.DocumentElement
open FsharpMyExtension.ShowList

module Printer =
    open Qsp.Ast

    let showLocation indentsOption isSplitStringPl location =
        Location.Printer.showLoc indentsOption isSplitStringPl location
        |> lines

    let showComment (comment: string) = showString comment

    let show indentsOption isSplitStringPl (docElement: DocumentElement) =
        match docElement with
        | DocumentElement.Location location ->
            showLocation indentsOption isSplitStringPl location
        | DocumentElement.CommentLine comment ->
            showComment comment
