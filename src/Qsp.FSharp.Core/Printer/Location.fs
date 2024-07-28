[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Printer.Ast.Location

module Printer =
    open FsharpMyExtension
    open FsharpMyExtension.ShowList

    open Qsp.Ast

    let showLoc indentsOption isSplitStringPl (Location(name, statements)) : ShowS list =
        [
            yield showString "# " << showString name
            yield!
                statements
                |> List.collect (
                    Statement.Printer.showStmt indentsOption isSplitStringPl
                )
            yield showString (sprintf "--- %s ----------" name)
        ]
