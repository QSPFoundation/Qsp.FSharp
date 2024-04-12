// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref build //"
#load "./.fake/build.fsx/intellisense.fsx"
#r "netstandard"

open Fake.Core
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators

// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------
let testProjName = "Test"
let testProjPath = "Test" </> sprintf "%s.fsproj" testProjName
let serverProjName = "QspServer"
let serverProjPath = "QspServer" </> sprintf "%s.fsproj" serverProjName
let parserProjName = "Qsp.FSharp.Core"
let parserProjPath = "src" </> "Qsp.FSharp.Core" </> sprintf "%s.fsproj" parserProjName
let utilityProjName = "Utility"
let utilityProjpath = "Utility" </> sprintf "%s.fsproj" utilityProjName
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet

let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

let targetFrameworks = ["net461"; "netcoreapp3.1"]
// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------

let commonBuildArgs =
    [
        yield "-c Release"
        if not Environment.isWindows then
            yield "--framework netcoreapp3.1"
    ]
    |> String.concat " "

let dotnetBuild =
    dotnet (sprintf "build %s" commonBuildArgs)

let dotnetRun framework =
    dotnet (sprintf "run --framework %s" framework)

Target.create "BuildServer" (fun _ ->
    serverProjPath
    |> Fake.IO.Path.getDirectory
    |> dotnetBuild
)

Target.create "BuildTest" (fun _ ->
    testProjPath
    |> Fake.IO.Path.getDirectory
    |> dotnetBuild
)

Target.create "BuildUtility" (fun _ ->
    utilityProjpath
    |> Fake.IO.Path.getDirectory
    |> dotnetBuild
)

Target.create "RunTest" (fun _ ->
    dotnetRun "netcoreapp3.1" (Path.getDirectory testProjPath)
)

Target.create "TrimTrailingWhitespace" (fun _ ->
    // по-хорошему, нужно использовать .gitignore, но и так пока сойдет
    let files =
        !! "**/*.fs"
        ++ "**/*.fsx"
        ++ "**/*.fsproj"
        ++ "**/*.cs"
        ++ "**/*.csproj"
        -- "**/obj/**"
        -- "**/paket-files/**"
        -- "**/packages/**"
    files
    |> Seq.iter (fun path ->
        System.IO.File.ReadAllLines path
        |> Array.map (fun x -> x.TrimEnd())
        |> fun content -> System.IO.File.WriteAllLines(path, content)
    )
)

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

Target.create "Default" ignore

"BuildServer"
  ==> "Default"

// "BuildServer" <=> "BuildTest"

// Copy3rd запускает и BuildServer, и BuildTest. Шо делать, чтобы он отдельно запускал?
Target.runOrDefault "Default"
