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
let testProjName = "Qsp.FSharp.Core.Tests"
let testProjPath = "tests" </> "Qsp.FSharp.Core.Tests" </> sprintf "%s.fsproj" testProjName
let serverProjName = "Qsp.FSharp.LanguageServer"
let serverProjDir = "src" </> "Qsp.FSharp.LanguageServer"
let serverProjPath = serverProjDir </> sprintf "%s.fsproj" serverProjName
let parserProjName = "Qsp.FSharp.Core"
let parserProjDir = "src" </> "Qsp.FSharp.Core"
let parserProjPath = parserProjDir </> sprintf "%s.fsproj" parserProjName
let utilityProjDir = "src" </> "Qsp.FSharp.Cli"
let utilityProjName = "Qsp.FSharp.Cli"
let utilityProjPath = utilityProjDir </> sprintf "%s.fsproj" utilityProjName
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet

let dotnet cmd workingDir =
    let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
    if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

module XmlText =
    let escape rawText =
        let doc = new System.Xml.XmlDocument()
        let node = doc.CreateElement("root")
        node.InnerText <- rawText
        node.InnerXml

let targetFrameworks = ["net461"; "netcoreapp3.1"]
// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------

let commonBuildArgs =
    [
        "-c Release"
    ]
    |> String.concat " "

let dotnetBuild =
    let commonBuildArgs =
        [
            yield commonBuildArgs
            if not Environment.isWindows then
                yield "--framework netcoreapp3.1"
        ]
        |> String.concat " "
    dotnet (sprintf "build %s" commonBuildArgs)

let dotnetRun framework =
    dotnet (sprintf "run --framework %s" framework)

Target.create "CoreMeta" (fun _ ->
    let release = ReleaseNotes.load (parserProjDir </> "RELEASE_NOTES.md")

    [
        "<Project xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">"
        "<ItemGroup>"
        "    <PackageReference Include=\"Microsoft.SourceLink.GitHub\" Version=\"1.0.0\" PrivateAssets=\"All\"/>"
        "</ItemGroup>"
        "<PropertyGroup>"
        "    <EmbedUntrackedSources>true</EmbedUntrackedSources>"
        "    <PackageProjectUrl>https://github.com/gretmn102/FParserQSP/tree/master/src/Qsp.FSharp.Core</PackageProjectUrl>"
        "    <PackageLicenseExpression>MIT</PackageLicenseExpression>"
        "    <RepositoryUrl>https://github.com/gretmn102/FParserQSP.git</RepositoryUrl>"
        sprintf "    <PackageReleaseNotes>%s</PackageReleaseNotes>"
            (String.concat "\n" release.Notes |> XmlText.escape)
        "    <PackageTags>fsharp;qsp;parser</PackageTags>"
        "    <Authors>gretmn102</Authors>"
        sprintf "    <Version>%s</Version>" release.SemVer.AsString
        "</PropertyGroup>"
        "</Project>"
    ]
    |> File.write false (parserProjDir </> "Directory.Build.props")
)

Target.create "CorePack" (fun _ ->
    let commonBuildArgs =
        [
            yield commonBuildArgs
            if not Environment.isWindows then
                yield "-p:TargetFrameworks=netstandard2.0"
        ]
        |> String.concat " "
    dotnet (sprintf "pack %s" commonBuildArgs) parserProjDir
)

Target.create "CorePushToGitlab" (fun _ ->
    let release = ReleaseNotes.load (parserProjDir </> "RELEASE_NOTES.md")
    let deployDir = parserProjDir </> "bin" </> "Release"
    let packPath =
        sprintf "Qsp.FSharp.Core.%s.nupkg" release.SemVer.AsString
    deployDir
    |> dotnet (sprintf "nuget push -s %s %s" "gitlab" packPath)
)

Target.create "LanguageServerMeta" (fun _ ->
    let release = ReleaseNotes.load (serverProjDir </> "RELEASE_NOTES.md")

    [
        "<Project xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">"
        "<ItemGroup>"
        "    <PackageReference Include=\"Microsoft.SourceLink.GitHub\" Version=\"1.0.0\" PrivateAssets=\"All\"/>"
        "</ItemGroup>"
        "<PropertyGroup>"
        "    <PackageId>Qsp.FSharp.LanguageServer</PackageId>"
        "    <EmbedUntrackedSources>true</EmbedUntrackedSources>"
        "    <PackageProjectUrl>https://github.com/gretmn102/FParserQSP/tree/master/src/Qsp.FSharp.LanguageServer</PackageProjectUrl>"
        "    <PackageLicenseExpression>MIT</PackageLicenseExpression>"
        "    <RepositoryUrl>https://github.com/gretmn102/FParserQSP.git</RepositoryUrl>"
        sprintf "    <PackageReleaseNotes>%s</PackageReleaseNotes>"
            (String.concat "\n" release.Notes |> XmlText.escape)
        "    <PackageTags>fsharp;qsp;lsp</PackageTags>"
        "    <Authors>gretmn102</Authors>"
        sprintf "    <Version>%s</Version>" release.SemVer.AsString
        "</PropertyGroup>"
        "</Project>"
    ]
    |> File.write false (serverProjDir </> "Directory.Build.props")
)

Target.create "LanguageServerPack" (fun _ ->
    let commonBuildArgs =
        [
            yield commonBuildArgs
            // If you use netcoreapp3.1, then Core will also try
            // to compile to that framework, which will cause errors
            if not Environment.isWindows then
                yield "-p:TargetFrameworks=netstandard2.0"
        ]
        |> String.concat " "
    dotnet (sprintf "pack %s" commonBuildArgs) serverProjDir
)

Target.create "LanguageServerPushToGitlab" (fun _ ->
    let release = ReleaseNotes.load (serverProjDir </> "RELEASE_NOTES.md")
    let deployDir = serverProjDir </> "bin" </> "Release"
    let packPath =
        sprintf "Qsp.FSharp.LanguageServer.%s.nupkg" release.SemVer.AsString
    deployDir
    |> dotnet (sprintf "nuget push -s %s %s" "gitlab" packPath)
)

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

Target.create "CliBuild" (fun _ ->
    utilityProjPath
    |> Fake.IO.Path.getDirectory
    |> dotnetBuild
)

Target.create "TestsRun" (fun _ ->
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

"CoreMeta"
  ==> "CorePack"
  ==> "CorePushToGitlab"

"LanguageServerMeta"
  ==> "LanguageServerPack"
  ==> "LanguageServerPushToGitlab"

"BuildServer"
  ==> "Default"

// "BuildServer" <=> "BuildTest"

// Copy3rd запускает и BuildServer, и BuildTest. Шо делать, чтобы он отдельно запускал?
Target.runOrDefault "Default"
