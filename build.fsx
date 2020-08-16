// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref build //"
#load ".fake/build.fsx/intellisense.fsx"
open Fake.IO.Globbing.Operators
open Fake.Core
// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------
let f projName =
    let pattern = sprintf @"**/%s.fsproj" projName
    let xs = !! pattern
    xs
    |> Seq.tryExactlyOne
    |> Option.defaultWith (fun () ->
        xs
        |> List.ofSeq
        |> failwithf "'%s' expected exactly one but:\n%A" pattern
    )
let testProjName = "Test"
let testProjPath = @"Test/Test.fsproj"
let serverProjName = "QspServer"
let parserProjName = "QSParse"
let serverProjPath = f serverProjName
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet
let buildConf = DotNet.BuildConfiguration.Release
let dotnetSdk = lazy DotNet.install DotNet.Versions.FromGlobalJson
let inline dtntSmpl arg = DotNet.Options.lift dotnetSdk.Value arg
let targetFrameworks = ["net461"; "netcoreapp3.1"]
// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
Target.create "BuildServer" (fun _ ->
    serverProjPath
    |> Fake.IO.Path.getDirectory
    |> DotNet.build (fun x ->
        { x with Configuration = buildConf }
        |> dtntSmpl)
)

Target.create "BuildTest" (fun _ ->
    testProjPath
    |> Fake.IO.Path.getDirectory
    |> DotNet.build (fun x ->
        { x with Configuration = buildConf }
        |> dtntSmpl)
)

Target.create "Copy3rd" <| fun _ ->
    let srcDir = @"3rd"
    if not <| System.IO.Directory.Exists srcDir then
        failwithf "'%s' not found" srcDir
    targetFrameworks
    |> List.iter (fun targetFramework ->
        let localPath = sprintf "bin/%A/%s" buildConf targetFramework
        let dstDir = sprintf "%s/%s/%s" serverProjName localPath srcDir
        Fake.IO.Shell.copyDir dstDir srcDir (fun _ -> true)
    )
let run projName targetFramework projPath =
    let dir = Fake.IO.Path.getDirectory projPath
    let localpath = sprintf "bin/%A/%s/%s.exe" buildConf targetFramework projName
    let path = Fake.IO.Path.combine dir localpath
    if not <| Fake.IO.File.exists path then
        failwithf "not found %s" path

    Command.RawCommand(path, Arguments.Empty)
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory (Fake.IO.Path.getDirectory path)
    |> Proc.run

Target.create "RunTest" (fun _ ->
    let targetFramework = targetFrameworks.[0]
    let x = run testProjName targetFramework testProjPath
    if x.ExitCode <> 0 then
        failwith "test error"
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

Target.create "CopyToMainProj" (fun _ ->
    let srcDir = sprintf @"QspServer/bin/%A" buildConf
    let dstDir = @"e:/Project/Qsp/QspVscodeExtension/release/bin"
    Fake.IO.Shell.copyDir dstDir srcDir (fun _ -> true)
)

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

Target.create "Default" ignore

"BuildServer"
  ==> "Copy3rd"
  ==> "Default"

"BuildTest"
  ==> "Copy3rd"
  ==> "CopyToMainProj"
  ==> "RunTest"
Target.runOrDefault "Default"
