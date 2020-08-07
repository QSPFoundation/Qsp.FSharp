// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "./packages/build/FAKE/tools/FakeLib.dll"
open Fake.IO.Globbing.Operators
open Fake.Core
// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------
let f projName =
    let pattern = sprintf @"**\%s.fsproj" projName
    let xs = !! pattern
    xs
    |> Seq.tryExactlyOne
    |> Option.defaultWith (fun () ->
        xs
        |> List.ofSeq
        |> failwithf "'%s' expected exactly one but:\n%A" pattern
    )
let testProjName = "Test"
let testProjPath = @"Test\Test.fsproj"
let mainProjName = "QspServer"
let mainProjName2 = "QSParse"
let mainProjPath = f mainProjName
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet
let buildConf = DotNet.BuildConfiguration.Release
let dotnetSdk = lazy DotNet.install DotNet.Versions.FromGlobalJson
let inline dtntSmpl arg = DotNet.Options.lift dotnetSdk.Value arg
let targetFramework = "netcoreapp3.1"
// --------------------------------------------------------------------------------------
// Targets
// --------------------------------------------------------------------------------------
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
    let localPath = sprintf "bin/%A/%s" buildConf targetFramework
    let dstDir = sprintf "%s/%s/%s" mainProjName localPath srcDir
    // printfn "%s\n%s" srcDir dstDir
    Fake.IO.Shell.copyDir dstDir srcDir (fun _ -> true)

let run projName projPath =
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
    let x = run testProjName testProjPath
    if x.ExitCode <> 0 then
        raise <| Fake.Testing.Common.FailedTestsException "test error"
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
    let srcDir = sprintf @"QspServer\bin\%A\%s" buildConf targetFramework
    let dstDir = @"e:\Project\Qsp\QspVscodeExtension\release\bin"
    Fake.IO.Shell.copyDir dstDir srcDir (fun _ -> true)
)

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

"BuildTest"
  ==> "Copy3rd"
//   ==> "CopyToMainProj"
  ==> "RunTest"
Target.runOrDefault "RunTest"
