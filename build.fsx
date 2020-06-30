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
let testProjPath = f testProjName
let mainProjName = "QspServer"
let mainProjPath = f mainProjName
// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
open Fake.DotNet
let buildConf = DotNet.BuildConfiguration.Debug
let dotnetSdk = lazy DotNet.install DotNet.Versions.FromGlobalJson
let inline dtntSmpl arg = DotNet.Options.lift dotnetSdk.Value arg
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

let run projName projPath =
    let dir = Fake.IO.Path.getDirectory projPath
    let localpath = sprintf "bin/%A/net461/%s.exe" buildConf projName
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

Target.create "RunMainProj" (fun _ ->
    run mainProjName mainProjPath |> ignore
)

Target.create "TrimTrailingWhitespace" (fun _ ->
    // по-хорошему, нужно использовать .gitignore, но и так пока сойдет
    let files =
        !! "**/*.fs"
        ++ "**/*.fsx"
        ++ "**/*.fsproj"
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

"BuildTest"
  ==> "RunTest"
  ==> "RunMainProj"

Target.runOrDefault "RunMainProj"
