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
let mainProjName = "QSParse"
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

open Fake.IO
// TODO: скачать и распаковать http://qsp.su/attachments/txt2gam011.zip в "Utils\txt2gam"
let compilerPath =
    Lazy.Create (fun _ ->
        let compilerPath = "Utils/txt2gam/txt2gam.exe"
        if System.IO.File.Exists compilerPath then compilerPath
        else
            failwithf "Compiler not found at '%A'" compilerPath
    )
let compiler src =
    let dst = Path.changeExtension ".qsp" src

    Command.RawCommand(compilerPath.Value, Arguments.ofList [src; dst])
    |> CreateProcess.fromCommand
    |> CreateProcess.withWorkingDirectory (Path.getDirectory compilerPath.Value)
    |> Proc.run
    |> fun x -> x.ExitCode

Target.create "Watch" (fun _ ->
     use watcher =
        !! "Utils/txt2gam/*.qsps"
        |> Fake.IO.ChangeWatcher.run (fun changes ->
            changes
            |> Seq.iter (fun x ->
                Trace.trace "Compilation..."
                let code = compiler x.FullPath
                Trace.trace (sprintf "Compilation completed with code %d" code)
            )
     )

     System.Console.ReadLine() |> ignore

     watcher.Dispose() // по-идеи, и так должен выгрузиться
)


// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
open Fake.Core.TargetOperators

"BuildTest"
  ==> "RunTest"
  ==> "RunMainProj"
Target.runOrDefault "RunMainProj"
