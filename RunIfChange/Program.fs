open System.Text
open System.Diagnostics
/// Запускаем новый процесс. Получаем кортеж (код_завершения, stdout)
let shellExecute program arguments =
    let startInfo = new ProcessStartInfo()
    startInfo.FileName  <- program
    startInfo.Arguments <- arguments
    startInfo.UseShellExecute        <- false
    startInfo.RedirectStandardOutput <- true
    let proc = new Process()
    proc.EnableRaisingEvents <- true
    // Добавить обработчик события ‘OutputDataRecieved’, чтобы можно было
    // сохранить поток STDOUT процесса.
    let driverOutput = new StringBuilder()
    proc.OutputDataReceived.AddHandler(
        DataReceivedEventHandler(
            (fun sender args -> driverOutput.AppendLine(args.Data) |> ignore)
        )
    )
    proc.StartInfo <- startInfo
    proc.Start() |> ignore

    proc.BeginOutputReadLine()
    proc.WaitForExit()
    (proc.ExitCode, driverOutput.ToString())

open System.IO
let dirwork = Path.Combine(System.Environment.CurrentDirectory, "temp")

[<EntryPoint>]
let main args = 
    ///<summary>Сравнивает два файла</summary>
    let compare path1 path2 = File.ReadAllBytes path1 = File.ReadAllBytes path2
    let f prog path args =
        let run () = 
            let args = System.String.Join(" ", path::args)
            shellExecute prog args
        if not <| File.Exists path then (0, sprintf "%s not exist" path)
        else
            let file' = Path.GetFileNameWithoutExtension path + ".fs"
            let dir = Path.GetDirectoryName path
            let path' = Path.Combine(dir, file')
            let file = Path.Combine(dirwork, Path.GetFileName path)
            (*
            let f cond else' then' = if cond() then then' () else else'(); File.Copy(path, file, true); run()
            f (fun () -> File.Exists path') (fun () -> printfn "not exist compile file") (fun () ->
            f (fun () -> File.Exists file)  (fun () -> printfn "not cache %s" file) (fun () ->
            f (fun () -> compare file path) (fun () -> printfn "changed") (fun () -> (0, sprintf "not change %s" file)))) *)
            let f func = File.Copy(path, file, true); run()
            if File.Exists path' then
                if File.Exists file  then
                    if compare file path then (0, sprintf "not change %s" file)
                    else printfn "changed"; f()
                else printfn "not cache %s" file; f()
            else printfn "not exist compile file"; f()

    match args |> List.ofArray with
    | "fslex.exe" as prog::path::args -> f prog path args
 //       let path = @"C:\All\Project\Parsers\QSParse\QSParse\QSLexer.fsl"
//        let arguments = @"""C:\All\Project\Parsers\QSParse\QSParse\QSLexer.fsl"" --unicode"
    | "fsyacc.exe" as prog::path::args -> f prog path args
    | _ -> 1, sprintf "cmd error"
    |> function code, stdout -> printfn "%s" stdout; code