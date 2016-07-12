module Program
open System
open System.IO

let genOp () = 
    #if INTERACTIVE
        #r "C:\Program Files\FSharpPowerPack-2.0.0.0\\bin\FSharp.PowerPack.dll"
        #load "QS.fs"
        #load "QSParser.fs"
        #load "QSLexer.fs"
    #endif
    let join sep (xs:string list) = System.String.Join(sep, xs)
    QSLexer.ops |> Map.toList |> List.map (fun (s, _) -> "\"" + s + "\"") |> join " | " |> printfn "%s"

let parseFile path =
    let read = File.ReadAllText path
    let lexbuf = Lexing.LexBuffer<_>.FromString read
    QSParser.start QSLexer.tokenize lexbuf

let parse (str:string) =
    let stri = string str // так надо, без нее f# interactive выдает нечто непонятное
    let lexbuf = Lexing.LexBuffer<char>.FromString str
    QSParser.parseStatements QSLexer.tokenize lexbuf

let lextest path = 
    let read = File.ReadAllText path
    let listTokens = System.Collections.Generic.List<(QSParser.token*int)>(10000)
    let lexbuf = Lexing.LexBuffer<_>.FromString read
    //let string_buff = new System.Text.StringBuilder(4000)

    let rec f currIdx =
        let exnf (e:exn) =
            let count = 15
            let lower = let n = currIdx - count - 1 in if n < 0 then 0 else n
            let xs = listTokens.GetRange(lower, currIdx - lower)
            xs |> Seq.iter (printfn "%A")
            printfn "%s" e.Message
            printfn "line %d" lexbuf.StartPos.Line
            printfn "%A" lexbuf.Lexeme
            failwithf "%A" e        
        if not lexbuf.IsPastEndOfStream then
            try
                (QSLexer.tokenize lexbuf, lexbuf.StartPos.Line) |> listTokens.Add
            with
            | :? QSLexer.Lexical_error as e -> exnf e
            | e -> exnf e
            f (currIdx + 1)
    f 0
    printfn "lex ok"

let rec y () = 
    printfn "parse"
    let read = File.ReadAllText("input")
    let lexbuf = Lexing.LexBuffer<_>.FromString read
    try
        QSParser.start QSLexer.tokenize lexbuf
    with | e -> 
        printfn "%A" e.Message
        let x = System.Console.ReadKey()
        y()

let writer = new StreamWriter("output")
let writen t s = let s:string = sprintf t s in writer.WriteLine s

QS.printLocs (y()) |> writer.Write
writer.Close()

printfn "Done!"
Console.ReadKey () |> ignore