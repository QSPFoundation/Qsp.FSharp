#I @"e:\Project\FsharpMyExtension\FsharpMyExtension\FsharpMyExtension\bin\Debug\net461\"
#r @"FParsecCS.dll"
#r @"FParsec.dll"
#r @"Fuchu.dll"
#r @"HtmlAgilityPack.dll"
#r @"Newtonsoft.Json.dll"
#r @"Newtonsoft.Json.Bson.dll"
#r @"FsharpMyExtension.dll"



let parsingP p = run p >> printfn "%A"

// parsingP pexpr ""
parsingP pstmt "a = obj 's'"
parsingP pstmt "a =- 10"
parsingP pstmt "a = a + 10"
parsingP pstmt "if c:
smt
act arg: pl
elseif c2:
if a: k else pre if cond: d  elseif celif: d
stmt2
end"

parsingP pstmt "if x:
s = {
s
}
end"
parsingP pstmt "if a: k else pre if cond: d  elseif celif: d"

parsingP pstmt "IF e:
stmt1 & stmt
elseif e2 :
    stmt2
    stmt3
elseif e3 :
    end2
    if e4 :
        stmt5
    elseif e5:
        tm
        elsei
    end
end
"

parsingP pstmt "if k:
wear += 1
end"

//System.Text.Encoding.Default
//System.Text.Encoding.UTF8
run (many ploc) (System.IO.File.ReadAllText(@"e:\Disc D\All\It\DefaultBox\drive\C\All2\Games\GamesSourceCode\etoEdit.txt", System.Text.Encoding.UTF8))
|> ignore
//|> fun x -> System.IO.File.WriteAllText(@"e:\res.txt", sprintf "%A" x)
run (many ploc) (System.IO.File.ReadAllText(@"e:\Disc D\All\It\DefaultBox\drive\C\All2\Games\GamesSourceCode\al.txt"))
|> ignore
//|> fun x -> System.IO.File.WriteAllText(@"e:\res.txt", sprintf "%A" x)

let str = System.IO.File.ReadAllText(@"e:\Disc D\All\It\DefaultBox\drive\C\All2\Games\GamesSourceCode\destiny 0.5.txt", System.Text.Encoding.Default)
let res = run (many ploc) str
let test2 =
    let f x =
        let s = printState x |> show
        if s |> run pstmt = x then None
        else Some(s)

    List.choose (function Location(name, stmts) -> match List.choose f stmts with [] -> None | xs -> Some(name, xs)) res

res |> fun x -> System.IO.File.WriteAllText(@"e:\res.txt", printLocs x)

//System.IO.File.WriteAllLines(@"e:\res2.txt", test2)
