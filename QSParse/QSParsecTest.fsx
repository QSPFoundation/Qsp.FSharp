module QSParsecTest

#if INTERACTIVE
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsecCS.dll"
#r @"..\packages\FParsec.1.0.2\lib\net40-client\FParsec.dll"
#endif

#if INTERACTIVE
#load "reflect.fs"
#load "QSAST.fs"
#load "show.fs"
#endif
#if INTERACTIVE
#load "QSParsec.fs"
#endif
open FParsec
open QSParsec
open QSAST
open Show

let parsingP p = parsing p >> printfn "%A"

parsingP pexpr "sprintchance =< nochance" // sprintchance =< (no chance)
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
parsing (many ploc) (System.IO.File.ReadAllText(@"e:\Disc D\All\It\DefaultBox\drive\C\All2\Games\GamesSourceCode\etoEdit.txt", System.Text.Encoding.UTF8))
|> ignore
//|> fun x -> System.IO.File.WriteAllText(@"e:\res.txt", sprintf "%A" x)
parsing (many ploc) (System.IO.File.ReadAllText(@"e:\Disc D\All\It\DefaultBox\drive\C\All2\Games\GamesSourceCode\al.txt"))
|> ignore
//|> fun x -> System.IO.File.WriteAllText(@"e:\res.txt", sprintf "%A" x)


let s = parsing pstmt "k = k + 1" //"a = a = (no a) > b"
let s' = parsing pstmt "a = a = no (a > b)"
let test p s =
    let p = parsing p
    let before = p s
    let after = printState before |> show |> p
    if  after <> before then failwithf "before:\n%A\nafter:\n%A" before after
test pstmt "a = a = no -a > b"
parsingP pstmt "asdf obj 'Персонаж'"
parsingP pstmt "a = pstam> (pmaxstam/4)*2 and pstam <= (pmaxstam/4)*3"
test pstmt "php -= 3*emdmg*2 - parm"
parsing pstmt """php =+ 3*emdmg*2 - parm"""
|> printState |> show |> printfn "%A"
printState s |> show |> printfn "%s"
printState s' |> show |> printfn "%s"

let str = System.IO.File.ReadAllText(@"e:\Disc D\All\It\DefaultBox\drive\C\All2\Games\GamesSourceCode\destiny 0.5.txt", System.Text.Encoding.Default)
let res = parsing (many ploc) str
let test2 =
    let f x = 
        let s = printState x |> show
        if s |> parsing pstmt = x then None
        else Some(s)

    List.choose (function Location(name, stmts) -> match List.choose f stmts with [] -> None | xs -> Some(name, xs)) res

res |> fun x -> System.IO.File.WriteAllText(@"e:\res.txt", printLocs x)

//System.IO.File.WriteAllLines(@"e:\res2.txt", test2)