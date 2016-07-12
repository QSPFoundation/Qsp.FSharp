#if INTERACTIVE
#r @"c:\All\Project\Parsers\QSParse\QSParse\bin\Debug\QSParse.exe"
#endif
open QS

module codeTools = 
    let filter func statemetns =
        let rec f acc = function
            | [] -> acc |> List.rev
            | h::t ->
                if func h then f (h::acc) t
                else
                    match h with
                    | Act(x, xs) ->
                        match f [] xs with
                        | [] -> f acc t
                        | xs -> f (Act(x, xs)::acc) t
                    | AssertCode(e, xs) ->
                        match f [] xs with
                        | [] -> f acc t
                        | xs -> f (AssertCode(e, xs)::acc) t
                    | If(x, xs, ys) ->
                        let xs = f [] xs
                        let ys = f [] ys
                        if List.isEmpty xs && List.isEmpty ys then f acc t
                        else f (If(x, xs, ys)::acc) t
                    | _ -> f acc t
        f [] statemetns    
    let filterSimple func statemetns =
        let rec f acc = function
            | [] -> acc |> List.rev
            | h::t ->
                match func h with
                | Some x -> f (x::acc) t
                | None ->
                    match h with
                    | Act(x, xs) -> f (f acc xs) t
                    | AssertCode(e, xs) -> f (f acc xs) t
                    | If(x, xs, ys) -> f (f (f acc xs) ys) t
                    | _ -> f acc t
        f [] statemetns

let p = Program.parseFile @"game.txt"
let filter f = 
    let f = codeTools.filter f
    let res = 
        List.choose (function Location(name, xs) -> 
                              match f xs with [] -> None | xs -> Location(name, f xs) |> QS.printLoc |> Some) p
    System.IO.File.WriteAllLines(@"c:\All\Project\Parsers\QSParse\QSParse\bin\Debug\output.txt", res)

filter (function FuncS(name, _) -> Set.contains name (set["gt"; "goto"; "xgt"; "xgoto"; "gs"; "gosub"]) | _ -> false)
filter (function FuncS("gt",Val (String "office")::_) -> true | _ -> false)
filter (function Assert(_, _) -> true | _ -> false)

Program.parse "'some text' + var + 'some text2'\r\n"
let res2 f = List.choose (function Location(name, xs) -> match codeTools.filterSimple f xs with [] -> None | xs -> Some xs) p

res2 (function StringS _ as x -> Some x | ExprS _ as x -> Some x | _ -> None)
res2 (function AssertCode(name, _) -> AssertCode(name, []) |> Some | _ -> None) |> List.concat
|> Seq.groupBy id
|> Seq.map (fun (key, v) -> key, Seq.length v) |> List.ofSeq |> List.filter (snd >> ((<)2))
