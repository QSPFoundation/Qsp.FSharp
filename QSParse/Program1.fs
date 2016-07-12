open System
open System.IO
open System.Text.RegularExpressions

type Statement =
    | If of string * Statement list * Statement list option 
    | Act of string * Statement list
    | End
    | Other of string

let test = [If ("true", [Other "dosomeshit"; Act("постучать", [Other "do"])], None); Other "do nothing"]

let parseExpr str = 
    let res = Regex.Match(str, "(.*?):")
    if res.Success then res else failwith "kjf"

let find str pattern =
    let m = Regex.Match(str, pattern)
    if m.Success then
        Some(m.Value), str.[m.Length + m.Index .. str.Length - 1]
    else
        None, str

let finds str pattern =
    let m = Regex.Match(str, pattern, RegexOptions.Singleline)
    if m.Success then
        Some(m.Value), str.[m.Length + m.Index .. str.Length - 1]
    else
        None, str

//let testFind = Regex.Match(rest, ".*end", RegexOptions.Singleline).Value

printfn ""
(*
let rec parseStr = 
    function
    | "" -> []
    | str -> 
            match find str "(?<=\s)[\*a-zA-Z0-9_]+?(?=[\'|\(|\s|\"])" with
            | Some(statement), rest ->
                let parseBody () = 
                    match find rest "(.*?):" with
                    | Some(exprstr), rest ->
                        match find rest "^\s*?\n" with // проверка на однострочность конструкции if
                        | Some(bodyMatch), rest ->
                            match finds rest ".*?(?=end)" with
                            | Some(body), rest ->
                                If(exprstr, parseStr body, None) :: (parseStr rest)
                            | None, _ -> failwithf "не совпадает end! %s" rest
                        | None, rest -> 
                            match find rest ".*" with
                            | Some(bodyMatch), rest -> If(exprstr, [Other(bodyMatch)], None) :: (parseStr rest)
                            | None, rest -> failwithf ".* случилось невозможное! %s" rest
                    | None, rest -> failwithf "(.*?): Syntax error %s" rest

                if statement = "if" then
                    parseBody ()
                elif statement = "act" then
                    parseBody ()
                else
                    match find rest ".+?\n" with
                    | Some(args), rest -> Other(statement + args)::(parseStr rest)
                    | None, rest -> Other(statement)::(parseStr rest)
            | None, _ -> []
*)

let f rest = 
    let rec parseStr2 = 
                function
                | [] -> None, []
                | statement::rest ->
                    let parseBody () = 
                            let restend = ref rest
                            let rec f' =
                                    function
                                    | None, _ -> failwithf "end not found %A" !restend
                                    | Some( End ), rest -> restend := rest; []
                                    | Some( state ), rest -> state :: (f' (parseStr2 rest))
                            If ("", f'( parseStr2 rest ), None )
                            |> Some, !restend

                    match statement with
                    | "if" -> parseBody ()
                    //| "act" -> 
                    | "end" -> Some(End), rest
                    | _ -> Some(Other statement), rest

    let rec f' = function
                | None, _ -> []
                | Some( End ), rest -> failwithf "какой-то левый end нарисовался %A" rest
                | Some( state ), rest -> state :: (f' (parseStr2 rest))
    in f'( parseStr2 rest )

let prog = ["p1"; "p2"; "if"; "p3"; "p4"; "if"; "p5"; "p6"; "end"; "p7"; "end"; "p8"]

let res = f prog

//let res = parseStr2 (File.ReadAllText("tryparse.txt", Text.Encoding.Default))

let progRaw = File.ReadAllText("tryparse.txt", Text.Encoding.Default)

type Tokens =
    | Word of string
    | Other of string


let parseWord input =
    let tor = 
        [("word", let word = @"[a-zA-Zа-яА-я]" in String.Format(@"(\${0}s|{0})+.*?({0}|\d*)*", word));
        ("whitespace", @"[\s\t]");
        ("newline", @"(\n|\r\n)");
        ("str", @"'(?:\\.|[^'])*'");
        ("jumpsign", @"#.*?" + @"(\n|\r\n)");
        ("minuses", @"-{3,}");
        ("tildes", @"~+");
        ("defloc", @"Название локации:");
        ("descloc", @"Описание локации:");
        ("endloc", @"Конец локации:");]

    let word = 
        let word = @"[a-zA-Zа-яА-я]"
        //@"(\$[a-zA-Zа-яА-я]|[a-zA-Zа-яА-я])+.*?([a-zA-Zа-яА-я]|\d*)*" //@"[[:word:]]+"
        String.Format(@"(\${0}s|{0})+.*?({0}|\d*)*", word)
    let whitespace = @"[\s\t]"
    let newline = @"(\n|\r\n)"
    let str = @"'(?:\\.|[^'])*'"
    let jumpsign = @"#.*?" + newline
    let minuses = @"-{3,}"
    let tildes = @"~+"
    let defloc = @"Название локации:"
    let descloc = @"Описание локации:"
    let endloc = @"Конец локации:"

    let toks = [ defloc; descloc; endloc; word; whitespace; newline; str; jumpsign; minuses; tildes; ]
               |> List.map (fun x -> "^" + x)
    
    let tryMatch str pattern = 
            let m = Regex.Match(str, pattern, RegexOptions.Singleline)
            if m.Success then
                Some(m.Value, str.[m.Length + m.Index .. str.Length - 1])
            else
                None
    
    let rec s' = function
        | "" -> []
        | str ->
            let rec f = function
                    | [] -> (str.[0].ToString(), "unknown"), (str.[1..str.Length-1])
                    | h::t -> match tryMatch str h with
                              | Some(finded, rest) -> (finded, h), rest
                              | None -> f t
            f toks
            |> (function h, rest -> h::s' rest)
    s' input

let res2 = parseWord progRaw
res2
|> List.iter (function a, b -> printfn "%s %s" a b)

printf "Done!"
Console.ReadKey() |> ignore