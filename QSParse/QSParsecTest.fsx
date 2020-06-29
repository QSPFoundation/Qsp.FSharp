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




let printState =
    let showValue = function
        | Int x -> shows x //| Float x -> x.ToString()
        | String x -> bet "'" "'" (x.Replace("'", "''") |> showString)
    let ops = Op.toString >> showString
//    let ops = function
//        | Plus -> "+" | Minus -> "-" | Times -> "*" | Divide -> "/"
//        | Eq -> "=" | Gt -> ">" | Ge -> ">=" | Lt -> "<" | Le -> "<=" | Ne -> "<>"   // =, >, >=, <, <=, (<>|!)
//        | And -> "and" | Or -> "or" | Mod -> "mod"
    let unar = function No -> "no" | Obj -> "obj" | Neg -> "-"
    let rec showExpr = function
        | Val v -> showValue v
        | Var v -> showString v
        | Func(name, es) -> showString name << showParen true (List.map showExpr es |> joinS ", ")
        | UnarExpr(op, e) ->
            let space = function Obj | No -> showChar ' ' | Neg -> id
            showString (unar op) << space op << showExpr e
        //| Expr(op, e1, e2) -> showExpr e1 << showChar ' ' << showString(ops op) << showChar ' ' << showExpr e2
        | Expr(op, e1, e2) -> 
            let prec = Precedences.OpB >> Precedences.prec
            let f = function
                | Expr(op', _, _) -> showParen (prec op > prec op')
                | UnarExpr _ -> showParen true | _ -> id
            let f x = f x (showExpr x)
            f e1 << showChar ' ' << ops op << showChar ' ' << f e2
        | Arr(name, es) -> showString name << bet "[" "]" (List.map showExpr es |> joinS ", ")
    let showAssign = function
        | Assign.AssignArr(nameVar, expr) -> showString nameVar << bet "[" "]" (showExpr expr)
        | Assign.AssignVar name -> showString name
    
    let (|OneStmt|_|) = function
        | [x] -> 
            match x with
            | Assign _ | CallSt _ | StarPl _ | Comment _ -> Some x
            | AssingCode _ -> None // спорно
            | Act _ | If _ -> None
            | Sign _ -> None // эту нечисть нужно как можно более нагляднее подчеркнуть. Да странно будет, если она окажется одна в списке инструкций.
        | _ -> None
    
    //let (OneStmt x) = [ parsing pstmt "a = 1"; ]
    let (|AssingName|) = function AssignArr(x, _) -> x | AssignVar x -> x
    let tabss n = Show.replicate n '\t'
    let rec state tabs xs = 
        let f = function [] -> nl | xs -> nl << joinS "\n" (List.map (state <| tabs + 1) xs)

        let indent = nl << tabss tabs : ShowS
        let rec f' = function
            | Assign(AssingName name' as ass, Expr((Plus|Minus) as op, Var name, e)) when name' = name -> 
                showAssign ass << showChar ' ' << ops op << showString "= " << showExpr e
            | Assign(AssingName name' as ass, Expr((Plus|Minus) as op, e, Var name)) when name' = name -> 
                showAssign ass << showString " =" << ops op << showChar ' ' << showExpr e
            | Assign(ass, e) -> showAssign ass << showString " = " << showExpr e
            | CallSt(name, es) -> showString name << showChar ' ' << (List.map showExpr es |> joinS ", ")
            | StarPl e -> showExpr e
            | Sign s -> showChar ':' << showString s
            | If(e, body, elseBody) -> 
                let ifBegin e = showString "if " << showExpr e << showChar ':'
                let body = 
                    match body, elseBody with
                    | OneStmt x, OneStmt y -> showChar ' ' << f' x << showString " else " << f' y
                    | OneStmt x, [] -> showChar ' ' << f' x
                    //| [CallSt _ as x], [CallSt _ as y] -> showChar ' ' << f' x << showString " else " << f' y
                    //| [Assign _ as x], [] | [CallSt _ as x], [] | [StarPl _ as x], [] -> showChar ' ' << f' x
                    | xs, ys -> 
                        let rec els xs =
                            if List.isEmpty xs then id
                            else 
                                let body = function [If(e, xs, ys)] -> ifBegin e << f xs << els ys | xs -> f xs
                                indent << showString "else" << body xs
//                            | [] -> id
//                            | [If(e, xs, ys)] -> 
//                                indent << showString "else" << ifBegin e << f xs << els ys
//                            | ys -> 
//                                indent << showString "else" << f ys
                        f xs << els ys << indent << showString "end"
                ifBegin e << body
            | Act(es, body) -> 
                let fbody = function
                    | OneStmt x -> showChar ' ' << f' x
                    | xs -> f xs << indent << showString "end"
                showString "act " << joinS ", " (List.map showExpr es) << showChar ':' << fbody body
            | Comment s -> showChar '!' << showString s
            | AssingCode(ass, stmts) -> 
                showAssign ass << showString " = " << showChar '{' << nl << (f stmts) << indent << showChar '}'
        tabss tabs << f' xs
    state 0
let showLoc (Location(name, statements)) = 
    showString "# " << showString name << nl
    << joinS "\n" (List.map printState statements) << nl
    << showString (sprintf "--- %s ----------" name)
let printLocs xs = List.map showLoc xs |> joinS "\n\n" |> show

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