module Qsp.Parser
open FsharpMyExtension
open FParsec
open Qsp.Ast

type A = Associativity


let parsing p str =
    match run p str with
    | Success(result, _, _)   -> result
    | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

//let ws = spaces
let ws =
    skipManySatisfy (function '\t' | ' ' -> true | _ -> false)
    //manyChars (pchar '\t' <|> pchar ' ')
let nl<'a> = skipMany1 newline : Parser<unit, 'a>
let str s = pstring s
let str_ws s = pstring s .>> ws
let char_ws c = pchar c .>> ws
let bet opened closed = between <| char_ws opened <| pchar closed
let bet_ws opened closed p = bet opened closed p .>> ws
let optList p = p <|>% []
//ident = ["*" | "$"] (underscore | letter) {letter | digit | underscore}
let identifier =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = '.'
    let p = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

    let keys = notFollowedByL ((choice <| List.map pstringCI ["let"; "set"; "if"; "end"; "elseif"; "else"; "act"; "no"; "obj"]) >>. (spaces1 <|> eof <|> skipChar '"' <|> skipChar ''')) "some excess"

    keys >>.
    p <|> (pstring "*" <|> pstring "$" >>= fun st -> p |>> fun x -> st + x)
    .>> ws // skips trailing whitespace

let stringLiteral =
    let normalChar c = satisfy (fun c' -> c' <> c)
    let p c = manyChars (normalChar c <|> attempt(pchar c >>. pchar c))

    bet_ws '"' '"' (p '"') <|> bet_ws '\'' '\'' (p '\'')
assert
    assert
        parsing stringLiteral "\" \"" = ""
    assert
        parsing stringLiteral "\"\"\"\"" = "\""
    assert
        parsing stringLiteral "''''" = "'"

    parsing stringLiteral "''" = ""



let pexpr =
    let opp = new OperatorPrecedenceParser<Expr,unit,unit>()
    let expr = opp.ExpressionParser
    let term =
        let varOrCall =
            let pcall name =
                bet_ws '(' ')' (sepBy expr (pchar ',' >>. ws)) |>> fun args ->
                Func(name, args)
            let idx name =
                bet_ws '[' ']' (sepBy expr (pchar ',' >>. ws)) |>> fun args ->
                Arr(name, args)
            identifier >>= fun name -> pcall name <|> idx name <|>% (Var name)
        let pred = (stringLiteral |>> String) <|> (pint32 |>> Int) |>> Val .>> ws
        //(pint32 .>> ws |>> (Int >> Val)) <|> varOrCall <|> bet_ws '(' ')' expr
        pred <|> varOrCall <|> bet_ws '(' ')' expr

    opp.TermParser <- term

    let addInf opT =
        let op = Op.toString opT
        let prec = Precedences.prec <| Precedences.OpB opT
        opp.AddOperator(InfixOperator(op, ws, prec, A.Left, fun x y -> Expr(opT, x, y)))

    Reflection.Reflection.initUnionMap<_, int>
    |> Map.iter (fun k v -> addInf k)

    // opp.AddOperator(InfixOperator("=>", ws, 4, A.Left, fun x y -> Expr(Ge, x, y)))
    // opp.AddOperator(InfixOperator("=<", ws, 4, A.Left, fun x y -> Expr(Le, x, y)))
    opp.AddOperator(InfixOperator("!",   ws, 4, A.Left, fun x y -> Expr(Le, x, y)))
    //Precedences.prec <| Precedences.OpB And
    opp.AddOperator(InfixOperator("OR",  ws, 1, A.Left, fun x y -> Expr(Le, x, y)))
    opp.AddOperator(InfixOperator("AND", ws, 2, A.Left, fun x y -> Expr(Le, x, y)))
    let addPref unT =
        let op = UnarOp.toString unT
        let prec = Precedences.prec <| Precedences.PrefB unT
        opp.AddOperator(PrefixOperator(op, ws, prec, false, fun x -> UnarExpr(unT, x)))

    Reflection.Reflection.initUnionMap<_, int>
    |> Map.iter (fun k v -> addPref k)

    expr

let nlws = nl >>. spaces
let strInsWs s = pstringCI s >>. ws
let identOnly s = pstringCI s >>? notFollowedBy (satisfy isLetter) >>. ws
let pstmt, pstmtRef = createParserForwardedToRef<Statement,unit>()
let stmts = sepEndBy pstmt (nlws <|> (char_ws '&' >>% ()))
let assign =
    let assdef name ass =
        let asscode =
            //let stmts = sepBy pstmt nlws
            between (pchar '{' >>. spaces) (spaces >>. char_ws '}') stmts
            |>> fun stmts -> AssingCode(ass, stmts)
        let call = identifier >>=? fun name -> followedBy (identifier <|> (puint32 >>% "") <|> stringLiteral) >>. (sepBy1 pexpr (char_ws ',')) |>> fun args -> Assign(ass, Func(name, args))
        let assexpr = call <|> (pexpr |>> fun defExpr -> Assign(ass, defExpr))

        (str_ws "-=" >>. pexpr |>> fun defExpr -> Assign(ass, Expr.Expr(Minus, Var name, defExpr)))
        <|> (str_ws "=-" >>. pexpr |>> fun defExpr -> Assign(ass, Expr.Expr(Minus, defExpr, Var name)))
        <|> ((str_ws "+=" <|> str_ws "=+") >>. pexpr |>> fun defExpr -> Assign(ass, Expr.Expr(Plus, Var name, defExpr)))
        <|> (char_ws '=' >>. (assexpr <|> asscode))

    let assign name =
        let arr = bet_ws '[' ']' pexpr |>> fun braketExpr -> AssignArr(name, braketExpr)
        arr <|>% (AssignVar name) >>= assdef name
    // assign = ["set" | "let"] ident ['[' expr ']'] ('=' ['+'|'-'] | ('+'|'-') '=') exprNotStartWithNeg
    let p = identOnly "set" <|> identOnly "let" >>. identifier >>= assign
    let all =
        identifier >>=? fun name ->
        let call name = (sepBy1 pexpr (char_ws ',')) |>> fun exprs -> CallSt(name, exprs)
        //assign name <|> (followedBy (identifier <|> (puint32 >>% "") <|> stringLiteral) >>. call name)
        assign name <|> (followedBy (identifier <|> ((strInsWs "obj" <|> strInsWs "no") >>% "") <|> (puint32 >>% "") <|> stringLiteral) >>. call name)
    p <|> all
    //attempt p <|> (pexpr |>> Statement.StarPl)
let pcomment<'a> =
    let stringLiteral2 c =
        let normalCharSnippet = many1Satisfy ((<>) c)
        let cs = string c
        let escapedChar = pstring (cs + cs)
        between (pchar c) (pchar c)
                (manyStrings (normalCharSnippet <|> escapedChar)) |>> fun x -> System.String.Concat([|cs; x; cs|])
    let brace =
        let normalCharSnippet = many1Satisfy ((<>) '}')
        let escapedChar = pstring "}}"
        between (pchar '{') (pchar '}')
                (manyStrings (normalCharSnippet <|> escapedChar)) |>> fun x -> System.String.Concat([|"{"; x; "}"|])
    let p =
        many1Satisfy (fun c -> c <> '\n' && c <> ''' && c <> '"' && c <> '{')
        <|> stringLiteral2 '"'
        <|> stringLiteral2 '''
        <|> brace
    pchar '!' >>. manyStrings p |>> Comment : Parser<Statement,'a>
let pexprStmt =
    pexpr |>> function Var x -> CallSt(x, []) | Func(name, args) -> CallSt(name,args) | x -> StarPl x
let psign = char_ws ':' >>. manySatisfy ((<>) '\n') |>> Sign
module IfMod =
    let ifBegin = strInsWs "if" >>. pexpr .>> char_ws ':'

    let stmtsOne, stmtsOneRef = createParserForwardedToRef<Statement list,unit>()

    //ifOneBody = stmtsOne ["else" stmtsOne];
    let ifOneBody cond =
        stmtsOne >>= fun body ->
        optList (strInsWs "else" >>. stmtsOne)
        |>> fun elseBody -> If(cond, body, elseBody)
    //ifOne = ifBegin ifOneBody;
    let ifone = ifBegin >>= ifOneBody

    //actBegin = "act" expr ":"
    let actBegin = strInsWs "act" >>. sepBy1 pexpr (char_ws ',') .>> char_ws ':'
    //actOne = actBegin stmtsOne;
    let actOne = actBegin >>= fun exprs -> stmtsOne |>> fun body -> Act(exprs, body)
    //stmtsOne = actOne | ifOne | assign { "&" assign } [actOne | ifOne]
    let p = (actOne <|> ifone |>> fun x -> [x])
    stmtsOneRef :=
        //p <|> sepBy1 (assign <|> pexprStmt <|> pcomment) (char_ws '&') .>> (skipChar 'a' <|>% ()) .>> ws >>= fun assigns ->
        //p <|> sepBy1 (assign <|> pexprStmt <|> pcomment <|> actOne <|> ifone) (char_ws '&') >>= fun assigns ->
        sepBy1 (assign <|> pexprStmt <|> pcomment <|> actOne <|> ifone) (char_ws '&') <|> p >>= fun assigns ->
        optList p |>> fun x -> assigns @ x

    let smtsMulti, smtsMultiRef = createParserForwardedToRef<Statement list,unit>()
    //stmtsMultiNl = nl smtsMulti nl {stmtsMulti nl}
    let stmtsMultiNl = nlws >>. sepEndBy1 smtsMulti nlws |>> List.concat
    //actMulti = actBegin (stmtsOne | stmtsMultiNl "end");
    let actMulti =
        actBegin >>= fun args ->
        stmtsOne <|> (stmtsMultiNl .>> strInsWs "end")
        |>> fun body -> Act(args, body)
    let elsifbody cond elsif =
        stmtsMultiNl >>= fun body ->
        let en = strInsWs "end" >>% If(cond, body, [])
        let el =
            strInsWs "else" >>. ((elsif()|>>fun x -> [x]) <|> (stmtsMultiNl .>> strInsWs "end"))
            |>> fun elseBody -> If(cond, body, elseBody)
        en <|> el
    //elif = ifBegin stmtsMultiNl ("end" | "else" (elif | stmtsMultiNl "end"));
    let rec elsif () = ifBegin >>= fun cond -> elsifbody cond elsif

    //ifMulti = ifBegin (elsifbody | ifOneBody)
    let ifMulti =
        ifBegin >>= fun cond ->
        elsifbody cond elsif <|> ifOneBody cond
    //stmtsMulti = ifMulti | actMulti | assign { "&" assign } [ifMulti | actMulti]
    let pIf = ifMulti <|> actMulti
    do
        let p =
            sepEndBy1 (assign <|> pcomment <|> pexprStmt <|> psign) (char_ws '&') >>= fun stmts ->
            opt(opt(char_ws '&') >>. ifone) |>> function None -> stmts | Some x -> List.append stmts [x]
        smtsMultiRef :=
            //ifActMulti <|> sepBy (assign <|> pcomment <|> pexprStmt <|> psign) (char_ws '&') >>= fun assigns -> optList ifActMulti |>> fun x -> assigns @ x
            //sepBy (assign <|> pcomment <|> pexprStmt <|> psign) (char_ws '&') <|> ifActMulti >>= fun assigns -> optList ifActMulti |>> fun x -> assigns @ x
            p <|> (pIf |>> fun x -> [x]) <|>% []



//let startStr = stringLiteral .>> ws .>> nlws |>> (String >> Val >> StarPl)

pstmtRef :=
    assign <|> IfMod.pIf <|> pcomment <|> (attempt pexprStmt) <|> psign

let ploc =
    let anyExceptNl = many1Satisfy (fun c -> c <> '\n')
    char_ws '#' >>. anyExceptNl .>> nlws >>= fun name ->
    stmts .>> (char_ws '-') .>> (skipManySatisfy (fun c -> c<>'\n')) .>> spaces |>> fun body -> Location(name, body)
//let plst = str_ws "begin" >>. sepEndBy ident (nl >>. ws) .>> str "end"
//parsing plst "begin a\n end"

