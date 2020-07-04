module Qsp.Parser.Main
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either
open Qsp

open Qsp.Ast
open Qsp.Parser.Generic
open Qsp.Parser.Expr

// ident = ["*" | "$"] (underscore | letter) {letter | digit | underscore}
let identifier =
    // let isIdentifierFirstChar c = isLetter c || c = '_'
    // let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = '.'
    // let p = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"

    // let keys = notFollowedByL ((choice <| List.map pstringCI ["let"; "set"; "if"; "end"; "elseif"; "else"; "act"; "no"; "obj"]) >>. (spaces1 <|> eof <|> skipChar '"' <|> skipChar ''')) "some excess"

    // keys >>.
    // p <|> (pstring "*" <|> pstring "$" >>= fun st -> p |>> fun x -> st + x)
    pexplicitVar
    <|> (ident |>> fun name -> ImplicitNumericType, name)

let nlws = nl >>. spaces
let strInsWs s = pstringCI s >>. ws
let identOnly s = pstringCI s >>? notFollowedBy (satisfy isLetter) >>. ws
let pstmt, pstmtRef = createParserForwardedToRef<Statement, _>()
let stmts = sepEndBy pstmt (nlws <|> (char_ws '&' >>% ()))
let assign =
    let assdef name ass =
        let call =
            ident >>=?
            fun name ->
                followedBy (ident <|> (puint32 >>% "") <|> stringLiteral) >>. (sepBy1 pexpr (char_ws ','))
                |>> fun args -> Assign(ass, Func(name, args))
        let assexpr = call <|> (pexpr |>> fun defExpr -> Assign(ass, defExpr))

        let str_ws s = pstring s .>> ws
        (str_ws "-=" >>. pexpr |>> fun defExpr -> Assign(ass, Expr.Expr(Minus, Var name, defExpr)))
        <|> (str_ws "=-" >>. pexpr |>> fun defExpr -> Assign(ass, Expr.Expr(Minus, defExpr, Var name)))
        <|> ((str_ws "+=" <|> str_ws "=+") >>. pexpr |>> fun defExpr -> Assign(ass, Expr.Expr(Plus, Var name, defExpr)))
        <|> (char_ws '=' >>. assexpr)

    let assign name =
        let arr =
            bet_ws '[' ']' pexpr
            |>> fun braketExpr -> AssignArr(name, braketExpr)
        arr <|>% AssignVar name >>= assdef name
    // assign = ["set" | "let"] ident ['[' expr ']'] ('=' ['+'|'-'] | ('+'|'-') '=') exprNotStartWithNeg
    let pexplicitAssign =
        let p =
            identOnly "set" <|> identOnly "let"
            >>. identifier
        p <|> pexplicitVar .>>? ws
        >>=? assign

    let pimlicitAssign =
        ident .>>? ws
        >>=? fun name ->
            let call name =
                (sepBy1 pexpr (char_ws ','))
                |>> fun exprs -> CallSt(name, exprs)
            assign (ImplicitNumericType, name)
            <|> (followedBy // TODO: Не нравится мне это место, ох как не нравится
                    (ident
                     <|> ((strInsWs "obj" <|> strInsWs "no") >>% "")
                     <|> (puint32 >>% "") <|> stringLiteral)
                >>. call name)
    pexplicitAssign <|> pimlicitAssign

let pcomment : _ Parser =
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
    pchar '!' >>. manyStrings p |>> Comment
let pexprStmt =
    pexpr
    |>> function
        // | Var x -> CallSt(x, [])
        // | Func(name, args) -> CallSt(name,args)
        | x -> StarPl x
let psign = char_ws ':' >>. manySatisfy ((<>) '\n') |>> Label
module IfMod =
    let ifBegin = strInsWs "if" >>. pexpr .>> char_ws ':'

    let stmtsOne, stmtsOneRef = createParserForwardedToRef<Statement list, _>()

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

    let smtsMulti, smtsMultiRef = createParserForwardedToRef<Statement list, _>()
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
    choice [
        IfMod.pIf
        assign
        pcomment
        attempt pexprStmt
        psign
    ]

let ploc =
    let anyExceptNl = many1Satisfy (fun c -> c <> '\n')
    char_ws '#' >>. anyExceptNl .>> nlws >>= fun name ->
    stmts .>> (char_ws '-') .>> (skipManySatisfy (fun c -> c<>'\n')) .>> spaces |>> fun body -> Location(name, body)
//let plst = str_ws "begin" >>. sepEndBy ident (nl >>. ws) .>> str "end"
//parsing plst "begin a\n end"
open Qsp.Tokens

let start str =
    let p =
        many1Satisfy (not << isLetter)
        >>. many
            ((getPosition .>>? many1Satisfy isLetter .>>. getPosition
              |>> fun (p1, p2) ->
                let range =
                    {
                        StartLine = int p1.Line
                        EndLine = int p2.Line
                        StartColumn = int p1.Column
                        EndColumn = int p2.Column
                    }
                { TokenType = Keyword
                  Range = range } )
             .>> many1Satisfy (not << isLetter))
    run p str
