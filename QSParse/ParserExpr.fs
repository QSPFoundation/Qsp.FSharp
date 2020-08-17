module Qsp.Parser.Expr
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either
open Qsp
open Qsp.Ast
open Qsp.Parser.Generic
open Qsp.Tokens

let pbinaryOperator : _ Parser =
    [
        Defines.exprNamedOperators |> List.map (fun (x, _, _) -> x)
        Defines.keywords |> List.map fst
    ]
    |> List.concat
    |> List.sortDescending
    |> List.map pstringCI
    |> choice

/// берёт только то, что начинается с `#` или `$`
let pexplicitVar varHighlightKind : _ Parser =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let p = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    // TODO: или просто `many1Satisfy isIdentifierChar` ?
    let varType =
        choice [
            pchar '#' >>% ExplicitNumericType
            pchar '$' >>% StringType
        ]

    (getPosition .>>.? varType) .>>. (p .>>. getPosition)
    >>= fun ((p1, typ), (varName, p2)) ->
        let range = toRange p1 p2
        let msg =
            match typ with
            | StringType ->
                Defines.vars
                |> Map.tryFind (sprintf "$%s" (varName.ToLower()))
                |> function
                    | Some dscr -> dscr
                    | None -> "Пользовательская глобальная переменная строчного типа"
            | ExplicitNumericType ->
                Defines.vars
                |> Map.tryFind (sprintf "#%s" (varName.ToLower()))
                |> function
                    | Some dscr -> dscr
                    | None -> "Пользовательская глобальная переменная числового типа"
            | ImplicitNumericType -> failwith "Not Implemented"
        appendToken2 Tokens.Variable range
        >>. appendHover2 msg range
        >>. appendVarHighlight range (typ, varName) varHighlightKind
        >>. preturn (typ, varName)
type ProcOrFunc =
    | Procedure of string
    | Function of string

let notFollowedByBinOpIdent =
    // конечно, тут нужно объяснить пользователю, почему именно нельзя использовать то или иное слово
    // проще назвать, что допустимо
    // let p =
    //     choice [
    //         spaces1
    //         skipChar '"'
    //         skipChar '''
    //         eof
    //     ]
    // let followedVarCont =
    //     followedBy (satisfy (fun c -> isDigit c || c = '_' || c = '.'))
    let p =
        pbinaryOperator
        .>> (skipSatisfy (not << isIdentifierChar)
             <|> eof)
    let p2 =
        notFollowedByL p "идентификатор, а не строковый бинарный оператор"
        >>. ident
    // runStateEither p2 emptyState "no"
    // runStateEither p2 emptyState "no " // нельзя
    // runStateEither p2 emptyState "node" // можно
    // runStateEither p2 emptyState "foobar" // можно
    p2
let ws =
    ws
    >>. skipMany
            (appendToken TokenType.Underscore (pchar '_')
             >>? ((ws1 >>? skipNewline) <|> skipNewline) >>. spaces)

let term expr =
    let getDesc (varType, (name:string)) =
        match varType with
        | StringType ->
            Defines.vars
            |> Map.tryFind (sprintf "$%s" (name.ToLower()))
            |> function
                | Some dscr -> dscr
                | None -> "Пользовательская глобальная переменная строчного типа"
        | ExplicitNumericType ->
            Defines.vars
            |> Map.tryFind (sprintf "#%s" (name.ToLower()))
            |> function
                | Some dscr -> dscr
                | None -> "Пользовательская глобальная переменная числового типа"
        | ImplicitNumericType ->
            Defines.vars
            |> Map.tryFind (name.ToLower())
            |> function
                | Some dscr -> dscr
                | None ->
                    "Пользовательская глобальная переменная числового типа"
    let pterm, ptermRef = createParserForwardedToRef()
    let pcallFuncOrArrOrVar =
        let pbraket = bet_ws '[' ']' (sepBy expr (skipChar ',' >>. ws))
        let pBracesArgs =
            bet_ws '(' ')' (sepBy expr (pchar ',' >>. ws))
        let pcallFunctionOrArrOrVar =
            let pimplicitVar =
                notFollowedByBinOpIdent |>> fun x -> ImplicitNumericType, x
            let pexplicitVar =
                let isIdentifierFirstChar c = isLetter c || c = '_'
                let p = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
                // TODO: или просто `many1Satisfy isIdentifierChar` ?
                let varType =
                    choice [
                        pchar '#' >>% ExplicitNumericType
                        pchar '$' >>% StringType
                    ]
                varType .>>. p

            tuple2
                (applyRange (pexplicitVar <|> pimplicitVar) .>> ws)
                ((pBracesArgs
                  |>> fun args ->
                    let tokenType = TokenType.Function
                    fun (funType, name) range ->
                        let p =
                            [
                                "Такой функции нет, а если есть, то напишите мне, автору расширения, пожалуйста, и я непременно добавлю."
                                "Когда-нибудь добавлю: 'Возможно, вы имели ввиду: ...'"
                            ]
                            |> String.concat "\n"
                            |> appendSemanticError range
                        p
                        >>. appendToken2 tokenType range
                        >>% Func(Undef name, args)
                    )
                //   Невозможно, поскольку неоднозначно трактуется `f+1` => `f(+1)` или `f + 1`
                //   <|> (pterm |>> fun arg -> TokenType.Function, fun (typ', name) -> Func(name, [arg]))
                  <|> (pbraket
                       |>> fun args ->
                            fun (varType, nameVar) range ->
                                let desc = getDesc(varType, nameVar)
                                appendHover2 desc range
                                >>. appendToken2 TokenType.Variable range
                                >>. appendVarHighlight range (varType, nameVar) VarHighlightKind.ReadAccess
                                >>% Arr((varType, nameVar), args))
                  <|>% fun (varType, nameVar) range ->
                        let desc = getDesc(varType, nameVar)
                        appendHover2 desc range
                        >>. appendToken2 TokenType.Variable range
                        >>. appendVarHighlight range (varType, nameVar) VarHighlightKind.ReadAccess
                        >>% Var(varType, nameVar))
            >>= fun ((range, (varType, name)), f) ->
                    f (varType, name) range

        let pPreDefFunc =
            Defines.functions
            |> Seq.sortByDescending (fun (KeyValue(name, _)) -> name) // для жадности
            |> Seq.map (fun (KeyValue(name, x)) ->
                applyRange (pstringCI name .>>? notFollowedVarCont)
                >>= fun (range, name) ->
                    appendToken2 TokenType.Function range
                    >>. appendHover2 x.Description range
                    >>% (name, range, x)
            )
            |> List.ofSeq
            |> choice
        pPreDefFunc .>> ws .>>. (pBracesArgs <|> (pterm |>> List.singleton) <|>% [])
        >>= fun ((stringName, range, x), args) ->
                let sign, returnType = x.Signature
                let p =
                    args
                    |> Array.ofList
                    |> Defines.getFuncByOverloadType sign
                    |> function
                        | None ->
                            let msg =
                                Defines.Show.printFuncSignature stringName returnType sign
                                |> sprintf "Ожидается одна из перегрузок:\n%s"
                            appendSemanticError range msg
                        | Some () ->
                            preturn ()
                p
                >>% Func(Predef x.SymbolicName, args)
        <|> pcallFunctionOrArrOrVar
    let pval =
        choice [
            // TODO: `pbraces` — он точно нужен?
            stringLiteralWithToken expr |>> String
            appendToken TokenType.ConstantNumericInteger
                (pint32 |>> Int)
        ]
        |>> Val
    ptermRef := pval <|> pcallFuncOrArrOrVar
    pval <|> pcallFuncOrArrOrVar <|> bet_ws '(' ')' expr

let pExprOld : _ Parser =
    let opp = new OperatorPrecedenceParser<Expr, unit, _>()

    let expr = opp.ExpressionParser
    opp.TermParser <- term expr .>> ws

    Op.ops
    |> Array.iter (fun (opTyp, (opName, isSymbolic)) ->
        let prec = Precedences.prec <| Precedences.OpB opTyp
        if isSymbolic then
            if opName = ">" then
                // внутри string есть подстановка `<<expr>>`, и эта условность нужна, чтобы не захватывать `>>`
                let p = notFollowedBy (pchar '>') >>. ws
                InfixOperator(opName, p, prec, Associativity.Left, fun x y -> Expr(opTyp, x, y))
            else
                InfixOperator(opName, ws, prec, Associativity.Left, fun x y -> Expr(opTyp, x, y))
            |> opp.AddOperator
        else
            let afterStringParser = notFollowedVarCont >>. ws
            InfixOperator(opName, afterStringParser, prec, Associativity.Left, fun x y -> Expr(opTyp, x, y))
            |> opp.AddOperator
            InfixOperator(opName.ToUpper(), afterStringParser, prec, Associativity.Left, fun x y -> Expr(opTyp, x, y))
            |> opp.AddOperator
    )

    Reflection.Reflection.unionEnum
    |> Array.iter (fun unT ->
        let afterStringParser opName =
            if String.forall isLetter opName then
                notFollowedVarCont
                >>. ws
            else
                ws
        let unarOp = UnarOp.toString unT
        let prec = Precedences.prec <| Precedences.PrefB unT
        PrefixOperator(unarOp, afterStringParser unarOp, prec, false, fun x -> UnarExpr(unT, x))
        |> opp.AddOperator
    )
    expr <?> "expr"

let pExprNew : _ Parser =
    let pExpr, pExprRef = createParserForwardedToRef()
    let term = term pExpr
    let pchar c typ =
        appendToken typ (pchar c)
    let pstringCI c typ =
        appendToken typ (pstringCI c)
    let pstring c typ =
        appendToken typ (pstring c)

    let pNeg =
        pchar '-' (TokenType.UnaryOperator Neg) >>. ws >>. term
        |>> fun e1 -> UnarExpr(Neg, e1)
    let pProd =
        chainl1 (pNeg <|> term .>> ws)
            ((pchar '*' (TokenType.BinaryOperator Times) >>% Times
              <|> (pchar '/' (TokenType.BinaryOperator Divide) >>% Divide))
             .>> ws |>> fun op e1 e2 -> Expr(op, e1, e2))
    let pMod =
        chainl1 (pProd .>> ws)
            ((pstringCI "mod" (TokenType.BinaryOperator Mod) >>? notFollowedVarCont >>. ws >>% Mod)
             .>> ws |>> fun op e1 e2 -> Expr(op, e1, e2))
    let pSum =
        chainl1 (pMod .>> ws)
            ((pchar '+' (TokenType.BinaryOperator Plus) >>% Plus
              <|> (pchar '-' (TokenType.BinaryOperator Minus) >>% Minus))
             .>> ws |>> fun op e1 e2 -> Expr(op, e1, e2))
    let pCompare pNo =
        chainl1 (pNo <|> pSum .>> ws)
            (choice [
                pstring "=>" (TokenType.BinaryOperator Eg) >>% Eg
                pstring "=<" (TokenType.BinaryOperator El) >>% El
                pchar '=' (TokenType.BinaryOperator Eq) >>% Eq

                pstring "<>" (TokenType.BinaryOperator Ne) >>% Ne
                pstring "<=" (TokenType.BinaryOperator Le) >>% Le
                pchar '<' (TokenType.BinaryOperator Lt) >>% Lt

                pstring ">=" (TokenType.BinaryOperator Ge) >>% Ge
                pchar '>' (TokenType.BinaryOperator Gt) .>>? notFollowedBy (FParsec.CharParsers.pchar '>') >>% Gt // чтобы исключить `>>`

                pchar '!' (TokenType.BinaryOperator Bang) >>% Bang
             ]
             .>> ws |>> fun op e1 e2 -> Expr(op, e1, e2))
    let pObj pNo =
        let pObj =
            pstringCI "obj" TokenType.Procedure .>>? notFollowedVarCont >>% Obj
            <|> (pstringCI "loc" TokenType.Procedure .>>? notFollowedVarCont >>% Loc)
            .>> ws .>>. pCompare pNo
            |>> fun (op, e1) -> UnarExpr(op, e1)
        pObj <|> pCompare pNo .>> ws
    let pNo =
        // TODO: `no` — ассоциативный оператор, потому допустимо такое: `no (no -1)`
        let pNo, pNoRef = createParserForwardedToRef()
        pNoRef :=
            pstringCI "no" TokenType.Procedure >>? notFollowedVarCont >>. ws >>. pObj pNo
            |>> fun e1 -> UnarExpr(No, e1)
        pNo <|> pObj pNo .>> ws
    let pAnd =
        chainl1 (pNo .>> ws)
            ((pstringCI "and" TokenType.Procedure >>? notFollowedVarCont >>. ws >>% And)
             .>> ws |>> fun op e1 e2 -> Expr(op, e1, e2))
    let pOr =
        chainl1 (pAnd .>> ws)
            ((pstringCI "or" TokenType.Procedure >>? notFollowedVarCont >>. ws >>% Or)
             .>> ws |>> fun op e1 e2 -> Expr(op, e1, e2))

    pExprRef := pOr
    pExpr
let pexpr = pExprNew
