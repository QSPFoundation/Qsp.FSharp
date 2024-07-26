[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Ast.Expr

module Parser =
    open FParsec
    open FsharpMyExtension
    open FsharpMyExtension.Either

    open Qsp
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

    let pstringIdent =
        let isIdentifierFirstChar c = isLetter c || c = '_' || c = '#'
        let ident = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
        let varType =
            appendToken TokenType.KeywordSymbol (pchar '$') >>% StringType
        varType .>>. applyRange ident

    /// берёт только то, что начинается с `$`
    let pstringVar varHighlightKind isLocal : _ Parser =
        pstringIdent
        >>= fun (typ, (range, varName)) ->
            let msg =
                match typ with
                | StringType ->
                    Defines.vars
                    |> Map.tryFind (sprintf "$%s" (varName.ToLower()))
                    |> function
                        | Some dscr -> dscr
                        | None ->
                            let scope = if isLocal then "локальная" else "глобальная"
                            sprintf "Пользовательская %s переменная строчного типа" scope
                | NumericType -> failwith "NumericType Not Implemented"
            appendToken2 TokenType.Variable range
            >>. appendHover2 (RawDescription msg) range
            >>. appendVarHighlight range (typ, varName) varHighlightKind isLocal
            >>. preturn (typ, varName)
    type ProcOrFunc =
        | Procedure of string
        | Function of string

    let notFollowedByBinOpIdent =
        let p =
            pbinaryOperator
            .>> (skipSatisfy (not << isIdentifierChar)
                <|> eof)
        let p2 =
            notFollowedByL p "keyword"
            >>. ident
        p2
    let ws =
        ws
        >>. skipMany
                (appendToken TokenType.Underscore (pchar '_')
                >>? ((ws1 >>? skipNewline) <|> skipNewline) >>. spaces)

    let term expr (pstmts: Statements Parser) =
        let getDesc (varType, (name:string)) =
            match varType with
            | StringType ->
                Defines.vars
                |> Map.tryFind (sprintf "$%s" (name.ToLower()))
                |> function
                    | Some dscr -> dscr
                    | None -> "Пользовательская глобальная переменная строчного типа"
            | NumericType ->
                Defines.vars
                |> Map.tryFind (name.ToLower())
                |> function
                    | Some dscr -> dscr
                    | None ->
                        "Пользовательская глобальная переменная числового типа"
        let pterm, ptermRef = createParserForwardedToRef()
        let pBracesArgs =
            bet_ws '(' ')' (sepBy expr (pchar ',' >>. ws))
        let pcallFuncOrArrOrVar =
            let pbraket =
                between
                    (appendToken TokenType.BraceSquareOpened (pchar '[' .>> ws))
                    (appendToken TokenType.BraceSquareClosed (pchar ']'))
                    (sepBy expr (skipChar ',' >>. ws))

            let pcallFunctionOrArrOrVar =
                let pnumericVar =
                    applyRange notFollowedByBinOpIdent |>> fun x -> NumericType, x

                tuple2
                    (pstringIdent <|> pnumericVar .>> ws)
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
                                    appendHover2 (RawDescription desc) range
                                    >>. appendToken2 TokenType.Variable range
                                    >>. appendVarHighlight range (varType, nameVar) VarHighlightKind.ReadAccess false
                                    >>% Arr((varType, nameVar), args))
                    <|>% fun (varType, nameVar) range ->
                            let desc = getDesc(varType, nameVar)
                            appendHover2 (RawDescription desc) range
                            >>. appendToken2 TokenType.Variable range
                            >>. appendVarHighlight range (varType, nameVar) VarHighlightKind.ReadAccess false
                            >>% Var(varType, nameVar))
                >>= fun ((varType, (range, name)), f) ->
                        f (varType, name) range
            // #load @"Defines.fs"
            // open Qsp
            let nullary, multiary =
                Defines.functionsByName
                |> Map.partition (fun _ x ->
                    let x, _ = x.Signature
                    match x with
                    | Defines.JustOverloads []
                    | Defines.JustOverloads [([||], ())] -> true
                    | _ -> false
                )
            let nullaryFunc =
                nullary
                |> Seq.sortByDescending (fun (KeyValue(name, _)) -> name) // для жадности
                |> Seq.map (fun (KeyValue(name, x)) ->
                    applyRange (opt (pchar '$') >>? pstringCI name .>>? notFollowedVarCont)
                    >>= fun (range, name) ->
                        appendToken2 TokenType.Function range
                        >>. appendHover2 (FuncDescription x.SymbolicName) range
                        >>% (name, range, x)
                )
                |> List.ofSeq
                |> choice

            let pPreDefFunc =
                multiary
                |> Seq.sortByDescending (fun (KeyValue(name, _)) -> name) // для жадности
                |> Seq.map (fun (KeyValue(name, x)) ->
                    applyRange (opt (pchar '$') >>? pstringCI name .>>? notFollowedVarCont)
                    >>= fun (range, name) ->
                        appendToken2 TokenType.Function range
                        >>. appendHover2 (FuncDescription x.SymbolicName) range
                        >>% (name, range, x)
                )
                |> List.ofSeq
                |> choice
            nullaryFunc .>>. (ws >>. (pBracesArgs <|>% []))
            <|> (pPreDefFunc .>> ws .>>. (pBracesArgs <|> (pterm |>> List.singleton) <|>% []))
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
                stringLiteralWithToken expr pstmts |>> String
                appendToken TokenType.ConstantNumericInteger
                    (pint32 |>> Int)
            ]
            |>> Val
        let pDirectCallFunc =
            appendToken TokenType.KeywordSymbol (pchar '@')
            >>. ws >>. applyRange notFollowedByBinOpIdent
            .>> ws .>>. (pBracesArgs <|> (pterm |>> List.singleton) <|>% [])
            >>= fun ((r, locName), args) ->
                let locNameLower = String.toLower locName
                appendLocHighlight r locNameLower VarHighlightKind.ReadAccess
                >>. pGetDefLocPos locNameLower
                    >>= function
                        | None ->
                            updateUserState (fun st ->
                                { st with
                                    NotDefinedLocs =
                                        st.NotDefinedLocs
                                        |> Map.addOrMod locNameLower [r] (fun xs -> r::xs)
                                }
                            )
                        | Some _ -> preturn ()
                >>% Func(Predef Defines.Func, Val (String [[StringKind locName]])::args)

        let ptuple =
            pBracesArgs
            |>> function
                | [x] -> x
                | xs -> Expr.Tuple xs

        ptermRef.Value <-
            choice [
                pval
                pDirectCallFunc
                pcallFuncOrArrOrVar
            ]
        pterm <|> ptuple

    let pexpr (pstmts: Ast.Statements Parser) : _ Parser =
        let pExpr, pExprRef = createParserForwardedToRef()
        let term = term pExpr pstmts
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
            pNoRef.Value <-
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

        pExprRef.Value <- pOr
        pExpr
