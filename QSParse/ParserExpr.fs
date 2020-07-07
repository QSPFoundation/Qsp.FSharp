module Qsp.Parser.Expr
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either
open Qsp
open Qsp.Ast
open Qsp.Parser.Generic
open Qsp.Tokens

let pbinaryOperator : _ Parser =
    // transferOperators
    // procedures
    [
        Defines.binaryOperators
        Defines.keywords
    ]
    |> List.concat
    |> List.sortDescending
    |> List.map pstringCI
    |> choice

/// берёт только то, что начинается с `#` или `$`
let pexplicitVar : _ Parser =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = '.'
    let p = many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    // TODO: или просто `many1Satisfy isIdentifierChar` ?
    let varType =
        choice [
            pchar '#' >>% ExplicitNumericType
            pchar '$' >>% StringType
        ]
    (getPosition .>>.? varType) .>>. (p .>>. getPosition)
    >>= fun ((p1, typ), (x, p2)) ->
        updateUserState (fun st ->
            let token =
                { Tokens.TokenType = Tokens.Variable
                  Tokens.Range = p1, p2 }

            { st with Tokens = token :: st.Tokens }
        )
        >>. preturn (typ, x)
type ProcOrFunc =
    | Procedure of string
    | Function of string
let pdefProcOrFunc : _ Parser =
    let pprocedure =
        [
            Defines.procedures
            Defines.proceduresWithAsterix
            Defines.transferOperators
        ]
        |> List.concat
        |> List.sortDescending // чтобы обеспечить жадность
        |> List.map pstring
        |> choice
        |>> Procedure
    let pfunction =
        Defines.functions
        |> List.sortDescending // чтобы обеспечить жадность
        |> List.map pstring
        |> choice |>> Function
    pprocedure <|> pfunction
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
        .>> (skipSatisfy (fun c ->
                not (isLetter c || isDigit c || c = '_' || c = '.'))
             <|> eof)
    let p2 =
        notFollowedByL p "идентификатор, а не строковый бинарный оператор"
        >>. ident
    // runStateEither p2 emptyState "no"
    // runStateEither p2 emptyState "no " // нельзя
    // runStateEither p2 emptyState "node" // можно
    // runStateEither p2 emptyState "foobar" // можно

    p2
    // notFollowedByL (keywords >>. p) "нельзя использовать здесь ключевое слово"
    // let keys = notFollowedByL ((choice <| List.map pstringCI ["let"; "set"; "if"; "end"; "elseif"; "else"; "act"; "no"; "obj"]) >>. (spaces1 <|> eof <|> skipChar '"' <|> skipChar ''')) "some excess"

let pexpr : _ Parser =
    let opp = new OperatorPrecedenceParser<Expr, unit, _>()

    let expr = opp.ExpressionParser

    let term =
        let pcallFuncOrArrOrVar =
            let pbraket = bet_ws '[' ']' (sepBy expr (skipChar ',' >>. ws))
            let pexplicitVar =
                pexplicitVar .>> ws .>>. opt pbraket
                |>> fun (var, arr) ->
                    match arr with
                    | Some args -> Arr(var, args)
                    | None -> Var var
            let pcallFunctionOrArrOrVar =
                let pfuncCall =
                    bet_ws '(' ')' (sepBy expr (pchar ',' >>. ws))
                    |>> fun args name -> Func(name, args)

                tuple2
                    (getPosition .>>.? notFollowedByBinOpIdent .>>. getPosition
                     .>> ws)
                    ((pfuncCall |>> fun f -> TokenType.Function, f)
                      <|> (pbraket
                           |>> fun arg ->
                                let f name = Arr((ImplicitNumericType, name), arg)
                                TokenType.Variable, f)
                      <|>% (TokenType.Variable, fun name -> Var(ImplicitNumericType, name)))
                >>= fun (((p1, name), p2), (tokenType, f)) ->
                    appendToken2 tokenType p1 p2
                    >>. preturn (f name)
            pexplicitVar <|> pcallFunctionOrArrOrVar
        let pval =
            choice [
                stringLiteralWithToken <|> pbraces |>> String
                pint32 |>> Int
            ]
            |>> Val
        pval <|> pcallFuncOrArrOrVar <|> bet_ws '(' ')' expr

    opp.TermParser <- term .>> ws

    Op.ops
    |> Array.iter (fun (opTyp, (opName, isSymbolic)) ->
        let prec = Precedences.prec <| Precedences.OpB opTyp
        if isSymbolic then
            InfixOperator(opName, ws, prec, Associativity.Left, fun x y -> Expr(opTyp, x, y))
            |> opp.AddOperator
        else
            let afterStringParser = notFollowedVarCont >>. ws
            InfixOperator(opName, afterStringParser, prec, Associativity.Left, fun x y -> Expr(opTyp, x, y))
            |> opp.AddOperator
            // TODO: @low в QSP все операторы case-insensitive
            InfixOperator(opName.ToUpper(), afterStringParser, prec, Associativity.Left, fun x y -> Expr(opTyp, x, y))
            |> opp.AddOperator
    )

    Reflection.Reflection.unionEnum
    |> Array.iter (fun unT ->
        // TODO: `-var` и `-(expr)` — можно, `- var` и `- (expr)` — нельзя
        let afterStringParser opName =
            if String.forall isLetter opName then
                notFollowedVarCont
                >>. ws
            else
                ws
        let unarOp = UnarOp.toString unT
        let prec = Precedences.prec <| Precedences.PrefB unT
        // TODO: @low в QSP все операторы case-insensitive
        PrefixOperator(unarOp, afterStringParser unarOp, prec, false, fun x -> UnarExpr(unT, x))
        |> opp.AddOperator
    )
    expr <?> "expr"
