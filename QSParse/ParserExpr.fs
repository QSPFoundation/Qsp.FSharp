module Qsp.Parser.Expr
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either
open Qsp
open Qsp.Ast
open Qsp.Parser.Generic

let pbinaryOperator : _ Parser =
    // keywords
    // transferOperators
    // procedures
    Defines.binaryOperators
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
    varType .>>. p
    |>> fun (typ, x) -> typ, x
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

let pexpr : _ Parser =
    let opp = new OperatorPrecedenceParser<Expr, unit, _>()
    let expr = opp.ExpressionParser
    /// Допустим, у нас есть заданная функция `mid`, и есть правила
    let notFollowedNewLiter =
        notFollowedBy (satisfy (fun c -> isDigit c || c = '_' || c = '.'))
    let notFollowedByBinOp =
        // конечно, тут нужно объяснить пользователю, почему именно нельзя использовать то или иное слово
        // проще назвать, что допустимо
        // let p =
        //     choice [
        //         spaces1
        //         skipChar '"'
        //         skipChar '''
        //         eof
        //     ]
        let p = pbinaryOperator .>> notFollowedNewLiter
        notFollowedByL p "выражение, а не строковый бинарный оператор"
        // notFollowedByL (keywords >>. p) "нельзя использовать здесь ключевое слово"
        // let keys = notFollowedByL ((choice <| List.map pstringCI ["let"; "set"; "if"; "end"; "elseif"; "else"; "act"; "no"; "obj"]) >>. (spaces1 <|> eof <|> skipChar '"' <|> skipChar ''')) "some excess"

    let term =
        let varOrCall =
            let pbraket = bet_ws '[' ']' (sepBy expr (skipChar ',' >>. ws))
            let pexplicitVar =
                pexplicitVar .>> ws .>>. opt pbraket
                |>> fun (var, arr) ->
                    match arr with
                    | Some args -> Arr(var, args)
                    | None -> Var var
            let pcallDefFunction =
                // let pfunction = functions |> List.map pstring |> choice
                // pdefProcOrFunc <|> identifier
                // подожди, теперь идет очередь семантики.
                // indent
                let pfuncCall =
                    bet_ws '(' ')' (sepBy expr (pchar ',' >>. ws))
                    |>> fun args name -> Func(name, args)
                notFollowedByBinOp
                >>. ident .>> ws
                .>>. (pfuncCall
                      <|> (pbraket |>> fun arg name -> Arr((ImplicitNumericType, name), arg))
                      <|>% fun name -> Var(ImplicitNumericType, name))

                |>> fun (name, f) -> f name
            pexplicitVar <|> pcallDefFunction
        let pval =
            choice [
                stringLiteral |>> String
                pint32 |>> Int
            ]
            |>> Val
        pval <|> varOrCall <|> bet_ws '(' ')' expr

    opp.TermParser <- term .>> ws

    let afterStringParser opName =
        if String.forall isLetter opName then
            notFollowedBy (satisfy (fun c -> isDigit c || c = '_' || c = '.'))
            >>. ws
        else
            ws
    Reflection.Reflection.unionEnum
    |> Array.iter (fun opT ->
        let binOp = Op.toString opT
        let prec = Precedences.prec <| Precedences.OpB opT
        InfixOperator(binOp, afterStringParser binOp, prec, Associativity.Left, fun x y -> Expr(opT, x, y))
        |> opp.AddOperator
    )
    Reflection.Reflection.unionEnum
    |> Array.iter (fun unT ->
        let unarOp = UnarOp.toString unT
        let prec = Precedences.prec <| Precedences.PrefB unT
        PrefixOperator(unarOp, afterStringParser unarOp, prec, false, fun x -> UnarExpr(unT, x))
        |> opp.AddOperator
    )
    expr
