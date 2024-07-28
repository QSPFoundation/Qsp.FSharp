[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Qsp.Parser.Ast.Value

module Parser =
    open FParsec

    open Qsp.Ast
    open Qsp.Tokens
    open Qsp.Parser.Generic

    let stringLiteralWithToken pexpr (pstmts: Statements Parser) : _ Parser =
        let bet tokenType openedChar closedChar pnested punnested pgetNested =
            let p nestedCount =
                many1Satisfy (fun c' -> not (c' = closedChar || c' = '\n' || c' = '<'))
                <|> (pstring (charsReplicate (pown 2 nestedCount) closedChar) // 1 2, 2 4
                    >>% string closedChar)
                <|> (skipChar '<' >>? notFollowedBy (skipChar '<' <|> skipChar 'a' <|> skipString "/a>") >>% "<")
            let pattValBody nestedCount closedCharAtt =
                many1Satisfy (fun c' -> not (c' = closedChar || c' = '\n' || c' = '&' || c' = closedCharAtt))
                <|> (pstring (charsReplicate (pown 2 nestedCount) closedChar)
                    >>% string closedChar)
                <|> (pchar '&'
                    >>. ((pstring "quot" >>% "\"" <|> pstring "apos" >>% "'") .>> pchar ';'
                        <|>% "&") )
                // <|> (skipChar '<' >>? notFollowedBy (skipChar '<' <|> skipChar 'a' <|> skipString "/a>") >>% "<")
            let plineKind nestedCount =
                let plineKind, plineKindRef = createParserForwardedToRef()
                let plineKinds =
                    pipe2
                        (many plineKind)
                        (many
                            (newline >>. many plineKind))
                        (fun x xs -> x::xs)
                let pATag =
                    // А вот здесь вообще начинается прелюбопытная штука:
                    // 1. Все `"` экранируются в `&quot;`, а сам `&` — в `&amp;`
                    // 2. Если нужно еще вложить, то используется `&quot;&quot;`
                    pstring "<a href=\"exec:"
                    >>. (attempt // TODO: Если в значении аттрибута нету подстановки, тогда нужно пытататься разобрать его статически. К черту производительность, главное, понятность
                        (pHtmlAttDoubleNested
                            >>. spaces >>. notEmpty pstmts
                            .>> pHtmlAttDoubleUnnested
                            |>> StaticStmts)
                        <|> (appendToken tokenType (many1Strings (pattValBody nestedCount '"')) // TODO: здесь можно и нужно отобразить подстановки.
                            |>> Raw))
                    .>> pchar '"' .>> spaces .>> pchar '>' // что ж, не всё так просто. Дело в том, что во вложенном `pstmts` все `stringLiteral` заместо привычных `"` и `'` использует либо `&quot;` и `''`, либо `&apos;`. Да еще и `&` экранирует в `&amp;`. И всё это кучу раз вкладывается и перевкладывается. Честно сказать, голова пухнет от всех этих страстей. А еще на `if` жаловался, ну-ну.
                    .>>. plineKinds .>> pstring "</a>" // вот надо были тебе эти дурацкие вложения? Еще скажи, что хотел полноценный HTML-parser сделать. Ой, точно, хочет! Ха-ха.
                    |>> fun (stmts, line) -> HyperLinkKind(stmts, line) // Вот смотрю я на эти былины и диву даюсь, право слово. Это ж надо было до такого додуматься. Метаметамета...программирование какое-то
                plineKindRef.Value <-
                    appendToken tokenType (many1Strings (p nestedCount)) |>> StringKind
                    <|> (appendToken TokenType.InterpolationBegin (pstring "<<")
                        >>. (ws >>. pexpr |>> ExprKind) // это может *немного* запутать, но, эм, но есть какое-то "но", да... Никакого "но" нету — код безнадежно запутанный 😭. Так, здесь экранизация — внутри экранизации, поэтому порождает в два раза больше открывающих скобок. Я сделал всего два уровня и наивно надеюсь, что этого хватит. То есть сейчас он обрабатывает вот эту зверюгу: `'<<''<<''''x''''>>''>>'`. Страшно, правда? Но что-то мне подсказывает, что это так не работает. Проверил, работает, что еще больше ужасает. И `'<<''<<''''<<''''''''это чудовище''''''''>>''''>>''>>'` работает...
                        .>> ws .>> appendToken TokenType.InterpolationEnd (pstring ">>"))
                    <|> attempt pATag // TODO: тут бы предупреждение какое-нибудь не помешало: мол, не осилил
                plineKind <|> (pchar '<' >>% StringKind "<")
            pgetNested >>=? fun nestedCount ->
            let pOpened = pstring (charsReplicate (pown 2 nestedCount) openedChar)
            let pClosed = pstring (charsReplicate (pown 2 nestedCount) closedChar)
            let plineKind = plineKind (nestedCount + 1)

            appendToken tokenType (pOpened .>> pnested)
            >>. pipe2
                    (many plineKind)
                    (many
                        (newline >>. many plineKind)
                    .>> punnested
                    .>> appendToken tokenType pClosed) // TODO: Здесь самое то использовать `PunctuationDefinitionStringEnd`
                    (fun x xs -> (x:Line)::xs)
        bet TokenType.StringQuotedSingle '\'' '\'' pSingleNested pSingleUnnested pGetSingleNested
        <|> (pGetHtmlAttDoubleNested >>=? fun x ->
            if x > 0 then
                fail "not implemented HtmlAttDoubleNested"
            else
                bet TokenType.StringQuotedDouble '"' '"' pDoubleNested pDoubleUnnested pGetDoubleNested)

    let pvalue pexpr pstmts : Value Parser =
        choice [
            stringLiteralWithToken pexpr pstmts |>> String

            appendToken TokenType.ConstantNumericInteger (
                pint32 |>> Int
            )
        ]
