module Qsp.Parser.Generic
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either
open Qsp

let fparsecPosToPos (pos:FParsec.Position) =
    Ast.positionCreate pos.StreamName pos.Index pos.Line pos.Column

let runEither p str =
    match run p str with
    | Success(x, _, _) -> Right x
    | Failure(x, _, _) -> Left x
let runStateEither p st str =
    match runParserOnString p st "" str with
    | Success(x, st, _) -> st, Right(x)
    | Failure(x, _, st) -> st, Left(x)
let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = '.' || c = '$'

let ident<'UserState> =
    skipChar '_' >>? many1Satisfy isIdentifierChar
    |>> fun ident -> "_" + ident
    <|> many1Satisfy2L (fun c -> isLetter c || c = '#') isIdentifierChar "identifier"
    : Parser<_, 'UserState>

let ws<'UserState> =
    skipManySatisfy (fun c -> System.Char.IsWhiteSpace c && c <> '\n')
    : Parser<_, 'UserState>
let ws1<'UserState> =
    skipMany1SatisfyL (fun c -> System.Char.IsWhiteSpace c && c <> '\n') "any white space except '\\n'"
    : Parser<_, 'UserState>

let char_ws c = pchar c .>> ws
let bet opened closed = between <| char_ws opened <| pchar closed
let bet_ws opened closed p = bet opened closed p .>> ws
let optList p = p <|>% []

let nl<'UserState> = skipMany1 newline : Parser<unit, 'UserState>

let stringLiteral<'UserState> =
    let normalChar c = satisfy (fun c' -> c' <> c)
    let p c = manyChars (normalChar c <|> attempt(pchar c >>. pchar c))

    let bet openedChar closedChar = between (pchar openedChar) (pchar closedChar)
    bet '"' '"' (p '"')
    <|> bet '\'' '\'' (p '\'')
    <|> bet '{' '}' (p '}') // TODO: забавно: проверил компилятор, и тот напрочь не воспринимает экранирование `}}`
    : Parser<_, 'UserState>


/// Дело в том, что названия переменных могут начинаться с ключевых слов ("**if**SomethingTrue", например), а значит, чтобы это пресечь, можно воспользоваться именно этой функцией так:
/// ```fsharp
/// pstring "if" .>>? notFollowedVar
/// ```
let notFollowedVarCont<'UserState> =
    notFollowedBy (satisfy isIdentifierChar)
    : Parser<_, 'UserState>

type VarHighlightKind =
    | ReadAccess
    | WriteAccess

type VarHighlights =
    {
        VarScopeSystem: Scope.ScopeSystem<Ast.VarName, Tokens.InlineRange * VarHighlightKind>
        Ranges: (Tokens.InlineRange * Scope.VarId) list
    }
let varHighlightsEmpty =
    {
        VarScopeSystem = Scope.scopeSystemEmpty
        Ranges = []
    }

type LocHighlights =
    {
        Ma: Map<Ast.LocationName, (Tokens.InlineRange * VarHighlightKind) list>
        Ranges: (Tokens.InlineRange * Ast.LocationName) list
    }
let locHighlightsEmpty =
    {
        Ma = Map.empty
        Ranges = []
    }
type Highlights =
    {
        VarHighlights: VarHighlights
        LocHighlights: LocHighlights
    }
let highlightsEmpty =
    {
        VarHighlights = varHighlightsEmpty
        LocHighlights = locHighlightsEmpty
    }
type HoverDescription =
    | FuncDescription of Defines.PredefFunc
    // | VarDescription of Defines.
    | RawDescription of string

type 'a Parser = Parser<'a, State>

and State =
    {
        Tokens: Tokens.Token list
        /// Здесь ошибки только те, что могут определиться во время поверхностного семантического разбора, то есть это то, что не нуждается в нескольких проходах. Например, можно определить, что в коде пытаются переопределить встроенную функцию, и это будет ошибкой.
        ///
        /// А если хочется понять, что инструкция `gt 'some loc'` верна, то придется пройтись дважды, чтобы определить, существует ли вообще `'some loc'`. Если бы локации определялись последовательно, то есть нельзя было бы обратиться к той, что — ниже, тогда потребовался только один проход. Но в таком случае придется вводить что-то вроде `rec`, чтобы перейти на локацию, определенную ниже. Но всё это возвращает к той же задаче, потому ну его.
        SemanticErrors: (Tokens.InlineRange * string) list
        /// Информация обо всём и вся
        Hovers: (Tokens.InlineRange * HoverDescription) list
        Highlights: Highlights
        /// Нужен для `elseif` конструкции. Эх, если бы ее можно было как-то именно там оставить, но увы.
        IsEndOptional : bool
        LastSymbolPos : FParsec.Position
        /// Локации, которые неопределенны именно в этом документе, но переходы к ним есть
        NotDefinedLocs: Map<Ast.LocationName, Tokens.InlineRange list>
        // Я тут, это самое, оставлю. Никто не возражает?
        PStmts: Parser<Ast.PosStatement list>
        /// `&lt;a gt ''x''>`
        SingleQuotNestedCount: int
        DoubleQuotNestedCount: int
        HtmlAttDoubleNested: int
    }
let emptyState =
    {
        Tokens = []
        SemanticErrors = []
        Hovers = []
        IsEndOptional = false
        LastSymbolPos = FParsec.Position("", 0L, 1L, 1L)
        Highlights = highlightsEmpty
        NotDefinedLocs = Map.empty
        PStmts = FParsec.Primitives.failFatally "PStmts not implemented"
        SingleQuotNestedCount = 0
        DoubleQuotNestedCount = 0
        HtmlAttDoubleNested = 0
    }
let updateScope fn =
    updateUserState (fun x ->
        let ss = x.Highlights.VarHighlights.VarScopeSystem

        { x with
            Highlights =
                { x.Highlights with
                    VarHighlights =
                        { x.Highlights.VarHighlights with
                            VarScopeSystem = fn ss
                        }
                }
        })
let pGetDefLocPos locName =
    getUserState
    |>> fun st ->
        match Map.tryFind locName st.Highlights.LocHighlights.Ma with
        | None ->
            None
        | Some(value) ->
            value
            |> List.tryPick (fun (r, kind) ->
                match kind with
                | WriteAccess -> Some r
                | _ -> None
            )

let appendVarHighlight (r:Tokens.InlineRange) (var:Ast.Var) highlightKind isLocal =
    let var = mapSnd String.toLower var // for case-insensitively
    updateUserState (fun st ->
        { st with
            Highlights =
                {
                    st.Highlights with
                        VarHighlights =
                            let varHighlights = st.Highlights.VarHighlights

                            if not <| isLocal then
                                let v = r, highlightKind
                                let varId, ss = Scope.addAsRead (snd var, (fun xs -> v::xs)) varHighlights.VarScopeSystem
                                {
                                    Ranges = (r, varId)::st.Highlights.VarHighlights.Ranges
                                    VarScopeSystem = ss
                                }
                            else
                                let v = r, highlightKind
                                let varId, ss = Scope.addAsWrite (snd var, fun xs -> v::xs) varHighlights.VarScopeSystem
                                {
                                    Ranges = (r, varId)::st.Highlights.VarHighlights.Ranges
                                    VarScopeSystem = ss
                                }
                }
        }
    )
let appendLocHighlight (r:Tokens.InlineRange) (loc:string) highlightKind =
    let loc = String.toLower loc // без шуток, они тоже case-insensitively, хотя и представляют из себя string
    updateUserState (fun st ->
        { st with
            Highlights =
                {
                    st.Highlights with
                        LocHighlights =
                            {
                                Ranges = (r, loc)::st.Highlights.LocHighlights.Ranges
                                Ma =
                                    let v = r, highlightKind
                                    st.Highlights.LocHighlights.Ma
                                    |> Map.addOrMod loc [v] (fun xs -> v::xs)
                            }
                }
        }
    )

let appendToken2 tokenType r =
    updateUserState (fun st ->
        let token =
            { Tokens.TokenType = tokenType
              Tokens.Range = r }
        { st with Tokens = token :: st.Tokens }
    )

let toRange (p1:FParsec.Position) (p2:FParsec.Position) =
    {
        Tokens.InlineRange.Line = p1.Line // Должно выполняться условие `p1.Line = p2.Line`
        Tokens.InlineRange.Column1 = p1.Column
        Tokens.InlineRange.Column2 = p2.Column // Должно выполняться условие `p2.Column > p1.Column`
    }
let appendToken tokenType p =
    getPosition .>>.? p .>>. getPosition
    >>= fun ((p1, p), p2) ->
        let r = toRange p1 p2
        appendToken2 tokenType r
        >>. preturn p

let applyRange p =
    getPosition .>>.? p .>>. getPosition
    >>= fun ((p1, p), p2) ->
        let range = toRange p1 p2
        preturn (range, p)

let appendHover2 msg range =
    updateUserState (fun st ->
        { st with Hovers = (range, msg) :: st.Hovers }
    )

let appendSemanticError range msg =
    updateUserState (fun st ->
        { st with SemanticErrors =
                    (range, msg) :: st.SemanticErrors })

let appendHover msg p =
    (getPosition .>>.? p .>>. getPosition)
    >>= fun ((p1, p), p2) ->
        let r = toRange p1 p2
        appendHover2 msg r
        >>. preturn p
let appendTokenHover tokenType msg p =
    (getPosition .>>.? p .>>. getPosition)
    >>= fun ((p1, p), p2) ->
        let r = toRange p1 p2
        appendToken2 tokenType r
        >>. appendHover2 msg r
        >>. preturn p

let pSingleNested =
    updateUserState (fun st ->
        { st with
            SingleQuotNestedCount = st.SingleQuotNestedCount + 1
        })
let pSingleUnnested =
    updateUserState (fun st ->
        { st with
            SingleQuotNestedCount = st.SingleQuotNestedCount - 1
        })
let pGetSingleNested =
    getUserState |>> fun x -> x.SingleQuotNestedCount
let pDoubleNested =
    updateUserState (fun st ->
        { st with
            DoubleQuotNestedCount = st.DoubleQuotNestedCount + 1
        })
let pDoubleUnnested =
    updateUserState (fun st ->
        { st with
            DoubleQuotNestedCount = st.DoubleQuotNestedCount - 1
        })
let pGetDoubleNested =
    getUserState |>> fun x -> x.DoubleQuotNestedCount
let pHtmlAttDoubleNested =
    updateUserState (fun st ->
        { st with
            HtmlAttDoubleNested = st.HtmlAttDoubleNested + 1
        })
let pHtmlAttDoubleUnnested =
    updateUserState (fun st ->
        { st with
            HtmlAttDoubleNested = st.HtmlAttDoubleNested - 1
        })
let pGetHtmlAttDoubleNested =
    getUserState |>> fun x -> x.HtmlAttDoubleNested

open Tokens

let charsReplicate n (c:char) =
    System.String.Concat (Array.replicate n c)

// Это такой фокус, чтобы напрочь во всем запутаться. А кто говорил, что это чисто функциональное программирование? Ну-ну.
let pstmts : _ Parser =
    getUserState >>= fun st -> st.PStmts
let stringLiteralWithToken pexpr : _ Parser =
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
                        |>> Ast.StaticStmts)
                     <|> (appendToken tokenType (many1Strings (pattValBody nestedCount '"')) // TODO: здесь можно и нужно отобразить подстановки.
                          |>> Ast.Raw))
                .>> pchar '"' .>> spaces .>> pchar '>' // что ж, не всё так просто. Дело в том, что во вложенном `pstmts` все `stringLiteral` заместо привычных `"` и `'` использует либо `&quot;` и `''`, либо `&apos;`. Да еще и `&` экранирует в `&amp;`. И всё это кучу раз вкладывается и перевкладывается. Честно сказать, голова пухнет от всех этих страстей. А еще на `if` жаловался, ну-ну.
                .>>. plineKinds .>> pstring "</a>" // вот надо были тебе эти дурацкие вложения? Еще скажи, что хотел полноценный HTML-parser сделать. Ой, точно, хочет! Ха-ха.
                |>> fun (stmts, line) -> Ast.HyperLinkKind(stmts, line) // Вот смотрю я на эти былины и диву даюсь, право слово. Это ж надо было до такого додуматься. Метаметамета...программирование какое-то
            plineKindRef :=
                appendToken tokenType (many1Strings (p nestedCount)) |>> Ast.StringKind
                <|> (appendToken TokenType.InterpolationBegin (pstring "<<")
                     >>. (ws >>. pexpr |>> Ast.ExprKind) // это может *немного* запутать, но, эм, но есть какое-то "но", да... Никакого "но" нету — код безнадежно запутанный 😭. Так, здесь экранизация — внутри экранизации, поэтому порождает в два раза больше открывающих скобок. Я сделал всего два уровня и наивно надеюсь, что этого хватит. То есть сейчас он обрабатывает вот эту зверюгу: `'<<''<<''''x''''>>''>>'`. Страшно, правда? Но что-то мне подсказывает, что это так не работает. Проверил, работает, что еще больше ужасает. И `'<<''<<''''<<''''''''это чудовище''''''''>>''''>>''>>'` работает...
                     .>> ws .>> appendToken TokenType.InterpolationEnd (pstring ">>"))
                <|> attempt pATag // TODO: тут бы предупреждение какое-нибудь не помешало: мол, не осилил
            plineKind <|> (pchar '<' >>% Ast.StringKind "<")
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
                (fun x xs -> (x:Ast.Line)::xs)
    bet TokenType.StringQuotedSingle '\'' '\'' pSingleNested pSingleUnnested pGetSingleNested
    <|> (pGetHtmlAttDoubleNested >>=? fun x ->
         if x > 0 then
            fail "not implemented HtmlAttDoubleNested"
         else
            bet TokenType.StringQuotedDouble '"' '"' pDoubleNested pDoubleUnnested pGetDoubleNested)

let pbraces tokenType : _ Parser =
    let pbraces, pbracesRef = createParserForwardedToRef()
    let p = many1Satisfy (isNoneOf "{}\n")

    pbracesRef :=
        pipe2
            (appendToken tokenType
                (many1Satisfy2 ((=) '{') (isNoneOf "{}\n")) )
            (many
                (appendToken tokenType (many1Strings p)
                 <|> newlineReturn "\n"
                 <|> pbraces
                )
             .>>. appendToken tokenType (pchar '}'))
            (fun x (xs, closedChar) ->
                seq {
                    yield x
                    yield! xs
                    yield string closedChar
                }
                |> System.String.Concat
            )
    pipe2
        (appendToken tokenType
            (pchar '{' >>. manyStrings p)
         .>>. opt (newlineReturn "\n"))
        (many
            (appendToken tokenType (many1Strings p)
             <|> newlineReturn "\n"
             <|> pbraces
            )
         .>> appendToken tokenType (pchar '}')) // TODO: Здесь самое то использовать `PunctuationDefinitionStringEnd`
        (fun (x, nl) xs ->
            match nl with
            | None ->
                x::xs |> System.String.Concat
            | Some nl ->
                x::nl::xs |> System.String.Concat)
