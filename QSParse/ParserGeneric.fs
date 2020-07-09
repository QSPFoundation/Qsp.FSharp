module Qsp.Parser.Generic
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either

let runEither p str =
    match run p str with
    | Success(x, _, _) -> Right x
    | Failure(x, _, _) -> Left x
let runStateEither p st str =
    match runParserOnString p st "" str with
    | Success(x, st, _) -> st, Right(x)
    | Failure(x, _, st) -> st, Left(x)
let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = '.'

let ident<'UserState> =
    skipChar '_' >>? many1Satisfy isIdentifierChar
    |>> fun ident -> "_" + ident
    <|> many1Satisfy2L isLetter isIdentifierChar "identifier"
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
type InlineRange =
    {
        Line: int64
        Column1: int64
        Column2: int64
    }
type State =
    {
        Tokens: Qsp.Tokens.Token list
        /// Здесь ошибки только те, что могут определиться во время поверхностного семантического разбора, то есть это то, что не нуждается в нескольких проходах. Например, можно определить, что в коде пытаются переопределить встроенную функцию, и это будет ошибкой.
        ///
        /// А если хочется понять, что инструкция `gt 'some loc'` верна, то придется пройтись дважды, чтобы определить, существует ли вообще `'some loc'`. Если бы локации определялись последовательно, то есть нельзя было бы обратиться к той, что — ниже, тогда потребовался только один проход. Но в таком случае придется вводить что-то вроде `rec`, чтобы перейти на локацию, определенную ниже. Но всё это возвращает к той же задаче, потому ну его.
        SemanticErrors: (InlineRange * string) list
        /// Информация обо всём и вся
        Hovers: (InlineRange * string) list
        /// Нужен для `elseif` конструкции. Эх, если бы ее можно было как-то именно там оставить, но увы.
        IsEndOptional : bool
        LastSymbolPos : FParsec.Position
    }
let emptyState =
    {
        Tokens = []
        SemanticErrors = []
        Hovers = []
        IsEndOptional = false
        LastSymbolPos = FParsec.Position("", 0L, 1L, 1L)
    }
type 'a Parser = Parser<'a, State>

let appendToken2 tokenType p1 p2 =
    updateUserState (fun st ->
        let token =
            { Qsp.Tokens.TokenType = tokenType
              Qsp.Tokens.Range = p1, p2 }

        { st with Tokens = token :: st.Tokens }
    )

let appendToken tokenType p =
    getPosition .>>.? p .>>. getPosition
    >>= fun ((p1, p), p2) ->
        appendToken2 tokenType p1 p2
        >>. preturn p
let toRange (p1:FParsec.Position, p2:FParsec.Position) =
    {
        Line = p1.Line // Должно выполняться условие `p1.Line = p2.Line`
        Column1 = p1.Column
        Column2 = p2.Column // Должно выполняться условие `p2.Column > p1.Column`
    }
let appendHover2 msg (p1:FParsec.Position) (p2:FParsec.Position) =
    updateUserState (fun st ->
        let range = toRange (p1, p2)
        { st with Hovers = (range, msg) :: st.Hovers }
    )

let appendSemanticError (p1:FParsec.Position) (p2:FParsec.Position) msg =
    let range = toRange (p1, p2)
    updateUserState (fun st ->
        { st with SemanticErrors =
                    (range, msg) :: st.SemanticErrors })
// let appendSemanticError2 p getMsg =
//     getPosition .>>.? p .>>. getPosition
//     >>= fun ((p1, x), p2) ->
//         appendSemanticError p1 p2 (getMsg x)
//         >>. preturn x
let appendHover msg p =
    (getPosition .>>.? p .>>. getPosition)
    >>= fun ((p1, p), p2) ->
        appendHover2 msg p1 p2
        >>. preturn p
let appendTokenHover tokenType msg p =
    (getPosition .>>.? p .>>. getPosition)
    >>= fun ((p1, p), p2) ->
        appendToken2 tokenType p1 p2
        >>. appendHover2 msg p1 p2
        >>. preturn p
open Qsp.Tokens

let stringLiteralWithToken : _ Parser =
    // Если не разбивать, то VS Code выбьет: "`range` cannot span multiple lines"
    let bet tokenType openedChar closedChar =
        let p =
            many1Satisfy (fun c' -> not (c' = closedChar || c' = '\n'))
            <|> (attempt(skipChar closedChar >>. skipChar closedChar)
                  >>% string closedChar)
        pipe2
            (appendToken tokenType (pchar openedChar)
             >>. appendToken tokenType (manyStrings p))
            (many
                (newline >>. appendToken tokenType (manyStrings p))
             .>> appendToken tokenType (pchar closedChar)) // TODO: Здесь самое то использовать `PunctuationDefinitionStringEnd`
            (fun x xs ->
                x::xs |> String.concat "\n")
    bet TokenType.StringQuotedSingle '\'' '\''
    <|> bet TokenType.StringQuotedDouble '"' '"'

let pbraces: _ Parser =
    let pbraces, pbracesRef = createParserForwardedToRef()
    let p = many1Satisfy (isNoneOf "{}\n")

    pbracesRef :=
        pipe2
            (appendToken TokenType.StringBraced
                (many1Satisfy2 ((=) '{') (isNoneOf "{}\n")) )
            (many
                (appendToken TokenType.StringBraced (many1Strings p)
                 <|> newlineReturn "\n"
                 <|> pbraces
                )
             .>>. appendToken TokenType.StringBraced (pchar '}'))
            (fun x (xs, closedChar) ->
                seq {
                    yield x
                    yield! xs
                    yield string closedChar
                }
                |> System.String.Concat
            )
    pipe2
        (appendToken TokenType.StringBraced
            (pchar '{' >>. manyStrings p)
         .>>. opt (newlineReturn "\n"))
        (many
            (appendToken TokenType.StringBraced (many1Strings p)
             <|> newlineReturn "\n"
             <|> pbraces
            )
         .>> appendToken TokenType.StringBraced (pchar '}')) // TODO: Здесь самое то использовать `PunctuationDefinitionStringEnd`
        (fun (x, nl) xs ->
            match nl with
            | None ->
                x::xs |> System.String.Concat
            | Some nl ->
                x::nl::xs |> System.String.Concat)
