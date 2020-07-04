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

let ident<'UserState> =
    let isIdentifierChar c = isLetter c || isDigit c || c = '_' || c = '.'

    skipChar '_' >>? many1Satisfy isIdentifierChar
    |>> fun ident -> "_" + ident
    <|> many1Satisfy2L isLetter isIdentifierChar "identifier"
    : Parser<_, 'UserState>

let ws<'UserState> =
    skipManySatisfy (fun c -> System.Char.IsWhiteSpace c && c <> '\n')
    : Parser<_, 'UserState>
let ws1<'UserState> =
    skipMany1Satisfy (fun c -> System.Char.IsWhiteSpace c && c <> '\n')
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

open Fuchu
[<Tests>]
let stringLiteralTest =
    testList "stringLiteralTest" [
        testCase "1" <| fun () ->
            Assert.Equal("", Right " ", runEither stringLiteral "\" \"")
        testCase "2" <| fun () ->
            Assert.Equal("", Right "\"", runEither stringLiteral "\"\"\"\"")
        testCase "3" <| fun () ->
            Assert.Equal("", Right "\"'", runEither stringLiteral "\"\"'\"\"")
        testCase "5" <| fun () ->
            Assert.Equal("", Right "", runEither stringLiteral "''")
        testCase "6" <| fun () ->
            Assert.Equal("", Right "'", runEither stringLiteral "''''")
        testCase "4" <| fun () ->
            Assert.Equal("", Right "\"", runEither stringLiteral "'\"'")
        testCase "braces1" <| fun () ->
            Assert.Equal("", Right "abc", runEither stringLiteral "{abc}")
        testCase "braces escaped" <| fun () ->
            Assert.Equal("", Right "}", runEither stringLiteral "{}}}")
    ]

type State =
    {
        Tokens: Qsp.Tokens.Token list
        /// Здесь ошибки только те, что могут определиться во время поверхностного семантического разбора, то есть это то, что не нуждается в нескольких проходах. Например, можно определить, что в коде пытаются переопределить встроенную функцию, и это будет ошибкой.
        ///
        /// А если хочется понять, что инструкция `gt 'some loc'` верна, то придется пройтись дважды, чтобы определить, существует ли вообще `'some loc'`. Если бы локации определялись последовательно, то есть нельзя было бы обратиться к той, что — ниже, тогда потребовался только один проход. Но в таком случае придется вводить что-то вроде `rec`, чтобы перейти на локацию, определенную ниже. Но всё это возвращает к той же задаче, потому ну его.
        SemanticErrors: (Qsp.Tokens.Range * string) list
        /// Информация обо всём и вся
        Hovers: (Qsp.Tokens.Range * string) list
    }
let emptyState =
    {
        Tokens = []
        SemanticErrors = []
        Hovers = []
    }
type 'a Parser = Parser<'a, State>
