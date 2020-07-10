module Qsp.Parser.Generic
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either
open Qsp

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

/// A document highlight kind.
[<RequireQualifiedAccess>]
type DocumentHighlightKind =
    /// A textual occurrence.
    | Text = 1

    /// Read-access of a symbol, like reading a variable.
    | Read = 2

    /// Write-access of a symbol, like writing to a variable.
    | Write = 3
type VarHighlightKind =
    | ReadAccess
    | WriteAccess
// type Var =
type VarHighlights =
    {
        Ma: Map<Ast.Var, (Tokens.InlineRange * VarHighlightKind) list>
        Ranges: (Tokens.InlineRange * Ast.Var) list
    }
let varHighlightsEmpty =
    {
        Ma = Map.empty
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
type State =
    {
        Tokens: Tokens.Token list
        /// Здесь ошибки только те, что могут определиться во время поверхностного семантического разбора, то есть это то, что не нуждается в нескольких проходах. Например, можно определить, что в коде пытаются переопределить встроенную функцию, и это будет ошибкой.
        ///
        /// А если хочется понять, что инструкция `gt 'some loc'` верна, то придется пройтись дважды, чтобы определить, существует ли вообще `'some loc'`. Если бы локации определялись последовательно, то есть нельзя было бы обратиться к той, что — ниже, тогда потребовался только один проход. Но в таком случае придется вводить что-то вроде `rec`, чтобы перейти на локацию, определенную ниже. Но всё это возвращает к той же задаче, потому ну его.
        SemanticErrors: (Tokens.InlineRange * string) list
        /// Информация обо всём и вся
        Hovers: (Tokens.InlineRange * string) list
        Highlights: Highlights
        /// Нужен для `elseif` конструкции. Эх, если бы ее можно было как-то именно там оставить, но увы.
        IsEndOptional : bool
        LastSymbolPos : FParsec.Position
        /// К ним обращаются раньше, чем она определена, потому проверяется по ходу дела
        LocsThatNeedCheck: Map<Ast.LocationName, Tokens.InlineRange list>
    }
let emptyState =
    {
        Tokens = []
        SemanticErrors = []
        Hovers = []
        IsEndOptional = false
        LastSymbolPos = FParsec.Position("", 0L, 1L, 1L)
        Highlights = highlightsEmpty
        LocsThatNeedCheck = Map.empty
    }
type 'a Parser = Parser<'a, State>

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

let appendVarHighlight (r:Tokens.InlineRange) (var:Ast.Var) highlightKind =
    let var = mapSnd String.toLower var // for case-insensitively
    updateUserState (fun st ->
        { st with
            Highlights =
                {
                    st.Highlights with
                        VarHighlights =
                            {
                                Ranges = (r, var)::st.Highlights.VarHighlights.Ranges
                                Ma =
                                    let v = r, highlightKind
                                    st.Highlights.VarHighlights.Ma
                                    |> Map.addOrMod var [v] (fun xs -> v::xs)
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
open Tokens

let stringLiteralWithToken pexpr : _ Parser =
    let bet tokenType openedChar closedChar =
        let p =
            many1Satisfy (fun c' -> not (c' = closedChar || c' = '\n' || c' = '<'))
            <|> (attempt(skipChar closedChar >>. skipChar closedChar)
                  >>% string closedChar)
            <|> (skipChar '<' >>? notFollowedBy (skipChar '<') >>% "<")
        let plineKind =
            appendToken tokenType (many1Strings p) |>> Ast.StringKind
            <|> (appendToken TokenType.InterpolationBegin (pstring "<<")
                 >>. (ws >>. pexpr |>> Ast.ExprKind)
                 .>> ws .>> appendToken TokenType.InterpolationEnd (pstring ">>"))
        pipe2
            (appendToken tokenType (pchar openedChar)
             >>. many plineKind)
            (many
                (newline >>. many plineKind)
             .>> appendToken tokenType (pchar closedChar)) // TODO: Здесь самое то использовать `PunctuationDefinitionStringEnd`
            (fun x xs -> (x:Ast.Line)::xs)
    bet TokenType.StringQuotedSingle '\'' '\''
    <|> bet TokenType.StringQuotedDouble '"' '"'

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
