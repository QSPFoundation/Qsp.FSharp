module Qsp.Parser.Generic
open FParsec
open FsharpMyExtension
open FsharpMyExtension.Either
open Qsp

let fparsecPosToPos (pos:FParsec.Position) =
    Ast.Position.create pos.StreamName pos.Index pos.Line pos.Column

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

module VarHighlights =
    let empty =
        {
            VarScopeSystem = Scope.scopeSystemEmpty
            Ranges = []
        }

type LocHighlights =
    {
        Ma: Map<Ast.LocationName, (Tokens.InlineRange * VarHighlightKind) list>
        Ranges: (Tokens.InlineRange * Ast.LocationName) list
    }

module LocHighlights =
    let empty =
        {
            Ma = Map.empty
            Ranges = []
        }

type Highlights =
    {
        VarHighlights: VarHighlights
        LocHighlights: LocHighlights
    }

module Highlights =
    let empty =
        {
            VarHighlights = VarHighlights.empty
            LocHighlights = LocHighlights.empty
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
        /// `&lt;a gt ''x''>`
        SingleQuotNestedCount: int
        DoubleQuotNestedCount: int
        HtmlAttDoubleNested: int
    }

module State =
    let empty =
        {
            Tokens = []
            SemanticErrors = []
            Hovers = []
            IsEndOptional = false
            LastSymbolPos = FParsec.Position("", 0L, 1L, 1L)
            Highlights = Highlights.empty
            NotDefinedLocs = Map.empty
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

let appendToken tokenType p =
    getPosition .>>.? p .>>. getPosition
    >>= fun ((p1, p), p2) ->
        let r = Tokens.InlineRange.ofFParsecPositions p1 p2
        appendToken2 tokenType r
        >>. preturn p

let applyRange p =
    getPosition .>>.? p .>>. getPosition
    >>= fun ((p1, p), p2) ->
        let range = Tokens.InlineRange.ofFParsecPositions p1 p2
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
        let r = Tokens.InlineRange.ofFParsecPositions p1 p2
        appendHover2 msg r
        >>. preturn p
let appendTokenHover tokenType msg p =
    (getPosition .>>.? p .>>. getPosition)
    >>= fun ((p1, p), p2) ->
        let r = Tokens.InlineRange.ofFParsecPositions p1 p2
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

let pbraces tokenType : _ Parser =
    let pbraces, pbracesRef = createParserForwardedToRef()
    let p = many1Satisfy (isNoneOf "{}\n")

    pbracesRef.Value <-
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

let ppunctuationTerminator : _ Parser =
    appendToken TokenType.PunctuationTerminatorStatement (pchar '&')
