module Qsp.Html.SimpleParser
open FParsec
open Qsp
open Qsp.Parser.Generic
// type 'a Parser = Parser<'a, unit>


let ident =
    many1Satisfy2L
        isLetter
        (fun c -> isLetter c || isDigit c || c = '_' || c = '.' || c = ':')
        "ident"

open FsharpMyExtension.XmlBuilder

let text =
    many1SatisfyL ((<>) '<') "text"
    |>> Text
let attributes: _ Parser =
    many (ident .>> pchar '=' .>>. stringLiteral)

// `<` и `>` — punctuation.definition.tag.xml

// Ключевые Entities:
// &lt; &#60;
// &amp; &#38;
// &quot; &#34;
// &apos; &#39;
type Entity =
    | NumericEntity of int
    | StringEntity of string
let pEntity =
    pchar '&'
    >>. (pchar '#' >>. pint32 |>> NumericEntity
         <|> (ident |>> StringEntity))
    .>> pchar ';'

let pnode: _ Parser =
    let pnode, pnodeRef = createParserForwardedToRef()
    pchar '<' >>. spaces >>. applyRange ident .>>. (spaces1 >>. attributes <|>% [])
    .>> pchar '>'
    .>>. many (text <|> pnode)
    .>>. (pchar '<' >>. pchar '/' >>. spaces >>. applyRange ident .>> spaces .>> pchar '>')
    |>> fun ((((openedTagRange, openedTag), z), y), (closedTagRange, closedTag)) ->
        openedTagRange // TODO

