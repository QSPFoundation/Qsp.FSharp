module Qsp.Tokens

type Range = FParsec.Position * FParsec.Position

[<Struct>]
type TokenType =
    | If
    | ElseIf
    | Else
    | Act
    | Colon
    | End
    | Underscore
    | Exit

    | For
    | To
    | Step

    | Loop
    | While

    /// в TS `var` называется `storage.type.js`
    | Type
    | Keyword
    /// `keyword.symbol.fsharp`
    | KeywordSymbol

    | SharpBeginLoc
    | MinusEndLoc

    | Function
    /// В QSP `comment.line` и `comment.block` объединены
    | Comment

    | Procedure
    | Variable
    /// `keyword.operator.assignment.js`
    ///
    /// `=`
    | OperatorAssignment
    // /// `keyword.operator.arithmetic.js`
    // ///
    // /// `-` `+` `*` `/`
    // | OperatorArithmetic
    // /// `keyword.operator.comparison.js`
    // ///
    // /// `=`
    // | OperatorComparison
    // /// `keyword.operator.relational.js`
    // ///
    // /// `>` `>=` `<` `<=`
    // | OperatorRelational
    /// `punctuation.terminator.statement.js`
    ///
    /// `&`
    | PunctuationTerminatorStatement

    | UnaryOperator of UnaryOperator : Ast.UnarOp
    | BinaryOperator of BinaryOperator : Ast.Op

    // | PunctuationDefinitionStringBegin
    // | PunctuationDefinitionStringEnd
    | StringQuotedDouble
    | StringQuotedSingle
    | StringBraced
    // | ConstantCharacterEscape
    /// `entity.name.label.cs`
    | NameLabel

    /// `punctuation.separator.colon.cs`
    | LabelColon
    /// `punctuation.definition.interpolation.begin.cs`
    ///
    /// `<<`
    | InterpolationBegin
    /// `punctuation.definition.interpolation.end.cs`
    ///
    /// `>>`
    | InterpolationEnd

    | ConstantNumericInteger
    /// meta.brace.square.js
    ///
    /// `[`
    | BraceSquareOpened
    /// meta.brace.square.js
    ///
    /// `]`
    | BraceSquareClosed
type InlineRange =
    {
        Line: int64
        Column1: int64
        Column2: int64
    }

module InlineRange =
    let ofFParsecPositions (p1: FParsec.Position) (p2: FParsec.Position) =
        {
            Line = p1.Line // Должно выполняться условие `p1.Line = p2.Line`
            Column1 = p1.Column
            Column2 = p2.Column // Должно выполняться условие `p2.Column > p1.Column`
        }

type Token =
    { TokenType: TokenType
      Range: InlineRange }
