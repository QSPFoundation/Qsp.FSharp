module Qsp.Tokens

type Range = FParsec.Position * FParsec.Position

type TokenType =
    /// в TS `var` называется `storage.type.js`
    | Type
    | Keyword
    /// `act`, `if`, `:`, `end`
    | KeywordControl
    | Function
    /// В QSP `comment.line` и `comment.block` объединены
    | Comment

    | Procedure
    | Variable
    /// `keyword.operator.assignment.js`
    ///
    /// `=`
    | OperatorAssignment
    /// `keyword.operator.arithmetic.js`
    ///
    /// `-` `+` `*` `/`
    | OperatorArithmetic
    /// `keyword.operator.comparison.js`
    ///
    /// `=`
    | OperatorComparison
    /// `keyword.operator.relational.js`
    ///
    /// `>` `>=` `<` `<=`
    | OperatorRelational
    /// `punctuation.terminator.statement.js`
    ///
    /// `&`
    | PunctuationTerminatorStatement

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
type Token =
    { TokenType: TokenType
      Range: Range }
