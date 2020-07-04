module Qsp.Tokens

type Range =
    { StartColumn : int
      StartLine : int
      EndColumn : int
      EndLine : int }
type TokenType =
    | Keyword
    /// `act`, `if`, `:`, `end`
    | KeywordControl
    | Function
    /// Она же, наверное, процедура... А чем, собственно, оператор отличается от функции?
    | Procedure

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

    | PunctuationDefinitionStringBegin
    | PunctuationDefinitionStringEnd
    | StringQuotedDouble
    | StringQuotedSingle
    | ConstantCharacterEscape
type Token =
    { TokenType: TokenType
      Range: Range }
