module Qsp.Tokens

type Range =
    { StartColumn : int
      StartLine : int
      EndColumn : int
      EndLine : int }
type TokenType =
    | Keyword
    /// `if`, `:`, `end`
    | KeywordControl
type Token =
    { TokenType: TokenType
      Range: Range }
