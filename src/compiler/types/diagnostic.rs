type ErrorCode = usize;

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub enum Category {
    Error,
    Warning,
    Suggestion,
    Message,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum Message {
    UnterminatedStringLiteral,
    IdentifierExpected,
    Expected { item: String },
    FileCannotReferenceItself,
    TrailingCommaNotAllowed,
    AsteriskSlashExpected,
    ElementAccessExpressionShouldTakeArgument,
    UnexpectedToken,
    RestParameterOrBindingPatternMayNotHaveTrailingComma,
    RestParameterMustBeLastInParameterList,
}

impl Message {
    /// Get the category of this message.
    ///
    /// # Examples
    /// ```rust
    /// assert_eq!(Message::UnterminatedStringLiteral.category(), Error);
    /// ```
    pub fn category(&self) -> Category {
        use Message::*;
        match self {
            UnterminatedStringLiteral
            | IdentifierExpected
            | Expected { .. }
            | FileCannotReferenceItself
            | TrailingCommaNotAllowed
            | AsteriskSlashExpected
            | ElementAccessExpressionShouldTakeArgument
            | UnexpectedToken
            | RestParameterOrBindingPatternMayNotHaveTrailingComma
            | RestParameterMustBeLastInParameterList => Category::Error,
        }
    }

    pub fn code(&self) -> ErrorCode {
        use Message::*;
        match self {
            UnterminatedStringLiteral => 1002,
            IdentifierExpected => 1003,
            Expected { .. } => 1005,
            FileCannotReferenceItself => 1006,
            TrailingCommaNotAllowed => 1009,
            AsteriskSlashExpected => 1010,
            ElementAccessExpressionShouldTakeArgument => 1011,
            UnexpectedToken => 1012,
            RestParameterOrBindingPatternMayNotHaveTrailingComma => 1013,
            RestParameterMustBeLastInParameterList => 1014,
        }
    }
}

impl std::fmt::Display for Message {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Message::*;
        match self {
            UnterminatedStringLiteral => write!(f, "Unterminated string literal."),
            IdentifierExpected => write!(f, "Identifier expected."),
            Expected { item } => write!(f, "'{}' expected.", item),
            FileCannotReferenceItself => write!(f, "A file cannot have a reference to itself."),
            TrailingCommaNotAllowed => write!(f, "Trailing comma not allowed."),
            AsteriskSlashExpected => write!(f, "'*/' expected."),
            ElementAccessExpressionShouldTakeArgument => {
                write!(f, "An element access expression should take an argument.")
            }
            UnexpectedToken => write!(f, "Unexpected token."),
            RestParameterOrBindingPatternMayNotHaveTrailingComma => write!(
                f,
                "A rest parameter or binding pattern may not have a trailing comma."
            ),
            RestParameterMustBeLastInParameterList => {
                write!(f, "A rest parameter must be last in a parameter list.")
            }
        }
    }
}
