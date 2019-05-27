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
    Expected {
        item: String,
    },
    FileCannotReferenceItself,
    TrailingCommaNotAllowed,
    AsteriskSlashExpected,
    ElementAccessExpressionShouldTakeArgument,
    UnexpectedToken,
    RestParameterOrBindingPatternMayNotHaveTrailingComma,
    RestParameterMustBeLastInParameterList,
    ParaneterCannotHaveQuestionMarkAndInitializer,
    RequiredParameterCannotFollowOptionalParameter,
    IndexSignatureCannotHaveRestParameter,
    IndexSignatureParameterCannotHaveAccessibilityModifier,
    IndexSignatureParameterCannotHaveQuestionMark,
    IndexSignatureParameterCannotHaveInitializer,
    IndexSignatureMustHaveTypeAnnotation,
    IndexSignatureParameterMustHaveTypeAnnotation,
    IndexSignatureMustBeStringOrNumber,
    ReadonlyCanOnlyAppearOnPropertyDeclarationOrIndexSignature,
    AccessibilityModifierAlreadySeen,
    ModifierMustPrecedeModifier {
        modifier0: String,
        modifier1: String,
    },
    ModifierAlreadySeen {
        modifier: String,
    },
    ModifierCannotAppearOnClassElement {
        modifier: String,
    },
    SuperMustBeFollowedByAnArgumentListOrMemberAccess,
    OnlyAmbientModulesCanUseQuotedNames,
    StatementsAreNotAllowedInAmbientContexts,
    DeclareModifierCannotBeUsedInAmbientContext,
    InitializersNotAllowedInAmbientContexts,
    ModifierCannotBeUsedInAmbientContext {
        modifier: String,
    },
    ModifierCannotBeUsedWithClassDeclaration {
        modifier: String,
    },
    ModifierCannotBeUsedHere {
        modifier: String,
    },
    ModifierCannotAppearOnDataProperty {
        modifier: String,
    },
    ModifierCannotAppearOnModuleOrNamespaceElement {
        modifier: String,
    },
    DigitExpected,
    UnexpectedEndOfText,
    UnterminatedTemplateLiteral,
    UnterminatedRegularExpressionLiteral,
    NumericSeparatorsAreNotAllowedHere,
    MultipleConsecutiveNumericSeparatorsNotPermitted,
    MergeConflictMarkerEncountered,
    HexadecimalDigitExpected,
    AnExtendedUnicodeEscapeValueMustBeBetween,
    UnterminatedUnicodeEscapeSequence,
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
            | RestParameterMustBeLastInParameterList
            | ParaneterCannotHaveQuestionMarkAndInitializer
            | RequiredParameterCannotFollowOptionalParameter
            | IndexSignatureCannotHaveRestParameter
            | IndexSignatureParameterCannotHaveAccessibilityModifier
            | IndexSignatureParameterCannotHaveQuestionMark
            | IndexSignatureParameterCannotHaveInitializer
            | IndexSignatureMustHaveTypeAnnotation
            | IndexSignatureParameterMustHaveTypeAnnotation
            | IndexSignatureMustBeStringOrNumber
            | ReadonlyCanOnlyAppearOnPropertyDeclarationOrIndexSignature
            | AccessibilityModifierAlreadySeen
            | ModifierMustPrecedeModifier { .. }
            | ModifierAlreadySeen { .. }
            | ModifierCannotAppearOnClassElement { .. }
            | SuperMustBeFollowedByAnArgumentListOrMemberAccess
            | OnlyAmbientModulesCanUseQuotedNames
            | StatementsAreNotAllowedInAmbientContexts
            | DeclareModifierCannotBeUsedInAmbientContext
            | InitializersNotAllowedInAmbientContexts
            | ModifierCannotBeUsedInAmbientContext { .. }
            | ModifierCannotBeUsedWithClassDeclaration { .. }
            | ModifierCannotBeUsedHere { .. }
            | ModifierCannotAppearOnDataProperty { .. }
            | ModifierCannotAppearOnModuleOrNamespaceElement { .. }
            | UnterminatedTemplateLiteral
            | DigitExpected
            | UnexpectedEndOfText
            | UnterminatedRegularExpressionLiteral
            | NumericSeparatorsAreNotAllowedHere
            | MultipleConsecutiveNumericSeparatorsNotPermitted => Category::Error,
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
            ParaneterCannotHaveQuestionMarkAndInitializer => 1015,
            RequiredParameterCannotFollowOptionalParameter => 1016,
            IndexSignatureCannotHaveRestParameter => 1017,
            IndexSignatureParameterCannotHaveAccessibilityModifier => 1018,
            IndexSignatureParameterCannotHaveQuestionMark => 1019,
            IndexSignatureParameterCannotHaveInitializer => 1020,
            IndexSignatureMustHaveTypeAnnotation => 1021,
            IndexSignatureParameterMustHaveTypeAnnotation => 1022,
            IndexSignatureMustBeStringOrNumber => 1023,
            ReadonlyCanOnlyAppearOnPropertyDeclarationOrIndexSignature => 1024,
            AccessibilityModifierAlreadySeen => 1028,
            ModifierMustPrecedeModifier { .. } => 1029,
            ModifierAlreadySeen { .. } => 1030,
            ModifierCannotAppearOnClassElement { .. } => 1031,
            SuperMustBeFollowedByAnArgumentListOrMemberAccess => 1034,
            OnlyAmbientModulesCanUseQuotedNames => 1035,
            StatementsAreNotAllowedInAmbientContexts => 1036,
            DeclareModifierCannotBeUsedInAmbientContext => 1038,
            InitializersNotAllowedInAmbientContexts => 1039,
            ModifierCannotBeUsedInAmbientContext { .. } => 1040,
            ModifierCannotBeUsedWithClassDeclaration { .. } => 1041,
            ModifierCannotBeUsedHere { .. } => 1042,
            ModifierCannotAppearOnDataProperty { .. } => 1043,
            ModifierCannotAppearOnModuleOrNamespaceElement { .. } => 1044,
            DigitExpected => 1124,
            UnexpectedEndOfText => 1126,
            UnterminatedTemplateLiteral => 1160,
            UnterminatedRegularExpressionLiteral => 1161,
            NumericSeparatorsAreNotAllowedHere => 6188,
            MultipleConsecutiveNumericSeparatorsNotPermitted => 6189,
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
            ParaneterCannotHaveQuestionMarkAndInitializer => {
                write!(f, "Parameter cannot have question mark and initializer")
            }
            RequiredParameterCannotFollowOptionalParameter => write!(
                f,
                "A required parameter cannot follow an optional parameter."
            ),
            IndexSignatureCannotHaveRestParameter => {
                write!(f, "An index signature cannot have a rest parameter.")
            }
            IndexSignatureParameterCannotHaveAccessibilityModifier => write!(
                f,
                "An index signature parameter cannot have an accessibility modifier."
            ),
            IndexSignatureParameterCannotHaveQuestionMark => write!(
                f,
                "An index signature parameter cannot have a question mark."
            ),
            IndexSignatureParameterCannotHaveInitializer => write!(
                f,
                "An index signature parameter cannot have an initializer."
            ),
            IndexSignatureMustHaveTypeAnnotation => {
                write!(f, "An index signature must have a type annotation.")
            }
            IndexSignatureParameterMustHaveTypeAnnotation => write!(
                f,
                "An index signature parameter must have a type annotation."
            ),
            IndexSignatureMustBeStringOrNumber => write!(
                f,
                "An index signature parameter type must be 'string' or 'number'."
            ),
            ReadonlyCanOnlyAppearOnPropertyDeclarationOrIndexSignature => write!(
                f,
                "'readonly' modifier can only appear on a property declaration or index signature."
            ),
            AccessibilityModifierAlreadySeen => write!(f, "Accessibility modifier already seen."),
            ModifierMustPrecedeModifier {
                modifier0,
                modifier1,
            } => write!(
                f,
                "'{0}' modifier must precede '{1}' modifier.",
                modifier0, modifier1
            ),
            ModifierAlreadySeen { modifier } => write!(f, "'{0}' modifier already seen.", modifier),
            ModifierCannotAppearOnClassElement { modifier } => write!(
                f,
                "'{0}' modifier cannot appear on a class element.",
                modifier
            ),
            SuperMustBeFollowedByAnArgumentListOrMemberAccess => write!(
                f,
                "'super' must be followed by an argument list or member access."
            ),
            OnlyAmbientModulesCanUseQuotedNames => {
                write!(f, "Only ambient modules can use quoted names.")
            }
            StatementsAreNotAllowedInAmbientContexts => {
                write!(f, "Statements are not allowed in ambient contexts.")
            }
            DeclareModifierCannotBeUsedInAmbientContext => write!(
                f,
                "A 'declare' modifier cannot be used in an already ambient context."
            ),
            InitializersNotAllowedInAmbientContexts => {
                write!(f, "Initializers are not allowed in ambient contexts.")
            }
            ModifierCannotBeUsedInAmbientContext { modifier } => write!(
                f,
                "'{0}' modifier cannot be used in an ambient context.",
                modifier
            ),
            ModifierCannotBeUsedWithClassDeclaration { modifier } => write!(
                f,
                "'{0}' modifier cannot be used with a class declaration.",
                modifier
            ),
            ModifierCannotBeUsedHere { modifier } => {
                write!(f, "'{0}' modifier cannot be used here.", modifier)
            }
            ModifierCannotAppearOnDataProperty { modifier } => write!(
                f,
                "'{0}' modifier cannot appear on a data property.",
                modifier
            ),
            ModifierCannotAppearOnModuleOrNamespaceElement { modifier } => write!(
                f,
                "'{0}' modifier cannot appear on a module or namespace element.",
                modifier
            ),
            DigitExpected => write!(f, "Digit expected."),
            UnexpectedEndOfText => write!(f, "Unexpected end of text."),
            UnterminatedTemplateLiteral => write!(f, "Unterminated template literal."),
            UnterminatedRegularExpressionLiteral => {
                write!(f, "Unterminated regular expression literal.")
            }
            NumericSeparatorsAreNotAllowedHere => {
                write!(f, "Numeric separators are not allowed here.")
            }
            MultipleConsecutiveNumericSeparatorsNotPermitted => write!(
                f,
                "Multiple consecutive numeric separators are not permitted."
            ),
        }
    }
}
