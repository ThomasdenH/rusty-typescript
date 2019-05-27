use core::str::FromStr;
use snafu::*;

#[derive(Copy, Clone, Debug, Hash, Eq, PartialEq)]
pub enum Token {
    // Punctuation
    OpenBrace,
    CloseBrace,
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    Dot,
    DotDotDot,
    Semicolon,
    Comma,
    LessThan,
    LessThanSlash,
    GreaterThan,
    LessThanEquals,
    GreaterThanEquals,
    EqualsEquals,
    ExclamationEquals,
    EqualsEqualsEquals,
    ExclamationEqualsEquals,
    EqualsGreaterThan,
    Plus,
    Minus,
    Asterisk,
    AsteriskAsterisk,
    Slash,
    Percent,
    PlusPlus,
    MinusMinus,
    LessThanLessThan,
    GreaterThanGreaterThan,
    GreaterThanGreaterThanGreaterThan,
    Ampersand,
    Bar,
    Caret,
    Exclamation,
    Tilde,
    AmpersandAmpersand,
    BarBar,
    Question,
    Colon,
    At,
    // Assignments
    Equals,
    PlusEquals,
    MinusEquals,
    AsteriskEquals,
    AsteriskAsteriskEquals,
    SlashEquals,
    PercentEquals,
    LessThanLessThanEquals,
    GreaterThanGreaterThanEquals,
    GreaterThanGreaterThanGreaterThanEquals,
    AmpersandEquals,
    BarEquals,
    CaretEquals,
}

#[derive(Eq, PartialEq, Clone, Hash, Snafu, Debug)]
pub enum FromStrError {
    #[snafu(display("not a token: {}", s))]
    NotAToken { s: String },
}

impl FromStr for Token {
    type Err = FromStrError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Token::*;
        match s {
            "{" => Ok(OpenBrace),
            "}" => Ok(CloseBrace),
            "(" => Ok(OpenParen),
            ")" => Ok(CloseParen),
            "[" => Ok(OpenBracket),
            "]" => Ok(CloseBracket),
            "." => Ok(Dot),
            "..." => Ok(DotDotDot),
            ";" => Ok(Semicolon),
            "," => Ok(Comma),
            "<" => Ok(LessThan),
            ">" => Ok(GreaterThan),
            "<=" => Ok(LessThanEquals),
            ">=" => Ok(GreaterThanEquals),
            "==" => Ok(EqualsEquals),
            "!=" => Ok(ExclamationEquals),
            "===" => Ok(EqualsEqualsEquals),
            "!==" => Ok(ExclamationEqualsEquals),
            "=>" => Ok(EqualsGreaterThan),
            "+" => Ok(Plus),
            "-" => Ok(Minus),
            "**" => Ok(AsteriskAsterisk),
            "*" => Ok(Asterisk),
            "/" => Ok(Slash),
            "%" => Ok(Percent),
            "++" => Ok(PlusPlus),
            "--" => Ok(MinusMinus),
            "<<" => Ok(LessThanLessThan),
            "</" => Ok(LessThanSlash),
            ">>" => Ok(GreaterThanGreaterThan),
            ">>>" => Ok(GreaterThanGreaterThanGreaterThan),
            "&" => Ok(Ampersand),
            "|" => Ok(Bar),
            "^" => Ok(Caret),
            "!" => Ok(Exclamation),
            "~" => Ok(Tilde),
            "&&" => Ok(AmpersandAmpersand),
            "||" => Ok(BarBar),
            "?" => Ok(Question),
            ":" => Ok(Colon),
            "=" => Ok(Equals),
            "+=" => Ok(PlusEquals),
            "-=" => Ok(MinusEquals),
            "*=" => Ok(AsteriskEquals),
            "**=" => Ok(AsteriskAsteriskEquals),
            "/=" => Ok(SlashEquals),
            "%=" => Ok(PercentEquals),
            "<<=" => Ok(LessThanLessThanEquals),
            ">>=" => Ok(GreaterThanGreaterThanEquals),
            ">>>=" => Ok(GreaterThanGreaterThanGreaterThan),
            "&=" => Ok(AmpersandEquals),
            "|=" => Ok(BarEquals),
            "^=" => Ok(CaretEquals),
            "@" => Ok(At),
            _ => Err(FromStrError::NotAToken { s: s.to_string() }),
        }
    }
}
