use crate::compiler::types::syntax_kind::{Keyword, SyntaxKind, Token};

#[derive(Copy, Clone, Hash, Eq, Debug, PartialEq)]
pub enum JsDoc {
    EndOfFileToken,
    WhitespaceTrivia,
    AtToken,
    NewLineTrivia,
    AsteriskToken,
    OpenBraceToken,
    CloseBraceToken,
    LessThanToken,
    OpenBracketToken,
    CloseBracketToken,
    EqualsToken,
    CommaToken,
    DotToken,
    Identifier,
    NoSubstitutionTemplateLiteral,
    Unknown,
    Keyword(Keyword),
}

impl From<JsDoc> for SyntaxKind {
    fn from(js_doc: JsDoc) -> SyntaxKind {
        use JsDoc::*;
        match js_doc {
            EndOfFileToken => SyntaxKind::EndOfFileToken,
            WhitespaceTrivia => SyntaxKind::WhitespaceTrivia,
            AtToken => SyntaxKind::Token(Token::At),
            NewLineTrivia => SyntaxKind::NewLineTrivia,
            AsteriskToken => SyntaxKind::Token(Token::Asterisk),
            OpenBraceToken => SyntaxKind::Token(Token::OpenBrace),
            CloseBraceToken => SyntaxKind::Token(Token::CloseBrace),
            LessThanToken => SyntaxKind::Token(Token::LessThan),
            OpenBracketToken => SyntaxKind::Token(Token::OpenBracket),
            CloseBracketToken => SyntaxKind::Token(Token::CloseBracket),
            EqualsToken => SyntaxKind::Token(Token::Equals),
            CommaToken => SyntaxKind::Token(Token::Comma),
            DotToken => SyntaxKind::Token(Token::Dot),
            Identifier => SyntaxKind::Identifier,
            NoSubstitutionTemplateLiteral => SyntaxKind::NoSubstitutionTemplateLiteral,
            Unknown => SyntaxKind::Unknown,
            Keyword(k) => SyntaxKind::Keyword(k),
        }
    }
}
