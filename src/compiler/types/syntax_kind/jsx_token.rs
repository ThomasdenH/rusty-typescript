use crate::compiler::types::syntax_kind::{SyntaxKind, Token};

#[derive(Debug, Hash, Eq, PartialEq, Copy, Clone)]
pub enum JsxToken {
    LessThanSlashToken,
    EndOfFileToken,
    ConflictMarkerTrivia,
    JsxText,
    JsxTextAllWhiteSpaces,
    OpenBraceToken,
    LessThanToken,
}

impl From<JsxToken> for SyntaxKind {
    fn from(t: JsxToken) -> SyntaxKind {
        use JsxToken::*;
        match t {
            LessThanSlashToken => SyntaxKind::Token(Token::LessThanSlash),
            EndOfFileToken => SyntaxKind::EndOfFileToken,
            ConflictMarkerTrivia => SyntaxKind::ConflictMarkerTrivia,
            JsxText => SyntaxKind::JsxText,
            JsxTextAllWhiteSpaces => SyntaxKind::JsxTextAllWhiteSpaces,
            OpenBraceToken => SyntaxKind::Token(Token::OpenBrace),
            LessThanToken => SyntaxKind::Token(Token::LessThan),
        }
    }
}
