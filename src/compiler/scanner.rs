use crate::compiler::types::ScriptTarget;
use crate::compiler::types::{
    CharacterCode, LanguageVariant, SyntaxKind, TokenFlags, diagnostics
};
use num_traits::ToPrimitive;
use std::convert::TryFrom;

const ABSTRACT: &str = "abstract";
const ANY: &str = "any";
const AS: &str = "as";
const BIGINT: &str = "bigint";
const BOOLEAN: &str = "boolean";
const BREAK: &str = "break";
const CASE: &str = "case";
const CATCH: &str = "catch";
const CLASS: &str = "class";
const CONTINUE: &str = "continue";
const CONST: &str = "const";
const CONSTRUCTOR: &str = "constructor";
const DEBUGGER: &str = "debugger";
const DECLARE: &str = "declare";
const DEFAULT: &str = "default";
const DELETE: &str = "delete";
const DO: &str = "do";
const ELSE: &str = "else";
const ENUM: &str = "enum";
const EXPORT: &str = "export";
const EXTENDS: &str = "extends";
const FALSE: &str = "false";
const FINALLY: &str = "finally";
const FOR: &str = "for";
const FROM: &str = "from";
const FUNCTION: &str = "function";
const GET: &str = "get";
const IF: &str = "if";
const IMPLEMENTS: &str = "implements";
const IMPORT: &str = "import";
const IN: &str = "in";
const INFER: &str = "infer";
const INSTANCEOF: &str = "instanceof";
const INTERFACE: &str = "interface";
const IS: &str = "is";
const KEYOF: &str = "keyof";
const LET: &str = "let";
const MODULE: &str = "module";
const NAMESPACE: &str = "namespace";
const NEVER: &str = "never";
const NEW: &str = "new";
const NULL: &str = "null";
const NUMBER: &str = "number";
const OBJECT: &str = "object";
const PACKAGE: &str = "package";
const PRIVATE: &str = "private";
const PROTECTED: &str = "protected";
const PUBLIC: &str = "public";
const READONLY: &str = "readonly";
const REQUIRE: &str = "require";
const GLOBAL: &str = "global";
const RETURN: &str = "return";
const SET: &str = "set";
const STATIC: &str = "static";
const STRING: &str = "string";
const SUPER: &str = "super";
const SWITCH: &str = "switch";
const SYMBOL: &str = "symbol";
const THIS: &str = "this";
const THROW: &str = "throw";
const TRUE: &str = "true";
const TRY: &str = "try";
const TYPE: &str = "type";
const TYPEOF: &str = "typeof";
const UNDEFINED: &str = "undefined";
const UNIQUE: &str = "unique";
const UNKNOWN: &str = "unknown";
const VAR: &str = "var";
const VOID: &str = "void";
const WHILE: &str = "while";
const WITH: &str = "with";
const YIELD: &str = "yield";
const ASYNC: &str = "async";
const AWAIT: &str = "await";
const OF: &str = "of";
const OPEN_BRACE_TOKEN: &str = "{";
const CLOSE_BRACE_TOKEN: &str = "}";
const OPEN_PAREN_TOKEN: &str = "(";
const CLOSE_PAREN_TOKEN: &str = ")";
const OPEN_BRACKET_TOKEN: &str = "[";
const CLOSE_BRACKET_TOKEN: &str = "]";
const DOT_TOKEN: &str = ".";
const DOT_DOT_DOT_TOKEN: &str = "...";
const SEMICOLON_TOKEN: &str = ";";
const COMMA_TOKEN: &str = ",";
const LESS_THAN_TOKEN: &str = "<";
const GREATER_THAN_TOKEN: &str = ">";
const LESS_THAN_EQUALS_TOKEN: &str = "<=";
const GREATER_THAN_EQUALS_TOKEN: &str = ">=";
const EQUALS_EQUALS_TOKEN: &str = "==";
const EXCLAMATION_EQUALS_TOKEN: &str = "!=";
const EQUALS_EQUALS_EQUALS_TOKEN: &str = "===";
const EXCLAMATION_EQUALS_EQUALS_TOKEN: &str = "!==";
const EQUALS_GREATER_THAN_TOKEN: &str = "=>";
const PLUS_TOKEN: &str = "+";
const MINUS_TOKEN: &str = "-";
const ASTERISK_ASTERISK_TOKEN: &str = "**";
const ASTERISK_TOKEN: &str = "*";
const SLASH_TOKEN: &str = "/";
const PERCENT_TOKEN: &str = "%";
const PLUS_PLUS_TOKEN: &str = "++";
const MINUS_MINUS_TOKEN: &str = "--";
const LESS_THAN_LESS_THAN_TOKEN: &str = "<<";
const LESS_THAN_SLASH_TOKEN: &str = "</";
const GREATER_THAN_GREATER_THAN_TOKEN: &str = ">>";
const GREATER_THAN_GREATER_THAN_GREATER_THAN_TOKEN: &str = ">>>";
const AMPERSAND_TOKEN: &str = "&";
const BAR_TOKEN: &str = "|";
const CARET_TOKEN: &str = "^";
const EXCLAMATION_TOKEN: &str = "!";
const TILDE_TOKEN: &str = "~";
const AMPERSAND_AMPERSAND_TOKEN: &str = "&&";
const BAR_BAR_TOKEN: &str = "||";
const QUESTION_TOKEN: &str = "?";
const COLON_TOKEN: &str = ":";
const EQUALS_TOKEN: &str = "=";
const PLUS_EQUALS_TOKEN: &str = "+=";
const MINUS_EQUALS_TOKEN: &str = "-=";
const ASTERISK_EQUALS_TOKEN: &str = "*=";
const ASTERISK_ASTERISK_EQUALS_TOKEN: &str = "**=";
const SLASH_EQUALS_TOKEN: &str = "/=";
const PERCENT_EQUALS_TOKEN: &str = "%=";
const LESS_THAN_LESS_THAN_EQUALS_TOKEN: &str = "<<=";
const GREATER_THAN_GREATER_THAN_EQUALS_TOKEN: &str = ">>=";
const GREATER_THAN_GREATER_THAN_GREATER_THAN_EQUALS_TOKEN: &str = ">>>=";
const AMPERSAND_EQUALS_TOKEN: &str = "&=";
const BAR_EQUALS_TOKEN: &str = "|=";
const CARET_EQUALS_TOKEN: &str = "^=";
const AT_TOKEN: &str = "@";

pub fn token_to_string(token: SyntaxKind) -> Option<&'static str> {
    match token {
        SyntaxKind::AbstractKeyword => Some(ABSTRACT),
        SyntaxKind::AnyKeyword => Some(ANY),
        SyntaxKind::AsKeyword => Some(AS),
        SyntaxKind::BigIntKeyword => Some(BIGINT),
        SyntaxKind::BooleanKeyword => Some(BOOLEAN),
        SyntaxKind::BreakKeyword => Some(BREAK),
        SyntaxKind::CaseKeyword => Some(CASE),
        SyntaxKind::CatchKeyword => Some(CATCH),
        SyntaxKind::ClassKeyword => Some(CLASS),
        SyntaxKind::ContinueKeyword => Some(CONTINUE),
        SyntaxKind::ConstKeyword => Some(CONST),
        SyntaxKind::ConstructorKeyword => Some(CONSTRUCTOR),
        SyntaxKind::DebuggerKeyword => Some(DEBUGGER),
        SyntaxKind::DeclareKeyword => Some(DECLARE),
        SyntaxKind::DefaultKeyword => Some(DEFAULT),
        SyntaxKind::DeleteKeyword => Some(DELETE),
        SyntaxKind::DoKeyword => Some(DO),
        SyntaxKind::ElseKeyword => Some(ELSE),
        SyntaxKind::EnumKeyword => Some(ENUM),
        SyntaxKind::ExportKeyword => Some(EXPORT),
        SyntaxKind::ExtendsKeyword => Some(EXTENDS),
        SyntaxKind::FalseKeyword => Some(FALSE),
        SyntaxKind::FinallyKeyword => Some(FINALLY),
        SyntaxKind::ForKeyword => Some(FOR),
        SyntaxKind::FromKeyword => Some(FROM),
        SyntaxKind::FunctionKeyword => Some(FUNCTION),
        SyntaxKind::GetKeyword => Some(GET),
        SyntaxKind::IfKeyword => Some(IF),
        SyntaxKind::ImplementsKeyword => Some(IMPLEMENTS),
        SyntaxKind::ImportKeyword => Some(IMPORT),
        SyntaxKind::InKeyword => Some(IN),
        SyntaxKind::InferKeyword => Some(INFER),
        SyntaxKind::InstanceOfKeyword => Some(INSTANCEOF),
        SyntaxKind::InterfaceKeyword => Some(INTERFACE),
        SyntaxKind::IsKeyword => Some(IS),
        SyntaxKind::KeyOfKeyword => Some(KEYOF),
        SyntaxKind::LetKeyword => Some(LET),
        SyntaxKind::ModuleKeyword => Some(MODULE),
        SyntaxKind::NamespaceKeyword => Some(NAMESPACE),
        SyntaxKind::NeverKeyword => Some(NEVER),
        SyntaxKind::NewKeyword => Some(NEW),
        SyntaxKind::NullKeyword => Some(NULL),
        SyntaxKind::NumberKeyword => Some(NUMBER),
        SyntaxKind::ObjectKeyword => Some(OBJECT),
        SyntaxKind::PackageKeyword => Some(PACKAGE),
        SyntaxKind::PrivateKeyword => Some(PRIVATE),
        SyntaxKind::ProtectedKeyword => Some(PROTECTED),
        SyntaxKind::PublicKeyword => Some(PUBLIC),
        SyntaxKind::ReadonlyKeyword => Some(READONLY),
        SyntaxKind::RequireKeyword => Some(REQUIRE),
        SyntaxKind::GlobalKeyword => Some(GLOBAL),
        SyntaxKind::ReturnKeyword => Some(RETURN),
        SyntaxKind::SetKeyword => Some(SET),
        SyntaxKind::StaticKeyword => Some(STATIC),
        SyntaxKind::StringKeyword => Some(STRING),
        SyntaxKind::SuperKeyword => Some(SUPER),
        SyntaxKind::SwitchKeyword => Some(SWITCH),
        SyntaxKind::SymbolKeyword => Some(SYMBOL),
        SyntaxKind::ThisKeyword => Some(THIS),
        SyntaxKind::ThrowKeyword => Some(THROW),
        SyntaxKind::TrueKeyword => Some(TRUE),
        SyntaxKind::TryKeyword => Some(TRY),
        SyntaxKind::TypeKeyword => Some(TYPE),
        SyntaxKind::TypeOfKeyword => Some(TYPEOF),
        SyntaxKind::UndefinedKeyword => Some(UNDEFINED),
        SyntaxKind::UniqueKeyword => Some(UNIQUE),
        SyntaxKind::UnknownKeyword => Some(UNKNOWN),
        SyntaxKind::VarKeyword => Some(VAR),
        SyntaxKind::VoidKeyword => Some(VOID),
        SyntaxKind::WhileKeyword => Some(WHILE),
        SyntaxKind::WithKeyword => Some(WITH),
        SyntaxKind::YieldKeyword => Some(YIELD),
        SyntaxKind::AsyncKeyword => Some(ASYNC),
        SyntaxKind::AwaitKeyword => Some(AWAIT),
        SyntaxKind::OfKeyword => Some(OF),
        SyntaxKind::OpenBraceToken => Some(OPEN_BRACE_TOKEN),
        SyntaxKind::CloseBraceToken => Some(CLOSE_BRACE_TOKEN),
        SyntaxKind::OpenParenToken => Some(OPEN_PAREN_TOKEN),
        SyntaxKind::CloseParenToken => Some(CLOSE_PAREN_TOKEN),
        SyntaxKind::OpenBracketToken => Some(OPEN_BRACKET_TOKEN),
        SyntaxKind::CloseBracketToken => Some(CLOSE_BRACKET_TOKEN),
        SyntaxKind::DotToken => Some(DOT_TOKEN),
        SyntaxKind::DotDotDotToken => Some(DOT_DOT_DOT_TOKEN),
        SyntaxKind::SemicolonToken => Some(SEMICOLON_TOKEN),
        SyntaxKind::CommaToken => Some(COMMA_TOKEN),
        SyntaxKind::LessThanToken => Some(LESS_THAN_TOKEN),
        SyntaxKind::GreaterThanToken => Some(GREATER_THAN_TOKEN),
        SyntaxKind::LessThanEqualsToken => Some(LESS_THAN_EQUALS_TOKEN),
        SyntaxKind::GreaterThanEqualsToken => Some(GREATER_THAN_EQUALS_TOKEN),
        SyntaxKind::EqualsEqualsToken => Some(EQUALS_EQUALS_TOKEN),
        SyntaxKind::ExclamationEqualsToken => Some(EXCLAMATION_EQUALS_TOKEN),
        SyntaxKind::EqualsEqualsEqualsToken => Some(EQUALS_EQUALS_EQUALS_TOKEN),
        SyntaxKind::ExclamationEqualsEqualsToken => Some(EXCLAMATION_EQUALS_EQUALS_TOKEN),
        SyntaxKind::EqualsGreaterThanToken => Some(EQUALS_GREATER_THAN_TOKEN),
        SyntaxKind::PlusToken => Some(PLUS_TOKEN),
        SyntaxKind::MinusToken => Some(MINUS_TOKEN),
        SyntaxKind::AsteriskAsteriskToken => Some(ASTERISK_ASTERISK_TOKEN),
        SyntaxKind::AsteriskToken => Some(ASTERISK_TOKEN),
        SyntaxKind::SlashToken => Some(SLASH_TOKEN),
        SyntaxKind::PercentToken => Some(PERCENT_TOKEN),
        SyntaxKind::PlusPlusToken => Some(PLUS_PLUS_TOKEN),
        SyntaxKind::MinusMinusToken => Some(MINUS_MINUS_TOKEN),
        SyntaxKind::LessThanLessThanToken => Some(LESS_THAN_LESS_THAN_TOKEN),
        SyntaxKind::LessThanSlashToken => Some(LESS_THAN_SLASH_TOKEN),
        SyntaxKind::GreaterThanGreaterThanToken => Some(GREATER_THAN_GREATER_THAN_TOKEN),
        SyntaxKind::GreaterThanGreaterThanGreaterThanToken => {
            Some(GREATER_THAN_GREATER_THAN_GREATER_THAN_TOKEN)
        }
        SyntaxKind::AmpersandToken => Some(AMPERSAND_TOKEN),
        SyntaxKind::BarToken => Some(BAR_TOKEN),
        SyntaxKind::CaretToken => Some(CARET_TOKEN),
        SyntaxKind::ExclamationToken => Some(EXCLAMATION_TOKEN),
        SyntaxKind::TildeToken => Some(TILDE_TOKEN),
        SyntaxKind::AmpersandAmpersandToken => Some(AMPERSAND_AMPERSAND_TOKEN),
        SyntaxKind::BarBarToken => Some(BAR_BAR_TOKEN),
        SyntaxKind::QuestionToken => Some(QUESTION_TOKEN),
        SyntaxKind::ColonToken => Some(COLON_TOKEN),
        SyntaxKind::EqualsToken => Some(EQUALS_TOKEN),
        SyntaxKind::PlusEqualsToken => Some(PLUS_EQUALS_TOKEN),
        SyntaxKind::MinusEqualsToken => Some(MINUS_EQUALS_TOKEN),
        SyntaxKind::AsteriskEqualsToken => Some(ASTERISK_EQUALS_TOKEN),
        SyntaxKind::AsteriskAsteriskEqualsToken => Some(ASTERISK_ASTERISK_EQUALS_TOKEN),
        SyntaxKind::SlashEqualsToken => Some(SLASH_EQUALS_TOKEN),
        SyntaxKind::PercentEqualsToken => Some(PERCENT_EQUALS_TOKEN),
        SyntaxKind::LessThanLessThanEqualsToken => Some(LESS_THAN_LESS_THAN_EQUALS_TOKEN),
        SyntaxKind::GreaterThanGreaterThanEqualsToken => {
            Some(GREATER_THAN_GREATER_THAN_EQUALS_TOKEN)
        }
        SyntaxKind::GreaterThanGreaterThanGreaterThanEqualsToken => {
            Some(GREATER_THAN_GREATER_THAN_GREATER_THAN_EQUALS_TOKEN)
        }
        SyntaxKind::AmpersandEqualsToken => Some(AMPERSAND_EQUALS_TOKEN),
        SyntaxKind::BarEqualsToken => Some(BAR_EQUALS_TOKEN),
        SyntaxKind::CaretEqualsToken => Some(CARET_EQUALS_TOKEN),
        SyntaxKind::AtToken => Some(AT_TOKEN),
        _ => None,
    }
}

pub fn is_white_space_single_line(charcode: CharacterCode) -> bool {
    use CharacterCode::*;
    charcode == Space
        || charcode == Tab
        || charcode == VerticalTab
        || charcode == FormFeed
        || charcode == NonBreakingSpace
        || charcode == NextLine
        || charcode == Ogham
        || charcode >= EnQuad && charcode <= ZeroWidthSpace
        || charcode == NarrowNoBreakSpace
        || charcode == MathematicalSpace
        || charcode == IdeographicSpace
        || charcode == IdeographicSpace
        || charcode == ByteOrderMark
}

/// ES5 7.3:
/// The ECMAScript line terminator characters are listed in Table 3.
///     Table 3: Line Terminator Characters
///     Code Unit Value     Name                    Formal Name
///     \u000A              Line Feed               <LF>
///     \u000D              Carriage Return         <CR>
///     \u2028              Line separator          <LS>
///     \u2029              Paragraph separator     <PS>
/// Only the characters in Table 3 are treated as line terminators. Other new line or line
/// breaking characters are treated as white space but not as line terminators.
pub fn is_line_break(charcode: CharacterCode) -> bool {
    use CharacterCode::*;
    charcode == LineFeed
        || charcode == CarriageReturn
        || charcode == LineSeparator
        || charcode == ParagraphSeparator
}

pub fn is_white_space_like(charcode: CharacterCode) -> bool {
    is_white_space_single_line(charcode) || is_line_break(charcode)
}

pub fn could_start_trivia(text: &str, pos: usize) -> bool {
    use CharacterCode::*;
    text.chars()
        .nth(pos)
        .map(|ch| {
            CharacterCode::try_from(ch)
                .map(|charcode: CharacterCode| match charcode {
                    CarriageReturn |
                    LineFeed |
                    Tab |
                    VerticalTab |
                    FormFeed |
                    Space |
                    Slash |
                        // starts of normal trivia
                    LessThan |
                    Bar |
                    Equals |
                    GreaterThan =>
                        // Starts of conflict marker trivia
                        true,
                    Hash =>
                        // Only if its the beginning can we have #! trivia
                        pos == 0,
                    _ => ch as u32 > MaxAsciiCharacter as u32,
                })
                .unwrap_or_default()
        })
        .unwrap_or_default()
}

pub struct Scanner {
    text: String,
    /// Current position (end position of text of current token)
    pos: usize,
    /// end of text
    end: usize,
    /// Start position of whitespace before current token
    start_pos: usize,
    // Start position of text of current token
    token_pos: usize,
    token: SyntaxKind,
    token_value: Option<String>,
    token_flags: TokenFlags,
    in_jsdoc_type: usize,
    on_error: Option<Box<ErrorCallback>>,
}

type ErrorCallback = (Fn(diagnostics::Message, usize));

impl Scanner {
    pub fn new(
        language_version: ScriptTarget,
        skip_trivia: bool,
        language_variant: LanguageVariant,
        text_initial: Option<String>,
        on_error: Option<Box<ErrorCallback>>,
        start: Option<usize>,
        length: Option<usize>,
    ) -> Scanner {
        let text = text_initial.unwrap_or_else(|| "".to_string());
        let start = start.unwrap_or(0);
        let end = length.map(|l| l + start).unwrap_or(text.len());

        let pos = start;
        let start_pos = start;
        let token_pos = start;
        let token = SyntaxKind::Unknown;
        let token_value = None;
        let token_flags = TokenFlags::NONE;

        Scanner {
            text,
            pos,
            end,
            start_pos,
            token_pos,
            token,
            token_value,
            token_flags,
            in_jsdoc_type: 0,
            on_error,
        }
    }

    fn error (&mut self, message: diagnostics::Message, err_pos: Option<usize>, length: Option<usize>) {
        if let Some(on_error) = self.on_error {
            let old_pos = self.pos;
            self.pos = err_pos.unwrap_or(self.pos);
            on_error(message, length.unwrap_or(0));
            self.pos = old_pos;
        }
    }

    pub fn start_pos(&self) -> usize {
        self.start_pos
    }

    pub fn text_pos(&self) -> usize {
        self.pos
    }

    pub fn token(&self) -> SyntaxKind {
        self.token
    }

    pub fn token_pos(&self) -> usize {
        self.token_pos
    }

    pub fn token_text(&self) -> Option<&str> {
        self.text.get(self.token_pos..self.pos)
    }

    pub fn get_token_value(&self) -> Option<&str> {
        match &self.token_value {
            Some(s) => Some(&s),
            None => None,
        }
    }

    pub fn has_extended_unicode_escape(&self) -> bool {
        self.token_flags
            .contains(TokenFlags::EXTENDED_UNICODE_ESCAPE)
    }

    pub fn has_preceding_line_break(&self) -> bool {
        self.token_flags.contains(TokenFlags::PRECEDING_LINE_BREAK)
    }

    pub fn is_identifier(&self) -> bool {
        self.token == SyntaxKind::Identifier
            // token > LastReservedWord
            || !self.token.to_u32().unwrap() > SyntaxKind::WithKeyword.to_u32().unwrap()
    }

    pub fn is_reserved_word(&self) -> bool {
        self.token.is_reserved_word()
    }

    pub fn is_unterminated(&self) -> bool {
        self.token_flags.contains(TokenFlags::UNTERMINATED)
    }

    pub fn token_flags(&self) -> TokenFlags {
        self.token_flags
    }

    fn scan_number_fragment(&mut self) -> String {
        let mut start = self.pos;
        let mut allow_separator = false;
        let mut is_previous_token_separator = false;
        let mut result = String::new();
        loop {
            let ch = self.text.chars().nth(self.pos);
            if ch == Some('_') {
                self.token_flags |= TokenFlags::CONTAINS_SEPARATOR;
                if allow_separator {
                    allow_separator = false;
                    is_previous_token_separator = true;
                    result += self.text.get(start..self.pos).unwrap();
                } else if is_previous_token_separator {
                    self.error(
                        diagnostics::Message::MultipleConsecutiveNumericSeparatorsNotPermitted,
                        Some(self.pos),
                        Some(1)
                    );
                } else {
                    self.error(
                        diagnostics::Message::NumericSeparatorsAreNotAllowedHere,
                        Some(self.pos),
                        Some(1)
                    );
                }
                self.pos += 1;
                start = self.pos;
                continue;
            }
            if ch.map(|ch| ch.is_digit(10)).unwrap_or(false) {
                allow_separator = true;
                is_previous_token_separator = false;
                self.pos += 1;
                continue;
            }
            break;
        }
        if Some('_') == self.text.chars().nth(self.pos - 1) {
            self.error(
                diagnostics::Message::NumericSeparatorsAreNotAllowedHere,
                Some(self.pos - 1),
                Some(1)
            );
        }
        result + self.text.get(start..self.pos).unwrap()
    }

    fn scan_number(&mut self) -> (SyntaxKind, String) {
        let start = self.pos;
        let main_fragment = self.scan_number_fragment();
        let decimal_fragment: Option<String>;
        let scientific_fragment: Option<String>;
        if self.text.chars().nth(self.pos) == Some('.') {
            self.pos += 1;
            decimal_fragment = self.scan_number_fragment();
        }
        let end = self.pos;
        if self.text.chars().nth(self.pos) == Some('E') || self.text.chars().nth(self.pos) == Some('e') {
            self.pos +=1;
            self.token_flags.insert(TokenFlags::SCIENTIFIC);
            if self.text.chars().nth(self.pos) == Some('+') || self.text.chars().nth(self.pos) == Some('-') {
                self.pos += 1;
            }
            let pre_numeric_part = self.pos;
            let final_fragment = self.scan_number_fragment();
            if final_fragment.len() == 0 {
                error(
                    diagnostics::Message::DigitExpected,
                    None,
                    None
                )
            } else {
                scientific_fragment = Some(self.text.get(end..pre_numeric_part).unwrap().to_string() + final_fragment);
                end = pos;
            }
        }
        let result: String;
        if self.token_flags.contains(TokenFlags::CONTAINS_SEPARATOR) {
            result = main_fragment;
            if let Some(decimal_fragment) = decimal_fragment {
                result += "." + decimal_fragment;
            }
            if let Some(scientific_fragment) = scientific_fragment {
                result += scientific_fragment;
            }
        } else {
            result = self.text.get(start..end);
        }

        if decimal_fragment.is_some() || self.token_flags.contains(TokenFlags::SCIENTIFIC) {
            check_for_identifier_start_after_numeric_literal(start, );
            (
                SyntaxKind::NumericLiteral,
                value: 
            )
        } else {
            self.token_value = result;
            let type = self.check_big_int_suffix();
            self.check_for_identifier_start_after_numeric_literal(start);
            (

            )
        }
    }

    fn scan_octal_digits(&mut self) -> usize {
        let start = self.pos;
        loop {
            if self.text.chars().nth(self.pos).map(|c| c.is_digit(8)).unwrap_or(false) {
                self.pos += 1;
            } else {
                break;
            }
        }
        self.text.get(start..self.pos).unwrap().parse().unwrap()
    }
}

#[cfg(feature = "wasm")]
mod wasm {
    #[wasm_bindgen(js_name = "tokenToString")]
    pub fn token_to_string(t: u32) -> Option<String> {
        FromPrimitive::from_u32(t)
            .map(super::token_to_string)
            .unwrap_or_default()
            .map(String::from)
    }

    /* Does not include line breaks. For that, see isWhiteSpaceLike(). */
    #[wasm_bindgen(js_name = "isWhiteSpaceSingleLine")]
    pub fn is_white_space_single_line(ch: u32) -> bool {
        // Note: NextLine is in the Zs space, and should be considered to be a whitespace.
        // It is explicitly not a line-break as it isn't in the exact set specified by EcmaScript.

        FromPrimitive::from_u32(ch)
            .map(super::is_white_space_single_line)
            .unwrap_or_default() // the default of bool is false
    }

    #[wasm_bindgen(js_name = "couldStartTrivia")]
    pub fn could_start_trivia(text: &str, pos: usize) -> bool {
        super::could_start_trivia(text, pos)
    }
}
