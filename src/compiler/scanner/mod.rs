use crate::compiler::types::pseudobigint::parse_pseudo_big_int;
use crate::compiler::types::ScriptTarget;
use crate::compiler::types::{
    character_codes, diagnostic, syntax_kind, syntax_kind::Token, LanguageVariant, SyntaxKind,
    TokenFlags,
};
use lazy_static::*;
use regex::Regex;
use std::convert::TryFrom;
use std::str::FromStr;

#[cfg(test)]
mod test;

lazy_static! {
    static ref SHEBANG_TRIVIA_REGEX: Regex =
        Regex::new("^#!.*").expect("Cannot compile shebang regex");
}

lazy_static! {
    static ref MERGE_CONFLICT_MARKER_LENGTH: usize = "<<<<<<<".len();
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Hash)]
enum BinaryOrOctal {
    Binary,
    Octal,
}

impl BinaryOrOctal {
    fn base(self) -> u32 {
        match self {
            BinaryOrOctal::Binary => 2,
            BinaryOrOctal::Octal => 8,
        }
    }
}

fn is_identifier_start(c: char, language_version: ScriptTarget) -> bool {
    c.is_ascii_alphabetic()
        || c == '$'
        || c == '_'
        || !c.is_ascii() && is_unicode_identifier_start(c, language_version)
}

fn is_identifier_part(ch: char, language_version: ScriptTarget) -> bool {
    ch.is_digit(10) || is_identifier_start(ch, language_version)
}

fn is_unicode_identifier_start(code: char, language_version: ScriptTarget) -> bool {
    if language_version >= ScriptTarget::ES5 {
        UNICODE_ES5_IDENTIFIER_START.lookup(code)
    } else {
        UNICODE_ES3_IDENTIFIER_START.lookup(code)
    }
}

fn is_unicode_identifier_part(code: char, language_version: ScriptTarget) -> bool {
    if language_version >= ScriptTarget::ES5 {
        UNICODE_ES5_IDENTIFIER_PART.lookup(code)
    } else {
        UNICODE_ES3_IDENTIFIER_PART.lookup(code)
    }
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
fn is_line_break(c: char) -> bool {
    use character_codes::*;
    match c {
        LINE_FEED | CARRIAGE_RETURN | LINE_SEPARATOR | PARAGRAPH_SEPARATOR => true,
        _ => false,
    }
}

pub fn is_white_space_single_line(c: char) -> bool {
    use character_codes::*;
    match c {
        SPACE
        | TAB
        | VERTICAL_TAB
        | FORM_FEED
        | NON_BREAKING_SPACE
        | NEXT_LINE
        | OGHAM
        | EN_QUAD...ZERO_WIDTH_SPACE
        | NARROW_NO_BREAK_SPACE
        | MATHEMATICAL_SPACE
        | IDEOGRAPHIC_SPACE
        | BYTE_ORDER_MARK => true,
        _ => false,
    }
}

pub fn is_white_space_like(c: char) -> bool {
    is_white_space_single_line(c) || is_line_break(c)
}

pub fn could_start_trivia(text: &str, pos: usize) -> bool {
    use character_codes::*;
    text.chars()
        .nth(pos)
        .map(|ch| match ch {
            CARRIAGE_RETURN |
            LINE_FEED |
            TAB |
            VERTICAL_TAB |
            FORM_FEED |
            ' ' |
            '/' |
                // starts of normal trivia
            '<' |
            '|' |
            '>' |
            '=' =>
                // Starts of conflict marker trivia
                true,
            '#' =>
                // Only if its the beginning can we have #! trivia
                pos == 0,
            _ => !ch.is_ascii(),
        })
        .unwrap_or(false)
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
    language_version: ScriptTarget,
    language_variant: LanguageVariant,
    skip_trivia: bool,
}

pub type ErrorCallback = (Fn(diagnostic::Message, usize));

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
        let end = length.map(|l| l + start).unwrap_or_else(|| text.len());

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
            language_version,
            language_variant,
            skip_trivia,
        }
    }

    pub fn set_on_error(&mut self, on_error: Option<Box<ErrorCallback>>) {
        self.on_error = on_error;
    }

    fn error(
        &mut self,
        message: diagnostic::Message,
        err_pos: Option<usize>,
        length: Option<usize>,
    ) {
        if let Some(on_error) = &self.on_error {
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

    pub fn token_value(&self) -> Option<&str> {
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
        use syntax_kind::Keyword::*;
        use SyntaxKind::*;
        match self.token {
            Identifier => true,
            Keyword(k) => match k {
                Implements|
                Interface|
                Let|
                Package|
                Private|
                Protected|
                Public|
                Static|
                Yield|
                // Contextual keywords
                Abstract|
                As|
                Any|
                Async|
                Await|
                Boolean|
                syntax_kind::Keyword::Constructor|
                Declare|
                Get|
                Infer|
                Is|
                KeyOf|
                Module|
                Namespace|
                Never|
                Readonly|
                Require|
                Number|
                Object|
                Set|
                String|
                Symbol|
                Type|
                Undefined|
                Unique|
                syntax_kind::Keyword::Unknown|
                From|
                Global|
                BigInt|
                Of => true,
                _ => false
            },
            _ => false,
        }
    }

    pub fn is_reserved_word(&self) -> bool {
        self.token.is_reserved_word()
    }

    pub fn is_unterminated(&self) -> bool {
        self.token_flags.contains(TokenFlags::UNTERMINATED)
    }

    pub(crate) fn token_flags(&self) -> TokenFlags {
        self.token_flags
    }

    pub fn re_scan_greater_token(&mut self) -> SyntaxKind {
        use syntax_kind::Token::*;
        if self.token == SyntaxKind::Token(GreaterThan) {
            if self.text.chars().nth(self.pos) == Some('>') {
                if self.text.chars().nth(self.pos + 1) == Some('>') {
                    if self.text.chars().nth(self.pos + 2) == Some('=') {
                        self.pos += 3;
                        self.token = SyntaxKind::Token(GreaterThanGreaterThanGreaterThanEquals);
                    } else {
                        self.pos += 2;
                        self.token = SyntaxKind::Token(GreaterThanGreaterThanGreaterThan);
                    }
                } else if self.text.chars().nth(self.pos + 1) == Some('=') {
                    self.pos += 2;
                    self.token = SyntaxKind::Token(GreaterThanGreaterThanEquals);
                } else {
                    self.pos += 1;
                    self.token = SyntaxKind::Token(GreaterThanGreaterThan);
                }
            } else if self.text.chars().nth(self.pos) == Some('=') {
                self.pos += 1;
                self.token = SyntaxKind::Token(GreaterThanEquals);
            }
        }
        self.token
    }

    /// Scans a JSX identifier; these differ from normal identifiers in that
    /// they allow dashes
    pub fn scan_jsx_identifier(&mut self) -> SyntaxKind {
        if self.token.is_identifier_or_keyword() {
            let first_char_position = self.pos;
            while self.pos < self.end {
                let ch = self.text.chars().nth(self.pos);
                if ch == Some('-')
                    || ch
                        .map(|ch| {
                            if first_char_position == self.pos {
                                is_identifier_start(ch, self.language_version)
                            } else {
                                is_identifier_part(ch, self.language_version)
                            }
                        })
                        .unwrap_or(false)
                {
                    self.pos += 1;
                } else {
                    break;
                }
            }

            self.token_value
                .get_or_insert_with(String::new)
                .push_str(&self.text[first_char_position..self.pos]);
        }
        self.token
    }

    pub fn re_scan_less_than_token(&mut self) -> SyntaxKind {
        if self.token == SyntaxKind::Token(Token::LessThanLessThan) {
            self.pos = self.token_pos + 1;
            self.token = SyntaxKind::Token(Token::LessThan);
        }
        self.token
    }

    pub fn scan_jsx_attribute_value(&mut self) -> SyntaxKind {
        self.start_pos = self.pos;

        match self.text.chars().nth(self.pos) {
            Some('"') | Some('\'') => {
                self.token_value = Some(self.scan_string(true));
                self.token = SyntaxKind::StringLiteral;
                self.token
            }
            _ => self.scan(),
        }
    }

    pub fn re_scan_jsx_token(&mut self) -> syntax_kind::JsxToken {
        self.token_pos = self.start_pos;
        self.pos = self.token_pos;
        let token = self.scan_jsx_token();
        self.token = token.into();
        token
    }

    pub fn scan_jsx_token(&mut self) -> syntax_kind::JsxToken {
        self.token_pos = self.pos;
        self.start_pos = self.pos;

        if self.pos >= self.end {
            self.token = SyntaxKind::EndOfFileToken;
            return syntax_kind::JsxToken::EndOfFileToken;
        }

        let c = self.text.chars().nth(self.pos);
        if c == Some('<') {
            if self.text.chars().nth(self.pos + 1) == Some('/') {
                self.pos += 2;
                self.token = SyntaxKind::Token(Token::LessThanSlash);
                return syntax_kind::JsxToken::LessThanSlashToken;
            }
            self.pos += 1;
            self.token = SyntaxKind::Token(Token::LessThan);
            return syntax_kind::JsxToken::LessThanToken;
        }

        if c == Some('{') {
            self.pos += 1;
            self.token = SyntaxKind::Token(Token::OpenBrace);
            return syntax_kind::JsxToken::OpenBraceToken;
        }

        // First non-whitespace character on this line.
        let mut first_non_whitespace: Option<usize> = Some(0);
        // These initial values are special because the first line is:
        // firstNonWhitespace = 0 to indicate that we want leading whitspace,

        while self.pos < self.end {
            let c = self.text.chars().nth(self.pos);

            if c == Some('{') {
                break;
            }
            if c == Some('<') {
                if self.is_conflict_marker_trivia() {
                    self.scan_conflict_marker_trivia();
                    self.token = SyntaxKind::ConflictMarkerTrivia;
                    return syntax_kind::JsxToken::ConflictMarkerTrivia;
                }
                break;
            }
            // FirstNonWhitespace is 0, then we only see whitespaces so far. If we see a linebreak, we want to ignore that whitespaces.
            // i.e (- : whitespace)
            //      <div>----
            //      </div> becomes <div></div>
            //
            //      <div>----</div> becomes <div>----</div>
            if c.map(is_line_break).unwrap_or(false) && first_non_whitespace == Some(0) {
                first_non_whitespace = None;
            } else if !c.map(is_white_space_like).unwrap_or(false) {
                first_non_whitespace = Some(self.pos);
            }
            self.pos += 1;
        }
        self.token_value = Some(
            self.text
                .chars()
                .skip(self.start_pos)
                .take(self.pos - self.start_pos)
                .collect(),
        );
        if first_non_whitespace.is_none() {
            syntax_kind::JsxToken::JsxTextAllWhiteSpaces
        } else {
            syntax_kind::JsxToken::JsxText
        }
    }

    // All conflict markers consist of the same character repeated seven times.  If it is
    // a <<<<<<< or >>>>>>> marker then it is also followed by a space.
    fn is_conflict_marker_trivia(&self) -> bool {
        if self.pos == 0
            || self
                .text
                .chars()
                .nth(self.pos - 1)
                .map(is_line_break)
                .unwrap_or(false)
        {
            let ch = self.text.chars().nth(self.pos);

            if (self.pos + *MERGE_CONFLICT_MARKER_LENGTH) < self.text.len() {
                if self
                    .text
                    .chars()
                    .skip(self.pos)
                    .take(*MERGE_CONFLICT_MARKER_LENGTH)
                    .any(|c| c != ch.unwrap())
                {
                    return false;
                }
            }

            return ch == Some('=')
                || self
                    .text
                    .chars()
                    .nth(self.pos + *MERGE_CONFLICT_MARKER_LENGTH)
                    == Some('=');
        }
        false
    }

    fn scan_conflict_marker_trivia(&mut self) {
        self.error(
            diagnostic::Message::MergeConflictMarkerEncountered,
            Some(self.pos),
            Some(*MERGE_CONFLICT_MARKER_LENGTH),
        );

        let ch = self.text.chars().nth(self.pos);

        if ch == Some('<') || ch == Some('>') {
            while self
                .text
                .chars()
                .nth(self.pos)
                .map(is_line_break)
                .unwrap_or(false)
            {
                self.pos += 1;
            }
        } else {
            assert!(ch == Some('|') || ch == Some('='));
            // Consume everything from the start of a ||||||| or ======= marker to the start
            // of the next ======= or >>>>>>> marker.
            loop {
                let current_char = self.text.chars().nth(self.pos);
                if (current_char == Some('=') || current_char == Some('|'))
                    && current_char != ch
                    && self.is_conflict_marker_trivia()
                {
                    break;
                }
                self.pos += 1;
            }
        }
    }

    pub fn scan_jsdoc_token(&mut self) -> syntax_kind::JsDoc {
        self.token_pos = self.pos;
        self.start_pos = self.token_pos;
        if self.pos >= self.end {
            self.token = SyntaxKind::EndOfFileToken;
            return syntax_kind::JsDoc::EndOfFileToken;
        }

        let ch = self.text.chars().nth(self.pos).unwrap();
        self.pos += 1;
        match ch {
            '\t' | character_codes::VERTICAL_TAB | character_codes::FORM_FEED | ' ' => {
                while self
                    .text
                    .chars()
                    .nth(self.pos)
                    .map(|c| is_white_space_single_line(c))
                    .unwrap_or(false)
                {
                    self.pos += 1;
                }
                self.token = syntax_kind::JsDoc::WhitespaceTrivia.into();
                return syntax_kind::JsDoc::WhitespaceTrivia;
            }
            '@' => {
                self.token = syntax_kind::JsDoc::AtToken.into();
                return syntax_kind::JsDoc::AtToken;
            }
            character_codes::LINE_FEED | character_codes::CARRIAGE_RETURN => {
                self.token_flags |= TokenFlags::PRECEDING_LINE_BREAK;
                self.token = SyntaxKind::NewLineTrivia;
                return syntax_kind::JsDoc::NewLineTrivia;
            }
            '*' => {
                self.token = syntax_kind::JsDoc::AsteriskToken.into();
                return syntax_kind::JsDoc::AsteriskToken;
            }
            '{' => {
                self.token = syntax_kind::JsDoc::OpenBraceToken.into();
                return syntax_kind::JsDoc::OpenBraceToken;
            }
            '}' => {
                self.token = syntax_kind::JsDoc::CloseBraceToken.into();
                return syntax_kind::JsDoc::CloseBraceToken;
            }
            '[' => {
                self.token = syntax_kind::JsDoc::OpenBracketToken.into();
                return syntax_kind::JsDoc::OpenBracketToken;
            }
            ']' => {
                self.token = syntax_kind::JsDoc::CloseBracketToken.into();
                return syntax_kind::JsDoc::CloseBracketToken;
            }
            '<' => {
                self.token = syntax_kind::JsDoc::LessThanToken.into();
                return syntax_kind::JsDoc::LessThanToken;
            }
            '=' => {
                self.token = syntax_kind::JsDoc::EqualsToken.into();
                return syntax_kind::JsDoc::EqualsToken;
            }
            ',' => {
                self.token = syntax_kind::JsDoc::CommaToken.into();
                return syntax_kind::JsDoc::CommaToken;
            }
            '.' => {
                self.token = syntax_kind::JsDoc::DotToken.into();
                return syntax_kind::JsDoc::DotToken;
            }
            '`' => {
                while self.pos < self.end && self.text.chars().nth(self.pos) != Some('`') {
                    self.pos += 1;
                }
                self.token_value = Some(
                    self.text
                        .chars()
                        .skip(self.token_pos + 1)
                        .take(self.pos - self.token_pos - 1)
                        .collect(),
                );
                self.pos += 1;
                self.token = SyntaxKind::NoSubstitutionTemplateLiteral;
                return syntax_kind::JsDoc::NoSubstitutionTemplateLiteral;
            }
            _ => {}
        }

        if is_identifier_start(ch, ScriptTarget::latest()) {
            while self
                .text
                .chars()
                .nth(self.pos)
                .map(|c| is_identifier_part(c, ScriptTarget::latest()))
                .unwrap_or(false)
            {
                self.pos += 1;
            }
            self.token_value = Some(
                self.text
                    .chars()
                    .skip(self.token_pos)
                    .take(self.pos - self.token_pos)
                    .collect(),
            );
            let identifier = self.get_identifier_token();
            self.token = identifier.into();
            return identifier.into();
        } else {
            self.token = SyntaxKind::Unknown;
            return syntax_kind::JsDoc::Unknown;
        }
    }

    fn get_identifier_token(&mut self) -> syntax_kind::Identifier {
        if let Ok(keyword) = self
            .token_value
            .as_ref()
            .expect("expected token value in get_identifier_token")
            .parse()
        {
            let identifier = syntax_kind::Identifier::from_keyword(keyword);
            self.token = identifier.into();
            return identifier;
        }
        self.token = syntax_kind::Identifier::identifier().into();
        return syntax_kind::Identifier::identifier();
    }

    pub fn scan(&mut self) -> SyntaxKind {
        self.start_pos = self.pos;
        self.token_flags = TokenFlags::NONE;
        let mut asterisk_seen = false;
        loop {
            self.token_pos = self.pos;
            let ch = self.text.chars().nth(self.pos);
            if let Some(ch) = ch {
                // Special handling for shebang
                if ch == '#' && self.pos == 0 && self.is_shebang_trivia() {
                    self.scan_shebang_trivia();
                    if self.skip_trivia {
                        continue;
                    } else {
                        self.token = SyntaxKind::ShebangTrivia;
                        return SyntaxKind::ShebangTrivia;
                    }
                }

                use character_codes::*;
                match ch {
                    LINE_FEED | CARRIAGE_RETURN => {
                        self.token_flags |= TokenFlags::PRECEDING_LINE_BREAK;
                        if self.skip_trivia {
                            self.pos += 1;
                            continue;
                        } else {
                            if ch == CARRIAGE_RETURN && self.pos + 1 < self.end {
                                self.pos += 2;
                            } else {
                                self.pos += 1;
                            }
                            self.token = SyntaxKind::NewLineTrivia;
                            return SyntaxKind::NewLineTrivia;
                        }
                    }
                    '\t'
                    | VERTICAL_TAB
                    | FORM_FEED
                    | ' '
                    | NON_BREAKING_SPACE
                    | OGHAM
                    | EN_QUAD
                    | EM_QUAD
                    | EN_SPACE
                    | EM_SPACE
                    | THREE_PER_EM_SPACE
                    | FOUR_PER_EM_SPACE
                    | SIX_PER_EM_SPACE
                    | FIGURE_SPACE
                    | PUNCTUATION_SPACE
                    | THIN_SPACE
                    | HAIR_SPACE
                    | ZERO_WIDTH_SPACE
                    | NARROW_NO_BREAK_SPACE
                    | MATHEMATICAL_SPACE
                    | IDEOGRAPHIC_SPACE
                    | BYTE_ORDER_MARK => {
                        if self.skip_trivia {
                            self.pos += 1;
                            continue;
                        } else {
                            while self
                                .text
                                .chars()
                                .nth(self.pos)
                                .map(|c| is_white_space_single_line(c))
                                .unwrap_or(false)
                            {
                                self.pos += 1;
                            }
                            self.token = SyntaxKind::WhitespaceTrivia;
                            return self.token;
                        }
                    }
                    '!' => {
                        if self.text.chars().nth(self.pos + 1) == Some('=') {
                            if self.text.chars().nth(self.pos + 2) == Some('=') {
                                self.pos += 3;
                                self.token = SyntaxKind::Token(Token::ExclamationEqualsEquals);
                                return self.token;
                            } else {
                                self.pos += 2;
                                self.token = SyntaxKind::Token(Token::ExclamationEquals);
                                return self.token;
                            }
                        } else {
                            self.pos += 1;
                            self.token = SyntaxKind::Token(Token::Exclamation);
                            return self.token;
                        }
                    }
                    '\'' | '"' => {
                        self.token_value = Some(self.scan_string(false));
                        self.token = SyntaxKind::StringLiteral;
                        return self.token;
                    }
                    '`' => {
                        self.token = self.scan_template_and_set_token_value();
                        return self.token;
                    }
                    '%' => {
                        if self.text.chars().nth(self.pos + 1) == Some('=') {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::PercentEquals);
                            return self.token;
                        } else {
                            self.pos += 1;
                            self.token = SyntaxKind::Token(Token::Percent);
                            return self.token;
                        }
                    }
                    '&' => {
                        if self.text.chars().nth(self.pos + 1) == Some('&') {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::AmpersandAmpersand);
                            return self.token;
                        } else if self.text.chars().nth(self.pos + 1) == Some('=') {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::AmpersandEquals);
                            return self.token;
                        } else {
                            self.pos += 1;
                            self.token = SyntaxKind::Token(Token::Ampersand);
                            return self.token;
                        }
                    }
                    '(' => {
                        self.pos += 1;
                        self.token = SyntaxKind::Token(Token::OpenParen);
                        return self.token;
                    }
                    ')' => {
                        self.pos += 1;
                        self.token = SyntaxKind::Token(Token::CloseParen);
                        return self.token;
                    }
                    '*' => {
                        if self.text.chars().nth(self.pos + 1) == Some('=') {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::AsteriskEquals);
                            return self.token;
                        } else if self.text.chars().nth(self.pos + 1) == Some('*') {
                            if self.text.chars().nth(self.pos + 2) == Some('=') {
                                self.pos += 3;
                                self.token = SyntaxKind::Token(Token::AsteriskAsteriskEquals);
                                return self.token;
                            } else {
                                self.pos += 2;
                                self.token = SyntaxKind::Token(Token::AsteriskAsterisk);
                                return self.token;
                            }
                        } else {
                            self.pos += 1;
                            if self.in_jsdoc_type > 0
                                && !asterisk_seen
                                && self.token_flags.contains(TokenFlags::PRECEDING_LINE_BREAK)
                            {
                                // decoration at the start of a JSDoc comment line
                                asterisk_seen = true;
                                continue;
                            } else {
                                self.token = SyntaxKind::Token(Token::Asterisk);
                                return self.token;
                            }
                        }
                    }
                    '+' => {
                        if self.text.chars().nth(self.pos + 1) == Some('+') {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::PlusPlus);
                            return self.token;
                        } else if self.text.chars().nth(self.pos + 1) == Some('=') {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::PlusEquals);
                            return self.token;
                        } else {
                            self.pos += 1;
                            self.token = SyntaxKind::Token(Token::Plus);
                            return self.token;
                        }
                    }
                    ',' => {
                        self.pos += 1;
                        self.token = SyntaxKind::Token(Token::Comma);
                        return self.token;
                    }
                    '-' => {
                        if self.text.chars().nth(self.pos + 1) == Some('-') {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::MinusMinus);
                            return self.token;
                        } else if self.text.chars().nth(self.pos + 1) == Some('=') {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::MinusEquals);
                            return self.token;
                        } else {
                            self.pos += 1;
                            self.token = SyntaxKind::Token(Token::Minus);
                            return self.token;
                        }
                    }
                    '.' => {
                        if self
                            .text
                            .chars()
                            .nth(self.pos + 1)
                            .map(|c| c.is_digit(10))
                            .unwrap_or(false)
                        {
                            let (token, value) = self.scan_number();
                            self.token = token;
                            self.token_value = Some(value);
                            return self.token;
                        } else if self.text.chars().nth(self.pos + 1) == Some('.')
                            && self.text.chars().nth(self.pos + 2) == Some('.')
                        {
                            self.pos += 3;
                            self.token = SyntaxKind::Token(Token::DotDotDot);
                            return self.token;
                        } else {
                            self.pos += 1;
                            self.token = SyntaxKind::Token(Token::Dot);
                            return self.token;
                        }
                    }
                    '/' => {
                        // Single-line comment
                        if self.text.chars().nth(self.pos + 1) == Some('/') {
                            self.pos += 2;
                            while self
                                .text
                                .chars()
                                .nth(self.pos)
                                .map(|c| !is_line_break(c))
                                .unwrap_or(false)
                            {
                                self.pos += 1;
                            }

                            if self.skip_trivia {
                                continue;
                            } else {
                                self.token = SyntaxKind::SingleLineCommentTrivia;
                            }
                        }
                        // Multi-line comment
                        if self.text.chars().nth(self.pos + 1) == Some('*') {
                            self.pos += 2;
                            if self.text.chars().nth(self.pos) == Some('*')
                                && self.text.chars().nth(self.pos + 1) != Some('/')
                            {
                                self.token_flags |= TokenFlags::PRECEDING_JSDOC_COMMENT;
                            }

                            let mut comment_closed = false;
                            loop {
                                let ch = self.text.chars().nth(self.pos);
                                if ch.is_none() {
                                    break;
                                }

                                if ch == Some('*')
                                    && self.text.chars().nth(self.pos + 1) == Some('/')
                                {
                                    self.pos += 2;
                                    comment_closed = true;
                                    break;
                                }

                                if ch.map(is_line_break).unwrap_or(false) {
                                    self.token_flags |= TokenFlags::PRECEDING_LINE_BREAK;
                                }

                                self.pos += 1;
                            }

                            if !comment_closed {
                                self.error(diagnostic::Message::AsteriskSlashExpected, None, None);
                            }

                            if self.skip_trivia {
                                continue;
                            } else {
                                if !comment_closed {
                                    self.token_flags |= TokenFlags::UNTERMINATED;
                                }
                                self.token = SyntaxKind::MultiLineCommentTrivia;
                                return self.token;
                            }
                        }

                        if self.text.chars().nth(self.pos + 1) == Some('=') {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::SlashEquals);
                            return self.token;
                        } else {
                            self.pos += 1;
                            self.token = SyntaxKind::Token(Token::Slash);
                            return self.token;
                        }
                    }
                    '0' => {
                        if self.pos + 2 < self.end
                            && self
                                .text
                                .chars()
                                .nth(self.pos + 1)
                                .map(|c| c.to_ascii_lowercase())
                                == Some('x')
                        {
                            self.pos += 2;
                            self.token_value =
                                Some(self.scan_minimum_number_of_hex_digits(1, true));
                            if self.token_value.as_ref().unwrap().is_empty() {
                                self.error(
                                    diagnostic::Message::HexadecimalDigitExpected,
                                    None,
                                    None,
                                );
                                self.token_value = Some("0".to_string());
                            }
                            self.token_value =
                                self.token_value.as_ref().map(|v| "0x".to_string() + v);
                            self.token_flags |= TokenFlags::HEX_SPECIFIER;
                            self.token = self.check_big_int_suffix();
                            return self.token;
                        } else if self.pos + 2 < self.end
                            && self
                                .text
                                .chars()
                                .nth(self.pos + 1)
                                .map(|c| c.to_ascii_lowercase())
                                == Some('b')
                        {
                            self.pos += 2;
                            self.token_value =
                                Some(self.scan_binary_or_octal_digits(BinaryOrOctal::Binary));
                            if self.token_value().is_none()
                                || self.token_value().unwrap().is_empty()
                            {
                                self.error(diagnostic::Message::BinaryDigitExpected, None, None);
                                self.token_value = Some("0".to_string());
                            } else {
                                self.token_value =
                                    self.token_value.as_ref().map(|s| "0b".to_string() + s);
                                self.token_flags |= TokenFlags::BINARY_SPECIFIER;
                                self.token = self.check_big_int_suffix();
                                return self.token;
                            }
                        } else if self.pos + 2 < self.end
                            && self
                                .text
                                .chars()
                                .nth(self.pos + 1)
                                .map(|c| c.to_ascii_lowercase())
                                == Some('o')
                        {
                            self.pos += 2;
                            self.token_value =
                                Some(self.scan_binary_or_octal_digits(BinaryOrOctal::Octal));
                            if self.token_value.is_none() {
                                self.error(diagnostic::Message::OctalDigitExpected, None, None);
                                self.token_value = Some("0".to_string());
                            }
                            self.token_value =
                                self.token_value.as_ref().map(|s| "0o".to_string() + s);
                            self.token_flags |= TokenFlags::OCTAL_SPECIFIER;
                            self.token = self.check_big_int_suffix();
                            return self.token;
                        }

                        // Try to parse as an octal
                        if self.pos + 1 < self.end
                            && self
                                .text
                                .chars()
                                .nth(self.pos + 1)
                                .map(|c| c.is_digit(8))
                                .unwrap_or(false)
                        {
                            self.token_value = Some(self.scan_octal_digits().to_string());
                            self.token_flags |= TokenFlags::OCTAL_SPECIFIER;
                            self.token = SyntaxKind::NumericLiteral;
                            return self.token;
                        }

                        // Parse as number
                        let (t, value) = self.scan_number();
                        self.token = t;
                        self.token_value = Some(value);
                        return self.token;
                    }
                    c if c.is_digit(10) => {
                        let (t, value) = self.scan_number();
                        self.token = t;
                        self.token_value = Some(value);
                        return self.token;
                    }
                    ':' => {
                        self.pos += 1;
                        self.token = SyntaxKind::Token(Token::Colon);
                        return self.token;
                    }
                    ';' => {
                        self.pos += 1;
                        self.token = SyntaxKind::Token(Token::Semicolon);
                        return self.token;
                    }
                    '<' => {
                        if self.is_conflict_marker_trivia() {
                            self.scan_conflict_marker_trivia();
                            if self.skip_trivia {
                                continue;
                            } else {
                                self.token = SyntaxKind::ConflictMarkerTrivia;
                                return self.token;
                            }
                        }

                        let next_char = self.text.chars().nth(self.pos + 1);
                        if next_char == Some('<') {
                            if self.text.chars().nth(self.pos + 2) == Some('=') {
                                self.pos += 3;
                                self.token = SyntaxKind::Token(Token::LessThanLessThanEquals);
                                return self.token;
                            } else {
                                self.pos += 2;
                                self.token = SyntaxKind::Token(Token::LessThanLessThan);
                                return self.token;
                            }
                        } else if next_char == Some('=') {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::LessThanEquals);
                            return self.token;
                        } else if self.language_variant == LanguageVariant::JSX
                            && next_char == Some('/')
                            && self.text.chars().nth(self.pos + 2) == Some('*')
                        {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::LessThanSlash);
                            return self.token;
                        } else {
                            self.pos += 1;
                            self.token = SyntaxKind::Token(Token::LessThan);
                            return self.token;
                        }
                    }
                    '=' => {
                        if self.is_conflict_marker_trivia() {
                            self.scan_conflict_marker_trivia();
                            if self.skip_trivia {
                                continue;
                            } else {
                                self.token = SyntaxKind::ConflictMarkerTrivia;
                                return self.token;
                            }
                        } else if self.text.chars().nth(self.pos + 1) == Some('=') {
                            if self.text.chars().nth(self.pos + 2) == Some('=') {
                                self.pos += 3;
                                self.token = SyntaxKind::Token(Token::EqualsEqualsEquals);
                                return self.token;
                            } else {
                                self.pos += 2;
                                self.token = SyntaxKind::Token(Token::EqualsEquals);
                                return self.token;
                            }
                        } else {
                            self.pos += 1;
                            self.token = SyntaxKind::Token(Token::Equals);
                            return self.token;
                        }
                    }
                    '>' => {
                        if self.is_conflict_marker_trivia() {
                            self.scan_conflict_marker_trivia();
                            if self.skip_trivia {
                                continue;
                            } else {
                                self.token = SyntaxKind::ConflictMarkerTrivia;
                                return self.token;
                            }
                        } else {
                            self.pos += 1;
                            self.token = SyntaxKind::Token(Token::GreaterThan);
                            return self.token;
                        }
                    }
                    '?' => {
                        self.pos += 1;
                        self.token = SyntaxKind::Token(Token::Question);
                        return self.token;
                    }
                    '[' => {
                        self.pos += 1;
                        self.token = SyntaxKind::Token(Token::OpenBracket);
                        return self.token;
                    }
                    ']' => {
                        self.pos += 1;
                        self.token = SyntaxKind::Token(Token::CloseBracket);
                        return self.token;
                    }
                    '^' => {
                        if self.text.chars().nth(self.pos + 1) == Some('=') {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::CaretEquals);
                            return self.token;
                        } else {
                            self.pos += 1;
                            self.token = SyntaxKind::Token(Token::Caret);
                        }
                    }
                    '{' => {
                        self.pos += 1;
                        self.token = SyntaxKind::Token(Token::OpenBrace);
                        return self.token;
                    }
                    '|' => {
                        if self.is_conflict_marker_trivia() {
                            self.scan_conflict_marker_trivia();
                            if self.skip_trivia {
                                continue;
                            } else {
                                self.token = SyntaxKind::ConflictMarkerTrivia;
                                return self.token;
                            }
                        } else if self.text.chars().nth(self.pos + 1) == Some('|') {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::BarBar);
                            return self.token;
                        } else if self.text.chars().nth(self.pos + 1) == Some('=') {
                            self.pos += 2;
                            self.token = SyntaxKind::Token(Token::BarEquals);
                            return self.token;
                        } else {
                            self.pos += 1;
                            self.token = SyntaxKind::Token(Token::Bar);
                            return self.token;
                        }
                    }
                    '}' => {
                        self.pos += 1;
                        self.token = SyntaxKind::Token(Token::CloseBrace);
                        return self.token;
                    }
                    '~' => {
                        self.pos += 1;
                        self.token = SyntaxKind::Token(Token::Tilde);
                        return self.token;
                    }
                    '@' => {
                        self.pos += 1;
                        self.token = SyntaxKind::Token(Token::At);
                        return self.token;
                    }
                    '\\' => {
                        let cooked_char = self.peek_unicode_escape();
                        if let Some(cooked_char) = cooked_char {
                            if is_identifier_start(cooked_char, self.language_version) {
                                self.pos += 6;
                                let mut token_value = String::new();
                                token_value.push(cooked_char);
                                token_value += &self.scan_identifier_parts();
                                self.token_value = Some(token_value);
                                self.token = self.get_identifier_token().into();
                                return self.token;
                            }
                        } else {
                            self.error(diagnostic::Message::InvalidCharacter, None, None);
                            self.pos += 1;
                            self.token = SyntaxKind::Unknown;
                            return self.token;
                        }
                    }
                    c => {
                        if is_identifier_start(c, self.language_version) {
                            self.pos += 1;
                            while self
                                .text
                                .chars()
                                .nth(self.pos)
                                .map(|c| is_identifier_part(c, self.language_version))
                                .unwrap_or(false)
                            {
                                self.pos += 1;
                            }
                            let mut token_value: String = self
                                .text
                                .chars()
                                .skip(self.token_pos)
                                .take(self.pos - self.token_pos)
                                .collect();
                            if self.text.chars().nth(self.pos) == Some('\\') {
                                token_value.push_str(&self.scan_identifier_parts());
                            }
                            self.token_value = Some(token_value);
                            self.token = self.get_identifier_token().into();
                            return self.token;
                        } else if is_white_space_single_line(c) {
                            self.pos += 1;
                            continue;
                        } else if is_line_break(c) {
                            self.token_flags |= TokenFlags::PRECEDING_LINE_BREAK;
                            self.pos += 1;
                            continue;
                        } else {
                            self.error(diagnostic::Message::InvalidCharacter, None, None);
                            self.pos += 1;
                            self.token = SyntaxKind::Unknown;
                            return self.token;
                        }
                    }
                }
            } else {
                self.token = SyntaxKind::EndOfFileToken;
                return SyntaxKind::EndOfFileToken;
            }
        }
    }

    fn scan_binary_or_octal_digits(&mut self, mode: BinaryOrOctal) -> String {
        let mut value = String::new();
        // For counting number of digits; Valid binaryIntegerLiteral must have at least one binary digit following B or b.
        // Similarly valid octalIntegerLiteral must have at least one octal digit following o or O.
        let mut separator_allowed = false;
        let mut is_previous_token_separator = false;
        loop {
            let ch = self.text.chars().nth(self.pos);
            // Numeric separators are allowed anywhere within a numeric literal, except not at the beginning, or following another separator
            if ch == Some('_') {
                self.token_flags |= TokenFlags::CONTAINS_SEPARATOR;
                if separator_allowed {
                    separator_allowed = false;
                    is_previous_token_separator = true;
                } else if is_previous_token_separator {
                    self.error(
                        diagnostic::Message::MultipleConsecutiveNumericSeparatorsNotPermitted,
                        Some(self.pos),
                        Some(1),
                    );
                } else {
                    self.error(
                        diagnostic::Message::NumericSeparatorsAreNotAllowedHere,
                        Some(self.pos),
                        Some(1),
                    );
                }
                self.pos += 1;
                continue;
            }
            separator_allowed = true;
            if ch.map(|c| !c.is_digit(mode.base())).unwrap_or(false) {
                break;
            }
            value.push(ch.unwrap());
            self.pos += 1;
            is_previous_token_separator = false;
        }
        if self.text.chars().nth(self.pos - 1) == Some('_') {
            self.error(
                diagnostic::Message::MultipleConsecutiveNumericSeparatorsNotPermitted,
                Some(self.pos),
                Some(1),
            );
        }
        return value;
    }

    pub(crate) fn is_shebang_trivia(&self) -> bool {
        assert!(self.pos == 0);
        SHEBANG_TRIVIA_REGEX.is_match(&self.text)
    }

    pub(crate) fn scan_shebang_trivia(&mut self) {
        self.pos += SHEBANG_TRIVIA_REGEX
            .find(&self.text)
            .unwrap()
            .as_str()
            .len();
    }

    pub fn scan_string(&mut self, jsx_attribute_string: bool) -> String {
        let quote = self.text.chars().nth(self.pos);
        self.pos += 1;
        let mut result = String::new();
        let mut start = self.pos;
        loop {
            if self.pos >= self.end {
                result += &self
                    .text
                    .chars()
                    .skip(start)
                    .take(self.pos - start)
                    .collect::<String>();
                self.token_flags |= TokenFlags::UNTERMINATED;
                self.error(diagnostic::Message::UnterminatedStringLiteral, None, None);
                break;
            }
            let ch = self.text.chars().nth(self.pos);
            if ch == quote {
                result += &self
                    .text
                    .chars()
                    .skip(start)
                    .take(self.pos - start)
                    .collect::<String>();
                self.pos += 1;
                break;
            }
            if ch == Some('\\') && !jsx_attribute_string {
                result += &self
                    .text
                    .chars()
                    .skip(start)
                    .take(self.pos - start)
                    .collect::<String>();
                if let Some(c) = self.scan_escape_sequence() {
                    result.push(c);
                }
                start = self.pos;
                continue;
            }
            if ch.map(is_line_break).unwrap_or(false) && !jsx_attribute_string {
                result += &self
                    .text
                    .chars()
                    .skip(start)
                    .take(self.pos - start)
                    .collect::<String>();
                self.token_flags |= TokenFlags::UNTERMINATED;
                self.error(diagnostic::Message::UnterminatedStringLiteral, None, None);
                break;
            }
            self.pos += 1;
        }
        result
    }

    pub fn re_scan_slash_token(&mut self) -> SyntaxKind {
        if self.token == SyntaxKind::Token(Token::Slash)
            || self.token == SyntaxKind::Token(Token::SlashEquals)
        {
            let mut p = self.token_pos + 1;
            let mut in_escape = false;
            let mut in_character_class = false;
            loop {
                // If we reach the end of a file, or hit a newline, then this is an unterminated
                // regex.  Report error and return what we have so far.
                if p >= self.end {
                    self.token_flags |= TokenFlags::UNTERMINATED;
                    self.error(
                        diagnostic::Message::UnterminatedRegularExpressionLiteral,
                        None,
                        None,
                    );
                    break;
                }

                let ch = self.text.chars().nth(p);
                if ch.map(|c| is_line_break(c)).unwrap_or(false) {
                    self.token_flags |= TokenFlags::UNTERMINATED;
                    self.error(
                        diagnostic::Message::UnterminatedRegularExpressionLiteral,
                        None,
                        None,
                    );
                    break;
                }

                if in_escape {
                    // Parsing an escape character;
                    // reset the flag and just advance to the next char.
                    in_escape = false;
                } else if ch == Some('/') && !in_character_class {
                    // A slash within a character class is permissible,
                    // but in general it signals the end of the regexp literal.
                    p += 1;
                    break;
                } else if ch == Some('[') {
                    in_character_class = true;
                } else if ch == Some('\\') {
                    in_escape = true;
                } else if ch == Some(']') {
                    in_character_class = false;
                }
                p += 1;
            }

            let ch = self.text.chars().nth(p);
            while ch
                .map(|c| is_identifier_part(c, self.language_version))
                .unwrap_or(false)
            {
                p += 1;
            }
            self.pos = p;
            self.token_value = Some(
                self.text
                    .chars()
                    .skip(self.token_pos)
                    .take(self.pos - self.token_pos)
                    .collect(),
            );
            self.token = SyntaxKind::RegularExpressionLiteral;
        }

        self.token
    }

    pub fn re_scan_template_token(&mut self) -> SyntaxKind {
        assert!(
            self.token == SyntaxKind::Token(Token::CloseBrace),
            "'reScanTemplateToken' should only be called on a '}'"
        );
        self.pos = self.token_pos;
        self.token = self.scan_template_and_set_token_value();
        self.token
    }

    pub fn set_language_variant(&mut self, variant: LanguageVariant) {
        self.language_variant = variant;
    }

    pub(crate) fn set_in_jsdoc_type(&mut self, in_type: bool) {
        if in_type {
            self.in_jsdoc_type += 1;
        } else {
            self.in_jsdoc_type -= 1;
        }
    }

    /// Sets the current 'tokenValue' and returns a NoSubstitutionTemplateLiteral or
    /// a literal component of a TemplateExpression.
    fn scan_template_and_set_token_value(&mut self) -> SyntaxKind {
        let started_with_backtick = self.text.chars().nth(self.pos) == Some('`');

        self.pos += 1;
        let mut start = self.pos;
        let mut contents = String::new();
        let resulting_token: SyntaxKind;

        loop {
            if self.pos >= self.end {
                contents += &self
                    .text
                    .chars()
                    .skip(start)
                    .take(self.pos - start)
                    .collect::<String>();
                self.token_flags |= TokenFlags::UNTERMINATED;
                self.error(diagnostic::Message::UnterminatedTemplateLiteral, None, None);
                resulting_token = if started_with_backtick {
                    SyntaxKind::NoSubstitutionTemplateLiteral
                } else {
                    SyntaxKind::TemplateTail
                };
                break;
            }

            let curr_char = self.text.chars().nth(self.pos);

            // '`'
            if curr_char == Some('`') {
                contents += &self
                    .text
                    .chars()
                    .skip(start)
                    .take(self.pos - start)
                    .collect::<String>();
                self.pos += 1;
                resulting_token = if started_with_backtick {
                    SyntaxKind::NoSubstitutionTemplateLiteral
                } else {
                    SyntaxKind::TemplateTail
                };
                break;
            }

            // '${'
            if curr_char == Some('$') && self.text.chars().nth(self.pos + 1) == Some('{') {
                contents += &self
                    .text
                    .chars()
                    .skip(start)
                    .take(self.pos - start)
                    .collect::<String>();
                self.pos += 2;
                resulting_token = if started_with_backtick {
                    SyntaxKind::TemplateHead
                } else {
                    SyntaxKind::TemplateMiddle
                };
                break;
            }

            // Escape character
            if curr_char == Some('\\') {
                contents += &self
                    .text
                    .chars()
                    .skip(start)
                    .take(self.pos - start)
                    .collect::<String>();
                if let Some(c) = self.scan_escape_sequence() {
                    contents.push(c);
                }
                start = self.pos;
                continue;
            }

            // Speculated ECMAScript 6 Spec 11.8.6.1:
            // <CR><LF> and <CR> LineTerminatorSequences are normalized to <LF> for Template Values
            if curr_char == Some(character_codes::CARRIAGE_RETURN) {
                contents += &self
                    .text
                    .chars()
                    .skip(start)
                    .take(self.pos - start)
                    .collect::<String>();
                self.pos += 1;

                if self.text.chars().nth(self.pos) == Some(character_codes::LINE_FEED) {
                    self.pos += 1;
                }

                contents += "\n";
                start = self.pos;
                continue;
            }

            self.pos += 1;
        }

        self.token_value = Some(contents);
        resulting_token
    }

    /// Invokes the callback with the scanner set to scan the specified range. When the callback
    /// returns, the scanner is restored to the state it was in before scanRange was called.
    pub fn scan_range<T, U: (Fn() -> T)>(&mut self, start: usize, length: usize, callback: U) -> T {
        let save_end = self.end;
        let save_pos = self.pos;
        let save_start_pos = self.start_pos;
        let save_token_pos = self.token_pos;
        let save_token = self.token;
        let save_token_value = self.token_value.clone();
        let save_token_flags = self.token_flags;

        self.set_text(self.text.clone(), Some(start), Some(length));
        let result = callback();

        self.end = save_end;
        self.pos = save_pos;
        self.start_pos = save_start_pos;
        self.token_pos = save_token_pos;
        self.token = save_token;
        self.token_value = save_token_value;
        self.token_flags = save_token_flags;

        result
    }

    pub fn text(&self) -> &str {
        &self.text
    }

    /// Sets the text for the scanner to scan. An optional subrange starting point and length
    /// can be provided to have the scanner only scan a portion of the text.
    pub fn set_text(&mut self, text: String, start: Option<usize>, length: Option<usize>) {
        self.text = text;
        self.end = length.unwrap_or_else(|| self.text.len() + start.unwrap_or(0));
        self.set_text_pos(start.unwrap_or(0))
    }

    pub fn set_script_target(&mut self, script_target: ScriptTarget) {
        self.language_version = script_target;
    }

    pub fn set_text_pos(&mut self, text_pos: usize) {
        self.pos = text_pos;
        self.start_pos = text_pos;
        self.token_pos = text_pos;
        self.token = SyntaxKind::Unknown;
        self.token_value = None;
        self.token_flags = TokenFlags::NONE;
    }

    fn scan_escape_sequence(&mut self) -> Option<char> {
        self.pos += 1;
        let ch = self.text.chars().nth(self.pos);
        if let Some(c) = ch {
            self.pos += 1;
            match c {
                '0' => Some('\0'),
                'b' => Some(character_codes::BACKSPACE),
                't' => Some('\t'),
                'n' => Some('\n'),
                'v' => Some(character_codes::VERTICAL_TAB),
                'f' => Some(character_codes::FORM_FEED),
                'r' => Some('\r'),
                'u' => {
                    if self.text.chars().nth(self.pos) == Some('{') {
                        self.token_flags |= TokenFlags::EXTENDED_UNICODE_ESCAPE;
                        self.pos += 1;
                        self.scan_extended_unicode_escape()
                    } else {
                        self.scan_hexadecimal_escape(4)
                    }
                }
                'x' => self.scan_hexadecimal_escape(2),
                character_codes::CARRIAGE_RETURN => {
                    if self.text.chars().nth(self.pos) == Some(character_codes::LINE_FEED) {
                        self.pos += 1;
                    }
                    None
                }
                character_codes::LINE_FEED
                | character_codes::LINE_SEPARATOR
                | character_codes::PARAGRAPH_SEPARATOR => None,
                other => Some(other),
            }
        } else {
            self.error(diagnostic::Message::UnexpectedEndOfText, None, None);
            None
        }
    }

    fn scan_hexadecimal_escape(&mut self, num_digits: usize) -> Option<char> {
        self.scan_exact_number_of_hex_digits(num_digits, false)
            .and_then(std::char::from_u32)
    }

    fn scan_extended_unicode_escape(&mut self) -> Option<char> {
        let escaped_value_string = self.scan_minimum_number_of_hex_digits(1, false);
        let escaped_value = u32::from_str_radix(&escaped_value_string, 16);
        let mut is_invalid_extended_escape = false;

        // Validate the value of the digit
        match escaped_value {
            Err(_) => {
                self.error(diagnostic::Message::HexadecimalDigitExpected, None, None);
                return None;
            }
            Ok(escaped_value) => {
                if escaped_value > 0x10FFF {
                    self.error(
                        diagnostic::Message::AnExtendedUnicodeEscapeValueMustBeBetween,
                        None,
                        None,
                    );
                    is_invalid_extended_escape = true;
                }

                let ch = self.text.chars().nth(self.pos);

                if let Some(ch) = ch {
                    if ch == '}' {
                        // Only swallow the following character up if it's a '}'.
                        self.pos += 1;
                    } else {
                        self.error(
                            diagnostic::Message::UnterminatedUnicodeEscapeSequence,
                            None,
                            None,
                        );
                        is_invalid_extended_escape = true;
                    }
                } else {
                    self.error(diagnostic::Message::UnexpectedEndOfText, None, None);
                    is_invalid_extended_escape = true;
                }

                if is_invalid_extended_escape {
                    return None;
                }

                std::char::from_u32(escaped_value)
            }
        }
    }

    /// Scans as many hexadecimal digits as are available in the text,
    /// returning "" if the given number of digits was unavailable.
    fn scan_minimum_number_of_hex_digits(
        &mut self,
        count: usize,
        can_have_separator: bool,
    ) -> String {
        self.scan_hex_digits(count, true, can_have_separator)
    }

    /// Scans as many hexadecimal digits as are available in the text,
    /// returning `None` if the given number of digits was unavailable.
    fn scan_exact_number_of_hex_digits(
        &mut self,
        count: usize,
        can_have_separator: bool,
    ) -> Option<u32> {
        let value_scan = self.scan_hex_digits(count, false, can_have_separator);
        u32::from_str_radix(&value_scan, 16).ok()
    }

    fn scan_hex_digits(
        &mut self,
        min_count: usize,
        scan_as_many_as_possible: bool,
        can_have_separator: bool,
    ) -> String {
        let mut value_chars = Vec::new();
        let mut allow_separator = false;
        let mut is_previous_token_separator = false;
        while value_chars.len() < min_count || scan_as_many_as_possible {
            let mut ch = self.text.chars().nth(self.pos);
            if can_have_separator && ch == Some('_') {
                self.token_flags |= TokenFlags::CONTAINS_SEPARATOR;
                if allow_separator {
                    allow_separator = false;
                    is_previous_token_separator = true;
                } else if is_previous_token_separator {
                    self.error(
                        diagnostic::Message::MultipleConsecutiveNumericSeparatorsNotPermitted,
                        None,
                        None,
                    );
                } else {
                    self.error(
                        diagnostic::Message::NumericSeparatorsAreNotAllowedHere,
                        None,
                        None,
                    );
                }
                self.pos += 1;
                continue;
            }
            allow_separator = can_have_separator;
            if ch.map(|c| c >= 'A' && c <= 'F').unwrap_or(false) {
                ch = ch.map(|c| c.to_ascii_lowercase());
            } else if !ch.map(|c| c.is_digit(16)).unwrap_or(false) {
                break;
            }
            value_chars.push(ch.unwrap());
            self.pos += 1;
            is_previous_token_separator = false;
        }
        if value_chars.len() < min_count {
            value_chars = Vec::new();
        }
        if self.text.chars().nth(self.pos - 1) == Some('_') {
            self.error(
                diagnostic::Message::NumericSeparatorsAreNotAllowedHere,
                Some(self.pos - 1),
                Some(1),
            );
        }
        value_chars.iter().collect()
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
                    result += &self
                        .text
                        .chars()
                        .skip(start)
                        .take(self.pos - start)
                        .collect::<String>();
                } else if is_previous_token_separator {
                    self.error(
                        diagnostic::Message::MultipleConsecutiveNumericSeparatorsNotPermitted,
                        Some(self.pos),
                        Some(1),
                    );
                } else {
                    self.error(
                        diagnostic::Message::NumericSeparatorsAreNotAllowedHere,
                        Some(self.pos),
                        Some(1),
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
                diagnostic::Message::NumericSeparatorsAreNotAllowedHere,
                Some(self.pos - 1),
                Some(1),
            );
        }
        result
            + &self
                .text
                .chars()
                .skip(start)
                .take(self.pos - start)
                .collect::<String>()
    }

    fn scan_number(&mut self) -> (SyntaxKind, String) {
        let start = self.pos;
        let main_fragment = self.scan_number_fragment();
        let mut decimal_fragment: Option<String> = None;
        let mut scientific_fragment: Option<String> = None;
        if self.text.chars().nth(self.pos) == Some('.') {
            self.pos += 1;
            decimal_fragment = Some(self.scan_number_fragment());
        }
        let mut end = self.pos;
        if self.text.chars().nth(self.pos) == Some('E')
            || self.text.chars().nth(self.pos) == Some('e')
        {
            self.pos += 1;
            self.token_flags.insert(TokenFlags::SCIENTIFIC);
            if self.text.chars().nth(self.pos) == Some('+')
                || self.text.chars().nth(self.pos) == Some('-')
            {
                self.pos += 1;
            }
            let pre_numeric_part = self.pos;
            let final_fragment = self.scan_number_fragment();
            if final_fragment.is_empty() {
                self.error(diagnostic::Message::DigitExpected, None, None)
            } else {
                scientific_fragment = Some(
                    self.text.get(end..pre_numeric_part).unwrap().to_string() + &final_fragment,
                );
                end = self.pos;
            }
        }
        let mut result: String;
        if self.token_flags.contains(TokenFlags::CONTAINS_SEPARATOR) {
            result = main_fragment;
            if let Some(decimal_fragment) = &decimal_fragment {
                result += ".";
                result += decimal_fragment;
            }
            if let Some(scientific_fragment) = scientific_fragment {
                result += &scientific_fragment;
            }
        } else {
            result = self.text.chars().skip(start).take(end - start).collect();
        }

        if decimal_fragment.is_some() || self.token_flags.contains(TokenFlags::SCIENTIFIC) {
            let is_scientific =
                decimal_fragment.is_none() && self.token_flags.contains(TokenFlags::SCIENTIFIC);
            self.check_for_identifier_start_after_numeric_literal(start, is_scientific);
            (
                SyntaxKind::NumericLiteral,
                result.parse::<isize>().unwrap().to_string(),
            )
        } else {
            self.token_value = Some(result);
            // if value is an integer, check whether it is a bigint
            let value_type = self.check_big_int_suffix();
            (value_type, self.token_value.as_ref().unwrap().to_string())
        }
    }

    fn check_big_int_suffix(&mut self) -> SyntaxKind {
        if self.text.chars().nth(self.pos) == Some('n') {
            self.token_value
                .get_or_insert_with(String::new)
                .push_str("n");
            // Use base 10 instead of base 2 or base 8 for shorter literals
            if self
                .token_flags
                .contains(TokenFlags::BINARY_OR_OCTAL_SPECIFIER)
            {
                self.token_value =
                    Some(parse_pseudo_big_int(self.token_value.as_ref().unwrap()) + "n");
            }
            self.pos += 1;
            SyntaxKind::BigIntLiteral
        } else {
            // Not a bigint, so can convert to number in simplified form
            self.token_value = Some(
                if self.token_flags.contains(TokenFlags::BINARY_SPECIFIER) {
                    usize::from_str_radix(&self.token_value.as_ref().unwrap()[2..], 2)
                } else if self.token_flags.contains(TokenFlags::OCTAL_SPECIFIER) {
                    usize::from_str_radix(&self.token_value.as_ref().unwrap()[2..], 8)
                } else {
                    usize::from_str(self.token_value.as_ref().unwrap())
                }
                .unwrap()
                .to_string(),
            );
            SyntaxKind::NumericLiteral
        }
    }

    fn check_for_identifier_start_after_numeric_literal(
        &mut self,
        numeric_start: usize,
        is_scientific: bool,
    ) {
        if let Some(c) = self.text.chars().nth(self.pos) {
            if !is_identifier_start(c, self.language_version) {
                return;
            }
        }

        let identifier_start = self.pos;
        let length = self.scan_identifier_parts().len();

        if length == 1 && self.text.chars().nth(identifier_start) == Some('n') {
            if is_scientific {
                self.error(
                    diagnostic::Message::BigIntLiteralCannotUseExponentialNotation,
                    Some(numeric_start),
                    Some(identifier_start - numeric_start + 1),
                );
            } else {
                self.error(
                    diagnostic::Message::BigIntLiteralMustBeAnInteger,
                    Some(numeric_start),
                    Some(identifier_start - numeric_start + 1),
                );
            }
        } else {
            self.error(
                diagnostic::Message::IdentifierOrKeywordCannotImmediatelyFollowANumericLiteral,
                Some(identifier_start),
                Some(length),
            );
            self.pos = identifier_start;
        }
    }

    /// Current character is known to be a backslash. Check for Unicode escape of the form '\uXXXX'
    /// and return code point value if valid Unicode escape is found. Otherwise return -1.
    fn peek_unicode_escape(&mut self) -> Option<char> {
        assert!(self.text.chars().nth(self.pos) == Some('\\'));
        if self.pos + 5 < self.end && self.text.chars().nth(self.pos + 1) == Some('u') {
            let start = self.pos;
            self.pos += 2;
            let value = self.scan_exact_number_of_hex_digits(4, false);
            self.pos = start;
            value.and_then(std::char::from_u32)
        } else {
            None
        }
    }

    fn scan_identifier_parts(&mut self) -> String {
        let mut result = String::new();
        let mut start = self.pos;
        while self.pos < self.end {
            let ch = self.text.chars().nth(self.pos);
            if ch
                .map(|ch| is_identifier_part(ch, self.language_version))
                .unwrap_or(false)
            {
                self.pos += 1;
            } else if ch == Some('\\') {
                if let Some(c) = self.peek_unicode_escape() {
                    if is_identifier_part(c, self.language_version) {
                        result += &self
                            .text
                            .chars()
                            .skip(start)
                            .take(self.pos - start)
                            .collect::<String>();
                        result.push(c);
                        self.pos += 6;
                        start = self.pos;
                    } else {
                        break;
                    }
                } else {
                    break;
                }
            }
        }
        result += &self
            .text
            .chars()
            .skip(start)
            .take(self.pos - start)
            .collect::<String>();
        result
    }

    fn scan_octal_digits(&mut self) -> usize {
        let start = self.pos;
        loop {
            if self
                .text
                .chars()
                .nth(self.pos)
                .map(|c| c.is_digit(8))
                .unwrap_or(false)
            {
                self.pos += 1;
            } else {
                break;
            }
        }
        self.text.get(start..self.pos).unwrap().parse().unwrap()
    }

    fn speculation_helper<T, U: (Fn() -> Option<T>)>(
        &mut self,
        callback: U,
        is_lookahead: bool,
    ) -> Option<T> {
        let save_pos = self.pos;
        let save_start_pos = self.start_pos;
        let save_token_pos = self.token_pos;
        let save_token = self.token;
        let save_token_value = self.token_value.clone();
        let save_token_flags = self.token_flags;
        let result = callback();

        if is_lookahead || result.is_none() {
            self.pos = save_pos;
            self.start_pos = save_start_pos;
            self.token_pos = save_token_pos;
            self.token = save_token;
            self.token_value = save_token_value;
            self.token_flags = save_token_flags;
        }
        result
    }

    /// Invokes the provided callback then unconditionally restores the scanner to the state it
    /// was in immediately prior to invoking the callback.  The result of invoking the callback
    /// is returned from this function.
    pub fn look_ahead<T, U: (Fn() -> Option<T>)>(&mut self, cb: U) -> Option<T> {
        self.speculation_helper(cb, true)
    }

    // Invokes the provided callback.  If the callback returns something falsy, then it restores
    // the scanner to the state it was in immediately prior to invoking the callback.  If the
    // callback returns something truthy, then the scanner state is not rolled back.  The result
    // of invoking the callback is returned from this function.
    pub fn try_scan<T, U: (Fn() -> Option<T>)>(&mut self, cb: U) -> Option<T> {
        self.speculation_helper(cb, false)
    }
}

struct UnicodeMap(Vec<char>);

impl UnicodeMap {
    fn from_char_codes(char_codes: impl Iterator<Item = u32>) -> UnicodeMap {
        UnicodeMap(char_codes.map(|c| char::try_from(c).unwrap()).collect())
    }

    fn lookup(&self, code: char) -> bool {
        let UnicodeMap(map) = self;

        // Bail out quickly if it couldn't possibly be in the map.
        if code < map[0] {
            return false;
        }

        // Perform binary search in one of the Unicode range maps
        let mut lo = 0;
        let mut hi = map.len();
        let mut mid;

        while lo + 1 < hi {
            mid = lo + (hi - lo) / 2;
            // mid has to be even to catch a range's beginning
            mid -= mid % 2;
            if map[mid] <= code && code <= map[mid + 1] {
                return true;
            }

            if code < map[mid] {
                hi = mid;
            } else {
                lo = mid + 2;
            }
        }

        false
    }
}

lazy_static! {
    /*
        As per ECMAScript Language Specification 3th Edition, Section 7.6: Identifiers
        IdentifierStart ::
            Can contain Unicode 3.0.0 categories:
            Uppercase letter (Lu),
            Lowercase letter (Ll),
            Titlecase letter (Lt),
            Modifier letter (Lm),
            Other letter (Lo), or
            Letter number (Nl).
        IdentifierPart :: =
            Can contain IdentifierStart + Unicode 3.0.0 categories:
            Non-spacing mark (Mn),
            Combining spacing mark (Mc),
            Decimal number (Nd), or
            Connector punctuation (Pc).

        Codepoint ranges for ES3 Identifiers are extracted from the Unicode 3.0.0 specification at:
        http://www.unicode.org/Public/3.0-Update/UnicodeData-3.0.0.txt
    */
    static ref UNICODE_ES3_IDENTIFIER_START: UnicodeMap = UnicodeMap::from_char_codes(vec![
        170, 170, 181, 181, 186, 186, 192, 214, 216, 246, 248, 543, 546, 563, 592, 685, 688, 696,
        699, 705, 720, 721, 736, 740, 750, 750, 890, 890, 902, 902, 904, 906, 908, 908, 910, 929,
        931, 974, 976, 983, 986, 1011, 1024, 1153, 1164, 1220, 1223, 1224, 1227, 1228, 1232, 1269,
        1272, 1273, 1329, 1366, 1369, 1369, 1377, 1415, 1488, 1514, 1520, 1522, 1569, 1594, 1600,
        1610, 1649, 1747, 1749, 1749, 1765, 1766, 1786, 1788, 1808, 1808, 1810, 1836, 1920, 1957,
        2309, 2361, 2365, 2365, 2384, 2384, 2392, 2401, 2437, 2444, 2447, 2448, 2451, 2472, 2474,
        2480, 2482, 2482, 2486, 2489, 2524, 2525, 2527, 2529, 2544, 2545, 2565, 2570, 2575, 2576,
        2579, 2600, 2602, 2608, 2610, 2611, 2613, 2614, 2616, 2617, 2649, 2652, 2654, 2654, 2674,
        2676, 2693, 2699, 2701, 2701, 2703, 2705, 2707, 2728, 2730, 2736, 2738, 2739, 2741, 2745,
        2749, 2749, 2768, 2768, 2784, 2784, 2821, 2828, 2831, 2832, 2835, 2856, 2858, 2864, 2866,
        2867, 2870, 2873, 2877, 2877, 2908, 2909, 2911, 2913, 2949, 2954, 2958, 2960, 2962, 2965,
        2969, 2970, 2972, 2972, 2974, 2975, 2979, 2980, 2984, 2986, 2990, 2997, 2999, 3001, 3077,
        3084, 3086, 3088, 3090, 3112, 3114, 3123, 3125, 3129, 3168, 3169, 3205, 3212, 3214, 3216,
        3218, 3240, 3242, 3251, 3253, 3257, 3294, 3294, 3296, 3297, 3333, 3340, 3342, 3344, 3346,
        3368, 3370, 3385, 3424, 3425, 3461, 3478, 3482, 3505, 3507, 3515, 3517, 3517, 3520, 3526,
        3585, 3632, 3634, 3635, 3648, 3654, 3713, 3714, 3716, 3716, 3719, 3720, 3722, 3722, 3725,
        3725, 3732, 3735, 3737, 3743, 3745, 3747, 3749, 3749, 3751, 3751, 3754, 3755, 3757, 3760,
        3762, 3763, 3773, 3773, 3776, 3780, 3782, 3782, 3804, 3805, 3840, 3840, 3904, 3911, 3913,
        3946, 3976, 3979, 4096, 4129, 4131, 4135, 4137, 4138, 4176, 4181, 4256, 4293, 4304, 4342,
        4352, 4441, 4447, 4514, 4520, 4601, 4608, 4614, 4616, 4678, 4680, 4680, 4682, 4685, 4688,
        4694, 4696, 4696, 4698, 4701, 4704, 4742, 4744, 4744, 4746, 4749, 4752, 4782, 4784, 4784,
        4786, 4789, 4792, 4798, 4800, 4800, 4802, 4805, 4808, 4814, 4816, 4822, 4824, 4846, 4848,
        4878, 4880, 4880, 4882, 4885, 4888, 4894, 4896, 4934, 4936, 4954, 5024, 5108, 5121, 5740,
        5743, 5750, 5761, 5786, 5792, 5866, 6016, 6067, 6176, 6263, 6272, 6312, 7680, 7835, 7840,
        7929, 7936, 7957, 7960, 7965, 7968, 8005, 8008, 8013, 8016, 8023, 8025, 8025, 8027, 8027,
        8029, 8029, 8031, 8061, 8064, 8116, 8118, 8124, 8126, 8126, 8130, 8132, 8134, 8140, 8144,
        8147, 8150, 8155, 8160, 8172, 8178, 8180, 8182, 8188, 8319, 8319, 8450, 8450, 8455, 8455,
        8458, 8467, 8469, 8469, 8473, 8477, 8484, 8484, 8486, 8486, 8488, 8488, 8490, 8493, 8495,
        8497, 8499, 8505, 8544, 8579, 12293, 12295, 12321, 12329, 12337, 12341, 12344, 12346,
        12353, 12436, 12445, 12446, 12449, 12538, 12540, 12542, 12549, 12588, 12593, 12686, 12704,
        12727, 13312, 19893, 19968, 40869, 40960, 42124, 44032, 55203, 63744, 64045, 64256, 64262,
        64275, 64279, 64285, 64285, 64287, 64296, 64298, 64310, 64312, 64316, 64318, 64318, 64320,
        64321, 64323, 64324, 64326, 64433, 64467, 64829, 64848, 64911, 64914, 64967, 65008, 65019,
        65136, 65138, 65140, 65140, 65142, 65276, 65313, 65338, 65345, 65370, 65382, 65470, 65474,
        65479, 65482, 65487, 65490, 65495, 65498, 65500
    ].into_iter());
    static ref UNICODE_ES3_IDENTIFIER_PART: UnicodeMap = UnicodeMap::from_char_codes(vec![
        170, 170, 181, 181, 186, 186, 192, 214, 216, 246, 248, 543, 546, 563, 592, 685, 688, 696,
        699, 705, 720, 721, 736, 740, 750, 750, 768, 846, 864, 866, 890, 890, 902, 902, 904, 906,
        908, 908, 910, 929, 931, 974, 976, 983, 986, 1011, 1024, 1153, 1155, 1158, 1164, 1220,
        1223, 1224, 1227, 1228, 1232, 1269, 1272, 1273, 1329, 1366, 1369, 1369, 1377, 1415, 1425,
        1441, 1443, 1465, 1467, 1469, 1471, 1471, 1473, 1474, 1476, 1476, 1488, 1514, 1520, 1522,
        1569, 1594, 1600, 1621, 1632, 1641, 1648, 1747, 1749, 1756, 1759, 1768, 1770, 1773, 1776,
        1788, 1808, 1836, 1840, 1866, 1920, 1968, 2305, 2307, 2309, 2361, 2364, 2381, 2384, 2388,
        2392, 2403, 2406, 2415, 2433, 2435, 2437, 2444, 2447, 2448, 2451, 2472, 2474, 2480, 2482,
        2482, 2486, 2489, 2492, 2492, 2494, 2500, 2503, 2504, 2507, 2509, 2519, 2519, 2524, 2525,
        2527, 2531, 2534, 2545, 2562, 2562, 2565, 2570, 2575, 2576, 2579, 2600, 2602, 2608, 2610,
        2611, 2613, 2614, 2616, 2617, 2620, 2620, 2622, 2626, 2631, 2632, 2635, 2637, 2649, 2652,
        2654, 2654, 2662, 2676, 2689, 2691, 2693, 2699, 2701, 2701, 2703, 2705, 2707, 2728, 2730,
        2736, 2738, 2739, 2741, 2745, 2748, 2757, 2759, 2761, 2763, 2765, 2768, 2768, 2784, 2784,
        2790, 2799, 2817, 2819, 2821, 2828, 2831, 2832, 2835, 2856, 2858, 2864, 2866, 2867, 2870,
        2873, 2876, 2883, 2887, 2888, 2891, 2893, 2902, 2903, 2908, 2909, 2911, 2913, 2918, 2927,
        2946, 2947, 2949, 2954, 2958, 2960, 2962, 2965, 2969, 2970, 2972, 2972, 2974, 2975, 2979,
        2980, 2984, 2986, 2990, 2997, 2999, 3001, 3006, 3010, 3014, 3016, 3018, 3021, 3031, 3031,
        3047, 3055, 3073, 3075, 3077, 3084, 3086, 3088, 3090, 3112, 3114, 3123, 3125, 3129, 3134,
        3140, 3142, 3144, 3146, 3149, 3157, 3158, 3168, 3169, 3174, 3183, 3202, 3203, 3205, 3212,
        3214, 3216, 3218, 3240, 3242, 3251, 3253, 3257, 3262, 3268, 3270, 3272, 3274, 3277, 3285,
        3286, 3294, 3294, 3296, 3297, 3302, 3311, 3330, 3331, 3333, 3340, 3342, 3344, 3346, 3368,
        3370, 3385, 3390, 3395, 3398, 3400, 3402, 3405, 3415, 3415, 3424, 3425, 3430, 3439, 3458,
        3459, 3461, 3478, 3482, 3505, 3507, 3515, 3517, 3517, 3520, 3526, 3530, 3530, 3535, 3540,
        3542, 3542, 3544, 3551, 3570, 3571, 3585, 3642, 3648, 3662, 3664, 3673, 3713, 3714, 3716,
        3716, 3719, 3720, 3722, 3722, 3725, 3725, 3732, 3735, 3737, 3743, 3745, 3747, 3749, 3749,
        3751, 3751, 3754, 3755, 3757, 3769, 3771, 3773, 3776, 3780, 3782, 3782, 3784, 3789, 3792,
        3801, 3804, 3805, 3840, 3840, 3864, 3865, 3872, 3881, 3893, 3893, 3895, 3895, 3897, 3897,
        3902, 3911, 3913, 3946, 3953, 3972, 3974, 3979, 3984, 3991, 3993, 4028, 4038, 4038, 4096,
        4129, 4131, 4135, 4137, 4138, 4140, 4146, 4150, 4153, 4160, 4169, 4176, 4185, 4256, 4293,
        4304, 4342, 4352, 4441, 4447, 4514, 4520, 4601, 4608, 4614, 4616, 4678, 4680, 4680, 4682,
        4685, 4688, 4694, 4696, 4696, 4698, 4701, 4704, 4742, 4744, 4744, 4746, 4749, 4752, 4782,
        4784, 4784, 4786, 4789, 4792, 4798, 4800, 4800, 4802, 4805, 4808, 4814, 4816, 4822, 4824,
        4846, 4848, 4878, 4880, 4880, 4882, 4885, 4888, 4894, 4896, 4934, 4936, 4954, 4969, 4977,
        5024, 5108, 5121, 5740, 5743, 5750, 5761, 5786, 5792, 5866, 6016, 6099, 6112, 6121, 6160,
        6169, 6176, 6263, 6272, 6313, 7680, 7835, 7840, 7929, 7936, 7957, 7960, 7965, 7968, 8005,
        8008, 8013, 8016, 8023, 8025, 8025, 8027, 8027, 8029, 8029, 8031, 8061, 8064, 8116, 8118,
        8124, 8126, 8126, 8130, 8132, 8134, 8140, 8144, 8147, 8150, 8155, 8160, 8172, 8178, 8180,
        8182, 8188, 8255, 8256, 8319, 8319, 8400, 8412, 8417, 8417, 8450, 8450, 8455, 8455, 8458,
        8467, 8469, 8469, 8473, 8477, 8484, 8484, 8486, 8486, 8488, 8488, 8490, 8493, 8495, 8497,
        8499, 8505, 8544, 8579, 12293, 12295, 12321, 12335, 12337, 12341, 12344, 12346, 12353,
        12436, 12441, 12442, 12445, 12446, 12449, 12542, 12549, 12588, 12593, 12686, 12704, 12727,
        13312, 19893, 19968, 40869, 40960, 42124, 44032, 55203, 63744, 64045, 64256, 64262, 64275,
        64279, 64285, 64296, 64298, 64310, 64312, 64316, 64318, 64318, 64320, 64321, 64323, 64324,
        64326, 64433, 64467, 64829, 64848, 64911, 64914, 64967, 65008, 65019, 65056, 65059, 65075,
        65076, 65101, 65103, 65136, 65138, 65140, 65140, 65142, 65276, 65296, 65305, 65313, 65338,
        65343, 65343, 65345, 65370, 65381, 65470, 65474, 65479, 65482, 65487, 65490, 65495, 65498,
        65500
    ].into_iter());

    /*
        As per ECMAScript Language Specification 5th Edition, Section 7.6: ISyntaxToken Names and Identifiers
        IdentifierStart ::
            Can contain Unicode 6.2 categories:
            Uppercase letter (Lu),
            Lowercase letter (Ll),
            Titlecase letter (Lt),
            Modifier letter (Lm),
            Other letter (Lo), or
            Letter number (Nl).
        IdentifierPart ::
            Can contain IdentifierStart + Unicode 6.2 categories:
            Non-spacing mark (Mn),
            Combining spacing mark (Mc),
            Decimal number (Nd),
            Connector punctuation (Pc),
            <ZWNJ>, or
            <ZWJ>.

        Codepoint ranges for ES5 Identifiers are extracted from the Unicode 6.2 specification at:
        http://www.unicode.org/Public/6.2.0/ucd/UnicodeData.txt
    */
    static ref UNICODE_ES5_IDENTIFIER_START: UnicodeMap = UnicodeMap::from_char_codes(vec![170, 170, 181, 181, 186, 186, 192, 214, 216, 246, 248, 705, 710, 721, 736, 740, 748, 748, 750, 750, 880, 884, 886, 887, 890, 893, 902, 902, 904, 906, 908, 908, 910, 929, 931, 1013, 1015, 1153, 1162, 1319, 1329, 1366, 1369, 1369, 1377, 1415, 1488, 1514, 1520, 1522, 1568, 1610, 1646, 1647, 1649, 1747, 1749, 1749, 1765, 1766, 1774, 1775, 1786, 1788, 1791, 1791, 1808, 1808, 1810, 1839, 1869, 1957, 1969, 1969, 1994, 2026, 2036, 2037, 2042, 2042, 2048, 2069, 2074, 2074, 2084, 2084, 2088, 2088, 2112, 2136, 2208, 2208, 2210, 2220, 2308, 2361, 2365, 2365, 2384, 2384, 2392, 2401, 2417, 2423, 2425, 2431, 2437, 2444, 2447, 2448, 2451, 2472, 2474, 2480, 2482, 2482, 2486, 2489, 2493, 2493, 2510, 2510, 2524, 2525, 2527, 2529, 2544, 2545, 2565, 2570, 2575, 2576, 2579, 2600, 2602, 2608, 2610, 2611, 2613, 2614, 2616, 2617, 2649, 2652, 2654, 2654, 2674, 2676, 2693, 2701, 2703, 2705, 2707, 2728, 2730, 2736, 2738, 2739, 2741, 2745, 2749, 2749, 2768, 2768, 2784, 2785, 2821, 2828, 2831, 2832, 2835, 2856, 2858, 2864, 2866, 2867, 2869, 2873, 2877, 2877, 2908, 2909, 2911, 2913, 2929, 2929, 2947, 2947, 2949, 2954, 2958, 2960, 2962, 2965, 2969, 2970, 2972, 2972, 2974, 2975, 2979, 2980, 2984, 2986, 2990, 3001, 3024, 3024, 3077, 3084, 3086, 3088, 3090, 3112, 3114, 3123, 3125, 3129, 3133, 3133, 3160, 3161, 3168, 3169, 3205, 3212, 3214, 3216, 3218, 3240, 3242, 3251, 3253, 3257, 3261, 3261, 3294, 3294, 3296, 3297, 3313, 3314, 3333, 3340, 3342, 3344, 3346, 3386, 3389, 3389, 3406, 3406, 3424, 3425, 3450, 3455, 3461, 3478, 3482, 3505, 3507, 3515, 3517, 3517, 3520, 3526, 3585, 3632, 3634, 3635, 3648, 3654, 3713, 3714, 3716, 3716, 3719, 3720, 3722, 3722, 3725, 3725, 3732, 3735, 3737, 3743, 3745, 3747, 3749, 3749, 3751, 3751, 3754, 3755, 3757, 3760, 3762, 3763, 3773, 3773, 3776, 3780, 3782, 3782, 3804, 3807, 3840, 3840, 3904, 3911, 3913, 3948, 3976, 3980, 4096, 4138, 4159, 4159, 4176, 4181, 4186, 4189, 4193, 4193, 4197, 4198, 4206, 4208, 4213, 4225, 4238, 4238, 4256, 4293, 4295, 4295, 4301, 4301, 4304, 4346, 4348, 4680, 4682, 4685, 4688, 4694, 4696, 4696, 4698, 4701, 4704, 4744, 4746, 4749, 4752, 4784, 4786, 4789, 4792, 4798, 4800, 4800, 4802, 4805, 4808, 4822, 4824, 4880, 4882, 4885, 4888, 4954, 4992, 5007, 5024, 5108, 5121, 5740, 5743, 5759, 5761, 5786, 5792, 5866, 5870, 5872, 5888, 5900, 5902, 5905, 5920, 5937, 5952, 5969, 5984, 5996, 5998, 6000, 6016, 6067, 6103, 6103, 6108, 6108, 6176, 6263, 6272, 6312, 6314, 6314, 6320, 6389, 6400, 6428, 6480, 6509, 6512, 6516, 6528, 6571, 6593, 6599, 6656, 6678, 6688, 6740, 6823, 6823, 6917, 6963, 6981, 6987, 7043, 7072, 7086, 7087, 7098, 7141, 7168, 7203, 7245, 7247, 7258, 7293, 7401, 7404, 7406, 7409, 7413, 7414, 7424, 7615, 7680, 7957, 7960, 7965, 7968, 8005, 8008, 8013, 8016, 8023, 8025, 8025, 8027, 8027, 8029, 8029, 8031, 8061, 8064, 8116, 8118, 8124, 8126, 8126, 8130, 8132, 8134, 8140, 8144, 8147, 8150, 8155, 8160, 8172, 8178, 8180, 8182, 8188, 8305, 8305, 8319, 8319, 8336, 8348, 8450, 8450, 8455, 8455, 8458, 8467, 8469, 8469, 8473, 8477, 8484, 8484, 8486, 8486, 8488, 8488, 8490, 8493, 8495, 8505, 8508, 8511, 8517, 8521, 8526, 8526, 8544, 8584, 11264, 11310, 11312, 11358, 11360, 11492, 11499, 11502, 11506, 11507, 11520, 11557, 11559, 11559, 11565, 11565, 11568, 11623, 11631, 11631, 11648, 11670, 11680, 11686, 11688, 11694, 11696, 11702, 11704, 11710, 11712, 11718, 11720, 11726, 11728, 11734, 11736, 11742, 11823, 11823, 12293, 12295, 12321, 12329, 12337, 12341, 12344, 12348, 12353, 12438, 12445, 12447, 12449, 12538, 12540, 12543, 12549, 12589, 12593, 12686, 12704, 12730, 12784, 12799, 13312, 19893, 19968, 40908, 40960, 42124, 42192, 42237, 42240, 42508, 42512, 42527, 42538, 42539, 42560, 42606, 42623, 42647, 42656, 42735, 42775, 42783, 42786, 42888, 42891, 42894, 42896, 42899, 42912, 42922, 43000, 43009, 43011, 43013, 43015, 43018, 43020, 43042, 43072, 43123, 43138, 43187, 43250, 43255, 43259, 43259, 43274, 43301, 43312, 43334, 43360, 43388, 43396, 43442, 43471, 43471, 43520, 43560, 43584, 43586, 43588, 43595, 43616, 43638, 43642, 43642, 43648, 43695, 43697, 43697, 43701, 43702, 43705, 43709, 43712, 43712, 43714, 43714, 43739, 43741, 43744, 43754, 43762, 43764, 43777, 43782, 43785, 43790, 43793, 43798, 43808, 43814, 43816, 43822, 43968, 44002, 44032, 55203, 55216, 55238, 55243, 55291, 63744, 64109, 64112, 64217, 64256, 64262, 64275, 64279, 64285, 64285, 64287, 64296, 64298, 64310, 64312, 64316, 64318, 64318, 64320, 64321, 64323, 64324, 64326, 64433, 64467, 64829, 64848, 64911, 64914, 64967, 65008, 65019, 65136, 65140, 65142, 65276, 65313, 65338, 65345, 65370, 65382, 65470, 65474, 65479, 65482, 65487, 65490, 65495, 65498, 65500].into_iter());
    static ref UNICODE_ES5_IDENTIFIER_PART: UnicodeMap = UnicodeMap::from_char_codes(vec![170, 170, 181, 181, 186, 186, 192, 214, 216, 246, 248, 705, 710, 721, 736, 740, 748, 748, 750, 750, 768, 884, 886, 887, 890, 893, 902, 902, 904, 906, 908, 908, 910, 929, 931, 1013, 1015, 1153, 1155, 1159, 1162, 1319, 1329, 1366, 1369, 1369, 1377, 1415, 1425, 1469, 1471, 1471, 1473, 1474, 1476, 1477, 1479, 1479, 1488, 1514, 1520, 1522, 1552, 1562, 1568, 1641, 1646, 1747, 1749, 1756, 1759, 1768, 1770, 1788, 1791, 1791, 1808, 1866, 1869, 1969, 1984, 2037, 2042, 2042, 2048, 2093, 2112, 2139, 2208, 2208, 2210, 2220, 2276, 2302, 2304, 2403, 2406, 2415, 2417, 2423, 2425, 2431, 2433, 2435, 2437, 2444, 2447, 2448, 2451, 2472, 2474, 2480, 2482, 2482, 2486, 2489, 2492, 2500, 2503, 2504, 2507, 2510, 2519, 2519, 2524, 2525, 2527, 2531, 2534, 2545, 2561, 2563, 2565, 2570, 2575, 2576, 2579, 2600, 2602, 2608, 2610, 2611, 2613, 2614, 2616, 2617, 2620, 2620, 2622, 2626, 2631, 2632, 2635, 2637, 2641, 2641, 2649, 2652, 2654, 2654, 2662, 2677, 2689, 2691, 2693, 2701, 2703, 2705, 2707, 2728, 2730, 2736, 2738, 2739, 2741, 2745, 2748, 2757, 2759, 2761, 2763, 2765, 2768, 2768, 2784, 2787, 2790, 2799, 2817, 2819, 2821, 2828, 2831, 2832, 2835, 2856, 2858, 2864, 2866, 2867, 2869, 2873, 2876, 2884, 2887, 2888, 2891, 2893, 2902, 2903, 2908, 2909, 2911, 2915, 2918, 2927, 2929, 2929, 2946, 2947, 2949, 2954, 2958, 2960, 2962, 2965, 2969, 2970, 2972, 2972, 2974, 2975, 2979, 2980, 2984, 2986, 2990, 3001, 3006, 3010, 3014, 3016, 3018, 3021, 3024, 3024, 3031, 3031, 3046, 3055, 3073, 3075, 3077, 3084, 3086, 3088, 3090, 3112, 3114, 3123, 3125, 3129, 3133, 3140, 3142, 3144, 3146, 3149, 3157, 3158, 3160, 3161, 3168, 3171, 3174, 3183, 3202, 3203, 3205, 3212, 3214, 3216, 3218, 3240, 3242, 3251, 3253, 3257, 3260, 3268, 3270, 3272, 3274, 3277, 3285, 3286, 3294, 3294, 3296, 3299, 3302, 3311, 3313, 3314, 3330, 3331, 3333, 3340, 3342, 3344, 3346, 3386, 3389, 3396, 3398, 3400, 3402, 3406, 3415, 3415, 3424, 3427, 3430, 3439, 3450, 3455, 3458, 3459, 3461, 3478, 3482, 3505, 3507, 3515, 3517, 3517, 3520, 3526, 3530, 3530, 3535, 3540, 3542, 3542, 3544, 3551, 3570, 3571, 3585, 3642, 3648, 3662, 3664, 3673, 3713, 3714, 3716, 3716, 3719, 3720, 3722, 3722, 3725, 3725, 3732, 3735, 3737, 3743, 3745, 3747, 3749, 3749, 3751, 3751, 3754, 3755, 3757, 3769, 3771, 3773, 3776, 3780, 3782, 3782, 3784, 3789, 3792, 3801, 3804, 3807, 3840, 3840, 3864, 3865, 3872, 3881, 3893, 3893, 3895, 3895, 3897, 3897, 3902, 3911, 3913, 3948, 3953, 3972, 3974, 3991, 3993, 4028, 4038, 4038, 4096, 4169, 4176, 4253, 4256, 4293, 4295, 4295, 4301, 4301, 4304, 4346, 4348, 4680, 4682, 4685, 4688, 4694, 4696, 4696, 4698, 4701, 4704, 4744, 4746, 4749, 4752, 4784, 4786, 4789, 4792, 4798, 4800, 4800, 4802, 4805, 4808, 4822, 4824, 4880, 4882, 4885, 4888, 4954, 4957, 4959, 4992, 5007, 5024, 5108, 5121, 5740, 5743, 5759, 5761, 5786, 5792, 5866, 5870, 5872, 5888, 5900, 5902, 5908, 5920, 5940, 5952, 5971, 5984, 5996, 5998, 6000, 6002, 6003, 6016, 6099, 6103, 6103, 6108, 6109, 6112, 6121, 6155, 6157, 6160, 6169, 6176, 6263, 6272, 6314, 6320, 6389, 6400, 6428, 6432, 6443, 6448, 6459, 6470, 6509, 6512, 6516, 6528, 6571, 6576, 6601, 6608, 6617, 6656, 6683, 6688, 6750, 6752, 6780, 6783, 6793, 6800, 6809, 6823, 6823, 6912, 6987, 6992, 7001, 7019, 7027, 7040, 7155, 7168, 7223, 7232, 7241, 7245, 7293, 7376, 7378, 7380, 7414, 7424, 7654, 7676, 7957, 7960, 7965, 7968, 8005, 8008, 8013, 8016, 8023, 8025, 8025, 8027, 8027, 8029, 8029, 8031, 8061, 8064, 8116, 8118, 8124, 8126, 8126, 8130, 8132, 8134, 8140, 8144, 8147, 8150, 8155, 8160, 8172, 8178, 8180, 8182, 8188, 8204, 8205, 8255, 8256, 8276, 8276, 8305, 8305, 8319, 8319, 8336, 8348, 8400, 8412, 8417, 8417, 8421, 8432, 8450, 8450, 8455, 8455, 8458, 8467, 8469, 8469, 8473, 8477, 8484, 8484, 8486, 8486, 8488, 8488, 8490, 8493, 8495, 8505, 8508, 8511, 8517, 8521, 8526, 8526, 8544, 8584, 11264, 11310, 11312, 11358, 11360, 11492, 11499, 11507, 11520, 11557, 11559, 11559, 11565, 11565, 11568, 11623, 11631, 11631, 11647, 11670, 11680, 11686, 11688, 11694, 11696, 11702, 11704, 11710, 11712, 11718, 11720, 11726, 11728, 11734, 11736, 11742, 11744, 11775, 11823, 11823, 12293, 12295, 12321, 12335, 12337, 12341, 12344, 12348, 12353, 12438, 12441, 12442, 12445, 12447, 12449, 12538, 12540, 12543, 12549, 12589, 12593, 12686, 12704, 12730, 12784, 12799, 13312, 19893, 19968, 40908, 40960, 42124, 42192, 42237, 42240, 42508, 42512, 42539, 42560, 42607, 42612, 42621, 42623, 42647, 42655, 42737, 42775, 42783, 42786, 42888, 42891, 42894, 42896, 42899, 42912, 42922, 43000, 43047, 43072, 43123, 43136, 43204, 43216, 43225, 43232, 43255, 43259, 43259, 43264, 43309, 43312, 43347, 43360, 43388, 43392, 43456, 43471, 43481, 43520, 43574, 43584, 43597, 43600, 43609, 43616, 43638, 43642, 43643, 43648, 43714, 43739, 43741, 43744, 43759, 43762, 43766, 43777, 43782, 43785, 43790, 43793, 43798, 43808, 43814, 43816, 43822, 43968, 44010, 44012, 44013, 44016, 44025, 44032, 55203, 55216, 55238, 55243, 55291, 63744, 64109, 64112, 64217, 64256, 64262, 64275, 64279, 64285, 64296, 64298, 64310, 64312, 64316, 64318, 64318, 64320, 64321, 64323, 64324, 64326, 64433, 64467, 64829, 64848, 64911, 64914, 64967, 65008, 65019, 65024, 65039, 65056, 65062, 65075, 65076, 65101, 65103, 65136, 65140, 65142, 65276, 65296, 65305, 65313, 65338, 65343, 65343, 65345, 65370, 65382, 65470, 65474, 65479, 65482, 65487, 65490, 65495, 65498, 65500].into_iter());
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
    #[wasm_bindgen(js_name = isWhiteSpaceSingleLine)]
    pub fn is_white_space_single_line(ch: char) -> bool {
        // Note: NextLine is in the Zs space, and should be considered to be a whitespace.
        // It is explicitly not a line-break as it isn't in the exact set specified by EcmaScript.
        super::is_white_space_single_line(ch)
    }

    #[wasm_bindgen(js_name = couldStartTrivia)]
    pub fn could_start_trivia(text: &str, pos: usize) -> bool {
        super::could_start_trivia(text, pos)
    }

    #[wasm_bindgen(js_name = isWhiteSpaceLike)]
    pub fn is_white_space_like(c: char) -> bool {
        super::is_white_space_like(c)
    }

    #[wasm_bindgen(js_name = isLineBreak)]
    pub fn is_line_break(ch: char) -> bool {
        super::is_line_break(ch)
    }
}
