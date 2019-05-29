use super::*;
use proptest::{bool, proptest};
use std::rc::Rc;
use std::sync::Mutex;

proptest! {
    /// Check for panics on random input.
    #[test]
    fn never_panic_on_random_input(
        language_version: ScriptTarget,
        skip_trivia: bool,
        language_variant: LanguageVariant,
        text in ".*"
    ) {
        let text_len = text.len();
        let mut scanner = Scanner::new(
            language_version,
            skip_trivia,
            language_variant,
            Some(text),
            None,
            None,
            None
        );
        for i in 0.. {
            if scanner.scan() == SyntaxKind::EndOfFileToken {
                break;
            }
            assert!(i < text_len, "expected less iterations than the text is long");
        }
    }
}

#[test]
fn char_boundary() {
    let mut scanner = Scanner::new(
        ScriptTarget::ES3,
        false,
        LanguageVariant::Standard,
        Some("<A".to_string()),
        None,
        None,
        None,
    );
    assert_eq!(scanner.scan(), SyntaxKind::Token(Token::LessThan));
    assert_eq!(scanner.scan(), SyntaxKind::Identifier);
    assert_eq!(scanner.token_value().unwrap(), "A");
    assert_eq!(scanner.scan(), SyntaxKind::EndOfFileToken);
}

#[test]
fn string() {
    let mut scanner = Scanner::new(
        ScriptTarget::ES3,
        false,
        LanguageVariant::Standard,
        Some("const i = \"some string\";".to_string()),
        Some(Box::new(|c, _length| panic!("Unexpected error: {}", c))),
        None,
        None,
    );
    assert_eq!(
        scanner.scan(),
        SyntaxKind::Keyword(syntax_kind::Keyword::Const)
    );
    assert_eq!(scanner.scan(), SyntaxKind::WhitespaceTrivia);
    assert_eq!(scanner.scan(), SyntaxKind::Identifier);
    assert_eq!(scanner.token_value().unwrap(), "i");
    assert_eq!(scanner.scan(), SyntaxKind::WhitespaceTrivia);
    assert_eq!(scanner.scan(), SyntaxKind::Token(Token::Equals));
    assert_eq!(scanner.scan(), SyntaxKind::WhitespaceTrivia);
    assert_eq!(scanner.scan(), SyntaxKind::StringLiteral);
    assert_eq!(scanner.token_value().unwrap(), "some string");
    assert_eq!(scanner.scan(), SyntaxKind::Token(Token::Semicolon));
    assert_eq!(scanner.scan(), SyntaxKind::EndOfFileToken);
}

#[test]
fn string_with_escape() {
    let mut scanner = Scanner::new(
        ScriptTarget::ES3,
        false,
        LanguageVariant::Standard,
        Some("let i = \"\\u{0}\";".to_string()),
        Some(Box::new(|c, _length| panic!("Unexpected error: {}", c))),
        None,
        None,
    );
    assert_eq!(
        scanner.scan(),
        SyntaxKind::Keyword(syntax_kind::Keyword::Let)
    );
    assert_eq!(scanner.scan(), SyntaxKind::WhitespaceTrivia);
    assert_eq!(scanner.scan(), SyntaxKind::Identifier);
    assert_eq!(scanner.token_value().unwrap(), "i");
    assert_eq!(scanner.scan(), SyntaxKind::WhitespaceTrivia);
    assert_eq!(scanner.scan(), SyntaxKind::Token(Token::Equals));
    assert_eq!(scanner.scan(), SyntaxKind::WhitespaceTrivia);
    assert_eq!(scanner.scan(), SyntaxKind::StringLiteral);
    assert_eq!(scanner.token_value().unwrap(), "\0");
    assert_eq!(scanner.scan(), SyntaxKind::Token(Token::Semicolon));
    assert_eq!(scanner.scan(), SyntaxKind::EndOfFileToken);
}

/// Tests an issue found by proptest where `scan_number_fragment` would panic.
#[test]
fn scan_number_fragment_panic() {
    let mut scanner = Scanner::new(
        ScriptTarget::ES3,
        false,
        LanguageVariant::Standard,
        Some(r#"¡¡��¡¡A\u{0}\u{0}0A"#.to_string()),
        None,
        None,
        None,
    );
    loop {
        if scanner.scan() == SyntaxKind::EndOfFileToken {
            break;
        }
    }
}

#[test]
fn scan_identifier_parts_panic() {
    let mut scanner = Scanner::new(
        ScriptTarget::ES3,
        false,
        LanguageVariant::Standard,
        Some(r#"¡��� ¡A¡\u{0}¡¡A\""#.to_string()),
        None,
        None,
        None,
    );
    loop {
        if scanner.scan() == SyntaxKind::EndOfFileToken {
            break;
        }
    }
}

#[test]
fn scan_template_and_set_token_value_panic() {
    use std::rc::Rc;
    use std::sync::Mutex;
    let errors = Rc::new(Mutex::new(Vec::new()));

    let errors_for_add_error = errors.clone();
    let add_error = Box::new(move |err, pos| errors_for_add_error.lock().unwrap().push((err, pos)));

    let mut scanner = Scanner::new(
        ScriptTarget::ES3,
        false,
        LanguageVariant::Standard,
        Some(r#"`¡\\"#.to_string()),
        Some(add_error),
        None,
        None,
    );
    assert_eq!(scanner.scan(), SyntaxKind::NoSubstitutionTemplateLiteral);
    assert_eq!(scanner.scan(), SyntaxKind::EndOfFileToken);
    assert_eq!(
        &*errors.lock().unwrap(),
        &[(diagnostic::Message::UnterminatedTemplateLiteral, 0)]
    );
}

#[test]
fn scan_string_panic() {
    let errors = Rc::new(Mutex::new(Vec::new()));
    let errors_for_add_error = errors.clone();
    let add_error = Box::new(move |err, pos| errors_for_add_error.lock().unwrap().push((err, pos)));

    let mut scanner = Scanner::new(
        ScriptTarget::ES3,
        false,
        LanguageVariant::Standard,
        Some(r#"¡¡\'\'"#.to_string()),
        Some(add_error),
        None,
        None,
    );
    assert_eq!(scanner.scan(), SyntaxKind::Unknown);
    assert_eq!(scanner.scan(), SyntaxKind::Unknown);
    assert_eq!(scanner.scan(), SyntaxKind::Unknown);
    assert_eq!(scanner.scan(), SyntaxKind::StringLiteral);
    assert_eq!(scanner.scan(), SyntaxKind::EndOfFileToken);
    assert_eq!(
        &*errors.lock().unwrap(),
        &[
            (diagnostic::Message::InvalidCharacter, 0),
            (diagnostic::Message::InvalidCharacter, 0),
            (diagnostic::Message::InvalidCharacter, 0),
            (diagnostic::Message::UnterminatedStringLiteral, 0)
        ]
    );
}

#[test]
fn scan_number() {
    let errors = Rc::new(Mutex::new(Vec::new()));
    let errors_for_add_error = errors.clone();
    let add_error = Box::new(move |err, pos| errors_for_add_error.lock().unwrap().push((err, pos)));
    let mut scanner = Scanner::new(
        ScriptTarget::ES3,
        false,
        LanguageVariant::Standard,
        Some("0.".to_string()),
        Some(add_error),
        None,
        None,
    );
    assert_eq!(scanner.scan(), SyntaxKind::NumericLiteral);
    assert_eq!(scanner.scan(), SyntaxKind::EndOfFileToken);
    assert_eq!(&*errors.lock().unwrap(), &[]);
}

#[test]
fn proptest_panic() {
    let errors = Rc::new(Mutex::new(Vec::new()));
    let errors_for_add_error = errors.clone();
    let add_error = Box::new(move |err, pos| errors_for_add_error.lock().unwrap().push((err, pos)));
    let mut scanner = Scanner::new(
        ScriptTarget::ES3,
        false,
        LanguageVariant::Standard,
        Some("0b:".to_string()),
        Some(add_error),
        None,
        None,
    );
    assert_eq!(scanner.scan(), SyntaxKind::NumericLiteral);
    assert_eq!(scanner.scan(), SyntaxKind::Token(Token::Colon));
    assert_eq!(scanner.scan(), SyntaxKind::EndOfFileToken);
    assert_eq!(
        &*errors.lock().unwrap(),
        &[(diagnostic::Message::BinaryDigitExpected, 0)]
    );
}
