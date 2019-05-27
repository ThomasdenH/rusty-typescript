use crate::compiler::types::SyntaxKind;
use lazy_static::*;
use regex::Regex;

pub fn is_pinned_comment(text: &str, start: usize) -> bool {
    text.chars().nth(start + 1) == Some('*') && text.chars().nth(start + 2) == Some('!')
}

#[derive(Eq, PartialEq, Ord, PartialOrd, Copy, Clone)]
pub struct Precedence(i8);

#[derive(num_derive::FromPrimitive, num_derive::ToPrimitive, Eq, PartialEq, Copy, Clone)]
pub enum Associativity {
    Left = 0,
    Right = 1,
}

pub fn get_operator_associativity(
    kind: SyntaxKind,
    operator: SyntaxKind,
    has_arguments: bool,
) -> Associativity {
    use crate::compiler::types::syntax_kind::Token::*;
    use SyntaxKind::*;
    match kind {
        NewExpression => {
            if has_arguments {
                Associativity::Left
            } else {
                Associativity::Right
            }
        }
        PrefixUnaryExpression
        | TypeOfExpression
        | VoidExpression
        | DeleteExpression
        | AwaitExpression
        | ConditionalExpression
        | YieldExpression => Associativity::Right,
        BinaryExpression => match operator {
            Token(AsteriskAsterisk)
            | Token(Equals)
            | Token(PlusEquals)
            | Token(MinusEquals)
            | Token(AsteriskAsteriskEquals)
            | Token(AsteriskEquals)
            | Token(SlashEquals)
            | Token(PercentEquals)
            | Token(LessThanLessThanEquals)
            | Token(GreaterThanGreaterThanEquals)
            | Token(GreaterThanGreaterThanGreaterThanEquals)
            | Token(AmpersandEquals)
            | Token(CaretEquals)
            | Token(BarEquals) => Associativity::Right,
            _ => Associativity::Left,
        },
        _ => Associativity::Left,
    }
}

pub fn get_binary_operator_precedence(kind: SyntaxKind) -> Precedence {
    use crate::compiler::types::syntax_kind::{Keyword::*, Token::*};
    use SyntaxKind::*;
    Precedence(match kind {
        Token(BarBar) => 5,
        Token(AmpersandAmpersand) => 6,
        Token(Bar) => 7,
        Token(Caret) => 8,
        Token(Ampersand) => 9,
        Token(EqualsEquals)
        | Token(ExclamationEquals)
        | Token(EqualsEqualsEquals)
        | Token(ExclamationEqualsEquals) => 10,
        Token(LessThan)
        | Token(GreaterThan)
        | Token(LessThanEquals)
        | Token(GreaterThanEquals)
        | Keyword(InstanceOf)
        | Keyword(In)
        | Keyword(As) => 11,
        Token(LessThanLessThan)
        | Token(GreaterThanGreaterThan)
        | Token(GreaterThanGreaterThanGreaterThan) => 12,
        Token(Plus) | Token(Minus) => 13,
        Token(Asterisk) | Token(Slash) | Token(Percent) => 14,
        Token(AsteriskAsterisk) => 15,
        // -1 is lower than all other precedences.  Returning it will cause binary expression
        // parsing to stop.
        _ => -1,
    })
}

pub fn get_operator_precedence(
    node_kind: SyntaxKind,
    operator_kind: SyntaxKind,
    has_arguments: bool,
) -> Precedence {
    use crate::compiler::types::syntax_kind::Keyword::*;
    use crate::compiler::types::syntax_kind::Token::*;
    use SyntaxKind::*;
    Precedence(match node_kind {
        CommaListExpression => 0,
        SpreadElement => 1,
        YieldExpression => 2,
        ConditionalExpression => 4,
        BinaryExpression => match operator_kind {
            Token(Comma) => 0,
            Token(Equals)
            | Token(PlusEquals)
            | Token(MinusEquals)
            | Token(AsteriskAsteriskEquals)
            | Token(SlashEquals)
            | Token(PercentEquals)
            | Token(LessThanLessThanEquals)
            | Token(GreaterThanGreaterThanEquals)
            | Token(GreaterThanGreaterThanGreaterThanEquals)
            | Token(AmpersandEquals)
            | Token(CaretEquals)
            | Token(BarEquals) => 3,
            _ => get_binary_operator_precedence(operator_kind).0,
        },
        PrefixUnaryExpression
        | TypeOfExpression
        | VoidExpression
        | DeleteExpression
        | AwaitExpression => 16,
        PostfixUnaryExpression => 17,
        CallExpression => 18,
        NewExpression => {
            if has_arguments {
                19
            } else {
                18
            }
        }
        TaggedTemplateExpression | PropertyAccessExpression | ElementAccessExpression => 19,
        Keyword(This)
        | Keyword(Super)
        | Identifier
        | Keyword(Null)
        | Keyword(True)
        | Keyword(False)
        | NumericLiteral
        | BigIntLiteral
        | StringLiteral
        | ArrayLiteralExpression
        | ObjectLiteralExpression
        | FunctionExpression
        | ArrowFunction
        | ClassExpression
        | JsxElement
        | JsxSelfClosingElement
        | JsxFragment
        | RegularExpressionLiteral
        | NoSubstitutionTemplateLiteral
        | TemplateExpression
        | ParenthesizedExpression
        | OmittedExpression => 20,
        _ => -1,
    })
}

pub fn get_property_name_for_known_symbol_name(symbol_name: &str) -> String {
    "__@".to_string() + symbol_name
}

pub fn has_zero_or_one_asterisk_character(s: &str) -> bool {
    let mut has_seen_asterisk = false;
    for ch in s.chars() {
        if ch == '*' {
            if has_seen_asterisk {
                return false;
            } else {
                has_seen_asterisk = true;
            }
        }
    }
    true
}

pub fn normalize_slashes(path: String) -> String {
    path.replace("\\", "/")
}

pub fn path_is_relative(path: &str) -> bool {
    lazy_static! {
        static ref RE: Regex = Regex::new("^..?($|[\\/])").unwrap();
    }
    RE.is_match(path)
}

#[cfg(feature = "wasm")]
mod wasm {
    use wasm_bindgen::prelude::*;

    #[wasm_bindgen(js_name = isPinnedComment)]
    pub fn is_pinned_comment(text: &str, start: usize) -> bool {
        super::is_pinned_comment(text, start)
    }

    #[wasm_bindgen(js_name=getOperatorAssociativity)]
    pub fn get_operator_associativity(kind: u32, operator: u32, has_arguments: Option<bool>) -> u8 {
        let kind: SyntaxKind = FromPrimitive::from_u32(kind).unwrap();
        let operator: SyntaxKind = FromPrimitive::from_u32(operator).unwrap();
        let has_arguments = has_arguments.unwrap_or(false);
        super::get_operator_associativity(kind, operator, has_arguments)
            .to_u8()
            .unwrap()
    }

    #[wasm_bindgen(js_name = normalizeSlashes)]
    pub fn normalize_slashes(path: String) -> String {
        super::normalize_slashes(path)
    }

    #[wasm_bindgen(js_name = getBinaryOperatorPrecedence)]
    pub fn get_binary_operator_precedence(kind: u32) -> i8 {
        super::get_binary_operator_precedence(FromPrimitive::from_u32(kind).unwrap()).0
    }

    #[wasm_bindgen(js_name = getOperatorPrecedence)]
    pub fn get_operator_precedence(
        node_kind: u32,
        operator_kind: u32,
        has_arguments: Option<bool>,
    ) -> i8 {
        super::get_operator_precedence(
            FromPrimitive::from_u32(node_kind).unwrap(),
            FromPrimitive::from_u32(operator_kind).unwrap(),
            has_arguments.unwrap_or(false),
        )
        .0
    }

    #[wasm_bindgen(js_name=getPropertyNameForKnownSymbolName)]
    pub fn get_property_name_for_known_symbol_name(symbol_name: &str) -> String {
        super::get_property_name_for_known_symbol_name(symbol_name)
    }

    #[wasm_bindgen(js_name=hasZeroOrOneAsteriskCharacter)]
    pub fn has_zero_or_one_asterisk_character(s: &str) -> bool {
        super::has_zero_or_one_asterisk_character(s)
    }

    #[wasm_bindgen(js_name = pathIsRelative)]
    pub fn path_is_relative(path: &str) -> bool {
        super::path_is_relative(path)
    }
}
