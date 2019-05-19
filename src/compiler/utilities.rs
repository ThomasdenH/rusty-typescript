use crate::types::SyntaxKind;
use num_traits::{FromPrimitive, ToPrimitive};
use wasm_bindgen::prelude::*;

#[wasm_bindgen(js_name = isPinnedComment)]
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
            AsteriskAsteriskToken
            | EqualsToken
            | PlusEqualsToken
            | MinusEqualsToken
            | AsteriskAsteriskEqualsToken
            | AsteriskEqualsToken
            | SlashEqualsToken
            | PercentEqualsToken
            | LessThanLessThanEqualsToken
            | GreaterThanGreaterThanEqualsToken
            | GreaterThanGreaterThanGreaterThanEqualsToken
            | AmpersandEqualsToken
            | CaretEqualsToken
            | BarEqualsToken => Associativity::Right,
            _ => Associativity::Left,
        },
        _ => Associativity::Left,
    }
}

#[wasm_bindgen(js_name=getOperatorAssociativity)]
pub fn get_operator_associativity_js(kind: u32, operator: u32, has_arguments: Option<bool>) -> u8 {
    let kind: SyntaxKind = FromPrimitive::from_u32(kind).unwrap();
    let operator: SyntaxKind = FromPrimitive::from_u32(operator).unwrap();
    let has_arguments = has_arguments.unwrap_or(false);
    get_operator_associativity(kind, operator, has_arguments)
        .to_u8()
        .unwrap()
}

pub fn get_binary_operator_precedence(kind: SyntaxKind) -> Precedence {
    use SyntaxKind::*;
    Precedence(match kind {
        BarBarToken => 5,
        AmpersandAmpersandToken => 6,
        BarToken => 7,
        CaretToken => 8,
        AmpersandToken => 9,
        EqualsEqualsToken
        | ExclamationEqualsToken
        | EqualsEqualsEqualsToken
        | ExclamationEqualsEqualsToken => 10,
        LessThanToken
        | GreaterThanToken
        | LessThanEqualsToken
        | GreaterThanEqualsToken
        | InstanceOfKeyword
        | InKeyword
        | AsKeyword => 11,
        LessThanLessThanToken
        | GreaterThanGreaterThanToken
        | GreaterThanGreaterThanGreaterThanToken => 12,
        PlusToken | MinusToken => 13,
        AsteriskToken | SlashToken | PercentToken => 14,
        AsteriskAsteriskToken => 15,
        // -1 is lower than all other precedences.  Returning it will cause binary expression
        // parsing to stop.
        _ => -1,
    })
}

#[wasm_bindgen(js_name = getBinaryOperatorPrecedence)]
pub fn get_binary_operator_precedence_js(kind: u32) -> i8 {
    get_binary_operator_precedence(FromPrimitive::from_u32(kind).unwrap()).0
}

pub fn get_operator_precedence(
    node_kind: SyntaxKind,
    operator_kind: SyntaxKind,
    has_arguments: bool,
) -> Precedence {
    use SyntaxKind::*;
    Precedence(match node_kind {
        CommaListExpression => 0,
        SpreadElement => 1,
        YieldExpression => 2,
        ConditionalExpression => 4,
        BinaryExpression => match operator_kind {
            CommaToken => 0,
            EqualsToken
            | PlusEqualsToken
            | MinusEqualsToken
            | AsteriskAsteriskEqualsToken
            | SlashEqualsToken
            | PercentEqualsToken
            | LessThanLessThanEqualsToken
            | GreaterThanGreaterThanEqualsToken
            | GreaterThanGreaterThanGreaterThanEqualsToken
            | AmpersandEqualsToken
            | CaretEqualsToken
            | BarEqualsToken => 3,
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
        ThisKeyword
        | SuperKeyword
        | Identifier
        | NullKeyword
        | TrueKeyword
        | FalseKeyword
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

#[wasm_bindgen(js_name = getOperatorPrecedence)]
pub fn get_operator_precedence_js(
    node_kind: u32,
    operator_kind: u32,
    has_arguments: Option<bool>,
) -> i8 {
    get_operator_precedence(
        FromPrimitive::from_u32(node_kind).unwrap(),
        FromPrimitive::from_u32(operator_kind).unwrap(),
        has_arguments.unwrap_or(false),
    )
    .0
}

#[wasm_bindgen(js_name=getPropertyNameForKnownSymbolName)]
pub fn get_property_name_for_known_symbol_name(symbol_name: &str) -> String {
    "__@".to_string() + symbol_name
}
