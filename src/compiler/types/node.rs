use crate::compiler::types::syntax_kind::SyntaxKind;
use crate::compiler::types::{TextRange, TokenFlags};

#[derive(Clone, Eq, PartialEq)]
pub struct BaseNode {
    text_range: TextRange,
}

/// A trait that gives access to the base node.
trait GetBaseNode {
    fn base_node(&self) -> &BaseNode;
    fn base_node_mut(&mut self) -> &mut BaseNode;
}

/// When this macro is called on a type, it implements `GetBaseNode` by
/// assuming the type has a `node` property.
macro_rules! impl_get_base_node {
    ($x:ident) => {
        impl GetBaseNode for $x {
            fn base_node(&self) -> &BaseNode {
                &self.node
            }
            fn base_node_mut(&mut self) -> &mut BaseNode {
                &mut self.node
            }
        }
    };
}

trait WithSyntaxKind {
    const SYNTAX_KIND: SyntaxKind;
}

macro_rules! impl_with_syntax_kind {
    ($x:ident) => {
        impl WithSyntaxKind for $x {
            const SYNTAX_KIND: SyntaxKind = SyntaxKind::$x;
        }
    };
}

macro_rules! token {
    ($x:ident) => {
        #[derive(Clone)]
        pub struct $x {
            node: BaseNode,
        }
        impl_get_base_node!($x);
        impl_with_syntax_kind!($x);
    };
}

token!(DotDotDotToken);
token!(QuestionToken);
token!(ExclamationToken);
token!(ColonToken);
token!(EqualsToken);
token!(AsteriskToken);
token!(EqualsGreaterThanToken);
token!(EndOfFileToken);
token!(ReadonlyToken);
token!(AwaitKeywordToken);
token!(PlusToken);
token!(MinusToken);

// Modifiers
token!(AbstractKeyword);
token!(AsyncKeyword);
token!(ConstKeyword);
token!(DeclareKeyword);
token!(DefaultKeyword);
token!(ExportKeyword);
token!(PublicKeyword);
token!(PrivateKeyword);
token!(ProtectedKeyword);
token!(ReadonlyKeyword);
token!(StaticKeyword);

pub trait Declaration {}
macro_rules! impl_declaration {
    ($x:ident) => {
        impl Declaration for $x {}
    };
}

pub enum DeclarationName {
    Identifier(Identifier),
    StringLiteralLike(StringLiteralLike),
    NumericLiteral(NumericLiteral),
    ComputedPropertyName(crate::compiler::types::node::DeclarationName),
    BindingPattern(BindingPattern),
}

pub trait NamedDeclaration {
    fn name(&self) -> &DeclarationName;
}

pub trait Expression {}
macro_rules! impl_expression {
    ($x:ident) => {
        impl Expression for $x {}
    };
}

pub trait UnaryExpression: Expression {}
macro_rules! impl_unary_expression {
    ($x:ident) => {
        impl_expression!($x);
        impl UnaryExpression for $x {}
    };
}

pub trait UpdateExpression: UnaryExpression {}
macro_rules! impl_update_expression {
    ($x:ident) => {
        impl_unary_expression!($x);
        impl UpdateExpression for $x {}
    };
}

pub trait LeftHandSideExpression: UpdateExpression {}
macro_rules! impl_left_hand_side_expression {
    ($x:ident) => {
        impl_update_expression!($x);
        impl LeftHandSideExpression for $x {}
    };
}

pub trait MemberExpression: LeftHandSideExpression {}
macro_rules! impl_member_expression {
    ($x:ident) => {
        impl_left_hand_side_expression!($x);
        impl MemberExpression for $x {}
    };
}

pub trait PrimaryExpression: MemberExpression {}
macro_rules! impl_primary_expression {
    ($x:ident) => {
        impl_member_expression!($x);
        impl PrimaryExpression for $x {}
    };
}

token!(OmittedExpression);
impl_expression!(OmittedExpression);

pub struct Identifier {
    node: BaseNode,
}

impl_get_base_node!(Identifier);
impl_with_syntax_kind!(Identifier);
impl_declaration!(Identifier);
impl_primary_expression!(Identifier);

pub struct QualifiedName {
    node: BaseNode,
    left: Box<EntityName>,
    right: Box<Identifier>,
}
impl_get_base_node!(QualifiedName);
impl_with_syntax_kind!(QualifiedName);

pub enum EntityName {
    Identifier(Identifier),
    QualifiedName(QualifiedName),
}

pub struct Decorator<'a> {
    node: BaseNode,
    parent: &'a NamedDeclaration,
    expression: Box<LeftHandSideExpression>,
}
impl_get_base_node!(Decorator);
impl_with_syntax_kind!(Decorator);

pub enum TextSourceNode {
    Identifier(Identifier),
    StringLiteralLike(StringLiteralLike),
    NumericLiteral(NumericLiteral),
}

pub struct StringLiteral {
    node: BaseNode,
    text_source_node: Box<TextSourceNode>,
    single_quote: bool,
}
impl_get_base_node!(StringLiteral);
impl_with_syntax_kind!(StringLiteral);

pub enum StringLiteralLike {
    StringLiteral(StringLiteral),
    NoSubstitutionTemplateLiteral(NoSubstitutionTemplateLiteral),
}

pub struct NoSubstitutionTemplateLiteral {
    text: String,
    is_unterminated: bool,
    has_extended_unicode_escape: bool,
    node: BaseNode,
}
impl_get_base_node!(NoSubstitutionTemplateLiteral);
impl_with_syntax_kind!(NoSubstitutionTemplateLiteral);
impl_primary_expression!(NoSubstitutionTemplateLiteral);

pub struct NumericLiteral {
    node: BaseNode,
    numeric_literal_flags: TokenFlags,
}
impl_get_base_node!(NumericLiteral);
impl_with_syntax_kind!(NumericLiteral);

pub struct ComputedPropertyName<'a> {
    parent: &'a dyn Declaration,
    node: BaseNode,
    expression: Box<Expression>,
}
impl_get_base_node!(ComputedPropertyName);
impl_with_syntax_kind!(ComputedPropertyName);

pub enum ArrayBindingElement<'a> {
    BindingElement(BindingElement<'a>),
    OmittedExpression(OmittedExpression),
}

pub struct ObjectBindingPattern<'a> {
    node: BaseNode,
    parent: &'a ObjectBindingPatternParent,
    elements: Vec<ArrayBindingElement<'a>>,
}
impl_get_base_node!(ObjectBindingPattern);
impl_with_syntax_kind!(ObjectBindingPattern);

pub enum ObjectBindingPatternParent {
    VariableDeclaration(VariableDeclaration),
    ParameterDeclaration(ParameterDeclaration),
    BindingElement(BindingElement),
}

pub struct ArrayBindingPattern<'a> {
    node: BaseNode,
    parent: &'a ArrayBindingPatternParent,
    elements: Vec<ArrayBindingElement>,
}
impl_get_base_node!(ArrayBindingPattern);
impl_with_syntax_kind!(ArrayBindingPattern);

pub enum ArrayBindingPatternParent {
    VariableDeclaration(VariableDeclaration),
    ParameterDeclaration(ParameterDeclaration),
    BindingElement(BindingElement),
}

pub enum BindingPattern {
    ObjectBindingPattern(ObjectBindingPattern),
    ArrayBindingPattern(ArrayBindingPattern),
}

pub enum BindingName {
    Identifier(Identifier),
    BindingPattern(BindingPattern),
}

pub struct BindingElement<'a> {
    node: BaseNode,
    parent: &'a BindingPattern,
    property_name: PropertyName<'a>,
    dot_dot_dot_token: DotDotDotToken,
    name: BindingName,
    initializer: Expression,
}
impl_get_base_node!(BindingElement);
impl_with_syntax_kind!(BindingElement);

pub enum PropertyName<'a> {
    Identifier(Identifier),
    StringLiteral(StringLiteral),
    NumericLiteral(NumericLiteral),
    ComputedPropertyName(ComputedPropertyName<'a>),
}


