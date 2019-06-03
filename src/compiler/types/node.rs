use crate::compiler::types::syntax_kind::SyntaxKind;
use crate::compiler::types::{syntax_kind, NodeFlags};
use crate::compiler::types::{TextRange, TokenFlags};

#[derive(Clone, Eq, PartialEq)]
pub struct BaseNode {
    text_range: TextRange,
    pub(crate) flags: NodeFlags,
}

impl BaseNode {
    pub fn new(pos: Option<usize>, end: Option<usize>) -> BaseNode {
        BaseNode {
            text_range: TextRange::from_pos_and_end(pos, end),
            flags: NodeFlags::NONE,
        }
    }
}

/// A trait that gives access to the base node.
pub trait GetBaseNode {
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

macro_rules! impl_get_base_node_with_lifetime {
    ($x:ident) => {
        impl GetBaseNode for $x<'_> {
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
    fn kind() -> SyntaxKind;
}

macro_rules! impl_with_syntax_kind {
    ($x:ident) => {
        impl WithSyntaxKind for $x {
            fn kind(&self) -> SyntaxKind {
                SyntaxKind::$x
            }
        }
    };
}

macro_rules! impl_with_syntax_kind_with_lifetime {
    ($x:ident) => {
        impl WithSyntaxKind for $x<'_> {
            fn kind(&self) -> SyntaxKind {
                SyntaxKind::$x
            }
        }
    };
}

pub trait Token: WithSyntaxKind + GetBaseNode + WithSyntaxKind {}

macro_rules! token {
    ($x:ident, $syntax_kind:expr) => {
        #[derive(Clone)]
        pub struct $x {
            node: BaseNode,
        }

        impl $x {
            pub fn new(node: BaseNode) -> Self {
                $x { node }
            }
        }

        impl_get_base_node!($x);
        impl WithSyntaxKind for $x {
            fn kind(&self) -> SyntaxKind {
                $syntax_kind
            }
        }
        impl Token for $x {}
    };
}

token!(
    DotDotDotToken,
    SyntaxKind::Token(syntax_kind::Token::DotDotDot)
);
token!(
    QuestionToken,
    SyntaxKind::Token(syntax_kind::Token::Question)
);
token!(
    ExclamationToken,
    SyntaxKind::Token(syntax_kind::Token::Exclamation)
);
token!(ColonToken, SyntaxKind::Token(syntax_kind::Token::Colon));
token!(EqualsToken, SyntaxKind::Token(syntax_kind::Token::Equals));
token!(
    AsteriskToken,
    SyntaxKind::Token(syntax_kind::Token::Asterisk)
);
token!(
    EqualsGreaterThanToken,
    SyntaxKind::Token(syntax_kind::Token::EqualsGreaterThan)
);
token!(EndOfFileToken, SyntaxKind::EndOfFileToken);

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
    parent: &'a dyn NamedDeclaration,
    expression: Box<dyn LeftHandSideExpression>,
}
impl_get_base_node_with_lifetime!(Decorator);
impl_with_syntax_kind_with_lifetime!(Decorator);

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

pub struct VariableDeclaration {
    base_node: BaseNode,
    name: BindingName,
}
impl_get_base_node!(NoSubstitutionTemplateLiteral);
impl_with_syntax_kind!(NoSubstitutionTemplateLiteral);

impl NamedDeclaration for VariableDeclaration {
    fn name(&self) -> &DeclarationName {
        &self.name
    }
}

pub struct ComputedPropertyName<'a> {
    parent: &'a dyn Declaration,
    node: BaseNode,
    expression: Box<dyn Expression>,
}
impl_get_base_node_with_lifetime!(ComputedPropertyName);
impl_with_syntax_kind_with_lifetime!(ComputedPropertyName);

pub enum ArrayBindingElement<'a> {
    BindingElement(BindingElement<'a>),
    OmittedExpression(OmittedExpression),
}

pub struct ObjectBindingPattern<'a> {
    node: BaseNode,
    parent: &'a ObjectBindingPatternParent,
    elements: Vec<ArrayBindingElement<'a>>,
}
impl_get_base_node_with_lifetime!(ObjectBindingPattern);
impl_with_syntax_kind_with_lifetime!(ObjectBindingPattern);

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
impl_get_base_node_with_lifetime!(ArrayBindingPattern);
impl_with_syntax_kind_with_lifetime!(ArrayBindingPattern);

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
impl_get_base_node_with_lifetime!(BindingElement);
impl_with_syntax_kind_with_lifetime!(BindingElement);

pub enum PropertyName<'a> {
    Identifier(Identifier),
    StringLiteral(StringLiteral),
    NumericLiteral(NumericLiteral),
    ComputedPropertyName(ComputedPropertyName<'a>),
}

pub struct SourceFile {
    node: BaseNode,
}
impl_get_base_node!(SourceFile);
impl_declaration!(SourceFile);
