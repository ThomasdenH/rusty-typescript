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

pub trait WithSyntaxKind {
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

pub trait Declaration: GetBaseNode {}
macro_rules! impl_declaration {
    ($x:ident) => {
        impl Declaration for $x {}
    };
}

pub enum DeclarationName<'a> {
    Identifier(Identifier),
    StringLiteralLike(StringLiteralLike),
    NumericLiteral(NumericLiteral),
    ComputedPropertyName(ComputedPropertyName<'a>),
    BindingPattern(BindingPattern<'a>),
}

pub trait NamedDeclaration: Declaration {
    fn name(&self) -> &DeclarationName;
}

pub trait Expression: GetBaseNode {}
macro_rules! impl_expression {
    ($x:ident) => {
        impl Expression for $x {}
    };
}

pub struct OmittedExpression {
    node: BaseNode,
}
impl_get_base_node!(OmittedExpression);
impl_expression!(OmittedExpression);
impl WithSyntaxKind for OmittedExpression {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::OmittedExpression
    }
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

pub struct VariableDeclaration<'a> {
    base_node: BaseNode,
    name: BindingName<'a>,
}
impl_get_base_node_with_lifetime!(NoSubstitutionTemplateLiteral);
impl_with_syntax_kind_with_lifetime!(NoSubstitutionTemplateLiteral);

impl NamedDeclaration for VariableDeclaration<'_> {
    fn name(&self) -> &DeclarationName {
        &self.name.as_ref()
    }
}

pub enum HasJsDoc {
    ParameterDeclaration,
    CallSignatureDeclaration,
    ConstructSignatureDeclaration,
    MethodSignature,
    PropertySignature,
    ArrowFunction,
    ParenthesizedExpression,
    SpreadAssignment,
    ShorthandPropertyAssignment,
    PropertyAssignment,
    FunctionExpression,
    LabeledStatement,
    ExpressionStatement,
    VariableStatement,
    FunctionDeclaration,
    ConstructorDeclaration,
    MethodDeclaration,
    PropertyDeclaration,
    AccessorDeclaration,
    ClassLikeDeclaration,
    InterfaceDeclaration,
    TypeAliasDeclaration,
    EnumMember,
    EnumDeclaration,
    ModuleDeclaration,
    ImportEqualsDeclaration,
    IndexSignatureDeclaration,
    FunctionTypeNode,
    ConstructorTypeNode,
    JsDocFunctionType,
    ExportDeclaration,
    EndOfFileToken,
}

pub trait TypeNode: GetBaseNode {}

pub enum JsDocOrJsDocTypeLiteral {
    JsDoc(JsDoc),
    JsDocTypeLiteral(JsDocTypeLiteral),
}

pub trait JsDocTag: GetBaseNode {
    fn parent(&self) -> &JsDocOrJsDocTypeLiteral;
    fn tag_name(&self) -> &Identifier;
    fn comment(&self) -> &String;
}

pub trait JsDocPropertyLikeTag: JsDocTag + Declaration {
    fn parent(&self) -> &JsDoc;
    fn name(&self) -> &EntityName;
    fn type_expression(&self) -> &Option<JsDocTypeExpression>;
    fn is_name_first(&self) -> bool;
    fn is_bracketed(&self) -> bool;
}

/// Represents a top level: { type } expression in a JSDoc comment.
pub struct JsDocTypeExpression {
    node: BaseNode,
    r#type: Box<dyn TypeNode>,
}

impl_get_base_node!(JsDocTypeExpression);
impl WithSyntaxKind for JsDocTypeExpression {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::JSDocTypeExpression
    }
}
impl TypeNode for JsDocTypeExpression {}

pub struct JsDocTypeLiteral {
    node: BaseNode,
    jsDoocPropertyTags: Option<Vec<dyn JsDocPropertyLikeTag>>,
    isArrayType: bool,
}
impl_get_base_node!(JsDocTypeLiteral);
impl WithSyntaxKind for JsDocTypeLiteral {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::JSDocTypeLiteral
    }
}
impl TypeNode for JsDocTypeLiteral {}

pub struct JsDoc {
    node: BaseNode,
    parent: Box<HasJsDoc>,
    tags: Option<Vec<Box<dyn JsDocTag>>>,
    comment: Option<String>,
}

pub enum JsDocTagParent {
    JsDoc(JsDoc),
    JsDocTypeLiteral(JsDocTypeLiteral),
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
    parent: &'a ObjectBindingPatternParent<'a>,
    elements: Vec<ArrayBindingElement<'a>>,
}
impl_get_base_node_with_lifetime!(ObjectBindingPattern);
impl_with_syntax_kind_with_lifetime!(ObjectBindingPattern);

pub struct BindingElement<'a> {
    node: BaseNode,
    parent: &'a BindingPattern<'a>,
    propteryName: Option<PropertyName<'a>>,
    DotDotDotToken: Option<DotDotDotToken>,
    name: BindingName<'a>,
    initializer: Option<Box<dyn Expression>>,
}
impl_get_base_node_with_lifetime!(BindingElement);
impl_with_syntax_kind_with_lifetime!(BindingElement);

pub struct ParameterDeclaration<'a> {
    node: BaseNode,
    parent: &'a SignatureDeclaration<'a>,
    dotDotDotToken: Option<DotDotDotToken>,
    name: BindingName<'a>,
    questionToken: Option<QuestionToken>,
    r#type: Option<Box<dyn TypeNode>>,
    initializer: Option<Box<dyn Expression>>,
}

pub enum SignatureDeclaration<'a> {
    CallSignatureDeclaration(CallSignatureDeclaration<'a>),
    ConstructSignatureDeclaration(ConstructSignatureDeclaration<'a>),
    MethodSignature(MethodSignature<'a>),
    IndexSignatureDeclaration(IndexSignatureDeclaration<'a>),
    FunctionTypeNode(FunctionTypeNode<'a>),
    ConstructorTypeNode(ConstructorTypeNode<'a>),
    JsDocFunctionType(JsDocFunctionType<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
    MethodDeclaration(MethodDeclaration<'a>),
    ConstructorDeclaration(ConstructorDeclaration<'a>),
    AccessorDeclaration(AccessorDeclaration<'a>),
    FunctionExpression(FunctionExpression<'a>),
}

pub enum ObjectBindingPatternParent<'a> {
    VariableDeclaration(VariableDeclaration<'a>),
    ParameterDeclaration(ParameterDeclaration<'a>),
    BindingElement(BindingElement<'a>),
}

pub struct ArrayBindingPattern<'a> {
    node: BaseNode,
    parent: &'a ArrayBindingPatternParent<'a>,
    elements: Vec<ArrayBindingElement<'a>>,
}
impl_get_base_node_with_lifetime!(ArrayBindingPattern);
impl_with_syntax_kind_with_lifetime!(ArrayBindingPattern);

pub enum ArrayBindingPatternParent<'a> {
    VariableDeclaration(VariableDeclaration<'a>),
    ParameterDeclaration(ParameterDeclaration<'a>),
    BindingElement(BindingElement<'a>),
}

pub enum BindingPattern<'a> {
    ObjectBindingPattern(ObjectBindingPattern<'a>),
    ArrayBindingPattern(ArrayBindingPattern<'a>),
}

pub enum BindingName<'a> {
    Identifier(Identifier),
    BindingPattern(BindingPattern<'a>),
}

impl AsRef<DeclarationName<'_>> for BindingName<'_> {
    fn as_ref(&self) -> &DeclarationName<'_> {
        match self {
            BindingName::Identifier(i) => &DeclarationName::Identifier(*i),
            BindingName::BindingPattern(b) => &DeclarationName::BindingPattern(*b),
        }
    }
}

pub enum PropertyName<'a> {
    Identifier(Identifier),
    StringLiteral(StringLiteral),
    NumericLiteral(NumericLiteral),
    ComputedPropertyName(ComputedPropertyName<'a>),
}

pub struct SourceFile {
    node: BaseNode,
    /// The first "most obvious" node that makes a file an external module.
    /// This is intended to be the first top-level import/export,
    /// but could be arbitrarily nested (e.g. `import.meta`).
    external_module_indicator: Option<Box<dyn GetBaseNode>>,
}
impl_get_base_node!(SourceFile);
impl_declaration!(SourceFile);

impl SourceFile {
    pub fn is_external_module(&self) -> bool {
        self.external_module_indicator.is_some()
    }
}

pub trait JsDocContainer {
    /// JSDoc that directly precedes this node
    fn js_doc(&self) -> &Option<&[JsDoc]>;
    /// Cache for getJSDocTags
    fn js_doc_cache(&self) -> &Option<&[dyn JsDocTag]>;
}

pub trait SignatureDeclarationBase: NamedDeclaration + JsDocContainer + WithSyntaxKind {
    fn name(&self) -> &Option<PropertyName>;
    fn type_parameters(&self) -> &Option<&[TypeParameterDeclaration]>;
}

pub struct CallSignatureDeclaration<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(CallSignatureDeclaration);
impl WithSyntaxKind for CallSignatureDeclaration<'_> {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::CallSignature
    }
}
impl JsDocContainer for CallSignatureDeclaration<'_> {}
impl NamedDeclaration for CallSignatureDeclaration<'_> {}
impl SignatureDeclarationBase for CallSignatureDeclaration<'_> {}

pub enum TypeParameterDeclarationParent {
    DeclarationWithTypeParameterChildren,
    InferTypeNode,
}

pub struct TypeParameterDeclaration<'a> {
    node: BaseNode,
    parent: &'a TypeParameterDeclarationParent,
    name: Identifier,
    contraint: Option<Box<dyn TypeNode>>,
    default: Option<Box<dyn TypeNode>>,
    expression: Option<Box<dyn Expression>>,
}
impl_get_base_node_with_lifetime!(TypeParameterDeclaration);
impl WithSyntaxKind for TypeParameterDeclaration<'_> {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::TypeParameter
    }
}
impl NamedDeclaration for TypeParameterDeclaration<'_> {}

pub struct ConstructSignatureDeclaration<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(ConstructSignatureDeclaration);
impl WithSyntaxKind for ConstructSignatureDeclaration<'_> {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::ConstructSignature
    }
}
impl JsDocContainer for ConstructSignatureDeclaration<'_> {}
impl NamedDeclaration for ConstructSignatureDeclaration<'_> {}
impl SignatureDeclarationBase for ConstructSignatureDeclaration<'_> {}

pub struct MethodSignature<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(MethodSignature);
impl_with_syntax_kind_with_lifetime!(MethodSignature);
impl JsDocContainer for MethodSignature<'_> {}
impl NamedDeclaration for MethodSignature<'_> {}
impl SignatureDeclarationBase for MethodSignature<'_> {}

pub enum ObjectTypeDeclaration<'a> {
    ClassLikeDeclaration(ClassLikeDeclaration<'a>),
    InterfaceDeclaration(InterfaceDeclaration<'a>),
    TypeLiteral(TypeLiteral),
}

pub struct IndexSignatureDeclaration<'a> {
    node: BaseNode,
    parent: &'a ObjectTypeDeclaration<'a>,
}
impl_get_base_node_with_lifetime!(IndexSignatureDeclaration);
impl WithSyntaxKind for IndexSignatureDeclaration<'_> {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::IndexSignature
    }
}
impl JsDocContainer for IndexSignatureDeclaration<'_> {}
impl NamedDeclaration for IndexSignatureDeclaration<'_> {}
impl SignatureDeclarationBase for IndexSignatureDeclaration<'_> {}

pub struct FunctionTypeNode<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(FunctionTypeNode);
impl WithSyntaxKind for FunctionTypeNode<'_> {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::FunctionType
    }
}
impl JsDocContainer for FunctionTypeNode<'_> {}
impl NamedDeclaration for FunctionTypeNode<'_> {}
impl SignatureDeclarationBase for FunctionTypeNode<'_> {}

pub struct ConstructorTypeNode<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(ConstructorTypeNode);
impl WithSyntaxKind for ConstructorTypeNode<'_> {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::ConstructorType
    }
}
impl JsDocContainer for ConstructorTypeNode<'_> {}
impl NamedDeclaration for ConstructorTypeNode<'_> {}
impl SignatureDeclarationBase for ConstructorTypeNode<'_> {}

pub struct JsDocFunctionType<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(JsDocFunctionType);
impl WithSyntaxKind for JsDocFunctionType<'_> {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::JSDocFunctionType
    }
}
impl JsDocContainer for JsDocFunctionType<'_> {}
impl NamedDeclaration for JsDocFunctionType<'_> {}
impl SignatureDeclarationBase for JsDocFunctionType<'_> {}

pub struct FunctionDeclaration<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(FunctionDeclaration);
impl_with_syntax_kind_with_lifetime!(FunctionDeclaration);
impl JsDocContainer for FunctionDeclaration<'_> {}
impl NamedDeclaration for FunctionDeclaration<'_> {}
impl SignatureDeclarationBase for FunctionDeclaration<'_> {}

pub struct MethodDeclaration<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(MethodDeclaration);
impl_with_syntax_kind_with_lifetime!(MethodDeclaration);
impl JsDocContainer for MethodDeclaration<'_> {}
impl NamedDeclaration for MethodDeclaration<'_> {}
impl SignatureDeclarationBase for MethodDeclaration<'_> {}

pub struct ConstructorDeclaration<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(ConstructorDeclaration);
impl WithSyntaxKind for ConstructorDeclaration<'_> {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::Constructor
    }
}
impl JsDocContainer for ConstructorDeclaration<'_> {}
impl NamedDeclaration for ConstructorDeclaration<'_> {}
impl SignatureDeclarationBase for ConstructorDeclaration<'_> {}

pub struct FunctionExpression<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(FunctionExpression);
impl_with_syntax_kind_with_lifetime!(FunctionExpression);
impl JsDocContainer for FunctionExpression<'_> {}
impl NamedDeclaration for FunctionExpression<'_> {}
impl SignatureDeclarationBase for FunctionExpression<'_> {}

pub enum DeclarationStatementName {
    Identifier(Identifier),
    StringLiteral(StringLiteral),
    NumericLiteral(NumericLiteral),
}

pub trait Statement: GetBaseNode + WithSyntaxKind {}

pub trait DeclarationStatement: NamedDeclaration + Statement {
    fn name(&self) -> &DeclarationStatementName;
}

pub trait ClassLikeDeclarationBase: NamedDeclaration + JsDocContainer + WithSyntaxKind {
    fn name(&self) -> Option<Identifier>;
}

pub struct ClassExpression<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(ClassExpression);
impl_with_syntax_kind_with_lifetime!(ClassExpression);
impl ClassLikeDeclarationBase for ClassExpression<'_> {}
impl PrimaryExpression for ClassExpression<'_> {}

pub struct ClassDeclaration<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(ClassDeclaration);
impl_with_syntax_kind_with_lifetime!(ClassDeclaration);
impl ClassLikeDeclarationBase for ClassDeclaration<'_> {}
impl DeclarationStatement for ClassDeclaration<'_> {}

pub enum ClassLikeDeclaration<'a> {
    ClassDeclaration(ClassDeclaration<'a>),
    ClassExpression(ClassExpression<'a>),
}

pub struct InterfaceDeclaration<'a> {
    node: BaseNode,
    name: Identifier,
    typeParameters: Option<Vec<TypeParameterDeclaration<'a>>>,
    heritageClauses: Option<Vec<HeritageClause<'a>>>,
    members: Option<Vec<Box<dyn TypeElement>>>,
}
impl_get_base_node_with_lifetime!(InterfaceDeclaration);
impl_with_syntax_kind_with_lifetime!(InterfaceDeclaration);
impl DeclarationStatement for InterfaceDeclaration<'_> {}
impl JsDocContainer for InterfaceDeclaration<'_> {}

pub struct TypeLiteral {
    node: BaseNode,
    members: Vec<Box<dyn TypeElement>>,
}
impl_get_base_node!(InterfaceDeclaration);
impl_with_syntax_kind!(InterfaceDeclaration);
impl_declaration!(TypeLiteral);
impl TypeNode for TypeLiteral {}

pub struct GetAccessorDeclaration<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(GetAccessorDeclaration);
impl WithSyntaxKind for GetAccessorDeclaration<'_> {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::GetAccessor
    }
}

pub struct SetAccessorDeclaration<'a> {
    node: BaseNode,
}
impl_get_base_node_with_lifetime!(SetAccessorDeclaration);
impl WithSyntaxKind for SetAccessorDeclaration<'_> {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::SetAccessor
    }
}

pub enum AccessorDeclaration<'a> {
    GetAccessorDeclaration(GetAccessorDeclaration<'a>),
    SetAccessorDeclaration(SetAccessorDeclaration<'a>),
}

pub enum HeritageClauseParent<'a> {
    InterfaceDeclaration(InterfaceDeclaration<'a>),
    ClassLikeDeclaration(ClassLikeDeclaration<'a>),
}

pub enum HeritageClauseToken {
    ExtendsKeyword,
    ImplementsKeyword,
}

pub struct HeritageClause<'a> {
    node: BaseNode,
    parent: &'a HeritageClauseParent<'a>,
    token: HeritageClauseToken,
    types: Vec<ExpressionWithTypeArguments<'a>>,
}
impl_get_base_node_with_lifetime!(HeritageClause);
impl_with_syntax_kind_with_lifetime!(HeritageClause);

pub trait TypeElement: NamedDeclaration {
    fn name(&self) -> &PropertyName;
    fn question_token(&self) -> &QuestionToken;
}

pub trait NodeWithTypeArguments: TypeNode {
    fn type_arguments(&self) -> &Option<&[dyn TypeNode]>;
}

pub enum ExpressionWithTypeArgumentsParent<'a> {
    HeritageClause(HeritageClause<'a>),
    JsDocAugmentsTag(JsDocAugmentsTag),
}

pub struct ExpressionWithTypeArguments<'a> {
    node: BaseNode,
    parent: &'a ExpressionWithTypeArgumentsParent<'a>,
    expression: Box<dyn LeftHandSideExpression>,
}
impl_get_base_node_with_lifetime!(ExpressionWithTypeArguments);
impl_with_syntax_kind_with_lifetime!(ExpressionWithTypeArguments);
impl NodeWithTypeArguments for ExpressionWithTypeArguments<'_> {}

pub struct JsDocAugmentsTag {
    node: BaseNode,
}
impl_get_base_node!(JsDocAugmentsTag);
impl WithSyntaxKind for JsDocAugmentsTag {
    fn kind(&self) -> SyntaxKind {
        SyntaxKind::JSDocAugmentsTag
    }
}
