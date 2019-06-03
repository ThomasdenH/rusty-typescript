use crate::compiler::scanner::Scanner;
use crate::compiler::types::diagnostic;
use crate::compiler::types::node::{self, GetBaseNode};
use crate::compiler::types::syntax_kind::{SyntaxKind, Token};
use crate::compiler::types::text_range::TextRange;
use crate::compiler::types::NodeFlags;
use crate::compiler::types::{LanguageVariant, ScriptKind, ScriptTarget};
use snafu::*;
use std::collections::HashMap;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum ParsingContext {
    SourceElements,           // Elements in source file
    BlockStatements,          // Statements in block
    SwitchClauses,            // Clauses in switch statement
    SwitchClauseStatements,   // Statements in switch clause
    TypeMembers,              // Members in interface or type literal
    ClassMembers,             // Members in class declaration
    EnumMembers,              // Members in enum declaration
    HeritageClauseElement,    // Elements in a heritage clause
    VariableDeclarations,     // Variable declarations in variable statement
    ObjectBindingElements,    // Binding elements in object binding list
    ArrayBindingElements,     // Binding elements in array binding list
    ArgumentExpressions,      // Expressions in argument list
    ObjectLiteralMembers,     // Members in object literal
    JsxAttributes,            // Attributes in jsx element
    JsxChildren,              // Things between opening and closing JSX tags
    ArrayLiteralMembers,      // Members in array literal
    Parameters,               // Parameters in parameter list
    JSDocParameters,          // JSDoc parameters in parameter list of JSDoc function type
    RestProperties,           // Property names in a rest type list
    TypeParameters,           // Type parameters in type parameter list
    TypeArguments,            // Type arguments in type argument list
    TupleElementTypes,        // Element types in tuple element type list
    HeritageClauses,          // Heritage clauses for a class or interface declaration.
    ImportOrExportSpecifiers, // Named import clause's import specifier list
    Count,                    // Number of parsing contexts
}

pub(crate) fn is_js_doc_like_text(text: &str, start: usize) -> bool {
    let mut c = text.chars().skip(1);
    c.next() == Some('*') && c.next() == Some('*') && c.next() != Some('/')
}

pub trait SyntaxCursor {}

pub struct Parser<'a> {
    scanner: Scanner<'a>,

    source_file: Option<SourceFile>,
    parse_diagnostics: Vec<Box<dyn diagnostic::DiagnosticWithLocation>>,
    syntax_cursor: Option<Box<dyn SyntaxCursor>>,

    current_token: Option<SyntaxKind>,
    source_text: String,
    node_count: usize,
    identifiers: HashMap<String, String>,
    identifier_count: usize,
    parsing_context: Option<ParsingContext>,
    // Flags that dictate what parsing context we're in.  For example:
    // Whether or not we are in strict parsing mode.  All that changes in strict parsing mode is
    // that some tokens that would be considered identifiers may be considered keywords.
    //
    // When adding more parser context flags, consider which is the more common case that the
    // flag will be in.  This should be the 'false' state for that flag.  The reason for this is
    // that we don't store data in our nodes unless the value is in the *non-default* state.  So,
    // for example, more often than code 'allows-in' (or doesn't 'disallow-in').  We opt for
    // 'disallow-in' set to 'false'.  Otherwise, if we had 'allowsIn' set to 'true', then almost
    // all nodes would need extra state on them to store this info.
    //
    // Note: 'allowIn' and 'allowYield' track 1:1 with the [in] and [yield] concepts in the ES6
    // grammar specification.
    //
    // An important thing about these context concepts.  By default they are effectively inherited
    // while parsing through every grammar production.  i.e. if you don't change them, then when
    // you parse a sub-production, it will have the same context values as the parent production.
    // This is great most of the time.  After all, consider all the 'expression' grammar productions
    // and how nearly all of them pass along the 'in' and 'yield' context values:
    //
    // EqualityExpression[In, Yield] :
    //      RelationalExpression[?In, ?Yield]
    //      EqualityExpression[?In, ?Yield] == RelationalExpression[?In, ?Yield]
    //      EqualityExpression[?In, ?Yield] != RelationalExpression[?In, ?Yield]
    //      EqualityExpression[?In, ?Yield] === RelationalExpression[?In, ?Yield]
    //      EqualityExpression[?In, ?Yield] !== RelationalExpression[?In, ?Yield]
    //
    // Where you have to be careful is then understanding what the points are in the grammar
    // where the values are *not* passed along.  For example:
    //
    // SingleNameBinding[Yield,GeneratorParameter]
    //      [+GeneratorParameter]BindingIdentifier[Yield] Initializer[In]opt
    //      [~GeneratorParameter]BindingIdentifier[?Yield]Initializer[In, ?Yield]opt
    //
    // Here this is saying that if the GeneratorParameter context flag is set, that we should
    // explicitly set the 'yield' context flag to false before calling into the BindingIdentifier
    // and we should explicitly unset the 'yield' context flag before calling into the Initializer.
    // production.  Conversely, if the GeneratorParameter context flag is not set, then we
    // should leave the 'yield' context flag alone.
    //
    // Getting this all correct is tricky and requires careful reading of the grammar to
    // understand when these values should be changed versus when they should be inherited.
    //
    // Note: it should not be necessary to save/restore these flags during speculative/lookahead
    // parsing.  These context flags are naturally stored and restored through normal recursive
    // descent parsing and unwinding.
    context_flags: NodeFlags,
    // Whether or not we've had a parse error since creating the last AST node.  If we have
    // encountered an error, it will be stored on the next AST node we create.  Parse errors
    // can be broken down into three categories:
    //
    // 1) An error that occurred during scanning.  For example, an unterminated literal, or a
    //    character that was completely not understood.
    //
    // 2) A token was expected, but was not present.  This type of error is commonly produced
    //    by the 'parseExpected' function.
    //
    // 3) A token was present that no parsing function was able to consume.  This type of error
    //    only occurs in the 'abortParsingListOrMoveToNextToken' function when the parser
    //    decides to skip the token.
    //
    // In all of these cases, we want to mark the next node as having had an error before it.
    // With this mark, we can know in incremental settings if this node can be reused, or if
    // we have to reparse it.  If we don't keep this information around, we may just reuse the
    // node.  in that event we would then not produce the same errors as we did before, causing
    // significant confusion problems.
    //
    // Note: it is necessary that this value be saved/restored during speculative/lookahead
    // parsing.  During lookahead parsing, we will often create a node.  That node will have
    // this value attached, and then this value will be set back to 'false'.  If we decide to
    // rewind, we must get back to the same value we had prior to the lookahead.
    //
    // Note: any errors at the end of the file that do not precede a regular node, should get
    // attached to the EOF token.
    parse_error_before_next_finished_node: bool,
}

impl<'a> Parser<'a> {
    fn initialize_state<SC>(
        source_text: String,
        language_version: ScriptTarget,
        syntax_cursor: Box<dyn SyntaxCursor>,
        script_kind: ScriptKind,
    ) -> Parser<'a> {
        let mut scanner = Scanner::new(
            language_version,
            false,
            LanguageVariant::Standard,
            Some(&source_text),
            None,
            None,
            None,
        );

        let context_flags = match script_kind {
            ScriptKind::JS | ScriptKind::JSX => NodeFlags::JAVA_SCRIPT_FILE,
            ScriptKind::JSON => NodeFlags::JAVA_SCRIPT_FILE | NodeFlags::JSON_FILE,
            _ => NodeFlags::NONE,
        };

        Parser {
            scanner,
            source_file: None,
            parse_diagnostics: Vec::new(),
            syntax_cursor: Some(syntax_cursor),
            current_token: None,
            source_text,
            node_count: 0,
            identifier_count: 0,
            identifiers: HashMap::new(),
            parsing_context: None,
            context_flags,
            parse_error_before_next_finished_node: false,
        }
    }

    fn token(&self) -> Option<SyntaxKind> {
        self.current_token
    }

    fn next_token(&mut self) -> Option<SyntaxKind> {
        self.current_token = Some(self.scanner.scan());
        self.current_token
    }

    fn can_parse_semicolon(&self) -> bool {
        self.token() == Some(SyntaxKind::Token(Token::Semicolon))
            || self.token() == Some(SyntaxKind::Token(Token::CloseBrace))
            || self.token() == Some(SyntaxKind::EndOfFileToken)
            || self.scanner.has_preceding_line_break()
    }

    fn parse_token_node(&mut self) -> Result<Box<dyn node::Token>, ParseTokenNodeError> {
        let base_node = node::BaseNode::new(
            Some(self.scanner.start_pos()),
            Some(self.scanner.start_pos()),
        );

        let mut node = if let Some(kind) = self.token() {
            match kind {
                SyntaxKind::Token(Token::DotDotDot) => node::DotDotDotToken::new(base_node),
                SyntaxKind::Token(token) => {
                    return Err(ParseTokenNodeError::TokenCannotBeMadeIntoANode { token })
                }
                kind => return Err(ParseTokenNodeError::NotAToken { kind }),
            }
        } else {
            return Err(ParseTokenNodeError::NoToken);
        };

        node.base_node_mut().flags = self.context_flags;
        if self.parse_error_before_next_finished_node {
            self.parse_error_before_next_finished_node = false;
            node.base_node_mut().flags |= NodeFlags::THIS_NODE_HAS_ERROR;
        }

        self.next_token();

        Ok(Box::new(node))
    }

    fn parse_error_at_position(
        &mut self,
        start: usize,
        length: usize,
        message: diagnostic::Message,
    ) {
        // Don't report another error if it would just be at the same position as the last error.
        let last_error = self.parse_diagnostics.last();
        if last_error.map(|last| last.start != start).unwrap_or(true) {
            self.parse_diagnostics
                .push(diagnostic::DiagnosticWithLocation::new(
                    self.file, start, length, message,
                ))
        }

        // Mark that we've encountered an error.  We'll set an appropriate bit on the next
        // node we finish so that it can't be reused incrementally.
        self.parse_error_before_next_finished_node = true;
    }

    fn parse_error_at(&mut self, start: usize, end: usize, message: diagnostic::Message) {
        self.parse_error_at_position(start, end - start, message);
    }

    fn parse_error_at_current_token(&mut self, message: diagnostic::Message) {
        self.parse_error_at(self.scanner.token_pos(), self.scanner.token_pos(), message);
    }

    fn parse_error_at_range(&mut self, range: TextRange, message: diagnostic::Message) {
        self.parse_error_at(range.start.unwrap(), range.end.unwrap(), message);
    }

    fn scan_error(&mut self, message: diagnostic::Message, length: usize) {
        self.parse_error_at_position(self.scanner.text_pos(), length, message);
    }

    fn re_scan_greater_token(&mut self) -> SyntaxKind {
        self.current_token = self.scanner.re_scan_greater_token();
        return self.current_token;
    }

    fn re_scan_slash_token(&mut self) -> SyntaxKind {
        self.current_token = self.scanner.re_scan_slash_token();
        return self.current_token;
    }

    fn re_scan_template_token(&mut self) -> SyntaxKind {
        self.current_token = self.scanner.re_scan_template_token();
        return self.current_token;
    }

    fn re_scan_less_than_token(&mut self) -> SyntaxKind {
        self.current_token = self.scanner.re_scan_less_than_token();
        return self.current_token;
    }

    fn scan_jsx_identifier(&mut self) -> SyntaxKind {
        self.current_token = self.scanner.scan_jsx_identifier();
        return self.current_token;
    }

    fn scan_jsx_text(&mut self) -> SyntaxKind {
        self.current_token = self.scanner.scan_jsx_token();
        return self.current_token;
    }

    fn scan_jsx_attribute_value(&mut self) -> SyntaxKind {
        self.current_token = self.scanner.scan_jsx_attribute_value();
        return self.current_token;
    }
}

#[derive(Clone, Hash, Debug, Snafu)]
enum ParseTokenNodeError {
    #[snafu(display("the syntaxkind is not a token: {:?}", kind))]
    NotAToken { kind: SyntaxKind },
    #[snafu(display("the token cannot currently be made into a node: {:?}", token))]
    TokenCannotBeMadeIntoANode {
        token: crate::compiler::types::syntax_kind::Token,
    },
    #[snafu(display("the parser currently has no token from the scanner"))]
    NoToken,
}
