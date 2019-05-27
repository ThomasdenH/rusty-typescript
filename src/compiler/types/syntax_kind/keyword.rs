use core::fmt::Display;
use core::str::FromStr;
use snafu::Snafu;

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

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug)]
pub enum Keyword {
    Abstract,
    Any,
    As,
    BigInt,
    Boolean,
    Break,
    Case,
    Catch,
    Class,
    Continue,
    Const,
    Constructor,
    Debugger,
    Declare,
    Default,
    Delete,
    Do,
    Else,
    Enum,
    Export,
    Extends,
    False,
    Finally,
    For,
    From,
    Function,
    Get,
    If,
    Implements,
    Import,
    In,
    Infer,
    InstanceOf,
    Interface,
    Is,
    KeyOf,
    Let,
    Module,
    Namespace,
    Never,
    New,
    Null,
    Number,
    Object,
    Package,
    Private,
    Protected,
    Public,
    Readonly,
    Require,
    Global,
    Return,
    Set,
    Static,
    String,
    Super,
    Switch,
    Symbol,
    This,
    Throw,
    True,
    Try,
    Type,
    TypeOf,
    Undefined,
    Unique,
    Unknown,
    Var,
    Void,
    While,
    With,
    Yield,
    Async,
    Await,
    Of,
}

impl Keyword {
    pub(crate) fn is_reserved_word(self) -> bool {
        use Keyword::*;
        match self {
            Break | Case | Catch | Class | Const | Continue | Debugger | Default | Delete | Do
            | Else | Enum | Export | Extends | False | Finally | For | Function | If | Import
            | In | InstanceOf | New | Null | Return | Super | Switch | This | Throw | True
            | Try | TypeOf | Var | Void | While | With => true,
            _ => false,
        }
    }
}

#[derive(Eq, PartialEq, Clone, Snafu, Hash, Debug)]
pub enum FromStrError {
    #[snafu(display("not a keyword: {}", s))]
    NotAKeyword { s: String },
}

impl FromStr for Keyword {
    type Err = FromStrError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Keyword::*;
        match s {
            ABSTRACT => Ok(Abstract),
            ANY => Ok(Any),
            AS => Ok(As),
            BIGINT => Ok(BigInt),
            BOOLEAN => Ok(Boolean),
            BREAK => Ok(Break),
            CASE => Ok(Case),
            CATCH => Ok(Catch),
            CLASS => Ok(Class),
            CONTINUE => Ok(Continue),
            CONST => Ok(Const),
            CONSTRUCTOR => Ok(Constructor),
            DEBUGGER => Ok(Debugger),
            DECLARE => Ok(Declare),
            DEFAULT => Ok(Default),
            DELETE => Ok(Delete),
            DO => Ok(Do),
            ELSE => Ok(Else),
            ENUM => Ok(Enum),
            EXPORT => Ok(Export),
            EXTENDS => Ok(Extends),
            FALSE => Ok(False),
            FINALLY => Ok(Finally),
            FOR => Ok(For),
            FROM => Ok(From),
            FUNCTION => Ok(Function),
            GET => Ok(Get),
            IF => Ok(If),
            IMPLEMENTS => Ok(Implements),
            IMPORT => Ok(Import),
            IN => Ok(In),
            INFER => Ok(Infer),
            INSTANCEOF => Ok(InstanceOf),
            INTERFACE => Ok(Interface),
            IS => Ok(Is),
            KEYOF => Ok(KeyOf),
            LET => Ok(Let),
            MODULE => Ok(Module),
            NAMESPACE => Ok(Namespace),
            NEVER => Ok(Never),
            NEW => Ok(New),
            NULL => Ok(Null),
            NUMBER => Ok(Number),
            OBJECT => Ok(Object),
            PACKAGE => Ok(Package),
            PRIVATE => Ok(Private),
            PROTECTED => Ok(Protected),
            PUBLIC => Ok(Public),
            READONLY => Ok(Readonly),
            REQUIRE => Ok(Require),
            GLOBAL => Ok(Global),
            RETURN => Ok(Return),
            SET => Ok(Set),
            STATIC => Ok(Static),
            STRING => Ok(String),
            SUPER => Ok(Super),
            SWITCH => Ok(Switch),
            SYMBOL => Ok(Symbol),
            THIS => Ok(This),
            THROW => Ok(Throw),
            TRUE => Ok(True),
            TRY => Ok(Try),
            TYPE => Ok(Type),
            TYPEOF => Ok(TypeOf),
            UNDEFINED => Ok(Undefined),
            UNIQUE => Ok(Unique),
            UNKNOWN => Ok(Unknown),
            VAR => Ok(Var),
            VOID => Ok(Void),
            WHILE => Ok(While),
            WITH => Ok(With),
            YIELD => Ok(Yield),
            ASYNC => Ok(Async),
            AWAIT => Ok(Await),
            OF => Ok(Of),
            _ => Err(FromStrError::NotAKeyword { s: s.to_string() }),
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        use Keyword::*;

        let s: &str = match self {
            Abstract => ABSTRACT,
            Any => ANY,
            As => AS,
            BigInt => BIGINT,
            Boolean => BOOLEAN,
            Break => BREAK,
            Case => CASE,
            Catch => CATCH,
            Class => CLASS,
            Continue => CONTINUE,
            Const => CONST,
            Constructor => CONSTRUCTOR,
            Debugger => DEBUGGER,
            Declare => DECLARE,
            Default => DEFAULT,
            Delete => DELETE,
            Do => DO,
            Else => ELSE,
            Enum => ENUM,
            Export => EXPORT,
            Extends => EXTENDS,
            False => FALSE,
            Finally => FINALLY,
            For => FOR,
            From => FROM,
            Function => FUNCTION,
            Get => GET,
            If => IF,
            Implements => IMPLEMENTS,
            Import => IMPORT,
            In => IN,
            Infer => INFER,
            InstanceOf => INSTANCEOF,
            Interface => INTERFACE,
            Is => IS,
            KeyOf => KEYOF,
            Let => LET,
            Module => MODULE,
            Namespace => NAMESPACE,
            Never => NEVER,
            New => NEW,
            Null => NULL,
            Number => NUMBER,
            Object => OBJECT,
            Package => PACKAGE,
            Private => PRIVATE,
            Protected => PROTECTED,
            Public => PUBLIC,
            Readonly => READONLY,
            Require => REQUIRE,
            Global => GLOBAL,
            Return => RETURN,
            Set => SET,
            Static => STATIC,
            String => STRING,
            Super => SUPER,
            Switch => SWITCH,
            Symbol => SYMBOL,
            This => THIS,
            Throw => THROW,
            True => TRUE,
            Try => TRY,
            Type => TYPE,
            TypeOf => TYPEOF,
            Undefined => UNDEFINED,
            Unique => UNIQUE,
            Unknown => UNKNOWN,
            Var => VAR,
            Void => VOID,
            While => WHILE,
            With => WITH,
            Yield => YIELD,
            Async => ASYNC,
            Await => AWAIT,
            Of => OF,
        };

        write!(f, "{}", s)
    }
}
