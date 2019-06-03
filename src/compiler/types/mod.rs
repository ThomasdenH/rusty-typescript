use num_derive::{FromPrimitive, ToPrimitive};

#[cfg(test)]
use proptest_derive::*;

pub mod character_codes;
pub mod diagnostic;
mod modifier_flags;
// pub mod node;
pub mod node_flags;
pub mod pseudobigint;
pub mod syntax_kind;
pub mod text_range;
pub mod textspan;
mod token_flags;

pub use modifier_flags::ModifierFlags;
pub use node_flags::NodeFlags;
pub use pseudobigint::PseudoBigInt;
pub use syntax_kind::SyntaxKind;
pub use text_range::TextRange;
pub use token_flags::TokenFlags;

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, FromPrimitive, ToPrimitive)]
pub enum ScriptKind {
    Unknown,
    JS,
    JSX,
    TS,
    TSX,
    External,
    JSON,
    /// Used on extensions that doesn't define the ScriptKind but the content defines it.
    /// Deferred extensions are going to be included in all project contexts.
    Deferred,
}

#[cfg_attr(test, derive(Arbitrary))]
#[derive(Eq, PartialEq, Ord, PartialOrd, Debug, Hash, Copy, Clone)]
pub enum ScriptTarget {
    ES3 = 0,
    ES5 = 1,
    ES2015 = 2,
    ES2016 = 3,
    ES2017 = 4,
    ES2018 = 5,
    ES2019 = 6,
    ESNext = 7,
    JSON = 100,
}

impl ScriptTarget {
    pub fn latest() -> ScriptTarget {
        ScriptTarget::ESNext
    }

    pub fn is_latest(self) -> bool {
        self == ScriptTarget::ESNext
    }
}

#[cfg_attr(test, derive(Arbitrary))]
#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, FromPrimitive, ToPrimitive)]
pub enum LanguageVariant {
    Standard,
    JSX,
}

impl LanguageVariant {
    pub(crate) fn from_script_kind(script_kind: ScriptKind) -> LanguageVariant {
        use ScriptKind::*;
        // .tsx and .jsx files are treated as jsx language variant.
        match script_kind {
            TSX | JSX | JS | JSON => LanguageVariant::JSX,
            _ => LanguageVariant::Standard,
        }
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub enum ModuleResolutionKind {
    Classic,
    NodeJs,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub enum ModuleKind {
    None,
    CommonJs,
    AMD,
    UMD,
    System,
    ES2015,
    ESNext,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub enum JsxEmit {
    None,
    Preserve,
    React,
    ReactNative,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub enum NewLineKind {
    CarriageReturnLineFeed,
    LineFeed,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub enum Extension {
    Ts,
    Tsx,
    Dts,
    Js,
    Jsx,
    Json,
    TsBuildInfo,
}

impl std::fmt::Display for Extension {
    /// Print the extension, including the preceding dot
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Extension::Ts => write!(f, ".ts"),
            Extension::Tsx => write!(f, ".tsx"),
            Extension::Dts => write!(f, ".d.ts"),
            Extension::Js => write!(f, ".js"),
            Extension::Jsx => write!(f, ".jsx"),
            Extension::Json => write!(f, ".json"),
            Extension::TsBuildInfo => write!(f, ".tsbuildinfo"),
        }
    }
}

#[cfg(feature = "wasm")]
mod wasm {
    #[wasm_bindgen]
    pub fn get_language_variant(script_kind: u32) -> u32 {
        super::LanguageVariant::from_script_kind(
            ScriptKind::from_u32(script_kind).expect("Could not convert to u32 to ScriptKind"),
        )
        .to_u32()
        .expect("Could not convert LanguageVariant to u32")
    }
}
