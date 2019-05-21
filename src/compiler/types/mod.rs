use num_derive::{FromPrimitive, ToPrimitive};

pub mod character_codes;
pub mod node_flags;
pub mod pseudobigint;
pub mod syntax_kind;
pub mod text_range;
pub mod textspan;

pub use character_codes::CharacterCode;
pub use node_flags::NodeFlags;
pub use pseudobigint::PseudoBigInt;
pub use syntax_kind::SyntaxKind;

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
    Deferred
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, FromPrimitive, ToPrimitive)]
pub enum LanguageVariant {
    Standard,
    JSX
}

impl LanguageVariant {
    pub(crate) fn from_script_kind(script_kind: ScriptKind) -> LanguageVariant {
        use ScriptKind::*;
        // .tsx and .jsx files are treated as jsx language variant.
        match script_kind {
            TSX | JSX | JS | JSON => LanguageVariant::JSX,
            _ => LanguageVariant::Standard
        }
    }
}

#[cfg(feature = "wasm")]
mod wasm {
    #[wasm_bindgen]
    pub fn get_language_variant(script_kind: u32) -> u32 {
        super::LanguageVariant::from_script_kind(
            ScriptKind::from_u32(script_kind).expect("Could not convert to u32 to ScriptKind")
        ).to_u32().expect("Could not convert LanguageVariant to u32")
    }
}
