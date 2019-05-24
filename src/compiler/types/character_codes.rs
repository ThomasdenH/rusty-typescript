pub const NULL_CHARACTER: char = '\u{0}';

pub const LINE_FEED: char = '\n';
pub const CARRIAGE_RETURN: char = '\r';
pub const LINE_SEPARATOR: char = '\u{2028}';
pub const PARAGRAPH_SEPARATOR: char = '\u{2029}';
pub const NEXT_LINE: char = '\u{0085}';

// Unicode 3.0 space characters
pub const SPACE: char = ' ';
pub const NON_BREAKING_SPACE: char = '\u{00A0}';
pub const EN_QUAD: char = '\u{2000}';
pub const EM_QUAD: char = '\u{2001}';
pub const EN_SPACE: char = '\u{2002}';
pub const EM_SPACE: char = '\u{2003}';
pub const THREE_PER_EM_SPACE: char = '\u{2004}';
pub const FOUR_PER_EM_SPACE: char = '\u{2005}';
pub const SIX_PER_EM_SPACE: char = '\u{2006}';
pub const FIGURE_SPACE: char = '\u{2007}';
pub const PUNCTUATION_SPACE: char = '\u{2008}';
pub const THIN_SPACE: char = '\u{2009}';
pub const HAIR_SPACE: char = '\u{200A}';
pub const ZERO_WIDTH_SPACE: char = '\u{200B}';
pub const NARROW_NO_BREAK_SPACE: char = '\u{202F}';
pub const IDEOGRAPHIC_SPACE: char = '\u{3000}';
pub const MATHEMATICAL_SPACE: char = '\u{205F}';
pub const OGHAM: char = '\u{1680}';

pub const BACKSPACE: char = '\u{08}';
pub const FORM_FEED: char = '\u{0C}';
pub const BYTE_ORDER_MARK: char = '\u{FEFF}';
pub const TAB: char = '\t';
pub const VERTICAL_TAB: char = '\u{0B}';

#[cfg(feature = "wasm")]
mod wasm {
    pub type CharacterCodes = char;
}
