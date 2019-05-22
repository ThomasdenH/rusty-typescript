use bitflags::bitflags;

bitflags! {
    pub struct TokenFlags: u16 {
        const NONE = 0;
        const PRECEDING_LINE_BREAK = 1;
        const PRECEDING_JSDOC_COMMENT = 1 << 1;
        const UNTERMINATED = 1 << 2;
        const EXTENDED_UNICODE_ESCAPE = 1 << 3;
        /// e.g. `10e2`
        const SCIENTIFIC = 1 << 4;
        /// e.g. `0777`
        const OCTAL = 1 << 5;
        /// e.g. `0x00000000`
        const HEX_SPECIFIER = 1 << 6;
        const BINARY_SPECIFIER = 1 << 7;
        const OCTAL_SPECIFIER = 1 << 8;
        const CONTAINS_SEPARATOR = 1 << 9;
        const BINARY_OR_OCTAL_SPECIFIER = Self::BINARY_SPECIFIER.bits | Self::OCTAL_SPECIFIER.bits;
        const NUMERIC_LITERAL_FLAGS = Self::SCIENTIFIC.bits
            | Self::OCTAL.bits
            | Self::HEX_SPECIFIER.bits
            | Self::BINARY_OR_OCTAL_SPECIFIER.bits
            | Self::CONTAINS_SEPARATOR.bits;
    }
}
