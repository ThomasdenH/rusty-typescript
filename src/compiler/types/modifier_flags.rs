use bitflags::bitflags;

bitflags! {
    pub struct ModifierFlags: u32 {
        const NONE = 0;
        const EXPORT = 1;
        const AMBIENT = 1 << 1;
        const PUBLIC = 1 << 2;
        const PRIVATE = 1 << 3;
        const PROTECTED = 1 << 4;
        const STATIC = 1 << 5;
        const READONLY = 1 << 6;
        const ABSTRACT = 1 << 7;
        const ASYNC = 1 << 8;
        const DEFAULT = 1 << 9;
        const CONST = 1 << 11;
        const HAS_COMPUTED_FLAGS = 1 << 29;

        const ACCESSIBILITY_MODIFIER = Self::PUBLIC.bits
            | Self::PRIVATE.bits
            | Self::PROTECTED.bits;

        const PARAMETER_PROPERTY_MODIFIER = Self::ACCESSIBILITY_MODIFIER.bits
            | Self::READONLY.bits;
        const NON_PUBLIC_ACCESSIBILITY_MODIFIER = Self::PRIVATE.bits
            | Self::PROTECTED.bits;

        const TYPE_SCRIPT_MODIFIER = Self::AMBIENT.bits
            | Self::PUBLIC.bits
            | Self::PRIVATE.bits
            | Self::PROTECTED.bits
            | Self::ABSTRACT.bits
            | Self::CONST.bits;
        const EXPORT_DEFAULT = Self::EXPORT.bits
            | Self::DEFAULT.bits;
        const ALL = Self::EXPORT.bits
            | Self::AMBIENT.bits
            | Self::PUBLIC.bits
            | Self::PRIVATE.bits
            | Self::PROTECTED.bits
            | Self::STATIC.bits
            | Self::READONLY.bits
            | Self::ABSTRACT.bits
            | Self::ASYNC.bits
            | Self::DEFAULT.bits
            | Self::CONST.bits;
    }
}
