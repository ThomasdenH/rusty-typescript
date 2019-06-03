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

        const ACCESSIBILITY_MODIFIER = Self::PUBLIC | Self::PRIVATE | Self::PROTECTED;

        const PARAMETER_PROPERTY_MODIFIER = Self::ACCESSIBILITY_MODIFIER | Self::READONLY;
        const NON_PUBLIC_ACCESSIBILITY_MODIFIER = Self::PRIVATE | Self::PROTECTED;

        const TYPE_SCRIPT_MODIFIER = Self::AMBIENT | Self::PUBLIC | Self::PRIVATE
            | Self::PROTECTED
            | Self::ABSTRACT
            | Self::CONST;
        const EXPORT_DEFAULT = Self::Export | Self::DEFAULT;
        const ALL = Self::EXPORT
            | Self::AMBIENT
            | Self::PUBLIC
            | Self::PRIVATE
            | Self::PROTECTED
            | Self::STATIC
            | Self::READONLY
            | Self::ABSTRACT
            | Self::ASYNC
            | Self::DEFAULT
            | Self::CONST;
    }
}
