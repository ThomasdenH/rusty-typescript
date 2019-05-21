use bitflags::bitflags;

bitflags! {
    pub struct NodeFlags: u32 {
        const NONE = 0;
        /// Variable declaration
        const LET = 1;
        /// Variable declaration
        const CONST = 1 << 1;
        /// Namespace declaration
        const NESTED_NAMESPACE = 1 << 2;
        /// Node was synthesized during transformation
        const SYNTHESIZED = 1 << 3;
        /// Namespace declaration
        const NAMESPACE = 1 << 4;
        /// Export context (initialized by binding)
        const EXPORT_CONTEXT = 1 << 5;
        /// Interface contains references to "this"
        const CONTAINS_THIS = 1 << 6;
        /// If function implicitly returns on one of codepaths (initialized by binding)
        const HAS_IMPLICIT_RETURN = 1 << 7;
        /// If function has explicit reachable return on one of codepaths (initialized by binding)
        const HAS_EXPLICIT_RETURN = 1 << 8;
        /// Set if module declaration is an augmentation for the global scope
        const GLOBAL_AUGMENTATION = 1 << 9;
        /// If the file has async functions (initialized by binding)
        const HAS_ASYNC_FUNCTIONS = 1 << 10;
        /// If node was parsed in a context where 'in-expressions' are not allowed
        const DISALLOW_IN_CONTEXT = 1 << 11;
        /// If node was parsed in the 'yield' context created when parsing a generator
        const YIELD_CONTEXT = 1 << 12;
        /// If node was parsed as part of a decorator
        const DECORATOR_CONTEXT = 1 << 13;
        /// If node was parsed in the 'await' context created when parsing an async function
        const AWAIT_CONTEXT = 1 << 14;
        /// If the parser encountered an error when parsing the code that created this node
        const THIS_NODE_HAS_ERROR = 1 << 15;
        /// If node was parsed in a JavaScript
        const JAVA_SCRIPT_FILE = 1 << 16;
        /// If this node or any of its children had an error
        const THIS_NODE_OR_ANY_SUB_NODES_HAS_ERROR = 1 << 17;
        /// If we've computed data from children and cached it in this node
        const HAS_AGGREGATED_CHILD_DATA = 1 << 18;

        // These flags will be set when the parser encounters a dynamic import expression or 'import.meta' to avoid
        // walking the tree if the flags are not set. However, these flags are just a approximation
        // (hence why it's named "PossiblyContainsDynamicImport") because once set, the flags never get cleared.
        // During editing, if a dynamic import is removed, incremental parsing will *NOT* clear this flag.
        // This means that the tree will always be traversed during module resolution, or when looking for external module indicators.
        // However, the removal operation should not occur often and in the case of the
        // removal, it is likely that users will add the import anyway.
        // The advantage of this approach is its simplicity. For the case of batch compilation,
        // we guarantee that users won't have to pay the price of walking the tree if a dynamic import isn't used.
        /* @internal */ const POSSIBLY_CONTAINS_DYNAMIC_IMPORT = 1 << 19;
        /* @internal */ const POSSIBLY_CONTAINS_IMPORT_META = 1 << 20;

        /// If node was parsed inside jsdoc
        const JSDOC = 1 << 21;
        /// If node was inside an ambient context -- a declaration file, or inside something with the `declare` modifier.
        /* @internal */ const AMBIENT = 1 << 22;
        /// If any ancestor of node was the `statement` of a WithStatement (not the `expression`)
        /* @internal */ const IN_WITH_STATEMENT = 1 << 23;
        /// If node was parsed in a Json
        const JSON_FILE = 1 << 24;

        const BLOCK_SCOPED = Self::LET.bits | Self::CONST.bits;

        const REACHABILITY_CHECK_FLAGS = Self::HAS_IMPLICIT_RETURN.bits | Self::HAS_EXPLICIT_RETURN.bits;
        const REACHABILITY_AND_EMIT_FLAGS = Self::REACHABILITY_CHECK_FLAGS.bits | Self::HAS_ASYNC_FUNCTIONS.bits;

        /// Parsing context flags
        const CONTEXT_FLAGS = Self::DISALLOW_IN_CONTEXT.bits
            | Self::YIELD_CONTEXT.bits
            | Self::DECORATOR_CONTEXT.bits
            | Self::AWAIT_CONTEXT.bits
            | Self::JAVA_SCRIPT_FILE.bits
            | Self::IN_WITH_STATEMENT.bits
            | Self::AMBIENT.bits;

        // Exclude these flags when parsing a Type
        const TYPE_EXCLUDES_FLAGS = Self::YIELD_CONTEXT.bits | Self::AWAIT_CONTEXT.bits;

        // Represents all flags that are potentially set once and
        // never cleared on SourceFiles which get re-used in between incremental parses.
        // See the comment above on `PossiblyContainsDynamicImport` and `PossiblyContainsImportMeta`.
        /* @internal */ const PERMANENTLY_SET_INCREMENTAL_FLAGS = Self::POSSIBLY_CONTAINS_DYNAMIC_IMPORT.bits
            | Self::POSSIBLY_CONTAINS_IMPORT_META.bits;
    }
}
