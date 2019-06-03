pub(crate) enum QuotePreference {
    Auto,
    Double,
    Single
}

pub(crate) enum ImportModuleSpecifierPreference {
    Relative,
    NonRelative
}

pub(crate) enum ImportModuleSpecifierEnding {
    Minimal,
    Index,
    Js
}

pub(crate) struct UserPreferences {
    disable_suggestions: Option<bool>,
    quote_preference: Option<QuotePreference>,
    include_completions_for_module_exports: Option<bool>,
    include_completions_with_insert_text: Option<bool>,
    import_module_specifier_preference: Option<ImportModuleSpecifierPreference>,
    import_module_specifier_ending: Option<ImportModuleSpecifierEnding>,
    allow_text_changes_in_new_files: Option<bool>,
    provide_prefix_and_suffix_text_for_rename: Option<bool>
}
