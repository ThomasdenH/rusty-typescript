use lazy_static::*;
use serde::Deserialize;
use std::collections::HashMap;

const DIAGNOSTIC_MESSAGES_JSON: &str = include_str!("diagnosticMessages.json");

lazy_static! {
    static ref IMPORTED_DIAGNOSTICS: HashMap<String, DiagnosticMessage> =
        serde_json::from_str(DIAGNOSTIC_MESSAGES_JSON).expect("Could not deserialize diagnostics");
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Deserialize)]
pub enum DiagnosticCategory {
    Warning,
    Error,
    Suggestion,
    Message,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, Debug, Deserialize)]
pub struct DiagnosticMessage {
    category: DiagnosticCategory,
    code: usize,
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_diagnostic_message() {
        assert_eq!(IMPORTED_DIAGNOSTICS.len(), 1228);
    }
}
